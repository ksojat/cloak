;; Copyright (c) 2009 Krešimir Šojat. All rights reserved.  The use and
;; distribution terms for this software are covered by the Common
;; Public License 1.0 (http://www.opensource.org/licenses/cpl1.0.php)
;; which can be found in the file CPL.TXT at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns cloak.core
  (:import
    (clojure.lang Symbol Fn Keyword)
    (java.io File FileNotFoundException)
    (org.apache.oro.text GlobCompiler)
    (org.apache.oro.text.regex Perl5Matcher))
  (:use
    [clojure.set :exclude [project]]
    cloak.utils))

(def info
  {:version "0.1a"
   :authors [{:name "Roland Sadowski" :email "szabla gmail com"}
             {:name "Krešimir Šojat"  :email "ksojat gmail com"}]})

;;
;; Event system.
;;

(def
  #^{:doc
      (str
        "Set of global event listeners. Every listener will be added to "
        "newly created build automaticaly and to every already created "
        "build."
        "If listener is removed from global listeners it will be removed"
        "from all active builds too.")}
  global-listeners (atom {}))

(defn on
  ([event callback-fn]
    (swap! global-listeners update-in [event] #(conj (set %1) %2) callback-fn))

  ([build event callback-fn]
    (swap! build update-in [:listeners event] #(conj (set %1) %2) callback-fn)))

(defn emmit [build event & args]
  (doseq [f (mapcat event [(:listeners @build) global-listeners])]
    (apply f build args)))

;;
;; Properties.
;;

(declare
  #^{:doc (str "Tasks sould bind this in there action function to provied"
               "property map.")}
  *properties*)

(defmulti property-resolver class)

(defmethod property-resolver Fn [pred]
  #(filter pred %))

(defmethod property-resolver Symbol [s]
  (fn [ps]
    (if (some #(= s %) ps)
      s
      (throwf "Can't find property: %s" s))))

(defmethod property-resolver Keyword [k]
  (property-resolver #(= k (type %))))

; TODO: Remove self
(defn resolve-properties [pm]
  (let [pk (keys pm)]
    (into (empty pm)
      (map
        (fn [[k v]]
          [k
           (assoc v :deps (map #(% pk) (:resolve-fns v)))])
        pm))))

(defn eval-properties [pm]
  (let [pm    (resolve-properties pm)
        pg    (into {} (map (fn [[k v]] [k (:deps v)]) pm))
        order (tsort pg)]
    (reduce
      (fn [m p]
        (assoc m p
          (let [deps (get-in pm [p :deps])
                args (map
                       (fn [x]
                         (if (seq? x) (map #(get m %) x) (get m x)))
                       deps)]
            (apply (get-in pm [p :expr-fn]) args))))
      {}
      order)))

(defn create-property* [name resolve-fns expr-fn]
  #^{:type :Property} {:name name, :resolve-fns resolve-fns, :expr-fn expr-fn})

(defn genp [meta-data]
  (with-meta (gensym "property__") (merge meta-data {:anonymous true})))

(defn create-property [id deps expr-fn]
  (let [name (cond
               (symbol?  id) id
               (keyword? id) (genp {:type id})
               (map?     id) (genp id)
               :else
                 (throwf "Invalid property id: %s" id))
        rfs (map property-resolver deps)]
    (create-property* name rfs expr-fn)))

(defmacro property
  ([id deps bindings expr]
    `(*collector*
       (create-property '~id '~deps (fn ~bindings ~expr))))

  ([id bindings expr]
    `(*collector*
       (create-property '~id '~bindings (fn ~bindings ~expr))))

  ([id expr]
    `(*collector*
       (create-property '~id [] (fn [] ~expr)))))

; TODO: This will work by name, but what i need is to use property-resolver
(defmacro with-properties [pm deps bindings & body]
  `(let [~bindings (map (fn [p#] (~pm p#)) ~deps)]
     ~@body))

(defmacro letp
  ([bindings]
    `(println "Not implemented for now"))

  ([imports bindings]
    `(println "Not implemented for now")))

;;
;; Build Collectors.
;;

(def *collector* identity)

(defn with-collector** [collector-fn body-fn]
  (reduce
    collector-fn
    {}
    (let [data (atom [])]
      (binding [*collector* #(swap! data conj %)]
        (body-fn))
      @data)))

(defn with-collector* [collector-fn body-fn]
  (let [{props :properties, tasks :tasks gm :groups}
         (with-collector** collector-fn body-fn)
        props (eval-properties props)]
    (println "Groups: " gm)
    {:tasks
      (into {}
        (map
          (fn [[k {r :resolve-fn a :action-fn :as t}]]
            [k
             (-> t
               (assoc :resolve-fn (partial r props))
               (assoc :action-fn  (partial a props)))])
          tasks))}))

(defmacro with-collector [collector-fn & body]
  `(with-collector* ~collector-fn (fn [] ~@body)))

(defn add-property [data {xn :name :as x}]
  (when (get-in data [:properties xn])
    (throwf "Property %s already defined." xn))

  (assoc-in data [:properties xn] (dissoc x :name)))

(defn add-task [data {xn :name :as x}]
  (when (get-in data [:tasks xn])
    (throwf "Task %s already defined." xn))

  (assoc-in data [:tasks xn] (dissoc x :name)))

(defn add-group [data {xn :name :as x}]
  (println "Adding group: " x)
  (when (get-in data [:groups xn])
    (throwf "Group %s already defined." xn))

  (assoc-in data [:groups xn] (dissoc x :name)))

(defmulti
  #^{:doc (str "Build collector user primary to collect build elements from "
               "cloakfile files.")}
  main-collector
  (fn [data x] (type x)))

(defmethod main-collector :Property [data x]
  (when (:group data)
    (throwf "Can't define property outside of main build group."))

  (add-property data x))

(defmethod main-collector ::Task [data x]
  (when (:group data)
    (throw "Can't define task outside of main build group."))

  (add-task data x))

(defmethod main-collector :TopGroup [{pm :properties tm :tasks gm :groups :as data} x]
  (when (not (empty? gm))
    (throwf "Can't redefine topleve build group."))

  (when (or pm tm); TODO: not empty
    (throwf
      (str "Can't define toplevel build group, tasks or properties are "
           "already declared.")))

  (add-group data x))

(defmethod main-collector :SubGroup [_ x]
  (throwf "Can't create sub group outside of toplevel build group."))

(defmulti
  #^{:doc (str "Collector used to collect build elements inside top-level "
               "build group.")}
  top-collector
  (fn [data x] (type x)))

(defmethod top-collector :Property [data x]
  (add-property data x))

(defmethod top-collector ::Task [data x]
  (add-task data x))

(defmethod top-collector :TopGroup [_ x]
  (throwf "Can't define toplevel build group inside another group."))

(defmethod top-collector :SubGroup [{gm :groups :as data} x]
  (when (get gm (:name x))
    (throwf "Can't define group %s, already defined." (:name x))))

(defmulti
  #^{:doc "Collector used to collect build elements inside sub group."}
  sub-collector
  (fn [data x] (type x)))

(defmethod sub-collector :Property [data x]
  (add-property data x))

(defmethod sub-collector ::Task [data x]
  (add-task data x))

(defmethod sub-collector :TopGroup [_ x]
  (throwf "Can't define top level group inside another group."))

(defmethod sub-collector :SubGroup [{gm :groups :as data} x]
  (when (get gm (:name x))
    (throwf "Can't define group %s, already defined." (:name x)))

  (assoc-in [:groups (:name x)] (dissoc x :name)))

;;
;; Task ids
;;

(defn id->string
  "Convert task id to string."
  [{p :prefix n :name}]
  (str (apply str (interpose ":" p)) ":" n))

(defn string->id
  "Convert string to task id."
  [s]
  (let [s (map symbol (seq (.split s ":")))]
    {:prefix (drop-last s) :name (last s)}))

(defn glob->pattern [g]
  (.compile (GlobCompiler.) g GlobCompiler/STAR_CANNOT_MATCH_NULL_MASK))

(defn glob-matcher [g]
  (let [p (glob->pattern g), m (Perl5Matcher.)]
    #(.contains m % p)))

(defn filter-ids
  "Filter seqence of task ids by given glob pattern."
  [glob ids]
  (let [m (glob-matcher glob)] (filter #(m (id->string %)) ids)))

;;
;; Task dependency resolvers.
;;

(defmulti task-resolver class)

; TODO:
; Remove set, add symbol and set can contain symbols, predicate fns and keywords.

(defmethod
  #^{:doc "Trie to resolve task's dependencies from set of exact task names."}
  task-resolver clojure.lang.IPersistentSet [s]
  (let [s (into (empty s) (map (fn [x] (if (list? x) x (list x))) s))]
    (fn [_ ts]
      (let [x (intersection s (set ts))]
        (if (= s x)
          x
          (throwf "Could not find dependencies: %s" (difference s ts)))))))

(defmethod
  #^{:doc "Dependencies are collected based on predicate used to
           filter task names."}
  task-resolver Fn [pred]
  (fn [_ ts]
    (into #{} (filter pred ts))))

(defmethod
  task-resolver Symbol [s]
  (fn [_ ts]
    (if (some #(= s %) ts)
      s
      (throwf "Can't find task: %s" s))))

(defmethod
  #^{:doc "Dependencies are collected based on value of :type key in there
           metadata."}
  task-resolver Keyword [k] (task-resolver #(= k (type %))))

;;
;; Tasks.
;;


(defmulti create-task
  (fn [type & _] type))

; TODO: Why do i need to import create-task in target namespace??
(defmacro deftask [type & fn-tail]
  (let [[type base] (if (seq? type) type [type ::Task])]
    `(do
      (derive ~type ~base)
      (defmethod create-task
        ~type
        ([_# & args#]
          (let [r# (apply (fn ~@fn-tail) args#)]
            (with-meta r# (assoc (meta r#) :type ~type))))))))

(defn resolve-tasks [tm]
  (let [tk (set (keys tm))]
    (into (empty tm)
      (map
        (fn [[k v]]
          [k (assoc v :deps ((:resolve-fn v) (disj tk k)))])
        tm))))

(defn execute-tasks [start tm]
  (let [tm (resolve-tasks tm)
        tg (into {} (map (fn [[k v]] [k (:deps v)]) tm))
        order (tsort tg start)]
    (doseq [t order]
      ((:action-fn (tm t))))))

;;
;; Fragments.
;;

(def +fragments+
  (atom {}))

(defmulti fragment-resolver type)

(defmethod fragment-resolver Fn [pred]
  #(filter pred %))

(defmethod fragment-resolver Symbol [s]
  (fn [fs]
    (if (some #(= s %) fs)
      s
      (throwf "Unknow fragment %s: " s))))

(defmethod fragment-resolver Keyword [k]
  (fragment-resolver #(= k (type %))))

(defmethod fragment-resolver clojure.lang.IPersistentMap [m]
  (fn [fs]
    (fragment-resolver #(= m (meta %)))))

(defn add-fragment! [fragment]
  (swap! +fragments+ assoc (:name fragment) (dissoc fragment :name)))

(defn create-fragment []
  (println "Working on this one"))

(defmacro fragment [name f-deps p-deps p-bindings & body]
  `(add-fragment!
     {:name '~name
      :f-deps ~f-deps
      :p-deps ~p-deps
      :expand-fn
        (fn ~p-bindings ~@body)}))

;;
;; Groups.
;;

#_(defn create-top-group [name expand-fn]
  #^{:type :TopGroup} {:name name :expand-fn expand-fn})

#_(defmacro build [name & body]
  `(*collector*
     (create-top-group '~name (fn [] ~@body))))

#_(defn create-sub-group [name expand-fn]
  #^{:type :SubGroup} {:name name :expand-fn expand-fn})

#_(defmacro group [name & body]
  `(*collector*
     (create-sub-group '~name (fn [] ~@body))))

;;
;; Build related functions.
;;

; TODO: Update log to use emmit function
(defn log [level & args]
  (println (apply str " " (.toUpperCase (name level)) ": " args)))

(defn build-settings []
  {:logger    {:level :info
               :fn log}
   :file      ["cloakfile" "cloakfile.clj"]
   :describe  false
   :verbose   false
   :targets   []; TODO: Autodiscover defaults
   :tasks     {}
   :queue     []; TODO ???
   :cwd       (System/getProperty "user.dir" "")
   :bin       (System/getProperty "cloak.bin" "cloak")
   :home      (System/getProperty "cloak.home" "")
   :listeners @global-listeners})

(defn create-build [settings]
  (let [build (atom (merge (build-settings) settings))]
    ; Notify all listeners that build was initialized.
    (emmit build ::build-created)
    build))

(defn find-cloakfile [files cwd]
  (let [cwd (File. cwd)]
    (first
      (filter #(.exists %)
        (map #(File. cwd %) files)))))

(defn load-cloakfile! [build]
  (let [files (:file @build), cwd (:cwd @build)]
    (if-let [file (find-cloakfile files cwd)]
      (do
        (emmit build ::cloakfile-found file)
        (try
          (with-collector main-collector
            (load-file (.getAbsolutePath file)))

          (catch FileNotFoundException e
            (emmit build ::cloakfile-missing files)
            (System/exit 1))

          (catch Exception e
            (emmit build ::cloakfile-failed file e)
            (System/exit 1))))
      (do
        (emmit build ::cloakfile-missing files)
        (System/exit 1)))))

; TODO: Add cli patterns matching
(defn start-tasks [patterns tasks]
  (filter #(get (meta (last %)) :default) (keys tasks)))

; TODO: If its describe, describe only and exit
(defn start-build! [build]
  (emmit build ::build-started)
  (let [defs  (load-cloakfile! build)
        tasks (resolve-tasks (:tasks defs))
        start (start-tasks (:targets @build) tasks)]
    ; TODO: If no start tasks are found, terminate
    (execute-tasks start tasks))
  (emmit build ::build-finished))
