;; Copyright (c) 2009 Krešimir Šojat. All rights reserved.  The use and
;; distribution terms for this software are covered by the Common
;; Public License 1.0 (http://www.opensource.org/licenses/cpl1.0.php)
;; which can be found in the file CPL.TXT at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns cloak.core
  (:use
    [clojure.set :exclude [project]]
    cloak.utils))

(import '(java.io File FileNotFoundException)
        '(org.apache.oro.text GlobCompiler)
        '(org.apache.oro.text.regex Perl5Matcher))

; Holds current active build.
(declare *build*)

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
;; Build Collectors.
;;

(def *collector* identity)

(defn with-collector* [collector-fn body-fn]
  (reduce
    collector-fn
    {}
    (let [data (atom [])]
      (binding [*collector* #(swap! data conj %)]
        (body-fn))
      @data)))

(defmacro with-collector [collector-fn & body]
  `(with-collector* ~collector-fn (fn [] ~@body)))

(defn add-property [data {xn :name :as x}]
  (when (get-in data [:properties xn])
    (throwf "Property %s already defined." xn))

  (update-in data [:properties xn] (dissoc x :name)))

(defn add-task [data {xn :name :as x}]
  (when (get-in data [:tasks xn])
    (throwf "Task %s already defined." xn))

  (update-in data [:tasks xn] (dissoc x :name)))

(defn add-group [data {xn :name :as x}]
  (when (get-in data [:groups xn])
    (throwf "Group %s already defined." xn))

  (update-in data [:groups xn] (dissoc x :name)))

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

(defmethod main-collector :TopGroup [{pm :properties tm :tasks g :group :as data} x]
  (when g
    (throwf "Can't redefine topleve build group."))

  (when (or pm tm)
    (throwf
      (str "Can't define toplevel build group, tasks or properties are "
           "already declared.")))

  (assoc data :group g))

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

  (update-in [:groups (:name x)] (dissoc x :name)))

;;
;; Properties.
;;

(defn genp [meta-data]
  (with-meta (gensym "property__") (merge meta-data {:anonymous true})))

(defn create-property [id resolve-fn expr-fn]
  (let [name (cond
               (symbol?  id) id
               (keyword? id) (genp {:type id})
               (map?     id) (genp id)
               :else
                 (throw (Exception. (str "Unsupported property id: " id))))]
    {:name name, :resolve-fn resolve-fn, :expr-fn expr-fn}))

(defmacro property
;  ([id deps bindigns expr]
;    `(create-property '~id (fn [] nil) (fn ~bindings ~expr)))

;  ([id bindings expr]
;    `(create-property '~id (fn [] nil) (fn ~bindings ~expr)))

  ([id expr]
    `(create-property '~id (fn [] nil) (fn [] ~expr))))

(defmacro letp
  ([bindings]
    `(println "Not implemented for now"))

  ([imports bindings]
    `(println "Not implemented for now")))

;;
;; Build related functions.
;;

#_(defn add-property! [build key value]
  (swap! build assoc-in [:properties key] value))

#_(defn property
  ([key value]
    (add-property! *build* key value)
    (emmit *build* ::property key value))

  ([key]
    (get-in @*build* [:properties key])))

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
  (first
    (filter #(.exists %)
      (map #(File. (File. cwd) %) files))))

(defn load-cloakfile! [build]
  (let [files (:file @build), cwd (:cwd @build)]
    (if-let [file (find-cloakfile files cwd)]
      (do
        (emmit build ::cloakfile-found file)
        (try
          (println; TODO: Remove this
            (with-collector main-collector
              (load-file (.getAbsolutePath file))))

          (catch FileNotFoundException e
            (emmit build ::cloakfile-missing files)
            (System/exit 1))

          (catch Exception e
            (emmit build ::cloakfile-failed file e)
            (System/exit 1))))
      (do
        (emmit build ::cloakfile-missing files)
        (System/exit 1)))))

; TODO: If its describe, describe only and exit
(defn start-build! [build]
  ;(println build)
  (emmit build ::build-started)
  (load-cloakfile! build)
  (emmit build ::build-finished))

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

(defmulti resolver class)

(defmethod
  #^{:doc "Trie to resolve task's dependencies from set of exact task names."}
  resolver clojure.lang.IPersistentSet [s]
  (let [s (into (empty s) (map (fn [x] (if (list? x) x (list x))) s))]
    (fn [ts]
      (let [x (intersection s ts)]
        (if (= s x)
          x
          (throwf "Could not find dependencies: %s" (difference s ts)))))))

(defmethod
  #^{:doc "Dependencies are collected based on predicate used to
           filter task names."}
  resolver clojure.lang.Fn [pred]
  (fn [ts]
    (into #{} (filter pred ts))))

(defmethod
  #^{:doc "Dependencies are collected based on value of :type key in there
           metadata."}
  resolver clojure.lang.Keyword [k] (resolver #(= k (type %))))

;;
;; Tasks.
;;


(defmulti create-task
  (fn [type & _] type))

; TODO: Fix fn-tail to remove first argument from bindings.
; TODO: Why do i need to import create-task in target namespace??
; TODO: Add with-meta to fn-tail output
; TODO: Add type as vector [type base] so TaskBase dosn't need to be
;       the only taks base.
(defmacro deftask [type & fn-tail]
  `(do
      (derive ~type ::Task)
      (defmethod create-task ~type ~@fn-tail)))

(defn resolve-1 [{d :deps, r :resolve :as task} ts]
  (let [ts (into (empty ts) (filter #(not= (:name task) %) ts))]
    ; TODO: Catch exceptions, report with full taks name
    (try
      (-> task
        (assoc :deps (r ts)) (dissoc :resolve))
      (catch Exception e
        (throw (Exception. (str "Task " (:name task) ": " (.getMessage e))))))))

(defn resolve-all [tasks]
  (let [ts (set (map :name tasks))]
    (println ts)
    (map #(resolve-1 % ts) tasks)))

; TODO: Remove this
;(defmethod print-method ::TaskBase [t #^java.io.Writer w]
;  (.write w (str "Task: " (:name t))))

(defmacro project [name & specs])

(defn pop-module-prefix [prefix tasks]
  (into (empty tasks)
    (map
      (fn [t]
        (if (and (= (first t) prefix) (next t))
          (pop t)
          t))
      tasks)))

(defn module-collector [name body-fn]
  (let [tasks (atom [])]
    (binding [*collector* #(swap! tasks conj %)]
      (body-fn))
    (doseq [{n :name, r :resolve :as t} @tasks]
      (let [n (conj n name)
            r (fn [ts]
                (let [ts (filter seq? ts)]
                  (r ts)))]
        (*collector* (assoc t :name n :resolve r))))))

(defmacro module [name & body]
  `(module-collector '~name (fn [] ~@body)))

(defmacro configuration [& body])

;;
;; Groups and fragments.
;;

(defn load-fragment [& names]
  (println "Not implemented for now"))

(defmacro fragment [name & body]
  `(println "Fragments are not implemented for now"))

(defn create-group [name fragments]
  (with-meta
    {:name name :fragments fragments}
    {:type ::Group}))

(defmacro group [name & body]
  `(println "Groups are not implemented for now"))
