;; Copyright (c) 2009 Krešimir Šojat. All rights reserved.  The use and
;; distribution terms for this software are covered by the Common
;; Public License 1.0 (http://www.opensource.org/licenses/cpl1.0.php)
;; which can be found in the file CPL.TXT at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns cloak.core
  (:use [clojure.set :exclude [project]]))

(import '(org.apache.oro.text GlobCompiler)
        '(org.apache.oro.text.regex Perl5Matcher))

;;
;; Utility functions
;;

(defn throwf [& args]
  (throw (Exception. (apply format args))))

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
      "Set of global event listeners. Every listener will be added to
       newly created build automaticaly and to every already created
       build.
       If listener is removed from global listeners it will be removed
       from all active builds too."}
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
;; Build related functions.
;;

(defn add-property! [build key value]
  (swap! build assoc-in [:properties key] value))

(defn property
  ([key value]
    (add-property! *build* key value)
    (emmit *build* ::property key value))

  ([key]
    (get-in @*build* [:properties key])))

(defn log [level & args]
  (println (apply str " " (.toUpperCase (name level)) ": " args)))

(defn build-settings []
  {:logger    {:level :info
               :fn log}
   :file      ["cloakfile" "cloakfile.clj"]
   :describe  false
   :verbose   false
   :targets   [:default]
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

(def *collector* identity)

(defmulti create-task
  (fn [type & _] type))

; TODO: Fix fn-tail to remove first argument from bindings.
; TODO: Why do i need to import create-task in target namespace??
; TODO: Add with-meta to fn-tail output
; TODO: Add type as vector [type base] so TaskBase dosn't need to be
;       the only taks base.
(defmacro deftask [type & fn-tail]
  `(do
      (derive ~type ::TaskBase)
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
(defmethod print-method ::TaskBase [t #^java.io.Writer w]
  (.write w (str "Task: " (:name t))))

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
