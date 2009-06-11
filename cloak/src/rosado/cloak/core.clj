;; Copyright (c) 2009 Krešimir Šojat. All rights reserved.  The use and
;; distribution terms for this software are covered by the Common
;; Public License 1.0 (http://www.opensource.org/licenses/cpl1.0.php)
;; which can be found in the file CPL.TXT at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns rosado.cloak.core
  (:use clojure.set))

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
    ;(swap! *build* assoc-in [:properties key] value)
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
;; Task dependency resolvers.
;;

(defmulti resolver class)

(defmethod
  #^{:doc "Trie to resolve task's dependencies from set of exact task names."}
  resolver clojure.lang.IPersistentSet [s]
  (fn [ts]
    (let [x (intersection s ts)]
      (if (= s x)
        x
        (throwf "Could not find dependencies: %s" (difference s ts))))))

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
(defmacro deftask [type & fn-tail]
  `(do
      (derive ~type ::TaskBase)
      (defmethod create-task ~type ~@fn-tail)))

;; TODO: If :deps then don't do resolve, just strip it out
(defn resolve-1 [task ts]
  ((:resolve task) (filter #(= task %) ts)))

;; TODO: Tag with ::TaskMap?
(defn resolve-all [tasks]
  (println "hi"))

; TODO: Remove this
(defmethod print-method ::TaskBase [t #^java.io.Writer w]
  (.write w (str "Task: " (:name t))))
