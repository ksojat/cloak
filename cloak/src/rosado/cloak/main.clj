;; simple build system in Clojure
;; Roland Sadowski [szabla gmail com] http://www.haltingproblem.net

;; Copyright (c) 2008 Roland Sadowski. All rights reserved.  The use and
;; distribution terms for this software are covered by the Common
;; Public License 1.0 (http://www.opensource.org/licenses/cpl1.0.php)
;; which can be found in the file CPL.TXT at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

;; Here's how it works: every (task ...) and (file ...) call adds a map
;; to the *tasks* map {:task-name {:actions ... :deps ... :desc ...}}.
;; Then a graph is created and sorted topologically (any cycles are
;; detected + exception is thrown). task-table provides a translation
;; between task names and indices of the graph.

(ns rosado.cloak.main
  (:import
    (java.io File)
    (org.apache.commons.io FileUtils))
  (:require [rosado.cloak.core :as core]))

(defstruct task-struct :actions :deps :desc)

(def *queue*)							;holds sorted tasks
(def #^{:private true}
     task-table (ref {:to-int {} :to-task {}}))
(def #^{:private true}
     task-order)

(def *current-task*)                    ;holds keyword of currently executed task
(def *verbose* false)
(def *try-only* false)

; TODO: Yes, i will add them to +settins+, so they can be reconfigured
(def *notify-handler* println); TODO: Can i merge this with error-handler in some concept of logger
(def *error-handler* println); TODO: Why default can't be task-error from rosado.cloak?

(defmulti to-task class)

(defmethod to-task java.lang.Integer [index]
  ((@task-table :to-task) index))

(defmethod to-task clojure.lang.Keyword [kw]
  ((@task-table :to-int) kw))

(defmethod to-task java.lang.String [fname]
  ((@task-table :to-int) fname))

(defn save-task [task-name task-info]
  (swap! core/*build* assoc-in [:tasks task-name] task-info))

(defn- annotate-task
  "Adds metadata to task. Does not save it in *tasks*."
  [task-name kw val]
  (assert (not= nil task-name))
  (let [t (get-in @core/*build* [:tasks task-name])]
    (save-task task-name (with-meta t (merge {} (meta t) {kw val})))))

(defn- task-annotations
  "Returns annotations (meta-data) of a task."
  [task-name]
  (meta (get-in @core/*build* [:tasks task-name])))

(defn do-task [task-name]
  (assert (not= nil task-name))
  (let [tsk (get-in @core/*build* [:tasks task-name])]
    (if-let [prefun (tsk :pre-check)]
      (if (prefun)
        (when-let [actions (tsk :actions)]
          (actions))
        (*notify-handler* " * skipping *"))
      (when-let [actions (tsk :actions)]
        (actions)))))

(defn clear-tasks!
  "Clears task table and *tasks* map (which holds defined tasks)"
  []
  (swap! core/*build* assoc :tasks {})
  (dosync
   (ref-set task-table {})))

(derive clojure.lang.LazilyPersistentVector ::Dependencies)
(derive clojure.lang.PersistentVector ::Dependencies)
(derive clojure.lang.IPersistentList ::Actions)

(defmulti parse-task (fn [mp elems] (class (first elems))))

(defmethod parse-task ::Dependencies [mp elems]
  (assoc mp :deps (first elems)))

(defmethod parse-task java.lang.String [mp elems]
  (assoc mp :desc (first elems)))

(defmethod parse-task ::Actions [mp elems]
  (assoc mp :actions `(fn [] (do ~@elems))))

; TODO: Do i need this?
(defmethod parse-task nil [mp elems]    ;dummy task, no actions
  (assoc mp :actions `(fn[] nil)))

(defn- to-task-struct [sequ]
  (loop [r sequ tsk (struct task-struct nil nil nil)]
    (if (not (tsk :actions))            ;:actions should be added last
      (recur (next r) (parse-task tsk r))
      tsk)))

(defn- fail-if-defined [task-name]
  (when (contains? (:tasks @core/*build*) task-name)
    (throw (Exception. "Task already defined."))))

(defmacro task [task-name & rst]
  (fail-if-defined task-name)
  (let [task (to-task-struct rst)]
    `(save-task ~task-name ~task)))

(defn- pre-check-fn [file-name fnames]
  `(fn [#^String e#]
     (let [f# (File. ~file-name)
           o# (File. e#)]
       (if (not (.exists o#))
         (let [msg# (format "File dependency not met: %s" e#)]
           (*error-handler* "Failure:" msg#)
           (throw (Exception. msg#))))
       (if (.exists f#)
         (org.apache.commons.io.FileUtils/isFileOlder o# f#)
         true))))

(defmacro file [file-name & rst]
  (fail-if-defined file-name)
  (let [task (to-task-struct rst)
        fnames (doall (filter #(isa? (class %) String) (:deps task)))
        pre-check  `(fn [] (some ~(pre-check-fn file-name fnames) (list ~@fnames)))
        ftask (assoc task :pre-check pre-check)]
    `(save-task ~file-name ~ftask)))

(defn- make-table
  "Makes a dispatch table between task names and indices."
  [task-map]
  (let [ks (keys task-map) indices (range 1 (inc (count ks)))]
    {:to-int (zipmap ks indices) :to-task (zipmap indices ks)}))

(defn- task-names [] (-> @task-table :to-int keys))

(defn- task-indices [] (-> @task-table :to-int vals))

(defn init-tasks []
  (dosync (ref-set task-table (make-table (:tasks @core/*build*)))))

(defn sort-tasks [& _]
  (println "Placeholder, remove later"))

(defn add-task-vertex [g index]
  (println "Placeholder, remove later"))

(defn make-task-graph [tasks]
  (println "Placeholder, remove later"))

(defn execute-task [task-kw]
  (binding [*queue* [] *current-task* task-kw]
    (when *verbose*
      (*notify-handler* "TASKS//Indices:" (task-indices) "//" (task-names)))
    (sort-tasks (make-task-graph (:tasks @core/*build*)) (to-task task-kw))
    (doseq [q *queue*]
      (try
       (when-not (:done (task-annotations (to-task q)))
         (*notify-handler* "== Executing task" (to-task q))
         (binding [*current-task* (to-task q)]
           (when-not *try-only*
             (do-task (to-task q))))
         (annotate-task (to-task q) :done true)
         (*notify-handler* "Done."))
       (catch Exception e
         (*error-handler* "Error executing task")
         (*error-handler* (.getMessage e))
         (when *verbose*
           (*error-handler* (interpose "\n" (.getStackTrace e))))
         (throw e))))))
