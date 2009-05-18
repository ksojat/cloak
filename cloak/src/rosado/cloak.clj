;; simple build system in Clojure
;; Roland Sadowski [szabla gmail com] http://www.haltingproblem.net

;; Copyright (c) 2008 Roland Sadowski. All rights reserved.  The use and
;; distribution terms for this software are covered by the Common
;; Public License 1.0 (http://www.opensource.org/licenses/cpl1.0.php)
;; which can be found in the file CPL.TXT at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns rosado.cloak
  (:import (java.lang System))
  (:use rosado.cloak.main)
  (:gen-class))

(def *default-cloak-file* "CLOAK")
(def *default-task* :default)
(def *describe-only* false)
(def *target-queue* [])
(def *CWD* (System/getProperty "user.dir"))
(def path-sep #^java.lang.String (java.io.File/separator))

(def +settings+
  (atom {:cwd  (System/getProperty "user.dir" "")
         :bin  (System/getProperty "cloak.bin" "cloak")
         :home (System/getProperty "cloak.home" "")})); TODO: Default to ~/.cloak

(defn notification [& args]
  (apply println (cons " NOTIFICATION:" args)))

(defn error [& args]
  (apply println (cons " ERROR:" args)))

(defn task-error
  "Error handler function used for task failures."
  [& args]
  (apply println (cons (str " " \[ (name *current-task*) \]) args)))

(defn- load-tasks
  "Loads tasks from file (*default-cloak-file*) and creates
  task-table for use by other fns"
  []
  (clear-tasks!)
  (try
   (load-file (str *CWD* path-sep *default-cloak-file*))
   (catch java.io.FileNotFoundException e
     (error "Can't find cloak file" (str \" *default-cloak-file* \"))
     (throw e))
   (catch Exception e
     (error "Loading cloak file" (str \" *default-cloak-file* \") "failed.")
     (throw e))))

(defn run-tasks
  "Run given tasks. Aborts on first failed task."
  [kwords]
  (println "Running tasks:" (apply str (interpose " "(map str kwords))))
  (doseq [kw kwords]
    (when-not (contains? @*tasks* kw)
      (error "No such task:" kw)
      (throw (Exception. "Specified task is not defined."))))
  (doseq [kw kwords]
    (try
     (binding [*error-handler* task-error]
       (execute-task kw))
     (catch Exception e
       (error (.getMessage e))
       (throw e)))))

(defn print-desc
  "Prints task descriptions."
  [taskmap]
  (newline)
  (do
    (doseq [t (for [key (keys taskmap)]
                (assoc (@*tasks* key) :name key))]
      (print (format " %1$-16s" (if (keyword? (t :name))
                                  (name (t :name))
                                  (t :name))))
      (if (t :desc)
        (println (t :desc))
        (newline)))))

(defn run-program []
  (when *default-cloak-file*
    (load-tasks))                ;lets try to load the tasks from file
  (try
   (init-tasks)
   (catch Exception e
     (error "Error initializing tasks")
     (error (.getMessage e))
     (throw e)))
  ;; perform actions
  (try
   (cond *describe-only* (print-desc @*tasks*)
         (empty? *target-queue*) (run-tasks [*default-task*])
         :else (run-tasks *target-queue*))))

(def #^{:private true}
     cmd-line-opts {"-d" "describe tasks"
                    "-f taskfile" "use taskfile instad of CLOAK"
                    "-t" "run cloak, but don't execute actions (try)"
                    "-h" "print help"})

(defn print-help []
  (println " Cloak. A simple automation tool.\n")
  (doseq [[opt des] cmd-line-opts]
    (printf " %1$-15s %2$s\n" opt des)))

(defn print-usage []
  (let [bin (:bin @+settings+)]
    (printf "usage: %s [options] [task-name]\n" bin)
    (printf "try '%s -h' for more information.\n" bin)))

;; parses command line arguments
(defmulti parse-arg first)

(defmethod parse-arg "-d" [args]
  (binding [*describe-only* true]
    (parse-arg (next args))))

(defmethod parse-arg "-t" [args]
  (notification "The actions won't be performed (option -t supplied).")
  (when (some #{"-h" "-d"} (next args))
    (print-usage)
    (throw (Exception. "Wrong parameters.")))
  (binding [*try-only* true]
    (parse-arg (next args))))

(defmethod parse-arg "-h" [args]
  (if (or (next args) *describe-only*)
    (do
      (print-usage)
      (throw (Exception. "Wrong parameters.")))
    (print-help)))

(defmethod parse-arg "-f" [args]
  (when-not (second args)
    (print-usage)
    (throw (Exception. "Missing file argument (-f ...)")))
  (let [cfname (second args)
        rargs (nnext args)]
    (if (some #{"-h"} rargs)          ;those args don't make sense now
      (do
        (print-usage)
        (throw (Exception. "Wrong parameters.")))
      (binding [*default-cloak-file* cfname]
        (parse-arg rargs)))))

(defmethod parse-arg :default [args]
  (let [tnames (vec (map keyword args))] ;remaining params are task names
    (binding [*target-queue* tnames]
      (run-program))))

(defmethod parse-arg nil [args]
  (run-program))

(defn -main [& args]
  (try
   (parse-arg args)
   (catch Exception e
     (.printStackTrace e)
     (println "\nBuild failed.\n")
     (System/exit 1))))
