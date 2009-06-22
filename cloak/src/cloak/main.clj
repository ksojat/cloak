;; simple build system in Clojure
;; Roland Sadowski [szabla gmail com] http://www.haltingproblem.net

;; Copyright (c) 2008 Roland Sadowski. All rights reserved.  The use and
;; distribution terms for this software are covered by the Common
;; Public License 1.0 (http://www.opensource.org/licenses/cpl1.0.php)
;; which can be found in the file CPL.TXT at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns cloak.main
  (:import
    (java.lang System)
    (java.io File)
    (org.apache.commons.cli
      Option Options GnuParser HelpFormatter UnrecognizedOptionException))
  (:require [cloak.core :as core])
  (:gen-class))

;; Load extensions so they can register there listeners.
(require 'cloak.ant
         'cloak.ivy
         'cloak.stats)

(defn notice [& args]
  (println (apply str " NOTICE: " args)))

(defn error [& args]
  (println (apply str " ERROR: " args)))

(def cli-options
  (doto (Options.)
    (.addOption "h" "help"     false "Print help")
    (.addOption nil "version"  false "Show version information")
    (.addOption nil "verbose"  false "Show verbose output")
    (.addOption (doto (Option. "q" "queue" true "Add task to execution queue")
                  (.setArgs Option/UNLIMITED_VALUES)))
    (.addOption (doto (Option. "s" "skip" true "Skip tasks")
                  (.setArgs Option/UNLIMITED_VALUES)))
    (.addOption "d" "describe" false "Describe tasks")
    (.addOption "f" "file"     true  "Use taskfile instead of CLOAK")
    (.addOption "t" "try"      false "Run Cloak but don't execute any actions (try)")
    (.addOption (doto (Option. "D" nil true "Set build propery")
                  (.setArgs 2)
                  (.setArgName "<property>=<value>")
                  (.setValueSeparator \=)))))

(defn cli-help []
  (.printHelp (HelpFormatter.)
    (System/getProperty "cloak.bin" "cloak") cli-options)
  (System/exit 0))

(defn cli-parse [command-line-arguments]
  (try
    (.parse (GnuParser.) cli-options (into-array command-line-arguments))
    (catch UnrecognizedOptionException e
      (cli-help))))

(defn show-version []
  (println (format "Cloak v%s" (:version core/info)))
  (System/exit 0))

#_(defn task-error
  "Error handler function used for task failures."
  [& args]
  (println (apply str " [" (name *current-task*) " ]" args)))

; TODO: Ovdje rebindati *build*
(defn- load-tasks
  "Loads tasks from input file and creates task-table for use by other fns"
  [file]
  (try
    (load-file file)
    (catch java.io.FileNotFoundException e
      (error (format "Can't find cloak file \"%s\"." file))
      (throw e))
    (catch Exception e
      (error (format "Loading cloak file \"%s\" failed." file))
      (throw e))))

(defn execute-task [& _]
  (println "Placeholder, remove later"))

(defn init-tasks [& _]
  (println "Placeholder, remove later"))

(defn run-tasks
  "Run given tasks. Aborts on first failed task."
  [kwords]
  (println "Running tasks:" (apply str (interpose " "(map str kwords))))
  (doseq [kw kwords]
    (when-not (contains? (:tasks @core/*build*) kw)
      (error "No such task:" kw)
      (throw (Exception. "Specified task is not defined."))))
  (doseq [kw kwords]
    (try
     (execute-task kw)
     (catch Exception e
       (error (.getMessage e))
       (throw e)))))

(defn print-desc
  "Prints task descriptions."
  [taskmap]
  (newline)
  (do
    (doseq [t (for [key (keys taskmap)]
                (assoc ((:tasks @core/*build*) key) :name key))]
      (printf " %1$-16s" (if (keyword? (t :name)) (name (t :name)) (t :name)))
      (if (t :desc)
        (println (t :desc))
        (newline)))))

(defn find-cloakfile [{file :file cwd :cwd}]
  (first
    (filter #(.exists %)
      (map #(File. (File. cwd) %) file))))

(defn run-program [{:keys [describe targets] :as build}]
  (println build)
  (if-let [file (find-cloakfile build)]
    (load-tasks (.getAbsolutePath file))
    (do
      (println "Can't find Cloak file.")
      (System/exit 1)))

  (try
    (init-tasks)
    (catch Exception e
      (error "Error initializing tasks")
      (error (.getMessage e))
      (throw e)))
  (try
    (if describe
      (print-desc (:tasks @core/*build*))
      (run-tasks targets))))


(defn -main [& args]
  (let [cmd         (cli-parse (or args (list "")))
        has-option? #(.hasOption cmd %)
        get-option  #(.getOptionValue cmd %)

        settings  ((comp
                     #(if (has-option? "describe")
                       (assoc % :describe true)
                       %)

                     #(if (has-option? "file")
                       (assoc % :file [(get-option "file")])
                       %)

                     #(if (has-option? "try")
                       (assoc % :try true)
                       %)

                     #(if (has-option? "verbose")
                       (assoc % :verbose true)
                       %)

                     #(if (has-option? "queue")
                       (assoc % :targets
                         (map keyword (seq (.getOptionValues cmd "queue"))))
                       %))
                   {})]

    (when (has-option? "help")
      (cli-help))

    (when (has-option? "version")
      (show-version))

    ; Collect all properties.
    (System/setProperties
      (.putAll (System/getProperties) (.getOptionProperties cmd "D")))

    (let [build (core/create-build settings)]
      ; Collect all properties
      (binding [core/*build* build]
        (doseq [[k v] (.getOptionProperties cmd "D")]
          (core/property k v))

          (core/emmit build ::core/build-started)
          (Thread/sleep 1000)
          (run-program @build); TODO: Rename this to start-build
          (core/emmit build ::core/build-finished))

      ;(println "Build created")
      (println @build))))

;; Standard run
(when (and (not *compile-files*) (System/getProperty "cloak.runmain"))
  (-main *command-line-args*))
