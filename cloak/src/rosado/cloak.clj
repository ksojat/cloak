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
  (:import
    (java.lang System)
    (java.io File)
    (org.apache.commons.cli
      Option Options GnuParser HelpFormatter UnrecognizedOptionException))
  (:use rosado.cloak.main)
  (:gen-class))

(defn notice [& args]
  (println (apply str " NOTICE: " args)))

(defn error [& args]
  (println (apply str " ERROR: " args)))

; TODO: Move this to cloak.core
(def +settings+
  (atom {:logger   {:error error, :notice notice}
         :file     ["cloakfile" "cloakfile.clj"]
         :describe false
         :verbose  false
         :targets  [:default]
         :cwd      (System/getProperty "user.dir" "")
         :bin      (System/getProperty "cloak.bin" "cloak")
         :home     (System/getProperty "cloak.home" "")})); TODO: Default to ~/.cloak

(def cli-options
  (doto (Options.)
    (.addOption "h" "help"     false "Print help")
    (.addOption (doto (Option. "q" "queue" true "Add task to execution queue")
                  (.setArgs Option/UNLIMITED_VALUES)))
    (.addOption "d" "describe" false "Describe tasks")
    (.addOption "f" "file"     true  "Use taskfile instead of CLOAK")
    (.addOption "t" "try"      false "Run Cloak but don't execute any actions (try)")
    (.addOption "a" "ant"      true  "Generate Ant build facade")))

(defn cli-help []
  (.printHelp (HelpFormatter.) (:bin @+settings+) cli-options)
  (System/exit 0))

(defn cli-parse [command-line-arguments]
  (try
    (.parse (GnuParser.) cli-options (into-array command-line-arguments))
    (catch UnrecognizedOptionException e
      (cli-help))))

(defn task-error
  "Error handler function used for task failures."
  [& args]
  (println (apply str " [" (name *current-task*) " ]" args)))

(defn- load-tasks
  "Loads tasks from input file and creates task-table for use by other fns"
  [file]
  (clear-tasks!)
  (try
    (load-file file)
    (catch java.io.FileNotFoundException e
      (error (format "Can't find cloak file \"%s\"." file))
      (throw e))
    (catch Exception e
      (error (format "Loading cloak file \"%s\" failed." file))
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
     (binding [*error-handler* task-error]; TODO: Why i can't just send in project settings?
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
      (printf " %1$-16s" (if (keyword? (t :name)) (name (t :name)) (t :name)))
      (if (t :desc)
        (println (t :desc))
        (newline)))))

(defn find-cloakfile [{file :file cwd :cwd}]
  (first
    (filter #(.exists %)
      (map #(File. (File. cwd) %) file))))

(defn run-program [{:keys [describe targets] :as settings}]
  (if-let [file (find-cloakfile settings)]
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
      (print-desc @*tasks*)
      (run-tasks targets))))

(defn generate-ant-facade [file]
  (println "This option will be implemented later."))

(defn -main [& args]
  (let [cmd (cli-parse (or args (list "")))
        has-option? #(.hasOption cmd %)
        get-option  #(.getOptionValue cmd %)]

    (when (has-option? "help")
      (cli-help))

    (when (has-option? "ant")
      (generate-ant-facade (get-option "ant"))
      (System/exit 0))

    (when (has-option? "describe")
      (swap! +settings+ assoc :describe true))

    (when (has-option? "file")
      (swap! +settings+ assoc :file [(get-option "file")]))

    (when (has-option? "try")
      (swap! +settings+ assoc :try true))

    (when (has-option? "verbose")
      (swap! +settings+ assoc :verbose true))

    (when (has-option? "queue")
      (swap! +settings+ assoc :targets
        (map keyword (seq (.getOptionValues cmd "queue")))))

    (run-program @+settings+)))

;; Standard run
(when (and (not *compile-files*) (System/getProperty "cloak.runmain"))
  (-main *command-line-args*))
