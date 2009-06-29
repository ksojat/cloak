;; Copyright (c) 2009 Krešimir Šojat. All rights reserved.  The use and
;; distribution terms for this software are covered by the Common
;; Public License 1.0 (http://www.opensource.org/licenses/cpl1.0.php)
;; which can be found in the file CPL.TXT at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns cloak.main
  (:import
    (java.lang System)
    (org.apache.commons.cli
      Option Options GnuParser HelpFormatter UnrecognizedOptionException))
  (:require
    (cloak [core :as core] ant ivy logger))
  (:gen-class))

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
    (let [ps (System/getProperties)]
      (doseq [[k v] (.getOptionProperties cmd "D")] (.setProperty ps k v))
      (System/setProperties ps))

    (let [build (core/create-build settings)]
      (core/start-build! build))))

;; Standard run (no need for aot)
(when (and (not *compile-files*) (System/getProperty "cloak.runmain"))
  (apply -main *command-line-args*))
