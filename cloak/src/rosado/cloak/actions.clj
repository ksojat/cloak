;; simple build system in Clojure
;; Roland Sadowski [szabla gmail com]

;; Copyright (c) 2008 Roland Sadowski. All rights reserved.  The use and
;; distribution terms for this software are covered by the Common
;; Public License 1.0 (http://www.opensource.org/licenses/cpl1.0.php)
;; which can be found in the file CPL.TXT at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


(ns rosado.cloak.actions
  (:use rosado.utils)
  (:import
    (clojure.lang RT)
    (java.io File FileInputStream FileOutputStream)
    (org.apache.commons.io IOUtils FileUtils)))

(defn sh
  "Performs a shell command."
  ([command]
     (run-command command))
  ([command flag]
     (cond (= flag :dofail)
             (when (= :fail (sh command))
               (throw (Exception. "shell command failed.")))
           :else (throw (IllegalArgumentException. (format "sh: unsupported flag %s" (str flag)))))))

(defn copy
  "Copies a file"
  [src-file dest-file]
  (FileUtils/copyFile (File. src-file) (File. dest-file)))

(defn copy-to
  "Copies a file to destionation directory."
  [src-file dest-dir]
  (FileUtils/copyFileToDirectory (File. src-file) (File. dest-dir)))

(defn move
  "Moves a file"
  [src target]
  (FileUtils/moveFile (File. src) (File. target)))

(defn mkdir
  "Creates directories, including necessary parent dirs."
  [& dirs]
  (doseq [dir dirs] (.mkdirs (File. dir))))

; TODO: Do i need this?
(defn exists? [fname] (.exists (File. fname)))

(defn rm [file]
  "Remove file."
  (.delete file))

(defn rmdir [dir]
  "Remove directory recursively (like 'rm -r dirname')."
  (FileUtils/deleteDirectory (File. dir)))

(defn ns-decl [file]
  (with-open [f (java.io.FileReader. file)
              b (java.io.BufferedReader. f)
              p (java.io.PushbackReader. b)]
    (reduce
      (fn [n [x & [y & _]]]
        (if (= x 'ns) (conj n y) n))
      []
      (take-while (complement nil?) (repeatedly #(read p false nil))))))

(defn clojure-file? [file]
  (.endsWith (.getName file) ".clj"))

(defn project-files [src-dirs]
  (println src-dirs)
  (filter clojure-file? (mapcat file-seq src-dirs)))

(defn project-ns [src-dirs]
  (mapcat ns-decl (project-files src-dirs)))

(defn clojurec [src-dirs dest-dir]
  ; Create destination directory.
  (.mkdirs dest-dir)

  ; Add dirs to classpath.
  (doseq [d (conj src-dirs dest-dir)]
    (add-classpath (.toURL d)))

  ; Compile all namespaces inside the project directories.
  (binding [*compile-path* dest-dir]
    (doseq [n (project-ns src-dirs)] (compile n))))

(defn resource-url [res]
  (when res (.getResource (RT/baseLoader) res)))

(defn resource-as-stream [res]
  (when res (.getResourceAsStream (RT/baseLoader) res)))

(defn resource-as-string [res]
  (IOUtils/toString (resource-as-stream res)))

(defn resource-as-file [res]
  (when-let [res (resource-url res)] (.getFile res)))

(defn repl-as-string [type]
  (resource-as-string (format "rosado/cloak/repl.%s" type)))

(defn clojure-repl [#^File file type {cp :cp, completitions :completitions}]
  (if (#{:standard :rlwrap :jline} type)
    (FileUtils/writeStringToFile file
      (-> (repl-as-string (name type))
        (.replaceAll "%PROJECT_COMPLETITIONS%" completitions)
        (.replaceAll "%PROJECT_CLASSPATH%"     cp)))
    (throw (Exception. (str "Unknown repl type: " type)))))

;(defn clojure-repl [file type {cp :cp, completitions :completitions}]
;  (condp = type
;    :standard
;      (let [s (IOUtils/toString (resource-as-stream "rosado/cloak/repl.standard"))
;            repl (-> s
;                   (.replaceAll "%PROJECT_COMPLETITIONS%" completitions)
;                   (.replaceAll "%PROJECT_CLASSPATH%"     cp))]
;        (FileUtils/writeStringToFile file repl))

;    :rlwrap
;      (let [s (IOUtils/toString (resource-as-stream "rosado/cloak/repl.rlwrap"))
;            repl (-> s
;                  (.replaceAll "%PROJECT_COMPLETITIONS%" completitions)
;                  (.replaceAll "%PROJECT_CLASSPATH%"     cp))]
;        (FileUtils/writeStringToFile file repl))
;
;    :jline
;      (println "Creating jline based repl.")

    ; Fail
;    (throw (Exception. (str "Unknown repl type: " type)))))
