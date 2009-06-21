;; simple build system in Clojure
;; Roland Sadowski [szabla gmail com]

;; Copyright (c) 2008 Roland Sadowski. All rights reserved.  The use and
;; distribution terms for this software are covered by the Common
;; Public License 1.0 (http://www.opensource.org/licenses/cpl1.0.php)
;; which can be found in the file CPL.TXT at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


(ns cloak.actions
  (:import
    (clojure.lang RT)
    (java.util Collections Arrays)
    (java.io File FileInputStream FileOutputStream)
    (org.apache.commons.io IOUtils FileUtils)))

;;
;; Filesystem related actions.
;;

(alter-meta! *ns* assoc-in [:groups :filesystem]
  {:name "Filesystem"
   :desc "Manage files and directories on filesystem."})

(def #^{:private true} p-info)
(def #^{:private true} *p*)

(defn- remember-pi
  "Utility fn for mutating a var in run-process."
  [kw val]
  (set! p-info (assoc p-info kw val)))

(defn run-command [#^String cmd-str]
  (let [params (java.util.ArrayList.)
		pb (ProcessBuilder. (Arrays/asList (.split cmd-str " ")))]
	(.redirectErrorStream pb true)
	(try
	 (binding [p-info {} *p* (.start pb)]
	   (remember-pi :in-stream (.getInputStream *p*))
	   (.waitFor *p*)
       (remember-pi :output (IOUtils/toString (p-info :in-stream)))
	   (.destroy *p*)
	   (if (not= 0 (.exitValue *p*))
		 (let []
		   (println "Error executing command: " cmd-str)
		   (print (:output p-info))
		   :fail)
		 (let []
		   (print (:output p-info))
		   :ok)))
	 (catch java.io.IOException ex :fail))))

(defn
  #^{:doc    "Performs a shell command."
     :action true
     :group  :filesystem}
  sh
  ([command]
     (run-command command))
  ([command flag]
     (cond (= flag :dofail)
             (when (= :fail (sh command))
               (throw (Exception. "shell command failed.")))
           :else (throw (IllegalArgumentException. (format "sh: unsupported flag %s" (str flag)))))))

(defn
  #^{:doc    "Copies a file"
     :action true
     :group  :filesystem}
  copy [src-file dest-file]
  (FileUtils/copyFile (File. src-file) (File. dest-file)))

(defn
  #^{:doc    "Copies a file to destionation directory."
    :action true
    :group  :filesystem}
  copy-to [src-file dest-dir]
  (FileUtils/copyFileToDirectory (File. src-file) (File. dest-dir)))

(defn
  #^{:doc    "Moves a file"
    :action true
    :group  :filesystem}
  move [src target]
  (FileUtils/moveFile (File. src) (File. target)))

(defn
  #^{:doc    "Creates directories, including necessary parent dirs."
     :action true
     :group  :filesystem}
  mkdir [& dirs]
  (doseq [dir dirs] (.mkdirs (File. dir))))

; TODO: Do i need this?
(defn exists? [fname] (.exists (File. fname)))

(defn
  #^{:doc    "Remove file."
     :action true
     :group  :filesystem}
  rm [file]
  (.delete file))

(defn
  #^{:doc   "Remove directory recursively (like 'rm -r dirname')."
    :action true
    :group  :filesystem}
  rmdir [dir]
  (FileUtils/deleteDirectory (File. dir)))

;;
;; Clojure related actions.
;;

(alter-meta! *ns* assoc-in [:groups :clojure]
  {:name "Clojure"
   :desc "Actions used to manipulate clojure files."})

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
  (filter clojure-file? (mapcat file-seq src-dirs)))

(defn project-ns [src-dirs]
  (filter (complement nil?)
    (mapcat ns-decl (project-files src-dirs))))

(defn
  #^{:doc "Compile all Clojure files from source directories to destionation
          directory."
    :action true
    :group  :clojure}
  clojurec [src-dirs dest-dir]
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
  (resource-as-string (format "cloak/repl.%s" type)))

(defn replace-tokens [s tokens-map]
  (reduce
    (fn [s [k v]]
      (.replaceAll s k (or v k)))
    s tokens-map))

(defn string-to-file [#^File f #^String s]
  (FileUtils/writeStringToFile f s))

(defn clojure-repl [#^File file type {cp :cp, completitions :completitions}]
  (if (#{:standard :rlwrap :jline} type)
    (string-to-file file
      (replace-tokens (repl-as-string (name type))
        {"%PROJECT_COMPLETITIONS%" completitions
         "%PROJECT_CLASSPATH%"     cp}))
    (throw (Exception. (str "Unknown repl type: " type)))))

(defn project-publics [src-dirs]
  (filter (complement nil?)
    (mapcat
      (fn [n]
        (when-let [n (find-ns n)]
          (keys (ns-publics n))))
      (project-ns src-dirs))))

(def standard-publics
  (mapcat
    #(keys (ns-publics (find-ns %)))
    '(clojure.core clojure.set clojure.xml clojure.zip)))

(defn clojure-completitions [src-dirs #^File dest-file]
  ; Add dirs to classpath.
  (doseq [d src-dirs]
    (add-classpath (.toURL d)))

  (let [publics (concat standard-publics (project-publics src-dirs))]
    (string-to-file dest-file
      (apply str (interleave publics (repeat "\n"))))))
