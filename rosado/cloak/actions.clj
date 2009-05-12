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
    (java.io File FileInputStream FileOutputStream)
    (org.apache.commons.io FileUtils)))

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

