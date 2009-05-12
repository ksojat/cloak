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

;(defn copy
;  "Copies a file"
;  [src target]
;  (let [src (File. src) t (File. target)
;        target (if (.isDirectory t)
;                 (File. target (.getName src))
;                 t)]
;    (when-not (.exists src)
;      (throw (java.io.FileNotFoundException. (format "copy: Source file not found: %s" src))))
;    (try
;     (let [in (FileInputStream. src)
;           out (FileOutputStream. target)
;           buff (make-array Byte/TYPE 4096)]
;       (loop [nbytes (.read in buff)]
;         (when (not= nbytes -1)
;           (.write out buff 0 nbytes)
;           (recur (.read in buff)))))
;     (finally
;      (try
;       (.close src)
;       (.close target)
;       (catch Exception e nil))))
;    :ok))
(defn copy
  "Copies a file"
  [src dest]
  (FileUtils/copyFile (File. src) (File. dest)))

(defn move
  "Moves a file"
  [src target]
  (let [src (File. src) target (File. target)]
    (.renameTo src target)))

(defn mkdir
  "Creates directories, including necessary parent dirs."
  [& dirs]
  (doseq [dir dirs] (.mkdirs (File. dir))))

(defn exists? [fname] (.exists (File. fname)))

; TODO: Split this to rm and rmdir, never do recursive delete with out any extra flags.!
;(defn rm
;  "Removes a file or direcory (like 'rm -r dirname')."
;  [fname]
;  (io/delete fname))
