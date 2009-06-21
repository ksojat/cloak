;; Copyright (c) 2009 Krešimir Šojat. All rights reserved.  The use and
;; distribution terms for this software are covered by the Common
;; Public License 1.0 (http://www.opensource.org/licenses/cpl1.0.php)
;; which can be found in the file CPL.TXT at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns rosado.cloak.fragments.base
  (:import
    (java.lang System)
    (java.io File)))

(defn base-fn [base-dir]
  #(File. base-dir (str %)))

; TODO: First binding is group name
(defmacro fragmet [group & body]
  `(println "wee fragment"))

(defn load-fragment [& fs]
  nil)

(fragment :ProjectBase [] [group]
  ; Calculate project base directory.
  (property :BaseDir
    (File. (System/getProperty "user.home")))

  ; Create a module base directory generator.
  (property :BaseDirFn [:BaseDir] [base-dir]
    (base-fn base-dir)))

(fragment :ModuleBase [:BaseDirFn] [group make-base]
  ; Calculate module base directory.
  (property :BaseDir
    (make-base group))

  ; Create a submodule base directory generator.
  (property :BaseDirFn [:BaseDir] [base-dir]
    (base-fn base-dir)))
