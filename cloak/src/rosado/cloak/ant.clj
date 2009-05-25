;; Copyright (c) 2009 Krešimir Šojat. All rights reserved.  The use and
;; distribution terms for this software are covered by the Common
;; Public License 1.0 (http://www.opensource.org/licenses/cpl1.0.php)
;; which can be found in the file CPL.TXT at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns rosado.cloak.ant
  (:import (clojure.lang Reflector)
           (java.io File)
           (java.beans Introspector)
           (org.apache.tools.ant Project NoBannerLogger)))

(defn standard-logger []
  (doto (NoBannerLogger.)
    (.setMessageOutputLevel Project/MSG_INFO)
    (.setOutputPrintStream  System/out)
    (.setErrorPrintStream   System/out)))

(defn standard-project [base-dir]
  (doto (Project.)
    (.init)
    (.setBaseDir base-dir)
    (.addBuildListener (standard-logger))))

; TODO: Register this standard-project thing to +settings+
(def +project+
  (standard-project (File. (java.lang.System/getProperty "user.home"))))

(defn set-property [task key value]
  (let [w (.getWriteMethod
            (first
              (filter
                #(= (name key) (.getName %))
                (.getPropertyDescriptors (Introspector/getBeanInfo (class task))))))]
    (Reflector/invokeInstanceMethod task (.getName w) (into-array [value]))))

(defn ant [task & specs]
  (let [t  (.createTask +project+ (name task))
        ps (when (map? (first specs)) (first specs))]
    (.init t)
    (.setProject t +project+)
    (doseq [[k v] ps]
      (set-property t k v))
    (.execute t)))
