;; Copyright (c) 2009 Krešimir Šojat. All rights reserved.  The use and
;; distribution terms for this software are covered by the Common
;; Public License 1.0 (http://www.opensource.org/licenses/cpl1.0.php)
;; which can be found in the file CPL.TXT at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns cloak.ant
  (:require [cloak.core :as core])
  (:import (java.io File)
           (org.apache.tools.ant Project NoBannerLogger UnknownElement)))

; TODO: Maybe add some kind of property convertor.

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
;(def +project+
;  (standard-project (File. (java.lang.System/getProperty "user.dir"))))

; TODO: Copy all current properties if any.
(core/on ::core/build-created
  (fn [build]
    (let [cwd (:cwd @build)]
      (swap! build assoc ::project (standard-project (File. cwd))))))

; Mirror Cloak properties to Ant project.
(core/on ::core/propery
  (fn [build key value]
    (when (and key value)
      (.setProperty (::project @build) key value))))

(defn unknown-element [el-name & specs]
  (let [ue (doto (UnknownElement. (name el-name))
             ;(.setProject +project+))
             (.setProject (::project @core/*build*)))
        w  (.getRuntimeConfigurableWrapper ue)
        [ps cs] (if (map? (first specs))
                  [(first specs) (next specs)] [nil specs])]
    (doseq [[k v] ps]
      (.setAttribute w (name k) v))

    (doseq [c cs]
      (let [x (apply unknown-element c)]
        (.addChild ue x)
        (.addChild w (.getRuntimeConfigurableWrapper x))))

    (.maybeConfigure ue)
    ue))

(defn ant [& args]
  (let [t (apply unknown-element args)]
    (.. t getRealThing execute)))
