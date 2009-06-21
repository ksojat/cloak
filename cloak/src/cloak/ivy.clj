;; Copyright (c) 2009 Krešimir Šojat. All rights reserved.  The use and
;; distribution terms for this software are covered by the Common
;; Public License 1.0 (http://www.opensource.org/licenses/cpl1.0.php)
;; which can be found in the file CPL.TXT at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns cloak.ivy
  (:import
    (org.apache.ivy.core.settings IvySettings XmlSettingsParser))
  (:require [cloak.core :as core]))

; TODO: Maybe add some kind of property convertor.

(defn create-settings []
  (IvySettings.))

(core/on ::core/build-created
  (fn [build]
    (swap! build assoc ::settings (create-settings))))

; Mirror all cloak build settings to IvySettings
(core/on ::core/property
  (fn [build key value]
    (when (and key value)
      (.setVariable (::settings @build) key value))))

;(defn resolver [name specs]
;  

;(defn settings [s]
;  ; Add resolvers
;  (doseq [[name specs] (:resolvers s)]
;    (add-resolver name specs))

;  (println "You are trying to set ivy settings."))

(defn load-settings-xml [#^java.net.URL url]
  (.parse (XmlSettingsParser. (::settings @core/*build*)) url))
