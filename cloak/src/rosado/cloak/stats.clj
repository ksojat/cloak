;; Copyright (c) 2009 Krešimir Šojat. All rights reserved.  The use and
;; distribution terms for this software are covered by the Common
;; Public License 1.0 (http://www.opensource.org/licenses/cpl1.0.php)
;; which can be found in the file CPL.TXT at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns rosado.cloak.stats
  (:import (java.lang System))
  (:require [rosado.cloak.core :as core]))

(defn now []
  (System/currentTimeMillis))

(defn elapsed-time [millis]
  (let [sec (/ millis 1000)
        min (/ sec 60)]
    (format "%d minutes, %d seconds" (int min) (int (mod sec 60)))))

(defn print-build-duration [{{start ::started finish ::finished} ::build}]
  (println "Build time: " (elapsed-time (- finish start))))

(core/on ::core/build-started
  (fn [build]
    (swap! build assoc-in [::build ::started] (now))))

(core/on ::core/build-finished
  (fn [build]
    (swap! build assoc-in [::build ::finished] (now))
    (print-build-duration @build)))

(core/on ::core/build-failed
  (fn [build]
    (swap! build assoc ::build {::finished (now)})
    (print-build-duration @build)))

;(core/on ::core/task-started
;  (fn [build name]
;    (swap! build assoc ::task {name {:started (now)}})))

;(core/on ::core/task-finished
;  (fn [build & _]
;    (println "Task finished")))

;(core/on ::core/task-failed
;  (fn [build & _]
;    (println "Task failed")))
