;; Copyright (c) 2009 Krešimir Šojat. All rights reserved.  The use and
;; distribution terms for this software are covered by the Common
;; Public License 1.0 (http://www.opensource.org/licenses/cpl1.0.php)
;; which can be found in the file CPL.TXT at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns rosado.cloak.core
  (:use clojure.set))

(def
  #^{:doc
      "Set of global event listeners. Every listener will be added to
       newly created build automaticaly and to every already created
       build.
       If listener is removed from global listeners it will be removed
       from all active builds too."}
  global-listeners (atom {}))

(defn on
  ([event callback-fn]
    (swap! global-listeners update-in [event] #(conj (set %1) %2) callback-fn))

  ([build event callback-fn]
    (swap! build update-in [:listeners event] #(conj (set %1) %2) callback-fn)))

; Holds current active build.
(declare *build*)

(defn trigger [event & args]
  (let [listeners (get-in @*build* [:listeners event])]
    (doseq [l listeners]
      (apply l *build* args))))

(defn create-build [defaults]
  (let [build (atom (merge defaults {:listeners @global-listeners}))]

    ; Update build specific listeners if global-listener changes.
    (add-watch global-listeners (gensym "build__")
      (fn [_ _ ov nv]
        (let [ov      (merge ov (into {} (map (fn [x] [x #{}]) (keys nv))))
              removed (merge-with difference ov nv)
              added   (merge-with difference nv ov)
              updater (comp #(merge-with difference % removed)
                            #(merge-with union % added))]
          (swap! build update-in [:listeners] updater))))

    ; Notify all listeners that build was initialized.
    (binding [*build* build] (trigger ::init))

    build))

(defn run-build [build]
  (binding [*build* build]
    (trigger ::started)

    (println "Build is starting")))
