;; Copyright (c) 2009 Krešimir Šojat. All rights reserved.  The use and
;; distribution terms for this software are covered by the Common
;; Public License 1.0 (http://www.opensource.org/licenses/cpl1.0.php)
;; which can be found in the file CPL.TXT at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns cloak.utils
  (:use clojure.set))

(defn throwf [& args]
  (throw (Exception. (apply format args))))

(defn tsort
  ([graph start]
    (println start)
    (let [hidden (gensym)
          graph  (assoc graph hidden start)
          visited (atom [])
          sort (fn sort [v back]
                 (cond
                   ; Detect cycles
                   (some #(= v %) back)
                     (throwf "Cycle detected: %s <-> %s" v (last back))

                   ; Check does node exists
                   (not (contains? graph v))
                     (throwf "Dependency %s for %s missing." (last back) v)

                   :else
                     (when-not (some #(= v %) @visited)
                       (let [ns (graph v)]
                         (if (not (empty? ns))
                           (let [back (conj back v)]
                             (doseq [n ns]
                               (sort n back)))))
                        (swap! visited conj v))))]
      (sort hidden [])
      (drop-last @visited)))

  ([graph]
    ; TODO: Fix this
    (let [start (difference
                  (set (keys graph)) (into #{} (mapcat (fn [[k v]] v) graph)))]
      (if (empty? start)
        (throwf "Graph is not a DAG.")
        (tsort graph start)))))
