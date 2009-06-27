;; Copyright (c) 2009 Krešimir Šojat. All rights reserved.  The use and
;; distribution terms for this software are covered by the Common
;; Public License 1.0 (http://www.opensource.org/licenses/cpl1.0.php)
;; which can be found in the file CPL.TXT at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns cloak.tasks
  (:use
    [cloak.core :only [*collector* resolver deftask create-task]]))

(deftask ::Task [_ name deps f]
  (with-meta
    {:name (if (list? name) name (list name)), :resolve (resolver deps), :f f}
    {:type ::Task}))

(defmacro task [name deps props & body]
  `(*collector*
     (create-task ::Task '~name ~deps (fn [] ~@body))))

;; TODO: Error, no name
(deftask ::Clean [filesets]
  (with-meta
    {:name name, :resolve (resolver #{}), :f (fn [] (println "Clean"))}
    {:type ::Clean}))

; TODO: Define clean macro

;; TODO: Error, no name
(deftask ::Package [filesets]
  (with-meta
    {:name name, :resolve (resolver #{}), :f (fn [] (println "Package"))}
    {:type ::Package}))

; TODO: Define package macro

; TODO: Define tests tasks, dist and repository update tasks
