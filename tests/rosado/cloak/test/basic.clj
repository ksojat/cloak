(ns rosado.cloak.test.basic
  (:use unittest.core)
  (:require [rosado.cloak :as cloak]))

;; testing command line parsing

(defn opt-f []
  (cloak/parse-arg ["-f"]))

(defn opt-t-h []
  (cloak/parse-arg ["-t" "-h"]))

(defn opt-h-t []
  (cloak/parse-arg ["-h" "-t"]))

(defn opt-h-word []
  (cloak/parse-arg ["-h" "some-word"]))

(deftest "Command line options"
  (assert-throws (opt-f))
  (assert-throws (opt-t-h))
  (assert-throws (opt-h-t))
  (assert-throws (opt-h-word)))

(deftest "Simple task"
  (assert-throws (cloak/parse-arg ["a"]))
  (assert-throws (cloak/parse-arg ["nonexisiting-task"])))

(deftest "Circular dependencies"
  (assert-throws (cloak/parse-arg ["-f" "tests/rosado/cloak/test/CIRCULAR" "a"]))
  (assert-throws (cloak/parse-arg ["-f" "tests/rosado/cloak/test/CIRCULAR" "b"])))

(cloak/parse-arg ["-f" "tests/rosado/cloak/test/ONCE" "a"])

(deftest "Run only once"
  (assert= 1 @cloak.tests.once/*counter-a*))

(cloak/parse-arg ["-f" "tests/rosado/cloak/test/ONCE" "b" "b"])

(deftest "Run only once"
  (assert= 1 @cloak.tests.once/*counter-b*))

