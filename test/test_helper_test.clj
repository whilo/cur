;; Ensure core namespaces are loaded before running tests
(ns test-helper-test
  (:require [clojure.test :refer :all]
            [cur.curnel.checker]
            [cur.curnel.ast]
            [cur.curnel.parser]))