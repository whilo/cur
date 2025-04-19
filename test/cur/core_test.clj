(ns cur.core-test
  (:require [clojure.test :refer [deftest is]]
            [cur.core :refer [defcur]]))

(defcur plus-one
  (is (= (+ 1 1) 2)))
