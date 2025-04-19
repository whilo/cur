(ns cur.core
  (:require [clojure.test :refer [deftest]]))

(defmacro defcur [name & body]
  `(deftest ~name ~@body))
