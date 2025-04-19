;;-----------------------------------------------------------------
;; cur.std.typeclass.macros-test
;;
;; Tests for the typeclass macros.
;;-----------------------------------------------------------------
(ns cur.std.typeclass.macros-test
  (:require [clojure.test :refer :all]
            [cur.std.typeclass.registry :as registry]
            [cur.std.typeclass.macros :refer [deftypeclass definstance]]))

(deftest macros-basic
  (registry/reset-registry!)
  ;; Define a class
  (deftypeclass MyClass {:foo :SigFoo})
  (is (contains? @registry/registry 'MyClass))
  ;; Define an instance
  (definstance MyClass MyType {:foo (fn [x] (str "foo:" x))})
  (let [f (registry/lookup-impl 'MyClass 'MyType :foo)]
    (is (fn? f))
    (is (= "foo:42" (f 42)))))