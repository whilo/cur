;;-----------------------------------------------------------------
;; cur.std.typeclass.registry-test
;;
;; Tests for the typeclass registry.
;;-----------------------------------------------------------------
(ns cur.std.typeclass.registry-test
  (:require [clojure.test :refer :all]
            [cur.std.typeclass.registry :as registry]))

(deftest registry-basic
  (registry/reset-registry!)
  ;; Registry should be empty
  (is (empty? (deref registry/registry)))
  ;; register a class
  (registry/register-class! 'C {:m1 :sig1 :m2 :sig2})
  (is (= {:methods {:m1 :sig1 :m2 :sig2} :instances {}}
         (get @registry/registry 'C)))
  ;; register instance
  (registry/register-instance! 'C 'T {:m1 inc :m2 dec})
  (is (= inc (registry/lookup-impl 'C 'T :m1)))
  (is (= dec (registry/lookup-impl 'C 'T :m2)))
  ;; unknown lookup
  (is (nil? (registry/lookup-impl 'C 'T :m3))))