;;-----------------------------------------------------------------
;; cur.std.bool-test
;;
;; Tests for the Bool module in the standard library.
;;-----------------------------------------------------------------
(ns cur.std.bool-test
  "Tests for the Bool module in the standard library."
  (:require [clojure.test :refer :all]
            [cur.std.bool :as bool]
            [cur.curnel.checker :refer [type-of]]
            [cur.curnel.ast :as ast])
  (:import [cur.curnel.ast App Elim]))

(deftest bool-constructors-have-correct-types
  (let [ctx (bool/register {})]
    (is (= (ast/->Var 'Bool) (type-of ctx (ast/->Var 'True))))
    (is (= (ast/->Var 'Bool) (type-of ctx (ast/->Var 'False))))))

(deftest bool-elim-type
  (let [ctx       (bool/register {})
        motive    (ast/->Lambda 'b (ast/->Var 'Bool) (ast/->Var 'Bool))
        m-true    (ast/->Var 'True)
        m-false   (ast/->Var 'False)
        target    (ast/->Var 'True)
        elim      (bool/elim motive m-true m-false target)
        ty        (type-of ctx elim)]
    (is (instance? App ty))
    (is (= motive (:fn ty)))
    (is (= target (:arg ty)))))