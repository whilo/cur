;;-----------------------------------------------------------------
;; cur.std.equality-test
;;
;; Tests for the equality module in the standard library.
;;-----------------------------------------------------------------
(ns cur.std.equality-test
  "Tests for the equality module in the standard library."
  (:require [clojure.test :refer :all]
            [cur.std.equality :as eq]
            [cur.curnel.parser :refer [parse-term]]
            [cur.curnel.checker :refer [type-of]]
            [cur.curnel.ast :as ast])
  (:import [cur.curnel.ast App Elim]))

(deftest eq-constructors-have-correct-types
  (let [ctx (eq/register {})]
    (is (= (parse-term '(Pi [A (Type 0)]
                            (Pi [x A]
                                (== A x x))))
           (type-of ctx (parse-term 'refl))))))

(deftest eq-elim-type
  (let [ctx      (eq/register {})
        motive   (ast/->Lambda 'p (parse-term '(== A x x)) (ast/->Var 'A))
        m-refl   (ast/->Var 'refl)
        target   (ast/->Var 'refl)
        elim     (eq/elim motive m-refl target)
        ty       (type-of ctx elim)]
    (is (instance? App ty))
    (is (= motive (:fn ty)))
    (is (= target (:arg ty)))))