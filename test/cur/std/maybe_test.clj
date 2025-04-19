;;-----------------------------------------------------------------
;; cur.std.maybe-test
;;
;; Tests for the Maybe module in the standard library.
;;-----------------------------------------------------------------
(ns cur.std.maybe-test
  "Tests for the Maybe module in the standard library."
  (:require [clojure.test :refer :all]
            [cur.std.maybe :as maybe]
            [cur.curnel.parser :refer [parse-term]]
            [cur.curnel.checker :refer [type-of]]
            [cur.curnel.ast :as ast])
  (:import [cur.curnel.ast App Elim]))

(deftest maybe-constructors-have-correct-types
  (let [ctx (maybe/register {})]
    (is (= (parse-term '(Pi [A (Type 0)] (Maybe A)))
           (type-of ctx (parse-term 'none))))
    (is (= (parse-term '(Pi [A (Type 0)] (Pi [a A] (Maybe A))))
           (type-of ctx (parse-term 'some))))))

(deftest maybe-elim-type
  (let [ctx      (maybe/register {})
        motive   (ast/->Lambda 'mx
                               (ast/->App (ast/->Var 'Maybe) (ast/->Var 'A))
                               (ast/->Var 'A))
        m-none   (ast/->Var 'none)
        m-some   (ast/->Var 'some)
        target   (ast/->Var 'none)
        elim     (maybe/elim motive m-none m-some target)
        ty       (type-of ctx elim)]
    (is (instance? App ty))
    (is (= motive (:fn ty)))
    (is (= target (:arg ty)))))