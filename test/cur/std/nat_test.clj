;;-----------------------------------------------------------------
;; cur.std.nat-test
;;
;; Tests for the Nat module in the standard library.
;;-----------------------------------------------------------------
(ns cur.std.nat-test
  "Tests for the Nat module in the standard library."
  (:require [clojure.test :refer :all]
            [cur.std.nat :as nat]
            [cur.curnel.checker :refer [type-of nf equal-terms?]]
            [cur.curnel.parser :refer [parse-term]]
            [cur.curnel.ast :as ast])
  (:import [cur.curnel.ast App Elim]))

(deftest nat-constructors-have-correct-types
  (let [ctx (nat/register {})]
    (is (= (ast/->Var 'Nat) (type-of ctx (ast/->Var 'z))))
    (let [sz (ast/->App (ast/->Var 's) (ast/->Var 'z))]
      (is (= (ast/->Var 'Nat) (type-of ctx sz))))))

(deftest nat-equality-and-normalization
  (let [ctx (nat/register {})
        z   (ast/->Var 'z)
        sz  (ast/->App (ast/->Var 's) z)]
    (is (equal-terms? ctx z z))
    (is (not (equal-terms? ctx z sz)))
    (is (= sz (nf ctx sz)))))

(deftest eliminator-nat-type
  (let [ctx     (nat/register {})
        motive  (ast/->Lambda 'n (ast/->Var 'Nat) (ast/->Var 'Nat))
        m-z     (ast/->Var 'z)
        m-s     (ast/->Lambda 'x (ast/->Var 'Nat)
                              (ast/->Lambda 'ih (ast/->Var 'Nat) (ast/->Var 'Nat)))
        target  (ast/->Var 'z)
        elim    (ast/->Elim 'Nat motive [m-z m-s] target)
        ty      (type-of ctx elim)]
    (is (instance? App ty))
    (is (= motive (:fn ty)))
    (is (= target (:arg ty)))))