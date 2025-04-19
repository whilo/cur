(ns cur.curnel.nat-test
  "Tests for natural number inductive type and basic operations."
  (:require [clojure.test :refer :all]
            [cur.curnel.parser :refer [parse-term]]
            [cur.curnel.checker :refer [register-inductive type-of nf equal-terms?]]
            [cur.curnel.ast :as ast])
  (:import [cur.curnel.ast InductiveType Universe Var App Lambda Elim]))

(deftest parse-nat-inductive
  (let [t (parse-term '(Inductive Nat [] (Type 0)
                                  [z Nat]
                                  [s (Pi [x Nat] Nat)]))]
    (is (instance? InductiveType t))
    (is (= 'Nat (:name t)))
    (is (empty? (:params t)))
    (is (instance? Universe (:result-type t)))
    (let [ctors (:constructors t)]
      (is (= 2 (count ctors)))
      (is (= 'z (-> ctors first :name)))
      (is (= 's (-> ctors second :name))))))

(deftest constructor-types
  (let [decl (parse-term '(Inductive Nat [] (Type 0)
                                     [z Nat]
                                     [s (Pi [x Nat] Nat)]))
        ctx  (register-inductive {} decl)
        z    (ast/->Var 'z)
        s    (ast/->App (ast/->Var 's) z)]
    (is (= (ast/->Var 'Nat) (type-of ctx z)))
    (is (= (ast/->Var 'Nat) (type-of ctx s)))))

(deftest nat-equality-and-normalization
  (let [decl (parse-term '(Inductive Nat [] (Type 0)
                                     [z Nat]
                                     [s (Pi [x Nat] Nat)]))
        ctx  (register-inductive {} decl)
        z    (ast/->Var 'z)
        sz   (ast/->App (ast/->Var 's) z)]
    (is (equal-terms? ctx z z))
    (is (not (equal-terms? ctx z sz)))
    ;; nf should preserve structure
    (is (= sz (nf ctx sz)))))

(deftest eliminator-nat-type
  (let [decl  (parse-term '(Inductive Nat [] (Type 0)
                                      [z Nat]
                                      [s (Pi [x Nat] Nat)]))
        ctx   (register-inductive {} decl)
        ;; motive: λ (n : Nat) . Nat
        motive (ast/->Lambda 'n (ast/->Var 'Nat) (ast/->Var 'Nat))
        ;; methods: base-case and step-case
        m-z    (ast/->Var 'z)
        m-s    (ast/->Lambda 'x (ast/->Var 'Nat)
                             (ast/->Lambda 'ih (ast/->Var 'Nat) (ast/->Var 'Nat)))
        target (ast/->Var 'z)
        elim   (ast/->Elim 'Nat motive [m-z m-s] target)
        ty     (type-of ctx elim)]
    (is (instance? App ty))
    (is (= motive (:fn ty)))
    (is (= target (:arg ty)))))