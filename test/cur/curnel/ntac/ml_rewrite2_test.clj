;;-----------------------------------------------------------------
;; clj-cur/test/cur/curnel/ntac/ml_rewrite2_test.clj
;;
;; Unit tests for ML-style rewrite (second suite) using plus and mult.
;;-----------------------------------------------------------------
(ns cur.curnel.ntac.ml-rewrite2-test
  (:require [clojure.test :refer :all]
            [cur.curnel.ast :as ast]
            [cur.curnel.ntac.ctx :as ctx]
            [cur.curnel.ntac.dsl :refer [proof]]
            [cur.std :refer [std-ctx]]
            [cur.std.nat :refer [plus]]))

(deftest plus-0-n-test
  (let [;; AST vars
        Nat     (ast/->Var 'Nat)
        n       (ast/->Var 'n)
        zero    (ast/->Var 'z)
        refl    (ast/->Var 'refl)
        ;; use plus builder for recursor AST
        %%plus    (ast/->Var 'plus)
        ;; term: λ [n:Nat] (refl Nat n)
        term    (ast/->Lambda 'n Nat
                              (ast/->App (ast/->App refl Nat) n))
        ;; expected type: ∀ [n:Nat] == Nat (plus zero n) n
        eq1     (ast/->App (ast/->App (ast/->App (ast/->Var '==) Nat)
                                      (plus zero n))
                           n)
        expected (ast/forall* [['n Nat]] eq1)
        ;; run proof: intro, simpl, reflexivity
        pf      (proof std-ctx term expected intro simpl reflexivity)]
    ;; proof is [ (refl Nat n) ]
    (is (= [(ast/->App (ast/->App refl Nat) n)] pf))))

;; Note: mult-0-plus and further tests require more infrastructure; deferred.