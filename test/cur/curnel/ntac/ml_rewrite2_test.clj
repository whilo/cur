;;-----------------------------------------------------------------
;; clj-cur/test/cur/curnel/ntac/ml_rewrite2_test.clj
;;
;; Unit tests for ML-style rewrite (second suite) using plus and mult.
;;-----------------------------------------------------------------
(ns cur.curnel.ntac.ml-rewrite2-test
  (:require [clojure.test :refer :all]
            [cur.curnel.ast :as ast]
            [cur.curnel.ntac.dsl :refer [proof]]
            [cur.std :refer [std-ctx]]
            [cur.std.nat :refer [plus mult]]))

; Test: plus-zero identity via DSL proof (intro, reflexivity)
(deftest plus-0-n-test
  (let [Nat      (ast/->Var 'Nat)
        n        (ast/->Var 'n)
        zero     (ast/->Var 'z)
        refl     (ast/->Var 'refl)
        ;; term: λ [n:Nat] (refl Nat n)
        term     (ast/->Lambda 'n Nat
                               (ast/->App (ast/->App refl Nat) n))
        ;; expected type: ∀ [n:Nat] == Nat (plus zero n) n
        eq-inner (ast/->App (ast/->App (ast/->App (ast/->Var '==) Nat)
                                       (plus zero n))
                            n)
        expected (ast/forall* [['n Nat]] eq-inner)
        ;; run proof: intro, reflexivity
        pf       (proof std-ctx term expected intro reflexivity)]
    (is (= [(ast/->App (ast/->App refl Nat) n)] pf))))
