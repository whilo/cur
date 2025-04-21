;;-----------------------------------------------------------------
;; clj-cur/test/cur/curnel/ntac/reflexivity_poly_test.clj
;;
;; Unit tests for polymorphic reflexivity via `intro` + `reflexivity`.
;;-----------------------------------------------------------------
(ns cur.curnel.ntac.reflexivity-poly-test
  (:require [clojure.test :refer :all]
            [cur.curnel.ast :as ast]
            [cur.curnel.checker :as chk]
            [cur.curnel.ntac.core :as core]
            [cur.curnel.ntac.ctx :as ctx]))

(deftest reflexivity-poly-basic
  (let [;; AST vars
        X     (ast/->Var 'X)
        list  (ast/->Var 'list)
        nil*  (ast/->Var 'nil*)
        ;; Build type: Π [X : Type] (== (list X) (nil* X) (nil* X))
        A     (ast/->App (ast/->App list X) nil)
        ;; Actually list applied to X: (list X)
        listX (ast/->App list X)
        nilX  (ast/->App nil* X)
        eqApp (ast/->App (ast/->App (ast/->App (ast/->Var '==) listX) nilX) nilX)
        piTy  (ast/->Pi 'X (ast/->Var 'Type) eqApp)
        ;; initial goal state
        goal  (core/->Goal (ctx/mk-empty-ctx) nil piTy nil)
        state0 (core/->TacticState [goal] [])
        ;; apply intro then reflexivity
        state1s (core/intro state0)
        state1  (first state1s)
        state2s (core/reflexivity state1)
        state2  (first state2s)
        [pf]    (:proof state2)]
    ;; pf should be (refl (list X) (nil* X))
    (is (= 1 (count state1s)))
    (is (= eqApp (:expected (first (:goals state1)))))
    (is (= 1 (count state2s)))
    (is (= [(ast/->App (ast/->App (ast/->Var 'refl) listX) nilX)] [pf]))))