;;-----------------------------------------------------------------
;; clj-cur/test/cur/curnel/ntac/auto_test.clj
;;
;; Unit tests for the `auto` tactic.
;;-----------------------------------------------------------------
(ns cur.curnel.ntac.auto-test
  (:require [clojure.test :refer :all]
            [cur.curnel.ntac.core :as core]
            [cur.curnel.ntac.ctx :as ctx]
            [cur.curnel.ntac.dsl :refer [proof]]
            [cur.curnel.ast :as ast]))

(deftest auto-assumption
  (let [A     (ast/->Var 'A)
        ctx0  (ctx/mk-empty-ctx)
        ctx1  (ctx/ctx-add ctx0 'x A)
        goal  (core/->Goal ctx1 (ast/->Var 'x) A nil)
        state (core/->TacticState [goal] [])
        res   (core/auto state)]
    (is (= 1 (count res)))
    (let [new-st (first res)]
      (is (empty? (:goals new-st)))
      (is (= [(ast/->Var 'x)] (:proof new-st))))))

(deftest auto-reflexivity
  (let [A     (ast/->Var 'A)
        x     (ast/->Var 'x)
        refl  (ast/->Var 'refl)
        ;; expected equality: == A x x
        eqp   (ast/->App (ast/->App (ast/->App (ast/->Var '==) A) x) x)
        ctx0  (ctx/mk-empty-ctx)
        goal  (core/->Goal ctx0 nil eqp nil)
        state (core/->TacticState [goal] [])
        res   (core/auto state)]
    (is (= 1 (count res)))
    (let [new-st (first res)]
      (is (empty? (:goals new-st)))
      (is (= [(ast/->App (ast/->App refl A) x)] (:proof new-st))))))

(deftest auto-fail
  (let [ctx0  (ctx/mk-empty-ctx)
        goal  (core/->Goal ctx0 nil (ast/->Var 'A) nil)
        state (core/->TacticState [goal] [])]
    (is (empty? (core/auto state)))))

(deftest auto-in-proof
  (let [A        (ast/->Var 'A)
        x        (ast/->Var 'x)
        refl     (ast/->Var 'refl)
        ;; proof term: λ (x:A). refl A x
        term     (ast/->Lambda 'x A (ast/->App (ast/->App refl A) x))
        ;; goal type: ∀ x:A, == A x x
        eq1      (ast/->App (ast/->App (ast/->App (ast/->Var '==) A) x) x)
        expected (ast/->Pi 'x A eq1)
        pf       (proof (ctx/mk-empty-ctx) term expected intro auto)]
    (is (= [(ast/->App (ast/->App refl A) x)] pf))))