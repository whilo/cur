;;-----------------------------------------------------------------
;; clj-cur/test/cur/curnel/ntac/simpl_test.clj
;;
;; Unit tests for the `simpl` tactic.
;;-----------------------------------------------------------------
(ns cur.curnel.ntac.simpl-test
  (:require [clojure.test :refer :all]
            [cur.curnel.ast :as ast]
            [cur.curnel.ntac.core :as core]
            [cur.curnel.ntac.ctx :as ctx]))

(deftest simpl-beta-reduction
  (let [A    (ast/->Var 'A)
        x    (ast/->Var 'x)
        body x
        lam  (ast/->Lambda 'x A body)
        expr (ast/->App lam x)
        ;; goal with expected = ((λ (x:A). x) x)
        goal (core/->Goal (ctx/mk-empty-ctx) nil expr nil)
        state (core/->TacticState [goal] [])
        res   (core/simpl state)]
    ;; After simpl, expected type should be x
    (is (= 1 (count res)))
    (let [st        (first res)
          [g]       (:goals st)
          expected' (:expected g)]
      (is (= x expected')))))

(deftest simpl-no-op
  (let [A    (ast/->Var 'A)
        goal (core/->Goal (ctx/mk-empty-ctx) nil A nil)
        state (core/->TacticState [goal] [])
        res   (core/simpl state)]
    ;; No reducible expression, expected remains A
    (is (= 1 (count res)))
    (is (= A (:expected (first (:goals (first res))))))))