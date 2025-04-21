(ns cur.curnel.ntac.assert-test
  "Unit tests for the `assert` tactic."
  (:require [clojure.test :refer :all]
            [cur.curnel.ntac.core :as core]
            [cur.curnel.ntac.ctx :as ctx]
            [cur.curnel.ast :as ast]))

(deftest assert-splits-goals
  (let [ctx0  (ctx/mk-empty-ctx)
        goal  (core/->Goal ctx0 (ast/->Var 'foo) (ast/->Var 'A) nil)
        state (core/->TacticState [goal] [])
        res   (core/assert 'h (ast/->Var 'Nat) state)]
    ;; Should produce exactly one new tactic state
    (is (= 1 (count res)))
    (let [st  (first res)
          gs  (:goals st)
          g1  (nth gs 0)
          g2  (nth gs 1)]
      ;; First subgoal: prove asserted type
      (is (= (ast/->Var 'Nat) (:expected g1)))
      (is (= [] (ctx/ctx-ids (:ctx g1))))
      ;; Second subgoal: original goal under extended context
      (is (= (ast/->Var 'A) (:expected g2)))
      (is (= ['h] (ctx/ctx-ids (:ctx g2)))))))