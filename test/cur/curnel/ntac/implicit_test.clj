;;-----------------------------------------------------------------
;; clj-cur/test/cur/curnel/ntac/implicit_test.clj
;;
;; Unit tests for the `implicit` tactic.
;;-----------------------------------------------------------------
(ns cur.curnel.ntac.implicit-test
  (:require [clojure.test :refer :all]
            [cur.curnel.ast :as ast]
            [cur.curnel.ntac.core :as core]
            [cur.curnel.ntac.ctx :as ctx]))

(deftest implicit-basic
  (let [A    (ast/->Var 'A)
        B    (ast/->Var 'B)
        C    (ast/->Var 'C)
        ;; expected type: Π x:A. Π y:B. C
        pi2  (ast/->Pi 'x A (ast/->Pi 'y B C))
        goal (core/->Goal (ctx/mk-empty-ctx) nil pi2 nil)
        state (core/->TacticState [goal] [])
        res   (core/implicit state)]
    ;; Should produce one state with expected C
    (is (= 1 (count res)))
    (let [st        (first res)
          [g]       (:goals st)
          expected' (:expected g)]
      (is (= C expected')))))

(deftest implicit-no-op
  (let [A    (ast/->Var 'A)
        goal (core/->Goal (ctx/mk-empty-ctx) nil A nil)
        state (core/->TacticState [goal] [])
        res   (core/implicit state)]
    ;; No Pi, should return the same state
    (is (= 1 (count res)))
    (is (= state (first res)))))