(ns cur.curnel.ntac.reflexivity-test
  "Unit tests for the `reflexivity` tactic."
  (:require [clojure.test :refer :all]
            [cur.curnel.ntac.core :as core]
            [cur.curnel.ntac.ctx :as ctx]
            [cur.curnel.ast :as ast]
            [cur.curnel.checker :as chk]))

(deftest reflexivity-success
  (let [A        (ast/->Var 'A)
        x        (ast/->Var 'x)
        ;; expected: (== A x x)
        expected (ast/->App (ast/->App (ast/->App (ast/->Var '==) A) x) x)
        goal     (core/->Goal (ctx/mk-empty-ctx) nil expected nil)
        state    (core/->TacticState [goal] [])
        result   (core/reflexivity state)
        ;; result should be vector with one state
        st       (first result)
        proof    (:proof st)
        ;; proof term should be (refl A x)
        expected-pf (ast/->App (ast/->App (ast/->Var 'refl) A) x)]
    (is (= 1 (count result)))
    (is (= [expected-pf] proof))))

(deftest reflexivity-failure
  (let [A        (ast/->Var 'A)
        x        (ast/->Var 'x)
        y        (ast/->Var 'y)
        ;; expected: (== A x y), x!=y
        expected (ast/->App (ast/->App (ast/->App (ast/->Var '==) A) x) y)
        goal     (core/->Goal (ctx/mk-empty-ctx) nil expected nil)
        state    (core/->TacticState [goal] [])]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"reflexivity: goal is not of form == A x x"
                          (core/reflexivity state)))))