;;-----------------------------------------------------------------
;; clj-cur/test/cur/curnel/ntac/rewrite_forall_test.clj
;;
;; Unit tests for rewrite-under-Pi (rewrite-forall) tactic.
;;-----------------------------------------------------------------
(ns cur.curnel.ntac.rewrite-forall-test
  (:require [clojure.test :refer :all]
            [cur.curnel.ast :as ast]
            [cur.curnel.ntac.core :as core]
            [cur.std :refer [std-ctx]]))

(deftest rewrite-forall-simple
  (testing "rewrite under Pi binder replaces inside body only"
    (let [A         (ast/->Var 'A)
          x         (ast/->Var 'x)
          y         (ast/->Var 'y)
          ;; H: (== A x y)
          eqp       (ast/->App (ast/->App (ast/->App (ast/->Var '==) A) x) y)
          ;; expected: Pi [z:A] == A x x
          body      (ast/->App (ast/->App (ast/->App (ast/->Var '==) A) x) x)
          expected1 (ast/->Pi 'z A body)
          ctx1      (assoc std-ctx 'H eqp)
          goal      (core/->Goal ctx1 nil expected1 nil)
          state     (core/->TacticState [goal] [])
          [st1]     (core/rewrite-forall (ast/->Var 'H) state)
          g1        (first (:goals st1))
          expected2 (ast/->Pi 'z A (ast/->App (ast/->App (ast/->App (ast/->Var '==) A) y) y))]
      (is (= expected2 (:expected g1))))))