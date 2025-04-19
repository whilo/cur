;;-----------------------------------------------------------------
;; clj-cur/test/cur/curnel/ntac/tactics_basic_test.clj
;;
;; Unit tests for basic NTac core tactics: rewrite, rewriteR, inversion.
;;-----------------------------------------------------------------
(ns cur.curnel.ntac.tactics-basic-test
  (:require [clojure.test :refer :all]
            [cur.curnel.ast :as ast]
            [cur.curnel.checker :as chk]
            [cur.curnel.ntac.core :as core]
            [cur.curnel.ntac.ctx :as ctx]))

(deftest rewrite-no-goals-test
  (let [dummy-proof (ast/->App (ast/->App (ast/->Var 'refl) (ast/->Var 'A)) (ast/->Var 'x))
        empty-state (core/->TacticState [] [])]
    (is (= [] (core/rewrite dummy-proof empty-state)))
    (is (= [] (core/rewriteR dummy-proof empty-state)))))

(deftest rewrite-noop-test
  "rewrite on trivial expected type leaves expected unchanged"
  (let [A         (ast/->Var 'A)
        eq-proof  (ast/->App (ast/->App (ast/->Var 'refl) A) A)
        g         (core/->Goal (ctx/mk-empty-ctx)
                              (ast/->Var 't)
                              A
                              nil)
        state     (core/->TacticState [g] [])
        [st1]     (core/rewrite eq-proof state)
        [st2]     (core/rewriteR eq-proof state)
        g1        (first (:goals st1))
        g2        (first (:goals st2))]
    (is (= A (:expected g1)))
    (is (= A (:expected g2)))))

(deftest inversion-basic-test
  "inversion on Nat hypothesis produces two subgoals with correct contexts"
  (let [ctx0  (ctx/mk-empty-ctx)
        ctx1  (ctx/ctx-add ctx0 'h (ast/->Var 'Nat))
        goal  (core/->Goal ctx1 (ast/->Var 'foo) (ast/->Var 'A) nil)
        state (core/->TacticState [goal] [])
        results (core/inversion 'h state)]
    (is (= 2 (count results)))
    (let [st1 (first results)
          st2 (second results)
          g1  (first (:goals st1))
          g2  (first (:goals st2))]
      ;; first branch: h removed
      (is (nil? (ctx/ctx-lookup (:ctx g1) 'h)))
      ;; second branch: h removed, x added of type Nat
      (is (nil? (ctx/ctx-lookup (:ctx g2) 'h)))
      (is (= (ast/->Var 'Nat) (ctx/ctx-lookup (:ctx g2) 'x))))))
  
(deftest destruct-basic-test
  "destruct on Nat hypothesis is alias to inversion"
  (let [ctx0  (ctx/mk-empty-ctx)
        ctx1  (ctx/ctx-add ctx0 'h (ast/->Var 'Nat))
        goal  (core/->Goal ctx1 (ast/->Var 'foo) (ast/->Var 'A) nil)
        state (core/->TacticState [goal] [])
        results (core/destruct 'h state)]
    (is (= (core/inversion 'h state) results))))

(deftest destruct-exist-basic-test
  "destruct-exist currently aliases destruct"
  (let [ctx0  (ctx/mk-empty-ctx)
        ctx1  (ctx/ctx-add ctx0 'h (ast/->Var 'Nat))
        goal  (core/->Goal ctx1 (ast/->Var 'foo) (ast/->Var 'A) nil)
        state (core/->TacticState [goal] [])
        res1  (core/destruct 'h state)
        res2  (core/destruct-exist 'h state)]
    (is (= res1 res2))))