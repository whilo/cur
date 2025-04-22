;;-----------------------------------------------------------------
;; clj-cur/test/cur/curnel/ntac/induction_test.clj
;;
;; Placeholder for tests ported from Racket `induction.rkt`.
;; TODO: implement dependent induction tests on `le` and `lt`.
;;-----------------------------------------------------------------
;;-----------------------------------------------------------------
;; clj-cur/test/cur/curnel/ntac/induction_test.clj
;;
;; Unit tests for the induction tactic on Nat.
;;-----------------------------------------------------------------
(ns cur.curnel.ntac.induction-test
  (:require [clojure.test :refer :all]
            [cur.curnel.ast :as ast]
            [cur.curnel.ntac.core :as core]
            [cur.curnel.ntac.ctx :as ctx]))

;; Basic induction splits on Nat hypothesis into base and step cases
(deftest induction-basic
  (let [;; AST terms
        Nat   (ast/->Var 'Nat)
        z     (ast/->Var 'z)
        h     (ast/->Var 'h)
        ;; initial context: h:Nat
        ctx0  (ctx/mk-empty-ctx)
        ctx1  (ctx/ctx-add ctx0 'h Nat)
        ;; dummy goal: prove A (any term)
        A     (ast/->Var 'A)
        goal  (core/->Goal ctx1 nil A nil)
        state (core/->TacticState [goal] [])
        [st-base st-step] (core/induction h state)
        g-base (first (:goals st-base))
        g-step (first (:goals st-step))
        ctx-base (:ctx g-base)
        ctx-step (:ctx g-step)]
    ;; Base case: hypothesis h removed
    (is (nil? (ctx/ctx-lookup ctx-base 'h)))
    ;; Step case: h removed, x and IH added
    (is (nil? (ctx/ctx-lookup ctx-step 'h)))
    (is (= Nat (ctx/ctx-lookup ctx-step 'x)))
    (is (= ;; IH type: substitute h->x in A yields A
         A
         (ctx/ctx-lookup ctx-step 'IH)))))