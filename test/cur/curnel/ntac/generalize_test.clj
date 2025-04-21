;;-----------------------------------------------------------------
;; clj-cur/test/cur/curnel/ntac/generalize_test.clj
;;
;; Unit tests for the `generalize` tactic.
;;-----------------------------------------------------------------
(ns cur.curnel.ntac.generalize-test
  (:require [clojure.test :refer :all]
            [cur.curnel.ast :as ast]
            [cur.curnel.ntac.core :as core]
            [cur.curnel.ntac.ctx :as ctx]))

(deftest generalize-basic
  (let [T      (ast/->Var 'T)
        E      (ast/->Var 'E)
        ctx0   (ctx/mk-empty-ctx)
        ctx1   (ctx/ctx-add ctx0 'x T)
        goal   (core/->Goal ctx1 nil E nil)
        state  (core/->TacticState [goal] [])
        results (core/generalize 'x state)]
    ;; Expect exactly one resulting state
    (is (= 1 (count results)))
    (let [st        (first results)
          [g]       (:goals st)
          ctx2      (:ctx g)
          expected' (:expected g)]
      ;; x should be removed from the context
      (is (nil? (ctx/ctx-lookup ctx2 'x)))
      ;; expected type is now (Π [x : T] E)
      (is (= (ast/->Pi 'x T E) expected')))))

(deftest generalize-no-id
  (let [T      (ast/->Var 'T)
        E      (ast/->Var 'E)
        ctx0   (ctx/mk-empty-ctx)
        ;; no binding for 'y
        goal   (core/->Goal ctx0 nil E nil)
        state  (core/->TacticState [goal] [])]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"generalize: hypothesis not found"
                          (core/generalize 'y state)))))