;;-----------------------------------------------------------------
;; clj-cur/test/cur/curnel/ntac/rewrite_in_test.clj
;;
;; Placeholder for tests ported from Racket `rewrite-in.rkt`.
;; TODO: implement rewrite-within-hypothesis tests.
;;-----------------------------------------------------------------
(ns cur.curnel.ntac.rewrite-in-test
  (:require [clojure.test :refer :all]
            [cur.curnel.ast :as ast]
            [cur.curnel.ntac.core :as core]
            [cur.curnel.ntac.ctx :as cctx]
            [cur.std :refer [std-ctx]]))

(deftest rewrite-in-basic
  (testing "rewrite-in replaces lhs with rhs in hypothesis type"
    (let [A          (ast/->Var 'A)
          x          (ast/->Var 'x)
          y          (ast/->Var 'y)
          z          (ast/->Var 'z)
          eq1        (ast/->App (ast/->App (ast/->App (ast/->Var '==) A) x) y)
          eq2        (ast/->App (ast/->App (ast/->App (ast/->Var '==) A) y) z)
          ctx1       (-> std-ctx
                         (assoc 'H1 eq1)
                         (assoc 'H2 eq2))
          goal       (core/->Goal ctx1 nil nil nil)
          state      (core/->TacticState [goal] [])
          ;; use hypothesis H2 as equality proof
          [st1]      (core/rewrite-in (ast/->Var 'H2) 'H1 state)
          new-goal   (first (:goals st1))
          new-ctx    (:ctx new-goal)
          got-type   (cctx/ctx-lookup new-ctx 'H1)
          expected   (ast/->App (ast/->App (ast/->App (ast/->Var '==) A) x) z)]
      (is (= expected got-type)))))