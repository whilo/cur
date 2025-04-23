;;-----------------------------------------------------------------
;; clj-cur/test/cur/curnel/ntac/admit_test.clj
;;
;; Placeholder for tests ported from Racket `admit.rkt`.
;; TODO: implement `admit` tactic and assumptions printing.
;;-----------------------------------------------------------------
(ns cur.curnel.ntac.admit-test
  (:require [clojure.test :refer :all]
            [cur.curnel.ast :as ast]
            [cur.curnel.ntac.core :as core]
            [cur.curnel.ntac.ctx :as ctx]))

(deftest admit-basic
  (testing "admit discharges a goal with placeholder"
    (let [empty-ctx  (ctx/mk-empty-ctx)
          term       (ast/->Var 't)
          expected   (ast/->Var 'A)
          goal       (core/->Goal empty-ctx term expected nil)
          state      (core/->TacticState [goal] [])
          [st]       (core/admit state)
          final-goals (:goals st)
          proof       (:proof st)]
      (is (empty? final-goals))
      (is (= [(ast/->Var 'admit)] proof)))))