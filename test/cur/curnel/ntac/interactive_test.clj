;;-----------------------------------------------------------------
;; clj-cur/test/cur/curnel/ntac/interactive_test.clj
;;
;; DSL unit test for the `interactive` tactic.
;;-----------------------------------------------------------------
(ns cur.curnel.ntac.interactive-test
  (:require [clojure.test :refer :all]
            [cur.curnel.ast :as ast]
            [cur.curnel.ntac.ctx :as ctx]
            [cur.curnel.ntac.dsl :refer [proof]]))

(deftest interactive-basic
  (let [A        (ast/->Var 'A)
        a        (ast/->Var 'a)
        term     (ast/->Var 'dummy)
        expected (ast/->Pi 'a A A)
        pf       (proof (ctx/mk-empty-ctx) term expected interactive)]
    (is (= [a] pf))))