;;-----------------------------------------------------------------
;; cur.curnel.ntac.ctx-test
;;
;; Tests for the NTac context operations.
;;-----------------------------------------------------------------
(ns cur.curnel.ntac.ctx-test
  (:require [clojure.test :refer :all]
            [cur.curnel.ntac.ctx :as ctx]
            [cur.curnel.ast :as ast]))

(deftest ctx-basic-ops
  (let [empty (ctx/mk-empty-ctx)]
    ;; empty context
    (is (= [] (ctx/ctx-ids empty)))
    ;; add a binding
    (let [c1 (ctx/ctx-add empty 'x (ast/->Var 'Nat))]
      (is (= ['x] (ctx/ctx-ids c1)))
      (is (= [(ast/->Var 'Nat)] (ctx/ctx-types c1)))
      ;; lookup existing
      (is (= (ast/->Var 'Nat) (ctx/ctx-lookup c1 'x)))
      ;; remove binding
      (let [c2 (ctx/ctx-remove c1 'x)]
        (is (= [] (ctx/ctx-ids c2)))
        (is (nil? (ctx/ctx-lookup c2 'x)))))))