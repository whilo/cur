;;-----------------------------------------------------------------
;; clj-cur/test/cur/curnel/ntac/ctx_racket_test.clj
;;
;; Port of Racket ntac/ctx.rkt tests for context operations.
;;-----------------------------------------------------------------
(ns cur.curnel.ntac.ctx-racket-test
  (:require [clojure.test :refer :all]
            [cur.curnel.ntac.ctx :as ctx]
            [cur.curnel.ast :as ast]))

(deftest ctx-empty-env-test
  "mk-empty-ctx -> empty environment"
  (is (= [] (ctx/ctx->env (ctx/mk-empty-ctx)))))

(deftest ctx-adds-and-env-test
  (let [ids ['x 'y]
        tys [(ast/->Var 'tmp1) (ast/->Var 'tmp2)]
        c   (ctx/ctx-adds (ctx/mk-empty-ctx) ids tys)
        env (ctx/ctx->env c)]
    ;; Environment is innermost-first
    (is (= [['y (ast/->Var 'tmp2)]
            ['x (ast/->Var 'tmp1)]]
           env))
    ;; env vector structure
    (is (= [['y (ast/->Var 'tmp2)]
            ['x (ast/->Var 'tmp1)]]
           env))))