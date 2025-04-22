;;-----------------------------------------------------------------
;; cur.curnel.ntac.dsl-test
;;
;; Tests for the proof-scripting DSL: proof and defproof macros.
;;-----------------------------------------------------------------
(ns cur.curnel.ntac.dsl-test
  (:require [clojure.test :refer :all]
            [cur.curnel.ntac.dsl :refer [proof defproof]]
            [cur.curnel.ntac.ctx :as ctx]
            [cur.curnel.ast :as ast]))

(deftest proof-exact-simple
  (let [A    (ast/->Var 'A)
        ctx0 (ctx/mk-empty-ctx)
        ctx1 (ctx/ctx-add ctx0 'x A)
        res  (proof ctx1 (ast/->Var 'x) A exact)]
    (is (= [(ast/->Var 'x)] res))))

(deftest defproof-defines-var
  (let [A    (ast/->Var 'A)
        ctx0 (ctx/mk-empty-ctx)
        ctx1 (ctx/ctx-add ctx0 'x A)]
    (defproof my-simple-proof ctx1 (ast/->Var 'x) A exact)
    (is (= [(ast/->Var 'x)] my-simple-proof))))

(deftest proof-incomplete
  (testing "proof without tactics throws on incomplete proof"
    (let [A    (ast/->Var 'A)
          x    (ast/->Var 'x)
          ctx0 (ctx/mk-empty-ctx)
          ctx1 (ctx/ctx-add ctx0 'x A)]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Proof incomplete"
                            (proof ctx1 x A))))))