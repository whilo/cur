;;-----------------------------------------------------------------
;; clj-cur/test/cur/curnel/ntac/ml_rewrite_test.clj
;;
;; Unit tests for ML-style rewrite tactics on `plus`.
;;-----------------------------------------------------------------
(ns cur.curnel.ntac.ml-rewrite-test
  (:require [clojure.test :refer :all]
            [cur.curnel.ast :as ast]
            [cur.curnel.ntac.core :as core]
            [cur.curnel.ntac.ctx :as ctx]
            [cur.std :refer [std-ctx]]))

(deftest ml-rewrite-basic
  (let [n         (ast/->Var 'n)
        m         (ast/->Var 'm)
        ;; Setup context with n:Nat, m:Nat, H:(== Nat n m)
        eq-ty     (ast/->App (ast/->App (ast/->App (ast/->Var '==) (ast/->Var 'Nat)) n) m)
        ctx1      (assoc std-ctx 'n (ast/->Var 'Nat) 'm (ast/->Var 'Nat) 'H eq-ty)
        ;; goal term: plus n n
        term1     (ast/->App (ast/->App (ast/->Var 'plus) n) n)
        state     (core/->TacticState [(core/->Goal ctx1 nil term1 nil)] [])
        ;; apply rewrite with hypothesis H
        [st1]     (core/rewrite (ast/->Var 'H) state)
        g1        (first (:goals st1))
        expected1 (:expected g1)
        ;; expected term: plus m m
        expected2 (ast/->App (ast/->App (ast/->Var 'plus) m) m)]
    (is (= expected2 expected1))))

(deftest ml-rewriteR-basic
  (let [n         (ast/->Var 'n)
        m         (ast/->Var 'm)
        ;; Setup context and goal for reverse rewrite
        eq-ty     (ast/->App (ast/->App (ast/->App (ast/->Var '==) (ast/->Var 'Nat)) n) m)
        ctx1      (assoc std-ctx 'n (ast/->Var 'Nat) 'm (ast/->Var 'Nat) 'H eq-ty)
        plus-nn   (ast/->App (ast/->App (ast/->Var 'plus) n) n)
        plus-mm   (ast/->App (ast/->App (ast/->Var 'plus) m) m)
        ;; goal term uses plus-mm, reverse rewrite yields plus-nn
        state     (core/->TacticState [(core/->Goal ctx1 nil plus-mm nil)] [])
        [st1]     (core/rewriteR (ast/->Var 'H) state)
        g1        (first (:goals st1))
        expected' (:expected g1)]
    (is (= plus-nn expected'))))