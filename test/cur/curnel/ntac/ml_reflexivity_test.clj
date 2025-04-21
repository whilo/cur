;;-----------------------------------------------------------------
;; clj-cur/test/cur/curnel/ntac/ml_reflexivity_test.clj
;;
;; Unit tests for ML-style `reflexivity` tactic on a custom `day` inductive.
;;-----------------------------------------------------------------
(ns cur.curnel.ntac.ml-reflexivity-test
  (:require [clojure.test :refer :all]
            [cur.curnel.ast :as ast]
            [cur.curnel.ntac.core :as core]
            [cur.curnel.ntac.ctx :as ctx]))

(deftest ml-reflexivity-success
  (let [day       (ast/->Var 'day)
        next-wd   (ast/->Var 'next-weekday)
        sat       (ast/->Var 'sat)
        tues      (ast/->Var 'tues)
        ;; inner: (next-weekday sat)
        inner     (ast/->App next-wd sat)
        ;; x: (next-weekday inner)
        x         (ast/->App next-wd inner)
        ;; expected: (== day x x)
        expected  (ast/->App (ast/->App (ast/->App (ast/->Var '==) day) x) x)
        goal      (core/->Goal (ctx/mk-empty-ctx) nil expected nil)
        state     (core/->TacticState [goal] [])
        result    (core/reflexivity state)
        st        (first result)
        proof     (:proof st)
        ;; proof: (refl day x)
        exp-pf    [(ast/->App (ast/->App (ast/->Var 'refl) day) x)]]
    (is (= 1 (count result)))
    (is (= exp-pf proof))))

(deftest ml-reflexivity-failure
  (let [day       (ast/->Var 'day)
        next-wd   (ast/->Var 'next-weekday)
        sat       (ast/->Var 'sat)
        mon       (ast/->Var 'mon)
        ;; inner: (next-weekday sat)
        inner     (ast/->App next-wd sat)
        ;; x: (next-weekday inner)
        x         (ast/->App next-wd inner)
        ;; expected: (== day x mon) — mon != x
        expected  (ast/->App (ast/->App (ast/->App (ast/->Var '==) day) x) mon)
        goal      (core/->Goal (ctx/mk-empty-ctx) nil expected nil)
        state     (core/->TacticState [goal] [])]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"reflexivity: goal is not of form == A x x"
                          (core/reflexivity state)))))