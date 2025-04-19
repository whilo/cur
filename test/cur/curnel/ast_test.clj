(ns cur.curnel.ast-test
  (:require [clojure.test :refer :all]
            [cur.curnel.ast :refer :all])
  (:import [cur.curnel.ast Var Universe Pi Lambda App Sigma Pair Fst Snd Let]))

(deftest var-term-test
  (let [v (->Var 'x)]
    (is (instance? Var v))
    (is (= 'x (:name v)))))

(deftest universe-term-test
  (let [u (->Universe 1)]
    (is (instance? Universe u))
    (is (= 1 (:level u)))))

(deftest pi-term-test
  (let [A (->Universe 0)
        B (->Universe 1)
        p (->Pi 'x A B)]
    (is (instance? Pi p))
    (is (= 'x (:param p)))
    (is (= A (:domain p)))
    (is (= B (:codomain p)))))

(deftest lambda-term-test
  (let [A (->Universe 0)
        body (->Var 'x)
        l (->Lambda 'y A body)]
    (is (instance? Lambda l))
    (is (= 'y (:param l)))
    (is (= A (:param-type l)))
    (is (= body (:body l)))))

(deftest app-term-test
  (let [f (->Var 'f)
        x (->Var 'x)
        a (->App f x)]
    (is (instance? App a))
    (is (= f (:fn a)))
    (is (= x (:arg a)))))

(deftest sigma-term-test
  (let [A (->Universe 0)
        B (->Universe 0)
        s (->Sigma 'x A B)]
    (is (instance? Sigma s))
    (is (= 'x (:param s)))
    (is (= A (:fst-type s)))
    (is (= B (:snd-type s)))))

(deftest pair-term-test
  (let [fstv (->Var 'a)
        sndv (->Var 'b)
        p (->Pair fstv sndv)]
    (is (instance? Pair p))
    (is (= fstv (:fst p)))
    (is (= sndv (:snd p)))))

(deftest projection-term-test
  (let [fstv (->Var 'a)
        sndv (->Var 'b)
        p (->Pair fstv sndv)
        fproj (->Fst p)
        sproj (->Snd p)]
    (is (instance? Fst fproj))
    (is (= p (:pair fproj)))
    (is (instance? Snd sproj))
    (is (= p (:pair sproj)))))

(deftest let-term-test
  (let [v (->Var 'z)
        l (->Let 'x v v)]
    (is (instance? Let l))
    (is (= 'x (:name l)))
    (is (= v (:value l)))
    (is (= v (:body l)))))