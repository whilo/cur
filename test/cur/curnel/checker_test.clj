(ns cur.curnel.checker-test
  (:require [clojure.test :refer :all]
            [cur.curnel.checker :refer [substitute eval-term type-of nf equal-terms? check-term register-inductive]]
            [cur.curnel.parser :refer [parse-term]]
            [cur.curnel.ast :as ast])
  (:import [cur.curnel.ast App Elim]))

(deftest substitute-basic
  (let [x   (ast/->Var 'x)
        y   (ast/->Var 'y)
        t   (ast/->App (ast/->Var 'f) x)
        res (substitute t 'x y)]
    (is (= res (ast/->App (ast/->Var 'f) y)))))

(deftest eval-let-test
  (let [t (ast/->Let 'x (ast/->Var 'a) (ast/->Var 'x))]
    (is (= (ast/->Var 'a)
           (eval-term {} t)))))

(deftest eval-app-lambda-test
  (let [body (ast/->Var 'z)
        lam  (ast/->Lambda 'z nil body)
        app  (ast/->App lam (ast/->Var 'y))]
    (is (= (ast/->Var 'y)
           (eval-term {} app)))))

(deftest type-universe-test
  (is (= (ast/->Universe 1)
         (type-of {} (ast/->Universe 0)))))

(deftest type-pi-test
  (let [A (ast/->Universe 0)
        B (ast/->Universe 0)
        p (ast/->Pi 'x A B)]
    (is (= (ast/->Universe 0)
           (type-of {} p)))))

(deftest type-app-test
  (let [A   (ast/->Universe 0)
        B   (ast/->Universe 1)
        p   (ast/->Pi 'x A B)
        f   (ast/->Lambda 'x A (ast/->Var 'x))
        app (ast/->App f (ast/->Var 'a))]
    ;; The return type of f is Pi x A A, so application returns A
    (is (= A
           (type-of {'a A} app)))))

(deftest nf-and-equality-test
  (let [A   (ast/->Universe 0)
        B   (ast/->Universe 0)
        id  (ast/->Lambda 'x A (ast/->Var 'x))
        id2 (ast/->Lambda 'y A (ast/->Var 'y))]
    (is (equal-terms? {} id id2))
    (is (equal-terms? {} (ast/->App id (ast/->Var 'z)) (ast/->Var 'z)))))

(deftest check-term-test
  (let [A  (ast/->Universe 0)
        id (ast/->Lambda 'x A (ast/->Var 'x))
        idPi (ast/->Pi 'x A A)]
    (is (nil? (check-term {} id idPi)))
    (is (thrown? Exception (check-term {} (ast/->Var 'y) A)))))

(deftest type-elim-test
  (let [decl  (parse-term '(Inductive Bool [] (Type 0) [True Bool] [False Bool]))
        ctx   (register-inductive {} decl)
        motive (ast/->Lambda 'x (ast/->Var 'Bool) (ast/->Var 'x))
        methods []
        target  (ast/->Var 'True)
        t      (ast/->Elim 'Bool motive methods target)
        ty     (type-of ctx t)]
    (is (instance? App ty))
    (is (= motive (:fn ty)))
    (is (= target (:arg ty)))))