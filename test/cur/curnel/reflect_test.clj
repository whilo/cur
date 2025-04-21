(ns cur.curnel.reflect-test
  "Tests for AST quotation (term) and reflection (ast->sexpr)."
  (:require [clojure.test :refer :all]
            [cur.curnel.reflect :refer [term sexpr->term ast->sexpr]]
            [cur.curnel.ast :as ast]))

(deftest sexpr->term-simple
  (is (= (sexpr->term 'x)
         (ast/->Var 'x)))
  (is (= (sexpr->term '(univ 2))
         (ast/->Universe 2)))
  (is (= (sexpr->term '(pi x (univ 0) (univ 1)))
         (ast/->Pi 'x (ast/->Universe 0) (ast/->Universe 1))))
  (is (= (sexpr->term '(lambda y (univ 0) y))
         (ast/->Lambda 'y (ast/->Universe 0) (ast/->Var 'y))))
  (is (= (sexpr->term '(app f z))
         (ast/->App (ast/->Var 'f) (ast/->Var 'z))))
  (is (= (sexpr->term '(sigma p (univ 0) (univ 0)))
         (ast/->Sigma 'p (ast/->Universe 0) (ast/->Universe 0))))
  (is (= (sexpr->term '(pair a b))
         (ast/->Pair (ast/->Var 'a) (ast/->Var 'b))))
  (is (= (sexpr->term '(fst pr))
         (ast/->Fst (ast/->Var 'pr))))
  (is (= (sexpr->term '(snd pr))
         (ast/->Snd (ast/->Var 'pr))))
  (is (= (sexpr->term '(let n x y))
         (ast/->Let 'n (ast/->Var 'x) (ast/->Var 'y))))
  (is (= (sexpr->term '(elim T m [h1 h2] t))
         (ast/->Elim 'T
                     (ast/->Var 'm)
                     [(ast/->Var 'h1) (ast/->Var 'h2)]
                     (ast/->Var 't)))))

(deftest term-macro
  (is (= (term x) (ast/->Var 'x)))
  (is (= (term (univ 3)) (ast/->Universe 3)))
  (is (= (term (pi x (univ 0) x))
         (ast/->Pi 'x (ast/->Universe 0) (ast/->Var 'x)))))

(deftest ast->sexpr-roundtrip
  (let [forms ['x
               '(univ 1)
               '(pi a (univ 0) (app a a))]
        terms [(term x)
               (term (univ 1))
               (term (pi a (univ 0) (app a a)))]
        pairs (map vector forms terms)]
    (doseq [[frm tm] pairs]
      (is (= (ast->sexpr tm) frm)))))
