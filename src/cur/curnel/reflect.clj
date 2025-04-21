(ns cur.curnel.reflect
  "Reflection and quotation support for cur AST terms."
  (:require [cur.curnel.ast :as ast])
  (:import [cur.curnel.ast Var Universe Pi Lambda App Sigma Pair Fst Snd Let Elim]))

(declare sexpr->term)

(defn read-term
  "Data reader function: convert a quoted s-expression into an AST term for tagged literal `#term`." [form]
  (sexpr->term form))

(defmacro term
  "Quote a term AST from a Clojure s-expression DSL.
  Supported forms:
    symbol           -> Var
    (univ n)         -> Universe
    (pi p dom cod)   -> Pi
    (lambda p ty b)  -> Lambda
    (app f a)        -> App
    (sigma p ft st)  -> Sigma
    (pair x y)       -> Pair
    (fst p)          -> Fst
    (snd p)          -> Snd
    (let n v b)      -> Let
    (elim n m ms t)  -> Elim
  Example:
    (term (pi x (univ 0) (app f x)))
  expands to a `Pi` AST node.
  "
  [form]
   ;; At compile time, convert the quoted s-expression into an AST term
  (sexpr->term form))

(defn sexpr->term
  "Convert a quoted s-expression into an AST term." [form]
  (cond
    (symbol? form)
    (ast/->Var form)

    (number? form)
    (ast/->Universe form)

    (seq? form)
    (let [op    (first form)
          args  (rest form)]
      (let [op#   (first form)
            args# (rest form)]
        (cond
          (= op# 'univ)
          (let [[n] args#]
            (ast/->Universe n))

          (= op# 'pi)
          (let [[p dom cod] args#]
            (ast/->Pi p (sexpr->term dom) (sexpr->term cod)))

          (= op# 'lambda)
          (let [[p ty b] args#]
            (ast/->Lambda p (sexpr->term ty) (sexpr->term b)))

          (= op# 'app)
          (let [[f a] args#]
            (ast/->App (sexpr->term f) (sexpr->term a)))

          (= op# 'sigma)
          (let [[p ft st] args#]
            (ast/->Sigma p (sexpr->term ft) (sexpr->term st)))

          (= op# 'pair)
          (let [[x y] args#]
            (ast/->Pair (sexpr->term x) (sexpr->term y)))

          (= op# 'fst)
          (let [[p] args#]
            (ast/->Fst (sexpr->term p)))

          (= op# 'snd)
          (let [[p] args#]
            (ast/->Snd (sexpr->term p)))

          (= op# 'let)
          (let [[n v b] args#]
            (ast/->Let n (sexpr->term v) (sexpr->term b)))

          (= op# 'elim)
          (let [[n m ms t] args#]
            (ast/->Elim n
                        (sexpr->term m)
                        (mapv sexpr->term ms)
                        (sexpr->term t)))

          :else
        ;; Fallback: treat op as a Var and apply it to each argument
          (let [f0#  (ast/->Var op#)
                as#  (map sexpr->term args#)]
            (reduce (fn [f# a#]
                      (ast/->App f# a#))
                    f0#
                    as#)))))

    :else
    (throw (ex-info "Cannot convert form to AST term" {:form form}))))

(defn ast->sexpr
  "Convert an AST term into a Clojure s-expression." [t]
  (cond
    (instance? Var t)
    (:name t)

    (instance? Universe t)
    ;; Represent a universe as a tagged list: (univ level)
    (list 'univ (:level t))

    (instance? Pi t)
    ;; Represent Pi type as (pi param domain codomain)
    (list 'pi
          (:param t)
          (ast->sexpr (:domain t))
          (ast->sexpr (:codomain t)))

    (instance? Lambda t)
    ;; Represent lambda as (lambda param param-type body)
    (list 'lambda
          (:param t)
          (ast->sexpr (:param-type t))
          (ast->sexpr (:body t)))

    (instance? App t)
    ;; Represent application as (app fn arg)
    (list 'app
          (ast->sexpr (:fn t))
          (ast->sexpr (:arg t)))

    (instance? Sigma t)
    ;; Represent Sigma type as (sigma param fst-type snd-type)
    (list 'sigma
          (:param t)
          (ast->sexpr (:fst-type t))
          (ast->sexpr (:snd-type t)))

    (instance? Pair t)
    ;; Represent pair as (pair fst snd)
    (list 'pair
          (ast->sexpr (:fst t))
          (ast->sexpr (:snd t)))

    (instance? Fst t)
    ;; Represent fst projection as (fst pair)
    (list 'fst
          (ast->sexpr (:pair t)))

    (instance? Snd t)
    ;; Represent snd projection as (snd pair)
    (list 'snd
          (ast->sexpr (:pair t)))

    (instance? Let t)
    ;; Represent let as (let name value body)
    (list 'let
          (:name t)
          (ast->sexpr (:value t))
          (ast->sexpr (:body t)))

    (instance? Elim t)
    ;; Represent eliminator as (elim name motive methods target)
    (list 'elim
          (:name t)
          (ast->sexpr (:motive t))
          (vec (map ast->sexpr (:methods t)))
          (ast->sexpr (:target t)))

    :else
    (throw (ex-info "Unknown AST node" {:node t}))))