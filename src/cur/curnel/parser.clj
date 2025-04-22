;;-----------------------------------------------------------------
;; cur.curnel.parser
;;
;; Parsing Clojure forms (s-exprs) into cur AST terms
;;-----------------------------------------------------------------

(ns cur.curnel.parser
  (:require [cur.curnel.ast :as ast]))

(defn parse-term
  "Recursively parse a Clojure form into a cur.curnel.ast Term."
  [form]
  (cond
    ;; Already a Term?
    (satisfies? ast/Term form)
    form

    ;; nil literal => treat as Var 'nil for inductive constructors
    (nil? form)
    (ast/->Var 'nil)

    ;; Symbol -> Var
    (symbol? form)
    (ast/->Var form)

    ;; Parse list forms
    (seq? form)
    (let [op    (first form)
          args  (rest form)]
      (cond
        ;; Universe: (Type n)
        (= op 'Type)
        (let [[lvl] args]
          (ast/->Universe lvl))

        ;; Dependent function type: (Pi [x A] B)
        (= op 'Pi)
        (let [[binding codom] args
              [param domain] binding]
          (ast/->Pi param (parse-term domain) (parse-term codom)))

        ;; Dependent pair type: (Sigma [x A] B)
        (= op 'Sigma)
        (let [[binding snd-type] args
              [param fst-type] binding]
          (ast/->Sigma param (parse-term fst-type) (parse-term snd-type)))

        ;; Lambda: (lambda [x A] body)
        (#{'lambda 'lam 'Λ} op)
        (let [[binding body] args
              [param param-type] binding]
          (ast/->Lambda param (parse-term param-type) (parse-term body)))

        ;; Pair: (pair fst snd)
        (= op 'pair)
        (let [[fst snd] args]
          (ast/->Pair (parse-term fst) (parse-term snd)))

        ;; Projections: (fst p), (snd p)
        (= op 'fst)
        (ast/->Fst (parse-term (first args)))
        (= op 'snd)
        (ast/->Snd (parse-term (first args)))

        ;; Let binding: (let [x val] body)
        (= op 'let)
        (let [[binding body] args
              [name val]        binding]
          (ast/->Let name (parse-term val) (parse-term body)))

        ;; Inductive type declaration: (Inductive Name [ [p t] ... ] ResultType [CtorName CtorType]...)
        (= op 'Inductive)
        (let [[name param-bindings result-type & ctor-bindings] args
              params       (map (fn [[p t]] [p (parse-term t)]) param-bindings)
              res-type     (parse-term result-type)
              constructors (map (fn [[cname ctype]]
                                  (ast/->Constructor cname (parse-term ctype)))
                                ctor-bindings)]
          (ast/->InductiveType name params res-type constructors))
        ;; Eliminator (fold) for an inductive type: (elim TypeName motive method1 ... target)
        (= op 'elim)
        (let [[type-name motive & methods-and-target] args
              methods-forms (butlast methods-and-target)
              target-form   (last methods-and-target)
              motive-term   (parse-term motive)
              methods       (map parse-term methods-forms)
              target-term   (parse-term target-form)]
          (ast/->Elim type-name motive-term methods target-term))

        ;; Addition on Nat: (plus n m) via stdlib builder
        (= op 'plus)
        (let [[n m] args
              plus-fn (do (require 'cur.std.nat)
                          (ns-resolve 'cur.std.nat 'plus))]
          (plus-fn (parse-term n) (parse-term m)))

        ;; Multiplication on Nat: (mult m n) via stdlib builder
        (= op 'mult)
        (let [[m n] args
              mult-fn (do (require 'cur.std.nat)
                          (ns-resolve 'cur.std.nat 'mult))]
          (mult-fn (parse-term m) (parse-term n)))


        ;; Application: (f x y ...) => (((f x) y) ...)


        :else
        (let [terms (map parse-term form)]
          (reduce (fn [f a] (ast/->App f a)) terms))))

    :else
    (throw (ex-info "Unable to parse term" {:form form}))))