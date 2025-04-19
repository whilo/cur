;;-----------------------------------------------------------------
;; cur.std.equality
;;
;; Standard library: propositional equality type and constructor.
;;-----------------------------------------------------------------
(ns cur.std.equality
  "Standard library: propositional equality (==) and refl constructor."
  (:require [cur.curnel.parser :refer [parse-term]]
            [cur.curnel.checker :refer [register-inductive]]
            [cur.curnel.ast :as ast]))

(def eq-decl
  "Inductive declaration for Martin-Löf/Paulin-Mohring equality."
  (parse-term
   '(Inductive ==
               [[A (Type 0)] [x A] [y A]]
               (Type 0)
               [refl (Pi [A (Type 0)]
                         (Pi [x A]
                             (== A x x)))])))

(defn register
  "Register == and its constructor refl into the context."
  [ctx]
  (register-inductive ctx eq-decl))

(defn elim
  "Eliminator (recursor) for propositional equality (==).
   Usage: (elim motive refl-case p)
   where motive is a Lambda over the equality proof,
   refl-case corresponds to the refl constructor,
   and p is the equality proof to eliminate."
  [motive refl-case p]
  (ast/->Elim '== motive [refl-case] p))