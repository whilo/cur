;;-----------------------------------------------------------------
;; cur.std.list
;;
;; Standard library: List type and constructors.
;;-----------------------------------------------------------------
 (ns cur.std.list
   "Standard library: List type, constructors, and eliminator."
   (:require [cur.curnel.parser :refer [parse-term]]
             [cur.curnel.checker :refer [register-inductive]]
             [cur.curnel.ast :as ast]))

(def list-decl
  "Inductive declaration for List, parameterized by type A."
  (parse-term
   '(Inductive List
               [[A (Type 0)]]
               (Type 0)
               [nil (Pi [A (Type 0)] (List A))]
               [cons (Pi [A (Type 0)]
                         (Pi [x A]
                             (Pi [xs (List A)]
                                 (List A))))])))

(defn register
  "Register List and its constructors into the context."
  [ctx]
  (register-inductive ctx list-decl))

(defn elim
  "Eliminator (recursor) for List.
   Usage: (elim motive nil-case cons-case xs)
   where motive is a Lambda over the scrutinee,
   nil-case and cons-case correspond to each constructor,
   and xs is the list to eliminate."
  [motive nil-case cons-case xs]
  (ast/->Elim 'List motive [nil-case cons-case] xs))