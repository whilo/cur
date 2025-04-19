;;-----------------------------------------------------------------
;; cur.std.nat
;;
;; Standard library: natural numbers (Nat) and basic constructors.
;;-----------------------------------------------------------------
 (ns cur.std.nat
   "Standard library: Nat type, zero, successor, and eliminator."
   (:require [cur.curnel.parser :refer [parse-term]]
             [cur.curnel.checker :refer [register-inductive]]
             [cur.curnel.ast :as ast]))

(def nat-decl
  "Inductive declaration for natural numbers."
  (parse-term
   '(Inductive Nat [] (Type 0)
               [z Nat]
               [s (Pi [x Nat] Nat)])))

(defn register
  "Register Nat and its constructors into the context."
  [ctx]
  (register-inductive ctx nat-decl))

(defn elim
  "Eliminator (recursor) for Nat.
   Usage: (elim motive z-case s-case n)
   where motive is a Lambda over the scrutinee,
   z-case and s-case correspond to constructors,
   and n is the Nat value to eliminate."
  [motive z-case s-case n]
  (ast/->Elim 'Nat motive [z-case s-case] n))