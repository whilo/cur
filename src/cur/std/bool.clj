;;-----------------------------------------------------------------
;; cur.std.bool
;;
;; Standard library: Bool type and constructors.
;;-----------------------------------------------------------------
 (ns cur.std.bool
   "Standard library: Bool type, constructors, and eliminator."
   (:require [cur.curnel.parser :refer [parse-term]]
             [cur.curnel.checker :refer [register-inductive]]
             [cur.curnel.ast :as ast]))

(def bool-decl
  "Inductive declaration for Bool."
  (parse-term
   '(Inductive Bool [] (Type 0)
               [True Bool]
               [False Bool])))

(defn register
  "Register Bool and its constructors into the context."
  [ctx]
  (register-inductive ctx bool-decl))

(defn elim
  "Eliminator (recursor) for Bool.
   Usage: (elim motive true-case false-case b)
   where motive is a Lambda over the scrutinee,
   true-case and false-case correspond to constructors,
   and b is the Bool value to eliminate."
  [motive true-case false-case b]
  (ast/->Elim 'Bool motive [true-case false-case] b))