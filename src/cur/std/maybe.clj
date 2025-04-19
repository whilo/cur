;;-----------------------------------------------------------------
;; cur.std.maybe
;;
;; Standard library: Maybe (optional) type and constructors.
;;-----------------------------------------------------------------
 (ns cur.std.maybe
   "Standard library: Maybe (optional) type, constructors, and eliminator."
   (:require [cur.curnel.parser :refer [parse-term]]
             [cur.curnel.checker :refer [register-inductive]]
             [cur.curnel.ast :as ast]))

(def maybe-decl
  "Inductive declaration for Maybe, parameterized by type A."
  (parse-term
   '(Inductive Maybe
               [[A (Type 0)]]
               (Type 0)
               [none (Pi [A (Type 0)] (Maybe A))]
               [some (Pi [A (Type 0)] (Pi [a A] (Maybe A)))])))

(defn register
  "Register Maybe and its constructors into the context."
  [ctx]
  (register-inductive ctx maybe-decl))

(defn elim
  "Eliminator (recursor) for Maybe.
   Usage: (elim motive none-case some-case mx)
   where motive is a Lambda over the scrutinee,
   none-case and some-case correspond to constructors,
   and mx is the Maybe value to eliminate."
  [motive none-case some-case mx]
  (ast/->Elim 'Maybe motive [none-case some-case] mx))