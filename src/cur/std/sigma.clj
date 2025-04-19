;;-----------------------------------------------------------------
;; cur.std.sigma
;;
;; Standard library: Sigma type and constructors (Pair, fst, snd).
;;-----------------------------------------------------------------
(ns cur.std.sigma
  "Standard library: Sigma type and Pair constructors."
  (:require [cur.curnel.ast :as ast]
            [cur.curnel.parser :refer [parse-term]]
            [cur.curnel.checker :refer [equal-terms?]]))

(defn register
  "No-op registration for Sigma primitives."
  [ctx] ctx)

(defn SigmaType
  "Construct a Sigma AST: Sigma [param domain] codomain."
  [param domain codomain]
  (ast/->Sigma param domain codomain))

(defn pair
  "Construct a Pair AST."
  [fst snd]
  (ast/->Pair fst snd))

(defn fst
  "Construct a first-projection AST."
  [p]
  (ast/->Fst p))

(defn snd
  "Construct a second-projection AST."
  [p]
  (ast/->Snd p))