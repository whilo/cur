;;-----------------------------------------------------------------
;; cur.curnel.ast
;;
;; Abstract Syntax Tree (AST) definitions for cur kernel terms
;;-----------------------------------------------------------------

(ns cur.curnel.ast)

;; A marker protocol for all term types
(defprotocol Term)

;; A variable reference
(defrecord Var [name]
  Term)

;; A universe (type of types) with a numeric level
(defrecord Universe [level]
  Term)

;; A dependent function type: Π (param : domain) -> codomain
(defrecord Pi [param domain codomain]
  Term)

;; A lambda abstraction: λ (param : param-type) . body
(defrecord Lambda [param param-type body]
  Term)

;; Function application: fn arg
(defrecord App [fn arg]
  Term)

;; A dependent pair type: Σ (param : fst-type) -> snd-type
(defrecord Sigma [param fst-type snd-type]
  Term)

;; Construction of a dependent pair: pair fst snd
(defrecord Pair [fst snd]
  Term)

;; First projection: fst p
(defrecord Fst [pair]
  Term)

;; Second projection: snd p
(defrecord Snd [pair]
  Term)

;; A let-binding: let [name = value] in body
(defrecord Let [name value body]
  Term)

;; A constructor declaration for an inductive type
(defrecord Constructor [name type])

;; An inductive type declaration
(defrecord InductiveType [name params result-type constructors])

;; Eliminator (fold) for an inductive type
(defrecord Elim [name motive methods target]
  Term)

;;-----------------------------------------------------------------
;; Sugar helpers for constructing nested Pi and arrow types
;;-----------------------------------------------------------------
(defn forall*
  "Build nested Pi-types from a seq of [id type] bindings and a body term."
  [bindings body]
  (reduce (fn [b [id ty]]
            (->Pi id ty b))
          body
          (reverse bindings)))

(defn arrow*
  "Sugar: non-dependent arrow type A1 -> A2 -> ... -> An as nested Pi with fresh names."
  [& types]
  (let [args (butlast types)
        ret  (last types)]
    (reduce (fn [b dom]
              (let [v (gensym "_arg")]
                (->Pi v dom b)))
            ret
            (reverse args))))