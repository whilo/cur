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
  (let [ctx0    (register-inductive ctx nat-decl)
        ;; Register plus : Nat -> Nat -> Nat
        plus-ty (ast/->Pi 'n (ast/->Var 'Nat)
                          (ast/->Pi 'm (ast/->Var 'Nat) (ast/->Var 'Nat)))]
    (-> ctx0
        (assoc 'plus plus-ty)
        (assoc 'mult    ; multiplication: Nat -> Nat -> Nat
               (ast/->Pi 'm (ast/->Var 'Nat)
                         (ast/->Pi 'n (ast/->Var 'Nat) (ast/->Var 'Nat)))))))

(defn elim
  "Eliminator (recursor) for Nat.
   Usage: (elim motive z-case s-case n)
   where motive is a Lambda over the scrutinee,
   z-case and s-case correspond to constructors,
   and n is the Nat value to eliminate."
  [motive z-case s-case n]
  (ast/->Elim 'Nat motive [z-case s-case] n))

;; Addition on Nat via the recursor
(defn plus
  "Addition on Nat: plus n m = elim n (λ [x : Nat] Nat) m (λ [x : Nat] [pm : Nat] (s pm))."
  [n m]
  (elim
    ;; motive: λ [x : Nat] Nat
   (ast/->Lambda 'x (ast/->Var 'Nat) (ast/->Var 'Nat))
    ;; z-case: m
   m
    ;; s-case: λ [x : Nat] (λ [pm : Nat] (s pm))
   (ast/->Lambda 'x (ast/->Var 'Nat)
                 (ast/->Lambda 'pm (ast/->Var 'Nat)
                               (ast/->App (ast/->Var 's) (ast/->Var 'pm))))
    ;; target: n
   n))

;; Multiplication on Nat via the recursor
(defn mult
  "Multiplication on Nat: mult m n = elim m (λ [x : Nat] Nat) z (λ [x : Nat] [p : Nat] (plus n p)) m"
  [m n]
  (elim
    ;; motive: λ [x : Nat] Nat
   (ast/->Lambda 'x (ast/->Var 'Nat) (ast/->Var 'Nat))
    ;; z-case: 0 (z)
   (ast/->Var 'z)
    ;; s-case: λ [x : Nat] (λ [p : Nat] (plus n p))
   (ast/->Lambda 'x (ast/->Var 'Nat)
                 (ast/->Lambda 'p (ast/->Var 'Nat)
                               (plus n (ast/->Var 'p))))
    ;; target: m
   m))