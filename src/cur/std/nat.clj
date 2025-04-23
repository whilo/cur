;;-----------------------------------------------------------------
;; cur.std.nat
;;
;; Standard library: natural numbers (Nat) and basic constructors.
;;-----------------------------------------------------------------
 (ns cur.std.nat
   "Standard library: Nat type, zero, successor, and recursors."
   (:require [cur.curnel.parser :refer [parse-term]]
             [cur.curnel.checker :refer [register-inductive]]
             [cur.curnel.ast :as ast])
  (:import [cur.curnel.ast Var App]))

(def nat-decl
  "Inductive declaration for natural numbers."
  (parse-term
   '(Inductive Nat [] (Type 0)
               [z Nat]
               [s (Pi [x Nat] Nat)])))

;;-----------------------------------------------------------------
;; natural-number less-or-equal relation (le)
;;-----------------------------------------------------------------
(def le-decl
  "Inductive declaration for `le`: Nat × Nat → Type 0 (n ≤ m)."
  (parse-term
   '(Inductive le [[n Nat] [m Nat]] (Type 0)
               [le-n (Pi [n Nat] (le n n))]
               [le-s (Pi [n Nat] (Pi [m Nat] (le n m) (le n (s m))))])))

(defn register
  "Register Nat and its constructors into the context."
  [ctx]
  (let [ctx0    (register-inductive ctx nat-decl)
        ctx1    (register-inductive ctx0 le-decl)
        ;; plus : Nat -> Nat -> Nat
        plus-ty  (ast/->Pi 'n (ast/->Var 'Nat)
                           (ast/->Pi 'm (ast/->Var 'Nat)
                                     (ast/->Var 'Nat)))
        ;; mult : Nat -> Nat -> Nat
        mult-ty (ast/->Pi 'm (ast/->Var 'Nat)
                          (ast/->Pi 'n (ast/->Var 'Nat)
                                    (ast/->Var 'Nat)))
        ;; leb  : Nat -> Nat -> Bool
        leb-ty  (ast/->Pi 'n (ast/->Var 'Nat)
                          (ast/->Pi 'm (ast/->Var 'Nat)
                                    (ast/->Var 'Bool)))]
    (-> ctx1
        (assoc 'plus plus-ty)
        (assoc 'mult mult-ty)
        (assoc 'leb  leb-ty))))

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

;; Boolean less-or-equal on Nat: returns True if n ≤ m, else False.
(defn leb
  "Boolean less-or-equal on Nat: returns True if n ≤ m, else False."
  [n m]
  (letfn [(to-int [t]
            (cond
              (and (instance? Var t) (= 'z (:name t)))
              0

              (and (instance? App t)
                   (instance? Var (.fn t))
                   (= 's (:name (.fn t))))
              (inc (to-int (.arg t)))

              :else
              (throw (ex-info "leb: malformed Nat term" {:term t}))))]
    (if (<= (to-int n) (to-int m))
      (ast/->Var 'True)
      (ast/->Var 'False))))