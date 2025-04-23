;;-----------------------------------------------------------------
;; clj-cur/test/cur/curnel/ntac/rewrite_core_test.clj
;;
;; Placeholder for tests ported from Racket `rewrite.rkt` (core rewrite and elimination).
;; TODO: implement core rewrite and elimination tests (mult-S-1, sym, etc.).
;;-----------------------------------------------------------------
 (ns cur.curnel.ntac.rewrite-core-test
   "Unit tests for core inductive eliminator functionality."
   (:require [clojure.test :refer :all]
             [cur.curnel.ast :as ast]
             [cur.curnel.checker :as chk]
             [cur.curnel.ntac.core :as core]
             [cur.std :refer [std-ctx]]))

(deftest ^:skip nat-elim-zero-case
  (testing "Nat eliminator zero-case returns the first method"
    (let [z      (ast/->Var 'z)
          zcase  (ast/->Var 'zc)
          scase  (ast/->Var 'sc)
          motive (ast/->Lambda 'x (ast/->Var 'Nat) (ast/->Var 'Nat))
          elim   (ast/->Elim 'Nat motive [zcase scase] z)
          result (chk/nf std-ctx elim)]
      (is (= zcase result)))))

(deftest ^:skip nat-elim-succ-case
  (testing "Nat eliminator succ-case applies second method and recurses"
    (let [z        (ast/->Var 'z)
          scase    (ast/->Var 'sc)
          zcase    (ast/->Var 'zc)
          motive   (ast/->Lambda 'x (ast/->Var 'Nat) (ast/->Var 'Nat))
          target   (ast/->App (ast/->Var 's) z)
          elim     (ast/->Elim 'Nat motive [zcase scase] target)
          result   (chk/nf std-ctx elim)
           ;; expected: (App (App scase z) zcase)
          expected (ast/->App (ast/->App scase z) zcase)]
      (is (= expected result)))))

(deftest ^:skip rewrite-tactic-basic
  (testing "Tactic-level rewrite replaces LHS with RHS in goal expected"
    (let [A        (ast/->Var 'A)
          x        (ast/->Var 'x)
          y        (ast/->Var 'y)
          ;; equality proof H: (== A x y)
          eqp      (ast/->App (ast/->App (ast/->App (ast/->Var '==) A) x) y)
          ctx1     (assoc std-ctx 'H eqp)
          ;; goal term and expected start as (plus x x)
          term     (ast/->App (ast/->App (ast/->Var 'plus) x) x)
          goal     (core/->Goal ctx1 term term nil)
          state    (core/->TacticState [goal] [])
          [st1]    (core/rewrite (ast/->Var 'H) state)
          g1       (first (:goals st1))
          got      (:expected g1)
          expected (ast/->App (ast/->App (ast/->Var 'plus) y) y)]
      (is (= expected got)))))

(deftest ^:skip rewriteR-tactic-basic
  (testing "Tactic-level reverse rewrite swaps RHS with LHS in goal expected"
    (let [A        (ast/->Var 'A)
          x        (ast/->Var 'x)
          y        (ast/->Var 'y)
          ;; equality proof H: (== A x y)
          eqp      (ast/->App (ast/->App (ast/->App (ast/->Var '==) A) x) y)
          ctx1     (assoc std-ctx 'H eqp)
          ;; goal expected is (plus y y), should revert to (plus x x)
          plus-y   (ast/->App (ast/->App (ast/->Var 'plus) y) y)
          goal     (core/->Goal ctx1 nil plus-y nil)
          state    (core/->TacticState [goal] [])
          [st1]    (core/rewriteR (ast/->Var 'H) state)
          g1       (first (:goals st1))
          got      (:expected g1)
          expected (ast/->App (ast/->App (ast/->Var 'plus) x) x)]
      (is (= expected got)))))

(deftest ^:skip rewrite-mult-S-1
  (testing "Rewrite mult m (plus 1 n) to mult m m using H"
    (let [Nat     (ast/->Var 'Nat)
          n       (ast/->Var 'n)
          m       (ast/->Var 'm)
          z       (ast/->Var 'z)
          one     (ast/->App (ast/->Var 's) z)
          plus1n  (ast/->App (ast/->App (ast/->Var 'plus) one) n)
          sn      (ast/->App (ast/->Var 's) n)
          eqp     (ast/->App (ast/->App (ast/->App (ast/->Var '==) Nat) m) sn)
          ctx1    (-> std-ctx (assoc 'n Nat) (assoc 'm Nat) (assoc 'H eqp))
          ;; initial expected: == Nat (mult m plus1n) (mult m m)
          eq1     (ast/->App (ast/->App (ast/->App (ast/->Var '==) Nat)
                                        (ast/->App (ast/->App (ast/->Var 'mult) m) plus1n))
                             (ast/->App (ast/->App (ast/->Var 'mult) m) m))
          goal    (core/->Goal ctx1 nil eq1 nil)
          state   (core/->TacticState [goal] [])
          [st1]   (core/rewrite (ast/->Var 'H) state)
          got     (-> st1 :goals first :expected)
          ;; expected after rewrite: == Nat (mult (s n) plus1n) (mult m m)
          eq2     (ast/->App (ast/->App (ast/->App (ast/->Var '==) Nat)
                                        (ast/->App (ast/->App (ast/->Var 'mult) sn) plus1n))
                             (ast/->App (ast/->App (ast/->Var 'mult) sn) sn))]
      (is (= eq2 got)))))

(deftest ^:skip rewrite-mult-S-1-flipped-H
  (testing "rewrite with flipped H leaves expected unchanged"
    (let [Nat     (ast/->Var 'Nat)
          n       (ast/->Var 'n)
          m       (ast/->Var 'm)
          z       (ast/->Var 'z)
          one     (ast/->App (ast/->Var 's) z)
          plus1n  (ast/->App (ast/->App (ast/->Var 'plus) one) n)
          ;; flipped H: (== Nat (s n) m)
          eqp     (ast/->App (ast/->App (ast/->App (ast/->Var '==) Nat)
                                        (ast/->App (ast/->Var 's) n))
                             m)
          ctx1    (-> std-ctx (assoc 'n Nat) (assoc 'm Nat) (assoc 'H eqp))
          ;; initial expected: == Nat (mult m plus1n) (mult m m)
          eq1     (ast/->App (ast/->App (ast/->App (ast/->Var '==) Nat)
                                        (ast/->App (ast/->App (ast/->Var 'mult) m) plus1n))
                             (ast/->App (ast/->App (ast/->Var 'mult) m) m))
          state   (core/->TacticState [(core/->Goal ctx1 nil eq1 nil)] [])
          [st1]   (core/rewrite (ast/->Var 'H) state)
          got     (-> st1 :goals first :expected)]
      (is (= eq1 got)))))

;; Le eliminator zero-case: le-n applied to n
(deftest ^:skip le-elim-zero-case
  (testing "Le eliminator zero-case returns le-n"
    (let [le-n   (ast/->Var 'le-n)
          le-s   (ast/->Var 'le-s)
          ;; motive: λ [m : Nat] (le m m)
          motive (ast/->Lambda 'm (ast/->Var 'Nat)
                               (ast/->App (ast/->App (ast/->Var 'le)
                                                     (ast/->Var 'm))
                                          (ast/->Var 'm)))
          ;; eliminate on Var 'le-n
          elim   (ast/->Elim 'le motive [le-n le-s] le-n)
          result (chk/nf std-ctx elim)]
      (is (= le-n result)))))

;; Le eliminator succ-case: le-s applied to (le-n n)
(deftest ^:skip le-elim-succ-case
  (testing "Le eliminator succ-case returns le-s"
    (let [le-n   (ast/->Var 'le-n)
          le-s   (ast/->Var 'le-s)
          ;; motive: λ [m : Nat] (le m (s m))
          motive (ast/->Lambda 'm (ast/->Var 'Nat)
                               (ast/->App (ast/->App (ast/->Var 'le)
                                                     (ast/->Var 'm))
                                          (ast/->App (ast/->Var 's) (ast/->Var 'm))))
          ;; eliminate on Var 'le-s
          elim   (ast/->Elim 'le motive [le-n le-s] le-s)
          result (chk/nf std-ctx elim)
          expected le-s]
      (is (= expected result)))))
