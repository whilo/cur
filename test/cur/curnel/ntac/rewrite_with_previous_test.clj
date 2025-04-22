;;-----------------------------------------------------------------
;; clj-cur/test/cur/curnel/ntac/rewrite_with_previous_test.clj
;;
;; Placeholder for tests ported from Racket `rewrite-with-previous.rkt`.
;; TODO: implement rewrite-with-previous tests for plus and mult.
;;-----------------------------------------------------------------
;;-----------------------------------------------------------------
;; clj-cur/test/cur/curnel/ntac/rewrite_with_previous_test.clj
;;
;; Ported tests from Racket `rewrite-with-previous.rkt` for by-rewrite tactics
;;-----------------------------------------------------------------
;;-----------------------------------------------------------------
;; clj-cur/test/cur/curnel/ntac/rewrite_with_previous_test.clj
;;
;; Tests for using previously established equality proofs in rewrite
;;-----------------------------------------------------------------
(ns cur.curnel.ntac.rewrite-with-previous-test
  (:require [clojure.test :refer :all]
            [cur.curnel.ast :as ast]
            [cur.curnel.ntac.core :as core]
            [cur.std :refer [std-ctx]]
            [cur.std.nat :refer [plus mult]]))

;; Test: rewrite using a previous equality hypothesis
(deftest rewrite-with-previous-basic
  (testing "rewrite: mult (plus 0 n) m → mult n m using H"
    (let [Nat     (ast/->Var 'Nat)
          n       (ast/->Var 'n)
          m       (ast/->Var 'm)
          z       (ast/->Var 'z)
          ;; equality proof H: (== Nat (plus z n) n)
          eqp     (ast/->App (ast/->App (ast/->App (ast/->Var '==) Nat)
                                    (plus z n))
                          n)
          ;; initial context with n, m, and H
          ctx1    (-> std-ctx
                       (assoc 'n Nat)
                       (assoc 'm Nat)
                       (assoc 'H eqp))
          ;; initial goal expected: == Nat (mult (plus z n) m) (mult n m)
          lhs     (mult (plus z n) m)
          rhs     (mult n m)
          eq-ty   (ast/->App (ast/->App (ast/->App (ast/->Var '==) Nat)
                                        lhs)
                           rhs)
          state   (core/->TacticState [(core/->Goal ctx1 nil eq-ty nil)] [])
          ;; perform rewrite H
          [st1]   (core/rewrite (ast/->Var 'H) state)
          got     (-> st1 :goals first :expected)
          want    (ast/->App (ast/->App (ast/->App (ast/->Var '==) Nat)
                                         (mult n m))
                        (mult n m))]
      (is (= want got)))))