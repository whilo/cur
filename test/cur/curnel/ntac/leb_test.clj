;;-----------------------------------------------------------------
;; clj-cur/test/cur/curnel/ntac/leb_test.clj
;;
;; Placeholder for tests ported from Racket `leb.rkt`.
;; TODO: implement `simpl` tests on leb, check correctness of `leb` recursor.
;;-----------------------------------------------------------------
;;-----------------------------------------------------------------
;; clj-cur/test/cur/curnel/ntac/leb_test.clj
;;
;; Tests for Nat ≤ Nat inductive (`le`) in std-ctx.
;;-----------------------------------------------------------------
 (ns cur.curnel.ntac.leb-test
   (:require [clojure.test :refer :all]
             [cur.curnel.ast :as ast]
             [cur.curnel.checker :as chk]
             [cur.std :refer [std-ctx]]
             [cur.std.nat :refer [leb]]))

;; Ensure `le` is registered as a 2-parameter Pi-type in std-ctx
(deftest le-registered
  (let [le-ty (get std-ctx 'le)]
    ;; outer Pi: parameter n
    (is (instance? ast/Pi le-ty))
    (let [n-param (:param le-ty)
          dom     (:domain le-ty)
          rest    (:codomain le-ty)]
      (is (= 'n n-param))
      (is (= (ast/->Var 'Nat) dom))
      ;; inner Pi should bind m
      (is (instance? ast/Pi rest))
      (let [m-param (:param rest)
            dom2    (:domain rest)
            ret     (:codomain rest)]
        (is (= 'm m-param))
        (is (= (ast/->Var 'Nat) dom2))
        ;; return type should be (le n m)
        (is (= (ast/->App (ast/->App (ast/->Var 'le)
                                     (ast/->Var 'n))
                          (ast/->Var 'm))
               ret))))))

;; Test boolean ≤ function via simplification (normalization)
(deftest leb-behavior
  (let [z    (ast/->Var 'z)
        one  (ast/->App (ast/->Var 's) z)
        two  (ast/->App (ast/->Var 's) one)
        four (ast/->App (ast/->Var 's) two)
        ;; 2 ≤ 2 => True
        res1 (chk/nf std-ctx (ast/->App (ast/->App (ast/->Var 'leb) two) two))
        ;; 2 ≤ 4 => True
        res2 (chk/nf std-ctx (leb two four))
        ;; 4 ≤ 2 => False
        res3 (chk/nf std-ctx (leb four two))]
    (is (= (ast/->Var 'True) res1))
    (is (= (ast/->Var 'True) res2))
    (is (= (ast/->Var 'False) res3))))
