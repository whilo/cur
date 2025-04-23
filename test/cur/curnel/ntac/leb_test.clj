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
   "Tests for Nat ≤ Nat inductive (`le`) and the boolean `leb` recursor."
   (:require [clojure.test :refer :all]
             [cur.curnel.ast :as ast]
             [cur.curnel.parser :refer [parse-term]]
             [cur.curnel.checker :as chk]
             [cur.std :refer [std-ctx]]
             [cur.std.nat :refer [leb]])
   (:import [cur.curnel.ast Pi]))

;; Ensure `le` is registered as a 2-parameter Pi-type in std-ctx
(deftest le-registered
  (let [le-ty (get std-ctx 'le)]
    ;; outer Pi: parameter n
    (is (instance? Pi le-ty))
    (let [n-param (:param le-ty)
          dom     (:domain le-ty)
          rest    (:codomain le-ty)]
      (is (= 'n n-param))
      (is (= (ast/->Var 'Nat) dom))
      ;; inner Pi should bind m
      (is (instance? Pi rest))
      (let [m-param (:param rest)
            dom2    (:domain rest)
            ret     (:codomain rest)]
        (is (= 'm m-param))
        (is (= (ast/->Var 'Nat) dom2))
        ;; return type is Universe 0 (Type 0)
        (is (= (ast/->Universe 0) ret))))))

(deftest leb-behavior
  (testing "Boolean less-or-equal recursor `leb` normalizes correctly"
    (let [two   (parse-term '(s (s z)))
          four  (parse-term '(s (s (s (s z)))))
          ;; test cases: 2 ≤ 2 → True, 2 ≤ 4 → True, 4 ≤ 2 → False
          res1  (chk/nf std-ctx (parse-term '(leb (s (s z)) (s (s z)))))
          res2  (chk/nf std-ctx (parse-term '(leb (s (s z)) (s (s (s (s z)))))))
          res3  (chk/nf std-ctx (parse-term '(leb (s (s (s (s z)))) (s (s z))))) ]
      (is (= (ast/->Var 'True)  res1))
      (is (= (ast/->Var 'True)  res2))
      (is (= (ast/->Var 'False) res3)))))
