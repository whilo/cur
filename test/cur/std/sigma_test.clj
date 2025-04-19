;;-----------------------------------------------------------------
;; cur.std.sigma-test
;;
;; Tests for the Sigma and Pair functions in the standard library.
;;-----------------------------------------------------------------
 (ns cur.std.sigma-test
   "Tests for the Sigma module in the standard library."
   (:require [clojure.test :refer :all]
             [cur.std.sigma :as sigma]
             [cur.curnel.parser :refer [parse-term]]
             [cur.curnel.ast :as ast]
             [cur.curnel.checker :refer [equal-terms?]])
   (:import [cur.curnel.ast Pair Fst Snd]))

(deftest sigma-type-fn-works
  (let [s1 (sigma/SigmaType 'x (parse-term '(Type 0)) (parse-term '(Type 1)))
        s2 (parse-term '(Sigma [x (Type 0)] (Type 1)))]
    (is (equal-terms? {} s1 s2))))

(deftest pair-and-projections-fn-works
  (let [p1  (parse-term '(pair a b))
        p2  (sigma/pair (ast/->Var 'a) (ast/->Var 'b))
        fp1 (sigma/fst p1)
        sp1 (sigma/snd p1)
        fp2 (parse-term '(fst (pair a b)))
        sp2 (parse-term '(snd (pair a b)))]
    (is (instance? Pair p2))
    (is (equal-terms? {} p1 p2))
    (is (= fp1 fp2))
    (is (= sp1 sp2))))