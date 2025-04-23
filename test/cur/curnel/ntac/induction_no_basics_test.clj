;;-----------------------------------------------------------------
;; clj-cur/test/cur/curnel/ntac/induction_no_basics_test.clj
;;
;; Placeholder for tests ported from Racket `Induction-no-Basics.rkt`.
;; TODO: implement ML-based induction tests for plus, minus, mult.
;;-----------------------------------------------------------------
 (ns cur.curnel.ntac.induction-no-basics-test
   "ML-style induction proofs for plus, minus, and mult from Induction-no-Basics.rkt."
   (:require [clojure.test :refer :all]
             [cur.curnel.ast :as ast]
             [cur.curnel.parser :refer [parse-term]]
             [cur.curnel.ntac.dsl :refer [proof by-induction]]
             [cur.curnel.ntac.core :refer [rewrite reflexivity]]
             [cur.std :refer [std-ctx]]
             [cur.std.nat :refer [plus mult]]))

;; ML-style induction tests (plus-n-0, minus-diag, mult_0_r) are pending
(deftest ^:skip plus-n-0-ml
  (testing "ML-style induction tests pending DSL helper implementation"
    (is true)))