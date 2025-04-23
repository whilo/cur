;;-----------------------------------------------------------------
;; clj-cur/test/cur/curnel/ntac/issue104_test.clj
;;
;; Placeholder for tests ported from Racket `issue104.rkt`.
;; TODO: implement metaprogramming tests for simple macros (mytac1, mytac2, mytac3).
;;-----------------------------------------------------------------
 (ns cur.curnel.ntac.issue104-test
   "Tests for issue #104: simple metaprogramming macros."
   (:require [clojure.test :refer :all]
             [cur.meta :refer [by-intros by-exact define-simple-macro define-tactical sequence]]
             [cur.curnel.ntac.dsl :refer [proof]]
             [cur.curnel.ast :as ast]
             [cur.std :refer [std-ctx]]))

(define-simple-macro (mytac1)
  (by-intros P p)
  (by-exact p))
(define-simple-macro (mytac2)
  (by-intros P p)
  (by-exact p))
(define-tactical (mytac3)
  ((sequence (by-intros P p) (by-exact p)) $ptz))

(deftest issue104-macros
  (testing "mytac1/2/3 discharge a simple forall goal"
    (let [P        (ast/->Var 'P)
          p        (ast/->Var 'p)
          term     (ast/->Lambda 'P (ast/->Universe 0)
                       (ast/->Lambda 'p P p))
          expected (ast/forall* [['P (ast/->Universe 0)] ['p P]] P)
          pf1      (proof std-ctx term expected mytac1)
          pf2      (proof std-ctx term expected mytac2)
          pf3      (proof std-ctx term expected mytac3)]
      (is (= [p] pf1))
      (is (= [p] pf2))
      (is (= [p] pf3)))))