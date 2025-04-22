;;-----------------------------------------------------------------
;; cur.std.integration-test
;;
;; Integration tests for the assembled standard-library context.
;;-----------------------------------------------------------------
(ns cur.std.integration-test
  "Integration tests for the global std-ctx from cur.std."
  (:require [clojure.test :refer :all]
            [cur.std :refer [std-ctx]]
            [cur.std.list :as list]
            [cur.curnel.parser :refer [parse-term]]
            [cur.curnel.checker :refer [type-of nf]]
            [cur.curnel.ast :as ast])
  (:import [cur.curnel.ast App]))

(deftest std-ctx-basic-constructors
  (is (= (ast/->Var 'Bool)
         (type-of std-ctx (parse-term 'True))))
  (is (= (parse-term '(List Nat))
         (type-of std-ctx (parse-term '(nil Nat)))))
  (is (= (ast/->Var 'Nat)
         (type-of std-ctx (parse-term 'z)))))

(deftest std-ctx-list-elim
  (let [motive (ast/->Lambda 'xs
                             (ast/->App (ast/->Var 'List) (ast/->Var 'Nat))
                             (ast/->Var 'Nat))
        m-nil  (ast/->Var 'nil)
        m-cons (ast/->Lambda 'x (ast/->Var 'Nat)
                             (ast/->Lambda 'xs
                                           (ast/->App (ast/->Var 'List) (ast/->Var 'Nat))
                                           (ast/->Var 'Nat)))
        target (ast/->Var 'nil)
        elim   (list/elim motive m-nil m-cons target)
        ty     (type-of std-ctx elim)]
    (is (instance? App ty))
    (is (= motive (:fn ty)))
    (is (= target (:arg ty)))))

(deftest std-ctx-plus-mult-normalization
  "Integration tests for plus and mult normalization in std-ctx."
  (testing "Plus normalization"
    (is (= (parse-term 'z)
           (nf std-ctx (parse-term '(plus z z)))))
    (is (= (parse-term '(s z))
           (nf std-ctx (parse-term '(plus z (s z)))))))
  (testing "Mult normalization"
    (is (= (parse-term 'z)
           (nf std-ctx (parse-term '(mult z (s z))))))
    (is (= (parse-term '(s z))
           (nf std-ctx (parse-term '(mult (s z) (s z))))))))