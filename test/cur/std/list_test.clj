;;-----------------------------------------------------------------
;; cur.std.list-test
;;
;; Tests for the List module in the standard library.
;;-----------------------------------------------------------------
(ns cur.std.list-test
  "Tests for the List module in the standard library."
  (:require [clojure.test :refer :all]
            [cur.std.list :as list]
            [cur.curnel.parser :refer [parse-term]]
            [cur.curnel.checker :refer [type-of]]
            [cur.curnel.ast :as ast])
  (:import [cur.curnel.ast App Elim]))

(deftest list-constructors-have-correct-types
  (let [ctx (list/register {})]
    (is (= (parse-term '(Pi [A (Type 0)] (List A)))
           (type-of ctx (parse-term 'nil))))
    (is (= (parse-term '(Pi [A (Type 0)]
                            (Pi [x A]
                                (Pi [xs (List A)]
                                    (List A)))))
           (type-of ctx (parse-term 'cons))))))

(deftest eliminator-list-type
  (let [ctx    (list/register {})
         ;; Define a simple motive: λ [xs (List A)] A
        motive (ast/->Lambda 'xs (ast/->App (ast/->Var 'List) (ast/->Var 'A)) (ast/->Var 'A))
        m-nil  (ast/->Var 'nil)
        m-cons (ast/->Lambda 'x (ast/->Var 'A)
                             (ast/->Lambda 'xs (ast/->App (ast/->Var 'List) (ast/->Var 'A))
                                           (ast/->Var 'A)))
        target (ast/->Var 'nil)
        elim   (ast/->Elim 'List motive [m-nil m-cons] target)
        ty     (type-of ctx elim)]
    (is (instance? App ty))
    (is (= motive (:fn ty)))
    (is (= target (:arg ty)))))