(ns cur.curnel.inductive-test
  "Tests for inductive type declarations and registration."
  (:require [clojure.test :refer :all]
            [cur.curnel.parser :refer [parse-term]]
            [cur.curnel.checker :refer [register-inductive type-of]]
            [cur.curnel.ast :as ast])
  (:import [cur.curnel.ast InductiveType Constructor Universe]))

(deftest parse-inductive
  (let [t (parse-term '(Inductive Bool [] (Type 0) [True Bool] [False Bool]))]
    (is (instance? InductiveType t))
    (is (= 'Bool (:name t)))
    (is (empty? (:params t)))
    (is (instance? Universe (:result-type t)))
    (is (= 2 (count (:constructors t))))
    (is (every? #(instance? Constructor %) (:constructors t)))))

(deftest register-inductive-basic
  (let [decl (parse-term '(Inductive Bool [] (Type 0) [True Bool] [False Bool]))
        ctx  (register-inductive {} decl)]
     ;; The constructors True and False should have type Bool
    (is (= (ast/->Var 'Bool) (type-of ctx (ast/->Var 'True))))
    (is (= (ast/->Var 'Bool) (type-of ctx (ast/->Var 'False))))))