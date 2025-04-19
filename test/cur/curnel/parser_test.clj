(ns cur.curnel.parser-test
  (:require [clojure.test :refer :all]
            [cur.curnel.parser :refer [parse-term]])
  (:import [cur.curnel.ast Var Universe Pi Lambda App Sigma Pair Fst Snd Let Elim]))

(deftest parse-var
  (let [t (parse-term 'x)]
    (is (instance? Var t))
    (is (= 'x (:name t)))))

(deftest parse-universe
  (let [t (parse-term '(Type 2))]
    (is (instance? Universe t))
    (is (= 2 (:level t)))))

(deftest parse-pi
  (let [t (parse-term '(Pi [x (Type 0)] (Type 1)))]
    (is (instance? Pi t))
    (is (= 'x (:param t)))
    (is (instance? Universe (:domain t)))
    (is (= 0 (:level (:domain t))))
    (is (instance? Universe (:codomain t)))
    (is (= 1 (:level (:codomain t))))))

(deftest parse-lambda
  (let [t (parse-term '(lambda [y (Type 0)] y))]
    (is (instance? Lambda t))
    (is (= 'y (:param t)))
    (is (instance? Universe (:param-type t)))
    (is (= 0 (:level (:param-type t))))
    (is (instance? Var (:body t)))
    (is (= 'y (:name (:body t))))))

(deftest parse-app
  (let [t (parse-term '(f x y))]
    (is (instance? App t))
    (let [inner (:fn t)]
      (is (instance? App inner))
      (is (instance? Var (:fn inner)))
      (is (= 'f (:name (:fn inner))))
      (is (instance? Var (:arg inner)))
      (is (= 'x (:name (:arg inner)))))
    (is (instance? Var (:arg t)))
    (is (= 'y (:name (:arg t))))))

(deftest parse-sigma
  (let [t (parse-term '(Sigma [s (Type 0)] (Type 1)))]
    (is (instance? Sigma t))
    (is (= 's (:param t)))
    (is (instance? Universe (:fst-type t)))
    (is (= 0 (:level (:fst-type t))))
    (is (instance? Universe (:snd-type t)))
    (is (= 1 (:level (:snd-type t))))))

(deftest parse-pair-and-projections
  (let [p (parse-term '(pair a b))
        f (parse-term '(fst (pair a b)))
        s (parse-term '(snd (pair a b)))]
    (is (instance? Pair p))
    (is (instance? Var (:fst p)))
    (is (= 'a (:name (:fst p))))
    (is (instance? Var (:snd p)))
    (is (= 'b (:name (:snd p))))
    (is (instance? Fst f))
    (is (instance? Pair (:pair f)))
    (is (instance? Snd s))
    (is (instance? Pair (:pair s)))))

(deftest parse-let
  (let [t (parse-term '(let [z x] y))]
    (is (instance? Let t))
    (is (= 'z (:name t)))
    (is (instance? Var (:value t)))
    (is (= 'x (:name (:value t))))
    (is (instance? Var (:body t)))
    (is (= 'y (:name (:body t))))))

(deftest parse-elim
  (let [t (parse-term '(elim Bool P m1 m2 x))]
    (is (instance? Elim t))
    (is (= 'Bool (:name t)))
    (is (instance? Var (:motive t)))
    (is (= 'P (:name (:motive t))))
    (is (= 2 (count (:methods t))))
    (is (= 'm1 (:name (first (:methods t)))))
    (is (= 'm2 (:name (second (:methods t)))))
    (is (instance? Var (:target t)))
    (is (= 'x (:name (:target t))))))