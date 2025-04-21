(ns cur.curnel.ntac.meta-test
  "Unit tests for the MetaNTac combinators."
  (:require [clojure.test :refer :all]
            [cur.curnel.ntac.core :as core]
            [cur.curnel.ntac.meta :as meta]))

(deftest ret-fail-bind-plus-tests
  (let [state (core/->TacticState [] [])]
    (is (= (meta/ret state) [state]))
    (is (= (meta/fail state) []))
    (is (= ((meta/bind meta/ret meta/ret) state) (meta/ret state)))
    (is (= ((meta/plus meta/ret meta/fail) state) (meta/ret state)))
    (is (= ((meta/plus meta/fail meta/ret) state) (meta/ret state)))))

(deftest try-tac-test
  (let [always-fail (fn [s] [])
        always-ok   (fn [s] [:ok])]
    (is (= ((meta/try-tac always-ok) 1) [:ok]))
    (is (= ((meta/try-tac always-fail) 1) [1]))))

(deftest once-test
  (let [multi (fn [x] [(* x 2) (* x 3)])
        none  (fn [x] [])]
    (is (= ((meta/once multi) 2) [4]))
    (is (= ((meta/once none) 2) []))))

(deftest when-unless-test
  (let [inc-tac (fn [x] [(inc x)])]
    (is (= ((meta/when-tac even? inc-tac) 2) [3]))
    (is (= ((meta/when-tac odd? inc-tac) 2) [2]))
    (is (= ((meta/unless-tac even? inc-tac) 2) [2]))
    (is (= ((meta/unless-tac odd? inc-tac) 2) [3]))))

(deftest sequence-choice-test
  (let [t1 (fn [x] [(inc x)])
        t2 (fn [x] [(* x 2)])]
    (is (= ((apply meta/sequence [t1 t2]) 1) [4]))
    (is (= ((apply meta/choice   [t1 t2]) 1) [2 2]))))

(deftest bind-numeric-test
  (let [t1 (fn [x] [(inc x) (* x 2)])
        t2 (fn [y] [(dec y)])]
    (is (= ((meta/bind t1 t2) 2) [2 3]))))