(ns cur.meta-test
  "Tests for the public cur.meta API (aliasing Meta-Tactics)."
  (:require [clojure.test :refer :all]
            [cur.meta :as meta]
            [cur.curnel.ntac.meta :as ntac]
            [cur.curnel.ntac.core :as core]))

(deftest alias-var-tests
  (is (identical? meta/ret      ntac/ret))
  (is (identical? meta/fail     ntac/fail))
  (is (identical? meta/bind     ntac/bind))
  (is (identical? meta/plus     ntac/plus))
  (is (identical? meta/try-tac  ntac/try-tac))
  (is (identical? meta/once     ntac/once))
  (is (identical? meta/when-tac ntac/when-tac))
  (is (identical? meta/unless-tac ntac/unless-tac))
  (is (identical? meta/sequence ntac/sequence))
  (is (identical? meta/choice   ntac/choice)))

(deftest basic-functionality
  (let [st (core/->TacticState [] [])]
    (is (= (meta/ret st) [st]))
    (is (= (meta/fail st) []))
    ;; sequence: ret then ret yields same state
    (is (= ((meta/sequence meta/ret meta/ret) st) [st]))
    ;; choice: ret or fail yields state
    (is (= ((meta/choice meta/ret meta/fail) st) [st]))))