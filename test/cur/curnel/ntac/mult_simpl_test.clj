;;-----------------------------------------------------------------
;; clj-cur/test/cur/curnel/ntac/mult_simpl_test.clj
;;
;; Test that mult recursor simplifies properly: (mult 0 n) → n.
;;-----------------------------------------------------------------
(ns cur.curnel.ntac.mult-simpl-test
  (:require [clojure.test :refer :all]
            [cur.curnel.ast :as ast]
            [cur.curnel.ntac.core :as core]
            [cur.curnel.ntac.ctx :as ctx]
            [cur.std :refer [std-ctx]]
            [cur.std.nat :refer [mult]]))

(deftest mult-zero-left-simpl
  (let [;; AST vars for z and n
        zero (ast/->Var 'z)
        n    (ast/->Var 'n)
         ;; term: (mult z n) using the mult builder
        term (mult zero n)
        goal (core/->Goal std-ctx nil term nil)
        state (core/->TacticState [goal] [])
        [st] (core/simpl state)
        g    (first (:goals st))]
    (is (= zero (:expected g)))))