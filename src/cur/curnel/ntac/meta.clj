;;-----------------------------------------------------------------
;; cur.curnel.ntac.meta
;;
;; Meta-tactics layer: combinators for scripting NTac proofs.
;;-----------------------------------------------------------------
(ns cur.curnel.ntac.meta
  "Meta-tactics layer: combinators for scripting NTac proofs."
  (:require [cur.curnel.ntac.core :as core]))

;; Re-export core combinators
(def ret core/tac-return)
(def fail core/tac-fail)
(def bind core/tac-bind)
(def plus core/tac-plus)

(defn try-tac
  "Try the tactic on the state; on failure, return the original state as singleton."
  [tac]
  (fn [state]
    (let [res (tac state)]
      (if (seq res)
        res
        [state]))))

(defn once
  "Run tac but emit at most one resulting state."
  [tac]
  (fn [state]
    (let [res (tac state)]
      (if (seq res)
        [(first res)]
        []))))

(defn when-tac
  "Run tac only if pred holds on the state; otherwise, return the state."
  [pred tac]
  (fn [state]
    (if (pred state)
      (tac state)
      [state])))

(defn unless-tac
  "Run tac only if pred does not hold on the state; otherwise, return the state."
  [pred tac]
  (fn [state]
    (if (pred state)
      [state]
      (tac state))))

(defn sequence
  "Sequentially compose a series of tactics."
  [& tacs]
  (if (empty? tacs)
    core/tac-return
    (reduce core/tac-bind tacs)))

(defn choice
  "Non-deterministic choice among tactics."
  [& tacs]
  (if (empty? tacs)
    core/tac-fail
    (reduce core/tac-plus tacs)))