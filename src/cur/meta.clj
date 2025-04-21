(ns cur.meta
  "Public API namespace for Meta-Tactics combinators."
  (:require [cur.curnel.ntac.meta :as ntac-meta]))

;; Core combinators re-exported under cur.meta
(def ret      ntac-meta/ret)
(def fail     ntac-meta/fail)
(def bind     ntac-meta/bind)
(def plus     ntac-meta/plus)

(def try-tac  ntac-meta/try-tac)
(def once     ntac-meta/once)
(def when-tac ntac-meta/when-tac)
(def unless-tac ntac-meta/unless-tac)

(def sequence ntac-meta/sequence)
(def choice   ntac-meta/choice)