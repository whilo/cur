 (ns cur.meta
   "Public API namespace for Meta-Tactics combinators and meta‑DSL."
   (:require [cur.curnel.ntac.meta :as ntac-meta]
             [cur.curnel.ntac.core :as core]
             [cur.curnel.ast :as ast]))

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

;; Meta‑DSL for simple tactic macros
(defmacro by-intros
  "Introduce multiple Pi-binders in sequence."
  [& names]
  `(ntac-meta/sequence ~@(repeat (count names)
                         'cur.curnel.ntac.core/intro)))

(defmacro by-exact
  "Discharge goal by exact matching hypothesis sym."
  [sym]
  `(fn [state#]
     (cur.curnel.ntac.core/exact (ast/->Var '~sym) state#)))

(defmacro define-simple-macro
  "Define a simple meta-tactic named NAME that tries a sequence of tactics."
  [[name] & body]
  `(def ~name
     (ntac-meta/try-tac (ntac-meta/sequence ~@body))))

(defmacro define-tactical
  "Define a named tactical as a function from state to new states.
   Usage: (define-tactical (name) ((compose t1 t2) $ptz))."
  [[name] tactical-call]
  (let [[fn-expr _arg] tactical-call]
    `(def ~name
       (fn [state#]
         (~fn-expr state#)))))