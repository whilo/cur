;;-----------------------------------------------------------------
;; cur.curnel.ntac.dsl
;;
;; DSL for scripting NTac tactics in Clojure.
;; Provides macros for sequential and named proof scripts.
;;-----------------------------------------------------------------
;;-----------------------------------------------------------------
;; cur.curnel.ntac.dsl
;;
;; DSL for scripting NTac tactics in Clojure.
;; Provides macros for sequential and named proof scripts.
;;-----------------------------------------------------------------
(ns cur.curnel.ntac.dsl
  "User-facing macros for proof scripting with NTac."
  (:require [cur.curnel.ntac.core :as core]
            [cur.curnel.ntac.meta :as meta]
            [cur.curnel.ast :as ast]))

;; Macro: proof
(defmacro proof
  "Run a sequence of tactics on an initial goal and return the proof vector.
   Usage:
     (proof ctx term expected
       intro
       (apply f)
       exact)
   Throws if goals remain at end."
  [ctx term expected & steps]
  (let [wrap-arg# (fn [a#]
                    (if (symbol? a#)
                      `(ast/->Var '~a#)
                      a#))
        fns#     (vec
                   (for [step# steps]
                     (if (seq? step#)
                       (let [f#    (first step#)
                             args# (map wrap-arg# (rest step#))
                             ;; qualify core tactics, leave others unqualified
                             sym#  (if (ns-resolve 'cur.curnel.ntac.core f#)
                                     (symbol "cur.curnel.ntac.core" (name f#))
                                     f#)]
                         `(fn [st#] (~sym# ~@args# st#)))
                       (let [sym# (if (ns-resolve 'cur.curnel.ntac.core step#)
                                    (symbol "cur.curnel.ntac.core" (name step#))
                                    step#)]
                         `(fn [st#] (~sym# st#))))))]
    `(let [init#  (core/->TacticState [(core/->Goal ~ctx ~term ~expected nil)] [])
           tacts# ~fns#]
       (loop [states# [init#] tacts# tacts#]
         (if (empty? tacts#)
           (let [s# (first states#)]
             (when (seq (:goals s#))
               (throw (ex-info "Proof incomplete, goals remain" {:goals (:goals s#)})))
             (:proof s#))
           (let [t#   (first tacts#)
                 nxt# (mapcat t# states#)]
             (recur nxt# (subvec tacts# 1))))))))

;; Macro: defproof
 (defmacro defproof
   "Define a var with the result of a proof script at compile-time.
    Usage:
      (defproof my-thm ctx term expected
        intro
        (apply f)
        exact)
    Binds `my-thm` to the proof AST vector."
   [name ctx term expected & steps]
   `(def ~name (proof ~ctx ~term ~expected ~@steps)))

;; Macro: by-induction
(defmacro by-induction
  "Perform induction on hypothesis h, running base-tactics on the base case and step-tactics on the succ case."
  [h base-tactics step-tactics]
  (let [qual-tac (fn [tac]
                    (cond
                      (seq? tac)
                      (let [f#        (first tac)
                            raw-args# (rest tac)
                            wrap-arg# (fn [a#]
                                        (if (symbol? a#)
                                          `(ast/->Var '~a#)
                                          a#))
                            args#    (map wrap-arg# raw-args#)
                            sym#      (if (ns-resolve 'cur.curnel.ntac.core f#)
                                        (symbol "cur.curnel.ntac.core" (name f#))
                                        f#)]
                        `(fn [st#] (~sym# ~@args# st#)))

                      (symbol? tac)
                      (let [sym# (if (ns-resolve 'cur.curnel.ntac.core tac)
                                    (symbol "cur.curnel.ntac.core" (name tac))
                                    tac)]
                        `(fn [st#] (~sym# st#)))

                      :else tac))
        base-quals  (map qual-tac base-tactics)
        step-quals  (map qual-tac step-tactics)]
    `(fn [state#]
       (let [[base# step#]
             (core/induction (ast/->Var '~h) state#)
             base-res# ((meta/sequence ~@base-quals) base#)
             step-res# ((meta/sequence ~@step-quals) step#)]
         (concat base-res# step-res#)))))
