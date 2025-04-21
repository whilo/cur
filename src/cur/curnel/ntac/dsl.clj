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
  (:require [cur.curnel.ntac.core :as core]))

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
  (let [fns# (vec (for [step# steps]
                    (if (seq? step#)
                      (let [f#    (first step#)
                            args# (rest step#)]
                        `(fn [st#] (~(symbol "cur.curnel.ntac.core" (name f#)) ~@args# st#)))
                      `(fn [st#] (~(symbol "cur.curnel.ntac.core" (name step#)) st#)))))]
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
