;;-----------------------------------------------------------------
;; cur.curnel.ntac.ctx
;;
;; Simple context representation for NTac proofs.
;;-----------------------------------------------------------------
(ns cur.curnel.ntac.ctx
  "Context operations for NTac proof states."
  (:require [cur.curnel.ast :as ast]))

;; A proof context holds a list of bindings (id -> type), innermost last.
(defrecord Ctx [items])

(defn mk-empty-ctx
  "Create an empty proof context."
  []
  (->Ctx []))

(defn ctx-add
  "Add a binding id with type ty into context (innermost scope)."
  [^Ctx ctx id ty]
  (->Ctx (conj (:items ctx) {:id id :type ty})))

(defn ctx-ids
  "Return list of ids in context, outermost first."  ; reverse items
  [^Ctx ctx]
  (mapv :id (:items ctx)))

(defn ctx-types
  "Return list of types in context, outermost first."
  [^Ctx ctx]
  (mapv :type (:items ctx)))

(defn ctx-lookup
  "Look up the type for id in context. Returns nil if not found.
  Supports both NTac Ctx records and plain symbol->type maps."
  [ctx id]
  (if (instance? Ctx ctx)
    (some (fn [item]
            (when (= id (:id item))
              (:type item)))
          (reverse (:items ctx)))
    (get ctx id)))

(defn ctx-remove
  "Remove the innermost binding with given id."
  [^Ctx ctx id]
  (->Ctx (->> (:items ctx)
              reverse
              (remove (fn [item] (= id (:id item))))
              (reverse)
              vec)))
;;-----------------------------------------------------------------
;; Extended context operations for porting Racket NTac tests
;;-----------------------------------------------------------------
;; Add multiple bindings in one go: ids and tys are sequences of equal length
(defn ctx-adds
  "Add multiple bindings (id -> type) into context in order."
  [ctx ids tys]
  (reduce (fn [c [id ty]] (ctx-add c id ty))
          ctx
          (map vector ids tys)))

;; Convert context to a sequence of [id type] pairs, innermost first
(defn ctx->env
  "Return context as a vector of [id type] pairs, with the most recent binding first."
  [ctx]
  (->> (:items ctx)
       (map (fn [{:keys [id type]}] [id type]))
       reverse
       vec))
