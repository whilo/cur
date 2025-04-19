;;-----------------------------------------------------------------
;; cur.curnel.ntac.core
;;
;; Core definitions for the tactics engine (NTac) in Clojure port.
;;-----------------------------------------------------------------
(ns cur.curnel.ntac.core
  "Core tactics definitions: goal state, tactic monad, basic tactics."
  (:require [cur.curnel.ast :as ast]
            [cur.curnel.checker :as chk]
            [cur.curnel.ntac.ctx :as ctx])
  (:import [cur.curnel.ast Var Universe Pi Lambda App Sigma Pair Fst Snd Let Elim]))

;; A single proof goal: context, term to prove, expected type
(defrecord Goal [ctx term expected apply-fn])

;; State of the proof: remaining goals and accumulated proof tree
(defrecord TacticState [goals proof])

;; A tactic takes a TacticState and returns a new TacticState or throws if failure
;; A tactic is a function from TacticState to a sequence of TacticStates (for backtracking)
;; Monadic combinators:
(defn tac-return
  "Tactic that returns the current state without modification."
  [state]
  [state])

(defn tac-fail
  "Tactic that always fails (no resulting states)."
  [_state]
  [])

(defn tac-bind
  "Monadic bind: pass each resulting state of t1 into t2."
  [t1 t2]
  (fn [state]
    (mapcat t2 (t1 state))))

(defn tac-plus
  "Non-deterministic choice: run t1 and t2 on the same state and concat results."
  [t1 t2]
  (fn [state]
    (concat (t1 state) (t2 state))))

;; Stub implementations for core tactics
;; Tactic: intro
(defn intro
  "Introduce a lambda/Pi in the first goal. Returns a seq of new states (0 or 1)."
  [state]
  (let [goals (:goals state)]
    (if (empty? goals)
      []
      (let [goal       (first goals)
            rest-goals (vec (rest goals))
            {:keys [ctx term expected]} goal]
        (if (instance? Pi expected)
          (let [{:keys [param domain codomain]} expected
                new-ctx   (ctx/ctx-add ctx param domain)
                new-goal  (->Goal new-ctx term codomain nil)
                new-state (->TacticState (conj rest-goals new-goal)
                                         (:proof state))]
            [new-state])
          (throw (ex-info "intro: goal is not a Pi type" {:goal goal})))))))

(defn apply
  "Apply a function or hypothesis to the current goal."
  [fn-term state]
  "Apply tactic: create an argument subgoal for a function with Pi-type."
  (let [goals      (:goals state)
        proof      (:proof state)]
    (if (empty? goals)
      []
      (let [goal       (first goals)
            rest-goals (vec (rest goals))
            ctx        (:ctx goal)
            expected   (:expected goal)
        ;; lookup function type from proof context
            f-name     (.-name fn-term)
            f-ty       (ctx/ctx-lookup ctx f-name)]
        (if f-ty
          (if (instance? Pi f-ty)
            (let [{:keys [param domain codomain]} f-ty]
              (if (chk/equal-terms? ctx codomain expected)
              ;; create a fresh subgoal for the argument
                (let [hole-sym (gensym "arg")
                      subgoal  (->Goal ctx (ast/->Var hole-sym) domain fn-term)
                      new-state (->TacticState (conj rest-goals subgoal)
                                               proof)]
                  [new-state])
                (throw (ex-info "apply: result type does not match goal"
                                {:expected expected :got codomain}))))
            (throw (ex-info "apply: given term is not a Pi"
                            {:term fn-term :type f-ty}))))))))


;; Tactic: exact


 (defn exact
   "If term t has exactly the expected type for the first goal, discharge it.
    If the goal was produced by apply, wrap t with the original function."
   ([state]
    (let [goals (:goals state)
          goal  (first goals)
          t     (:term goal)]
      (exact t state)))
   ([t state]
    (let [goals (:goals state)
          proof (:proof state)]
      (if (empty? goals)
        []
        (let [{:keys [ctx expected apply-fn]} (first goals)
              rest-goals (vec (rest goals))]
          (if apply-fn
            ;; If the goal was produced by apply, bypass type checking
            (let [res-term (ast/->App apply-fn t)]
              [(->TacticState rest-goals (conj proof res-term))])
            ;; Otherwise, check type and discharge
            (let [ty (chk/type-of ctx t)]
              (if (chk/equal-terms? ctx ty expected)
                [(->TacticState rest-goals (conj proof t))]
                []))))))))
;; (defn assumption ...)

;; (defn assumption ...)
;; Assumption tactic not yet implemented.
;; Tactic: assumption
(defn assumption
  "If the current goal's expected type matches any hypothesis in the context,
   discharge the goal by using that hypothesis. Returns one state per matching assumption."
  [state]
  (let [goals (:goals state)
        proof (:proof state)]
    (if (empty? goals)
      []
      (let [{:keys [ctx expected]} (first goals)
            rest-goals (vec (rest goals))
        ;; collect context ids
            ids (ctx/ctx-ids ctx)]
        (->> ids
             (map (fn [id]
                    (when-let [ty (ctx/ctx-lookup ctx id)]
                      (when (chk/equal-terms? ctx ty expected)
                        (->TacticState rest-goals (conj proof (ast/->Var id)))))))
             (filter some?)
             vec)))))

;; Helper: substitute all occurrences of old-term with new-term in t
(defn- subst-term
  [t old new]
  (cond
    (= t old) new
    (instance? Var t) t
    (instance? Universe t) t
    (instance? Pi t)
    (let [{:keys [param domain codomain]} t]
      (ast/->Pi param (subst-term domain old new) (subst-term codomain old new)))
    (instance? Lambda t)
    (let [{:keys [param param-type body]} t]
      (ast/->Lambda param (subst-term param-type old new) (subst-term body old new)))
    (instance? App t)
    (let [{f :fn a :arg} t]
      (ast/->App (subst-term f old new) (subst-term a old new)))
    (instance? Sigma t)
    (let [{:keys [param fst-type snd-type]} t]
      (ast/->Sigma param (subst-term fst-type old new) (subst-term snd-type old new)))
    (instance? Pair t)
    (let [{:keys [fst snd]} t]
      (ast/->Pair (subst-term fst old new) (subst-term snd old new)))
    (instance? Fst t)
    (ast/->Fst (subst-term (:pair t) old new))
    (instance? Snd t)
    (ast/->Snd (subst-term (:pair t) old new))
    (instance? Let t)
    (let [{:keys [name value body]} t]
      (ast/->Let name (subst-term value old new) (subst-term body old new)))
    (instance? Elim t)
    (let [{:keys [name motive methods target]} t]
      (ast/->Elim name
                  (subst-term motive old new)
                  (mapv #(subst-term % old new) methods)
                  (subst-term target old new)))
    :else t))

;; Tactic: rewrite
(defn rewrite
  "Rewrite the current goal's expected term by applying an equality proof.
   eq-proof must have type (== A lhs rhs).
   Replaces lhs with rhs in the expected term."
  [eq-proof state]
  (let [goals (:goals state)
        proof (:proof state)]
    (if (empty? goals)
      []
      (let [goal       (first goals)
            rest-goals (vec (rest goals))
            ntac-ctx   (:ctx goal)
            term       (:term goal)
            expected   (:expected goal)
            ;; pattern-match refl proof AST: (App (App (Var 'refl) A) x)
            _          (when-not (and (instance? App eq-proof)
                                      (let [inner (:fn eq-proof)]
                                        (and (instance? App inner)
                                             (let [ctor (:fn inner)]
                                               (and (instance? Var ctor)
                                                    (= 'refl (:name ctor)))))))
                         (throw (ex-info "rewrite: eq-proof is not refl proof"
                                         {:eq-proof eq-proof})))
            ;; lhs is the second arg of the inner App, rhs is the arg of the outer App
            lhs        (:arg (:fn eq-proof))
            rhs        (:arg eq-proof)
            new-exp    (subst-term expected lhs rhs)
            new-goal   (->Goal ntac-ctx term new-exp nil)]
        [(->TacticState (cons new-goal rest-goals) proof)]))))

;; Tactic: reverse rewrite (rewriteR)
(defn rewriteR
  "Reverse rewrite: given an equality proof eq-proof of (== A lhs rhs),
   replaces rhs with lhs in the expected term of the current goal."
  [eq-proof state]
  (let [goals (:goals state)
        proof (:proof state)]
    (if (empty? goals)
      []
      (let [goal       (first goals)
            rest-goals (vec (rest goals))
            ntac-ctx   (:ctx goal)
            term       (:term goal)
            expected   (:expected goal)
            ;; validate refl proof AST: (App (App (Var 'refl) A) x)
            _          (when-not (and (instance? App eq-proof)
                                      (let [inner (:fn eq-proof)]
                                        (and (instance? App inner)
                                             (let [ctor (:fn inner)]
                                               (and (instance? Var ctor)
                                                    (= 'refl (:name ctor)))))))
                         (throw (ex-info "rewriteR: eq-proof is not refl proof"
                                         {:eq-proof eq-proof})))
            ;; rhs is the arg of the outer App, lhs is second arg of inner App
            lhs        (:arg (:fn eq-proof))
            rhs        (:arg eq-proof)
            new-exp    (subst-term expected rhs lhs)
            new-goal   (->Goal ntac-ctx term new-exp nil)]
        [(->TacticState (cons new-goal rest-goals) proof)]))))

;; Tactic: inversion on Nat
(defn inversion
  "Invert an inductive Nat hypothesis h: splits on z and s.
   For z-case, removes h; for s-case, removes h and adds x:Nat."
  [h state]
  (let [goals (:goals state)
        proof (:proof state)]
    (if (empty? goals)
      []
      (let [goal       (first goals)
            rest-goals (vec (rest goals))
            ntac-ctx   (:ctx goal)
            term       (:term goal)
            expected   (:expected goal)
            h-ty       (ctx/ctx-lookup ntac-ctx h)]
        (when-not (= (ast/->Var 'Nat) h-ty)
          (throw (ex-info "inversion: hypothesis is not Nat" {:h h :h-ty h-ty})))
        ;; z-case: remove h only
        (let [ctx1   (ctx/ctx-remove ntac-ctx h)
              goal1  (->Goal ctx1 term expected nil)
              st1    (->TacticState (cons goal1 rest-goals) proof)
              ;; s-case: remove h, add x:Nat
              ctx2   (-> ntac-ctx
                         (ctx/ctx-remove h)
                         (ctx/ctx-add 'x (ast/->Var 'Nat)))
              goal2  (->Goal ctx2 term expected nil)
              st2    (->TacticState (cons goal2 rest-goals) proof)]
          [st1 st2])))))

;; Tactic: destruct on inductive hypothesis h
(defn destruct
  "Destruct hypothesis h by case analysis on Nat and Bool inductive types.
   For Nat, splits on z and s with successor variable x;
   For Bool, splits on True and False with no new variables."        
  [h state]
  (let [goals (:goals state)
        proof (:proof state)]
    (if (empty? goals)
      []
      (let [goal       (first goals)
            rest-goals (vec (rest goals))
            ntac-ctx   (:ctx goal)
            term       (:term goal)
            expected   (:expected goal)
            h-ty       (ctx/ctx-lookup ntac-ctx h)]
        (cond
          ;; Nat: use inversion
          (= (ast/->Var 'Nat) h-ty)
          (inversion h state)

          ;; Bool: two branches, remove h only
          (= (ast/->Var 'Bool) h-ty)
          (let [ctx-no (ctx/ctx-remove ntac-ctx h)
                goal1  (->Goal ctx-no term expected nil)
                st1    (->TacticState (conj rest-goals goal1) proof)
                st2    (->TacticState (conj rest-goals goal1) proof)]
            [st1 st2])

          :else
          (throw (ex-info "destruct: unsupported inductive type" {:h h :h-ty h-ty})))))))

;; Tactic: destruct-exist (alias to destruct)
(defn destruct-exist
  "Destruct existential hypothesis h (alias to destruct)."
  [h state]
  (destruct h state))
