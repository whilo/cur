;;-----------------------------------------------------------------
;; cur.curnel.ntac.core
;;
;; Core definitions for the tactics engine (NTac) in Clojure port.
;;-----------------------------------------------------------------
(ns cur.curnel.ntac.core
  "Core tactics definitions: goal state, tactic monad, basic tactics."
  (:require [cur.curnel.ast :as ast]
            [cur.curnel.checker :as chk]
            [cur.curnel.ntac.ctx :as ctx]
            [cur.std :refer [std-ctx]])
  (:import [cur.curnel.ast Var Universe Pi Lambda App Sigma Pair Fst Snd Let Elim]
           [cur.curnel.ntac.ctx Ctx]))

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

;; Tactic that discharges a goal of form (== A x x) by applying `refl`.
(defn reflexivity
  "Tactic that discharges a goal of form (== A x x) by applying `refl`."
  [state]
  (let [goals (:goals state)
        proof (:proof state)]
    (if (empty? goals)
      []
      (let [{:keys [ctx expected]} (first goals)
            rest-goals (vec (rest goals))]
        ;; Normalize the expected equality first
        (let [exp* (chk/nf std-ctx expected)]
          ;; Check that exp* is of form (== A x x)
          (if (and (instance? App exp*)
                   (instance? App (:fn exp*))
                   (instance? App (-> exp* :fn :fn))
                   (instance? Var (-> exp* :fn :fn :fn))
                   (= '== (-> exp* :fn :fn :fn :name))
                   (let [x    (-> exp* :fn :arg)
                         y    (:arg exp*)]
                     (chk/equal-terms? ctx x y)))
            ;; build refl proof: (refl A x)
            (let [A  (-> exp* :fn :fn :arg)
                  x  (-> exp* :fn :arg)
                  pf (ast/->App (ast/->App (ast/->Var 'refl) A) x)]
              [(->TacticState rest-goals (conj proof pf))])
            (throw (ex-info "reflexivity: goal is not of form == A x x"
                            {:expected exp*}))))))))

(defn assert
  "Assert a new hypothesis `id` of type `ty`, splitting the current goal into:
   1) a subgoal to prove `ty`;
   2) the original goal under an extended context with `id` bound to `ty`.
   Usage: (assert id ty)"
  [id ty state]
  (let [goals (:goals state)
        proof (:proof state)]
    (if (empty? goals)
      []
      (let [{:keys [ctx term expected]} (first goals)
            rest-goals (vec (rest goals))
            ;; Subgoal 1: prove the asserted type
            g1 (->Goal ctx ty ty nil)
            ;; Extended context with new hypothesis
            ctx2 (ctx/ctx-add ctx id ty)
            ;; Subgoal 2: original goal under extended context
            g2 (->Goal ctx2 term expected nil)
            new-goals (into [] (concat [g1 g2] rest-goals))]
        [(->TacticState new-goals proof)]))))

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
            ;; Determine lhs and rhs depending on proof form
            [lhs rhs]
            (cond
              ;; refl proof AST: (App (App (Var 'refl) A) x)
              (and (instance? App eq-proof)
                   (let [mid  (:fn eq-proof)
                         head (:fn mid)]
                     (and (instance? App mid)
                          (instance? Var head)
                          (= 'refl (:name head)))))
              (let [mid (:fn eq-proof)]
                ;; lhs is the second arg of inner App, rhs is the arg of outer App
                [(:arg mid) (:arg eq-proof)])
              ;; hypothesis var proof: type-of var must be equality
              (instance? Var eq-proof)
              (let [ty  (ctx/ctx-lookup ntac-ctx (:name eq-proof))
                    mid (:fn ty)]
                [(:arg mid) (:arg ty)])
              :else
              (throw (ex-info "rewrite: eq-proof is not equality proof"
                              {:eq-proof eq-proof})))
            new-exp    (subst-term expected lhs rhs)
            new-goal   (->Goal ntac-ctx term new-exp nil)]
        [(->TacticState (cons new-goal rest-goals) proof)]))))

;; Declare forward references for composite tactics
(declare simpl)

;; Reverse rewrite: ...
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
            ;; Determine lhs and rhs depending on proof form
            [lhs rhs]
            (cond
              ;; refl proof AST: (App (App (Var 'refl) A) x)
              (and (instance? App eq-proof)
                   (let [mid  (:fn eq-proof)
                         head (:fn mid)]
                     (and (instance? App mid)
                          (instance? Var head)
                          (= 'refl (:name head)))))
              (let [mid (:fn eq-proof)]
                ;; reverse: rhs is the arg of inner, lhs is the arg of outer
                [(:arg eq-proof) (:arg mid)])
              ;; hypothesis var proof: lhs is mid.arg, rhs is ty.arg
              (instance? Var eq-proof)
              (let [ty  (ctx/ctx-lookup ntac-ctx (:name eq-proof))
                    mid (:fn ty)]
                [(:arg mid) (:arg ty)])
              :else
              (throw (ex-info "rewriteR: eq-proof is not equality proof"
                              {:eq-proof eq-proof})))
            new-exp    (subst-term expected rhs lhs)
            new-goal   (->Goal ntac-ctx term new-exp nil)]
        [(->TacticState (cons new-goal rest-goals) proof)]))))

;; Composite ML-rewrite: rewrite, normalize plus and mult, then reflexivity
(defn ml-rewrite
  "Composite tactic: rewrite using eq-proof, simplify twice, then reflexivity."
  [eq-proof state]
  (->> (rewrite eq-proof state)
       (mapcat cur.curnel.ntac.core/simpl)
       (mapcat cur.curnel.ntac.core/simpl)
       (mapcat cur.curnel.ntac.core/reflexivity)))

;; Tactic: rewrite under Pi-binder
(defn rewrite-forall
  "Apply rewrite eq-proof under the leading Pi in goal's expected type."
  [eq-proof state]
  (let [{:keys [goals proof]} state]
    (if (empty? goals)
      []
      (let [goal       (first goals)
            rest-goals (vec (rest goals))
            {:keys [ctx term expected]} goal]
        (if (instance? Pi expected)
          (let [{:keys [param domain codomain]} expected
                sub-goal     (->Goal ctx term codomain nil)
                sub-state    (->TacticState [sub-goal] proof)
                sub-results  (rewrite eq-proof sub-state)]
            (for [st sub-results]
              (let [new-g (first (:goals st))
                    new-exp (ast/->Pi param domain (:expected new-g))]
                (->TacticState (cons (->Goal ctx term new-exp nil) rest-goals)
                                (:proof st)))))
          (throw (ex-info "rewrite-forall: expected type is not Pi" {:expected expected})))))))

(def by-ml-rewrite ml-rewrite)

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

(defn destruct-exist
  "Destruct existential hypothesis h (alias to destruct)."
  [h state]
  (destruct h state))

;; Aliases for DSL rewriting tactics
(def by-rewrite rewrite)
(def by-rewriteL rewriteR)
;; Tactic: rewrite inside a hypothesis
(defn rewrite-in
  "Rewrite hypothesis `h-name` using equality proof `eq-proof`.
   `eq-proof` must have type (== A lhs rhs).
   In the first goal of `state`, replace the type of `h-name` in the context by substituting `lhs` → `rhs`."
  [eq-proof h-name state]
  (let [goals (:goals state)
        proof (:proof state)]
    (if (empty? goals)
      []
      (let [goal       (first goals)
            rest-goals (vec (rest goals))
            {:keys [ctx term expected]} goal
            ;; original hypothesis type
            h-ty      (ctx/ctx-lookup ctx h-name)
            ;; determine lhs and rhs from the equality proof
            [lhs rhs]
            (cond
              ;; refl proof AST: (App (App (Var 'refl) A) x)
              (and (instance? App eq-proof)
                   (let [mid  (:fn eq-proof)
                         head (:fn mid)]
                     (and (instance? App mid)
                          (instance? Var head)
                          (= 'refl (:name head)))))
              (let [mid (:fn eq-proof)]
                [(:arg mid) (:arg eq-proof)])
              ;; hypothesis variable proof
              (instance? Var eq-proof)
              (let [ty  (ctx/ctx-lookup ctx (:name eq-proof))
                    mid (:fn ty)]
                [(:arg mid) (:arg ty)])
              :else
              (throw (ex-info "rewrite-in: eq-proof is not equality proof"
                              {:eq-proof eq-proof})))
            ;; substitute in hypothesis type
            new-h-ty (subst-term h-ty lhs rhs)
            ;; update context: Ctx or plain map
            new-ctx   (if (instance? Ctx ctx)
                        (-> ctx
                            (ctx/ctx-remove h-name)
                            (ctx/ctx-add    h-name new-h-ty))
                        (assoc ctx h-name new-h-ty))
            new-goal  (->Goal new-ctx term expected nil)]
        [(->TacticState (cons new-goal rest-goals) proof)]))))

;; Tactic that moves a hypothesis into the goal as a Pi-abstraction.
(defn generalize
  "Tactic that abstracts over hypothesis id: removes id from context and
   wraps the expected type with a Pi binder for id."
  [id state]
  (let [{:keys [goals proof]} state]
    (if (empty? goals)
      []
      (let [goal       (first goals)
            rest-goals (vec (rest goals))
            {:keys [ctx term expected]} goal
            ty         (ctx/ctx-lookup ctx id)]
        (if ty
          (let [ctx2     (ctx/ctx-remove ctx id)
                new-exp  (ast/->Pi id ty expected)
                new-goal (->Goal ctx2 term new-exp nil)]
            [(->TacticState (cons new-goal rest-goals) proof)])
          (throw (ex-info "generalize: hypothesis not found" {:id id})))))))

;; Tactic that simplifies (normalizes) the expected type of the current goal.
(defn simpl
  "Tactic that replaces the expected type of the first goal with its normal form."
  [state]
  (let [{:keys [goals proof]} state]
    (if (empty? goals)
      []
      (let [goal       (first goals)
            rest-goals (vec (rest goals))
            {:keys [ctx term expected]} goal
            ;; determine normalization context: if this is an NTac proof Ctx, use the global std-ctx;
            ;; otherwise (ctx is already a kernel context map) use ctx directly
            tctx       (if (instance? cur.curnel.ntac.ctx.Ctx ctx) std-ctx ctx)
            new-exp    (chk/nf tctx expected)
            new-goal   (->Goal ctx term new-exp nil)]
        [(->TacticState (cons new-goal rest-goals) proof)]))))

;; Tactic that repeatedly applies intro to eliminate all Pi binders.
(defn implicit
  "Tactic that introduces all leading Pi quantifiers in the goal.
   Equivalent to repeating `intro` until the expected type is not a Pi."
  [state]
  (letfn [(step [st]
            (let [goals (:goals st)]
              (if (and (seq goals)
                       (instance? Pi (:expected (first goals))))
                (mapcat step (intro st))
                [st])))]
    (step state)))

;; Tactic combinator: tac-try to apply a tactic, returning the original state on failure or no result.
(defn tac-try
  "Tactic combinator: attempt t; if it fails or yields no states, return the input state."
  [t]
  (fn [state]
    (try
      (let [res (t state)]
        (if (seq res) res [state]))
      (catch Exception _#
        [state]))))

;; Tactic combinator: extend the context for the next tactic.
(defn with-ctx
  "Tactic combinator: extend context by binding id to ty when running t on the first goal."
  [id ty t]
  (fn [state]
    (let [{:keys [goals proof]} state]
      (if (empty? goals)
        []
        (let [goal       (first goals)
              rest-goals (vec (rest goals))
              {:keys [ctx term expected]} goal
              ctx2       (ctx/ctx-add ctx id ty)
              new-goal   (->Goal ctx2 term expected nil)
              new-state  (->TacticState (cons new-goal rest-goals) proof)]
          (t new-state))))))

(defn auto
  "Automatic proof search: try `assumption` first, then `reflexivity` on the current goal."
  [state]
  (let [;; attempt to discharge by assumption
        as-res (assumption state)]
    (if (seq as-res)
      as-res
      ;; otherwise attempt reflexivity, catching failures
      (try
        (let [rf-res (reflexivity state)]
          (if (seq rf-res) rf-res []))
        (catch Exception _#
          [])))))

(defn interactive
  "Interactive proof tactic: introduce all Pi binders, then discharge by assumption."
  [state]
  (->> (implicit state)
       (mapcat assumption)))
