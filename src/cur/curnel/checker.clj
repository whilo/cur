;;-----------------------------------------------------------------
;; cur.curnel.checker
;;
;; Type checker and evaluator for cur AST
;;-----------------------------------------------------------------

 (ns cur.curnel.checker
  (:require [cur.curnel.ast :as ast]
            [cur.curnel.ntac.ctx])
  (:import [cur.curnel.ast Var Universe Pi Lambda App Sigma Pair Fst Snd Let Constructor InductiveType Elim]))

;; Substitute occurrences of variable sym in term t with value v
(defn substitute
  [t sym v]
  (cond
     ;; Variables
    (instance? Var t)
    (if (= sym (:name t)) v t)

     ;; Universe
    (instance? Universe t)
    t

     ;; Pi type
    (instance? Pi t)
    (let [{:keys [param domain codomain]} t
          domain* (substitute domain sym v)]
      (if (= param sym)
        (ast/->Pi param domain* codomain)
        (ast/->Pi param domain* (substitute codomain sym v))))

     ;; Lambda abstraction
    (instance? Lambda t)
    (let [{:keys [param param-type body]} t
          pt* (substitute param-type sym v)]
      (if (= param sym)
        (ast/->Lambda param pt* body)
        (ast/->Lambda param pt* (substitute body sym v))))

     ;; Application
    (instance? App t)
    (ast/->App (substitute (:fn t) sym v)
               (substitute (:arg t) sym v))

     ;; Sigma type
    (instance? Sigma t)
    (let [{:keys [param fst-type snd-type]} t
          fst* (substitute fst-type sym v)]
      (if (= param sym)
        (ast/->Sigma param fst* snd-type)
        (ast/->Sigma param fst* (substitute snd-type sym v))))

     ;; Pair
    (instance? Pair t)
    (ast/->Pair (substitute (.fst t) sym v)
                (substitute (.snd t) sym v))

     ;; Projections
    (instance? Fst t)
    (ast/->Fst (substitute (.pair t) sym v))

    (instance? Snd t)
    (ast/->Snd (substitute (.pair t) sym v))

     ;; Let-binding
    (instance? Let t)
    (let [{:keys [name value body]} t
          val* (substitute value sym v)]
      (if (= name sym)
        (ast/->Let name val* body)
        (ast/->Let name val* (substitute body sym v))))

     ;; Fallback: return unchanged
    :else t))

;; Evaluate term to weak head normal form
(defn eval-term
  [ctx t]
  (cond
     ;; Let-binding: evaluate value then substitute into body
    (instance? Let t)
    (let [val* (eval-term ctx (:value t))
          body (:body t)]
      (eval-term ctx (substitute body (:name t) val*)))

    ;; Application: evaluate fn and arg
    (instance? App t)
    (let [f* (eval-term ctx (:fn t))
          a* (eval-term ctx (:arg t))]
      (if (instance? Lambda f*)
        (eval-term ctx (substitute (:body f*) (:param f*) a*))
        (ast/->App f* a*)))

    ;; Otherwise return term
    :else t))

;; Compute full normal form by recursively normalizing subterms
(defn nf
  [ctx t]
  (let [t* (eval-term ctx t)]
    (cond
      (instance? Var t*) t*
      (instance? Universe t*) t*
      (instance? Lambda t*)
      (let [{:keys [param param-type body]} t*
            pt*   (nf ctx param-type)
            body* (nf (assoc ctx param pt*) body)]
        (ast/->Lambda param pt* body*))
      (instance? Pi t*)
      (let [{:keys [param domain codomain]} t*
            dom* (nf ctx domain)
            co*  (nf (assoc ctx param dom*) codomain)]
        (ast/->Pi param dom* co*))
      (instance? App t*)
      (let [{:keys [fn arg]} t*
            f* (nf ctx fn)
            a* (nf ctx arg)]
        (ast/->App f* a*))
      :else t*)))

;; Alpha-equivalence of two terms with environment mapping symbols of t1 to t2
(defn- alpha-eq
  [env t1 t2]
  (cond
    ;; Variables
    (and (instance? Var t1) (instance? Var t2))
    (let [n1 (:name t1)
          n2 (:name t2)]
      (if-let [m (get env n1)]
        (= m n2)
        (= n1 n2)))
    ;; Universes
    (and (instance? Universe t1) (instance? Universe t2))
    (= (:level t1) (:level t2))
    ;; Lambda
    (and (instance? Lambda t1) (instance? Lambda t2))
    (let [p1 (:param t1) p2 (:param t2)
          pt1 (:param-type t1) pt2 (:param-type t2)
          b1  (:body t1) b2  (:body t2)]
      (and (alpha-eq env pt1 pt2)
           (alpha-eq (assoc env p1 p2) b1 b2)))
    ;; Pi
    (and (instance? Pi t1) (instance? Pi t2))
    (let [p1 (:param t1) p2 (:param t2)
          d1 (:domain t1) d2 (:domain t2)
          c1 (:codomain t1) c2 (:codomain t2)]
      (and (alpha-eq env d1 d2)
           (alpha-eq (assoc env p1 p2) c1 c2)))
    ;; Application
    (and (instance? App t1) (instance? App t2))
    (and (alpha-eq env (:fn t1) (:fn t2))
         (alpha-eq env (:arg t1) (:arg t2)))
    ;; Sigma
    (and (instance? Sigma t1) (instance? Sigma t2))
    (let [p1 (:param t1) p2 (:param t2)
          f1 (:fst-type t1) f2 (:fst-type t2)
          s1 (:snd-type t1) s2 (:snd-type t2)]
      (and (alpha-eq env f1 f2)
           (alpha-eq (assoc env p1 p2) s1 s2)))
    ;; Pair
    (and (instance? Pair t1) (instance? Pair t2))
    (and (alpha-eq env (:fst t1) (:fst t2))
         (alpha-eq env (:snd t1) (:snd t2)))
    ;; Projections
    (and (instance? Fst t1) (instance? Fst t2))
    (alpha-eq env (:pair t1) (:pair t2))
    (and (instance? Snd t1) (instance? Snd t2))
    (alpha-eq env (:pair t1) (:pair t2))
    ;; Let-binding
    (and (instance? Let t1) (instance? Let t2))
    (let [n1 (:name t1) n2 (:name t2)
          v1 (:value t1) v2 (:value t2)
          b1 (:body t1)  b2 (:body t2)]
      (and (alpha-eq env v1 v2)
           (alpha-eq (assoc env n1 n2) b1 b2)))
    ;; Fallback: not equal
    :else false))

;; Definitional equality: compare normal forms up to alpha-equivalence
(defn equal-terms?
  [ctx t1 t2]
  (alpha-eq {} (nf ctx t1) (nf ctx t2)))

;; Infer the type of term t under context ctx (map symbol -> Term)
(defmulti type-of
  "Infer the type of term t under context ctx (map symbol -> Term)."
  (fn [ctx t] (class t)))

(defmethod type-of Universe
  [_ t]
  ;; Universe: Type n : Type (n+1)
  (ast/->Universe (inc (:level t))))

 (defmethod type-of Var
   [ctx t]
   ;; Variable: look up in context (supports both map and Ctx record)
   (if-let [ty (cur.curnel.ntac.ctx/ctx-lookup ctx (:name t))]
     ty
     (throw (ex-info "Unbound variable" {:var t}))))

(defmethod type-of Lambda
  [ctx t]
  ;; Lambda abstraction: type-of lambda as Pi
  (let [{:keys [param param-type body]} t]
    (let [pt-ty (type-of ctx param-type)]
      (when-not (instance? Universe pt-ty)
        (throw (ex-info "Lambda parameter type is not a universe" {:param-type param-type})))
      (let [ctx2     (assoc ctx param param-type)
            body-ty (type-of ctx2 body)]
        (ast/->Pi param param-type body-ty)))))

(defmethod type-of Pi
  [ctx t]
  ;; Pi type: check domain and codomain
  (let [{:keys [param domain codomain]} t]
    (when-not (instance? Universe domain)
      (throw (ex-info "Pi domain is not a universe" {:domain domain})))
    (let [lvl-dom (:level domain)
          ctx2    (assoc ctx param domain)
          lvl-cod (if (instance? Universe codomain)
                    (:level codomain)
                    (let [cod-ty (type-of ctx2 codomain)]
                      (when-not (instance? Universe cod-ty)
                        (throw (ex-info "Pi codomain is not a universe" {:codomain codomain})))
                      (:level cod-ty)))]
      (ast/->Universe (max lvl-dom lvl-cod)))))

(defmethod type-of App
  [ctx t]
   ;; Application
  (let [{:keys [fn arg]} t
        f-ty (type-of ctx fn)]
    (if (instance? Pi f-ty)
      (let [{:keys [param domain codomain]} f-ty
            arg-ty (type-of ctx arg)]
        (when-not (= domain arg-ty)
          (throw (ex-info "Argument type mismatch" {:expected domain :found arg-ty})))
         ;; Return the codomain with the argument substituted as the result type
        (substitute codomain param arg))
      (throw (ex-info "Applying non-function" {:term t})))))

(defmethod type-of Elim
  [_ t]
  ;; Eliminator (fold) for inductive types
  (let [{:keys [motive target]} t]
    (ast/->App motive target)))

(defmethod type-of :default
  [_ t]
  (throw (ex-info "Cannot infer type for term" {:term t})))

;; Check that term t has expected type ty under context ctx
(defn check-term
  "Check that term t has expected type ty under context ctx."
  [ctx t ty]
  (let [inferred (type-of ctx t)]
    (when-not (equal-terms? ctx inferred ty)
      (throw (ex-info "Type mismatch in check-term" {:expected ty :inferred inferred})))
    nil))

;; Register an inductive type declaration into the context.
(defn register-inductive
  "Given a context map and an InductiveType decl, register the type and its constructors.
  Returns an extended context mapping the inductive type name to its type, and each constructor
  name to its constructor type."
  [ctx decl]
  (if (instance? InductiveType decl)
    (let [{:keys [name params result-type constructors]} decl
          ;; Build the inductive type as a Pi over its parameters, if any
          inductive-type
          (reduce (fn [acc [p ty]]
                    (ast/->Pi p ty acc))
                  result-type
                  (reverse params))
          ctx-with-type (assoc ctx name inductive-type)
          ctx-with-ctors
          (reduce (fn [cctx ctor]
                    (assoc cctx
                           (:name ctor)
                           (:type ctor)))
                  ctx-with-type
                  constructors)]
      ctx-with-ctors)
    (throw (ex-info "Not an inductive declaration" {:decl decl}))))
