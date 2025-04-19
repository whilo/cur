;;-----------------------------------------------------------------
;; cur.curnel.ntac.core-test
;;
;; Smoke tests for NTac core namespace.
;;-----------------------------------------------------------------
(ns cur.curnel.ntac.core-test
  (:require [clojure.test :refer :all]
            [cur.curnel.ntac.core :as ntac]
            [cur.curnel.ntac.ctx :as ctx]
            [cur.curnel.ast :as ast]
            [cur.std.equality])
  (:import [cur.curnel.ast Var]))

(deftest namespace-loads
  (is (fn? ntac/intro))
  (is (fn? ntac/apply)))

(deftest monad-return-fail
  (is (= [1] (ntac/tac-return 1)))
  (is (= []  (ntac/tac-fail 1))))

(deftest monad-bind-plus
  (let [t1 (fn [s] [s (inc s)])
        t2 (fn [s] [(* 2 s)])
        b  ((ntac/tac-bind t1 t2) 3)
        p  ((ntac/tac-plus t1 t2) 3)]
    (is (= [6 8] b))
    (is (= [3 4 6] p))))

(deftest intro-basic
  (let [empty-ctx (ctx/mk-empty-ctx)
        ;; expected type: Pi [x A] B
        A         (ast/->Var 'A)
        B         (ast/->Var 'B)
        pi        (ast/->Pi 'x A B)
        ;; initial goal: prove term T of type Pi
        T         (ast/->Var 'T)
        goal      (ntac/->Goal empty-ctx T pi nil)
        state     (ntac/->TacticState [goal] [])
        results   (ntac/intro state)
        new-state (first results)
        [new-goal] (:goals new-state)
        new-ctx   (:ctx new-goal)
        new-exp   (:expected new-goal)]
    ;; context now has binding x of type A
    (is (= ['x] (ctx/ctx-ids new-ctx)))
    (is (= [A]   (ctx/ctx-types new-ctx)))
    ;; expected type is B
    (is (= B new-exp))))

(deftest exact-basic
  (let [term     (ast/->Universe 0)
        expected (ast/->Universe 1)
        goal     (ntac/->Goal {} term expected nil)
        state    (ntac/->TacticState [goal] [])
        results  (ntac/exact term state)]
    (is (= 1 (count results)))
    (let [new-state (first results)]
      (is (empty? (:goals new-state)))
      (is (= [term] (:proof new-state))))))

(deftest apply-basic
  (let [empty-ctx (ctx/mk-empty-ctx)
        A         (ast/->Var 'A)
        B         (ast/->Var 'B)
        fty       (ast/->Pi 'x A B)
        ;; register f : Pi x:A.B in context
        ctx1      (ctx/ctx-add empty-ctx 'f fty)
        f         (ast/->Var 'f)
        goal      (ntac/->Goal ctx1 f B nil)
        state     (ntac/->TacticState [goal] [])
        results   (ntac/apply f state)]
    (is (= 1 (count results)))
    (let [new-state (first results)
          goals     (:goals new-state)
          subgoal   (first goals)]
      (is (= ctx1 (:ctx subgoal)))
      (is (= A (:expected subgoal)))
      (is (instance? Var (:term subgoal))))))

(deftest apply-then-exact-integration
    ;; Integration test: apply then exact yields (App f arg-term)
  (let [A          (ast/->Var 'A)
        B          (ast/->Var 'B)
        fty        (ast/->Pi 'x A B)
        ctx1       {'f fty}
        f          (ast/->Var 'f)
        goal       (ntac/->Goal ctx1 f B nil)
        init       (ntac/->TacticState [goal] [])
          ;; apply f to produce subgoal for x:A
        [s1]       (ntac/apply f init)
        subgoal    (first (:goals s1))
        arg-term   (:term subgoal)
          ;; discharge the subgoal by exacting the variable
        [s2]       (ntac/exact arg-term s1)
        final-goals (:goals s2)
        proof      (:proof s2)]
    (is (empty? final-goals))
    (is (= [(ast/->App f arg-term)] proof))))

  ;; rewrite and inversion tests
(deftest rewrite-basic
  (let [type-ctx (cur.std.equality/register {})
        ntac-ctx (reduce (fn [c [id ty]] (ctx/ctx-add c id ty))
                         (ctx/mk-empty-ctx) type-ctx)
        A        (ast/->Var 'A)
        x        (ast/->Var 'x)
        eqp      (ast/->App (ast/->App (ast/->Var 'refl) A) x)
        goal     (ntac/->Goal ntac-ctx (ast/->Var 'T) x nil)
        state    (ntac/->TacticState [goal] [])
        [s1]     (ntac/rewrite eqp state)
        g1       (first (:goals s1))]
    (is (= x (:expected g1)))
    (is (empty? (:proof s1)))))

(deftest inversion-basic
  (let [empty    (ctx/mk-empty-ctx)
        ctx1     (ctx/ctx-add empty 'n (ast/->Var 'Nat))
        T        (ast/->Var 'T)
        goal     (ntac/->Goal ctx1 T (ast/->Var 'Nat) nil)
        state    (ntac/->TacticState [goal] [])
        [st1 st2] (ntac/inversion 'n state)
        g1        (first (:goals st1))
        g2        (first (:goals st2))]
      ;; z-case: context should have no n or x
    (is (empty? (ctx/ctx-ids (:ctx g1))))
      ;; s-case: context should have x
    (is (= ['x] (ctx/ctx-ids (:ctx g2))))))

(deftest assumption-basic
  (let [A       (ast/->Var 'A)
         ;; context with hypothesis x:A
        ctx1    (ctx/ctx-add (ctx/mk-empty-ctx) 'x A)
        goal    (ntac/->Goal ctx1 (ast/->Var 'x) A nil)
        state   (ntac/->TacticState [goal] [])
        results (ntac/assumption state)]
    (is (= 1 (count results)))
    (let [new-state (first results)]
      (is (empty? (:goals new-state)))
      (is (= [(ast/->Var 'x)] (:proof new-state))))))
