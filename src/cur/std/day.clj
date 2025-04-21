;;-----------------------------------------------------------------
;; cur.std.day
;;
;; Standard library: custom inductive type `day` and its recursor.
;;-----------------------------------------------------------------
(ns cur.std.day
  "Standard library: day type, constructors, and next-weekday recursor."
  (:require [cur.curnel.parser :refer [parse-term]]
            [cur.curnel.checker :refer [register-inductive]]
            [cur.curnel.ast :as ast]))

(def day-decl
  "Inductive declaration for days of the week."
  (parse-term
   '(Inductive day [] (Type 0)
               [mon day]
               [tues day]
               [wed day]
               [thurs day]
               [fri day]
               [sat day]
               [sun day])))

(defn register
  "Register `day` inductive into the context."
  [ctx]
  (register-inductive ctx day-decl))

(defn next-weekday
  "Recursor for day: maps each day to the next weekday."
  [d]
  (ast/->Elim 'day
              ;; motive: λ [x : day] day
              (ast/->Lambda 'x (ast/->Var 'day) (ast/->Var 'day))
              ;; one branch per constructor in order: mon->tues, tues->wed, wed->thurs,
              ;; thurs->fri, fri->mon, sat->mon, sun->mon
              [(ast/->Var 'tues)
               (ast/->Var 'wed)
               (ast/->Var 'thurs)
               (ast/->Var 'fri)
               (ast/->Var 'mon)
               (ast/->Var 'mon)
               (ast/->Var 'mon)]
              d))