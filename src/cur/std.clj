;;-----------------------------------------------------------------
;; cur.std
;;
;; Assembly of standard library modules into a single context.
;;-----------------------------------------------------------------
(ns cur.std
  "Assembly of standard library modules into a single context."
  (:require [cur.std.bool :as bool]
            [cur.std.nat :as nat]
            [cur.std.day :as day]
            [cur.std.maybe :as maybe]
            [cur.std.list :as list]
            [cur.std.equality :as eq]
            [cur.std.sigma :as sigma]
            [cur.std.typeclass.registry]
            [cur.std.typeclass.macros :refer [deftypeclass definstance]]))

(def std-ctx
  (-> {}
      bool/register
      nat/register
      day/register
      maybe/register
      list/register
      eq/register
      sigma/register
      ;; typeclass registry may follow
      ))