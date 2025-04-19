;;-----------------------------------------------------------------
;; cur.std.typeclass.macros
;;
;; Macros for declaring typeclasses and instances.
;;-----------------------------------------------------------------
(ns cur.std.typeclass.macros
  "Macros to support typeclass declarations and instances."
  (:require [clojure.walk :as walk]
            [cur.std.typeclass.registry :as registry]))

(defmacro deftypeclass
  "Define a typeclass named class-name with method signatures map.
  Usage: (deftypeclass C {m1 sig1, m2 sig2, ...})"
  [class-name methods-map]
  `(registry/register-class! '~class-name ~methods-map))

(defmacro definstance
  "Register an instance for class-name and type-key with implementations.
  Usage: (definstance C TypeKey {m1 impl-fn1, m2 impl-fn2, ...})"
  [class-name type-key impl-map]
  `(registry/register-instance! '~class-name '~type-key ~impl-map))

(defmacro defmethod
  "Define a dispatcher macro for a method of a given class.
  Usage: (defmethod C m)
  Expands (m x args...) to dispatch on x's type-key in registry."
  [class-name method-name]
  (let [m# (symbol (name method-name))]
    `(defmacro ~m# [x# & args#]
       `(let [impl## (registry/lookup-impl '~class-name (type x##) '~method-name)]
          (if impl##
            (apply impl### x## args#)
            (throw (ex-info (str "No impl for " '~class-name " on " (type x##)) {})))))))