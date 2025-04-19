;;-----------------------------------------------------------------
;; cur.std.typeclass.registry
;;
;; Registry for typeclass declarations and instances.
;;-----------------------------------------------------------------
(ns cur.std.typeclass.registry)

(def registry
  "Atom holding map: class-name -> {:methods {method signature} :instances {type-key {method fn}}}."
  (atom {}))

(defn reset-registry!
  "Clear all registered classes and instances (for testing)."
  []
  (reset! registry {}))

(defn register-class!
  "Register a new typeclass named class-name with a map of methods to signatures."
  [class-name methods]
  (swap! registry assoc class-name {:methods methods :instances {}}))

(defn register-instance!
  "Register an implementation map of methods->fn for a given class and type key."
  [class-name type-key impl-map]
  (swap! registry update-in [class-name :instances] assoc type-key impl-map))

(defn lookup-impl
  "Look up the implementation fn for class-name, type-key, method.
  Returns the fn or nil if not found."
  [class-name type-key method]
  (get-in @registry [class-name :instances type-key method]))