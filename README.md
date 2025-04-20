# clj-cur: A Clojure Port of `cur`

`clj-cur` is a Clojure port of the `cur` proof assistant kernel, tactics engine (NTac),
and a growing standard library. It aims to provide:

- An identical core kernel for type-checking and evaluation of dependent type terms
- A tactics engine with backtracking (NTac) and scripting DSL macros
- A growing standard library (booleans, naturals, lists, equality, sigma types, etc.)
- Test coverage via Racket’s examples and Clojure unit tests

This module lives under the `clj-cur` directory.  To get started:

## Prerequisites

- Java 8+ JVM
- Clojure 1.10.3
- [tools.deps](https://clojure.org/guides/deps_and_cli)

## Building & Testing

From the `clj-cur` directory, run:

  clojure -M:test

This will execute the entire test suite under `test/cur`.

## Namespaces Overview

- `cur.curnel.ast`  – AST definitions (Var, Pi, Lambda, App, Sigma, etc.)
- `cur.curnel.checker` – Type inference, definitional equality (`nf`, `equal-terms?`), `type-of`
- `cur.curnel.parser` – S-expression parser mapping to AST
- `cur.std.*`     – Standard library modules (bool, nat, list, maybe, equality, sigma, typeclass)
- `cur.curnel.ntac.core` – Core NTac tactics (`intro`, `apply`, `exact`, `rewrite`, `inversion`, etc.)
- `cur.curnel.ntac.ctx`  – Proof context operations for NTac
- `cur.curnel.ntac.dsl`  – User-facing macros `proof` and `defproof`

## Quickstart Example

~~~clojure
;; import core DSL and standard context
(require '[cur.std :refer [std-ctx]]
         '[cur.curnel.ast :as ast]
         '[cur.curnel.ntac.dsl :refer [proof]])

;; Prove symmetry of equality over naturals:
(def sym-proof
  (proof std-ctx
         ;; term to prove: (Π [x Nat] [y Nat], (== Nat x y) → (== Nat y x))
         (ast/parse-term
           '(Pi [x Nat] [y Nat] (-> (== Nat x y) (== Nat y x))))
         (ast/parse-term
           '(Pi [x Nat] [y Nat] (-> (== Nat x y) (== Nat y x))))
         intro      ;; introduce x
         intro      ;; introduce y
         (apply x=y) ;; apply hypothesis x=y
         rewrite    ;; use `refl` to rewrite x=y to y=x
         exact))     ;; discharge remaining goal by assumption

;; `sym-proof` is now a vector of ASTs representing the proof term.
~~~

## Contributing

- New tactics and monadic combinators live in `src/cur/curnel/ntac/core.clj`.
- Proof DSL macros are in `src/cur/curnel/ntac/dsl.clj`.
- To port more Racket examples, add tests under `test/cur/curnel/ntac/`.
- Please run `clojure -M:test` before submitting PRs.

## Roadmap

See `CODEX.md` at the repository root for detailed phases, next steps,
and priorities for porting Racket NTac scripts, tracing, and DSL sugar.
