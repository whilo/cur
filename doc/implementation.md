 # Implementation Comparison: Racket cur vs clj-cur

 This document summarizes the current state of the Clojure port (clj-cur) compared to the original Racket-based `cur` proof assistant.

 ## Project Structure
 - **Racket implementation**: source in `cur/`, tests under `cur-test/cur/tests/ntac/`
 - **Clojure port**:
   - Source: `clj-cur/src/cur/`
     - Kernel and tactics in `curnel/ntac`
     - DSL macros in `curnel/ntac/dsl.clj`
     - Meta layer in `cur.meta`
     - Standard library in `cur.std`
   - Tests: `clj-cur/test/cur/curnel/ntac/` and `clj-cur/test/cur/std/`

 ## Test Suite Mapping
 The table below maps Racket NTac test scripts to their Clojure equivalents and current coverage.

 | Racket Test                       | Clojure Test                             | Status       |
 |-----------------------------------|------------------------------------------|--------------|
 | `ctx.rkt`                         | `ctx_test.clj`, `ctx_racket_test.clj`    | implemented  |
 | `assert.rkt`                      | `assert_test.clj`                        | implemented  |
 | `reflexivity-poly.rkt`            | `reflexivity_poly_test.clj`              | implemented  |
 | `ML-reflexivity.rkt`              | `ml_reflexivity_test.clj`                | implemented  |
 | `ML-rewrite.rkt`                  | `ml_rewrite_test.clj`                    | implemented  |
 | `ML-rewrite-2.rkt`                | `ml_rewrite2_test.clj`                   | implemented  |
| `rewrite.rkt`                     | `rewrite_core_test.clj`                  | stubbed      |
| `rewrite-forall.rkt`              | `rewrite_forall_test.clj`                | stubbed      |
| `rewrite-in.rkt`                  | `rewrite_in_test.clj`                    | stubbed      |
| `rewrite-with-previous.rkt`       | `rewrite_with_previous_test.clj`         | stubbed      |
| `inversion.rkt`                   | `tactics_basic_test.clj`                 | implemented  |
| `destruct.rkt`                    | `tactics_basic_test.clj`                 | implemented  |
| `destruct-exist.rkt`              | `tactics_basic_test.clj`                 | implemented  |
| `induction.rkt`                   | `induction_test.clj`                     | stubbed      |
| `Induction-no-Basics.rkt`         | `induction_no_basics_test.clj`           | stubbed      |
 | `generalize.rkt`                  | `generalize_test.clj`                    | implemented  |
 | `implicit.rkt`                    | `implicit_test.clj`                      | implemented  |
 | `simpl.rkt`                       | `simpl_test.clj`                         | implemented  |
 | `auto.rkt`                        | `auto_test.clj`                          | implemented  |
 | `interactive.rkt`                 | `interactive_test.clj`                   | implemented  |
| `issue104.rkt`                    | `issue104_test.clj`                      | stubbed      |
| `admit.rkt`                       | `admit_test.clj`                         | stubbed      |
| `leb.rkt`                         | `leb_test.clj`                           | stubbed      |
 | software-foundations suite        | *not ported*                             | missing      |

 _Additional tests:_ `core_test.clj`, `dsl_test.clj`, `meta_test.clj`, `tactics_basic_test.clj`, `mult_simpl_test.clj`, etc., cover internal functionality and DSL usage beyond the NTac suite.

 ## Feature Comparison & Limitations

### Kernel & Evaluation
- AST, parser, type-checker, and evaluator fully ported.
- Normalizer (`nf`) fully supports generic inductive recursor elimination.

 ### Standard Library
 - Ported modules: `bool`, `nat`, `maybe`, `list`, `sigma` (pair), `equality`, `typeclass` (plus `day`).
 - No additional datatypes (e.g., vectors, finite types) beyond the core suite.

 ### Tactics Engine (NTac)
 - Core tactics implemented: `intro`, `exact`, `apply`, `assumption`, `rewrite`, `inversion`, `destruct`, `destruct-exist`, `generalize`, `implicit`, `simpl`, `auto`, `interactive`.
- All core tactics have corresponding tests, including `inversion`, `destruct`, and `destruct-exist` (in `tactics_basic_test.clj`).
 - Meta-tactics layer (`cur.meta`) exists but is lightly tested.

 ### DSL & Proof Scripting
 - Macros provide `defproof`, `proof`, `thm`, and basic combinators.
 - Missing higher-level proof scripting macros (`by-intros`, `by-rewrite`, Unicode binders, arrow syntax).
 - Reflection and full AST quotation/unquotation support is incomplete.

 ## Next Steps
1. Implement stub tests for core rewrite features:
   - Fill out `rewrite_core_test.clj`, `rewrite_forall_test.clj`, `rewrite_in_test.clj`, and `rewrite_with_previous_test.clj` based on the Racket originals.
2. Expand test coverage for `inversion`, `destruct`, and `destruct-exist` to cover additional cases beyond the basic alias tests.
3. Port or adapt key scripts from the software-foundations suite as integration tests.
4. Refactor the normalizer to handle arbitrary inductive recursors via the context.
5. Expand DSL sugar and proof scripting support for ergonomics (Unicode, `by-` macros).
6. Increase test coverage of the `cur.meta` layer and tracing facilities.