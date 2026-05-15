(ns com.blockether.vis.internal.extension.sci-patches
  "Monkey-patches over SCI's `:no-doc` impl namespaces.

   SCI 0.12.51 ships no public hook for `def` evaluation. The pivot
   contract needs both:
     1. Capture every def/defn/defmacro into a per-iteration sink so the
        engine can flush expression_state rows in one transaction at
        iteration-end (no post-eval source parsing).
     2. Enforce mandatory docstring on every `def` the SCI sandbox runs.

   We `alter-var-root` `sci.impl.evaluator/eval-def` once at namespace
   load. The wrap is global (alter-var-root affects the whole JVM); the
   `*def-sink-atom*` dynamic var is the per-iteration boundary. When
   the engine binds it before `sci/eval-string+`, every def the model
   runs lands in the sink. When it's nil (e.g. tests evaluating SCI
   directly without engine context), the wrap is silent.

   `defonce` guards both the captured `original-eval-def` and the
   install action so namespace reload never wraps the wrap.

   Fragility note: `sci.impl.evaluator` is `^:no-doc`. The signature
   `[ctx bindings var-name init m]` has been stable from sci 0.8.41 →
   0.12.51 (5 releases). Startup precondition asserts the var exists;
   if SCI ever refactors this fn, the first iteration aborts with
   `:vis.sci-patches/precondition-failed` instead of silently doing
   the wrong thing."
  (:require
   [sci.core]
   [sci.impl.evaluator]
   [sci.impl.types :as sci-types]))

;; =============================================================================
;; Per-iteration sink
;; =============================================================================

(def ^:dynamic *def-sink-atom*
  "Per-iteration atom (or nil). When bound, the patched `eval-def`
   appends `{:ns :name :init :meta :var}` for each def the SCI sandbox
   executes. The engine reads it after `(sci/eval-string+ …)` returns
   and flushes to expression_state rows in the iteration's transaction."
  nil)

(defn fresh-sink-atom
  "Allocate a fresh empty atom suitable for binding to `*def-sink-atom*`.
   Engine uses this at iteration start; reads `@sink` after eval."
  []
  (atom []))

;; =============================================================================
;; Patch: sci.impl.evaluator/eval-def
;; =============================================================================

(defonce precondition-checked!
  ;; Side-effect bootstrap: verifies the patched fn exists with the
  ;; expected shape before we touch it. Raises a structured exception at
  ;; startup so a SCI bump that renames eval-def fails loud, not silent.
  ;; Public defonce (not ^:private) so clj-kondo doesn't flag it as
  ;; unused — these are run-once side effects, not callable APIs.
  (let [v (resolve 'sci.impl.evaluator/eval-def)]
    (when-not (and (var? v) (fn? @v))
      (throw (ex-info "sci.impl.evaluator/eval-def missing or wrong shape"
               {:type :vis.sci-patches/precondition-failed
                :resolved v})))
    :ok))

(defonce ^:private original-eval-def
  ;; Snapshot the unpatched fn ONCE. defonce ensures namespace reload
  ;; does not capture the already-patched version (which would compound
  ;; on every reload).
  (var-get #'sci.impl.evaluator/eval-def))

(defn- patched-eval-def
  [ctx bindings var-name init m]
  ;; SCI passes `m` (the metadata map) as an unevaluated Node — see
  ;; sci.impl.evaluator/eval-def, which calls (types/eval m ctx bindings)
  ;; on its first line. We have to materialize it here BEFORE we can
  ;; read `:doc` for the docstring check.
  (let [m-val (sci-types/eval m ctx bindings)]
    (when-not (:doc m-val)
      (throw (ex-info (str "def requires a docstring: (def " var-name " \"doc\" …)")
               {:type :vis/missing-docstring
                :name var-name})))
    (let [v (original-eval-def ctx bindings var-name init m)]
      (when-let [sink *def-sink-atom*]
        (swap! sink conj
          {:ns   (some-> (:ns m-val) sci-types/getName str)
           :name var-name
           :init init
           :meta m-val
           :var  v}))
      v)))

(defonce install-once!
  ;; Side-effect bootstrap: alter-var-root replaces SCI's eval-def with
  ;; the patched wrap. Public defonce so clj-kondo doesn't flag the
  ;; load-time side effect as unused.
  (do
    (alter-var-root #'sci.impl.evaluator/eval-def (constantly patched-eval-def))
    :installed))
