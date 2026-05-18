(ns com.blockether.vis.internal.env.sci-patches
  "Monkey-patches over SCI's `:no-doc` impl namespaces.

   SCI 0.12.51 ships no public hook for `def` evaluation. The engine
   contract needs to capture every def/defn/defmacro into a per-iteration
   sink so the engine can flush definition_state rows in one transaction at
   iteration-end (no post-eval source parsing). Docstrings are optional;
   context is explicit in `ctx`, not hidden in var docs.

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
   [edamame.core :as edamame]
   [sci.core]
   [sci.impl.evaluator]
   [sci.impl.resolve]
   [sci.impl.types :as sci-types]))

;; =============================================================================
;; Per-iteration sink
;; =============================================================================

(def ^:dynamic *def-sink-atom*
  "Per-iteration atom (or nil). When bound, the patched `eval-def`
   appends `{:ns :name :init :meta :var}` for each def the SCI sandbox
   executes. The engine reads it after `(sci/eval-string+ …)` returns
   and flushes to definition_state rows in the iteration's transaction."
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
  ;; on its first line. Materialize it for the def sink; do not require
  ;; docstrings.
  (let [m-val (sci-types/eval m ctx bindings)
        v     (original-eval-def ctx bindings var-name init m)]
    (when-let [sink *def-sink-atom*]
      (swap! sink conj
        {:ns   (some-> (:ns m-val) sci-types/getName str)
         :name var-name
         :init init
         :meta m-val
         :var  v}))
    v))

(defonce install-once!
  ;; Side-effect bootstrap: alter-var-root replaces SCI's eval-def with
  ;; the patched wrap. Public defonce so clj-kondo doesn't flag the
  ;; load-time side effect as unused.
  (do
    (alter-var-root #'sci.impl.evaluator/eval-def (constantly patched-eval-def))
    :installed))

;; =============================================================================
;; Patch: sci.impl.resolve/resolve-symbol* — runtime-resolution LRU
;; =============================================================================

(def ^:dynamic *lru-atom*
  "Per-iteration atom (or nil). When bound, the patched
   `resolve-symbol*` stamps `(name sym) -> *current-turn-position*`
   on every successful sandbox-symbol resolution. The live-vars
   renderer reads this map to decide which user vars to surface in
   the discovery line and which have aged past the 10-turn LRU window.

   Per-iteration atoms (vs per-conversation) are intentional: each
   iteration sees a clean snapshot of \"vars referenced THIS iteration\";
   the engine merges into the long-lived per-env LRU after eval. That
   isolation keeps a runaway iteration's symbol noise from polluting
   every prior iteration's stamps."
  nil)

(def ^:dynamic *current-turn-position*
  "Per-iteration current turn position (1-based int). Stamped onto the
   `*lru-atom*` for every resolved symbol. Engine binds this from the
   iteration loop so the LRU map carries semantic turn coordinates,
   not wall-clock time."
  nil)

(defn fresh-lru-atom
  "Allocate a fresh empty atom suitable for binding to `*lru-atom*`.
   Engine uses this at iteration start; merges `@lru` into the per-env
   long-lived LRU after eval."
  []
  (atom {}))

(defonce resolve-precondition-checked!
  ;; Same shape check as the eval-def precondition.
  (let [v (resolve 'sci.impl.resolve/resolve-symbol*)]
    (when-not (and (var? v) (fn? @v))
      (throw (ex-info "sci.impl.resolve/resolve-symbol* missing or wrong shape"
               {:type :vis.sci-patches/precondition-failed
                :resolved v})))
    :ok))

(defonce ^:private original-resolve-symbol*
  (var-get #'sci.impl.resolve/resolve-symbol*))

(defn- patched-resolve-symbol*
  [ctx sym call? m]
  (let [result (original-resolve-symbol* ctx sym call? m)]
    (when (and result *lru-atom*)
      (swap! *lru-atom* assoc (name sym) (or *current-turn-position* 0)))
    result))

(defonce resolve-install-once!
  (do
    (alter-var-root #'sci.impl.resolve/resolve-symbol*
      (constantly patched-resolve-symbol*))
    :installed))

;; =============================================================================
;; Single-form block validation
;; =============================================================================

(def ^:private edamame-opts
  "Tag-tolerant edamame parse opts. Mirrors loop.clj's parser config so
   the validation here counts forms exactly the same way the engine
   does at execution time."
  {:all true
   :fn true
   :regex true
   :readers (fn [_tag] (fn [val] (list 'do val)))})

(defn count-top-level-forms
  "Parse `code` as Clojure source and return the number of top-level
   forms it contains. Comments and `#_(...)` discards count as zero.
   Returns 0 for empty / comment-only blocks. Raises edamame parse
   errors verbatim — those are syntax issues, not multi-form issues."
  [code]
  (count (edamame/parse-string-all (str code) edamame-opts)))

(defn validate-non-empty-block!
  "Throws `:vis/empty-block` when `code` parses to zero top-level forms
   (comment-only / `#_`-discard-only blocks). Iterations that produce
   no evidence cannot carry forward, so the engine rejects them at the
   model boundary instead of round-tripping an empty `:code` through
   persistence and the next iteration's trailer.

   Multi-form blocks are accepted: SCI's `eval-string+` parses and
   evaluates each top-level form in sequence; the eval-def patch
   captures every `(def …)` regardless of nesting; and the def-sink
   → vars-snapshot path indexes per-form source by var name, so
   functions round-trip whether the model writes
     (do (def a …) (def b …))
   or
     (def a …)
     (def b …)
   The single-form constraint that lived here in earlier drafts
   was prescriptive ceremony, not a technical requirement.

   Single and multi-form blocks pass through silently."
  [code]
  (when (zero? (count-top-level-forms code))
    (throw (ex-info "Block is empty (only comments / discards). Iteration produces no evidence."
             {:type :vis/empty-block
              :form-count 0}))))

;; =============================================================================
;; Banned def-head enforcement
;; =============================================================================

(def BANNED_DEF_HEADS
  "Def-like heads the sandbox refuses pre-eval. Produce JVM classes /
   protocol method tables / interop surfaces that cannot round-trip
   through λVis's per-var restore path — the var name persists but the
   class/interface does not, so the next process boot resurrects a
   half-broken binding.

   Both bare and `clojure.core/`-qualified forms are rejected; `reify`
   anywhere in the form tree also fails since it constructs an
   anonymous JVM instance that can't be reconstituted from source
   without the surrounding lexical context."
  '#{defrecord deftype defprotocol gen-class extend-type extend-protocol
     definterface reify
     clojure.core/defrecord clojure.core/deftype clojure.core/defprotocol
     clojure.core/gen-class clojure.core/extend-type clojure.core/extend-protocol
     clojure.core/definterface clojure.core/reify})

(defn- find-banned-head
  "Walk `form` (a parsed Clojure form) looking for any banned def head.
   Returns the first banned symbol found, or nil. Recurses through
   every sub-collection so a banned head buried inside `(do …)` /
   `(let …)` / `(when …)` / a fn body / a vector literal is still
   caught."
  [form]
  (let [found (atom nil)]
    (doseq [node (tree-seq coll? seq form)
            :while (nil? @found)]
      (when (and (seq? node)
              (symbol? (first node))
              (contains? BANNED_DEF_HEADS (first node)))
        (reset! found (first node))))
    @found))

(defn validate-no-banned-defs!
  "Throws `:vis/banned-def-head` when `code` contains any of
   `BANNED_DEF_HEADS` (anywhere in the form tree).

   Parse failures are silent here — the SCI eval that follows will
   surface a clean parse error with line/column. We only refuse when
   the parser succeeds AND finds a banned head."
  [code]
  (try
    (let [forms (edamame/parse-string-all (str code) edamame-opts)]
      (when-let [banned (some find-banned-head forms)]
        (throw (ex-info
                 (str "Block uses `" banned "` which is banned in the λVis sandbox. "
                   "Reason: it produces a JVM class / protocol method table that "
                   "cannot round-trip through per-var restore. "
                   "Use plain maps + multimethods (defmulti / defmethod) instead.")
                 {:type :vis/banned-def-head
                  :head banned}))))
    (catch clojure.lang.ExceptionInfo ei
      (if (= :vis/banned-def-head (:type (ex-data ei)))
        (throw ei)
        nil))
    (catch Throwable _ nil)))
