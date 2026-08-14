(ns com.blockether.vis.internal.persistance
  "Persistence facade: backend registry, connection lifecycle, and every
   delegated `store-*`/`db-*` fn.

   Extensions register backend adapters through `:ext/persistance`. The
   facade dispatches each delegated call by resolving the matching var on
   the chosen backend namespace (`(ns-resolve ns-sym 'db-store-iteration!)`
   etc.) and applying it to the original args. This keeps the facade
   dialect-agnostic - every migration runner / driver-specific oddity stays
   inside the backend adapter.

   Frontends still call `db-error->user-message` here, but the actual
   translation is offered by backend adapters. Same for store-staleness
   checks used by the process-wide shared connection."
  (:require [charred.api :as json]
            [clojure.walk :as walk]
            [com.blockether.vis.internal.manifest :as manifest])
  (:import (java.time Instant)
           (java.util Date UUID)))

;; =============================================================================
;; Storage base helpers
;; =============================================================================

(defn ds [db-info] (:datasource db-info))

(defn now-ms ^long [] (System/currentTimeMillis))

(defn ->id
  [v]
  (cond (nil? v) nil
        (uuid? v) (str v)
        (string? v) v
        :else (str v)))

(defn ->uuid
  ^UUID [v]
  (cond (nil? v) nil
        (uuid? v) v
        (string? v) (try (UUID/fromString v) (catch IllegalArgumentException _ nil))
        :else nil))

(defn ->ref
  "Normalize an entity reference to a string ID for SQL.
   Accepts: UUID, string, or nil. Returns string or nil.

   The ONLY way to extract a SQL-ready string from an entity
   reference -- pass the plain UUID or string directly."
  [v]
  (cond (nil? v) nil
        (uuid? v) (str v)
        (string? v) v
        :else (str v)))

(defn ->kw
  "Keyword/string -> TEXT, stripping the leading colon. Nil -> nil."
  [v]
  (cond (nil? v) nil
        (keyword? v) (subs (str v) 1)
        :else (str v)))

(defn ->kw-back [v] (when (and v (not= "" v)) (keyword v)))

(defn ->epoch-ms
  [v]
  (cond (nil? v) nil
        (instance? Date v) (.getTime ^Date v)
        (instance? Instant v) (.toEpochMilli ^Instant v)
        (number? v) (long v)
        :else nil))

(defn ->date ^Date [v] (when v (Date. (long v))))

(defn new-uuid ^UUID [] (UUID/randomUUID))

(defn new-id [] (->id (new-uuid)))

;; =============================================================================
;; Column codecs (shared by EVERY backend)
;; =============================================================================

(defn- json-key
  "A map key charred can actually write. Keyword/symbol/string keys keep their
   EXACT current spelling (persisted columns must not shift under an existing
   database); anything else — the int keys a Python `Counter` or a decoded JSON
   value carries out of the sandbox — is RENDERED rather than left to blow up
   the whole write."
  [k]
  (cond (string? k) k
        (keyword? k) (subs (str k) 1)
        (symbol? k) (str k)
        (nil? k) "null"
        :else (str k)))

(defn- json-encodable
  "Rewrite ONLY what charred REFUSES to encode: non-string map keys and
   non-finite doubles (a pandas `NaN`, a `x/0.0`). Everything else passes
   through untouched, so no persisted spelling changes. Without this the
   encoder throws mid-write and the caller loses the whole column — the final
   answer content of a settled turn — over one exotic value inside it."
  [x]
  (cond (map? x) (persistent! (reduce-kv (fn [m k v]
                                           (assoc! m (json-key k) (json-encodable v)))
                                         (transient {})
                                         x))
        (coll? x) (mapv json-encodable x)
        (and (float? x) (not (Double/isFinite (double x)))) nil
        :else x))

(defn ->json
  "Serialize a value to a JSON TEXT column. Nil in, nil out."
  [m]
  (when m (json/write-json-str (json-encodable m))))

(defn <-json
  "Parse a JSON TEXT column. STRINGS-ONLY: keys come back as VERBATIM STRINGS -
   no `:key-fn keyword` re-keywordizing. Whatever needs an internal keyword
   shape converts at ONE named adapter, never here."
  [s]
  (when s (json/read-json s)))

(def canonical-wire-key->key
  "Inverse of `wire/canonical` for PERSISTED assistant messages: the canonical
   snake_case wire key -> the keyword the engine/Svar serializers dispatch on.
   Anything absent stays verbatim."
  {"role" :role
   "content" :content
   "model" :model
   "type" :type
   "text" :text
   "thinking" :thinking
   "thinking_signature" :thinking-signature
   "is_redacted" :redacted?
   "data" :data
   "id" :id
   "name" :name
   "input" :input
   "tool_use_id" :tool-use-id
   "is_error" :is-error
   "image_url" :image_url
   "source" :source
   "url" :url
   "detail" :detail
   "media_type" :media_type
   "cache_control" :cache_control})

(defn restore-canonical
  "Restore a persisted canonical wire envelope to the engine's keyword shape.
   Tool `:input` stays opaque so user map keys are never rewritten.

   Without this a replayed thinking block keeps `thinking_signature`/
   `is_redacted` string keys, misses Svar's keyword serializer, and Anthropic
   rejects the request with `Extra inputs are not permitted`."
  ([value] (restore-canonical value false))
  ([value opaque?]
   (cond opaque? value
         (vector? value) (mapv restore-canonical value)
         (map? value)
         (persistent! (reduce-kv
                        (fn [out k v]
                          (let [canonical-k (get canonical-wire-key->key k k)]
                            (assoc! out canonical-k (restore-canonical v (= :input canonical-k)))))
                        (transient {})
                        value))
         :else value)))

(defn <-json-canonical-lazy
  "Lazily restore ONE persisted canonical assistant message.
   Storage holds wire strings; replay needs the keyword envelope."
  [s]
  (delay (some-> s
                 <-json
                 restore-canonical)))

(defn normalize-status
  "Map runtime status keywords to the schema CHECK constraint values.
   Allowed: running, done, error, interrupted."
  [status]
  (case status
    (:success :done)
    "done"

    :error
    "error"

    (:cancelled :interrupted)
    "interrupted"

    :running
    "running"

    (->kw (or status :done))))

;; =============================================================================
;; Backend registry
;; =============================================================================

(defonce ^:private backends
  ;; {:sqlite {:ns 'com.blockether.vis.ext.persistance-sqlite.core}}
  (atom {}))

(defn register-backend!
  "Register a persistence backend implementation.

   `id`     - keyword identity, e.g. `:sqlite`.
   `ns-sym` - fully qualified namespace symbol that defines the backend
              functions (`db-open!`, `db-close!`, `db-log!`, every
              `store-*`/`db-*` fn used by this facade). Vars are
              resolved lazily via `ns-resolve` so REPL redefinition
              just works.

   Idempotent on `id`. Returns `id`."
  [id ns-sym]
  (when-not (keyword? id) (throw (ex-info "Backend id must be a keyword" {:id id})))
  (when-not (symbol? ns-sym) (throw (ex-info "Backend ns-sym must be a symbol" {:ns-sym ns-sym})))
  (swap! backends assoc id {:ns ns-sym})
  id)

(defn deregister-backend! [id] (swap! backends dissoc id) nil)

(defn registered-backends "Map of registered backends keyed by id." [] @backends)

;; ----------------------------------------------------------------------------
;; Auto-discovery
;;
;; There is no backend-specific scanner. The single source of truth
;; is `manifest/scan-extensions!`, which scans every
;; `META-INF/vis-extension/vis.edn` on the classpath and `require`s the
;; namespaces listed inside. Any loaded extension with `:ext/persistance`
;; entries lands in this registry as a side effect. The facade triggers a
;; scan on the first `db-create-connection!` so callers reach the registered backend
;; without wiring discovery themselves.
;; ----------------------------------------------------------------------------

(defn- pick-backend-id
  "Decide which backend handles this call. Honors an explicit
   `:backend` key on the spec/store; otherwise falls back to the
   single registered backend; otherwise throws."
  [db-spec-or-store]
  (or (when (map? db-spec-or-store) (:backend db-spec-or-store))
      (when (= 1 (count @backends)) (first (keys @backends)))
      (throw (ex-info (str "No persistence backend selected. "
                           (if (empty? @backends)
                             "No backends registered. Did you forget to require "
                             "Multiple backends registered, pass {:backend ...} in db-spec. ")
                           (when (empty? @backends)
                             "`com.blockether.vis.ext.persistance-sqlite.core`?"))
                      {:registered (vec (keys @backends))}))))

(defn- require-backend-ns!
  "Ensure the backend's heavyweight namespace has been loaded. Backends
   are typically registered through a lightweight `registrar` ns whose
   `:persistance/ns` points at the heavy ns; this fn does the deferred
   `(require ...)` so callers don't pay the load cost until they actually
   dispatch a backend call. Silent no-op when the ns is already loaded.

   SERIALIZED under Clojure's global require lock (what
   `requiring-resolve` uses): plain `require` is not thread-safe, and
   the gateway's first DB touch can be N concurrent virtual threads
   (parallel browser requests after a restart). Unserialized, a second
   thread could observe the HALF-LOADED namespace — seen live as
   \"Backend :sqlite ... does not implement 'db-open!'\" and as an
   unbound `taoensso.nippy/freeze` mid-turn."
  [bid ns-sym]
  (try (locking clojure.lang.RT/REQUIRE_LOCK (require ns-sym))
       (catch Throwable t
         (throw (ex-info
                  (str "Backend " bid " (" ns-sym ") failed to load: " (or (ex-message t) (str t)))
                  {:backend bid :ns ns-sym}
                  t)))))

(defn- resolve-impl
  "Resolve the var implementing `fn-name` on the chosen backend.
   Auto-loads the backend ns on first call so backends can ship as a
   tiny registrar + heavy implementation pair. Throws a useful error
   when the backend is missing the fn."
  [db-spec-or-store fn-name]
  (let
    [bid
     (pick-backend-id db-spec-or-store)

     ns-sym
     (get-in @backends [bid :ns])

     _
     (when-not ns-sym
       (throw (ex-info (str "Backend " bid " not registered")
                       {:backend bid :registered (vec (keys @backends))})))

     _
     (require-backend-ns! bid ns-sym)

     v
     (ns-resolve ns-sym fn-name)]

    (when-not v
      (throw (ex-info (str "Backend " bid " (" ns-sym ") does not implement '" fn-name "'")
                      {:backend bid :ns ns-sym :fn fn-name})))
    v))

(defn- normalize-spec
  "Reshape explicit-sqlite nested forms into the canonical shape
   backends accept. `:memory` is the canonical shorthand for the
   ephemeral in-process DB."
  [db-spec]
  (cond (and (map? db-spec) (= :sqlite (:backend db-spec)))
        (cond (:datasource db-spec) {:datasource (:datasource db-spec) :backend :sqlite}
              (:conn db-spec) {:conn (:conn db-spec) :backend :sqlite}
              (:path db-spec) (assoc {:backend :sqlite} :path (:path db-spec))
              :else db-spec)
        :else db-spec))

(defn- resolve-optional-impl
  "Resolve an optional backend var. Missing vars return nil; bad backend
   selection still throws because the store/spec itself is invalid.
   Auto-loads the backend ns the same way `resolve-impl` does."
  [db-spec-or-store fn-name]
  (let
    [bid
     (pick-backend-id db-spec-or-store)

     ns-sym
     (get-in @backends [bid :ns])]

    (when-not ns-sym
      (throw (ex-info (str "Backend " bid " not registered")
                      {:backend bid :registered (vec (keys @backends))})))
    (require-backend-ns! bid ns-sym)
    (some-> (ns-resolve ns-sym fn-name)
            deref)))

;; =============================================================================
;; Connection lifecycle
;; =============================================================================

(defn db-create-connection!
  "Open a persistence connection from `db-spec`.

   Common spec forms:
     nil              - no DB (returns nil)
     :memory          - in-memory ephemeral store (backend-defined)
     \"path/to.db\"   - file-backed store (backend-defined)
     {:backend :sqlite :path ...}     - explicit backend selection
     {:backend :sqlite :datasource ds} - caller-owned DataSource

   With a single registered backend, omitting `:backend` works and the
   facade tags the returned store map with the chosen backend so all
   subsequent facade calls dispatch correctly.

   Triggers `manifest/scan-extensions!` on first call so a freshly
   started JVM with no extensions yet required has a chance to load
   any backend-providing namespaces from the classpath."
  [db-spec]
  (manifest/scan-extensions!)
  (let
    [normalized
     (normalize-spec db-spec)

     bid
     (pick-backend-id (if (map? normalized) normalized {:backend (pick-backend-id {})}))

     f
     @(resolve-impl {:backend bid} 'db-open!)

     store
     (f normalized)]

    (cond (nil? store) nil
          (map? store) (assoc store :backend bid)
          :else store)))

(defn db-dispose-connection!
  [store]
  (when store
    (let [f @(resolve-impl store 'db-close!)]
      (f store))))

;; =============================================================================
;; Delegated API
;;
;; Every fn delegates to the selected backend adapter's var of the same
;; name. Keep this as a compact, readable index; add a new entry here only
;; after the matching fn lands in at least one backend.
;; =============================================================================

(defmacro ^:private defdelegate
  "Define a facade fn whose body resolves the matching backend var
   (using the first arg as the dispatch value) and applies it to
   the original args."
  [sym arglist]
  (let [bsym (gensym "backend-fn")]
    `(defn ~sym
       ~arglist
       (let [~bsym @(resolve-impl ~(first arglist) (quote ~sym))]
         (~bsym ~@arglist)))))

;; --- Iteration lifecycle ---
(defn db-store-iteration!
  "Same delegating shape as the macro-defined fns, but with input
   validation kept here so every backend gets the same precondition
   guarantees for free."
  [db-info opts]
  (when-not (map? opts)
    (throw (ex-info "db-store-iteration! opts must be a map" {:got (type opts)})))
  (when-not (:session-turn-id opts)
    (throw (ex-info "db-store-iteration! requires :session-turn-id" {:opts (keys opts)})))
  ((deref (resolve-impl db-info 'db-store-iteration!)) db-info opts))

;; --- Turn outcome ---

(def ^:const max-persisted-error-chars
  "Hard cap on ONE persisted DIAGNOSTIC string (256K chars).

   SQLite refuses any bound value over `SQLITE_MAX_LENGTH` (1e9 bytes) with
   `[SQLITE_TOOBIG]`, and the value carrying a turn's terminal error is the one
   most likely to be unbounded: a runtime message can quote the entire document
   that broke it. An error is a DIAGNOSTIC, so a truncated head is worth
   strictly more than the lost turn an oversized one costs. An answer's own
   content and the CTX snapshot are DATA and are never truncated here -- an
   oversized one degrades through the caller's outcome guard instead."
  (* 256 1024))

(defn bounded-error-text
  "Truncate ONE diagnostic string so the result NEVER exceeds `max-chars`,
   naming what was cut. A string already within the cap comes back identical, so
   a normal error is persisted byte for byte."
  ([s] (bounded-error-text s max-persisted-error-chars))
  ([s max-chars]
   (let
     [s
      (str s)

      n
      (count s)

      max-chars
      (long max-chars)]

     (if (<= n max-chars)
       s
       ;; Reserve room for the marker the cut itself produces, sized by the
       ;; WIDEST it can be, so the bounded string counts its own tail in.
       (let
         [marker
          (fn [keep]
            (str " ...<+" (- n (long keep)) " chars truncated>"))

          keep
          (max 0 (- max-chars (count (marker 0))))]

         (str (subs s 0 keep) (marker keep)))))))

(defn bound-error-data
  "Bound every string inside a structured terminal error, at any depth. Pure;
   nil in, nil out."
  [error]
  (when (some? error)
    (walk/postwalk (fn [x]
                     (if (string? x) (bounded-error-text x) x))
                   error)))

(defn- bound-content-errors
  "Bound the diagnostic text of ERROR blocks in a turn's canonical content, and
   ONLY those: prose, images and every other block are the answer itself and are
   persisted verbatim."
  [content]
  (if (sequential? content)
    (mapv (fn [block]
            (if (and (map? block) (= "error" (get block "type"))) (bound-error-data block) block))
          content)
    content))

(defn db-update-session-turn!
  "Write a turn's terminal outcome. Same delegating shape as the macro-defined
   fns, with the DIAGNOSTIC text bound here so every backend gets the same
   guarantee for free: the write that records HOW a turn ended can never be lost
   to an unbounded error message (see [[max-persisted-error-chars]])."
  [db-info session-turn-id opts]
  ((deref (resolve-impl db-info 'db-update-session-turn!))
    db-info
    session-turn-id
    (cond-> opts
      (some? (:error opts))
      (update :error bound-error-data)

      (some? (:content opts))
      (update :content bound-content-errors))))

;; --- Logging ---
(defdelegate db-log! [db-info opts])

;; --- Workspace ---
(defdelegate db-workspace-insert! [db-info opts])

(defdelegate db-workspace-update-state! [db-info workspace-id new-state])

;; Label override + focus stamp + per-repo focus pointer.
(defdelegate db-workspace-update-label! [db-info workspace-id label])

(defdelegate db-workspace-touch-focus! [db-info workspace-id])

(defdelegate db-repo-focus-get [db-info repo-id])

(defdelegate db-repo-focus-set! [db-info repo-id workspace-id])

(defdelegate db-workspace-get [db-info workspace-id])

(defn db-workspace-list-by-repo
  ([db-info repo-id] ((deref (resolve-impl db-info 'db-workspace-list-by-repo)) db-info repo-id))
  ([db-info repo-id state-set]
   ((deref (resolve-impl db-info 'db-workspace-list-by-repo)) db-info repo-id state-set)))

(defdelegate db-workspace-list-drafts [db-info])

(defdelegate db-workspace-for-session [db-info session-state-id])

(defdelegate db-session-state-list-for-workspace [db-info workspace-id])

(defdelegate db-session-state-set-workspace! [db-info session-state-id workspace-id])

;; --- Session lifecycle ---
(defdelegate db-store-session! [db-info opts])

(defdelegate db-get-session [db-info ref])

(defdelegate db-resolve-session-id [db-info sel])

(defdelegate db-list-sessions [db-info channel])

(defdelegate db-search-session-ids [db-info channel query])

(defdelegate db-search-session-matches [db-info channel query])

(defdelegate db-find-session-by-external [db-info channel ext-id])

(defdelegate db-update-session-title! [db-info ref title])

(defdelegate db-claim-session! [db-info ref])

(defdelegate db-delete-session-tree! [db-info id])

(defdelegate db-fork-session! [db-info session-id opts])

(defdelegate db-fork-session-at-turn! [db-info session-id opts])

(defdelegate db-list-session-states [db-info session-id])

(defdelegate db-latest-session-state-id [db-info session-id])

;; Per-session model preference (session_soul.llm_pref_provider + llm_pref_model) — shared by every
;; channel; read by the engine at turn start (see session-model + loop.clj).
(defdelegate db-get-session-model-pref [db-info session-id])

(defdelegate db-set-session-model-pref! [db-info session-id provider model])

;; --- Projects (cross-channel) + movable project sessions + ownership (V6/V7) ---
(defdelegate db-get-project [db-info project-id])

(defdelegate db-list-projects [db-info opts])

(defdelegate db-get-project-by-root [db-info owner-id root])

(defdelegate db-create-project! [db-info opts])

(defdelegate db-update-project! [db-info project-id opts])

(defdelegate db-delete-project! [db-info project-id])

(defdelegate db-set-session-project! [db-info session-id project-id])

(defdelegate db-reorder-project-sessions! [db-info project-id session-ids])

(defdelegate db-adopt-and-reorder-project-sessions! [db-info project-id session-ids])

;; --- Turn lifecycle ---
(defdelegate db-store-session-turn! [db-info opts])

;; db-update-session-turn! is an explicit defn above: it bounds diagnostics.

(defdelegate db-list-session-turns-by-status [db-info status])

(defdelegate db-list-session-turns [db-info session-ref])

(defn db-session-turn-stats
  "Per-session turn aggregates. 1-arity: the whole store, `{soul-id-str
   {:turn-count n :latest-turn-at Date}}`. 2-arity: ONE session's stats
   unwrapped (nil when unknown), so a single-session read never scans the
   whole store."
  ([db-info] ((deref (resolve-impl db-info 'db-session-turn-stats)) db-info))
  ([db-info session-id] ((deref (resolve-impl db-info 'db-session-turn-stats)) db-info session-id)))

(defdelegate db-session-usage-stats [db-info session-id])

(defdelegate db-retry-session-turn! [db-info session-turn-soul-id opts])

(defdelegate db-list-session-turn-states [db-info session-turn-id])

(defdelegate db-list-turn-attachments [db-info session-turn-soul-id])

(defdelegate db-list-turns-attachments [db-info session-turn-soul-ids])

(defdelegate db-list-turn-all-attachments [db-info session-turn-soul-id])

(defdelegate db-list-session-attachments [db-info session-id])

(defdelegate db-list-session-attachments-meta [db-info session-id])

(defdelegate db-list-session-turn-iterations [db-info session-turn-ref])

(defdelegate db-list-iteration-attachments [db-info iteration-id])

(defdelegate db-list-iterations-attachments [db-info iteration-ids])

(defdelegate db-list-iteration-attachments-meta [db-info iteration-id])

(defdelegate db-list-iterations-attachments-meta [db-info iteration-ids])

(defdelegate db-read-attachment [db-info attachment-id])

(defdelegate db-append-iteration-attachment! [db-info iteration-id att])

;; --- Full-text search ---
(def search-query-dsl-doc
  "Canonical, BACKEND-NEUTRAL search-query DSL — the single source of truth for
   what a `db-search` query means, independent of the engine underneath
   (SQLite FTS5 today, Postgres tsvector/tsquery planned). The query is DATA,
   not an engine operator string: callers/agent compose a value, each backend
   RENDERS it to its native full-text query. Because every leaf term is escaped
   by the renderer, punctuation/quotes in code text are inert — a query can
   never be `broken` by its content, so there is no `parse mode` to choose.

   A query node is one of:
     \"word\"                          bare string: implicit-AND of its words
     {:term   \"w\"}                   one term
     {:phrase \"a b\"}                 adjacent phrase (verbatim run)
     {:prefix \"wor\"}                 prefix match (wor…)
     {:all  [node …]}                AND of children
     {:any  [node …]}                OR of children
     {:not  node}                    negation — ONLY as a child of :all
                                     (`{:all [\"a\" {:not \"b\"}]}` = a, not b)
     {:near {:terms [\"a\" \"b\" …]    the terms within :within tokens
             :within k}}

   Portability (validated node-by-node) — the core nodes map cleanly to every
   boolean FTS engine:
                  SQLite FTS5      Postgres tsquery     MariaDB/MySQL BOOLEAN
     :term        \"w\"              'w'                  +w
     :all         a AND b          a & b                +a +b
     :any         a OR b           a | b                (a b)
     :not(in all) a NOT x          a & !x               +a -x
     :phrase      \"a b\"            a <-> b              \"a b\"
     :prefix      \"w\"*            'w':*                w*
     :near k      NEAR(a b,k) ✓    <N> exact/ordered ✗  \"a b\" @k ✓

   :near is the ONLY divergence: SQLite and MariaDB do within-k natively;
   Postgres has only `<N>` (exact distance, ordered), so a PG adapter must
   OR-expand it or DEGRADE :near -> :all (AND). Per the contract above, an
   engine that can't express a node degrades it — it never rejects well-formed
   DSL. Each adapter also owns its own term ESCAPING (FTS5 double-quote, PG
   lexemes, MySQL boolean-mode metachar stripping); the DSL gives it clean
   structure to do so. Keeping the DSL here (data, not dialect) is what makes a
   second backend a localized add.")

(defn db-search
  "Backend-neutral full-text search facade. Delegates to the registered
   persistence backend, which RENDERS the neutral query DSL into its native
   full-text query and runs it. No caller passes an engine dialect — only the
   DSL in `search-query-dsl-doc`.

   `query` is the DSL — a string (implicit-AND of its words) or a DSL map.
   `opts`:
     :owner-table  restrict to one owner table (string)
     :field        restrict to one indexed field (string)
     :limit        max hits (backend default applies when nil)

   Returns a vector of hits sorted by relevance (best first), each
   `{:owner-table :owner-id :field :snippet :rank}`. Backends MUST honor the
   DSL; an engine that cannot express a node should degrade it (e.g. :near ->
   :all), never reject well-formed DSL. A MALFORMED query (e.g. a lone :not)
   may throw — that is a DSL logic error, distinct from un-matchable content."
  [db-info query opts]
  ((deref (resolve-impl db-info 'db-search)) db-info query opts))

;; --- Turn history (read-only projection) ---
(defdelegate db-turn-history [db-info session-ref])

;; --- CTX snapshots (per-turn string-keyed session_* state, Nippy in session_turn_state.ctx) ---
(defdelegate db-load-latest-ctx [db-info session-id])

(defdelegate db-load-ctx-history [db-info session-id])

;; --- Extension aggregate sidecars ---
(defdelegate db-create-extension-aggregate! [db-info opts])

(defdelegate db-put-extension-aggregate! [db-info opts])

(defdelegate db-get-extension-aggregate [db-info opts])

(defdelegate db-list-extension-aggregates [db-info opts])

(defdelegate db-delete-extension-aggregates! [db-info opts])

(defdelegate db-swap-extension-aggregate! [db-info opts f args])

;; =============================================================================
;; Error translation
;;
;; Frontends (TUI, CLI) all surface persistence exceptions in
;; chat bubbles. The facade stays backend-agnostic: adapters may expose
;; `db-error->user-message`; the first non-empty translation wins.
;; =============================================================================

(defn- backend-error-translators
  []
  (try (manifest/scan-extensions!) (catch Throwable _ nil))
  (keep (fn [[_ {:keys [ns]}]]
          (some-> (ns-resolve ns 'db-error->user-message)
                  deref))
        @backends))

(defn db-error->user-message
  "Translate a persistence exception into something a human can act on.
   Backend adapters own backend-specific recognition; unknown errors fall
   back to `(ex-message e)`."
  [^Throwable e]
  (or (some (fn [f]
              (try (when-let [message (f e)]
                     (when (seq (str message)) (str message)))
                   (catch Throwable _ nil)))
            (backend-error-translators))
      (ex-message e)
      "Internal error"))

;; =============================================================================
;; Process-wide shared connection (singleton helper)
;;
;; vis runs every channel (TUI, CLI) against one persistence
;; store per process. Owning the singleton here - instead of in any
;; particular frontend - keeps the DB lifecycle behind the persistence
;; facade. Backend adapters may expose `db-store-stale?` for
;; adapter-specific file/handle replacement detection.
;; =============================================================================

(defonce ^:private shared-conn (atom nil))

(defn- store-stale?
  [store db-spec]
  (boolean (when store
             (when-let [f (resolve-optional-impl store 'db-store-stale?)]
               (f store (normalize-spec db-spec))))))

(defn db-shared-connection!
  "Return the process-wide shared persistence connection for `db-spec`,
   opening it on first call and caching the handle for the lifetime of
   the JVM. Subsequent calls return the cached handle regardless of
   the `db-spec` argument - the singleton intentionally pins to the
   first spec it saw.

  Pair with `db-dispose-shared-connection!` on process shutdown."
  [db-spec]
  (or (when-let [cur @shared-conn]
        (when-not (store-stale? cur db-spec) cur))
      ;; SERIALIZED open. The previous swap! ran `db-create-connection!`
      ;; INSIDE the swap fn — a side effect swap! may run N times under
      ;; contention, and N threads racing the first DB touch each opened
      ;; the SQLite file concurrently: observed live as
      ;; java.nio.channels.OverlappingFileLockException on 11/12 parallel
      ;; first requests through the gateway. One thread opens; the rest
      ;; wait on the monitor and reuse.
      (locking shared-conn
        (let [cur @shared-conn]
          (if (and cur (not (store-stale? cur db-spec)))
            cur
            (do (when cur (try (db-dispose-connection! cur) (catch Exception _ nil)))
                (let [fresh (db-create-connection! db-spec)]
                  (reset! shared-conn fresh)
                  fresh)))))))

(defn db-dispose-shared-connection!
  "Close the shared connection if one is open. Idempotent."
  []
  (when-let [c @shared-conn]
    (try (db-dispose-connection! c) (catch Exception _ nil))
    (reset! shared-conn nil)))
