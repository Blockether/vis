(ns com.blockether.vis.internal.persistance.core
  "Persistence facade: the `Store` protocol, the connection lifecycle and the
   guarantees every backend gets before it sees a call.

   SQLite is the one backend Vis ships. Its namespace loads on the first store
   operation, not with this facade (see `sqlite`), so commands that never touch
   the store skip ~480 ms of JDBC/Hikari/Flyway class loading on a cold JVM.
   Every `Store` op forwards to that backend, which hands the facade its
   implementation through `store-implementation`: a backend missing an op fails
   to compile.

   Frontends still call `db-error->user-message` here; the backend owns the
   actual translation. Same for the store-staleness check the process-wide
   shared connection uses."
  (:require [clojure.walk :as walk]))

;; Full-text search query DSL

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

;; Store protocol

(defprotocol Store
  "Every operation a persistence backend implements. The first argument is the
   store `db-create-connection!` opened, or nil when there is none; every value
   dispatches to the SQLite backend."
  ;; --- Logging ---
  (db-log! [db-info opts])
  (db-workspace-insert! [db-info opts])
  (db-workspace-update-state! [db-info workspace-id new-state])
  ;; Label override + focus stamp + per-repo focus pointer.
  (db-workspace-update-label! [db-info workspace-id label])
  (db-workspace-touch-focus! [db-info workspace-id])
  (db-repo-focus-get [db-info repo-id])
  (db-repo-focus-set! [db-info repo-id workspace-id])
  (db-workspace-get [db-info workspace-id])
  (db-workspace-list-by-repo [db-info repo-id]
                             [db-info repo-id state-set])
  (db-workspace-list-drafts [db-info])
  (db-workspace-for-session [db-info session-state-id])
  (db-session-state-list-for-workspace [db-info workspace-id])
  (db-session-state-set-workspace! [db-info session-state-id workspace-id])
  ;; --- Session lifecycle ---
  (db-store-session! [db-info opts])
  (db-get-session [db-info ref])
  (db-resolve-session-id [db-info sel])
  (db-list-sessions [db-info channel])
  (db-search-session-ids [db-info channel query])
  (db-search-session-matches [db-info channel query])
  (db-find-session-by-external [db-info channel ext-id])
  (db-update-session-title! [db-info ref title])
  (db-get-session-goal [db-info session-id])
  (db-compare-session-goal! [db-info session-id revision goal])
  (db-claim-session! [db-info ref])
  (db-delete-session-tree! [db-info id])
  (db-fork-session! [db-info session-id opts])
  (db-fork-session-at-turn! [db-info session-id opts])
  (db-agent-info [db-info session-id])
  (db-agent-list [db-info leader-id])
  (db-agent-checkpoint [db-info session-id])
  (db-agent-update! [db-info session-id changes])
  (db-agent-claim-iteration! [db-info session-id])
  (db-routing-locked? [db-info session-id])
  (db-lock-routing! [db-info session-id locked?])
  (db-list-session-states [db-info session-id])
  (db-latest-session-state-id [db-info session-id])
  (db-get-session-prompt-cache-state [db-info session-state-id])
  (db-set-session-prompt-cache-state! [db-info session-state-id state])
  ;; Per-session model preference (session_soul.llm_pref_provider + llm_pref_model) — shared by every
  ;; channel; read by the engine at turn start (see session-model + loop.clj).
  (db-get-session-model-pref [db-info session-id])
  (db-set-session-model-pref! [db-info session-id provider model])
  ;; --- Projects (cross-channel) + movable project sessions + ownership (V6/V7) ---
  (db-get-project [db-info project-id])
  (db-list-projects [db-info opts])
  (db-get-project-by-root [db-info owner-id root])
  (db-create-project! [db-info opts])
  (db-update-project! [db-info project-id opts])
  (db-delete-project! [db-info project-id])
  (db-set-session-project! [db-info session-id project-id])
  ;; The human's star on a session soul. Backend-owned: the gateway is the ONE
  ;; place a star lives, so every client of it reads the same answer.
  (db-set-session-favorite! [db-info session-id is-favorite])
  ;; The human's archive on a session soul. Backend-owned for the same reason the
  ;; star is: a session put out of sight is out of sight on every client of this
  ;; gateway, not only on the device that archived it.
  (db-set-session-archived! [db-info session-id archived?])
  (db-reorder-project-sessions! [db-info project-id session-ids])
  (db-adopt-and-reorder-project-sessions! [db-info project-id session-ids])
  ;; --- Session groups: the human's own groups inside ONE project (V8) ---
  (db-get-session-group [db-info group-id])
  (db-list-session-groups [db-info project-id opts])
  (db-archived-session-group-ids [db-info])
  (db-create-session-group! [db-info project-id opts])
  (db-update-session-group! [db-info group-id opts])
  (db-delete-session-group! [db-info group-id])
  (db-set-session-group! [db-info session-id group-id])
  (db-session-group-session-ids [db-info group-id])
  (db-project-session-ids [db-info project-id])
  ;; --- Read marks: how far a reader has read each conversation (the "NEW" badge) ---
  (db-session-read-marks [db-info reader-id])
  (db-seed-session-read-marks! [db-info reader-id marks])
  (db-mark-session-read! [db-info reader-id session-id seen-answers])
  ;; --- Turn lifecycle ---
  (db-store-session-turn! [db-info opts])
  (db-update-session-turn! [db-info session-turn-id opts]
    "Write a turn's terminal outcome. The facade bounds the DIAGNOSTIC text before
     the backend sees it, so the write that records HOW a turn ended can never be
     lost to an unbounded error message (see [[max-persisted-error-chars]]).")
  (db-store-iteration! [db-info opts]
    "Store one iteration row. The facade refuses `opts` that is not a map or lacks
     `:session-turn-id` before the backend sees it.")
  (db-list-session-turns-by-status [db-info status])
  (db-list-session-turns [db-info session-ref])
  (db-list-session-turns-meta [db-info session-ref])
  (db-read-session-turn [db-info session-ref turn-ref])
  (db-session-turn-stats [db-info]
                         [db-info session-id]
    "Per-session turn aggregates. 1-arity: the whole store, `{soul-id-str
     {:turn-count n :latest-turn-at Date}}`. 2-arity: ONE session's stats
     unwrapped (nil when unknown), so a single-session read never scans the
     whole store.")
  (db-session-usage-stats [db-info session-id])
  (db-retry-session-turn! [db-info session-turn-soul-id opts])
  (db-list-session-turn-states [db-info session-turn-id])
  (db-list-turn-attachments [db-info session-turn-soul-id])
  (db-set-turn-attachment-transcription! [db-info session-turn-soul-id position transcription
                                          segments])
  (db-list-turns-attachments [db-info session-turn-soul-ids])
  (db-list-turn-all-attachments [db-info session-turn-soul-id])
  (db-list-session-attachments [db-info session-id])
  (db-list-session-attachments-meta [db-info session-id])
  (db-list-session-turn-iterations [db-info session-turn-ref])
  (db-list-session-turns-iterations [db-info session-turn-ids])
  (db-list-session-turns-iterations-meta [db-info session-turn-ids])
  (db-list-iterations [db-info iteration-ids])
  (db-latest-turn-request-usage [db-info session-turn-id])
  (db-list-iteration-attachments [db-info iteration-id])
  (db-list-iterations-attachments [db-info iteration-ids])
  (db-list-iteration-attachments-meta [db-info iteration-id])
  (db-list-iterations-attachments-meta [db-info iteration-ids])
  (db-read-attachment [db-info attachment-id])
  (db-append-iteration-attachment! [db-info iteration-id att])
  ;; --- Full-text search ---
  (db-search [db-info query opts]
    "Backend-neutral full-text search. The backend RENDERS the neutral query DSL
     into its native full-text query and runs it. No caller passes an engine
     dialect — only the DSL in `search-query-dsl-doc`.

     `query` is the DSL — a string (implicit-AND of its words) or a DSL map.
     `opts`:
       :owner-table  restrict to one owner table (string)
       :field        restrict to one indexed field (string)
       :limit        max hits (backend default applies when nil)

     Returns a vector of hits sorted by relevance (best first), each
     `{:owner-table :owner-id :field :snippet :rank}`. Backends MUST honor the
     DSL; an engine that cannot express a node should degrade it (e.g. :near ->
     :all), never reject well-formed DSL. A MALFORMED query (e.g. a lone :not)
     may throw — that is a DSL logic error, distinct from un-matchable content.")
  ;; --- Turn history (read-only projection) ---
  (db-turn-history [db-info session-ref])
  ;; --- CTX snapshots (per-turn string-keyed session_* state, Nippy in session_turn_state.ctx) ---
  (db-checkpoint-session-turn-ctx! [db-info session-turn-id state-id ctx])
  (db-load-latest-ctx [db-info session-id])
  (db-load-ctx-history [db-info session-id])
  ;; --- Extension aggregate sidecars ---
  (db-create-extension-aggregate! [db-info opts])
  (db-put-extension-aggregate! [db-info opts])
  (db-get-extension-aggregate [db-info opts])
  (db-list-extension-aggregates [db-info opts])
  (db-delete-extension-aggregates! [db-info opts])
  (db-swap-extension-aggregate! [db-info opts f args])
  ;; --- Improve register ---
  (db-improve-list [db-info opts])
  (db-improve-get [db-info id])
  (db-improve-create! [db-info attrs])
  (db-improve-update! [db-info id attrs])
  (db-improve-project-ids [db-info])
  (db-improve-apply-review! [db-info proposal still-current?])
  ;; --- Council ---
  (db-council-source [db-info sid source])
  (db-council-get [db-info id])
  (db-council-replay [db-info sid key])
  (db-council-insert! [db-info row recipients infer-reply?])
  (db-council-bind-wake! [db-info entry-id sid activation])
  (db-council-page [db-info gid thread roots? after limit])
  (db-council-pending [db-info sid activation gid after limit])
  (db-council-unanswered [db-info sid ids])
  (db-council-delivered! [db-info sid ids])
  (db-council-interrupt! [db-info sid activation])
  (db-council-unavailable! [db-info sid id])
  ;; --- Activity ---
  (db-activity-apply! [db-info sid aid event])
  (db-activity-settle! [db-info aid outcome summary])
  (db-activity-page [db-info sid aid opts]))

(defmacro store-implementation
  "Expand, inside a backend namespace, to its `Store` op map: every op keyed to
   the backend's own fn of the same name. A backend missing an op fails to
   compile instead of failing at its first call."
  []
  (into {}
        (map (fn [op]
               [op (symbol (name op))]))
        (keys (:sigs Store))))

;; Turn outcome

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
   (let [s
         (str s)

         n
         (count s)

         max-chars
         (long max-chars)]

     (if (<= n max-chars)
       s
       ;; Reserve room for the marker the cut itself produces, sized by the
       ;; WIDEST it can be, so the bounded string counts its own tail in.
       (let [marker
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

(defn- bound-turn-outcome
  "Bound the diagnostic fields of a turn's terminal outcome: the error and the
   ERROR content blocks."
  [opts]
  (cond-> opts
    (some? (:error opts))
    (update :error bound-error-data)

    (some? (:content opts))
    (update :content bound-content-errors)))

(defn- check-iteration-opts!
  [opts]
  (when-not (map? opts)
    (throw (ex-info "db-store-iteration! opts must be a map" {:got (type opts)})))
  (when-not (:session-turn-id opts)
    (throw (ex-info "db-store-iteration! requires :session-turn-id" {:opts (keys opts)}))))

;; Backend

(defn- load-backend
  "Load the backend namespace and return its `backend` map. `requiring-resolve`
   loads under Clojure's global require lock, so a thread can never observe a
   half-loaded namespace. A failure keeps its cause and names the namespace."
  [sym]
  (try @(requiring-resolve sym)
       (catch Throwable t
         (throw (ex-info (str "Persistence backend " (namespace sym)
                              " failed to load: " (or (ex-message t) (str t)))
                         {:ns (symbol (namespace sym))}
                         t)))))

(def ^:private sqlite
  "The SQLite backend: `{:open :close :stale? :error-message :implementation}`.
   Loaded once, by the first store operation; concurrent first touches (parallel
   gateway requests after a restart) wait for that one load."
  (delay (load-backend 'com.blockether.vis.internal.persistance.sqlite.core/backend)))

(defn- backend-op
  "The backend's fn for the `Store` op `op`."
  [op]
  (get (:implementation @sqlite) op))

(defn- forward
  "A `Store` op that forwards its call to the backend."
  [op]
  (fn [db-info & args]
    (apply (backend-op op) db-info args)))

(def ^:private store-ops
  "Every `Store` op forwarded to the backend, with the facade's own checks and
   bounds applied first, so every backend gets them for free."
  (assoc (into {} (map (juxt identity forward)) (keys (:sigs Store)))
    :db-store-iteration! (fn [db-info opts]
                           (check-iteration-opts! opts)
                           ((backend-op :db-store-iteration!) db-info opts))
    :db-update-session-turn!
    (fn [db-info session-turn-id opts]
      ((backend-op :db-update-session-turn!) db-info session-turn-id (bound-turn-outcome opts)))))

;; Extended once, while this namespace loads: re-exports copy the protocol fns,
;; and a later `extend` would rebind them behind those copies.
(extend nil
  Store
    store-ops)

(extend Object
  Store
    store-ops)

;; Connection lifecycle

(defn- check-spec!
  "Refuse a spec naming a backend Vis does not ship, before any backend loads."
  [db-spec]
  (let [backend (when (map? db-spec) (:backend db-spec))]
    (when-not (contains? #{nil :sqlite} backend)
      (throw (ex-info (str "Unknown persistence backend " backend)
                      {:backend backend :known [:sqlite]})))))

(defn db-create-connection!
  "Open a persistence connection from `db-spec`.

   Common spec forms:
     nil              - no DB (returns nil)
     :memory          - in-memory ephemeral store
     \"path/to.db\"   - file-backed store
     {:backend :sqlite :path ...}     - explicit backend selection
     {:backend :sqlite :datasource ds} - caller-owned DataSource

   SQLite is the only backend; a spec naming another `:backend` is refused."
  [db-spec]
  (when (some? db-spec) (check-spec! db-spec) ((:open @sqlite) db-spec)))

(defn db-dispose-connection! [store] (when store ((:close @sqlite) store)))

;; Error translation
;;
;; Frontends (TUI, CLI) all surface persistence exceptions in chat bubbles. The
;; backend recognizes its own errors once it has loaded; anything else falls
;; back to the exception message.

(defn db-error->user-message
  "Translate a persistence exception into something a human can act on.
   The backend owns backend-specific recognition; unknown errors fall
   back to `(ex-message e)`."
  [^Throwable e]
  (or (when (realized? sqlite)
        (try (when-let [message ((:error-message @sqlite) e)]
               (when (seq (str message)) (str message)))
             (catch Throwable _ nil)))
      (ex-message e)
      "Internal error"))

;; Process-wide shared connection (singleton helper)
;;
;; vis runs every channel (TUI, CLI) against one persistence
;; store per process. Owning the singleton here - instead of in any
;; particular frontend - keeps the DB lifecycle behind the persistence
;; facade. The backend detects file/handle replacement (`:stale?`).

(defonce ^:private shared-conn (atom nil))

(defn- store-stale? [store db-spec] (boolean (when store ((:stale? @sqlite) store db-spec))))

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
