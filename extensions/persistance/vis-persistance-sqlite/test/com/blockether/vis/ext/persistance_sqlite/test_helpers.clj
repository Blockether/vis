(ns com.blockether.vis.ext.persistance-sqlite.test-helpers
  "Shared test utilities - in-memory SQLite store via lazytest context.

   Usage in test ns:
     (require '[com.blockether.vis.ext.persistance-sqlite.test-helpers :as h])
     (h/use-mem-store!)

   Then in each `it`:
     (let [s (h/store)] ...)

   Each test gets an isolated in-memory DB. No manual setup/teardown."
  (:require [com.blockether.vis.core :as vis]
            ;; Register the SQLite backend so `vis/db-create-connection!` can
            ;; dispatch to it. Production wires this through classpath manifest
            ;; discovery; tests need it explicit because requiring `core` no
            ;; longer self-registers (see `registrar.clj` for the lazy-load
            ;; split rationale + load-cost numbers).
            [com.blockether.vis.ext.persistance-sqlite.registrar]
            [honey.sql :as sql]
            [lazytest.core :as lt]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [taoensso.nippy :as nippy]))

(def ^:dynamic *store* nil)

(defn store "Get the current test's in-memory store." [] *store*)

(defn use-mem-store!
  "Call at top-level in a test ns to get a fresh in-memory SQLite store
   for each test. Access via `(h/store)`."
  []
  (lt/set-ns-context! [(lt/around-each [f]
                                       (let [s (vis/db-create-connection! :memory)]
                                         (try (binding [*store* s]
                                                (f))
                                              (finally (vis/db-dispose-connection! s)))))]))

(defn raw-query
  "Execute raw HoneySQL against the store's datasource."
  [store q]
  (jdbc/execute! (:datasource store) (sql/format q) {:builder-fn rs/as-unqualified-lower-maps}))

(defn raw-count
  "Count rows in a table, optionally with a where clause.

   Note: the result-set builder is `as-unqualified-lower-maps`, which
   returns keys exactly as the JDBC driver reports them. HoneySQL
   default-rewrites the alias `:row-count` to SQL `row_count`, so the
   row key is `:row_count` (underscore), not `:row-count` (hyphen).
   This helper hides that asymmetry from callers."
  ([store table]
   (:row_count (first (raw-query store {:select [[[:count :*] :row_count]] :from [table]}))))
  ([store table where]
   (:row_count
     (first (raw-query store {:select [[[:count :*] :row_count]] :from [table] :where where})))))

(defn thaw-blob "Thaw a nippy BLOB from a raw query result." [^bytes bs] (when bs (nippy/thaw bs)))

(defn- fresh-workspace!
  "Insert a lightweight workspace row (NO rift clone) rooted at a
   throwaway temp dir, satisfying `session_state.workspace_id` (NOT
   NULL, 1:1). Tests never `apply`, so a real CoW clone of cwd would
   only be slow and litter ~/.rifts."
  [store]
  (let
    [root
     (.getCanonicalPath (.toFile (java.nio.file.Files/createTempDirectory
                                   "vis-test-ws"
                                   (make-array java.nio.file.attribute.FileAttribute 0))))

     id
     (str (java.util.UUID/randomUUID))]

    (:id (vis/db-workspace-insert!
           store
           {:id id :repo-id "test" :repo-root root :root root :state :active :fork-ms 0}))))

(defn store-session!
  "Test wrapper for `vis/db-store-session!` that injects a fresh
   workspace per call (session_state.workspace_id is NOT NULL, 1:1 with
   workspace) via `fresh-workspace!` when no `:workspace-id` supplied.

   Use everywhere the test would have called `vis/db-store-session!`
   directly with a plain opts map."
  [store opts]
  (let [ws-id (or (:workspace-id opts) (fresh-workspace! store))]
    (vis/db-store-session! store (assoc opts :workspace-id ws-id))))

(defn fork-session!
  "Test wrapper for `vis/db-fork-session!` that injects a fresh
   workspace when no `:workspace-id` was supplied. Each fork gets its
   own workspace row to satisfy `UNIQUE(session_state.workspace_id)`."
  [store session-id opts]
  (let [ws-id (or (:workspace-id opts) (fresh-workspace! store))]
    (vis/db-fork-session! store session-id (assoc opts :workspace-id ws-id))))

(defn fork-session-at-turn!
  "Test wrapper for `vis/db-fork-session-at-turn!` that injects a fresh
   workspace when no `:workspace-id` was supplied (session_state.workspace_id
   is NOT NULL, 1:1 with workspace)."
  [store session-id opts]
  (let [ws-id (or (:workspace-id opts) (fresh-workspace! store))]
    (vis/db-fork-session-at-turn! store session-id (assoc opts :workspace-id ws-id))))

(defn store-iteration!
  "Test wrapper for `vis/db-store-iteration!` that wraps the old flat
   `:code`/`:result`/`:error` test ergonomics into a canonical single-form
   `:forms` envelope vec — exactly what an iter with one top-level form
   would produce in production. Fixtures that already pass `:forms`
   directly are forwarded untouched.

   THIS IS A TEST CONVENIENCE ONLY. Production callers (loop.clj) pass
   `:forms` directly — there is no flat path on the persistance core.
   Tests get this shim purely so fixtures stay readable as
   `{:code \"(+ 1 1)\" :result 2}` instead of spelling out the envelope.

   Multi-form fixtures (those passing `:forms`) skip the wrap entirely.
   Mixing the two (passing BOTH `:forms` and `:result`/`:error`) is a
   fixture bug — the `:forms` wins and `:result`/`:error` are dropped."
  [store opts]
  (let
    [{:keys [forms code result error] :as opts}
     opts

     forms-vec
     (cond (seq forms) (vec forms)
           (or (contains? opts :result) (some? error))
           [(cond-> {:scope nil :tag :observation :src (str code)}
              (contains? opts :result)
              (assoc :result result)

              (some? error)
              (assoc :error error))]
           :else nil)

     opts
     (cond-> (dissoc opts :result :error)
       forms-vec
       (assoc :forms forms-vec))]

    (vis/db-store-iteration! store opts)))
