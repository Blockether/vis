(ns com.blockether.vis.internal.persistance.sqlite.test-helpers
  "Shared test utilities - in-memory SQLite store via lazytest context.

   Usage in test ns:
     (require '[com.blockether.vis.internal.persistance.sqlite.test-helpers :as h])
     (h/use-mem-store!)

   Then in each `it`:
     (let [s (h/store)] ...)

   Each test gets an isolated in-memory DB. No manual setup/teardown."
  (:require [com.blockether.vis.internal.persistance.core :as persistance]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [honey.sql :as sql]
            [lazytest.core :as lt]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [taoensso.nippy :as nippy]))

(def ^:dynamic *store* nil)

(defn store "Get the current test's in-memory store." [] *store*)

(defn use-mem-store!
  "Call at top-level in a test ns to get a fresh in-memory SQLite store
   for each test. Access via `(h/store)`. Optional toggle overrides are restored after each test."
  ([] (use-mem-store! {}))
  ([overrides]
   (lt/set-ns-context! [(lt/around-each [f]
                                        (let [before (into {}
                                                           (map (fn [id]
                                                                  [id (toggles/value-of id)]))
                                                           (keys overrides))]
                                          (try (doseq [[id value] overrides]
                                                 (toggles/set-value! id value))
                                               (let [s (persistance/db-create-connection! :memory)]
                                                 (try (binding [*store* s]
                                                        (f))
                                                      (finally (persistance/db-dispose-connection!
                                                                 s))))
                                               (finally (doseq [[id value] before]
                                                          (toggles/set-value! id value))))))])))

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
  (let [root
        ;; The JVM deletes the empty root at exit. Without that, the suite left
        ;; about 200,000 directories in the user's temp directory.
        (.getCanonicalPath (doto (.toFile (java.nio.file.Files/createTempDirectory
                                            "vis-test-ws"
                                            (make-array java.nio.file.attribute.FileAttribute 0)))
                             .deleteOnExit))

        id
        (str (java.util.UUID/randomUUID))]

    (:id (persistance/db-workspace-insert!
           store
           {:id id :repo-id "test" :repo-root root :root root :state :active :fork-ms 0}))))

(defn store-session!
  "Test wrapper for `persistance/db-store-session!` that injects a fresh
   workspace per call (session_state.workspace_id is NOT NULL, 1:1 with
   workspace) via `fresh-workspace!` when no `:workspace-id` supplied.

   Use everywhere the test would have called `persistance/db-store-session!`
   directly with a plain opts map."
  [store opts]
  (let [ws-id (or (:workspace-id opts) (fresh-workspace! store))]
    (persistance/db-store-session! store (assoc opts :workspace-id ws-id))))

(defn fork-session!
  "Test wrapper for `persistance/db-fork-session!` that injects a fresh
   workspace when no `:workspace-id` was supplied. Each fork gets its
   own workspace row to satisfy `UNIQUE(session_state.workspace_id)`."
  [store session-id opts]
  (let [ws-id (or (:workspace-id opts) (fresh-workspace! store))]
    (persistance/db-fork-session! store session-id (assoc opts :workspace-id ws-id))))

(defn fork-session-at-turn!
  "Test wrapper for `persistance/db-fork-session-at-turn!` that injects a fresh
   workspace when no `:workspace-id` was supplied (session_state.workspace_id
   is NOT NULL, 1:1 with workspace)."
  [store session-id opts]
  (let [ws-id (or (:workspace-id opts) (fresh-workspace! store))]
    (persistance/db-fork-session-at-turn! store session-id (assoc opts :workspace-id ws-id))))

(defn store-iteration!
  "Test wrapper for `persistance/db-store-iteration!` that wraps flat
   `:code`/`:stdout`/`:error` test ergonomics into a canonical single-form
   `:forms` envelope vec — exactly what an iteration with one Python execution
   would produce in production. Fixtures that already pass `:forms` directly
   are forwarded untouched.

   THIS IS A TEST CONVENIENCE ONLY. Production callers pass `:forms` directly;
   there is no flat output path on the persistence core."
  [store opts]
  (let [{:keys [forms code stdout error] :as opts}
        opts

        forms-vec
        (cond (seq forms) (vec forms)
              (or (contains? opts :stdout) (some? error))
              [(cond-> {:scope nil :tag :observation :src (str code)}
                 (contains? opts :stdout)
                 (assoc :stdout stdout)

                 (some? error)
                 (assoc :error error))]
              :else nil)

        opts
        (cond-> (dissoc opts :stdout :error)
          forms-vec
          (assoc :forms forms-vec))]

    (persistance/db-store-iteration! store opts)))
