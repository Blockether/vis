(ns com.blockether.vis.ext.persistance-sqlite.test-helpers
  "Shared test utilities — in-memory SQLite store via lazytest context.

   Usage in test ns:
     (require '[com.blockether.vis.ext.persistance-sqlite.test-helpers :as h])
     (h/use-mem-store!)

   Then in each `it`:
     (let [s (h/store)] ...)

   Each test gets an isolated in-memory DB. No manual setup/teardown."
  (:require
   [com.blockether.vis.core :as vis]
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

(defn store
  "Get the current test's in-memory store."
  []
  *store*)

(defn use-mem-store!
  "Call at top-level in a test ns to get a fresh in-memory SQLite store
   for each test. Access via `(h/store)`."
  []
  (lt/set-ns-context!
    [(lt/around-each [f]
       (let [s (vis/db-create-connection! :memory)]
         (try
           (binding [*store* s]
             (f))
           (finally
             (vis/db-dispose-connection! s)))))]))

(defn raw-query
  "Execute raw HoneySQL against the store's datasource."
  [store q]
  (jdbc/execute! (:datasource store) (sql/format q)
    {:builder-fn rs/as-unqualified-lower-maps}))

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
   (:row_count (first (raw-query store {:select [[[:count :*] :row_count]] :from [table] :where where})))))

(defn thaw-blob
  "Thaw a nippy BLOB from a raw query result."
  [^bytes bs]
  (when bs (nippy/thaw bs)))
