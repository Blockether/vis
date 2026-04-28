(ns com.blockether.vis.ext.persistance-sqlite.test-helpers
  "Shared test utilities — in-memory SQLite store via lazytest context.

   Usage in test ns:
     (require '[com.blockether.vis.ext.persistance-sqlite.test-helpers :as h])
     (h/use-mem-store!)

   Then in each `it`:
     (let [s (h/store)] ...)

   Each test gets an isolated in-memory DB. No manual setup/teardown."
  (:require
   [com.blockether.vis-persistance.core :as db]
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
       (let [s (db/create-store-connection :memory)]
         (try
           (binding [*store* s]
             (f))
           (finally
             (db/dispose-store-connection! s)))))]))

(defn raw-query
  "Execute raw HoneySQL against the store's datasource."
  [store q]
  (jdbc/execute! (:datasource store) (sql/format q)
    {:builder-fn rs/as-unqualified-lower-maps}))

(defn raw-count
  "Count rows in a table, optionally with a where clause."
  ([store table]
   (:cnt (first (raw-query store {:select [[[:count :*] :cnt]] :from [table]}))))
  ([store table where]
   (:cnt (first (raw-query store {:select [[[:count :*] :cnt]] :from [table] :where where})))))

(defn thaw-blob
  "Thaw a nippy BLOB from a raw query result."
  [^bytes bs]
  (when bs (nippy/thaw bs)))
