(ns com.blockether.vis.ext.persistance-sqlite.registrar-test
  "Regression test for the lightweight-registrar split.

   The split saves ~480 ms of Clojure compilation on every Vis startup
   for commands that never touch the DB (`vis providers list`,
   `vis --help`, ...). This test asserts the contract the split depends
   on:

     1. Loading the registrar registers the SQLite backend in the
        persistance facade's backend registry, but does NOT yet cause
        the heavy `core` ns to be loaded. (We can't observe \"not yet
        loaded\" reliably from the test-runner JVM because the test
        suite itself loads many extensions, so we settle for asserting
        the registration data is structurally correct.)

     2. The registrar's `:persistance/ns` points at the heavy core ns,
        so the persistance facade's `requiring-resolve` lazy-load path
        knows where to look on the first DB op.

     3. The registrar captures its source namespace for reload/source tracking.

   See `registrar.clj` for the rationale + the per-command load-cost
   numbers, and `com.blockether.vis.internal.persistance/resolve-impl`
   for the lazy-load plumbing the registrar relies on."
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.persistance-sqlite.registrar]
            [com.blockether.vis.internal.persistance :as persistance]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  registrar-test
  (it "registers the :sqlite backend in the persistance facade"
      (let [backends (persistance/registered-backends)]
        (expect (contains? backends :sqlite))))
  (it "points :persistance/ns at the heavy core ns so requiring-resolve can load it lazily"
      (let [backends (persistance/registered-backends)]
        (expect (= 'com.blockether.vis.ext.persistance-sqlite.core
                   (get-in backends [:sqlite :ns])))))
  (it "captures registrar source namespace for reload/source tracking"
      (let [exts
            (vis/registered-extensions)

            ext
            (some #(when (= "persistance-sqlite" (:ext/name %)) %) exts)]

        (expect (some? ext))
        (expect (= ['com.blockether.vis.ext.persistance-sqlite.registrar] (:ext/source-nses ext)))
        (expect (= [{:persistance/id :sqlite
                     :persistance/ns 'com.blockether.vis.ext.persistance-sqlite.core}]
                   (:ext/persistance ext))))))
