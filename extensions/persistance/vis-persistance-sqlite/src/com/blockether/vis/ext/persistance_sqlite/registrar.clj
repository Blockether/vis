(ns com.blockether.vis.ext.persistance-sqlite.registrar
  "Lightweight backend registrar for the SQLite persistence extension.

   Manifest discovery loads this namespace on every Vis startup. The
   heavyweight `com.blockether.vis.ext.persistance-sqlite.core` namespace
   (HikariCP, sqlite-jdbc, charred, honey.sql, next.jdbc, nippy, ... -
   ~480 ms of class loading on a cold JVM) is NOT required here.

   `:ext/persistance` registers the backend with `:persistance/ns` pointing
   at the heavy `core` ns. The persistance facade resolves backend vars
   with `requiring-resolve` (see
   `com.blockether.vis.internal.persistance/resolve-impl`), so the heavy
   ns auto-loads on the first real DB op. Commands that never touch the
   DB (`vis providers list`, `vis --help`, `vis doctor` for
   non-DB-touching extensions, ...) skip the load entirely.

   The contract that flips with this split:
   - `(require 'com.blockether.vis.ext.persistance-sqlite.core)` does
     NOT register the extension on its own anymore - it just defines the
     fns. Tests that only need the fns can keep requiring core directly.
     Tests / runtime that need the extension registered should require
     this `registrar` ns OR rely on classpath manifest discovery (which
     loads this ns).

   Runtime semantics are untouched: when the persistance
   facade actually dispatches a backend call, the heavy ns loads, and
   every fn is found exactly as before. The split only changes WHEN the
   heavy ns is loaded, never WHAT it does."
  (:require [com.blockether.vis.core :as vis]))

(vis/register-extension! (vis/extension {:ext/name "persistance-sqlite"
                                         :ext/description "SQLite + Flyway persistence backend."
                                         :ext/version "0.3.0"
                                         :ext/author "Blockether"
                                         :ext/owner "vis"
                                         :ext/license "Apache-2.0"
                                         :ext/persistance
                                         [{:persistance/id :sqlite
                                           :persistance/ns
                                           'com.blockether.vis.ext.persistance-sqlite.core}]}))
