(ns com.blockether.vis.internal.workspace-test
  "Workspace primitive tests — rift CoW clones, git-free.

   The `*workspace-root*` binding contract is pure. The mutation paths
   (`create!` / `apply!` / `abandon!`) clone a tiny temp tree via rift
   (instant on CoW filesystems) and clean the clone up in `finally`, so
   the live repo and ~/.rifts are never touched."
  (:require
   [clojure.java.io :as io]
   [com.blockether.vis.ext.persistance-sqlite.core :as ps]
   [com.blockether.vis.ext.persistance-sqlite.registrar]
   [com.blockether.vis.internal.workspace :as ws]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- with-store
  "Open an :memory sqlite store, run `f` with it, dispose."
  [f]
  (let [store (assoc (ps/db-open! :memory) :backend :sqlite)]
    (try (f store)
      (finally (ps/db-close! store)))))

(defn- temp-dir
  [prefix]
  (.getCanonicalPath
    (.toFile
      (java.nio.file.Files/createTempDirectory
        prefix
        (make-array java.nio.file.attribute.FileAttribute 0)))))

(defn- delete-tree!
  [root]
  (doseq [f (reverse (file-seq (io/file root)))]
    (io/delete-file f true)))

(defn- seed-workspace!
  "Insert a lightweight 'current' workspace row rooted at `base` (no
   clone), to serve as the fork parent for `create!`."
  [store base]
  (ps/db-workspace-insert! store
    {:id        (str (random-uuid))
     :repo-id   "rt"
     :repo-root base
     :root      base
     :state     :active
     :fork-ms   0}))

(defdescribe cwd-binding-test
  (it "falls back to process cwd when *workspace-root* is unbound (REPL/test convenience)"
    (let [process-cwd (System/getProperty "user.dir")]
      (expect (= process-cwd (.getPath (ws/cwd))))))

  (it "returns the bound root inside a binding"
    (binding [ws/*workspace-root* "/tmp"]
      (expect (= "/private/tmp" (.getCanonicalPath (ws/cwd))))))

  (it "workspace-root reads :workspace/root from an env map"
    (expect (= "/private/tmp"
              (ws/workspace-root {:workspace/root "/tmp"}))))

  (it "workspace-root accepts a raw string and canonicalises it"
    (expect (= "/private/tmp" (ws/workspace-root "/tmp"))))

  (it "workspace-root returns nil for blank input"
    (expect (nil? (ws/workspace-root "   ")))
    (expect (nil? (ws/workspace-root nil)))))

(defdescribe changed-paths-test
  (it "lists only files with mtime newer than the fork ms, skipping .git"
    (let [dir (temp-dir "vis-changed")]
      (try
        (spit (io/file dir "old.txt") "old\n")
        (.mkdirs (io/file dir ".git"))
        (spit (io/file dir ".git" "config") "gitstuff\n")
        (let [fork-ms (do (Thread/sleep 8) (System/currentTimeMillis))]
          (Thread/sleep 8)
          (spit (io/file dir "new.txt") "new\n")
          ;; a change inside .git must NOT be reported (would corrupt trunk)
          (spit (io/file dir ".git" "HEAD") "ref: refs/heads/x\n")
          (expect (= ["new.txt"] (sort (ws/changed-paths dir fork-ms)))))
        (finally (delete-tree! dir)))))

  (it "prunes churny build/cache dirs (clj-kondo cache, target, cpcache) but keeps tracked .clj-kondo/config.edn"
    ;; Regression: a sub_loop child clones the whole repo; clj-kondo/JVM rewrite
    ;; their caches on startup, so thousands of cache files get fresh mtimes.
    ;; Reporting them flooded `changed_files` and overflowed the model ctx.
    (let [dir (temp-dir "vis-prune")]
      (try
        (let [fork-ms (do (Thread/sleep 8) (System/currentTimeMillis))]
          (Thread/sleep 8)
          (spit (io/file dir "real.txt") "edit\n")
          (.mkdirs (io/file dir ".clj-kondo" ".cache" "v1" "cljc"))
          (spit (io/file dir ".clj-kondo" ".cache" "v1" "cljc" "x.transit.json") "cache\n")
          (spit (io/file dir ".clj-kondo" "config.edn") "{}\n") ; tracked → reported
          (.mkdirs (io/file dir "target" "classes"))
          (spit (io/file dir "target" "classes" "C.class") "bytes\n")
          (.mkdirs (io/file dir ".cpcache"))
          (spit (io/file dir ".cpcache" "deadbeef.basis") "cp\n")
          (.mkdirs (io/file dir "node_modules" "left-pad"))
          (spit (io/file dir "node_modules" "left-pad" "index.js") "x\n")
          (expect (= #{"real.txt" (str (io/file ".clj-kondo" "config.edn"))}
                    (set (ws/changed-paths dir fork-ms)))))
        (finally (delete-tree! dir))))))

(defdescribe rift-roundtrip-test
  (it "create! clones a parent, apply! lands since-fork edits, abandon! discards"
    (let [base (temp-dir "vis-ws-rt")]
      (try
        (spit (io/file base "a.txt") "original\n")
        (with-store
          (fn [store]
            (let [seed     (seed-workspace! store base)
                  draft    (ws/create! store {:from seed})
                  draft-id (:id draft)]
              (try
                ;; a real, distinct clone carrying the parent's tree
                (expect (some? (:root draft)))
                (expect (not= base (:root draft)))
                (expect (.exists (io/file (:root draft) "a.txt")))
                ;; trunk inherited from the parent so apply lands back into base
                (expect (= base (:repo-root draft)))
                ;; edit + add inside the clone AFTER the fork
                (Thread/sleep 8)
                (spit (io/file (:root draft) "a.txt") "EDITED\n")
                (spit (io/file (:root draft) "b.txt") "NEW\n")
                (let [{:keys [landed changed]} (ws/apply! store {:workspace-id draft-id})]
                  (expect (= 2 landed))
                  (expect (= #{"a.txt" "b.txt"} (set (map :path changed))))
                  (expect (= "EDITED\n" (slurp (io/file base "a.txt"))))
                  (expect (= "NEW\n" (slurp (io/file base "b.txt")))))
                ;; abandon trashes the clone + marks the row discarded
                (let [done (ws/abandon! store {:workspace-id draft-id :reason "done"})]
                  (expect (= :discarded (:state done)))
                  (expect (= "done" (:reason done))))
                (finally
                  (try (ws/abandon! store {:workspace-id draft-id})
                    (catch Throwable _ nil)))))))
        (finally (delete-tree! base)))))

  (it "apply! throws for an unknown workspace-id"
    (with-store
      (fn [store]
        (expect
          (try (ws/apply! store {:workspace-id "nope"}) false
            (catch clojure.lang.ExceptionInfo _ true)))))))

(defdescribe cow-readonly-source-test
  (it "clones a tree containing a mode-444 file and restores its perms (rift CoW EACCES workaround)"
    (let [base (temp-dir "vis-ws-ro")
          ro   (io/file base "readonly.txt")]
      (try
        (spit ro "locked\n")
        ;; mode 0444 — exactly how git stores loose/pack objects; without
        ;; `with-source-writable` rift's macOS per-entry CoW aborts EACCES here.
        (java.nio.file.Files/setPosixFilePermissions
          (.toPath ro)
          (java.nio.file.attribute.PosixFilePermissions/fromString "r--r--r--"))
        (with-store
          (fn [store]
            (let [seed     (seed-workspace! store base)
                  draft    (ws/create! store {:from seed})
                  draft-id (:id draft)]
              (try
                ;; the clone succeeded despite the read-only source file
                (expect (.exists (io/file (:root draft) "readonly.txt")))
                (expect (= "locked\n" (slurp (io/file (:root draft) "readonly.txt"))))
                ;; and the source's exact 444 perms were restored afterwards
                (expect (= "r--r--r--"
                          (java.nio.file.attribute.PosixFilePermissions/toString
                            (java.nio.file.Files/getPosixFilePermissions
                              (.toPath ro)
                              (make-array java.nio.file.LinkOption 0)))))
                (finally
                  (try (ws/abandon! store {:workspace-id draft-id})
                    (catch Throwable _ nil)))))))
        (finally
          (.setWritable ro true) ; so delete-tree! can remove it
          (delete-tree! base))))))

(defdescribe hooks-test
  (it "register-hook! returns the hook id"
    (expect (= :on-apply
              (ws/register-hook! :on-apply (fn [& _] nil))))))

(defdescribe trunk-info-test
  (it "returns the canonical cwd as repo-root"
    (expect (= (.getCanonicalPath (io/file (System/getProperty "user.dir")))
              (:repo-root (ws/trunk-info))))))
