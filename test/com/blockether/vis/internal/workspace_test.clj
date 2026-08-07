(ns com.blockether.vis.internal.workspace-test
  "Workspace primitive tests — rift CoW clones, git-free.

   The `*workspace-root*` binding contract is pure. The mutation paths
   (`create!` / `apply!` / `abandon!`) clone a tiny temp tree via rift
   (instant on CoW filesystems) and clean the clone up in `finally`, so
   the live repo and ~/.rifts are never touched."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.ext.persistance-sqlite.core :as ps]
            [com.blockether.vis.ext.persistance-sqlite.registrar]
            [com.blockether.vis.internal.workspace :as ws]
            [lazytest.core :refer [defdescribe expect it]]
            [next.jdbc :as jdbc]))

;; Regression, reported issue: a configured `~/vis` root was canonicalized as a
;; child of the process directory, so new sessions opened in the wrong worktree.
(defdescribe normalize-root-test
             (it "expands a leading home marker before canonicalizing the workspace root"
                 (expect (= (.getCanonicalPath (io/file (System/getProperty "user.home") "vis"))
                            (ws/normalize-root "~/vis")))))

(defn- with-store
  "Open an :memory sqlite store, run `f` with it, dispose."
  [f]
  (let [store (assoc (ps/db-open! :memory) :backend :sqlite)]
    (try (f store) (finally (ps/db-close! store)))))

(defn- temp-dir
  [prefix]
  (.getCanonicalPath (.toFile (java.nio.file.Files/createTempDirectory
                                prefix
                                (make-array java.nio.file.attribute.FileAttribute 0)))))

(defn- delete-tree!
  [root]
  (doseq [f (reverse (file-seq (io/file root)))]
    (io/delete-file f true)))

(defn- git!
  [^java.io.File root & args]
  (let [pb (ProcessBuilder. ^java.util.List (into ["git"] (map str) args))]
    (.directory pb root)
    (.redirectErrorStream pb true)
    (let [p (.start pb)]
      (slurp (.getInputStream p))
      (.waitFor p))))

(defn- git-output!
  [^java.io.File root & args]
  (let [pb (ProcessBuilder. ^java.util.List (into ["git"] (map str) args))]
    (.directory pb root)
    (.redirectErrorStream pb true)
    (let
      [p (.start pb)
       out (slurp (.getInputStream p))
       exit (.waitFor p)]

      (when-not (zero? exit)
        (throw (ex-info "git command failed" {:args args :exit exit :out out})))
      (.trim ^String out))))

(defn- init-repo!
  "Initialise a real git repo at `root` (shells out to the git binary),
   with one committed file so it has a HEAD."
  [^java.io.File root]
  (git! root "init" "-q")
  (git! root "config" "user.name" "Vis Test")
  (git! root "config" "user.email" "vis-test@example.invalid")
  (git! root "config" "commit.gpgsign" "false")
  (spit (io/file root "a.txt") "x\n")
  (git! root "add" "a.txt")
  (git! root "commit" "-q" "-m" "init"))

(defn- seed-workspace!
  "Insert a lightweight 'current' workspace row rooted at `base` (no
   clone), to serve as the fork parent for `create!`. fork-ms is 1, NOT 0:
   a ZERO baseline marks a FRESH lineage (inherited by `:from` children),
   and these seeds stand in for ordinary drafts."
  [store base]
  (ps/db-workspace-insert!
    store
    {:id (str (random-uuid)) :repo-id "rt" :repo-root base :root base :state :active :fork-ms 1}))

(defn- with-fork-mechanism
  "Redef rift-fork! so `create!` reports `mechanism` (nil = a native library
   older than kind reporting, exactly like the bare-path case)."
  [mechanism f]
  (with-redefs
    [ws/rift-fork!
     (fn [{:keys [store-root name]}]
       (let [root (io/file store-root name)]
         (.mkdirs root)
         {:root (.getCanonicalPath root) :mechanism mechanism}))

     ws/rift-available?
     (constantly {:available? true})]

    (f)))

(defn- pin-session!
  "Insert a session_soul + session_state pinned 1:1 to `workspace-id`, so
   `discard-session-clones!` can resolve soul → state → workspace."
  [store soul-id workspace-id]
  (let
    [ds
     (:datasource store)

     st
     (str (random-uuid))]

    (jdbc/execute! ds
                   ["INSERT INTO session_soul (id, channel, created_at) VALUES (?,?,?)" soul-id
                    "tui" 1])
    (jdbc/execute! ds
                   [(str "INSERT INTO session_state "
                         "(id, session_soul_id, workspace_id, version, created_at) "
                         "VALUES (?,?,?,?,?)") st soul-id workspace-id 0 1])
    st))

(defdescribe
  cwd-binding-test
  (it "falls back to process cwd when *workspace-root* is unbound (REPL/test convenience)"
      (let [process-cwd (System/getProperty "user.dir")]
        (expect (= process-cwd (.getPath (ws/cwd))))))
  (it "returns the bound root inside a binding"
      (binding [ws/*workspace-root* "/tmp"]
        (expect (= (.getCanonicalPath (java.io.File. "/tmp")) (.getCanonicalPath (ws/cwd))))))
  (it "workspace-root reads :workspace/root from an env map"
      (expect (= (.getCanonicalPath (java.io.File. "/tmp"))
                 (ws/workspace-root {:workspace/root "/tmp"}))))
  (it "workspace-root accepts a raw string and canonicalises it"
      (expect (= (.getCanonicalPath (java.io.File. "/tmp")) (ws/workspace-root "/tmp"))))
  (it "workspace-root returns nil for blank input"
      (expect (nil? (ws/workspace-root "   ")))
      (expect (nil? (ws/workspace-root nil)))))

(defdescribe
  changed-paths-test
  (it "lists only files with mtime newer than the fork ms, skipping .git"
      (let [dir (temp-dir "vis-changed")]
        (try (spit (io/file dir "old.txt") "old\n")
             (.mkdirs (io/file dir ".git"))
             (spit (io/file dir ".git" "config") "gitstuff\n")
             (let [fork-ms (do (Thread/sleep 8) (System/currentTimeMillis))]
               (Thread/sleep 8)
               (spit (io/file dir "new.txt") "new\n")
               ;; a change inside .git must NOT be reported (would corrupt trunk)
               (spit (io/file dir ".git" "HEAD") "ref: refs/heads/x\n")
               (expect (= ["new.txt"] (sort (ws/changed-paths dir fork-ms)))))
             (finally (delete-tree! dir)))))
  (it
    "prunes churny build/cache dirs (clj-kondo cache, target, cpcache) but keeps tracked .clj-kondo/config.edn"
    ;; Regression: a sub_loop child clones the whole repo; clj-kondo/JVM rewrite
    ;; their caches on startup, so thousands of cache files get fresh mtimes.
    ;; Reporting them flooded `changed_files` and overflowed the model ctx.
    (let [dir (temp-dir "vis-prune")]
      (try (let [fork-ms (do (Thread/sleep 8) (System/currentTimeMillis))]
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
             ;; changed-paths returns `/`-separated display paths on EVERY OS.
             (expect (= #{"real.txt" ".clj-kondo/config.edn"}
                        (set (ws/changed-paths dir fork-ms)))))
           (finally (delete-tree! dir)))))
  (it "prunes churny build/cache dirs at ANY depth, not just the repo root"
      ;; Regression: a monorepo keeps its churn NESTED — apps/web/node_modules,
      ;; extensions/<ext>/target, packages/<p>/.clj-kondo/.cache. Matching only
      ;; path segment 0 let thousands of gitignored generated files through, so
      ;; they were reported as agent edits and landed into trunk by `apply!`.
      (let [dir (temp-dir "vis-prune-deep")]
        (try (let [fork-ms (do (Thread/sleep 8) (System/currentTimeMillis))]
               (Thread/sleep 8)
               (spit (io/file dir "real.txt") "edit\n")
               (.mkdirs (io/file dir "apps" "web" "node_modules" "left-pad"))
               (spit (io/file dir "apps" "web" "node_modules" "left-pad" "index.js") "x\n")
               (.mkdirs (io/file dir "extensions" "ext-a" "target" "classes"))
               (spit (io/file dir "extensions" "ext-a" "target" "classes" "C.class") "bytes\n")
               (.mkdirs (io/file dir "apps" "web" "ios" ".git"))
               (spit (io/file dir "apps" "web" "ios" ".git" "HEAD") "ref: refs/heads/x\n")
               (.mkdirs (io/file dir "packages" "p" ".clj-kondo" ".cache" "v1"))
               (spit (io/file dir "packages" "p" ".clj-kondo" ".cache" "v1" "x.transit.json") "c\n")
               ;; a NESTED tracked source file / kondo config is still an edit
               (spit (io/file dir "packages" "p" ".clj-kondo" "config.edn") "{}\n")
               (.mkdirs (io/file dir "apps" "web" "src"))
               (spit (io/file dir "apps" "web" "src" "app.ts") "edit\n")
               (expect (= #{"real.txt" "apps/web/src/app.ts" "packages/p/.clj-kondo/config.edn"}
                          (set (ws/changed-paths dir fork-ms)))))
             (finally (delete-tree! dir)))))
  (it "never reports what the clone's OWN repository ignores as an agent change"
      ;; Regression: a gitignore-aware fork never copies an ignored tree, so every
      ;; ignored file INSIDE a draft was generated there — a rebuilt dist, a
      ;; regenerated native project. Reporting them made `apply!` dump thousands
      ;; of build artifacts into the user's real repo.
      (let [dir (temp-dir "vis-changed-ignored")]
        (try (let [fork-ms (do (Thread/sleep 8) (System/currentTimeMillis))]
               (git! (io/file dir) "init" "-q")
               (Thread/sleep 8)
               (spit (io/file dir ".gitignore") "dist/\nlocal.env\n")
               (.mkdirs (io/file dir "dist"))
               (spit (io/file dir "dist" "bundle.js") "GENERATED IN THE DRAFT\n")
               (spit (io/file dir "local.env") "TOKEN=1\n")
               (spit (io/file dir "src.txt") "the agent's real edit\n")
               ;; A force-added ignored file is TRACKED, so it still lands.
               (spit (io/file dir "pinned.env") "PINNED=1\n")
               (spit (io/file dir ".gitignore") "dist/\nlocal.env\n*.env\n")
               (git! (io/file dir) "add" "-f" "pinned.env")
               (expect (= #{".gitignore" "src.txt" "pinned.env"}
                          (set (ws/changed-paths dir fork-ms)))))
             (finally (delete-tree! dir))))))

(defdescribe
  rift-roundtrip-test
  (it
    "create! clones a parent, apply! lands since-fork edits, abandon! discards"
    (let [base (temp-dir "vis-ws-rt")]
      (try (if-not (ws/isolated-workspaces-supported? base)
             ;; No copy-on-write backend here (CI: ext4/NTFS, no reflink) — the
             ;; rift clone round-trip can't run. The linked-worktree /
             ;; capability-matrix tests cover the unavailable path.
             (expect (not (ws/isolated-workspaces-supported? base)))
             (do (spit (io/file base "a.txt") "original\n")
                 (with-store
                   (fn [store]
                     (let
                       [seed (seed-workspace! store base)
                        draft (ws/create! store {:from seed})
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
                         (finally (try (ws/abandon! store {:workspace-id draft-id})
                                       (catch Throwable _ nil))))))))) ; close fn/with-store, then do + if-not
           (finally (delete-tree! base)))))
  (it
    "apply! lands nested edits AND deletions; stash!/resume! keep the clone intact"
    (let [base (temp-dir "vis-ws-del")]
      (try
        (if-not (ws/isolated-workspaces-supported? base)
          ;; No copy-on-write backend here (CI) — the live round-trip can't run.
          (expect (not (ws/isolated-workspaces-supported? base)))
          (do (spit (io/file base "keep.txt") "KEEP\n")
              (spit (io/file base "gone.txt") "DOOMED\n")
              (.mkdirs (io/file base "sub"))
              (spit (io/file base "sub" "nested.txt") "NESTED\n")
              (with-store
                (fn [store]
                  (let
                    [seed (seed-workspace! store base)
                     state-id (pin-session! store (str (random-uuid)) (:id seed))
                     draft (ws/create! store {:session-state-id state-id :from seed})
                     draft-id (:id draft)]

                    (try
                      ;; the clone carries trunk's whole tree, nested dirs included
                      (expect (= "NESTED\n" (slurp (io/file (:root draft) "sub" "nested.txt"))))
                      ;; edit a NESTED file and delete a trunk file inside the draft
                      (Thread/sleep 20)
                      (spit (io/file (:root draft) "sub" "nested.txt") "NESTED-EDIT\n")
                      (io/delete-file (io/file (:root draft) "gone.txt"))
                      ;; parking the draft repoints the session at trunk and lists it as parked
                      (binding [ws/*workspace-root* base]
                        (ws/stash! store state-id))
                      (expect (= base (:root (ws/for-session store state-id))))
                      (expect (= [draft-id] (mapv :id (ws/list-drafts store (:repo-id draft)))))
                      ;; re-entering restores the pin AND the clone's uncommitted work
                      (ws/resume! store {:session-state-id state-id :workspace-id draft-id})
                      (expect (= draft-id (:id (ws/for-session store state-id))))
                      (expect (= "NESTED-EDIT\n"
                                 (slurp (io/file (:root draft) "sub" "nested.txt"))))
                      (let [{:keys [landed changed]} (ws/apply! store {:workspace-id draft-id})]
                        (expect (= 2 landed))
                        (expect (= #{"sub/nested.txt" "gone.txt"} (set (map :path changed))))
                        (expect (= #{:modify :delete} (set (map :status changed)))))
                      ;; the deletion travelled, the nested edit landed, the rest survives
                      (expect (not (.exists (io/file base "gone.txt"))))
                      (expect (= "NESTED-EDIT\n" (slurp (io/file base "sub" "nested.txt"))))
                      (expect (= "KEEP\n" (slurp (io/file base "keep.txt"))))
                      (finally (try (ws/abandon! store {:workspace-id draft-id})
                                    (catch Throwable _ nil)))))))))
        (finally (delete-tree! base)))))
  (it "apply! throws for an unknown workspace-id"
      (with-store (fn [store]
                    (expect (try (ws/apply! store {:workspace-id "nope"})
                                 false
                                 (catch clojure.lang.ExceptionInfo _ true)))))))


;; Regression, reported Rift clean issue: the clone kept the copied Git index,
;; exposing staged and intermediate source state instead of one exact commit.
(defdescribe
  clean-draft-roundtrip-test
  (it
    "create! :clean? delegates an exact clean commit to Rift and apply! preserves omitted source files"
    (let [base (temp-dir "vis-ws-clean")]
      (try
        (if-not (ws/isolated-workspaces-supported? base)
          ;; No copy-on-write backend here (CI) — the live round-trip can't run.
          (expect (not (ws/isolated-workspaces-supported? base)))
          (do (init-repo! (io/file base))
              (spit (io/file base "tracked.txt") "COMMITTED\n")
              (io/make-parents (io/file base "sub" "deep.txt"))
              (spit (io/file base "sub" "deep.txt") "NESTED\n")
              (spit (io/file base ".gitignore") "ignored/\n")
              (git! (io/file base) "add" "tracked.txt" "sub/deep.txt" ".gitignore")
              (git! (io/file base) "commit" "-q" "-m" "tracked")
              (let [head (git-output! (io/file base) "rev-parse" "HEAD")]
                ;; The user's UNCOMMITTED work: dirty, untracked, staged, and
                ;; ignored state. None may reach the cleaned draft, and none may
                ;; be deleted from or overwritten in trunk when it is applied.
                (spit (io/file base "tracked.txt") "DIRTY\n")
                (spit (io/file base "scratch.txt") "UNTRACKED\n")
                (io/make-parents (io/file base "notes" "x.md"))
                (spit (io/file base "notes" "x.md") "NOTES\n")
                (io/make-parents (io/file base "ignored" "cache.bin"))
                (spit (io/file base "ignored" "cache.bin") "IGNORED\n")
                (spit (io/file base "staged.txt") "STAGED\n")
                (git! (io/file base) "add" "staged.txt")
                (with-store
                  (fn [store]
                    (let
                      [seed (seed-workspace! store base)
                       draft (ws/create! store {:from seed :clean? true})
                       draft-id (:id draft)
                       root (:root draft)]

                      (try (expect (some? root))
                           (expect (not= base root))
                           ;; Exactly the requested committed state: HEAD, real
                           ;; index, worktree, and ignored content are all clean.
                           (expect (= head (git-output! (io/file root) "rev-parse" "HEAD")))
                           (expect (= "" (git-output! (io/file root) "status" "--porcelain")))
                           (expect (= "COMMITTED\n" (slurp (io/file root "tracked.txt"))))
                           (expect (.exists (io/file root "a.txt")))
                           (expect (not (.exists (io/file root "scratch.txt"))))
                           (expect (not (.exists (io/file root "notes" "x.md"))))
                           (expect (not (.exists (io/file root "staged.txt"))))
                           (expect (not (.exists (io/file root "ignored" "cache.bin"))))
                           (expect (= "NESTED\n" (slurp (io/file root "sub" "deep.txt"))))
                           ;; a real baseline: this draft saw trunk's committed
                           ;; files and may report deletions of them
                           (expect (pos? (long (:fork-ms draft))))
                           (spit (io/file root "made.txt") "MADE\n")
                           (let [{:keys [changed]} (ws/apply! store {:workspace-id draft-id})]
                             (expect (contains? (set (map :path changed)) "made.txt"))
                             ;; What Rift omitted is not an agent deletion.
                             (expect (not (some #(= :delete (:status %)) changed)))
                             (expect (= "MADE\n" (slurp (io/file base "made.txt"))))
                             (expect (= "UNTRACKED\n" (slurp (io/file base "scratch.txt"))))
                             (expect (= "NOTES\n" (slurp (io/file base "notes" "x.md"))))
                             (expect (= "STAGED\n" (slurp (io/file base "staged.txt"))))
                             (expect (= "IGNORED\n" (slurp (io/file base "ignored" "cache.bin"))))
                             (expect (= "DIRTY\n" (slurp (io/file base "tracked.txt")))))
                           (finally (try (ws/abandon! store {:workspace-id draft-id})
                                         (catch Throwable _ nil))))))))))
        (finally (delete-tree! base))))))

;; Regression, reported Rift clean issue: a project that was never `git init`-ed
;; reached the native clone at all — the refusal came from Rift's Git error after
;; a copy had been made, instead of from Vis before one was.
(defdescribe clean-draft-requires-a-git-project-test
             (it "create! :clean? refuses a project that is not Git-managed"
                 (let [base (temp-dir "vis-ws-clean-nogit")]
                   (try (with-store (fn [store]
                                      (let [seed (seed-workspace! store base)]
                                        (expect (try (ws/create! store {:from seed :clean? true})
                                                     false
                                                     (catch clojure.lang.ExceptionInfo e
                                                       (= :workspace/clean-unavailable
                                                          (:type (ex-data e)))))))))
                        (finally (delete-tree! base))))))

(defdescribe
  linked-worktree-source-test
  (it
    "refuses linked Git worktrees before entering the native rift clone path"
    (let
      [base
       (temp-dir "vis-ws-worktree-base")

       linked
       (temp-dir "vis-ws-worktree-linked")]

      (try
        ;; Real base repo via the git binary. A linked worktree's working dir
        ;; is just a dir whose `.git` is a FILE pointing at the main repo's
        ;; worktree admin dir — exactly what the preflight detects. Rather than
        ;; run `git worktree add`, we write that gitdir pointer directly,
        ;; reproducing git's on-disk shape deterministically.
        (init-repo! (io/file base))
        (let [admin (io/file base ".git" "worktrees" "linked")]
          (.mkdirs admin)
          (spit (io/file linked ".git") (str "gitdir: " (.getCanonicalPath admin) "\n")))
        (with-store
          (fn [store]
            (let
              [seed
               (seed-workspace! store linked)

               data
               (try (ws/create! store {:from seed})
                    nil
                    (catch clojure.lang.ExceptionInfo e (ex-data e)))]

              ;; The capability flow probes backends first: rift reports the
              ;; linked-worktree source UNAVAILABLE, so no backend covers the
              ;; draft and create! fails with the unavailable matrix (which
              ;; carries rift's :linked-git-worktree reason).
              (expect (= :workspace/capability-unavailable (:type data)))
              (expect (some #(and (= :rift (:backend %))
                                  (not (:available? %))
                                  (= :linked-git-worktree (:reason %)))
                            (:capability-matrix data))))))
        (finally (delete-tree! base) (delete-tree! linked))))))

(defdescribe
  cow-readonly-source-test
  (it
    "clones a tree containing a mode-444 file and restores its perms (rift CoW EACCES workaround)"
    (let
      [base
       (temp-dir "vis-ws-ro")

       ro
       (io/file base "readonly.txt")]

      (try
        (if-not (ws/isolated-workspaces-supported? base)
          ;; No CoW backend (CI) — rift's per-entry clone can't run here.
          (expect (not (ws/isolated-workspaces-supported? base)))
          (do (spit ro "locked\n")
              ;; mode 0444 — exactly how git stores loose/pack objects; without
              ;; `with-source-writable` rift's macOS per-entry CoW aborts EACCES here.
              (java.nio.file.Files/setPosixFilePermissions
                (.toPath ro)
                (java.nio.file.attribute.PosixFilePermissions/fromString "r--r--r--"))
              (with-store
                (fn [store]
                  (let
                    [seed
                     (seed-workspace! store base)

                     draft
                     (ws/create! store {:from seed})

                     draft-id
                     (:id draft)]

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
                      (finally (try (ws/abandon! store {:workspace-id draft-id})
                                    (catch Throwable _ nil)))))))))
        (finally (.setWritable ro true) ; so delete-tree! can remove it
                 (delete-tree! base))))))

(defdescribe hooks-test
             (it "register-hook! returns the hook id"
                 (expect (= :on-apply
                            (ws/register-hook! :on-apply
                                               (fn [& _]
                                                 nil))))))

(defdescribe trunk-info-test
             (it "returns the canonical cwd as repo-root"
                 (expect (= (.getCanonicalPath (io/file (System/getProperty "user.dir")))
                            (:repo-root (ws/trunk-info))))))

;; Regression, issue #workspace-root-single-source: the gateway used display-only
;; `~/…` input as a path relative to its launch directory and persisted `/…/~/…`.
(defdescribe create-trunk-root-normalization-test
             (it "normalizes a home-relative root before persisting the workspace"
                 (with-store (fn [store]
                               (let [workspace (ws/create-trunk-at! store "~")]
                                 (expect (= (ws/normalize-root "~") (:root workspace)))
                                 (expect (= (:root workspace) (:repo-root workspace))))))))

(defdescribe
  change-root-test
  (it "repoints the session to a trunk at the new path"
      (let
        [a
         (temp-dir "vis-root-a")

         b
         (temp-dir "vis-root-b")]

        (try (with-store (fn [store]
                           (let
                             [trunk
                              (ws/create-trunk-at! store a)

                              state-id
                              (pin-session! store (str (random-uuid)) (:id trunk))

                              ws2
                              (ws/change-root! store state-id b)]

                             (expect (= (ws/normalize-root b) (:root ws2)))
                             (expect (= (:id ws2) (:id (ws/for-session store state-id))))
                             (expect (not (ws/draft? ws2))))))
             (finally (delete-tree! a) (delete-tree! b)))))
  (it "is a no-op returning the SAME workspace when the path already is the root"
      (let [a (temp-dir "vis-root-same")]
        (try (with-store (fn [store]
                           (let
                             [trunk (ws/create-trunk-at! store a)
                              state-id (pin-session! store (str (random-uuid)) (:id trunk))
                              ws2 (ws/change-root! store state-id a)]

                             (expect (= (:id trunk) (:id ws2))))))
             (finally (delete-tree! a)))))
  (it "refuses while the session is in a draft"
      (let
        [a
         (temp-dir "vis-root-draft")

         b
         (temp-dir "vis-root-draft-b")]

        (try (with-store (fn [store]
                           (let
                             [seed
                              (seed-workspace! store a)

                              state-id
                              (pin-session! store (str (random-uuid)) (:id seed))

                              thrown
                              (try (ws/change-root! store state-id b) nil (catch Exception t t))]

                             (expect (some? thrown))
                             (expect (= :workspace/root-change-in-draft
                                        (:type (ex-data thrown)))))))
             (finally (delete-tree! a) (delete-tree! b)))))
  (it "throws on a non-directory path"
      (let [a (temp-dir "vis-root-nodir")]
        (try (with-store (fn [store]
                           (let
                             [trunk (ws/create-trunk-at! store a)
                              state-id (pin-session! store (str (random-uuid)) (:id trunk))
                              thrown (try (ws/change-root! store state-id (str a "/nope-missing"))
                                          nil
                                          (catch Exception t t))]

                             (expect (some? thrown))
                             (expect (= :workspace/not-a-directory (:type (ex-data thrown)))))))
             (finally (delete-tree! a))))))

(defdescribe
  deleted-paths-guard-test
  (it "a positive baseline reports trunk files missing from the clone as deletions"
      (let
        [trunk
         (temp-dir "vis-delguard-t")

         clone
         (temp-dir "vis-delguard-c")]

        (try (spit (io/file trunk "gone.txt") "x\n")
             (Thread/sleep 8)
             ;; sanity: the mechanism itself works when the baseline is real
             (expect (= ["gone.txt"] (ws/deleted-paths clone trunk (System/currentTimeMillis))))
             (finally (delete-tree! trunk) (delete-tree! clone)))))
  (it "a non-positive (FRESH) baseline can NEVER infer a deletion, whatever the trees hold"
      (let
        [trunk
         (temp-dir "vis-freshguard-t")

         clone
         (temp-dir "vis-freshguard-c")]

        (try (spit (io/file trunk "head.txt") "REAL WORK\n")
             ;; pathological pre-baseline mtime (epoch, e.g. from a tarball):
             ;; the `<` comparison alone would NOT save this file under a 0
             ;; baseline — only the hard non-positive early-exit does
             (java.nio.file.Files/setLastModifiedTime (.toPath (io/file trunk "head.txt"))
                                                      (java.nio.file.attribute.FileTime/fromMillis
                                                        0))
             (expect (= [] (ws/deleted-paths clone trunk 0)))
             (expect (= [] (ws/deleted-paths clone trunk -5)))
             (finally (delete-tree! trunk) (delete-tree! clone)))))
  (it "a Rift-recorded ignored trunk tree is NEVER an agent deletion"
      (let
        [trunk
         (temp-dir "vis-ignguard-t")

         clone
         (temp-dir "vis-ignguard-c")]

        (try (git! (io/file trunk) "init" "-q")
             (spit (io/file trunk ".gitignore") "generated/\nsecret.env\n")
             (.mkdirs (io/file trunk "generated"))
             (spit (io/file trunk "generated/out.js") "REGENERABLE OUTPUT\n")
             (spit (io/file trunk "secret.env") "TOKEN=keep-me\n")
             (spit (io/file trunk "src.txt") "source\n")
             (spit (io/file trunk "gone.txt") "the agent really deleted this\n")
             ;; A gitignore-aware Rift fork omits these paths and records that
             ;; decision in its marker. Vis consumes that native contract rather
             ;; than rediscovering Git ignore semantics itself.
             (spit (io/file clone ".rift")
                   "01JRIFTWORKSPACEID\nexcluded generated\nexcluded secret.env\n")
             (spit (io/file clone ".gitignore") "generated/\nsecret.env\n")
             (spit (io/file clone "src.txt") "source\n")
             (Thread/sleep 8)
             (expect (= ["gone.txt"] (ws/deleted-paths clone trunk (System/currentTimeMillis))))
             (finally (delete-tree! trunk) (delete-tree! clone)))))
  (it
    "a regenerable artifact tree the fork filters out is NEVER an agent deletion, even when git tracks it"
    (let
      [trunk
       (temp-dir "vis-artguard-t")

       clone
       (temp-dir "vis-artguard-c")]

      (try (git! (io/file trunk) "init" "-q")
           (.mkdirs (io/file trunk "dist"))
           (spit (io/file trunk "dist/bundle.js") "COMMITTED BUILD OUTPUT\n")
           (.mkdirs (io/file trunk "apps/web/coverage"))
           (spit (io/file trunk "apps/web/coverage/lcov.info") "nested report\n")
           (spit (io/file trunk "src.txt") "source\n")
           (git! (io/file trunk) "add" "-A")
           (git! (io/file trunk) "-c" "user.email=t@t" "-c" "user.name=t" "commit" "-qm" "init")
           ;; The fork filtered these regenerable trees out whatever git thinks
           ;; of them, and RECORDED that in the clone's workspace marker: the
           ;; id, then one `excluded <path>` line per tree it never copied.
           (spit (io/file clone ".rift")
                 "01JRIFTWORKSPACEID\nexcluded dist\nexcluded apps/web/coverage\n")
           (spit (io/file clone "src.txt") "source\n")
           (Thread/sleep 8)
           (expect (= [] (ws/deleted-paths clone trunk (System/currentTimeMillis))))
           (finally (delete-tree! trunk) (delete-tree! clone)))))
  (it
    "a filtered `.yarn` artifact PAIR the fork drops is NEVER an agent deletion, even when git tracks it"
    (let
      [trunk
       (temp-dir "vis-yarnguard-t")

       clone
       (temp-dir "vis-yarnguard-c")]

      (try (git! (io/file trunk) "init" "-q")
           (.mkdirs (io/file trunk ".yarn/cache"))
           (spit (io/file trunk ".yarn/cache/pkg.zip") "COMMITTED ZERO-INSTALL CACHE\n")
           (.mkdirs (io/file trunk ".yarn/releases"))
           (spit (io/file trunk ".yarn/releases/yarn.cjs") "runner\n")
           (spit (io/file trunk ".yarn/releases/old.cjs") "the agent really deleted this\n")
           (git! (io/file trunk) "add" "-A")
           (git! (io/file trunk) "-c" "user.email=t@t" "-c" "user.name=t" "commit" "-qm" "init")
           ;; The backend dropped the `.yarn/cache` PAIR and recorded exactly
           ;; that path in the marker, so `releases` still reaches the clone.
           (spit (io/file clone ".rift") "01JRIFTWORKSPACEID\nexcluded .yarn/cache\n")
           (.mkdirs (io/file clone ".yarn/releases"))
           (spit (io/file clone ".yarn/releases/yarn.cjs") "runner\n")
           (Thread/sleep 8)
           (expect (= [".yarn/releases/old.cjs"]
                      (ws/deleted-paths clone trunk (System/currentTimeMillis))))
           (finally (delete-tree! trunk) (delete-tree! clone))))))


(defdescribe
  draft-isolation-test
  (it
    "per-root draft policy: isolates copy roots, withholds not-allowed, and fails CLOSED when no clone was minted"
    (let
      [trunk
       (temp-dir "vis-di-trunk")

       draft
       (temp-dir "vis-di-draft")

       shared
       (temp-dir "vis-di-shared")

       copy
       (temp-dir "vis-di-copy")

       copy-clone
       (temp-dir "vis-di-clone")

       secret
       (temp-dir "vis-di-secret")

       pending
       (temp-dir "vis-di-pending")

       policy
       {:sandbox true :draft-policies {copy :copy-and-apply secret :not-allowed pending :copy-only}}

       configured
       [shared copy secret pending]]

      (try (let
             [entries
              (ws/env-filesystem-roots {:security-policy policy
                                        :workspace {:repo-root trunk
                                                    :root draft
                                                    :filesystem-roots [{:trunk copy
                                                                        :clone copy-clone
                                                                        :policy "copy-and-apply"
                                                                        :backend "rift"}]}
                                        :security/filesystem-roots configured
                                        :security/no-search-roots []})

              by-trunk
              (into {} (map (juxt :trunk identity)) entries)]

             ;; The session's OWN trunk↔clone pair comes FIRST, so an absolute trunk
             ;; path remaps into the clone before any broad root can accept it verbatim.
             (expect (= {:trunk trunk :clone draft :draft :copy-and-apply :primary? true}
                        (first entries)))
             (expect (= :shared (:draft (by-trunk shared))))
             (expect (nil? (:denied? (by-trunk shared))))
             (expect (= copy-clone (:clone (by-trunk copy))))
             (expect (nil? (:denied? (by-trunk copy))))
             (expect (true? (:denied? (by-trunk secret))))
             ;; copy policy + NO clone for this draft ⇒ withheld, never written through
             (expect (true? (:denied? (by-trunk pending))))
             (binding
               [ws/*workspace-root*
                draft

                ws/*filesystem-roots*
                entries]

               (expect (= #{secret pending} (ws/denied-roots)))
               (expect (contains? (set (ws/allowed-roots)) copy-clone))
               (expect (not-any? #{secret pending} (ws/allowed-roots)))
               (expect (not-any? #{secret pending} (map :trunk (ws/filesystem-root-mappings))))))
           ;; A TRUNK session isolates nothing, but plans what a new draft must copy.
           (binding
             [ws/*filesystem-roots* (ws/env-filesystem-roots {:security-policy policy
                                                              :workspace {:repo-root trunk
                                                                          :root trunk}
                                                              :security/filesystem-roots configured
                                                              :security/no-search-roots []})]
             (expect (empty? (ws/denied-roots)))
             (expect (= [{:trunk copy :policy :copy-and-apply} {:trunk pending :policy :copy-only}]
                        (ws/draft-isolation-plan))))
           (finally (run! delete-tree! [trunk draft shared copy copy-clone secret pending])))))
  (it
    "create! mints a private clone per copy-policy root, apply! lands copy-and-apply, abandon! releases it"
    (let
      [base
       (temp-dir "vis-di-base")

       extra
       (temp-dir "vis-di-extra")]

      (try
        (if-not (ws/isolated-workspaces-supported? base)
          ;; No copy-on-write backend here — same guard as the rift round-trip.
          (expect (not (ws/isolated-workspaces-supported? base)))
          (do (spit (io/file base "a.txt") "original\n")
              (spit (io/file extra "e.txt") "extra original\n")
              (with-store
                (fn [store]
                  (let
                    [seed
                     (seed-workspace! store base)

                     draft
                     (ws/create! store
                                 {:from seed
                                  :filesystem-roots [{:trunk extra :policy :copy-and-apply}]})

                     draft-id
                     (:id draft)

                     entry
                     (first (ws/extra-root-entries draft))

                     clone
                     (:clone entry)]

                    (try (expect (= extra (:trunk entry)))
                         (expect (= :copy-and-apply (:policy entry)))
                         ;; a real, distinct clone carrying the extra root's tree
                         (expect (not= extra clone))
                         (expect (= "extra original\n" (slurp (io/file clone "e.txt"))))
                         (Thread/sleep 8)
                         (spit (io/file clone "e.txt") "EDITED IN DRAFT\n")
                         ;; the REAL root stays untouched until apply!
                         (expect (= "extra original\n" (slurp (io/file extra "e.txt"))))
                         (let [{:keys [changed]} (ws/apply! store {:workspace-id draft-id})]
                           (expect (contains? (set (map :path changed)) "e.txt"))
                           (expect (contains? (set (map :root changed)) extra))
                           (expect (= "EDITED IN DRAFT\n" (slurp (io/file extra "e.txt")))))
                         (let [done (ws/abandon! store {:workspace-id draft-id :reason "done"})]
                           (some-> (:discard-future done)
                                   deref)
                           (expect (= :discarded (:state done)))
                           ;; both the primary clone and the extra-root clone are released
                           (expect (not (.exists (io/file clone))))
                           (expect (not (.exists (io/file (:root draft))))))
                         (finally (try (ws/abandon! store {:workspace-id draft-id})
                                       (catch Throwable _ nil)))))))))
        (finally (delete-tree! base) (delete-tree! extra))))))

(defdescribe
  isolation-hint-test
  "Why drafts are unavailable is an ACTIONABLE sentence, not just a capability
   matrix: the rift backend clones copy-on-write, so the filesystem — APFS on
   macOS, btrfs on Linux/WSL2 — is the real requirement."
  (it "names the platform's copy-on-write requirement"
      (expect (re-find #"(?i)apfs" (ws/cow-platform-hint "Mac OS X")))
      (expect (re-find #"(?i)btrfs" (ws/cow-platform-hint "Linux")))
      ;; WSL2 reports a plain Linux `os.name`; the btrfs requirement is identical.
      (expect (re-find #"(?i)btrfs"
                       (ws/cow-platform-hint "Linux 5.15.153.1-microsoft-standard-WSL2")))
      ;; A clone never crosses a filesystem boundary: btrfs (or APFS) over ONE
      ;; directory is enough, but root and store must share that single mount.
      (expect (re-find #"(?i)same APFS" (ws/cow-platform-hint "Mac OS X")))
      (expect (re-find #"(?i)same btrfs" (ws/cow-platform-hint "Linux")))
      (expect (re-find #"(?i)one directory is enough" (ws/cow-platform-hint "Linux")))
      (expect (re-find #"vis\.drafts\.dir" (ws/cow-platform-hint "Linux")))
      (expect (re-find #"(?i)copy-on-write" (ws/cow-platform-hint "Windows 11"))))
  (it "falls back to the platform requirement when no backend is available"
      (with-redefs [ws/workspace-capability-matrix (constantly [])]
        (expect (= (ws/cow-platform-hint (System/getProperty "os.name"))
                   (ws/isolation-unavailable-hint "/tmp")))))
  (it "reports an unforkable linked git worktree from the capability matrix"
      (with-redefs
        [ws/workspace-capability-matrix
         (constantly [{:backend :rift :available? false :reason :linked-git-worktree}])]
        (expect (re-find #"(?i)worktree" (ws/isolation-unavailable-hint "/tmp")))))
  (it "falls back to the platform requirement when a registered backend cannot clone"
      (with-redefs
        [ws/workspace-capability-matrix
         (constantly [{:backend :rift :available? false :reason :probe-failed}])]
        (expect (= (ws/cow-platform-hint (System/getProperty "os.name"))
                   (ws/isolation-unavailable-hint "/tmp"))))))

(defdescribe
  workspace-mechanism-test
  (it "persists the mechanism the backend reports, and nil when it reports none"
      (let
        [base
         (temp-dir "vis-ws-mech")

         drafts
         (temp-dir "vis-ws-mech-drafts")]

        (try (spit (io/file base "a.txt") "x\n")
             (binding [ws/*drafts-home* (io/file drafts)]
               (with-store
                 (fn [store]
                   (let
                     [seed (seed-workspace! store base)
                      reported (with-fork-mechanism :worktree
                                                    (fn []
                                                      (ws/create! store {:from seed})))
                      ;; a backend from before mechanism reporting returns a BARE path
                      legacy (with-fork-mechanism nil
                                                  (fn []
                                                    (ws/create! store {:from seed})))]

                     (expect (= :worktree (:workspace-mechanism reported)))
                     ;; and it survives the sqlite column, not just the in-memory return
                     (expect (= :worktree (:workspace-mechanism (ws/get store (:id reported)))))
                     (expect (nil? (:workspace-mechanism legacy)))
                     (expect (nil? (:workspace-mechanism (ws/get store (:id legacy)))))))))
             (finally (delete-tree! base) (delete-tree! drafts)))))
  (it "rift names the real mechanism it used for the clone"
      (let [base (temp-dir "vis-ws-mech-rift")]
        (try (if-not (ws/isolated-workspaces-supported? base)
               ;; No copy-on-write backend here (CI: ext4/NTFS) — the real clone can't run.
               (expect (not (ws/isolated-workspaces-supported? base)))
               (do (spit (io/file base "a.txt") "x\n")
                   (with-store
                     (fn [store]
                       (let
                         [seed (seed-workspace! store base)
                          draft (ws/create! store {:from seed})
                          mech (:workspace-mechanism draft)]

                         (try
                           ;; rift always names the mechanism it actually used
                           (expect (contains? #{:btrfs :reflink :apfs :worktree :copy} mech))
                           ;; a worktree-backed clone is a LINKED git worktree: .git is a FILE
                           (expect (= (= :worktree mech) (.isFile (io/file (:root draft) ".git"))))
                           (expect (= mech (:workspace-mechanism (ws/get store (:id draft)))))
                           (finally (try (ws/abandon! store {:workspace-id (:id draft)})
                                         (catch Throwable _ nil)))))))))
             (finally (delete-tree! base))))))
