(ns com.blockether.vis.internal.workspace-test
  "Workspace primitive tests — rift CoW clones, git-free.

   The `*workspace-root*` binding contract is pure. The mutation paths
   (`create!` / `apply!` / `abandon!`) clone a tiny temp tree via rift
   (instant on CoW filesystems) and clean the clone up in `finally`, so
   the live repo and ~/.rifts are never touched."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.ext.persistance-sqlite.core :as ps]
            [com.blockether.vis.ext.persistance-sqlite.registrar]
            ;; Registers the :rift workspace backend (the capability that create!/apply!
            ;; dispatch to) at load via its top-level register-extension!.
            [com.blockether.vis.ext.workspace-rift]
            [com.blockether.vis.internal.workspace :as ws]
            [lazytest.core :refer [defdescribe expect it]]
            [next.jdbc :as jdbc]))

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

(defn- pin-session!
  "Insert a session_soul + session_state pinned 1:1 to `workspace-id`, so
   `discard-session-clones!` can resolve soul → state → workspace."
  [store soul-id workspace-id]
  (let [ds
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
                     (let [seed (seed-workspace! store base)
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
  (it "apply! throws for an unknown workspace-id"
      (with-store (fn [store]
                    (expect (try (ws/apply! store {:workspace-id "nope"})
                                 false
                                 (catch clojure.lang.ExceptionInfo _ true)))))))
(defdescribe
  fresh-draft-roundtrip-test
  (it
    "create! :fresh? starts EMPTY; apply! lands only created files and never deletes trunk"
    (let [base (temp-dir "vis-ws-fresh")]
      (try
        (if-not (ws/isolated-workspaces-supported? base)
          ;; No copy-on-write backend here (CI) — the live round-trip can't run.
          (expect (not (ws/isolated-workspaces-supported? base)))
          (do
            (spit (io/file base "keep.txt") "KEEP\n")
            (with-store
              (fn [store]
                (let [seed (seed-workspace! store base)
                      draft (ws/create! store {:from seed :fresh? true})
                      draft-id (:id draft)]

                  (try
                    ;; an isolated root that carries NOTHING from trunk (HEAD)
                    (expect (some? (:root draft)))
                    (expect (not= base (:root draft)))
                    (expect (not (.exists (io/file (:root draft) "keep.txt"))))
                    ;; still a real draft applying back into base, with the
                    ;; zero baseline that makes deletions un-inferable
                    (expect (ws/draft? draft))
                    (expect (= base (:repo-root draft)))
                    (expect (= 0 (:fork-ms draft)))
                    ;; a file created in the fresh draft lands on apply!…
                    (spit (io/file (:root draft) "new.txt") "NEW\n")
                    ;; adversarial: recreate a trunk-named file inside the
                    ;; draft, then delete it again — the deletion must NOT
                    ;; travel to trunk (the draft never owned that file)
                    (spit (io/file (:root draft) "keep.txt") "SCRATCH\n")
                    (io/delete-file (io/file (:root draft) "keep.txt"))
                    (let [{:keys [changed]} (ws/apply! store {:workspace-id draft-id})]
                      (expect (= #{"new.txt"} (set (map :path changed))))
                      (expect (not (some #(= :delete (:status %)) changed)))
                      (expect (= "NEW\n" (slurp (io/file base "new.txt"))))
                      ;; …and trunk's pre-existing file SURVIVES: a fresh
                      ;; draft can never report files it never saw as deleted
                      (expect (= "KEEP\n" (slurp (io/file base "keep.txt")))))
                    (finally (try (ws/abandon! store {:workspace-id draft-id})
                                  (catch Throwable _ nil)))))))))
        (finally (delete-tree! base))))))


(defdescribe
  linked-worktree-source-test
  (it
    "refuses linked Git worktrees before entering the native rift clone path"
    (let [base
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
            (let [seed
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
    (let [base
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
                  (let [seed
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

(defmacro ^:private when-cow
  "Run a draft round-trip body only when a copy-on-write workspace backend is
   available for `base` (skipped in CI on ext4/NTFS — no reflink); otherwise
   assert it IS unavailable so the test still carries one expectation."
  [base & body]
  `(if (ws/isolated-workspaces-supported? ~base)
     (do ~@body)
     (expect (not (ws/isolated-workspaces-supported? ~base)))))

(defdescribe
  filesystem-roots-test
  (it "on a TRUNK session: adds live (clone==trunk), dedups by trunk, persists, removes"
      (with-store
        (fn [store]
          (let [ws
                (ps/db-workspace-insert!
                  store
                  {:id (str (random-uuid)) :repo-id "r" :repo-root "/tmp" :root "/tmp"})

                wid
                (:id ws)

                a
                (temp-dir "ctx-a")

                b
                (temp-dir "ctx-b")]

            (try (ws/add-filesystem-root! store wid a)
                 (ws/add-filesystem-root! store wid b)
                 (ws/add-filesystem-root! store wid b) ;; duplicate -> no-op
                 (let [roots (ws/filesystem-roots (ws/get store wid))]
                   (expect (= [a b] (mapv :trunk roots)))
                   ;; trunk session → live, NOT cloned
                   (expect (every? #(and (= (:trunk %) (:clone %)) (nil? (:fork-ms %))) roots)))
                 (ws/remove-filesystem-root! store wid a)
                 (expect (= [b] (mapv :trunk (ws/filesystem-roots (ws/get store wid)))))
                 (expect (= :threw
                            (try (ws/add-filesystem-root! store wid "/no/such/dir/zzz")
                                 :no-throw
                                 (catch clojure.lang.ExceptionInfo _ :threw))))
                 (finally (delete-tree! a) (delete-tree! b)))))))
  (it
    "on a DRAFT session: a filesystem root is auto-cloned, edits land on apply!, clones trashed on abandon!"
    (let [base
          (temp-dir "vis-ws-ctx-base")

          ext
          (temp-dir "vis-ws-ctx-ext")]

      (try (spit (io/file base "a.txt") "base\n")
           (spit (io/file ext "lib.txt") "orig\n")
           (when-cow
             base
             (with-store
               (fn [store]
                 (let [seed
                       (seed-workspace! store base)

                       draft
                       (ws/create! store {:from seed})

                       draft-id
                       (:id draft)]

                   (try (ws/add-filesystem-root! store draft-id ext)
                        (let [entry (first (ws/filesystem-roots (ws/get store draft-id)))]
                          ;; auto-cloned: a real distinct working copy with a fork baseline
                          (expect (= (.getCanonicalPath (io/file ext)) (:trunk entry)))
                          (expect (not= (:trunk entry) (:clone entry)))
                          (expect (some? (:fork-ms entry)))
                          (expect (.exists (io/file (:clone entry) "lib.txt")))
                          ;; edit inside the context CLONE (isolated from the real dir)
                          (Thread/sleep 8)
                          (spit (io/file (:clone entry) "lib.txt") "EDITED\n")
                          (expect (= "orig\n" (slurp (io/file ext "lib.txt")))) ;; real dir untouched pre-apply
                          ;; apply! lands the context clone back into its own trunk
                          (let [{:keys [changed]} (ws/apply! store {:workspace-id draft-id})]
                            (expect (some #(and (= "lib.txt" (:path %))
                                                (= (.getCanonicalPath (io/file ext)) (:root %)))
                                          changed))
                            (expect (= "EDITED\n" (slurp (io/file ext "lib.txt")))))
                          ;; abandon! trashes the context clone too
                          (ws/abandon! store {:workspace-id draft-id :reason "done"})
                          (expect (not (.exists (io/file (:clone entry))))))
                        (finally (try (ws/abandon! store {:workspace-id draft-id})
                                      (catch Throwable _ nil))))))))
           (finally (delete-tree! base) (delete-tree! ext)))))
  (it
    "a DRAFT with MORE THAN ONE filesystem root clones each, and apply! lands them all"
    (let [base
          (temp-dir "vis-multi-base")

          e1
          (temp-dir "vis-multi-e1")

          e2
          (temp-dir "vis-multi-e2")]

      (try (spit (io/file base "a.txt") "base\n")
           (spit (io/file e1 "f1.txt") "o1\n")
           (spit (io/file e2 "f2.txt") "o2\n")
           (when-cow base
                     (with-store
                       (fn [store]
                         (let [draft
                               (ws/create! store {:from (seed-workspace! store base)})

                               did
                               (:id draft)]

                           (try (ws/add-filesystem-root! store did e1)
                                (ws/add-filesystem-root! store did e2)
                                (let [roots (ws/filesystem-roots (ws/get store did))]
                                  (expect (= 2 (count roots)))
                                  (expect (every? #(not= (:trunk %) (:clone %)) roots)) ;; both cloned
                                  (Thread/sleep 8)
                                  (doseq [r roots]
                                    (spit (io/file
                                            (:clone r)
                                            (if (re-find #"e1" (:trunk r)) "f1.txt" "f2.txt"))
                                          "EDITED\n"))
                                  (ws/apply! store {:workspace-id did})
                                  (expect (= "EDITED\n" (slurp (io/file e1 "f1.txt"))))
                                  (expect (= "EDITED\n" (slurp (io/file e2 "f2.txt")))))
                                (finally (try (ws/abandon! store {:workspace-id did})
                                              (catch Throwable _ nil))))))))
           (finally (delete-tree! base) (delete-tree! e1) (delete-tree! e2)))))
  (it
    "DELETE trashes a draft's clones (primary + context); a TRUNK session's real dirs survive"
    (let [base
          (temp-dir "vis-del-base")

          ext
          (temp-dir "vis-del-ext")

          live
          (temp-dir "vis-del-live")]

      (try (spit (io/file base "a.txt") "x\n")
           (spit (io/file ext "e.txt") "x\n")
           (when-cow base
                     (with-store
                       (fn [store]
                         ;; DRAFT session → discard-session-clones! trashes primary + context clones
                         (let [draft
                               (ws/create! store {:from (seed-workspace! store base)})

                               soul
                               (str (random-uuid))]

                           (pin-session! store soul (:id draft))
                           (ws/add-filesystem-root! store (:id draft) ext)
                           (let [entry (first (ws/filesystem-roots (ws/get store (:id draft))))]
                             (expect (.exists (io/file (:root draft))))
                             (expect (.exists (io/file (:clone entry))))
                             (ws/discard-session-clones! store soul)
                             (expect (not (.exists (io/file (:root draft)))))
                             (expect (not (.exists (io/file (:clone entry)))))
                             (expect (.exists (io/file ext))))) ;; REAL filesystem dir untouched
                         ;; TRUNK session → discard must NEVER touch the user's real cwd
                         (let [trunk
                               (ws/create-trunk-at! store live)

                               soul2
                               (str (random-uuid))]

                           (pin-session! store soul2 (:id trunk))
                           (ws/discard-session-clones! store soul2)
                           (expect (.exists (io/file live)))))))
           (finally (delete-tree! base) (delete-tree! ext) (delete-tree! live))))))

(defdescribe
  change-root-test
  (it "repoints the session to a trunk at the new path and carries extras over"
      (let [a
            (temp-dir "vis-root-a")

            b
            (temp-dir "vis-root-b")

            ext
            (temp-dir "vis-root-ext")]

        (try (with-store (fn [store]
                           (let [trunk
                                 (ws/create-trunk-at! store a)

                                 state-id
                                 (pin-session! store (str (random-uuid)) (:id trunk))]

                             (ws/add-filesystem-root! store (:id trunk) ext)
                             (let [ws2 (ws/change-root! store state-id b)]
                               ;; a fresh TRUNK at b, now pinned to the session
                               (expect (= (ws/normalize-root b) (:root ws2)))
                               (expect (= (:id ws2) (:id (ws/for-session store state-id))))
                               (expect (not (ws/draft? ws2)))
                               ;; additional filesystem roots are session permissions — they carry
                               (expect (= [(ws/normalize-root ext)]
                                          (mapv :trunk (ws/filesystem-roots ws2))))))))
             (finally (delete-tree! a) (delete-tree! b) (delete-tree! ext)))))
  (it "is a no-op returning the SAME workspace when the path already is the root"
      (let [a (temp-dir "vis-root-same")]
        (try (with-store (fn [store]
                           (let [trunk (ws/create-trunk-at! store a)
                                 state-id (pin-session! store (str (random-uuid)) (:id trunk))
                                 ws2 (ws/change-root! store state-id a)]

                             (expect (= (:id trunk) (:id ws2))))))
             (finally (delete-tree! a)))))
  (it "drops a carried extra that equals the new root (it is the root now)"
      (let [a
            (temp-dir "vis-root-promote")

            ext
            (temp-dir "vis-root-promote-ext")]

        (try (with-store (fn [store]
                           (let [trunk
                                 (ws/create-trunk-at! store a)

                                 state-id
                                 (pin-session! store (str (random-uuid)) (:id trunk))]

                             (ws/add-filesystem-root! store (:id trunk) ext)
                             (let [ws2 (ws/change-root! store state-id ext)]
                               (expect (= (ws/normalize-root ext) (:root ws2)))
                               (expect (empty? (ws/filesystem-roots ws2)))))))
             (finally (delete-tree! a) (delete-tree! ext)))))
  (it "refuses while the session is in a draft"
      (let [a
            (temp-dir "vis-root-draft")

            b
            (temp-dir "vis-root-draft-b")]

        (try (with-store
               (fn [store]
                 ;; seed-workspace! stamps fork-ms 1 → draft? true
                 (let [seed
                       (seed-workspace! store a)

                       state-id
                       (pin-session! store (str (random-uuid)) (:id seed))

                       thrown
                       (try (ws/change-root! store state-id b) nil (catch Exception t t))]

                   (expect (some? thrown))
                   (expect (= :workspace/root-change-in-draft (:type (ex-data thrown)))))))
             (finally (delete-tree! a) (delete-tree! b)))))
  (it "throws on a non-directory path"
      (let [a (temp-dir "vis-root-nodir")]
        (try (with-store (fn [store]
                           (let [trunk (ws/create-trunk-at! store a)
                                 state-id (pin-session! store (str (random-uuid)) (:id trunk))
                                 thrown (try
                                          (ws/change-root! store state-id (str a "/nope-missing"))
                                          nil
                                          (catch Exception t t))]

                             (expect (some? thrown))
                             (expect (= :workspace/not-a-directory (:type (ex-data thrown)))))))
             (finally (delete-tree! a))))))

(defdescribe
  deleted-paths-guard-test
  (it "a positive baseline reports trunk files missing from the clone as deletions"
      (let [trunk
            (temp-dir "vis-delguard-t")

            clone
            (temp-dir "vis-delguard-c")]

        (try (spit (io/file trunk "gone.txt") "x\n")
             (Thread/sleep 8)
             ;; sanity: the mechanism itself works when the baseline is real
             (expect (= ["gone.txt"] (ws/deleted-paths clone trunk (System/currentTimeMillis))))
             (finally (delete-tree! trunk) (delete-tree! clone)))))
  (it "a non-positive (FRESH) baseline can NEVER infer a deletion, whatever the trees hold"
      (let [trunk
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
             (finally (delete-tree! trunk) (delete-tree! clone))))))

(defdescribe
  fresh-lineage-inheritance-test
  (it
    "a draft forked :from a FRESH draft (sub-loop/revision) inherits baseline 0, so its apply! can never delete trunk (HEAD) files"
    (let [base (temp-dir "vis-ws-fresh-lineage")]
      (try (if-not (ws/isolated-workspaces-supported? base)
             ;; No copy-on-write backend here (CI) — the live round-trip can't run.
             (expect (not (ws/isolated-workspaces-supported? base)))
             (do (spit (io/file base "head.txt") "REAL WORK\n")
                 (with-store
                   (fn [store]
                     (let [seed (seed-workspace! store base)
                           fresh (ws/create! store {:from seed :fresh? true})
                           ;; exactly what loop/child-workspace! does for a
                           ;; sub-loop spawned INSIDE the fresh draft
                           child (ws/create! store {:from fresh :label "subloop"})]

                       (try
                         ;; the child clones the (near-empty) fresh clone and
                         ;; MUST inherit the zero baseline: a wall-clock
                         ;; baseline here would make apply! read every trunk
                         ;; file (older + absent from the clone) as DELETED
                         ;; and wipe the repo
                         (expect (= 0 (:fork-ms child)))
                         (expect (not (.exists (io/file (:root child) "head.txt"))))
                         (spit (io/file (:root child) "made.txt") "BY CHILD\n")
                         (let [{:keys [changed]} (ws/apply! store {:workspace-id (:id child)})]
                           ;; a fresh lineage NEVER deletes…
                           (expect (not (some #(= :delete (:status %)) changed)))
                           (expect (some #(= "made.txt" (:path %)) changed))
                           ;; …and trunk's real HEAD work survives the merge
                           (expect (= "REAL WORK\n" (slurp (io/file base "head.txt"))))
                           (expect (= "BY CHILD\n" (slurp (io/file base "made.txt")))))
                         (finally (doseq [wid [(:id child) (:id fresh)]]
                                    (try (ws/abandon! store {:workspace-id wid})
                                         (catch Throwable _ nil))))))))))
           (finally (delete-tree! base))))))
