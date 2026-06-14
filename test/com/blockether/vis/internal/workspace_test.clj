(ns com.blockether.vis.internal.workspace-test
  "Workspace primitive tests — rift CoW clones, git-free.

   The `*workspace-root*` binding contract is pure. The mutation paths
   (`create!` / `apply!` / `abandon!`) clone a tiny temp tree via rift
   (instant on CoW filesystems) and clean the clone up in `finally`, so
   the live repo and ~/.rifts are never touched."
  (:require
   [clojure.java.io :as io]
   [clojure.java.shell]
   [clojure.string :as str]
   [com.blockether.vis.ext.persistance-sqlite.core :as ps]
   [com.blockether.vis.ext.persistance-sqlite.registrar]
   [com.blockether.vis.internal.workspace :as ws]
   [lazytest.core :refer [defdescribe expect it]]
   [next.jdbc :as jdbc]))

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

(defn- sh!
  [& args]
  (let [{:keys [exit out err]} (apply clojure.java.shell/sh args)]
    (when-not (zero? exit)
      (throw (ex-info (str "shell failed: " (str/join " " args))
               {:exit exit :out out :err err})))
    out))

(defn- seed-workspace!
  "Insert a lightweight 'current' workspace row rooted at `base` (no
   clone), to serve as the fork parent for `create!`."
  [store base]
  (ps/db-workspace-insert! store
    {:id        (str (random-uuid))
     :repo-id   "rt"
     :repo-root base
     :root      base
     :state     :active}))

(defn- pin-session!
  "Insert a session_soul + session_state pinned 1:1 to `workspace-id`, so
   `discard-session-clones!` can resolve soul → state → workspace."
  [store soul-id workspace-id]
  (let [ds (:datasource store)
        st (str (random-uuid))]
    (jdbc/execute! ds ["INSERT INTO session_soul (id, channel, created_at) VALUES (?,?,?)"
                       soul-id "tui" 1])
    (jdbc/execute! ds [(str "INSERT INTO session_state "
                         "(id, session_soul_id, workspace_id, version, created_at) "
                         "VALUES (?,?,?,?,?)") st soul-id workspace-id 0 1])
    st))

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

(defdescribe linked-worktree-source-test
  (it "refuses linked Git worktrees before entering the native rift clone path"
    (let [base   (temp-dir "vis-ws-worktree-base")
          linked (temp-dir "vis-ws-worktree-linked")]
      (try
        (sh! "git" "-C" base "init")
        (spit (io/file base "a.txt") "x\n")
        (sh! "git" "-C" base "add" "a.txt")
        (sh! "git" "-C" base "-c" "user.name=test" "-c" "user.email=test@example.com"
          "commit" "-m" "init")
        (delete-tree! linked)
        (sh! "git" "-C" base "worktree" "add" linked "-b" "linked")
        (with-store
          (fn [store]
            (let [seed   (seed-workspace! store linked)
                  result (try
                           (ws/create! store {:from seed})
                           nil
                           (catch clojure.lang.ExceptionInfo e
                             (:type (ex-data e))))]
              (expect (= :workspace/unsupported-rift-source result)))))
        (finally
          (try (sh! "git" "-C" base "worktree" "remove" "--force" linked)
            (catch Throwable _ nil))
          (delete-tree! base)
          (when (.exists (io/file linked))
            (delete-tree! linked)))))))

(defdescribe checkpoint-chain-test
  (it "accepts, rejects, undoes, redoes, and applies cumulative checkpoint edits"
    (let [base (temp-dir "vis-checkpoint-chain")]
      (try
        (spit (io/file base "a.txt") "original\n")
        (with-store
          (fn [store]
            (let [trunk    (seed-workspace! store base)
                  soul     (str (random-uuid))
                  state-id (pin-session! store soul (:id trunk))
                  cp1      (ws/checkpoint-create! store {:session-state-id state-id
                                                         :label "expr-1"})]
              (Thread/sleep 8)
              (spit (io/file (:root cp1) "a.txt") "one\n")
              (spit (io/file (:root cp1) "b.txt") "from-one\n")
              (let [receipt (ws/checkpoint-diff store (:id cp1))]
                (expect (= #{[:modify "a.txt"] [:add "b.txt"]}
                          (set (map (juxt :status :path) (:changes receipt)))))
                (expect (every? :after-sha256 (:changes receipt))))
              (expect (= :accepted
                        (:status (ws/checkpoint-accept! store
                                   {:session-state-id state-id
                                    :checkpoint-id (:id cp1)}))))
              (expect (= (:id cp1) (:id (ws/for-session store state-id))))

              ;; A rejected child never mutates or repoints the accepted tip.
              (let [rejected (ws/checkpoint-create! store {:session-state-id state-id
                                                           :label "expr-reject"})]
                (Thread/sleep 8)
                (spit (io/file (:root rejected) "a.txt") "reject-me\n")
                (ws/checkpoint-reject! store {:session-state-id state-id
                                              :checkpoint-id (:id rejected)})
                (expect (= (:id cp1) (:id (ws/for-session store state-id))))
                (expect (not (.exists (io/file (:root rejected))))))

              ;; A second accepted child can move backward and forward without
              ;; reverse patches; both immutable trees remain available.
              (let [cp2 (ws/checkpoint-create! store {:session-state-id state-id
                                                      :label "expr-2"})]
                (Thread/sleep 8)
                (spit (io/file (:root cp2) "a.txt") "two\n")
                (ws/checkpoint-accept! store {:session-state-id state-id
                                              :checkpoint-id (:id cp2)})
                (expect (= "two\n" (slurp (io/file (:root (ws/for-session store state-id)) "a.txt"))))
                (ws/checkpoint-undo! store {:session-state-id state-id})
                (expect (= (:id cp1) (:id (ws/for-session store state-id))))
                (expect (= "one\n" (slurp (io/file (:root (ws/for-session store state-id)) "a.txt"))))
                (ws/checkpoint-redo! store {:session-state-id state-id
                                            :checkpoint-id (:id cp2)})
                (expect (= (:id cp2) (:id (ws/for-session store state-id))))

                ;; Cumulative apply includes b.txt from cp1 even though its
                ;; mtime predates cp2's immediate fork baseline.
                (let [{:keys [landed]} (ws/apply! store {:workspace-id (:id cp2)})]
                  (expect (= 2 landed))
                  (expect (= "two\n" (slurp (io/file base "a.txt"))))
                  (expect (= "from-one\n" (slurp (io/file base "b.txt")))))
                (ws/abandon-lineage! store {:workspace-id (:id cp2)
                                            :reason "test complete"})
                (expect (not (.exists (io/file (:root cp2)))))
                (expect (not (.exists (io/file (:root cp1)))))))))
        (finally (delete-tree! base)))))

  (it "refuses a checkpoint when the session workspace has extra context roots"
    (let [base (temp-dir "vis-checkpoint-multi")
          ext  (temp-dir "vis-checkpoint-extra")]
      (try
        (with-store
          (fn [store]
            (let [trunk    (seed-workspace! store base)
                  state-id (pin-session! store (str (random-uuid)) (:id trunk))]
              (ws/add-context-root! store (:id trunk) ext)
              (expect (= :workspace/checkpoint-multi-root
                        (try
                          (ws/checkpoint-create! store {:session-state-id state-id})
                          nil
                          (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))))
        (finally (delete-tree! base) (delete-tree! ext))))))

(defdescribe checkpoint-graph-revision-test
  (it "accept, undo, and redo move filesystem and CTX revisions together"
    (let [base (temp-dir "vis-checkpoint-graph")]
      (try
        (with-store
          (fn [store]
            (let [trunk    (seed-workspace! store base)
                  soul     (str (random-uuid))
                  state-id (pin-session! store soul (:id trunk))
                  before   {:session/tasks
                            {"goal" {:id "t1/goal" :title "Ship DAG" :status :doing}}
                            :session/facts {}}
                  child    (ws/checkpoint-create! store
                             {:session-state-id state-id :label "graph-revision"})
                  after    {:session/tasks
                            {"goal" {:id "t1/goal" :title "Ship DAG" :status :doing}
                             "render" {:id "t1/render" :title "Render F2" :status :done
                                       :parent "goal"
                                       :depends_on [[:fact "verified"]]}}
                            :session/facts
                            {"verified" {:id "t1/verified" :content "Focused tests pass"
                                         :status :active}}}]
              (ws/checkpoint-accept! store
                {:session-state-id state-id
                 :checkpoint-id (:id child)
                 :ctx-before before
                 :ctx after
                 :settlement {:tasks {"render" {:status :done}}}
                 :receipt {:tasks ["render"] :facts ["verified"]}})

              (expect (= after (:ctx (ps/db-workspace-graph-revision store (:id child)))))
              (expect (= (:session/tasks after) (ps/db-list-tasks store state-id)))
              (expect (= (:session/facts after) (ps/db-list-facts store state-id)))
              (expect (= after (ps/db-load-latest-ctx store soul)))

              (let [details (ws/dag-details store soul after)]
                (expect (:tracked? details))
                (expect (= [3 2 1 2 1]
                          ((juxt :node-count :task-count :fact-count :edge-count :root-count)
                           details)))
                (expect (= #{["task:render" :parent "task:goal"]
                             ["task:render" :depends-on "fact:verified"]}
                          (set (map (juxt :from :relation :to) (:edges details)))))
                (expect (= #{["task:goal" :task :doing]
                             ["task:render" :task :done]
                             ["fact:verified" :fact :active]}
                          (set (map (juxt :id :kind :status) (:nodes details)))))
                (expect (= ["render"] (:settlement-tasks details)))
                (expect (= ["verified"] (:settlement-facts details))))

              ;; A later ordinary turn on the same workspace refreshes that
              ;; workspace's graph head without replacing settlement metadata.
              (let [refreshed (assoc-in after [:session/facts "note"]
                                {:id "t2/note" :content "normal turn" :status :active})
                    turn-id   (ps/db-store-session-turn! store
                                {:parent-session-id soul
                                 :user-request "continue"
                                 :status :running})]
                (ps/db-update-session-turn! store turn-id
                  {:status :done :iteration-count 1 :duration-ms 1 :ctx refreshed})
                (expect (= refreshed (:ctx (ps/db-workspace-graph-revision store (:id child)))))
                (expect (= refreshed (ps/db-load-latest-ctx store soul))))

              (let [undone (ws/checkpoint-undo! store {:session-state-id state-id})]
                (expect (:graph? undone))
                (expect (= before (:ctx undone)))
                (expect (= before (ps/db-load-latest-ctx store soul)))
                (expect (= (:session/tasks before) (ps/db-list-tasks store state-id)))
                (expect (= {} (ps/db-list-facts store state-id))))

              (let [redone (ws/checkpoint-redo! store
                             {:session-state-id state-id
                              :checkpoint-id (:id child)})]
                (expect (:graph? redone))
                (expect (= "normal turn" (get-in (:ctx redone) [:session/facts "note" :content])))
                (expect (= "normal turn"
                          (get-in (ps/db-load-latest-ctx store soul)
                            [:session/facts "note" :content]))))

              (ws/abandon-lineage! store {:workspace-id (:id child)
                                          :reason "test complete"}))))
        (finally (delete-tree! base))))))

(defdescribe checkpoint-stale-test
  (it "rejects stale acceptance after another checkpoint becomes the tip"
    (let [base (temp-dir "vis-checkpoint-stale")]
      (try
        (with-store
          (fn [store]
            (let [trunk    (seed-workspace! store base)
                  state-id (pin-session! store (str (random-uuid)) (:id trunk))
                  stale    (ws/checkpoint-create! store {:session-state-id state-id
                                                         :label "stale"})
                  winner   (ws/checkpoint-create! store {:session-state-id state-id
                                                         :label "winner"})]
              (ws/checkpoint-accept! store {:session-state-id state-id
                                            :checkpoint-id (:id winner)})
              (expect (= :workspace/checkpoint-stale
                        (try
                          (ws/checkpoint-accept! store {:session-state-id state-id
                                                        :checkpoint-id (:id stale)})
                          nil
                          (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
              (ws/checkpoint-undo! store {:session-state-id state-id})
              (ws/checkpoint-reject! store {:session-state-id state-id
                                            :checkpoint-id (:id stale)})
              (ws/checkpoint-reject! store {:session-state-id state-id
                                            :checkpoint-id (:id winner)}))))
        (finally (delete-tree! base))))))

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

(defdescribe context-roots-test
  (it "on a TRUNK session: adds live (clone==trunk), dedups by trunk, persists, removes"
    (with-store
      (fn [store]
        (let [ws  (ps/db-workspace-insert! store {:id        (str (random-uuid))
                                                  :repo-id   "r"
                                                  :repo-root "/tmp"
                                                  :root      "/tmp"})
              wid (:id ws)
              a   (temp-dir "ctx-a")
              b   (temp-dir "ctx-b")]
          (try
            (ws/add-context-root! store wid a)
            (ws/add-context-root! store wid b)
            (ws/add-context-root! store wid b) ;; duplicate -> no-op
            (let [roots (ws/context-roots (ws/get store wid))]
              (expect (= [a b] (mapv :trunk roots)))
                ;; trunk session → live, NOT cloned
              (expect (every? #(and (= (:trunk %) (:clone %)) (nil? (:fork-ms %))) roots)))
            (ws/remove-context-root! store wid a)
            (expect (= [b] (mapv :trunk (ws/context-roots (ws/get store wid)))))
            (expect (= :threw
                      (try (ws/add-context-root! store wid "/no/such/dir/zzz")
                        :no-throw
                        (catch clojure.lang.ExceptionInfo _ :threw))))
            (finally (delete-tree! a) (delete-tree! b)))))))

  (it "on a DRAFT session: a context root is auto-cloned, edits land on apply!, clones trashed on abandon!"
    (let [base (temp-dir "vis-ws-ctx-base")
          ext  (temp-dir "vis-ws-ctx-ext")]
      (try
        (spit (io/file base "a.txt") "base\n")
        (spit (io/file ext "lib.txt") "orig\n")
        (with-store
          (fn [store]
            (let [seed     (seed-workspace! store base)
                  draft    (ws/create! store {:from seed})
                  draft-id (:id draft)]
              (try
                (ws/add-context-root! store draft-id ext)
                (let [entry (first (ws/context-roots (ws/get store draft-id)))]
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
                (finally
                  (try (ws/abandon! store {:workspace-id draft-id}) (catch Throwable _ nil)))))))
        (finally (delete-tree! base) (delete-tree! ext)))))

  (it "a DRAFT with MORE THAN ONE context root clones each, and apply! lands them all"
    (let [base (temp-dir "vis-multi-base")
          e1   (temp-dir "vis-multi-e1")
          e2   (temp-dir "vis-multi-e2")]
      (try
        (spit (io/file base "a.txt") "base\n")
        (spit (io/file e1 "f1.txt") "o1\n")
        (spit (io/file e2 "f2.txt") "o2\n")
        (with-store
          (fn [store]
            (let [draft (ws/create! store {:from (seed-workspace! store base)})
                  did   (:id draft)]
              (try
                (ws/add-context-root! store did e1)
                (ws/add-context-root! store did e2)
                (let [roots (ws/context-roots (ws/get store did))]
                  (expect (= 2 (count roots)))
                  (expect (every? #(not= (:trunk %) (:clone %)) roots)) ;; both cloned
                  (Thread/sleep 8)
                  (doseq [r roots]
                    (spit (io/file (:clone r) (if (re-find #"e1" (:trunk r)) "f1.txt" "f2.txt"))
                      "EDITED\n"))
                  (ws/apply! store {:workspace-id did})
                  (expect (= "EDITED\n" (slurp (io/file e1 "f1.txt"))))
                  (expect (= "EDITED\n" (slurp (io/file e2 "f2.txt")))))
                (finally (try (ws/abandon! store {:workspace-id did}) (catch Throwable _ nil)))))))
        (finally (delete-tree! base) (delete-tree! e1) (delete-tree! e2)))))

  (it "DELETE trashes a draft's clones (primary + context); a TRUNK session's real dirs survive"
    (let [base (temp-dir "vis-del-base")
          ext  (temp-dir "vis-del-ext")
          live (temp-dir "vis-del-live")]
      (try
        (spit (io/file base "a.txt") "x\n")
        (spit (io/file ext "e.txt") "x\n")
        (with-store
          (fn [store]
            ;; DRAFT session → discard-session-clones! trashes primary + context clones
            (let [draft (ws/create! store {:from (seed-workspace! store base)})
                  soul  (str (random-uuid))]
              (pin-session! store soul (:id draft))
              (ws/add-context-root! store (:id draft) ext)
              (let [entry (first (ws/context-roots (ws/get store (:id draft))))]
                (expect (.exists (io/file (:root draft))))
                (expect (.exists (io/file (:clone entry))))
                (ws/discard-session-clones! store soul)
                (expect (not (.exists (io/file (:root draft)))))
                (expect (not (.exists (io/file (:clone entry)))))
                (expect (.exists (io/file ext)))))            ;; REAL context dir untouched
            ;; TRUNK session → discard must NEVER touch the user's real cwd
            (let [trunk (ws/create-trunk-at! store live)
                  soul2 (str (random-uuid))]
              (pin-session! store soul2 (:id trunk))
              (ws/discard-session-clones! store soul2)
              (expect (.exists (io/file live))))))
        (finally (delete-tree! base) (delete-tree! ext) (delete-tree! live))))))
