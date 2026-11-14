(ns com.blockether.vis.internal.workspace.core-test
  "Workspace primitive tests: roots, the workspace row of a session and `/cd`.
   Each test works in its own temporary directory and store."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.persistance.sqlite.core :as ps]
            [com.blockether.vis.internal.workspace.core :as ws]
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
  (let [store (ps/db-open! :memory)]
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

(defn- pin-session!
  "Insert a session_soul + session_state pinned 1:1 to `workspace-id`, so
   `for-session` can resolve state → workspace."
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

(defdescribe create-trunk-root-normalization-test
             (it "normalizes a home-relative root before persisting the workspace"
                 (with-store (fn [store]
                               (let [workspace (ws/create-trunk-at! store "~")]
                                 (expect (= (ws/normalize-root "~") (:root workspace))))))))

;; A new session in a project gets the project folder, not the process directory.
(defdescribe new-session-default-root-test
             (it "creates the default trunk in the bound project folder, not user.dir"
                 (let [project (temp-dir "vis-new-session-project")]
                   (try (with-store (fn [store]
                                      (binding [ws/*workspace-root* project]
                                        (let [workspace (ws/ensure-workspace! store {})]
                                          (expect (= project (:root workspace)))
                                          (expect (not= (ws/trunk-root) (:root workspace)))))))
                        (finally (delete-tree! project))))))

(defdescribe
  change-root-test
  (it "repoints the session to a trunk at the new path"
      (let [a
            (temp-dir "vis-root-a")

            b
            (temp-dir "vis-root-b")]

        (try (with-store (fn [store]
                           (let [trunk
                                 (ws/create-trunk-at! store a)

                                 state-id
                                 (pin-session! store (str (random-uuid)) (:id trunk))

                                 ws2
                                 (ws/change-root! store state-id b)]

                             (expect (= (ws/normalize-root b) (:root ws2)))
                             (expect (= (:id ws2) (:id (ws/for-session store state-id)))))))
             (finally (delete-tree! a) (delete-tree! b)))))
  ;; Regression: /cd relative paths must follow the session root, not the JVM cwd.
  (it "resolves relative paths from the session's current root"
      (let [parent
            (temp-dir "vis-root-relative")

            first
            (io/file parent "first")

            next-dir
            (io/file parent "next")]

        (try (.mkdir first)
             (.mkdir next-dir)
             (with-store (fn [store]
                           (let [trunk
                                 (ws/create-trunk-at! store first)

                                 state-id
                                 (pin-session! store (str (random-uuid)) (:id trunk))

                                 up
                                 (ws/change-root! store state-id "..")]

                             (expect (= parent (:root up)))
                             (let [child (ws/change-root! store state-id "next")]
                               (expect (= (.getCanonicalPath next-dir) (:root child)))
                               (expect (= (:id child)
                                          (:id (ws/change-root! store state-id "."))))))))
             (finally (delete-tree! parent)))))
  (it "is a no-op returning the SAME workspace when the path already is the root"
      (let [a (temp-dir "vis-root-same")]
        (try (with-store (fn [store]
                           (let [trunk (ws/create-trunk-at! store a)
                                 state-id (pin-session! store (str (random-uuid)) (:id trunk))
                                 ws2 (ws/change-root! store state-id a)]

                             (expect (= (:id trunk) (:id ws2))))))
             (finally (delete-tree! a)))))
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

;; Regression: after `/cd` into another project's folder, a session stayed filed in the
;; old project and its group. Clients list it under the new folder's project, which has
;; no such group, so the session was in no list.
(defn- filed-session!
  "A session at `root`, filed in a project for `root` and in one of its groups.
   Returns `[soul-id state-id]`."
  [store root]
  (let [project
        (ps/db-create-project! store {:name "old" :workspace-root root})

        group
        (ps/db-create-session-group! store (:id project) {:name "band"})

        soul-id
        (str (random-uuid))

        state-id
        (pin-session! store soul-id (:id (ws/create-trunk-at! store root)))]

    (ps/db-set-session-group! store soul-id (:id group))
    [soul-id state-id]))

(defdescribe change-session-root-test
             (it "moves a filed session to the project of its new root and out of its old group"
                 (let [a
                       (temp-dir "vis-session-root-a")

                       b
                       (temp-dir "vis-session-root-b")]

                   (try (with-store (fn [store]
                                      (let [[soul-id state-id]
                                            (filed-session! store a)

                                            target
                                            (ps/db-create-project! store
                                                                   {:name "new" :workspace-root b})

                                            moves
                                            (ws/root-change-count)]

                                        (ws/change-session-root! store soul-id state-id b)
                                        (let [session (ps/db-get-session store soul-id)]
                                          (expect (= (:id target) (:project-id session)))
                                          (expect (nil? (:group-id session))))
                                        (expect (< (long moves) (long (ws/root-change-count)))))))
                        (finally (delete-tree! a) (delete-tree! b)))))
             (it "takes a filed session out of its project when no project owns the new root"
                 (let [a
                       (temp-dir "vis-session-root-a")

                       b
                       (temp-dir "vis-session-root-b")]

                   (try (with-store (fn [store]
                                      (let [[soul-id state-id] (filed-session! store a)]
                                        (ws/change-session-root! store soul-id state-id b)
                                        (let [session (ps/db-get-session store soul-id)]
                                          (expect (nil? (:project-id session)))
                                          (expect (nil? (:group-id session)))))))
                        (finally (delete-tree! a) (delete-tree! b)))))
             (it "keeps the project and group when the root does not move"
                 (let [a (temp-dir "vis-session-root-same")]
                   (try (with-store (fn [store]
                                      (let [[soul-id state-id] (filed-session! store a)
                                            before (ps/db-get-session store soul-id)]

                                        (ws/change-session-root! store soul-id state-id a)
                                        (let [session (ps/db-get-session store soul-id)]
                                          (expect (= (:project-id before) (:project-id session)))
                                          (expect (some? (:group-id session)))
                                          (expect (= (:group-id before) (:group-id session)))))))
                        (finally (delete-tree! a))))))

(defdescribe
  live-workspace-filesystem-roots-test
  (it "lists the configured roots and marks the ones that default searches skip"
      (let [base
            (temp-dir "vis-filesystem-roots")

            [extra shared]
            (mapv (fn [name]
                    (.getCanonicalPath (doto (io/file base name) .mkdirs)))
                  ["extra" "shared"])

            env
            {:workspace/root base
             :security/filesystem-roots [extra shared extra]
             :security/no-search-roots [extra]
             :security-policy {:jail-enabled true}}]

        (try (expect (= [{:root extra :no-search? true} {:root shared}]
                        (ws/env-filesystem-roots env)))
             (finally (delete-tree! base)))))
  (it "resolves a moved root again after the workspace changes"
      ;; The roots cache must not keep the old target of a symlinked root.
      (let [base
            (temp-dir "vis-moved-root")

            old-target
            (doto (io/file base "old") .mkdirs)

            new-target
            (doto (io/file base "new") .mkdirs)

            link
            (.toPath (io/file base "link"))

            point!
            (fn [^java.io.File target]
              (java.nio.file.Files/deleteIfExists link)
              (java.nio.file.Files/createSymbolicLink
                link
                (.toPath target)
                (make-array java.nio.file.attribute.FileAttribute 0)))

            live
            (atom nil)

            env
            {:workspace-atom live
             :security/filesystem-roots [(str link)]
             :security-policy {:jail-enabled true}}]

        (try (point! old-target)
             (reset! live {:root (str link)})
             (expect (= [(.getCanonicalPath ^java.io.File old-target)]
                        (map :root (ws/env-filesystem-roots env))))
             (expect (= (.getCanonicalPath ^java.io.File old-target) (ws/workspace-root env)))
             (point! new-target)
             (reset! live {:root (str link)})
             (expect (= [(.getCanonicalPath ^java.io.File new-target)]
                        (map :root (ws/env-filesystem-roots env))))
             (expect (= (.getCanonicalPath ^java.io.File new-target) (ws/workspace-root env)))
             (finally (java.nio.file.Files/deleteIfExists link) (delete-tree! base))))))

(defn- git!
  [^java.io.File root & args]
  (let [pb (ProcessBuilder. ^java.util.List (into ["git"] (map str) args))]
    (.directory pb root)
    (.redirectErrorStream pb true)
    (let [p (.start pb)]
      (slurp (.getInputStream p))
      (.waitFor p))))

(defdescribe workspace-files-test
             ;; #396: the language guards of a session share one host listing, also under a jail.
             (it "lists what git sees once for every caller of the same root"
                 (let [dir (temp-dir "vis-workspace-files")]
                   (try (git! (io/file dir) "init" "-q")
                        (spit (io/file dir ".gitignore") "dist/\n")
                        (.mkdirs (io/file dir "dist"))
                        (spit (io/file dir "dist" "bundle.js") "generated\n")
                        (.mkdirs (io/file dir "src"))
                        (spit (io/file dir "src" "core.clj") "(ns core)\n")
                        (let [listed (ws/workspace-files dir)
                              root (.getAbsolutePath (io/file dir))]

                          (expect (= #{(str root "/.gitignore") (str root "/src/core.clj")}
                                     (set listed)))
                          ;; A file written right after the listing waits for the next one.
                          (spit (io/file dir "src" "late.clj") "(ns late)\n")
                          (expect (= listed (ws/workspace-files dir))))
                        (finally (delete-tree! dir)))))
             (it "answers nil outside a Git working tree"
                 (let [dir (temp-dir "vis-workspace-files-plain")]
                   (try (spit (io/file dir "a.clj") "(ns a)\n")
                        (expect (nil? (ws/workspace-files dir)))
                        (finally (delete-tree! dir))))))

(defdescribe config-sources-test
             (it "reads every tier without a choice"
                 (expect (nil? (ws/normalize-config-sources nil)))
                 (expect (nil? (ws/normalize-config-sources ["project" "global"])))
                 (expect (ws/source-enabled? "global")))
             (it "keeps a chosen subset, also the empty --vanilla set"
                 (expect (= #{"project"} (ws/normalize-config-sources ["project"])))
                 (expect (= #{} (ws/normalize-config-sources [])))
                 (binding [ws/*config-sources* #{"project"}]
                   (expect (not (ws/source-enabled? "global")))
                   (expect (ws/source-enabled? "project"))))
             (it "rejects an unknown tier with a 400 error"
                 (let [e (try (ws/normalize-config-sources ["global" "home"])
                              nil
                              (catch clojure.lang.ExceptionInfo e e))]
                   (expect (= {:status 400 :type :config/unknown-source :sources ["home"]}
                              (ex-data e))))))
