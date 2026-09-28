(ns com.blockether.vis.internal.sandbox.draft-policy-test
  "Draft prerequisites through the real session Python and process boundaries."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.foundation.core :as foundation]
            [com.blockether.vis.internal.foundation.drafts :as drafts]
            [com.blockether.vis.internal.foundation.editing.core :as editing]
            [com.blockether.vis.internal.loop.environment :as loop-env]
            [com.blockether.vis.internal.loop.python-exec :as python-exec]
            [com.blockether.vis.internal.persistance.core :as persistance]
            [com.blockether.vis.internal.python.env :as ep]
            [com.blockether.vis.internal.sandbox.jail :as jail]
            [com.blockether.vis.internal.sandbox.policy :as policy]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [com.blockether.vis.internal.workspace.git :as git]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- with-project
  ([backend f] (with-project backend nil true f))
  ([backend cache-policy f] (with-project backend cache-policy true f))
  ([backend cache-policy jail? f]
   (let [dir
         (.toFile (java.nio.file.Files/createTempDirectory
                    "vis-draft-policy"
                    (make-array java.nio.file.attribute.FileAttribute 0)))

         db
         (persistance/db-create-connection! :memory)

         entries
         (mapv (fn [id]
                 (cond-> {"id" id
                          "path" (.getCanonicalPath (doto (io/file dir id) (.mkdirs)))
                          "python_name" (str id "_path")}
                   (contains? #{"cache" "extra"} id)
                   (assoc "search" false)

                   (and (= id "cache") cache-policy)
                   (assoc "draft" (name cache-policy))

                   (= id "reference")
                   (assoc "access" "read-only")))
               ["project" "extra" "unselected" "cache" "reference"])

         root
         (get (first entries) "path")

         snapshot
         (policy/snapshot {"workspace" {"filesystem" entries}
                           "jail" {"enabled" jail?
                                   "filesystem" {"allow" ["project" "extra" "unselected" "cache"
                                                          "reference"]}}})]

     (try (doseq [entry entries]
            (let [project (io/file (get entry "path"))]
              (spit (io/file project "source.txt") "original")
              (when-not (= "cache" (get entry "id"))
                (doseq [args [["init" "-q" "-b" "main"] ["config" "user.name" "Vis Test"]
                              ["config" "user.email" "vis-test@example.invalid"]
                              ["config" "commit.gpgsign" "false"] ["add" "source.txt"]
                              ["commit" "-q" "-m" "initial"]]]
                  (expect (zero? (:exit (git/run-git project args))))))))
          (with-redefs-fn {#'loop-env/security-config-snapshot (constantly snapshot)
                           #'workspace/draft-isolation-plan
                           (constantly (mapv (fn [entry]
                                               {:trunk (get entry "path")
                                                :policy (keyword (get entry "draft"))})
                                             (filter #(get % "draft") entries)))}
            #(binding [workspace/*draft-backend* backend workspace/*drafts-home*
                       (.getPath (io/file dir "drafts"))] (let [ws
                                                                (workspace/create-trunk-at! db root)

                                                                environment
                                                                (loop-env/create-environment
                                                                  ::router
                                                                  {:db db :workspace-id (:id ws)})]

                                                            (reset! (:extensions environment)
                                                              [foundation/vis-extension])
                                                            (try (f environment root)
                                                                 (finally
                                                                   (loop-env/dispose-environment!
                                                                     environment))))))
          (finally (doseq [file (reverse (file-seq dir))]
                     (io/delete-file file true)))))))

(defn- shell-result
  [environment root code]
  (let [^Process child
        (jail/spawn! ["/bin/sh" "-c" code]
                     (io/file root)
                     ((:jail-policy-fn environment))
                     {:merge-stderr? true})

        output
        (future (slurp (.getInputStream ^Process child)))]

    {:exit (.waitFor child) :out @output}))

(defdescribe
  enabled-draft-trunk-is-read-only
  (it "enabled draft trunk is read only"
      ;; #242: protect a continued shared checkout across real raw Python, shell and host writers.
      (with-project
        :worktree
        (fn [environment root]
          (let [context
                (ep/python-context environment)

                result
                (ep/run-python-block
                  context
                  (str "from pathlib import Path\nimport os\n"
                       "p = Path("
                       (pr-str (str root "/source.txt"))
                       ")\n"
                       "print(p.read_text())\n"
                       "for change in [lambda: p.write_text('changed'), lambda: open(p, 'w'), "
                       "lambda: p.unlink(), lambda: p.rename(p.with_suffix('.moved')), "
                       "lambda: os.replace(p, p.with_suffix('.moved'))]:\n"
                       "    try:\n        change()\n"
                       "    except PermissionError:\n        print('draft required')\n"))]

            (expect (= "original" (slurp (io/file root "source.txt"))))
            (expect (= 5 (count (re-seq #"draft required" (str (:stdout result))))))
            (expect (not (zero? (long (:exit (shell-result environment
                                                           root
                                                           "printf changed > source.txt"))))))
            (expect (= "original" (slurp (io/file root "source.txt")))))
          (let [before
                (:ext.symbol/before-fn editing/patch-symbol)

                out
                (before environment #'editing/patch-tool [(str root "/source.txt") []])]

            (expect (contains? out :result))
            (expect (re-find #"draft_create" (str out)))
            (expect (= "original" (slurp (io/file root "source.txt")))))
          (let [cache
                (.getCanonicalPath (io/file root ".." "cache"))

                reference
                (.getCanonicalPath (io/file root ".." "reference"))

                result
                (#'python-exec/execute-code
                 environment
                 (str "from pathlib import Path\n"
                      "Path("
                      (pr-str (str cache "/source.txt"))
                      ").write_text('cached')\n"
                      "try:\n    Path(" (pr-str (str reference "/source.txt"))
                      ").write_text('escaped')\n"
                      "except PermissionError:\n    print('reference protected')\n"))]

            (expect (nil? (:error result)) (str result))
            (expect (= "cached" (slurp (io/file cache "source.txt"))))
            (expect (= "original" (slurp (io/file reference "source.txt")))))))))

(defdescribe
  existing-worker-follows-new-draft
  (it
    "existing worker follows new draft"
    ;; #242: rebinding Path names alone cannot expand an already-launched kernel policy.
    (with-project
      :worktree
      (fn [environment root]
        (let [context
              (ep/python-context environment)

              extra
              (.getCanonicalPath (io/file root ".." "extra"))

              _
              (ep/run-python-block
                context
                (str "from pathlib import Path\nretained_value = 17\noriginal = Path("
                     (pr-str root)
                     ")\n"))

              opened
              (drafts/draft-create environment "worker-transition" true [root extra])

              clone
              (get-in opened [:result "root"])]

          (expect (string? clone) (str opened))
          (let
            [result
             (#'python-exec/execute-code
              environment
              (str
                "(project_root_path / 'source.txt').write_text('draft')\n"
                "(extra_path / 'source.txt').write_text('extra draft')\n"
                "assert retained_value == 17\n"
                "assert extra_path != Path("
                (pr-str extra)
                ")\n"
                "for p in [original, Path("
                (pr-str extra)
                "), unselected_path, reference_path]:\n"
                "    try:\n        (p / 'source.txt').write_text('escaped')\n"
                "    except PermissionError:\n        print('protected')\n    else:\n        raise AssertionError('original stayed writable')\n"))

             extra-clone
             (:clone (first (filter #(= extra (:trunk %))
                                    (workspace/env-filesystem-roots
                                      (assoc environment
                                        :workspace @(:workspace-atom environment))))))]

            (expect (nil? (:error result)) (str result))
            (expect (= "draft" (slurp (io/file clone "source.txt"))))
            (expect (= "extra draft" (slurp (io/file extra-clone "source.txt"))))
            (expect (= "original" (slurp (io/file extra "source.txt")))))
          (expect (= "original" (slurp (io/file root "source.txt"))))
          (expect (zero? (:exit (shell-result environment clone "printf shell > source.txt"))))
          (expect (not (zero? (long (:exit (shell-result environment
                                                         root
                                                         "printf escaped > source.txt"))))))
          (let [discarded (drafts/draft-discard environment)]
            (expect (nil? (:error discarded)) (str discarded)))
          (let
            [result
             (#'python-exec/execute-code
              environment
              (str
                "try:\n    Path(" (pr-str (str clone "/source.txt"))
                ").write_text('stale')\n"
                "except PermissionError:\n    print('closed draft protected')\nelse:\n    raise AssertionError('closed draft stayed writable')\n"))]
            (expect (nil? (:error result)) (str result))
            (expect (= root (:root @(:workspace-atom environment))))))))))

(defdescribe
  off-preserves-shared-writes-and-mode-changes-stop-stale-context
  (it "off preserves shared writes and mode changes stop stale context"
      (with-project
        :off
        (fn [environment root]
          (let [context (ep/python-context environment)]
            (ep/run-python-block context
                                 (str "from pathlib import Path\nPath("
                                      (pr-str (str root "/source.txt"))
                                      ").write_text('off')\n"))
            (expect (= "off" (slurp (io/file root "source.txt")))))
          (binding [workspace/*draft-backend* :worktree]
            (expect (= :draft/policy-changed
                       (try (#'python-exec/execute-code environment "print('must not execute')")
                            nil
                            (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))))))

(defdescribe
  disposed-session-resumes-its-owned-draft
  (it "disposed session resumes its owned draft"
      ;; #242: a fresh worker must recover private grants, not silently return to shared writes.
      (with-project
        :worktree
        (fn [environment root]
          (ep/python-context environment)
          (let [opened
                (drafts/draft-create environment "resume-private")

                clone
                (get-in opened [:result "root"])

                db
                (:db-info environment)

                session-id
                (:session-id @(:state-atom environment))]

            (expect (string? clone) (str opened))
            (expect (some? session-id))
            (loop-env/dispose-environment! environment)
            (let [resumed (loop-env/create-environment ::router {:db db :session session-id})]
              (try (reset! (:extensions resumed) [foundation/vis-extension])
                   (expect (= clone (:root @(:workspace-atom resumed))))
                   (let [result (#'python-exec/execute-code
                                 resumed
                                 "(project_root_path / 'source.txt').write_text('resumed')\n")]
                     (expect (nil? (:error result)) (str result))
                     (expect (= "resumed" (slurp (io/file clone "source.txt"))))
                     (expect (= "original" (slurp (io/file root "source.txt")))))
                   (drafts/draft-discard resumed)
                   (finally (loop-env/dispose-environment! resumed)))))))))

(defdescribe
  new-repository-in-a-pregranted-cache-requires-rebuild
  (it "new repository in a pregranted cache requires rebuild"
      ;; #242: audit updates cannot revoke a writable handle opened before root selection.
      (with-project
        :worktree
        (fn [environment root]
          (let [context
                (ep/python-context environment)

                cache
                (.getCanonicalPath (io/file root ".." "cache"))

                epoch
                @python-exec/policy-reload-epoch]

            (ep/run-python-block
              context
              (str "held_cache_file = open(" (pr-str (str cache "/source.txt")) ", 'r+')\n"))
            (doseq [args [["init" "-q" "-b" "main"] ["config" "user.name" "Vis Test"]
                          ["config" "user.email" "vis-test@example.invalid"]
                          ["config" "commit.gpgsign" "false"] ["add" "source.txt"]
                          ["commit" "-q" "-m" "initial"]]]
              (let [result (git/run-git (io/file cache) args)]
                (expect (= 0 (:exit result)) (str result))))
            ;; The lifecycle call can occur inside the current Python block, before
            ;; execute-code gets another chance to reject a newly discovered root.
            (let [opened (drafts/draft-create environment "late-repository" true [root cache])]
              (try (expect (some? (:error opened)) (str opened))
                   (expect (re-find #"Start a new turn" (get-in opened [:error :message] "")))
                   (expect (= root (:root @(:workspace-atom environment))))
                   (finally (when (workspace/draft? @(:workspace-atom environment))
                              (drafts/draft-discard environment)))))
            (expect (= :draft/policy-expanded
                       (try (#'python-exec/execute-code
                             environment
                             "held_cache_file.write('escaped'); held_cache_file.flush()\n")
                            nil
                            (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
            (expect (#'loop-env/policy-stale?
                     {:policy-epoch (java.util.concurrent.atomic.AtomicLong. epoch)}))
            (expect (= "original" (slurp (io/file cache "source.txt"))))
            (ep/run-python-block context "held_cache_file.close()\n"))))))

(defn- assert-copied-cache-protection
  [environment root]
  (let [cache
        (.getCanonicalPath (io/file root ".." "cache"))

        available?
        (workspace/isolated-workspaces-supported? root)]

    (ep/python-context environment)
    (expect (some #{cache} (:workspace/draft-protected-roots environment)))
    (let [opened
          (drafts/draft-create environment "copied-cache" true)

          clone
          (:clone (first (filter #(= cache (:trunk %))
                                 (workspace/env-filesystem-roots environment))))]

      (try (if available?
             (do (expect (nil? (:error opened)) (str opened))
                 (expect (and (string? clone) (not= cache clone))))
             ;; Filesystems without CoW must refuse, not copy or widen source access.
             (do (expect (= "workspace/capability-unavailable"
                            (get-in opened [:error :details "type"]))
                         (str opened))
                 (expect (= root (:root @(:workspace-atom environment))))
                 (expect (not (workspace/draft? @(:workspace-atom environment))))
                 (expect (= cache clone))))
           (expect (not (jail/draft-policy-expanded? environment)))
           (let [result
                 (#'python-exec/execute-code
                  environment
                  (str (when available? "(cache_path / 'source.txt').write_text('private cache')\n")
                       "try:\n    Path("
                       (pr-str (str cache "/source.txt"))
                       ").write_text('escaped')\n"
                       "except PermissionError:\n    pass\n"
                       "else:\n    raise AssertionError('copied cache source stayed writable')\n"))]
             (expect (nil? (:error result)) (str result))
             (when available? (expect (= "private cache" (slurp (io/file clone "source.txt")))))
             (expect (= "original" (slurp (io/file cache "source.txt")))))
           (finally (when (workspace/draft? @(:workspace-atom environment))
                      (drafts/draft-discard environment)))))))

(defdescribe copied-no-search-cache-is-protected-at-worker-launch
             (it "copied no search cache is protected at worker launch"
                 ;; #242: private dependency copies must not invalidate the worker on the next block.
                 (with-project :rift :copy-only assert-copied-cache-protection)))

(defdescribe unavailable-copy-keeps-no-search-cache-protected
             (it "unavailable copy keeps no search cache protected"
                 ;; Exercise the refusal boundary even on development filesystems with CoW support.
                 (with-redefs [workspace/rift-available? (constantly {:available? false
                                                                      :reason :probe-failed})]
                   (with-project :rift :copy-only assert-copied-cache-protection))))

(defdescribe
  whole-filesystem-grant-is-not-a-protected-original
  (it "whole filesystem grant is not a protected original"
      ;; A disabled jail grants every host filesystem root. Protecting one of those made the
      ;; draft prerequisite refuse EVERY host write of a drafted session — the draft's own
      ;; private copy included — so `patch` could not land an edit anywhere while raw Python
      ;; kept writing.
      (let [dir
            (.toFile (java.nio.file.Files/createTempDirectory
                       "vis-draft-grant"
                       (make-array java.nio.file.attribute.FileAttribute 0)))

            project
            (doto (io/file dir "project") (.mkdirs))

            drafts
            (doto (io/file project ".vis" "drafts") (.mkdirs))

            clone
            (doto (io/file drafts "sessions" "session" "project" "label") (.mkdirs))

            moved
            (doto (io/file project "review" "sessions" "session" "project" "label") (.mkdirs))

            snapshot
            (policy/snapshot {"workspace" {"filesystem" [{"id" "project"
                                                          "path" (.getCanonicalPath project)}]}
                              "jail" {"enabled" false}})

            environment
            {:workspace {:repo-root (.getCanonicalPath project)}
             :security-policy snapshot
             :security/filesystem-roots (policy/read-write-roots snapshot)}

            host-root?
            (fn [path]
              (nil? (.getParentFile (io/file path))))]

        (try (binding [workspace/*draft-backend*
                       :worktree

                       workspace/*drafts-home*
                       (.getCanonicalPath drafts)]

               ;; The grant a disabled jail adds is the whole machine, never a checkout.
               (expect (some host-root? (policy/read-write-roots snapshot)))
               (expect (not-any? host-root? (jail/draft-source-roots environment)))
               (expect (some? (jail/draft-write-refusal environment
                                                        (.getCanonicalPath
                                                          (io/file project "source.txt")))))
               (expect (nil? (jail/draft-write-refusal environment
                                                       (.getCanonicalPath
                                                         (io/file dir "scratch.txt")))))
               ;; The private clone store is where the prerequisite steers writes TO, even when
               ;; a protected root holds it.
               (expect (nil? (jail/draft-write-refusal environment
                                                       (.getCanonicalPath (io/file clone
                                                                                   "source.txt")))))
               (expect (nil? (jail/draft-write-refusal
                               (assoc environment
                                 :workspace/drafts-home (.getCanonicalPath (io/file project
                                                                                    "review")))
                               (.getCanonicalPath (io/file moved "source.txt"))))))
             (finally (doseq [file (reverse (file-seq dir))]
                        (io/delete-file file true)))))))

(defdescribe
  disabled-jail-drafts-write-their-own-copies
  (it "disabled jail drafts write their own copies"
      ;; The default configuration runs WITHOUT the jail, and that grants every host filesystem
      ;; root. Sweeping those grants as protected originals refused every host write of a drafted
      ;; session — the draft's own copies included — so `patch` could not land an edit anywhere
      ;; while raw Python kept writing the shared checkout.
      (with-project
        :worktree nil
        false
        (fn [environment root]
          (let [extra
                (.getCanonicalPath (io/file root ".." "extra"))

                unselected
                (.getCanonicalPath (io/file root ".." "unselected"))

                scratch
                (.getCanonicalPath (io/file root ".." "scratch.txt"))

                opened
                (drafts/draft-create environment "disabled-jail" true [root extra])

                clone
                (get-in opened [:result "root"])

                extra-clone
                (:clone (first (filter #(= extra (:trunk %))
                                       (workspace/env-filesystem-roots
                                         (assoc environment
                                           :workspace @(:workspace-atom environment))))))

                gate
                (fn [path]
                  ((:ext.symbol/before-fn editing/patch-symbol)
                    environment
                    #'editing/patch-tool
                    [path [{"from" "1:000" "replace" "changed"}]]))]

            (expect (nil? (:error opened)) (str opened))
            (expect (string? clone) (str opened))
            (expect (not= root clone))
            (expect (string? extra-clone) (str opened))
            (expect (not= extra extra-clone))
            ;; Every selected repository takes host writes in its own copy, and so does scratch
            ;; space the blanket grant reaches.
            (doseq [path [(str clone "/source.txt") (str extra-clone "/source.txt") scratch]]
              ;; The pass-through carries the live environment: report the path, never the map.
              (expect (nil? (:result (gate path))) path))
            ;; Their originals, and a repository this draft never selected, stay read-only.
            (doseq [path [(str root "/source.txt") (str extra "/source.txt")
                          (str unselected "/source.txt")]]
              (let [refusal (:result (gate path))]
                (expect (some? refusal) path)
                (expect (re-find #"draft_create" (str refusal)) path)))
            (let [result (#'python-exec/execute-code
                          environment
                          (str "(project_root_path / 'source.txt').write_text('draft')\n"
                               "(extra_path / 'source.txt').write_text('extra draft')\n"))]
              (expect (nil? (:error result)) (str result)))
            (expect (= "draft" (slurp (io/file clone "source.txt"))))
            (expect (= "extra draft" (slurp (io/file extra-clone "source.txt"))))
            (doseq [original [root extra unselected]]
              (expect (= "original" (slurp (io/file original "source.txt"))) original))
            (let [discarded (drafts/draft-discard environment)]
              (expect (nil? (:error discarded)) (str discarded))))))))
