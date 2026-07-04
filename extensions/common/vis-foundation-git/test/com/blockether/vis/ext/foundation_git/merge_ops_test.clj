(ns com.blockether.vis.ext.foundation-git.merge-ops-test
  "Merge-resolve helper fns.

   The host-side Clojure fns under test are aliased as `mr/` in this
   namespace for brevity; on the Python side they surface under the `git/`
   alias as `git/merge-status`, `git/merge-accept-ours`, etc. Each
   test sets up a real temp git repo with a divergent commit on the
   same file from both sides, runs `git merge` to land the conflict,
   then exercises the helpers against the resulting worktree."
  (:require
   [clojure.java.io :as io]
   [com.blockether.vis.ext.foundation-git.merge-ops :as mr]
   [com.blockether.vis.internal.channel-events :as channel-events]
   [com.blockether.vis.internal.workspace :as workspace]
   [lazytest.core :refer [defdescribe expect it]])
  (:import
   [org.eclipse.jgit.api Git MergeCommand$FastForwardMode]))

(defn- temp-dir [prefix]
  (.getCanonicalPath
    (.toFile
      (java.nio.file.Files/createTempDirectory
        prefix
        (make-array java.nio.file.attribute.FileAttribute 0)))))

(defn- delete-tree! [root]
  (doseq [f (reverse (file-seq (io/file root)))]
    (io/delete-file f true)))

(defn- open-or-init!
  "Init a fresh repo at `base` via JGit (never shell out to `git`) with a
   deterministic identity and line-ending policy. `core.autocrlf=false` +
   `core.eol=lf` are pinned ON THE REPO so it never inherits the runner's
   global config (GitHub's Windows runner sets `autocrlf=true`, which would
   rewrite committed LF content to CRLF on checkout and break exact-content
   assertions). Returns an open `Git` — close it."
  ^Git [base]
  (.mkdirs (io/file base))
  (let [g   (-> (Git/init) (.setDirectory (io/file base)) .call)
        cfg (.. g getRepository getConfig)]
    (.setString cfg "user" nil "name" "Vis Test")
    (.setString cfg "user" nil "email" "vis-test@example.invalid")
    (.setString cfg "core" nil "autocrlf" "false")
    (.setString cfg "core" nil "eol" "lf")
    (.save cfg)
    g))

(defn- commit-file!
  "Write `content` to `rel` under `base`, stage it, and commit `msg`."
  [^Git g base rel content msg]
  (spit (io/file base rel) content)
  (-> g .add (.addFilepattern rel) .call)
  (-> g .commit (.setMessage msg) .call))

(defn- init-conflict-repo!
  "Build a git repo whose HEAD has both a `feature` branch and a
   trunk-side divergent commit on the same file, then merge `feature`
   (JGit, no-ff, no-commit) so the worktree carries an active MERGE_HEAD +
   a conflicting `note.txt`. Returns the repo root."
  [base]
  (with-open [g (open-or-init! base)]
    (commit-file! g base "note.txt" "base\n" "base")
    (let [trunk (.getBranch (.getRepository g))]
      (-> g .checkout (.setCreateBranch true) (.setName "feature") .call)
      (commit-file! g base "note.txt" "feature-side\n" "feature-side")
      (-> g .checkout (.setName trunk) .call)
      (commit-file! g base "note.txt" "trunk-side\n" "trunk-divergence")
      ;; Merge feature → CONTENT_CONFLICT; setCommit(false) leaves MERGE_HEAD.
      (-> g .merge
        (.include (.resolve (.getRepository g) "feature"))
        (.setCommit false)
        (.setFastForward MergeCommand$FastForwardMode/NO_FF)
        .call)
      base)))

;; =============================================================================
;; mr/status
;; =============================================================================

(defdescribe status-test
  (it "returns {:in-progress? false} when no merge is active"
    (let [base (temp-dir "vis-mr-status-clean")]
      (try
        (with-open [g (open-or-init! base)]
          (commit-file! g base "seed.txt" "seed\n" "seed"))
        (binding [workspace/*workspace-root* base]
          (let [s (mr/status)]
            (expect (false? (get s "in_progress")))))
        (finally (delete-tree! base)))))

  (it "surfaces conflicts + branches when a merge is in progress"
    (let [base (temp-dir "vis-mr-status-conflict")]
      (try
        (init-conflict-repo! base)
        (binding [workspace/*workspace-root* base]
          (let [s (mr/status)]
            (expect (true? (get s "in_progress")))
            (expect (some? (get s "head")))
            (expect (some? (get s "merge_head")))
            (expect (some #(= "note.txt" (get % "path")) (get s "conflicts")))))
        (finally (delete-tree! base))))))

;; =============================================================================
;; mr/accept-* + mr/continue!
;; =============================================================================

(defdescribe accept-and-continue-test
  (it "accept-ours keeps trunk content; continue! commits the merge"
    (let [base (temp-dir "vis-mr-accept-ours")]
      (try
        (init-conflict-repo! base)
        (binding [workspace/*workspace-root* base]
          (mr/accept-ours "note.txt")
          (let [out (mr/continue! {"message" "test merge"})]
            (expect (= "continued" (get out "result")))
            (expect (= "trunk-side\n" (slurp (io/file base "note.txt"))))
            ;; merge complete -> no in-progress, no conflicts
            (let [s (mr/status)]
              (expect (false? (get s "in_progress"))))))
        (finally (delete-tree! base)))))

  (it "accept-theirs keeps the branch content"
    (let [base (temp-dir "vis-mr-accept-theirs")]
      (try
        (init-conflict-repo! base)
        (binding [workspace/*workspace-root* base]
          (mr/accept-theirs "note.txt")
          (mr/continue! {"message" "took theirs"})
          (expect (= "feature-side\n" (slurp (io/file base "note.txt")))))
        (finally (delete-tree! base))))))

;; =============================================================================
;; mr/continue! refuses with outstanding conflicts
;; =============================================================================

(defdescribe continue-refuses-test
  (it "raises :merge-ops/unresolved-conflicts when conflicts remain"
    (let [base (temp-dir "vis-mr-unresolved")]
      (try
        (init-conflict-repo! base)
        (binding [workspace/*workspace-root* base]
          (try (mr/continue! {"message" "nope"})
            (expect false)
            (catch clojure.lang.ExceptionInfo e
              (expect (= :merge-ops/unresolved-conflicts
                        (:type (ex-data e)))))))
        (finally (delete-tree! base))))))

;; =============================================================================
;; mr/abort!
;; =============================================================================

(defdescribe abort-test
  (it "abort! reverts the merge attempt and clears MERGE_HEAD"
    (let [base (temp-dir "vis-mr-abort")]
      (try
        (init-conflict-repo! base)
        (binding [workspace/*workspace-root* base]
          (let [out (mr/abort! {})]
            (expect (= "aborted" (get out "result"))))
          (let [s (mr/status)]
            (expect (false? (get s "in_progress")))))
        (finally (delete-tree! base))))))

;; =============================================================================
;; UX events
;; =============================================================================

(defdescribe events-test
  (it "continue! publishes :session/merge-resolve-finished when :channel-id passed"
    (let [base (temp-dir "vis-mr-events-continue")]
      (try
        (init-conflict-repo! base)
        (let [events (atom [])]
          (with-redefs [channel-events/publish-channel-event!
                        (fn [channel event] (swap! events conj [channel event]))]
            (binding [workspace/*workspace-root* base]
              (mr/accept-ours "note.txt")
              ;; "message" is model-facing (string key); :channel-id/:session-id
              ;; are host-injected internals (keyword keys).
              (mr/continue! {"message" "done"
                             :channel-id :tui
                             :session-id "soul-1"})))
          (expect (some (fn [[ch ev]]
                          (and (= :tui ch)
                            (= :session/merge-resolve-finished (:type ev))
                            (= :continued (:result ev))
                            (= "soul-1" (:session-id ev))))
                    @events)))
        (finally (delete-tree! base)))))

  (it "abort! publishes :session/merge-resolve-finished :result :aborted"
    (let [base (temp-dir "vis-mr-events-abort")]
      (try
        (init-conflict-repo! base)
        (let [events (atom [])]
          (with-redefs [channel-events/publish-channel-event!
                        (fn [channel event] (swap! events conj [channel event]))]
            (binding [workspace/*workspace-root* base]
              (mr/abort! {:channel-id :telegram
                          :session-id "soul-2"})))
          (expect (some (fn [[ch ev]]
                          (and (= :telegram ch)
                            (= :session/merge-resolve-finished (:type ev))
                            (= :aborted (:result ev))))
                    @events)))
        (finally (delete-tree! base))))))
