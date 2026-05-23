(ns com.blockether.vis.ext.foundation-core.merge-ops-test
  "PLAN.md §12 step 10 follow-up — merge-resolve SCI ops.

   Each test sets up a real temp git repo with a divergent commit
   on the same file from both sides, runs `git merge` to land the
   conflict, then exercises mr/* against the resulting worktree."
  (:require
   [clojure.java.io :as io]
   [clojure.java.shell :as sh]
   [clojure.string :as str]
   [com.blockether.vis.ext.foundation-core.merge-ops :as mr]
   [com.blockether.vis.internal.channel-events :as channel-events]
   [com.blockether.vis.internal.workspace :as workspace]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- temp-dir [prefix]
  (.getCanonicalPath
    (.toFile
      (java.nio.file.Files/createTempDirectory
        prefix
        (make-array java.nio.file.attribute.FileAttribute 0)))))

(defn- delete-tree! [root]
  (doseq [f (reverse (file-seq (io/file root)))]
    (io/delete-file f true)))

(defn- git!
  [dir args]
  (let [argv   (into ["git" "-C" (.getCanonicalPath (io/file dir))] args)
        result (apply sh/sh argv)]
    (when-not (zero? (:exit result))
      (throw (ex-info (str "git failed: " (str/join " " argv))
               (assoc result :argv argv))))
    (str/trim (or (:out result) ""))))

(defn- init-conflict-repo!
  "Build a git repo whose HEAD has both a `feature` branch and a
   trunk-side divergent commit on the same file, then run `git merge`
   so the worktree carries an active MERGE_HEAD + a conflicting
   `note.txt`. Returns the repo root."
  [base]
  (.mkdirs (io/file base))
  (git! base ["init"])
  (git! base ["config" "user.name" "Vis Test"])
  (git! base ["config" "user.email" "vis-test@example.invalid"])
  (spit (io/file base "note.txt") "base\n")
  (git! base ["add" "note.txt"])
  (git! base ["commit" "-m" "base"])
  (let [trunk (git! base ["rev-parse" "--abbrev-ref" "HEAD"])]
    (git! base ["checkout" "-b" "feature"])
    (spit (io/file base "note.txt") "feature-side\n")
    (git! base ["commit" "-am" "feature-side"])
    (git! base ["checkout" trunk])
    (spit (io/file base "note.txt") "trunk-side\n")
    (git! base ["commit" "-am" "trunk-divergence"])
    ;; Attempt merge — expected to land in conflict; swallow non-zero exit.
    (try (apply sh/sh ["git" "-C" base "merge" "--no-ff" "--no-commit" "feature"])
      (catch Throwable _ nil))
    base))

;; =============================================================================
;; mr/status
;; =============================================================================

(defdescribe status-test
  (it "returns {:in-progress? false} when no merge is active"
    (let [base (temp-dir "vis-mr-status-clean")]
      (try
        (.mkdirs (io/file base))
        (git! base ["init"])
        (git! base ["config" "user.name"  "Vis Test"])
        (git! base ["config" "user.email" "vis-test@example.invalid"])
        (spit (io/file base "seed.txt") "seed\n")
        (git! base ["add" "seed.txt"])
        (git! base ["commit" "-m" "seed"])
        (binding [workspace/*workspace-root* base]
          (let [s (mr/status)]
            (expect (false? (:in-progress? s)))))
        (finally (delete-tree! base)))))

  (it "surfaces conflicts + branches when a merge is in progress"
    (let [base (temp-dir "vis-mr-status-conflict")]
      (try
        (init-conflict-repo! base)
        (binding [workspace/*workspace-root* base]
          (let [s (mr/status)]
            (expect (true? (:in-progress? s)))
            (expect (some? (:head s)))
            (expect (some? (:merge-head s)))
            (expect (some #(= "note.txt" (:path %)) (:conflicts s)))))
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
          (let [out (mr/continue! {:message "test merge"})]
            (expect (= :continued (:result out)))
            (expect (= "trunk-side\n" (slurp (io/file base "note.txt"))))
            ;; merge complete -> no in-progress, no conflicts
            (let [s (mr/status)]
              (expect (false? (:in-progress? s))))))
        (finally (delete-tree! base)))))

  (it "accept-theirs keeps the branch content"
    (let [base (temp-dir "vis-mr-accept-theirs")]
      (try
        (init-conflict-repo! base)
        (binding [workspace/*workspace-root* base]
          (mr/accept-theirs "note.txt")
          (mr/continue! {:message "took theirs"})
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
          (try (mr/continue! {:message "nope"})
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
            (expect (= :aborted (:result out))))
          (let [s (mr/status)]
            (expect (false? (:in-progress? s)))))
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
              (mr/continue! {:message "done"
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
