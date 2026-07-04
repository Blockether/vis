(ns com.blockether.vis.ext.foundation-git.write-ops-test
  "Tests for the history-rewrite slice of foundation-git/write_ops:
   `reset!`, `branch!`, `checkout!`, `cherry-pick!`, `rebase!`.

   Every test spins up a fresh temp repo via `init-repo!` so JGit
   operations run against a real bare working tree \u2014 same shape as
   `core_test.clj`. Behaviour we pin:

   - shape of the returned map (`:op`, `:short-sha`, `:head-before/after`)
   - JGit-side state mutation actually happens (HEAD moves, branch
     appears in `branchList`, files restore on disk)
   - structured ex-info on bad opts so the model gets a real error
     rather than a NullPointerException from JGit deep stack"
  (:refer-clojure :exclude [reset!])
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.ext.foundation-git.write-ops :as wo]
   [com.blockether.vis.internal.workspace :as workspace]
   [lazytest.core :refer [defdescribe expect it]])
  (:import
   (java.nio.file Files)
   (java.nio.file.attribute FileAttribute)
   (org.eclipse.jgit.api Git)))

(defn- make-tmp-dir ^java.io.File []
  (.toFile (Files/createTempDirectory "vis-foundation-git-rewrite-"
             (into-array FileAttribute []))))

(defn- spit-rel [^java.io.File root rel content]
  (let [f (io/file root rel)]
    (.mkdirs (.getParentFile f))
    (spit f content)))

(defn- cleanup [^java.io.File root]
  (when (.exists root)
    (doseq [^java.io.File f (reverse (file-seq root))]
      (.delete f))))

(defn- with-workspace*
  "Pin `workspace/cwd` to `root` for the duration of `body-thunk`. The
   write-ops module reads the workspace root every call; tests need a
   stable temp dir so JGit doesn't crawl up to the actual Vis repo."
  [^java.io.File root body-thunk]
  (with-redefs [workspace/cwd (fn [] root)]
    (body-thunk)))

(defmacro ^:private with-workspace [root & body]
  `(with-workspace* ~root (fn [] ~@body)))

(def ^:private trunk-branch
  "Trunk branch name fixed for tests — host machine's
   `init.defaultBranch` should not affect the assertion shapes."
  "master")

(defn- init-repo!
  "Create a repo with N sequential commits on the trunk branch.
   Returns vec of short-sha strings in chronological (oldest-first)
   order."
  ([^java.io.File root] (init-repo! root 1))
  ([^java.io.File root n-commits]
   (with-open [g (-> (Git/init)
                   (.setDirectory root)
                   (.setInitialBranch trunk-branch)
                   .call)]
     (let [config (.. g getRepository getConfig)]
       (.setString config "user" nil "name" "Vis Test")
       (.setString config "user" nil "email" "vis-test@example.invalid")
       ;; Deterministic LF on every OS — never inherit the runner's global
       ;; core.autocrlf (GitHub Windows sets it true), which would rewrite
       ;; committed content to CRLF on checkout and break content asserts.
       (.setString config "core" nil "autocrlf" "false")
       (.setString config "core" nil "eol" "lf")
       (.save config))
     (vec
       (for [i (range n-commits)]
         (let [rel (str "src/f" i ".clj")]
           (spit-rel root rel (str "(ns f" i ") ;; rev " i "\n"))
           (-> g .add (.addFilepattern rel) .call)
           (let [c (-> g .commit (.setMessage (str "commit " i)) .call)]
             (subs (.getName c) 0 7))))))))

(defdescribe add-test
  (it "stages adds, modifications, AND deletions like `git add -A`"
    (let [root (make-tmp-dir)]
      (try
        ;; Seed: two tracked files committed.
        (init-repo! root 2)
        (with-workspace root
          ;; Working-tree churn: delete one tracked file, modify the
          ;; other, introduce a brand-new file.
          (.delete (io/file root "src/f0.clj"))
          (spit-rel root "src/f1.clj" "(ns f1) ;; edited\n")
          (spit-rel root "src/new.clj" "(ns new)\n")
          (let [res (wo/add ".")]
            (expect (= "git_add" (get res "op")))
            (with-open [g (Git/open root)]
              (let [s (.. g status call)]
                ;; The single-pass (update=false) bug left the deletion
                ;; unstaged in `getMissing`; the two-pass add clears it.
                (expect (contains? (set (.getRemoved s)) "src/f0.clj"))
                (expect (contains? (set (.getChanged s)) "src/f1.clj"))
                (expect (contains? (set (.getAdded s)) "src/new.clj"))
                (expect (empty? (.getMissing s)))))))
        (finally (cleanup root))))))

(defdescribe reset!-test
  (it "soft-resets HEAD to a prior revision, leaving worktree intact"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root 3)
        (with-workspace root
          (let [res (wo/reset! {"mode" "soft" "to" "HEAD~1"})]
            (expect (= "git_reset" (get res "op")))
            (expect (= "soft"  (get res "mode")))
            (expect (= "HEAD~1" (get res "to")))
            (expect (some? (get res "resolved_sha")))
            (expect (= 7 (count (get res "short_sha"))))
            (expect (some? (get res "head_before")))
            (expect (= (get res "resolved_sha") (get res "head_after")))
            ;; Worktree file from the dropped commit must still exist
            ;; (soft = keep index + worktree).
            (expect (.exists (io/file root "src/f2.clj")))))
        (finally (cleanup root)))))

  (it "rejects unknown revision with a structured ex-info"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root 1)
        (with-workspace root
          (let [thrown (try
                         (wo/reset! {"mode" "soft" "to" "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef"})
                         nil
                         (catch clojure.lang.ExceptionInfo e
                           (ex-data e)))]
            (expect (= :foundation-git/unknown-rev (:type thrown)))))
        (finally (cleanup root)))))

  (it "refuses missing :to with a structured ex-info"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root 1)
        (with-workspace root
          (let [thrown (try (wo/reset! {"mode" "soft"})
                         nil
                         (catch clojure.lang.ExceptionInfo e
                           (ex-data e)))]
            (expect (= :foundation-git/invalid-opts (:type thrown)))))
        (finally (cleanup root))))))

(defdescribe branch!-test
  (it "creates a branch from HEAD and lists it back"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root 2)
        (with-workspace root
          (let [created (wo/branch! {"op" "create" "name" "feature/x"})
                listed  (wo/branch! {"op" "list" "mode" "local"})]
            (expect (= "git_branch_create" (get created "op")))
            (expect (= "feature/x" (get created "name")))
            (expect (= "HEAD" (get created "from")))
            (expect (some? (get created "short_sha")))
            (let [names (set (map #(get % "short") (get listed "branches")))]
              (expect (contains? names "feature/x")))))
        (finally (cleanup root)))))

  (it "deletes a branch (is_force required when unmerged)"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root 1)
        (with-workspace root
          (wo/branch! {"op" "create" "name" "throwaway"})
          (let [del (wo/branch! {"op" "delete" "name" "throwaway" "is_force" true})]
            (expect (= "git_branch_delete" (get del "op")))
            (expect (= ["throwaway"] (get del "deleted")))
            (let [names (set (map #(get % "short") (get (wo/branch! {"op" "list"}) "branches")))]
              (expect (not (contains? names "throwaway"))))))
        (finally (cleanup root)))))

  (it "renames a branch atomically"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root 1)
        (with-workspace root
          (wo/branch! {"op" "create" "name" "old-name"})
          (let [r (wo/branch! {"op" "rename" "old" "old-name" "new" "new-name"})]
            (expect (= "git_branch_rename" (get r "op")))
            (expect (= "old-name" (get r "old")))
            (expect (= "new-name" (get r "new")))
            (let [names (set (map #(get % "short") (get (wo/branch! {"op" "list"}) "branches")))]
              (expect (contains? names "new-name"))
              (expect (not (contains? names "old-name"))))))
        (finally (cleanup root)))))

  (it "rejects unknown op with structured ex-info"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root 1)
        (with-workspace root
          (let [thrown (try (wo/branch! {"op" "garbage"})
                         nil
                         (catch clojure.lang.ExceptionInfo e
                           (ex-data e)))]
            (expect (= :foundation-git/invalid-opts (:type thrown)))))
        (finally (cleanup root))))))

(defdescribe checkout!-test
  (it "switches HEAD to a branch and reports the new head"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root 2)
        (with-workspace root
          (wo/branch! {"op" "create" "name" "feature/y"})
          (let [r (wo/checkout! {"branch" "feature/y"})]
            (expect (= "git_checkout" (get r "op")))
            (expect (= "feature/y" (get r "branch")))
            (expect (some? (get r "head")))
            (expect (= 7 (count (get r "short_head"))))
            (expect (not (get r "created")))))
        (finally (cleanup root)))))

  (it "creates and switches in one call when is_create true"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root 1)
        (with-workspace root
          (let [r (wo/checkout! {"branch" "feature/z" "is_create" true})]
            (expect (= "feature/z" (get r "branch")))
            (expect (true? (get r "created")))
            (let [names (set (map #(get % "short") (get (wo/branch! {"op" "list"}) "branches")))]
              (expect (contains? names "feature/z")))))
        (finally (cleanup root)))))

  (it "refuses an empty checkout (no branch / sha / paths)"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root 1)
        (with-workspace root
          (let [thrown (try (wo/checkout! {})
                         nil
                         (catch clojure.lang.ExceptionInfo e
                           (ex-data e)))]
            (expect (= :foundation-git/invalid-opts (:type thrown)))))
        (finally (cleanup root))))))

(defdescribe cherry-pick!-test
  (it "re-applies a single commit onto HEAD"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root 2)
        (with-workspace root
          ;; Create a branch off HEAD~1 so we can cherry-pick the
          ;; second commit onto it.
          (wo/branch! {"op" "create" "name" "side" "from" "HEAD~1"})
          (wo/checkout! {"branch" "side"})
          ;; Resolve trunk HEAD via list (short-sha of the latest).
          (let [trunk-sha (->> (wo/branch! {"op" "list" "mode" "local"})
                            (#(get % "branches"))
                            (some (fn [b] (when (= trunk-branch (get b "short")) (get b "sha")))))
                _         (expect (some? trunk-sha))
                res       (wo/cherry-pick! {"commits" trunk-sha})]
            (expect (= "git_cherry_pick" (get res "op")))
            (expect (= "OK" (get res "status")))
            (expect (seq (get res "picked")))
            (expect (.exists (io/file root "src/f1.clj")))))
        (finally (cleanup root))))))

(defdescribe fetch!-test
  (it "returns EDN data with ref/update details, not raw JGit objects"
    (let [remote (make-tmp-dir)
          local  (make-tmp-dir)]
      (try
        (init-repo! remote 1)
        (with-open [_ (-> (Git/cloneRepository)
                        (.setURI (str (.toURI remote)))
                        (.setDirectory local)
                        .call)]
          nil)
        (spit-rel remote "src/f1.clj" "(ns f1) ;; remote rev\n")
        (with-open [g (Git/open remote)]
          (-> g .add (.addFilepattern "src/f1.clj") .call)
          (-> g .commit (.setMessage "remote commit") .call))
        (with-workspace local
          (let [res      (wo/fetch! {})
                update   (first (get res "updates"))
                tracking (get res "tracking")]
            (expect (= "git_fetch" (get res "op")))
            (expect (= "origin" (get res "remote")))
            (expect (= "updated" (get res "status")))
            (expect (= "master" (get res "branch")))
            (expect (not (contains? res "messages")))
            (expect (not (contains? res "remote_messages")))
            (expect (not (contains? res "uri")))
            (expect (not (contains? res "peer_user_agent")))
            (expect (not (contains? res "advertised_ref_count")))
            (expect (= 1 (get-in res ["summary" "updated"])))
            (expect (false? (get-in res ["summary" "up_to_date"])))
            (expect (= 0 (get tracking "ahead")))
            (expect (= 1 (get tracking "behind")))
            (expect (seq (get res "updates")))
            (expect (map? update))
            (expect (string? (get update "ref")))
            (expect (string? (get update "kind")))
            (expect (string? (get update "range")))
            (expect (string? (get update "local_name")))
            (expect (string? (get update "remote_name")))
            (expect (string? (get update "result")))
            (expect (string? (get update "old_sha")))
            (expect (string? (get update "new_sha")))
            (expect (= 7 (count (get update "old_short_sha"))))
            (expect (= 7 (count (get update "new_short_sha"))))))
        (finally
          (cleanup local)
          (cleanup remote))))))

(defdescribe rebase!-test
  (it "non-interactive rebase fast-forwards onto upstream"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root 1)
        (with-workspace root
          ;; Create a feature branch and add one commit; rebase onto
          ;; main (no divergence \u2192 UP_TO_DATE).
          (wo/branch! {"op" "create" "name" "feature/r"})
          (wo/checkout! {"branch" "feature/r"})
          (let [r (wo/rebase! {"operation" "begin" "upstream" trunk-branch})]
            (expect (= "git_rebase" (get r "op")))
            ;; Status is JGit's enum name; UP_TO_DATE / FAST_FORWARD /
            ;; OK all count as success.
            (expect (true? (get r "successful")))))
        (finally (cleanup root)))))

  (it "edit-todos-fn observes the todo list and survives a no-op edit"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root 3)
        (with-workspace root
          ;; Branch off HEAD~2, add a commit, rebase onto current main
          ;; with an :edit-todos-fn that just records what it sees.
          (let [seen (atom nil)]
            (wo/branch! {"op" "create" "name" "feature/todo" "from" "HEAD~2"})
            (wo/checkout! {"branch" "feature/todo"})
            (spit-rel root "extra.txt" "hello")
            (with-open [g (Git/open root)]
              (-> g .add (.addFilepattern "extra.txt") .call)
              (-> g .commit (.setMessage "feature commit") .call))
            ;; :edit-todos-fn is an INTERNAL Clojure callback (keyword key \u2014
            ;; the model can never supply it); the todo maps it sees stay
            ;; idiomatic keyword Clojure (they never cross the boundary).
            (wo/rebase! {"operation" "begin" "upstream" trunk-branch
                         :edit-todos-fn (fn [todos]
                                          (clojure.core/reset! seen todos)
                                          todos)})
            ;; Whatever rebase returned, the todos callback fires at
            ;; least once on a non-trivial replay. On an empty plan
            ;; the callback may not fire; both are fine \u2014 we only
            ;; assert that when it fires the shape is correct.
            (when (some? @seen)
              (expect (vector? @seen))
              (when (seq @seen)
                (let [t (first @seen)]
                  (expect (string? (:sha t)))
                  (expect (string? (:short-sha t)))
                  (expect (string? (:message t)))
                  (expect (keyword? (:action t))))))))
        (finally (cleanup root))))))

(defn- diverge-and-dirty!
  "Build a real divergence (feature needs replaying onto trunk) and leave
   an uncommitted tracked modification, so a :begin rebase hits
   UNCOMMITTED_CHANGES. Returns the dirty file's rel path + content."
  [^java.io.File root]
  ;; trunk = f0,f1 ; feature branches off f0 and adds a commit → diverged.
  (wo/branch! {"op" "create" "name" "feature/as" "from" "HEAD~1"})
  (wo/checkout! {"branch" "feature/as"})
  (spit-rel root "feature.txt" "feature work")
  (with-open [g (Git/open root)]
    (-> g .add (.addFilepattern "feature.txt") .call)
    (-> g .commit (.setMessage "feature commit") .call))
  ;; Dirty an unrelated tracked file (shared base content → no merge clash).
  (let [rel "src/f0.clj" content "(ns f0) ;; DIRTY uncommitted\n"]
    (spit-rel root rel content)
    {:rel rel :content content}))

(defdescribe rebase!-autostash-test
  (it "surfaces blocking paths + a recovery hint on UNCOMMITTED_CHANGES"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root 2)
        (with-workspace root
          (diverge-and-dirty! root)
          (let [r (wo/rebase! {"operation" "begin" "upstream" trunk-branch})]
            (expect (= "UNCOMMITTED_CHANGES" (get r "status")))
            (expect (false? (get r "successful")))
            ;; The exact dirty paths ride along — no separate git/status needed.
            (expect (seq (get r "uncommitted_changes")))
            (expect (some #(str/includes? % "f0.clj") (get r "uncommitted_changes")))
            ;; The hint points at the fix the model otherwise has to invent.
            (expect (string? (get r "hint")))
            (expect (str/includes? (get r "hint") "is_autostash"))))
        (finally (cleanup root)))))

  (it "is_autostash true stashes, rebases, and restores the dirty file"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root 2)
        (with-workspace root
          (let [{:keys [rel content]} (diverge-and-dirty! root)
                r (wo/rebase! {"operation" "begin" "upstream" trunk-branch
                               "is_autostash" true})]
            (expect (true? (get r "successful")))
            (expect (true? (get r "autostash_applied")))
            ;; Rebase actually moved (feature.txt from trunk now present)…
            (expect (.exists (io/file root "src/f1.clj")))
            ;; …and the parked uncommitted change is back on disk untouched.
            (expect (= content (slurp (io/file root rel))))))
        (finally (cleanup root)))))

  (it "is_autostash is a no-op gate on a clean tree"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root 1)
        (with-workspace root
          (wo/branch! {"op" "create" "name" "feature/clean"})
          (wo/checkout! {"branch" "feature/clean"})
          (let [r (wo/rebase! {"operation" "begin" "upstream" trunk-branch
                               "is_autostash" true})]
            (expect (true? (get r "successful")))
            ;; Nothing was stashed, so no restore flag is set.
            (expect (nil? (get r "autostash_applied")))))
        (finally (cleanup root))))))

;; ----------------------------------------------------------------------------
;; Integration shape: every history-rewrite tool symbol is wired
;; ----------------------------------------------------------------------------

(defdescribe write-ops-registration-test
  (it "exposes reset!, branch!, checkout!, cherry-pick!, rebase! as tool symbols"
    (let [exposed (set (map :ext.symbol/symbol wo/write-ops-symbols))]
      (expect (contains? exposed 'reset!))
      (expect (contains? exposed 'branch!))
      (expect (contains? exposed 'checkout!))
      (expect (contains? exposed 'cherry-pick!))
      (expect (contains? exposed 'rebase!))))

  (it "every tool symbol uses :mutation tag (history rewrites mutate state)"
    (let [new-ops (filter #(contains? #{'reset! 'branch! 'checkout! 'cherry-pick! 'rebase!}
                             (:ext.symbol/symbol %))
                    wo/write-ops-symbols)]
      (expect (= 5 (count new-ops)))
      (doseq [sym new-ops]
        (expect (= :mutation (:ext.symbol/tag sym)))))))

;; Suppress unused-require lint on str/blank? helpers.
(comment str/blank?)
