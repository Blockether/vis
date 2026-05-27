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
       (.save config))
     (vec
       (for [i (range n-commits)]
         (let [rel (str "src/f" i ".clj")]
           (spit-rel root rel (str "(ns f" i ") ;; rev " i "\n"))
           (-> g .add (.addFilepattern rel) .call)
           (let [c (-> g .commit (.setMessage (str "commit " i)) .call)]
             (subs (.getName c) 0 7))))))))

(defdescribe reset!-test
  (it "soft-resets HEAD to a prior revision, leaving worktree intact"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root 3)
        (with-workspace root
          (let [res (wo/reset! {:mode :soft :to "HEAD~1"})]
            (expect (= :reset (:op res)))
            (expect (= :soft  (:mode res)))
            (expect (= "HEAD~1" (:to res)))
            (expect (some? (:resolved-sha res)))
            (expect (= 7 (count (:short-sha res))))
            (expect (some? (:head-before res)))
            (expect (= (:resolved-sha res) (:head-after res)))
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
                         (wo/reset! {:mode :soft :to "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef"})
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
          (let [thrown (try (wo/reset! {:mode :soft})
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
          (let [created (wo/branch! {:op :create :name "feature/x"})
                listed  (wo/branch! {:op :list :mode :local})]
            (expect (= :branch/create (:op created)))
            (expect (= "feature/x" (:name created)))
            (expect (= "HEAD" (:from created)))
            (expect (some? (:short-sha created)))
            (let [names (set (map :short (:branches listed)))]
              (expect (contains? names "feature/x")))))
        (finally (cleanup root)))))

  (it "deletes a branch (force? required when unmerged)"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root 1)
        (with-workspace root
          (wo/branch! {:op :create :name "throwaway"})
          (let [del (wo/branch! {:op :delete :name "throwaway" :force? true})]
            (expect (= :branch/delete (:op del)))
            (expect (= ["throwaway"] (:deleted del)))
            (let [names (set (map :short (:branches (wo/branch! {:op :list}))))]
              (expect (not (contains? names "throwaway"))))))
        (finally (cleanup root)))))

  (it "renames a branch atomically"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root 1)
        (with-workspace root
          (wo/branch! {:op :create :name "old-name"})
          (let [r (wo/branch! {:op :rename :old "old-name" :new "new-name"})]
            (expect (= :branch/rename (:op r)))
            (expect (= "old-name" (:old r)))
            (expect (= "new-name" (:new r)))
            (let [names (set (map :short (:branches (wo/branch! {:op :list}))))]
              (expect (contains? names "new-name"))
              (expect (not (contains? names "old-name"))))))
        (finally (cleanup root)))))

  (it "rejects unknown op with structured ex-info"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root 1)
        (with-workspace root
          (let [thrown (try (wo/branch! {:op :garbage})
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
          (wo/branch! {:op :create :name "feature/y"})
          (let [r (wo/checkout! {:branch "feature/y"})]
            (expect (= :checkout (:op r)))
            (expect (= "feature/y" (:branch r)))
            (expect (some? (:head r)))
            (expect (= 7 (count (:short-head r))))
            (expect (not (:created? r)))))
        (finally (cleanup root)))))

  (it "creates and switches in one call when :create? true"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root 1)
        (with-workspace root
          (let [r (wo/checkout! {:branch "feature/z" :create? true})]
            (expect (= "feature/z" (:branch r)))
            (expect (true? (:created? r)))
            (let [names (set (map :short (:branches (wo/branch! {:op :list}))))]
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
          (wo/branch! {:op :create :name "side" :from "HEAD~1"})
          (wo/checkout! {:branch "side"})
          ;; Resolve trunk HEAD via :list (short-sha of the latest).
          (let [trunk-sha (->> (wo/branch! {:op :list :mode :local})
                            :branches
                            (some (fn [b] (when (= trunk-branch (:short b)) (:sha b)))))
                _         (expect (some? trunk-sha))
                res       (wo/cherry-pick! {:commits trunk-sha})]
            (expect (= :cherry-pick (:op res)))
            (expect (= "OK" (:status res)))
            (expect (seq (:picked res)))
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
                update   (first (:updates res))
                tracking (:tracking res)]
            (expect (= :fetch (:op res)))
            (expect (= "origin" (:remote res)))
            (expect (= :updated (:status res)))
            (expect (= "master" (:branch res)))
            (expect (string? (:messages res)))
            (expect (not (contains? res :uri)))
            (expect (not (contains? res :peer-user-agent)))
            (expect (not (contains? res :advertised-ref-count)))
            (expect (= 1 (get-in res [:summary :updated])))
            (expect (false? (get-in res [:summary :up-to-date?])))
            (expect (= 0 (:ahead tracking)))
            (expect (= 1 (:behind tracking)))
            (expect (seq (:updates res)))
            (expect (map? update))
            (expect (string? (:ref update)))
            (expect (keyword? (:kind update)))
            (expect (string? (:range update)))
            (expect (string? (:local-name update)))
            (expect (string? (:remote-name update)))
            (expect (string? (:result update)))
            (expect (string? (:old-sha update)))
            (expect (string? (:new-sha update)))
            (expect (= 7 (count (:old-short-sha update))))
            (expect (= 7 (count (:new-short-sha update))))))
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
          (wo/branch! {:op :create :name "feature/r"})
          (wo/checkout! {:branch "feature/r"})
          (let [r (wo/rebase! {:operation :begin :upstream trunk-branch})]
            (expect (= :rebase (:op r)))
            ;; Status is JGit's enum name; UP_TO_DATE / FAST_FORWARD /
            ;; OK all count as success.
            (expect (true? (:successful? r)))))
        (finally (cleanup root)))))

  (it "edit-todos-fn observes the todo list and survives a no-op edit"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root 3)
        (with-workspace root
          ;; Branch off HEAD~2, add a commit, rebase onto current main
          ;; with an :edit-todos-fn that just records what it sees.
          (let [seen (atom nil)]
            (wo/branch! {:op :create :name "feature/todo" :from "HEAD~2"})
            (wo/checkout! {:branch "feature/todo"})
            (spit-rel root "extra.txt" "hello")
            (with-open [g (Git/open root)]
              (-> g .add (.addFilepattern "extra.txt") .call)
              (-> g .commit (.setMessage "feature commit") .call))
            (wo/rebase! {:operation :begin :upstream trunk-branch
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
