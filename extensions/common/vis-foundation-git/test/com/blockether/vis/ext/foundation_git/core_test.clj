(ns com.blockether.vis.ext.foundation-git.core-test
  (:require
   [clojure.java.io :as io]
   [clojure.string]
   [com.blockether.vis.ext.foundation-git.core :as git]
   [com.blockether.vis.internal.extension :as extension]
   [lazytest.core :refer [defdescribe expect it]])
  (:import
   (java.nio.file Files)
   (java.nio.file.attribute FileAttribute)
   (org.eclipse.jgit.api Git)))

(defn- make-tmp-dir ^java.io.File []
  (.toFile (Files/createTempDirectory "vis-foundation-git-"
             (into-array FileAttribute []))))

(defn- spit-rel [^java.io.File root rel content]
  (let [f (io/file root rel)]
    (.mkdirs (.getParentFile f))
    (spit f content)))

(defn- cleanup [^java.io.File root]
  (when (.exists root)
    (doseq [^java.io.File f (reverse (file-seq root))]
      (.delete f))))

(defn- init-repo! [^java.io.File root]
  (with-open [g (-> (Git/init) (.setDirectory root) .call)]
    (let [config (.. g getRepository getConfig)]
      (.setString config "user" nil "name" "Vis Test")
      (.setString config "user" nil "email" "vis-test@example.invalid")
      (.save config))
    (spit-rel root "src/a.clj" "(ns a)\n")
    (-> g .add (.addFilepattern "src/a.clj") .call)
    (-> g .commit (.setMessage "base") .call)))

(defn- spit-binary-rel
  "Write a deliberately-binary blob (null bytes interleaved) at `rel`
   under `root`. Used by binary-detection tests so we don't have to
   ship real binary fixtures in the repo."
  [^java.io.File root rel]
  (let [f (io/file root rel)]
    (.mkdirs (.getParentFile f))
    (clojure.java.io/copy
      (byte-array (concat (range 0 100) (repeat 100 0) (range 0 56)))
      f)))

(defdescribe git-diff-test
  (it "accepts optional opts map and reads diff/status through JGit"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (spit-rel root "src/a.clj" "(ns a)\n(def x 1)\n")
        (spit-rel root "src/new.clj" "(ns new)\n")
        (let [result (git/git-diff-fn {:workspace/root (.getCanonicalPath root)} {:stat? true})
              data   (:result result)]
          (expect (extension/tool-result? result))
          (expect (= [{:file "src/a.clj" :add 1 :del 0}] (:files data)))
          ;; tracked changes ride :files ONLY — no porcelain duplication;
          ;; :untracked carries just the paths numstat can't line-count
          (expect (not (contains? data :porcelain)))
          (expect (= ["src/new.clj"] (:untracked data)))
          (expect (<= (count (:head data)) 10)))
        (finally (cleanup root)))))

  (it "rejects non-map opts"
    (try
      (git/git-diff-fn {:workspace/root "/repo"} :bad)
      (expect false)
      (catch clojure.lang.ExceptionInfo e
        (expect (= :foundation-git/invalid-opts (:type (ex-data e))))
        (expect (clojure.string/includes? (ex-message e)
                  "git_diff expected optional opts dict, got :bad")))))

  (it "accepts {:from sha :to sha} for arbitrary range diff"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (with-open [g (org.eclipse.jgit.api.Git/open root)]
          (spit-rel root "src/a.clj" "(ns a)\n(def x 1)\n")
          (-> g .add (.addFilepattern "src/a.clj") .call)
          (-> g .commit (.setMessage "add x") .call)
          (spit-rel root "src/a.clj" "(ns a)\n(def x 1)\n(def y 2)\n")
          (-> g .add (.addFilepattern "src/a.clj") .call)
          (-> g .commit (.setMessage "add y") .call))
        (let [result (git/git-diff-fn {:workspace/root (.getCanonicalPath root)}
                       {:from "HEAD~1" :to "HEAD"})
              data   (:result result)]
          (expect (= :range (:kind data)))
          (expect (= "HEAD~1" (:from data)))
          (expect (= "HEAD" (:to data)))
          (expect (= 1 (get-in data [:stat :files])))
          (expect (= 1 (get-in data [:stat :add])))
          ;; ref-to-ref mode: no working tree, so no :untracked key at all
          (expect (not (contains? data :untracked))))
        (finally (cleanup root)))))

  (it "includes per-file unified-diff text when :patch? is true"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (with-open [g (org.eclipse.jgit.api.Git/open root)]
          (spit-rel root "src/a.clj" "(ns a)\n(def x 1)\n")
          (-> g .add (.addFilepattern "src/a.clj") .call)
          (-> g .commit (.setMessage "v1") .call)
          (spit-rel root "src/a.clj" "(ns a)\n(def x 2)\n")
          (-> g .add (.addFilepattern "src/a.clj") .call)
          (-> g .commit (.setMessage "v2") .call))
        (let [data (:result (git/git-diff-fn {:workspace/root (.getCanonicalPath root)}
                              {:from "HEAD~1" :to "HEAD" :is_patch true}))
              entry (first (:files data))]
          (expect (string? (:patch entry)))
          (expect (clojure.string/includes? (:patch entry) "-(def x 1)"))
          (expect (clojure.string/includes? (:patch entry) "+(def x 2)")))
        (finally (cleanup root)))))

  (it "exposes a top-level :patch (concatenated per-file diffs) for working-tree changes"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (with-open [g (org.eclipse.jgit.api.Git/open root)]
          (spit-rel root "src/a.clj" "(ns a)\n(def x 1)\n")
          (-> g .add (.addFilepattern "src/a.clj") .call)
          (-> g .commit (.setMessage "v1") .call)
          (spit-rel root "src/a.clj" "(ns a)\n(def x 2)\n"))
        (let [dirty (:result (git/git-diff-fn {:workspace/root (.getCanonicalPath root)}
                               {:is_patch true}))]
          ;; top-level :patch is ALWAYS a string (never nil) so (.get "patch")
          ;; is slice-safe; per-file :patch carries real WT diff text too
          (expect (string? (:patch dirty)))
          (expect (clojure.string/includes? (:patch dirty) "+(def x 2)"))
          (expect (clojure.string/includes? (:patch dirty) "-(def x 1)"))
          (expect (clojure.string/includes? (:patch (first (:files dirty))) "+(def x 2)")))
        (finally (cleanup root)))))

  (it "exposes a top-level :patch that is an empty string on a clean tree"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (with-open [g (org.eclipse.jgit.api.Git/open root)]
          (spit-rel root "src/a.clj" "(ns a)\n(def x 1)\n")
          (-> g .add (.addFilepattern "src/a.clj") .call)
          (-> g .commit (.setMessage "v1") .call))
        (let [clean (:result (git/git-diff-fn {:workspace/root (.getCanonicalPath root)}
                               {:is_patch true}))]
          (expect (string? (:patch clean)))
          (expect (= "" (:patch clean))))
        (finally (cleanup root)))))

  (it "omits :branch in :range mode since the workspace's branch is misleading there"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (with-open [g (org.eclipse.jgit.api.Git/open root)]
          (spit-rel root "src/a.clj" "(ns a)\n(def x 1)\n")
          (-> g .add (.addFilepattern "src/a.clj") .call)
          (-> g .commit (.setMessage "v1") .call)
          (spit-rel root "src/a.clj" "(ns a)\n(def x 2)\n")
          (-> g .add (.addFilepattern "src/a.clj") .call)
          (-> g .commit (.setMessage "v2") .call))
        (let [range-data    (:result (git/git-diff-fn {:workspace/root (.getCanonicalPath root)}
                                       {:from "HEAD~1" :to "HEAD"}))
              default-data  (:result (git/git-diff-fn {:workspace/root (.getCanonicalPath root)}
                                       nil))]
          (expect (= :range (:kind range-data)))
          (expect (not (contains? range-data :branch)))
          ;; Default (workspace) mode still surfaces :branch when JGit
          ;; could resolve one for the bound workspace; the test repo
          ;; has no workspace row so :branch stays absent here too,
          ;; but the cond-> shape is the same.
          (expect (not= :range (:kind default-data))))
        (finally (cleanup root)))))

  (it "applies :path filter to limit a diff to one subtree"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (with-open [g (org.eclipse.jgit.api.Git/open root)]
          (spit-rel root "src/a.clj" "(ns a)\n(def x 1)\n")
          (spit-rel root "docs/x.md" "hello\n")
          (-> g .add (.addFilepattern ".") .call)
          (-> g .commit (.setMessage "add stuff") .call))
        (let [result (git/git-diff-fn {:workspace/root (.getCanonicalPath root)}
                       {:from "HEAD~1" :to "HEAD" :path "src"})
              data   (:result result)]
          (expect (= "src" (:path data)))
          (expect (every? #(clojure.string/starts-with? (:file %) "src/")
                    (:files data))))
        (finally (cleanup root)))))

  (it "rejects non-string :from/:to/:path with foundation-git/invalid-opts"
    (try
      (git/git-diff-fn {:workspace/root "/repo"} {:from 42})
      (expect false)
      (catch clojure.lang.ExceptionInfo e
        (expect (= :foundation-git/invalid-opts (:type (ex-data e))))
        (expect (clojure.string/includes? (ex-message e) "git_diff :from must be a string"))))))

(defdescribe git-status-test
  (it "returns branch, short head, and changes grouped by bucket keyword"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (spit-rel root "new.txt" "new")
        (let [result (git/git-status-fn {:workspace/root (.getCanonicalPath root)})]
          (expect (extension/tool-result? result))
          (expect (string? (get-in result [:result :branch])))
          ;; prompt-diet: head is the 10-char SHORT sha (still a valid
          ;; ref); the derivable :clean? boolean is gone — a clean tree
          ;; is simply changes == {}
          (expect (= 10 (count (get-in result [:result :head]))))
          (expect (nil? (get-in result [:result :clean?])))
          ;; grouped: the bucket keyword is a key, its files a vec stated once
          ;; (keyword buckets survive the GraalPy boundary; "??" string keys did not)
          (expect (nil? (get-in result [:result :entries])))
          (expect (= ["new.txt"] (get-in result [:result :changes :untracked])))
          ;; the before-fn prepends env, so a caller passing an opts dict
          ;; (git_status({})) arrives as a 2nd arg — status must TOLERATE and
          ;; ignore it, not throw ArityException. Both shapes agree.
          (let [with-opts (git/git-status-fn {:workspace/root (.getCanonicalPath root)} {})]
            (expect (extension/tool-result? with-opts))
            (expect (= (:result result) (:result with-opts)))))
        (finally (cleanup root))))))

(defdescribe git-status-filesystem-roots-test
  (it "reports DIRTY filesystem roots that live in a separate repository"
    (let [primary (make-tmp-dir)
          extra   (make-tmp-dir)]
      (try
        (init-repo! primary)
        (init-repo! extra)
        (spit-rel extra "untracked.txt" "x")
        (let [result (git/git-status-fn
                       {:workspace/root (.getCanonicalPath primary)
                        :workspace/filesystem-roots
                        [{:trunk (.getCanonicalPath extra)
                          :clone (.getCanonicalPath extra)}]})
              data   (:result result)]
          (expect (extension/tool-result? result))
          ;; primary repo is clean (committed base)
          (expect (= {} (:changes data)))
          ;; the dirty separate-repo filesystem root surfaces under :context-repos
          (expect (seq (:context-repos data)))
          (let [ctx (first (:context-repos data))]
            (expect (= (.getCanonicalPath extra) (:root ctx)))
            (expect (= ["untracked.txt"] (get-in ctx [:changes :untracked])))))
        (finally
          (cleanup primary)
          (cleanup extra)))))

  (it "omits clean separate repos and roots sharing the primary repo"
    (let [primary (make-tmp-dir)
          extra   (make-tmp-dir)]
      (try
        (init-repo! primary)
        (init-repo! extra) ;; separate repo but clean
        ;; one root is the clean separate repo, one is a SUBDIR of primary
        (let [result (git/git-status-fn
                       {:workspace/root (.getCanonicalPath primary)
                        :workspace/filesystem-roots
                        [{:trunk (.getCanonicalPath extra)
                          :clone (.getCanonicalPath extra)}
                         {:trunk (.getCanonicalPath primary)
                          :clone (.getCanonicalPath primary)}]})
              data (:result result)]
          ;; no separate DIRTY repo -> :context-repos absent
          (expect (nil? (:context-repos data))))
        (finally
          (cleanup primary)
          (cleanup extra))))))

(defdescribe git-log-test
  (it "returns recent commits from JGit"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (let [result (git/git-log-fn {:workspace/root (.getCanonicalPath root)} 1)
              commit (first (get-in result [:result :commits]))]
          (expect (extension/tool-result? result))
          (expect (= 1 (count (get-in result [:result :commits]))))
          (expect (= "base" (:subject commit)))
          (expect (= "Vis Test" (:author commit)))
          (expect (string? (:sha commit)))
          (expect (integer? (:at commit))))
        (finally (cleanup root)))))

  (it "accepts {:limit N} map form so model intuition does not blow up"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (let [r-map (git/git-log-fn {:workspace/root (.getCanonicalPath root)} {:limit 1})
              r-n   (git/git-log-fn {:workspace/root (.getCanonicalPath root)} {:n 1})
              r-nil (git/git-log-fn {:workspace/root (.getCanonicalPath root)} nil)]
          (expect (= 1 (count (get-in r-map [:result :commits]))))
          (expect (= 1 (count (get-in r-n   [:result :commits]))))
          (expect (extension/tool-result? r-nil)))
        (finally (cleanup root)))))

  (it "caps very long commit bodies so a single log call cannot blow context"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (let [huge-body (apply str (repeat 6000 "x"))]
          (spit-rel root "src/note.txt" "hi\n")
          (with-open [g (org.eclipse.jgit.api.Git/open root)]
            (-> g .add (.addFilepattern ".") .call)
            (-> g .commit (.setMessage (str "big commit\n\n" huge-body)) .call)))
        (let [commit (-> (git/git-log-fn {:workspace/root (.getCanonicalPath root)} {:limit 1 :is_body true})
                       :result :commits first)]
          (expect (< (count (:body commit)) 5000))
          (expect (clojure.string/includes? (:body commit) "body truncated")))
        (finally (cleanup root)))))

  (it "subject_only returns only short_sha + subject; is_body opts the body in"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (let [env  {:workspace/root (.getCanonicalPath root)}
              so   (-> (git/git-log-fn env {:limit 1 :subject_only true})
                     :result :commits first)
              def  (-> (git/git-log-fn env 1) :result :commits first)
              body (-> (git/git-log-fn env {:limit 1 :is_body true})
                     :result :commits first)]
          ;; subject_only: exactly short_sha + subject, nothing else (snake
          ;; keys: the Clojure spelling now matches the Python-visible one)
          (expect (= #{:short_sha :subject} (set (keys so))))
          ;; default: body is dropped (opt-in only)
          (expect (not (contains? def :body)))
          (expect (string? (:subject def)))
          ;; is_body: body present
          (expect (string? (:body body))))
        (finally (cleanup root)))))

  (it "rejects garbage arg with foundation-git/invalid-opts and a usage hint"
    (try
      (git/git-log-fn {:workspace/root "/repo"} :bad)
      (expect false)
      (catch clojure.lang.ExceptionInfo e
        (expect (= :foundation-git/invalid-opts (:type (ex-data e))))
        (expect (clojure.string/includes? (ex-message e) "git_log expected"))
        (expect (clojure.string/includes? (ex-message e) "git_log({\"limit\": 50})")))))

  (it "keeps always-present fields and OMITS derivable ones on a normal commit"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (let [result (git/git-log-fn {:workspace/root (.getCanonicalPath root)} {:limit 1 :is_body true})
              commit (first (get-in result [:result :commits]))]
          ;; ALWAYS present: sha, short_sha, author, email, at, subject
          (expect (string? (:sha commit)))
          (expect (= 7 (count (:short_sha commit))))
          (expect (= "Vis Test" (:author commit)))
          (expect (= "vis-test@example.invalid" (:email commit)))
          (expect (= "base" (:subject commit)))
          ;; :at is millis (Java units), not POSIX seconds. A commit-time of
          ;; 2026-05-01 lands ~1.7e12; seconds would have been ~1.7e9. Lock
          ;; the unit so a future regression to seconds is caught.
          (expect (> (:at commit) 1000000000000))
          ;; body kept ONLY when non-blank — "base" is non-blank, so present
          (expect (= "base" (:body commit)))
          ;; committer == author / committed-at == at on a normal commit ->
          ;; those keys are OMITTED entirely (not repeated per row)
          (expect (not (contains? commit :committer)))
          (expect (not (contains? commit :committer_email)))
          (expect (not (contains? commit :committed-at)))
          ;; single-parent (here: root, 0 parents) -> :parents omitted
          (expect (not (contains? commit :parents))))
        (finally (cleanup root)))))

  (it "filters commits by :author substring (case-insensitive name OR email)"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (let [match (git/git-log-fn {:workspace/root (.getCanonicalPath root)}
                      {:author "VIS" :limit 5})
              miss  (git/git-log-fn {:workspace/root (.getCanonicalPath root)}
                      {:author "nobody" :limit 5})]
          (expect (pos? (count (get-in match [:result :commits]))))
          (expect (zero? (count (get-in miss  [:result :commits])))))
        (finally (cleanup root)))))

  (it "filters commits by :since/:until ISO date strings"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (let [now    (System/currentTimeMillis)
              tmrw   (+ now (* 24 60 60 1000))
              future (git/git-log-fn {:workspace/root (.getCanonicalPath root)}
                       {:since tmrw :limit 5})
              all    (git/git-log-fn {:workspace/root (.getCanonicalPath root)}
                       {:since 0 :limit 5})]
          (expect (zero? (count (get-in future [:result :commits]))))
          (expect (pos? (count (get-in all [:result :commits])))))
        (finally (cleanup root)))))

  (it "throws :foundation-git/invalid-date for unparseable date strings"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (try
          (git/git-log-fn {:workspace/root (.getCanonicalPath root)}
            {:since "not-a-date"})
          (expect false)
          (catch clojure.lang.ExceptionInfo e
            (expect (= :foundation-git/invalid-date (:type (ex-data e))))))
        (finally (cleanup root)))))

  (it "restricts log to commits touching a path when :path is provided"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (spit-rel root "src/b.clj" "(ns b)\n")
        (with-open [g (org.eclipse.jgit.api.Git/open root)]
          (-> g .add (.addFilepattern "src/b.clj") .call)
          (-> g .commit (.setMessage "add b") .call))
        (let [all  (git/git-log-fn {:workspace/root (.getCanonicalPath root)} 10)
              only-a (git/git-log-fn {:workspace/root (.getCanonicalPath root)}
                       {:path "src/a.clj" :limit 10})]
          (expect (= 2 (count (get-in all [:result :commits]))))
          (expect (= 1 (count (get-in only-a [:result :commits]))))
          (expect (= "base" (:subject (first (get-in only-a [:result :commits]))))))
        (finally (cleanup root))))))

(defdescribe git-show-test
  (it "returns commit detail with per-file numstat against the parent"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (spit-rel root "src/a.clj" "(ns a)\n(def x 1)\n")
        (with-open [g (org.eclipse.jgit.api.Git/open root)]
          (-> g .add (.addFilepattern "src/a.clj") .call)
          (-> g .commit (.setMessage "add x") .call))
        (let [result (git/git-show-fn {:workspace/root (.getCanonicalPath root)} "HEAD")
              data   (:result result)]
          (expect (extension/tool-result? result))
          (expect (= "add x" (:subject data)))
          (expect (= 1 (get-in data [:stat :files])))
          (expect (= 1 (get-in data [:stat :add])))
          (expect (= [{:file "src/a.clj" :add 1 :del 0}] (:files data)))
          (expect (= 1 (count (:parents data)))))
        (finally (cleanup root)))))

  (it "handles root commit (no parent) by diffing against the empty tree"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (let [result (git/git-show-fn {:workspace/root (.getCanonicalPath root)} "HEAD")
              data   (:result result)]
          (expect (= "base" (:subject data)))
          (expect (= [] (:parents data)))
          ;; Root commit numstat is reported against the empty tree; the
          ;; single committed file shows up as one row of additions.
          (expect (= 1 (get-in data [:stat :files]))))
        (finally (cleanup root)))))

  (it "includes per-file unified-diff text when :patch? is true"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (spit-rel root "src/a.clj" "(ns a)\n(def x 1)\n")
        (with-open [g (org.eclipse.jgit.api.Git/open root)]
          (-> g .add (.addFilepattern "src/a.clj") .call)
          (-> g .commit (.setMessage "add x") .call))
        (let [no-patch (:result (git/git-show-fn {:workspace/root (.getCanonicalPath root)}
                                  {:rev "HEAD"}))
              with-patch (:result (git/git-show-fn {:workspace/root (.getCanonicalPath root)}
                                    {:rev "HEAD" :is_patch true}))
              entry (first (:files with-patch))]
          (expect (not (contains? (first (:files no-patch)) :patch)))
          (expect (string? (:patch entry)))
          (expect (clojure.string/includes? (:patch entry) "+++ b/src/a.clj"))
          (expect (clojure.string/includes? (:patch entry) "+(def x 1)")))
        (finally (cleanup root)))))

  (it "rejects empty or bogus rev with a clean ex-info"
    (try
      (git/git-show-fn {:workspace/root "/no/repo"} "")
      (expect false)
      (catch clojure.lang.ExceptionInfo e
        (expect (= :foundation-git/invalid-opts (:type (ex-data e))))))
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (try (git/git-show-fn {:workspace/root (.getCanonicalPath root)} "deadbeef")
          (expect false)
          (catch clojure.lang.ExceptionInfo e
            (expect (= :foundation-git/unknown-rev (:type (ex-data e))))))
        (finally (cleanup root))))))

(defdescribe binary-detection-test
  (it "git/show flags binary file entries with :binary? true and +0 -0"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (spit-binary-rel root "data.bin")
        (spit-rel root "docs/x.md" "hello\n")
        (with-open [g (org.eclipse.jgit.api.Git/open root)]
          (-> g .add (.addFilepattern ".") .call)
          (-> g .commit (.setMessage "add mixed") .call))
        (let [data (:result (git/git-show-fn {:workspace/root (.getCanonicalPath root)}
                              {:rev "HEAD"}))
              by-file (into {} (map (juxt :file identity)) (:files data))]
          (expect (true? (:binary? (get by-file "data.bin"))))
          (expect (= 0 (:add (get by-file "data.bin"))))
          (expect (nil? (:binary? (get by-file "docs/x.md"))))
          (expect (= 1 (:add (get by-file "docs/x.md")))))
        (finally (cleanup root)))))

  (it "git/show :patch? returns 'Binary files differ' instead of UTF-8 garbage"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (spit-binary-rel root "data.bin")
        (with-open [g (org.eclipse.jgit.api.Git/open root)]
          (-> g .add (.addFilepattern ".") .call)
          (-> g .commit (.setMessage "add bin") .call))
        (let [data (:result (git/git-show-fn {:workspace/root (.getCanonicalPath root)}
                              {:rev "HEAD" :is_patch true}))
              entry (first (filter #(= "data.bin" (:file %)) (:files data)))]
          ;; Root-commit path doesn't go through entry-patch (no parent
          ;; tree to diff against). The :patch key is only populated for
          ;; non-root commits, so we assert the numstat flag here and
          ;; cover entry-patch's binary handling in the diff test below.
          (expect (true? (:binary? entry))))
        (finally (cleanup root)))))

  (it "git/diff :patch? returns binary marker line for binary entries"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (spit-binary-rel root "data.bin")
        (with-open [g (org.eclipse.jgit.api.Git/open root)]
          (-> g .add (.addFilepattern ".") .call)
          (-> g .commit (.setMessage "v1") .call))
        (let [f (io/file root "data.bin")]
          (clojure.java.io/copy
            (byte-array (concat (range 0 80) (repeat 80 0) (range 0 96)))
            f))
        (with-open [g (org.eclipse.jgit.api.Git/open root)]
          (-> g .add (.addFilepattern ".") .call)
          (-> g .commit (.setMessage "v2") .call))
        (let [data (:result (git/git-diff-fn {:workspace/root (.getCanonicalPath root)}
                              {:from "HEAD~1" :to "HEAD" :is_patch true}))
              entry (first (:files data))]
          (expect (true? (:binary? entry)))
          (expect (= 0 (:add entry)))
          (expect (= 0 (:del entry)))
          (expect (clojure.string/includes? (:patch entry) "Binary files"))
          (expect (clojure.string/includes? (:patch entry) "differ")))
        (finally (cleanup root)))))

  (it "git/blame refuses binary blobs with :foundation-git/binary"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (spit-binary-rel root "data.bin")
        (with-open [g (org.eclipse.jgit.api.Git/open root)]
          (-> g .add (.addFilepattern ".") .call)
          (-> g .commit (.setMessage "add bin") .call))
        (try
          (git/git-blame-fn {:workspace/root (.getCanonicalPath root)}
            {:path "data.bin"})
          (expect false)
          (catch clojure.lang.ExceptionInfo e
            (expect (= :foundation-git/binary (:type (ex-data e))))
            (expect (clojure.string/includes? (ex-message e) "binary blob"))
            (expect (clojure.string/includes? (ex-message e) "git_log"))))
        (finally (cleanup root))))))

(defdescribe git-blame-test
  (it "returns per-line blame with sha, author, content for the whole file"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (let [result (git/git-blame-fn {:workspace/root (.getCanonicalPath root)} "src/a.clj")
              data   (:result result)]
          (expect (extension/tool-result? result))
          (expect (= "src/a.clj" (:path data)))
          (expect (= 1 (:total data)))
          (expect (= 1 (count (:lines data))))
          (let [line (first (:lines data))]
            (expect (= 1 (:line line)))
            (expect (= "(ns a)" (:content line)))
            ;; :sha is now the SHORT sha (the commit-legend key)
            (expect (string? (:sha line)))
            (expect (= 7 (count (:sha line))))
            ;; per-line author/email/at moved into the legend, stated once
            (expect (nil? (:author line)))
            (let [commit (get (:commits data) (:sha line))]
              (expect (= "Vis Test" (:author commit)))
              (expect (string? (:sha commit)))
              (expect (= 40 (count (:sha commit)))))))
        (finally (cleanup root)))))

  (it "caps default blame at 1000 lines and flags :truncated? when more exist"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        ;; 2000 lines so we go past the 1000-line default cap.
        (spit-rel root "big.txt"
          (clojure.string/join "\n" (map str (range 1 2001))))
        (with-open [g (org.eclipse.jgit.api.Git/open root)]
          (-> g .add (.addFilepattern ".") .call)
          (-> g .commit (.setMessage "add big") .call))
        (let [no-range (:result (git/git-blame-fn {:workspace/root (.getCanonicalPath root)}
                                  "big.txt"))
              explicit (:result (git/git-blame-fn {:workspace/root (.getCanonicalPath root)}
                                  {:path "big.txt" :from 1 :to 1500}))]
          (expect (= 2000 (:total no-range)))
          (expect (true? (:truncated? no-range)))
          (expect (= 1000 (count (:lines no-range))))
          (expect (= 2000 (:total explicit)))
          ;; Explicit :from/:to bypasses the safety cap; honour exactly
          ;; what the caller asked for.
          (expect (false? (boolean (:truncated? explicit))))
          (expect (= 1500 (count (:lines explicit)))))
        (finally (cleanup root)))))

  (it "honours :from/:to range so blame on huge files is cheap to summarise"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (spit-rel root "big.txt" (clojure.string/join "\n" (map str (range 1 21))))
        (with-open [g (org.eclipse.jgit.api.Git/open root)]
          (-> g .add (.addFilepattern "big.txt") .call)
          (-> g .commit (.setMessage "add big") .call))
        (let [data (:result (git/git-blame-fn {:workspace/root (.getCanonicalPath root)}
                              {:path "big.txt" :from 5 :to 8}))]
          (expect (= [5 6 7 8] (mapv :line (:lines data))))
          (expect (= ["5" "6" "7" "8"] (mapv :content (:lines data)))))
        (finally (cleanup root)))))

  (it "peels lines past :ignore-revs to surface the underlying author"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        ;; Layer 1: real authorship commit. The repo's `init-repo!` already
        ;; committed src/a.clj as \"(ns a)\n\" with Vis Test as author. We
        ;; need a second author for the \"noisy\" layer so the test can
        ;; tell the two apart after peeling.
        (with-open [g (org.eclipse.jgit.api.Git/open root)]
          (let [config (.. g getRepository getConfig)]
            (.setString config "user" nil "name" "Noisy Bot")
            (.setString config "user" nil "email" "noise@example.invalid")
            (.save config))
          ;; Layer 2: whitespace-only reformat that actually touches L1
          ;; (extra space between `ns` and `a`). JGit attributes L1 to
          ;; this commit because the line bytes changed.
          (spit-rel root "src/a.clj" "(ns  a)\n")
          (-> g .add (.addFilepattern "src/a.clj") .call)
          (-> g .commit (.setMessage "reformat") .call))
        (let [noisy-sha (-> (git/git-log-fn {:workspace/root (.getCanonicalPath root)} 1)
                          :result :commits first :sha)
              ;; Without ignore: line 1 attributed to noisy refactor commit.
              before (:result (git/git-blame-fn {:workspace/root (.getCanonicalPath root)}
                                {:path "src/a.clj"}))
              ;; With ignore: peel past noisy commit, surface Vis Test.
              after  (:result (git/git-blame-fn {:workspace/root (.getCanonicalPath root)}
                                {:path "src/a.clj" :ignore_revs [noisy-sha]}))
              before-line (first (:lines before))
              after-line  (first (:lines after))
              before-author (get-in before [:commits (:sha before-line) :author])
              after-author  (get-in after  [:commits (:sha after-line)  :author])]
          (expect (= "Noisy Bot" before-author))
          (expect (= [noisy-sha] (:ignored-revs after)))
          (expect (= "Vis Test" after-author))
          (expect (not= (:sha before-line) (:sha after-line))))
        (finally (cleanup root)))))

  (it "rejects :ignore-revs that is not a sequence"
    (try
      (git/git-blame-fn {:workspace/root "/repo"}
        {:path "src/foo.clj" :ignore_revs "abc"})
      (expect false)
      (catch clojure.lang.ExceptionInfo e
        (expect (= :foundation-git/invalid-opts (:type (ex-data e))))
        (expect (clojure.string/includes? (ex-message e) "ignore_revs")))))

  (it "rejects bad arg shapes with foundation-git/invalid-opts"
    (try
      (git/git-blame-fn {:workspace/root "/repo"} 42)
      (expect false)
      (catch clojure.lang.ExceptionInfo e
        (expect (= :foundation-git/invalid-opts (:type (ex-data e))))))
    (try
      (git/git-blame-fn {:workspace/root "/repo"} {})
      (expect false)
      (catch clojure.lang.ExceptionInfo e
        (expect (= :foundation-git/invalid-opts (:type (ex-data e))))))))
