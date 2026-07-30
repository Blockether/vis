(ns com.blockether.vis.internal.git-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.git :as git]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.io File)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- make-tmp-dir
  ^File []
  (.toFile (Files/createTempDirectory "vis-internal-git-" (into-array FileAttribute []))))

(defn- spit-rel
  [^File root rel content]
  (let [f (io/file root rel)]
    (.mkdirs (.getParentFile f))
    (spit f content)))

(defn- cleanup
  [^File root]
  (when (.exists root)
    (doseq [^File f (reverse (file-seq root))]
      (.delete f))))

(defn- git!
  [^File root & args]
  ;; Shell out to the real `git` binary — the same implementation the code
  ;; under test uses, so there is no behavioural skew between the fixture and
  ;; the subject.
  (let [pb (ProcessBuilder. ^java.util.List (into ["git"] (map str) args))]
    (.directory pb root)
    (.redirectErrorStream pb true)
    (let [p (.start pb)]
      (slurp (.getInputStream p))
      (.waitFor p))))

(defn- init-repo!
  [^File root]
  (git! root "init" "-q")
  (git! root "config" "user.name" "Vis Test")
  (git! root "config" "user.email" "vis-test@example.invalid")
  (git! root "config" "commit.gpgsign" "false")
  (spit-rel root "a.txt" "a\n")
  (git! root "add" "a.txt")
  (git! root "commit" "-q" "-m" "base"))

(defn- git-out
  [^File root & args]
  (some-> (git/run-git root (vec args))
          :out
          str/trim
          not-empty))

(def ^:private parse-invocation #'git/parse-invocation)

(def ^:private index-changing-commit-arg #'git/index-changing-commit-arg)

(defdescribe file-dirty?-test
             (it "is false for clean/untracked/repo-less, true once a tracked file is modified"
                 (let [root (make-tmp-dir)]
                   (try (init-repo! root)
                        (let [a (io/file root "a.txt")]
                          (expect (false? (git/file-dirty? a))) ; clean tracked
                          (spit a "a changed\n")
                          (expect (true? (git/file-dirty? a)))  ; modified → dirty
                          (let [n (io/file root "new.txt")]
                            (spit n "fresh\n")
                            (expect (false? (git/file-dirty? n)))) ; untracked is NOT dirty
                          (expect (false? (git/file-dirty? (io/file root "missing.txt")))) ; absent
                          (expect (false? (git/file-dirty? (io/file "/nonexistent/zzz.txt"))))) ; no repo
                        (finally (cleanup root))))))

(defdescribe repository-detection-test
             (it "detects a repository by shelling out to git"
                 (let [root (make-tmp-dir)]
                   (try (expect (false? (git/in-repository? root)))
                        (expect (= :none (git/vcs-kind root)))
                        (init-repo! root)
                        (expect (true? (git/in-repository? root)))
                        (expect (= :git (git/vcs-kind root)))
                        (expect (= (.getName root) (git/repo-name root)))
                        (finally (cleanup root))))))

(defdescribe exact-commit-args-test
             (it "normalizes Git-global options before locating commit"
                 (expect (= {:args ["-C" "other" "-c" "commit.gpgsign=false" "commit" "-m" "x"]
                             :global-args ["-C" "other" "-c" "commit.gpgsign=false"]
                             :command "commit"
                             :command-args ["-m" "x"]}
                            (parse-invocation
                              ["-C" "other" "-c" "commit.gpgsign=false" "commit" "-m" "x"])))
                 (expect (= "commit"
                            (:command
                              (parse-invocation
                                ["--git-dir=/tmp/repo/.git" "commit" "--amend"])))))
             (it "allows options that preserve the staged index tree"
                 (expect (nil? (index-changing-commit-arg ["-m" "message"])))
                 (expect (nil? (index-changing-commit-arg ["-ma message"])))
                 (expect (nil? (index-changing-commit-arg ["--amend" "--no-edit"])))
                 (expect (nil? (index-changing-commit-arg ["-U" "3" "-m" "message"]))))
             (it "rejects flags and pathspecs that make Git construct another tree"
                 (expect (= "-a" (index-changing-commit-arg ["-am" "message"])))
                 (expect (= "-a" (index-changing-commit-arg ["-va" "-m" "message"])))
                 (expect (= "--pathspec-from-file"
                            (index-changing-commit-arg ["--pathspec-from-file=paths.txt"])))
                 (expect (= "--fixup=reword"
                            (index-changing-commit-arg ["--fixup" "reword:HEAD"])))
                 (expect (= "pathspec src/core.clj"
                            (index-changing-commit-arg ["src/core.clj"])))
                 (expect (= "-- <pathspec>"
                            (index-changing-commit-arg ["--" "src/core.clj"])))))

(defdescribe commit-operation-test
             (it "uses extension-owned around-hook lifecycle and fails closed"
                 (let [root (make-tmp-dir)
                       extension-name "test.git-commit-gate"
                       seen (atom nil)]
                   (try
                     (init-repo! root)
                     (spit-rel root "a.txt" "changed\n")
                     (git! root "add" "a.txt")
                     (let [head-before (git-out root "rev-parse" "HEAD")]
                       (extension/register-extension!
                         {:ext/name extension-name
                          :ext/description "test commit gate"
                          :ext/op-hooks
                          [{:op :git/commit
                            :phase :around
                            :fn (fn [context op args _next]
                                  (reset! seen [context op args])
                                  (throw (ex-info "verification required" {})))}]})
                       (let [blocked (git/commit! root ["commit" "-m" "blocked"] nil)]
                         (expect (= 1 (:exit blocked)))
                         (expect (str/includes? (:err blocked) "verification required"))
                         (expect (= head-before (git-out root "rev-parse" "HEAD")))
                         (expect (= :git/commit (second @seen)))
                         (expect (= [] (nth @seen 2)))
                         (expect (= (.getCanonicalPath root)
                                    (get-in @seen [0 :root])))
                         (expect (true? (get-in @seen [0 :index-preserving?])))
                         (expect (string? (get-in @seen [0 :candidate-tree]))))
                       (extension/deregister-extension! extension-name)
                       (expect (= 0 (:exit (git/commit! root ["commit" "-m" "allowed"] nil))))
                       (expect (= "allowed" (git-out root "log" "-1" "--format=%s"))))
                     (finally
                       (extension/deregister-extension! extension-name)
                       (cleanup root)))))
             (it "resolves -C before presenting the semantic repository to hooks"
                 (let [workspace-root (make-tmp-dir)
                       repo (io/file workspace-root "other")
                       extension-name "test.git-effective-repo"
                       seen (atom nil)]
                   (try
                     (.mkdirs repo)
                     (init-repo! repo)
                     (spit-rel repo "a.txt" "changed in other\n")
                     (git! repo "add" "a.txt")
                     (extension/register-extension!
                       {:ext/name extension-name
                        :ext/description "observe effective commit repository"
                        :ext/op-hooks
                        [{:op :git/commit
                          :phase :around
                          :fn (fn [context _op args next]
                                (reset! seen context)
                                (next args))}]})
                     (let [result
                           (git/run-command
                             workspace-root
                             ["-C" "other" "commit" "-m" "other repo"]
                             nil)]
                       (expect (= 0 (:exit result)))
                       (expect (= (.getCanonicalPath repo) (:root @seen)))
                       (expect (= (:candidate-tree @seen)
                                  (git-out repo "rev-parse" "HEAD^{tree}"))))
                     (finally
                       (extension/deregister-extension! extension-name)
                       (cleanup workspace-root)))))
             (it "rechecks T0 after authorization and refuses a changed index"
                 (let [root (make-tmp-dir)
                       extension-name "test.git-index-race"]
                   (try
                     (init-repo! root)
                     (spit-rel root "a.txt" "candidate\n")
                     (spit-rel root "late.txt" "late\n")
                     (git! root "add" "a.txt")
                     (let [head-before (git-out root "rev-parse" "HEAD")]
                       (extension/register-extension!
                         {:ext/name extension-name
                          :ext/description "mutate index during authorization"
                          :ext/op-hooks
                          [{:op :git/commit
                            :phase :around
                            :fn (fn [{:keys [root]} _op args next]
                                  (git/run-git (io/file root) ["add" "late.txt"])
                                  (next args))}]})
                       (let [result (git/commit! root ["commit" "-m" "raced"] nil)]
                         (expect (= 1 (:exit result)))
                         (expect (str/includes? (:err result) "index changed"))
                         (expect (= head-before (git-out root "rev-parse" "HEAD")))))
                     (finally
                       (extension/deregister-extension! extension-name)
                       (cleanup root)))))
             (it "detects a pre-commit hook that changes the resulting tree"
                 (let [root (make-tmp-dir)]
                   (try
                     (init-repo! root)
                     (spit-rel root "a.txt" "candidate\n")
                     (spit-rel root "late.txt" "late\n")
                     (git! root "add" "a.txt")
                     (let [hook (io/file root ".git/hooks/pre-commit")]
                       (spit hook "#!/bin/sh\ngit add late.txt\n")
                       (.setExecutable hook true)
                       (let [result (git/commit! root ["commit" "-m" "hook changed tree"] nil)]
                         (expect (= 1 (:exit result)))
                         (expect (str/includes? (:err result) "does not match"))
                         ;; Git did create the commit; the postcondition makes
                         ;; the unapproved tree visible instead of reporting it
                         ;; as an authorized success.
                         (expect (= "hook changed tree"
                                    (git-out root "log" "-1" "--format=%s")))))
                     (finally (cleanup root))))))

(defdescribe status-snapshot-test
             (it "reports branch/head and porcelain entries (modified + untracked)"
                 (let [root (make-tmp-dir)]
                   (try (init-repo! root)
                        (spit-rel root "a.txt" "a\nb\n")
                        (spit-rel root "new.txt" "new")
                        (let [{:keys [entries clean? head]} (git/status-snapshot root)]
                          (expect (false? clean?))
                          (expect (string? head))
                          (expect (= [{:status "M" :file "a.txt"} {:status "??" :file "new.txt"}]
                                     (sort-by :file entries))))
                        (finally (cleanup root))))))

(defdescribe working-tree-status-test
             (it "counts modified/created/deleted for the footer and reports the branch"
                 (let [root (make-tmp-dir)]
                   (try (init-repo! root)
                        (spit-rel root "a.txt" "a\nb\n") ; modify tracked
                        (spit-rel root "new.txt" "new")  ; create untracked
                        (let
                          [{:keys [workspace? modified created deleted branch]}
                           (git/working-tree-status root)]
                          (expect (true? workspace?))
                          (expect (= 1 modified))
                          (expect (= 1 created))
                          (expect (= 0 deleted))
                          (expect (string? branch)))
                        (finally (cleanup root)))))
             (it "returns {:workspace? false} outside any repository"
                 (let [root (make-tmp-dir)]
                   (try (expect (false? (:workspace? (git/working-tree-status root))))
                        (finally (cleanup root))))))
