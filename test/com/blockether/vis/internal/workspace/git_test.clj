(ns com.blockether.vis.internal.workspace.git-test
  "Invocation contracts for the shared Git helpers.

   These are a thin wrapper around the `git` binary, so nothing here builds a
   repository: shelling out to real `git` measured whatever Git does, not what
   Vis decides, and cost the suite seconds per namespace. What Vis actually owns
   is the INVOCATION — where the global options stop, which command was asked
   for, which locks a read is allowed to take, and whether the child is
   reaped. The one spawn below needs no repository state."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.workspace.git :as git]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private git-argv #'git/git-argv)

(defn- git-descendants
  "Live `git` processes descended from THIS JVM — the only ones a test may claim."
  []
  (->> (.toList (.descendants (java.lang.ProcessHandle/current)))
       (keep (fn [^java.lang.ProcessHandle h]
               (let [c (.command (.info h))]
                 (when (.isPresent c) (.get c)))))
       (filter #(str/ends-with? % "git"))
       count))

(defn- settles?
  "True when `pred` holds within ~5s: a SIGTERMed child exits asynchronously."
  [pred]
  (loop [n 0]
    (cond (pred) true
          (>= n 100) false
          :else (do (Thread/sleep 50) (recur (inc n))))))

(defdescribe subprocess-test
             ;; Regression: Vis read git as a plain `git status`, which REFRESHES the index
             ;; and so takes `.git/index.lock`. The footer polled it every 4s per root, so
             ;; the user's own add/commit/checkout failed with
             ;; "Unable to create '.git/index.lock': File exists".
             (it "never lets a Vis read take the index lock"
                 (expect (= ["git" "--no-optional-locks" "status" "--porcelain=v2" "--branch" "-z"]
                            (git-argv ["status" "--porcelain=v2" "--branch" "-z"])))
                 (expect (= ["git" "--no-optional-locks" "-C" "other" "rev-parse" "HEAD"]
                            (git-argv ["-C" "other" "rev-parse" "HEAD"]))))
             ;; Regression: a cancelled turn re-arms the calling thread's interrupt flag, so
             ;; `.waitFor` threw at once and `run-git` returned while its `git` child kept
             ;; running — one orphan per call, each able to hold the index lock.
             (it "reaps its child when the calling thread is already interrupted"
                 (let [before
                       (git-descendants)

                       result
                       (promise)

                       worker
                       (doto (Thread. (fn []
                                        (.interrupt (Thread/currentThread))
                                        (deliver result
                                                 (git/run-git
                                                   (io/file (System/getProperty "user.dir"))
                                                   ["-c" "alias.snooze=!sleep 20" "snooze"]))))
                         (.start))]

                   (.join worker 10000)
                   (expect (nil? (:exit (deref result 5000 {:exit :never-returned}))))
                   (expect (settles? #(= before (git-descendants)))))))

(defn- temp-dir
  "A fresh empty directory outside any repository."
  ^java.io.File []
  (.toFile (java.nio.file.Files/createTempDirectory
             "vis-git-test"
             (make-array java.nio.file.attribute.FileAttribute 0))))

(defdescribe repo-work-tree-test
             ;; Perf pass 2026-11: each context render spawned `git rev-parse --show-toplevel`
             ;; twice for the same session root.
             (it "asks git once for a repository whose .git entry stays the same"
                 (let [root
                       (doto (temp-dir)
                         (-> (io/file ".git")
                             .mkdir))

                       runs
                       (atom 0)]

                   (with-redefs [git/git-ok (fn [_ _]
                                              (swap! runs inc)
                                              (str (.getPath root) "\n"))]
                     (expect (= (.getCanonicalFile root) (git/repo-work-tree root)))
                     (expect (= (.getCanonicalFile root) (git/repo-work-tree root))))
                   (expect (= 1 @runs))))
             (it "runs no git outside a repository and sees one created later"
                 (let [root
                       (temp-dir)

                       runs
                       (atom 0)]

                   (with-redefs [git/git-ok (fn [_ _]
                                              (swap! runs inc)
                                              (str (.getPath root) "\n"))]
                     (expect (nil? (git/repo-work-tree root)))
                     (expect (= 0 @runs))
                     (.mkdir (io/file root ".git"))
                     (expect (= (.getCanonicalFile root) (git/repo-work-tree root))))
                   (expect (= 1 @runs)))))
