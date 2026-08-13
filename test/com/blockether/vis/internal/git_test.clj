(ns com.blockether.vis.internal.git-test
  "Invocation contracts for the shared Git helpers.

   These are a thin wrapper around the `git` binary, so nothing here builds a
   repository: shelling out to real `git` measured whatever Git does, not what
   Vis decides, and cost the suite seconds per namespace. What Vis actually owns
   is the INVOCATION — where the global options stop, which command was asked
   for, whether a `commit` would build a tree other than the one that was
   authorized, which locks a read is allowed to take, and whether the child is
   reaped. The one spawn below needs no repository state."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.git :as git]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private parse-invocation #'git/parse-invocation)

(def ^:private git-argv #'git/git-argv)

(def ^:private index-changing-commit-arg #'git/index-changing-commit-arg)


(defdescribe
  exact-commit-args-test
  (it "normalizes Git-global options before locating commit"
      (expect (= {:args ["-C" "other" "-c" "commit.gpgsign=false" "commit" "-m" "x"]
                  :global-args ["-C" "other" "-c" "commit.gpgsign=false"]
                  :command "commit"
                  :command-args ["-m" "x"]}
                 (parse-invocation ["-C" "other" "-c" "commit.gpgsign=false" "commit" "-m" "x"])))
      (expect (= "commit"
                 (:command (parse-invocation ["--git-dir=/tmp/repo/.git" "commit" "--amend"])))))
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
      (expect (= "--fixup=reword" (index-changing-commit-arg ["--fixup" "reword:HEAD"])))
      (expect (= "pathspec src/core.clj" (index-changing-commit-arg ["src/core.clj"])))
      (expect (= "-- <pathspec>" (index-changing-commit-arg ["--" "src/core.clj"])))))

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
                 (let
                   [before
                    (git-descendants)

                    result
                    (promise)

                    worker
                    (doto (Thread. (fn []
                                     (.interrupt (Thread/currentThread))
                                     (deliver result
                                              (git/run-git (io/file (System/getProperty "user.dir"))
                                                           ["-c" "alias.snooze=!sleep 20"
                                                            "snooze"]))))
                      (.start))]

                   (.join worker 10000)
                   (expect (nil? (:exit (deref result 5000 {:exit :never-returned}))))
                   (expect (settles? #(= before (git-descendants)))))))
