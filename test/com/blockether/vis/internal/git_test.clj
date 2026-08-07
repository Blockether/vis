(ns com.blockether.vis.internal.git-test
  "Argument parsing for the git tool.

   The tool itself is a thin wrapper around the `git` binary, so nothing here
   spawns a repository: shelling out to real `git` measured whatever Git does,
   not what Vis decides, and cost the suite seconds per namespace. What Vis
   actually owns is the INVOCATION — where the global options stop, which
   command was asked for, and whether a `commit` would build a tree other than
   the one that was authorized. That is pure, and it is what this pins."
  (:require [com.blockether.vis.internal.git :as git]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private parse-invocation #'git/parse-invocation)

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
