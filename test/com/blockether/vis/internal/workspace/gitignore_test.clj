(ns com.blockether.vis.internal.gitignore-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.gitignore :as gi]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- rules
  "Compile config-style patterns into a matcher."
  [& patterns]
  (gi/compile-rules patterns))

(defn- ignored? [matcher rel] (gi/ignored? matcher rel false))

(defn- dir-ignored? [matcher rel] (gi/ignored? matcher rel true))

(defdescribe compile-rules-test
             (it "drops blank and comment lines and returns nil when nothing compiles"
                 (expect (nil? (gi/compile-rules [])))
                 (expect (nil? (gi/compile-rules ["" "   " "# just a comment"])))
                 (expect (= 2 (count (gi/compile-rules ["# c" "" "a" "b/"])))))
             (it "ignores trailing whitespace, the way git does"
                 (expect (ignored? (rules "target   ") "target"))))

(defdescribe ignored?-test
             (it "is false for a nil matcher or an empty path"
                 (expect (not (gi/ignored? nil "anything" false)))
                 (expect (not (ignored? (rules "*") ""))))
             (it "matches a slash-free pattern at ANY depth"
                 ;; Implemented by testing each `/`-aligned suffix rather than an
                 ;; `(?:^|.*/)` regex prefix, which backtracked catastrophically (ReDoS).
                 (let [m (rules "*.log")]
                   (expect (ignored? m "a.log"))
                   (expect (ignored? m "deep/nested/a.log"))
                   (expect (not (ignored? m "a.log.keep")))))
             (it "anchors a pattern that contains a slash to the root"
                 (let [m (rules "/root-only.txt")]
                   (expect (ignored? m "root-only.txt"))
                   (expect (not (ignored? m "sub/root-only.txt"))))
                 (let [m (rules "a/b.txt")]
                   (expect (ignored? m "a/b.txt"))
                   (expect (not (ignored? m "z/a/b.txt")))))
             (it "restricts a trailing-slash rule to directories and their children"
                 (let [m (rules "build/")]
                   (expect (dir-ignored? m "build"))
                   (expect (ignored? m "build/x.o"))
                   (expect (dir-ignored? m "deep/build"))
                   ;; a FILE named `build` is not a directory, so the rule must not match
                   (expect (not (ignored? m "build")))))
             (it "ignores the children of a plain (non-directory) rule too"
                 (let [m (rules "node_modules")]
                   (expect (ignored? m "node_modules/pkg/index.js"))))
             (it "lets the last matching rule win, so `!` re-includes"
                 (let [m (rules "*.log" "!keep.log")]
                   (expect (ignored? m "a.log"))
                   (expect (not (ignored? m "keep.log"))))
                 ;; order matters: a negation BEFORE the ignore does not survive it
                 (let [m (rules "!keep.log" "*.log")]
                   (expect (ignored? m "keep.log")))))

(defdescribe pattern-syntax-test
             (it "keeps `*` inside one path segment and `**` across segments"
                 (expect (not (ignored? (rules "a/*.txt") "a/b/c.txt")))
                 (expect (ignored? (rules "a/**/c.txt") "a/b/x/c.txt")))
             (it "treats a leading `**/` as zero or more leading directories"
                 (let [m (rules "docs/**/*.md")]
                   (expect (ignored? m "docs/x/y.md"))
                   ;; zero directories is still a match
                   (expect (ignored? m "docs/y.md"))))
             (it "matches exactly one character for `?`"
                 (let [m (rules "temp?")]
                   (expect (ignored? m "temp1"))
                   (expect (ignored? m "temps"))
                   (expect (not (ignored? m "tempxy")))
                   (expect (not (ignored? m "temp")))))
             (it "supports character classes, including the gitignore `[!…]` negation"
                 (let [m (rules "[Bb]in")]
                   (expect (dir-ignored? m "Bin"))
                   (expect (dir-ignored? m "bin"))
                   (expect (not (dir-ignored? m "cin"))))
                 (let [m (rules "x[!0-9].txt")]
                   (expect (ignored? m "xa.txt"))
                   (expect (not (ignored? m "x1.txt")))))
             (it "takes regex metacharacters literally"
                 ;; `.` must not become "any char", or `a.txt` would swallow `axtxt`.
                 (let [m (rules "a.txt")]
                   (expect (ignored? m "a.txt"))
                   (expect (not (ignored? m "axtxt"))))
                 (let [m (rules "v(1).log")]
                   (expect (ignored? m "v(1).log"))))
             (it "un-escapes a leading backslash so a literal `#`/`!` name can be ignored"
                 (expect (ignored? (rules "\\#notes.md") "#notes.md"))
                 (expect (ignored? (rules "\\!bang.md") "!bang.md"))))

(defdescribe redos-guard-test
             (it "evaluates a deep path in linear time, not quadratically"
                 ;; Regression: `(?:^|.*/)body/.*$` has TWO unbounded `.*` and pinned a core
                 ;; for ~2ms per deep path, making a repo-wide walk take a minute.
                 (let [m
                       (rules "*.log" "build/" "**/node_modules/**")

                       deep
                       (str (apply str (repeat 200 "some-directory-name/")) "file.txt")

                       start
                       (System/nanoTime)

                       _
                       (dotimes [_ 200]
                         (gi/ignored? m deep false))

                       ms
                       (/ (- (System/nanoTime) start) 1e6)]

                   (expect (not (gi/ignored? m deep false)))
                   (expect (< ms 2000) (str "200 deep-path matches took " ms "ms")))))

(defdescribe
  load-matcher-test
  (it "returns nil when the root has no ignore file at all"
      (let [root (.toFile (Files/createTempDirectory "vis-gitignore" (make-array FileAttribute 0)))]
        (expect (nil? (gi/load-matcher root)))))
  (it "reads .gitignore from the walk root"
      (let [root (.toFile (Files/createTempDirectory "vis-gitignore" (make-array FileAttribute 0)))]
        (spit (io/file root ".gitignore") "# comment\ntarget/\n*.class\n")
        (let [m (gi/load-matcher root)]
          (expect (some? m))
          (expect (dir-ignored? m "target"))
          (expect (ignored? m "src/Main.class"))
          (expect (not (ignored? m "src/Main.java"))))))
  (it "layers .gitignore < .ignore < .rgignore so the tool-only files win"
      ;; git never reads `.ignore`/`.rgignore`, so a `!corp/` there makes OUR
      ;; tools descend into `corp/` while git keeps ignoring it.
      (let [root (.toFile (Files/createTempDirectory "vis-gitignore" (make-array FileAttribute 0)))]
        (spit (io/file root ".gitignore") "corp/\nsecret.txt\n")
        (spit (io/file root ".ignore") "!corp/\n")
        (spit (io/file root ".rgignore") "!secret.txt\n")
        (let [m (gi/load-matcher root)]
          (expect (not (dir-ignored? m "corp")))
          (expect (not (ignored? m "secret.txt"))))))
  (it "keeps a lower-precedence ignore that no later file negates"
      (let [root (.toFile (Files/createTempDirectory "vis-gitignore" (make-array FileAttribute 0)))]
        (spit (io/file root ".gitignore") "*.class\n")
        (spit (io/file root ".rgignore") "*.tmp\n")
        (let [m (gi/load-matcher root)]
          (expect (ignored? m "Main.class"))
          (expect (ignored? m "a.tmp"))))))
