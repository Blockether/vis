;; Pins the bin/check-test-drops contract that AGENTS.md (## Tests) gives agents:
;; a deleted test that names an issue, a regression, a user report or a contract
;; fixture needs a Drops-Test trailer with a reason.
(ns com.blockether.vis.check-test-drops-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe describe expect it]])
  (:import [java.io File]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def ^:private script (.getAbsolutePath (io/file "bin/check-test-drops")))

(def ^:private commit-flags
  ["-c" "user.name=Vis Test" "-c" "user.email=vis@example.com" "-c" "commit.gpgsign=false"])

(defn- run-in
  "Run `args` in `dir` and return the exit status and the merged output."
  [^File dir args]
  (let [process
        (.start (doto (ProcessBuilder. ^java.util.List (vec args))
                  (.directory dir)
                  (.redirectErrorStream true)))

        output
        (slurp (.getInputStream process))]

    {:exit (.waitFor process) :output output}))

(defn- git!
  "Run git in `dir`, returning its trimmed output or throwing with the command output."
  [dir & args]
  (let [{:keys [exit output]} (run-in dir (into ["git"] args))]
    (when-not (zero? (long exit))
      (throw (ex-info "Git fixture command failed" {:dir dir :args args :output output})))
    (str/trim output)))

(defn- commit!
  "Write `files` (a nil text deletes the file), commit with `message` and return the commit."
  [dir message files]
  (doseq [[path text] files]
    (let [file (io/file dir path)]
      (if (nil? text) (io/delete-file file) (do (io/make-parents file) (spit file text)))))
  (git! dir "add" "--all")
  (apply git! dir (into commit-flags ["commit" "--quiet" "--allow-empty" "-m" message]))
  (git! dir "rev-parse" "HEAD"))

(defn- drop-check
  "Commit `before`, then `after` with the first message and an empty commit for each
  later message. Check the commits after `before`."
  [before after [message & later]]
  (let [dir (.toFile (Files/createTempDirectory "check-test-drops-" (make-array FileAttribute 0)))]
    (try (git! dir "init" "--quiet" "--initial-branch=main")
         (let [base (commit! dir "test: add tests" before)]
           (commit! dir message after)
           (doseq [text later]
             (commit! dir text {}))
           (run-in dir [script base "HEAD"]))
         (finally (doseq [file (reverse (file-seq dir))]
                    (io/delete-file file true))))))

(def ^:private cursor-before
  "(ns foo-test (:require [lazytest.core :refer [defdescribe expect it]]))
(defdescribe cursor-test
  ;; Issue #204: the cursor jumped after a resize.
  (it \"keeps the cursor on its line\" (expect (= 1 1)))
  (it \"keeps the scroll offset\" (expect (= 2 2))))
")

(def ^:private cursor-after
  "(ns foo-test (:require [lazytest.core :refer [defdescribe expect it]]))
(defdescribe cursor-test
  (it \"keeps the scroll offset\" (expect (= 2 2))))
")

(def ^:private cursor-trailer "Drops-Test: test/foo_test.clj keeps the cursor on its line - ")

(defn- cursor-check
  [messages]
  (drop-check {"test/foo_test.clj" cursor-before} {"test/foo_test.clj" cursor-after} messages))

(def ^:private kept-before
  "(defdescribe kept-test
  (it \"checks a plain case\" (expect (= 1 1)))
  ;; Regression: the move lost this case.
  (it \"moves to another file\" (expect (= 2 2)))
  ;; Regression: the rename lost this case.
  (it \"keeps its body\" (expect (= 3 3)))
  ;; Regression: the edit lost this case.
  (it \"rounds a ratio down to the nearest step\" (expect (= 4 4))))
")

(def ^:private kept-after
  "(defdescribe kept-test
  ;; Regression: the rename lost this case.
  (it \"has a new name\" (expect (= 3 3)))
  ;; Regression: the edit lost this case.
  (it \"rounds a ratio down to the nearest whole step\" (expect (= 4 5))))
")

(def ^:private moved-file
  "(defdescribe moved-test
  ;; Regression: the move lost this case.
  (it \"moves to another file\" (expect (= 2 2))))
")

;; The apostrophe in JSX text, the regex literal and the typed it.each must not
;; hide a test from the parser.
(def ^:private chat-file
  "// Regression, user report: the stream painted every row again.
import { expect, it } from 'vitest';
const closing = /<\\/b>/g;
it('renders the row', () => {
  expect(render(<p>Don't stop</p>)).toBeTruthy();
});
it.each<string>(['a', 'b'])('keeps %s', (x) => {
  expect(`${x}`).toBe(x);
});
")

(def ^:private footer-before
  ";; Regression (user report): the footer hid the limits.
(defdescribe footer-test
  (it \"shows the limits\" (expect (= 1 1))))

(defdescribe other-test
  (it \"stays\" (expect (= 2 2))))
")

(def ^:private footer-after "(defdescribe other-test
  (it \"stays\" (expect (= 2 2))))
")

(def ^:private cli-before
  "# Issue #12: the CLI exited 0 on a bad flag.
def test_bad_flag_exits_2():
    assert 2 == 2


def test_help():
    assert 1 == 1
")

(def ^:private cli-after "def test_help():
    assert 1 == 1
")

(defdescribe
  check-test-drops-test
  (describe "a deleted test with an issue in the comment above it"
            (it "fails and prints the trailer line to copy"
                (let [{:keys [exit output]} (cursor-check ["test: drop a test"])]
                  (expect (= 1 exit))
                  (expect (str/includes? output "Issue #204"))
                  (expect (str/includes? output (str cursor-trailer "<reason>")))))
            (it "still fails when the trailer keeps the placeholder reason"
                (let [{:keys [exit output]}
                      (cursor-check [(str "test: drop a test\n\n" cursor-trailer "<reason>")])]
                  (expect (= 1 exit))
                  (expect (str/includes? output "\"keeps the cursor on its line\": Issue #204"))))
            (it "passes when a later commit of the range gives a reason"
                (let [{:keys [exit output]} (cursor-check ["test: drop a test"
                                                           (str "docs: explain the dropped test\n\n"
                                                                cursor-trailer
                                                                "the resize code is gone")])]
                  (expect (= 0 exit))
                  (expect (str/includes? output "1 deleted test, 1 marked")))))
  (it "passes an unmarked deletion and keeps moved, renamed and edited tests"
      (let [{:keys [exit output]} (drop-check {"test/kept_test.clj" kept-before}
                                              {"test/kept_test.clj" kept-after
                                               "test/moved_test.clj" moved-file}
                                              ["test: move and rename tests"])]
        (expect (= 0 exit))
        (expect (str/includes? output "1 deleted test, 0 marked"))))
  (describe
    "a deleted file with a user report in its header"
    (it "asks once for the whole file"
        (let [{:keys [exit output]} (drop-check {"src/Chat.test.tsx" chat-file}
                                                {"src/Chat.test.tsx" nil}
                                                ["test: delete a file"])]
          (expect (= 1 exit))
          (expect (str/includes? output "deleted file with 2 tests: Regression"))
          (expect (str/includes? output "Drops-Test: src/Chat.test.tsx - <reason>"))))
    (it "passes with one trailer for the file"
        (let [{:keys [exit output]}
              (drop-check
                {"src/Chat.test.tsx" chat-file}
                {"src/Chat.test.tsx" nil}
                ["test: delete a file\n\nDrops-Test: src/Chat.test.tsx - the stream view is gone"])]
          (expect (= 0 exit))
          (expect (str/includes? output "2 deleted tests, 1 marked")))))
  (it "reads the comment above a describe that the commit deletes whole"
      (let [{:keys [exit output]} (drop-check {"test/footer_test.clj" footer-before}
                                              {"test/footer_test.clj" footer-after}
                                              ["test: delete a describe"])]
        (expect (= 1 exit))
        (expect (str/includes? output "line 3 \"shows the limits\": Regression (line 1)"))))
  (it "reads Python test functions and the comment above them"
      (let [{:keys [exit output]} (drop-check {"tests/test_cli.py" cli-before}
                                              {"tests/test_cli.py" cli-after}
                                              ["test: drop a Python test"])]
        (expect (= 1 exit))
        (expect (str/includes? output
                               "Drops-Test: tests/test_cli.py test_bad_flag_exits_2 - <reason>")))))
