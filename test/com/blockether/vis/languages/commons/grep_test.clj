(ns com.blockether.vis.languages.commons.grep-test
  "Regression tests for the grep tool's input/output contract.

   Focus: the tool must accept EITHER a directory (recursive walk) OR a
   single file (scan just that file). Prior versions threw when the path
   pointed at a file, forcing agents to discover the file's parent
   directory AND a glob AND retry — a miserable detour when the agent
   already knew the exact file it wanted to search."
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [lazytest.core :refer [defdescribe describe it expect]]
    [com.blockether.vis.languages.commons.grep :as sut]))

(defn- tmp-dir []
  (let [d (.toFile (java.nio.file.Files/createTempDirectory
                     "vis-grep-test-"
                     (make-array java.nio.file.attribute.FileAttribute 0)))]
    (.deleteOnExit d)
    d))

(defn- write! [^java.io.File f ^String content]
  (when-let [p (.getParentFile f)] (.mkdirs p))
  (spit f content :encoding "UTF-8")
  f)

(defdescribe grep-directory-mode
  (describe "grep on a directory root"
    (it "recursively scans and returns matches"
      (let [d (tmp-dir)
            _ (write! (io/file d "a.txt") "alpha\nbeta\nalpha again")
            _ (write! (io/file d "nested" "b.txt") "beta only")
            result (sut/grep "alpha" (.getAbsolutePath d))]
        (expect (= :directory (:mode result)))
        (expect (= 2 (count (:matches result))))
        (expect (every? string? (map :path (:matches result))))))))

(defdescribe grep-file-mode
  (describe "grep on a single file (regression)"
    (it "accepts a file path and scans just that file"
      ;; Previously this threw "grep path must be a directory". The agent
      ;; would then have to switch to read-file + walk the whole string
      ;; itself. Accepting the file directly is strictly more useful.
      (let [d (tmp-dir)
            f (write! (io/file d "style.css")
                ".var-row-label{font-size:9px}\n.var-row-code{color:blue}\n.var-row-value{margin:0}")
            result (sut/grep "var-row" (.getAbsolutePath f))]
        (expect (map? result))
        (expect (= :file (:mode result)))
        (expect (= 1 (:files-scanned result)))
        (expect (= 1 (:files-with-matches result)))
        (expect (= 3 (count (:matches result))))
        ;; In :file mode the per-match :path is the basename (not a full
        ;; path), mirroring the directory-mode relative-path convention.
        (expect (every? #(= "style.css" (:path %)) (:matches result)))))

    (it "supports opts (case-insensitive?) in file mode"
      (let [d (tmp-dir)
            f (write! (io/file d "x.txt") "ScrollBar Thumb\nscrollbar track")
            result (sut/grep "scrollbar" (.getAbsolutePath f)
                     {:case-insensitive? true})]
        (expect (= 2 (count (:matches result))))))

    (it "still throws when path does not exist"
      (let [ex (try (sut/grep "x" "/definitely/not/a/real/path-abc123")
                 nil
                 (catch Exception e e))]
        (expect (some? ex))
        (expect (= :tool/invalid-input (:type (ex-data ex))))
        (expect (str/includes? (ex-message ex) "not found"))))))

(defdescribe grep-relative-path-regression
  (describe "grep with a RELATIVE path root (regression)"
    ;; Prior versions crashed with `'other' is different type of Path` because
    ;; the root was left as a relative File (`(io/file "src")`) while child
    ;; files were built from `(.getCanonicalPath root)` -> absolute. The
    ;; `Path.relativize` call then compared a relative Path to an absolute
    ;; one and blew up. Agents calling `(grep "foo" "src")` or `(grep "foo"
    ;; ".")` burned entire iteration budgets on this.
    (it "accepts \".\" as a path without crashing"
      ;; This is the canonical repro. The JVM's cwd is whatever the test
      ;; runner started in; `.` just needs to exist (it always does). The
      ;; bug triggers deterministically regardless of what cwd contains,
      ;; because the crash is in Path.relativize, not in file-walking.
      (let [result (sut/grep "definitely-will-not-match-anything-xyz-123" ".")]
        (expect (= :directory (:mode result)))
        (expect (vector? (:matches result)))))

    (it "accepts a relative path segment without crashing"
      ;; Equivalent repro using a real relative subdir that exists in any
      ;; checkout of this repo. Same root cause as the `.` case.
      (let [result (sut/grep "definitely-will-not-match-anything-xyz-123" "src")]
        (expect (= :directory (:mode result)))
        (expect (vector? (:matches result)))))))
