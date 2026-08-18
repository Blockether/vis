(ns com.blockether.vis.ext.language-python.ruff-test
  "format_code / lint_code with {\"language\": \"python\"}: the ruff-backed handlers.
   Everything runs in-process through com.blockether/ruff (no `ruff` binary),
   so these exercise the real formatter/linter against a throwaway project."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.ext.language-python.ruff :as pyruff]
            [com.blockether.vis.internal.foundation.surface-contract :as contract]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- tmp-dir
  ^java.io.File []
  (.toFile (Files/createTempDirectory "vis-py-ruff-" (into-array FileAttribute []))))

(defn- cleanup
  [^java.io.File root]
  (when (.exists root)
    (doseq [^java.io.File f (reverse (file-seq root))]
      (.delete f))))

(defn- spit!
  [^java.io.File root ^String rel ^String content]
  (let [f (io/file root rel)]
    (io/make-parents f)
    (spit f content)
    f))

(defn- sample-project
  "A tiny src-layout project: one unformatted module, one test with an undefined
   name, plus a `.venv` copy that must never be touched."
  ^java.io.File []
  (let [root (tmp-dir)]
    (spit! root "pyproject.toml" "[project]\nname = \"p\"\n\n[tool.ruff]\nline-length = 100\n")
    (spit! root "src/pkg/a.py" "import os\ndef  f( x ):\n    y=1\n    return   x+y\n")
    (spit! root "tests/test_a.py" "def test_x():\n    assert undefined_thing == 1\n")
    (spit! root ".venv/lib/junk.py" "import  os\n")
    root))

(defn- env [^java.io.File root] {:workspace/root (.getPath root)})

;; ── format_code ─────────────────────────────────────────────────────────────

(defdescribe py-format-fn-test
             "ruff format behind the format_code facade."
             (it "formats a code string without returning the text"
                 (let [r (:result (pyruff/py-format-fn nil {"code" "x=[1,  2]\n"}))]
                   (expect (contract/valid? :format-fn r))
                   (expect (= true (get r "changed")))
                   (expect (= "ruff" (get r "formatter")))
                   (expect (nil? (get r "code")))))
             (it "leaves already-formatted source alone"
                 (let [r (:result (pyruff/py-format-fn nil {"code" "x = [1, 2]\n"}))]
                   (expect (= false (get r "changed")))))
             (it "returns the source verbatim for unparsable Python (never throws)"
                 (let [r (:result (pyruff/py-format-fn nil {"code" "def (:\n"}))]
                   (expect (= false (get r "changed")))))
             (it "formats a single file IN PLACE and reports its relative path"
                 (let [root (sample-project)]
                   (try (let [r (:result (pyruff/py-format-fn (env root) {"path" "src/pkg/a.py"}))]
                          (expect (contract/valid? :format-fn r))
                          ;; a named path takes the SAME batch shape as {paths}:
                          ;; "changed" counts files, each file reports its own flag
                          (expect (= 1 (get r "changed")))
                          (expect (= [{"path" "src/pkg/a.py" "changed" true "formatter" "ruff"}]
                                     (get r "files")))
                          (expect (= "import os\n\n\ndef f(x):\n    y = 1\n    return x + y\n"
                                     (slurp (io/file root "src/pkg/a.py")))))
                        (finally (cleanup root)))))
             (it "walks the whole project by default and SKIPS .venv"
                 (let [root (sample-project)]
                   (try (let [r (:result (pyruff/py-format-fn (env root) {}))
                              paths (set (map #(get % "path") (get r "files")))]

                          (expect (contract/valid? :format-fn r))
                          (expect (= #{"src/pkg/a.py" "tests/test_a.py"} paths))
                          (expect (= ["ruff"] (get r "formatters")))
                          (expect (= 1 (get r "changed")))
                          ;; the vendored copy is untouched, unformatted as written
                          (expect (= "import  os\n" (slurp (io/file root ".venv/lib/junk.py")))))
                        (finally (cleanup root)))))
             (it "errors on a target that does not exist instead of reporting 0 files"
                 (let [root (sample-project)]
                   (try (let [r (pyruff/py-format-fn (env root) {"path" "nope.py"})]
                          (expect (false? (:success? r)))
                          (expect (re-find #"does not exist" (get-in r [:error :message]))))
                        (finally (cleanup root))))))

;; ── lint_code ───────────────────────────────────────────────────────────────

(defdescribe
  py-lint-fn-test
  "ruff lint behind the lint_code facade."
  (it "lints a code string and grades findings by severity"
      (let [r
            (:result (pyruff/py-lint-fn nil {"code" "import os\nx = 1\n"}))

            codes
            (into {} (map (juxt #(get % "type") #(get % "level"))) (get r "findings"))]

        (expect (contract/valid? :lint-fn r))
        ;; an unused import is a WARNING — the file still runs
        (expect (= "warning" (get codes "F401")))
        (expect (= ["ruff"] (get r "providers")))
        (expect (= "import os\nx = 1\n" (get r "snippet")))
        (expect (= 0 (get r "error")))))
  (it "grades an undefined name (F821) as an error"
      (let [r (:result (pyruff/py-lint-fn nil {"code" "x = undefined_thing\n"}))]
        (expect (= 1 (get r "error")))
        (expect (= "F821" (get-in r ["findings" 0 "type"])))))
  (it "reports unparsable Python as a finding, not a throw"
      (let [r (:result (pyruff/py-lint-fn nil {"code" "def (:\n"}))]
        (expect (contract/valid? :lint-fn r))
        (expect (pos? (get r "error")))))
  (it "is clean on clean source"
      (let [r (:result (pyruff/py-lint-fn nil {"code" "x = 1\n"}))]
        (expect (= [] (get r "findings")))
        (expect (= 0 (get r "error") (get r "warning") (get r "info")))))
  (it "lints the whole project by default, relativizing files and skipping .venv"
      (let [root (sample-project)]
        (try (let [r (:result (pyruff/py-lint-fn (env root) {}))
                   by-file (reduce (fn [m f]
                                     (update m (get f "file") (fnil conj #{}) (get f "type")))
                                   {}
                                   (get r "findings"))]

               (expect (contract/valid? :lint-fn r))
               (expect (= 2 (get r "files")))
               (expect (contains? (get by-file "src/pkg/a.py") "F401"))
               (expect (contains? (get by-file "tests/test_a.py") "F821"))
               (expect (= 1 (get r "error")))
               (expect (not-any? #(re-find #"\.venv" (str (get % "file"))) (get r "findings"))))
             (finally (cleanup root)))))
  (it "honors an explicit select over the project's default rule set"
      (let [r (:result (pyruff/py-lint-fn nil {"code" "import os\nx = 1\n" "select" "E501"}))]
        (expect (= [] (get r "findings")))))
  (it "errors on a target that does not exist"
      (let [root (sample-project)]
        (try (let [r (pyruff/py-lint-fn (env root) {"path" "nope.py"})]
               (expect (false? (:success? r)))
               (expect (re-find #"does not exist" (get-in r [:error :message]))))
             (finally (cleanup root))))))

;; ── what the result SAYS was linted ─────────────────────────────────────────

;; Regression: `lint_code("python", {"paths" [...]})` counted the files that HAD
;; findings, not the files it linted, and never echoed `targets` (the Clojure pack
;; does). A clean run over a dozen sources therefore came back `files 0` with no
;; targets and rendered as `0 files — clean`, indistinguishable from a run that
;; matched nothing and silently linted NOTHING.
(defdescribe lint-target-reporting-test
             "the result names what was linted, findings or not."
             (it "counts the files it LINTED, not the files that had findings"
                 (let [root (tmp-dir)]
                   (try (spit! root "ruff.toml" "line-length = 100\n")
                        (spit! root "src/a.py" "x = 1\n")
                        (spit! root "src/b.py" "y = 2\n")
                        (let [r (:result (pyruff/py-lint-fn (env root) {"paths" ["src"]}))]
                          (expect (contract/valid? :lint-fn r))
                          (expect (= [] (get r "findings")))
                          (expect (= 2 (get r "files"))))
                        (finally (cleanup root)))))
             (it "echoes the requested targets, relative to the workspace root"
                 (let [root (tmp-dir)]
                   (try (spit! root "src/a.py" "x = 1\n")
                        (spit! root "tests/test_a.py" "y = 2\n")
                        (let [r (:result (pyruff/py-lint-fn (env root)
                                                            {"path" "src"
                                                             "paths" ["tests/test_a.py"]}))]
                          (expect (= ["src" "tests/test_a.py"] (get r "targets"))))
                        (finally (cleanup root)))))
             (it "reports a whole-project lint by the files it walked"
                 (let [root (sample-project)]
                   (try (let [r (:result (pyruff/py-lint-fn (env root) {}))]
                          ;; src/pkg/a.py + tests/test_a.py; .venv/lib/junk.py is pruned
                          (expect (= 2 (get r "files")))
                          (expect (nil? (get r "targets"))))
                        (finally (cleanup root)))))
             (it "counts a snippet lint as one file, like the Clojure pack's stdin lint"
                 (let [r (:result (pyruff/py-lint-fn nil {"code" "x = 1\n"}))]
                   (expect (= 1 (get r "files")))
                   (expect (nil? (get r "targets"))))))

;; ── project configuration ───────────────────────────────────────────────────

(defdescribe config-for-test
             "ruff's OWN configuration discovery is what vis uses."
             (it "finds a pyproject.toml carrying [tool.ruff]"
                 (let [root (sample-project)]
                   (try (expect (= (.getCanonicalPath (io/file root "pyproject.toml"))
                                   (some-> (pyruff/config-for (io/file root "src/pkg/a.py"))
                                           io/file
                                           .getCanonicalPath)))
                        (finally (cleanup root)))))
             (it "prefers a ruff.toml over pyproject.toml"
                 (let [root (sample-project)]
                   (try (spit! root "ruff.toml" "line-length = 60\n")
                        (expect (= (.getCanonicalPath (io/file root "ruff.toml"))
                                   (some-> (pyruff/config-for (io/file root "src/pkg/a.py"))
                                           io/file
                                           .getCanonicalPath)))
                        (finally (cleanup root)))))
             (it "returns nil for a tree without any ruff configuration"
                 (let [root (tmp-dir)]
                   (try (spit! root "a.py" "x = 1\n")
                        (expect (nil? (pyruff/config-for (io/file root "a.py"))))
                        (finally (cleanup root))))))

(defdescribe project-config-test
             "the discovered config drives the run, and explicit opts win."
             (it "reports the configuration file it used and no hint"
                 (let [root (sample-project)]
                   (try (let [r (:result (pyruff/py-lint-fn (env root) {"path" "src"}))]
                          (expect (= "pyproject.toml" (get r "config")))
                          (expect (nil? (get r "hint"))))
                        (finally (cleanup root)))))
             (it "hints at a ruff config file when the project pins nothing"
                 (let [root (tmp-dir)]
                   (try (spit! root "a.py" "x = 1\n")
                        (let [r (:result (pyruff/py-lint-fn (env root) {"path" "a.py"}))]
                          (expect (nil? (get r "config")))
                          (expect (= pyruff/no-config-hint (get r "hint")))
                          (expect (re-find #"ruff[.]toml" (get r "hint"))))
                        (finally (cleanup root)))))
             (it "honours select and line-length from ruff.toml"
                 (let [root (tmp-dir)]
                   (try (spit! root "ruff.toml" "line-length = 20\nlint.select = [\"E501\"]\n")
                        (spit! root "a.py" (str "x = \"" (apply str (repeat 40 "a")) "\"\n"))
                        (let [r (:result (pyruff/py-lint-fn (env root) {"path" "a.py"}))]
                          (expect (= "E501" (get-in r ["findings" 0 "type"]))))
                        (finally (cleanup root)))))
             (it "honours per-file-ignores from ruff.toml"
                 (let [root (tmp-dir)]
                   (try (spit! root
                               "ruff.toml"
                               (str "lint.select = [\"F\"]\n"
                                    "[lint.per-file-ignores]\n\"scripts/*.py\" = [\"F401\"]\n"))
                        (spit! root "scripts/s.py" "import os\n")
                        (spit! root "lib.py" "import os\n")
                        (let [ignored (:result (pyruff/py-lint-fn (env root) {"path" "scripts"}))
                              flagged (:result (pyruff/py-lint-fn (env root) {"path" "lib.py"}))]

                          (expect (= [] (get ignored "findings")))
                          (expect (= "F401" (get-in flagged ["findings" 0 "type"]))))
                        (finally (cleanup root)))))
             (it "lets an explicit line-length override the project config"
                 (let [root (sample-project)]
                   (try
                     ;; 100 chars fit the project width; 20 does not -> E501 appears
                     (let [long-line (str "x = \"" (apply str (repeat 40 "a")) "\"\n")
                           wide (:result (pyruff/py-lint-fn (env root) {"code" long-line}))
                           narrow (:result (pyruff/py-lint-fn
                                             (env root)
                                             {"code" long-line "line_length" 20 "select" "E501"}))]

                       (expect (= [] (get wide "findings")))
                       (expect (= "E501" (get-in narrow ["findings" 0 "type"]))))
                     (finally (cleanup root))))))

;; ── targets that hold no Python ─────────────────────────────────────────────

;; Regression: an EXISTING target with no Python in it — a README, a docs
;; directory, a tree whose sources are all `.txt` — expanded to zero files and
;; came back `files 0 … clean` (lint) / `changed 0` (format): byte-identical to
;; the answer a clean project gives. A model that aimed `lint_code` at the wrong
;; directory was told its code was fine, with nothing read at all. Only a
;; NON-EXISTENT path was ever an error, though both docstrings promised that "a
;; target that resolves to nothing is an ERROR, not a silent `clean`".
(defdescribe empty-target-test
             "a named target that holds no Python is an error, not a clean report."
             (it "lint errors on a named file that is not Python"
                 (let [root (tmp-dir)]
                   (try (spit! root "README.md" "# hi\n")
                        (let [r (pyruff/py-lint-fn (env root) {"path" "README.md"})]
                          (expect (false? (:success? r)))
                          (expect (re-find #"no Python" (get-in r [:error :message])))
                          (expect (re-find #"README\.md" (get-in r [:error :message]))))
                        (finally (cleanup root)))))
             (it "lint errors on a directory that holds no Python"
                 (let [root (tmp-dir)]
                   (try (spit! root "docs/guide.md" "# hi\n")
                        (let [r (pyruff/py-lint-fn (env root) {"paths" ["docs"]})]
                          (expect (false? (:success? r)))
                          (expect (re-find #"docs" (get-in r [:error :message]))))
                        (finally (cleanup root)))))
             (it "names only the empty targets, not the ones that carried Python"
                 (let [root (tmp-dir)]
                   (try (spit! root "src/a.py" "x = 1\n")
                        (spit! root "docs/guide.md" "# hi\n")
                        (let [r (pyruff/py-lint-fn (env root) {"paths" ["src" "docs"]})
                              m (get-in r [:error :message])]

                          (expect (false? (:success? r)))
                          (expect (re-find #"docs" m))
                          (expect (nil? (re-find #"src" m))))
                        (finally (cleanup root)))))
             (it "format errors on the same target instead of reporting 0 changed"
                 (let [root (tmp-dir)]
                   (try (spit! root "README.md" "# hi\n")
                        (let [r (pyruff/py-format-fn (env root) {"path" "README.md"})]
                          (expect (false? (:success? r)))
                          (expect (re-find #"no Python" (get-in r [:error :message]))))
                        (finally (cleanup root)))))
             (it "still lints a target that does carry Python"
                 (let [root (tmp-dir)]
                   (try (spit! root "src/a.py" "x = 1\n")
                        (let [r (:result (pyruff/py-lint-fn (env root) {"paths" ["src"]}))]
                          (expect (= 1 (get r "files")))
                          (expect (= [] (get r "findings"))))
                        (finally (cleanup root)))))
             (it "leaves a whole-project run over a Python-less tree CLEAN, not an error"
                 (let [root (tmp-dir)]
                   (try (spit! root "README.md" "# hi\n")
                        (let [r (:result (pyruff/py-lint-fn (env root) {}))]
                          (expect (contract/valid? :lint-fn r))
                          (expect (= 0 (get r "files")))
                          (expect (= [] (get r "findings"))))
                        (finally (cleanup root))))))
