(ns com.blockether.vis.ext.language-python.interpreter-test
  "Which interpreter the pack launches: `uv run python` only for a project that
   really is uv-managed. The `[tool.uv]` question is answered by PARSING
   pyproject.toml, so a table named in a comment or in a description string is
   not a declaration."
  (:require [com.blockether.vis.ext.language-python.interpreter :as interp]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.io File)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- project
  "A throwaway project root holding a `pyproject.toml` of `content`."
  ^File [^String content]
  (let [root (.toFile (Files/createTempDirectory "vis-py-interp-" (into-array FileAttribute [])))]
    (spit (File. root "pyproject.toml") content)
    root))

(def ^:private table-headers #'interp/toml-table-headers)

(def ^:private uv-project? #'interp/uv-project?)

(defdescribe
  toml-table-headers-test
  "Table headers come off the parse tree, never out of a substring scan."
  (it "names every table and table-array header in order"
      (expect (= ["tool.uv" "tool.uv.index" "project"]
                 (table-headers
                   "[tool.uv]\nx = 1\n\n[[tool.uv.index]]\nname = \"a\"\n\n[project]\n"))))
  (it "answers nothing for input that does not parse" (expect (= [] (table-headers "[[[ broken")))))

(defdescribe uv-project-test
             "`uv run python` is picked only for a real uv project."
             (it "recognizes a declared [tool.uv] table, and a subtable of it"
                 (expect (true? (uv-project? (project "[tool.uv]\n"))))
                 (expect (true? (uv-project? (project "[tool.uv.sources]\n")))))
             ;; A commented-out table, a description that merely mentions one, and a package
             ;; whose name only starts the same way are all NOT uv projects: picking
             ;; `uv run python` for them launches the wrong interpreter.
             (it "leaves a table that only LOOKS declared alone"
                 (expect (false? (uv-project? (project "# [tool.uv]\n"))))
                 (expect (false? (uv-project?
                                   (project "[project]\ndescription = \"not [tool.uv] here\"\n"))))
                 (expect (false? (uv-project? (project "[tool.uvicorn]\n"))))))
