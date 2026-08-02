(ns com.blockether.vis.ext.language-python.interpreter
  "Resolve WHICH Python launches a REPL, mirroring how the Clojure pack picks
   deps.edn / lein / bb. Prefers a project-managed env so the REPL sees the
   project's dependencies — not the bare system interpreter."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- exists? [root rel] (.isFile (io/file root rel)))

(defn- on-path?
  "Is executable `bin` resolvable on PATH?"
  [bin]
  (let
    [dirs (str/split (or (System/getenv "PATH") "")
                     (re-pattern (java.util.regex.Pattern/quote (System/getProperty
                                                                  "path.separator"))))]
    (boolean (some (fn [d]
                     (let [f (io/file d bin)]
                       (and (.isFile f) (.canExecute f))))
                   dirs))))

(defn- venv-python
  "Canonical path of a project-local virtualenv's interpreter, or nil. Handles
   the POSIX layout (.venv/bin/python)."
  [root]
  (some (fn [v]
          (some (fn [rel]
                  (let [f (io/file root v rel)]
                    (when (.isFile f) (.getCanonicalPath f))))
                ["bin/python" "bin/python3"]))
        [".venv" "venv"]))

(defn- toml-table-headers
  "The dotted paths of the TOML TABLE HEADERS `text` declares, in order — `tool.uv`
   for `[tool.uv]`, `tool.uv.sources` for `[[tool.uv.sources]]`. A header counts
   only when it is the whole line (comments allowed after it) and outside a
   multi-line string, so `[tool.uv]` inside a comment, a description or a
   docstring never registers. Quotes and whitespace are stripped from segments.
   This answers WHICH tables a file declares; it is not a TOML parser."
  [^String text]
  (letfn [(unquote-seg [s]
            (let [s (str/trim s)]
              (if (and (>= (count s) 2)
                       (or (and (str/starts-with? s "\"") (str/ends-with? s "\""))
                           (and (str/starts-with? s "'") (str/ends-with? s "'"))))
                (subs s 1 (dec (count s)))
                s)))]
    (loop
      [[line & more]
       (str/split-lines text)

       open
       nil

       acc
       []]

      (if (nil? line)
        acc
        (let
          [delims
           (if open [open] ["\"\"\"" "'''"])

           toggled
           (some (fn [d]
                   (when (odd? (count (re-seq (re-pattern (java.util.regex.Pattern/quote d)) line)))
                     d))
                 delims)

           header
           (when-not open
             (some-> (re-matches #"\s*\[\[?\s*([^\[\]]+?)\s*\]\]?\s*(?:#.*)?" line)
                     second))]

          (recur more
                 (cond (and open toggled) nil
                       toggled toggled
                       :else open)
                 (if header
                   (conj acc (str/join "." (map unquote-seg (str/split header #"\."))))
                   acc)))))))

(defn- declares-table?
  "Does TOML `text` declare table `path`, or any table beneath it? `[tool.uv]` and
   `[tool.uv.sources]` both answer true for `\"tool.uv\"`; `[tool.uvicorn]` does not."
  [^String text ^String path]
  (boolean (some (fn [h]
                   (or (= h path) (str/starts-with? h (str path "."))))
                 (toml-table-headers text))))

(defn- uv-project?
  "Is `root` uv-managed? A `uv.lock`, or a REAL `[tool.uv]` table (or a subtable of
   it) in `pyproject.toml` — read as TOML table headers, never as substring soup:
   `[tool.uvicorn]`, a commented-out `[tool.uv]` and a description that merely
   mentions one are all NOT uv projects, and picking `uv run python` for them
   launches the wrong interpreter."
  [root]
  (or (exists? root "uv.lock")
      (and (exists? root "pyproject.toml")
           (try (declares-table? (slurp (io/file root "pyproject.toml")) "tool.uv")
                (catch Throwable _ false)))))

(defn resolve-command
  "The argv PREFIX that launches a project-aware Python in `root`. Detection
   order (first hit wins):
     1. uv      — uv.lock / [tool.uv] in pyproject + `uv` on PATH → [uv run python]
     2. poetry  — poetry.lock + `poetry` on PATH                  → [poetry run python]
     3. venv    — .venv/ or venv/ interpreter                     → [<abs path>]
     4. system  — python3, else python                            → [python3]"
  [root]
  (let [sys-py (if (on-path? "python3") "python3" "python")]
    (cond (and (uv-project? root) (on-path? "uv")) ["uv" "run" "python"]
          (and (exists? root "poetry.lock") (on-path? "poetry")) ["poetry" "run" "python"]
          (venv-python root) [(venv-python root)]
          :else [sys-py])))
