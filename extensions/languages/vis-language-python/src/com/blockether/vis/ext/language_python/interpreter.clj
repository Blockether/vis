(ns com.blockether.vis.ext.language-python.interpreter
  "Resolve WHICH Python launches a REPL or a `run_tests` shell-out, mirroring how
   the Clojure pack picks deps.edn / lein / bb. A project may PIN the launcher in
   merged config (`python.interpreter`); otherwise the project-managed env is
   detected, so the interpreter sees the project's dependencies — not the bare
   system interpreter."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.foundation.editing.parse :as parse]
            [com.blockether.vis.internal.paths :as paths]))

(defn- exists? [root rel] (.isFile (io/file root rel)))

(defn- on-path?
  "Is executable `bin` resolvable on PATH?"
  [bin]
  (let [dirs (str/split (or (System/getenv "PATH") "")
                        (re-pattern (java.util.regex.Pattern/quote (System/getProperty
                                                                     "path.separator"))))]
    (boolean (some (fn [d]
                     (let [f (io/file d bin)]
                       (and (.isFile f) (.canExecute f))))
                   dirs))))

(defn- venv-python
  "ABSOLUTE path of a project-local virtualenv's interpreter, or nil. Handles the
   POSIX layout (.venv/bin/python).

   Never CANONICAL: `.venv/bin/python3` is a symlink chain that ends at the base
   installation (on macOS + Homebrew, `/opt/homebrew/Cellar/python@3.x/...`).
   Resolving it walks OUT of the virtualenv, so `sys.prefix` becomes the base
   prefix, `pyvenv.cfg` is never consulted, and the run dies with `No module
   named pytest` while the same suite passes under `.venv/bin/python`."
  [root]
  (some (fn [v]
          (some (fn [rel]
                  (let [f (io/file root v rel)]
                    (when (.isFile f) (.getAbsolutePath f))))
                ["bin/python" "bin/python3"]))
        [".venv" "venv"]))

(defn- unquote-seg
  "A TOML key segment with its surrounding quotes (if any) removed."
  [s]
  (let [s (str/trim s)]
    (if (and (>= (count s) 2)
             (or (and (str/starts-with? s "\"") (str/ends-with? s "\""))
                 (and (str/starts-with? s "'") (str/ends-with? s "'"))))
      (subs s 1 (dec (count s)))
      s)))

(defn- toml-table-headers
  "The dotted paths of the TOML TABLE HEADERS `text` declares, in order — `tool.uv`
   for `[tool.uv]`, `tool.uv.index` for `[[tool.uv.index]]`.

   PARSED, never scanned: the tree-sitter TOML grammar already shipped with vis
   (the one the edit guard parses with) does the reading, so a `[tool.uv]` sitting
   in a comment, a description string or a multi-line docstring is simply not a
   table node. Unparsable input yields no headers."
  [^String text]
  (into []
        (comp (filter (fn [n]
                        (#{"table" "table_array_element"} (:kind n))))
              ;; Child 0 of a table node is its header key (bare/quoted/dotted).
              (keep (fn [n]
                      (:text (first (:children n)))))
              (map (fn [k]
                     (str/join "." (map unquote-seg (str/split k #"\."))))))
        (parse/top-level-nodes "toml" text)))

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

(defn- pin->argv
  "A raw `python.interpreter` value as an argv PREFIX. A vector is taken verbatim;
   a bare string is ONE argument and is never word-split (the same shape as a
   provider's `api_key_command` — there is no shell to quote for, so a path may
   contain spaces). Blank entries are dropped; anything else yields nil."
  [configured]
  (let [argv (into []
                   (comp (map str) (map str/trim) (remove str/blank?))
                   (cond (string? configured) [configured]
                         (sequential? configured) configured
                         :else nil))]
    (when (seq argv) argv)))

(defn- pin->program
  "`program` as the host will launch it: `~` expands, and a PATH-LIKE entry (one
   holding a separator) resolves against `root`, so `.venv/bin/python` in a
   project's `vis.yml` means THAT project's interpreter. A bare name such as
   `vis-agent` is left alone for PATH lookup."
  ^String [^String root ^String program]
  (let [expanded
        (paths/expand-home program)

        ^java.io.File f
        (io/file expanded)]

    (cond (.isAbsolute f) (.getAbsolutePath f)
          (str/includes? expanded "/") (.getAbsolutePath (io/file root expanded))
          :else expanded)))

(defn pinned-command
  "The argv prefix `raw-config` (merged config, string keys) pins as
   `python.interpreter` for `root`, or nil when nothing is pinned:

     python:
       interpreter: [vis-agent, python]

   Pure — reading the config file is `resolve-command`'s job."
  [^String root raw-config]
  (some-> (pin->argv (get-in raw-config ["python" "interpreter"]))
          (update 0 #(pin->program root %))))

(defn pinned-runner
  "The `run_tests` backend `raw-config` pins as `python.runner` (`graalpy` or
   `project`), or nil for anything else. Explicit call arguments still win."
  [raw-config]
  (let [r (some-> (get raw-config "python")
                  (get "runner")
                  str
                  str/trim
                  str/lower-case
                  not-empty)]
    (when (contains? #{"graalpy" "project"} r) r)))

(defn configured-runner
  "`python.runner` from merged config, or nil. Config failures degrade to nil
   rather than breaking a run."
  []
  (try (pinned-runner (config/load-config-raw)) (catch Throwable _ nil)))

(defn detect-command
  "The argv PREFIX detected for `root`, ignoring config. Detection order (first
   hit wins):
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

(defn resolve-command
  "The argv PREFIX that launches a project-aware Python in `root`: the one pinned
   in merged config as `python.interpreter`, else `detect-command`'s detection.
   A pin is how a workspace whose only sanctioned invocation is something vis
   cannot detect (`vis-agent python`, a wrapper script, a container shim) still
   gets its own launcher out of `repl_start` / `repl_eval` / `run_tests`."
  [root]
  (or (try (pinned-command root (config/load-config-raw)) (catch Throwable _ nil))
      (detect-command root)))
