(ns com.blockether.vis.internal.language.python.interpreter
  "Detect WHICH Python launches a REPL or a `run_tests` shell-out, mirroring how
   the Clojure pack picks deps.edn / lein / bb. The project-managed environment
   is detected so the interpreter sees the project's dependencies."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.config.core :as config]))

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

(defn- toml-literal-at?
  "Does `lit` start at index `i` of `s`?"
  [^String s ^long i ^String lit]
  (and (<= (+ i (.length lit)) (.length s)) (.startsWith s lit (int i))))

(defn- toml-skip-line
  "Index of the first character after the line holding index `i` of `s`."
  [^String s ^long i]
  (let [j (.indexOf s "\n" (int i))]
    (if (neg? j) (.length s) (inc j))))

(defn- toml-skip-past
  "Index just past the first `close` at or after `from` in `s`, or the end of `s`
   when it never closes. With `escapes?` a backslash consumes the character behind
   it, the way a TOML basic string escapes its own quote."
  [^String s ^long from ^String close escapes?]
  (let [n (.length s)]
    (loop [i from]
      (cond (>= i n) n
            (and escapes? (= \\ (.charAt s (int i)))) (recur (+ i 2))
            (toml-literal-at? s i close) (+ i (.length close))
            :else (recur (inc i))))))

(defn- toml-header-at
  "The TOML table header opening at index `i` of `s` (the index of its `[`), as
   [path end]: the dotted path — nil when the brackets never close on that line —
   and the index just past what was read. Quoted key segments are unquoted and keep
   their own dots, so a quoted `a.b` stays one key."
  [^String s ^long i]
  (let [n
        (.length s)

        start
        (if (toml-literal-at? s i "[[") (+ i 2) (inc i))]

    (loop [j
           start

           seg
           ""

           segs
           []]

      (if (>= j n)
        [nil n]
        (let [c (.charAt s (int j))]
          (cond (= c \newline) [nil j]
                (= c \]) [(let [segs (conj segs (str/trim seg))]
                            (when (every? seq segs) (str/join "." segs)))
                          (if (toml-literal-at? s (inc j) "]") (+ j 2) (inc j))]
                (= c \.) (recur (inc j) "" (conj segs (str/trim seg)))
                (or (= c \") (= c \'))
                (let [e (long (toml-skip-past s (inc j) (str c) (= c \")))]
                  (recur e (str seg (subs s (inc j) (max (inc j) (dec e)))) segs))
                :else (recur (inc j) (str seg c) segs)))))))

(defn- toml-table-headers
  "The dotted paths of the TOML TABLE HEADERS `text` declares, in order — `tool.uv`
   for `[tool.uv]`, `tool.uv.index` for `[[tool.uv.index]]`.

   SCANNED in TOML's own lexical states, never as substring soup: line comments,
   basic and literal strings and both multi-line string forms are walked over, so a
   `[tool.uv]` sitting in a comment, in a description string or inside a docstring
   is not a table header. A header has to OPEN its line, and one whose brackets
   never close yields nothing."
  [^String text]
  (let [^String s
        (str text)

        n
        (.length s)]

    (loop [i
           0

           line-start?
           true

           acc
           []]

      (if (>= i n)
        acc
        (let [c (.charAt s (int i))]
          (cond (= c \newline) (recur (inc i) true acc)
                (or (= c \space) (= c \tab) (= c \return)) (recur (inc i) line-start? acc)
                (= c \#) (recur (long (toml-skip-line s i)) true acc)
                (toml-literal-at? s i "\"\"\"")
                (recur (long (toml-skip-past s (+ i 3) "\"\"\"" true)) false acc)
                (toml-literal-at? s i "'''")
                (recur (long (toml-skip-past s (+ i 3) "'''" false)) false acc)
                (= c \") (recur (long (toml-skip-past s (inc i) "\"" true)) false acc)
                (= c \') (recur (long (toml-skip-past s (inc i) "'" false)) false acc)
                (and line-start? (= c \[)) (let [[path end] (toml-header-at s i)]
                                             (recur (long end)
                                                    false
                                                    (cond-> acc
                                                      path
                                                      (conj path))))
                :else (recur (inc i) false acc)))))))

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

(defn pinned-runner
  "The `run_tests` backend `raw-config` pins as `python.runner` (`vispython` or
   `project`), or nil for anything else. Explicit call arguments still win."
  [raw-config]
  (let [r (some-> (get raw-config "python")
                  (get "runner")
                  str
                  str/trim
                  str/lower-case
                  not-empty)]
    (when (contains? #{"vispython" "project"} r) r)))

(defn configured-runner
  "`python.runner` from merged config, or nil. Config failures degrade to nil
   rather than breaking a run."
  []
  (try (pinned-runner (config/load-config-raw)) (catch Throwable _ nil)))

(defn detect-command
  "The argv PREFIX that launches a project-aware Python in `root`.
   Detection order (first hit wins):
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
