(ns com.blockether.vis.ext.language-python.interpreter
  "Resolve WHICH Python launches a REPL, mirroring how the Clojure pack picks
   deps.edn / lein / bb. Prefers a project-managed env so the REPL sees the
   project's dependencies — not the bare system interpreter."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- windows? [] (str/starts-with? (str (System/getProperty "os.name")) "Windows"))

(defn- exists? [root rel] (.isFile (io/file root rel)))

(defn- on-path?
  "Is executable `bin` resolvable on PATH? (Tries the name and, on Windows, the
   .exe variant.)"
  [bin]
  (let
    [names
     (cond-> [bin]
       (windows?)
       (conj (str bin ".exe")))

     dirs
     (str/split (or (System/getenv "PATH") "")
                (re-pattern (java.util.regex.Pattern/quote (System/getProperty "path.separator"))))]

    (boolean (some (fn [d]
                     (some (fn [n]
                             (let [f (io/file d n)]
                               (and (.isFile f) (.canExecute f))))
                           names))
                   dirs))))

(defn- venv-python
  "Canonical path of a project-local virtualenv's interpreter, or nil. Handles
   both POSIX (.venv/bin/python) and Windows (.venv/Scripts/python.exe)."
  [root]
  (some (fn [v]
          (some (fn [rel]
                  (let [f (io/file root v rel)]
                    (when (.isFile f) (.getCanonicalPath f))))
                ["bin/python" "bin/python3" "Scripts/python.exe"]))
        [".venv" "venv"]))

(defn- uv-project?
  [root]
  (or (exists? root "uv.lock")
      (and (exists? root "pyproject.toml")
           (try (str/includes? (slurp (io/file root "pyproject.toml")) "[tool.uv")
                (catch Throwable _ false)))))

(defn resolve-command
  "The argv PREFIX that launches a project-aware Python in `root`. Detection
   order (first hit wins):
     1. uv      — uv.lock / [tool.uv] in pyproject + `uv` on PATH → [uv run python]
     2. poetry  — poetry.lock + `poetry` on PATH                  → [poetry run python]
     3. venv    — .venv/ or venv/ interpreter                     → [<abs path>]
     4. system  — python3, else python                            → [python3]"
  [root]
  (let [sys-py (if (on-path? "python3") "python3" (if (windows?) "python.exe" "python"))]
    (cond (and (uv-project? root) (on-path? "uv")) ["uv" "run" "python"]
          (and (exists? root "poetry.lock") (on-path? "poetry")) ["poetry" "run" "python"]
          (venv-python root) [(venv-python root)]
          :else [sys-py])))
