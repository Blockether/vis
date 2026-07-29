(ns com.blockether.vis.ext.language-python.ruff
  "`format_code` / `lint_code` for Python, backed by ruff (com.blockether/ruff).

   ruff runs IN-PROCESS: the Rust ruff_python_formatter / ruff_linter crates are
   linked as a cdylib and called over the FFM API. There is no `ruff` binary to
   install, no subprocess, no virtualenv, and no PATH lookup — the same code path
   works from the native image. Because it is in-process, ruff's own config
   discovery is NOT used: `pyproject.toml` / `ruff.toml` are read HERE (see
   `project-ruff-opts`), which keeps the behaviour identical whichever directory
   the tool is invoked from.

   Both handlers accept the SAME argument shapes as the Clojure pack:
   a code string, {\"code\"}, {\"path\"}, {\"paths\"}, or nothing (whole project)."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.ruff :as ruff]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.foundation.surface-contract :as contract]))

;; =============================================================================
;; Paths
;; =============================================================================

(def ^:private python-exts #{"py" "pyi"})

(def ^:private skip-dirs
  "Directories never walked: virtualenvs, caches, build output, vendored deps.
   Formatting `.venv/lib/python3.12/site-packages` would rewrite thousands of
   third-party files — that is a data-loss bug, not slow."
  #{".git" ".hg" ".svn" ".venv" "venv" "env" "__pycache__" ".mypy_cache" ".pytest_cache"
    ".ruff_cache" ".tox" ".nox" ".eggs" "site-packages" "node_modules" "build" "dist" "target"
    ".gradle" ".idea" ".vis"})

(defn- python-file?
  [^java.io.File f]
  (and (.isFile f)
       (let
         [n
          (.getName f)

          i
          (.lastIndexOf n ".")]

         (and (pos? i) (contains? python-exts (subs n (inc i)))))))

(defn- under
  "Absolute path string for `p`, resolved against workspace `root` when relative."
  ^String [^java.io.File root p]
  (let [f (io/file (str p))]
    (str (if (.isAbsolute f) f (io/file root (str p))))))

(defn relativize-path
  "`p` written relative to `root` when it lives under it, else left absolute."
  ^String [^java.io.File root p]
  (let
    [r
     (str (.getAbsolutePath root))

     s
     (str (.getAbsolutePath (io/file (str p))))]

    (cond (= s r) "."
          (str/starts-with? s (str r java.io.File/separator)) (subs s (inc (count r)))
          :else s)))

(defn expand-python-files
  "Absolute paths of every Python source file under `targets` (files kept as-is,
   DIRECTORIES walked recursively, `skip-dirs` pruned). Sorted + distinct."
  [^java.io.File root targets]
  (into []
        (comp (distinct) (map str))
        (sort (distinct (mapcat (fn [t]
                                  (let [f (io/file (under root t))]
                                    (cond (python-file? f) [(.getAbsolutePath f)]
                                          (.isDirectory f)
                                          (->> (tree-seq (fn [^java.io.File d]
                                                           (and (.isDirectory d)
                                                                (not (contains? skip-dirs
                                                                                (.getName d)))))
                                                         (fn [^java.io.File d]
                                                           (seq (.listFiles d)))
                                                         f)
                                               (filter python-file?)
                                               (mapv #(.getAbsolutePath ^java.io.File %)))
                                          :else [])))
                                targets)))))

(defn discover-python-source-paths
  "Default targets when neither `path` nor `paths` is given: the conventional
   source roots that exist (`src`, the workspace root's top-level packages,
   `tests`), falling back to the whole workspace root."
  [^java.io.File root]
  (let [named (into [] (filter #(.isDirectory (io/file root %))) ["src" "tests" "test"])]
    (if (seq named) (mapv #(str (io/file root %)) named) [(str root)])))

;; =============================================================================
;; Project config
;; =============================================================================

(defn- config-line-length
  "`line-length = N` under a `[tool.ruff]` (pyproject.toml) or top-level
   (ruff.toml) table. Deliberately a light scan: ruff's own discovery does not
   run in-process, and vis carries no TOML parser."
  [^String text in-tool-table?]
  (let [lines (str/split-lines (or text ""))]
    (loop
      [[l & more] lines
       in? (not in-tool-table?)]

      (when l
        (let [t (str/trim l)]
          (cond (str/starts-with? t "[")
                (recur more
                       (if in-tool-table? (= t "[tool.ruff]") (not (str/starts-with? t "[tool."))))
                (and in? (re-find #"^line[-_]length\s*=\s*\d+" t)) (parse-long (re-find #"\d+" t))
                :else (recur more in?)))))))

(defn project-ruff-opts
  "ruff options read from the project itself: `ruff.toml` / `.ruff.toml`, else
   `[tool.ruff]` in `pyproject.toml`. Only `line-length` is honoured today —
   it is the one setting whose absence visibly mangles a file (ruff would
   re-wrap at 88 against a project that standardised on 100/120)."
  [^java.io.File root]
  (let
    [from (fn [name in-tool?]
            (let [f (io/file root name)]
              (when (.isFile f) (config-line-length (slurp f) in-tool?))))]
    (when-let
      [n (or (from "ruff.toml" false) (from ".ruff.toml" false) (from "pyproject.toml" true))]
      {:line-length n})))

(defn- call-opts
  "Merge explicit tool options over the project's config."
  [^java.io.File root arg]
  (let
    [opts
     (when (map? arg) arg)

     n
     (some-> (get opts "line_length")
             str
             parse-long)

     sel
     (get opts "select")

     ign
     (get opts "ignore")]

    (cond-> (or (project-ruff-opts root) {})
      n
      (assoc :line-length n)

      (seq (if (coll? sel)
             sel
             (some-> sel
                     str
                     not-empty)))
      (assoc :select sel)

      (seq (if (coll? ign)
             ign
             (some-> ign
                     str
                     not-empty)))
      (assoc :ignore ign))))

;; =============================================================================
;; Argument shapes (mirrors the Clojure pack)
;; =============================================================================

(defn- arg-code
  "The code snippet an `arg` carries, or nil. A BLANK `\"code\": \"\"` never
   shadows a real `path`/`paths`: models routinely emit every schema key with an
   empty default, and formatting/linting an empty snippet silently skips the file."
  [arg]
  (cond (string? arg) (when-not (str/blank? arg) arg)
        (and (map? arg) (not (str/blank? (str (get arg "code"))))) (str (get arg "code"))
        :else nil))

(defn- arg-targets
  "`path` and `paths` UNIONED (not shadowing), as raw (unresolved) strings."
  [arg]
  (when (map? arg)
    (let
      [p
       (get arg "path")

       ps
       (get arg "paths")]

      (into []
            (distinct (concat (when-not (str/blank? (str p)) [(str p)])
                              (when (coll? ps) (map str ps))))))))

(defn- missing-error
  [op missing]
  (extension/failure {:error {:message (str op
                                            " target does not exist: "
                                            (str/join ", " missing)
                                            " — relative paths resolve against the workspace root")
                              :hint (str "pass an existing .py file/dir, or omit path/paths to "
                                         op
                                         " the whole project")}}))

;; =============================================================================
;; format_code
;; =============================================================================

(defn- format-one-file!
  [opts ^String abs ^java.io.File root]
  (let
    [src
     (slurp abs)

     out
     (try (ruff/format src opts) (catch Throwable _ src))

     changed?
     (not= out src)]

    (when changed? (spit abs out))
    {"path" (relativize-path root abs) "changed" changed? "formatter" "ruff"}))

(defn py-format-fn
  "Format Python source with ruff via the language facade (`format_code`).
   Accepts:
     - a raw code string / {\"code\": ...}  -> report changed? + char delta (NO text)
     - {\"path\": \"src/foo.py\"}             -> format that file IN PLACE
     - {\"paths\": [\"src\" \"tests\"]}         -> format those paths IN PLACE; a
         DIRECTORY is walked RECURSIVELY (every .py/.pyi under it, minus venv /
         cache / build dirs)
     - nothing / {}                        -> format the project's source roots
   `line_length` (or the project's ruff.toml / [tool.ruff] line-length) sets the
   wrap width. Syntactically invalid Python is left VERBATIM rather than failing
   the call, so a formatter run never destroys a half-written file."
  ([arg] (py-format-fn nil arg))
  ([env arg]
   (let
     [root
      (io/file (or (:workspace/root env) "."))

      opts
      (call-opts root arg)

      code
      (arg-code arg)

      targets
      (arg-targets arg)

      missing
      (into [] (remove #(.exists (io/file (under root %))) targets))]

     (cond (seq missing) (missing-error "format" missing)
           code (let [out (ruff/format-or code opts)]
                  (extension/success {:result (contract/check :format-fn
                                                              {"op" "ruff-format"
                                                               "language" "python"
                                                               "changed" (not= out code)
                                                               "chars" (- (count out) (count code))
                                                               "formatter" "ruff"})}))
           :else (let
                   [files (mapv #(format-one-file! opts % root)
                                (expand-python-files
                                  root
                                  (if (seq targets) targets (discover-python-source-paths root))))]
                   (extension/success
                     {:result (contract/check :format-fn
                                              {"op" "ruff-format"
                                               "language" "python"
                                               "files" files
                                               "changed" (count (filter #(get % "changed") files))
                                               "by-dir" (reduce (fn [acc f]
                                                                  (let
                                                                    [p (io/file (get f "path"))
                                                                     d (or (some-> (.getParent p)
                                                                                   str)
                                                                           ".")]

                                                                    (assoc-in acc
                                                                      [d (.getName p)]
                                                                      (dissoc f "path"))))
                                                                {}
                                                                files)
                                               "formatters" ["ruff"]})}))))))

;; =============================================================================
;; lint_code
;; =============================================================================

(def ^:private error-code-re
  "ruff codes that are genuine BREAKAGE, not style: a parse failure, the E9
   syntax family, or a pyflakes fatal — an undefined name (F82x), an always-false
   comparison / `assert (a, b)` (F63x), or invalid syntax in a doctest / `break`
   outside a loop (F7xx). Unused imports (F401) and friends are WARNINGS: they
   are real findings, but the file still runs, and inflating the error count
   makes a model treat a tidy-up as a broken build."
  #"^(invalid-syntax|E9\d*|F(6\d\d|7\d\d|82\d))$")

(defn- level-for
  [^String code]
  (cond (str/blank? code) "warning"
        (re-find error-code-re code) "error"
        ;; W/C/N/D/I/UP… are conventions — advisory, and must not inflate the error
        ;; count the model reads as \"this build is broken\".
        (re-find #"^(W|C|N|D|I|Q|COM|ANN|TID|RUF1)" code) "info"
        :else "warning"))

(defn- ->finding
  [file d]
  (cond->
    {"level" (level-for (:code d))
     "type" (:code d)
     "message" (:message d)
     "row" (:row d)
     "col" (:col d)
     "provider" "ruff"
     "is_fixable" (boolean (:is-fixable d))}
    file
    (assoc "file" file)))

(defn py-lint-fn
  "Lint Python with ruff via the language facade (`lint_code`). Accepts:
     - a raw code string / {\"code\": ...} -> lint the snippet
     - {\"path\": \"src/foo.py\"}            -> lint that file
     - {\"paths\": [\"src\" \"tests\"]}        -> lint those paths (dirs recursive)
     - nothing / {}                       -> lint the project's source roots
   `path` and `paths` are UNIONED; a target that resolves to nothing is an
   ERROR, not a silent `clean`. `select` / `ignore` (a string, or a list of rule
   selectors like [\"F\" \"B\" \"E501\"]) narrow the rule set; omitted, ruff's own
   default selection (E4, E7, E9, F) runs. Findings carry the ruff code as
   `type`, `is_fixable`, and a `level` derived from the code: syntax/pyflakes
   fatals are errors, conventions (W/C/N/D/I/UP…) info, the rest warnings."
  ([arg] (py-lint-fn nil arg))
  ([env arg]
   (let
     [root
      (io/file (or (:workspace/root env) "."))

      opts
      (call-opts root arg)

      code
      (arg-code arg)

      targets
      (when-not code (arg-targets arg))

      missing
      (into [] (remove #(.exists (io/file (under root %))) targets))]

     (if (seq missing)
       (missing-error "lint" missing)
       (let
         [findings
          (if code
            (mapv #(->finding nil %) (ruff/lint code opts))
            (into []
                  (mapcat (fn [abs]
                            (let [rel (relativize-path root abs)]
                              (map #(->finding rel %) (ruff/lint-or (slurp abs) opts [])))))
                  (expand-python-files
                    root
                    (if (seq targets) targets (discover-python-source-paths root)))))

          by-level
          (frequencies (map #(get % "level") findings))]

         (extension/success
           {:result (contract/check
                      :lint-fn
                      {"op" "ruff-lint"
                       "language" "python"
                       "error" (get by-level "error" 0)
                       "warning" (get by-level "warning" 0)
                       "info" (get by-level "info" 0)
                       "files" (count (distinct (keep #(get % "file") findings)))
                       "findings" findings
                       "providers" ["ruff"]
                       "by-dir"
                       (reduce
                         (fn [acc f]
                           (let
                             [p
                              (io/file (get f "file" "<snippet>"))

                              d
                              (or (some-> (.getParent p)
                                          str)
                                  ".")]

                             (update-in acc [d (.getName p) (get f "level")] (fnil conj []) f)))
                         {}
                         findings)})}))))))
