(ns com.blockether.vis.ext.language-python.ruff
  "`format_code` / `lint_code` for Python, backed by ruff (com.blockether/ruff).

   ruff runs IN-PROCESS: the Rust ruff_python_formatter / ruff_linter crates are
   linked as a cdylib and called over the FFM API. There is no `ruff` binary to
   install, no subprocess, no virtualenv, and no PATH lookup — the same code path
   works from the native image. Configuration is RUFF'S OWN discovery: for every
   source, ruff walks up from the file to the nearest `.ruff.toml` / `ruff.toml` /
   `pyproject.toml` with a `[tool.ruff]` table and honours it whole — `extend`,
   `per-file-ignores`, `target-version`, formatter options — so a run here and a
   `ruff` CLI run agree on every source it reads. With no config anywhere the
   tool still runs on ruff's defaults and SAYS SO in a `hint`.

   ONE divergence, the same one the sandbox `ruff` shim documents: the FFI is a
   one-source-one-call linter/formatter, so the WALK is ours and a config's
   `exclude` / `extend-exclude` globs are NOT applied to it — the noise
   directories in `skip-dirs` are pruned instead. `per-file-ignores` DO apply:
   ruff matches them itself against the path we hand it.

   Both handlers accept the SAME argument shapes as the Clojure pack:
   a code string, {\"code\"}, {\"path\"}, {\"paths\"}, or nothing (whole project)."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.ruff :as ruff]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.foundation.surface-contract :as contract]))

;; Paths

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

(defn- canon
  "Canonical absolute path: symlinks resolved, so a workspace reached through one
   (macOS `/tmp` -> `/private/tmp`) still matches the config file ruff found and
   the `per-file-ignores` globs anchored at it."
  ^String [^java.io.File f]
  (try (.getCanonicalPath f) (catch Exception _ (.getAbsolutePath f))))

(defn- under
  "Absolute path string for `p`, resolved against workspace `root` when relative."
  ^String [^java.io.File root p]
  (let [f (io/file (str p))]
    (canon (if (.isAbsolute f) f (io/file root (str p))))))

(defn relativize-path
  "`p` written relative to `root` when it lives under it, else left absolute."
  ^String [^java.io.File root p]
  (let
    [r
     (canon root)

     s
     (canon (io/file (str p)))]

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

;; Project config

(defn config-for
  "The ruff configuration file governing `p` (a file or a directory), or nil when
   the tree has none. Discovery is RUFF'S OWN — `.ruff.toml`, `ruff.toml`, then a
   `pyproject.toml` carrying a `[tool.ruff]` table, walking ancestors — so vis
   sees exactly the file the `ruff` CLI would use, `extend` chains and all."
  ^String [p]
  (let
    [f
     (io/file (str p))

     d
     (if (.isDirectory f) f (or (.getParentFile f) f))]

    (try (ruff/config-file (.getAbsolutePath d)) (catch Throwable _ nil))))

(defn config-finder
  "`config-for`, memoised per directory for ONE tool call: a project walk asks
   once per directory instead of once per file, and a config edited between calls
   is still picked up."
  []
  (let [cache (atom {})]
    (fn ^String [p]
      (let
        [f (io/file (str p))
         d (str (if (.isDirectory f) f (or (.getParentFile f) f)))]

        (if-let [hit (find @cache d)]
          (val hit)
          (let [v (config-for d)]
            (swap! cache assoc d v)
            v))))))

(def no-config-hint
  "What to tell the caller when the project pins nothing: ruff still ran, with
   ITS defaults, and the fix is a config file rather than a tool flag."
  (str "no ruff configuration found — ruff's own defaults were used "
       "(line-length 88, rules E4/E7/E9/F). Add a `ruff.toml` (or a `[tool.ruff]` "
       "table in `pyproject.toml`) at the project root to pin the line length, "
       "`select`/`ignore` and `target-version` for every run."))

(defn- call-opts
  "Explicit tool options only. Everything else — line length, rule selection,
   per-file ignores, target version — comes from the discovered configuration
   file, which ruff resolves itself; an explicit option OVERRIDES it."
  [arg]
  (let
    [opts
     (when (map? arg) arg)

     n
     (some-> (get opts "line_length")
             str
             parse-long)

     cfg
     (some-> (get opts "config")
             str
             not-empty)

     sel
     (get opts "select")

     ign
     (get opts "ignore")]

    (cond-> {}
      n
      (assoc :line-length n)

      cfg
      (assoc :config cfg)

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

(defn- with-config
  "`opts` aimed at ONE source: the discovered configuration for `abs` unless the
   caller passed `config` explicitly, plus `:path` so ruff sees the real file
   name (`.pyi` stubs format differently, `per-file-ignores` need it)."
  [opts find-config ^String abs]
  (cond-> opts
    abs
    (assoc :path abs)

    (not (:config opts))
    (assoc :config (find-config abs))))

;; Argument shapes (mirrors the Clojure pack)

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

(defn- expand-targets
  "Python files per REQUESTED target (`{target [abs …]}`), or the project's
   default source roots under `::default` when nothing was requested. Expanding
   once per target — instead of once over the union — is what lets an empty
   target be named back to the caller."
  [^java.io.File root targets]
  (if (seq targets)
    (into {} (map (juxt identity #(expand-python-files root [%]))) targets)
    {::default (expand-python-files root (discover-python-source-paths root))}))

(defn- target-files
  "Every Python file `expand-targets` found, sorted and de-duplicated."
  [expanded]
  (into [] (sort (distinct (mapcat val expanded)))))

(defn- empty-targets
  "Requested targets that carry NO Python: a README, a docs directory, a tree
   whose sources are all `.txt`. Zero files means zero findings and zero
   rewrites, which reads exactly like a clean project — so it is an ERROR."
  [expanded targets]
  (into [] (remove #(seq (get expanded %))) targets))

(defn- no-python-error
  [op empties]
  (extension/failure
    {:error
     {:message (str op
                    " target has no Python in it: "
                    (str/join ", " empties)
                    " — only .py/.pyi sources are read (directories are walked recursively)")
      :hint (str "pass a .py/.pyi file, or a directory that contains Python, or omit path/paths to "
                 op
                 " the whole project")}}))

;; format_code

(defn- format-one-file!
  [opts find-config ^String abs ^java.io.File root]
  (let
    [src
     (slurp abs)

     out
     (try (ruff/format src (with-config opts find-config abs)) (catch Throwable _ src))

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
   A named target that resolves to nothing is an ERROR, not a silent `changed 0`:
   both a path that does not exist and one that carries no Python at all.
   Rule set and wrap width come from the project's ruff configuration file
   (`ruff.toml` / `[tool.ruff]`), discovered by ruff itself per file; `config`
   pins one explicitly and `line_length` overrides it. Syntactically invalid
   Python is left VERBATIM rather than failing the call, so a formatter run never
   destroys a half-written file."
  ([arg] (py-format-fn nil arg))
  ([env arg]
   (let
     [root
      (io/file (or (:workspace/root env) "."))

      opts
      (call-opts arg)

      find-config
      (config-finder)

      code
      (arg-code arg)

      targets
      (arg-targets arg)

      missing
      (into [] (remove #(.exists (io/file (under root %))) targets))

      expanded
      (when-not (or code (seq missing)) (expand-targets root targets))

      empties
      (empty-targets expanded targets)]

     (cond (seq missing) (missing-error "format" missing)
           code (let
                  [cfg
                   (or (:config opts) (find-config (.getAbsolutePath root)))

                   out
                   (ruff/format-or code (assoc opts :config cfg))]

                  (extension/success {:result (contract/check
                                                :format-fn
                                                (cond->
                                                  {"op" "ruff-format"
                                                   "language" "python"
                                                   "changed" (not= out code)
                                                   "chars" (- (count out) (count code))
                                                   "formatter" "ruff"}
                                                  cfg
                                                  (assoc "config" (relativize-path root cfg))

                                                  (not cfg)
                                                  (assoc "hint" no-config-hint)))}))
           (seq empties) (no-python-error "format" empties)
           :else (let
                   [abs-files
                    (target-files expanded)

                    files
                    (mapv #(format-one-file! opts find-config % root) abs-files)

                    cfg
                    (or (:config opts)
                        (find-config (.getAbsolutePath root))
                        (some find-config abs-files))]

                   (extension/success
                     {:result (contract/check :format-fn
                                              (cond->
                                                {"op" "ruff-format"
                                                 "language" "python"
                                                 "files" files
                                                 "changed" (count (filter #(get % "changed") files))
                                                 "by-cwd" (reduce (fn [acc f]
                                                                    (let
                                                                      [p
                                                                       (io/file (get f "path"))

                                                                       d
                                                                       (or (some-> (.getParent p)
                                                                                   str)
                                                                           ".")]

                                                                      (assoc-in acc
                                                                        [d (.getName p)]
                                                                        (dissoc f "path"))))
                                                                  {}
                                                                  files)
                                                 "formatters" ["ruff"]}
                                                cfg
                                                (assoc "config" (relativize-path root cfg))

                                                (not cfg)
                                                (assoc "hint" no-config-hint)))}))))))

;; lint_code

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
   ERROR, not a silent `clean` — both a path that does not exist and one that
   holds no Python at all, because zero files read reads exactly like zero
   findings. The RULE SET is the project's ruff configuration file, discovered
   by ruff itself (`.ruff.toml` / `ruff.toml` / `[tool.ruff]` in
   `pyproject.toml`, nearest ancestor wins) and reported back as `config`; with
   no such file the run falls back to ruff's defaults (E4, E7, E9, F) and returns
   a `hint` telling the caller to add one. `config` pins a file explicitly and
   `select` / `ignore` (a string, or a list of selectors like [\"F\" \"B\" \"E501\"])
   override it. Findings carry the ruff code as `type`, `is_fixable`, and a
   `level` derived from the code: syntax/pyflakes fatals are errors, conventions
   (W/C/N/D/I/UP…) info, the rest warnings. `files` is how many files were
   LINTED (a snippet counts as 1), NOT how many carried findings, and `targets`
   echoes the requested path/paths relative to the workspace root: a CLEAN run
   still has to say what it looked at."
  ([arg] (py-lint-fn nil arg))
  ([env arg]
   (let
     [root
      (io/file (or (:workspace/root env) "."))

      opts
      (call-opts arg)

      find-config
      (config-finder)

      code
      (arg-code arg)

      targets
      (when-not code (arg-targets arg))

      missing
      (into [] (remove #(.exists (io/file (under root %))) targets))

      expanded
      (when-not (or code (seq missing)) (expand-targets root targets))

      empties
      (empty-targets expanded targets)]

     (cond (seq missing) (missing-error "lint" missing)
           (seq empties) (no-python-error "lint" empties)
           :else
           (let
             [abs-files
              (when-not code (target-files expanded))

              findings
              (if code
                (mapv #(->finding nil %)
                      (ruff/lint code
                                 (assoc opts
                                   :config (or (:config opts)
                                               (find-config (.getAbsolutePath root))))))
                (into []
                      (mapcat (fn [abs]
                                (let [rel (relativize-path root abs)]
                                  (map #(->finding rel %)
                                       (ruff/lint-or (slurp abs)
                                                     (with-config opts find-config abs)
                                                     [])))))
                      abs-files))

              cfg
              (or (:config opts) (find-config (.getAbsolutePath root)) (some find-config abs-files))

              by-level
              (frequencies (map #(get % "level") findings))]

             (extension/success
               {:result
                (contract/check
                  :lint-fn
                  (cond->
                    {"op" "ruff-lint"
                     "language" "python"
                     "error" (get by-level "error" 0)
                     "warning" (get by-level "warning" 0)
                     "info" (get by-level "info" 0)
                     "files" (if code 1 (count abs-files))
                     "findings" findings
                     "providers" ["ruff"]
                     "by-cwd"
                     (reduce (fn [acc f]
                               (let
                                 [p
                                  (io/file (get f "file" "<snippet>"))

                                  d
                                  (or (some-> (.getParent p)
                                              str)
                                      ".")]

                                 (update-in acc [d (.getName p) (get f "level")] (fnil conj []) f)))
                             {}
                             findings)}
                    code
                    (assoc "snippet" code)

                    (seq targets)
                    (assoc "targets" (mapv #(relativize-path root (under root %)) targets))

                    cfg
                    (assoc "config" (relativize-path root cfg))

                    (not cfg)
                    (assoc "hint" no-config-hint)))}))))))
