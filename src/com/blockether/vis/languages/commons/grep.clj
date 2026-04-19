(ns com.blockether.vis.languages.commons.grep
  "Base GREP tool for RLM agents.
   Recursively search files for a regex pattern, returning `path:line:text`
   matches. Supports glob-based path filtering and per-file match caps. Built
   on top of `list-dir` so it honors the same symlink/permission rules.

   Regex engine: **google/re2j** (linear-time, ReDoS-safe). Agent-authored
   patterns never run `java.util.regex`'s backtracker, so a pathological
   pattern like `(a+)+$` on adversarial input still terminates in linear time.
   re2j covers ~99% of practical regex syntax; it deliberately drops
   backreferences, PCRE lookarounds, and a few rarely-used flags — none of
   which agents typically need for code search."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.languages.commons.list :as list-cmd]
            [com.blockether.vis.loop.tool :as sci-tool])
  (:import [com.google.re2j Pattern Matcher]
           [java.io File]))

;; Note: `Pattern` here is `com.google.re2j.Pattern`, the linear-time engine.
;; `java.util.regex.Pattern` is used only by caller-supplied `#\"...\"` literals
;; and is referenced by its fully-qualified name so it never collides.
(set! *warn-on-reflection* true)

;;; ── Defaults ──────────────────────────────────────────────────────────

(def ^:private default-max-matches
  "Soft default cap on returned matches across the whole grep call.
   The LLM is expected to set `:max-matches` explicitly when the default
   isn't right — larger for broad audits, smaller for peek-only searches.
   500 is a safe starting point for code search in a mid-sized repo."
  500)

(def ^:private default-max-line-chars
  "Max characters per matching line in the result. Longer lines are truncated
   with a trailing ellipsis so one huge minified file cannot blow the budget."
  400)

(def ^:private default-max-file-bytes
  "Skip any file larger than this (2 MB). Keeps grep snappy and prevents the
   LLM from accidentally slurping a 500MB core.basis or index file."
  (* 2 1024 1024))

(def default-ignore-dirs
  "Directory names pruned by default. Pass `:ignore-dirs` to override
   (empty set disables). Matched by exact segment name anywhere in the tree."
  #{".git" ".hg" ".svn"
    ".cache" ".cpcache" ".gitlibs" ".m2" ".shadow-cljs"
    ".clj-kondo" ".lsp" ".calva" "target"
    "node_modules" ".next" ".nuxt" ".turbo" ".yarn" ".pnpm-store"
    "__pycache__" ".venv" "venv" ".tox" ".mypy_cache" ".pytest_cache" ".ruff_cache"
    ".idea" ".vscode"
    "coverage" ".nyc_output"
    ".terraform" ".vercel" ".netlify"
    "DerivedData" "Pods"})

;;; ── Pattern coercion ──────────────────────────────────────────────────

(defn- pattern-source
  "Best-effort source extraction from whatever the caller passed in. re2j's
   `Pattern.compile` takes a source string — if we get a string, it's already
   the source; if we get a `java.util.regex.Pattern` literal (`#\"...\"`)
   or a re2j Pattern, ask it for its pattern string. Anything else is a
   programmer error."
  ^String [p]
  (cond
    (string? p) p
    (instance? Pattern p) (.pattern ^Pattern p)
    (instance? java.util.regex.Pattern p) (.pattern ^java.util.regex.Pattern p)
    :else
    (throw (ex-info (str "grep pattern must be a string or regex Pattern, got: " (type p))
             {:type :tool/invalid-input :tool 'grep :got p}))))

(defn- ->re2j
  "Coerce `p` to a `com.google.re2j.Pattern`. `case-insensitive?` toggles
   `(?i)` mode at compile-time.

   Returns `{:pattern <Pattern> :mode :regex|:literal-fallback :source <str>}`
   so callers can report which interpretation actually ran.

   Forgiveness: when `p` is a string AND regex compile fails, we quote the
   string and retry as a literal-match pattern. A real `#\"...\"` literal
   bypasses the fallback — the caller clearly meant regex, so compile
   errors surface as thrown ex-infos. Same for `:literal? true` opt
   (coming from the caller, not handled here — `grep` inlines that)."
  [p case-insensitive?]
  (let [src   (pattern-source p)
        flags (if case-insensitive? Pattern/CASE_INSENSITIVE 0)
        is-string-pattern? (string? p)]
    (try
      {:pattern (Pattern/compile ^String src ^int flags)
       :mode    :regex
       :source  src}
      (catch Exception e
        (if is-string-pattern?
          ;; String pattern with a regex compile error — user likely meant
          ;; literal text (e.g. `"*query*"`, `"foo(bar)"`). Retry as
          ;; Pattern.quote()'d literal and flag the fallback so the tool's
          ;; output teaches the LLM what happened.
          (let [quoted (Pattern/quote src)]
            (try
              {:pattern (Pattern/compile ^String quoted ^int flags)
               :mode    :literal-fallback
               :source  src}
              (catch Exception e2
                (throw (ex-info (str "grep pattern failed to compile as regex AND as literal: "
                                  (ex-message e2))
                         {:type :tool/invalid-input :tool 'grep
                          :pattern src
                          :case-insensitive? case-insensitive?}
                         e2)))))
          (throw (ex-info (str "grep pattern failed to compile: " (ex-message e))
                   {:type :tool/invalid-input :tool 'grep
                    :pattern src
                    :case-insensitive? case-insensitive?}
                   e)))))))

(defn- clip
  "Clip a line to `n` chars, appending ' …' if the line was truncated."
  ^String [^String line n]
  (if (> (.length line) n)
    (str (.substring line 0 n) " …")
    line))

;;; ── Core ──────────────────────────────────────────────────────────────

(defn- scan-file
  "Scan one file for `pattern`. Returns a realized vector of match maps
   `{:path :line :text}`. Silently skips unreadable/too-large files."
  [^File f ^Pattern pattern rel-path max-line-chars]
  (try
    (when (and (.isFile f)
            (<= (.length f) default-max-file-bytes))
      (with-open [rdr (io/reader f :encoding "UTF-8")]
        (let [matcher (.matcher pattern "")]
          (loop [idx 0
                 lines (line-seq rdr)
                 acc   (transient [])]
            (if-let [^String line (first lines)]
              (let [found? (-> matcher (.reset ^CharSequence line) .find)
                    acc'   (if found?
                             (conj! acc {:path rel-path
                                         :line (inc idx)
                                         :text (clip line max-line-chars)})
                             acc)]
                (recur (inc idx) (rest lines) acc'))
              (persistent! acc))))))
    (catch Exception _ nil)))

(defn grep
  "Recursively search files for a regex pattern.

   Params:
   - pattern — Regex source. Accepts a plain string (\"HITL|human-in-the-loop\"),
     or a Java regex literal (`#\"...\"`) whose source string is re-compiled
     under the re2j engine. re2j syntax: RE2 flavor — no backreferences, no
     PCRE lookarounds; everything else works.
   - path    — Root directory (defaults to current working directory).
   - opts    — Map of options:
       :glob              — Glob pattern for candidate files. When absent,
                            every file under `path` is scanned.
       :depth             — Max recursion depth. Defaults to 20.
       :case-insensitive? — Toggle `(?i)` flag. Defaults to false.
       :max-matches       — Global cap on returned matches. The LLM is
                            expected to set this explicitly (larger for
                            broad audits, smaller for peek searches).
                            Defaults to 500.
       :max-line-chars    — Per-line char cap before truncation. Default 400.
       :ignore-dirs       — Directory-name set to prune during the walk.
                            Defaults to `default-ignore-dirs` (VCS, build
                            caches, node_modules, venvs, IDE state, etc.).
                            Pass an empty set `#{}` to disable pruning
                            entirely. Pass a custom set to replace — not
                            merge with — the default.

   Returns a map:
   - :path       — Canonical root directory path.
   - :pattern    — The pattern source that was searched.
   - :matches    — Vector of {:path rel-str :line int :text str}.
   - :files      — Number of files scanned.
   - :truncated? — true when global cap was hit."
  ([pattern] (grep pattern "." nil))
  ([pattern path] (grep pattern path nil))
  ([pattern path opts]
   (let [{:keys [glob depth case-insensitive? max-matches max-line-chars literal?
                 ignore-dirs]
          :or   {depth 20
                 case-insensitive? false
                 max-matches default-max-matches
                 max-line-chars default-max-line-chars
                 literal? false
                 ignore-dirs default-ignore-dirs}} (or opts {})
         root (io/file (or path "."))
         _    (when-not (.exists root)
                (throw (ex-info (str "Path not found: " path)
                         {:type :tool/invalid-input :tool 'grep :path path})))
         _    (when-not (.isDirectory root)
                (throw (ex-info (str "grep path must be a directory: " path
                                  ". Use read-file to search inside a single file.")
                         {:type :tool/invalid-input :tool 'grep :path path})))
         ;; `:literal? true` forces Pattern.quote() — caller wants a plain
         ;; text match, no regex interpretation. Otherwise `->re2j` tries
         ;; regex first and falls back to literal for compile errors on
         ;; string inputs. Fallback is reported as `:mode :literal-fallback`
         ;; on the output so the LLM learns.
         pre-source (pattern-source pattern)
         coerced (if literal?
                   {:pattern (Pattern/compile
                               (Pattern/quote pre-source)
                               (if case-insensitive? Pattern/CASE_INSENSITIVE 0))
                    :mode :literal-forced
                    :source pre-source}
                   (->re2j pattern case-insensitive?))
         pat  (:pattern coerced)
         ;; Default: walk every file under `root` to the given depth. Only
         ;; pass `glob` down when the caller wants it — list-dir's glob
         ;; matcher treats `**/*` as "must contain a /" which drops root-level
         ;; files like README.md. Leaving `glob` nil matches everything.
         listing (list-cmd/list-dir (.getCanonicalPath root)
                   (cond-> {:depth depth :limit 5000
                            :ignore-dirs ignore-dirs}
                     glob (assoc :glob glob)))
         files (->> (:entries listing)
                 (filter #(= "file" (:type %)))
                 (map #(io/file (.getCanonicalPath root) (:name %))))
         all-matches
         (persistent!
           (reduce (fn [acc ^File f]
                     (if (>= (count acc) max-matches)
                       (reduced acc)
                       (let [rel (str (.relativize (.toPath root) (.toPath f)))
                             hits (scan-file f pat rel max-line-chars)]
                         (reduce (fn [a hit]
                                   (if (>= (count a) max-matches)
                                     (reduced a)
                                     (conj! a hit)))
                           acc
                           (or hits [])))))
             (transient [])
             files))
         truncated? (>= (count all-matches) max-matches)]
     (cond-> {:path       (.getCanonicalPath root)
              :pattern    (.pattern ^Pattern pat)
              :matches    all-matches
              :files      (count files)
              :truncated? truncated?}
       ;; Surface the pattern mode when it's NOT plain regex so the LLM
       ;; knows whether its regex was honored or silently literalized.
       (not= :regex (:mode coerced))
       (assoc :pattern-mode (:mode coerced)
         :pattern-source (:source coerced))))))

;;; ── Validators ────────────────────────────────────────────────────────

(defn- validate-grep-input
  [{:keys [args]}]
  (let [[pattern path opts & extra] args]
    (when (seq extra)
      (throw (ex-info "grep expects 1-3 positional args: (grep pattern [path] [opts])"
               {:type :tool/invalid-input :tool 'grep :args args})))
    (when-not (or (string? pattern)
                (instance? Pattern pattern)
                (instance? java.util.regex.Pattern pattern))
      (throw (ex-info "grep pattern must be a string or regex Pattern"
               {:type :tool/invalid-input :tool 'grep :got pattern :got-type (type pattern)})))
    (when (and (some? path) (not (string? path)))
      (throw (ex-info "grep path must be a string when provided"
               {:type :tool/invalid-input :tool 'grep :got path :got-type (type path)})))
    (when (and (some? opts) (not (map? opts)))
      (throw (ex-info "grep opts must be a map when provided"
               {:type :tool/invalid-input :tool 'grep :got opts :got-type (type opts)})))
    {:args (vec args)}))

(defn- validate-grep-output
  [{:keys [result]}]
  (when-not (map? result)
    (throw (ex-info "grep must return a map"
             {:type :tool/invalid-output :tool 'grep :got-type (type result)})))
  (when-not (vector? (:matches result))
    (throw (ex-info "grep output :matches must be a vector"
             {:type :tool/invalid-output :tool 'grep :result result})))
  (when-not (string? (:pattern result))
    (throw (ex-info "grep output :pattern must be a string"
             {:type :tool/invalid-output :tool 'grep :result result})))
  {:result result})

;;; ── Tool definition ────────────────────────────────────────────────────

(def tool-def
  (sci-tool/make-tool-def
    'grep
    grep
    {:doc (:doc (meta #'grep))
     :arglists (:arglists (meta #'grep))
     :validate-input validate-grep-input
     :validate-output validate-grep-output
     :activation-fn (constantly true)
     :group "filesystem" :activation-doc "always active"
     :examples ["(grep \"HITL\" \"src\")"
                "(grep \"approval|confirm\" \"src\" {:glob \"**/*.clj\" :case-insensitive? true})"
                "(grep \"*query*\" \"src\" {:literal? true})"
                "(grep #\"\\bhuman-in-the-loop\\b\" \"src\" {:max-matches 50})"
                "(grep \"[TODO]\" \".\" {:literal? true})"
                "(grep \"password\" \".\" {:ignore-dirs #{}})  ;; disable pruning when you NEED the caches"
                "(grep \"FIXME\" \"src\" {:max-matches 2000})  ;; broad audit, override default cap"]
     :prompt "Recursively search files for a pattern. First stop for \"where is X used?\", \"who touches this function?\", \"find all callers\".

Pattern modes:
- String pattern is tried as regex; if it fails to compile, grep retries as literal text and marks `:pattern-mode :literal-fallback` in the output.
- `#\"...\"` pattern is always regex; compile errors throw.
- Pass `{:literal? true}` when you want exact text containing regex metacharacters (`* + ? ( ) [ ] { } | ^ $ . \\`). Always use literal mode for user-supplied text — most users don't mean regex.

Defaults you should consciously override:
- `:max-matches` defaults to 500. That's fine for a focused lookup; bump it when you're auditing (`:max-matches 2000+`), lower it when you only need a first sighting (`:max-matches 20`). Don't assume 500 is always right — tune per task.
- `:ignore-dirs` prunes junk directories (VCS, build caches, node_modules, venvs, IDE state, coverage, OS metadata) BEFORE they're ever walked. Pass `{:ignore-dirs #{}}` to search caches too, or a custom set to replace the default.

Prefer one grep with an alternation (`\"foo|bar|baz\"`) over three separate greps. Narrow with `:glob \"**/*.clj\"` instead of walking the whole repo."}))
