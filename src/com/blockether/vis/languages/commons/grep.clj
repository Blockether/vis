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
  "Global cap on returned matches across the whole grep call."
  500)

(def ^:private default-max-line-chars
  "Max characters per matching line in the result. Longer lines are truncated
   with a trailing ellipsis so one huge minified file cannot blow the budget."
  400)

(def ^:private default-max-file-bytes
  "Skip any file larger than this (2 MB). Keeps grep snappy and prevents the
   LLM from accidentally slurping a 500MB core.basis or index file."
  (* 2 1024 1024))

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
   `(?i)` mode at compile-time."
  ^Pattern [p case-insensitive?]
  (let [src (pattern-source p)
        flags (if case-insensitive? Pattern/CASE_INSENSITIVE 0)]
    (try
      (Pattern/compile ^String src ^int flags)
      (catch Exception e
        ;; Surface compile errors inside an ex-info so the RLM error-hint path
        ;; can feed them back to the LLM as actionable feedback.
        (throw (ex-info (str "grep pattern failed to compile: " (ex-message e))
                 {:type :tool/invalid-input :tool 'grep
                  :pattern src
                  :case-insensitive? case-insensitive?}
                 e))))))

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
       :max-matches       — Hard cap on returned matches (global). Default 500.
       :max-line-chars    — Per-line char cap before truncation. Default 400.

   Returns a map:
   - :path       — Canonical root directory path.
   - :pattern    — The pattern source that was searched.
   - :matches    — Vector of {:path rel-str :line int :text str}.
   - :files      — Number of files scanned.
   - :truncated? — true when global cap was hit."
  ([pattern] (grep pattern "." nil))
  ([pattern path] (grep pattern path nil))
  ([pattern path opts]
   (let [{:keys [glob depth case-insensitive? max-matches max-line-chars]
          :or   {depth 20
                 case-insensitive? false
                 max-matches default-max-matches
                 max-line-chars default-max-line-chars}} (or opts {})
         root (io/file (or path "."))
         _    (when-not (.exists root)
                (throw (ex-info (str "Path not found: " path)
                         {:type :tool/invalid-input :tool 'grep :path path})))
         _    (when-not (.isDirectory root)
                (throw (ex-info (str "grep path must be a directory: " path
                                  ". Use read-file to search inside a single file.")
                         {:type :tool/invalid-input :tool 'grep :path path})))
         pat  (->re2j pattern case-insensitive?)
         ;; Default: walk every file under `root` to the given depth. Only
         ;; pass `glob` down when the caller wants it — list-dir's glob
         ;; matcher treats `**/*` as "must contain a /" which drops root-level
         ;; files like README.md. Leaving `glob` nil matches everything.
         listing (list-cmd/list-dir (.getCanonicalPath root)
                   (cond-> {:depth depth :limit 5000}
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
     {:path       (.getCanonicalPath root)
      :pattern    (.pattern pat)
      :matches    all-matches
      :files      (count files)
      :truncated? truncated?})))

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
     :group "Filesystem" :activation-doc "always active"
     :examples ["(grep \"HITL\" \"src\")"
                "(grep \"approval|confirm\" \"src\" {:glob \"**/*.clj\" :case-insensitive? true})"
                "(grep #\"\\bhuman-in-the-loop\\b\" \"src\" {:max-matches 50})"]}))
