(ns com.blockether.vis.languages.commons.read
  "Base READ tool for RLM agents.
   Reads files with optional offset (line) and limit (line count).
   Returns raw content; line numbers are added by format-result for display."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.loop.tool :as sci-tool]))

;;; ── Safety ─────────────────────────────────────────────────────────────

(def ^:private max-file-size
  "Max file size to read without offset+limit (10 MB)."
  (* 10 1024 1024))

(def FULL_FILE_THRESHOLD
  "Files with at most this many lines are returned in full when no explicit
   offset/limit is provided. Deterministic default — no probabilistic
   trimming, no model-side heuristic. 1500 lines keeps context
   manageable and forces pagination on larger files."
  1500)

(def PARTIAL_READ_LIMIT
  "When a file exceeds FULL_FILE_THRESHOLD lines and the caller didn't
   specify offset/limit, return this many lines starting at line 1.
   Callers page through larger files with explicit offset."
  1500)

(defn- validate-path!
  "Validate file path: must exist, not a directory, file itself not a symlink."
  [path]
  (let [f (io/file path)]
    (when-not (.exists f)
      (throw (ex-info (str "File not found: " path) {:path path :error :not-found})))
    (when (.isDirectory f)
      (throw (ex-info (str "Path is a directory: " path ". Use list-dir instead.")
               {:path path :error :is-directory})))
    (when (java.nio.file.Files/isSymbolicLink (.toPath f))
      (throw (ex-info (str "Refusing to follow symlink: " path) {:path path :error :symlink})))
    f))

;;; ── Language detection ───────────────────────────────────────────────

(def ^:private ext->lang
  "Map file extensions (lower-case, with dot) to highlight.js language ids."
  {".clj"  "clojure"  ".cljs" "clojure"  ".cljc" "clojure"  ".edn" "clojure"
   ".js"   "javascript" ".mjs" "javascript" ".cjs" "javascript" ".jsx" "javascript"
   ".ts"   "typescript" ".tsx" "typescript" ".mts" "typescript"
   ".py"   "python"    ".pyi" "python"    ".pyw" "python"
   ".rb"   "ruby"      ".rake" "ruby"
   ".java" "java"      ".kt"  "kotlin"    ".scala" "scala"   ".groovy" "groovy"
   ".go"   "go"
   ".rs"   "rust"
   ".c"    "c"         ".h"   "c"
   ".cpp"  "cpp"       ".cc"  "cpp"       ".cxx" "cpp"      ".hpp" "cpp"
   ".cs"   "csharp"
   ".swift" "swift"    ".m"   "objectivec"
   ".css"  "css"       ".scss" "scss"     ".less" "less"
   ".html" "html"      ".htm" "html"
   ".xml"  "xml"       ".svg" "xml"       ".xsl" "xml"
   ".json" "json"      ".jsonc" "json"
   ".yaml" "yaml"      ".yml" "yaml"
   ".toml" "toml"
   ".md"   "markdown"  ".mdx" "markdown"
   ".sql"  "sql"
   ".sh"   "bash"      ".bash" "bash"     ".zsh" "bash"
   ".ps1"  "powershell"
   ".r"    "r"
   ".lua"  "lua"
   ".php"  "php"
   ".pl"   "perl"      ".pm"  "perl"
   ".ex"   "elixir"    ".exs" "elixir"
   ".hs"   "haskell"
   ".ml"   "ocaml"     ".mli" "ocaml"
   ".tf"   "hcl"       ".hcl" "hcl"
   ".ini"  "ini"       ".cfg" "ini"       ".properties" "ini"
   ".vim"  "vim"
   ".el"   "lisp"      ".lisp" "lisp"    ".scm" "scheme"
   ".proto" "protobuf"
   ".diff" "diff"      ".patch" "diff"
   ".graphql" "graphql" ".gql" "graphql"
   ".cmake" "cmake"
   ".nix"  "nix"
   ".zig"  "zig"
   ".dart" "dart"})

(def ^:private filename->lang
  "Exact filename matches for files without a meaningful extension."
  {"Dockerfile"    "dockerfile"
   "Makefile"      "makefile"
   "Rakefile"      "ruby"
   "Gemfile"       "ruby"
   "CMakeLists.txt" "cmake"
   "Vagrantfile"   "ruby"
   ".gitignore"    "plaintext"
   ".dockerignore" "plaintext"})

(defn- path->lang
  "Derive highlight.js language identifier from file path. Returns nil for unknown."
  [^java.io.File f]
  (let [name (.getName f)]
    (or (get filename->lang name)
        (let [dot (.lastIndexOf name ".")]
          (when (pos? dot)
            (get ext->lang (str/lower-case (subs name dot))))))))

;;; ── Core ───────────────────────────────────────────────────────────────

(defn read-file
  "Read a file with optional offset and limit.

   Params:
   - path   — File path (string, required)
   - offset — Starting line number, 1-based (int, optional). When omitted,
              the tool auto-pages based on file size (see below).
   - limit  — Max lines to return (int, optional). Must be >= 1 when given.

   Auto-pagination (default when NO offset AND NO limit are passed):
   - Files with ≤ FULL_FILE_THRESHOLD (1500) lines return in full.
   - Files with > FULL_FILE_THRESHOLD lines return the first
     PARTIAL_READ_LIMIT (1500) lines. The footer marks the result as
     `auto-paged` so the caller knows to pass explicit offset/limit to
     fetch the remainder.

   Callers who want a specific range pass `offset` and/or `limit`
   explicitly; the auto-pagination path is then skipped.

   Returns map `{:content raw-string :lang highlight-js-id-or-nil}`."
  ([path] (read-file path nil nil))
  ([path offset] (read-file path offset nil))
  ([path offset limit]
   (let [f   (validate-path! path)
         lang (path->lang f)
         ;; Guard: map passed instead of positional int (LLM doc mismatch)
         _   (when (map? offset)
               (throw (ex-info (str "read-file takes positional args: (read-file path offset limit). "
                                 "Got a map — use (read-file path 5 10) not (read-file path {:offset 5 :limit 10}).")
                        {:error :invalid-args :got offset})))
         _   (when (and offset (not (integer? offset)))
               (throw (ex-info (str "Invalid offset: expected integer, got " (type offset) ".")
                        {:offset offset :error :invalid-offset})))
         _   (when (and limit (not (integer? limit)))
               (throw (ex-info (str "Invalid limit: expected integer, got " (type limit) ".")
                        {:limit limit :error :invalid-limit})))
         ;; Validate offset: 1-based, must be >= 1 when specified
         _   (when (and offset (< offset 1))
               (throw (ex-info (str "Invalid offset: " offset ". Lines are 1-based (minimum 1).")
                        {:offset offset :error :invalid-offset})))
         ;; Validate limit: must be >= 1 when specified
         _   (when (and limit (< limit 1))
               (throw (ex-info (str "Invalid limit: " limit ". Must be >= 1.")
                        {:limit limit :error :invalid-limit})))
         explicit-range? (or (some? offset) (some? limit))
         off (max 0 (dec (or offset 1)))]

     ;; Byte-level guard — even with auto-pagination we refuse absurdly large
     ;; files unless the caller explicitly passes offset/limit. Loading a
     ;; 200MB log into memory to count lines is not what anyone wants.
     (when (and (not explicit-range?) (> (.length f) max-file-size))
       (throw (ex-info (str "File too large: " (quot (.length f) 1024) "KB. "
                         "Use offset+limit to read a portion.")
                {:path path :size (.length f) :max max-file-size})))

     ;; Single buffered pass — we always materialize all lines into a vector
     ;; so auto-pagination can compare against the total. The 10MB byte
     ;; guard above caps worst-case memory.
     (with-open [rdr (io/reader f :encoding "UTF-8")]
       (let [all-lines (vec (line-seq rdr))
             total     (count all-lines)
             content
             (cond
               ;; Empty file
               (zero? total) ""

               ;; Explicit range — honor offset/limit exactly.
               explicit-range?
               (let [taken (vec (cond->> (drop off all-lines) limit (take limit)))]
                 (if (empty? taken)
                   (throw (ex-info (str "[offset " (+ off 1) " beyond file end — file has " total " lines]")
                            {:offset (+ off 1) :total total :error :offset-past-eof}))
                   (str/join "\n" taken)))

               ;; Auto-pagination path — deterministic, no-knob defaults.
               (<= total FULL_FILE_THRESHOLD)
               (str/join "\n" all-lines)

               :else
               (str/join "\n" (take PARTIAL_READ_LIMIT all-lines)))]
         {:content content :lang lang})))))


;;; ── Display formatting ──────────────────────────────────────────────

(defn- format-read-result
  "Format read-file result for LLM display. Adds lang tag, line numbers,
   and a line-count footer. The raw return value (bound to the SCI var)
   stays clean for programmatic use.

   Contract: 1-arity, pure, returns string, handles nil."
  [result]
  (cond
    (nil? result) ""
    (not (map? result)) (str result)
    :else
    (let [{:keys [content lang]} result]
      (if (= "" content)
        "[empty file]"
        (let [lines (str/split-lines content)
              n     (count lines)]
          (str (when lang (str "[lang:" lang "] "))
            "[" n " lines]\n"
            (str/join "\n" (map-indexed (fn [i line] (str (inc i) "\t" line)) lines))))))))

(defn- validate-read-input
  [{:keys [args]}]
  (let [[path offset limit & extra] args]
    (when (seq extra)
      (throw (ex-info "read-file expects at most 3 positional args: (read-file path [offset] [limit])"
               {:type :tool/invalid-input :tool 'read-file :args args})))
    (when-not (string? path)
      (throw (ex-info "read-file path must be a string"
               {:type :tool/invalid-input :tool 'read-file :got path :got-type (type path)})))
    (when (and (some? offset) (not (integer? offset)))
      (throw (ex-info "read-file offset must be an integer when provided"
               {:type :tool/invalid-input :tool 'read-file :offset offset :got-type (type offset)})))
    (when (and (some? limit) (not (integer? limit)))
      (throw (ex-info "read-file limit must be an integer when provided"
               {:type :tool/invalid-input :tool 'read-file :limit limit :got-type (type limit)})))
    {:args (vec args)}))

(defn- validate-read-output
  [{:keys [result]}]
  (when-not (map? result)
    (throw (ex-info "read-file must return a map with :content and :lang"
             {:type :tool/invalid-output :tool 'read-file :got-type (type result)})))
  (when-not (string? (:content result))
    (throw (ex-info "read-file :content must be a string"
             {:type :tool/invalid-output :tool 'read-file :got-type (type (:content result))})))
  {:result result})

;;; ── Rescue ────────────────────────────────────────────────────────────

(defn- basename [^String path]
  (let [i (.lastIndexOf path "/")]
    (if (neg? i) path (subs path (inc i)))))

(defn- parent-dir [^String path]
  (let [f (io/file path)
        p (.getParentFile f)]
    (cond
      p                   (.getPath p)
      (.isAbsolute f)     "/"
      :else               ".")))

(defn- nearby-candidates
  "When a path lookup misses, scan the parent directory for entries whose
   basename shares a prefix (case-insensitive) with what the caller asked
   for. Returns up to 5 absolute paths. Keeps the scan local — no deep
   walking, so a huge repo doesn't slow rescue down."
  [requested-path]
  (try
    (let [dir     (io/file (parent-dir requested-path))
          needle  (str/lower-case (basename (str requested-path)))]
      (when (and (.isDirectory dir) (seq needle))
        (->> (.listFiles dir)
          (keep (fn [^java.io.File f]
                  (let [n (.getName f)]
                    (when (str/starts-with? (str/lower-case n)
                            (subs needle 0 (min 3 (count needle))))
                      (.getAbsolutePath f)))))
          (take 5)
          (vec))))
    (catch Throwable _ nil)))

(defn- rescue-read-file
  "Invoked when `read-file` throws. Today it only specializes the
   `:not-found` path — ex-data's `:error :not-found` is rewrapped into a
   richer message that lists nearby files so the LLM can retry with a
   valid path instead of guessing. Every other exception re-throws
   unchanged so the agent's normal error-hint path sees them verbatim.

   Args mirror `read-file`'s arglist: [err path offset limit]."
  [err & args]
  (let [path      (first args)
        data      (ex-data err)
        not-found? (= :not-found (:error data))]
    (if (and not-found? (string? path))
      (let [candidates (nearby-candidates path)
            hint       (if (seq candidates)
                         (str "Did you mean one of:\n  - "
                           (str/join "\n  - " candidates))
                         "No similarly-named files nearby.")]
        (throw (ex-info (str "File not found: " path "\n" hint)
                 (assoc data
                   :error :not-found
                   :requested-path path
                   :nearby candidates)
                 err)))
      (throw err))))

;;; ── Freshness tracking ────────────────────────────────────────────────

(defn- stat-path
  "Return `{:mtime :size}` for an existing path, nil when missing."
  [path]
  (try
    (let [f (java.io.File. ^String path)]
      (when (.exists f)
        {:mtime (.lastModified f)
         :size  (.length f)}))
    (catch Throwable _ nil)))

(defn freshness
  "`:freshness` implementation for the read-file tool. Single-arg map
   with the standard `{:args :result :metadata}` shape — see
   `runtime.core` docstring for the contract.

   Seed phase (`:metadata nil`): pulls the path out of `args`, stats
   it, returns the baseline + `:freshness? true` (the file was just
   read).
   Re-check phase (`:metadata {…}`): re-stats the stored path and
   compares mtime+size; returns `:freshness? false` on drift, and
   `:metadata {…}` reflecting the CURRENT on-disk values so callers
   can show `STALE` vs `fresh` without a second round-trip."
  [{:keys [args metadata]}]
  (let [path (or (:path metadata) (first args))]
    (if (nil? metadata)
      ;; Seed: tool just ran, args present, file by definition fresh.
      (let [{:keys [mtime size]} (stat-path path)]
        {:metadata   {:kind :file :path path :mtime mtime :size size}
         :freshness? true})
      ;; Re-check: compare live stat to stored snapshot.
      (if-let [{:keys [mtime size]} (stat-path path)]
        {:metadata   {:kind :file :path path :mtime mtime :size size}
         :freshness? (and (= mtime (:mtime metadata))
                       (= size (:size metadata)))}
        ;; File was deleted since bind. Leave :metadata at the last
        ;; known-good snapshot so the column renderer can distinguish
        ;; missing from stale; downstream render code maps exceptions
        ;; from this fn to `MISSING` via render-var-freshness' catch.
        (throw (ex-info (str "read-file freshness: path no longer exists: " path)
                 {:type :rlm.freshness/missing :path path}))))))

;;; ── Tool definition ────────────────────────────────────────────────────

(defn- read-file-superseded-by
  "A read-file call is superseded when another read-file in the same batch
   targets the EXACT same path (duplicate read). The first occurrence wins."
  [{:keys [path]} other-calls]
  (some (fn [other]
          (and (= (:tool other) 'read-file)
               (= (:path other) path)))
        other-calls))

(def tool-def
  (sci-tool/make-tool-def
    'read-file
    read-file
    {:doc (:doc (meta #'read-file))
     :arglists (:arglists (meta #'read-file))
     :validate-input validate-read-input
     :validate-output validate-read-output
     :format-result format-read-result
     :rescue-fn rescue-read-file
     :superseded-by read-file-superseded-by
     :activation-fn (constantly true)
     :group "filesystem" :activation-doc "always active"
     :requires-freshness? true
     :freshness freshness
     :examples ["(read-file \"/path/to/file.clj\")"
                "(def src (read-file \"/path/to/file.clj\"))"
                "(read-file \"/path/to/file.clj\" 1501 1500)"]
     :prompt "Returns `{:content raw-string :lang highlight-js-id}`.
`(:content result)` for raw text — use with str/replace, write-file, etc.
`(:lang result)` for syntax highlight language id (nil when unknown).
`(read-file path)` = full if ≤1500 lines, else first 1500 auto-paged.
Page bigger files: `(read-file path 1501 1500)`.
`(def src (read-file path))` once, reuse. Don't re-read."}))

