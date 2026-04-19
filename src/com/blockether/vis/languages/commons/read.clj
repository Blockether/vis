(ns com.blockether.vis.languages.commons.read
  "Base READ tool for RLM agents.
   Reads files with optional offset (line) and limit (line count).
   Returns content with line numbers for precise referencing."
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
   trimming, no model-side heuristic."
  3000)

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

;;; ── Core ───────────────────────────────────────────────────────────────

(defn read-file
  "Read a file with optional offset and limit.

   Params:
   - path   — File path (string, required)
   - offset — Starting line number, 1-based (int, optional). When omitted,
              the tool auto-pages based on file size (see below).
   - limit  — Max lines to return (int, optional). Must be >= 1 when given.

   Auto-pagination (default when NO offset AND NO limit are passed):
   - Files with ≤ FULL_FILE_THRESHOLD (3000) lines return in full.
   - Files with > FULL_FILE_THRESHOLD lines return the first
     PARTIAL_READ_LIMIT (1500) lines. The footer marks the result as
     `auto-paged` so the caller knows to pass explicit offset/limit to
     fetch the remainder.

   Callers who want a specific range pass `offset` and/or `limit`
   explicitly; the auto-pagination path is then skipped.

   Returns string with numbered lines: \"1\\t(ns foo)\\n2\\t(:require ...)\""
  ([path] (read-file path nil nil))
  ([path offset] (read-file path offset nil))
  ([path offset limit]
   (let [f   (validate-path! path)
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
     ;; so the footer carries an accurate total. The 10MB byte guard above
     ;; caps worst-case memory.
     (with-open [rdr (io/reader f :encoding "UTF-8")]
       (let [all-lines (vec (line-seq rdr))
             total     (count all-lines)]
         (cond
           ;; Empty file
           (zero? total) "[empty file]"

           ;; Explicit range — honor offset/limit exactly.
           explicit-range?
           (let [taken (vec (cond->> (drop off all-lines) limit (take limit)))]
             (if (empty? taken)
               (throw (ex-info (str "[offset " (+ off 1) " beyond file end — file has " total " lines]")
                        {:offset (+ off 1) :total total :error :offset-past-eof}))
               (let [numbered (map-indexed
                                (fn [i line] (str (+ off i 1) "\t" line))
                                taken)]
                 (str (str/join "\n" numbered)
                   "\n[lines " (+ off 1) "-" (+ off (count taken)) " of " total "]"))))

           ;; Auto-pagination path — deterministic, no-knob defaults.
           (<= total FULL_FILE_THRESHOLD)
           (let [numbered (map-indexed (fn [i line] (str (inc i) "\t" line)) all-lines)]
             (str (str/join "\n" numbered)
               "\n[lines 1-" total " of " total "]"))

           :else
           (let [taken    (vec (take PARTIAL_READ_LIMIT all-lines))
                 numbered (map-indexed (fn [i line] (str (inc i) "\t" line)) taken)]
             (str (str/join "\n" numbered)
               "\n[lines 1-" PARTIAL_READ_LIMIT " of " total " — auto-paged; "
               "pass offset/limit to read the rest]"))))))))

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
  (when-not (string? result)
    (throw (ex-info "read-file must return a string"
             {:type :tool/invalid-output :tool 'read-file :got-type (type result)})))
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

;;; ── Tool definition ────────────────────────────────────────────────────

(def tool-def
  (sci-tool/make-tool-def
    'read-file
    read-file
    {:doc (:doc (meta #'read-file))
     :arglists (:arglists (meta #'read-file))
     :validate-input validate-read-input
     :validate-output validate-read-output
     :rescue-fn rescue-read-file
     :activation-fn (constantly true)
     :group "filesystem" :activation-doc "always active"
     :examples ["(read-file \"/path/to/file.clj\")"
                "(def src (read-file \"/path/to/file.clj\"))"
                "(read-file \"/path/to/file.clj\" 1501 1500)"]
     :prompt "Numbered lines + `[lines N-M of TOTAL]` footer.
`(read-file path)` = full if ≤3000 lines, else first 1500 auto-paged.
Page bigger files: `(read-file path 1501 1500)`.
`(def src (read-file path))` once, reuse. Don't re-read."}))

