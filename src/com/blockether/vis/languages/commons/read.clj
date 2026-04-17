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
   - offset — Starting line number, 1-based (int, optional, default 1).
              Must be >= 1. Passing 0 is an error (lines are 1-based).
   - limit  — Max lines to return (int, optional, default all).
              Must be >= 1 when specified.

   Uses buffered reading when offset/limit specified — safe for large files.
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
         off (max 0 (dec (or offset 1)))
         use-range? (or (and offset (> offset 1)) limit)]

     ;; Guard: full reads on huge files require offset+limit
     (when (and (not use-range?) (> (.length f) max-file-size))
       (throw (ex-info (str "File too large: " (quot (.length f) 1024) "KB. Use offset+limit to read a portion.")
                {:path path :size (.length f) :max max-file-size})))

     (if use-range?
       ;; Buffered line-by-line — read all lines to get accurate total count
       (with-open [rdr (io/reader f :encoding "UTF-8")]
         (let [all-lines (vec (line-seq rdr))
               total     (count all-lines)
               taken     (vec (cond->> (drop off all-lines) limit (take limit)))]
           (if (empty? taken)
             (throw (ex-info (str "[offset " (+ off 1) " beyond file end — file has " total " lines]")
                      {:offset (+ off 1) :total total :error :offset-past-eof}))
             (let [numbered (map-indexed
                              (fn [i line] (str (+ off i 1) "\t" line))
                              taken)]
               (str (str/join "\n" numbered)
                 "\n[lines " (+ off 1) "-" (+ off (count taken)) " of " total "]")))))

       ;; Full read — guarded by size check above
       (let [content  (slurp f :encoding "UTF-8")
             lines    (if (str/blank? content) [] (str/split-lines content))
             total    (count lines)
             numbered (map-indexed (fn [i line] (str (inc i) "\t" line)) lines)]
         (if (zero? total)
           "[empty file]"
           (str (str/join "\n" numbered)
             "\n[lines 1-" total " of " total "]")))))))

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

;;; ── Tool definition ────────────────────────────────────────────────────

(def tool-def
  (sci-tool/make-tool-def
    'read-file
    read-file
    {:doc (:doc (meta #'read-file))
     :arglists (:arglists (meta #'read-file))
     :validate-input validate-read-input
     :validate-output validate-read-output
     :examples ["(read-file \"/path/to/file.clj\")"
                "(read-file \"/path/to/file.clj\" 40 30)"]}))
