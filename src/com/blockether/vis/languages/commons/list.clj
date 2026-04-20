(ns com.blockether.vis.languages.commons.list
  "Base LIST tool for RLM agents.
   Lists directory contents with metadata (type, size, permissions, modified).
   Supports glob filtering, depth control, pagination via :offset/:limit, and
   a default ignore-set that prunes VCS, build cache, and IDE-state directories
   so plain `(list-dir \".\")` doesn't drown the prompt in `.cpcache` /
   `.clj-kondo` / `node_modules` noise."
  (:require [clojure.string :as str]
            [com.blockether.vis.loop.tool :as sci-tool])
  (:import [java.io File]
           [java.nio.file Files Path FileSystems LinkOption]
           [java.nio.file.attribute PosixFilePermissions]
           [java.time Instant]))

;;; ── Defaults ──────────────────────────────────────────────────────────

(def ^:private default-max-entries
  "Default page size when the caller doesn't pass `:limit`. Deliberately
   tighter than before — pages of 100 are big enough to skim in one
   iteration without burning the prompt budget on noise. The agent can
   bump it for a one-shot wide listing or paginate via `:offset`."
  100)

(def default-ignore-dirs
  "Directory names pruned by default. Pass `:ignore-dirs #{}` to disable
   the prune entirely (useful when you actually need to inspect `.git/HEAD`
   or `.cpcache`). Matched by exact segment name anywhere in the tree.

   This is the canonical noise-set shared with `grep` so both tools agree
   on what counts as junk. Touch with care — every entry below has burned
   somebody's iteration budget at least once."
  #{".git" ".hg" ".svn"
    ".cache" ".cpcache" ".gitlibs" ".m2" ".shadow-cljs"
    ".clj-kondo" ".lsp" ".calva" "target"
    "node_modules" ".next" ".nuxt" ".turbo" ".yarn" ".pnpm-store"
    "__pycache__" ".venv" "venv" ".tox" ".mypy_cache" ".pytest_cache" ".ruff_cache"
    ".idea" ".vscode"
    "coverage" ".nyc_output"
    ".terraform" ".vercel" ".netlify"
    "DerivedData" "Pods"})

(def ^:private no-follow (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))

;;; ── Helpers ───────────────────────────────────────────────────────────

(defn- posix-perms
  "Get POSIX permission string (e.g. \"rwxr-xr-x\") or nil on non-POSIX fs."
  [^Path path]
  (try
    (PosixFilePermissions/toString (Files/getPosixFilePermissions path no-follow))
    (catch Exception _ nil)))

(defn- dir-size
  "Recursively compute total size of files in a directory.
   Follows only real directories (not symlinks). Skips unreadable entries."
  [^File f]
  (let [children (try (.listFiles f) (catch SecurityException _ nil))]
    (if children
      (reduce + 0 (map (fn [^File child]
                         (try
                           (if (and (.isDirectory child)
                                 (not (Files/isSymbolicLink (.toPath child))))
                             (dir-size child)
                             (.length child))
                           (catch Exception _ 0)))
                    children))
      0)))

(defn- file-entry
  "Build a map describing one file/directory entry."
  [^File f]
  (let [path     (.toPath f)
        symlink? (Files/isSymbolicLink path)
        dir?     (and (.isDirectory f) (not symlink?))
        perms    (posix-perms path)
        modified (try (str (Instant/ofEpochMilli (.lastModified f))) (catch Exception _ nil))]
    (cond-> {:name (.getName f)
             :type (cond symlink? "symlink"
                     dir?     "directory"
                     :else    "file")
             :size (if dir? (dir-size f) (.length f))}
      perms    (assoc :permissions perms)
      modified (assoc :modified modified))))

(defn- matches-glob?
  "Check if an entry name matches a glob pattern.
   For globstar patterns (containing **), matches against the full relative path.
   For simple patterns, matches against just the filename."
  [^String pattern ^String name]
  (let [path-glob? (or (.contains pattern "**") (.contains pattern "/"))
        matcher (.getPathMatcher (FileSystems/getDefault) (str "glob:" pattern))
        target  (if path-glob?
                  (java.nio.file.Path/of name (into-array String []))
                  (.getFileName (java.nio.file.Path/of name (into-array String []))))]
    (.matches matcher target)))

;;; ── Core ──────────────────────────────────────────────────────────────

(defn list-dir
  "List directory contents.

   Params (positional or as a single opts map after `path`):
   - path        — Directory path (string, required)
   - glob        — Glob pattern to filter entries (\"*.clj\", \"**/*.clj\")
   - depth       — Max depth to recurse. Default 1 (flat listing). 0 = info only.
                   Auto-jumps to 20 when `glob` contains `**` and `:depth`
                   is unspecified.
   - limit       — Max entries to return per page. Default 100.
   - offset      — Number of entries to skip before returning a page. Default 0.
                   Use with `:next-offset` from the previous response to paginate.
   - ignore-dirs — Set of directory names to PRUNE during recursion (entries
                   inside are never walked). Defaults to `default-ignore-dirs`
                   (VCS, build caches, node_modules, IDE state, …). Pass
                   `#{}` to disable pruning when you actually need `.git/HEAD`
                   or `.cpcache/*.basis`.

   Returns a map:
   - :path        — Canonical directory path
   - :entries     — Vector of {:name :type :size [:permissions] [:modified]}
   - :total       — Total entry count after glob+ignore filtering, BEFORE
                    offset/limit slicing. Tells the agent how big the full
                    listing is so it can decide whether to paginate.
   - :offset      — Echoed back so the caller can confirm what page it got.
   - :truncated?  — true when `:total > offset + (count :entries)` — i.e.
                    there are MORE entries beyond this page.
   - :next-offset — int suggesting the next `:offset` value when truncated,
                    nil otherwise. Just feed this back to paginate."
  ([path] (list-dir path nil))
  ([path glob-or-opts]
   (if (map? glob-or-opts)
     (list-dir path
       (:glob glob-or-opts)
       (:depth glob-or-opts)
       (:limit glob-or-opts)
       (:offset glob-or-opts)
       (:ignore-dirs glob-or-opts ::default))
     (list-dir path glob-or-opts nil nil nil ::default)))
  ([path glob depth] (list-dir path glob depth nil nil ::default))
  ([path glob depth limit] (list-dir path glob depth limit nil ::default))
  ([path glob depth limit offset] (list-dir path glob depth limit offset ::default))
  ([path glob depth limit offset ignore-dirs]
   (let [dir       (File. ^String path)
         _         (when-not (.exists dir)
                     (throw (ex-info (str "Path not found: " path) {:path path :error :not-found})))
         _         (when-not (.isDirectory dir)
                     (throw (ex-info (str "Not a directory (is a file): " path ". Use read-file instead.")
                              {:path path :error :not-directory})))
         ;; Auto-recurse deep when globstar is used without explicit depth
         depth     (max 0 (or depth
                            (if (and glob (.contains ^String glob "**")) 20 1)))
         max-entries (or limit default-max-entries)
         skip      (max 0 (or offset 0))
         ;; ::default sentinel → use shared default-ignore-dirs.
         ;; Explicit nil or any other value → use as-is (empty set disables pruning).
         resolved-ignore (cond
                           (= ::default ignore-dirs) default-ignore-dirs
                           (nil? ignore-dirs)        nil
                           :else                     ignore-dirs)
         ignore-set (when (seq resolved-ignore) (set resolved-ignore))]

     (letfn [(ignored-dir? [^File f]
               (and ignore-set (contains? ignore-set (.getName f))))
             (collect [^File d current-depth]
               (when (and (pos? depth) (<= current-depth depth))
                 (let [children (try
                                  (sort-by #(.getName ^File %) (.listFiles d))
                                  (catch SecurityException _ nil))]
                   (when children
                     (mapcat (fn [^File f]
                               (try
                                 (cond
                                   ;; Prune ignored dirs pre-descent. Their entries
                                   ;; are omitted entirely — not even a stub row —
                                   ;; because huge trees like `.m2` or `node_modules`
                                   ;; would otherwise burn the entry budget before
                                   ;; we ever reach the source files.
                                   (and (.isDirectory f) (ignored-dir? f))
                                   nil

                                   :else
                                   (let [entry (file-entry f)
                                         entry (if (> current-depth 1)
                                                 (let [rel (.relativize (.toPath dir) (.toPath f))]
                                                   (assoc entry :name (str rel)))
                                                 entry)]
                                     (if (and (.isDirectory f)
                                           (not (Files/isSymbolicLink (.toPath f)))
                                           (< current-depth depth))
                                       (cons entry (collect f (inc current-depth)))
                                       [entry])))
                                 (catch Exception _ nil)))
                       children)))))]

       (let [all-entries (collect dir 1)
             filtered    (if glob
                           (filter #(matches-glob? glob (:name %)) all-entries)
                           all-entries)
             materialized (vec filtered)
             total       (count materialized)
             page        (vec (take max-entries (drop skip materialized)))
             returned    (count page)
             truncated?  (> total (+ skip returned))
             next-offset (when truncated? (+ skip returned))]
         {:path        (.getCanonicalPath dir)
          :entries     page
          :total       total
          :offset      skip
          :truncated?  truncated?
          :next-offset next-offset})))))

(defn- validate-list-input
  [{:keys [args]}]
  (let [[path glob-or-opts depth limit offset & extra] args]
    (when (seq extra)
      (throw (ex-info "list-dir expects 1-5 positional args: (list-dir path [glob-or-opts] [depth] [limit] [offset])"
               {:type :tool/invalid-input :tool 'list-dir :args args})))
    (when-not (string? path)
      (throw (ex-info "list-dir path must be a string"
               {:type :tool/invalid-input :tool 'list-dir :got path :got-type (type path)})))
    (when (and (some? glob-or-opts)
            (not (string? glob-or-opts))
            (not (map? glob-or-opts)))
      (throw (ex-info "list-dir second arg must be a glob string or an opts map"
               {:type :tool/invalid-input :tool 'list-dir :got glob-or-opts :got-type (type glob-or-opts)})))
    (when (and (some? depth) (not (integer? depth)))
      (throw (ex-info "list-dir depth must be an integer when provided"
               {:type :tool/invalid-input :tool 'list-dir :depth depth :got-type (type depth)})))
    (when (and (some? limit) (not (integer? limit)))
      (throw (ex-info "list-dir limit must be an integer when provided"
               {:type :tool/invalid-input :tool 'list-dir :limit limit :got-type (type limit)})))
    (when (and (some? offset) (not (integer? offset)))
      (throw (ex-info "list-dir offset must be an integer when provided"
               {:type :tool/invalid-input :tool 'list-dir :offset offset :got-type (type offset)})))
    {:args (vec args)}))

(defn- validate-list-output
  [{:keys [result]}]
  (when-not (map? result)
    (throw (ex-info "list-dir must return a map"
             {:type :tool/invalid-output :tool 'list-dir :got-type (type result)})))
  (when-not (string? (:path result))
    (throw (ex-info "list-dir output :path must be a string"
             {:type :tool/invalid-output :tool 'list-dir :result result})))
  (when-not (vector? (:entries result))
    (throw (ex-info "list-dir output :entries must be a vector"
             {:type :tool/invalid-output :tool 'list-dir :result result})))
  (when-not (integer? (:total result))
    (throw (ex-info "list-dir output :total must be an int"
             {:type :tool/invalid-output :tool 'list-dir :result result})))
  (when-not (integer? (:offset result))
    (throw (ex-info "list-dir output :offset must be an int"
             {:type :tool/invalid-output :tool 'list-dir :result result})))
  (when-not (boolean? (:truncated? result))
    (throw (ex-info "list-dir output :truncated? must be a boolean"
             {:type :tool/invalid-output :tool 'list-dir :result result})))
  (when-not (or (nil? (:next-offset result))
              (integer? (:next-offset result)))
    (throw (ex-info "list-dir output :next-offset must be an int or nil"
             {:type :tool/invalid-output :tool 'list-dir :result result})))
  {:result result})

(defn- format-list-result
  "Pure formatter for list-dir's return map. Pattern:
     <path> — showing <returned>/<total> entr(y|ies) [offset N] [<more> more, retry with :offset M]
       <entry>
       <entry>
       ...

   The header is the load-bearing part: the LLM must see at a glance
   whether it got a slice or the full listing. When truncated we spell out
   the exact opts to retry (`:offset N :limit L`) so it doesn't have to
   reconstruct them."
  [result]
  (if (nil? result)
    ""
    (let [{:keys [path entries total offset truncated? next-offset]} result
          returned (count entries)
          singular? (= 1 total)
          remaining (when truncated? (- total (+ (or offset 0) returned)))
          header (str path " — showing " returned "/" total " entr"
                   (if singular? "y" "ies")
                   (when (and offset (pos? offset)) (str " (offset " offset ")"))
                   (when truncated?
                     (str " — " remaining " more, retry with `:offset "
                       next-offset "`")))
          body (when (seq entries)
                 (str "\n  " (str/join "\n  " entries)))]
      (str header (or body "")))))

;;; ── Tool definition ────────────────────────────────────────────────────

(defn- list-dir-superseded-by
  "A list-dir call is superseded when another list-dir in the same batch
   targets the EXACT same path (duplicate listing).
   NOTE: grep on the same dir does NOT supersede list-dir — they serve
   different purposes (tree structure vs content matches)."
  [{:keys [path]} other-calls]
  (some (fn [other]
          (and (= (:tool other) 'list-dir)
               (= (:path other) path)))
        other-calls))

(def tool-def
  (sci-tool/make-tool-def
    'list-dir
    list-dir
    {:doc (:doc (meta #'list-dir))
     :arglists (:arglists (meta #'list-dir))
     :validate-input-fn validate-list-input
     :validate-output-fn validate-list-output
     :format-result-fn format-list-result
     :superseded-by-fn list-dir-superseded-by
     :activation-fn (constantly true)
     :group "filesystem"
     :examples ["(list-dir \"src\")"
                "(list-dir \"src\" {:glob \"**/*.clj\" :depth 4 :limit 200})"
                "(list-dir \".\" {:limit 50 :offset 50})  ;; second page of 50"
                "(list-dir \".git\" {:ignore-dirs #{}})    ;; really inspect .git/"]
     :prompt "List entries (type + size + name) under a path. `:glob \"**/*.clj\"` for recursive filter; omit for flat listing. One call with precise `:glob` > many calls.

Defaults you should know:
- `:limit` defaults to 100 entries per call. The result tells you the TOTAL count and whether more pages exist (`:truncated? true` + `:next-offset`). To paginate, just feed `:next-offset` back as the next call's `:offset`.
- `:ignore-dirs` defaults to a curated junk-set (`.git`, `.cpcache`, `.clj-kondo`, `node_modules`, `target`, `.idea`, `.vscode`, `.venv`, `__pycache__`, …). It's identical to grep's. To actually inspect those directories, pass `{:ignore-dirs #{}}`."}))
