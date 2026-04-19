(ns com.blockether.vis.languages.commons.list
  "Base LIST tool for RLM agents.
   Lists directory contents with metadata (type, size, permissions, modified).
   Supports glob filtering, depth control, and configurable limits."
  (:require [clojure.string :as str]
            [com.blockether.vis.loop.tool :as sci-tool])
  (:import [java.io File]
           [java.nio.file Files Path FileSystems LinkOption]
           [java.nio.file.attribute PosixFilePermissions]
           [java.time Instant]))

;;; ── Defaults ──────────────────────────────────────────────────────────

(def ^:private default-max-entries
  "Default max entries to return (prevents huge listings)."
  1000)

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

   Params:
   - path  — Directory path (string, required)
   - glob  — Glob pattern to filter entries (string, optional, e.g. \"*.clj\", \"**/*.clj\")
   - depth — Max depth to recurse (int, optional, default 1 = flat listing, 0 = dir info only)
   - limit — Max entries to return (int, optional, default 1000)
   - ignore-dirs — Set of directory names to prune during recursion (pre-filter,
                   never descended into). Applies by exact segment name — so
                   `#{\".git\" \"node_modules\"}` prunes every `.git`/`node_modules`
                   anywhere in the tree. Defaults to nil (no pruning) so plain
                   `list-dir` stays literal. Callers like `grep` inject a
                   sensible junk-dir default.

   Returns a map:
   - :path      — Canonical directory path
   - :entries   — Vector of {:name str :type str :size int :permissions str :modified str}
   - :total     — Total count of entries (before truncation)
   - :truncated — true if results were truncated to limit"
  ([path] (list-dir path nil nil nil nil))
  ([path glob-or-opts]
   (if (map? glob-or-opts)
     (list-dir path (:glob glob-or-opts) (:depth glob-or-opts) (:limit glob-or-opts)
       (:ignore-dirs glob-or-opts))
     (list-dir path glob-or-opts nil nil nil)))
  ([path glob depth] (list-dir path glob depth nil nil))
  ([path glob depth limit] (list-dir path glob depth limit nil))
  ([path glob depth limit ignore-dirs]
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
         ignore-set (when (seq ignore-dirs) (set ignore-dirs))]

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
             total       (count (vec filtered))
             truncated?  (> total max-entries)
             entries     (vec (take max-entries filtered))]
         {:path      (.getCanonicalPath dir)
          :entries   entries
          :total     total
          :truncated truncated?})))))

(defn- validate-list-input
  [{:keys [args]}]
  (let [[path _glob depth limit & extra] args]
    (when (seq extra)
      (throw (ex-info "list-dir expects 1-4 positional args: (list-dir path [glob-or-opts] [depth] [limit])"
               {:type :tool/invalid-input :tool 'list-dir :args args})))
    (when-not (string? path)
      (throw (ex-info "list-dir path must be a string"
               {:type :tool/invalid-input :tool 'list-dir :got path :got-type (type path)})))
    (when (and (some? depth) (not (integer? depth)))
      (throw (ex-info "list-dir depth must be an integer when provided"
               {:type :tool/invalid-input :tool 'list-dir :depth depth :got-type (type depth)})))
    (when (and (some? limit) (not (integer? limit)))
      (throw (ex-info "list-dir limit must be an integer when provided"
               {:type :tool/invalid-input :tool 'list-dir :limit limit :got-type (type limit)})))
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
  (when-not (boolean? (:truncated result))
    (throw (ex-info "list-dir output :truncated must be a boolean"
             {:type :tool/invalid-output :tool 'list-dir :result result})))
  {:result result})

(defn- format-list-result
  "Pure formatter for list-dir's return map. Pattern:
     <path> — <total> entr(y|ies) [truncated]
       <entry>
       <entry>
       ...

   All entries are shown — list-dir already caps the entry list at its
   configured limit. Handles nil (validator probes) gracefully."
  [result]
  (if (nil? result)
    ""
    (let [{:keys [path entries total truncated]} result
          header (str path " — " total " entr" (if (= 1 total) "y" "ies")
                   (when truncated " (truncated)"))
          body (when (seq entries)
                 (str "\n  " (str/join "\n  " entries)))]
      (str header (or body "")))))

;;; ── Tool definition ────────────────────────────────────────────────────

(def tool-def
  (sci-tool/make-tool-def
    'list-dir
    list-dir
    {:doc (:doc (meta #'list-dir))
     :arglists (:arglists (meta #'list-dir))
     :validate-input validate-list-input
     :validate-output validate-list-output
     :format-result format-list-result
     :activation-fn (constantly true)
     :group "filesystem" :activation-doc "always active"
     :examples ["(list-dir \"src\")"
                "(list-dir \"src\" {:glob \"**/*.clj\" :depth 4 :limit 200})"]
     :prompt "List entries (type + size + name) under a path. `:glob \"**/*.clj\"` for recursive filter; omit for flat listing. One call with precise `:glob` > many calls."}))

