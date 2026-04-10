(ns com.blockether.vis.languages.commons.list
  "Base LIST tool for RLM agents.
   Lists directory contents with metadata (type, size, permissions, modified).
   Supports glob filtering, depth control, and configurable limits."
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

   Returns a map:
   - :path      — Canonical directory path
   - :entries   — Vector of {:name str :type str :size int :permissions str :modified str}
   - :total     — Total count of entries (before truncation)
   - :truncated — true if results were truncated to limit"
  ([path] (list-dir path nil nil nil))
  ([path glob-or-opts]
   (if (map? glob-or-opts)
     (list-dir path (:glob glob-or-opts) (:depth glob-or-opts) (:limit glob-or-opts))
     (list-dir path glob-or-opts nil nil)))
  ([path glob depth] (list-dir path glob depth nil))
  ([path glob depth limit]
   (let [dir       (File. ^String path)
         _         (when-not (.exists dir)
                     (throw (ex-info (str "Path not found: " path) {:path path :error :not-found})))
         _         (when-not (.isDirectory dir)
                     (throw (ex-info (str "Not a directory (is a file): " path ". Use read-file instead.")
                                     {:path path :error :not-directory})))
         ;; Auto-recurse deep when globstar is used without explicit depth
         depth     (max 0 (or depth
                              (if (and glob (.contains ^String glob "**")) 20 1)))
         max-entries (or limit default-max-entries)]

     (letfn [(collect [^File d current-depth]
               (when (and (pos? depth) (<= current-depth depth))
                 (let [children (try
                                  (sort-by #(.getName ^File %) (.listFiles d))
                                  (catch SecurityException _ nil))]
                   (when children
                     (mapcat (fn [^File f]
                               (try
                                 (let [entry (file-entry f)
                                       entry (if (> current-depth 1)
                                               (let [rel (.relativize (.toPath dir) (.toPath f))]
                                                 (assoc entry :name (str rel)))
                                               entry)]
                                   (if (and (.isDirectory f)
                                            (not (Files/isSymbolicLink (.toPath f)))
                                            (< current-depth depth))
                                     (cons entry (collect f (inc current-depth)))
                                     [entry]))
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

;;; ── Tool definition ────────────────────────────────────────────────────

(def tool-def
  {:sym 'list-dir
   :fn  list-dir
   :doc "List directory contents with type, size, permissions, and modified timestamp. Supports glob filtering (including **/*.clj globstar), recursive depth, and configurable result limits."
   :params [{:name "path" :type :string :required true
             :description "Directory path to list"}
            {:name "glob" :type :string :required false
             :description "Glob pattern to filter entries (e.g. \"*.clj\", \"**/*.clj\", \"*.{clj,edn}\"). Globstar ** auto-sets depth=20."}
            {:name "depth" :type :int :required false
             :description "Max recursion depth (default 1 = flat listing, 0 = dir info only with no entries)"}
            {:name "limit" :type :int :required false
             :description "Max entries to return (default 1000). Use to control output size for large directories."}]
   :returns {:type :map
             :description "{:path str :entries [{:name str :type str :size int :permissions str :modified str}] :total int :truncated bool}"}})
