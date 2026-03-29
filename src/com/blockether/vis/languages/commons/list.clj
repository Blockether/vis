(ns com.blockether.vis.languages.commons.list
  "Base LIST tool for RLM agents.
   Lists directory contents with metadata (type, size, permissions).
   Supports glob filtering and depth control."
  (:import [java.io File]
           [java.nio.file Files Path FileSystems]
           [java.nio.file.attribute PosixFilePermissions]))

;;; ── Defaults ──────────────────────────────────────────────────────────

(def ^:private max-entries
  "Max entries to return (prevents huge listings)."
  1000)

;;; ── Helpers ───────────────────────────────────────────────────────────

(defn- posix-perms
  "Get POSIX permission string (e.g. \"rwxr-xr-x\") or nil on non-POSIX fs."
  [^Path path]
  (try
    (PosixFilePermissions/toString (Files/getPosixFilePermissions path (into-array java.nio.file.LinkOption [])))
    (catch UnsupportedOperationException _ nil)))

(defn- dir-size
  "Recursively compute total size of files in a directory."
  [^File f]
  (let [children (.listFiles f)]
    (if children
      (reduce + 0 (map (fn [^File child]
                          (if (.isDirectory child)
                            (dir-size child)
                            (.length child)))
                       children))
      0)))

(defn- file-entry
  "Build a map describing one file/directory entry."
  [^File f]
  (let [path  (.toPath f)
        dir?  (.isDirectory f)
        perms (posix-perms path)]
    (cond-> {:name (.getName f)
             :type (cond dir?                   "directory"
                         (Files/isSymbolicLink path) "symlink"
                         :else                  "file")
             :size (if dir? (dir-size f) (.length f))}
      perms (assoc :permissions perms))))

(defn- matches-glob?
  "Check if a filename matches a glob pattern."
  [^String pattern ^String name]
  (let [matcher (.getPathMatcher (FileSystems/getDefault) (str "glob:" pattern))]
    (.matches matcher (.getFileName (java.nio.file.Path/of name (into-array String []))))))

;;; ── Core ──────────────────────────────────────────────────────────────

(defn list-dir
  "List directory contents.

   Params:
   - path  — Directory path (string, required)
   - glob  — Glob pattern to filter entries (string, optional, e.g. \"*.clj\")
   - depth — Max depth to recurse (int, optional, default 1 = flat listing)

   Returns a map:
   - :path    — Canonical directory path
   - :entries — Vector of {:name str :type \"file\"|\"directory\"|\"symlink\" :size int|nil :permissions str|nil}
   - :total   — Total count of entries (before truncation)
   - :truncated — true if results were truncated to max-entries"
  ([path] (list-dir path nil nil))
  ([path glob-or-opts]
   (if (map? glob-or-opts)
     (list-dir path (:glob glob-or-opts) (:depth glob-or-opts))
     (list-dir path glob-or-opts nil)))
  ([path glob depth]
   (let [dir   (File. ^String path)
         _     (when-not (.exists dir)
                 (throw (ex-info (str "Path not found: " path) {:path path})))
         _     (when-not (.isDirectory dir)
                 (throw (ex-info (str "Not a directory: " path) {:path path})))
         depth (max 1 (or depth 1))]

     (letfn [(collect [^File d current-depth]
               (when (<= current-depth depth)
                 (let [children (sort-by #(.getName ^File %) (.listFiles d))]
                   (mapcat (fn [^File f]
                             (let [entry (file-entry f)
                                   entry (if (> current-depth 1)
                                           (let [rel (.relativize (.toPath dir) (.toPath f))]
                                             (assoc entry :name (str rel)))
                                           entry)]
                               (if (and (.isDirectory f) (< current-depth depth))
                                 (cons entry (collect f (inc current-depth)))
                                 [entry])))
                           children))))]

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
   :doc "List directory contents with type, size, and permissions. Supports glob filtering and recursive depth."
   :params [{:name "path" :type :string :required true
             :description "Directory path to list"}
            {:name "glob" :type :string :required false
             :description "Glob pattern to filter entries (e.g. \"*.clj\", \"*.{clj,edn}\")"}
            {:name "depth" :type :int :required false
             :description "Max recursion depth (default 1 = flat listing)"}]
   :returns {:type :map
             :description "{:path str :entries [{:name str :type str :size int|nil :permissions str|nil}] :total int :truncated bool}"}})
