(ns com.blockether.vis.internal.file-picker
  "Backend for file-picking UIs.

   This namespace owns the NON-UI parts of the `@` picker:
   - repository discovery
   - git dirty / ignored metadata via the native `git` binary
   - filesystem indexing via java.nio.file
   - filtering, ranking, sorting
   - compact display labels for size / age / git status

   Channels render and keybind on top of this data; they should not
   reimplement repository walking or git-status logic themselves."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.git :as git]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.workspace :as workspace]
            [com.blockether.fff :as fff])
  (:import [java.io File]
           [java.nio.file FileVisitResult Files Path SimpleFileVisitor]
           [java.nio.file.attribute BasicFileAttributes]
           [java.util Locale]))

(def ^:const max-results 200)

(def ^:private sort-order [:recent :relevance])

(defn cwd-path
  "Current explicit workspace cwd as a Path. Indirected for tests."
  ^Path []
  (.toPath (workspace/cwd)))

(defn display-path
  "`path` relativized against `root`, normalized to `/` separators."
  ^String [^Path root ^Path path]
  (paths/unixify (.relativize root path)))

(defn status-priority
  "Higher = more visually important in the picker."
  [status]
  (case status
    :conflict
    6

    :deleted
    5

    :modified
    4

    :added
    3

    :untracked
    2

    :ignored
    1

    0))

(defn git-status-snapshot
  "Git metadata for the repository containing `cwd`, or a reduced empty shape
   when the directory is not in git. Backed by one `git status` subprocess."
  [^Path cwd]
  (if-let [^File top (git/repo-work-tree (.toFile cwd))]
    (try (assoc (git/git-status-snapshot top) :repo-root (.toPath top))
         (catch Throwable _
           {:repo-root (.toPath top) :path-status {} :ignored-exact #{} :ignored-prefixes []}))
    {:repo-root nil :path-status {} :ignored-exact #{} :ignored-prefixes []}))

(defn ignored-path?
  "True when `repo-rel-path` is ignored according to the snapshot map
   from `git-status-snapshot`."
  [{:keys [ignored-exact ignored-prefixes]} repo-rel-path]
  (boolean (and repo-rel-path
                (or (contains? ignored-exact repo-rel-path)
                    (some #(str/starts-with? repo-rel-path %) ignored-prefixes)))))

(defn file-picker-entry
  "Build one picker entry from a filesystem path + attrs. `cwd` is the
   scan root; `git-info` is from `git-status-snapshot`."
  [^Path cwd {:keys [repo-root path-status] :as git-info} ^Path path ^BasicFileAttributes attrs]
  (let
    [abs-path
     (.toAbsolutePath path)

     rel-path
     (display-path cwd abs-path)

     parent-path
     (let [parent (.getParent abs-path)]
       (cond (nil? parent) "."
             (= (.toAbsolutePath parent) (.toAbsolutePath cwd)) "."
             :else (display-path cwd parent)))

     repo-rel
     (when repo-root
       (let [repo-root-abs (.toAbsolutePath ^Path repo-root)]
         (when (.startsWith abs-path repo-root-abs) (display-path repo-root-abs abs-path))))

     ignored?
     (ignored-path? git-info repo-rel)

     git-status
     (or (get path-status repo-rel) (when ignored? :ignored))]

    {:path rel-path
     :name (str (.getFileName abs-path))
     :parent (if (= rel-path parent-path) "." parent-path)
     :size (.size attrs)
     :mtime-ms (.toMillis (.lastModifiedTime attrs))
     :git-status git-status
     :ignored? ignored?}))

(defn collect-file-picker-entries
  "Index regular files under the current working directory. Skips `.git`
   internals unconditionally; everything else stays eligible so the UI can
   toggle ignored files on/off without rebuilding the index."
  []
  (let
    [cwd
     (cwd-path)

     git-info
     (git-status-snapshot cwd)

     entries
     (volatile! [])]

    (Files/walkFileTree cwd
                        (proxy [SimpleFileVisitor] []
                          (preVisitDirectory [^Path dir ^BasicFileAttributes _attrs]
                            (if (= ".git"
                                   (some-> dir
                                           .getFileName
                                           str))
                              FileVisitResult/SKIP_SUBTREE
                              FileVisitResult/CONTINUE))
                          (visitFile [^Path file ^BasicFileAttributes attrs]
                            (when (or (.isRegularFile attrs) (.isSymbolicLink attrs))
                              (vswap! entries conj (file-picker-entry cwd git-info file attrs)))
                            FileVisitResult/CONTINUE)
                          (visitFileFailed [_file _exc] FileVisitResult/CONTINUE)))
    @entries))

(defn format-bytes
  "Human-ish byte string for picker rows."
  [^long n]
  (cond (< n 1024) (str n "B")
        (< n (* 1024 1024))
        (String/format Locale/US "%.1fK" (into-array Object [(/ (double n) 1024.0)]))
        :else (String/format Locale/US "%.1fM" (into-array Object [(/ (double n) 1048576.0)]))))

(defn format-relative-age
  "Compact relative age for picker rows."
  [^long now-ms ^long mtime-ms]
  (let
    [delta-ms
     (long (max 0 (- now-ms mtime-ms)))

     minutes
     (quot delta-ms 60000)

     hours
     (quot delta-ms 3600000)

     days
     (quot delta-ms 86400000)]

    (cond (< minutes 1) "now"
          (< minutes 60) (str minutes "m")
          (< hours 24) (str hours "h")
          :else (str days "d"))))

(defn file-picker-score
  "Heuristic fuzzy score for `query` against `path`. Nil means no match."
  [path query]
  (let
    [path-lc
     (str/lower-case path)

     query-lc
     (str/lower-case (or query ""))

     file-name
     (last (str/split path-lc #"/"))

     segments
     (str/split path-lc #"/")]

    (cond (str/blank? query-lc) nil
          (= path-lc query-lc) 1000
          (= file-name query-lc) 950
          (str/starts-with? file-name query-lc) 900
          (some #(str/starts-with? % query-lc) segments) 850
          (str/includes? file-name query-lc) (- 700 (.indexOf ^String file-name query-lc))
          (str/starts-with? path-lc query-lc) 650
          (str/includes? path-lc query-lc) (- 500 (.indexOf ^String path-lc query-lc))
          :else nil)))

(defn resolved-sort-mode
  "Relevance falls back to path order when the query is blank; recent is
   the default newest-first ordering."
  [sort-mode query]
  (case sort-mode
    :relevance
    (if (str/blank? query) :path :relevance)

    :recent))

(defn status-label
  "Human-readable git status for picker table rows."
  [status]
  (case status
    :modified
    "modified"

    :untracked
    "untracked"

    :added
    "added"

    :deleted
    "deleted"

    :conflict
    "conflict"

    :ignored
    "ignored"

    "clean"))

(defn decorate-entry
  "Attach derived display fields used by the picker renderer."
  [entry now-ms query]
  (let [score (file-picker-score (:path entry) query)]
    (assoc entry
      :label (:path entry)
      :score score
      :status-label (status-label (:git-status entry))
      :size-label (format-bytes (:size entry))
      :age-label (format-relative-age now-ms (:mtime-ms entry)))))

(defn file-picker-items
  "Filter, score, and sort picker entries.

   Options:
   - :include-ignored?  include gitignored files
   - :sort-mode         :recent | :relevance
   - :now-ms            override current time for deterministic tests"
  [entries query
   {:keys [include-ignored? sort-mode now-ms]
    :or {include-ignored? false sort-mode :recent now-ms (System/currentTimeMillis)}}]
  (let [sort-mode* (resolved-sort-mode sort-mode query)]
    (->> entries
         (filter #(or include-ignored? (not (:ignored? %))))
         (map #(decorate-entry % now-ms query))
         (filter #(or (str/blank? query) (some? (:score %))))
         (sort-by (fn [entry]
                    (case sort-mode*
                      :recent
                      [(- (long (:mtime-ms entry))) (- (long (status-priority (:git-status entry))))
                       (:path entry)]

                      :relevance
                      [(- (double (or (:score entry) 0))) (- (long (:mtime-ms entry)))
                       (- (long (status-priority (:git-status entry)))) (:path entry)]

                      [(:path entry)])))
         (take max-results)
         vec)))

(defn cycle-sort-mode
  "Advance to the next picker sort mode."
  [sort-mode]
  (let [idx (.indexOf ^java.util.List sort-order sort-mode)]
    (nth sort-order (mod (inc (max idx 0)) (count sort-order)))))

(defn sort-label
  "Human label for the current sort mode."
  [sort-mode query]
  (name (resolved-sort-mode sort-mode query)))

;; ── fff-backed fuzzy search (shared with the `find_files` tool) ───────────────
;; The `@`-mention pickers (TUI + gateway/web) rank files with the SAME engine
;; the `find_files` tool uses — real typo-tolerant subsequence matching ranked
;; by frecency — instead of the substring-only `file-picker-score` heuristic.

(defn open-fuzzy-index
  "Open a FRESH fff instance scoped to the current workspace cwd, blocking
   until its initial scan completes. The caller OWNS the instance and MUST
   close it — but NEVER while a search may still be running on it: fff's
   native `search` crashes the JVM (SIGSEGV) on a closed handle."
  ^java.io.Closeable []
  (let
    [root
     (.toFile (cwd-path))

     idx
     (fff/create {:base-path (.getCanonicalPath root)
                  :watch? false
                  :ai-mode? true
                  :enable-mmap-cache? false})]

    (fff/wait-for-scan idx 30000)
    idx))

(defn fuzzy-file-rows
  "Frecency-ranked, typo-tolerant fuzzy file search via fff against an already
   OPEN index `idx` (see `open-fuzzy-index`) — the SAME engine behind the
   `find_files` tool. Returns rows shaped like `file-picker-items`
   (`:path :label :status-label :size-label :age-label`), capped at `limit`.

   A blank `query` yields fff's default frecency/recency ordering."
  ([idx query] (fuzzy-file-rows idx query {}))
  ([idx query {:keys [now-ms limit] :or {now-ms (System/currentTimeMillis) limit max-results}}]
   (->> (:items (fff/search idx {:query (or query "") :page-size limit}))
        (mapv (fn [{:keys [relative-path git-status size modified]}]
                (let
                  [status (when (and (string? git-status)
                                     (not (str/blank? git-status))
                                     (not= "clean" git-status))
                            git-status)]
                  {:path relative-path
                   :label relative-path
                   :status-label (or status "clean")
                   :size-label (format-bytes (or size 0))
                   ;; fff `:modified` is epoch SECONDS; the age helper wants ms.
                   :age-label (format-relative-age now-ms (* 1000 (long (or modified 0))))}))))))

(defn ->wire
  "Project ONE rich fuzzy/picker row (`:path :size-label :age-label
   :status-label`, the shape `fuzzy-file-rows` yields) into the channel-agnostic
   WIRE shape `{:name :size :age :status}` the gateway `/v1/sessions/:sid/suggest`
   service serves to the web composer.

   This is the SINGLE web-specific step: both the web and the TUI start from the
   SAME rich rows; the TUI renders them in-process (richer `size · age · status`
   chip), the web projects them last through here."
  [{:keys [path size-label age-label status-label]}]
  {:name (or (some-> path
                     str)
             "")
   :size (or size-label "")
   :age (or age-label "")
   :status (or status-label "")})

(defn suggest-file-rows
  "Self-contained fuzzy file suggestion for the shared `@`/suggest surface.

   Opens a FRESH fff index, searches `query`, and closes it SAFELY — the whole
   dance the gateway `/v1/sessions/:sid/suggest` service and any other caller
   would otherwise hand-roll. Reuses the SAME `fuzzy-file-rows` engine the TUI
   picker uses, then projects each rich row through `->wire`, so the web and TUI
   never diverge on ranking or field derivation — only on the final shape.

   Returns the channel-agnostic WIRE rows `{:name :size :age :status}` (bare
   relative path in `:name`), realized eagerly so nothing lazy escapes the closed
   native handle. Never throws; on any error yields `[]`.

   A blank `query` yields fff's default frecency/recency ordering."
  ([query] (suggest-file-rows query {}))
  ([query {:keys [limit] :or {limit max-results}}]
   (try (with-open [idx (open-fuzzy-index)]
          (into [] (map ->wire) (fuzzy-file-rows idx query {:limit limit})))
        (catch Throwable _ []))))
