(ns com.blockether.vis.internal.git
  "Shared JGit workspace inspection.

   UI surfaces and extensions should depend on this namespace instead of
   embedding their own JGit or shell-git calls. It reports small,
   renderable facts about the current repository and exposes read-only
   primitives for status, diff and log. Workspace lifecycle mutations
   stay in `com.blockether.vis.internal.workspace` because git worktree
   porcelain is still shell-backed there."
  (:require [com.blockether.vis.internal.workspace :as workspace])
  (:import [java.io ByteArrayOutputStream File]
           [org.eclipse.jgit.api Git Status]
           [org.eclipse.jgit.blame BlameResult]
           [org.eclipse.jgit.diff DiffEntry DiffFormatter Edit RawTextComparator]
           [org.eclipse.jgit.lib BranchTrackingStatus PersonIdent Repository]
           [org.eclipse.jgit.revwalk RevCommit RevWalk]
           [org.eclipse.jgit.storage.file FileRepositoryBuilder]
           [org.eclipse.jgit.treewalk AbstractTreeIterator CanonicalTreeParser FileTreeIterator]))

(set! *warn-on-reflection* true)

(def ^:private default-cache-ms 5000)

(defonce ^:private working-tree-status-cache
  (atom {:cwd nil :expires-at 0 :value nil}))

(defn cwd-file
  "Canonical current working directory as a File. Indirected for tests."
  ^File []
  (.getCanonicalFile (workspace/cwd)))

(defn open-repository
  "Open the git repository containing `start`, or nil when `start` is
   outside git. Caller owns closing the returned Repository."
  ^Repository [^File start]
  (try
    (let [^FileRepositoryBuilder builder (FileRepositoryBuilder.)]
      (.. builder
        (findGitDir start)
        readEnvironment)
      (when (.getGitDir builder)
        (.build builder)))
    (catch Throwable _ nil)))

(defn in-repository?
  "True when `start` is inside a git repository. Uses JGit only."
  [^File start]
  (boolean
    (when-let [^Repository repo (open-repository start)]
      (try true
        (finally
          (try (.close repo) (catch Throwable _ nil)))))))

(defn repo-name
  "Human label for a repository, currently its work-tree directory name."
  [^Repository repo]
  (some-> repo .getWorkTree .getName))

(defn branch-label
  "Current branch label. Detached HEADs are shortened to `detached:<sha>`."
  [^Repository repo]
  (let [branch (.getBranch repo)]
    (if (and branch (re-matches #"(?i)[0-9a-f]{40}" branch))
      (str "detached:" (subs branch 0 8))
      (or branch "unknown"))))

(defn head-id
  "Full HEAD object id, or nil when unavailable."
  [^Repository repo]
  (try
    (some-> (.resolve repo "HEAD") .getName)
    (catch Throwable _ nil)))

(defn count-status-sets
  "Count unique-ish paths across JGit status sets. JGit already de-dupes
   inside each set; callers choose which sets belong to one bucket."
  [& sets]
  (reduce + (map count sets)))

(defn status-counts
  "Detailed, environment-facing counters from JGit Status."
  [^Status status]
  (let [modified    (count (.getModified status))
        untracked   (count (.getUntracked status))
        added       (count (.getAdded status))
        changed     (count (.getChanged status))
        missing     (count (.getMissing status))
        removed     (count (.getRemoved status))
        conflicting (count (.getConflicting status))
        changes?    (boolean (some pos? [modified untracked added changed
                                         missing removed conflicting]))]
    {:clean?      (.isClean status)
     :dirty?      (not (.isClean status))
     :changes?    changes?
     :modified    modified
     :untracked   untracked
     :added       added
     :changed     changed
     :missing     missing
     :removed     removed
     :conflicting conflicting}))

(defn dirty-counts
  "Summarise JGit Status into user-facing dirty buckets."
  [^Status status]
  (if (.isClean status)
    {:modified 0
     :created  0
     :deleted  0}
    {:modified (count-status-sets (.getModified status)
                 (.getChanged status))
     :created  (count-status-sets (.getAdded status)
                 (.getUntracked status)
                 (.getUntrackedFolders status))
     :deleted  (count-status-sets (.getRemoved status)
                 (.getMissing status))}))

(defn tracking-counts
  "Ahead/behind counts for `branch`, or nil when there is no upstream."
  [^Repository repo branch]
  (try
    (when-let [^BranchTrackingStatus tracking (BranchTrackingStatus/of repo branch)]
      {:upstream? true
       :ahead  (.getAheadCount tracking)
       :behind (.getBehindCount tracking)})
    (catch Throwable _ nil)))

(defn- sorted-entries
  [status paths]
  (mapv (fn [path] {:status status :file path}) (sort paths)))

(defn status-entries
  "Porcelain-like status entries from JGit Status.

   Status codes intentionally stay compact (`M`, `A`, `D`, `??`, `UU`)
   because callers render summaries, not exact index/worktree XY state."
  [^Status status]
  (->> (concat
         (sorted-entries "A"  (.getAdded status))
         (sorted-entries "M"  (.getChanged status))
         (sorted-entries "M"  (.getModified status))
         (sorted-entries "D"  (.getMissing status))
         (sorted-entries "D"  (.getRemoved status))
         (sorted-entries "??" (.getUntracked status))
         (sorted-entries "??" (.getUntrackedFolders status))
         (sorted-entries "UU" (.getConflicting status)))
    distinct
    vec))

(defn status-snapshot
  "Read branch, head and porcelain-like entries for `start` with JGit."
  [^File start]
  (when-let [^Repository repo (open-repository start)]
    (try
      (let [^Git git    (Git/wrap repo)
            ^Status st (try
                         (.. git status call)
                         (finally
                           (try (.close git) (catch Throwable _ nil))))]
        {:branch  (branch-label repo)
         :head    (head-id repo)
         :clean?  (.isClean st)
         :entries (status-entries st)})
      (finally
        (try (.close repo) (catch Throwable _ nil))))))

(defn- tree-parser
  ^CanonicalTreeParser [^Repository repo rev]
  (when-let [commit-id (.resolve repo ^String rev)]
    (let [parser (CanonicalTreeParser.)]
      (with-open [reader (.newObjectReader repo)
                  walk   (RevWalk. repo)]
        (let [commit (.parseCommit walk commit-id)]
          (.reset parser reader (.getId (.getTree commit)))))
      parser)))

(defn- diff-path
  [^DiffEntry entry]
  (case (some-> (.getChangeType entry) .name)
    "DELETE" (.getOldPath entry)
    (.getNewPath entry)))

(defn- entry-numstat
  [^DiffFormatter formatter ^DiffEntry entry]
  (let [path (diff-path entry)]
    (try
      (let [edits (.toEditList (.toFileHeader formatter entry))
            adds  (reduce (fn [n ^Edit edit]
                            (+ n (- (.getEndB edit) (.getBeginB edit))))
                    0 edits)
            dels  (reduce (fn [n ^Edit edit]
                            (+ n (- (.getEndA edit) (.getBeginA edit))))
                    0 edits)]
        {:file path :+ adds :- dels})
      (catch Throwable _
        {:file path :+ 0 :- 0}))))

(defn- untracked-paths
  [^Repository repo]
  (try
    (let [^Git git (Git/wrap repo)]
      (try
        (let [^Status status (.. git status call)]
          (set (concat (.getUntracked status) (.getUntrackedFolders status))))
        (finally
          (try (.close git) (catch Throwable _ nil)))))
    (catch Throwable _ #{})))

(defn diff-numstat
  "Return git-numstat-like entries using JGit.

   `old-rev` is required. `new-rev` nil means compare `old-rev` to the
   working tree, matching `git diff <old-rev>` for tracked files.
   When `path` is non-nil, restrict the diff to entries whose old- or
   new-path matches that repo-relative path (prefix match for dirs)."
  ([^File start old-rev new-rev] (diff-numstat start old-rev new-rev nil))
  ([^File start old-rev new-rev path]
   (when-let [^Repository repo (open-repository start)]
     (try
       (with-open [out       (ByteArrayOutputStream.)
                   formatter (doto (DiffFormatter. out)
                               (.setRepository repo)
                               (.setDiffComparator RawTextComparator/DEFAULT)
                               (.setDetectRenames true))]
         (if-let [old-tree (tree-parser repo old-rev)]
           (let [^AbstractTreeIterator new-tree (if new-rev
                                                  (tree-parser repo new-rev)
                                                  (FileTreeIterator. repo))
                 untracked (when-not new-rev (untracked-paths repo))
                 path-match? (cond
                               (nil? path) (constantly true)
                               (= "" path) (constantly true)
                               :else       (fn [p]
                                             (or (= p path)
                                               (.startsWith ^String p (str path "/")))))]
             (if new-tree
               (->> (.scan formatter ^AbstractTreeIterator old-tree new-tree)
                 (remove #(contains? untracked (diff-path %)))
                 (filter #(path-match? (diff-path %)))
                 (mapv #(entry-numstat formatter %)))
               []))
           []))
       (finally
         (try (.close repo) (catch Throwable _ nil)))))))

(defn- commit->map
  "Convert a JGit RevCommit into the canonical commit map. Centralised so
   every git/* surface (log, show, blame line) returns the same shape."
  [^RevCommit commit]
  (let [author    (.getAuthorIdent commit)
        committer (.getCommitterIdent commit)]
    {:sha              (some-> (.getId commit) .getName)
     :short-sha        (some-> (.getId commit) .getName (subs 0 7))
     :author           (some-> author .getName)
     :email            (some-> author .getEmailAddress)
     :at               (long (.getCommitTime commit))
     :committer        (some-> committer .getName)
     :committer-email  (some-> committer .getEmailAddress)
     :committed-at     (long (.getCommitTime commit))
     :subject          (.getShortMessage commit)
     :body             (.getFullMessage commit)
     :parents          (mapv (fn [^RevCommit p] (some-> (.getId p) .getName))
                         (.getParents commit))}))

(defn- ->date
  "Coerce `x` into a java.util.Date. Accepts already-a-Date, epoch ms
   (long), epoch seconds (long < 1e12), `java.time.Instant`, or an ISO-8601
   date / datetime string. Returns nil for nil. Throws ex-info for
   anything else so callers see a clean usage error instead of a JVM
   cast deep in JGit."
  ^java.util.Date [x]
  (cond
    (nil? x)                      nil
    (instance? java.util.Date x)  x
    (instance? java.time.Instant x) (java.util.Date/from x)
    (number? x)                   (java.util.Date.
                                    (long (if (< (long x) 100000000000)
                                            (* (long x) 1000)
                                            x)))
    (string? x)
    (let [s ^String x]
      (try
        (cond
          (re-matches #"\d{4}-\d{2}-\d{2}" s)
          (java.util.Date/from (.toInstant (.atStartOfDay (java.time.LocalDate/parse s)
                                             (java.time.ZoneId/systemDefault))))
          :else
          (java.util.Date/from (java.time.Instant/parse s)))
        (catch Throwable t
          (throw (ex-info (str "git: cannot parse date " (pr-str x))
                   {:type :foundation-git/invalid-date :date x} t)))))
    :else
    (throw (ex-info (str "git: cannot coerce to date " (pr-str x))
             {:type :foundation-git/invalid-date :date x}))))

(defn recent-commits
  "Recent commits for `start`, newest first, using JGit only.

   `opts` is an optional map:
     :path        restrict to commits touching this repo-relative path
     :ref         start log from this branch/sha (default HEAD)
     :since       lower-bound author date — Date, epoch ms/s, ISO string
     :until       upper-bound author date — same coercion as :since
     :author      substring match on author name OR email (case-insensitive)"
  ([^File start n] (recent-commits start n nil))
  ([^File start n opts]
   (when-let [^Repository repo (open-repository start)]
     (try
       (let [^Git git (Git/wrap repo)
             {:keys [path ref since until author]} (or opts {})
             since-d (->date since)
             until-d (->date until)
             auth-q  (when (and author (string? author) (seq author))
                       (clojure.string/lower-case author))]
         (try
           (let [cmd (.. git log (setMaxCount (int (max 1 (* 4 n)))))
                 _   (when (and ref (string? ref))
                       (when-let [oid (.resolve repo ^String ref)]
                         (.add cmd oid)))
                 _   (when (and path (string? path) (seq path))
                       (.addPath cmd path))
                 _   (cond
                       (and since-d until-d)
                       (.setRevFilter cmd
                         (org.eclipse.jgit.revwalk.filter.CommitTimeRevFilter/between
                           since-d until-d))
                       since-d
                       (.setRevFilter cmd
                         (org.eclipse.jgit.revwalk.filter.CommitTimeRevFilter/after since-d))
                       until-d
                       (.setRevFilter cmd
                         (org.eclipse.jgit.revwalk.filter.CommitTimeRevFilter/before until-d)))
                 matches? (fn [m]
                            (or (nil? auth-q)
                              (clojure.string/includes?
                                (clojure.string/lower-case
                                  (str (:author m) " " (:email m)))
                                auth-q)))]
             (->> (.call cmd)
               (map commit->map)
               (filter matches?)
               (take (max 1 n))
               vec))
           (finally
             (try (.close git) (catch Throwable _ nil)))))
       (finally
         (try (.close repo) (catch Throwable _ nil)))))))

(defn- count-lines
  "Number of LF-delimited lines in a byte array. JGit emits one entry per
   blob; root-commit numstat reports `+N` where N = line count."
  ^long [^bytes bs]
  (if (zero? (alength bs))
    0
    (let [len (alength bs)
          nl  (reduce (fn [n i] (if (= 10 (aget bs i)) (inc n) n))
                0 (range len))]
      ;; Match `wc -l` semantics: trailing-newline files = N lines, no-
      ;; trailing-newline files = N+1 (counts the final unterminated line).
      (if (= 10 (aget bs (dec len))) nl (inc nl)))))

(defn- root-commit-numstat
  "Walk every blob reachable from `commit`'s tree and emit numstat-shaped
   entries (`{:file path :+ <lines> :- 0}`). Used when a commit has no
   parent, where the regular `diff-numstat` path has no tree to compare
   against."
  [^Repository repo ^RevCommit commit]
  (with-open [reader (.newObjectReader repo)
              tw     (org.eclipse.jgit.treewalk.TreeWalk. repo)]
    (.addTree tw (.getTree commit))
    (.setRecursive tw true)
    (loop [acc []]
      (if (.next tw)
        (let [path (.getPathString tw)
              oid  (.getObjectId tw 0)
              bytes (try (.getBytes (.open reader oid)) (catch Throwable _ (byte-array 0)))]
          (recur (conj acc {:file path :+ (count-lines bytes) :- 0})))
        acc))))

(defn show-commit
  "Detailed view of a single commit. Returns the canonical commit map
   enriched with `:files` (numstat against first parent, or every blob
   in the commit's tree for root commits) and `:stat` totals."
  [^File start rev]
  (when-let [^Repository repo (open-repository start)]
    (try
      (when-let [oid (try (.resolve repo ^String rev) (catch Throwable _ nil))]
        (with-open [walk (RevWalk. repo)]
          (let [commit  (.parseCommit walk oid)
                _       (.parseBody walk commit)
                base    (commit->map commit)
                parents (.getParents commit)
                parent  (when (pos? (alength parents))
                          (some-> ^RevCommit (aget parents 0) .getId .getName))
                files   (vec (or (if parent
                                   (diff-numstat start parent
                                     (.getName (.getId commit)))
                                   (root-commit-numstat repo commit))
                               []))
                +sum    (reduce + 0 (map :+ files))
                -sum    (reduce + 0 (map :- files))]
            (assoc base
              :files files
              :stat  {:files (count files) :+ +sum :- -sum}))))
      (finally
        (try (.close repo) (catch Throwable _ nil))))))

(defn- repo-relative-path
  "Normalise an absolute or repo-relative path against the repo work tree.
   Returns a forward-slash-separated path JGit's BlameCommand expects, or
   nil if `path` escapes the work tree."
  [^Repository repo ^String path]
  (let [^File work-tree (.getWorkTree repo)
        ^File work-can  (.getCanonicalFile work-tree)
        ^File f         (let [^File candidate (java.io.File. path)]
                          (if (.isAbsolute candidate)
                            candidate
                            (java.io.File. work-tree path)))
        ^File can       (try (.getCanonicalFile f) (catch Throwable _ nil))]
    (when can
      (let [^String work-prefix (str (.getPath work-can) java.io.File/separator)
            ^String can-path    (.getPath can)]
        (when (or (= can-path (.getPath work-can))
                (.startsWith can-path work-prefix))
          (let [rel (subs can-path (count work-prefix))]
            (clojure.string/replace rel java.io.File/separator "/")))))))

(defn blame-file
  "Per-line blame for `path` inside the repo containing `start`. Returns
   `{:path :head :lines [{:line :sha :short-sha :author :email :at :content :source-line} ...]}`
   or nil when `path` is outside the repo / not tracked.

   `opts` map:
     :from L  1-based start line (inclusive)
     :to   L  1-based end line   (inclusive)"
  ([^File start path] (blame-file start path nil))
  ([^File start path opts]
   (when-let [^Repository repo (open-repository start)]
     (try
       (when-let [rel (repo-relative-path repo path)]
         (let [^Git git (Git/wrap repo)
               {:keys [from to]} (or opts {})]
           (try
             (let [^BlameResult result (.. git blame (setFilePath rel) call)]
               (when result
                 (.computeAll result)
                 (let [contents   (.getResultContents result)
                       total      (.size contents)
                       lo         (max 0 (dec (long (or from 1))))
                       hi-default (dec total)
                       hi         (min hi-default
                                    (dec (long (or to total))))
                       lines      (when (<= lo hi)
                                    (mapv
                                      (fn [i]
                                        (let [^RevCommit commit  (.getSourceCommit result i)
                                              ^PersonIdent author (when commit (.getSourceAuthor result i))
                                              sha     (some-> commit .getId .getName)]
                                          {:line       (inc i)
                                           :sha        sha
                                           :short-sha  (when sha (subs sha 0 7))
                                           :author     (some-> author .getName)
                                           :email      (some-> author .getEmailAddress)
                                           :at         (when author (long (.getTime (.getWhen author))))
                                           :source-line (inc (.getSourceLine result i))
                                           :content    (.getString contents i)}))
                                      (range lo (inc hi))))]
                   {:path  rel
                    :head  (head-id repo)
                    :total total
                    :lines (or lines [])})))
             (finally
               (try (.close git) (catch Throwable _ nil))))))
       (finally
         (try (.close repo) (catch Throwable _ nil)))))))

(defn working-tree-status
  "Return git facts for `start` (default cwd).

   Outside git, returns `{:workspace? false}` so callers can explicitly
   render the absence of a workspace. Inside git, returns:

     {:workspace? true
      :repo <repo-name>
      :branch <branch-name>
      :modified 2 :created 1 :deleted 0
      :upstream? true :ahead 4 :behind 0}"
  ([] (working-tree-status (cwd-file)))
  ([^File start]
   (if-let [^Repository repo (open-repository start)]
     (try
       (let [branch   (branch-label repo)
             tracking (or (tracking-counts repo branch)
                        {:upstream? false :ahead 0 :behind 0})
             ^Git git (Git/wrap repo)
             ^Status status (try
                              (.. git status call)
                              (finally
                                (try (.close git) (catch Throwable _ nil))))]
         (merge {:workspace? true
                 :repo (repo-name repo)
                 :branch branch}
           (dirty-counts status)
           tracking))
       (catch Throwable _
         {:workspace? false})
       (finally
         (try (.close repo) (catch Throwable _ nil))))
     {:workspace? false})))

(defn cached-working-tree-status
  "Cached `working-tree-status` for hot render paths.

   The TUI footer repaints often, and other surfaces will need the same
   git workspace facts. Keep the cache here so every caller shares one
   resolved view instead of each UI namespace running JGit independently."
  ([] (cached-working-tree-status (System/currentTimeMillis) default-cache-ms))
  ([^File start]
   (cached-working-tree-status start (System/currentTimeMillis) default-cache-ms))
  ([now-ms ttl-ms]
   (let [cwd (.getPath (cwd-file))
         {:keys [expires-at value]} @working-tree-status-cache]
     (if (and (= cwd (:cwd @working-tree-status-cache))
           (< (long now-ms) (long expires-at)))
       value
       (let [value (working-tree-status)]
         (reset! working-tree-status-cache {:cwd cwd
                                            :expires-at (+ (long now-ms) (long ttl-ms))
                                            :value value})
         value))))
  ([^File start now-ms ttl-ms]
   (let [cwd (.getPath (.getCanonicalFile start))
         {:keys [expires-at value]} @working-tree-status-cache]
     (if (and (= cwd (:cwd @working-tree-status-cache))
           (< (long now-ms) (long expires-at)))
       value
       (let [value (working-tree-status start)]
         (reset! working-tree-status-cache {:cwd cwd
                                            :expires-at (+ (long now-ms) (long ttl-ms))
                                            :value value})
         value)))))
