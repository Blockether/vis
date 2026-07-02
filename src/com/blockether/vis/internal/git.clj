(ns com.blockether.vis.internal.git
  "Shared JGit workspace inspection.

   UI surfaces and extensions should depend on this namespace instead of
   embedding their own JGit or shell-git calls. It reports small,
   renderable facts about the current repository and exposes read-only
   primitives for status, diff and log. Workspace lifecycle mutations
   stay in `com.blockether.vis.internal.workspace` because git worktree
   porcelain is still shell-backed there."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.workspace :as workspace])
  (:import [java.io ByteArrayOutputStream File]
           [org.eclipse.jgit.api Git Status]
           [org.eclipse.jgit.blame BlameResult]
           [org.eclipse.jgit.diff DiffEntry DiffFormatter Edit RawText RawTextComparator]
           [org.eclipse.jgit.lib BranchTrackingStatus ObjectId ObjectReader PersonIdent Repository]
           [org.eclipse.jgit.revwalk RevCommit RevWalk]
           [org.eclipse.jgit.storage.file FileRepositoryBuilder]
           [org.eclipse.jgit.treewalk AbstractTreeIterator CanonicalTreeParser FileTreeIterator]))

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
  "True when `start` is inside a git repository. Uses JGit only.
   Closes the discovered Repository via `with-open` so callers can
   probe freely without leaking handles."
  [^File start]
  (if-let [^Repository repo (open-repository start)]
    (with-open [_ repo] true)
    false))

(defn vcs-kind
  "The `:vcs/kind` for a workspace `root`: `:git` when the root sits inside
   a git repository, else `:none`. Single source of truth so the model-facing
   CTX, the env, and channels all agree — and stay inside the ctx-spec set
   #{:git :hg :jj :fossil :none}. NOTE: `:rift` (the CoW-clone sandbox
   mechanism) is NOT a VCS and must never appear here; sandbox-ness lives on
   `:workspace/sandbox?`."
  [root]
  (if (and root
        (let [f (File. (str root))]
          (and (.exists f) (in-repository? f))))
    :git
    :none))

(defn repo-name
  "Human label for a repository, currently its work-tree directory name."
  [^Repository repo]
  (some-> repo .getWorkTree .getName))

(defn repo-work-tree
  "Canonical work-tree directory of the git repository containing `start`,
  or nil when `start` is outside any git repo. Used to tell whether an
  added filesystem root lives in a DIFFERENT repository than the primary
  workspace root (subdirs of the primary repo resolve to the SAME work
  tree, so they are already covered by the primary status snapshot)."
  ^File [^File start]
  (when-let [^Repository repo (open-repository start)]
    (try
      (some-> repo .getWorkTree .getCanonicalFile)
      (finally
        (try (.close repo) (catch Throwable _ nil))))))

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

(defn file-dirty?
  "True when `f` is a TRACKED file carrying UNCOMMITTED changes — modified in
   the worktree, staged, deleted/missing, or conflicting. An UNTRACKED
   (brand-new) file is NOT dirty (write is how you create one) and a clean
   tracked file is fine. Scoped to the single path via `StatusCommand.addPath`
   so it stays cheap on a large repo. Repo-less / nil-safe → false."
  [^File f]
  (boolean
    (when (and f (.exists ^File f))
      (when-let [^Repository repo (open-repository f)]
        (try
          (let [^Git git   (Git/wrap repo)
                rel        (-> (.toPath (.getCanonicalFile (.getWorkTree repo)))
                             (.relativize (.toPath (.getCanonicalFile ^File f)))
                             str (.replace "\\" "/"))
                ^Status st (try (.. git status (addPath rel) call)
                             (finally (try (.close git) (catch Throwable _ nil))))]
            (or (seq (.getModified st)) (seq (.getChanged st))
              (seq (.getRemoved st)) (seq (.getMissing st))
              (seq (.getConflicting st))))
          (finally (try (.close repo) (catch Throwable _ nil))))))))

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

(def ^:private binary-peek-bytes
  "How many bytes to read before classifying a blob as binary. JGit's
   own DiffFormatter uses the same window via `RawText.isBinary`; a
   binary marker in the first 8KB is the canonical heuristic git C
   itself ships with (null byte or non-text byte pattern)."
  8192)

(defn- blob-peek
  "Read at most `binary-peek-bytes` from `oid` via `reader`. Returns the
   byte array (possibly short) or nil when the blob can't be opened."
  ^bytes [^ObjectReader reader ^ObjectId oid]
  (try
    (let [loader (.open reader oid)]
      (with-open [stream (.openStream loader)]
        (let [buf (byte-array binary-peek-bytes)
              n   (.read stream buf 0 binary-peek-bytes)]
          (cond
            (neg? n)                       (byte-array 0)
            (= n binary-peek-bytes)        buf
            :else                          (java.util.Arrays/copyOf buf n)))))
    (catch Throwable _ nil)))

(defn- binary-blob?
  "True when the blob `oid` looks binary (null byte / non-text pattern
   in first `binary-peek-bytes`). Missing or empty blobs are NOT binary."
  [^ObjectReader reader ^ObjectId oid]
  (boolean
    (when (and oid (not= oid (ObjectId/zeroId)))
      (when-let [peek (blob-peek reader oid)]
        (and (pos? (alength peek))
          (RawText/isBinary peek))))))

(defn- diff-entry-blob-id
  "Resolve the active blob id for one side of a DiffEntry. JGit gives
   us an AbbreviatedObjectId which may be incomplete; `toObjectId` makes
   it real when it can. Returns nil for the zero id (missing side, e.g.
   ADD entry's old side or DELETE entry's new side)."
  ^ObjectId [^DiffEntry entry side]
  (let [aid (case side
              :old (.getOldId entry)
              :new (.getNewId entry))]
    (when aid
      (let [oid (.toObjectId aid)]
        (when (and oid (not= oid (ObjectId/zeroId)))
          oid)))))

(defn- entry-binary?
  "True when either side of `entry` is a binary blob. Conservative: a
   single binary side is enough to flip the entry to binary, mirroring
   git's behaviour for adds, deletes, and modifies."
  [^Repository repo ^DiffEntry entry]
  (with-open [reader (.newObjectReader repo)]
    (or (binary-blob? reader (diff-entry-blob-id entry :old))
      (binary-blob? reader (diff-entry-blob-id entry :new)))))

(defn- entry-numstat
  "Numstat for one DiffEntry. Binary entries are flagged `:binary? true`
   and reported as `:add 0 :del 0` so callers can distinguish 'no real text
   changes' from 'this is a binary blob, line counts are meaningless'."
  [^Repository repo ^DiffFormatter formatter ^DiffEntry entry]
  (let [path (diff-path entry)]
    (if (entry-binary? repo entry)
      {:file path :add 0 :del 0 :binary? true}
      (try
        (let [edits (.toEditList (.toFileHeader formatter entry))
              adds  (reduce (fn [n ^Edit edit]
                              (+ n (- (.getEndB edit) (.getBeginB edit))))
                      0 edits)
              dels  (reduce (fn [n ^Edit edit]
                              (+ n (- (.getEndA edit) (.getBeginA edit))))
                      0 edits)]
          {:file path :add adds :del dels})
        (catch Throwable _
          {:file path :add 0 :del 0})))))

(def ^:private default-patch-byte-cap
  "Per-entry cap for unified-diff text. Huge binary or generated diffs
   would otherwise blow LLM context; we truncate and append a marker so
   the model knows there is more."
  65536)

(defn- entry-patch
  "Format `entry` as unified-diff text using the SAME DiffFormatter that
  scanned the entries. Binary entries skip the formatter entirely and
  return a deterministic 'Binary files differ' marker so callers never
  see UTF-8-garbled bytes leaking into the patch string. Text entries
  reset the shared formatter's output stream, format the single entry,
  and read it back. We MUST reuse the scan formatter: the new side of a
  working-tree diff lives in the working tree, not the object database,
  so a fresh formatter cannot re-resolve it and silently returns empty."
  [^Repository repo ^DiffFormatter formatter ^ByteArrayOutputStream out
   ^DiffEntry entry byte-cap]
  (if (entry-binary? repo entry)
    (let [op (some-> (.getChangeType entry) .name)
          old-path (.getOldPath entry)
          new-path (.getNewPath entry)]
      (str "Binary files "
        (case op
          "ADD"    (str "/dev/null and b/" new-path)
          "DELETE" (str "a/" old-path " and /dev/null")
          (str "a/" old-path " and b/" new-path))
        " differ\n"))
    (try
      (.reset out)
      (.format formatter entry)
      (.flush formatter)
      (let [bytes (.toByteArray out)
            n     (alength bytes)]
        (if (<= n byte-cap)
          (String. bytes "UTF-8")
          (str (String. bytes 0 (int byte-cap) "UTF-8")
            "\n[…patch truncated, " (- n byte-cap) " more bytes]")))
      (catch Throwable _ ""))))

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
   new-path matches that repo-relative path (prefix match for dirs).

   When `with-patch?` is true, each entry carries a `:patch` field with
   the per-file unified-diff text (truncated at `default-patch-byte-cap`)."
  ([^File start old-rev new-rev] (diff-numstat start old-rev new-rev nil false))
  ([^File start old-rev new-rev path] (diff-numstat start old-rev new-rev path false))
  ([^File start old-rev new-rev path with-patch?]
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
                 (mapv (fn [^DiffEntry e]
                         (let [ns (entry-numstat repo formatter e)]
                           (if with-patch?
                             (assoc ns :patch (entry-patch repo formatter out e default-patch-byte-cap))
                             ns)))))
               []))
           []))
       (finally
         (try (.close repo) (catch Throwable _ nil)))))))

(def ^:private body-byte-cap
  "Per-commit cap for the full message body. Most commits are <500B but
   merges, release notes, and ChatGPT-style commit messages can run
   tens of KB; we truncate so a single (git/log {:limit 200}) call
   can't dump 4MB of historical commit bodies into the model."
  4096)

(defn- cap-body
  ^String [^String s]
  (if (<= (count s) body-byte-cap)
    s
    (str (subs s 0 body-byte-cap)
      "\n[…body truncated, " (- (count s) body-byte-cap) " more bytes]")))

(defn- ident-millis
  "Epoch milliseconds for a JGit PersonIdent's `when` field. Used to
   keep `:at` consistent across git/log, git/show, and git/blame — JGit
   exposes commit-time as POSIX seconds and PersonIdent.when as Java
   millis, so we normalise to millis everywhere."
  ^long [^PersonIdent ident]
  (long (.getTime (.getWhen ident))))

(defn- commit->map
  "Convert a JGit RevCommit into the canonical commit map. Centralised so
   every git/* surface (log, show, blame line) returns the same shape.
   `:at` is the AUTHOR time in millis (when the work was done) and
   `:committed-at` is the COMMITTER time in millis (when the object was
   written). They're equal for normal commits; rebase / cherry-pick /
   amend split them apart, which is exactly when the model needs the
   distinction."
  [^RevCommit commit]
  (let [author    (.getAuthorIdent commit)
        committer (.getCommitterIdent commit)
        sha       (some-> (.getId commit) .getName)]
    {:sha              sha
     :short-sha        (when sha (subs sha 0 7))
     :author           (some-> author .getName)
     :email            (some-> author .getEmailAddress)
     :at               (when author (ident-millis author))
     :committer        (some-> committer .getName)
     :committer-email  (some-> committer .getEmailAddress)
     :committed-at     (when committer (ident-millis committer))
     :subject          (.getShortMessage commit)
     :body             (cap-body (.getFullMessage commit))
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
                       (str/lower-case author))]
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

(def ^:private root-numstat-cap
  "Max number of blob entries returned by root-commit-numstat. Repos
   with thousands of files in the initial commit would otherwise blow
   model context with one entry per path."
  500)

(defn- root-commit-numstat
  "Walk every blob reachable from `commit`'s tree and emit numstat-shaped
   entries. Binary blobs are flagged `:binary? true` with `:add 0 :del 0`
   so we don't pretend null-byte counts are line counts. Text blobs go
   through `count-lines` like normal. Used when a commit has no parent
   and the regular `diff-numstat` path has no tree to compare against.

   Capped at `root-numstat-cap`; callers see a trailing
   `{:truncated? true :remaining N}` marker entry when the tree had more."
  [^Repository repo ^RevCommit commit]
  (with-open [reader (.newObjectReader repo)
              tw     (org.eclipse.jgit.treewalk.TreeWalk. repo)]
    (.addTree tw (.getTree commit))
    (.setRecursive tw true)
    (loop [acc []]
      (cond
        (>= (count acc) root-numstat-cap)
        (let [remaining (loop [n 0] (if (.next tw) (recur (inc n)) n))]
          (cond-> acc
            (pos? remaining) (conj {:truncated? true :remaining remaining})))

        (.next tw)
        (let [path (.getPathString tw)
              oid  (.getObjectId tw 0)]
          (recur
            (conj acc
              (if (binary-blob? reader oid)
                {:file path :add 0 :del 0 :binary? true}
                (let [bytes (try (.getBytes (.open reader oid))
                              (catch Throwable _ (byte-array 0)))]
                  {:file path :add (count-lines bytes) :del 0})))))

        :else acc))))

(defn show-commit
  "Detailed view of a single commit. Returns the canonical commit map
   enriched with `:files` (numstat against first parent, or every blob
   in the commit's tree for root commits) and `:stat` totals.

   `opts` map:
     :with-patch?  when true, every file entry also carries `:patch`
                   with the per-file unified-diff text (truncated at
                   `default-patch-byte-cap` bytes)."
  ([^File start rev] (show-commit start rev nil))
  ([^File start rev opts]
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
                 with-patch? (boolean (:with-patch? opts))
                 files   (vec (or (if parent
                                    (diff-numstat start parent
                                      (.getName (.getId commit))
                                      nil with-patch?)
                                    (root-commit-numstat repo commit))
                                []))
                 +sum    (reduce + 0 (map :add files))
                 -sum    (reduce + 0 (map :del files))]
             (assoc base
               :files files
               :stat  {:files (count files) :add +sum :del -sum}))))
       (finally
         (try (.close repo) (catch Throwable _ nil)))))))

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

(defn- run-blame
  "Run JGit's blame on `rel` (repo-relative path) inside `repo`. When
   `start-sha` is non-nil, walk back from that commit; otherwise start
   at HEAD. Returns a fully computed `BlameResult` or nil."
  ^BlameResult [^Repository repo ^String rel start-sha]
  (let [^Git git (Git/wrap repo)]
    (try
      (let [cmd (.. git blame (setFilePath rel) (setFollowFileRenames true))]
        (when start-sha
          (try (.setStartCommit cmd (org.eclipse.jgit.lib.ObjectId/fromString ^String start-sha))
            (catch Throwable _ nil)))
        (when-let [^BlameResult r (.call cmd)]
          (.computeAll r)
          r))
      (finally
        (try (.close git) (catch Throwable _ nil))))))

(defn- first-parent-sha
  "Sha of the first parent of `sha`, or nil for root commits / unknown."
  [^Repository repo ^String sha]
  (try
    (with-open [walk (RevWalk. repo)]
      (let [oid     (.resolve repo ^String sha)
            commit  (.parseCommit walk oid)
            parents (.getParents commit)]
        (when (pos? (alength parents))
          (some-> ^RevCommit (aget parents 0) .getId .getName))))
    (catch Throwable _ nil)))

(defn- normalize-sha
  "Resolve `s` (sha prefix, branch, HEAD~N, ...) to its full sha string
   inside `repo`, or nil if it cannot be resolved."
  [^Repository repo s]
  (when (string? s)
    (try (some-> (.resolve repo ^String s) .getName)
      (catch Throwable _ nil))))

(defn- blame-line-record
  "Materialise the canonical per-line blame entry from index `i` of a
   BlameResult."
  [^BlameResult result ^long i]
  (let [^RevCommit commit   (.getSourceCommit result i)
        ^PersonIdent author (when commit (.getSourceAuthor result i))
        sha                 (some-> commit .getId .getName)
        contents            (.getResultContents result)]
    {:line        (inc i)
     :sha         sha
     :short-sha   (when sha (subs sha 0 7))
     :author      (some-> author .getName)
     :email       (some-> author .getEmailAddress)
     :at          (when author (ident-millis author))
     :source-line (inc (.getSourceLine result i))
     :content     (.getString contents i)}))

(defn- normalize-ws
  "Collapse runs of ASCII whitespace into a single space and trim. Used
   as the fallback equality predicate when peeling past whitespace-only
   refactor commits: the bytes differ but the underlying line is the
   same once normalized."
  [^String s]
  (when s
    (-> s
      (.replaceAll "\\s+" " ")
      (.trim))))

(defn- find-line-with [^BlameResult result eq? hint-i]
  (let [contents (.getResultContents result)
        n        (long (.size contents))
        hi       (long hint-i)]
    (when (pos? n)
      (loop [delta 0]
        (let [a (- hi delta)
              b (+ hi delta)]
          (cond
            (and (>= a 0) (< a n)
              (eq? (.getString contents a)))
            a
            (and (not= a b) (>= b 0) (< b n)
              (eq? (.getString contents b)))
            b
            (and (< a 0) (>= b n))
            nil
            :else
            (recur (inc delta))))))))

(defn- find-nearest-matching-line
  "Search a BlameResult for a line whose text matches `target`, preferring
   the line whose index is closest to `hint-i`. Returns the matched
   index or nil. Used to map a line through a re-blame after peeling
   past an ignored commit, where line numbers shift but text typically
   stays close.

   Tries exact byte equality first; falls back to a whitespace-normalized
   compare so peeling past pure whitespace / formatter commits still
   finds the corresponding line in the parent blame."
  [^BlameResult result ^String target ^long hint-i]
  (or (find-line-with result #(.equals target ^String %) hint-i)
    (let [^String tn (normalize-ws target)]
      (find-line-with result #(.equals tn ^String (normalize-ws ^String %)) hint-i))))

(def ^:private max-blame-peel-depth
  "Hard cap on how many ignored commits we'll peel through for a single
   line before giving up and returning the last attribution. Prevents
   pathological loops on adversarial histories. 16 matches git itself's
   default for `--ignore-rev` recursion in practice."
  16)

(defn- peel-blame-line
  "For line `i` in `result`, if its source sha is in `ignored`, re-blame
   the file starting at the parent of that sha and map the line forward
   by content/index proximity. Recurse until the attribution leaves the
   ignore set, the parent chain ends, or `max-blame-peel-depth` fires.
   `cache` is an `atom {sha BlameResult}` shared across lines so each
   peel-target re-blame happens at most once per call.

   `i` is intentionally NOT primitive-hinted: Clojure caps primitive-
   hinted fns at 4 args and this one takes 6."
  [^Repository repo ^String rel ^BlameResult result i ignored cache]
  (loop [^BlameResult res result i (long i) depth 0]
    (let [^RevCommit commit (.getSourceCommit res i)
          sha (some-> commit .getId .getName)]
      (if (or (>= depth max-blame-peel-depth)
            (nil? sha)
            (not (contains? ignored sha)))
        (blame-line-record res i)
        (let [parent (first-parent-sha repo sha)]
          (if (nil? parent)
            (blame-line-record res i)
            (let [target-content (.getString (.getResultContents res) i)
                  alt (or (get @cache parent)
                        (let [r (run-blame repo rel parent)]
                          (swap! cache assoc parent r)
                          r))
                  alt-i (when alt
                          (find-nearest-matching-line alt target-content i))]
              (if (nil? alt-i)
                (blame-line-record res i)
                (recur alt (long alt-i) (inc depth))))))))))

(defn blame-file
  "Per-line blame for `path` inside the repo containing `start`. Returns
   `{:path :head :total :ignored-revs :lines [...]}` or nil when `path` is
   outside the repo / not tracked.

   `opts` map:
     :from L         1-based start line (inclusive)
     :to   L         1-based end line   (inclusive)
     :ignore-revs S  vec of sha strings (full or prefix) to peel past.
                     Lines attributed to an ignored commit are re-blamed
                     from that commit's parent, recursively. Capped at
                     `max-blame-peel-depth` to keep adversarial inputs
                     safe."
  ([^File start path] (blame-file start path nil))
  ([^File start path opts]
   (when-let [^Repository repo (open-repository start)]
     (try
       (when-let [rel (repo-relative-path repo path)]
         (let [{:keys [from to ignore-revs]} (or opts {})
               ignored (when (seq ignore-revs)
                         (->> ignore-revs
                           (keep #(normalize-sha repo %))
                           set))
               head-binary?
               ;; Open HEAD's tree properly: resolve HEAD -> parseCommit
               ;; -> getTree -> TreeWalk.forPath. JGit doesn't expose a
               ;; one-shot path-to-blob lookup, hence the four-step dance.
               ;; Wrapped in try/catch because any of resolve/parseCommit
               ;; can throw on detached / empty repos; we treat unknown
               ;; status as 'not binary' so blame falls through to its
               ;; normal codepath instead of refusing.
               (try
                 (with-open [reader (.newObjectReader repo)
                             walk   (RevWalk. repo)]
                   (when-let [head-id (.resolve repo "HEAD")]
                     (let [commit (.parseCommit walk head-id)
                           tree   (.getTree commit)]
                       (with-open [tw (org.eclipse.jgit.treewalk.TreeWalk/forPath
                                        repo ^String rel tree)]
                         (when tw (binary-blob? reader (.getObjectId tw 0)))))))
                 (catch Throwable _ false))]
           (if head-binary?
             {:path rel :binary? true :head (head-id repo) :total 0 :ignored-revs [] :lines []}
             (when-let [^BlameResult base (run-blame repo rel nil)]
               (let [contents (.getResultContents base)
                     total    (.size contents)
                     lo            (max 0 (dec (long (or from 1))))
                     hi-uncapped   (min (dec total) (dec (long (or to total))))
                     explicit?     (or (some? from) (some? to))
                     default-line-cap 1000
                     hi            (if explicit?
                                     hi-uncapped
                                     (min hi-uncapped (+ lo default-line-cap -1)))
                     truncated?    (and (not explicit?)
                                     (> total (+ lo default-line-cap)))
                     cache    (atom {})
                     lines    (when (<= lo hi)
                                (if ignored
                                  (mapv #(peel-blame-line repo rel base % ignored cache)
                                    (range lo (inc hi)))
                                  (mapv #(blame-line-record base %)
                                    (range lo (inc hi)))))]
                 {:path         rel
                  :head         (head-id repo)
                  :total        total
                  :truncated?   truncated?
                  :ignored-revs (if ignored (vec (sort ignored)) [])
                  :lines        (or lines [])})))))
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
