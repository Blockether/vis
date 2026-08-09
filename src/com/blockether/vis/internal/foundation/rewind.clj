(ns com.blockether.vis.internal.foundation.rewind
  "DURABLE file-state rewind: put the working tree back the way it was before a
   turn, without owning a git/branch/commit lifecycle.

   Two independent coverage sources, combined:

     1. SNAPSHOT POOL — an `:around` op-hook on every mutating tool
        (`write`/`patch`/`struct_patch`/`fs`/`format_code`)
        captures each touched path's PRE-mutation state before the op runs.
        Content lands in a content-addressed pool (`objects/aa/<sha256>`), so
        the same bytes are stored once no matter how many turns touch them.
        The first capture of a path in a turn WINS — later writes in the same
        turn never overwrite the turn-start pre-image.

     2. GIT BASELINE — at the FIRST hooked op of a turn we record `HEAD` plus
        the full dirty set (`git status --porcelain -z -uall`) and snapshot the
        pre-image of every DIRTY file. That closes the hole every other agent's
        rewind leaves open: a `sed -i`, a formatter, a build step, any `shell`
        write. A file that was CLEAN at turn start is recoverable from
        `git show <baseline-head>:<path>`; a file that was DIRTY at turn start
        already has its bytes in the pool. Coverage is therefore COMPLETE for a
        git workspace and honestly reported as PARTIAL otherwise.

   Everything is journalled as NDJSON (`journal.ndjson`) under
   `~/.vis/rewind/<session>/`, so rewind survives a restart — the history is
   NOT process-scoped. Entries are append-only and keyed by turn; a
   truncated/corrupt trailing line (crash mid-append) is skipped, never fatal.

   Restore semantics for `turn` T: every path touched in turns >= T is set back
   to the EARLIEST recorded pre-image at or after T. A file created inside the
   rewound region is deleted; a deleted file is recreated; a symlink is
   recreated as a symlink; a recursively deleted directory is rebuilt and any
   file created inside it since is pruned.

   This layer owns FILES ONLY. Conversation truncation is the channel's job —
   `points` exposes the turn ids to truncate to. Because that boundary is
   invisible to a user typing `/rewind`, the slash READS the session store for
   each turn's context size and says out loud, in every branch, that the
   conversation stays."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.workspace :as workspace]
            [taoensso.telemere :as tel])
  (:import [java.io File RandomAccessFile]
           [java.nio ByteBuffer]
           [java.nio.charset StandardCharsets]
           [java.nio.file CopyOption Files LinkOption StandardCopyOption]
           [java.security MessageDigest]))

;; =============================================================================
;; Tunables
;; =============================================================================

(def ^:dynamic *enabled?*
  "Master switch. Bound false in tests that must observe an unhooked tool."
  true)

(def ^:dynamic *store-root*
  "Override the `~/.vis/rewind` store root (tests point this at a temp dir)."
  nil)

(def ^:dynamic *max-blob-bytes*
  "Files larger than this are NOT pooled; they are journalled as `uncovered`
   with a reason so `plan` can report partial coverage instead of lying."
  (* 32 1024 1024))

(def ^:dynamic *max-dir-files*
  "Cap on descendants captured when a whole directory is about to be deleted or
   moved. Above it the directory is journalled `uncovered`."
  2000)

(def ^:dynamic *max-baseline-files*
  "Cap on turn-start DIRTY files pre-imaged from the git baseline."
  500)

(def ^:dynamic *git-baseline?*
  "Capture the per-turn git baseline (HEAD + dirty set + dirty pre-images).
   This is what makes `shell`/`sed -i`/formatter writes recoverable."
  true)

(def mutation-ops
  "Tool ops whose arguments name the files they are about to change."
  #{:write :patch :struct_patch :fs :format_code})

(def sweep-ops
  "Tool ops that can change arbitrary files WITHOUT naming them. These get a
   post-op git sweep so their damage is still rewindable."
  #{:shell})

(def ^:private path-keys
  "Argument keys whose values are filesystem paths. Applied to string AND
   keyword spellings so the walker is agnostic to the caller's convention."
  #{"path" "paths" "src" "dest" "file" "files" "target_path" "dest_path"})

;; =============================================================================
;; Store layout
;; =============================================================================

(defn- default-store-root ^File [] (io/file (System/getProperty "user.home") ".vis" "rewind"))

(defn- sha256-hex
  ^String [^bytes b]
  (let
    [d
     (.digest (MessageDigest/getInstance "SHA-256") b)

     sb
     (StringBuilder. 64)]

    (dotimes [i (alength d)]
      (let [v (bit-and (aget d i) 0xff)]
        (when (< v 16) (.append sb \0))
        (.append sb (Integer/toHexString v))))
    (.toString sb)))

(defn- safe-seg
  "One filesystem-safe path segment for a session id. Sanitizing is LOSSY, so an
   id that had to be rewritten or truncated also carries a digest of the RAW id:
   `proj/main` and `proj:main` (or two ids sharing a 128-char prefix) must never
   land in the same store and rewind each other's files."
  ^String [id]
  (let
    [raw
     (str id)

     t
     (str/replace raw #"[^A-Za-z0-9._-]" "_")]

    (cond (str/blank? raw) "default"
          (and (= t raw) (<= (count t) 128)) t
          :else (str ;; 119 + "-" + 8 hex = 128, the segment budget the test pins.
                  (subs t 0 (min 119 (count t)))
                  "-"
                  (subs (sha256-hex (.getBytes raw "UTF-8")) 0 8)))))

(defn store-dir
  "Per-session store directory: `<root>/<session>`."
  ^File [session-id]
  (io/file (if *store-root* (io/file *store-root*) (default-store-root)) (safe-seg session-id)))

(defn- journal-file ^File [^File dir] (io/file dir "journal.ndjson"))

(defn- blob-file ^File [^File dir ^String hex] (io/file dir "objects" (subs hex 0 2) hex))

;; =============================================================================
;; Content-addressed pool
;; =============================================================================

(defn- put-blob!
  "Store `b` in the pool; return its sha256. Idempotent — identical bytes from
   any turn/session-write collapse onto one object."
  ^String [^File dir ^bytes b]
  (let
    [hex
     (sha256-hex b)

     f
     (blob-file dir hex)]

    (when-not (.isFile f)
      (io/make-parents f)
      (let [tmp (File/createTempFile "blob" ".tmp" (.getParentFile f))]
        (io/copy b tmp)
        ;; ATOMIC publish: a concurrent reader never sees a half-written object.
        (Files/move (.toPath tmp)
                    (.toPath f)
                    (into-array CopyOption [StandardCopyOption/REPLACE_EXISTING]))))
    hex))

(defn- blob-bytes
  ^bytes [^File dir ^String hex]
  (let [f (blob-file dir hex)]
    (when (.isFile f) (Files/readAllBytes (.toPath f)))))

;; =============================================================================
;; Journal
;; =============================================================================

(defonce ^:private journal-lock (Object.))

(defn- append-entries!
  "Append `entries` as NDJSON. In-process `locking` plus an OS file lock, so two
   vis processes sharing a session cannot interleave a partial line."
  [^File dir entries]
  (when (seq entries)
    (let
      [f
       (journal-file dir)

       ^String payload
       (str/join (map #(str (json/write-json-str %) "\n") entries))

       bs
       (.getBytes payload StandardCharsets/UTF_8)]

      (io/make-parents f)
      (locking journal-lock
        (with-open [raf (RandomAccessFile. f "rw")]
          (let
            [ch (.getChannel raf)
             lk (.lock ch)]

            (try (.position ch (.size ch))
                 (let [bb (ByteBuffer/wrap bs)]
                   (while (.hasRemaining bb) (.write ch bb)))
                 (finally (.release lk))))))))
  entries)

(defn- as-long
  "Best-effort long from a journal value: numbers coerce, numeric strings parse,
   anything else is 0. The journal is DURABLE and outlives the code that wrote
   it, so one hand-edited or older-format entry must never make a session
   permanently un-rewindable."
  ^long [v]
  (cond (number? v) (long v)
        (string? v) (or (parse-long (str/trim ^String v)) 0)
        :else 0))

(defn journal
  "Every journal entry for `session-id`, in append order. A truncated or corrupt
   line (crash mid-append) is skipped — a damaged tail can never make the whole
   history unreadable. `turn` is normalized to a long on the way out, so a line
   that is valid JSON but carries the wrong TYPE cannot poison every reader."
  [session-id]
  (let [f (journal-file (store-dir session-id))]
    (if (.isFile f)
      (with-open [r (io/reader f)]
        (vec (keep (fn [line]
                     (when-not (str/blank? line)
                       (try (let [m (json/read-json line)]
                              (when (map? m) (assoc m "turn" (as-long (get m "turn")))))
                            (catch Throwable _ nil))))
                   (line-seq r))))
      [])))

;; =============================================================================
;; Path helpers
;; =============================================================================

(def ^:private nofollow (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))

(defn- abs-path
  "Absolute, normalized path string. Deliberately NOT canonical: canonicalizing
   resolves symlinks, and a symlink's own identity is exactly what a rewind of
   `fs move`/`delete` has to restore."
  ^String [p]
  (try (str (.normalize (.toAbsolutePath (.toPath (io/file (str p)))))) (catch Throwable _ nil)))

(defn- exists? [^File f] (Files/exists (.toPath f) nofollow))

(defn- symlink? [^File f] (Files/isSymbolicLink (.toPath f)))

(defn- real-dir?
  "A directory that is not itself a symlink (so tree walks cannot be lured out
   of the tree by a symlinked directory)."
  [^File f]
  (and (.isDirectory f) (not (symlink? f))))

(defn- leaf-strings
  "Strings reachable from a path-keyed value: a bare string, or a collection of
   strings (collections of MAPS are handled by the recursive walk instead)."
  [v]
  (cond (string? v) [v]
        (sequential? v) (filter string? v)
        :else []))

(defn- walk-paths
  "Every filesystem path mentioned anywhere in `x`, at any nesting depth. One
   generic walk instead of a per-tool argument schema, so a new tool or a new
   batch shape is covered the day it lands."
  [x]
  (let
    [acc
     (volatile! (transient []))

     step
     (fn step [v]
       (cond (map? v) (doseq [[k val] v]
                        (when (contains? path-keys (if (keyword? k) (name k) (str k)))
                          (doseq [s (leaf-strings val)]
                            (vswap! acc conj! s)))
                        (step val))
             (sequential? v) (doseq [i v]
                               (step i))
             :else nil))]

    (step x)
    (into [] (distinct) (persistent! @acc))))

;; =============================================================================
;; Pre-image capture
;; =============================================================================

(defn- missing-ancestors
  "Ancestor directories of `f` that do not exist YET, deepest first. Writing
   `a/b/c/new.txt` also creates `a`, `a/b` and `a/b/c`; restoring only the file
   would leave the directories the turn invented standing."
  [^File f]
  (loop
    [p
     (.getParentFile f)

     acc
     []]

    (if (and p (not (exists? p)) (< (count acc) 64))
      (recur (.getParentFile p) (conj acc (abs-path p)))
      (filterv some? acc))))

(defn- file-entry
  "Journal entry(s) describing `f`'s CURRENT state, pooling content when needed."
  [^File dir ^File f base]
  (cond (not (exists? f)) [(assoc base
                             "kind" "pre"
                             "state" "absent"
                             ;; Directories the op is about to invent on the way
                             ;; to this path, so a rewind can take them out too.
                             "missing_ancestors" (missing-ancestors f))]
        (symlink? f) (let
                       [link
                        (assoc base
                          "kind" "pre"
                          "state" "symlink"
                          "target" (str (Files/readSymbolicLink (.toPath f))))

                        ;; Every write tool writes THROUGH a link, so the bytes the op is about
                        ;; to destroy live in the RESOLVED file, not in the link. Pre-image it
                        ;; too, or a rewind restores the link and silently loses the content.
                        ^java.io.File real
                        (try (.toFile (.toRealPath (.toPath f) (make-array LinkOption 0)))
                             (catch Throwable _ nil))]

                       (if (and real (.isFile real) (not= (abs-path real) (get base "path")))
                         (into [link] (file-entry dir real (assoc base "path" (abs-path real))))
                         [link]))
        (.isDirectory f) [(assoc base
                            "kind" "pre"
                            "state" "dir")]
        :else (let [len (.length f)]
                (if (> len (long *max-blob-bytes*))
                  [(assoc base
                     "kind" "uncovered"
                     "reason" "too-large"
                     "size" len)]
                  (try (let [b (Files/readAllBytes (.toPath f))]
                         [(assoc base
                            "kind" "pre"
                            "state" "file"
                            "sha" (put-blob! dir b)
                            "size" (alength b)
                            "exec" (.canExecute f))])
                       (catch Throwable t
                         [(assoc base
                            "kind" "uncovered"
                            "reason" (str "unreadable: " (ex-message t)))]))))))

(defn- tree-entries
  "Recursive pre-image of a directory about to be deleted or moved: a `dir`
   entry carrying its children (so restore can PRUNE files created inside it
   afterwards) plus one entry per descendant."
  [^File dir ^File root base]
  (let [kids (vec (take (inc (long *max-dir-files*)) (rest (file-seq root))))]
    (if (> (count kids) (long *max-dir-files*))
      [(assoc base
         "kind" "uncovered"
         "reason" "dir-too-large")]
      (into [(assoc base
               "kind" "pre"
               "state" "dir"
               "children" (mapv abs-path kids))]
            (mapcat (fn [^File c]
                      (file-entry dir c (assoc base "path" (abs-path c)))))
            kids))))

;; Per-turn memo so N writes to the same file cost ONE pre-image. Not a
;; correctness dependency: restore always takes the EARLIEST journal entry, so a
;; cold process that re-snapshots mid-turn still rewinds to the right bytes.
(defonce ^:private covered (atom {}))

(defonce ^:private baselines (atom {}))

(defn- turn-key [session turn] [(str session) (long (or turn 0))])

(defn- forget-other-turns!
  "Keep only the live turn's memo per session — the map cannot grow without
   bound across a long-lived daemon."
  [a k]
  (swap! a (fn [m]
             (into {}
                   (remove (fn [[[s t] _]]
                             (and (= s (first k)) (not= t (second k)))))
                   m))))

(defn reset-memo!
  "Drop the in-memory per-turn memo (tests; also safe at runtime — the journal
   is the source of truth)."
  []
  (reset! covered {})
  (reset! baselines {})
  nil)

;; =============================================================================
;; git baseline
;; =============================================================================

(defn- run-git
  [root args]
  (try (let
         [pb
          (doto (ProcessBuilder. ^java.util.List (into ["git"] args))
            (.directory (io/file root))
            (.redirectError java.lang.ProcessBuilder$Redirect/DISCARD))

          pr
          (.start pb)

          out
          (with-open [is (.getInputStream pr)]
            (.readAllBytes is))]

         (.waitFor pr)
         {:exit (.exitValue pr) :bytes out :out (String. ^bytes out StandardCharsets/UTF_8)})
       (catch Throwable t {:exit -1 :out "" :bytes (byte-array 0) :error (ex-message t)})))

(defn- git-repo-root
  [root]
  (when root
    (let [{:keys [exit out]} (run-git root ["rev-parse" "--show-toplevel"])]
      (when (zero? (long exit)) (not-empty (str/trim out))))))

(defn- git-head
  [repo]
  (let [{:keys [exit out]} (run-git repo ["rev-parse" "HEAD"])]
    (when (zero? (long exit)) (not-empty (str/trim out)))))

(defn- parse-porcelain-z
  "`git status --porcelain=v1 -z -uall` → {abs-path status-code}. Rename/copy
   records carry a second NUL-separated ORIGIN path; both sides are recorded."
  [repo s]
  (loop
    [items
     (vec (remove str/blank? (str/split (str s) #"\u0000")))

     acc
     {}]

    (if-let [it (first items)]
      (let
        [code (subs it 0 (min 2 (count it)))
         p (str/triml (subs it (min 3 (count it))))
         path (abs-path (io/file repo p))]

        (if (or (str/starts-with? code "R") (str/starts-with? code "C"))
          (recur (vec (drop 2 items))
                 (cond-> (assoc acc path code)
                   (second items)
                   (assoc (abs-path (io/file repo (second items))) code)))
          (recur (vec (rest items)) (assoc acc path code))))
      acc)))

(defn- git-dirty
  [repo]
  (let [{:keys [exit out]} (run-git repo ["status" "--porcelain=v1" "-z" "--untracked-files=all"])]
    (when (zero? (long exit)) (parse-porcelain-z repo out))))

(defn- capture-baseline!
  "Once per turn: HEAD + dirty set, and a pre-image of every dirty file. Anything
   CLEAN at this instant is recoverable from HEAD; anything dirty is now in the
   pool. Together that covers writes made by tools we do not hook at all."
  [^File dir {:keys [session turn turn-id root op]}]
  (let [k (turn-key session turn)]
    (or
      (get @baselines k)
      (let
        [repo (when *git-baseline?* (git-repo-root root))
         head (when repo (git-head repo))
         dirty (when repo (git-dirty repo))
         base {:repo repo :head head :dirty (or dirty {}) :captured? true}
         dirty-paths (vec (take (long *max-baseline-files*) (sort (keys (or dirty {})))))
         entries (into [{"kind" "baseline"
                         "session" (str session)
                         "turn" (long (or turn 0))
                         "turn_id" turn-id
                         "ts" (System/currentTimeMillis)
                         "root" root
                         "repo" repo
                         "git_head" head
                         "dirty_count" (count (or dirty {}))
                         "dirty_truncated" (> (count (or dirty {})) (long *max-baseline-files*))}]
                       (mapcat (fn [p]
                                 (file-entry dir
                                             (io/file p)
                                             {"session" (str session)
                                              "turn" (long (or turn 0))
                                              "turn_id" turn-id
                                              "ts" (System/currentTimeMillis)
                                              "op" (some-> op
                                                           name)
                                              "origin" "baseline"
                                              "path" p})))
                       dirty-paths)]

        (append-entries! dir entries)
        (swap! covered update k (fnil into #{}) dirty-paths)
        (swap! baselines assoc k base)
        (forget-other-turns! baselines k)
        base))))

;; =============================================================================
;; Recording
;; =============================================================================

(defn record-pre!
  "Snapshot the pre-mutation state of `paths` for this session/turn. The FIRST
   capture of a path in a turn wins; repeat calls are cheap no-ops. Returns the
   entries actually appended."
  [{:keys [session turn turn-id op user-request]} paths & [{:keys [recurse?]}]]
  (let
    [dir
     (store-dir session)

     k
     (turn-key session turn)

     _
     (forget-other-turns! covered k)

     seen
     (get @covered k #{})

     todo
     (into [] (comp (keep abs-path) (distinct) (remove seen)) paths)

     base
     {"session" (str session)
      "turn" (long (or turn 0))
      "turn_id" turn-id
      "ts" (System/currentTimeMillis)
      "op" (some-> op
                   name)
      "user_request" user-request}

     entries
     (into []
           (mapcat (fn [p]
                     (let [f (io/file p)]
                       (if (and recurse? (real-dir? f))
                         (tree-entries dir f (assoc base "path" p))
                         (file-entry dir f (assoc base "path" p))))))
           todo)]

    (when (seq todo) (swap! covered update k (fnil into #{}) todo))
    (when (seq entries) (append-entries! dir entries))
    ;; A recursive capture also covers each descendant it just pooled.
    (when (seq entries) (swap! covered update k (fnil into #{}) (keep #(get % "path") entries)))
    entries))

(defn- sweep-git!
  "Post-op reconciliation for tools that change files WITHOUT naming them
   (`shell`). Anything newly dirty since the turn baseline gets an entry:
   untracked-at-HEAD → `absent` (delete on restore); tracked → `git` (restore
   from the baseline HEAD blob)."
  [^File dir {:keys [session turn turn-id op]} baseline]
  (when-let [repo (:repo baseline)]
    (let
      [before (:dirty baseline)
       after (or (git-dirty repo) {})
       k (turn-key session turn)
       seen (get @covered k #{})
       fresh (into [] (comp (remove #(contains? before %)) (remove seen)) (sort (keys after)))
       entries (mapv (fn [p]
                       (let [untracked? (str/starts-with? (str (get after p)) "?")]
                         (cond->
                           {"session" (str session)
                            "turn" (long (or turn 0))
                            "turn_id" turn-id
                            "ts" (System/currentTimeMillis)
                            "op" (some-> op
                                         name)
                            "origin" "git-sweep"
                            "kind" "pre"
                            "path" p}
                           untracked?
                           (assoc "state" "absent")

                           (not untracked?)
                           (assoc "state"
                             "git" "git_head"
                             (:head baseline) "repo"
                             repo))))
                     fresh)]

      (when (seq entries)
        (swap! covered update k (fnil into #{}) fresh)
        (append-entries! dir entries))
      entries)))

;; =============================================================================
;; Rewind points / plan / restore
;; =============================================================================

(defn points
  "Rewind targets for a session, newest last: one entry per turn that changed
   files, with the prompt that triggered it."
  [session-id]
  (let
    [es
     (journal session-id)

     by-turn
     (reduce (fn [acc e]
               (if (#{"pre" "uncovered"} (get e "kind"))
                 (update acc
                         (as-long (get e "turn"))
                         (fn [m]
                           (-> (or m
                                   {"turn" (as-long (get e "turn"))
                                    "turn_id" (get e "turn_id")
                                    "ts" (get e "ts")
                                    "user_request" (get e "user_request")
                                    "files" #{}
                                    "ops" #{}
                                    "uncovered" 0})
                               (update "files" conj (get e "path"))
                               (update "ops"
                                       (fn [s]
                                         (if (get e "op") (conj s (get e "op")) s)))
                               (update "uncovered" + (if (= "uncovered" (get e "kind")) 1 0))
                               (update "user_request" #(or % (get e "user_request"))))))
                 acc))
             {}
             es)]

    (->> (vals by-turn)
         (map (fn [m]
                (assoc m
                  "files" (count (get m "files"))
                  "ops" (vec (sort (get m "ops"))))))
         (sort-by #(get % "turn"))
         vec)))

(defn plan
  "What `restore!` would do to put the tree back to its state BEFORE `turn`.
   `restore` holds ONE entry per path — the EARLIEST pre-image recorded at or
   after `turn`, which is exactly that path's turn-start state."
  [session-id turn]
  (let
    [t
     (as-long turn)

     es
     (journal session-id)

     in-range
     (filter #(>= (as-long (get % "turn")) t) es)

     restore
     (->> in-range
          (filter #(= "pre" (get % "kind")))
          (reduce (fn [acc e]
                    (if (contains? acc (get e "path")) acc (assoc acc (get e "path") e)))
                  {})
          vals
          (sort-by #(get % "path"))
          vec)

     restored-paths
     ;; A SET, not a linear `some` over `restore` for every uncovered entry:
     ;; both lists hold one entry per FILE, so the old scan was
     ;; O(uncovered x restore) and a `shell` turn that sweeps a big tree fills
     ;; both at once. Measured on a 1600+1600 journal: 124 ms, quadrupling
     ;; every time the turn's file count doubled. `map` (not `keep`) so a nil
     ;; path still matches a nil path exactly as `=` did.
     (into #{} (map #(get % "path")) restore)

     unc
     (->> in-range
          (filter #(= "uncovered" (get % "kind")))
          (remove #(contains? restored-paths (get % "path")))
          (sort-by #(get % "path"))
          vec)]

    {"session" (str session-id)
     "turn" t
     "restore" restore
     "uncovered" unc
     "coverage" (if (seq unc) "partial" "complete")}))

(defn- delete-tree!
  [^File f]
  (when (exists? f)
    (if (real-dir? f)
      (do (doseq [c (reverse (rest (file-seq f)))]
            (io/delete-file c true))
          (io/delete-file f true))
      (Files/deleteIfExists (.toPath f)))))

(defn- write-bytes!
  [^File f ^bytes b exec]
  (when (real-dir? f) (delete-tree! f))
  (io/make-parents f)
  (let [tmp (File/createTempFile ".rewind" ".tmp" (.getParentFile f))]
    (io/copy b tmp)
    (Files/move (.toPath tmp)
                (.toPath f)
                (into-array CopyOption [StandardCopyOption/REPLACE_EXISTING]))
    (when (some? exec) (.setExecutable f (boolean exec)))))

(defn- prune-invented-dirs!
  "Delete the directories an op invented under a path that did not exist before
   the turn, deepest first, stopping at the first one that is not an EMPTY real
   directory. Never touches a directory that already held something."
  [entry]
  (loop
    [ds
     (filter string? (get entry "missing_ancestors"))

     acc
     []]

    (if-let [d (first ds)]
      (let
        [df (io/file d)
         kids (when (real-dir? df) (.list df))]

        (if (and (= (abs-path d) d) kids (zero? (alength ^objects kids)) (.delete df))
          (recur (rest ds) (conj acc d))
          acc))
      acc)))

(defn- apply-entry!
  [^File dir entry]
  (let
    [p
     (get entry "path")

     f
     (io/file p)

     state
     (get entry "state")]

    (try
      (cond
        ;; Refuse anything that is not already an absolute, normalized path —
        ;; a hand-edited or corrupted journal cannot steer a write elsewhere.
        (or (str/blank? (str p)) (not= (abs-path p) p))
        {"path" p "action" "skipped" "error" "non-canonical path"}
        (= "absent" state) ;; Delete FIRST: the invented directories only become empty
        ;; once the file the turn created is gone.
        (do (delete-tree! f)
            (let [pruned (prune-invented-dirs! entry)]
              (cond-> {"path" p "action" "deleted"}
                (seq pruned)
                (assoc "pruned_dirs" pruned))))
        (= "dir" state) (do (when-not (real-dir? f) (delete-tree! f) (.mkdirs f))
                            (let
                              [keep-set
                               (set (get entry "children"))

                               pruned
                               (when (seq keep-set)
                                 (->> (rest (file-seq f))
                                      (map abs-path)
                                      (remove keep-set)
                                      sort
                                      reverse
                                      (mapv (fn [c]
                                              (io/delete-file (io/file c) true)
                                              c))))]

                              {"path" p "action" "dir" "pruned" (count (or pruned []))}))
        (= "symlink" state) (do (delete-tree! f)
                                (io/make-parents f)
                                (Files/createSymbolicLink
                                  (.toPath f)
                                  (.toPath (io/file (get entry "target")))
                                  (make-array java.nio.file.attribute.FileAttribute 0))
                                {"path" p "action" "symlink"})
        (= "file" state) (if-let [b (blob-bytes dir (get entry "sha"))]
                           (do (write-bytes! f b (get entry "exec"))
                               {"path" p "action" "restored" "bytes" (alength b)})
                           {"path" p "action" "skipped" "error" "blob missing from pool"})
        (= "git" state)
        (let
          [repo
           (get entry "repo")

           head
           (get entry "git_head")

           rel
           (when repo (str (.relativize (.toPath (io/file repo)) (.toPath f))))

           {:keys [exit bytes]}
           (if (and repo head rel)
             (run-git repo ["show" (str head ":" rel)])
             {:exit -1 :bytes (byte-array 0)})]

          (if (zero? (long exit))
            (do (write-bytes! f bytes nil)
                {"path" p "action" "restored-from-git" "bytes" (alength ^bytes bytes)})
            {"path" p "action" "skipped" "error" "git object unavailable"}))
        :else {"path" p "action" "skipped" "error" (str "unknown state " state)})
      (catch Throwable t
        {"path" p "action" "failed" "error" (or (ex-message t) (str (class t)))}))))

(defn restore!
  "Put every path touched at or after `turn` back to its turn-start state.
   `:is-dry-run` reports the plan without touching the tree. Returns the plan
   plus a per-path `applied` list; a per-path failure is reported, never
   aborts the rest (a half-applied restore is still strictly closer to the
   target than not trying)."
  [session-id turn & [{:keys [is-dry-run]}]]
  (let
    [pl
     (plan session-id turn)

     dir
     (store-dir session-id)

     applied
     (if is-dry-run [] (mapv #(apply-entry! dir %) (get pl "restore")))]

    (assoc pl
      "is_dry_run" (boolean is-dry-run)
      "applied" applied
      "restored" (count (filter #(str/starts-with? (str (get % "action")) "restored") applied))
      "deleted" (count (filter #(= "deleted" (get % "action")) applied))
      "failed" (filterv #(#{"failed" "skipped"} (get % "action")) applied))))

;; =============================================================================
;; The op hook
;; =============================================================================

(defn- env->ctx
  [env op]
  (let
    [ts (some-> (:turn-state-atom env)
                deref)]
    {:session (or (:session-id env) "default")
     :turn (or (:turn-position ts) 0)
     :turn-id (:session-turn-id ts)
     :user-request (some-> (:user-request ts)
                           str
                           (subs 0 (min 200 (count (str (:user-request ts))))))
     :op op
     :root (or (workspace/workspace-root env)
               (try (.getPath (workspace/cwd)) (catch Throwable _ nil)))}))

(defn- capture!
  "Pre-image everything this op names, after making sure the turn baseline
   exists. Best-effort by contract: rewind bookkeeping must NEVER fail the tool
   it wraps."
  [ctx args]
  (let
    [dir
     (store-dir (:session ctx))

     baseline
     (capture-baseline! dir ctx)

     ;; Only `fs` can destroy a whole subtree (delete/move), so only `fs` pays
     ;; for a recursive capture. Everything else treats a directory argument as
     ;; a container — the files it actually edits are named individually, and
     ;; the git baseline covers the rest.
     recurse?
     (= :fs (:op ctx))]

    (record-pre! ctx (walk-paths args) {:recurse? recurse?})
    baseline))

(defn around-hook
  "`:around` op-hook. Snapshots BEFORE the op, sweeps AFTER it, and is
   transparent to the operation in every path — including when the op throws,
   because a failed write can still have partially mutated the file."
  [env op-kw args next-fn]
  (if-not *enabled?*
    (next-fn args)
    (let
      [ctx
       (try (env->ctx env op-kw) (catch Throwable _ nil))

       baseline
       (when ctx
         (try (capture! ctx args)
              (catch Throwable t
                (tel/log!
                  {:level :warn :id ::capture-failed :data {:op op-kw :error (ex-message t)}})
                nil)))

       sweep!
       (fn []
         (when (and ctx baseline (contains? sweep-ops op-kw))
           (try (sweep-git! (store-dir (:session ctx)) ctx baseline)
                (catch Throwable t
                  (tel/log!
                    {:level :warn :id ::sweep-failed :data {:op op-kw :error (ex-message t)}})
                  nil))))]

      (try (let [r (next-fn args)]
             (sweep!)
             r)
           (catch Throwable t (sweep!) (throw t))))))

(def op-hooks
  (mapv (fn [op]
          {:op op :phase :around :fn #'around-hook})
        (concat mutation-ops sweep-ops)))

;; =============================================================================
;; Slash command
;; =============================================================================

(defn- err [msg & {:as extras}] (merge {:slash/status :error :slash/title msg} extras))

(defn- display-path
  "Shortest honest label for `p`: relative to a session root when it lives
   under one, `~/…` under the home directory, else the absolute path. Slash
   output is read by a human in a narrow TUI bubble and on a phone, so a
   40-character temp/absolute prefix repeated on every row is noise."
  [roots p]
  (let
    [s
     (str p)

     under
     (some (fn [root]
             (let
               [r
                (str root)

                r
                (if (str/ends-with? r "/") r (str r "/"))]

               (when (and (> (count r) 1) (str/starts-with? s r) (> (count s) (count r)))
                 (subs s (count r)))))
           roots)

     home
     (str (System/getProperty "user.home"))]

    (cond under under
          (and (seq home) (str/starts-with? s (str home "/"))) (str "~" (subs s (count home)))
          :else s)))

(defn- cell
  "One-line, pipe-safe Markdown table cell elided to `n` characters. A raw `|`
   or newline from a user prompt would otherwise tear the table apart."
  [v ^long n]
  (let
    [s (-> (str v)
           (str/replace #"\s+" " ")
           str/trim
           (str/replace "|" "\\|"))]
    (if (> (count s) n) (str (str/trimr (subs s 0 (dec n))) "…") s)))

(defn- fmt-tokens
  "Token counts sized for a narrow table cell: `840` stays `840`, `12480`
   becomes `12.5k`, `1205000` becomes `1.2M`. A phone-width bubble cannot
   afford seven raw digits in a column.

   Rounded by hand rather than with `format`: `%.1f` is LOCALE-dependent and
   rendered `12,5k` on a Polish JVM, while `Double/toString` is not."
  [n]
  (let
    [v
     (long (or n 0))

     tenth
     (fn [^double d]
       (str (/ (double (Math/round (* d 10.0))) 10.0)))]

    (cond (>= v 999950) (str (tenth (/ (double v) 1000000.0)) "M")
          (>= v 1000) (str (tenth (/ (double v) 1000.0)) "k")
          :else (str v))))

(defn- turn-context
  "Per-turn CONTEXT reading, keyed by turn position:
   `{pos {:tokens n :output n :iterations n}}`.

   `:tokens` is the turn's INPUT size — how heavy the conversation was when
   that turn ran. Input already carries the whole prior conversation, so these
   readings must never be summed across turns; the series is a growth curve,
   and the newest value is the live context size.

   The rewind journal knows FILES only; this lives in the session store, so the
   two are joined on `position` — the same number `points` reports as `turn`.
   Best-effort by design: no `:db-info`, no session, or an unreadable store
   yields `{}` and the context column simply disappears instead of failing the
   slash."
  [ctx session-id]
  (let [db (:db-info ctx)]
    (if (and db session-id)
      (try (into {}
                 (keep (fn [t]
                         ;; `as-long` never returns nil (0 on junk), so the
                         ;; presence check has to be on the raw value.
                         (when (some? (:position t))
                           [(as-long (:position t))
                            {:tokens (long (or (:input-tokens t) 0))
                             :output (long (or (:output-tokens t) 0))
                             :iterations (long (or (:iteration-count t) 0))}])))
                 (vis/db-list-session-turns db session-id))
           (catch Throwable t
             (tel/log! {:level :debug
                        :id ::turn-context-failed
                        :data {:session-id session-id :error (ex-message t)}})
             {}))
      {})))

(defn- with-context
  "Attach each point's conversation cost as `ctx_tokens` / `ctx_iterations` so
   the rendered table and `:slash/data` carry the SAME numbers. A turn the
   store does not know keeps its file data and simply has no context figures."
  [ps by-turn]
  (mapv (fn [p]
          (if-let [c (get by-turn (as-long (get p "turn")))]
            (assoc p
              "ctx_tokens" (:tokens c)
              "ctx_iterations" (:iterations c))
            p))
        ps))

(defn- context-summary
  "The conversation from `turn` onward: `{:turns n :tokens n}`, where `:tokens`
   is the LARGEST (newest) context reading in that span — the live size of the
   conversation, never a sum, because each turn's input already contains the
   previous ones. These turns are exactly what rewind does NOT remove, which is
   the part users assume is undone along with the files."
  [by-turn turn]
  (let
    [rows (keep (fn [[pos c]]
                  (when (>= (long pos) (long turn)) c))
                by-turn)]
    {:turns (count rows)
     :tokens (long (reduce (fn [acc c]
                             (max (long acc) (long (:tokens c))))
                           0
                           rows))}))

(defn- ctx-legend
  "One sentence under the table. A rewind list reads as “undo everything”, so
   the scope must be explicit: the numbers are how big the conversation was on
   each turn, and rewinding moves FILES only — nothing leaves the context."
  [ps]
  (if (some #(get % "ctx_tokens") ps)
    (str "**Ctx** is how heavy the conversation was on that turn. "
         "Rewinding restores files only — the conversation itself stays.")
    "Rewinding restores files only — the conversation itself stays."))

(defn- render-points
  "Rewind points as a GFM table. Both channels render this: the TUI draws a
   boxed table and the companion a real `<table>`. The previous fixed-width
   columns were collapsed into one paragraph by every Markdown renderer.

   The `Ctx` column appears only when the session store answered, so a rewind
   list is never a table of empty cells."
  [ps]
  (let
    [ctx?
     (boolean (some #(get % "ctx_tokens") ps))

     row
     (fn [cells]
       (str "| " (str/join " | " cells) " |"))]

    (str/join "\n"
              (concat [(row (conj (cond-> ["Turn" "Files" "Ops"]
                                    ctx?
                                    (conj "Ctx"))
                                  "What you asked"))
                       (row (conj (cond-> ["---:" "---:" "---"]
                                    ctx?
                                    (conj "---:"))
                                  "---"))]
                      (map (fn [p]
                             (row (conj (cond->
                                          [(cell (get p "turn") 8) (cell (get p "files") 8)
                                           (cell (str/join ", " (get p "ops")) 28)]
                                          ctx?
                                          (conj (if-let [tks (get p "ctx_tokens")]
                                                  (fmt-tokens tks)
                                                  "—")))
                                        (cell (get p "user_request") 80))))
                           ps)))))

(defn- plan-verb
  "What restoring this journal entry will DO to the path. `absent` means the
   path did not exist before the turn, so putting the tree back deletes it."
  [entry]
  (case (str (get entry "state"))
    "absent"
    "delete"

    "dir"
    "restore dir"

    "symlink"
    "restore link"

    "restore"))

(defn- applied-verb
  "Human phrasing for what `apply-entry!` actually DID. The raw action words
   are wire tokens (`dir`, `restored-from-git`); a bubble should read as prose."
  [entry]
  (case (str (get entry "action"))
    "restored"
    "restored"

    "restored-from-git"
    "restored from git"

    "dir"
    "restored dir"

    "symlink"
    "restored link"

    "deleted"
    "deleted"

    "skipped"
    "skipped"

    "failed"
    "failed"

    (str (get entry "action"))))

(defn- render-file-lines
  "One Markdown bullet per file: verb, code-spanned display path, and the
   reason when an entry was skipped or failed — a silent skip is a lie."
  [roots entries verb-fn]
  (map (fn [e]
         (str "- " (verb-fn e)
              " `" (display-path roots (get e "path"))
              "`" (when-let [why (get e "error")]
                    (str " — " why))))
       entries))

(defn- handle-rewind
  "`/rewind` lists rewind points; `/rewind <turn>` puts the tree back to that
   turn's start; `/rewind <turn> --dry-run` shows the plan only.

   Every channel renders the SAME Markdown, so the body is written for the
   narrowest one: a table for the list, bullets for a plan, and a closing line
   that names the exact next command.

   Files are only half of what a turn changed, so every branch also reports
   CONTEXT — per-turn tokens in the list, and the turns that stay in the
   conversation on a restore — and says plainly that rewind moves files only
   and never truncates the conversation."
  [ctx]
  (let
    [sid
     (or (:session/id ctx) (:session-id ctx))

     roots
     (keep identity [(:workspace/root ctx) (:root ctx) (System/getProperty "user.dir")])

     argv
     (vec (remove str/blank? (map str (:command/argv ctx))))

     dry?
     (boolean (some #{"--dry-run" "-n"} argv))

     target
     (first (remove #(str/starts-with? % "-") argv))

     by-turn
     (turn-context ctx sid)]

    (cond (nil? sid) (err "Send a message first, then /rewind (session not ready yet)")
          (nil? target)
          (let [ps (with-context (points sid) by-turn)]
            (if (empty? ps)
              {:slash/status :ok
               :slash/title "Nothing to rewind yet"
               :slash/body (str "Vis snapshots every file a tool is about to change. "
                                "As soon as a turn edits something, that turn shows up here.")
               :slash/data {:points [] :context {}}}
              {:slash/status :ok
               :slash/title (str (count ps) " rewind point" (when (not= 1 (count ps)) "s"))
               :slash/body (str (render-points ps)
                                "\n\n`/rewind " (get (last ps) "turn")
                                "` puts every file back to how it was before that turn · "
                                "add `--dry-run` to see the plan first."
                                "\n\n" (ctx-legend ps))
               :slash/data {:points ps :context by-turn}}))
          (not (re-matches #"\d+" target)) (err
                                             "Usage: /rewind [<turn>] [--dry-run]"
                                             :slash/body
                                             "`/rewind` alone lists every turn you can go back to.")
          :else
          (let
            [r
             (restore! sid (parse-long target) {:is-dry-run dry?})

             entries
             (if dry? (get r "restore") (get r "applied"))

             failed
             (get r "failed")

             uncovered
             (get r "uncovered")

             n
             (count entries)

             kept
             (context-summary by-turn (parse-long target))

             lines
             (if dry?
               (render-file-lines roots entries plan-verb)
               (render-file-lines roots entries applied-verb))]

            {:slash/status (if (seq failed) :error :ok)
             :slash/title (str (if dry? "Rewind plan for turn " "Rewound to turn ")
                               target
                               " — " n
                               " file" (when (not= 1 n) "s")
                               ", coverage " (get r "coverage"))
             :slash/body
             (str/join
               "\n"
               (concat (if (seq lines) lines ["Nothing to put back for that turn."])
                       (when (seq uncovered)
                         (cons "\n**Not covered — left exactly as they are:**"
                               (map #(str "- `" (display-path roots (get % "path"))
                                          "` — " (or (get % "reason") "no pre-image"))
                                    uncovered)))
                       (when (pos? (long (:turns kept)))
                         [(str "\n**Context is untouched** — "
                               (:turns kept)
                               " turn"
                               (when (not= 1 (:turns kept)) "s")
                               " from turn "
                               target
                               (if (= 1 (:turns kept))
                                 " on is still in the conversation ("
                                 " on are still in the conversation (")
                               (fmt-tokens (:tokens kept))
                               " tokens at the last one); "
                               (if dry? "rewinding moves" "rewinding moved")
                               " files only.")])
                       [(str "\n"
                             (if dry?
                               (str "Nothing has changed yet — `/rewind " target "` applies this.")
                               (str "Your files are back to their state before turn "
                                    target
                                    " · `/rewind` lists the other points.")))]))
             :slash/data {:turn (parse-long target)
                          :is-dry-run dry?
                          :files n
                          :coverage (get r "coverage")
                          :context kept}}))))

(def slash-specs
  ;; NO `:slash/prompt-arg`: the turn is OPTIONAL, and a prompt-arg makes the
  ;; TUI pop a text-input dialog for bare `/rewind`, which made the list — the
  ;; command's main entry point — unreachable (Esc/empty simply cancelled).
  [{:slash/name "rewind"
    :slash/doc "List rewind points, or put the working tree back to a turn's start."
    :slash/usage "/rewind [<turn>] [--dry-run]"
    :slash/requires #{:session}
    :slash/run-fn handle-rewind}])

;; =============================================================================
;; Gateway routes — rewind is available from every channel, not one keybinding
;; =============================================================================

(defn- json-response
  [status body]
  {:status status
   :headers {"Content-Type" "application/json; charset=utf-8"}
   :body (json/write-json-str body)})

(defn- points-handler
  [request]
  (let [sid (get-in request [:path-params :sid])]
    (json-response 200 {"session" sid "points" (points sid)})))

(defn- read-json-body
  [request]
  (try (let [b (:body request)]
         (when b (json/read-json (slurp b))))
       (catch Throwable _ nil)))

(defn- restore-handler
  [request]
  (let
    [sid
     (get-in request [:path-params :sid])

     body
     (or (read-json-body request) {})

     turn
     (get body "turn")]

    (if-not (number? turn)
      (json-response 400 {"error" "turn (number) required"})
      (json-response 200
                     (restore! sid (long turn) {:is-dry-run (boolean (get body "is_dry_run"))})))))

(defn routes-contribution
  []
  {:prefix "/v1/rewind"
   :rev (str (System/identityHashCode #'routes-contribution))
   :routes (fn [_token]
             ["/v1/rewind/:sid" {:get points-handler :post restore-handler}])})

;; =============================================================================
;; Registration
;; =============================================================================

(def vis-extension
  (vis/extension {:ext/name "rewind"
                  :ext/description
                  (str
                    "Durable file-state rewind: snapshots every tool mutation's pre-image into a "
                    "content-addressed pool and pairs it with a per-turn git baseline, so a turn's "
                    "file changes — including ones made by `shell` — can be put back.")
                  :ext/kind "foundation"
                  :ext/op-hooks op-hooks
                  :ext/slash-commands slash-specs
                  :ext/channel-contributions {:gateway.slot/http-routes
                                              [{:id :rewind/http :fn routes-contribution}]}}))

(vis/register-extension! vis-extension)
