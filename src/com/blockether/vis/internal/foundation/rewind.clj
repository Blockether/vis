(ns com.blockether.vis.internal.foundation.rewind
  "DURABLE file-state rewind: put the working tree back the way it was before a
   turn, without owning a git/branch/commit lifecycle.

   Two independent coverage sources, combined:

     1. SNAPSHOT POOL — an `:around` op-hook on every mutating tool
        (`write`/`patch`/`struct_patch`/`fs`/`format_code`/`struct_rename`)
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
   `points` exposes the turn ids to truncate to."
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
  #{:write :patch :struct_patch :fs :format_code :struct_rename})

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

(defn- safe-seg
  "One filesystem-safe path segment for a session id."
  ^String [id]
  (let [t (str/replace (str id) #"[^A-Za-z0-9._-]" "_")]
    (if (str/blank? t) "default" (subs t 0 (min 128 (count t))))))

(defn store-dir
  "Per-session store directory: `<root>/<session>`."
  ^File [session-id]
  (io/file (if *store-root* (io/file *store-root*) (default-store-root)) (safe-seg session-id)))

(defn- journal-file ^File [^File dir] (io/file dir "journal.ndjson"))

(defn- blob-file ^File [^File dir ^String hex] (io/file dir "objects" (subs hex 0 2) hex))

;; =============================================================================
;; Content-addressed pool
;; =============================================================================

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

(defn journal
  "Every journal entry for `session-id`, in append order. A truncated or corrupt
   line (crash mid-append) is skipped — a damaged tail can never make the whole
   history unreadable."
  [session-id]
  (let [f (journal-file (store-dir session-id))]
    (if (.isFile f)
      (with-open [r (io/reader f)]
        (vec (keep (fn [line]
                     (when-not (str/blank? line)
                       (try (let [m (json/read-json line)]
                              (when (map? m) m))
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

(defn- file-entry
  "Journal entry(s) describing `f`'s CURRENT state, pooling content when needed."
  [^File dir ^File f base]
  (cond (not (exists? f)) [(assoc base
                             "kind" "pre"
                             "state" "absent")]
        (symlink? f) [(assoc base
                        "kind" "pre"
                        "state" "symlink"
                        "target" (str (Files/readSymbolicLink (.toPath f))))]
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
      (when (zero? exit) (not-empty (str/trim out))))))

(defn- git-head
  [repo]
  (let [{:keys [exit out]} (run-git repo ["rev-parse" "HEAD"])]
    (when (zero? exit) (not-empty (str/trim out)))))

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
    (when (zero? exit) (parse-porcelain-z repo out))))

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
  [{:keys [session turn turn-id op user-request] :as ctx} paths & [{:keys [recurse?]}]]
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
                         (long (or (get e "turn") 0))
                         (fn [m]
                           (-> (or m
                                   {"turn" (long (or (get e "turn") 0))
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
     (long (or turn 0))

     es
     (journal session-id)

     in-range
     (filter #(>= (long (or (get % "turn") 0)) t) es)

     restore
     (->> in-range
          (filter #(= "pre" (get % "kind")))
          (reduce (fn [acc e]
                    (if (contains? acc (get e "path")) acc (assoc acc (get e "path") e)))
                  {})
          vals
          (sort-by #(get % "path"))
          vec)

     unc
     (->> in-range
          (filter #(= "uncovered" (get % "kind")))
          (remove #(some (fn [r]
                           (= (get r "path") (get % "path")))
                         restore))
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
        (= "absent" state) (do (delete-tree! f) {"path" p "action" "deleted"})
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

(defn- render-points
  [ps]
  (if (empty? ps)
    "No file changes recorded yet in this session."
    (str/join "\n"
              (cons "turn  files  ops                        prompt"
                    (map (fn [p]
                           (format "%-5s %-6s %-26s %s"
                                   (str (get p "turn"))
                                   (str (get p "files"))
                                   (str/join "," (get p "ops"))
                                   (or (get p "user_request") "")))
                         ps)))))

(defn- handle-rewind
  "`/rewind` lists rewind points; `/rewind <turn>` puts the tree back to that
   turn's start; `/rewind <turn> --dry-run` shows the plan only."
  [ctx]
  (let
    [sid
     (or (:session/id ctx) (:session-id ctx))

     argv
     (vec (remove str/blank? (map str (:command/argv ctx))))

     dry?
     (boolean (some #{"--dry-run" "-n"} argv))

     target
     (first (remove #(str/starts-with? % "-") argv))]

    (cond (nil? sid) (err "Send a message first, then /rewind (session not ready yet)")
          (nil? target)
          {:slash/status :ok :slash/title "Rewind points" :slash/body (render-points (points sid))}
          (not (re-matches #"\d+" target)) (err "Usage: /rewind [<turn>] [--dry-run]")
          :else (let [r (restore! sid (parse-long target) {:is-dry-run dry?})]
                  {:slash/status (if (seq (get r "failed")) :error :ok)
                   :slash/title (str (if dry? "Rewind plan for turn " "Rewound to turn ")
                                     target
                                     " — " (count (get r "restore"))
                                     " file(s), coverage " (get r "coverage"))
                   :slash/body
                   (str/join "\n"
                             (concat
                               (map #(str "  " (get % "action") "  " (get % "path"))
                                    (if dry? (get r "restore") (get r "applied")))
                               (when (seq (get r "uncovered"))
                                 (cons "  -- uncovered (NOT restored):"
                                       (map #(str "     " (get % "path") "  (" (get % "reason") ")")
                                            (get r "uncovered"))))))}))))

(def slash-specs
  [{:slash/name "rewind"
    :slash/doc "List rewind points, or put the working tree back to a turn's start."
    :slash/usage "/rewind [<turn>] [--dry-run]"
    :slash/prompt-arg "Turn number (optional)"
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
