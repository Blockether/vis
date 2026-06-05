(ns com.blockether.vis.internal.workspace
  "Rift copy-on-write workspaces, DB-pinned to session_state 1:1.

   The user's real cwd is *trunk* — Vis never mutates it, and Vis no
   longer requires it to be a git repo. A session works in trunk by
   default; `/draft new` opts into an isolated `rift` CoW clone of cwd
   (APFS `clonefile` / btrfs snapshot) stored under ~/.vis/drafts. RIFT
   ONLY — there is no plain-copy fallback; on an unsupported filesystem
   the clone fails loudly rather than silently degrading to a slow copy.

   'What changed since the fork' is computed git-free: `clonefile`
   preserves source mtimes, so files the agent touches in the clone get
   a fresh mtime greater than the fork timestamp we capture at clone
   time. `apply!` lands exactly those files back into cwd, uncommitted,
   and leaves the user to commit with their own tools — Vis owns no
   git/branch/commit/merge lifecycle whatsoever.

   Vis never mutates JVM user.dir. Channels rebind *workspace-root* per
   turn from the active workspace; tools resolve paths via
   (workspace/cwd). There is NO process-cwd fallback in production -
   the env carries `:workspace/root` from `create-environment` onward."
  (:refer-clojure :exclude [get])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.rift :as rift]
            [com.blockether.vis.internal.persistance :as p])
  (:import [java.io File]
           [java.nio.file CopyOption FileVisitResult Files LinkOption Path
            SimpleFileVisitor StandardCopyOption]
           [java.nio.file.attribute BasicFileAttributes FileAttribute]
           [java.util UUID]))

;; =============================================================================
;; Dynamic cwd binding
;; =============================================================================

(def ^:dynamic *workspace-root*
  "Canonical workspace root for the current tool call. Bound per-turn
   by the channel layer; never `nil` in normal operation."
  nil)

(defn normalize-root
  "Canonicalize a workspace root string/File. Blank/nil → nil."
  [root]
  (let [s (some-> root str str/trim)]
    (when (seq s)
      (.getCanonicalPath (io/file s)))))

(defn workspace-root
  "Extract a canonical :workspace/root from an env map or raw root value."
  [env-or-root]
  (normalize-root (if (map? env-or-root)
                    (:workspace/root env-or-root)
                    env-or-root)))

(defn cwd
  "Resolve the current workspace cwd. In production the channel
   wrapper binds `*workspace-root*` per turn, so the process-cwd
   fallback only fires from REPL / test / one-off CLI paths that have
   no session context."
  ^File []
  (io/file (or *workspace-root* (System/getProperty "user.dir"))))

(defn- file-path ^String [f]
  (.getCanonicalPath (io/file f)))

(defn trunk-root
  "The user's real working directory — where they launched `vis`.
   Canonical absolute path. This is *trunk*: never mutated, never
   required to be a git repo. (`bin/vis` preserves the invocation cwd
   as JVM user.dir even though it cd's to the repo for deps.)"
  ^String []
  (file-path (System/getProperty "user.dir")))

(defn- sanitize-id
  [s]
  (let [s (-> (str (or s "ws"))
            str/lower-case
            (str/replace #"[^a-z0-9._-]+" "-")
            (str/replace #"(^-+|-+$)" ""))]
    (if (str/blank? s) "ws" s)))

(defn- repo-id-for
  "Stable per-root grouping id (sanitized basename + path hash).
   Groups a repo's clones together in listings; no git involved."
  [root]
  (let [root (file-path root)
        name (sanitize-id (.getName (io/file root)))
        hash (Long/toUnsignedString (Integer/toUnsignedLong (hash root)) 36)]
    (str name "-" hash)))

;; =============================================================================
;; Clone mechanism — rift CoW only (no fallback)
;; =============================================================================

;; Keep vis's draft trees under ~/.vis instead of rift's default ~/.rifts,
;; so all of a session's visible state lives in one home dir. Only the
;; CLONE location is relocated (via `:into`); rift's internal registry
;; stays at its platform default — re-homing it breaks the per-repo rift
;; marker ("marker does not match the registry"), which silently demotes
;; every clone to a slow plain copy.
(defn- draft-store-root
  "Parent storage dir for `trunk`'s drafts: ~/.vis/drafts/<repo-basename>.
   Passed to rift `create` as `:into` so clones live here, not in rift's
   default ~/.rifts. Grouped per-repo so a draft labelled `feature` in one
   repo never collides with the same label in another."
  ^File [trunk]
  (io/file (System/getProperty "user.home") ".vis" "drafts"
    (.getName (io/file trunk))))

(def ^:private copy-opts
  ^"[Ljava.nio.file.CopyOption;"
  (into-array CopyOption
    [StandardCopyOption/REPLACE_EXISTING
     StandardCopyOption/COPY_ATTRIBUTES
     LinkOption/NOFOLLOW_LINKS]))

(defn- cow-clone!
  "Clone `src` tree under `name` via rift's CoW `create`, returning the
   clone's absolute path. RIFT ONLY — there is no plain-copy fallback: a
   draft must be a real copy-on-write clone (instant, near-zero disk) or
   nothing. On an unsupported FS/platform rift throws and the error
   propagates so the failure is loud, never a silent slow copy."
  [src name]
  (rift/init {:at src})
  (let [into (draft-store-root src)]
    ;; rift won't materialise into a non-existent parent — ensure the
    ;; ~/.vis/drafts/<repo> store dir exists first, else `create` errors.
    ;; `createDirectories` is idempotent: a no-op when the dir already
    ;; exists, and throws only on a genuine failure (e.g. permissions).
    (Files/createDirectories (.toPath into) (make-array FileAttribute 0))
    (rift/create {:from src :name name :into (str into)})))

(defn- rift-trash!
  "Trash a clone via rift `remove!` + `gc`. RIFT ONLY — a failure
   propagates rather than falling back to a manual recursive delete."
  [clone]
  (rift/remove! {:at clone})
  (rift/gc))

(defn- delete-tree!
  "Best-effort recursive delete of `dir` (a path string)."
  [dir]
  (let [f (io/file dir)]
    (when (.exists f)
      (run! #(.delete ^File %) (reverse (file-seq f))))))

(def ^:private rift-supported*
  ;; One real CoW probe per process: temp src -> rift/init -> rift/create ->
  ;; cleanup. Memoized via delay so the FS work runs at most once. On an
  ;; unsupported FS/platform rift throws and we record false.
  (delay
    (let [no-attrs (make-array FileAttribute 0)
          src (str (Files/createTempDirectory "vis-rift-probe-src" no-attrs))
          dst (str (Files/createTempDirectory "vis-rift-probe-dst" no-attrs))]
      (try
        (rift/init {:at src})
        (boolean (rift/create {:from src :name "probe" :into dst}))
        (catch Throwable _ false)
        (finally (run! delete-tree! [src dst]))))))

(defn rift-supported?
  "True when this filesystem/platform supports a real rift CoW clone.
   Memoized — the FS probe runs at most once per process. Drafts are only
   offered when this is true (see `workspace-slashes/specs`)."
  []
  @rift-supported*)

;; =============================================================================
;; Since-fork diff — pure mtime, git-free
;; =============================================================================

(defn changed-paths
  "Repo-relative paths of files under `clone` whose mtime is newer than
   `fork-ms` — i.e. exactly what the agent touched since the fork
   (`clonefile` preserves source mtimes, so untouched files stay older).
   Skips `.git/` (landing it would corrupt trunk's repo). Returns a vec
   of strings."
  [clone fork-ms]
  (let [root (.toPath (io/file clone))
        acc  (java.util.ArrayList.)]
    (Files/walkFileTree
      root
      (proxy [SimpleFileVisitor] []
        (preVisitDirectory [dir ^BasicFileAttributes _a]
          (let [rel (.relativize root ^Path dir)]
            (if (and (pos? (.getNameCount rel))
                  (= ".git" (str (.getName rel 0))))
              FileVisitResult/SKIP_SUBTREE
              FileVisitResult/CONTINUE)))
        (visitFile [file ^BasicFileAttributes attrs]
          (when (> (.toMillis (.lastModifiedTime attrs)) (long fork-ms))
            (.add acc (str (.relativize root ^Path file))))
          FileVisitResult/CONTINUE)
        (visitFileFailed [_file _exc]
          FileVisitResult/CONTINUE)))
    (vec acc)))

(defn- fork-ms-of [ws]
  (:fork-ms ws))

(defn deleted-paths
  "Repo-relative paths the agent DELETED in the draft: present under
   `trunk` (skipping `.git`) with an mtime older than `fork-ms` — so they
   existed at the fork and are not user post-fork additions — yet absent
   from `clone`. The mtime guard means `apply!` never reverts a file the
   user added to cwd after forking."
  [clone trunk fork-ms]
  (let [troot    (.toPath (io/file trunk))
        croot    (.toPath (io/file clone))
        nofollow (into-array LinkOption [LinkOption/NOFOLLOW_LINKS])
        acc      (java.util.ArrayList.)]
    (Files/walkFileTree
      troot
      (proxy [SimpleFileVisitor] []
        (preVisitDirectory [dir ^BasicFileAttributes _a]
          (let [rel (.relativize troot ^Path dir)]
            (if (and (pos? (.getNameCount rel))
                  (= ".git" (str (.getName rel 0))))
              FileVisitResult/SKIP_SUBTREE
              FileVisitResult/CONTINUE)))
        (visitFile [file ^BasicFileAttributes attrs]
          (let [rel (.relativize troot ^Path file)]
            (when (and (< (.toMillis (.lastModifiedTime attrs)) (long fork-ms))
                    (not (Files/exists (.resolve croot rel) nofollow)))
              (.add acc (str rel))))
          FileVisitResult/CONTINUE)
        (visitFileFailed [_file _exc]
          FileVisitResult/CONTINUE)))
    (vec acc)))

;; =============================================================================
;; Hooks
;; =============================================================================

(defonce ^:private hooks (atom {:on-spawn [] :on-apply [] :on-discard []}))

(defn register-hook!
  "Register `hook-fn` for `hook-id` ∈ {:on-spawn :on-apply :on-discard}.
   Synchronous; exceptions swallowed."
  [hook-id hook-fn]
  (swap! hooks update hook-id (fnil conj []) hook-fn)
  hook-id)

(defn- fire-hook!
  [hook-id & args]
  (doseq [f (clojure.core/get @hooks hook-id)]
    (try (apply f args)
      (catch Throwable _ nil))))

;; =============================================================================
;; Lookup
;; =============================================================================

(defn get
  "Return the workspace with `workspace-id`, or nil."
  [db-info workspace-id]
  (p/db-workspace-get db-info workspace-id))

(defn list-active
  "Active workspaces for `repo-id`, newest first."
  [db-info repo-id]
  (p/db-workspace-list-by-repo db-info repo-id #{:active}))

(defn list-finished
  "Discarded workspaces for `repo-id`, newest first."
  [db-info repo-id]
  (p/db-workspace-list-by-repo db-info repo-id #{:discarded}))

(defn for-session
  "Workspace pinned to `session-state-id`, or nil."
  [db-info session-state-id]
  (p/db-workspace-for-session db-info session-state-id))

;; -----------------------------------------------------------------------------
;; Label + focus + hydration
;; -----------------------------------------------------------------------------

(defn set-label!
  "Set the workspace's human-friendly `:label`. Empty/nil clears it."
  [db-info {:keys [workspace-id label]}]
  (let [trimmed (some-> label str str/trim not-empty)]
    (p/db-workspace-update-label! db-info workspace-id trimmed)))

(defn focus!
  "Stamp `last_focused_at_ms` and upsert the per-repo `repo_focus`
   pointer. Returns the updated workspace record."
  [db-info workspace-id]
  (when-let [ws (p/db-workspace-touch-focus! db-info workspace-id)]
    (when (:repo-id ws)
      (p/db-repo-focus-set! db-info (:repo-id ws) workspace-id))
    ws))

(defn last-focused
  "Workspace id from `repo_focus` for `repo-id`, or nil."
  [db-info repo-id]
  (some-> (p/db-repo-focus-get db-info repo-id) :workspace-id))

(defn display-label
  "Human-facing label for `workspace`. Order: explicit `:label` →
   pinned session title → clone name (`:branch`) → id prefix."
  ([workspace]
   (display-label nil workspace nil))
  ([db-info workspace session]
   (let [hydrated (or session
                    (when (and db-info (:id workspace))
                      (some->> (:id workspace)
                        (p/db-session-state-list-for-workspace db-info)
                        first)))]
     (or (some-> (:label workspace) str/trim not-empty)
       (some-> hydrated :title str/trim not-empty)
       (some-> (:id workspace) str (subs 0 (min 8 (count (str (:id workspace))))))))))

(defn workspace-with-session
  "Hydrate `workspace-id` with its pinned `session_state`. Returns
   `{:workspace <ws> :session-state <ss>}`."
  [db-info workspace-id]
  (when-let [ws (p/db-workspace-get db-info workspace-id)]
    {:workspace     ws
     :session-state (some->> workspace-id
                      (p/db-session-state-list-for-workspace db-info)
                      first)}))

(defn list-active-with-sessions
  "Like `list-active` but each entry is the `{:workspace :session-state}`
   pair, sorted by `last_focused_at_ms` DESC NULLS LAST, then
   `created_at` DESC."
  [db-info repo-id]
  (let [rows (list-active db-info repo-id)
        cmp  (fn [a b]
               (let [recency-of #(or (:last-focused-at-ms %) Long/MIN_VALUE)
                     ra (recency-of a) rb (recency-of b)]
                 (cond
                   (not= ra rb) (compare rb ra)
                   :else        (compare (str (:created-at b)) (str (:created-at a))))))]
    (mapv (fn [ws]
            (let [pair (workspace-with-session db-info (:id ws))]
              (assoc pair :workspace ws)))
      (sort cmp rows))))

(defn status
  "Enrich a workspace record with live status of its clone. Stamps
   `:workspace/root`, `:workspace/sandbox?` (always true — every
   workspace is a clone), `:workspace/exists?`, `:workspace/changed`
   (count of since-fork edits) and `:workspace/dirty?`. No git."
  [db-info workspace-id]
  (when-let [ws (get db-info workspace-id)]
    (let [root    (:root ws)
          fork-ms (fork-ms-of ws)]
      (try
        (let [exists? (.exists (io/file root))
              changed (when (and exists? fork-ms) (count (changed-paths root fork-ms)))]
          (assoc ws
            :workspace/root     root
            :workspace/sandbox? true
            :workspace/exists?  exists?
            :workspace/changed  (or changed 0)
            :workspace/dirty?   (boolean (and changed (pos? changed)))
            ;; Back-compat alias for channels still reading `:vcs/dirty?`.
            :vcs/kind           :rift
            :vcs/dirty?         (boolean (and changed (pos? changed)))))
        (catch Throwable t
          (assoc ws
            :workspace/exists? (.exists (io/file root))
            :workspace/error   (or (ex-message t) (str t))))))))

(defn trunk-info
  "The user's real cwd (trunk). No git read; just the launch dir."
  ([] (trunk-info nil))
  ([root]
   {:repo-root (or (some-> root file-path) (trunk-root))}))

;; =============================================================================
;; Mutations
;; =============================================================================

(defn draft?
  "A workspace is a DRAFT (rift clone) when it carries a fork timestamp.
   Trunk workspaces (the real cwd) have none."
  [ws]
  (some? (fork-ms-of ws)))

(defn- rift-clone-dir
  "Where rift materialises a clone named `name` for `trunk`:
   ~/.vis/drafts/<repo-basename>/<name>."
  [trunk name]
  (io/file (draft-store-root trunk) name))

(defn- free-draft-name
  "Draft folder name derived from `label` that doesn't collide with an
   existing rift clone (appends -2, -3, … on collision). So `/draft new
   feature` lives at ~/.vis/drafts/<repo>/feature."
  [trunk label]
  (let [base (sanitize-id (or label "draft"))]
    (loop [n base i 2]
      (if (.exists (rift-clone-dir trunk n))
        (recur (str base "-" i) (inc i))
        n))))

(defn- insert-trunk!
  "Insert a fresh TRUNK workspace row (root = repo_root = `root`, defaulting
   to the real cwd; no clone, no fork_ms) and pin it to `session-state-id`
   when given."
  ([db-info session-state-id] (insert-trunk! db-info session-state-id (trunk-root)))
  ([db-info session-state-id root]
   (let [trunk (file-path root)
         ws    (p/db-workspace-insert! db-info
                 {:repo-id   (repo-id-for trunk)
                  :repo-root trunk
                  :root      trunk
                  :state     :active})]
     (when session-state-id
       (p/db-session-state-set-workspace! db-info session-state-id (:id ws)))
     ws)))

(defn ensure-workspace!
  "Find-or-create the session's workspace. The DEFAULT is TRUNK — the
   user's real cwd (no clone); the agent works directly in the repo
   until `/draft new`. Resume returns whatever the session was pinned to
   (trunk, or an open draft). Idempotent per session-state."
  [db-info {:keys [session-state-id]}]
  (or (for-session db-info session-state-id)
    (insert-trunk! db-info session-state-id)))

(defn create-trunk-at!
  "Mint a TRUNK workspace rooted at `root` (an arbitrary directory), not
   pinned to any session. Lets a channel open a session under a directory
   OTHER than the one vis was launched from — a tab in another project.
   Returns the workspace row (with `:id`) to pass as `:workspace-id` when
   creating the session."
  [db-info root]
  (insert-trunk! db-info nil (file-path root)))

(defn create!
  "Create a DRAFT: a rift CoW clone of a parent tree whose folder is
   named after `label` (`/draft new feature` → ~/.vis/drafts/<repo>/feature),
   and pin it 1:1 to `:session-state-id` (enters the draft). Captures the
   fork timestamp (`fork_ms`) for the since-fork diff. Fires :on-spawn.

   The fork PARENT is chosen so `apply!` lands back where it forked from:
   pass `:from <parent-workspace>` to clone that workspace's `:root` and
   inherit its `:repo-root` (apply target); otherwise the parent is the
   user's real cwd (trunk)."
  [db-info {:keys [session-state-id label from]}]
  (let [parent  (or (:root from) (trunk-root))
        trunk   (or (:repo-root from) (trunk-root))
        rid     (repo-id-for trunk)
        nm      (free-draft-name trunk label)
        clone   (cow-clone! parent nm)
        ;; Capture AFTER the clone returns: cloned files keep their (older)
        ;; source mtime, so only post-fork agent edits exceed this.
        fork-ms (System/currentTimeMillis)
        ws      (p/db-workspace-insert! db-info
                  {:repo-id   rid
                   :repo-root trunk
                   :root      clone
                   :state     :active
                   :fork-ms   fork-ms})
        ;; Label = the actual folder name (`nm`), so the displayed `<label>
        ;; (DRAFT)` always matches the rift dir — including the -2/-3 suffix
        ;; added when the requested name already exists.
        ws      (or (p/db-workspace-update-label! db-info (:id ws) nm) ws)]
    (when session-state-id
      (p/db-session-state-set-workspace! db-info session-state-id (:id ws)))
    (fire-hook! :on-spawn ws)
    ws))

(defn exit-to-trunk!
  "Repoint `session-state-id` back to a TRUNK workspace (the real cwd),
   leaving any draft. Returns the trunk workspace now pinned."
  [db-info session-state-id]
  (insert-trunk! db-info session-state-id))

(defn apply!
  "Land the clone's since-fork edits into the user's real cwd (trunk),
   leaving them uncommitted for the user to review/commit themselves.
   Vis owns no git lifecycle. Adds/modifications come from the mtime
   diff; deletions are files that existed at the fork but the agent
   removed in the draft. Returns
   `{:status :ok :changed [{:status :path}] :landed n :workspace ws}`."
  [db-info {:keys [workspace-id]}]
  (let [ws (get db-info workspace-id)]
    (when-not ws
      (throw (ex-info "Unknown workspace" {:workspace-id workspace-id})))
    (let [clone   (:root ws)
          trunk   (:repo-root ws)
          fork-ms (fork-ms-of ws)]
      (when-not fork-ms
        (throw (ex-info "Workspace has no fork timestamp; cannot apply"
                 {:type :workspace/no-baseline :workspace-id workspace-id})))
      (let [edits   (mapv (fn [path]
                            (let [src    (io/file clone path)
                                  dst    (io/file trunk path)
                                  status (if (.exists dst) :modify :add)]
                              (io/make-parents dst)
                              (Files/copy (.toPath src) (.toPath dst) copy-opts)
                              {:status status :path path}))
                      (changed-paths clone fork-ms))
            deletes (mapv (fn [path]
                            (.delete (io/file trunk path))
                            {:status :delete :path path})
                      (deleted-paths clone trunk fork-ms))
            changes (into edits deletes)]
        (fire-hook! :on-apply ws {:changed changes})
        {:status    :ok
         :changed   changes
         :landed    (count changes)
         :workspace ws}))))

(defn abandon!
  "Trash the clone (rift `remove!` + `gc`, or recursive delete on the
   fallback) and transition the row to :discarded. `:reason` is passed
   to the :on-discard hook and echoed back for the lineage record.
   Returns the updated workspace."
  [db-info {:keys [workspace-id reason]}]
  (let [ws (get db-info workspace-id)]
    (when-not ws
      (throw (ex-info "Unknown workspace" {:workspace-id workspace-id})))
    (rift-trash! (:root ws))
    (let [done (p/db-workspace-update-state! db-info workspace-id :discarded)]
      (fire-hook! :on-discard done {:reason reason})
      (assoc (or done ws) :reason reason))))
