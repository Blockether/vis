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
           [java.nio.file.attribute BasicFileAttributes FileAttribute
            PosixFilePermission]
           [java.util UUID]))

;; =============================================================================
;; Dynamic cwd binding
;; =============================================================================

(def ^:dynamic *workspace-root*
  "Canonical workspace root for the current tool call. Bound per-turn
   by the channel layer; never `nil` in normal operation."
  nil) 

 (def ^:dynamic *context-roots*
  "Extra context roots the current tool call may ALSO operate under, beyond the
   primary `*workspace-root*`, as `[{:trunk :clone}]` canonical pairs: `:trunk`
   is the REAL directory the user added (what the model addresses), `:clone` is
   the rift CoW working copy edits land in (== `:trunk` when live). Bound
   per-turn by the channel layer from the session's persisted context roots.
   Empty in the common single-root case. The editing layer confines to the
   clones and transparently remaps trunk↔clone (see `context-root-mappings`)."
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

(defn- root-entry
  "Normalize one persisted context-root entry to `{:trunk :clone :fork-ms}`.
   Entries are always maps (`<-json` keywordizes keys): `:trunk` is the real
   dir, `:clone` its rift CoW working copy (== `:trunk` when live), `:fork-ms`
   the since-fork mtime baseline (nil = live). Returns nil for junk."
  [e]
  (when (map? e)
    (let [t (normalize-root (:trunk e))
          c (normalize-root (:clone e))]
      (when t {:trunk t :clone (or c t) :fork-ms (:fork-ms e)}))))

(defn env-context-roots
  "Canonical `[{:trunk :clone}]` pairs for the current tool call's extra
   context roots, beyond the primary. Reads `:workspace/context-roots` from an
   env map (or a raw coll). `:trunk` is the real dir the model addresses;
   `:clone` is the rift CoW working copy edits land in (== trunk when live).
   The channel layer binds `*context-roots*` from this per turn; the editing
   layer confines to the clones and transparently remaps trunk↔clone."
  [env-or-roots]
  (let [roots (if (map? env-or-roots)
                (:workspace/context-roots env-or-roots)
                env-or-roots)]
    (vec (keep (fn [e] (when-let [{:keys [trunk clone]} (root-entry e)]
                         {:trunk trunk :clone clone}))
           roots))))

(defn cwd
  "Resolve the current workspace cwd. In production the channel
   wrapper binds `*workspace-root*` per turn, so the process-cwd
   fallback only fires from REPL / test / one-off CLI paths that have
   no session context."
  ^File []
  (io/file (or *workspace-root* (System/getProperty "user.dir")))) 

 (defn allowed-roots
  "Canonical absolute CLONE/working-copy paths the current tool call may
   operate under: the primary cwd FIRST, then each bound context root's
   `:clone`. Deduped; the primary is always present. The confinement set the
   editing layer's `safe-path` checks the (possibly remapped) target against."
  []
  (let [primary (.getCanonicalPath (cwd))
        extra   (keep #(some-> (:clone %) normalize-root) *context-roots*)]
    (vec (distinct (cons primary extra)))))

(defn context-root-mappings
  "The bound context roots as canonical `[{:trunk :clone}]` pairs — the
   trunk↔clone remap table the editing layer uses so the model can address a
   context file by its REAL (trunk) path while edits land in the `:clone`.
   Empty in the single-root case. Does NOT include the primary (relative paths
   resolve under cwd directly)."
  []
  (vec *context-roots*))

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

(defn- read-only-perms
  "Map {Path original-POSIX-perms} for every regular file under `src` lacking
   OWNER_WRITE. Empty on a non-POSIX filesystem (rift is unsupported there
   anyway), so callers degrade to a plain clone."
  [src]
  (try
    (let [no-link (make-array LinkOption 0)]
      (into {}
        (comp (filter #(.isFile ^File %))
          (map (fn [^File f]
                 (let [p (.toPath f)]
                   [p (Files/getPosixFilePermissions p no-link)])))
          (filter (fn [[_ ^java.util.Set perms]]
                    (not (.contains perms PosixFilePermission/OWNER_WRITE)))))
        (file-seq (io/file src))))
    (catch Exception _ {})))

(defn- with-source-writable
  "Run `thunk` with every read-only file under `src` temporarily granted
   OWNER_WRITE, then restore each file's exact original perms. Works around
   rift's macOS per-entry CoW clone failing EACCES on mode-444 source files —
   git stores ALL loose/pack objects 444, so without this `/draft` can never
   clone a real repo on macOS. `chmod` leaves mtime untouched, so the
   since-fork diff is unaffected; a 444->644 object left behind on an
   interrupted restore is functionally harmless to git."
  [src thunk]
  (let [orig (read-only-perms src)]
    (doseq [[^Path p ^java.util.Set perms] orig]
      (let [w (java.util.HashSet. perms)]
        (.add w PosixFilePermission/OWNER_WRITE)
        (Files/setPosixFilePermissions p w)))
    (try
      (thunk)
      (finally
        (doseq [[^Path p perms] orig]
          (try (Files/setPosixFilePermissions p perms) (catch Exception _ nil)))))))

(defn- cow-clone!
  "Clone `src` tree under `name` via rift's CoW `create`, returning the
   clone's absolute path. RIFT ONLY — there is no plain-copy fallback: a
   draft must be a real copy-on-write clone (instant, near-zero disk) or
   nothing. On an unsupported FS/platform rift throws and the error
   propagates so the failure is loud, never a silent slow copy.

   The clone runs inside `with-source-writable` so read-only (mode-444)
   source files — every git loose/pack object — don't abort rift's macOS
   per-entry CoW with EACCES; their exact perms are restored afterward."
  [src name]
  (rift/init {:at src})
  (let [into (draft-store-root src)]
    ;; rift won't materialise into a non-existent parent — ensure the
    ;; ~/.vis/drafts/<repo> store dir exists first, else `create` errors.
    ;; `createDirectories` is idempotent: a no-op when the dir already
    ;; exists, and throws only on a genuine failure (e.g. permissions).
    (Files/createDirectories (.toPath into) (make-array FileAttribute 0))
    (with-source-writable src
      #(rift/create {:from src :name name :into (str into)}))))

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

(def ^:private prune-root-dirs
  "Top-level directory names pruned from the since-fork diff: VCS internals
   plus build/dependency/editor caches that churn on mtime but are never
   meaningful agent edits and are gitignored anyway. Landing them into trunk
   is always wrong, and — because a cache like `.clj-kondo/.cache` or `target`
   holds thousands of files the JVM/clj-kondo rewrites on startup — letting
   them into `changed-paths` bloats `changed_files` (and any sub_loop result
   built from it) enough to overflow the model context."
  #{".git" ".cpcache" ".lsp" ".lsp-cache" "target" "node_modules"
    ".shadow-cljs" ".cljs_node_repl" ".gitlibs" ".gradle" ".idea"})

(defn- prune-dir?
  "True when the clone-relative directory `rel` should be skipped by the
   diff: a top-level VCS/build/cache dir (`prune-root-dirs`), or the
   clj-kondo analysis cache specifically (`.clj-kondo/.cache` — we keep the
   tracked `.clj-kondo/config.edn`, prune only the churny cache subtree)."
  [^Path rel]
  (let [c (.getNameCount rel)]
    (and (pos? c)
      (let [s0 (str (.getName rel 0))]
        (or (contains? prune-root-dirs s0)
          (and (= ".clj-kondo" s0) (>= c 2)
            (= ".cache" (str (.getName rel 1)))))))))

(defn changed-paths
  "Repo-relative paths of files under `clone` whose mtime is newer than
   `fork-ms` — i.e. exactly what the agent touched since the fork
   (`clonefile` preserves source mtimes, so untouched files stay older).
   Prunes VCS/build/cache dirs (`prune-dir?`) — landing `.git/` would corrupt
   trunk's repo, and tool caches would flood the result. Returns a vec
   of strings."
  [clone fork-ms]
  (let [root (.toPath (io/file clone))
        acc  (java.util.ArrayList.)]
    (Files/walkFileTree
      root
      (proxy [SimpleFileVisitor] []
        (preVisitDirectory [dir ^BasicFileAttributes _a]
          (if (prune-dir? (.relativize root ^Path dir))
            FileVisitResult/SKIP_SUBTREE
            FileVisitResult/CONTINUE))
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
   `trunk` (pruning VCS/build/cache dirs via `prune-dir?`) with an mtime
   older than `fork-ms` — so they existed at the fork and are not user
   post-fork additions — yet absent from `clone`. The mtime guard means
   `apply!` never reverts a file the user added to cwd after forking, and
   the prune keeps cache churn (e.g. `.clj-kondo/.cache` rewritten in the
   clone) from being reported as spurious deletions."
  [clone trunk fork-ms]
  (let [troot    (.toPath (io/file trunk))
        croot    (.toPath (io/file clone))
        nofollow (into-array LinkOption [LinkOption/NOFOLLOW_LINKS])
        acc      (java.util.ArrayList.)]
    (Files/walkFileTree
      troot
      (proxy [SimpleFileVisitor] []
        (preVisitDirectory [dir ^BasicFileAttributes _a]
          (if (prune-dir? (.relativize troot ^Path dir))
            FileVisitResult/SKIP_SUBTREE
            FileVisitResult/CONTINUE))
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

;; `draft?` / `free-draft-name` are defined further down (Mutations); the
;; context-root autoclone path needs them here.
(declare draft? free-draft-name)

(defn context-roots
  "Extra context roots configured for `ws` (a workspace record), normalized to
   `[{:trunk :clone :fork-ms}]`. `:trunk` is the real directory the user added;
   `:clone` is its rift CoW working copy in a draft (== `:trunk` on a live
   trunk session or an unsupported FS); `:fork-ms` is the since-fork mtime
   baseline (nil = live, no isolation). Empty vec when none."
  [ws]
  (vec (keep root-entry (:context-roots ws))))

(defn add-context-root!
  "Add `path` (an arbitrary directory) to the workspace's extra context roots,
   so the session may also operate on files under it. AUTOCLONE BY DEFAULT:
   when the session's workspace is a DRAFT (and rift CoW is supported), the
   added root is cloned too — edits land in the clone, isolated, and ride the
   SAME `/draft apply` / `/draft abandon` lifecycle as the primary (a draft is
   about the WHOLE workspace). On a live trunk session (or unsupported FS) the
   root is added live (clone == trunk). Canonicalizes, dedups by trunk,
   persists. Throws when `path` is not an existing directory. Returns the
   updated workspace record."
  [db-info workspace-id path]
  (when-let [ws (get db-info workspace-id)]
    (let [canon (normalize-root path)
          dir   (some-> canon io/file)
          roots (context-roots ws)]
      (cond
        (nil? canon)
        (throw (ex-info "Path is blank" {:type :workspace/blank-path :path path}))

        (not (.isDirectory ^File dir))
        (throw (ex-info (str "Not a directory: " path)
                 {:type :workspace/not-a-directory :path path}))

        (some #(= canon (:trunk %)) roots)
        ws ;; idempotent — already a context root

        :else
        (let [entry (if (and (draft? ws) (rift-supported?))
                      ;; Mirror the primary: CoW-clone the added root so the
                      ;; draft isolates it too. fork-ms captured AFTER the clone
                      ;; (cloned files keep older source mtimes; only post-fork
                      ;; edits exceed it — same baseline trick as create!).
                      (let [nm    (free-draft-name canon "ctx")
                            clone (cow-clone! canon nm)]
                        {:trunk canon :clone clone :fork-ms (System/currentTimeMillis)})
                      {:trunk canon :clone canon :fork-ms nil})]
          (p/db-workspace-set-context-roots! db-info workspace-id
            (conj roots entry)))))))

(defn remove-context-root!
  "Remove `path` from the workspace's extra context roots, trashing its CoW
   clone (rift `remove!` + `gc`) when it had one. Returns the updated workspace
   record (unchanged set when `path` was not a context root)."
  [db-info workspace-id path]
  (when-let [ws (get db-info workspace-id)]
    (let [canon (normalize-root path)
          roots (context-roots ws)
          gone  (some #(when (= canon (:trunk %)) %) roots)]
      (when (and gone (:clone gone) (not= (:clone gone) (:trunk gone)))
        (try (rift-trash! (:clone gone)) (catch Throwable _ nil)))
      (p/db-workspace-set-context-roots! db-info workspace-id
        (vec (remove #(= canon (:trunk %)) roots))))))

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
            ;; Sandbox-ness is on :workspace/sandbox?, not :vcs/kind — `:rift`
            ;; is the CoW-clone mechanism, not a VCS, and isn't in the ctx-spec
            ;; set. The real :vcs/kind is computed model-side in
            ;; foundation.workspace-ctx (this status fn is intentionally git-free).
            ;; Back-compat alias for channels still reading `:vcs/dirty?`.
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

(defn- land-clone!
  "Copy one clone tree's since-fork edits + deletions into its `trunk`,
   tagging each change with the `trunk` it landed under (so a multi-root
   apply is unambiguous). Returns a vec of `{:status :path :root}`."
  [clone trunk fork-ms]
  (let [edits   (mapv (fn [path]
                        (let [src    (io/file clone path)
                              dst    (io/file trunk path)
                              status (if (.exists dst) :modify :add)]
                          (io/make-parents dst)
                          (Files/copy (.toPath src) (.toPath dst) copy-opts)
                          {:status status :path path :root trunk}))
                  (changed-paths clone fork-ms))
        deletes (mapv (fn [path]
                        (.delete (io/file trunk path))
                        {:status :delete :path path :root trunk})
                  (deleted-paths clone trunk fork-ms))]
    (into edits deletes)))

(defn apply!
  "Land the draft's since-fork edits into the user's real dirs (trunk),
   leaving them uncommitted for the user to review/commit. A draft is about
   the WHOLE workspace: this lands the primary clone AND every auto-cloned
   context root (each into its own trunk). Vis owns no git lifecycle.
   Adds/modifications come from the mtime diff; deletions are files that
   existed at the fork but the agent removed in the draft. Returns
   `{:status :ok :changed [{:status :path :root}] :landed n :workspace ws}`."
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
      (let [primary (land-clone! clone trunk fork-ms)
            ;; each isolated context root lands back into its own trunk
            extra   (mapcat (fn [{:keys [trunk clone fork-ms]}]
                              (when (and fork-ms (not= clone trunk))
                                (land-clone! clone trunk fork-ms)))
                      (context-roots ws))
            changes (vec (concat primary extra))]
        (fire-hook! :on-apply ws {:changed changes})
        {:status    :ok
         :changed   changes
         :landed    (count changes)
         :workspace ws}))))

(defn abandon!
  "Trash the draft's clones (rift `remove!` + `gc`) — the primary AND every
   auto-cloned context root — and transition the row to :discarded. `:reason`
   is passed to the :on-discard hook and echoed back for the lineage record.
   Returns the updated workspace."
  [db-info {:keys [workspace-id reason]}]
  (let [ws (get db-info workspace-id)]
    (when-not ws
      (throw (ex-info "Unknown workspace" {:workspace-id workspace-id})))
    (rift-trash! (:root ws))
    ;; trash every isolated context-root clone too (a draft is whole-workspace)
    (doseq [{:keys [trunk clone]} (context-roots ws)
            :when (and clone (not= clone trunk))]
      (try (rift-trash! clone) (catch Throwable _ nil)))
    (let [done (p/db-workspace-update-state! db-info workspace-id :discarded)]
      (fire-hook! :on-discard done {:reason reason})
      (assoc (or done ws) :reason reason))))

(defn discard-session-clones!
  "On session DELETE (not a mere quit/close): trash the on-disk rift clones of
   the session's workspace — the primary draft clone AND every auto-cloned
   context root — so deleting a session leaves no orphan drafts under
   ~/.vis/drafts. ONLY drafts carry clones; a TRUNK workspace's roots are the
   user's REAL directories and are never touched (the `draft?` guard is the
   safety invariant). No DB writes — the session tree is being deleted anyway.
   Quitting/closing a session without deleting keeps the draft intact."
  [db-info session-soul-id]
  (when (and db-info session-soul-id)
    (when-let [state-id (p/db-latest-session-state-id db-info session-soul-id)]
      (when-let [ws (for-session db-info state-id)]
        (when (draft? ws)
          (try (rift-trash! (:root ws)) (catch Throwable _ nil))
          (doseq [{:keys [trunk clone]} (context-roots ws)
                  :when (and clone (not= clone trunk))]
            (try (rift-trash! clone) (catch Throwable _ nil))))))))
