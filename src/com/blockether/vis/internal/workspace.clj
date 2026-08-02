(ns com.blockether.vis.internal.workspace
  "Backend-neutral workspaces, DB-pinned to session_state 1:1.

   The user's real cwd is *trunk* — Vis never mutates it, and Vis no
   longer requires it to be a git repo. A session works in trunk by
   default; `/draft new` opts into an isolated workspace supplied by a
   registered backend. Backends declare concrete capabilities such as
   isolated fork, rollback, merge-back, retained revisions, and parallel
   safety. Core never assumes which implementation provides them.

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
            [clojure.set :as set]
            [clojure.string :as str]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.persistance :as p])
  (:import [java.io File]
           [java.nio.file CopyOption FileVisitResult Files LinkOption Path SimpleFileVisitor
            StandardCopyOption]
           [java.nio.file.attribute BasicFileAttributes]))

;; =============================================================================
;; Dynamic cwd binding
;; =============================================================================

(def ^:dynamic *workspace-root*
  "Canonical workspace root for the current tool call. Bound per-turn
   by the channel layer; never `nil` in normal operation."
  nil)

(def ^:dynamic *filesystem-roots*
  "Configured filesystem catalog entries available to the current tool call, as
   canonical `[{:trunk :clone}]` pairs. These are rebuilt from `vis.yml` on
   `/reload`; they are never session-persisted or mutable through a command."
  nil)

(defn normalize-root
  "Canonicalize a workspace root string/File. Blank/nil → nil."
  [root]
  (let
    [s (some-> root
               str
               str/trim)]
    (when (seq s) (.getCanonicalPath (io/file s)))))

(defn workspace-root
  "Extract a canonical :workspace/root from an env map or raw root value."
  [env-or-root]
  (normalize-root (if (map? env-or-root) (:workspace/root env-or-root) env-or-root)))

(defn- backend-id
  [value]
  (cond (keyword? value) value
        (string? value) (keyword value)
        :else :live))

(defn- root-entry
  "Normalize one persisted filesystem-root entry to `{:trunk :clone :fork-ms}`.
   Entries are always maps (`<-json` keywordizes keys): `:trunk` is the real
   dir, `:clone` its backend working copy (== `:trunk` when live), `:fork-ms`
   the since-fork mtime baseline (nil = live). Returns nil for junk."
  [e]
  (when (map? e)
    (let
      [t
       (normalize-root (:trunk e))

       c
       (normalize-root (:clone e))]

      (when t
        {:trunk t :clone (or c t) :fork-ms (:fork-ms e) :backend (backend-id (:backend e))}))))

(defn env-filesystem-roots
  "Canonical `[{:trunk :clone}]` pairs available to the current tool call.
   Includes configured workspace catalog entries and immutable read/write roots from
   the environment security snapshot. Configured roots map to themselves (no draft
   clone). With the jail disabled, host filesystem roots are granted and marked
   no-search so explicit paths are unrestricted without making default searches
   crawl the machine."
  [env-or-roots]
  (let
    [environment?
     (map? env-or-roots)

     unrestricted?
     (and environment? (false? (get-in env-or-roots [:security-policy :sandbox])))

     workspace-roots
     (if environment? (:workspace/filesystem-roots env-or-roots) env-or-roots)

     host-roots
     (when unrestricted?
       (keep (fn [^java.io.File root]
               (.getCanonicalPath root))
             (java.io.File/listRoots)))

     configured-roots
     (if unrestricted? host-roots (when environment? (:security/filesystem-roots env-or-roots)))

     no-search
     (if unrestricted?
       (into #{} (keep normalize-root) host-roots)
       (when environment?
         (into #{} (keep normalize-root) (:security/no-search-roots env-or-roots))))]

    (vec (distinct (concat (keep (fn [e]
                                   (when-let [{:keys [trunk clone]} (root-entry e)]
                                     {:trunk trunk :clone clone}))
                                 workspace-roots)
                           (keep (fn [path]
                                   (when-let [root (normalize-root path)]
                                     {:trunk root
                                      :clone root
                                      :no-search? (boolean (and no-search
                                                                (contains? no-search root)))}))
                                 configured-roots))))))

(defn cwd
  "Resolve the current workspace cwd. In production the channel
   wrapper binds `*workspace-root*` per turn, so the process-cwd
   fallback only fires from REPL / test / one-off CLI paths that have
   no session context."
  ^File []
  (io/file (or *workspace-root* (System/getProperty "user.dir"))))

(defn allowed-roots
  "Canonical absolute CLONE/working-copy paths the current tool call may
   operate under: the primary cwd FIRST, then each bound filesystem root's
   `:clone`. Deduped; the primary is always present. The confinement set the
   editing layer's `safe-path` checks the (possibly remapped) target against."
  []
  (let
    [primary
     (.getCanonicalPath (cwd))

     extra
     (keep #(some-> (:clone %)
                    normalize-root)
           *filesystem-roots*)]

    (vec (distinct (cons primary extra)))))

(defn no-search-roots
  "Canonical CLONE paths of bound filesystem roots flagged `search: false` in
   the workspace catalog. `resolve-search-roots` prunes these from the DEFAULT
   grep sweep; explicit paths still reach them."
  []
  (into #{}
        (keep (fn [e]
                (when (:no-search? e)
                  (some-> (:clone e)
                          normalize-root))))
        *filesystem-roots*))

(defn filesystem-root-mappings
  "The bound filesystem roots as canonical `[{:trunk :clone}]` pairs — the
   trunk↔clone remap table the editing layer uses so the model can address a
   context file by its REAL (trunk) path while edits land in the `:clone`.
   Empty in the single-root case. Does NOT include the primary (relative paths
   resolve under cwd directly)."
  []
  (vec *filesystem-roots*))

(defn- file-path ^String [f] (.getCanonicalPath (io/file f)))

(defn trunk-root
  "The user's real working directory — where they launched `vis`.
   Canonical absolute path. This is *trunk*: never mutated, never
   required to be a git repo. (`bin/vis-agent` preserves the invocation cwd
   as JVM user.dir even though it cd's to the repo for deps.)"
  ^String []
  (file-path (System/getProperty "user.dir")))

(defn- sanitize-id
  [s]
  (let
    [s (-> (str (or s "ws"))
           str/lower-case
           (str/replace #"[^a-z0-9._-]+" "-")
           (str/replace #"(^-+|-+$)" ""))]
    (if (str/blank? s) "ws" s)))

(defn- repo-id-for
  "Stable per-root grouping id (sanitized basename + path hash).
   Groups a repo's clones together in listings; no git involved."
  [root]
  (let
    [root
     (file-path root)

     name
     (sanitize-id (.getName (io/file root)))

     hash
     (Long/toUnsignedString (Integer/toUnsignedLong (hash root)) 36)]

    (str name "-" hash)))

;; =============================================================================
;; Workspace backend registry and capability matrix
;; =============================================================================

(def workspace-capabilities
  "Closed capability vocabulary for workspace backends."
  #{:isolated-fork :merge-back :rollback :retained-revisions :parallel-safe})

(def ^:private draft-required-capabilities
  #{:isolated-fork :merge-back :rollback :retained-revisions})

(defonce ^:private backend-registry (atom {}))

(defn workspace-backend
  "Validate and return a workspace backend descriptor.

   Required keys:
     :workspace.backend/id            keyword
     :workspace.backend/capabilities  capability set
     :workspace.backend/available-fn  ({:source-root :store-root} -> bool or
                                      {:available? bool :reason keyword :details map})
     :workspace.backend/fork-fn       ({:source-root :store-root :name} -> path)
     :workspace.backend/discard-fn    ({:root} -> nil)"
  [backend]
  (let
    [id
     (:workspace.backend/id backend)

     caps
     (:workspace.backend/capabilities backend)]

    (when-not (keyword? id)
      (throw (ex-info "Workspace backend id must be a keyword"
                      {:type :workspace/invalid-backend :backend backend})))
    (when-not (and (set? caps) (every? workspace-capabilities caps))
      (throw (ex-info "Workspace backend has invalid capabilities"
                      {:type :workspace/invalid-backend :backend-id id :capabilities caps})))
    (doseq
      [k [:workspace.backend/available-fn :workspace.backend/fork-fn :workspace.backend/discard-fn]]
      (when-not (ifn? (clojure.core/get backend k))
        (throw (ex-info (str "Workspace backend requires " k)
                        {:type :workspace/invalid-backend :backend-id id :key k}))))
    (update backend :workspace.backend/priority #(long (or % 0)))))

(defn register-backend!
  "Register a workspace backend. Idempotent by backend id."
  [backend]
  (let
    [backend
     (workspace-backend backend)

     id
     (:workspace.backend/id backend)]

    (swap! backend-registry assoc id backend)
    backend))

(defn deregister-backend! [backend-id] (swap! backend-registry dissoc backend-id) nil)

(defn registered-backends
  "Registered workspace backends ordered by descending priority."
  []
  (->> @backend-registry
       vals
       (sort-by (juxt (comp - :workspace.backend/priority) (comp str :workspace.backend/id)))
       vec))

(defn capability-matrix
  "Describe every registered backend for `source-root`, including availability
   and declared capabilities. This is the public feature-discovery surface.
   It never loads extensions: extension discovery owns backend registration."
  ([source-root] (capability-matrix source-root source-root))
  ([source-root store-root]
   (mapv (fn [backend]
           (let
             [availability
              (try ((:workspace.backend/available-fn backend)
                     {:source-root (file-path source-root) :store-root (file-path store-root)})
                   (catch Throwable t
                     {:available? false
                      :reason :availability-check-failed
                      :details {:error (or (ex-message t) (str t))}}))

              availability
              (if (map? availability) availability {:available? (boolean availability)})]

             (merge {:backend (:workspace.backend/id backend)
                     :priority (:workspace.backend/priority backend)
                     :available? (boolean (:available? availability))
                     :capabilities (:workspace.backend/capabilities backend)}
                    (select-keys availability [:reason :details]))))
         (registered-backends))))

(defn select-backend
  "Select the highest-priority available backend covering `required`.
   Returns nil when no backend can provide the requested semantics."
  [source-root store-root required]
  (let
    [required
     (set required)

     available
     (capability-matrix source-root store-root)]

    (some (fn [{:keys [backend available? capabilities]}]
            (when (and available? (set/subset? required capabilities))
              (clojure.core/get @backend-registry backend)))
          available)))

(defn supports?
  "True when some backend can provide `required` for the given roots."
  ([source-root required] (supports? source-root source-root required))
  ([source-root store-root required] (boolean (select-backend source-root store-root required))))

(declare draft-store-root)

(defn workspace-capability-matrix
  "Capability matrix for a workspace (or root path), using the real derived
   workspace storage location rather than assuming source and destination are
   on the same filesystem."
  [workspace-or-root]
  (let
    [source-root
     (if (map? workspace-or-root) (:root workspace-or-root) workspace-or-root)

     repo-root
     (if (map? workspace-or-root) (:repo-root workspace-or-root) workspace-or-root)]

    (capability-matrix source-root (draft-store-root repo-root))))

(defn isolated-workspaces-supported?
  "True when the current root can create full draft workspaces."
  ([] (isolated-workspaces-supported? (trunk-root)))
  ([root] (supports? root (draft-store-root root) draft-required-capabilities)))

(def
  ^{:dynamic true
    :doc
    "Parent dir for every trunk's draft store. `nil` (production default)
             resolves to the `vis.drafts.dir` system property, else
             `~/.vis/drafts`. Tests point it at a throwaway dir (the `:test` alias
             sets `-Dvis.drafts.dir=target/vis-drafts-test`) so the suite never
             litters the real ~/.vis."}
  *drafts-home*
  nil)

(defn- drafts-home
  ^File []
  (io/file (or *drafts-home*
               (System/getProperty "vis.drafts.dir")
               (io/file (System/getProperty "user.home") ".vis" "drafts"))))

(defn- draft-store-root
  "Backend-neutral parent storage dir for a trunk's derived workspaces."
  ^File [trunk]
  (io/file (drafts-home) (.getName (io/file trunk))))

(defn drafts-store-path
  "Canonical path of the drafts-store parent (`~/.vis/drafts` by default, or the
   `vis.drafts.dir` override / `*drafts-home*` test binding). The DEFAULT search
   sweep prunes the raw `~/.vis` grant tree but KEEPS real draft clones, which
   live under this dir and remain searchable. nil when unresolvable."
  ^String []
  (try (.getCanonicalPath (drafts-home)) (catch Throwable _ nil)))

(def ^:private ^"[Ljava.nio.file.CopyOption;" copy-opts
  ^"[Ljava.nio.file.CopyOption;"
  (into-array CopyOption
              [StandardCopyOption/REPLACE_EXISTING StandardCopyOption/COPY_ATTRIBUTES
               LinkOption/NOFOLLOW_LINKS]))

(defn- backend-fork!
  [source-root store-root name required]
  (let
    [store-root
     (draft-store-root store-root)

     backend
     (select-backend source-root store-root required)]

    (when-not backend
      (throw (ex-info "No workspace backend provides the required capabilities"
                      {:type :workspace/capability-unavailable
                       :required (set required)
                       :source-root (file-path source-root)
                       :capability-matrix (capability-matrix source-root store-root)})))
    (let
      [root ((:workspace.backend/fork-fn backend)
              {:source-root (file-path source-root) :store-root (file-path store-root) :name name})]
      {:root (file-path root) :backend (:workspace.backend/id backend)})))

(defn- git*
  "Run `git <args>` inside `dir` and return `{:exit :out}` — `:exit` is nil when
   git could not be spawned or timed out. `env` entries are added to the child's
   environment (used to point git at a PRIVATE index). Deliberately LOCAL:
   `internal.git` already requires this namespace, so requiring it back would
   close a load cycle."
  ([^File dir args] (git* dir args nil))
  ([^File dir args env]
   (try (let
          [pb
           (doto (ProcessBuilder. ^java.util.List (into ["git"] (map str) args))
             (.directory dir)
             (.redirectErrorStream true))

           _
           (doseq [[k v] env]
             (.put (.environment pb) (str k) (str v)))

           p
           (.start pb)

           out
           (slurp (.getInputStream p))

           done
           (.waitFor p 120 java.util.concurrent.TimeUnit/SECONDS)]

          (when-not done (.destroyForcibly p))
          {:exit (when done (.exitValue p)) :out out})
        (catch Throwable t {:exit nil :out (str (ex-message t))}))))

(defn- git-lines
  "Non-blank stdout lines of a SUCCESSFUL `git <args>` in `dir`, else []."
  [^File dir args]
  (let [{:keys [exit out]} (git* dir args)]
    (if (= 0 exit) (into [] (remove str/blank?) (str/split-lines (str out))) [])))

(defn committed-head?
  "True when `root` sits in a git repository that ALREADY HAS a commit — the
   only thing a CLEAN draft can be seeded from."
  [root]
  (= 0 (:exit (git* (io/file root) ["rev-parse" "--verify" "--quiet" "HEAD"]))))

(defn- clean-seed-manifest
  "Sidecar file listing the repo-relative paths a CLEAN seed dropped from
   `clone`. It lives NEXT TO the clone, dot-prefixed: housekeeping ignores dot
   entries, the agent never sees it inside its workspace, and it can never be
   mistaken for repo content that `apply!` should land."
  ^File [clone]
  (let [f (io/file clone)]
    (io/file (.getParentFile f) (str "." (.getName f) ".clean-seed"))))

(defn- clean-seed-skips
  "Paths the clean seed deliberately left OUT of `clone`: the user's own
   uncommitted trunk files. They are absent from the clone by construction, so
   `deleted-paths` must never read that absence as an agent deletion and wipe
   them from trunk."
  [clone]
  (let [f (clean-seed-manifest clone)]
    (if (.isFile f) (into #{} (remove str/blank?) (str/split-lines (slurp f))) #{})))

(defn- clean-seed!
  "Scrub `clone` back to its COMMITTED HEAD: restore every tracked file to its
   committed content and delete the files HEAD does not carry (untracked plus
   index-added), while leaving gitignored build/dependency trees in place so the
   draft still builds. Returns — and records in `clean-seed-manifest` — the
   repo-relative paths it dropped, which is exactly the set `deleted-paths` must
   ignore forever after.

   Deliberately NOT `git reset --hard`: when trunk is a LINKED WORKTREE the
   clone's `.git` still points at the ORIGINAL repository's admin directory, so
   a reset would rewrite the user's real index and silently unstage their work.
   Reading HEAD into a PRIVATE index and checking that out touches only files
   inside the clone; no shared git state is ever written."
  [clone]
  (let
    [dir
     (io/file clone)

     ;; Snapshot BEFORE scrubbing — afterwards these files are gone and git can
     ;; no longer name them. Untracked files plus files staged as ADDED but
     ;; never committed: neither exists in HEAD, both are the user's own.
     dropped
     (into (sorted-set)
           (concat (git-lines dir ["ls-files" "--others" "--exclude-standard"])
                   (git-lines dir ["diff" "--cached" "--name-only" "--diff-filter=A" "HEAD"])))

     index
     (io/file (.getParentFile (io/file clone)) (str "." (.getName (io/file clone)) ".clean-index"))

     env
     {"GIT_INDEX_FILE" (.getCanonicalPath index)}

     read-tree
     (git* dir ["read-tree" "HEAD"] env)

     checkout
     (when (= 0 (:exit read-tree)) (git* dir ["checkout-index" "-a" "-f"] env))]

    (try (when-not (and (= 0 (:exit read-tree)) (= 0 (:exit checkout)))
           (throw (ex-info "Could not seed the draft from the committed HEAD"
                           {:type :workspace/clean-seed-failed
                            :root (file-path clone)
                            :read-tree-out (:out read-tree)
                            :checkout-out (:out checkout)})))
         ;; `git clean` would consult the SHARED index (and so keep index-added
         ;; files); deleting the snapshot directly is both exact and inert.
         (doseq [rel dropped]
           (io/delete-file (io/file dir (str rel)) true))
         (spit (clean-seed-manifest clone) (str/join "\n" dropped))
         (vec dropped)
         (finally (io/delete-file index true)))))

(defn- discard-root!
  [backend-id root]
  ;; The clean-seed sidecar lives NEXT TO the clone, so the backend's own
  ;; discard never sees it — drop it with the clone it describes.
  (when root (io/delete-file (clean-seed-manifest root) true))
  (when (and root (not= :live backend-id))
    (if-let [backend (clojure.core/get @backend-registry backend-id)]
      ((:workspace.backend/discard-fn backend) {:root (file-path root)})
      (throw (ex-info "Workspace backend is not registered"
                      {:type :workspace/backend-unavailable :backend backend-id :root root})))))

(defonce ^:private discard-executor
  ;; Single daemon thread: serializes physical clone reclamation OFF the request
  ;; thread so `/draft abandon` never blocks the UI on file deletion, and keeps
  ;; the global `rift/gc` from running concurrently with a fork.
  (delay (java.util.concurrent.Executors/newSingleThreadExecutor
           (reify
             java.util.concurrent.ThreadFactory
               (newThread [_ r]
                 (doto (Thread. ^Runnable r "vis-workspace-discard") (.setDaemon true)))))))

(defn- discard-roots-async!
  "Physically release each `{:backend :root}` off the request thread. Returns a
   Future callers/tests can await; the DB `:discarded` transition has already
   happened synchronously, so the UI never waits on deletion. Best-effort: a
   failed reclamation is swallowed rather than resurrecting the abandoned row."
  [roots]
  (.submit ^java.util.concurrent.ExecutorService @discard-executor
           ^Callable
           (fn []
             (doseq
               [{:keys [backend root]}
                roots

                :when root]

               (try (discard-root! backend root) (catch Throwable _ nil))))))

;; =============================================================================
;; Since-fork diff — pure mtime, git-free
;; =============================================================================

(def ^:private prune-dir-names
  "Directory names pruned from the since-fork diff AT ANY DEPTH: VCS internals
   plus build/dependency/editor caches that churn on mtime but are never
   meaningful agent edits and are gitignored anyway. Landing them into trunk
   is always wrong, and — because a cache like `.clj-kondo/.cache` or `target`
   holds thousands of files the JVM/clj-kondo rewrites on startup — letting
   them into `changed-paths` bloats `changed_files` (and any sub_loop result
   built from it) enough to overflow the model context."
  #{".git" ".rift" ".trash" ".cpcache" ".lsp" ".lsp-cache" "target" "node_modules" ".shadow-cljs"
    ".cljs_node_repl" ".gitlibs" ".gradle" ".idea"})

(defn- prune-dir?
  "True when the clone-relative path `rel` lies in a VCS/build/cache subtree the
   diff must skip: ANY segment named in `prune-dir-names`, or a `.clj-kondo/.cache`
   subtree (we keep the tracked `.clj-kondo/config.edn`, prune only the churny
   cache).

   Depth matters — a monorepo keeps its churn NESTED (`apps/web/node_modules`,
   `extensions/<ext>/target`, a generated native project's own `.git`). Matching
   only segment 0 reported all of that as agent edits and landed it into trunk."
  [^Path rel]
  (let [c (.getNameCount rel)]
    (loop [i 0]
      (if (>= i c)
        false
        (let [s (str (.getName rel i))]
          (if (or (contains? prune-dir-names s)
                  (and (= ".clj-kondo" s) (< (inc i) c) (= ".cache" (str (.getName rel (inc i))))))
            true
            (recur (inc i))))))))

(defn changed-paths
  "Repo-relative paths of files under `clone` whose mtime is newer than
   `fork-ms` — i.e. exactly what the agent touched since the fork
   (`clonefile` preserves source mtimes, so untouched files stay older).
   Prunes VCS/build/cache dirs (`prune-dir?`) — landing `.git/` would corrupt
   trunk's repo, and tool caches would flood the result. Returns a vec
   of strings."
  [clone fork-ms]
  (let
    [root
     (.toPath (io/file clone))

     acc
     (java.util.ArrayList.)]

    (Files/walkFileTree root
                        (proxy [SimpleFileVisitor] []
                          (preVisitDirectory [dir ^BasicFileAttributes _a]
                            (if (prune-dir? (.relativize root ^Path dir))
                              FileVisitResult/SKIP_SUBTREE
                              FileVisitResult/CONTINUE))
                          (visitFile [file ^BasicFileAttributes attrs]
                            (let [rel (.relativize root ^Path file)]
                              (when (and (not (prune-dir? rel))
                                         (> (.toMillis (.lastModifiedTime attrs)) (long fork-ms)))
                                ;; Repo-relative DISPLAY paths are `/`-separated on every OS.
                                (.add acc (paths/unixify rel))))
                            FileVisitResult/CONTINUE)
                          (visitFileFailed [_file _exc] FileVisitResult/CONTINUE)))
    (vec acc)))

(defn- fork-ms-of [ws] (:fork-ms ws))

(defn- apply-fork-ms-of [ws] (or (:apply-fork-ms ws) (fork-ms-of ws)))

(defn deleted-paths
  "Repo-relative paths the agent DELETED in the draft: present under
   `trunk` (pruning VCS/build/cache dirs via `prune-dir?`) with an mtime
   older than `fork-ms` — so they existed at the fork and are not user
   post-fork additions — yet absent from `clone`. The mtime guard means
   `apply!` never reverts a file the user added to cwd after forking, and
   the prune keeps cache churn (e.g. `.clj-kondo/.cache` rewritten in the
   clone) from being reported as spurious deletions.

   A non-positive `fork-ms` is the FRESH-lineage baseline: the clone never
   saw trunk's files, so a deletion is semantically impossible — return []
   unconditionally, whatever the trees contain. The hard exit (rather than
   relying on the `<` comparison alone) also keeps pathological trunk
   mtimes (epoch/pre-epoch, e.g. unpacked from a tarball) from ever
   mtimes (epoch/pre-epoch, e.g. unpacked from a tarball) from ever
   comparing below the baseline into a real `.delete` of the user's files.

   Paths a CLEAN seed dropped (`clean-seed-skips`) are excluded too: the
   clone never received the user's UNCOMMITTED trunk files, so their absence
   is the SEED's doing and applying it must not delete them from trunk."
  [clone trunk fork-ms]
  (if-not (pos? (long fork-ms))
    []
    (let
      [troot
       (.toPath (io/file trunk))

       croot
       (.toPath (io/file clone))

       nofollow
       (into-array LinkOption [LinkOption/NOFOLLOW_LINKS])

       skips
       ;; Absent from the clone BY CONSTRUCTION, never by the agent.
       (clean-seed-skips clone)

       acc
       (java.util.ArrayList.)]

      (Files/walkFileTree troot
                          (proxy [SimpleFileVisitor] []
                            (preVisitDirectory [dir ^BasicFileAttributes _a]
                              (if (prune-dir? (.relativize troot ^Path dir))
                                FileVisitResult/SKIP_SUBTREE
                                FileVisitResult/CONTINUE))
                            (visitFile [file ^BasicFileAttributes attrs]
                              (let
                                [rel
                                 (.relativize troot ^Path file)

                                 ;; Repo-relative DISPLAY paths are `/`-separated on every OS.
                                 rel-str
                                 (paths/unixify rel)]

                                (when (and (not (prune-dir? rel))
                                           (not (contains? skips rel-str))
                                           (< (.toMillis (.lastModifiedTime attrs)) (long fork-ms))
                                           (not (Files/exists (.resolve croot rel) nofollow)))
                                  (.add acc rel-str)))
                              FileVisitResult/CONTINUE)
                            (visitFileFailed [_file _exc] FileVisitResult/CONTINUE)))
      (vec acc))))

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
    (try (apply f args) (catch Throwable _ nil))))

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
  (let
    [trimmed (some-> label
                     str
                     str/trim
                     not-empty)]
    (p/db-workspace-update-label! db-info workspace-id trimmed)))

;; `draft?` / `free-draft-name` are defined further down (Mutations); the
;; filesystem-root autoclone path needs them here.
(declare draft? free-workspace-name)

(declare abandon!)

(defn subdirs
  "Child directory names (non-hidden) of `path`, case-insensitively sorted.
   Empty vec when `path` is blank, not a directory, or unreadable."
  [path]
  (let
    [dir (some-> path
                 normalize-root
                 io/file)]
    (->> (when (and dir (.isDirectory ^File dir)) (.listFiles ^File dir))
         (filter (fn [^File f]
                   (and (.isDirectory f) (not (.isHidden f)))))
         (map (fn [^File f]
                (.getName f)))
         (sort String/CASE_INSENSITIVE_ORDER)
         vec)))

(defn create-dir!
  "Create a single child directory `name` under existing directory `parent`.
   Returns the canonical path of the (possibly already-existing) child. Throws
   when `parent` is not a directory or `name` is not a single safe path segment.
   `name` may not contain a separator, be blank, or be `.`/`..`."
  [parent name]
  (let
    [base
     (some-> parent
             normalize-root
             io/file)

     seg
     (some-> name
             str
             str/trim)]

    (cond (or (nil? base) (not (.isDirectory ^File base)))
          (throw (ex-info (str "Not a directory: " parent)
                          {:type :workspace/not-a-directory :path parent}))
          (or (str/blank? seg)
              (= seg ".")
              (= seg "..")
              (str/includes? seg "/")
              (str/includes? seg "\\"))
          (throw (ex-info (str "Invalid folder name: " name)
                          {:type :workspace/invalid-name :name name}))
          :else (let [child (io/file base seg)]
                  (when (and (not (.exists child)) (not (.mkdir child)))
                    (throw (ex-info (str "Could not create folder: " seg)
                                    {:type :workspace/mkdir-failed :name name})))
                  (.getCanonicalPath child)))))

(defn focus!
  "Stamp `last_focused_at_ms` and upsert the per-repo `repo_focus`
   pointer. Returns the updated workspace record."
  [db-info workspace-id]
  (when-let [ws (p/db-workspace-touch-focus! db-info workspace-id)]
    (when (:repo-id ws) (p/db-repo-focus-set! db-info (:repo-id ws) workspace-id))
    ws))

(defn last-focused
  "Workspace id from `repo_focus` for `repo-id`, or nil."
  [db-info repo-id]
  (some-> (p/db-repo-focus-get db-info repo-id)
          :workspace-id))

(defn display-label
  "Human-facing label for `workspace`. Order: explicit `:label` →
   pinned session title → clone name (`:branch`) → id prefix."
  ([workspace] (display-label nil workspace nil))
  ([db-info workspace session]
   (let
     [hydrated (or session
                   (when (and db-info (:id workspace))
                     (some->> (:id workspace)
                              (p/db-session-state-list-for-workspace db-info)
                              first)))]
     (or (some-> (:label workspace)
                 str/trim
                 not-empty)
         (some-> hydrated
                 :title
                 str/trim
                 not-empty)
         (some-> (:id workspace)
                 str
                 (subs 0 (min 8 (count (str (:id workspace))))))))))

(defn workspace-with-session
  "Hydrate `workspace-id` with its pinned `session_state`. Returns
   `{:workspace <ws> :session-state <ss>}`."
  [db-info workspace-id]
  (when-let [ws (p/db-workspace-get db-info workspace-id)]
    {:workspace ws
     :session-state (some->> workspace-id
                             (p/db-session-state-list-for-workspace db-info)
                             first)}))

(defn list-active-with-sessions
  "Like `list-active` but each entry is the `{:workspace :session-state}`
   pair, sorted by `last_focused_at_ms` DESC NULLS LAST, then
   `created_at` DESC."
  [db-info repo-id]
  (let
    [rows
     (list-active db-info repo-id)

     cmp
     (fn [a b]
       (let
         [recency-of
          #(or (:last-focused-at-ms %) Long/MIN_VALUE)

          ra
          (recency-of a)

          rb
          (recency-of b)]

         (cond (not= ra rb) (compare rb ra)
               :else (compare (str (:created-at b)) (str (:created-at a))))))]

    (mapv (fn [ws]
            (let [pair (workspace-with-session db-info (:id ws))]
              (assoc pair :workspace ws)))
          (sort cmp rows))))

(defn status
  "Enrich a workspace record with live status. Stamps
   `:workspace/root`, `:workspace/sandbox?`, `:workspace/exists?`, `:workspace/changed`
   (count of since-fork edits) and `:workspace/dirty?`. No git."
  [db-info workspace-id]
  (when-let [ws (get db-info workspace-id)]
    (let
      [root (:root ws)
       fork-ms (apply-fork-ms-of ws)]

      (try (let
             [exists? (.exists (io/file root))
              changed (when (and exists? fork-ms) (count (changed-paths root fork-ms)))]

             (assoc ws
               :workspace/root root
               :workspace/sandbox? (not= :live (:workspace-backend ws))
               :workspace/exists? exists?
               :workspace/changed (or changed 0)
               :workspace/dirty? (boolean (and changed (pos? (long changed))))
               ;; Sandbox-ness is independent from VCS identity. The real
               ;; :vcs/kind is computed model-side in foundation.workspace-ctx.
               ;; Back-compat alias for channels still reading `:vcs/dirty?`.
               :vcs/dirty? (boolean (and changed (pos? (long changed))))))
           (catch Throwable t
             (assoc ws
               :workspace/exists? (.exists (io/file root))
               :workspace/error (or (ex-message t) (str t))))))))

(defn trunk-info
  "The user's real cwd (trunk). No git read; just the launch dir."
  ([] (trunk-info nil))
  ([root]
   {:repo-root (or (some-> root
                           file-path)
                   (trunk-root))}))

;; =============================================================================
;; Mutations
;; =============================================================================

(defn draft?
  "True when a workspace carries an apply baseline and therefore represents
   isolated filesystem state rather than a logical-only graph revision."
  [ws]
  (some? (fork-ms-of ws)))

(defn- workspace-dir
  "Conventional backend storage path for `name` under `trunk`."
  ^java.io.File [trunk name]
  (io/file (draft-store-root trunk) name))

(defn- free-workspace-name
  "Workspace name derived from `label`, with a numeric collision suffix."
  [trunk label]
  (let [base (sanitize-id (or label "draft"))]
    (loop
      [n base
       i 2]

      (if (.exists (workspace-dir trunk n)) (recur (str base "-" i) (inc i)) n))))

(defn- insert-trunk!
  "Insert a fresh TRUNK workspace row (root = repo_root = `root`, defaulting
   to the real cwd; no clone, no fork_ms) and pin it to `session-state-id`
   when given."
  ([db-info session-state-id] (insert-trunk! db-info session-state-id (trunk-root)))
  ([db-info session-state-id root]
   (let
     [trunk
      (file-path root)

      ws
      (p/db-workspace-insert! db-info
                              {:repo-id (repo-id-for trunk)
                               :repo-root trunk
                               :root trunk
                               :workspace-kind :trunk
                               :workspace-backend :live
                               :state :active})]

     (when session-state-id (p/db-session-state-set-workspace! db-info session-state-id (:id ws)))
     ws)))

(defn ensure-workspace!
  "Find-or-create the session's workspace. The DEFAULT is TRUNK — the
   user's real cwd (no clone); the agent works directly in the repo
   until `/draft new`. Resume returns whatever the session was pinned to
   (trunk, or an open draft). Idempotent per session-state."
  [db-info {:keys [session-state-id]}]
  (or (for-session db-info session-state-id) (insert-trunk! db-info session-state-id)))

(defn create-trunk-at!
  "Mint a TRUNK workspace rooted at `root` (an arbitrary directory), not
   pinned to any session. Lets a channel open a session under a directory
   OTHER than the one vis was launched from — a tab in another project.
   Returns the workspace row (with `:id`) to pass as `:workspace-id` when
   creating the session."
  [db-info root]
  (insert-trunk! db-info nil (file-path root)))

(defn change-root!
  "Repoint `session-state-id`'s primary workspace root to `path`. Refuses while
   the session is in a draft, which must be applied or abandoned first."
  [db-info session-state-id path]
  (let
    [canon
     (normalize-root path)

     dir
     (some-> canon
             io/file)]

    (when-not canon (throw (ex-info "Path is blank" {:type :workspace/blank-path :path path})))
    (when-not (.isDirectory ^File dir)
      (throw (ex-info (str "Not a directory: " path)
                      {:type :workspace/not-a-directory :path path})))
    (let [current (for-session db-info session-state-id)]
      (when (draft? current)
        (throw (ex-info
                 "Session is in a draft — /draft apply or /draft abandon before changing the root"
                 {:type :workspace/root-change-in-draft :workspace-id (:id current)})))
      (if (= canon
             (some-> (:root current)
                     normalize-root))
        current
        (insert-trunk! db-info session-state-id canon)))))

(defn- fresh-seed-root
  "Empty, reusable fork SOURCE for FRESH drafts of `trunk`: cloning it yields
   a draft that starts with NO files. Lives inside the trunk's draft store so
   the backend's availability probe/fork see the same filesystem, and stays
   around (a tiny empty dir) so backend bookkeeping never dangles on a
   deleted source."
  ^File [trunk]
  (doto (io/file (draft-store-root trunk) ".fresh-seed") (.mkdirs)))

(defn create!
  "Create an isolated DRAFT using the strongest available backend and pin it
   to `:session-state-id`. The backend must provide the full draft capability
   set; core never silently falls back to a shared root.

   The fork PARENT is chosen so `apply!` lands back where it forked from:
   pass `:from <parent-workspace>` to clone that workspace's `:root` and
   inherit its `:repo-root` (apply target); otherwise the parent is the
   user's real cwd (trunk).

   `:fresh? true` forks from an EMPTY seed instead: the draft starts with
   NONE of the files currently in trunk (HEAD). Its baseline `:fork-ms` is 0,
   so every file created inside the draft counts as a since-fork edit and
   lands on `apply!`, while `deleted-paths` (trunk files OLDER than the
   baseline yet absent from the clone) can never match — a fresh draft only
   ever ADDS/OVERWRITES into trunk, it cannot infer deletions of files it
   never saw. The ZERO baseline is HEREDITARY: a draft forked `:from` a
   fresh-lineage workspace (a sub-loop child, a revision) keeps baseline 0
   too — its clone still lacks the trunk files the lineage never saw, so
   applying it with a real fork timestamp would mass-report trunk's files
   never saw. The ZERO baseline is HEREDITARY: a draft forked `:from` a
   fresh-lineage workspace (a sub-loop child, a revision) keeps baseline 0
   too — its clone still lacks the trunk files the lineage never saw, so
   applying it with a real fork timestamp would mass-report trunk's files
   as deletions and wipe the repo.

   `:clean? true` forks the REAL tree and then scrubs the clone back to the
   committed HEAD: every tracked file is present at its committed content and
   the user's uncommitted work is left behind in trunk. The baseline is a
   real timestamp captured AFTER the scrub, so the revert itself is not read
   as agent edits, and the dropped paths are recorded so `deleted-paths`
   never mistakes them for deletions. Requires a repository with a commit;
   throws `:workspace/clean-seed-unavailable` otherwise."
  [db-info {:keys [session-state-id label from required-capabilities blank? clean?]}]
  (let
    [trunk
     (or (:repo-root from) (trunk-root))

     _
     (when (and clean? (not (committed-head? trunk)))
       (throw (ex-info "This project has no commit to start a clean draft from"
                       {:type :workspace/clean-seed-unavailable :root (file-path trunk)})))

     parent
     (if blank? (fresh-seed-root trunk) (or (:root from) (trunk-root)))

     rid
     (repo-id-for trunk)

     nm
     (free-workspace-name trunk label)

     {:keys [root backend]}
     (backend-fork! parent trunk nm (or required-capabilities draft-required-capabilities))

     ;; BEFORE the baseline below: the revert rewrites mtimes, and a clone
     ;; that cannot be scrubbed must never survive as a half-seeded draft.
     _
     (when clean?
       (try
         (clean-seed! root)
         (catch Throwable t (try (discard-root! backend root) (catch Throwable _ nil)) (throw t))))

     ;; Capture AFTER the clone returns: cloned files keep their (older)
     ;; source mtime, so only post-fork agent edits exceed this. A FRESH
     ;; LINEAGE instead anchors at 0 — the lineage never saw trunk's files,
     ;; so everything that ever appears in the clone is an agent edit and
     ;; no trunk file can predate the baseline into a spurious deletion.
     ;; Zero-heredity applies only to a real non-trunk parent (its clone
     ;; lacks trunk's files); a nil `from` / trunk parent forks the REAL
     ;; tree and must get a real timestamp or deletions can never land.
     inherited-fresh?
     (boolean (and from
                   (not= (:root from) (:repo-root from))
                   (zero? (long (or (apply-fork-ms-of from) 0)))))

     fork-ms
     (if (or blank? inherited-fresh?) 0 (System/currentTimeMillis))

     ws
     (p/db-workspace-insert! db-info
                             {:repo-id rid
                              :repo-root trunk
                              :root root
                              :workspace-kind :draft
                              :workspace-backend backend
                              :parent-workspace-id (:id from)
                              :state :active
                              :fork-ms fork-ms
                              ;; Drafts apply from their immediate fork; apply-fork-ms
                              ;; equals fork-ms so apply! reads one baseline uniformly.
                              :apply-fork-ms fork-ms})

     ;; Label = the actual folder name, including collision suffixes.
     ws
     (or (p/db-workspace-update-label! db-info (:id ws) nm) ws)]

    (when session-state-id (p/db-session-state-set-workspace! db-info session-state-id (:id ws)))
    (fire-hook! :on-spawn ws)
    ws))

(defn exit-to-trunk!
  "Repoint `session-state-id` back to a TRUNK workspace (the real cwd),
   leaving any draft. Returns the trunk workspace now pinned. The three-argument
   arity preserves an explicit repo root when the session was opened away from
   the process launch directory."
  ([db-info session-state-id] (insert-trunk! db-info session-state-id))
  ([db-info session-state-id root] (insert-trunk! db-info session-state-id root)))

(defn list-drafts
  "Active DRAFTS for `repo-id`, newest first — the parked/stashable drafts a
   session can `resume!`. Trunk workspaces are filtered out. A draft that a
   session leaves via `stash!` stays `:active` and keeps appearing here until it
   is applied or abandoned."
  [db-info repo-id]
  (filterv draft? (list-active db-info repo-id)))

(defn stash!
  "Park the session's current draft: repoint `session-state-id` back to trunk
   while LEAVING the draft row `:active` and its clone on disk intact, so it can
   be resumed later. This is the non-destructive twin of `abandon!` — nothing is
   discarded. Returns `{:draft <stashed-draft-or-nil> :trunk <trunk-ws>}`; on
   trunk `:draft` is nil and this just ensures a trunk pin. The draft's own
   `:repo-root` is preserved for sessions opened away from the launch directory."
  [db-info session-state-id]
  (let
    [current
     (for-session db-info session-state-id)

     stashed
     (when (draft? current) current)

     trunk-root
     (or (:repo-root current) (:root current) (trunk-root))

     trunk
     (exit-to-trunk! db-info session-state-id trunk-root)]

    {:draft stashed :trunk trunk}))

(defn resume!
  "Re-enter a previously stashed DRAFT: pin `session-state-id` to `workspace-id`,
   which MUST be an `:active` draft from the session's current repo and not
   currently pinned to another session. The session must already have left any
   draft (be on trunk) — a stash/apply/abandon precedes a resume. Returns the
   draft workspace now pinned. Throws `ex-info` with a `:type` on any precondition
   failure."
  [db-info {:keys [session-state-id workspace-id]}]
  (let
    [ws
     (get db-info workspace-id)

     current
     (for-session db-info session-state-id)]

    (when-not (draft? ws)
      (throw (ex-info "Not a resumable draft"
                      {:type :workspace/not-a-draft :workspace-id workspace-id})))
    (when (not= :active (:state ws))
      (throw (ex-info "Draft is no longer active"
                      {:type :workspace/draft-inactive :workspace-id workspace-id})))
    (when (and current (not= (:repo-id current) (:repo-id ws)))
      (throw (ex-info "Draft belongs to a different repository"
                      {:type :workspace/draft-repo-mismatch
                       :workspace-id workspace-id
                       :repo-id (:repo-id current)
                       :draft-repo-id (:repo-id ws)})))
    (let
      [pinned-elsewhere (remove #(= (str session-state-id) (str (:id %)))
                          (p/db-session-state-list-for-workspace db-info workspace-id))]
      (when (seq pinned-elsewhere)
        (throw (ex-info "Draft is in use by another session"
                        {:type :workspace/draft-in-use :workspace-id workspace-id}))))
    (p/db-session-state-set-workspace! db-info session-state-id workspace-id)
    ws))

(defn- land-clone!
  "Copy one clone tree's since-fork edits + deletions into its `trunk`,
   tagging each change with the `trunk` it landed under (so a multi-root
   apply is unambiguous). Returns a vec of `{:status :path :root}`."
  [clone trunk fork-ms]
  (let
    [edits
     (mapv (fn [path]
             (let
               [src
                (io/file clone path)

                dst
                (io/file trunk path)

                status
                (if (.exists dst) :modify :add)]

               (io/make-parents dst)
               (Files/copy (.toPath src) (.toPath dst) copy-opts)
               {:status status :path path :root trunk}))
           (changed-paths clone fork-ms))

     deletes
     (mapv (fn [path]
             (.delete (io/file trunk path))
             {:status :delete :path path :root trunk})
           (deleted-paths clone trunk fork-ms))]

    (into edits deletes)))

(defn apply!
  "Land a draft's primary clone changes into its real workspace root."
  [db-info {:keys [workspace-id]}]
  (let [ws (get db-info workspace-id)]
    (when-not ws (throw (ex-info "Unknown workspace" {:workspace-id workspace-id})))
    (let [fork-ms (apply-fork-ms-of ws)]
      (when-not fork-ms
        (throw (ex-info "Workspace has no fork timestamp; cannot apply"
                        {:type :workspace/no-baseline :workspace-id workspace-id})))
      (let [changes (vec (land-clone! (:root ws) (:repo-root ws) fork-ms))]
        (fire-hook! :on-apply ws {:changed changes})
        {:status :ok :changed changes :landed (count changes) :workspace ws}))))

(defn abandon-lineage!
  "Discard `workspace-id` and each draft ancestor up to (but never including)
   trunk. Used when an operator applies or abandons the session draft. A plain
   draft's parent is its trunk, so this normally abandons the single draft."
  [db-info {:keys [workspace-id reason]}]
  (loop
    [ws
     (get db-info workspace-id)

     discarded
     []]

    (if-not (draft? ws)
      {:status :discarded :workspace-ids discarded}
      (let
        [parent-id
         (:parent-workspace-id ws)

         done
         (abandon! db-info {:workspace-id (:id ws) :reason reason})]

        (recur (some->> parent-id
                        (get db-info))
               (conj discarded (:id done)))))))

(defn abandon!
  "Transition a workspace to :discarded and release its primary backend-owned clone."
  [db-info {:keys [workspace-id reason]}]
  (let [ws (get db-info workspace-id)]
    (when-not ws (throw (ex-info "Unknown workspace" {:workspace-id workspace-id})))
    (let
      [done (p/db-workspace-update-state! db-info workspace-id :discarded)
       fut (discard-roots-async! [{:backend (:workspace-backend ws) :root (:root ws)}])]

      (fire-hook! :on-discard done {:reason reason})
      (assoc (or done ws)
        :reason reason
        :discard-future fut))))

(defn discard-session-clones!
  "On session DELETE, release primary backend-owned clones in its lineage."
  [db-info session-soul-id]
  (when (and db-info session-soul-id)
    (when-let [state-id (p/db-latest-session-state-id db-info session-soul-id)]
      (let
        [roots (loop
                 [ws (for-session db-info state-id)
                  acc []]

                 (if-not ws
                   acc
                   (recur (some-> (:parent-workspace-id ws)
                                  (get db-info))
                          (conj acc {:backend (:workspace-backend ws) :root (:root ws)}))))]
        (discard-roots-async! roots)))))
