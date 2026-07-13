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
  "Extra filesystem roots the current tool call may ALSO operate under, beyond the
   primary `*workspace-root*`, as `[{:trunk :clone}]` canonical pairs: `:trunk`
   is the REAL directory the user added (what the model addresses), `:clone` is
   the backend working copy edits land in (== `:trunk` when live). Bound
   per-turn by the channel layer from the session's persisted filesystem roots.
   Empty in the common single-root case. The editing layer confines to the
   clones and transparently remaps trunk↔clone (see `filesystem-root-mappings`)."
  nil)

(defn normalize-root
  "Canonicalize a workspace root string/File. Blank/nil → nil."
  [root]
  (let [s (some-> root
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
    (let [t
          (normalize-root (:trunk e))

          c
          (normalize-root (:clone e))]

      (when t
        {:trunk t :clone (or c t) :fork-ms (:fork-ms e) :backend (backend-id (:backend e))}))))

(defn env-filesystem-roots
  "Canonical `[{:trunk :clone}]` pairs for the current tool call's extra
   filesystem roots, beyond the primary. Reads `:workspace/filesystem-roots` from an
   env map (or a raw coll). `:trunk` is the real dir the model addresses;
   `:clone` is the backend working copy edits land in (== trunk when live).
   The channel layer binds `*filesystem-roots*` from this per turn; the editing
   layer confines to the clones and transparently remaps trunk↔clone."
  [env-or-roots]
  (let [roots (if (map? env-or-roots) (:workspace/filesystem-roots env-or-roots) env-or-roots)]
    (vec (keep (fn [e]
                 (when-let [{:keys [trunk clone]} (root-entry e)]
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
   operate under: the primary cwd FIRST, then each bound filesystem root's
   `:clone`. Deduped; the primary is always present. The confinement set the
   editing layer's `safe-path` checks the (possibly remapped) target against."
  []
  (let [primary
        (.getCanonicalPath (cwd))

        extra
        (keep #(some-> (:clone %)
                       normalize-root)
              *filesystem-roots*)]

    (vec (distinct (cons primary extra)))))

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
  (let [root
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
  (let [id
        (:workspace.backend/id backend)

        caps
        (:workspace.backend/capabilities backend)]

    (when-not (keyword? id)
      (throw (ex-info "Workspace backend id must be a keyword"
                      {:type :workspace/invalid-backend :backend backend})))
    (when-not (and (set? caps) (every? workspace-capabilities caps))
      (throw (ex-info "Workspace backend has invalid capabilities"
                      {:type :workspace/invalid-backend :backend-id id :capabilities caps})))
    (doseq [k [:workspace.backend/available-fn :workspace.backend/fork-fn
               :workspace.backend/discard-fn]]
      (when-not (ifn? (clojure.core/get backend k))
        (throw (ex-info (str "Workspace backend requires " k)
                        {:type :workspace/invalid-backend :backend-id id :key k}))))
    (update backend :workspace.backend/priority #(long (or % 0)))))

(defn register-backend!
  "Register a workspace backend. Idempotent by backend id."
  [backend]
  (let [backend
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
           (let [availability
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
  (let [required
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
  (let [source-root
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

(def ^:private copy-opts
  ^"[Ljava.nio.file.CopyOption;"
  (into-array CopyOption
              [StandardCopyOption/REPLACE_EXISTING StandardCopyOption/COPY_ATTRIBUTES
               LinkOption/NOFOLLOW_LINKS]))

(defn- backend-fork!
  [source-root store-root name required]
  (let [store-root
        (draft-store-root store-root)

        backend
        (select-backend source-root store-root required)]

    (when-not backend
      (throw (ex-info "No workspace backend provides the required capabilities"
                      {:type :workspace/capability-unavailable
                       :required (set required)
                       :source-root (file-path source-root)
                       :capability-matrix (capability-matrix source-root store-root)})))
    (let [root
          ((:workspace.backend/fork-fn backend)
            {:source-root (file-path source-root) :store-root (file-path store-root) :name name})]
      {:root (file-path root) :backend (:workspace.backend/id backend)})))

(defn- discard-root!
  [backend-id root]
  (when (and root (not= :live backend-id))
    (if-let [backend (clojure.core/get @backend-registry backend-id)]
      ((:workspace.backend/discard-fn backend) {:root (file-path root)})
      (throw (ex-info "Workspace backend is not registered"
                      {:type :workspace/backend-unavailable :backend backend-id :root root})))))

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
  #{".git" ".rift" ".trash" ".cpcache" ".lsp" ".lsp-cache" "target" "node_modules" ".shadow-cljs"
    ".cljs_node_repl" ".gitlibs" ".gradle" ".idea"})

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
               (and (= ".clj-kondo" s0) (>= c 2) (= ".cache" (str (.getName rel 1)))))))))

(defn changed-paths
  "Repo-relative paths of files under `clone` whose mtime is newer than
   `fork-ms` — i.e. exactly what the agent touched since the fork
   (`clonefile` preserves source mtimes, so untouched files stay older).
   Prunes VCS/build/cache dirs (`prune-dir?`) — landing `.git/` would corrupt
   trunk's repo, and tool caches would flood the result. Returns a vec
   of strings."
  [clone fork-ms]
  (let [root
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
   comparing below the baseline into a real `.delete` of the user's files."
  [clone trunk fork-ms]
  (if-not (pos? (long fork-ms))
    []
    (let [troot
          (.toPath (io/file trunk))

          croot
          (.toPath (io/file clone))

          nofollow
          (into-array LinkOption [LinkOption/NOFOLLOW_LINKS])

          acc
          (java.util.ArrayList.)]

      (Files/walkFileTree troot
                          (proxy [SimpleFileVisitor] []
                            (preVisitDirectory [dir ^BasicFileAttributes _a]
                              (if (prune-dir? (.relativize troot ^Path dir))
                                FileVisitResult/SKIP_SUBTREE
                                FileVisitResult/CONTINUE))
                            (visitFile [file ^BasicFileAttributes attrs]
                              (let [rel (.relativize troot ^Path file)]
                                (when (and (not (prune-dir? rel))
                                           (< (.toMillis (.lastModifiedTime attrs)) (long fork-ms))
                                           (not (Files/exists (.resolve croot rel) nofollow)))
                                  ;; Repo-relative DISPLAY paths are `/`-separated on every OS.
                                  (.add acc (paths/unixify rel))))
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
  (let [trimmed (some-> label
                        str
                        str/trim
                        not-empty)]
    (p/db-workspace-update-label! db-info workspace-id trimmed)))

;; `draft?` / `free-draft-name` are defined further down (Mutations); the
;; filesystem-root autoclone path needs them here.
(declare draft? free-workspace-name)
(declare abandon!)

(defn filesystem-roots
  "Extra filesystem roots configured for `ws`, normalized to
   `[{:trunk :clone :fork-ms :backend}]`."
  [ws]
  (vec (keep root-entry (:filesystem-roots ws))))

(defn add-filesystem-root!
  "Add `path` to the workspace's extra filesystem roots. Drafts require the same
   isolation capabilities for every added root; live workspaces add it live."
  [db-info workspace-id path]
  (when-let [ws (get db-info workspace-id)]
    (let [canon (normalize-root path)
          dir (some-> canon
                      io/file)
          roots (filesystem-roots ws)]

      (cond (nil? canon) (throw (ex-info "Path is blank" {:type :workspace/blank-path :path path}))
            (not (.isDirectory ^File dir)) (throw (ex-info (str "Not a directory: " path)
                                                           {:type :workspace/not-a-directory
                                                            :path path}))
            (some #(= canon (:trunk %)) roots) ws ;; idempotent — already a filesystem root
            :else
            (let [entry (if (draft? ws)
                          (let [nm (free-workspace-name canon "ctx")
                                {:keys [root backend]}
                                (backend-fork! canon canon nm draft-required-capabilities)]

                            {:trunk canon
                             :clone root
                             :fork-ms (System/currentTimeMillis)
                             :backend backend})
                          {:trunk canon :clone canon :fork-ms nil :backend :live})]
              (p/db-workspace-set-filesystem-roots! db-info workspace-id (conj roots entry)))))))

(defn remove-filesystem-root!
  "Remove `path` from the workspace's extra filesystem roots and release any
   backend-owned isolated root."
  [db-info workspace-id path]
  (when-let [ws (get db-info workspace-id)]
    (let [canon (normalize-root path)
          roots (filesystem-roots ws)
          gone (some #(when (= canon (:trunk %)) %) roots)]

      (when (and gone (:clone gone) (not= (:clone gone) (:trunk gone)))
        (try (discard-root! (:backend gone) (:clone gone)) (catch Throwable _ nil)))
      (p/db-workspace-set-filesystem-roots! db-info
                                            workspace-id
                                            (vec (remove #(= canon (:trunk %)) roots))))))

(defn subdirs
  "Child directory names (non-hidden) of `path`, case-insensitively sorted.
   Empty vec when `path` is blank, not a directory, or unreadable."
  [path]
  (let [dir (some-> path
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
  (let [base
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
   (let [hydrated (or session
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
  (let [rows
        (list-active db-info repo-id)

        cmp
        (fn [a b]
          (let [recency-of
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
    (let [root (:root ws)
          fork-ms (apply-fork-ms-of ws)]

      (try (let [exists? (.exists (io/file root))
                 changed (when (and exists? fork-ms) (count (changed-paths root fork-ms)))]

             (assoc ws
               :workspace/root root
               :workspace/sandbox? (not= :live (:workspace-backend ws))
               :workspace/exists? exists?
               :workspace/changed (or changed 0)
               :workspace/dirty? (boolean (and changed (pos? changed)))
               ;; Sandbox-ness is independent from VCS identity. The real
               ;; :vcs/kind is computed model-side in foundation.workspace-ctx.
               ;; Back-compat alias for channels still reading `:vcs/dirty?`.
               :vcs/dirty? (boolean (and changed (pos? changed)))))
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
  [trunk name]
  (io/file (draft-store-root trunk) name))

(defn- free-workspace-name
  "Workspace name derived from `label`, with a numeric collision suffix."
  [trunk label]
  (let [base (sanitize-id (or label "draft"))]
    (loop [n base
           i 2]

      (if (.exists (workspace-dir trunk n)) (recur (str base "-" i) (inc i)) n))))

(defn- insert-trunk!
  "Insert a fresh TRUNK workspace row (root = repo_root = `root`, defaulting
   to the real cwd; no clone, no fork_ms) and pin it to `session-state-id`
   when given."
  ([db-info session-state-id] (insert-trunk! db-info session-state-id (trunk-root)))
  ([db-info session-state-id root]
   (let [trunk
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
  "Repoint `session-state-id`'s PRIMARY filesystem root to `path` — the
   session now works in a different project directory: shell cwd, relative
   path resolution, file tools (cat/patch/write), and search (rg/find_files)
   all follow from the next turn, because they resolve through the session's
   pinned workspace. Additional filesystem roots CARRY OVER (they are
   session-scoped permissions, not root-relative); an entry equal to the new
   root is dropped as redundant. Refuses while the session is in a draft —
   a draft is a fork of the OLD root, so apply/abandon it first. Returns the
   newly pinned trunk workspace (or the current one when `path` already IS
   the root)."
  [db-info session-state-id path]
  (let [canon
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
        (let [carried (vec (remove #(= canon (:trunk %)) (filesystem-roots current)))
              ws (insert-trunk! db-info session-state-id canon)]

          (if (seq carried)
            (or (p/db-workspace-set-filesystem-roots! db-info (:id ws) carried) ws)
            ws))))))

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
   as deletions and wipe the repo."
  [db-info {:keys [session-state-id label from required-capabilities fresh?]}]
  (let [trunk
        (or (:repo-root from) (trunk-root))

        parent
        (if fresh? (fresh-seed-root trunk) (or (:root from) (trunk-root)))

        rid
        (repo-id-for trunk)

        nm
        (free-workspace-name trunk label)

        {:keys [root backend]}
        (backend-fork! parent trunk nm (or required-capabilities draft-required-capabilities))

        ;; Capture AFTER the clone returns: cloned files keep their (older)
        ;; source mtime, so only post-fork agent edits exceed this. A FRESH
        ;; LINEAGE (this draft, or any ancestor the clone descends from)
        ;; instead anchors at 0 — the lineage never saw trunk's files, so
        ;; everything that ever appears in the clone is an agent edit and no
        ;; trunk file can predate the baseline into a spurious deletion.
        fork-ms
        (if (or fresh?
                (some-> (apply-fork-ms-of from)
                        long
                        zero?))
          0
          (System/currentTimeMillis))

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
   leaving any draft. Returns the trunk workspace now pinned."
  [db-info session-state-id]
  (insert-trunk! db-info session-state-id))

(defn- land-clone!
  "Copy one clone tree's since-fork edits + deletions into its `trunk`,
   tagging each change with the `trunk` it landed under (so a multi-root
   apply is unambiguous). Returns a vec of `{:status :path :root}`."
  [clone trunk fork-ms]
  (let [edits
        (mapv (fn [path]
                (let [src
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
  "Land the draft's since-fork edits into the user's real dirs (trunk),
   leaving them uncommitted for the user to review/commit. A draft is about
   the WHOLE workspace: this lands the primary clone AND every auto-cloned
   filesystem root (each into its own trunk). Vis owns no git lifecycle.
   Adds/modifications come from the mtime diff; deletions are files that
   existed at the fork but the agent removed in the draft. Returns
   `{:status :ok :changed [{:status :path :root}] :landed n :workspace ws}`."
  [db-info {:keys [workspace-id]}]
  (let [ws (get db-info workspace-id)]
    (when-not ws (throw (ex-info "Unknown workspace" {:workspace-id workspace-id})))
    (let [clone (:root ws)
          trunk (:repo-root ws)
          fork-ms (apply-fork-ms-of ws)]

      (when-not fork-ms
        (throw (ex-info "Workspace has no fork timestamp; cannot apply"
                        {:type :workspace/no-baseline :workspace-id workspace-id})))
      (let [primary (land-clone! clone trunk fork-ms)
            ;; each isolated filesystem root lands back into its own trunk
            extra (mapcat (fn [{:keys [trunk clone fork-ms]}]
                            (when (and fork-ms (not= clone trunk))
                              (land-clone! clone trunk fork-ms)))
                          (filesystem-roots ws))
            changes (vec (concat primary extra))]

        (fire-hook! :on-apply ws {:changed changes})
        {:status :ok :changed changes :landed (count changes) :workspace ws}))))

(defn abandon-lineage!
  "Discard `workspace-id` and each draft ancestor up to (but never including)
   trunk. Used when an operator applies or abandons the session draft. A plain
   draft's parent is its trunk, so this normally abandons the single draft."
  [db-info {:keys [workspace-id reason]}]
  (loop [ws
         (get db-info workspace-id)

         discarded
         []]

    (if-not (draft? ws)
      {:status :discarded :workspace-ids discarded}
      (let [parent-id
            (:parent-workspace-id ws)

            done
            (abandon! db-info {:workspace-id (:id ws) :reason reason})]

        (recur (some->> parent-id
                        (get db-info))
               (conj discarded (:id done)))))))

(defn abandon!
  "Release backend-owned roots and transition the row to :discarded. A `:live`
   backend (trunk) owns no clone, so its shared root is never deleted."
  [db-info {:keys [workspace-id reason]}]
  (let [ws (get db-info workspace-id)]
    (when-not ws (throw (ex-info "Unknown workspace" {:workspace-id workspace-id})))
    (discard-root! (:workspace-backend ws) (:root ws))
    (doseq [{:keys [trunk clone backend]} (filesystem-roots ws)
            :when (and clone (not= clone trunk))]

      (try (discard-root! backend clone) (catch Throwable _ nil)))
    (let [done (p/db-workspace-update-state! db-info workspace-id :discarded)]
      (fire-hook! :on-discard done {:reason reason})
      (assoc (or done ws) :reason reason))))

(defn discard-session-clones!
  "On session DELETE, walk the complete revision lineage and release every
   backend-owned root. Live roots are never deleted."
  [db-info session-soul-id]
  (when (and db-info session-soul-id)
    (when-let [state-id (p/db-latest-session-state-id db-info session-soul-id)]
      (loop [ws (for-session db-info state-id)]
        (when ws
          (try (discard-root! (:workspace-backend ws) (:root ws)) (catch Throwable _ nil))
          (doseq [{:keys [trunk clone backend]} (filesystem-roots ws)
                  :when (and clone (not= clone trunk))]

            (try (discard-root! backend clone) (catch Throwable _ nil)))
          (recur (some->> (:parent-workspace-id ws)
                          (get db-info))))))))
