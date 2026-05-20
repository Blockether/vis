(ns com.blockether.vis.internal.workspace
  "Git worktree-backed workspaces, DB-pinned to session_state 1:1.

   See PLAN.md §3 for the public API. Trunk-kind workspaces have no
   materialised worktree directory (root = repo_root); branch-kind
   workspaces live under ~/.vis/workspaces/<repo-id>/<workspace-id>/.

   Vis never mutates JVM user.dir. Channels rebind *workspace-root*
   per turn from the active workspace; tools resolve paths via
   (workspace/cwd). There is NO process-cwd fallback - calling
   workspace/cwd outside a binding is an error."
  (:refer-clojure :exclude [get])
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [com.blockether.vis.internal.persistance :as p])
  (:import [java.io File]
           [java.util UUID]))

(set! *warn-on-reflection* true)

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
   wrapper binds `*workspace-root*` per turn (PLAN.md §5), so the
   process-cwd fallback only fires from REPL / test / one-off CLI
   paths that have no session context. Production tool callers never
   see the fallback because the env carries `:workspace/root` from
   `create-environment` onward."
  ^File []
  (io/file (or *workspace-root* (System/getProperty "user.dir"))))

;; =============================================================================
;; Legacy EDN cleanup
;;
;; ~/.vis/workspaces.edn predated the DB-backed workspace table. PLAN.md
;; §1 decision 4 dropped EDN entirely with no import; this is the
;; idempotent one-shot delete that runs on first ns load.
;; =============================================================================

(defn- legacy-edn-file ^File []
  (io/file (System/getProperty "user.home") ".vis" "workspaces.edn"))

(defn delete-legacy-edn!
  "Best-effort delete of ~/.vis/workspaces.edn. Idempotent."
  []
  (let [f (legacy-edn-file)]
    (when (.exists f)
      (try (.delete f) (catch Throwable _ nil)))))

(defonce ^:private _legacy-edn-cleanup
  (delete-legacy-edn!))

;; =============================================================================
;; Git helpers
;; =============================================================================

(defn- file-path [^File f]
  (.getCanonicalPath f))

(defn- git-result
  [dir args]
  (let [argv (cond-> ["git"]
               dir (into ["-C" (file-path (io/file dir))])
               true (into args))]
    (assoc (apply sh/sh argv) :argv argv)))

(defn- git!
  [dir args]
  (let [result (git-result dir args)]
    (when-not (zero? (:exit result))
      (throw (ex-info (str "git failed: " (str/join " " (:argv result)))
               {:argv (:argv result)
                :exit (:exit result)
                :out  (:out result)
                :err  (:err result)})))
    (str/trim (or (:out result) ""))))

(defn- local-branch-exists?
  [repo-root branch]
  (zero? (:exit (git-result repo-root
                  ["show-ref" "--verify" "--quiet"
                   (str "refs/heads/" branch)]))))

(defn- discover-repo-root
  "Discover the enclosing git repo root from the JVM cwd. Throws a
   user-actionable error when no repo is found (PLAN.md decision 14)."
  []
  (try (git! nil ["rev-parse" "--show-toplevel"])
    (catch Throwable t
      (throw (ex-info "Vis requires a git repository (no repo discovered)."
               {:type :workspace/no-git}
               t)))))

(defn- current-branch [repo-root]
  (try (git! repo-root ["rev-parse" "--abbrev-ref" "HEAD"])
    (catch Throwable _ nil)))

(defn- current-head [repo-root]
  (try (git! repo-root ["rev-parse" "HEAD"])
    (catch Throwable _ nil)))

(defn- dirty-worktree?
  [root]
  (not (str/blank? (git! root ["status" "--porcelain"]))))

(defn- commit-worktree-if-dirty!
  [root workspace-id]
  (when (dirty-worktree? root)
    (git! root ["add" "-A"])
    (git! root ["-c" "user.name=Vis Workspace"
                "-c" "user.email=vis-workspace@localhost"
                "commit" "-m" (str "Apply workspace " workspace-id)])
    (current-head root)))

(defn- sanitize-id
  [s]
  (let [s (-> (str (or s "workspace"))
            str/lower-case
            (str/replace #"[^a-z0-9._-]+" "-")
            (str/replace #"(^-+|-+$)" ""))]
    (if (str/blank? s) "workspace" s)))

(defn- repo-id-for
  [repo-root]
  (let [root (file-path (io/file repo-root))
        name (sanitize-id (.getName (io/file root)))
        hash (Long/toUnsignedString (Integer/toUnsignedLong (hash root)) 36)]
    (str name "-" hash)))

(defn- worktree-root
  [repo-id workspace-id]
  (file-path (io/file (System/getProperty "user.home") ".vis" "workspaces"
               repo-id workspace-id)))

;; =============================================================================
;; Hooks
;; =============================================================================

(defonce ^:private hooks (atom {:on-spawn [] :on-apply [] :on-discard []}))

(defn register-hook!
  "Register `hook-fn` for `hook-id` ∈ {:on-spawn :on-apply :on-discard}.
   Synchronous, post-commit; exceptions swallowed."
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
  "Active + merging workspaces for `repo-id`, newest first."
  [db-info repo-id]
  (p/db-workspace-list-by-repo db-info repo-id #{:active :merging}))

(defn list-finished
  "Merged + discarded workspaces for `repo-id`, newest first.
   Finished workspaces are invisible in the TUI strip (PLAN.md
   decision 12) but remain queryable for transcript references."
  [db-info repo-id]
  (p/db-workspace-list-by-repo db-info repo-id #{:merged :discarded}))

(defn for-session
  "Workspace pinned to `session-state-id`. Returns nil for unbound
   states (transitional, pre-step-4); non-nil under the 1:1 invariant."
  [db-info session-state-id]
  (p/db-workspace-for-session db-info session-state-id))

(defn status
  "Enrich a workspace record with live git status. Adds :git/branch,
   :git/head, :git/dirty?, :workspace/exists?. On error: :workspace/error."
  [db-info workspace-id]
  (when-let [ws (get db-info workspace-id)]
    (let [root (:root ws)]
      (try
        (let [exists? (.exists (io/file root))
              branch  (when exists? (current-branch root))
              head    (when exists? (current-head root))
              dirty?  (when exists?
                        (not (str/blank? (git! root ["status" "--porcelain"]))))]
          (assoc ws
            :workspace/exists? exists?
            :git/branch        branch
            :git/head          head
            :git/dirty?        dirty?))
        (catch Throwable t
          (assoc ws
            :workspace/exists? (.exists (io/file root))
            :workspace/error   (or (ex-message t) (str t))))))))

(defn trunk-info
  "Live git snapshot of trunk for `repo-root` (or discovered repo).
   No DB read; pure git introspection."
  ([] (trunk-info nil))
  ([repo-root]
   (let [root (or repo-root (discover-repo-root))]
     {:repo-root root
      :branch    (current-branch root)
      :head      (current-head root)})))

;; =============================================================================
;; Mutations
;; =============================================================================

(defn ensure-trunk!
  "Find-or-create the trunk workspace for `:session-state-id`. If the
   session_state already has a pinned workspace, returns it (regardless
   of kind). Otherwise materialises a new trunk row for the discovered
   repo and pins it. Idempotent per session-state."
  [db-info {:keys [session-state-id]}]
  (or (for-session db-info session-state-id)
    (let [repo-root (discover-repo-root)
          branch    (current-branch repo-root)
          ws (p/db-workspace-insert! db-info
               {:repo-id   (repo-id-for repo-root)
                :repo-root repo-root
                :kind      :trunk
                :branch    branch
                :root      repo-root
                :state     :active
                :commit-id (current-head repo-root)})]
      (when session-state-id
        (p/db-session-state-set-workspace! db-info session-state-id (:id ws)))
      ws)))

(defn spawn-branch!
  "Spawn a new branch-kind workspace. Branch name (`vis/<short-id>`)
   and worktree path (`~/.vis/workspaces/<repo>/<id>/`) are both
   auto-minted (PLAN.md decision 15) - callers never pass them.

   Opts:
     :from              - workspace map to fork from; nil = derive from
                          current git discovery
     :session-state-id  - optional; when present, pins the new workspace
                          to this session_state row

   Returns the inserted workspace. Fires :on-spawn hook."
  [db-info {:keys [from session-state-id]}]
  (let [repo-root (or (:repo-root from) (discover-repo-root))
        rid       (or (:repo-id from) (repo-id-for repo-root))
        ws-id     (str (UUID/randomUUID))
        branch    (str "vis/" (subs ws-id 0 8))
        root      (worktree-root rid ws-id)]
    (.mkdirs (.getParentFile (io/file root)))
    (if (local-branch-exists? repo-root branch)
      (git! repo-root ["worktree" "add" root branch])
      (git! repo-root ["worktree" "add" "-b" branch root "HEAD"]))
    (let [commit (try (git! root ["rev-parse" "HEAD"]) (catch Throwable _ nil))
          ws (p/db-workspace-insert! db-info
               {:id        ws-id
                :repo-id   rid
                :repo-root repo-root
                :kind      :branch
                :branch    branch
                :root      root
                :parent-id (:id from)
                :state     :active
                :commit-id commit})]
      (when session-state-id
        (p/db-session-state-set-workspace! db-info session-state-id (:id ws)))
      (fire-hook! :on-spawn ws)
      ws)))

(defn apply-to-trunk!
  "Merge `:workspace-id`'s branch into the trunk branch currently
   checked out in repo-root. Refuses trunk-kind. Walks
   :active → :merging → :merged on success; leaves at :merging for
   retry on failure.

   Opts:
     :workspace-id    - required
     :strategy        - :no-ff (default) | :ff-only
     :delete-branch?  - delete local branch after success

   Returns {:workspace ws :merge {:exit :out :branch}}."
  [db-info {:keys [workspace-id strategy delete-branch?]
            :or   {strategy :no-ff}}]
  (let [ws (get db-info workspace-id)]
    (when-not ws
      (throw (ex-info "Unknown workspace" {:workspace-id workspace-id})))
    (when (= :trunk (:kind ws))
      (throw (ex-info "Trunk workspace cannot be merged"
               {:type :workspace/trunk-merge :workspace-id workspace-id})))
    (when-not (contains? #{:active :merging} (:state ws))
      (throw (ex-info (str "Workspace must be :active or :merging to merge (state="
                        (:state ws) ")")
               {:workspace-id workspace-id :state (:state ws)})))
    (let [repo-root  (:repo-root ws)
          root       (:root ws)
          branch     (:branch ws)
          merge-args (cond-> ["merge"]
                       (= strategy :no-ff)   (conj "--no-ff")
                       (= strategy :ff-only) (conj "--ff-only")
                       true                  (conj branch))]
      (commit-worktree-if-dirty! root workspace-id)
      (p/db-workspace-update-state! db-info workspace-id :merging)
      (try
        (let [out  (git! repo-root merge-args)
              done (p/db-workspace-update-state! db-info workspace-id :merged)]
          (when delete-branch?
            (try (git! repo-root ["branch" "-D" branch]) (catch Throwable _ nil)))
          (fire-hook! :on-apply done {:exit 0 :out out :branch branch})
          {:workspace done :merge {:exit 0 :out out :branch branch}})
        (catch Throwable t
          (throw (ex-info "Merge failed; workspace left in :merging for retry"
                   {:workspace-id workspace-id
                    :branch       branch
                    :error        (or (ex-message t) (str t))}
                   t)))))))

(defn discard!
  "Remove `:workspace-id`'s worktree from disk and transition the row
   to :discarded. Refuses trunk-kind.

   Opts:
     :workspace-id    - required
     :delete-branch?  - true ≡ hard discard (also `git branch -D`),
                        false ≡ soft (worktree only)
     :force?          - allow removing a dirty worktree (`--force`)

   Returns the updated workspace. Fires :on-discard hook."
  [db-info {:keys [workspace-id delete-branch? force?]}]
  (let [ws (get db-info workspace-id)]
    (when-not ws
      (throw (ex-info "Unknown workspace" {:workspace-id workspace-id})))
    (when (= :trunk (:kind ws))
      (throw (ex-info "Trunk workspace cannot be discarded"
               {:type :workspace/trunk-discard :workspace-id workspace-id})))
    (let [repo-root (:repo-root ws)
          root      (:root ws)
          branch    (:branch ws)
          rm-args   (cond-> ["worktree" "remove"]
                      force? (conj "--force")
                      true   (conj root))]
      (try (git! repo-root rm-args) (catch Throwable _ nil))
      (when delete-branch?
        (try (git! repo-root ["branch" "-D" branch]) (catch Throwable _ nil)))
      (let [done (p/db-workspace-update-state! db-info workspace-id :discarded)]
        (fire-hook! :on-discard done)
        done))))
