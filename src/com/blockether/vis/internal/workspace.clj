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
           [java.nio.file CopyOption FileVisitResult Files LinkOption Path
            SimpleFileVisitor StandardCopyOption]
           [java.nio.file.attribute BasicFileAttributes FileAttribute]
           [java.util UUID]
           [org.eclipse.jgit.api Git]))

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
  (try
    (with-open [git (Git/open (io/file repo-root))]
      (.getBranch (.getRepository git)))
    (catch Throwable _ nil)))

(defn detect-trunk-branch
  "Discover the repo's default trunk branch (PLAN.md §2). Order:
     1. `origin/HEAD` symbolic-ref — most reliable, follows the
        repo's published default (set via `git remote set-head origin
        --auto` or by clone).
     2. local `main` branch existence.
     3. local `master` branch existence.
     4. `current-branch` — degenerate fallback for lone-branch repos
        with no remote tracking.
   Returns a plain branch name with no `refs/` prefix.

   Behaviour change vs the previous `(current-branch repo-root)`
   trunk inference: a session spawned while user is on `feat/foo`
   now correctly pins the trunk-kind workspace to `main` (or
   whatever origin/HEAD points at), not `feat/foo`. Existing trunk
   rows keep their stored `:branch` field; only new rows pick up
   the corrected logic."
  [repo-root]
  (or (try
        (some-> (git! repo-root ["symbolic-ref" "--short" "refs/remotes/origin/HEAD"])
          str/trim
          (str/replace #"^origin/" "")
          not-empty)
        (catch Throwable _ nil))
    (when (local-branch-exists? repo-root "main") "main")
    (when (local-branch-exists? repo-root "master") "master")
    (current-branch repo-root)))

(defn- current-head [repo-root]
  (try
    (with-open [git (Git/open (io/file repo-root))]
      (some-> (.resolve (.getRepository git) "HEAD") .getName))
    (catch Throwable _ nil)))

(defn- mirror-tree!
  "Mirror `src` worktree → `dst` worktree, preserving symlinks, mtimes,
   and permissions. Skips the top-level `.git/` (submodules carry their
   own nested `.git/` deeper in the tree which is left intact). REPLACEs
   files in `dst`. Never reads or writes outside either tree.

   PLAN.md §4.4 — branch-kind workspaces inherit trunk's dirty,
   untracked, and gitignored state so `npm install` / `clojure -P`
   artefacts are reused verbatim. Does NOT delete files present in
   `dst` but absent from `src`; the worktree was just minted via
   `git worktree add HEAD`, so the only divergence vs HEAD that can
   shadow a deletion is something the user staged — outside scope."
  [^String src ^String dst]
  (let [src-path (.toPath (io/file src))
        dst-path (.toPath (io/file dst))
        attrs    (make-array FileAttribute 0)
        copy-opts ^"[Ljava.nio.file.CopyOption;"
        (into-array CopyOption
          [StandardCopyOption/REPLACE_EXISTING
           StandardCopyOption/COPY_ATTRIBUTES
           LinkOption/NOFOLLOW_LINKS])
        skip-git? (fn [^Path rel]
                    (and (pos? (.getNameCount rel))
                      (= ".git" (str (.getName rel 0)))))]
    (Files/walkFileTree
      src-path
      (proxy [SimpleFileVisitor] []
        (preVisitDirectory [dir ^BasicFileAttributes _attrs]
          (let [rel (.relativize src-path ^Path dir)]
            (cond
              (zero? (.getNameCount rel))
              FileVisitResult/CONTINUE

              (skip-git? rel)
              FileVisitResult/SKIP_SUBTREE

              :else
              (let [target (.resolve dst-path rel)]
                (when-not (Files/exists target
                            (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))
                  (Files/createDirectories target attrs))
                FileVisitResult/CONTINUE))))
        (visitFile [file ^BasicFileAttributes _attrs]
          (let [rel (.relativize src-path ^Path file)]
            (if (skip-git? rel)
              FileVisitResult/CONTINUE
              (let [target (.resolve dst-path rel)]
                (when-let [parent (.getParent target)]
                  (when-not (Files/exists parent
                              (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))
                    (Files/createDirectories parent attrs)))
                (Files/copy ^Path file ^Path target copy-opts)
                FileVisitResult/CONTINUE))))
        (visitFileFailed [_file _exc]
          ;; Best-effort — ignore unreadable entries instead of aborting
          ;; the whole mirror. The worktree's `git worktree add` step
          ;; already wrote tracked@HEAD; missing untracked extras are
          ;; non-fatal.
          FileVisitResult/CONTINUE)))
    nil))

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

;; -----------------------------------------------------------------------------
;; Per-repo locking (PLAN.md §4.7)
;; -----------------------------------------------------------------------------
;;
;; Git operations that mutate refs / worktrees / the index must NOT run
;; concurrently against the same repository, even from independent
;; sessions on different workspaces. The shared `repo_root` is the
;; serialization domain. `with-repo-lock` acquires an `Object` monitor
;; keyed on `repo-id`, so:
;;
;;   - `(with-repo-lock "vis" …)` two sessions → second blocks until
;;     first releases.
;;   - `(with-repo-lock "vis" …)` + `(with-repo-lock "svar" …)` →
;;     orthogonal; both proceed.
;;
;; The lock map is unbounded but bounded in practice by the number of
;; distinct repos a Vis process touches. Entries are never evicted;
;; cheap (one Object per repo).

(defonce ^:private repo-locks (atom {}))

(defn- repo-lock-for ^Object [repo-id]
  (or (clojure.core/get @repo-locks repo-id)
    (-> (swap! repo-locks update repo-id #(or % (Object.)))
      (clojure.core/get repo-id))))

(defmacro with-repo-lock
  "Run `body` with the per-repo monitor held. Serializes git mutations
   (worktree add, commit, merge, branch delete) against the same
   repository across threads / sessions. `repo-id` is the canonical
   sanitized id used in the workspace table."
  [repo-id & body]
  `(locking (repo-lock-for ~repo-id)
     ~@body))

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

;; -----------------------------------------------------------------------------
;; Label + focus + hydration (PLAN.md §4.3, §6)
;; -----------------------------------------------------------------------------

(defn set-label!
  "Set the workspace's human-friendly `:label`. Empty string / nil
   clears the label and reverts to the default heuristic
   (`display-label`). Returns the updated workspace record.
   PLAN.md §1 + §6."
  [db-info {:keys [workspace-id label]}]
  (let [trimmed (some-> label str clojure.string/trim not-empty)]
    (p/db-workspace-update-label! db-info workspace-id trimmed)))

(defn focus!
  "Stamp `last_focused_at_ms` on the workspace AND upsert the per-repo
   `repo_focus` pointer. Called whenever the user switches the active
   tab / selects a workspace (TUI tab click, Telegram `/workspace
   switch …`). Cheap; safe to call repeatedly. Returns the updated
   workspace record."
  [db-info workspace-id]
  (when-let [ws (p/db-workspace-touch-focus! db-info workspace-id)]
    (when (:repo-id ws)
      (p/db-repo-focus-set! db-info (:repo-id ws) workspace-id))
    ws))

(defn last-focused
  "Return the workspace id from `repo_focus` for `repo-id`, or nil.
   Used by the TUI tab-restore on startup and by Telegram's switcher
   as the default landing for `/workspace switch` without an arg."
  [db-info repo-id]
  (some-> (p/db-repo-focus-get db-info repo-id) :workspace-id))

(defn display-label
  "Return the human-facing label for `workspace`. Order:
     1. `:label` field when set (model / user explicit override).
     2. The pinned session's `title` when present.
     3. The branch name (sans `vis/` prefix for branch-kind).
     4. Final fallback: the workspace id's leading 8 chars.

   `session` is optional; the caller passes it when already hydrated.
   When nil and the workspace has a pin, the fn fetches the session
   itself (defensive)."
  ([workspace]
   (display-label nil workspace nil))
  ([db-info workspace session]
   (let [strip-branch #(some-> % (clojure.string/replace #"^vis/" ""))
         hydrated     (or session
                        (when (and db-info (:id workspace))
                          (some->> (:id workspace)
                            (p/db-session-state-list-for-workspace db-info)
                            first)))]
     (or (some-> (:label workspace) clojure.string/trim not-empty)
       (some-> hydrated :title clojure.string/trim not-empty)
       (case (:kind workspace)
         :branch (strip-branch (:branch workspace))
         :trunk  (:branch workspace)
         nil)
       (some-> (:id workspace) str (subs 0 (min 8 (count (str (:id workspace))))))))))

(defn workspace-with-session
  "Hydrate `workspace-id` with its pinned `session_state`. Returns
   `{:workspace <ws> :session-state <ss>}` (single fetch; channel
   layer never N+1s). `:session-state` is nil for unpinned workspaces."
  [db-info workspace-id]
  (when-let [ws (p/db-workspace-get db-info workspace-id)]
    {:workspace     ws
     :session-state (some->> workspace-id
                      (p/db-session-state-list-for-workspace db-info)
                      first)}))

(defn list-active-with-sessions
  "Like `list-active` but each entry is the `{:workspace :session-state}`
   pair already hydrated. Sorted by `last_focused_at_ms` DESC NULLS
   LAST, then `created_at` DESC. Drives the TUI strip + Telegram
   switcher (PLAN.md §9, §10)."
  [db-info repo-id]
  (let [rows (list-active db-info repo-id)
        ;; Compare by [recency-bucket created-at] descending. NULLS
        ;; last via Long/MIN_VALUE sentinel.
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
  "Enrich a workspace record with live VCS status. Stamps the canonical
   `:vcs/*` keys (`:vcs/kind :git`, `:vcs/branch`, `:vcs/head`,
   `:vcs/dirty?`). Future Mercurial / Jujutsu detectors fork this fn
   (or dispatch on a `:vcs/kind` discriminator at the call site) and
   emit the same `:vcs/*` core keys; the engine reads VCS-agnostic.
   On error: `:workspace/error`. PLAN.md §12 step 4 — the legacy
   `:git/*` aliases are GONE."
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
            :vcs/kind          :git
            :vcs/branch        branch
            :vcs/head          head
            :vcs/dirty?        dirty?))
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
          ;; PLAN.md §2: pin trunk-kind workspace to the repo's
          ;; published default (origin/HEAD → main → master → current)
          ;; instead of whatever branch the user happens to be on.
          branch    (detect-trunk-branch repo-root)
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

   After `git worktree add HEAD`, the worktree is mirrored from the
   trunk root via `mirror-tree!` so the new branch inherits the
   trunk's dirty tracked edits, untracked files, AND gitignored
   artefacts (`node_modules/`, `target/`, `.env`, ...). The branch
   `just works` without re-running `npm install` / `clojure -P`.
   PLAN.md §4.4.

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
    (with-repo-lock rid
      (.mkdirs (.getParentFile (io/file root)))
      (if (local-branch-exists? repo-root branch)
        (git! repo-root ["worktree" "add" root branch])
        (git! repo-root ["worktree" "add" "-b" branch root "HEAD"]))
      ;; Overlay trunk dirty + untracked + ignored onto the fresh worktree.
      ;; `.git/` (the worktree's gitdir link file) is preserved by the
      ;; skip rule in `mirror-tree!`.
      (mirror-tree! repo-root root)
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
        ws))))

(defn commit!
  "Stage everything (`git add -A`) and commit inside the workspace's
   worktree. Refuses trunk-kind. Returns
   `{:status :nothing-to-commit :workspace ws}` (no throw) when the
   index has no diff vs HEAD. On success, bumps `workspace.commit_id`
   to the new HEAD and returns
   `{:status :ok :sha :message :branch :workspace}`. PLAN.md §4.3."
  [db-info {:keys [workspace-id message]}]
  (let [ws  (get db-info workspace-id)
        msg (or (some-> message str str/trim not-empty) "vis workspace commit")]
    (when-not ws
      (throw (ex-info "Unknown workspace" {:workspace-id workspace-id})))
    (when (= :trunk (:kind ws))
      (throw (ex-info "Trunk workspace cannot be committed via workspace/commit!"
               {:type :workspace/trunk-commit :workspace-id workspace-id})))
    (let [root   (:root ws)
          branch (:branch ws)]
      (with-repo-lock (:repo-id ws)
        (git! root ["add" "-A"])
        (let [staged (str/trim
                       (:out (git-result root ["diff" "--cached" "--name-only"])))]
          (if (str/blank? staged)
            {:status :nothing-to-commit :workspace ws :branch branch}
            (do (git! root ["commit" "-m" msg])
              (let [sha (git! root ["rev-parse" "HEAD"])
                    done (or (p/db-workspace-update-commit-id! db-info workspace-id sha)
                           (assoc ws :commit-id sha))]
                {:status :ok :sha sha :message msg :branch branch :workspace done}))))))))

(defn ff-apply!
  "Fast-forward merge `workspace-id`'s branch onto the repo's trunk
   (PLAN.md §4.5). Refuses trunk-kind.

   Sequence (under `with-repo-lock`):
     1. transition workspace state :active|:merging → :merging
     2. auto-stash trunk if dirty (workspace already mirrors the trunk
        state we want; stash is just for FF cleanliness)
     3. `git checkout <trunk-branch>` (idempotent)
     4. `git merge --ff-only vis/<id>`
     5. auto-pop the stash
     6. transition workspace state → :merged on success

   Returns:
     `{:status :ok :sha <new-HEAD> :branch :workspace}` on success
     `{:status :ff-failed :reason :workspace :trunk}` on non-ancestor
        / conflict; workspace is LEFT in :merging so the engine can
        hand off to `start-merge-resolve!` (§7) without losing the
        partial state."
  [db-info {:keys [workspace-id]}]
  (let [ws (get db-info workspace-id)]
    (when-not ws
      (throw (ex-info "Unknown workspace" {:workspace-id workspace-id})))
    (when (= :trunk (:kind ws))
      (throw (ex-info "Trunk workspace cannot be merged onto itself"
               {:type :workspace/trunk-merge :workspace-id workspace-id})))
    (when-not (contains? #{:active :merging} (:state ws))
      (throw (ex-info (str "Workspace must be :active or :merging to ff-apply (state="
                        (:state ws) ")")
               {:workspace-id workspace-id :state (:state ws)})))
    (let [repo-root (:repo-root ws)
          branch    (:branch ws)
          trunk     (detect-trunk-branch repo-root)]
      (with-repo-lock (:repo-id ws)
        (p/db-workspace-update-state! db-info workspace-id :merging)
        (let [trunk-dirty? (not (str/blank?
                                  (:out (git-result repo-root
                                          ["status" "--porcelain"]))))
              stash-tag    (str "vis-ff-" workspace-id)
              stashed?     (when trunk-dirty?
                             (try (git! repo-root ["stash" "push" "-u" "-m" stash-tag])
                               true
                               (catch Throwable _ false)))]
          (try
            (git! repo-root ["checkout" trunk])
            (git! repo-root ["merge" "--ff-only" branch])
            (let [sha  (git! repo-root ["rev-parse" "HEAD"])
                  done (p/db-workspace-update-state! db-info workspace-id :merged)]
              (fire-hook! :on-apply done
                {:exit 0 :sha sha :branch branch :trunk trunk})
              {:status :ok :sha sha :branch branch :workspace done})
            (catch Throwable t
              {:status    :ff-failed
               :reason    (or (ex-message t) (str t))
               :workspace ws
               :branch    branch
               :trunk     trunk})
            (finally
              (when stashed?
                (try (git! repo-root ["stash" "pop"]) (catch Throwable _ nil))))))))))

(defn- parse-conflicts
  "Read `git status --porcelain=v1` and project conflicting paths
   (`UU`, `AA`, `DD`, `AU`, `UA`, `DU`, `UD`) into a vec of
   `{:path :state}` maps. Empty vec when no conflicts. PLAN.md §7."
  [repo-root]
  (let [conflict-codes #{"UU" "AA" "DD" "AU" "UA" "DU" "UD"}
        out (try (git! repo-root ["status" "--porcelain=v1"])
              (catch Throwable _ ""))]
    (vec
      (for [line (str/split-lines (or out ""))
            :let  [xy   (when (>= (count line) 2) (subs line 0 2))
                   path (when (>= (count line) 3) (str/trim (subs line 3)))]
            :when (and xy path (contains? conflict-codes xy))]
        {:path path :state xy}))))

(defn start-merge-resolve!
  "Bootstrap a merge-resolve sub-session for an `:ff-failed` workspace.
   PLAN.md §7.1.

   1. Take the per-repo lock so no other Vis process touches refs
      while we mutate the trunk worktree's index.
   2. `git checkout <trunk-branch>` followed by `git merge <branch>`
      (without `--ff-only`). This lands the conflicting tree state
      in the trunk worktree's working copy + index so a human (or
      a future `merge/*` op family) can resolve the markers.
   3. Spawn a sub-session_state row pinned to the SAME workspace as
      the parent (the partial UNIQUE index on workspace_id allows
      this when `merge_resolve_parent_id` is non-NULL). The new
      row's `:merge_resolve_parent_id` references the parent.
   4. Return `{:status :ok|:already-merged|:nothing-to-merge
              :sub-session-state-id
              :conflicts [{:path :state} ...]
              :workspace}`. The engine loop / channel layer renders
      a `:session/merge-resolve-started` event on top of this.

   `merge/*` SCI op family + sub-session prompt + channel UX events
   are out of scope for this commit (incremental delivery; the
   bootstrap is the gate for the rest)."
  [db-info {:keys [workspace-id parent-session-state-id]}]
  (let [ws (get db-info workspace-id)]
    (when-not ws
      (throw (ex-info "Unknown workspace" {:workspace-id workspace-id})))
    (when (= :trunk (:kind ws))
      (throw (ex-info "Cannot merge-resolve trunk-kind workspace"
               {:type :workspace/trunk-merge-resolve
                :workspace-id workspace-id})))
    (when-not parent-session-state-id
      (throw (ex-info "start-merge-resolve! requires :parent-session-state-id"
               {:type :workspace/missing-parent-session-state})))
    (let [repo-root (:repo-root ws)
          branch    (:branch ws)
          trunk     (detect-trunk-branch repo-root)]
      (with-repo-lock (:repo-id ws)
        ;; Make sure we're on trunk before attempting the merge.
        (try (git! repo-root ["checkout" trunk]) (catch Throwable _ nil))
        (let [merge-result (try {:ok? true
                                 :out (git! repo-root ["merge" "--no-ff" "--no-commit" branch])}
                             (catch clojure.lang.ExceptionInfo e
                               {:ok? false
                                :exit (:exit (ex-data e))
                                :out  (:out (ex-data e))
                                :err  (:err (ex-data e))}))
              conflicts    (parse-conflicts repo-root)
              sub-id       (p/db-session-state-spawn-merge-resolve!
                             db-info parent-session-state-id)]
          (cond
            ;; Merge succeeded cleanly (no conflicts) AND no diff to
            ;; commit — we landed in a no-op state. Abort the merge
            ;; (clean index) and surface :nothing-to-merge.
            (and (:ok? merge-result) (empty? conflicts))
            (do (try (git! repo-root ["merge" "--abort"]) (catch Throwable _ nil))
              {:status :nothing-to-merge
               :sub-session-state-id sub-id
               :workspace ws
               :trunk trunk})

            :else
            {:status :ok
             :sub-session-state-id sub-id
             :conflicts conflicts
             :workspace ws
             :trunk trunk
             :merge merge-result}))))))

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
