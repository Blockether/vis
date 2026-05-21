(ns com.blockether.vis.ext.foundation-git.core
  "Git observation tools for the LLM under the `git/` alias.

   Ships three observation tools: `git/diff`, `git/status`,
   `git/log`. All three are read-only, JGit-backed, and run inside the
   currently bound workspace root (channels rebind `*workspace-root*`
   per turn, PLAN.md §5).

   The extension activates only when the active workspace root is inside
   a git repo. No host `git` binary is required for observation tools."
  (:require
   [clojure.java.io :as io]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.git :as git-core]))

;; =============================================================================
;; Activation
;; =============================================================================

(defn- in-repo?
  "True when the active workspace root sits inside a git repo. Uses JGit
   only; returns false on any failure so symbols stay hidden when there
   is no repo to read."
  [env]
  (let [root (:workspace/root env)]
    (when (string? root)
      (try (git-core/in-repository? (io/file root))
        (catch Throwable _ false)))))

(defn- activation-fn
  "Per-turn activation. Returns true only when the workspace root is in
   a git repository; false silently hides all `git/*` symbols from the
   model for that turn."
  [env]
  (boolean (in-repo? env)))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- env-root
  "Canonical workspace root from the env. Throws if missing — the
   activation-fn should have prevented the symbol from firing without
   one, so this is a defensive last line."
  [env]
  (or (:workspace/root env)
    (throw (ex-info "git/* tool fired without :workspace/root in env"
             {:type :foundation-git/no-workspace}))))

;; =============================================================================
;; Tools
;; =============================================================================

(defn git-diff-fn
  "Diff the active workspace. For a branch-kind workspace, diffs the
   workspace HEAD against the commit it was spawned from (recorded in
   `:workspace/id`'s row as `commit_id`). Otherwise diffs the working
   tree against HEAD.

   Returns:
     {:branch    \"vis/abc\" | nil
      :head      \"sha\"
      :kind      :trunk | :branch | nil
      :stat      {:files N :+ N :- N}
      :files     [{:file :+ :-} ...]
      :porcelain [{:status :file} ...]}"
  ([env]
   (git-diff-fn env nil))
  ([env opts]
   (when (and (some? opts) (not (map? opts)))
     (throw (ex-info (str "git/diff expected optional opts map, got " (pr-str opts) ". "
                       "Call (git/diff) or (git/diff {:stat? true}).")
              {:type :foundation-git/invalid-opts
               :opts opts
               :expected "nil or map"
               :examples ["(git/diff)" "(git/diff {:stat? true})"]})))
   (let [root        (env-root env)
         root-file   (io/file root)
         ws-id       (:workspace/id env)
         db-info     (:db-info env)
         ws          (when (and db-info ws-id) (vis/workspace-get db-info ws-id))
         base        (when (and (= :branch (:kind ws)) (:commit-id ws))
                       (:commit-id ws))
         status      (git-core/status-snapshot root-file)
         files       (vec (or (git-core/diff-numstat root-file (or base "HEAD") (when base "HEAD")) []))
         porcelain   (vec (or (:entries status) []))
         head        (:head status)
         +sum        (reduce + 0 (map :+ files))
         -sum        (reduce + 0 (map :- files))]
     (extension/success
       {:result {:branch    (:branch ws)
                 :head      head
                 :kind      (:kind ws)
                 :stat      {:files (count files) :+ +sum :- -sum}
                 :files     files
                 :porcelain porcelain}}))))

(defn git-status-fn
  "Working-tree status of the active workspace as parsed porcelain.

   Returns:
     {:branch \"main\"
      :head   \"sha\"
      :clean? bool
      :entries [{:status :file} ...]}"
  [env]
  (let [snapshot (or (git-core/status-snapshot (io/file (env-root env)))
                   {:branch nil :head nil :clean? true :entries []})]
    (extension/success {:result snapshot})))

(defn git-log-fn
  "Recent commits in the active workspace. Default limit is 20; pass a
   positive int up to 200 to override.

   Returns:
     {:branch \"main\"
      :commits [{:sha :author :at :subject} ...]}"
  ([env] (git-log-fn env 20))
  ([env n]
   (let [root      (io/file (env-root env))
         limit     (max 1 (min 200 (long (or n 20))))
         status    (git-core/status-snapshot root)
         commits   (vec (or (git-core/recent-commits root limit) []))]
     (extension/success
       {:result {:branch  (:branch status)
                 :commits commits}}))))

(def ^{:doc "Diff stat + porcelain for the currently bound workspace. Branch workspaces diff against their spawn commit; trunk workspaces diff against HEAD. Optional opts map accepted (e.g. {:stat? true}); result always includes stat, files, and porcelain. Returns {:branch :head :kind :stat {:files :+ :-} :files [...] :porcelain [...]}. JGit-backed; no host git binary needed."
       :arglists '([] [opts])} diff git-diff-fn)

(def ^{:doc "Working-tree status of the currently bound workspace. Returns {:branch :head :clean? :entries [{:status :file} ...]}. JGit-backed; no host git binary needed."
       :arglists '([])} status git-status-fn)

(def ^{:doc "Recent commits on the currently bound workspace's branch. Default 20 (max 200). Returns {:branch :commits [{:sha :author :at :subject} ...]}. JGit-backed; no host git binary needed."
       :arglists '([] [n])} log git-log-fn)

(defn- inject-env
  [env f args]
  {:env env :fn f :args (into [env] args)})

(def diff-symbol
  (vis/symbol #'diff
    {:before-fn inject-env
     :render-fn vis/render-string}))

(def status-symbol
  (vis/symbol #'status
    {:before-fn inject-env
     :render-fn vis/render-string}))

(def log-symbol
  (vis/symbol #'log
    {:before-fn inject-env
     :render-fn vis/render-string}))

(def git-symbols
  [diff-symbol status-symbol log-symbol])

;; =============================================================================
;; Op registry
;; =============================================================================

(doseq [op [:git/diff :git/status :git/log]]
  (vis/register-op! op {:tag :observation}))

;; =============================================================================
;; Extension manifest
;; =============================================================================

(def vis-extension
  (vis/extension
    {:ext/name           "foundation-git"
     :ext/description    "JGit-backed observation tools under git/: diff, status, log. Activates only when the active workspace sits inside a repo."
     :ext/version        "0.1.0"
     :ext/author         "Blockether"
     :ext/owner          "vis"
     :ext/license        "Apache-2.0"
     :ext/activation-fn  activation-fn
     :ext/sci            {:ext.sci/alias 'git
                          :ext.sci/symbols git-symbols}
     :ext/kind           "git"}))

(vis/register-extension! vis-extension)
