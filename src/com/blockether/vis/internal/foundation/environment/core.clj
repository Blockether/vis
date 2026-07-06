(ns com.blockether.vis.internal.foundation.environment.core
  "vis-foundation — the agent's environment-awareness layer.

   Owns the environment facts: cwd, user, platform, shell, plus:

     * git repository facts via the git binary (root, branch, dirty status,
       submodules, worktree),
     * a bounded language scan over the working tree (top languages
       by total bytes, primary language),
     * monorepo / multi-package shape detection (polylith, workspace,
       submodules) by counting per-ecosystem manifests.

   Model-facing VCS/workspace truth lives in `:session/workspace` CTX.
   Remaining helpers cover coarse project shape (`languages`,
   `monorepo`, `repositories`) and cache invalidation (`refresh!`).

   Runtime facts are computed lazily on first access and cached per
   working-directory. The cache is invalidated automatically when
   `cwd` changes between calls and explicitly by `(refresh!)`."
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.foundation.environment.agents :as agents]
            [com.blockether.vis.internal.foundation.environment.git :as git]
            [com.blockether.vis.internal.foundation.environment.host :as host]
            [com.blockether.vis.internal.foundation.environment.languages :as languages]
            [com.blockether.vis.internal.foundation.environment.monorepo :as monorepo]
            [com.blockether.vis.internal.foundation.environment.render :as render]
            [com.blockether.vis.internal.foundation.environment.repositories :as repositories]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.workspace :as workspace]
            [taoensso.telemere :as tel]))

;; ---------------------------------------------------------------------------
;; Snapshot cache. Keyed by canonical cwd so we recompute on
;; directory change. Recomputation is cheap (the language scan is
;; bounded) but redundant on every iteration of the same turn, so
;; we memoize at the extension boundary.
;; ---------------------------------------------------------------------------

;; `defonce` so the atom survives a `(require :reload)` during an
;; extension reload (per plan caveat: extensions holding mutable
;; state across reload MUST use defonce). The cwd-keyed snapshot
;; covers host/git/languages/monorepo only — agents hold their own cache.
(defonce ^:private cache (atom {:key nil :value nil}))

(defn- canonical-cwd
  ^String []
  ;; Production: channel rebinds *workspace-root* per turn.
  ;; The try/catch covers REPL / test paths where no binding exists.
  (try (.getCanonicalPath (workspace/cwd)) (catch Throwable _ (.getPath (workspace/cwd)))))

(defn- compute-snapshot
  "Build the full snapshot map. Each piece is independently guarded
   so a failure in one section never poisons the others."
  [^String cwd]
  (let [cwd-file
        (java.io.File. cwd)

        host-map
        (try (host/snapshot)
             (catch Throwable t
               (tel/log! {:level :warn :id ::host-failed :data {:error (ex-message t)}})
               {}))

        git-map
        (try (git/snapshot cwd-file)
             (catch Throwable t
               (tel/log! {:level :warn :id ::git-failed :data {:error (ex-message t)}})
               nil))

        scan-root
        (or (some-> ^String (:root git-map)
                    (java.io.File.))
            cwd-file)

        langs-map
        (try (languages/scan scan-root)
             (catch Throwable t
               (tel/log! {:level :warn :id ::languages-failed :data {:error (ex-message t)}})
               nil))

        mono-map
        (try (monorepo/snapshot scan-root)
             (catch Throwable t
               (tel/log! {:level :warn :id ::monorepo-failed :data {:error (ex-message t)}})
               nil))

        repos-map
        (try (repositories/snapshot scan-root)
             (catch Throwable t
               (tel/log! {:level :warn :id ::repositories-failed :data {:error (ex-message t)}})
               nil))]

    {:host host-map :git git-map :languages langs-map :monorepo mono-map :repositories repos-map}))

(defn snapshot
  "Full environment snapshot map {:host :git :languages :monorepo :repositories}. Cached per cwd; host helper, not a model tool."
  []
  (let [cwd
        (canonical-cwd)

        cached
        @cache]

    (if (= cwd (:key cached))
      (:value cached)
      (let [value (compute-snapshot cwd)]
        (reset! cache {:key cwd :value value})
        value))))

(defn refresh!
  "await refresh()
Drop the cached env snapshot and recompute. Returns the fresh snapshot. Use after large tree/branch changes."
  []
  (reset! cache {:key nil :value nil})
  (try (agents/reload!)
       (catch Throwable t
         (tel/log! {:level :warn :id ::agents-reload-failed :data {:error (ex-message t)}})))
  (snapshot))

;; ---------------------------------------------------------------------------
;; Extension definition.
;; ---------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
;; Local thin wrappers around the snapshot accessors so each `v/` callable
;; corresponds to a real var with `:doc` + `:arglists` baked in. `vis/symbol`
;; reads both straight from the var meta - no separate side-map at the
;; registration callsite.
;; -----------------------------------------------------------------------------

(defn repositories
  "await repositories()
Returns {\"count\": N, \"repositories\": [{\"path\", \"branch\", \"dirty\": bool, \"changes\": bool, \"stale\": bool, \"stash_count\": N, ...}], \"truncated\": bool}."
  []
  (:repositories (snapshot)))

(defn git
  "await git()
Returns {\"root\", \"branch\", \"detached\": bool, \"submodules\": bool, \"worktree\": bool, \"stash_count\", \"upstream\", \"ahead\", \"behind\", \"stale\", \"dirty\", \"clean\", \"modified\", \"untracked\", \"added\", \"changed\", \"removed\", \"missing\", \"conflicting\"}, or None outside a repo."
  []
  (:git (snapshot)))

(defn languages
  "await languages()
Returns {\"total_files\": N, \"total_bytes\": N, \"primary\": \"clojure\", \"languages\": [{\"language\", \"files\": N, \"bytes\": N, \"files_pct\", \"bytes_pct\"}, ...], \"truncated\": bool, \"elapsed_ms\": N}. List sorted by files desc."
  []
  (:languages (snapshot)))

(defn monorepo
  "await monorepo()
Returns {\"shape\": \"polylith\"|\"workspace\"|\"submodules\"|None, \"totals\": {\"clojure\": N, ...}, \"files\": {\"clojure\": [\"path/deps.edn\", ...], ...}, \"truncated\": bool}. \"shape\" is None for single-package repos."
  []
  (:monorepo (snapshot)))

(defn- success-envelope [result] (extension/success {:result result}))

(defn- repositories-tool
  "await repositories()
Returns {\"count\": N, \"repositories\": [{\"path\", \"branch\", \"dirty\": bool, \"changes\": bool, \"stale\": bool, \"stash_count\": N, ...}], \"truncated\": bool}."
  []
  (success-envelope (repositories)))

(defn- languages-tool
  "await languages()
Returns {\"total_files\": N, \"total_bytes\": N, \"primary\": \"clojure\", \"languages\": [{\"language\", \"files\": N, \"bytes\": N, \"files_pct\", \"bytes_pct\"}, ...], \"truncated\": bool, \"elapsed_ms\": N}. List sorted by files desc."
  []
  (success-envelope (languages)))

(defn- monorepo-tool
  "await monorepo()
Returns {\"shape\": \"polylith\"|\"workspace\"|\"submodules\"|None, \"totals\": {\"clojure\": N, ...}, \"files\": {\"clojure\": [\"path/deps.edn\", ...], ...}, \"truncated\": bool}. \"shape\" is None for single-package repos."
  []
  (success-envelope (monorepo)))

(defn- refresh!-tool
  "await refresh()
Drop the cached env snapshot and recompute. Returns the fresh snapshot."
  []
  (success-envelope (refresh!)))

(defn- env-data-symbol
  "Register an explicit envelope-returning tool var under a stable `v/` name.
   The public helper vars above stay plain Clojure functions for host callers;
   only the sandbox symbol implementation returns a tool envelope.
   Every env data symbol is an :observation (pure read); the
   inline `:tag` lets `register-extension!` populate the op registry
   without an out-of-band `vis/register-op!` doseq."
  [v sym]
  (vis/symbol v {:symbol sym :tag :observation}))

(def repositories-symbol (env-data-symbol #'repositories-tool 'repositories))

(def languages-symbol (env-data-symbol #'languages-tool 'languages))

(def monorepo-symbol (env-data-symbol #'monorepo-tool 'monorepo))

(def refresh!-symbol (env-data-symbol #'refresh!-tool 'refresh!))

;; ---------------------------------------------------------------------------
;; Project guidance surface.
;; ---------------------------------------------------------------------------

(defn main-agent-instructions
  "await main_agent_instructions()
Returns {\"found\": True, \"source\", \"path\", \"bytes\": N, \"content\"} from AGENTS.md/CLAUDE.md, else {\"found\": False}. Check found first."
  []
  (agents/instructions))

(defn- environment-warnings
  []
  ;; Keep extension load failures in `(:project ctx) :warnings`. This is not
  ;; a public `v/` tool; it is emergency context for broken extension loads.
  (vec (vis/extension-load-failures)))

(defn- main-agent-instructions-tool
  "await main_agent_instructions()
Returns {\"found\": True, \"source\", \"path\", \"bytes\": N, \"content\"} from AGENTS.md/CLAUDE.md, else {\"found\": False}. Check found first."
  []
  (success-envelope (main-agent-instructions)))

(def main-agent-instructions-symbol
  (env-data-symbol #'main-agent-instructions-tool 'main-agent-instructions))

(def environment-symbols
  [repositories-symbol languages-symbol monorepo-symbol refresh!-symbol
   main-agent-instructions-symbol])

(def ^:private FN_INDEX
  "One-line strategy for the environment fns."
  "Env strategy: read workspace/VCS facts from the `workspace` key of `<context>`; use the focused env helpers only for coarse project shape. Project guidance auto-refreshes when AGENTS.md/CLAUDE.md markers change.")

(defn environment-ctx
  "Foundation-owned structured ctx contribution. Runtime facts, project
   guidance, and extension-load warnings live under `(:project ctx)`."
  [_environment]
  (try (render/project-context (snapshot) (agents/instructions) (environment-warnings))
       (catch Throwable t
         (tel/log! {:level :error :id ::environment-ctx-failed :data {:error (ex-message t)}})
         {})))

(defn environment-prompt
  [_environment]
  (try FN_INDEX
       (catch Throwable t
         (tel/log! {:level :error :id ::prompt-render-failed :data {:error (ex-message t)}})
         "")))

;; The extension that owns all `v/`-aliased symbols is built
;; and registered by `com.blockether.vis.internal.foundation.core`,
;; not here — this namespace only exposes the symbol vec + prompt
;; fragment for the aggregator to assemble.
