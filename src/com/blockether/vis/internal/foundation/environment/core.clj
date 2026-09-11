(ns com.blockether.vis.internal.foundation.environment.core
  "vis-foundation — the agent's environment-awareness layer.

   Owns the environment facts: cwd, user, platform, shell, plus:

     * git repository facts via the git binary (root, branch, dirty status,
       submodules, worktree),
     * a bounded language scan over the working tree (top languages
       by file count, primary language),
     * monorepo / multi-package shape detection (polylith, workspace,
       submodules) by counting per-ecosystem manifests.

   Model-facing VCS/workspace truth lives in `session['workspace']`;
   `session['env']['project']` supplies project kind and primary language.
   Detailed scans remain host data for context and language-tool dispatch.

   Runtime facts are computed lazily on first access and cached per
   working-directory. The cache is invalidated automatically when
   `cwd` changes between calls, and explicitly by the HOST-ONLY
   `refresh!` — which `/reload` runs and the sandbox cannot call."
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.activity.presenter :as presenter]
            [com.blockether.vis.internal.context.agents :as agents]
            [com.blockether.vis.internal.foundation.environment.git :as git]
            [com.blockether.vis.internal.foundation.environment.host :as host]
            [com.blockether.vis.internal.foundation.environment.languages :as languages]
            [com.blockether.vis.internal.foundation.environment.monorepo :as monorepo]
            [com.blockether.vis.internal.foundation.environment.render :as render]
            [com.blockether.vis.internal.foundation.environment.repositories :as repositories]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [taoensso.telemere :as tel]))

;; Snapshot cache. Keyed by canonical cwd so we recompute on
;; directory change. Recomputation is cheap (the language scan is
;; bounded) but redundant on every iteration of the same turn, so
;; we memoize at the extension boundary.

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
  "Drop the cached env snapshot, forget the repository inventory, rescan project
   guidance, and return the fresh snapshot.

   HOST-ONLY: refreshing the environment is a USER action registered as a
   `/reload` hook. The sandbox receives project facts through `session`."
  []
  (reset! cache {:key nil :value nil})
  (repositories/refresh-inventory!)
  (try (agents/reload!)
       (catch Throwable t
         (tel/log! {:level :warn :id ::agents-reload-failed :data {:error (ex-message t)}})))
  (snapshot))

;; `/reload` — and only `/reload` — refreshes the environment. The snapshot is
;; cached per cwd, so a user who reshapes the tree and reloads must not keep
;; reading a stale scan. `defonce` keeps the registration idempotent across
;; `(require ... :reload)`.
(defonce ^:private _environment-reload-hook
  (extension/register-reload-hook! ::environment-refresh refresh!))

(defn git
  "Git facts for the environment block (host helper, not a model tool).
Returns {\"root\", \"branch\", \"is_detached\": bool, \"is_submodules\": bool, \"is_worktree\": bool, \"stash_count\", \"upstream\", \"ahead\", \"behind\", \"is_stale\", \"is_dirty\", \"is_clean\", \"modified\", \"untracked\", \"added\", \"changed\", \"removed\", \"missing\", \"conflicting\"}, or None outside a repo."
  []
  (:git (snapshot)))

(defn- success-envelope
  "Envelope for a sandbox env symbol. The snapshot pieces are ENGINE data —
   kebab-case keyword keys, `foo?` booleans — but the Clojure->Python boundary
   is STRINGS-ONLY and throws on the first keyword key it meets, so every one
   of these tools used to die with `non-string-key :host` instead of
   answering. `wire/->wire` is this repo's one deterministic engine->wire
   encoder (kebab->snake, `foo?` -> `is_foo`, keyword values stringified), so
   the payload crosses already string-clean and the docstrings above name the
   keys Python actually holds."
  [result]
  (extension/success {:result (wire/->wire result)}))

(defn main-agent-instructions
  "The project's own guidance file — AGENTS.md or CLAUDE.md — whole, with where
it was found.
Returns {\"is_found\": True, \"source\", \"path\", \"bytes\": N, \"content\", \"files\"}, else {\"is_found\": False}. Check is_found first."
  []
  (agents/instructions))

(defn- environment-warnings
  []
  ;; Surface project-local Python extension failures in project context.
  (vec (vis/python-extension-load-failures)))

(defn- main-agent-instructions-tool
  "The project's own guidance file — AGENTS.md or CLAUDE.md — whole, with where
it was found.
Returns {\"is_found\": True, \"source\", \"path\", \"bytes\": N, \"content\", \"files\"}, else {\"is_found\": False}. Check is_found first."
  []
  (success-envelope (main-agent-instructions)))

(def main-agent-instructions-symbol
  (vis/symbol
    #'main-agent-instructions-tool
    {:activity (presenter/for-tool :main_agent_instructions)
     :symbol 'main-agent-instructions
     :tag :observation
     :description
     (str "The project's own guidance file — AGENTS.md or CLAUDE.md — WHOLE, with where it was "
          "found. Takes no arguments.")
     :result (str "String-keyed `{is_found, source, path, bytes, content, files}`; a miss is "
                  "`{is_found: False}`, so check `is_found` first.")}))

(def environment-symbols [main-agent-instructions-symbol])

(defn environment-ctx
  "Foundation-owned structured ctx contribution. Runtime facts, project
   guidance, and Python extension warnings live under `(:project ctx)`."
  [_environment]
  (try (render/project-context (snapshot) (agents/instructions) (environment-warnings))
       (catch Throwable t
         (tel/log! {:level :error :id ::environment-ctx-failed :data {:error (ex-message t)}})
         {})))

;; The extension that owns all `v/`-aliased symbols is built
;; and registered by `com.blockether.vis.internal.foundation.core`,
;; not here — this namespace only exposes symbols and structured context.
