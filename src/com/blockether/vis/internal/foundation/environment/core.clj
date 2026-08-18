(ns com.blockether.vis.internal.foundation.environment.core
  "vis-foundation — the agent's environment-awareness layer.

   Owns the environment facts: cwd, user, platform, shell, plus:

     * git repository facts via the git binary (root, branch, dirty status,
       submodules, worktree),
     * a bounded language scan over the working tree (top languages
       by file count, primary language),
     * monorepo / multi-package shape detection (polylith, workspace,
       submodules) by counting per-ecosystem manifests.

   Model-facing VCS/workspace truth lives in `session[\"workspace\"]`.
   Remaining helpers cover coarse project shape (`languages`,
   `monorepo`, `repositories`).

   Runtime facts are computed lazily on first access and cached per
   working-directory. The cache is invalidated automatically when
   `cwd` changes between calls, and explicitly by the HOST-ONLY
   `refresh!` — which `/reload` runs and the sandbox cannot call."
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.foundation.environment.agents :as agents]
            [com.blockether.vis.internal.foundation.environment.git :as git]
            [com.blockether.vis.internal.foundation.environment.host :as host]
            [com.blockether.vis.internal.foundation.environment.languages :as languages]
            [com.blockether.vis.internal.foundation.environment.monorepo :as monorepo]
            [com.blockether.vis.internal.foundation.environment.render :as render]
            [com.blockether.vis.internal.foundation.environment.repositories :as repositories]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [com.blockether.vis.internal.workspace :as workspace]
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

   HOST-ONLY, deliberately NOT a `v/` symbol: refreshing the environment is a
   USER action. It is registered as a `/reload` hook below, and nothing running
   inside `python_execution` can reach it — the sandbox reads the snapshot
   through `repositories()` / `languages()` / `monorepo()`, it never reloads."
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

;; Extension definition.

;; Local thin wrappers around the snapshot accessors so each `v/` callable
;; corresponds to a real var with `:doc` + `:arglists` baked in. `vis/symbol`
;; reads both straight from the var meta - no separate side-map at the
;; registration callsite.

(defn repositories
  "await repositories()
Returns {\"root\", \"count\": N, \"repositories\": [{\"path\", \"branch\", \"is_dirty\": bool, \"is_changes\": bool, \"is_stale\": bool, \"stash_count\": N, ...}], \"is_truncated\": bool}."
  []
  (:repositories (snapshot)))

(defn git
  "Git facts for the environment block (host helper, not a model tool).
Returns {\"root\", \"branch\", \"is_detached\": bool, \"is_submodules\": bool, \"is_worktree\": bool, \"stash_count\", \"upstream\", \"ahead\", \"behind\", \"is_stale\", \"is_dirty\", \"is_clean\", \"modified\", \"untracked\", \"added\", \"changed\", \"removed\", \"missing\", \"conflicting\"}, or None outside a repo."
  []
  (:git (snapshot)))

(defn languages
  "await languages()
Returns {\"total_files\": N, \"total_bytes\": N, \"primary\": \"clojure\", \"languages\": [{\"language\", \"files\": N, \"bytes\": N, \"files_pct\", \"bytes_pct\"}, ...], \"is_truncated\": bool, \"elapsed_ms\": N}. List sorted by files desc."
  []
  (:languages (snapshot)))

(defn monorepo
  "await monorepo()
Returns {\"shape\": \"polylith\"|\"workspace\"|\"submodules\"|None, \"totals\": {\"clojure\": N, ...}, \"files\": {\"clojure\": [\"path/deps.edn\", ...], ...}, \"is_truncated\": bool}. \"shape\" is None for single-package repos."
  []
  (:monorepo (snapshot)))

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

(defn- repositories-tool
  "Every git repository under the workspace root with its branch and working-tree
state — the map of a multi-checkout tree, answered in one call.
Returns {\"root\", \"count\": N, \"repositories\": [{\"path\", \"branch\", \"is_dirty\": bool, \"is_changes\": bool, \"is_stale\": bool, \"stash_count\": N, ...}], \"is_truncated\": bool}."
  []
  (success-envelope (repositories)))

(defn- languages-tool
  "What this workspace is WRITTEN IN: the primary language plus the whole
distribution by file and byte count. Takes no arguments — it reads the scan.
Returns {\"total_files\": N, \"total_bytes\": N, \"primary\": \"clojure\", \"languages\": [{\"language\", \"files\": N, \"bytes\": N, \"files_pct\", \"bytes_pct\"}, ...], \"is_truncated\": bool, \"elapsed_ms\": N}. List sorted by files desc."
  []
  (success-envelope (languages)))

(defn- monorepo-tool
  "Whether this workspace is a MONOREPO and of what shape, with the build files
that prove it.
Returns {\"shape\": \"polylith\"|\"workspace\"|\"submodules\"|None, \"totals\": {\"clojure\": N, ...}, \"files\": {\"clojure\": [\"path/deps.edn\", ...], ...}, \"is_truncated\": bool}. \"shape\" is None for single-package repos."
  []
  (success-envelope (monorepo)))

(defn- env-data-symbol
  "Register an explicit envelope-returning tool var under a stable `v/` name.
   The public helper vars above stay plain Clojure functions for host callers;
   only the sandbox symbol implementation returns a tool envelope.
   Every env data symbol is an :observation (pure read) taking NO arguments; the
   inline `:tag` lets `register-extension!` populate the op registry
   without an out-of-band `vis/register-op!` doseq.

   `description` and `result` are the model-facing pair `doc(name)` renders under
   the call line — the implementation docstring is developer documentation and
   never substitutes for either."
  [v sym description result]
  (vis/symbol v {:symbol sym :tag :observation :description description :result result}))

(def repositories-symbol
  (env-data-symbol
    #'repositories-tool
    'repositories
    (str "Every git repository under the workspace root with its branch and working-tree state — "
         "the map of a multi-checkout tree in ONE call. Takes no arguments.")
    (str "String-keyed `{root, count, repositories, is_truncated}`; each repository is "
         "`{path, branch, is_dirty, is_changes, is_stale, stash_count, …}`.")))

(def languages-symbol
  (env-data-symbol
    #'languages-tool
    'languages
    (str "What this workspace is WRITTEN IN: the primary language plus the whole distribution by "
         "file and byte count, read off the workspace scan. Takes no arguments.")
    (str
      "String-keyed `{total_files, total_bytes, primary, languages, is_truncated, elapsed_ms}`; "
      "each language is `{language, files, bytes, files_pct, bytes_pct}`, sorted by files desc.")))

(def monorepo-symbol
  (env-data-symbol
    #'monorepo-tool
    'monorepo
    (str "Whether this workspace is a MONOREPO and of what shape, with the build files that prove "
         "it. Takes no arguments.")
    (str "String-keyed `{shape, totals, files, is_truncated}`; `shape` is "
         "`polylith` | `workspace` | `submodules`, or None for a single-package repo.")))

;; Project guidance surface.

(defn main-agent-instructions
  "The project's own guidance file — AGENTS.md or CLAUDE.md — whole, with where
it was found.
Returns {\"is_found\": True, \"source\", \"path\", \"bytes\": N, \"content\", \"files\"}, else {\"is_found\": False}. Check is_found first."
  []
  (agents/instructions))

(defn- environment-warnings
  []
  ;; Keep extension load failures in `(:project ctx) :warnings`. This is not
  ;; a public `v/` tool; it is emergency context for broken extension loads.
  (vec (vis/extension-load-failures)))

(defn- main-agent-instructions-tool
  "The project's own guidance file — AGENTS.md or CLAUDE.md — whole, with where
it was found.
Returns {\"is_found\": True, \"source\", \"path\", \"bytes\": N, \"content\", \"files\"}, else {\"is_found\": False}. Check is_found first."
  []
  (success-envelope (main-agent-instructions)))

(def main-agent-instructions-symbol
  (env-data-symbol
    #'main-agent-instructions-tool
    'main-agent-instructions
    (str "The project's own guidance file — AGENTS.md or CLAUDE.md — WHOLE, with where it was "
         "found. Takes no arguments.")
    (str "String-keyed `{is_found, source, path, bytes, content, files}`; a miss is "
         "`{is_found: False}`, so check `is_found` first.")))

(def environment-symbols
  [repositories-symbol languages-symbol monorepo-symbol main-agent-instructions-symbol])

(defn environment-ctx
  "Foundation-owned structured ctx contribution. Runtime facts, project
   guidance, and extension-load warnings live under `(:project ctx)`."
  [_environment]
  (try (render/project-context (snapshot) (agents/instructions) (environment-warnings))
       (catch Throwable t
         (tel/log! {:level :error :id ::environment-ctx-failed :data {:error (ex-message t)}})
         {})))

;; The extension that owns all `v/`-aliased symbols is built
;; and registered by `com.blockether.vis.internal.foundation.core`,
;; not here — this namespace only exposes symbols and structured context.
