(ns com.blockether.vis.ext.foundation-core.environment.core
  "vis-foundation — the agent's environment-awareness layer.

   Owns the environment facts that used to live hard-coded in
   `com.blockether.vis.internal.prompt`: cwd, user, platform, shell, plus:

     * git repository facts via JGit (root, branch, dirty status,
       submodules, worktree),
     * a bounded language scan over the working tree (top languages
       by total bytes, primary language),
     * monorepo / multi-package shape detection (polylith, workspace,
       submodules) by counting per-ecosystem manifests.

   Model-facing VCS/workspace truth lives in `:session/workspace` CTX.
   Remaining helpers cover coarse project shape (`v/languages`,
   `v/monorepo`, `v/repositories`) and cache invalidation (`v/refresh!`).

   Runtime facts are computed lazily on first access and cached per
   working-directory. The cache is invalidated automatically when
   `cwd` changes between calls and explicitly by `(refresh!)`."
  (:require
   [clojure.string :as string]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.foundation-core.environment.agents :as agents]
   [com.blockether.vis.ext.foundation-core.environment.git :as git]
   [com.blockether.vis.ext.foundation-core.environment.host :as host]
   [com.blockether.vis.ext.foundation-core.environment.languages :as languages]
   [com.blockether.vis.ext.foundation-core.environment.monorepo :as monorepo]
   [com.blockether.vis.ext.foundation-core.environment.render :as render]
   [com.blockether.vis.ext.foundation-core.environment.repositories :as repositories]
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
(defonce ^:private cache
  (atom {:key nil :value nil}))

(defn- canonical-cwd ^String []
  ;; Production: channel rebinds *workspace-root* per turn.
  ;; The try/catch covers REPL / test paths where no binding exists.
  (try
    (.getCanonicalPath (workspace/cwd))
    (catch Throwable _
      (or workspace/*workspace-root*
        (System/getProperty "user.dir")))))

(defn- compute-snapshot
  "Build the full snapshot map. Each piece is independently guarded
   so a failure in one section never poisons the others."
  [^String cwd]
  (let [cwd-file   (java.io.File. cwd)
        host-map   (try (host/snapshot)
                     (catch Throwable t
                       (tel/log! {:level :warn :id ::host-failed
                                  :data  {:error (ex-message t)}})
                       {}))
        git-map    (try (git/snapshot cwd-file)
                     (catch Throwable t
                       (tel/log! {:level :warn :id ::git-failed
                                  :data  {:error (ex-message t)}})
                       nil))
        scan-root  (or (some-> ^String (:root git-map) (java.io.File.))
                     cwd-file)
        langs-map  (try (languages/scan scan-root)
                     (catch Throwable t
                       (tel/log! {:level :warn :id ::languages-failed
                                  :data  {:error (ex-message t)}})
                       nil))
        mono-map   (try (monorepo/snapshot scan-root)
                     (catch Throwable t
                       (tel/log! {:level :warn :id ::monorepo-failed
                                  :data  {:error (ex-message t)}})
                       nil))
        repos-map  (try (repositories/snapshot scan-root)
                     (catch Throwable t
                       (tel/log! {:level :warn :id ::repositories-failed
                                  :data  {:error (ex-message t)}})
                       nil))]
    {:host         host-map
     :git          git-map
     :languages    langs-map
     :monorepo     mono-map
     :repositories repos-map}))

(defn snapshot
  "Full environment snapshot as a map: {:host :git :languages :monorepo :repositories}. Cached per cwd; recomputed automatically when cwd changes or after `(refresh!)`."
  []
  (let [cwd     (canonical-cwd)
        cached  @cache]
    (if (= cwd (:key cached))
      (:value cached)
      (let [value (compute-snapshot cwd)]
        (reset! cache {:key cwd :value value})
        value))))

(defn refresh!
  "Drop the cached snapshot and recompute. Useful after the working tree changes substantially (new files, branch checkout, etc.). Cascades into the project-guidance cache."
  []
  (reset! cache {:key nil :value nil})
  (try (agents/reload!)
    (catch Throwable t
      (tel/log! {:level :warn :id ::agents-reload-failed
                 :data  {:error (ex-message t)}})))
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
  "Multirepo Git snapshot: {:count :repositories [{:path :branch :dirty? :changes? :stale? :stash-count ...}]}."
  [] (:repositories (snapshot)))

(defn git
  "Git submap of the snapshot, or nil when not in a repo. Includes :root :branch :detached? :submodules? :worktree? plus dirty-status counts."
  [] (:git (snapshot)))

(defn languages
  "Language scan: {:total-files :total-bytes :primary :languages [...]} sorted by total bytes desc."
  [] (:languages (snapshot)))

(defn monorepo
  "Monorepo shape detection: {:shape :totals :files} or :shape nil for single-package repos."
  [] (:monorepo (snapshot)))

(defn- success-envelope
  [result]
  (extension/success {:result result}))

(defn- repositories-tool
  "Multirepo Git snapshot: {:count :repositories [{:path :branch :dirty? :changes? :stale? :stash-count ...}]} - returned in a canonical tool envelope."
  []
  (success-envelope (repositories)))

(defn- languages-tool
  "Language scan: {:total-files :total-bytes :primary :languages [...]} sorted by total bytes desc; returned in a canonical tool envelope."
  []
  (success-envelope (languages)))

(defn- monorepo-tool
  "Monorepo shape detection: {:shape :totals :files} or :shape nil for single-package repos; returned in a canonical tool envelope."
  []
  (success-envelope (monorepo)))

(defn- refresh!-tool
  "Drop the cached snapshot and recompute. Returns the refreshed snapshot in a canonical tool envelope."
  []
  (success-envelope (refresh!)))

(defn- present
  [v]
  (let [s (str v)]
    (if (string/blank? s) "-" s)))

(defn- git-status
  [git-map]
  (cond
    (nil? git-map) "not in git repo"
    (:dirty? git-map) "dirty"
    (:clean? git-map) "clean"
    :else "unknown"))

(defn- git-summary
  [git-map]
  (if git-map
    (str "git " (present (:branch git-map))
      " / " (git-status git-map)
      (when-let [n (:modified git-map)] (str " / modified " n))
      (when-let [n (:untracked git-map)] (str " / untracked " n))
      (when-let [n (:ahead git-map)] (when (pos? (long n)) (str " / ahead " n)))
      (when-let [n (:behind git-map)] (when (pos? (long n)) (str " / behind " n))))
    "git unavailable"))

(defn- language-summary
  [languages-map]
  (if languages-map
    (let [top (->> (:languages languages-map)
                (take 5)
                (map (fn [{:keys [language files bytes-pct]}]
                       (str (present language) " " (or files 0) " file(s)"
                         (when bytes-pct (format " %.1f%%%%" (double bytes-pct))))))
                (string/join ", "))]
      (str "languages " (present (:primary languages-map))
        " / " (or (:total-files languages-map) 0) " file(s)"
        (when (seq top) (str " / top " top))))
    "languages unavailable"))

(defn- repositories-summary
  [repositories-map]
  (if repositories-map
    (str "repositories " (or (:count repositories-map) 0)
      (when (:truncated? repositories-map) " / truncated"))
    "repositories unavailable"))

(defn- monorepo-summary
  [monorepo-map]
  (if-let [shape (:shape monorepo-map)]
    (str "monorepo " (name shape))
    "monorepo none detected"))

(defn- snapshot-lines
  [{:keys [host git languages monorepo repositories]}]
  (remove nil?
    [(str "cwd " (present (:cwd host)))
     (git-summary git)
     (language-summary languages)
     (monorepo-summary monorepo)
     (repositories-summary repositories)]))

(defn- ir-text [s] [:span {} (str s)])
(defn- ir-strong [s] [:strong {} (str s)])
(defn- ir-p [& children] (into [:p {}] children))

(defn- badge-channel
  "Badge preview: `[:strong BADGE]  <inline summary>` followed by
   an optional `[:ul]` of lines. Replaces the legacy `lines-channel`
   helper that prefixed every preview with `[:c \"v/foo\"]`."
  [badge lines]
  (let [first-line (some-> lines first)
        rest-lines (rest lines)]
    (into [:ir {}
           (ir-p (ir-strong badge)
             (when first-line (ir-text (str "  " first-line))))]
      (when (seq rest-lines)
        [(into [:ul {}]
           (map (fn [line] [:li {} (ir-p (ir-text line))]) rest-lines))]))))

(defn- render-refresh-channel
  [result]
  (badge-channel "REFRESH" (snapshot-lines result)))

(defn- render-languages-channel
  [result]
  (badge-channel "LANGUAGES" [(language-summary result)]))

(defn- render-monorepo-channel
  [result]
  (badge-channel "MONOREPO" [(monorepo-summary result)]))

(defn- render-repositories-channel
  [result]
  (let [repos (take 10 (:repositories result))]
    (badge-channel "REPOS"
      (cons (repositories-summary result)
        (map (fn [{:keys [path branch dirty? clean?]}]
               (str (present path) " / " (present branch) " / "
                 (cond dirty? "dirty" clean? "clean" :else "unknown")))
          repos)))))

(defn- guidance-summary
  [{:keys [found? source path content]}]
  (if found?
    (str (present source) " / " (present path)
      " / " (count (string/split-lines (or content ""))) " line(s)")
    "not found"))

(defn- render-guidance-channel
  [{:keys [content found?] :as result}]
  (into [:ir {}
         (ir-p (ir-strong (if found? "GUIDANCE" "NO GUIDANCE"))
           (ir-text (str "  " (guidance-summary result))))]
    (when (seq content)
      [[:code {:lang "text"} content]])))

(defn- env-renderers
  [sym]
  (case sym
    refresh!                {:render-fn render-refresh-channel}
    languages               {:render-fn render-languages-channel}
    monorepo                {:render-fn render-monorepo-channel}
    repositories            {:render-fn render-repositories-channel}
    main-agent-instructions {:render-fn render-guidance-channel}))

(defn- env-data-symbol
  "Register an explicit envelope-returning tool var under a stable `v/` name.
   The public helper vars above stay plain Clojure functions for host callers;
   only the SCI symbol implementation returns a tool envelope.
   Every env data symbol is an :observation (pure read); the
   inline `:tag` lets `register-extension!` populate the op registry
   without an out-of-band `vis/register-op!` doseq."
  [v sym]
  (vis/symbol v
    (assoc (env-renderers sym) :symbol sym :tag :observation)))

(def repositories-symbol
  (env-data-symbol #'repositories-tool 'repositories))

(def languages-symbol
  (env-data-symbol #'languages-tool 'languages))

(def monorepo-symbol
  (env-data-symbol #'monorepo-tool 'monorepo))

(def refresh!-symbol
  (env-data-symbol #'refresh!-tool 'refresh!))

;; ---------------------------------------------------------------------------
;; Project guidance surface.
;; ---------------------------------------------------------------------------

(defn main-agent-instructions
  "Project guidance from AGENTS.md or CLAUDE.md fallback. Returns {:found? ...}."
  []
  (agents/instructions))

(defn- environment-warnings []
  ;; Keep extension load failures in `(:project ctx) :warnings`. This is not
  ;; a public `v/` tool; it is emergency context for broken extension loads.
  (vec (vis/extension-load-failures)))

(defn- main-agent-instructions-tool
  "Project guidance from AGENTS.md or CLAUDE.md fallback, returned in a canonical tool envelope."
  []
  (success-envelope (main-agent-instructions)))

(def main-agent-instructions-symbol
  (env-data-symbol #'main-agent-instructions-tool 'main-agent-instructions))

(def environment-symbols
  [repositories-symbol languages-symbol monorepo-symbol
   refresh!-symbol
   main-agent-instructions-symbol])

(def ^:private FN_INDEX
  "One-line strategy for environment fns under the `v/` alias."
  "`v/` env strategy: read workspace/VCS facts from `:session/workspace` CTX; use focused env helpers only for coarse project shape. Project guidance auto-refreshes when AGENTS.md/CLAUDE.md markers change.")

(defn environment-ctx
  "Foundation-owned structured ctx contribution. Runtime facts, project
   guidance, and extension-load warnings live under `(:project ctx)`."
  [_environment]
  (try
    (render/project-context (snapshot) (agents/instructions) (environment-warnings))
    (catch Throwable t
      (tel/log! {:level :error :id ::environment-ctx-failed
                 :data  {:error (ex-message t)}})
      {})))

(defn environment-prompt
  [_environment]
  (try
    FN_INDEX
    (catch Throwable t
      (tel/log! {:level :error :id ::prompt-render-failed
                 :data  {:error (ex-message t)}})
      "")))

;; The extension that owns all `v/`-aliased symbols is built
;; and registered by `com.blockether.vis.ext.foundation-core.core`,
;; not here — this namespace only exposes the symbol vec + prompt
;; fragment for the aggregator to assemble.
