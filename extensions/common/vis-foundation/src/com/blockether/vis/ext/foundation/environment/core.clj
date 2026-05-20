(ns com.blockether.vis.ext.foundation.environment.core
  "vis-foundation — the agent's environment-awareness layer.

   Owns the environment facts that used to live hard-coded in
   `com.blockether.vis.internal.prompt`: cwd, user, platform, shell, plus:

     * git repository facts via JGit (root, branch, dirty status,
       submodules, worktree),
     * a bounded language scan over the working tree (top languages
       by total bytes, primary language),
     * monorepo / multi-package shape detection (polylith, workspace,
       submodules) by counting per-ecosystem manifests.

   Surface inside `:code`:

     (v/snapshot)   ;; full structured map
     (v/git)        ;; git submap or nil
     (v/languages)  ;; language stats
     (v/refresh!)   ;; invalidate the cache (call after cd, etc.)

   The snapshot is computed lazily on first access and cached per
   working-directory. The cache is invalidated automatically when
   `cwd` changes between calls and explicitly by `(refresh!)`."
  (:require
   [clojure.string :as string]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.foundation.environment.agents :as agents]
   [com.blockether.vis.ext.foundation.environment.git :as git]
   [com.blockether.vis.ext.foundation.environment.host :as host]
   [com.blockether.vis.ext.foundation.environment.languages :as languages]
   [com.blockether.vis.ext.foundation.environment.monorepo :as monorepo]
   [com.blockether.vis.ext.foundation.environment.render :as render]
   [com.blockether.vis.ext.foundation.environment.repositories :as repositories]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.workspace :as workspace]
   [taoensso.telemere :as tel]))

(set! *warn-on-reflection* true)

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
  ;; Production: channel rebinds *workspace-root* per turn (PLAN.md §5).
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

(defn- snapshot-tool
  "Full environment snapshot as a map: {:host :git :languages :monorepo :repositories}. Cached per cwd; recomputed automatically when cwd changes or after `(refresh!)`."
  []
  (success-envelope (snapshot)))

(defn- repositories-tool
  "Multirepo Git snapshot: {:count :repositories [{:path :branch :dirty? :changes? :stale? :stash-count ...}]} - returned in a canonical tool envelope."
  []
  (success-envelope (repositories)))

(defn- git-tool
  "Git submap of the snapshot, or nil when not in a repo. Includes :root :branch :detached? :submodules? :worktree? plus dirty-status counts; returned in a canonical tool envelope."
  []
  (success-envelope (git)))

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
(defn- ir-code [s] [:c {} (str s)])
(defn- ir-p [& children] (into [:p {}] children))
(defn- ir-cell [tag v] [tag {} (ir-text (present v))])
(defn- ir-row [cell-tag values]
  (into [:tr {}] (map #(ir-cell cell-tag %) values)))

(defn- lines-channel
  [title lines]
  (into [:ir {}
         (ir-p (ir-code title))]
    (when (seq lines)
      [(into [:ul {}]
         (map (fn [line] [:li {} (ir-p (ir-text line))]) lines))])))

(defn- render-snapshot-channel
  [result]
  (lines-channel "v/snapshot" (snapshot-lines result)))

(defn- render-refresh-channel
  [result]
  (lines-channel "v/refresh! refreshed environment" (snapshot-lines result)))

(defn- render-git-channel
  [result]
  (lines-channel "v/git" [(git-summary result) (when result (str "root " (present (:root result))))]))

(defn- render-languages-channel
  [result]
  (lines-channel "v/languages" [(language-summary result)]))

(defn- render-monorepo-channel
  [result]
  (lines-channel "v/monorepo" [(monorepo-summary result)]))

(defn- render-repositories-channel
  [result]
  (let [repos (take 10 (:repositories result))]
    (lines-channel "v/repositories"
      (cons (repositories-summary result)
        (map (fn [{:keys [path branch dirty? clean?]}]
               (str (present path) " / " (present branch) " / "
                 (cond dirty? "dirty" clean? "clean" :else "unknown")))
          repos)))))

(defn- guidance-summary
  [{:keys [found? source path content]}]
  (if found?
    (str "guidance " (present source) " / " (present path)
      " / " (count (string/split-lines (or content ""))) " line(s)")
    "guidance not found"))

(defn- render-guidance-channel
  [{:keys [content] :as result}]
  (into [:ir {}
         (ir-p (ir-code "v/main-agent-instructions") (ir-text (str " — " (guidance-summary result))))]
    (when (seq content)
      [[:code {:lang "text"} content]])))

(defn- reload-summary
  [result]
  (str "added " (count (:added result))
    " / removed " (count (:removed result))
    " / reloaded " (count (:reloaded result))
    " / failed " (count (:failed result))
    " / unchanged " (count (:unchanged result))
    " / errors " (count (:errors result))))

(defn- reload-summary-table
  [result]
  [:table {}
   (ir-row :th ["Metric" "Value"])
   (ir-row :td ["engine" (or (some-> (:reload-engine result) name) "require-reload")])
   (ir-row :td ["plan" (or (some-> (:reload-plan result) name) "all")])
   (ir-row :td ["added" (count (:added result))])
   (ir-row :td ["removed" (count (:removed result))])
   (ir-row :td ["reloaded" (count (:reloaded result))])
   (ir-row :td ["failed" (count (:failed result))])
   (ir-row :td ["unchanged" (count (:unchanged result))])
   (ir-row :td ["errors" (count (:errors result))])
   (ir-row :td ["duration-ms" (:duration-ms result)])
   (ir-row :td ["env refresh" (str (name (get-in result [:env-refresh :status] :unknown))
                                " / "
                                (get-in result [:env-refresh :scheduled] 0)
                                " scheduled / "
                                (name (get-in result [:env-refresh :when] :unknown)))])])

(defn- reload-action-rows
  [result]
  (concat
    (map (fn [ns-sym] [(str ns-sym) "added"]) (:added result))
    (map (fn [ns-sym] [(str ns-sym) "removed"]) (:removed result))
    (map (fn [ns-sym] [(str ns-sym) "reloaded"]) (:reloaded result))
    (map (fn [ns-sym] [(str ns-sym) "failed"]) (:failed result))
    (map (fn [ns-sym] [(str ns-sym) "unchanged"]) (:unchanged result))))

(defn- truncate-cell
  [s n]
  (let [s (present s)]
    (if (> (count s) n)
      (str (subs s 0 n) "…")
      s)))

(defn- reload-errors-table
  [result]
  (let [rows (take 5 (:errors result))]
    (when (seq rows)
      (into [:table {}
             (ir-row :th ["Namespace" "Phase" "Reason"])]
        (map (fn [{:keys [ns phase reason]}]
               (ir-row :td [(str ns) (name phase) (truncate-cell reason 120)]))
          rows)))))

(defn- reload-actions-table
  [result]
  (let [rows (take 12 (reload-action-rows result))]
    (when (seq rows)
      (into [:table {}
             (ir-row :th ["Namespace" "Action"])]
        (map #(ir-row :td %) rows)))))

(defn- reload-noisy?
  "Happy path is silent (no errors, no failures, no env-reseat-skipped,
   no blocked-ms wait, env-refresh status fine). Anything else earns the
   table + per-ns breakdown."
  [result]
  (or (seq (:errors result))
    (seq (:failed result))
    (seq (:env-reseat-skipped result))
    (pos? (long (or (:blocked-ms result) 0)))
    (let [s (get-in result [:env-refresh :status])]
      (and s (not (#{:scheduled :idle :ok} s))))))

(defn- reload-happy-line
  "One-line summary for the happy path. Mentions counts that matter and
   silently elides the rest."
  [result]
  (let [parts (cond-> []
                (seq (:reloaded result))  (conj (str (count (:reloaded result)) " reloaded"))
                (seq (:added result))     (conj (str (count (:added result)) " added"))
                (seq (:removed result))   (conj (str (count (:removed result)) " removed"))
                (seq (:unchanged result)) (conj (str (count (:unchanged result)) " unchanged")))
        body  (if (seq parts) (string/join ", " parts) "no changes")
        env-q (get-in result [:env-refresh :scheduled] 0)]
    (str " — " body
      (when (:duration-ms result) (str " in " (:duration-ms result) "ms"))
      (when (pos? (long env-q)) (str "; " env-q " env queued for next turn")))))

(defn- render-reload-channel
  [result]
  (if-not (reload-noisy? result)
    ;; Happy path: single line. Tables and bullets earn their space
    ;; only when something is off.
    [:ir {} (ir-p (ir-code "v/reload-extensions!") (ir-text (reload-happy-line result)))]
    ;; Off-nominal: full diagnostic surface.
    (into [:ir {}
           (ir-p (ir-code "v/reload-extensions!")
             (ir-text (str " — " (reload-summary result))))
           (reload-summary-table result)]
      (remove nil? [(reload-errors-table result)
                    (reload-actions-table result)]))))

(defn- env-renderers
  [sym]
  (case sym
    snapshot                {:render-fn render-snapshot-channel}
    refresh!                {:render-fn render-refresh-channel}
    git                     {:render-fn render-git-channel}
    languages               {:render-fn render-languages-channel}
    monorepo                {:render-fn render-monorepo-channel}
    repositories            {:render-fn render-repositories-channel}
    main-agent-instructions {:render-fn render-guidance-channel}
    reload-extensions!      {:render-fn render-reload-channel}))

(defn- env-data-symbol
  "Register an explicit envelope-returning tool var under a stable `v/` name.
   The public helper vars above stay plain Clojure functions for host callers;
   only the SCI symbol implementation returns a tool envelope."
  [v sym]
  (vis/symbol v
    (assoc (env-renderers sym) :symbol sym)))

(def snapshot-symbol
  (env-data-symbol #'snapshot-tool 'snapshot))

(def repositories-symbol
  (env-data-symbol #'repositories-tool 'repositories))

(def git-symbol
  (env-data-symbol #'git-tool 'git))

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

(defn reload-extensions!
  "Reload extension registry. Returns diff: {:added :removed :reloaded :errors ...}."
  ([] (vis/reload-extensions!))
  ([opts] (vis/reload-extensions! opts)))

(defn- main-agent-instructions-tool
  "Project guidance from AGENTS.md or CLAUDE.md fallback, returned in a canonical tool envelope."
  []
  (success-envelope (main-agent-instructions)))

(defn- reload-extensions!-tool
  "Reload extension registry. Returns diff in a canonical tool envelope: {:added :removed :reloaded :errors ...}."
  ([]
   (success-envelope (reload-extensions!)))
  ([opts]
   (success-envelope (reload-extensions! opts))))

(def main-agent-instructions-symbol
  (env-data-symbol #'main-agent-instructions-tool 'main-agent-instructions))

(def reload-extensions!-symbol
  (env-data-symbol #'reload-extensions!-tool 'reload-extensions!))

(def environment-symbols
  [snapshot-symbol repositories-symbol git-symbol languages-symbol monorepo-symbol
   refresh!-symbol
   main-agent-instructions-symbol
   reload-extensions!-symbol])

(def ^:private FN_INDEX
  "One-line strategy for environment fns under the `v/` alias."
  "`v/` env strategy: use v/snapshot or focused env helpers when combining runtime facts; project guidance auto-refreshes when AGENTS.md/CLAUDE.md markers change; use v/reload-extensions! only after extension changes.")

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
;; and registered by `com.blockether.vis.ext.foundation.core`,
;; not here — this namespace only exposes the symbol vec + prompt
;; fragment for the aggregator to assemble.
