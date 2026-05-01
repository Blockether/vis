(ns com.blockether.vis.ext.foundation.environment.core
  "vis-foundation — the agent's environment-awareness layer.

   Owns the `<environment>` block that used to live hard-coded in
   `com.blockether.vis.internal.prompt`. Everything in there
   (cwd, user, platform, shell) is still reported, plus:

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
   [com.blockether.vis.ext.foundation.environment.skills :as skills]
   [taoensso.telemere :as tel]))

(set! *warn-on-reflection* true)

;; ---------------------------------------------------------------------------
;; Snapshot cache. Keyed by canonical cwd so we recompute on
;; directory change. Recomputation is cheap (the language scan is
;; bounded) but redundant on every iteration of the same query, so
;; we memoize at the extension boundary.
;; ---------------------------------------------------------------------------

;; `defonce` so the atom survives a `(require :reload)` during an
;; extension reload (per plan caveat: extensions holding mutable
;; state across reload MUST use defonce). The cwd-keyed snapshot
;; covers host/git/languages/monorepo only — agents + skills hold
;; their own caches in their respective namespaces (plan Q8).
(defonce ^:private cache
  (atom {:key nil :value nil}))

(defn- canonical-cwd ^String []
  (try
    (.getCanonicalPath (java.io.File. (System/getProperty "user.dir")))
    (catch Throwable _ (System/getProperty "user.dir"))))

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
                       nil))]
    {:host      host-map
     :git       git-map
     :languages langs-map
     :monorepo  mono-map}))

(defn snapshot
  "Return the cached snapshot, computing it on first access or after
   a `(refresh!)`. Recomputed automatically when cwd changes."
  []
  (let [cwd     (canonical-cwd)
        cached  @cache]
    (if (= cwd (:key cached))
      (:value cached)
      (let [value (compute-snapshot cwd)]
        (reset! cache {:key cwd :value value})
        value))))

(defn refresh!
  "Invalidate the cached snapshot AND cascade into the agents +
   skills caches. Next call to `snapshot` (and `(v/main-agent-instructions)`,
   the `TURN_ACCESSIBLE_SKILLS` snapshot) will recompute. Returns the
   freshly computed snapshot.

   Cascade rationale: users editing `AGENTS.md` reach for
   `(vis/refresh!)` (existing muscle memory). Without the cascade,
   `<environment>` would refresh but `<project-guidance>` and
   `<skills>` would stay stale until the explicit reload fns are
   called. See plan caveat: `(vis/refresh!) cascades`."
  []
  (reset! cache {:key nil :value nil})
  (try (agents/reload!)
    (catch Throwable t
      (tel/log! {:level :warn :id ::agents-reload-failed
                 :data  {:error (ex-message t)}})))
  (try (skills/reload!)
    (catch Throwable t
      (tel/log! {:level :warn :id ::skills-reload-failed
                 :data  {:error (ex-message t)}})))
  (snapshot))

;; ---------------------------------------------------------------------------
;; Extension definition.
;; ---------------------------------------------------------------------------

(def snapshot-symbol
  (vis/symbol 'snapshot snapshot
    {:doc      "Full environment snapshot as a map: {:host :git :languages :monorepo}. Cached per cwd."
     :arglists '([])
     :examples ["(v/snapshot)"
                "(get-in (v/snapshot) [:git :branch])"]}))

(def git-symbol
  (vis/symbol 'git #(:git (snapshot))
    {:doc      "Git submap of the snapshot, or nil when not in a repo. Includes :root :branch :detached? :submodules? :worktree? plus dirty-status counts."
     :arglists '([])
     :examples ["(v/git)"
                "(:branch (v/git))"]}))

(def languages-symbol
  (vis/symbol 'languages #(:languages (snapshot))
    {:doc      "Language scan: {:total-files :total-bytes :primary :languages [...]} sorted by total bytes desc."
     :arglists '([])
     :examples ["(v/languages)"
                "(:primary (v/languages))"]}))

(def monorepo-symbol
  (vis/symbol 'monorepo #(:monorepo (snapshot))
    {:doc      "Monorepo shape detection: {:shape :totals :files} or :shape nil for single-package repos."
     :arglists '([])
     :examples ["(v/monorepo)"
                "(:shape (v/monorepo))"]}))

(def refresh!-symbol
  (vis/symbol 'refresh! refresh!
    {:doc      "Drop the cached snapshot and recompute. Useful after the working tree changes substantially (new files, branch checkout, etc.)."
     :arglists '([])
     :examples ["(v/refresh!)"]}))

(def render-symbol
  (vis/symbol 'render #(render/render (snapshot))
    {:doc      "Render the current snapshot as the same `<environment>` block embedded in the system prompt. Useful for debugging or surfacing the block on demand."
     :arglists '([])
     :examples ["(println (v/render))"]}))

;; ---------------------------------------------------------------------------
;; Project guidance + skills + scan-warnings surface (plan §3).
;; ---------------------------------------------------------------------------

(def main-agent-instructions-symbol
  (vis/symbol 'main-agent-instructions agents/instructions
    {:doc      "Project guidance loaded from <repo>/AGENTS.md (or <repo>/CLAUDE.md fallback). Always returns a map. {:found? true :source :repo|:repo:claude-md-fallback :path \"...\" :bytes N :content \"...\" :truncated? bool :original-bytes N} OR {:found? false}."
     :arglists '([])
     :examples ["(v/main-agent-instructions)"
                "(:content (v/main-agent-instructions))"]}))

;; (v/skills) WAS HERE. Removed: enumeration is now the
;; `TURN_ACCESSIBLE_SKILLS` SYSTEM var (frozen at turn start, vec of
;; summaries, no body). Having both `(v/skills)` AND the SYSTEM var
;; trained the model to call the symbol — see conversation
;; eeaf9651-06c7-4dda-9e97-877fcef06337's `(def skills (v/skills))`
;; followed by an answer composed straight off the call result, when
;; the same data was already in the prompt's <skills> block + the
;; SYSTEM var. ONE source of truth wins; the redundant call goes.
;;
;; Surfaces still exposed: TURN_ACCESSIBLE_SKILLS for filtering,
;; `(v/load-skill "name")` for activation (loads body),
;; `(v/reload-skills!)` for the cache-bust after editing on disk.

(def load-skill-symbol
  (vis/symbol 'load-skill skills/lookup
    {:doc      "Load one skill's full body by name. This is the activation step — TURN_ACCESSIBLE_SKILLS only carries `:name`/`:description` summaries; `(v/load-skill name)` returns the full SKILL.md content under `:body`. Always returns a map. Present: {:found? true :name :description :path :source :body :extra}. Absent: {:found? false :name <queried>}."
     :arglists '([skill-name])
     :examples ["(v/load-skill \"diagnose\")"
                "(:body (v/load-skill \"caveman\"))"]}))

(defn- combined-scan-warnings []
  ;; Three sources, all `{:source :reason :path}` shaped so the
  ;; renderer can splice them into one `<scan-warnings>` block:
  ;;
  ;;   (a) AGENTS.md / CLAUDE.md read failures              — agents/scan-warnings
  ;;   (b) SKILL.md frontmatter / shape rejections           — skills/scan-warnings
  ;;   (c) Extension namespace `(require)` failures collected
  ;;       during classpath discovery                        — vis/extension-load-failures
  ;;
  ;; (c) is the load-bearing addition. Pre-fix a single typo in any
  ;; extension source file silently disabled its alias namespace
  ;; (`v/`, `z/`, `clj/`, …). The user saw nothing; the LLM saw
  ;; "Unable to resolve symbol" forever (conversation
  ;; d8aff512-d60d-42b6-a009-041f1bec3891 burned 200+ blocks on this).
  ;; Surfacing the failure here puts the actual root cause — "foundation.core
  ;; failed to load: Syntax error reading source at markdown.clj:328:17" —
  ;; into the system prompt where the model will read it.
  (vec (concat (agents/scan-warnings)
         (skills/scan-warnings)
         (vis/extension-load-failures))))

(def scan-warnings-symbol
  (vis/symbol 'scan-warnings combined-scan-warnings
    {:doc      "Vec of warnings detected scanning project guidance + skills frontmatter. Each entry: {:source :reason :path}. Empty vec when clean. Cleared on next reload after fixing the file."
     :arglists '([])
     :examples ["(v/scan-warnings)"
                "(when (seq (v/scan-warnings)) :issues)"]}))

(def reload-instructions!-symbol
  (vis/symbol 'reload-instructions! agents/reload!
    {:doc      "Re-scan AGENTS.md / CLAUDE.md from disk and update the cache. Returns the fresh map. Use after editing the file mid-session."
     :arglists '([])
     :examples ["(v/reload-instructions!)"]}))

(def reload-skills!-symbol
  (vis/symbol 'reload-skills! skills/reload!
    {:doc      "Re-scan SKILL.md files from disk and update the cache. Returns {:scanned N :loaded M :dropped K :warnings [...]}. Use after editing skill files / installing new skills mid-session."
     :arglists '([])
     :examples ["(v/reload-skills!)"]}))

(def reload-extensions!-symbol
  (vis/symbol 'reload-extensions! vis/reload-extensions!
    {:doc      "Re-discover extensions on the classpath, diff against current registry, surgically apply :added / :removed / :reloaded. F1-lite: every still-present extension is re-required + re-registered (no change-detection in v1). Continue-on-error per-extension; the calling env's reseat is deferred until the current iteration completes. Returns the diff summary: {:added :removed :reloaded :errors :envs-reseated :env-reseat-deferred :env-reseat-skipped :duration-ms :blocked-ms}."
     :arglists '([] [{:reload/timeout-ms n}])
     :examples ["(v/reload-extensions!)"
                "(v/reload-extensions! {:reload/timeout-ms 5000})"]}))

(def environment-symbols
  [snapshot-symbol git-symbol languages-symbol monorepo-symbol
   refresh!-symbol render-symbol
   main-agent-instructions-symbol load-skill-symbol
   scan-warnings-symbol
   reload-instructions!-symbol reload-skills!-symbol
   reload-extensions!-symbol])

(def ^:private FN_INDEX
  "One-line surface listing for the environment fns under the `v/`
   alias. Authored here (not auto-rendered from `:ext/symbols`)
   because the runtime no longer auto-canonicalizes symbols into
   prompt text — see
   `com.blockether.vis.internal.prompt/render-extension-prompt-block`
   for the rationale."
  (str "`v/` environment fns: (v/snapshot) (v/git) (v/languages) "
    "(v/monorepo) (v/render) (v/refresh!)"
    " | project-guidance + skills: (v/main-agent-instructions) (v/load-skill \"name\") (v/scan-warnings)"
    " | reload: (v/reload-instructions!) (v/reload-skills!) (v/reload-extensions!)"))

(defn environment-prompt
  "Renders the live foundation block: <project-guidance> (when
   present) → <environment> → <scan-warnings> (when issues exist)
   → <skills> (when populated) → FN_INDEX. Each XML block is
   conditionally rendered; absent sources contribute nothing.
   Called by the system-prompt assembler each time the prompt is
   built."
  [_environment]
  (try
    (let [pg-block       (render/format-project-guidance-block (agents/instructions))
          env-block      (render/render (snapshot))
          warnings       (combined-scan-warnings)
          warnings-block (render/format-scan-warnings-block warnings)
          skills-list    (skills/list-all)
          skills-block   (render/format-skills-block skills-list)
          parts          (cond-> []
                           pg-block       (conj pg-block)
                           true           (conj env-block)
                           warnings-block (conj warnings-block)
                           skills-block   (conj skills-block)
                           true           (conj FN_INDEX))]
      (string/join "\n\n" parts))
    (catch Throwable t
      (tel/log! {:level :error :id ::prompt-render-failed
                 :data  {:error (ex-message t)}})
      "")))

;; The extension that owns all `v/`-aliased symbols is built
;; and registered by `com.blockether.vis.ext.foundation.core`,
;; not here — this namespace only exposes the symbol vec + prompt
;; fragment for the aggregator to assemble.
