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
   [com.blockether.vis.ext.foundation.environment.repositories :as repositories]
   ;; Skills catalog moved to `com.blockether.vis.internal.skills` during
   ;; the May 2026 merge consolidation (the legacy
   ;; `environment.skills` namespace was removed). The internal skills
   ;; loader is API-compatible: `list-all`, `reload!`, `lookup`,
   ;; `scan-warnings` all unchanged.
   [com.blockether.vis.internal.skills :as skills]
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
;; covers host/git/languages/monorepo only — agents + skills hold
;; their own caches in their respective namespaces (plan Q8).
(defonce ^:private cache
  (atom {:key nil :value nil}))

(defn- canonical-cwd ^String []
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
  "Drop the cached snapshot and recompute. Useful after the working tree changes substantially (new files, branch checkout, etc.). Cascades into agents + skills caches so `<project-guidance>` and `<skills>` also refresh."
  ;; Cascade rationale: users editing `AGENTS.md` reach for
  ;; `(vis/refresh!)` (existing muscle memory). Without the cascade,
  ;; `<environment>` would refresh but `<project-guidance>` and
  ;; `<skills>` would stay stale until the explicit reload fns are
  ;; called. See plan caveat: `(vis/refresh!) cascades`.
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

(defn render
  "Render the current snapshot as the same `<environment>` block embedded in the system prompt. Useful for debugging or surfacing the block on demand."
  [] (render/render (snapshot)))

(defn- env-data-symbol
  "Environment helpers all return Clojure data structures; share the same
   pr-str renderers so each entry below stays a one-liner."
  [v opts]
  (vis/symbol v
    (merge {:journal-render-fn vis/render-pr-str-journal
            :channel-render-fn vis/render-pr-str-channel}
      opts)))

(defn- env-string-symbol
  "Environment helpers that already return strings (markdown render)."
  [v opts]
  (vis/symbol v
    (merge {:journal-render-fn vis/render-string-journal
            :channel-render-fn vis/render-string-channel}
      opts)))

(def snapshot-symbol
  (env-data-symbol #'snapshot
    {}))

(def repositories-symbol
  (env-data-symbol #'repositories
    {}))

(def git-symbol
  (env-data-symbol #'git
    {}))

(def languages-symbol
  (env-data-symbol #'languages
    {}))

(def monorepo-symbol
  (env-data-symbol #'monorepo
    {}))

(def refresh!-symbol
  (env-data-symbol #'refresh!
    {}))

(def render-symbol
  (env-string-symbol #'render
    {}))

;; ---------------------------------------------------------------------------
;; Project guidance + skills + scan-warnings surface (plan §3).
;; ---------------------------------------------------------------------------

(defn main-agent-instructions
  "Project guidance from AGENTS.md or CLAUDE.md fallback. Returns {:found? ...}."
  []
  (agents/instructions))

(def main-agent-instructions-symbol
  (env-data-symbol #'main-agent-instructions
    {}))

;; (v/skills) WAS HERE. Removed: enumeration is now the
;; `TURN_ACCESSIBLE_SKILLS` SYSTEM var (frozen at turn start, vec of
;; summaries, no body). Having both `(v/skills)` AND the SYSTEM var
;; trained the model to call the symbol — see conversation
;; eeaf9651-06c7-4dda-9e97-877fcef06337's `(def skills (v/skills))`
;; followed by an answer composed straight off the call result, when
;; the same data was already in the prompt's <skills> block + the
;; SYSTEM var. ONE source of truth wins; the redundant call goes.
;;
;; Surfaces still exposed: TURN_ACCESSIBLE_SKILLS for filtering and
;; `(v/reload-skills!)` for the cache-bust after editing on disk.
;; Skill body loading is INTERNAL host state: use `(load-skill! "name")`,
;; not a `v/` extension symbol.

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

(defn ^{:doc "Scan warnings vec: {:source :reason :path}. Empty when clean."
        :arglists '([])} scan-warnings
  []
  (combined-scan-warnings))

(defn reload-instructions!
  "Reload AGENTS.md / CLAUDE.md cache."
  []
  (agents/reload!))

(defn reload-skills!
  "Reload SKILL.md cache. Returns {:scanned :loaded :dropped :warnings}."
  []
  (skills/reload!))

(defn reload-extensions!
  "Reload extension registry. Returns diff: {:added :removed :reloaded :errors ...}."
  ([] (vis/reload-extensions!))
  ([opts] (vis/reload-extensions! opts)))

(def scan-warnings-symbol
  (env-data-symbol #'scan-warnings
    {}))

(def reload-instructions!-symbol
  (env-data-symbol #'reload-instructions!
    {}))

(def reload-skills!-symbol
  (env-data-symbol #'reload-skills!
    {}))

(def reload-extensions!-symbol
  (env-data-symbol #'reload-extensions!
    {}))

(def environment-symbols
  [snapshot-symbol repositories-symbol git-symbol languages-symbol monorepo-symbol
   refresh!-symbol render-symbol
   main-agent-instructions-symbol
   scan-warnings-symbol
   reload-instructions!-symbol reload-skills!-symbol
   reload-extensions!-symbol])

(def ^:private FN_INDEX
  "One-line strategy for environment fns under the `v/` alias."
  "`v/` env strategy: use v/snapshot or focused env helpers when combining runtime facts; v/render only when you need the prompt block; reload helpers after instruction/skill/extension changes.")

(defn environment-info
  "Render the foundation-owned environment_info contribution. The
   internal prompt assembler owns placement; this function owns the
   host/git/language/monorepo/multirepo snapshot text."
  [_environment]
  (try
    (render/render (snapshot))
    (catch Throwable t
      (tel/log! {:level :error :id ::environment-info-render-failed
                 :data  {:error (ex-message t)}})
      "")))

(defn environment-prompt
  [_environment]
  (try
    (let [pg-block       (render/format-project-guidance-block (agents/instructions))
          warnings       (combined-scan-warnings)
          warnings-block (render/format-scan-warnings-block warnings)
          parts          (cond-> []
                           pg-block       (conj pg-block)
                           warnings-block (conj warnings-block)
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
