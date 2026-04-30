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
   [com.blockether.vis.core :as sdk]
   [com.blockether.vis.ext.foundation.environment.git :as git]
   [com.blockether.vis.ext.foundation.environment.host :as host]
   [com.blockether.vis.ext.foundation.environment.languages :as languages]
   [com.blockether.vis.ext.foundation.environment.monorepo :as monorepo]
   [com.blockether.vis.ext.foundation.environment.render :as render]
   [taoensso.telemere :as tel]))

(set! *warn-on-reflection* true)

;; ---------------------------------------------------------------------------
;; Snapshot cache. Keyed by canonical cwd so we recompute on
;; directory change. Recomputation is cheap (the language scan is
;; bounded) but redundant on every iteration of the same query, so
;; we memoize at the extension boundary.
;; ---------------------------------------------------------------------------

(def ^:private cache
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
  "Invalidate the cached snapshot. Next call to `snapshot` will
   recompute. Returns the freshly computed snapshot."
  []
  (reset! cache {:key nil :value nil})
  (snapshot))

;; ---------------------------------------------------------------------------
;; Extension definition.
;; ---------------------------------------------------------------------------

(def snapshot-symbol
  (sdk/symbol 'snapshot snapshot
    {:doc      "Full environment snapshot as a map: {:host :git :languages :monorepo}. Cached per cwd."
     :arglists '([])
     :examples ["(v/snapshot)"
                "(get-in (v/snapshot) [:git :branch])"]}))

(def git-symbol
  (sdk/symbol 'git #(:git (snapshot))
    {:doc      "Git submap of the snapshot, or nil when not in a repo. Includes :root :branch :detached? :submodules? :worktree? plus dirty-status counts."
     :arglists '([])
     :examples ["(v/git)"
                "(:branch (v/git))"]}))

(def languages-symbol
  (sdk/symbol 'languages #(:languages (snapshot))
    {:doc      "Language scan: {:total-files :total-bytes :primary :languages [...]} sorted by total bytes desc."
     :arglists '([])
     :examples ["(v/languages)"
                "(:primary (v/languages))"]}))

(def monorepo-symbol
  (sdk/symbol 'monorepo #(:monorepo (snapshot))
    {:doc      "Monorepo shape detection: {:shape :totals :files} or :shape nil for single-package repos."
     :arglists '([])
     :examples ["(v/monorepo)"
                "(:shape (v/monorepo))"]}))

(def refresh!-symbol
  (sdk/symbol 'refresh! refresh!
    {:doc      "Drop the cached snapshot and recompute. Useful after the working tree changes substantially (new files, branch checkout, etc.)."
     :arglists '([])
     :examples ["(v/refresh!)"]}))

(def render-symbol
  (sdk/symbol 'render #(render/render (snapshot))
    {:doc      "Render the current snapshot as the same `<environment>` block embedded in the system prompt. Useful for debugging or surfacing the block on demand."
     :arglists '([])
     :examples ["(println (v/render))"]}))

(def environment-symbols
  [snapshot-symbol git-symbol languages-symbol monorepo-symbol
   refresh!-symbol render-symbol])

(def ^:private FN_INDEX
  "One-line surface listing for the environment fns under the `v/`
   alias. Authored here (not auto-rendered from `:ext/symbols`)
   because the runtime no longer auto-canonicalizes symbols into
   prompt text — see
   `com.blockether.vis.internal.prompt/render-extension-prompt-block`
   for the rationale."
  (str "`v/` environment fns: (v/snapshot) (v/git) (v/languages) "
    "(v/monorepo) (v/render) (v/refresh!)"))

(defn environment-prompt
  "Renders the live `<environment>` block + a one-line surface listing
   so the model knows the alias has callable fns. Called by the system-
   prompt assembler each time the prompt is built."
  [_environment]
  (try
    (str (render/render (snapshot)) "\n\n" FN_INDEX)
    (catch Throwable t
      (tel/log! {:level :error :id ::prompt-render-failed
                 :data  {:error (ex-message t)}})
      "")))

;; The extension that owns all `v/`-aliased symbols is built
;; and registered by `com.blockether.vis.ext.foundation.core`,
;; not here — this namespace only exposes the symbol vec + prompt
;; fragment for the aggregator to assemble.
