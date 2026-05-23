(ns com.blockether.vis.ext.language-clojure.core
  "vis-language-clojure — Clojure-specific tooling for Vis.

   Activates ONLY when the workspace language scan (foundation-core)
   detects Clojure files. Surfaces a small `clj/` alias inside the
   SCI sandbox:

     (clj/ports)    -> discover nREPL ports (workspace + home)
     (clj/eval ...) -> eval in a running nREPL (per-port conn pool)
     (clj/outline path)            -> collapsed file view
     (clj/find {:name regex ...})  -> structured def search
     (clj/edit {:path :op :target :code}) -> rewrite-clj edit

   No surface duplicates foundation-core's `v/*`. `v/cat`/`v/rg`/
   `v/patch` stay the right answer for everything Clojure-agnostic.

   The SCI surface registered here exposes `clj/eval` and `clj/find`.
   Inside this namespace the matching vars (`eval`, `find`) shadow
   `clojure.core/eval` and `clojure.core/find`; that is intentional
   — we never use the core fns here, and `:refer-clojure :exclude`
   silences the load-time warning so the build log stays clean."
  (:refer-clojure :exclude [eval find])
  (:require
   [clojure.java.io :as io]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.foundation-core.environment.languages :as languages]
   [com.blockether.vis.ext.language-clojure.edit :as edit]
   [com.blockether.vis.ext.language-clojure.find :as cfind]
   [com.blockether.vis.ext.language-clojure.nrepl-client :as nrepl-client]
   [com.blockether.vis.ext.language-clojure.outline :as outline]
   [com.blockether.vis.ext.language-clojure.ports :as ports]
   [com.blockether.vis.ext.language-clojure.render :as render]
   [com.blockether.vis.internal.extension :as extension]))

;; =============================================================================
;; Activation
;; =============================================================================

(defn- workspace-has-clojure?
  "Cheap activation check. Strategy, in order of preference:

   1. `:env/languages` already on the env — the engine MAY pre-populate
      it from a higher-level digest. Free win.
   2. Project-file probe — single `File.exists?` for `deps.edn`,
      `project.clj`, `shadow-cljs.edn`, `bb.edn`, `.nrepl-port`.
      This is the fast path: 1-5 syscalls, no walk.
   3. Bounded language scan (`languages/scan`). Only runs when the
      probe missed; some Clojure repos have no manifest at the
      workspace root (e.g. polylith sub-project pinned via channels)."
  [env]
  (let [root (some-> (:workspace/root env) io/file)]
    (when (and root (.isDirectory root))
      (or
        ;; (1) pre-populated env hint
        (boolean (some #(= "clojure" (:language %))
                   (some-> env :env/languages :languages)))

        ;; (2) project-file probe
        (some (fn [n] (.exists (io/file root n)))
          ["deps.edn" "project.clj" "shadow-cljs.edn" "bb.edn" ".nrepl-port"])

        ;; (3) bounded fallback scan
        (try
          (let [scan (languages/scan root {:max-files 2000 :deadline-ms 250})]
            (boolean (some #(= "clojure" (:language %)) (:languages scan))))
          (catch Throwable _ false))))))

(defn- activation-fn [env]
  (boolean (workspace-has-clojure? env)))

;; =============================================================================
;; Tool fns
;; =============================================================================

(defn- env-root ^String [env]
  (or (:workspace/root env)
    (throw (ex-info "clj/* tool fired without :workspace/root in env"
             {:type :clj/no-workspace}))))

(defn clj-ports-fn
  "Return nREPL ports discoverable from the active workspace.

   Returns: {:default <int|nil> :ports [{:port :source} ...]}.
   :default is the first hit in priority order — workspace
   `.nrepl-port` wins over home-level fallbacks."
  [env]
  (let [hits (vec (ports/discover-all (env-root env)))]
    (extension/success
      {:result {:default (some-> hits first :port)
                :ports   hits}})))

(defn- coerce-eval-arg
  "Accept the call shapes the model is most likely to type:
     (clj/eval \"(+ 1 1)\")
     (clj/eval {:code \"(+ 1 1)\"})
     (clj/eval {:code \"...\" :port 7888 :ns \"user\" :timeout-ms 5000})"
  [arg]
  (cond
    (string? arg) {:code arg}
    (map? arg)    arg
    :else (throw (ex-info "clj/eval expects a code string or opts map"
                   {:type :clj/bad-args :got arg
                    :examples ["(clj/eval \"(+ 1 1)\")"
                               "(clj/eval {:code \"...\" :port 7888})"]}))))

(defn clj-eval-fn
  ([env arg]
   (let [{:keys [code port host ns timeout-ms]} (coerce-eval-arg arg)
         root (env-root env)
         port (or port (ports/find-default root))]
     (when-not port
       (throw (ex-info "no nREPL port found — start one (e.g. `bin/dev`) or call (clj/ports) to inspect candidates"
                {:type :clj/no-port
                 :workspace-root root})))
     (extension/success
       {:result (nrepl-client/eval!
                  {:host       (or host "localhost")
                   :port       port
                   :code       code
                   :ns         ns
                   :timeout-ms (or timeout-ms 30000)})}))))

(defn clj-outline-fn
  ([env arg]
   (let [path (cond
                (string? arg) arg
                (map? arg)    (:path arg)
                :else nil)]
     (when-not (and (string? path) (seq path))
       (throw (ex-info "clj/outline requires a path string or {:path P}"
                {:type :clj/bad-args :got arg
                 :examples ["(clj/outline \"src/foo.clj\")"
                            "(clj/outline {:path \"src/foo.clj\"})"]})))
     (extension/success {:result (outline/outline-file (env-root env) path)}))))

(defn clj-find-fn
  ([env arg]
   (let [opts (cond
                (string? arg) {:name arg}
                (map? arg)    arg
                :else (throw (ex-info "clj/find requires an opts map or name regex string"
                               {:type :clj/bad-args :got arg
                                :examples ["(clj/find \"^make-\")"
                                           "(clj/find {:name \"render\" :kind :defn})"]})))]
     (extension/success {:result (cfind/find-defs (env-root env) opts)}))))

(defn clj-edit-fn
  ([env arg]
   (when-not (map? arg)
     (throw (ex-info "clj/edit requires an opts map"
              {:type :clj/bad-args :got arg
               :examples ["(clj/edit {:path \"src/a.clj\" :op :replace :target \"foo\" :code \"(defn foo [] :ok)\"})"
                          "(clj/edit {:path \"src/a.clj\" :op :replace :target [\"area\" :rectangle] :code \"...\"})"
                          "(clj/edit {:path \"src/a.clj\" :op :insert-after :target \"foo\" :code \"(defn bar [] 1)\"})"
                          "(clj/edit {:path \"src/a.clj\" :op :replace-sexp :target \"foo\" :match \"(+ 1 1)\" :code \"(+ 2 2)\"})"]})))
   (extension/success {:result (edit/apply-edit! (env-root env) arg)})))

;; =============================================================================
;; Symbols
;; =============================================================================

(defn- inject-env [env f args] {:env env :fn f :args (into [env] args)})

(def ^{:doc "Discover nREPL ports reachable from the workspace. Returns {:default <int|nil> :ports [{:port :source} ...]}. Priority: workspace `.nrepl-port` then ancestor / home fallbacks. No JVM scan — explicit port files only."
       :arglists '([])} ports clj-ports-fn)

(def ^{:doc "Evaluate Clojure code in a running nREPL. Accepts a code string or `{:code :port? :host? :ns? :timeout-ms?}`. Returns `{:value :values :out :err :ns :status :ex :root-ex :ms :port :host :timed-out?}`. Default port is the first `(clj/ports)` hit; throws `:clj/no-port` when nothing is running."
       :arglists '([arg])} eval clj-eval-fn)

(def ^{:doc "Collapsed view of a Clojure file. Path is workspace-relative or absolute. Returns `{:ns {...} :counts {:defn N ...} :forms [{:kind :name :line :arglists :doc :private? [:dispatch]} ...] :total N :path :bytes}`. Bodies are elided — pair with `v/cat` + `:line` for deep reads."
       :arglists '([arg])} outline clj-outline-fn)

(def ^{:doc "Structured def search across the workspace. Opts: `{:name regex :kind :defn|#{...} :max-files :deadline-ms :limit}`. Returns `{:matches [{:path :line :kind :name [:dispatch] [:doc]} ...] :scanned :truncated? :elapsed-ms}`. Use for jumping to a def by name; use `v/rg` for content search."
       :arglists '([arg])} find clj-find-fn)

(def ^{:doc "Structure-aware Clojure edit via rewrite-clj. Opts: `{:path :op :target :code [:match] [:format?]}`. `:op` ∈ #{:replace :insert-before :insert-after :replace-sexp}. `:target` is a defn/def name string, `[name dispatch]` for defmethod, or the wrapping form name for `:replace-sexp` (use `:match` for the sexp text to swap). Writes only when the result round-trips parse-clean. `:format? true` (default) runs zprint before writing."
       :arglists '([opts])} edit clj-edit-fn)

;; Each `:render-fn` is a structured IR builder over the raw
;; `:result` map (see render.clj). The MODEL surface is the SCI
;; return value (`pr-str` of the map) — these renderers shape
;; ONLY the channel/TUI preview, never what the LLM reads.

(def ports-symbol
  (vis/symbol #'ports
    {:before-fn inject-env :tag :observation :render-fn render/render-ports}))

(def eval-symbol
  (vis/symbol #'eval
    {:before-fn inject-env :tag :mutation :render-fn render/render-eval}))

(def outline-symbol
  (vis/symbol #'outline
    {:before-fn inject-env :tag :observation :render-fn render/render-outline}))

(def find-symbol
  (vis/symbol #'find
    {:before-fn inject-env :tag :observation :render-fn render/render-find}))

(def edit-symbol
  (vis/symbol #'edit
    {:before-fn inject-env :tag :mutation :render-fn render/render-edit}))

(def clj-symbols
  [ports-symbol eval-symbol outline-symbol find-symbol edit-symbol])

;; =============================================================================
;; Extension manifest
;; =============================================================================

(def ^:private prompt-text
  (str "Clojure language pack active. Symbols under `clj/`:\n"
    "  (clj/ports)                       — discover running nREPL ports\n"
    "  (clj/eval \"...\" | {:code :port? :ns? :timeout-ms?})\n"
    "  (clj/outline \"src/foo.clj\")       — collapsed file catalog\n"
    "  (clj/find {:name regex :kind :defn})\n"
    "  (clj/edit {:path :op :target :code [:match] [:format?]})\n"
    "Use `clj/edit` for Clojure def/defmethod changes — it is name-addressed and round-trip-validated; prefer it over `v/patch` for `.clj/.cljc/.cljs`. Use `clj/eval` to verify behaviour against the running REPL before claiming a fix."))

(def vis-extension
  (vis/extension
    {:ext/name           "language-clojure"
     :ext/description    "Clojure language pack: nREPL eval (clj/eval, clj/ports), structure-aware edits (clj/edit via rewrite-clj), collapsed file view (clj/outline), def search (clj/find). Activates only when the workspace has Clojure sources."
     :ext/version        "0.1.0"
     :ext/author         "Blockether"
     :ext/owner          "vis"
     :ext/license        "Apache-2.0"
     :ext/activation-fn  activation-fn
     :ext/sci            {:ext.sci/alias 'clj
                          :ext.sci/symbols clj-symbols}
     :ext/prompt         (fn [_env] prompt-text)
     :ext/kind           "language"}))

(vis/register-extension! vis-extension)
