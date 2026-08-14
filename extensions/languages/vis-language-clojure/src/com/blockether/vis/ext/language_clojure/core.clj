(ns com.blockether.vis.ext.language-clojure.core
  "vis-language-clojure — Clojure language handlers for Vis.

   Format/test/REPL are exposed through the generic language facade
   (`format`, `test`, `repl_eval`, `repl`, `repl_stop`) —
   `format` here does parinfer delimiter repair + cljfmt. The pack also registers
   a cross-cutting op-hook that parinfer-repairs `.clj` source rejected by the
   foundation's struct_patch, so unbalanced delimiters never fail an edit."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.foundation.environment.languages :as languages]
            [com.blockether.vis.ext.language-clojure.format :as fmt]
            [com.blockether.vis.ext.language-clojure.paren-repair :as repair]
            [com.blockether.vis.ext.language-clojure.lint :as lint]
            [com.blockether.vis.ext.language-clojure.reflection :as reflection]
            [com.blockether.vis.ext.language-clojure.nrepl-client :as nrepl-client]
            [com.blockether.vis.ext.language-clojure.nrepl-ctx :as nrepl-ctx]
            [com.blockether.vis.ext.language-clojure.repl-manager :as repl-manager]
            [com.blockether.vis.ext.language-clojure.test-runner :as test-runner]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.foundation.surface-contract :as contract]
            [taoensso.telemere :as tel]))

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
  (let
    [root (some-> (:workspace/root env)
                  io/file)]
    (when (and root (.isDirectory root))
      (or
        ;; (1) pre-populated env hint
        (boolean (some #(= "clojure" (:language %))
                       (some-> env
                               :env/languages
                               :languages)))
        ;; (2) project-file probe
        (some (fn [n]
                (.exists (io/file root n)))
              ["deps.edn" "project.clj" "shadow-cljs.edn" "bb.edn"])
        ;; (3) bounded fallback scan
        (try (let [scan (languages/scan root {:max-files 2000 :deadline-ms 250})]
               (boolean (some #(= "clojure" (:language %)) (:languages scan))))
             (catch Throwable _ false))))))

(defn- activation-fn [env] (boolean (workspace-has-clojure? env)))

;; =============================================================================
;; Tool fns
;; =============================================================================

(defn- env-root
  ^String [env]
  (or (:workspace/root env)
      (throw (ex-info "clj/* tool fired without :workspace/root in env"
                      {:type :clj/no-workspace}))))

(defn- expand-home
  "Expand a leading `~` / `~/…` to the user's home dir (`user.home`), so a REPL
   `cwd` written the way a human types it resolves to a real absolute path
   instead of a bogus `~` segment under the workspace root. `~user` (another
   user's home) is NOT resolved — it passes through untouched."
  ^String [^String d]
  (let [home (System/getProperty "user.home")]
    (cond (= "~" d) (or home d)
          (str/starts-with? d "~/") (if home (str home (subs d 1)) d)
          :else d)))

(defn- resolve-repl-dir
  "Resolve a `:start`/`:status`/`:stop` target dir against the workspace `root`.
   A blank dir means the workspace root; a leading `~`/`~/…` expands to the
   user's home dir; a relative dir is taken under root; an absolute dir is used
   as-is. Returns a canonical path string — the SAME value for a given target no
   matter how it was spelled, so start, stop, and eval-by-id all agree on one id."
  ^String [root dir]
  (let
    [d
     (expand-home (str dir))

     f
     (cond (= "" d) (io/file root)
           (.isAbsolute (io/file d)) (io/file d)
           :else (io/file root d))]

    (.getCanonicalPath f)))

(defn- coerce-aliases
  "Accept [\"dev\" \"test\"], \"dev\", [:dev], or nil → a vec of alias name
   STRINGS or nil. No keyword minting: aliases stay strings end-to-end (deps.edn
   alias suffix, resource detail that crosses the strings-only boundary)."
  [a]
  (cond (nil? a) nil
        (sequential? a) (mapv name a)
        :else [(name a)]))

(defn- repl-resource-id
  "Stable session-resource id for the REPL rooted at `cwd` — the SAME id
   `repl-manager/id-of` stamps, so ctx, eval targeting, and the footer all agree
   on one name per dir. Addressing a REPL is always by this id."
  [dir]
  (repl-manager/id-of dir))

(defn register-repl-resource!
  "Mirror a session's managed nREPL into the session-scoped resource registry so
   it shows in ctx (resources) + the footer, and can be stopped by id from the
   agent or the UI. No-op without a session or a live spawn. The stop-fn IS the
   canonical teardown — the footer and resource_stop both drive repl-manager
   through it, scoped to `session-id`. There is deliberately NO restart thunk:
   a REPL is stopped, then started, never silently swapped underneath a caller."
  [session-id dir aliases result]
  ;; `result` is repl-manager/start!'s STRING-keyed lifecycle map. The resource
  ;; map handed to `vis/register-resource!` is the CENTRAL resources.clj DATA
  ;; shape (keyword keys/values) — that projection is what crosses to the model,
  ;; and its strings-only migration lives in resources.clj (flagged hand-off).
  (when (and session-id
             (#{"started" "starting" "already-running" "connected"} (get result "result"))
             (or (get result "pid") (get result "port")))
    (let
      [;; Prefer the aliases start! actually booted with (STRING names) so the
       ;; label/detail reflect the real [:dev :test] classpath even when the
       ;; caller passed none.
       aliases
       (or (seq (get result "aliases")) (map name (or aliases [])))

       id
       (repl-resource-id dir)

       log-path
       (get result "log")

       status
       (or (get result "status") :up)

       external?
       (boolean (get result "external"))

       ext-host
       (get result "host")]

      (vis/register-resource!
        session-id
        {:id id
         :kind :nrepl
         :label (str "nREPL "
                     (.getName (io/file dir))
                     (when external? " (external)")
                     (when (seq aliases) (apply str (map #(str " :" %) aliases))))
         :status status
         ;; `:detail` is passed THROUGH verbatim by resources.clj/->data (it only
         ;; stringifies its own keys + the kind/status/owner/language enums), so it
         ;; must already be STRING-keyed to survive the strings-only boundary.
         :detail (cond-> {"cwd" dir}
                   (get result "port")
                   (assoc "port" (get result "port"))

                   external?
                   (assoc "host"
                     (or ext-host "localhost") "external"
                     true)

                   (seq aliases)
                   (assoc "aliases" (vec aliases))

                   log-path
                   (assoc "log" log-path))
         :pid (get result "pid")
         :owner :ext/language-clojure
         :language :clojure}
        (cond->
          {:stop-fn (fn []
                      (repl-manager/stop! session-id dir))
           ;; Keep a FAILED REPL visible (alive while a failure is on
           ;; record) instead of letting the registry prune it the moment
           ;; the pid dies — the failure + its log tail stay inspectable
           ;; in F4 until an explicit stop.
           :alive-fn (fn []
                       (boolean (or (repl-manager/repl-by-id session-id id)
                                    (repl-manager/last-failure session-id dir))))
           ;; "alive, but is it WORKING?" — the registry probes this on
           ;; every list/render and flips `status` to reality.
           :health-fn (fn []
                        (repl-manager/health session-id dir))}
          log-path
          (assoc :logs-fn
            (fn []
              (repl-manager/tail-log log-path)))))
      ;; Surface the registration in the TUI (header toast) so a spawned REPL is
      ;; visible the moment it lands, not just as a silent ● bump in the footer.
      (vis/notify! (str "● nREPL "
                        (if (= "starting" status) "starting" "up")
                        " — "
                        (.getName (io/file dir))
                        (when-let [p (get result "port")]
                          (str " :" p)))
                   :level (if (= "starting" status) :info :success)
                   :ttl-ms 4000))))

(defn repl-start-fn
  "Manage THIS session's workspace nREPL(s). Positional op (default \"status\") +
   optional opts dict `{\"cwd\": <path>, \"aliases\": [\"dev\", \"test\"]}`:

     \"status\"  — managed-process view for this session (always allowed)
     \"start\"   — start a project nREPL subprocess (always allowed)
     \"stop\"    — stop a Vis-managed nREPL / DETACH an external one (always allowed)
     \"connect\" — attach to an EXTERNAL user-started nREPL: opts {\"port\": N,
                 \"host\"?: S (default localhost)}; vis never spawns/kills it

   \"cwd\" runs the REPL in a subdir (e.g. an extension) instead of the workspace
   root — that's how MULTIPLE REPLs coexist, each addressed by its id. \"aliases\"
   default to [:dev :test] (full deps/paths, user :main-opts dropped). Live nREPL
   state already rides in ctx under `:session/env :languages :clojure :nrepl`;
   this tool acts on it."
  ([env] (repl-start-fn env "status" nil))
  ([env op] (repl-start-fn env op nil))
  ([env op opts]
   (let
     [root
      (env-root env)

      sid
      (:session-id env)

      ;; Positional op arrives as a STRING from the model (strings-only
      ;; boundary); dispatch on it directly, no keyword minting. Default
      ;; "status".
      op
      (if (string? op) op "status")

      opts
      (when (map? opts) opts)

      dir
      (resolve-repl-dir root (get opts "cwd"))

      aliases
      (coerce-aliases (get opts "aliases"))]

     (case op
       "status"
       (extension/success {:result (repl-manager/status sid dir)})

       "connect"
       (let
         [port
          (get opts "port")

          host
          (get opts "host")]

         (when-not port
           (throw (ex-info (str "repl \"connect\" needs {\"port\": <the external nREPL's port>}"
                                " (optional \"host\", \"cwd\") — e.g."
                                " repl(\"clojure\", \"connect\", {\"port\": 7888})")
                           {:type :clj/bad-args :got opts})))
         (let
           [r (repl-manager/connect!
                sid
                dir
                {:host host
                 :port (if (string? port) (Long/parseLong (str/trim port)) (long port))})]
           (register-repl-resource! sid dir aliases r)
           (extension/success {:result r})))

       "stop"
       (let [r (repl-manager/stop! sid dir)]
         ;; Drop the session's resource mirror (best-effort; the thunk
         ;; already ran the real teardown above).
         (vis/unregister-resource! sid (repl-resource-id dir))
         (extension/success {:result r}))

       "start"
       (do (when-not (.isDirectory (io/file dir))
             (throw (ex-info (str "repl \"start\" target cwd does not exist: "
                                  (repl-manager/home-relativize (str dir)))
                             {:type :clj/bad-args :dir dir})))
           ;; No "restart": start! REUSES a healthy REPL ("already-running") and
           ;; a REPL you actually want replaced is stopped explicitly first, so a
           ;; hung relaunch can never leave the caller with nothing.
           (let [result (repl-manager/start! sid dir {:aliases aliases})]
             ;; Mirror the live REPL into the session resource registry → ctx +
             ;; footer + stoppable by id.
             (register-repl-resource! sid dir aliases result)
             (extension/success {:result result})))

       (throw
         (ex-info
           (str "repl unknown op: " (pr-str op))
           {:type :clj/bad-args
            :got op
            :examples
            ["repl(\"clojure\")" "repl(\"clojure\", \"status\")" "repl(\"clojure\", \"start\")"
             "repl(\"clojure\", \"start\", {\"cwd\": \"extensions/languages/vis-language-clojure\", \"aliases\": [\"dev\", \"test\"]})"
             "repl(\"clojure\", \"stop\")"]}))))))


(defn available-aliases
  "Alias names declared in the workspace `deps.edn` — surfaced to the UI so the
   user picks REAL aliases (`:dev`, `:test`, …) instead of guessing. Returns a
   sorted vec of strings WITHOUT the leading colon; empty on any read/parse
   failure or a non-deps project."
  [env]
  (try (let
         [root
          (env-root env)

          dir
          (resolve-repl-dir root nil)

          f
          (io/file dir "deps.edn")]

         (if (.isFile f)
           (->> (:aliases (edn/read-string (slurp f)))
                keys
                (map name)
                sort
                vec)
           []))
       (catch Throwable _ [])))

(defn- coerce-eval-arg
  "Accept the call shapes the model is most likely to type:
     clj_eval(\"(+ 1 1)\")
     clj_eval({\"code\": \"(+ 1 1)\"})
     clj_eval({\"code\": \"...\", \"port\": 7888, \"ns\": \"user\", \"timeout_ms\": 5000})
     clj_eval({\"code\": \"...\", \"id\": \"<repl-id>\"})   ; target a registered REPL"
  [arg]
  (cond (string? arg) {"code" arg}
        (map? arg) arg
        :else (throw (ex-info "clj_eval expects a code string or opts map"
                              {:type :clj/bad-args
                               :got arg
                               :examples ["clj_eval(\"(+ 1 1)\")"
                                          "clj_eval({\"code\": \"...\", \"port\": 7888})"]}))))

(defn- strip-blank-repl-fields
  "Prune result fields the model gains nothing from seeing: nil, blank strings,
   empty collections, and the pr-str of nil (a bare `\"nil\"` value, or a `values`
   vector that is only nils). Keeps every informative field — a real value,
   captured stdout/stderr, errors, status, ns and timing — so a `(println …)`
   run surfaces its STDOUT WITHOUT a redundant `\"value\": \"nil\"`, and a plain
   `(+ 1 2)` shows only its value, no empty `out`/`err`/`ex`/`root_ex` noise.
   Presentation-only: the UI op-card and the internal `eval!` callers still see
   the full nREPL shape; this trims just the map that crosses to the model."
  [m]
  (into {}
        (remove (fn [[_ v]]
                  (or (nil? v)
                      (= "nil" v)
                      (and (string? v) (str/blank? v))
                      (and (coll? v) (or (empty? v) (every? #(= "nil" %) v))))))
        m))

(defn clj-eval-fn
  "Evaluate Clojure over a RUNNING nREPL in this session. Target resolution:
     - explicit `port` → dial it directly (escape hatch);
     - `id`/`repl_id`  → the REPL registered under that id in THIS session;
     - `cwd`           → the REPL rooted at that directory (when the session owns one);
     - no id, 1 REPL   → use it (the implicit default);
     - no id, >1 REPLs → the REPL owning `cwd` (default: the workspace root) when
                         present, else the first (dir-sorted);
     - no id, 0 REPLs  → error (:clj/no-repl): no running nREPL to hit.
   A connect failure surfaces as DATA so the model can repl / wait."
  ([env arg]
   (let
     [m
      (coerce-eval-arg arg)

      code
      (get m "code")

      port
      (get m "port")

      host
      (or (get m "host") "localhost")

      ns
      (get m "ns")

      timeout_ms
      (get m "timeout_ms")

      root
      (env-root env)

      sid
      (:session-id env)

      requested-dir?
      (contains? m "cwd")

      requested-rid
      (some-> (or (get m "id") (get m "repl_id"))
              str
              str/trim
              not-empty)

      rid
      ;; A model may carry a stale/previous ctx resource id while also passing
      ;; an explicit `cwd`. If that id is not live in THIS session, let `cwd`
      ;; drive the default resolution instead of failing on the unknown id.
      ;; With no explicit cwd, keep the strict id contract and surface the error.
      (when-not (and requested-dir? requested-rid (not (repl-manager/repl-by-id sid requested-rid)))
        requested-rid)

      default-dir
      (resolve-repl-dir root (get m "cwd"))

      run
      (fn [h p repl-label]
        ;; Carry the evaluated FORM back on the result (string key, crosses the
        ;; strings-only boundary) so the repl_eval op-card can show it in the
        ;; collapsed chip / expanded FORM section. `repl` names WHICH nREPL
        ;; actually ran it, so a multi-REPL session reports the target used.
        (-> (nrepl-client/eval!
              {:host h :port p :code code :ns ns :pretty? true :timeout-ms (or timeout_ms 30000)})
            strip-blank-repl-fields
            (assoc "code" code
                   "repl" repl-label)))]

     (if port
       ;; Explicit port: the escape hatch — dial exactly what was asked.
       (extension/success {:result (run host port (str host ":" port))})
       ;; Resolve a RUNNING REPL. A missing/unknown REPL is an EXPECTED,
       ;; actionable condition — catch it and return a TIGHT failure envelope
       ;; so the model sees just the one-line message + hint, NOT the raw
       ;; `clojure.lang.ExceptionInfo` class + `{:type … :dir …}` ex-data and
       ;; the internal nREPL/Compiler stack trace `ex->op-error` would attach.
       (try (let [target (repl-manager/resolve-target! sid rid default-dir)]
              ;; An EXTERNAL attachment may live on a non-localhost host — dial ITS
              ;; host, not the caller's default.
              (extension/success {:result
                                  (run (or (:host target) host) (:port target) (:id target))}))
            (catch clojure.lang.ExceptionInfo e
              (case (:type (ex-data e))
                :clj/no-repl
                (extension/failure {:error {:message (str "no REPL running in "
                                                          ;; Home-homogenized: the message reads
                                                          ;; `~/vis`, matching the REPL ids in
                                                          ;; session["resources"] — never a raw
                                                          ;; `/Users/you/vis`.
                                                          (repl-manager/home-relativize
                                                            (str (:dir (ex-data e))))
                                                          " — start one: repl(\"clojure\")")
                                            :hint "then retry the eval"}})

                :clj/unknown-repl-id
                (extension/failure
                  {:error {:message (str "no REPL under id '"
                                         (:id (ex-data e))
                                         "' — check session[\"resources\"][\"repls\"][\"clojure\"]")
                           :hint "pass a live id, or omit it to use the default REPL"}})

                (throw e))))))))

(defn clj-repair+format
  "The combined Clojure tidy used by BOTH `format` and the post-edit hook:
   parinfer delimiter repair FIRST (so unbalanced ( [ { from a raw edit are
   fixed), THEN indentation via the config-driven formatter (`fmt/format-source`
   picks zprint when a `.zprint.edn`/`.zprintrc` is near `path`, else cljfmt).
   Total — returns `code` unchanged on any failure of either step."
  ([code] (clj-repair+format code nil))
  ([code path]
   (let [repaired (or (repair/fix-delimiters code) code)]
     (fmt/format-source repaired path))))

(defn- relativize-path
  "Rewrite an absolute path to one relative to workspace `root` so tool output
   reads `src/foo.clj` instead of the noisy machine-absolute `/Users/…/src/foo.clj`.
   Paths outside root (and the root itself) collapse their user-home prefix to
   `~`; non-path sentinels like `<stdin>` pass through unchanged."
  [^java.io.File root file]
  (let [s (str file)]
    (if (and root (seq s) (not= s "<stdin>"))
      (try (let
             [rp (.toPath (.getCanonicalFile root))
              fp (.toPath (.getCanonicalFile (io/file s)))
              rel (when (.startsWith fp rp) (str (.relativize rp fp)))]

             ;; Under root -> `src/foo.clj`. Otherwise (outside root, or the root
             ;; itself, whose relativization is "") fall back to a home-homogenized
             ;; absolute path so output reads `~/vis` — never a raw `/Users/you/…`.
             (if (seq rel) rel (repl-manager/home-relativize (str fp))))
           (catch Throwable _ (repl-manager/home-relativize s)))
      s)))

;; Only true Clojure SOURCE dialects — the same set the canonical
;; `clojure -M:format` (codestyle) formats. Deliberately NOT `.edn`: zprint
;; sorts map keys + reflows, which would churn hand-ordered config files
;; (deps.edn, `.zprint.edn`) that codestyle never touches, making the
;; format-on-write hook fight the canonical formatter.
(def ^:private clj-source-exts [".clj" ".cljs" ".cljc" ".cljx"])

(defn- clj-source-file?
  "True when `path` names a Clojure source file (by extension)."
  [path]
  (let [p (str/lower-case (str path))]
    (boolean (some #(str/ends-with? p %) clj-source-exts))))

(def ^:private denied-dir-names
  "Directory names we NEVER format or lint: build artifacts, vendored deps and
   tool caches. Even when a caller points a recursive walk straight at one (or a
   real source path happens to contain one), everything under such a dir is
   skipped."
  #{"target" "dist" "build" "out" "classes" ".cpcache" ".gradle" "node_modules" ".shadow-cljs"
    ".cljs_node_repl" ".clj-kondo" ".clojure-lsp" ".lsp" ".calva" ".git" ".hg" ".svn" ".bzr" ".idea"
    ".vscode"})

(defn- under-denied-dir?
  "True when any ancestor directory of `f` is in `denied-dir-names`."
  [^java.io.File f]
  (loop [p (.getParentFile f)]
    (cond (nil? p) false
          (contains? denied-dir-names (.getName p)) true
          :else (recur (.getParentFile p)))))

(defn- expand-clj-source-files
  "Expand `paths` (resolved against workspace `root` when relative) into concrete
   Clojure source files. A DIRECTORY is walked RECURSIVELY, collecting every
   `.clj`/`.cljs`/`.cljc`/`.cljx` file under it; a plain file is kept
   as-is; a non-existent path is dropped. Returns a de-duplicated, sorted vector
   of absolute path strings."
  [^java.io.File root paths]
  (->> paths
       (mapcat (fn [p]
                 (let
                   [g
                    (io/file (str p))

                    f
                    (if (.isAbsolute g) g (io/file root (str p)))]

                   (cond (.isDirectory f) (->> (file-seq f)
                                               (filter #(.isFile ^java.io.File %))
                                               (filter #(clj-source-file? (str %)))
                                               (remove under-denied-dir?))
                         (.isFile f) [f]
                         :else nil))))
       (map str)
       (distinct)
       (sort)
       (vec)))

(defn- read-edn-safe
  "Read `f` as EDN, returning nil on any failure (missing / malformed)."
  [f]
  (try (edn/read-string (slurp (str f))) (catch Throwable _ nil)))

(defn- deps-source-paths
  "Source/test roots declared by a parsed deps.edn map (relative to its dir):
   its `:paths` plus every alias `:extra-paths`."
  [deps]
  (when (map? deps) (concat (:paths deps) (mapcat :extra-paths (vals (:aliases deps))))))

(defn- deps-local-roots
  "Every `:local/root` module dir declared by a parsed deps.edn map: from its
   `:deps` plus every alias `:extra-deps`."
  [deps]
  (when (map? deps)
    (->> (concat (vals (:deps deps)) (mapcat (comp vals :extra-deps) (vals (:aliases deps))))
         (keep #(when (map? %) (:local/root %))))))

(defn- discover-project-source-paths
  "Best-effort discovery of the project's OWN Clojure source roots, driven by the
   root `deps.edn` (option B — self-maintaining allowlist, not a hardcoded layout):
     - the root module's `:paths` + every alias `:extra-paths`
     - every `:local/root` module reachable TRANSITIVELY (a local dep's own
       deps.edn may point at further locals — we follow them, guarding against
       cycles / repeats), using each module's declared paths (or `src`+`test`).
   Returns existing directories as de-duplicated, sorted absolute strings. This
   naturally excludes vendored code and test fixtures (nothing points at them).
   Falls back to `src`+`test`, then the workspace root, when no deps.edn is found."
  [^java.io.File root]
  (let
    [root-deps
     (read-edn-safe (io/file root "deps.edn"))

     modules
     (loop
       [queue
        (map #(io/file root (str %)) (deps-local-roots root-deps))

        seen
        #{}

        acc
        []]

       (if-let [dir (first queue)]
         (let [canon (try (.getCanonicalPath ^java.io.File dir) (catch Throwable _ (str dir)))]
           (if (contains? seen canon)
             (recur (rest queue) seen acc)
             (let
               [md (read-edn-safe (io/file dir "deps.edn"))
                subs (map #(io/file dir (str %)) (deps-local-roots md))]

               (recur (concat (rest queue) subs) (conj seen canon) (conj acc [dir md])))))
         acc))

     candidates
     (concat (map #(io/file root (str %)) (deps-source-paths root-deps))
             (mapcat (fn [[dir md]]
                       (map #(io/file dir (str %))
                            (or (seq (deps-source-paths md)) ["src" "test"])))
                     modules))

     dirs
     (->> candidates
          (filter #(.isDirectory ^java.io.File %))
          (map #(try (.getCanonicalPath ^java.io.File %) (catch Throwable _ (str %))))
          (distinct)
          (sort)
          (vec))]

    (cond (seq dirs) dirs
          :else (let
                  [d (->> ["src" "test"]
                          (map #(io/file root %))
                          (filter #(.exists ^java.io.File %))
                          (mapv str))]
                  (if (seq d) d [(str root)])))))

(defn- clj-format-one-file!
  "Format a single file at `path` IN PLACE (paren-repair + cljfmt), writing
   back ONLY when the content changes. Returns a per-file result map with the
   workspace-relative path.

   Runs parinfer ONCE and reuses that result both as the formatter's input and
   as the `\"repaired\"` flag — the old shape called `fix-delimiters` a second
   time purely to answer the flag."
  [env path]
  (let
    [code
     (slurp (str path))

     for-path
     (or path (:workspace/root env))

     fixed
     (repair/fix-delimiters code)

     repaired?
     (and (string? fixed) (not= fixed code))

     out
     (fmt/format-source (if (string? fixed) fixed code) for-path)]

    (when (not= out code) (spit (str path) out))
    {"path" (relativize-path (io/file (or (:workspace/root env) ".")) path)
     "changed" (not= out code)
     "repaired" repaired?
     "wrote" (not= out code)
     "formatter" (name (fmt/formatter-for for-path))}))

(defn- group-format-by-cwd
  "Nest the per-file format results under their DIRECTORY so each directory
   prefix is written once: `{<dir> {<basename> {\"changed\" .. \"repaired\" ..
   \"wrote\" ..}}}`. `<dir>` is the file's parent (`\".\"` when it has none); the
   inner key is the basename, its payload the per-file map minus the now-implied
   `\"path\"`. Mirrors `lint/group-by-cwd` so format and lint share one shape."
  [files]
  (reduce (fn [m f]
            (let
              [jf
               (java.io.File. ^String (get f "path"))

               dir
               (or (.getParent jf) ".")

               base
               (.getName jf)]

              (assoc-in m [dir base] (dissoc f "path"))))
          {}
          files))

(defn clj-format-fn
  "Format Clojure source via the language facade (`format_code`). Accepts:
     - a raw code string / {\"code\": ...}   -> report changed? + char delta (NO text)
     - {\"path\": \"src/foo.clj\"}              -> format that file IN PLACE
     - {\"paths\": [\"src\" \"test\" ...]}        -> format those paths IN PLACE; a
         DIRECTORY is walked RECURSIVELY (every .clj/.cljs/.cljc/.cljx under it)
     - nothing / {}                         -> format the whole project's source
         roots (every deps.edn module's :paths + test), skipping build/vendor
         dirs (target, dist, node_modules, .clj-kondo, .clojure-lsp, .cpcache…)
   dirs (target, dist, node_modules, .clj-kondo, .clojure-lsp, .cpcache…)
   Paths are resolved against the workspace root when relative. Every result
   NAMES the backend that ran: `\"formatter\"` (\"zprint\" | \"cljfmt\") on a
   single file / code string, and the distinct `\"formatters\"` set on a batch."
  ([arg] (clj-format-fn nil arg))
  ([env arg]
   (let
     [root
      (io/file (or (:workspace/root env) "."))

      paths
      (when (map? arg) (get arg "paths"))

      path
      (when-let
        [p (and (map? arg)
                (let [p (get arg "path")]
                  (when-not (str/blank? (str p)) p)))]
        (let [f (io/file (str p))]
          (str (if (.isAbsolute f) f (io/file root (str p))))))

      has-code?
      ;; A blank `"code": ""` default must not shadow a real `path`/`paths`
      ;; (otherwise we'd format an empty snippet instead of the file).
      (and (map? arg) (not (str/blank? (str (get arg "code")))))

      default?
      (or (nil? arg) (and (map? arg) (not (seq paths)) (not path) (not has-code?)))

      batch
      (cond (seq paths) (expand-clj-source-files root paths)
            default? (expand-clj-source-files root (discover-project-source-paths root)))]

     (if batch
       (let [files (mapv #(clj-format-one-file! env %) batch)]
         (extension/success {:result (contract/check
                                       :format-fn
                                       {"op" "clj-format"
                                        "files" files
                                        "changed" (count (filter #(get % "changed") files))
                                        "by-cwd" (group-format-by-cwd files)
                                        "formatters" (vec (sort (distinct (keep #(get % "formatter")
                                                                                files))))})}))
       (let
         [code
          (cond
            (string? arg) arg
            has-code? (str (get arg "code"))
            path (slurp (str path))
            :else
            (throw
              (ex-info
                "format expects a code string, {\"code\": ...}, {\"path\": ...}, {\"paths\": [...]}, or {} for the whole project"
                {:type :clj/bad-args
                 :got arg
                 :examples ["format(\"clojure\", \"(defn f [x]\\n(* x 2))\")"
                            "format(\"clojure\", {\"code\": \"...\"})"
                            "format(\"clojure\", {\"path\": \"src/foo.clj\"})"
                            "format(\"clojure\", {\"paths\": [\"src\" \"test\"]})"
                            "format(\"clojure\", {})"]})))

          for-path
          (or path (:workspace/root env))

          fixed
          (repair/fix-delimiters code)

          out
          (fmt/format-source (if (string? fixed) fixed code) for-path)]

         (when (and path (not= out code)) (spit (str path) out))
         (extension/success
           {:result (contract/check
                      :format-fn
                      (cond->
                        {"op" "clj-format"
                         "changed" (not= out code)
                         "chars" (- (count out) (count code))
                         "repaired" (and (string? fixed) (not= fixed code))
                         "formatter" (name (fmt/formatter-for for-path))}
                        path
                        (assoc "path"
                          (relativize-path (io/file (or (:workspace/root env) ".")) path) "wrote"
                          (not= out code))))}))))))

(defn- nearest-kondo-dir
  "The nearest `.clj-kondo` config directory walking UP from `file`, or nil when
   none exists above it. Mirrors format's per-file `.zprint.edn` walk so a NESTED
   project's `.clj-kondo` wins over the workspace root's: clj-kondo's own `run!`
   otherwise resolves config from the process CWD (`user.dir`) and never sees a
   nested config dir. nil means 'no project config' -> clj-kondo default."
  ^java.io.File [^java.io.File file]
  (loop [dir (if (.isDirectory file) file (.getParentFile file))]
    (when dir
      (let [c (io/file dir ".clj-kondo")]
        (if (.isDirectory c) c (recur (.getParentFile dir)))))))

(defn- lint-grouped
  "Lint absolute source-`files`, GROUPED by each file's nearest `.clj-kondo` dir,
   running clj-kondo once per group under that group's `:config-dir` — so files
   in a nested project are linted against ITS config, not the workspace root's.
   Files with no `.clj-kondo` above them fall back to clj-kondo's default
   resolution (key nil). The per-group results are merged into one uniform map."
  [files]
  (if (empty? files)
    lint/empty-result
    (->> files
         (group-by #(nearest-kondo-dir (io/file (str %))))
         (mapv (fn [[cfg-dir group]]
                 (lint/lint-paths group cfg-dir)))
         (lint/merge-results))))

(defn clj-lint-fn
  "clj-kondo lint via the language facade (`lint_code`). Accepts:
     - a raw code string / {\"code\": ...}  -> lint it on stdin
     - {\"path\": \"src/foo.clj\"}           -> lint that file
     - {\"paths\": [\"src\", \"test\"]}        -> lint those paths
     - nothing / {}                       -> lint the whole project's source roots
         (every deps.edn module's :paths + test), skipping build/vendor dirs
   `path` and `paths` are UNIONED (not shadowing); a target that resolves to
   nothing is an ERROR, not a silent `clean`.
   Paths are resolved against the workspace root when relative. Finding \"file\"
   paths are reported RELATIVE to the workspace root (absolute only when outside).

   Findings come from one or more PROVIDERS, tagged per finding as `\"provider\"`
   and listed under `\"providers\"`: `\"clj-kondo\"` (static analysis, every
   branch) and `\"general\"` (the compiler's reflection + boxed-math warnings).
   Reflection/boxed-math only exist at compile time, so `\"general\"` COMPILES its
   target: the code-string snippet, or every source file the lint targets (path /
   paths / whole project) — each in a throwaway namespace that is torn down. The
   flat `\"findings\"` vector is also grouped under `\"by-cwd\"` — nested by
   directory to write each path prefix once:
   `{<dir> {<basename> {\"error\"/\"warning\"/\"info\" [...]}}}`."
  [env arg]
  (let
    [root
     (io/file (or (:workspace/root env) "."))

     path
     (when (map? arg)
       (let [p (get arg "path")]
         (when-not (str/blank? (str p)) p)))

     paths
     (when (map? arg) (get arg "paths"))

     code
     ;; Models routinely emit EVERY schema key with an empty default
     ;; (`"code": ""` alongside a real `"path"`); a blank `code` must NOT
     ;; shadow the path (that would lint an empty snippet and falsely
     ;; report `snippet — clean` while the file goes unlinted).
     (cond (string? arg) (when-not (str/blank? arg) arg)
           (and (map? arg) (not (str/blank? (str (get arg "code"))))) (str (get arg "code"))
           :else nil)

     under
     (fn [p]
       (let [f (io/file (str p))]
         (str (if (.isAbsolute f) f (io/file root (str p))))))

     ;; `path` and `paths` are UNIONED, not shadowing — a model that emits BOTH
     ;; (e.g. a junk `path` beside a real `paths`) lints/validates every named
     ;; target instead of silently dropping one.
     requested
     (when-not code (into [] (distinct (concat (when path [path]) (when (seq paths) paths)))))

     ;; A named target that resolves to nothing must be an ERROR, not a silent
     ;; `clean`: a non-existent path expands to zero files → 0 findings, so the
     ;; model gets meaningless `clean` feedback with nothing to correct and spins.
     missing
     (into [] (remove #(.exists (io/file (under %))) requested))]

    (if (seq missing)
      (extension/failure
        {:error {:message (str "lint target does not exist: "
                               (str/join ", " missing)
                               " — relative paths resolve against the workspace root")
                 :hint "pass an existing file/dir, or omit path/paths to lint the whole project"}})
      (let
        [targets
         (when (seq requested) (mapv #(relativize-path root (under %)) requested))

         base
         ;; clj-kondo is the STATIC provider (all branches). The :general provider
         ;; (reflection + boxed-math) is a COMPILER pass — the warnings only exist
         ;; while the code is compiled — so it runs over whatever we're TARGETING:
         ;; the `code` snippet, or each source file being linted (compiled in a
         ;; throwaway namespace that is torn down afterwards, so nothing leaks).
         ;; Its warnings merge into the flat findings and the warning count.
         (let
           [merge-general (fn [k g]
                            (-> k
                                (update "findings" into g)
                                (update "warning" (fnil + 0) (count g))))]
           (if code
             (merge-general (lint/lint-code code) (reflection/compile-warnings code "<stdin>"))
             (let
               [src-files (expand-clj-source-files root
                                                   (if (seq requested)
                                                     (mapv under requested)
                                                     (discover-project-source-paths root)))]
               (merge-general (lint-grouped src-files)
                              (into []
                                    (mapcat #(reflection/compile-warnings (slurp (io/file %))
                                                                          (str %)))
                                    src-files)))))

         providers
         ["clj-kondo" "general"]

         findings
         (mapv #(update % "file" (partial relativize-path root)) (get base "findings"))]

        (extension/success {:result (contract/check :lint-fn
                                                    (cond->
                                                      (assoc base
                                                        "findings" findings
                                                        "language" "clojure"
                                                        "providers" providers
                                                        "by-cwd" (lint/group-by-cwd findings))
                                                      code
                                                      (assoc "snippet" code)

                                                      (seq targets)
                                                      (assoc "targets" (vec targets))))})))))

(defn clj-struct-patch-no-fail-around
  "MIDDLEWARE (:around) on struct_patch so a Clojure structural edit does NOT
   fail on unbalanced delimiters. If the call throws and it targeted a `.clj`
   file with a `:code` form, parinfer-repair the code and retry ONCE. If the
   repair changes nothing — or the retried edit still fails — the ORIGINAL error
   is surfaced (we never bury a real structural failure). Non-clj / non-code
   calls pass straight through to `next`."
  [_env _op-kw args next]
  (try
    (next args)
    (catch clojure.lang.ExceptionInfo e
      (let
        [m
         (first args)

         path
         (and (map? m) (get m "path"))

         code
         (and (map? m) (get m "code"))

         fixed
         (when (and (clj-source-file? path) (string? code)) (repair/fix-delimiters code))]

        (if (and fixed (not= fixed code))
          (try
            (next (assoc-in (vec args) [0 "code"] fixed))
            (catch Throwable retry-t
              (tel/log!
                {:level :warn
                 :id :clojure-struct-patch-repair-retry-failed
                 :data {:path path :error (.getMessage retry-t)}}
                "Clojure struct_patch delimiter repair retry failed; surfacing original editor error")
              (throw e))) ; repaired retry failed → original
          (throw e))))))

(defn- repair-edit-replacement
  "One `edits` entry with its `replace` parinfer-repaired, whichever key form it
   arrived in: string keys off the Python call, keyword keys when Clojure code
   builds the batch. Anything else rides through untouched — the repair is
   SYNTAX-ONLY and never invents a key the caller did not pass."
  [edit]
  (cond (not (map? edit)) edit
        (string? (get edit "replace")) (update edit "replace" repair/fix-delimiters)
        (string? (get edit :replace)) (update edit :replace repair/fix-delimiters)
        :else edit))

(defn clj-patch-no-fail-around
  "MIDDLEWARE (:around) on `patch` so an anchored Clojure edit does NOT fail on
   unbalanced delimiters. `patch` is `(path edits)`, so every replacement the call
   carries sits in an entry's `replace` key and the repair maps over the WHOLE
   batch: if the call throws against a `.clj` file, parinfer-repair each
   replacement, retry ONCE when any of them changed, and say so on the status line
   the model reads. The batch is atomic in the editor, so the retry is the whole
   batch again — never a partial re-application. If nothing repaired, or the
   retried batch still fails, the ORIGINAL error is surfaced: a real refusal (a
   stale anchor, an overlap, a broken parse) is never buried. Non-clj calls pass
   straight through to `next`.

   Per repo doctrine the repair stays SYNTAX-ONLY: the smallest mechanical
   change that restores parseable source, never a semantic rewrite."
  [_env _op-kw args next]
  (try
    (next args)
    (catch clojure.lang.ExceptionInfo e
      (let
        [argv
         (vec args)

         path
         (first argv)

         edits
         (second argv)

         ;; ONE bare edit map is the same argument, coerced: `normalize-edits-arg`
         ;; accepts it on the editor side, so the repair must see it too.
         entries
         (cond (sequential? edits) (vec edits)
               (map? edits) [edits]
               :else nil)

         fixed
         (when (and (clj-source-file? path) (seq entries)) (mapv repair-edit-replacement entries))]

        (if (and fixed (not= fixed entries))
          (try (let [res (next (assoc argv 1 fixed))]
                 (if (string? (:result res))
                   (update res
                           :result
                           (fn [^String s]
                             (let [[head & tail] (str/split-lines s)]
                               (str/join "\n" (cons (str head " (delimiters repaired)") tail)))))
                   res))
               (catch Throwable retry-t
                 (tel/log!
                   {:level :warn
                    :id :clojure-patch-repair-retry-failed
                    :data {:path path :error (.getMessage retry-t)}}
                   "Clojure patch delimiter repair retry failed; surfacing original editor error")
                 (throw e))) ; repaired retry failed → original
          (throw e))))))

;; =============================================================================
;; Extension manifest
;; =============================================================================

;; No :ext/prompt-fn — the foundation advertises this pack's verbs through the
;; AUTO capability matrix (language_surface/capability-matrix). :ext/ctx-fn syncs
;; nREPLs into the canonical session resources view.
(def vis-extension
  (vis/extension
    {:ext/name "language-clojure"
     :ext/description
     "Clojure pack: managed nREPL; generic format/lint/test/repl handlers; formatting and delimiter repair. Active only in Clojure workspaces."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/activation-fn activation-fn
     :ext/ctx-fn nrepl-ctx/contribute
     :ext/language-tools [{:language "clojure"
                           :format-fn (fn [env arg]
                                        (clj-format-fn env arg))
                           :lint-fn clj-lint-fn
                           :test-fn test-runner/clj-test-fn
                           :repl-eval-fn clj-eval-fn
                           :start-repl-fn (fn [env op opts]
                                            (repl-start-fn env op opts))}]
     ;; Declarative cross-cutting op-hooks — registered/unregistered WITH this
     ;; extension's lifecycle (no imperative side effects at ns load). They make
     ;; the two Clojure editors NOT fail on unbalanced delimiters: an :around
     ;; that parinfer-repairs the new code — struct_patch's `:code`, patch's
     ;; trailing `replacement` — and retries once. :owner is set to this
     ;; extension automatically.
     :ext/op-hooks [{:op :struct_patch :phase :around :fn clj-struct-patch-no-fail-around}
                    {:op :patch :phase :around :fn clj-patch-no-fail-around}]
     :ext/kind "language"}))

(vis/register-extension! vis-extension)
