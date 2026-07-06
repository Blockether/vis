(ns com.blockether.vis.ext.language-clojure.core
  "vis-language-clojure — Clojure language handlers for Vis.

   Format/test/REPL are exposed through the generic language facade
   (`format`, `test`, `repl_eval`, `repl_start`, `repl_stop`) —
   `format` here does parinfer delimiter repair + cljfmt. The pack also registers
   a cross-cutting op-hook that auto repairs+formats `.clj` files after the
   foundation's struct_patch / patch / write, so no separate repair step is
   needed."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.foundation.environment.languages :as languages]
            [com.blockether.vis.ext.language-clojure.format :as fmt]
            [com.blockether.vis.ext.language-clojure.paren-repair :as repair]
            [com.blockether.vis.ext.language-clojure.lint :as lint]
            [com.blockether.vis.ext.language-clojure.nrepl-client :as nrepl-client]
            [com.blockether.vis.ext.language-clojure.nrepl-ctx :as nrepl-ctx]
            [com.blockether.vis.ext.language-clojure.repl-manager :as repl-manager]
            [com.blockether.vis.ext.language-clojure.test-runner :as test-runner]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.foundation.editing.core :as editing]))

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
  (let [root (some-> (:workspace/root env)
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
   `dir` written the way a human types it resolves to a real absolute path
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
  (let [d
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
  "Stable session-resource id for the REPL rooted at `dir` — the SAME id
   `repl-manager/id-of` stamps, so ctx, eval targeting, and the footer all agree
   on one name per dir. Addressing a REPL is always by this id."
  [dir]
  (repl-manager/id-of dir))

(defn register-repl-resource!
  "Mirror a session's managed nREPL into the session-scoped resource registry so
   it shows in ctx (resources) + the footer, and can be stopped/restarted by id
   from the agent or the UI. No-op without a session or a live spawn. The
   stop-fn/restart-fn thunks ARE the canonical lifecycle — the footer and
   resource_stop both drive repl-manager through them, scoped to `session-id`."
  [session-id dir aliases result]
  ;; `result` is repl-manager/start!'s STRING-keyed lifecycle map. The resource
  ;; map handed to `vis/register-resource!` is the CENTRAL resources.clj DATA
  ;; shape (keyword keys/values) — that projection is what crosses to the model,
  ;; and its strings-only migration lives in resources.clj (flagged hand-off).
  (when (and session-id (or (get result "pid") (get result "port")))
    (let [;; Prefer the aliases start! actually booted with (STRING names) so the
          ;; label/detail reflect the real [:dev :test] classpath even when the
          ;; caller passed none.
          aliases
          (or (seq (get result "aliases")) (map name (or aliases [])))

          id
          (repl-resource-id dir)]

      (vis/register-resource!
        session-id
        {:id id
         :kind :nrepl
         :label (str "nREPL "
                     (.getName (io/file dir))
                     (when (seq aliases) (apply str (map #(str " :" %) aliases))))
         :status (or (get result "status") :up)
         ;; `:detail` is passed THROUGH verbatim by resources.clj/->data (it only
         ;; stringifies its own keys + the kind/status/owner/language enums), so it
         ;; must already be STRING-keyed to survive the strings-only boundary.
         :detail (cond-> {"dir" dir}
                   (get result "port")
                   (assoc "port" (get result "port"))

                   (seq aliases)
                   (assoc "aliases" (vec aliases)))
         :pid (get result "pid")
         :owner :ext/language-clojure
         :language :clojure}
        {:stop-fn (fn []
                    (repl-manager/stop! session-id dir))
         :restart-fn (fn []
                       (repl-manager/stop! session-id dir)
                       (let [r (repl-manager/start! session-id dir {:aliases aliases})]
                         (register-repl-resource! session-id dir aliases r)
                         r))})
      ;; Surface the registration in the TUI (header toast) so a spawned REPL is
      ;; visible the moment it lands, not just as a silent ● bump in the footer.
      (vis/notify! (str "● nREPL up — "
                        (.getName (io/file dir))
                        (when-let [p (get result "port")]
                          (str " :" p)))
                   :level :success
                   :ttl-ms 4000))))

(defn clj-repl-fn
  "Manage THIS session's workspace nREPL(s). Positional op (default \"status\") +
   optional opts dict `{\"dir\": <path>, \"aliases\": [\"dev\", \"test\"]}`:

     \"status\"  — managed-process view for this session (always allowed)
     \"start\"   — self-start a project nREPL subprocess (always allowed, autostart
                 is the norm; the model rarely needs to call this by hand)
     \"restart\" — stop then start (always allowed)
     \"stop\"    — stop this session's Vis-managed nREPL (always allowed)

   \"dir\" runs the REPL in a subdir (e.g. an extension) instead of the workspace
   root — that's how MULTIPLE REPLs coexist, each addressed by its id. \"aliases\"
   default to [:dev :test] (full deps/paths, user :main-opts dropped). Live nREPL
   state already rides in ctx under `:session/env :languages :clojure :nrepl`;
   this tool acts on it."
  ([env] (clj-repl-fn env "status" nil))
  ([env op] (clj-repl-fn env op nil))
  ([env op opts]
   (let [root
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
         (resolve-repl-dir root (get opts "dir"))

         aliases
         (coerce-aliases (get opts "aliases"))]

     (case op
       "status"
       (extension/success {:result (repl-manager/status sid dir)})

       "stop"
       (let [r (repl-manager/stop! sid dir)]
         ;; Drop the session's resource mirror (best-effort; the thunk
         ;; already ran the real teardown above).
         (vis/unregister-resource! sid (repl-resource-id dir))
         (extension/success {:result r}))

       ("start" "restart")
       (do (when-not (.isDirectory (io/file dir))
             (throw (ex-info (str "clj_repl \"" op "\" target dir does not exist: " dir)
                             {:type :clj/bad-args :dir dir})))
           (let [result (if (= op "restart")
                          (do (repl-manager/stop! sid dir)
                              (repl-manager/start! sid dir {:aliases aliases}))
                          (repl-manager/start! sid dir {:aliases aliases}))]
             ;; Mirror the live REPL into the session resource registry → ctx +
             ;; footer + stoppable by id.
             (register-repl-resource! sid dir aliases result)
             (extension/success {:result result})))

       (throw
         (ex-info
           (str "clj_repl unknown op: " (pr-str op))
           {:type :clj/bad-args
            :got op
            :examples
            ["clj_repl()" "clj_repl(\"status\")" "clj_repl(\"start\")"
             "clj_repl(\"start\", {\"dir\": \"extensions/languages/vis-language-clojure\", \"aliases\": [\"dev\", \"test\"]})"
             "clj_repl(\"stop\")" "clj_repl(\"restart\")"]}))))))

(defn ui-start-repl!
  "Channel-invokable nREPL start for the Resources UI (web modal / TUI F4).
   Resolves the workspace dir from `env`, starts a managed nREPL with `aliases`
   (vec/seq of keyword-or-string names, or nil → [:dev :test]), and mirrors it
   into the session resource registry (ctx + footer + stop/restart). ALWAYS
   allowed: a user clicking Start is explicit consent. Returns the start result."
  [env aliases]
  (let [root
        (env-root env)

        sid
        (:session-id env)

        dir
        (resolve-repl-dir root (:startable/dir env))

        als
        (coerce-aliases aliases)]

    (when-not (.isDirectory (io/file dir))
      (throw (ex-info (str "REPL target dir does not exist: " dir) {:type :clj/bad-args :dir dir})))
    (let [result (repl-manager/start! sid dir {:aliases als})]
      (register-repl-resource! sid dir als result)
      result)))

(defn available-aliases
  "Alias names declared in the workspace `deps.edn` — surfaced to the UI so the
   user picks REAL aliases (`:dev`, `:test`, …) instead of guessing. Returns a
   sorted vec of strings WITHOUT the leading colon; empty on any read/parse
   failure or a non-deps project."
  [env]
  (try (let [root
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

(defn clj-eval-fn
  "Evaluate Clojure over the session's nREPL. Target resolution (autostart is ON):
     - explicit `port` → dial it directly (escape hatch; no autostart/recovery);
     - `id`/`repl_id`  → the REPL registered under that id in THIS session;
     - no id, 0 REPLs  → AUTOSTART one in the workspace root (:dev :test) and use it;
     - no id, 1 REPL   → use it (the implicit default);
     - no id, >1 REPLs → error listing ids (the model must pick one).
   The just-autostarted REPL is mirrored into the session resources on the next
   ctx render (footer + stop/restart).

   AUTO-RECOVERY: a managed REPL whose OS process is alive but whose nREPL socket
   is dead/wedged (a boot that never bound its port, or a crashed server thread)
   makes the resolver hand back a port nothing listens on. On that connect
   failure we restart THIS session's REPL for the target dir and retry the eval
   ONCE — so an eval Just Works instead of surfacing a dead-port error. A pinned
   `port`, or a real eval/timeout error, is never retried."
  ([env arg]
   (let [m
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

         rid
         (some-> (or (get m "id") (get m "repl_id"))
                 str
                 str/trim
                 not-empty)

         root
         (env-root env)

         sid
         (:session-id env)

         run
         (fn [p]
           ;; Carry the evaluated FORM back on the result (string key, crosses the
           ;; strings-only boundary) so the repl_eval op-card can show it in the
           ;; collapsed chip / expanded FORM section — the render fn sees only the
           ;; result map, never the call args.
           (assoc (nrepl-client/eval! {:host host
                                       :port p
                                       :code code
                                       :ns ns
                                       :pretty? true
                                       :timeout-ms (or timeout_ms 30000)})
             "code" code))]

     (if port
       ;; Explicit port: the escape hatch — dial exactly what was asked, with no
       ;; autostart and no auto-recovery.
       (extension/success {:result (run port)})
       (let [target
             (repl-manager/resolve-target! sid rid root)

             tport
             (:port target)]

         (when-not tport
           (throw
             (ex-info
               "no nREPL port resolved — could not autostart a project nREPL (no deps.edn / project.clj / bb.edn?)"
               {:type :clj/no-port :workspace-root root})))
         (extension/success
           {:result (try (run tport)
                         (catch clojure.lang.ExceptionInfo e
                           (if (and (= :clj/nrepl-connect-failed (:type (ex-data e))) (:dir target))
                             ;; Managed target's process is alive but not serving nREPL:
                             ;; restart it and retry the eval once.
                             (let [fresh (repl-manager/restart-for-dir! sid (:dir target))]
                               (when-not (:port fresh) (throw e))
                               ;; Drop the stale resource mirror so ctx re-adds it with the
                               ;; fresh port on the next render.
                               (vis/unregister-resource! sid (repl-resource-id (:dir target)))
                               (run (:port fresh)))
                             (throw e))))}))))))

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
   Paths that aren't under root, and non-path sentinels like `<stdin>`, pass
   through unchanged."
  [^java.io.File root file]
  (let [s (str file)]
    (if (and root (seq s) (not= s "<stdin>"))
      (try (let [rp (.toPath (.getCanonicalFile root))
                 fp (.toPath (.getCanonicalFile (io/file s)))]

             (if (.startsWith fp rp) (str (.relativize rp fp)) s))
           (catch Throwable _ s))
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

(defn- expand-clj-source-files
  "Expand `paths` (resolved against workspace `root` when relative) into concrete
   Clojure source files. A DIRECTORY is walked RECURSIVELY, collecting every
   `.clj`/`.cljs`/`.cljc`/`.cljx` file under it; a plain file is kept
   as-is; a non-existent path is dropped. Returns a de-duplicated, sorted vector
   of absolute path strings."
  [^java.io.File root paths]
  (->> paths
       (mapcat (fn [p]
                 (let [g
                       (io/file (str p))

                       f
                       (if (.isAbsolute g) g (io/file root (str p)))]

                   (cond (.isDirectory f) (->> (file-seq f)
                                               (filter #(.isFile ^java.io.File %))
                                               (filter #(clj-source-file? (str %))))
                         (.isFile f) [f]
                         :else nil))))
       (map str)
       (distinct)
       (sort)
       (vec)))

(defn- clj-format-one-file!
  "Format a single file at `path` IN PLACE (paren-repair + cljfmt), writing
   back ONLY when the content changes. Returns a per-file result map with the
   workspace-relative path."
  [env path]
  (let [code
        (slurp (str path))

        out
        (clj-repair+format code (or path (:workspace/root env)))]

    (when (not= out code) (spit (str path) out))
    {"path" (relativize-path (io/file (or (:workspace/root env) ".")) path)
     "changed" (not= out code)
     "repaired" (not= (or (repair/fix-delimiters code) code) code)
     "wrote" (not= out code)}))

(defn clj-format-fn
  "Format Clojure source via the language facade (`format_code`). Accepts:
     - a raw code string / {\"code\": ...}   -> return the formatted text
     - {\"path\": \"src/foo.clj\"}              -> format that file IN PLACE
     - {\"paths\": [\"src\" \"test\" ...]}        -> format those paths IN PLACE; a
         DIRECTORY is walked RECURSIVELY (every .clj/.cljs/.cljc/.cljx under it)
     - nothing / {}                         -> format the workspace's src + test
         (or the root) RECURSIVELY
   Paths are resolved against the workspace root when relative."
  ([arg] (clj-format-fn nil arg))
  ([env arg]
   (let [root
         (io/file (or (:workspace/root env) "."))

         paths
         (when (map? arg) (get arg "paths"))

         path
         (when (map? arg) (get arg "path"))

         has-code?
         (and (map? arg) (contains? arg "code"))

         default?
         (or (nil? arg) (and (map? arg) (not (seq paths)) (not path) (not has-code?)))

         batch
         (cond (seq paths) (expand-clj-source-files root paths)
               default? (expand-clj-source-files root
                                                 (let [ds (->> ["src" "test"]
                                                               (map #(io/file root %))
                                                               (filter #(.exists ^java.io.File %))
                                                               (mapv str))]
                                                   (if (seq ds) ds [(str root)]))))]

     (if batch
       (let [files (mapv #(clj-format-one-file! env %) batch)]
         (extension/success {:result {"op" "clj-format"
                                      "files" files
                                      "changed" (count (filter #(get % "changed") files))}}))
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

          out
          (clj-repair+format code (or path (:workspace/root env)))]

         (when (and path (not= out code)) (spit (str path) out))
         (extension/success
           {:result (cond-> {"op" "clj-format"
                             "changed" (not= out code)
                             "repaired" (not= (or (repair/fix-delimiters code) code) code)
                             "text" out}
                      path
                      (assoc "path"
                        (relativize-path (io/file (or (:workspace/root env) ".")) path) "wrote"
                        (not= out code)))}))))))

(defn clj-lint-fn
  "clj-kondo lint via the language facade (`lint_code`). Accepts:
     - a raw code string / {\"code\": ...}  -> lint it on stdin
     - {\"path\": \"src/foo.clj\"}           -> lint that file
     - {\"paths\": [\"src\", \"test\"]}        -> lint those paths
     - nothing / {}                       -> lint the workspace's src + test (or root)
   Paths are resolved against the workspace root when relative. Finding \"file\"
   paths are reported RELATIVE to the workspace root (absolute only when outside)."
  [env arg]
  (let [root
        (io/file (or (:workspace/root env) "."))

        path
        (when (map? arg) (get arg "path"))

        paths
        (when (map? arg) (get arg "paths"))

        code
        (cond (string? arg) arg
              (and (map? arg) (contains? arg "code")) (str (get arg "code"))
              :else nil)

        under
        (fn [p]
          (let [f (io/file (str p))]
            (str (if (.isAbsolute f) f (io/file root (str p))))))

        base
        (cond code (lint/lint-code code)
              path (lint/lint-paths [(under path)])
              (seq paths) (lint/lint-paths (mapv under paths))
              :else (let [defaults (->> ["src" "test"]
                                        (map #(io/file root %))
                                        (filter #(.exists ^java.io.File %))
                                        (mapv str))]
                      (lint/lint-paths (if (seq defaults) defaults [(str root)]))))]

    (extension/success
      {:result (assoc (update base
                              "findings"
                              (fn [fs]
                                (mapv #(update % "file" (partial relativize-path root)) fs)))
                 "language" "clojure")})))

;; ── Auto-repair hook: keep .clj source tidy after a generic edit op ──────────

(defn- edit-arg-paths
  "Edited file path(s) from a struct_patch/write call (a map with \"path\") or a
   patch call (a seq of such maps). Model-supplied args are STRING-keyed
   (strings-only boundary)."
  [args]
  (let [a (first args)]
    (cond (map? a) (when-let [p (get a "path")]
                     [p])
          (sequential? a) (keep #(when (map? %) (get % "path")) a)
          (string? a) [a]
          :else nil)))

(defn- resolve-under-root
  ^java.io.File [env path]
  (let [f (io/file (str path))]
    (if (.isAbsolute f) f (io/file (or (:workspace/root env) ".") (str path)))))

(defn- repair-clj-file!
  "Re-tidy a just-edited Clojure file IN PLACE (paren-repair + cljfmt), writing
   back ONLY when the content actually changes — a clean file is a no-op so
   nothing churns. Returns `{:changed? bool :structural? bool}` describing the
   rewrite (`:structural?` = parinfer MOVED/added/removed delimiters, i.e. NOT
   a whitespace-only cljfmt reflow — the case that can silently re-nest code),
   or nil when `path` is not an existing Clojure file."
  [env path]
  (let [f (resolve-under-root env path)]
    (when (and (clj-source-file? path) (.isFile f))
      (let [code (slurp f)
            fixed (repair/fix-delimiters code)
            structural (and (string? fixed) (not= fixed code))
            out (clj-repair+format code f)
            changed (and (string? out) (not= out code))]

        (when changed (spit f out))
        {:changed? changed :structural? (boolean structural)}))))

(defn clj-edit-repair-hook
  "An :after op-hook (registered on struct_patch / patch / write): after a
   SUCCESSFUL edit, paren-repair + cljfmt every Clojure file it touched, so the
   model never needs a separate repair/format step and a raw `patch` that left
   delimiters unbalanced is auto-corrected.

   It then makes the RESULT TRUTHFUL: the foundation rendered each file's
   `diff` from what the raw edit wrote, BEFORE this repair/format rewrote the
   bytes on disk, so without this the model would be shown its INTENT, not the
   file. Using the pre-edit content the foundation stashes on
   `:metadata :file-befores`, we re-diff every touched summary against the
   FINAL on-disk bytes, and when parinfer CHANGED STRUCTURE (moved delimiters,
   which can silently re-nest code) we flag the summary loudly
   (`repaired`/`note`) so a scope shift can never hide behind a stale diff.

   A throwing repair is caught + logged by the op-hook runner, never breaking
   the underlying edit."
  [env _op-kw args result]
  (if-not (:success? result)
    result
    (let [befores (get-in result [:metadata :file-befores])]
      (if-not (seq befores)
        ;; No pre-edit content stashed (e.g. a pack :around already committed
        ;; and built its own result) -- just tidy the files, result unchanged.
        (do (doseq [p (edit-arg-paths args)]
              (repair-clj-file! env p))
            result)
        ;; Repair each touched file, then RE-DIFF the model-facing summaries
        ;; against the final disk bytes and flag any structural repair.
        (let [reports (into {}
                            (keep (fn [{:keys [path before]}]
                                    (when-let [r (repair-clj-file! env path)]
                                      [path (assoc r :before before)]))
                                  befores))]
          (update result
                  :result
                  (fn [summaries]
                    (mapv (fn [s]
                            (if-let [{:keys [before structural?]} (get reports (get s "path"))]
                              (let [f (resolve-under-root env (get s "path"))
                                    after (when (.isFile f) (slurp f))
                                    s' (if after (editing/refresh-file-summary s before after) s)]

                                (cond-> s'
                                  structural?
                                  (assoc "repaired"
                                    true "note"
                                    (str "parinfer paren-repair CHANGED STRUCTURE "
                                         "(moved/added/removed delimiters); the diff "
                                         "shows the FINAL on-disk result -- verify the "
                                         "nesting is what you intended."))))
                              s))
                          summaries))))))))

(defn clj-struct-patch-no-fail-around
  "MIDDLEWARE (:around) on struct_patch so a Clojure structural edit does NOT
   fail on unbalanced delimiters. If the call throws and it targeted a `.clj`
   file with a `:code` form, parinfer-repair the code and retry ONCE. If the
   repair changes nothing — or the retried edit still fails — the ORIGINAL error
   is surfaced (we never bury a real structural failure). Non-clj / non-code
   calls pass straight through to `next`."
  [_env _op-kw args next]
  (try (next args)
       (catch clojure.lang.ExceptionInfo e
         (let [m
               (first args)

               path
               (and (map? m) (get m "path"))

               code
               (and (map? m) (get m "code"))

               fixed
               (when (and (clj-source-file? path) (string? code)) (repair/fix-delimiters code))]

           (if (and fixed (not= fixed code))
             (try (next (assoc-in (vec args) [0 "code"] fixed)) (catch Throwable _ (throw e))) ; repaired retry failed → original
             (throw e))))))

(defn clj-patch-no-fail-around
  "MIDDLEWARE (:around) on patch — the anchored-edit mirror of
   `clj-struct-patch-no-fail-around`. A raw `patch` whose `:replace` leaves a
   cleanly-parsing `.clj` file with unbalanced delimiters is REFUSED by the
   foundation's re-parse guard (a `{:success? false … :reason :syntax-error}`
   envelope, nothing written). The refusal's `:metadata` carries the
   WHOLE-BATCH `:candidate-plans` (every planned file with its edits applied,
   unwritten) plus `:broken-paths`.

   Here we parinfer-repair each broken candidate as a WHOLE SOURCE — fragment
   repair cannot fix CONTEXTUAL imbalance (a replacement that is locally
   balanced but swallows or duplicates an enclosing closer only shows up in
   the full file) — and, when every broken file is Clojure and every one
   repairs clean, COMMIT the whole batch ourselves and return a success
   envelope (the `:after` repair+format hook then cljfmt-polishes the files
   as it does for any successful edit). If any broken file is not Clojure, or
   any repair fails, the ORIGINAL refusal is surfaced untouched — we never
   bury a real failure. The baseline is always 'nothing written'."
  [env _op-kw args next]
  (let [result (next args)]
    (if (and (map? result)
             (false? (:success? result))
             (= :syntax-error (get-in result [:error :reason]))
             (seq (get-in result [:metadata :candidate-plans])))
      (let [plans (get-in result [:metadata :candidate-plans])
            broken (set (get-in result [:metadata :broken-paths]))
            repaired-plans (reduce (fn [acc {:keys [path after] :as plan}]
                                     (if-not (contains? broken path)
                                       (conj acc plan)
                                       (let [fixed (when (clj-source-file? path)
                                                     (try (repair/fix-delimiters (str after))
                                                          (catch Throwable _ nil)))]
                                         ;; unchanged output means the brokenness was not a
                                         ;; delimiter problem — nothing we can fix here.
                                         (if (and (string? fixed) (not= fixed (str after)))
                                           (conj acc
                                                 (assoc plan
                                                   :after fixed
                                                   :repaired? true))
                                           (reduced nil)))))
                                   []
                                   plans)]

        (if (nil? repaired-plans)
          result
          (do
            (doseq [{:keys [path after]} repaired-plans]
              (let [f (resolve-under-root env path)]
                (io/make-parents f)
                (spit f after)))
            (extension/success
              ;; `:op`/`:metadata` are ENVELOPE fields (op-tag + internal
              ;; timing/aux) — keyword. The `:result` VALUE is the model-facing
              ;; per-file summary that crosses the strings-only boundary → STRING
              ;; keys + STRING enum values.
              {:op :patch
               :path (or (:path (first repaired-plans)) ".")
               :kind :file
               :result (mapv (fn [{:keys [path repaired?]}]
                               (cond-> {"path" path "op" "update" "changed" true}
                                 repaired?
                                 (assoc "repaired"
                                   true "note"
                                   "unbalanced delimiters auto-repaired (parinfer) before write")))
                             repaired-plans)
               :metadata {:mode :exact-replace
                          :file-count (count repaired-plans)
                          :changed-count (count repaired-plans)
                          :auto-repaired-paths (mapv :path (filter :repaired? repaired-plans))}}))))
      result)))

;; =============================================================================
;; Extension manifest
;; =============================================================================

;; No :ext/prompt-fn — the foundation advertises this pack's verbs through the
;; AUTO capability matrix (language_surface/capability-matrix). nREPL state still
;; rides in ctx via :ext/ctx-fn.
(def vis-extension
  (vis/extension
    {:ext/name "language-clojure"
     :ext/description
     "Clojure language pack: live nREPL state in ctx, generic language-surface handlers for format/test/repl, and REPL/format/paren-repair tooling. Activates only when the workspace has Clojure sources."
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
                                            (clj-repl-fn env op opts))}]
     ;; Declarative cross-cutting op-hooks — registered/unregistered WITH this
     ;; extension's lifecycle (no imperative side effects at ns load). They keep
     ;; Clojure source tidy after the foundation's struct_patch / patch / write
     ;; (an :after repair+format), and make struct_patch AND patch NOT fail on
     ;; unbalanced delimiters (struct_patch: an :around that repairs the :code
     ;; + retries; patch: an :around that WHOLE-SOURCE-repairs the refusal's
     ;; candidate plans and commits the batch). :owner is set to this
     ;; extension automatically.
     :ext/op-hooks [{:op :struct_patch :phase :after :fn clj-edit-repair-hook}
                    {:op :patch :phase :after :fn clj-edit-repair-hook}
                    {:op :write :phase :after :fn clj-edit-repair-hook}
                    {:op :struct_patch :phase :around :fn clj-struct-patch-no-fail-around}
                    {:op :patch :phase :around :fn clj-patch-no-fail-around}]
     ;; Declarative startable resource — the Resources UI (web modal / TUI F4)
     ;; renders this generically: its title, the proposed deps.edn aliases, and
     ;; Start. Always allowed (the self-start flag gates only the model).
     :ext/startable-resources [{:kind :nrepl
                                :dir? true
                                :label "nREPL"
                                :options-label "aliases"
                                ;; options-fn RENDERS aliases in deps.edn spelling (":dev"); start-fn
                                ;; consumes its own display format back — the exact inverse (drop the
                                ;; one ":" this options-fn prepended). No regex, no dual-spelling
                                ;; tolerance.
                                :options-fn (fn [env]
                                              (mapv #(str ":" %) (available-aliases env)))
                                :start-fn (fn [env selected]
                                            (ui-start-repl! env (map #(subs (str %) 1) selected)))}]
     :ext/kind "language"}))

(vis/register-extension! vis-extension)