(ns com.blockether.vis.ext.language-typescript-bun.core
  "vis-language-typescript-bun — a managed TypeScript/Bun REPL exposed through
   the generic language facade (repl_start / repl_status / repl_stop / repl_eval) plus
   run_tests -> `bun test`. Activates only when the workspace looks like a Bun
   project. The REPL is a persistent `bun` subprocess running a line-framed
   JSON eval server with REAL REPL semantics (persistent globals, top-level
   await, cache-busted `reload(path)`), registered as a session resource so it
   shows in ctx + the footer and is stoppable by id. The point is the Clojure
   reloaded workflow for TS: the app STARTS from the repl, lives in the repl,
   and its state is printable — no debugger needed."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.language-typescript-bun.repl-manager :as repl]
            [com.blockether.vis.ext.language-typescript-bun.runner :as runner]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.test-contract :as contract]))

;; Activation

(def ^:private source-extensions
  "File suffixes Bun runs natively — TS/TSX/JS/JSX plus the .mjs/.cjs module
   variants. Any of them in a plain `package.json` workspace marks it a
   Bun/Node project the pack should light up on."
  [".ts" ".tsx" ".js" ".jsx" ".mjs" ".cjs"])

(defn- ts-file?
  [^java.io.File f]
  (and (.isFile f)
       (let [n (.getName f)]
         (boolean (some (fn [^String ext]
                          (str/ends-with? n ext))
                        source-extensions)))))

(defn- workspace-has-bun?
  [env]
  (let
    [root (some-> (:workspace/root env)
                  io/file)]
    (when (and root (.isDirectory root))
      (or (some #(.exists (io/file root %)) ["bunfig.toml" "bun.lock" "bun.lockb" ".bun-version"])
          ;; a generic package.json workspace with TS/TSX/JS/JSX sources runs on
          ;; bun too (bounded scan for one anywhere, lazy file-seq, capped)
          (and (.exists (io/file root "package.json"))
               (boolean (some ts-file? (take 3000 (file-seq root)))))))))

(defn- activation-fn [env] (boolean (workspace-has-bun? env)))

;; Helpers

(defn- env-root
  ^String [env]
  (or (:workspace/root env)
      (throw (ex-info "typescript tool fired without :workspace/root in env"
                      {:type :ts/no-workspace}))))

(defn- resolve-dir
  ^String [root dir]
  (let [d (paths/expand-home (str (or dir "")))]
    (.getCanonicalPath (cond (= "" d) (io/file root)
                             (.isAbsolute (io/file d)) (io/file d)
                             :else (io/file root d)))))

(defn- repl-resource-id
  [dir id]
  (let
    [id (some-> id
                str
                str/trim)]
    (if (seq id) id (str "bunrepl:" dir))))

(defn register-repl-resource!
  "Mirror a managed Bun REPL into the session resource registry (ctx + footer +
   stop by id; no restart — stop, then start). No-op without a session or a live pid."
  [session dir result & [id]]
  ;; `result` is repl/start!'s STRING-keyed lifecycle map. The resource map is
  ;; the CENTRAL resources.clj DATA shape (keyword keys), but `:detail` is
  ;; passed THROUGH verbatim, so it must already be STRING-keyed.
  (when (and session (get result "pid"))
    (vis/register-resource! session
                            {:id (repl-resource-id dir id)
                             :kind :repl
                             :label (str "bun REPL " (.getName (io/file dir)))
                             :status (or (get result "status") :up)
                             :detail {"cwd" dir "cmd" (get result "cmd")}
                             :pid (get result "pid")
                             :owner :ext/language-typescript-bun
                             :language :typescript}
                            {:stop-fn (fn []
                                        (repl/stop! dir))})
    (vis/notify! (str "● bun REPL up — " (.getName (io/file dir))) :level :success :ttl-ms 4000)))

;; Language-facade handlers

(defn- monorepo-root-hint
  "When `cwd` IS the workspace root and its package.json declares `workspaces`,
   a REPL there is almost always a mistake: it reads the ROOT tsconfig /
   package.json, so app code misbehaves (e.g. NestJS decorators crash without
   `experimentalDecorators`). Returns an actionable hint string listing the
   workspace app dirs, else nil."
  [root dir]
  (when (= (str dir) (.getCanonicalPath (io/file root)))
    (let
      [pj
       (io/file root "package.json")

       m
       (when (.exists pj) (try (json/read-json (slurp pj)) (catch Throwable _ nil)))

       ws
       (get m "workspaces")

       globs
       (cond (sequential? ws) ws
             (map? ws) (get ws "packages")
             :else nil)]

      (when (seq globs)
        (let
          [candidates
           (->> globs
                (mapcat (fn [g]
                          (let [g (str g)]
                            (if (str/ends-with? g "/*")
                              (let [d (io/file root (subs g 0 (- (count g) 2)))]
                                (when (.isDirectory d) (.listFiles d)))
                              [(io/file root g)]))))
                (filter (fn [^java.io.File f]
                          (and f (.isDirectory f) (.exists (io/file f "package.json")))))
                (map (fn [^java.io.File f]
                       (str (.getName (.getParentFile f)) "/" (.getName f))))
                sort
                (take 8))

           suggestion
           (or (first (filter #(str/ends-with? % "/api") candidates))
               (first candidates)
               "apps/<app>")]

          (str "This is a Bun MONOREPO ROOT (package.json has \"workspaces\") — a REPL "
               "here picks up the ROOT tsconfig/package.json, so app code misbehaves "
               "(e.g. NestJS decorators crash). Pass the app dir explicitly: "
               "repl_eval(\"typescript\", code, {\"cwd\": \""
               suggestion
               "\"})."
               (when (seq candidates) (str " Workspace dirs: " (str/join ", " candidates) "."))
               " To force a root REPL anyway: repl_start(\"typescript\", {\"cwd\": \".\"})."))))))

(defn ts-start-repl-fn
   "REPL-lifecycle handler for TypeScript/Bun. The facade's `repl_start` / `repl_status` /
   `repl_stop` verbs reach a pack as a positional `op` STRING plus opts
   `{dir, id, env}` — there is NO restart (stop, then start), and a `repl_start`
   for a REPL that is already running REUSES it, refusing only when this call
   named a different `env`. `op` arrives as a STRING from the model
   (strings-only boundary) — dispatch on it, no keyword minting."
  [env op opts]
  (let
    [root
     (env-root env)

      ;; A MISSING op must never spawn: every pack defaults to "status", the one
      ;; step with no side effect.
      op
      (if (string? op) op "status")

     id
     (or (get opts "id") (get opts "repl_id"))

     dir
     (resolve-dir root (get opts "cwd"))]

     (case op
       "status"
       (extension/success {:result (assoc (repl/status dir) "id" (repl-resource-id dir id))})

       "stop"
       (let [r (assoc (repl/stop! dir) "id" (repl-resource-id dir id))]
         (vis/unregister-resource! (:session-id env) (repl-resource-id dir id))
         (extension/success {:result r}))

       "start"
       (do
         ;; Starting at a monorepo ROOT without an explicit cwd is (almost)
         ;; always a mistake — refuse with the app-dir hint. Explicit
         ;; {"cwd": "."} still forces a root REPL.
         (when (nil? (get opts "cwd"))
           (when-let [hint (monorepo-root-hint root dir)]
             (throw (ex-info hint {:type :ts/monorepo-root :dir dir}))))
         (let [r (assoc (repl/start! dir (assoc (or opts {})
                                           "id" (repl-resource-id dir id)
                                           :session-id (:session-id env)))
                   "id" (repl-resource-id dir id))]
           (register-repl-resource! (:session-id env) dir r id)
           (extension/success {:result r})))

      (throw (ex-info (str "TypeScript REPL lifecycle: unknown op "
                           (pr-str op)
                           " — the verbs are repl_start / repl_status / repl_stop; there is no"
                           " repl_connect for Bun, Vis owns the runtime process.")
                      {:type :ts/bad-args :got op})))))

(defn ts-repl-eval-fn
  "repl_eval handler for TypeScript/Bun. Accepts a code string or
   `{code, dir, timeout_ms}`. Requires a running REPL for the dir, then evaluates
   (globals persist across calls; top-level await works;
   `reload(path)` re-imports a project module cache-busted)."
  [env arg]
  (let
    [root
     (env-root env)

     code
     (cond (string? arg) arg
           (map? arg) (str (or (get arg "code") (get arg "source")))
           :else (throw (ex-info "repl_eval(typescript) expects a code string or {\"code\": ...}"
                                 {:type :ts/bad-args :got arg})))

     dir
     (resolve-dir root (and (map? arg) (get arg "cwd")))

     tmo
     (and (map? arg) (get arg "timeout_ms"))]

    (when-not (= "up" (get (repl/status dir) "status"))
      ;; Preserve the more specific monorepo error when the caller omitted dir.
      (when-not (and (map? arg) (get arg "cwd"))
        (when-let [hint (monorepo-root-hint root dir)]
          (throw (ex-info hint {:type :ts/monorepo-root :dir dir}))))
      ;; Home-homogenized: the message reads `~/app`, matching the REPL ids in
      ;; session["resources"] — and `resolve-dir` expands `~` back, so the cwd
      ;; shown can be pasted straight into the retry call.
      (let [shown (paths/abbreviate-home (str dir))]
        (throw (ex-info (str "TypeScript REPL is not up for "
                             shown
                             "; call repl_start(\"typescript\", {\"cwd\": "
                             (pr-str shown)
                             "}) first")
                        {:type :ts/no-repl :dir dir}))))
    ;; Carry the evaluated code back on the result (string key) so the shared
    ;; repl_eval op-card can surface the FORM section.
    (let [res (repl/eval! dir code tmo)]
      (extension/success {:result (cond-> res
                                    (map? res)
                                    (assoc "code" code))}))))

(defn- tail-str [^String s ^long n] (if (<= (count s) n) s (subs s (- (count s) n))))

(defn- bun-counts
  "`bun test`'s own summary lines (`N pass`, `N fail`) in the run_tests CONTRACT's
   words: `{\"pass\" n \"fail\" n}`. A key bun did not print is ABSENT, not zero —
   a crashed run reports UNKNOWN counts instead of a green-looking zero. Bun has
   no separate erroring outcome, so a test that threw is one of its `fail`s and
   the result carries no `errored`."
  [^String s]
  (let
    [n
     (fn [re]
       (some-> (second (re-find re (str s)))
               parse-long))

     pass
     (n #"(?m)^\s*(\d+) pass")

     fail
     (n #"(?m)^\s*(\d+) fail")]

    (cond-> {}
      pass
      (assoc "pass" pass)

      fail
      (assoc "fail" fail))))

(defn- test-command
  "The `bun test` argv for ONE call: the resolved runner command, then the `-t`
   name pattern and the path targets the `{paths}` node ids split into. bun's -t
   is a REGEX over the test name and bun keeps ONE pattern, so several node ids
   ALTERNATE inside it instead of fighting over the flag. Pure, so the grammar is
   pinned without launching bun."
  [dir opts]
  (let
    [ids
     (mapv contract/split-node-id (map str (get opts "paths")))

     locations
     (into [] (keep :path) ids)

     names
     (into [] (comp (keep :var) (distinct)) ids)]

    (cond-> (conj (runner/resolve-command dir) "test")
      (seq names)
      (conj "-t" (str/join "|" names))

      (seq locations)
      (into locations))))

(defn ts-test-fn
  "run_tests handler: `bun test` in the workspace (or `{dir}`), narrowed by
   `{paths [...]}` — files, directories, or `<path>::<test-name>` NODE IDS. The
   `::<test-name>` half becomes bun's own `-t` (a name REGEX), so the several
   names of one run are joined with `|` into the single pattern bun accepts, and
   a PATHLESS `::<test-name>` filters the whole suite. There is no second
   `filter` key: one grammar names where AND which, in every pack.
   Returns the parsed pass/fail counts + the output tail."
  [env arg]
  (let
    [root
     (env-root env)

     opts
     (if (map? arg) arg {})

     _
     (when (contains? opts "filter")
       (throw (ex-info (str "run_tests(typescript) no longer takes filter — put the test name IN"
                            " the path as a node id instead. {\"paths\":"
                            " [\"src/math.test.ts::adds\"]} runs that one test, and \"::adds\""
                            " finds it wherever it lives.")
                       {:type :ts/bad-args :got arg})))

     dir
     (resolve-dir root (get opts "cwd"))

     cmd
     (test-command dir opts)

     launch
     (vis/session-process-launch (:session-id env) cmd)

     pb
     (doto (ProcessBuilder. ^java.util.List (:argv launch))
       (.directory (io/file dir))
       (.redirectErrorStream true))

     _env
     (let [^java.util.Map e (.environment ^ProcessBuilder pb)]
       (when (:replace-env? launch) (.clear e))
       (doseq [[k v] (:env launch)]
         (.put e ^String k ^String v)))

     p
     (.start pb)

     out
     (future (slurp (.getInputStream p)))

     done?
     (.waitFor p 300 java.util.concurrent.TimeUnit/SECONDS)]

    (when-not done? (.destroyForcibly p))
    (let [s (str @out)]
      (extension/success {:result (merge {"mode" "cli"
                                          "framework" "bun:test"
                                          "tool" "bun"
                                          "command" (str/join " " cmd)
                                          "cwd" dir
                                          "exit" (if done? (.exitValue p) nil)
                                          "timed_out" (not done?)
                                          "output" (tail-str s 8000)}
                                         (bun-counts s))}))))

;; Manifest

;; No :ext/prompt-fn — the foundation advertises repl_eval / repl /
;; run_tests through the AUTO capability matrix; repl_eval's own result
;; ({ok,out,err,value,data,type,exc}; opaque values carry __type__/__attrs__/
;; __opaque__) is self-documenting.

(def ^:private facade-languages
  "Every grammar Bun runs natively — TS, TSX, JS, JSX — each routed to the SAME
   managed-Bun handlers. Registering all four means the facade resolves however
   the language is derived: an explicit `repl_eval(\"jsx\", …)`, a `.tsx` file's
   detected grammar, or a javascript/typescript workspace primary language."
  ["typescript" "javascript" "tsx" "jsx"])

(def vis-extension
  (vis/extension
    {:ext/name "language-typescript-bun"
     :ext/description
     "Bun TS/JS pack: managed `repl_start`/`repl_eval` with persistent globals, top-level await, `reload()`; `run_tests` uses `bun test`. Covers TS/TSX/JS/JSX; active in Bun/Node workspaces."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/activation-fn activation-fn
     :ext/language-tools (mapv (fn [language]
                                 {:language language
                                  :test-fn ts-test-fn
                                  :repl-eval-fn ts-repl-eval-fn
                                  :start-repl-fn (fn [env op opts]
                                                   (ts-start-repl-fn env op opts))})
                               facade-languages)
     :ext/kind "language"}))

(vis/register-extension! vis-extension)
