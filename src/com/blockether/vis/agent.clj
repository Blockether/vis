(ns com.blockether.vis.agent
  "Sandcastle-inspired agent orchestration over svar RLM.

   Define agents as data, register tools, run queries programmatically.
   Defaults to Blockether provider when BLOCKETHER_OPENAI_API_KEY is set.

   Example:
     (def reviewer (agent {:name \"reviewer\"
                           :system-prompt \"You are a senior Clojure engineer.\"
                           :tools [(tool 'read-file slurp
                                    {:doc \"Read file contents\"
                                     :params [{:name \"path\" :type :string}]
                                     :returns {:type :string}})]}))
     (run! reviewer \"Review auth.clj\")
     ;; => {:answer \"...\" :iterations 5 :duration-ms 2340 :tokens {...} :cost {...}}"
  (:refer-clojure :exclude [agent run!])
  (:require
   [charred.api :as json]
   [com.blockether.svar.internal.rlm :as rlm]
   [com.blockether.vis.config :as config]
   [com.blockether.vis.languages.commons.edit :as edit]
   [com.blockether.vis.languages.commons.list :as list]
   [com.blockether.vis.languages.commons.read :as read]
   [com.blockether.vis.languages.commons.shell :as shell]
   [com.blockether.vis.languages.commons.write :as write]))

;;; ── Agent Definition ─────────────────────────────────────────────────────

(defn tool
  "Create a tool definition for agent registration.

   Params:
   - sym  — Symbol name for the tool in the RLM SCI sandbox
   - f    — Implementation function
   - opts — Map with :doc, :params, :returns, :examples

   Example:
     (tool 'read-file slurp
       {:doc \"Read a file from disk\"
        :params [{:name \"path\" :type :string :required true
                  :description \"Absolute file path\"}]
        :returns {:type :string :description \"File contents\"}})"
  [sym f opts]
  (assoc opts :sym sym :fn f))

(def base-tools
  "Common tools available to every agent: file read/write/edit, directory listing, shell execution."
  [read/tool-def
   write/tool-def
   edit/tool-def
   list/tool-def
   shell/tool-def
   shell/bg-read-tool-def
   shell/bg-kill-tool-def])

(defn default-system-prompt
  "Default system prompt for all vis agents. Includes workspace rules and FINAL format."
  []
  "You are a helpful AI assistant powered by vis agent framework.

WORKSPACE RULES:
- Your workspace (@P) has :context (auto-populated), :learnings (scratch notes), and REPL variables.
- :context is AUTO-POPULATED with reasoning + execution summaries after each iteration.
- :context PERSISTS across queries in this session — it IS your long-term memory.
- USE VARS: (def data (some-call ...)) — vars auto-persist across queries in the session.
  Context only shows [stored in var: data] for def'd values. Use the var name to access full data.
  IMPORTANT: vars are VALUES not functions. Use `data` to reference, NOT `(data)`. Maps are not callable with 0 args.
- Use (ctx-add! text) for extra notes. Use (ctx-remove! idx) or (ctx-replace! from to summary) to manage.
- When context exceeds 12 entries, a [SYSTEM_NUDGE] will ask you to clean up. ALWAYS obey it.
- <recent-messages> shows the last 3 user messages as safety net.

FINAL FORMAT (MANDATORY):
- ALWAYS use (FINAL {:answer [\"part1\" \"part2\"]}) — NEVER (FINAL \"string\").
- For long answers: (FINAL {:answer [\"part 1\" \"part 2\"]}). Elements are auto-joined.
- Include :learn for persistent insights: (FINAL {:answer [...] :learn [{:insight \"...\" :tags [\"tag\"]}]})")

(defn agent
  "Create an agent definition (data map).

   Options:
   - :name           — Agent name (string, default \"default\")
   - :description    — What the agent does
   - :system-prompt  — System instructions injected into the RLM system prompt
   - :tools          — Vector of additional tool definitions (from `tool`), merged after base-tools
   - :constants      — Map of {symbol value} constants for SCI sandbox
   - :hooks          — Hooks map (see svar docs: :iteration, :code-exec, :tool-call, :llm-call, :query + data hooks)
   - :model          — Override default model selection
   - :max-iterations — Max RLM iterations (default 50)
   - :path           — Persistent storage path (default ~/.vis/agents/<name>)
   - :no-base-tools? — If true, skip base-tools (default false)

   Example:
     (agent {:name \"code-reviewer\"
             :description \"Reviews Clojure code for quality\"
             :system-prompt \"You are a senior Clojure engineer. Review code for bugs, performance, and style.\"
             :tools [(tool 'read-file slurp {...})]
             :model \"claude-sonnet-4-6\"
             :max-iterations 30})"
  [{:keys [name no-base-tools?] :as opts}]
  (let [agent-name  (or name "default")
        extra-tools (or (:tools opts) [])
        all-tools   (if no-base-tools?
                      extra-tools
                      (into (vec base-tools) extra-tools))]
    (merge {:name           agent-name
            :tools          all-tools
            :constants      {}
            :max-iterations 50
            :system-prompt  (or (:system-prompt opts) (default-system-prompt))
            :path           (str (System/getProperty "user.home") "/.vis/agents/" agent-name)}
           (assoc opts :tools all-tools))))

;;; ── Config Resolution ────────────────────────────────────────────────────
;; Delegates to config.clj — single source of truth for config I/O.
;; See SAMPLE_CONFIG.edn for svar-native format.

;;; ── Environment Info ─────────────────────────────────────────────────────

(defn environment-info
  "Build an environment context string (CWD, platform, shell) for injection
   into the agent system prompt.  Mirrors Claude Code's # Environment section."
  []
  (let [cwd    (System/getProperty "user.dir")
        os     (System/getProperty "os.name")
        arch   (System/getProperty "os.arch")
        shell  (or (System/getenv "SHELL") "unknown")
        user   (System/getProperty "user.name")
        home   (System/getProperty "user.home")]
    (str "\n<environment>\n"
         "  Working directory: " cwd "\n"
         "  Home directory: " home "\n"
         "  User: " user "\n"
         "  Platform: " os " (" arch ")\n"
         "  Shell: " shell "\n"
         "</environment>")))

;;; ── Execution ────────────────────────────────────────────────────────────

(defn run!
  "Execute a one-shot agent query.

   Creates RLM env → registers tools/constants → queries → disposes → returns.

   Returns map with:
   - :answer       — The agent's response (string or structured data if :spec)
   - :iterations   — Number of RLM iterations executed
   - :duration-ms  — Total wall-clock time
   - :tokens       — {:input N :output N :reasoning N :cached N :total N}
   - :cost         — {:input-cost N :output-cost N :total-cost N :model str}
   - :trace        — Full execution trace (always included)
   - :error        — Error message (only on failure)

   Options:
   - :system-prompt  — Override agent's system prompt
   - :context        — Data context for the RLM (string → P handle, map → `context` var in SCI)
   - :spec           — svar output spec for structured responses
   - :model          — Override model (agent-level or per-run)
   - :max-iterations — Override max iterations
   - :hooks          — Per-run hooks override (deep-merged into env hooks)
   - :verify?        — Enable claim verification (default false)
   - :debug?         — Enable svar debug logging (default false)
   - :config         — Provider config override (skips ~/.vis/config.edn)"
  [agent-def prompt & [{:keys [system-prompt context spec model max-iterations hooks
                               verify? debug? config]
                        :as _opts}]]
  (let [rlm-cfg (config/resolve-config config)
        path    (:path agent-def)
         ;; Create environment with agent-level hooks
        env     (rlm/create-env (cond-> {:config rlm-cfg :path path}
                                  (:hooks agent-def) (assoc :hooks (:hooks agent-def))))
        ;; Register tools
        env     (reduce (fn [e {:keys [sym fn] :as tool-def}]
                          (rlm/register-env-fn! e sym fn
                                                (dissoc tool-def :sym :fn)))
                        env
                        (:tools agent-def))
        ;; Register constants
        env     (reduce-kv (fn [e sym value]
                             (rlm/register-env-def! e sym value
                                                    {:doc (str sym)
                                                     :returns {:type :any
                                                               :description (pr-str value)}}))
                           env
                           (:constants agent-def))
        ;; Merge query opts: agent defaults < per-run overrides
        iters   (or max-iterations (:max-iterations agent-def) 50)
        mdl     (or model (:model agent-def))
        q-opts  (cond-> {:max-iterations iters}
                  context      (assoc :context context)
                  spec         (assoc :spec spec)
                  mdl          (assoc :model mdl)
                  hooks        (assoc :hooks hooks)
                  verify?      (assoc :verify? true)
                  debug?       (assoc :debug? true))
        ;; System prompt: passed as first-class option to query-env!
        ;; Rendered as <agent_instructions> in the RLM system prompt.
        raw-sys (or system-prompt (:system-prompt agent-def))
        sys     (str raw-sys (environment-info))
        q-opts  (assoc q-opts :system-prompt sys)]
    (try
      (let [result (rlm/query-env! env prompt q-opts)]
        (cond-> {:answer      (:answer result)
                 :iterations  (:iterations result)
                 :duration-ms (:duration-ms result)
                 :tokens      (:tokens result)
                 :cost        (:cost result)
                 :trace       (:trace result)}
          (:status result)    (assoc :status (:status result))
          (:confidence result) (assoc :confidence (:confidence result))
          (:learn result)     (assoc :learn (:learn result))))
      (catch Exception e
        {:error     (ex-message e)
         :type      (str (type e))
         :exception e})
      (finally
        (rlm/dispose-env! env)))))

;;; ── Output Formatting ───────────────────────────────────────────────────

(defn- sanitize-for-json
  "Recursively prepare Clojure data for JSON serialization.
   Converts keyword keys to strings, drops nil values."
  [x]
  (cond
    (map? x)     (persistent!
                  (reduce-kv (fn [m k v]
                               (if (nil? v)
                                 m
                                 (assoc! m
                                         (cond (keyword? k) (name k)
                                               (symbol? k)  (str k)
                                               :else        k)
                                         (sanitize-for-json v))))
                             (transient {})
                             x))
    (coll? x)    (mapv sanitize-for-json x)
    (keyword? x) (name x)
    (symbol? x)  (str x)
    :else        x))

(defn result->json
  "Convert agent result to a JSON string."
  [result]
  (json/write-json-str (sanitize-for-json result)))

(defn result->edn
  "Convert agent result to a pretty-printed EDN string."
  [result]
  (pr-str result))


