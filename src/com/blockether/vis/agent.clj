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
   [com.blockether.svar.internal.llm :as llm]
   [com.blockether.svar.internal.rlm :as rlm]
   [com.blockether.vis.config :as config]
   [com.blockether.vis.languages.commons.edit :as edit]
   [com.blockether.vis.languages.commons.list :as list]
   [com.blockether.vis.languages.commons.read :as read]
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
  "Common tools available to every agent: file read/write/edit + directory listing.
   Shell execution is intentionally NOT in this set — the Telegram bot and any
   other untrusted caller would otherwise have arbitrary code execution on the
   host. If an agent truly needs shell, wire a scoped tool at the call site."
  [read/tool-def
   write/tool-def
   edit/tool-def
   list/tool-def])

(defn default-system-prompt
  "Tiny vis-side persona block. svar's own RLM prompt already covers ARCH,
   GROUNDING, iteration spec, execution receipts, def discipline, final
   mechanics. Here we only add what svar cannot know: the vis persona, the
   specific tools vis registers, cross-query var persistence. Keep short."
  []
  "You are a vis agent — senior engineer pair, Clojure SCI sandbox, file tools only.

Cross-query memory: `(def name \"doc\" val)` persists in this conversation; next
query sees it in <var_index>. If a symbol should exist but isn't there, call
(restore-var 'name) or (restore-vars ['a 'b]).

File tools (positional args, never maps): read-file, write-file, edit-file, list-dir.
No shell, no git CLI, no network. If the user needs a shell command, say so plainly.

Iteration budget: (request-more-iterations N) when you know the task will need more.")

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
   - :no-base-tools? — If true, skip base-tools (default false)

   Storage: every agent run goes into the shared `~/.vis/vis.mdb` DB. Each
   `run!` call creates a fresh :conversation by default (not resumed). To
   resume a prior run, pass `:conversation {:name \"agent:<name>:<id>\"}` or
   a `[:entity/id uuid]` lookup ref to `run!`.

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
            :system-prompt  (or (:system-prompt opts) (default-system-prompt))}
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
   - :context        — Data context for the RLM. String becomes P (the symbolic handle
                       with get-page/page-count); maps/vectors are bound to `context`
                       in the SCI sandbox.
   - :spec           — svar output spec for structured responses
   - :model          — Override model (agent-level or per-run)
   - :max-iterations — Override max iterations
   - :on-chunk       — Streaming callback fn. Receives {:iteration :thinking :code
                       :final :done?} on each partial chunk and once with :done? true
                       when the iteration produces a final answer.
   - :verify?        — Enable claim verification (default false)
   - :debug?         — Enable svar debug logging (default false)
   - :config         — Provider config override (skips ~/.vis/config.edn)
   - :conversation   — Resume a conversation. :latest reopens the most recent, or pass
                       a [:entity/id uuid] lookup ref."
  [agent-def prompt & [{:keys [system-prompt context spec model max-iterations on-chunk
                               verify? debug? config conversation]
                        :as _opts}]]
  (let [_cfg    (config/resolve-config config)  ;; ensures a router exists / throws on missing
        router  (config/get-router)
        ;; Shared SQLite DB for everything vis does (TUI, web, telegram, CLI).
        ;; Without `:conversation`, svar creates a fresh conversation scoped to
        ;; this run. Callers that want to resume pass `:conversation {:name …}`
        ;; or a [:entity/id uuid] lookup ref.
        env     (rlm/create-env router
                  (cond-> {:db config/db-path}
                    conversation (assoc :conversation conversation)))
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
                  on-chunk     (assoc :on-chunk on-chunk)
                  verify?      (assoc :verify? true)
                  debug?       (assoc :debug? true))
        ;; System prompt: passed as first-class option to query-env!
        ;; Rendered as <agent_instructions> in the RLM system prompt.
        raw-sys (or system-prompt (:system-prompt agent-def))
        sys     (str raw-sys (environment-info))
        q-opts  (assoc q-opts :system-prompt sys)
        ;; svar requires a vector of message maps, not a bare string.
        messages (if (string? prompt) [(llm/user prompt)] prompt)]
    (try
      (let [result (rlm/query-env! env messages q-opts)]
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


