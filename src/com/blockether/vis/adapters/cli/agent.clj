(ns com.blockether.vis.adapters.cli.agent
  "Sandcastle-inspired agent orchestration over svar RLM.

   Define agents as data, register tools, run queries programmatically.
   Defaults to Blockether provider when BLOCKETHER_OPENAI_API_KEY is set.

   Example:
     (def reviewer (agent {:name \"reviewer\"
                           :system-prompt \"You are a senior Clojure engineer.\"
                           :tools [(tool 'read-file slurp
                                      {:doc \"Read file contents\"})]}))
     (run! reviewer \"Review auth.clj\")
     ;; => {:answer \"...\" :iterations 5 :duration-ms 2340 :tokens {...} :cost {...}}"
  (:refer-clojure :exclude [agent run!])
  (:require
   [charred.api :as json]
   [clojure.string :as str]
   [com.blockether.svar.internal.llm :as llm]
   [com.blockether.vis.loop.conversations.core :as conversations]
   [com.blockether.vis.loop.conversations.shared :as shared]
   [com.blockether.vis.core :as core]
   [com.blockether.vis.config :as config]))

;;; ── Agent Definition ─────────────────────────────────────────────────────

(defn tool
  "Create a tool definition for agent registration.

   Params:
   - sym  — Symbol name for the tool in the RLM SCI sandbox
   - f    — Implementation function
   - opts — Map with :doc, :arglists, :validate-input, :validate-output, :examples

   Example:
      (tool 'read-file slurp
         {:doc \"Read a file from disk\"})"
  [sym f opts]
  (assoc opts :sym sym :fn f))

(def base-tools
  "Alias for `com.blockether.vis.loop.conversations.shared/base-tools`. Every env
   opened through `conversations/create!` already has these registered — agent-defs
   only need to declare *extra* tools beyond this set."
  shared/base-tools)

(defn default-system-prompt
  "Tiny vis-side persona block. svar's own RLM prompt already covers ARCH,
   GROUNDING, iteration spec, execution receipts, def discipline, final
   mechanics. Here we only add what svar cannot know: the vis persona, the
   specific tools vis registers, cross-query var persistence. Keep short."
  []
  "You are a vis agent — senior engineer pair, Clojure SCI sandbox, file tools only.

Cross-query memory: only `(def name \"doc\" val)` persists in this conversation; next
query sees it in <var_index>. Plain final answers do not persist unless you also
def them. If a symbol should exist but isn't there, call (restore-var 'name) or
(restore-vars ['a 'b]). Use `:forget` for scratch vars once they stop being useful.

File tools (positional args, never maps): read-file, write-file, edit-file, list-dir.
edit-file expects one patch string in OpenAI envelope format:
*** Begin Patch / *** Update|Add|Delete File / *** End Patch.
Update hunks must use strict headers: @@ -a[,b] +c[,d] @@.
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
            :max-iterations core/MAX_ITERATIONS
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
   - :spec           — svar output spec for structured responses
   - :model          — Override model (agent-level or per-run)
   - :max-iterations — Override max iterations
   - :on-chunk       — Streaming callback fn. Receives {:iteration :thinking :code
                       :final :done?} on each partial chunk and once with :done? true
                       when the iteration produces a final answer.
   - :debug?         — Enable svar debug logging (default false)
   - :config         — Provider config override (skips ~/.vis/config.edn)

   Each call creates a fresh conversation in the `:cli` channel and runs
   the query against it. The conversation is persisted — it never mixes
   with the web/TUI sidebar (`:vis`) or Telegram chats (`:telegram`), but
   you can list past runs with `(conversations/by-channel :cli)` and resume one via
   `conversations/send!` against its id. A short title is derived from the first
   100 characters of the prompt for browsing."
  [agent-def prompt & [{:keys [system-prompt spec model max-iterations on-chunk
                               debug? config]
                        :as _opts}]]
  (let [_cfg      (config/resolve-config config)
        prompt-s  (if (string? prompt) prompt (pr-str prompt))
        title     (let [t (str/trim prompt-s)]
                    (if (> (count t) 100) (str (subs t 0 97) "…") t))
        {conv-id :id} (conversations/create! :cli {:title title})
        env       (conversations/env-for conv-id)
        ;; Register agent-def's extra tools + constants on top of conv's base tools
        _         (doseq [{:keys [sym fn] :as tool-def} (:tools agent-def)]
                    (core/register-env-fn! env sym fn (dissoc tool-def :sym :fn)))
        _         (doseq [[sym value] (:constants agent-def)]
                    (core/register-env-def! env sym value
                      {:doc (str sym)}))
        iters     (or max-iterations (:max-iterations agent-def) core/MAX_ITERATIONS)
        mdl       (or model (:model agent-def))
        raw-sys   (or system-prompt (:system-prompt agent-def))
        sys       (str raw-sys (environment-info))
        q-opts    (cond-> {:max-iterations iters
                           :system-prompt  sys}
                    spec     (assoc :spec spec)
                    mdl      (assoc :model mdl)
                    on-chunk (assoc :hooks {:on-chunk on-chunk})
                    debug?   (assoc :debug? true))
        messages  (if (string? prompt) [(llm/user prompt)] prompt)]
    (try
      (let [result (conversations/send! conv-id messages q-opts)]
        (cond-> {:conv-id     conv-id
                 :answer      (:answer result)
                 :iterations  (:iterations result)
                 :duration-ms (:duration-ms result)
                 :tokens      (:tokens result)
                 :cost        (:cost result)
                 :trace       (:trace result)}
          (:status result)     (assoc :status (:status result))
          (:confidence result) (assoc :confidence (:confidence result))
          (:learn result)      (assoc :learn (:learn result))))
      (catch Exception e
        {:conv-id   conv-id
         :error     (shared/error->user-message e)
         :type      (str (type e))
         :exception e}))))

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
