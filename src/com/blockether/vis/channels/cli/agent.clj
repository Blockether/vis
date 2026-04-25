(ns com.blockether.vis.channels.cli.agent
  "Agent orchestration over vis.

   Define agents as data, run queries programmatically.

   Example:
     (def reviewer (agent {:name \"reviewer\"
                           :system-prompt \"You are a senior Clojure engineer.\"}))
     (run! reviewer \"Review auth.clj\")
     ;; => {:answer \"...\" :iterations 5 :duration-ms 2340 :tokens {...} :cost {...}}"
  (:refer-clojure :exclude [agent run!])
  (:require
   [charred.api :as json]
   [clojure.string :as str]
   [com.blockether.svar.internal.llm :as llm]
   [com.blockether.vis.loop.runtime.conversation.core :as conversations]
   [com.blockether.vis.persistance.spec :as spec]
   [com.blockether.vis.config :as config]))

;;; ── Agent Definition ─────────────────────────────────────────────────────

(defn agent
  "Create an agent definition (data map).

   Options:
   - :name           — Agent name (string, default \"default\")
   - :description    — What the agent does
   - :system-prompt  — System instructions for the agent
   - :constants      — Map of {symbol value} constants for SCI sandbox
   - :model          — Override default model selection
   - :max-iterations — Max iterations (default 50)

   Example:
     (agent {:name \"code-reviewer\"
             :description \"Reviews Clojure code for quality\"
             :system-prompt \"You are a senior Clojure engineer.\"
             :model \"claude-sonnet-4-6\"
             :max-iterations 30})"
  [{:keys [name] :as opts}]
  (let [agent-name (or name "default")]
    (merge {:name           agent-name
            :constants      {}
            :max-iterations spec/MAX_ITERATIONS
            :system-prompt  (:system-prompt opts)}
      opts)))

;;; ── Execution ────────────────────────────────────────────────────────────

(defn run!
  "Execute a one-shot agent query.

   Creates a conversation → runs the query → returns result.

   Returns map with:
   - :conv-id      — Conversation ID (UUID string)
   - :answer       — The agent's response (string or structured data if :spec)
   - :iterations   — Number of iterations executed
   - :duration-ms  — Total wall-clock time
   - :tokens       — {:input N :output N :reasoning N :cached N :total N}
   - :cost         — {:input-cost N :output-cost N :total-cost N :model str}
   - :trace        — Full iteration trace
   - :confidence   — :high/:medium/:low (when present)
   - :status       — Only on failure: :max-iterations, :error, etc.
   - :error        — Error message (only on failure)

   Options:
   - :system-prompt  — Override agent's system prompt
   - :spec           — Output spec for structured responses
   - :model          — Override model (agent-level or per-run)
   - :max-iterations — Override max iterations
   - :on-chunk       — Streaming callback fn. Receives:
                        {:iteration N :thinking str
                         :code [expr-strings] :final {:answer :confidence}
                         :done? bool :timeline [events]}
   - :debug?         — Enable debug logging (default false)
   - :config         — Provider config override (skips ~/.vis/config.edn)

   Each call creates a fresh conversation in the `:cli` channel.
   Past runs are browsable via `(conversations/by-channel :cli)`."
  [agent-def prompt & [{:keys [system-prompt spec model max-iterations on-chunk
                               debug? config]
                        :as _opts}]]
  (let [_cfg      (config/resolve-config config)
        prompt-s  (if (string? prompt) prompt (pr-str prompt))
        title     (let [t (str/trim prompt-s)]
                    (if (> (count t) 100) (str (subs t 0 97) "…") t))
        {conv-id :id} (conversations/create! :cli {:title title})
        iters     (or max-iterations (:max-iterations agent-def) spec/MAX_ITERATIONS)
        mdl       (or model (:model agent-def))
        sys       (or system-prompt (:system-prompt agent-def))
        projector (when on-chunk
                    (conversations/make-on-chunk-projector))
        on-chunk* (when on-chunk
                    (fn [chunk]
                      (on-chunk (assoc chunk :timeline (projector chunk)))))
        q-opts    (cond-> {:max-iterations iters
                           :system-prompt  sys}
                    spec      (assoc :spec spec)
                    mdl       (assoc :model mdl)
                    on-chunk* (assoc :hooks {:on-chunk on-chunk*})
                    debug?    (assoc :debug? true))
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
          (:confidence result) (assoc :confidence (:confidence result))))
      (catch Exception e
        {:conv-id   conv-id
         :error     (conversations/error->user-message e)
         :type      (str (type e))
         :exception e}))))

;;; ── Output Formatting ───────────────────────────────────────────────────

(defn result->json
  "Convert agent result to a JSON string."
  [result]
  (json/write-json-str result))

(defn result->edn
  "Convert agent result to an EDN string."
  [result]
  (pr-str result))
