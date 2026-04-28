(ns com.blockether.vis.channels.cli.agent
  "Agent orchestration over vis.

   Define agents as data, run queries programmatically.

   Example:
     (def reviewer (agent {:name \"reviewer\"}))
     (run! reviewer \"Review auth.clj\")
     ;; => {:answer \"...\" :iterations 5 :duration-ms 2340 :tokens {...} :cost {...}}"
  (:refer-clojure :exclude [agent run!])
  (:require
   [charred.api :as json]
   [clojure.string :as str]
   [com.blockether.svar.internal.llm :as llm]
   [com.blockether.vis.channels.core :as channels]
   [com.blockether.vis.loop.runtime.conversation.core :as conversations]
   [com.blockether.vis.persistance.core :as db]
   [com.blockether.vis.config :as config]))

;;; ── Agent Definition ─────────────────────────────────────────────────────

(defn agent
  "Create an agent definition (data map).

   Options:
   - :name        — Agent name (string, default \"default\")
   - :description — What the agent does
   - :constants   — Map of {symbol value} constants for SCI sandbox
   - :model       — Override default model selection

   The iteration loop runs until the model emits `:answer` or the
   user cancels. There is no per-agent budget and no iteration cap.

   Example:
     (agent {:name \"code-reviewer\"
             :description \"Reviews Clojure code for quality\"
             :model \"gpt-4o\"})"
  [{:keys [name] :as opts}]
  (let [agent-name (or name "default")]
    (merge {:name      agent-name
            :constants {}}
      opts)))

;;; ── Execution ────────────────────────────────────────────────────────────

(defn run!
  "Execute a one-shot agent query.

   Creates a conversation → runs the query → returns result.

   Returns map with:
   - :conv-id      — Conversation ID (UUID string)
   - :answer       — The agent's response
   - :iterations   — Number of iterations executed
   - :duration-ms  — Total wall-clock time
   - :tokens       — {:input N :output N :reasoning N :cached N :total N}
   - :cost         — {:input-cost N :output-cost N :total-cost N :model str}
   - :trace        — Full iteration trace
   - :confidence   — :high/:medium/:low (when present)
   - :status — Only on failure (`:error` or `:cancelled`).
   - :error  — Error message (only on failure).

   Options:
   - :spec     — Output spec for structured responses
   - :model    — Override model
   - :on-chunk — Streaming callback fn
   - :debug?   — Enable debug logging (default false)
   - :config   — Provider config override (skips ~/.vis/config.edn)

   Each call creates a fresh conversation in the `:cli` channel.
   Past runs are browsable via `(conversations/by-channel :cli)`."
  [agent-def prompt & [{:keys [spec model on-chunk
                               debug? config]
                        :as _opts}]]
  (let [_cfg      (config/resolve-config config)
        prompt-s  (if (string? prompt) prompt (pr-str prompt))
        title     (let [t (str/trim prompt-s)]
                    (if (> (count t) 100) (str (subs t 0 97) "…") t))
        {conv-id :id} (conversations/create! :cli {:title title})
        mdl       (or model (:model agent-def))
        tracker   (when on-chunk
                    (channels/make-progress-tracker {:on-update (fn [_timeline chunk] (on-chunk chunk))}))
        on-chunk* (when tracker (:on-chunk tracker))
        q-opts    (cond-> {}
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
         :error     (db/error->user-message e)
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
