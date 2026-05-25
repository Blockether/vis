(ns com.blockether.vis.internal.consult
  "Phase H — secondary-model consultation via SCI bindings.

   Three semantically-named SCI fns the model calls from inside a fence:

     (consult-fast     \"question\")    ; haiku-class — quick critique, sanity
     (consult-balanced \"question\")    ; sonnet-class — mid-range
     (consult-deep     \"question\")    ; opus-class — hard reasoning

   The engine RESOLVES preference → concrete provider+model via the
   user-editable `:consult` map in `~/.vis/config.edn`. The model never
   sees provider/model names — just the 3 fns. Provider availability
   changes do not break agent code.

   Consult calls RUN IN PARALLEL when wrapped in SCI `future`s. Engine
   embeds the current system prompt + user request + ctx snapshot
   into the consultant call invisibly so the secondary brain has
   enough context to answer; primary model never has to re-pass them
   and never sees them as raw values that could leak into the trailer.

   Budget: per-session cap (default 20). Hard fail above cap with
   `{:error :consult-budget-exhausted}`. Recursion depth = 2 (consult
   can spawn consult once; that second-level consult cannot recurse)."
  (:require
   [clojure.string :as str]
   [com.blockether.svar.core :as svar]
   [taoensso.telemere :as tel]))

(def ^:private DEFAULT_CONSULT_BUDGET 20)
(def ^:private MAX_RECURSION_DEPTH 2)
(def ^:private CONSULT_TIMEOUT_MS 30000)
(def ^:private CONSULT_TTFT_MS    30000)
(def ^:private CONSULT_IDLE_MS    20000)

(def ^:private DEFAULT_PREFERENCE_MAP
  "Fallback mapping when `~/.vis/config.edn` has no `:consult` block.
   Picks the most universally-available model class per preference."
  {:fast     {:provider :anthropic-coding-plan :model "claude-haiku-4-5"}
   :balanced {:provider :anthropic-coding-plan :model "claude-sonnet-4-6"}
   :deep     {:provider :anthropic-coding-plan :model "claude-opus-4-6"}})

(def ^:dynamic *recursion-depth*
  "Per-thread depth counter so a consult triggered FROM inside another
   consult does not blow stack. Hard cap = `MAX_RECURSION_DEPTH`."
  0)

(defn- resolve-preference
  [env preference]
  (let [config (or (:consult-config env) DEFAULT_PREFERENCE_MAP)]
    (get config preference)))

(defn- bump-budget!
  [env]
  (when-let [atm (:consult-budget-atom env)]
    (swap! atm update :used (fnil inc 0))
    (:used @atm)))

(defn- budget-exhausted? [env]
  (when-let [atm (:consult-budget-atom env)]
    (let [{:keys [used cap]} @atm
          cap (or cap DEFAULT_CONSULT_BUDGET)]
      (>= (or used 0) cap))))

(defn- ctx-snapshot-text
  "Best-effort textual snapshot of the live ctx for embedding in the
   consultant's context block. pr-str's the ctx-atom value without
   engine-ephemeral keys. Returns empty string when no ctx-atom is
   present (e.g. in tests)."
  [env]
  (try
    (when-let [ctx-atom (:ctx-atom env)]
      (let [c @ctx-atom]
        (pr-str (select-keys c [:session/id :session/turn :session/scope
                                :session/specs :session/tasks :session/facts
                                :session/trailer]))))
    (catch Throwable _ "")))

(defn- build-consultant-messages
  "Build the consultant's message vec. Primary system prompt + user
   request + a ctx snapshot are embedded; the model's question is the
   tail. Model NEVER sees these messages."
  [env question]
  (let [primary-system (or (:turn/system-prompt env) "")
        primary-user   (or (:turn/user-request env) "")
        ctx-text       (or (ctx-snapshot-text env) "")
        consultant-system
        (str "You are a SECONDARY CONSULTANT being queried by a primary\n"
          "agent operating the Vis neurosymbolic engine. Answer the\n"
          "primary agent's question concisely and concretely. Do NOT\n"
          "try to satisfy the user yourself — the primary owns the\n"
          "final answer; you provide one focused input.\n\n"
          "PRIMARY AGENT'S SYSTEM RULES (read for context):\n"
          primary-system)]
    [{:role "system"    :content consultant-system}
     {:role "user"      :content (str "PRIMARY USER MESSAGE:\n" primary-user)}
     {:role "assistant" :content (str "Current ctx snapshot:\n" ctx-text)}
     {:role "user"      :content question}]))

(defn- call-consultant!
  "Synchronous router call; returns string content or throws."
  [env preference messages]
  (let [{:keys [provider model]} (resolve-preference env preference)
        router  (:router env)]
    (when-not (and router provider model)
      (throw (ex-info "consult-router-or-model-missing"
               {:type :consult/router-missing
                :preference preference})))
    (let [opts {:lang     "clojure"
                :messages messages
                :routing  {:provider provider :model model}
                :timeout-ms CONSULT_TIMEOUT_MS
                :ttft-timeout-ms CONSULT_TTFT_MS
                :idle-timeout-ms CONSULT_IDLE_MS
                :reasoning :minimal
                :preserved-thinking? false}
          result (svar/ask-code! router opts)]
      (or (some-> result :content) (some-> result :raw) ""))))

(defn consult!
  "Public entry. Engine-owned. Returns the consultant's response string
   on success; `{:error <kw> :reason <string>}` map on bounded failure
   (budget exhausted, unknown preference, provider error). Never throws
   for normal failure paths — the model can read the map and recover."
  [env preference question]
  (cond
    (>= *recursion-depth* MAX_RECURSION_DEPTH)
    {:error :consult-recursion-cap
     :reason (str "consult recursion depth " *recursion-depth*
               " ≥ cap " MAX_RECURSION_DEPTH)}

    (budget-exhausted? env)
    {:error :consult-budget-exhausted
     :reason (str "session consult budget exhausted at "
               (some-> (:consult-budget-atom env) deref :used)
               "/" (or (some-> (:consult-budget-atom env) deref :cap)
                     DEFAULT_CONSULT_BUDGET))}

    (not (#{:fast :balanced :deep} preference))
    {:error :consult-unknown-preference
     :reason (str "preference " preference
               " not in #{:fast :balanced :deep}")}

    (or (not (string? question)) (str/blank? question))
    {:error :consult-empty-question
     :reason "consult question must be a non-blank string"}

    :else
    (binding [*recursion-depth* (inc *recursion-depth*)]
      (let [call-no (bump-budget! env)
            started (System/currentTimeMillis)]
        (try
          (let [messages (build-consultant-messages env question)
                response (call-consultant! env preference messages)
                duration (- (System/currentTimeMillis) started)]
            (tel/log! {:level :info :id ::consult-ok
                       :data {:preference preference
                              :call-no call-no
                              :duration-ms duration
                              :response-len (count (str response))}}
              "consult succeeded")
            response)
          (catch Throwable t
            (tel/log! {:level :warn :id ::consult-failed
                       :data {:preference preference
                              :call-no call-no
                              :duration-ms (- (System/currentTimeMillis) started)
                              :ex-class (.getName (class t))
                              :ex-msg (ex-message t)}}
              "consult failed")
            {:error  :consult-error
             :reason (ex-message t)}))))))

(defn fresh-budget-atom
  ([] (fresh-budget-atom DEFAULT_CONSULT_BUDGET))
  ([cap] (atom {:used 0 :cap cap})))
