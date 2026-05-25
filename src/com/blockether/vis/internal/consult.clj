(ns com.blockether.vis.internal.consult
  "Phase H — secondary-model consultation via async cross-iter requests.

   Model declares an INTENT inside a fence:

     (consult-request! :review :deep \"hard question…\")
     ;; → {:consult-id :review :preference :deep :status :pending}

   Iter completes WITHOUT blocking. Engine processes pending consults
   between iters (in parallel where multiple). Result materialises as
   a fact in the NEXT iter under the `:consult/<id>` namespaced key:

     ;; iter N+1, model sees in ctx:
     :session/facts {:consult/:review
                     {:content \"…consultant's answer…\"
                      :status :active
                      :consult-meta {:preference :deep
                                     :call-no 3
                                     :duration-ms 18432}
                      :born \"tN/iM/fK\"}}

   On failure the fact lands with :status :failed and :consult-meta
   carrying :error + :reason.

   Preferences (semantic, never provider/model names):
     :fast      haiku-class — quick critique, sanity, format check
     :balanced  sonnet-class — mid-range cost/quality
     :deep      opus-class — hard reasoning, novel decomposition

   Engine maps preference → provider+model via `~/.vis/config.edn`
   `:consult` block. Model never sees the mapping; it just picks
   the semantic preference.

   The consultant call embeds INVISIBLY:
     - primary system prompt (CORE + addendum)
     - primary user request (turn message)
     - current ctx snapshot (specs/tasks/facts/trailer)

   So the secondary brain has enough context to answer without the
   primary re-passing it. Embedded context never lands in the trailer
   because it's engine state, not SCI bindings.

   Budget: per-session cap (default 20). Recursion depth = 2 (a consult
   spawned BY another consult counts; a third level fails fast)."
  (:require
   [clojure.string :as str]
   [com.blockether.svar.core :as svar]
   [taoensso.telemere :as tel]))

(def ^:private DEFAULT_CONSULT_BUDGET 20)
(def ^:private MAX_RECURSION_DEPTH 2)
(def ^:private CONSULT_TIMEOUT_MS 60000)
(def ^:private CONSULT_TTFT_MS    30000)
(def ^:private CONSULT_IDLE_MS    20000)

(def ^:private DEFAULT_PREFERENCE_MAP
  {:fast     {:provider :anthropic-coding-plan :model "claude-haiku-4-5"}
   :balanced {:provider :anthropic-coding-plan :model "claude-sonnet-4-6"}
   :deep     {:provider :anthropic-coding-plan :model "claude-opus-4-6"}})

(def ^:dynamic *recursion-depth* 0)

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
  [env]
  (try
    (when-let [ctx-atom (:ctx-atom env)]
      (let [c @ctx-atom]
        (pr-str (select-keys c [:session/id :session/turn :session/scope
                                :session/specs :session/tasks :session/facts
                                :session/trailer]))))
    (catch Throwable _ "")))

(defn- build-consultant-messages
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

;; =============================================================================
;; ASYNC API
;; =============================================================================
;;
;; consult-request! writes an INTENT into :engine/pending-consults.
;; The loop's end-of-iter pass calls execute-pending! which drains the
;; vec, fires each consult (potentially in parallel via future), and
;; materialises results as `:consult/<id>` facts on the live ctx-atom.

(defn request-consult!
  "Declare an async consult intent. Pure-data wrt return value —
   pushes an entry onto `:engine/pending-consults` on the ctx-atom
   and returns a tiny ack map. Engine fulfils between iters.

   Validates preference + question shape SYNCHRONOUSLY so the model
   knows immediately on bad input. Returns:

     {:ok? true  :consult-id :K :preference :deep :status :pending}
     {:ok? false :consult-id :K? :error <kw> :reason <string>}

   On :ok? false the intent is NOT enqueued; nothing happens between
   iters. The model can read the map and recover."
  [env consult-id preference question]
  (cond
    (not (keyword? consult-id))
    {:ok?    false
     :error  :invalid-consult-id
     :reason (str "consult-id must be a keyword; got " (type consult-id))}

    (not (#{:fast :balanced :deep} preference))
    {:ok?        false
     :consult-id consult-id
     :error      :unknown-preference
     :reason     (str "preference " preference
                   " not in #{:fast :balanced :deep}")}

    (or (not (string? question)) (str/blank? question))
    {:ok?        false
     :consult-id consult-id
     :error      :empty-question
     :reason     "consult question must be a non-blank string"}

    (budget-exhausted? env)
    {:ok?        false
     :consult-id consult-id
     :error      :budget-exhausted
     :reason     (str "session consult budget exhausted at "
                   (some-> (:consult-budget-atom env) deref :used)
                   "/" (or (some-> (:consult-budget-atom env) deref :cap)
                         DEFAULT_CONSULT_BUDGET))}

    :else
    (let [intent {:consult-id consult-id
                  :preference preference
                  :question   question
                  :born       (or (:current-form-scope env) "tN/iM/f?")}]
      (when-let [ctx-atom (:ctx-atom env)]
        (swap! ctx-atom
          (fn [c] (update c :engine/pending-consults (fnil conj []) intent))))
      {:ok?        true
       :consult-id consult-id
       :preference preference
       :status     :pending})))

(defn- execute-one!
  "Run ONE pending intent. Returns a fact-shaped map ready to be
   `assoc`'d under `:consult/<id>` on `:session/facts`. Internal."
  [env {:keys [consult-id preference question born]}]
  (binding [*recursion-depth* (inc *recursion-depth*)]
    (let [started (System/currentTimeMillis)
          call-no (bump-budget! env)]
      (if (>= *recursion-depth* (inc MAX_RECURSION_DEPTH))
        {:status  :failed
         :born    born
         :consult-meta {:preference preference
                        :call-no call-no
                        :error :recursion-cap
                        :reason (str "recursion depth >= cap " MAX_RECURSION_DEPTH)}}
        (try
          (let [messages (build-consultant-messages env question)
                response (call-consultant! env preference messages)
                duration (- (System/currentTimeMillis) started)]
            (tel/log! {:level :info :id ::consult-ok
                       :data {:consult-id consult-id
                              :preference preference
                              :call-no call-no
                              :duration-ms duration
                              :response-len (count (str response))}}
              "consult executed")
            {:content (str response)
             :status  :active
             :born    born
             :consult-meta {:preference preference
                            :call-no call-no
                            :duration-ms duration}})
          (catch Throwable t
            (let [duration (- (System/currentTimeMillis) started)]
              (tel/log! {:level :warn :id ::consult-failed
                         :data {:consult-id consult-id
                                :preference preference
                                :call-no call-no
                                :duration-ms duration
                                :ex-class (.getName (class t))
                                :ex-msg (ex-message t)}}
                "consult failed")
              {:status  :failed
               :born    born
               :consult-meta {:preference preference
                              :call-no call-no
                              :duration-ms duration
                              :error :consult-error
                              :reason (ex-message t)}})))))))

(defn execute-pending!
  "Drain `:engine/pending-consults` and run every intent in PARALLEL
   (one future per intent). Materialise each result as a fact under
   `:session/facts :consult/<id>` on the live ctx-atom. Called from
   the iteration loop's end-of-iter pass.

   Pure-ish: side effects are the ctx-atom swap + the LLM calls.
   Returns the vec of consult-ids that were processed."
  [env]
  (when-let [ctx-atom (:ctx-atom env)]
    (let [intents (or (:engine/pending-consults @ctx-atom) [])]
      (when (seq intents)
        ;; spawn futures in parallel
        (let [futures (mapv (fn [i] [i (future (execute-one! env i))])
                        intents)
              ;; await all (these are SHORT — bounded by CONSULT_TIMEOUT_MS)
              results (mapv (fn [[intent f]] [intent @f]) futures)]
          (swap! ctx-atom
            (fn [c]
              (-> c
                (assoc :engine/pending-consults [])
                (update :session/facts
                  (fn [facts]
                    (reduce (fn [acc [{:keys [consult-id]} result]]
                              (assoc acc
                                (keyword "consult" (name consult-id))
                                result))
                      (or facts {}) results))))))
          (mapv (comp :consult-id first) results))))))

(defn fresh-budget-atom
  ([] (fresh-budget-atom DEFAULT_CONSULT_BUDGET))
  ([cap] (atom {:used 0 :cap cap})))
