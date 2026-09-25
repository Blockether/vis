(ns com.blockether.vis.internal.loop.errors
  "Exception summaries and iteration error normalization.

   Formats exception chains for logs and model feedback, classifies
   infrastructure, provider, user-configuration and Python failures, and turns
   an exception inside an iteration into the `::iteration-error` result that the
   iteration loop either feeds back to the model or stops the turn on."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.attachment.vision-describe :as vision-describe]
            [com.blockether.vis.internal.content :as content]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.provider.error :as perr]
            [com.blockether.vis.internal.python.env :as env]
            [com.blockether.vis.internal.util :as util]
            [taoensso.telemere :as tel]))

(def ^:private MINI_STACK_DEPTH 12)

(defn- throwable-chain
  [^Throwable t]
  (vec (take-while some?
                   (iterate (fn [^Throwable x]
                              (.getCause x))
                            t))))

(def ^:private CAUSE_CHAIN_LIMIT
  "How many links of a cause chain the error path inspects. Wrapped provider
   failures sit one or two causes deep; a bound keeps a pathological chain from
   turning error formatting into work."
  8)

(defn bounded-cause-chain [^Throwable e] (take CAUSE_CHAIN_LIMIT (throwable-chain e)))

(defn- throwable-cause-summary
  [^Throwable t]
  (mapv (fn [^Throwable x]
          (cond-> {:class (.getName (class x)) :message (or (ex-message x) (str x))}
            (:type (ex-data x))
            (assoc :type (:type (ex-data x)))))
        (throwable-chain t)))

(defn- mini-stack-trace
  [^Throwable t]
  (when t
    (let [frames (take MINI_STACK_DEPTH (.getStackTrace t))]
      (str/join "\n"
                (map (fn [^StackTraceElement frame]
                       (str "  at " frame))
                     frames)))))

(def ^:private STREAM_FINALIZATION_LOG_KEYS
  "The bounded part of svar's `:stream-finalization` summary: every value is a
   scalar, so the whole selection is safe to log verbatim. It answers what a
   truncated or stalled stream always raises — which SSE event arrived last,
   whether a finish reason was seen, how much had already accumulated — and none
   of it used to survive into the log, so `:svar.core/stream-truncated` read as a
   bare message with no evidence at all.

   The sibling `:partial-content` / `:reasoning` keys of the same ex-data are the
   whole assistant turn and are deliberately never copied here."
  [:terminal? :terminal-kind :terminal-event-type :last-event-type :finish-reason :incomplete?
   :incomplete-reason :content-acc-len :reasoning-acc-len :http-status])

(defn format-exception-short
  [^Throwable t]
  (let [ed
        (ex-data t)

        finalization
        (let [sf (:stream-finalization ed)]
          (when (map? sf)
            (not-empty
              (into {} (filter (comp some? val)) (select-keys sf STREAM_FINALIZATION_LOG_KEYS)))))]

    (cond-> {:class (.getName (class t))
             :message (or (ex-message t) (str t))
             :causes (throwable-cause-summary t)
             :mini-trace (mini-stack-trace t)}
      (:type ed)
      (assoc :type (:type ed))

      (:status ed)
      (assoc :status (:status ed))

      (:cause-class ed)
      (assoc :cause-class (:cause-class ed))

      finalization
      (assoc :stream-finalization finalization)

      (some? (:content-acc-len ed))
      (assoc :content-acc-len (:content-acc-len ed))

      (some? (:reasoning-acc-len ed))
      (assoc :reasoning-acc-len (:reasoning-acc-len ed)))))

(defn- format-exception
  [^Throwable t & [{:keys [context]}]]
  (merge (format-exception-short t) {:data (ex-data t) :context context}))

;; Error normalization

(defn op-error
  "Coerce engine/model error values into the canonical structured :error map.

   Iteration blocks require `:error` to be nil or a map. Preflight gates and
   answer validators naturally produce strings; wrap them before persistence so
   a useful model-facing error does not become `:vis/invalid-iteration-block`."
  ([err] (op-error err nil))
  ([err {:keys [code phase]}]
   (cond (nil? err) nil
         (map? err) err
         (instance? Throwable err) (try (extension/ex->op-error err
                                                                (cond-> {}
                                                                  code
                                                                  (assoc :form-source code)))
                                        (catch Throwable _
                                          {:message (or (ex-message err) (.getName (class err)))}))
         :else (cond-> {:message (str err)}
                 code
                 (assoc :block {:source code :phase (or phase :preflight)})))))

(def ^:private INFRASTRUCTURE_ERROR_TYPES
  ;; Provider/runtime failures cannot be repaired by feeding them to the model.
  (into #{:svar.core/http-error :svar.core/stream-cancelled :svar.core/stream-idle-timeout
          :svar.core/stream-semantic-timeout :svar.llm/all-providers-exhausted
          :svar.llm/circuit-open :svar.llm/provider-exhausted :svar.llm/provider-unavailable}
        perr/CONTEXT_OVERFLOW_TYPES))

(defn- infrastructure-error?
  [ex-data-map]
  (contains? INFRASTRUCTURE_ERROR_TYPES (:type ex-data-map)))

(defn output-budget-exhausted-data?
  "The exact provider signal that an unchanged request cannot recover from, but a
   smaller NEXT Vis iteration can. Svar owns same-request retry safety; Vis owns
   changing strategy after the signal crosses that boundary."
  [data]
  (and (= :svar.core/stream-incomplete (:type data)) (= "max_output_tokens" (str (:reason data)))))

(defn- output-budget-exhausted-cause
  [^Throwable e]
  (some (fn [^Throwable t]
          (when (output-budget-exhausted-data? (ex-data t)) t))
        (bounded-cause-chain e)))

(defn- provider-failure-cause
  "The throwable carrying a provider failure that escaped svar, or nil. Svar has
   already classified it and exhausted every retry/fallback policy it owns, so
   feeding it back to the model would issue a second provider request from Vis.
   HTTP clients wrap the typed exception, so inspect bounded causes without
   reclassifying the error here; the throwable returned is the one whose
   `perr/provider-error-kind` names the failure on the card AND in the log."
  [^Throwable e]
  (some (fn [^Throwable t]
          (when (perr/provider-failure? t) t))
        (bounded-cause-chain e)))

(defn- non-correctable-log-message
  "The fatal log line for a provider failure svar already gave up on. It names
   the CLASSIFIED kind — the same one the card shows — because the previous fixed
   text called every one of them a rate limit / auth / spend cap failure, which
   sent readers of a truncated stream hunting for a billing problem."
  [^Throwable provider-failure]
  (str "Non-correctable provider error ("
       (name (perr/provider-error-kind provider-failure))
       ") - failing turn instead of re-asking the same provider"))

(defn- user-error-data?
  "True when an ex-data / iteration-error `:data` map marks a user-fixable failure."
  [d]
  (or (true? (:vis/user-error d)) (= :svar/no-providers (:type d))))

(defn- user-configuration-error?
  "True for a failure the USER must fix outside the conversation.

   `:vis/user-error` marks exactly that class: an unset `${API_KEY}` env var,
   a router with no usable provider, or a bad CLI/config value. The model cannot
   repair any of those inside the conversation, so fail once with the actionable
   message intact."
  [^Throwable e ex-data-map]
  (boolean (or (user-error-data? ex-data-map)
               (some user-error-data? (map ex-data (bounded-cause-chain e))))))

(defn user-error-content
  "Terminal content for a turn killed by a user-fixable configuration error.

   The generic provider card (`provider_unavailable`) would swallow the ONE
   thing the user needs — the name of the unset env var — so render the
   actionable message itself. Returns nil for every other failure, leaving the
   provider card path untouched."
  [iteration-error-data]
  (let [d
        (:data iteration-error-data)

        msg
        (some-> (:message iteration-error-data)
                str
                str/trim
                not-empty)]

    (when (and msg (or (user-error-data? d) (user-error-data? iteration-error-data)))
      [(content/error "config_error" msg false)])))

(defn python-error-content
  "Terminal local-runtime card, never provider retry or model-switch advice."
  [error]
  (when (= ::env/context-retired (or (:type error) (get-in error [:data :type])))
    [(content/error
       "python_environment_retired"
       (str "The Python environment was retired, so Vis ended this turn without replaying code. "
            "Check operations already started before continuing in a new turn; "
            "Python will be rebuilt and in-memory variables will be lost.")
       false)]))

(def ^:private CONTEXT_OVERFLOW_HOPELESS_FACTOR
  "A preflight `:svar.tokens/context-overflow` whose measured input exceeds
   the call's max-input budget by this factor is unrecoverable INSIDE the
   turn: the overflow fires before the provider call, so the fed-back error
   never reaches the model (the next call dies in the same preflight), and
   appending the error only GROWS the input. Feeding it anyway produces a
   runaway iteration loop — observed live as VIS-9: claude-fable-5 on a
   stale svar catalog resolved an 8192 fallback limit, an ~81k base prompt
   (10x over) re-failed every ~1s for 376+ iterations until cancelled.

   BELOW the factor a marginal overflow stays on the feed path on purpose:
   trailer folding / summarize can legitimately shrink the next iteration,
   and that recovery path must keep working."
  1.5)

(defn- hopeless-context-overflow?
  "True when ex-data is a preflight context overflow too large for any
   realistic same-model compaction pass to rescue."
  [ex-data-map]
  (let [input
        (:input-tokens ex-data-map)

        max-input
        (:max-input-tokens ex-data-map)]

    (and (number? input)
         (number? max-input)
         (pos? (long max-input))
         (>= (double input) (* (double CONTEXT_OVERFLOW_HOPELESS_FACTOR) (double max-input))))))

(def ^:private LAST_USER_PREVIEW_CHARS 500)

(defn- last-user-message-preview
  [messages]
  (when-let [c (some (fn [m]
                       (when (= (:role m) "user") (:content m)))
                     (reverse messages))]
    (let [s (if (sequential? c)
              ;; Multimodal content: preview the text blocks only — stringifying
              ;; the vector would dump base64 image payloads into error logs.
              (str/join " " (keep #(when (= "text" (:type %)) (:text %)) c))
              (str c))
          n (long (count s))]

      (if (> n (long LAST_USER_PREVIEW_CHARS))
        (str (subs s 0 LAST_USER_PREVIEW_CHARS)
             " ...<+"
             (- n (long LAST_USER_PREVIEW_CHARS))
             " chars>")
        s))))

(defn- exception->iteration-error-data
  "Normalize an exception into the iteration-error-data map stored on the turn row.
   Delegates to the unified `format-exception` and adds iteration context."
  [^Throwable e ctx]
  (cond-> (format-exception e
                            {:context {:iteration (:iteration ctx)
                                       :messages-count (count (:messages ctx))
                                       :routing (:routing ctx)
                                       :reasoning-level (:reasoning-level ctx)
                                       :last-user-preview (last-user-message-preview (:messages
                                                                                       ctx))}})
    (:stream-recovery ctx)
    (assoc-in [:data :stream-recovery] (:stream-recovery ctx))))

(defn handle-iteration-exception!
  "Error path for the main-loop try/catch around `run-iteration`.
   Infrastructure failures are terminal for the turn; model/format/code
   failures still return `{::iteration-error ...}` for RLM self-correction."
  [^Throwable e ctx]
  (let [ex-data-map
        (ex-data e)

        iteration
        (:iteration ctx)

        hopeless-overflow?
        (hopeless-context-overflow? ex-data-map)

        ;; Svar refuses an unchanged replay for this deterministic cap. Preserve
        ;; the typed inner cause across wrappers and let the OUTER Vis loop ask for
        ;; a materially smaller strategy instead of terminalizing it as a generic
        ;; provider failure.
        output-budget-exhaustion
        (output-budget-exhausted-cause e)

        provider-failure
        (when-not output-budget-exhaustion (provider-failure-cause e))

        ;; The WIRE just answered a question no capability table can: whether this
        ;; endpoint can carry an image content part at all, or whether only that one
        ;; model cannot read pixels. Remember it at the scope it proves, while the
        ;; failure is in hand, so the next request degrades to a description instead
        ;; of repeating the same 400 on every replay of that attachment.
        _
        (run! vision-describe/remember-image-refusal!
              (mapcat perr/image-rejections (bounded-cause-chain e)))

        ;; Keep the TYPED worker failure across wrappers: the card, the log and the
        ;; message fed back are all built from the throwable chosen here, and a
        ;; generic wrapper turns a dead LOCAL worker back into "Provider
        ;; unavailable" — which is what sent one session re-asking the model for
        ;; two hours over a sandbox no model could repair (vis session f2cfccd5).
        worker-failure
        (some (fn [cause]
                (when (perr/python-worker-error? cause) cause))
              (bounded-cause-chain e))

        ;; A RETIRED worker is the one LOCAL Python failure the model can still act
        ;; on: worker.clj addresses it directly — the sandbox is gone, finish this
        ;; turn with what you have. It classifies to the same named `:python-worker`
        ;; kind for the card, so keep it out of the non-correctable verdict.
        worker-retired?
        (= :vis/python-worker-retired (:type (ex-data worker-failure)))

        non-correctable?
        (and (some? provider-failure) (not worker-retired?))

        user-error?
        (user-configuration-error? e ex-data-map)

        retired-context
        (some (fn [cause]
                (when (= ::env/context-retired (:type (ex-data cause))) cause))
              (bounded-cause-chain e))

        fatal?
        (or retired-context
            (infrastructure-error? ex-data-map)
            hopeless-overflow?
            non-correctable?
            user-error?)

        iteration-error-data
        (exception->iteration-error-data
          (or retired-context output-budget-exhaustion worker-failure e)
          ctx)]

    (tel/log!
      {:level (if fatal? :error :warn)
       :data (let [base
                   (assoc (format-exception-short e) :iteration iteration)

                   ed
                   (ex-data e)

                   body
                   (some-> (:body ed)
                           str)]

               (cond-> base
                 (:status ed)
                 (assoc :status (:status ed))

                 (:request-id ed)
                 (assoc :request-id (:request-id ed))

                 (:request_id ed)
                 (assoc :request-id (:request_id ed))

                 (and body (not (str/blank? body)))
                 (assoc :body-snippet (util/truncate body 1000))))}
      (cond
        retired-context "Python environment retired - ending turn without replaying code"
        hopeless-overflow?
        "Hopeless preflight context overflow - failing turn (feeding it back can never reach the model and only grows the input; VIS-9)"
        (and worker-failure (not worker-retired?))
        "Local Python worker failed - failing turn instead of re-asking the model to rewrite code"
        non-correctable? (non-correctable-log-message provider-failure)
        user-error?
        "User configuration error (unset env var / no usable provider) - failing turn once with the actionable message"
        fatal? "Provider infrastructure error - failing turn without RLM restarts"
        :else "RLM iteration failed, feeding error to LLM"))
    (cond-> {::iteration-error iteration-error-data}
      fatal?
      (assoc ::fatal-iteration-error true))))
