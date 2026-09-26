(ns com.blockether.vis.internal.loop.accounting
  "Token, cost and context accounting for one turn.

   A turn keeps one accounting map. `initial-usage` seeds it, `add-usage` folds in
   each provider response's reported tokens and `add-cost` folds in each response's
   price. `turn-cost` and `turn-utilization` read the totals when the turn ends.
   Every function is pure; the caller owns the map and where it lives."
  (:require [com.blockether.vis.internal.context.engine :as ctx-engine]
            [com.blockether.vis.internal.loop.router :as loop-router]))

(defn initial-usage
  "Accounting map at turn start. Until the turn measures its first request, the
   session's latest persisted request (`:last-request-tokens` of `previous-usage`)
   stands in for context pressure. `:accrued-cost` stays nil until a response is
   priced."
  [previous-usage]
  {:input-tokens 0
   :output-tokens 0
   :reasoning-tokens 0
   :reasoning-reported? false
   :cached-tokens 0
   :cache-creation-tokens 0
   :last-iter-input 0
   :last-iter-reasoning 0
   :previous-request-input (long (or (:last-request-tokens previous-usage) 0))
   :iter-count 0
   :accrued-cost nil})

(defn add-usage
  "Fold one response's provider usage into `acc`. Token totals accumulate; the
   `:last-iter-*` fields describe only the latest response. Reasoning tokens count
   only when the provider reports them. A nil `api-usage` leaves `acc` unchanged."
  [acc api-usage]
  (if-not api-usage
    acc
    (let [iter-in
          (long (or (:input-tokens api-usage) 0))

          iter-reason
          (get-in api-usage [:output-tokens-details :reasoning])]

      (cond-> (-> acc
                  (update :input-tokens + iter-in)
                  (update :output-tokens + (or (:output-tokens api-usage) 0))
                  (update :cached-tokens
                          +
                          (or (get-in api-usage [:input-tokens-details :cache-read]) 0))
                  (update :cache-creation-tokens
                          +
                          (or (get-in api-usage [:input-tokens-details :cache-write]) 0))
                  (assoc :last-iter-input iter-in)
                  (assoc :last-iter-reasoning iter-reason)
                  (update :iter-count inc))
        (some? iter-reason)
        (-> (update :reasoning-tokens + (long iter-reason))
            (assoc :reasoning-reported? true))))))

(defn pricing
  "Pricing context of a turn. `model` and `provider` price a response that names no
   serving route; `extra-body` and `turn-features` select a provider's fast-mode
   price multiplier."
  [model provider extra-body turn-features]
  {:model model :provider provider :extra-body extra-body :turn-features turn-features})

(defn response-cost
  "Tokens and cost of one provider response, priced by the model and provider that
   served it: a fallback response must not bill at the selected model's rates. A
   missing `served-model` prices at the turn's model; a missing `served-provider`
   falls back to the usage's routing data, then to the turn's provider.

   Returns `{:tokens {\"input\" … \"output\" … \"cached\" … \"cache_created\" …} :cost-usd
   :cost-map}`, with `\"reasoning\"` only when the provider reported it, or nil when
   the response carried no usage. `:cost-map` is the priced breakdown `add-cost`
   accumulates; it is nil for an unpriced model."
  [pricing api-usage served-model served-provider]
  (when api-usage
    (let [in
          (long (or (:input-tokens api-usage) 0))

          out
          (long (or (:output-tokens api-usage) 0))

          reasoning
          (get-in api-usage [:output-tokens-details :reasoning])

          cached
          (long (or (get-in api-usage [:input-tokens-details :cache-read]) 0))

          cache-created
          (long (or (get-in api-usage [:input-tokens-details :cache-write]) 0))

          provider
          (or served-provider (:llm-provider api-usage) (:provider api-usage) (:provider pricing))

          cost-map
          (loop-router/estimate-token-cost (or (some-> served-model
                                                       str
                                                       not-empty)
                                               (:model pricing))
                                           in
                                           out
                                           {:api-usage api-usage
                                            :cost-multiplier (loop-router/fast-mode-cost-multiplier
                                                               (:extra-body pricing)
                                                               (:turn-features pricing)
                                                               provider)})

          cost-map
          (when (map? cost-map) cost-map)

          total
          (get cost-map "total_cost")]

      {:tokens (cond-> {"input" in "output" out "cached" cached "cache_created" cache-created}
                 (some? reasoning)
                 (assoc "reasoning" (long reasoning)))
       :cost-usd (when (number? total) (double total))
       :cost-map cost-map})))

(defn add-cost
  "Add one priced response to the turn's running cost. An unpriced response leaves
   `acc` unchanged, so a turn served only by unpriced models keeps a nil
   `:accrued-cost`."
  [acc response-cost]
  (if-let [cost-map (:cost-map response-cost)]
    (update acc :accrued-cost #(loop-router/merge-cost-maps (or % {}) cost-map))
    acc))

(defn cache-created-tokens
  "Prompt-cache tokens one priced response wrote, or nil when it wrote none."
  [response-cost]
  (let [created (long (or (get-in response-cost [:tokens "cache_created"]) 0))]
    (when (pos? created) created)))

(defn turn-cost
  "Final token totals and cost of a turn. The cost is the sum of the per-response
   prices in `acc`; a turn without a priced response is estimated at the rates of
   the turn's model."
  [acc pricing]
  (let [{:keys [input-tokens output-tokens reasoning-tokens cached-tokens cache-creation-tokens
                reasoning-reported? accrued-cost]}
        acc]
    {:tokens (cond-> {"input" input-tokens
                      "output" output-tokens
                      "cached" cached-tokens
                      "cache_created" cache-creation-tokens
                      "total" (+ (long input-tokens) (long output-tokens))}
               reasoning-reported?
               (assoc "reasoning" reasoning-tokens))
     :cost (or accrued-cost
               (loop-router/estimate-token-cost
                 (:model pricing)
                 input-tokens
                 output-tokens
                 {:cached-tokens cached-tokens
                  :cache-creation-tokens cache-creation-tokens
                  :cost-multiplier (loop-router/fast-mode-cost-multiplier (:extra-body pricing)
                                                                          (:turn-features pricing)
                                                                          (:provider pricing))}))}))

(defn latest-request-tokens
  "Input tokens of the latest measured request: this turn's latest response, or the
   session's previous request before the turn measured one."
  [acc]
  (if (pos? (long (:iter-count acc)))
    (long (:last-iter-input acc))
    (long (:previous-request-input acc))))

(defn pending-utilization
  "Context utilization of a request that measured `input-tokens` before its usage
   is folded into `acc`, against an input `window`."
  [acc input-tokens window]
  (ctx-engine/utilization input-tokens
                          window
                          (+ (long (:input-tokens acc)) (long input-tokens))
                          (loop-router/context-fold-budget window)))

(defn measured-utilization
  "Context utilization after the turn's latest folded response, against an input
   `window`."
  [acc window]
  (ctx-engine/utilization (:last-iter-input acc)
                          window
                          (:input-tokens acc)
                          (loop-router/context-fold-budget window)))

(defn turn-utilization
  "Context utilization at the end of a turn, with Svar's prompt-cache status
   attached when one was reported."
  [acc context-limit fold-budget prompt-cache-status]
  (ctx-engine/with-prompt-cache-status (ctx-engine/utilization (latest-request-tokens acc)
                                                               context-limit
                                                               (:input-tokens acc)
                                                               fold-budget)
                                       prompt-cache-status))
