(ns com.blockether.vis.internal.loop.accounting-test
  (:require [com.blockether.vis.internal.loop.accounting :as accounting]
            [com.blockether.vis.test-provider-policies :as policies]
            [lazytest.core :refer [around-each defdescribe expect it set-ns-context!]]))

;; Fast-mode pricing comes from provider policy; route through the first-party
;; declarations, as a booted engine does.
(set-ns-context! [(around-each [f] (policies/with-policies f))])

(def ^:private priced-model "gpt-5.6-sol")

(defn- close? [a b] (< (Math/abs (- (double a) (double b))) 1.0E-12))

(defdescribe initial-usage-test
             (it "starts every total at zero and seeds context pressure from the previous request"
                 (let [acc (accounting/initial-usage {:last-request-tokens 1234})]
                   (expect (= 0 (:input-tokens acc) (:output-tokens acc) (:iter-count acc)))
                   (expect (= 1234 (:previous-request-input acc)))
                   (expect (false? (:reasoning-reported? acc)))
                   (expect (nil? (:accrued-cost acc)))))
             (it "treats a session without a previous request as unmeasured"
                 (expect (= 0 (:previous-request-input (accounting/initial-usage nil))))))

(defdescribe
  add-usage-test
  (it "sums totals and keeps only the latest response in the per-iteration fields"
      (let [acc (-> (accounting/initial-usage nil)
                    (accounting/add-usage {:input-tokens 100
                                           :output-tokens 10
                                           :input-tokens-details {:cache-read 40 :cache-write 5}})
                    (accounting/add-usage {:input-tokens 150
                                           :output-tokens 20
                                           :input-tokens-details {:cache-read 90}}))]
        (expect (= 250 (:input-tokens acc)))
        (expect (= 30 (:output-tokens acc)))
        (expect (= 130 (:cached-tokens acc)))
        (expect (= 5 (:cache-creation-tokens acc)))
        (expect (= 150 (:last-iter-input acc)))
        (expect (= 2 (:iter-count acc)))
        (expect (false? (:reasoning-reported? acc)))))
  (it "counts reasoning only when the provider reports it"
      (let [acc (-> (accounting/initial-usage nil)
                    (accounting/add-usage
                      {:input-tokens 1 :output-tokens 1 :output-tokens-details {:reasoning 7}})
                    (accounting/add-usage {:input-tokens 1 :output-tokens 1}))]
        (expect (= 7 (:reasoning-tokens acc)))
        (expect (true? (:reasoning-reported? acc)))
        (expect (nil? (:last-iter-reasoning acc)))))
  (it "leaves the map unchanged when a response carried no usage"
      (let [acc (accounting/initial-usage nil)]
        (expect (= acc (accounting/add-usage acc nil))))))

(defdescribe
  response-cost-test
  (it "prices the model that served the response and reports its token classes"
      (let [pricing
            (accounting/pricing "unpriced-root-model" :openai nil nil)

            usage
            {:input-tokens 8298
             :output-tokens 6
             :input-tokens-details {:cache-read 8000 :cache-write 12}
             :output-tokens-details {:reasoning 3}}

            cost
            (accounting/response-cost pricing usage priced-model :openai)]

        (expect (= {"input" 8298 "output" 6 "cached" 8000 "cache_created" 12 "reasoning" 3}
                   (:tokens cost)))
        (expect (pos? (double (:cost-usd cost))))
        (expect (close? (:cost-usd cost) (get (:cost-map cost) "total_cost")))))
  (it "falls back to the turn's model when the response names none"
      (let [pricing
            (accounting/pricing priced-model :openai nil nil)

            usage
            {:input-tokens 100 :output-tokens 10}]

        (expect (= (:cost-usd (accounting/response-cost pricing usage priced-model :openai))
                   (:cost-usd (accounting/response-cost pricing usage nil nil))))))
  (it "applies fast-mode pricing only for the provider that declares it"
      (let [pricing
            (accounting/pricing priced-model nil {} {"codex_fast_mode" true})

            usage
            {:input-tokens 100 :output-tokens 10}

            fast
            (accounting/response-cost pricing usage priced-model :openai-codex)

            standard
            (accounting/response-cost pricing usage priced-model :openai)]

        (expect (close? (* 2.0 (double (:cost-usd standard))) (:cost-usd fast)))))
  (it "returns nil when the response carried no usage"
      (expect (nil? (accounting/response-cost (accounting/pricing priced-model :openai nil nil)
                                              nil
                                              nil
                                              nil)))))

(defdescribe
  add-cost-test
  (it "keeps the running cost unchanged for a response without a price"
      (let [acc (accounting/initial-usage nil)]
        (expect (= acc
                   (accounting/add-cost acc {:tokens {"input" 10} :cost-usd nil :cost-map nil})))
        (expect (= acc (accounting/add-cost acc nil))))))

(defdescribe
  turn-cost-test
  (it "sums the prices of the served responses instead of re-pricing the turn"
      (let [pricing
            (accounting/pricing "unpriced-root-model" :local nil nil)

            usage
            {:input-tokens 100 :output-tokens 10}

            cost
            (accounting/response-cost pricing usage priced-model :openai)

            acc
            (-> (accounting/initial-usage nil)
                (accounting/add-usage usage)
                (accounting/add-cost cost)
                (accounting/add-usage usage)
                (accounting/add-cost cost))

            result
            (accounting/turn-cost acc pricing)]

        (expect (= {"input" 200 "output" 20 "cached" 0 "cache_created" 0 "total" 220}
                   (:tokens result)))
        (expect (close? (* 2.0 (double (:cost-usd cost))) (get (:cost result) "total_cost")))))
  (it "estimates the turn at its model's rates when no response was priced"
      (let [pricing
            (accounting/pricing priced-model :openai nil nil)

            usage
            {:input-tokens 100 :output-tokens 10}

            acc
            (-> (accounting/initial-usage nil)
                (accounting/add-usage usage)
                (accounting/add-cost nil))

            result
            (accounting/turn-cost acc pricing)]

        (expect (nil? (:accrued-cost acc)))
        (expect (close? (:cost-usd (accounting/response-cost pricing usage nil nil))
                        (get (:cost result) "total_cost")))))
  (it "reports reasoning tokens only when a provider reported them"
      (let [pricing
            (accounting/pricing priced-model :openai nil nil)

            plain
            (-> (accounting/initial-usage nil)
                (accounting/add-usage {:input-tokens 1 :output-tokens 1}))

            reasoning
            (accounting/add-usage
              plain
              {:input-tokens 1 :output-tokens 1 :output-tokens-details {:reasoning 0}})]

        (expect (not (contains? (:tokens (accounting/turn-cost plain pricing)) "reasoning")))
        (expect (= 0 (get-in (accounting/turn-cost reasoning pricing) [:tokens "reasoning"]))))))

(defdescribe cache-created-tokens-test
             (it "reports only a positive cache write"
                 (expect (= 12 (accounting/cache-created-tokens {:tokens {"cache_created" 12}})))
                 (expect (nil? (accounting/cache-created-tokens {:tokens {"cache_created" 0}})))
                 (expect (nil? (accounting/cache-created-tokens nil)))))

(defdescribe
  utilization-test
  (it "measures the previous session request until the turn measures its own"
      (let [acc (accounting/initial-usage {:last-request-tokens 5000})]
        (expect (= 5000 (accounting/latest-request-tokens acc)))
        (expect (= 700
                   (accounting/latest-request-tokens
                     (accounting/add-usage acc {:input-tokens 700 :output-tokens 1}))))))
  (it "reports a streaming request on top of the folded turn input"
      (let [acc
            (accounting/add-usage (accounting/initial-usage nil)
                                  {:input-tokens 300 :output-tokens 1})

            util
            (accounting/pending-utilization acc 400 1000)]

        (expect (= 400 (get util "last_request_tokens")))
        (expect (= 700 (get util "turn_total_tokens")))
        (expect (= 1000 (get util "model_input_limit")))))
  (it "reports the latest folded response"
      (let [acc
            (-> (accounting/initial-usage nil)
                (accounting/add-usage {:input-tokens 300 :output-tokens 1})
                (accounting/add-usage {:input-tokens 400 :output-tokens 1}))

            util
            (accounting/measured-utilization acc 1000)]

        (expect (= 400 (get util "last_request_tokens")))
        (expect (= 700 (get util "turn_total_tokens")))))
  (it "attaches the prompt-cache status to the turn's final utilization"
      (let [acc
            (accounting/add-usage (accounting/initial-usage nil)
                                  {:input-tokens 300 :output-tokens 1})

            status
            {"state" "warm"}

            util
            (accounting/turn-utilization acc 1000 900 status)]

        (expect (= 300 (get util "last_request_tokens")))
        (expect (= 900 (get util "auto_compress_above")))
        (expect (= status (get util "prompt_cache")))
        (expect (not (contains? (accounting/turn-utilization acc 1000 900 nil) "prompt_cache")))))
  (it "reports nothing before any request was measured"
      (expect (nil? (accounting/turn-utilization (accounting/initial-usage nil) 1000 900 nil)))))
