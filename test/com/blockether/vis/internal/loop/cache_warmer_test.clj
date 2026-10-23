(ns com.blockether.vis.internal.loop.cache-warmer-test
  (:require [com.blockether.vis.internal.loop.accounting :as accounting]
            [com.blockether.vis.internal.loop.cache-warmer :as sut]
            [lazytest.core :refer [defdescribe describe expect it]]))

(defn- fake-cost
  "Prices tokens per million: input 3, cache read 0.3, cache write 3.75, output 15."
  [_pricing usage _model _provider]
  (let [details
        (:input-tokens-details usage)

        cache-read
        (double (or (:cache-read details) 0))

        cache-write
        (double (or (:cache-write details) 0))

        regular
        (- (double (:input-tokens usage)) cache-read cache-write)

        output
        (double (:output-tokens usage))

        usd
        (/ (+ (* 3.0 regular) (* 0.3 cache-read) (* 3.75 cache-write) (* 15.0 output)) 1e6)]

    {:cost-usd usd :cost-map {"total_cost" usd}}))

(defn- harness
  "Returns a warmer with a manual clock and a list of planned timers."
  []
  (let [now
        (atom 0)

        timers
        (atom [])]

    {:now now
     :timers timers
     :warmer (sut/create {:clock #(deref now)
                          :schedule (fn [wait-ms task]
                                      (let [cancelled (atom false)]
                                        (swap! timers conj
                                          {:wait-ms wait-ms :task task :cancelled cancelled})
                                        #(reset! cancelled true)))})}))

(defn- request
  "Returns a 5-minute TTL request whose warm records its cancel state in `warms`."
  ([mode prompt-tokens warms]
   (request mode
            prompt-tokens
            warms
            (fn [])))
  ([mode prompt-tokens warms before-reply]
   {:mode (constantly mode)
    :ttl-ms 300000
    :prompt-tokens prompt-tokens
    :pricing {:model "test-model"}
    :provider :test
    :model "test-model"
    :warm! (fn [cancel?]
             (before-reply)
             (swap! warms conj (cancel?))
             {:api-usage {:input-tokens prompt-tokens
                          :output-tokens 1
                          :input-tokens-details {:cache-read prompt-tokens}}})}))

(defn- fire-last!
  "Moves the clock to `at-ms` and runs the last planned timer."
  [{:keys [now timers]} at-ms]
  (reset! now at-ms)
  ((:task (peek @timers))))

(defdescribe warm-delay-ms-test
             (it "warms at 90% of the TTL, at least 10 seconds before it ends"
                 (expect (= 270000 (sut/warm-delay-ms 300000)))
                 (expect (= 3240000 (sut/warm-delay-ms 3600000)))
                 (expect (= 5000 (sut/warm-delay-ms 15000)))
                 (expect (nil? (sut/warm-delay-ms 10000)))
                 (expect (nil? (sut/warm-delay-ms nil)))))

(defdescribe warm-savings-usd-test
             (it "weighs the avoided cache write by the reuse chance"
                 (with-redefs [accounting/response-cost fake-cost]
                   (let [request
                         {:pricing {} :provider :test :model "test-model" :prompt-tokens 100000}]
                     (expect (< 0.3149 (sut/warm-savings-usd request 1.0) 0.3151))
                     (expect (< 0.0217 (sut/warm-savings-usd request 0.15) 0.0218)))))
             (it "returns nil for an unpriced route"
                 (with-redefs [accounting/response-cost (constantly {:cost-usd nil})]
                   (expect (nil? (sut/warm-savings-usd {:pricing {} :prompt-tokens 100000} 1.0))))))

(defdescribe
  warmer-test
  (describe
    "a running turn"
    (it "warms before the TTL ends and counts the next warm from the warm start"
        (with-redefs [accounting/response-cost fake-cost]
          (let [{:keys [timers warmer] :as h} (harness)
                warms (atom [])]

            (sut/request-finished! warmer (request "running" 100000 warms))
            (expect (= [270000] (mapv :wait-ms @timers)))
            (fire-last! h 270000)
            (expect (= [false] @warms))
            (expect (= [270000 270000] (mapv :wait-ms @timers)))
            (expect (= 1 (count (:pending-costs @warmer)))))))
    (it "cancels the planned warm when a real request starts"
        (with-redefs [accounting/response-cost fake-cost]
          (let [{:keys [timers warmer] :as h} (harness)
                warms (atom [])]

            (sut/request-finished! warmer (request "running" 100000 warms))
            (sut/request-started! warmer)
            (expect @(:cancelled (peek @timers)))
            (fire-last! h 270000)
            (expect (empty? @warms)))))
    (it "aborts a running warm when a real request starts during it"
        (with-redefs [accounting/response-cost fake-cost]
          (let [{:keys [timers warmer] :as h} (harness)
                warms (atom [])]

            (sut/request-finished! warmer
                                   (request "running" 100000 warms #(sut/request-started! warmer)))
            (fire-last! h 270000)
            (expect (= [true] @warms))
            (expect (= 1 (count @timers))))))
    (it "skips a timer that fires after the cache can expire"
        (with-redefs [accounting/response-cost fake-cost]
          (let [{:keys [warmer] :as h} (harness)
                warms (atom [])]

            (sut/request-finished! warmer (request "running" 100000 warms))
            (fire-last! h (+ 270000 15001))
            (expect (empty? @warms)))))
    (it "skips a prompt too small to pay for its warm"
        (with-redefs [accounting/response-cost fake-cost]
          (let [{:keys [warmer] :as h} (harness)
                warms (atom [])]

            (sut/request-finished! warmer (request "running" 10000 warms))
            (fire-last! h 270000)
            (expect (empty? @warms)))))
    (it "plans nothing when keepalive is off or the route has no TTL"
        (let [{:keys [timers warmer]} (harness)]
          (sut/request-finished! warmer (request "off" 100000 (atom [])))
          (sut/request-finished! warmer (assoc (request "running" 100000 (atom [])) :ttl-ms nil))
          (expect (empty? @timers)))))
  (describe "a settled turn"
            (it "stops warms in running mode"
                (let [{:keys [timers warmer]} (harness)]
                  (sut/request-finished! warmer (request "running" 100000 (atom [])))
                  (sut/turn-settled! warmer)
                  (expect @(:cancelled (peek @timers)))
                  (expect (nil? (:request @warmer)))))
            (it "keeps warming a large prompt in idle mode until the idle window ends"
                (with-redefs [accounting/response-cost fake-cost]
                  (let [{:keys [warmer] :as h} (harness)
                        warms (atom [])]

                    (sut/request-finished! warmer (request "idle" 500000 warms))
                    (sut/turn-settled! warmer)
                    (doseq [n (range 1 8)]
                      (fire-last! h (* n 270000)))
                    (expect (= 6 (count @warms))))))
            (it "skips a prompt that idle reuse does not pay for"
                (with-redefs [accounting/response-cost fake-cost]
                  (let [{:keys [warmer] :as h} (harness)
                        warms (atom [])]

                    (sut/request-finished! warmer (request "idle" 100000 warms))
                    (sut/turn-settled! warmer)
                    (fire-last! h 270000)
                    (expect (empty? @warms))))))
  (describe "costs"
            (it "adds each finished warm cost to the turn accounting once"
                (with-redefs [accounting/response-cost fake-cost]
                  (let [{:keys [warmer] :as h} (harness)
                        acc (atom {})]

                    (sut/request-finished! warmer (request "running" 100000 (atom [])))
                    (fire-last! h 270000)
                    (sut/drain-costs! warmer acc)
                    (expect (some? (:accrued-cost @acc)))
                    (expect (empty? (:pending-costs @warmer))))))
            (it "ignores a missing warmer"
                (expect (nil? (sut/request-started! nil)))
                (expect (nil? (sut/turn-settled! nil)))
                (expect (nil? (sut/drain-costs! nil (atom {})))))))
