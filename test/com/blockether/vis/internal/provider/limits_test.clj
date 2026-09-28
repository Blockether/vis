(ns com.blockether.vis.internal.provider.limits-test
  "Anti-stampede contract for the provider limits cache.

   `:provider/limits-fn` is an upstream HTTP call and every channel reads limits
   independently (TUI footer thread, companion router dialog, `/v1/router`
   fan-out), so the cache must collapse a burst into ONE call and an auth change
   must drop it immediately."
  (:require [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]]
            [com.blockether.vis.contract.provider :as contract-provider]
            [com.blockether.vis.internal.provider.limits :as provider-limits]
            [com.blockether.vis.internal.extension.registry :as registry]))

(defn- counting-provider
  [calls]
  {:provider/id :limits-cache-test
   :provider/limits-fn (fn []
                         (swap! calls inc)
                         (Thread/sleep 200)
                         {:status :ok})})

(defdescribe
  concurrent-limits-reads-share-one-upstream-call
  (it "concurrent limits reads share one upstream call"
      (let [calls (atom 0)]
        (with-redefs [registry/provider-by-id (fn [_]
                                                (counting-provider calls))]
          (provider-limits/flush-limits-cache!)
          (let [readers (doall (repeatedly 8
                                           #(future (provider-limits/provider-limits
                                                      :limits-cache-test))))]
            (run! deref readers)
            ;; eight simultaneous readers stampede into a single fetch
            (expect (= 1 @calls))
            ;; a later read inside the TTL is served from cache
            (provider-limits/provider-limits :limits-cache-test)
            (expect (= 1 @calls))
            ;; every reader still got a valid report
            (expect (= :ok (:status (provider-limits/provider-limits :limits-cache-test)))))))))

(defdescribe flushing-the-cache-forces-the-next-read-upstream
             (it "flushing the cache forces the next read upstream"
                 (let [calls (atom 0)]
                   (with-redefs [registry/provider-by-id (fn [_]
                                                           (counting-provider calls))]
                     (provider-limits/flush-limits-cache!)
                     (provider-limits/provider-limits :limits-cache-test)
                     (expect (= 1 @calls))
                     (provider-limits/flush-limits-cache! :limits-cache-test)
                     (provider-limits/provider-limits :limits-cache-test)
                     ;; sign-in / sign-out must not leave a stale :unauthenticated report
                     (expect (= 2 @calls))))))

(defdescribe
  a-non-fetching-read-answers-only-what-is-already-known
  (it "a non fetching read answers only what is already known"
      ;; A fleet mutation (adding or removing a provider) repaints from what the
      ;; daemon already holds: the human behind it is waiting on the next screen, so
      ;; no row of that payload may call a provider's usage endpoint.
      (let [calls (atom 0)]
        (with-redefs [registry/provider-by-id (fn [_]
                                                (counting-provider calls))]
          (provider-limits/flush-limits-cache! :limits-cache-test)
          ;; nothing cached: no report, and nothing fetched
          (expect (nil? (provider-limits/cached-limits-report :limits-cache-test)))
          (expect (zero? @calls))
          ;; the padded read still answers a contract-valid report, still without fetching
          (let [report (provider-limits/limits-without-fetching :limits-cache-test)]
            (expect (contract-provider/report-valid? report))
            (expect (= :limits-cache-test (:provider-id report)))
            (expect (= :ok (:status report)))
            (expect (= [] (get-in report [:dynamic :limits]))
                    "an unchecked quota is reported as none")
            (expect (zero? @calls)))
          ;; one live read later, both answer from the cache
          (expect (= :ok (:status (provider-limits/provider-limits :limits-cache-test))))
          (expect (= 1 @calls))
          (expect (= :ok (:status (provider-limits/cached-limits-report :limits-cache-test))))
          (expect (= :ok (:status (provider-limits/limits-without-fetching :limits-cache-test))))
          (expect (= 1 @calls))
          ;; a flush takes the answer away instead of serving a verdict auth just changed
          (provider-limits/flush-limits-cache! :limits-cache-test)
          (expect (nil? (provider-limits/cached-limits-report :limits-cache-test)))
          (expect (= 1 @calls))))))

(defdescribe a-thrown-auth-rejection-is-an-unauthenticated-report
             (it "a thrown auth rejection is an unauthenticated report"
                 (with-redefs [registry/provider-by-id
                               (constantly {:provider/id :rejected-limits-test
                                            :provider/limits-fn
                                            (fn []
                                              (throw (ex-info "Provider rejected the credential"
                                                              {:status 401})))})]
                   (provider-limits/flush-limits-cache! :rejected-limits-test)
                   (let [report (provider-limits/provider-limits :rejected-limits-test)]
                     (expect (= :unauthenticated (:status report)))
                     (expect (= [] (get-in report [:dynamic :limits])))))))

(defdescribe a-provider-declares-how-long-its-report-may-be-reused
             (it "a 1ms budget expires where the 15s default would still be serving"
                 (let [calls (atom 0)]
                   (with-redefs [registry/provider-by-id (fn [_]
                                                           {:provider/id :budget-limits-test
                                                            :provider/limits-cache-ms 1
                                                            :provider/limits-fn (fn []
                                                                                  (swap! calls inc)
                                                                                  {:status :ok})})]
                     (provider-limits/flush-limits-cache! :budget-limits-test)
                     (provider-limits/provider-limits :budget-limits-test)
                     (Thread/sleep 5)
                     (provider-limits/provider-limits :budget-limits-test)
                     (expect (= 2 @calls))))))

(defdescribe a-throttled-usage-endpoint-serves-the-last-good-report
             (it "a throttled usage endpoint serves the last good report"
                 (let [calls (atom 0)]
                   (with-redefs [registry/provider-by-id
                                 (fn [_]
                                   {:provider/id :throttled-limits-test
                                    :provider/limits-cache-ms 1
                                    :provider/limits-fn
                                    (fn []
                                      (if (= 1 (swap! calls inc))
                                        {:status :ok :dynamic {:limits [] :note "live usage"}}
                                        {:status :error
                                         :error {:type :test/throttled
                                                 :message "usage endpoint refused the check"
                                                 :data {:status 429}}}))})]
                     (provider-limits/flush-limits-cache! :throttled-limits-test)
                     (expect (= :ok
                                (:status (provider-limits/provider-limits :throttled-limits-test))))
                     (Thread/sleep 5)
                     (let [throttled (provider-limits/provider-limits :throttled-limits-test)]
                       ;; the last good quota stands in for the refusal, and says why
                       (expect (= :ok (:status throttled)))
                       (expect (str/includes? (get-in throttled [:dynamic :note]) "HTTP 429"))
                       ;; the back-off holds: no third call while it lasts
                       (provider-limits/provider-limits :throttled-limits-test)
                       (expect (= 2 @calls)))))))
