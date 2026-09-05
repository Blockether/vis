(ns com.blockether.vis.internal.provider.limits-test
  "Anti-stampede contract for the provider limits cache.

   `:provider/limits-fn` is an upstream HTTP call and every channel reads limits
   independently (TUI footer thread, companion router dialog, `/v1/router`
   fan-out), so the cache must collapse a burst into ONE call and an auth change
   must drop it immediately."
  (:require [clojure.string :as str]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]
            [com.blockether.vis.internal.provider.limits :as provider-limits]
            [com.blockether.vis.internal.extension.registry :as registry]))

(defn- counting-provider
  [calls]
  {:provider/id :limits-cache-test
   :provider/limits-fn (fn []
                         (swap! calls inc)
                         (Thread/sleep 200)
                         {:status :ok})})

(deftest concurrent-limits-reads-share-one-upstream-call
  (let [calls (atom 0)]
    (with-redefs [registry/provider-by-id (fn [_]
                                            (counting-provider calls))]
      (provider-limits/flush-limits-cache!)
      (let [readers
            (doall (repeatedly 8 #(future (provider-limits/provider-limits :limits-cache-test))))]
        (run! deref readers)
        (testing "eight simultaneous readers stampede into a single fetch" (is (= 1 @calls)))
        (testing "a later read inside the TTL is served from cache"
          (provider-limits/provider-limits :limits-cache-test)
          (is (= 1 @calls)))
        (testing "every reader still got a valid report"
          (is (= :ok (:status (provider-limits/provider-limits :limits-cache-test)))))))))

(deftest flushing-the-cache-forces-the-next-read-upstream
  (let [calls (atom 0)]
    (with-redefs [registry/provider-by-id (fn [_]
                                            (counting-provider calls))]
      (provider-limits/flush-limits-cache!)
      (provider-limits/provider-limits :limits-cache-test)
      (is (= 1 @calls))
      (provider-limits/flush-limits-cache! :limits-cache-test)
      (provider-limits/provider-limits :limits-cache-test)
      (testing "sign-in / sign-out must not leave a stale :unauthenticated report"
        (is (= 2 @calls))))))

(deftest a-thrown-auth-rejection-is-an-unauthenticated-report
  (with-redefs [registry/provider-by-id (constantly {:provider/id :rejected-limits-test
                                                     :provider/limits-fn
                                                     (fn []
                                                       (throw (ex-info
                                                                "Provider rejected the credential"
                                                                {:status 401})))})]
    (provider-limits/flush-limits-cache! :rejected-limits-test)
    (let [report (provider-limits/provider-limits :rejected-limits-test)]
      (is (= :unauthenticated (:status report)))
      (is (= [] (get-in report [:dynamic :limits]))))))

(deftest a-provider-declares-how-long-its-report-may-be-reused
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
      (testing "a 1ms budget expires where the 15s default would still be serving"
        (is (= 2 @calls))))))

(deftest a-throttled-usage-endpoint-serves-the-last-good-report
  (let [calls (atom 0)]
    (with-redefs [registry/provider-by-id
                  (fn [_]
                    {:provider/id :throttled-limits-test
                     :provider/limits-cache-ms 1
                     :provider/limits-fn (fn []
                                           (if (= 1 (swap! calls inc))
                                             {:status :ok :dynamic {:limits [] :note "live usage"}}
                                             {:status :error
                                              :error {:type :test/throttled
                                                      :message "usage endpoint refused the check"
                                                      :data {:status 429}}}))})]
      (provider-limits/flush-limits-cache! :throttled-limits-test)
      (is (= :ok (:status (provider-limits/provider-limits :throttled-limits-test))))
      (Thread/sleep 5)
      (let [throttled (provider-limits/provider-limits :throttled-limits-test)]
        (testing "the last good quota stands in for the refusal, and says why"
          (is (= :ok (:status throttled)))
          (is (str/includes? (get-in throttled [:dynamic :note]) "HTTP 429")))
        (testing "the back-off holds: no third call while it lasts"
          (provider-limits/provider-limits :throttled-limits-test)
          (is (= 2 @calls)))))))
