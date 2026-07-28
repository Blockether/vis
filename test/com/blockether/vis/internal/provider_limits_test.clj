(ns com.blockether.vis.internal.provider-limits-test
  "Anti-stampede contract for the provider limits cache.

   `:provider/limits-fn` is an upstream HTTP call and every channel reads limits
   independently (TUI footer thread, companion router dialog, `/v1/router`
   fan-out), so the cache must collapse a burst into ONE call and an auth change
   must drop it immediately."
  (:require [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]
            [com.blockether.vis.internal.provider-limits :as provider-limits]
            [com.blockether.vis.internal.registry :as registry]))

(defn- counting-provider
  [calls]
  {:provider/id :limits-cache-test
   :provider/limits-fn (fn []
                         (swap! calls inc)
                         (Thread/sleep 200)
                         {:status :ok})})

(deftest concurrent-limits-reads-share-one-upstream-call
  (let [calls (atom 0)]
    (with-redefs
      [registry/provider-by-id (fn [_]
                                 (counting-provider calls))]
      (provider-limits/flush-limits-cache!)
      (let
        [readers (doall
                   (repeatedly 8 #(future (provider-limits/provider-limits :limits-cache-test))))]
        (run! deref readers)
        (testing "eight simultaneous readers stampede into a single fetch" (is (= 1 @calls)))
        (testing "a later read inside the TTL is served from cache"
          (provider-limits/provider-limits :limits-cache-test)
          (is (= 1 @calls)))
        (testing "every reader still got a valid report"
          (is (= :ok (:status (provider-limits/provider-limits :limits-cache-test)))))))))

(deftest flushing-the-cache-forces-the-next-read-upstream
  (let [calls (atom 0)]
    (with-redefs
      [registry/provider-by-id (fn [_]
                                 (counting-provider calls))]
      (provider-limits/flush-limits-cache!)
      (provider-limits/provider-limits :limits-cache-test)
      (is (= 1 @calls))
      (provider-limits/flush-limits-cache! :limits-cache-test)
      (provider-limits/provider-limits :limits-cache-test)
      (testing "sign-in / sign-out must not leave a stale :unauthenticated report"
        (is (= 2 @calls))))))
