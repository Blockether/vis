(ns com.blockether.vis.internal.provider.flow-test
  (:require [com.blockether.vis.internal.provider.flow :as flow]
            [com.blockether.vis.internal.provider.auth :as auth]
            [com.blockether.vis.internal.provider.service :as providers]
            [com.blockether.vis.internal.extension.registry :as registry]
            [com.blockether.vis.internal.util :as util]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- app-start
  []
  {:kind :pkce
   :callback-mode "app"
   :redirect-uri flow/app-callback-uri
   :url (str "https://gateway.example.com/authorize?state=test-state&redirect_uri="
             "com.blockether.viscompanion%3A%2F%2Foauth%2Fcallback")
   :flow {:state "test-state" :verifier "private-test-verifier"}})

(def ^:private callback-url (str flow/app-callback-uri "?state=test-state&code=test-code"))

(defdescribe
  shared-engine-test
  (it "uses one validated app-return and retained verdict contract for both domains"
      (doseq [domain [:provider :mcp]]
        (let [exchanges (atom [])
              settled (atom 0)
              started (flow/start! [domain :contract]
                                   {:start app-start
                                    :complete (fn [private input]
                                                (swap! exchanges conj [private input])
                                                {:token "never-public"})
                                    :settle #(swap! settled inc)})
              id (get-in started [:flow :flow-id])]

          (try (expect (not (.contains (pr-str started) "private-test-verifier")))
               (expect (= :invalid-input (:error (flow/complete! domain id "test-code"))))
               (expect (= :invalid-input
                          (:error (flow/complete! domain id (str callback-url "&error=")))))
               (expect (= :invalid-input
                          (:error (flow/complete! domain
                                                  id
                                                  (str flow/app-callback-uri
                                                       "?state=wrong&code=test-code")))))
               (expect (empty? @exchanges))
               (expect (= "ok" (:status (flow/complete! domain id callback-url))))
               (expect (= "ok" (:status (flow/poll! domain id))))
               (expect (= :unknown-flow (:error (flow/complete! domain id callback-url))))
               (expect (= 1 @settled))
               (expect (= [[{:state "test-state" :verifier "private-test-verifier"} callback-url]]
                          @exchanges))
               (expect (not (.contains (pr-str (flow/poll! domain id)) "never-public")))
               (finally (flow/cancel! domain id))))))
  (it
    "routes a registered model adapter through the same app callback, with no special vendor branch"
    (with-redefs [registry/provider-by-id
                  (constantly {:provider/auth-start-fn app-start
                               :provider/auth-complete-fn (fn [& _]
                                                            :ok)})

                  providers/rebuild-shared-router!
                  (constantly nil)]

      (let [started
            (auth/start-auth! :example-oauth)

            id
            (get-in started [:flow :flow-id])]

        (try (expect (:ok? started))
             (expect (= "app" (get-in started [:flow :callback-mode])))
             (expect (= "ok" (:status (auth/complete-auth! id callback-url))))
             (expect (= "ok" (:status (auth/poll-auth! id))))
             (finally (auth/cancel-auth! id))))))
  (it "does not let one domain spend, read or cancel the other domain's flow"
      (let [id (get-in (flow/start! [:mcp :same-name]
                                    {:start app-start
                                     :complete (fn [& _]
                                                 :ok)})
                       [:flow :flow-id])]
        (try (expect (= :unknown-flow (:error (flow/complete! :provider id callback-url))))
             (expect (= :unknown-flow (:error (flow/poll! :provider id))))
             (flow/cancel! :provider id)
             (expect (= "pending" (:status (flow/poll! :mcp id))))
             (finally (flow/cancel! :mcp id)))))
  (it "applies the same expiry ceiling and refuses exchange exactly at the deadline"
      (let [now
            (atom 1000)

            exchanges
            (atom 0)]

        (with-redefs [util/now-ms (fn ^long []
                                    (long @now))]
          (let [started (flow/start! [:mcp :expiry]
                                     {:start #(assoc (app-start) :expires-in-ms 3600000)
                                      :complete (fn [& _]
                                                  (swap! exchanges inc))})
                id (get-in started [:flow :flow-id])]

            (try (expect (= 901000 (get-in started [:flow :expires-at])))
                 (reset! now 901000)
                 (expect (= :unknown-flow (:error (flow/complete! :mcp id callback-url))))
                 (expect (zero? @exchanges))
                 (finally (flow/cancel! :mcp id)))))))
  (it "does not publish or settle a cancelled device worker that ignores interruption"
      (let [entered
            (promise)

            finished
            (promise)

            settled
            (atom 0)

            started
            (flow/start! [:provider :device]
                         {:start (fn []
                                   {:kind :device :flow {}})
                          :await (fn [_]
                                   (deliver entered true)
                                   (try (Thread/sleep 10000) (catch InterruptedException _ nil))
                                   (deliver finished true))
                          :settle #(swap! settled inc)})

            id
            (get-in started [:flow :flow-id])]

        (try (expect (= true (deref entered 2000 :timeout)))
             (flow/cancel! :provider id)
             (expect (= true (deref finished 2000 :timeout)))
             (expect (= :unknown-flow (:error (flow/poll! :provider id))))
             (expect (zero? @settled))
             (finally (flow/cancel! :provider id)))))
  (it "never lets a slow old start replace a newer attempt"
      (let [entered
            (promise)

            release
            (promise)

            old
            (future (flow/start! [:mcp :concurrent]
                                 {:start (fn []
                                           (deliver entered true)
                                           @release
                                           (app-start))}))]

        (expect (= true (deref entered 2000 :timeout)))
        (let [newer
              (flow/start! [:mcp :concurrent] {:start app-start})

              id
              (get-in newer [:flow :flow-id])]

          (try (deliver release true)
               (expect (= :unknown-flow (:error (deref old 2000 {}))))
               (expect (= "pending" (:status (flow/poll! :mcp id))))
               (finally (flow/cancel-owner! [:mcp :concurrent]) (future-cancel old)))))))
