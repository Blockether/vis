(ns com.blockether.vis.internal.provider.auth-limits-test
  (:require [com.blockether.vis.internal.extension.registry :as registry]
            [com.blockether.vis.internal.provider.auth-health :as auth-health]
            [com.blockether.vis.internal.loop.router :as loop-router]
            [com.blockether.vis.internal.provider.limits :as limits]
            [com.blockether.vis.internal.provider.auth :as auth]
            [com.blockether.vis.internal.provider.service :as providers]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]))

(deftest automatic-auth-invalidates-limits
  ;; Issue #214: first-use login must not leave the pre-login quota cached.
  (doseq [outcome [:ok :cancelled :failed]]
    (testing (name outcome)
      (let [token (atom nil)
            calls (atom 0)
            notifications (atom [])
            provider {:provider/is-managed true
                      :provider/get-token-fn #(when @token {:token @token})
                      :provider/auth-fn (fn [_]
                                          (case outcome
                                            :ok
                                            (reset! token "signed-in")

                                            :cancelled
                                            nil

                                            :failed
                                            (throw (ex-info "Login failed" {}))))
                      :provider/limits-fn #(do (swap! calls inc)
                                               {:status (if @token :ok :unauthenticated)})}]

        (limits/flush-limits-cache! :auth-limits-test)
        (try (with-redefs [registry/provider-by-id (constantly provider)
                           limits/auth-change-listeners (atom #{#(swap! notifications conj %)})]

               (is (= :unauthenticated (:status (limits/provider-limits :auth-limits-test))))
               (try (auth-health/ensure-authenticated! :auth-limits-test)
                    (catch clojure.lang.ExceptionInfo _ nil))
               (is (= (if (= :ok outcome) :ok :unauthenticated)
                      (:status (limits/provider-limits :auth-limits-test))))
               (is (= (if (= :ok outcome) 2 1) @calls))
               (is (= (if (= :ok outcome) [:auth-limits-test] []) @notifications)))
             (finally (limits/flush-limits-cache! :auth-limits-test)))))))

(deftest refresh-and-logout-notify-after-invalidation
  ;; Issue #214: the same lifecycle signal applies to refresh and sign-out.
  (let [token
        (atom "old")

        observed
        (atom [])

        provider
        {:provider/is-managed true
         :provider/get-token-fn #(when @token {:token @token})
         :provider/auth-fn (fn [_]
                             (throw (ex-info "Unexpected login" {})))
         :provider/refresh-token-fn (fn [_]
                                      (reset! token "fresh"))
         :provider/logout-fn #(reset! token nil)
         :provider/limits-fn #(hash-map :status (if @token :ok :unauthenticated)
                                        :dynamic {:note (or @token "signed-out")})}]

    (limits/flush-limits-cache! :auth-limits-test)
    (try (with-redefs [registry/provider-by-id
                       (constantly provider)

                       providers/rebuild-shared-router!
                       (constantly nil)

                       limits/auth-change-listeners
                       (atom #{(fn [pid]
                                 (swap! observed conj
                                   (get-in (limits/provider-limits pid) [:dynamic :note])))
                               (fn [_]
                                 (throw (ex-info "Disconnected view" {})))})]

           (is (= "old" (get-in (limits/provider-limits :auth-limits-test) [:dynamic :note])))
           (with-redefs-fn {#'auth-health/refresh-allowed? (constantly true)
                            #'auth-health/last-refreshed (atom {})}
             #(is (true? (#'loop-router/try-refresh-provider-token!
                          {:providers [{:id :auth-limits-test :api-key "old"}]}
                          {:provider :auth-limits-test}))))
           (is (= ["fresh"] @observed))
           (is (:ok? (auth/logout! :auth-limits-test)))
           (is (= ["fresh" "signed-out"] @observed)))
         (finally (limits/flush-limits-cache! :auth-limits-test)))))
