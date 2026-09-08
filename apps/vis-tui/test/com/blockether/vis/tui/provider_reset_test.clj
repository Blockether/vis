(ns com.blockether.vis.tui.provider-reset-test
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.capture :as cap]
            [com.blockether.vis.tui.client :as client]
            [com.blockether.vis.tui.config :as config]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.provider :as provider]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(def credits {:status :ok :account-id "account-1" :available-count 2})

(deftest pending-reset-survives-an-unknown-result
  (let [saved
        (atom {})

        posts
        (atom [])]

    (with-redefs [config/load-raw
                  (fn []
                    @saved)

                  config/update!
                  (fn [f]
                    (swap! saved f))

                  client/consume-provider-reset-credit!
                  (fn [pid account key]
                    (swap! posts conj [pid account key])
                    (if (= 1 (count @posts))
                      (throw (ex-info "Unknown response" {}))
                      {:outcome "already_redeemed"}))]

      (with-redefs-fn {#'client/target* (atom {:base-url "https://gateway.example.com"})}
        (fn []
          (is (try (client/reset-provider-limits! :openai-codex "account-1")
                   false
                   (catch Exception _ true)))
          (is (client/pending-provider-reset? :openai-codex "account-1"))
          (is (not (client/pending-provider-reset? :openai-codex "account-2")))
          (is (= {:outcome "already_redeemed"}
                 (client/reset-provider-limits! :openai-codex "account-1")))
          (is (= (first @posts) (second @posts)))
          (is (not (client/pending-provider-reset? :openai-codex "account-1"))))))))

(deftest reset-action-is-authenticated-codex-only
  (is (= "1 limit reset available"
         (client/provider-reset-summary (assoc credits :available-count 1))))
  (is (= "2 limit resets available" (client/provider-reset-summary credits)))
  (is (= "Reset limits... (2 available)"
         (:label (some #(when (= :reset-limits (:id %)) %)
                       (provider/provider-action-items {:id :openai-codex}
                                                       {"is_authenticated" true}
                                                       false
                                                       false
                                                       credits)))))
  (doseq [[pid authed] [[:openai true] [:openai-codex false]]]
    (is (not-any? #(= :reset-limits (:id %))
                  (provider/provider-action-items {:id pid}
                                                  {"is_authenticated" authed}
                                                  false
                                                  false
                                                  credits)))))

(defn- wait-for-operation
  [_title _line done?]
  (loop [remaining 500]
    (cond (done?) true
          (zero? remaining) (throw (ex-info "Operation did not finish" {}))
          :else (do (Thread/sleep 10) (recur (dec remaining))))))

(deftest confirmation-and-refresh-are-separate-from-consumption
  (doseq [answer [false true]]
    (let [confirmed (atom nil)
          spent (atom [])
          notes (atom [])
          reads (atom 0)]

      (with-redefs [client/gateway-provider-limits (fn [_]
                                                     (swap! reads inc)
                                                     {:dynamic {:reset-credits credits}})
                    client/pending-provider-reset? (constantly false)
                    client/reset-provider-limits! (fn [pid account]
                                                    (swap! spent conj [pid account])
                                                    {:outcome "reset"})
                    dlg/band-questions (fn [& _]
                                         {:wait! wait-for-operation
                                          :confirm! (fn [question opts]
                                                      (reset! confirmed [question opts])
                                                      answer)
                                          :note! (fn [title text]
                                                   (swap! notes conj [title text]))})]

        (is (= answer (boolean (provider/reset-provider-limits! nil nil nil {:id :openai-codex}))))
        (is (str/includes? (str @confirmed) "account-1"))
        (is (str/includes? (str @confirmed) "all devices"))
        (is (= "Cancel" (get-in @confirmed [1 :no-label])))
        (is (= (if answer [[:openai-codex "account-1"]] []) @spent))
        (is (= (if answer 2 1) @reads))))))

(deftest unknown-and-zero-allowance-never-spend
  (doseq [summary [nil {:status :error} {:status :unsupported} (assoc credits :available-count 0)]]
    (let [calls (atom [])]
      (with-redefs [client/gateway-provider-limits (fn [_]
                                                     {:dynamic {:reset-credits summary}})
                    client/pending-provider-reset? (constantly false)
                    client/reset-provider-limits! (fn [& _]
                                                    (swap! calls conj :spend))
                    dlg/band-questions (fn [& _]
                                         {:wait! wait-for-operation
                                          :confirm! (fn [& _]
                                                      (swap! calls conj :confirm)
                                                      true)
                                          :note! (fn [& _])})]

        (is (false? (provider/reset-provider-limits! nil nil nil {:id :openai-codex})))
        (is (empty? @calls))))))

(deftest real-terminal-confirmation-does-not-consume-on-enter-or-escape
  (let [questions
        dlg/band-questions

        spends
        (atom 0)]

    (with-redefs [client/gateway-provider-limits
                  (fn [_]
                    {:dynamic {:reset-credits credits}})

                  client/pending-provider-reset?
                  (constantly false)

                  client/reset-provider-limits!
                  (fn [& _]
                    (swap! spends inc))

                  dlg/band-questions
                  (fn [& args]
                    (assoc (apply questions args) :wait! wait-for-operation))]

      (let [capture
            (cap/capture! {:cols 100
                           :rows 30
                           :keys [:enter :esc]
                           :paint!
                           (fn [{:keys [screen g]}]
                             (provider/reset-provider-limits!
                               screen
                               g
                               {:left 0 :top 2 :inner-w 96 :hint-row 24 :text-w 92 :bottom 26}
                               {:id :openai-codex}))})

            text
            (str/join "\n" (map cap/frame-text (:frames capture)))]

        (is (nil? (:error capture)))
        (is (zero? @spends))
        (is (str/includes? text "account-1"))
        (is (str/includes? text "Cancel"))
        (is (str/includes? text "all devices"))))))
