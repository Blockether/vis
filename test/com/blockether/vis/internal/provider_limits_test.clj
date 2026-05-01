(ns com.blockether.vis.internal.provider-limits-test
  (:require [clojure.spec.alpha :as s]
            [com.blockether.vis.internal.provider-limits :as limits]
            [com.blockether.vis.internal.registry :as registry]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe provider-limits-test
  (it "merges static svar metadata with a provider-reported dynamic envelope"
    (with-redefs [registry/provider-by-id
                  (fn [pid]
                    (when (= pid :openai-codex)
                      {:provider/id :openai-codex
                       :provider/limits-fn
                       (fn []
                         {:provider-id :openai-codex
                          :status :ok
                          :fetched-at-ms 42
                          :dynamic {:limits [{:id :requests-per-day
                                              :label "Requests / day"
                                              :scope :account
                                              :kind :requests
                                              :precision :exact
                                              :source :provider-api
                                              :unlimited? false
                                              :used 1
                                              :limit 200
                                              :remaining 199
                                              :window {:kind :calendar
                                                       :unit :day
                                                       :size 1}}]}})}))]
      (let [report (limits/provider-limits :openai-codex)]
        (expect (= :ok (:status report)))
        (expect (= 500 (get-in report [:static :rpm])))
        (expect (= 2000000 (get-in report [:static :tpm])))
        (expect (s/valid? ::limits/report report)))))

  (it "returns :unsupported when the provider exposes no limit metadata at all"
    (with-redefs [registry/provider-by-id
                  (fn [pid]
                    (when (= pid :plain)
                      {:provider/id :plain}))]
      (let [report (limits/provider-limits :plain)]
        (expect (= :unsupported (:status report)))
        (expect (= [] (get-in report [:dynamic :limits])))
        (expect (s/valid? ::limits/report report)))))

  (it "returns :ok for static-only providers even when no runtime limits fn exists"
    (with-redefs [registry/provider-by-id (constantly nil)]
      (let [report (limits/provider-limits :openai-codex)]
        (expect (= :ok (:status report)))
        (expect (= 500 (get-in report [:static :rpm])))
        (expect (= 2000000 (get-in report [:static :tpm])))
        (expect (s/valid? ::limits/report report)))))

  (it "wraps invalid provider output in a valid :error envelope"
    (with-redefs [registry/provider-by-id
                  (fn [pid]
                    (when (= pid :broken)
                      {:provider/id :broken
                       :provider/limits-fn (fn [] {:provider-id :broken
                                                   :status :bogus})}))]
      (let [report (limits/provider-limits :broken)]
        (expect (= :error (:status report)))
        (expect (= :provider/invalid-limits-report (get-in report [:error :type])))
        (expect (s/valid? ::limits/report report)))))

  (it "returns one report per registered provider"
    (with-redefs [registry/registered-providers (fn [] [{:provider/id :one}
                                                        {:provider/id :two}])
                  registry/provider-by-id       (fn [pid] {:provider/id pid})]
      (expect (= [:one :two]
                (mapv :provider-id (limits/all-provider-limits)))))))
