(ns com.blockether.vis.internal.extension-aggregate-test
  (:require
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.extension-aggregate :as aggregate]
   [com.blockether.vis.internal.persistance :as persistance]
   [lazytest.core :refer [defdescribe expect it]]))

(def ^:private fixture-extension
  {:ext/namespace 'test.aggregate.extension})

(defdescribe extension-aggregate-helper-contract-test
  (it "injects the current extension id and resolves conversation-state scope"
    (let [seen (atom nil)]
      (with-redefs [persistance/db-latest-conversation-state-id (fn [_db conversation-id]
                                                                  (str conversation-id "-state"))
                    persistance/db-put-extension-aggregate! (fn [db opts]
                                                              (reset! seen [db opts])
                                                              opts)]
        (binding [extension/*current-extension* fixture-extension]
          (let [row (aggregate/ext-put! {:db-info ::db :conversation-id "conv-1"}
                      {:key :index/status
                       :kind :background/status
                       :scope :conversation-state
                       :metadata {:schema-version 1}
                       :content {:state :ready}})]
            (expect (= ::db (first @seen)))
            (expect (= "test.aggregate.extension" (:extension-id row)))
            (expect (= :index/status (:aggregate-key row)))
            (expect (= :background/status (:kind row)))
            (expect (= "conv-1" (:conversation-soul-id row)))
            (expect (= "conv-1-state" (:conversation-state-id row))))))))

  (it "rejects caller supplied extension ids"
    (binding [extension/*current-extension* fixture-extension]
      (let [thrown (try
                     (aggregate/ext-put! {:db-info ::db}
                       {:extension-id 'evil.extension
                        :key :k
                        :kind :state
                        :scope :global
                        :content {}})
                     nil
                     (catch clojure.lang.ExceptionInfo e e))]
        (expect (= :extension-aggregate/extension-id-forbidden
                  (:type (ex-data thrown)))))))

  (it "requires extension callback context"
    (let [thrown (try
                   (aggregate/ext-list {:db-info ::db} {:kind :state})
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
      (expect (= :extension-aggregate/no-extension-context
                (:type (ex-data thrown))))))

  (it "scopes normal reads to the current extension only"
    (let [seen (atom nil)]
      (with-redefs [persistance/db-list-extension-aggregates (fn [_db opts]
                                                               (reset! seen opts)
                                                               [])]
        (binding [extension/*current-extension* fixture-extension]
          (expect (= [] (aggregate/ext-list {:db-info ::db}
                          {:kind :cache/search-result
                           :scope :global})))
          (expect (= "test.aggregate.extension" (:extension-id @seen)))
          (expect (= :cache/search-result (:kind @seen))))))))
