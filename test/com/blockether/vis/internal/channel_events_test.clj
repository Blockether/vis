(ns com.blockether.vis.internal.channel-events-test
  (:require [com.blockether.vis.internal.channel-events :as events]
            [lazytest.core :refer [defdescribe it expect]]))

(defdescribe channel-events-test
  (it "publishes events to registered listeners and stamps channel id"
    (let [seen (atom [])]
      (events/add-channel-event-listener! :tui ::a #(swap! seen conj %))
      (try
        (expect (= 1 (events/publish-channel-event! :tui {:op :status/set :text "x"})))
        (expect (= [{:op :status/set :text "x" :channel/id :tui}] @seen))
        (finally
          (events/remove-channel-event-listener! :tui ::a)))))

  (it "isolates listener failures"
    (let [seen (atom [])]
      (events/add-channel-event-listener! :tui ::bad (fn [_] (throw (ex-info "boom" {}))))
      (events/add-channel-event-listener! :tui ::good #(swap! seen conj (:op %)))
      (try
        (expect (= 2 (events/publish-channel-event! :tui {:op :input/replace})))
        (expect (= [:input/replace] @seen))
        (finally
          (events/remove-channel-event-listener! :tui ::bad)
          (events/remove-channel-event-listener! :tui ::good))))))
