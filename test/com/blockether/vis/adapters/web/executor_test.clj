(ns com.blockether.vis.adapters.web.executor-test
  (:require
   [lazytest.core :refer [defdescribe expect it]]
   [com.blockether.vis.adapters.web.executor :as sut]
   [com.blockether.vis.adapters.web.conversations :as web-conversations]))

(defdescribe on-chunk-handler-test
  (it "accepts streamed code blocks as maps"
    (let [conversation-id "test-conversation"
          handler (#'sut/on-chunk-handler conversation-id)]
      (try
        (swap! web-conversations/live-status dissoc conversation-id)
        (handler {:iteration 0
                  :thinking "Need cwd"
                  :code [{:expr "(def files (list-dir \".\"))"
                          :time-ms 200}]
                  :final nil
                  :done? false})
        (let [status (get @web-conversations/live-status conversation-id)]
          (expect (= "Iteration 1 → def" (:current status)))
          (expect (= [{:code "(def files (list-dir \".\"))"}]
                    (get-in status [:iterations 0 :executions]))))
        (finally
          (swap! web-conversations/live-status dissoc conversation-id))))))
