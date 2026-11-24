(ns com.blockether.vis.internal.gateway.event-check-test
  (:require [com.blockether.vis.internal.gateway.event-check :as event-check]
            [com.blockether.vis.internal.gateway.journal :as journal]
            [com.blockether.vis.internal.gateway.state :as state]
            [lazytest.core :refer [defdescribe expect it throws?]]))

(defdescribe
  event-check-refusal-test
  (it "refuses a published event whose required payload field has another name"
      ;; Without the hook, a renamed field reaches each SDK and client unseen (#487).
      (let [sid (str (random-uuid))]
        (with-redefs [journal/publish! (fn [& _])]
          (try (event-check/checked
                 #(expect (throws?
                            clojure.lang.ExceptionInfo
                            (fn []
                              (state/append-event! sid "queue.paused" {:why "manual" :held 1})))))
               (event-check/checked
                 #(state/append-event! sid "queue.paused" {:reason "manual" :held 1}))
               (expect (contains? @event-check/checked-types "queue.paused"))
               (finally (#'state/drop-session! sid))))))
  (it "refuses a fleet frame whose required payload field is missing"
      (event-check/checked
        #(expect (throws?
                   clojure.lang.ExceptionInfo
                   (fn []
                     (#'state/publish-fleet!
                      [{"schema" 1 "type" "session.status" "session_id" "s1" "is_live" true}])))))))
