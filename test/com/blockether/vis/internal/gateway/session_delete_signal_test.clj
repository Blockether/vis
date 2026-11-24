(ns com.blockether.vis.internal.gateway.session-delete-signal-test
  (:require [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.internal.bus :as bus]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.persistance.core :as persistance]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [lazytest.core :refer [defdescribe expect it]]))

(h/use-mem-store!)

(defdescribe
  session-delete-signal-test
  (it "reports each committed deletion once even when a parent delete cascades"
      (let [db (h/store)
            sid (h/store-session! db {:channel :api})
            child (h/store-session! db {:channel :api
                                        :parent-state-id (persistance/db-latest-session-state-id db sid)})
            seen (atom [])
            drained (promise)
            subscription (bus/subscribe!
                           "vis.session.deleted"
                           (fn [signal]
                             (let [id (get-in signal ["data" "session_id"])]
                               (cond (= "delete-signal-test-fence" id) (deliver drained true)
                                     (contains? #{(str sid) (str child)} id)
                                     (swap! seen conj [signal (persistance/db-get-session db id)])))))]
        (try
          (with-redefs-fn {#'lp/db-info (constantly db)
                           #'state/teardown-session-async! (constantly nil)}
            #(do (state/close-session! sid)
                 (state/close-session! sid)
                 (state/close-session! child)))
          (bus/publish! "vis.session.deleted" {"session_id" "delete-signal-test-fence"})
          (expect (true? (deref drained 5000 false)))
          (expect (= {(str sid) 1 (str child) 1}
                     (frequencies (map #(get-in (first %) ["data" "session_id"]) @seen))))
          (doseq [[signal row] @seen]
            (expect (nil? row))
            (expect (document/valid? "gateway" "bus_vis_signal" (select-keys signal ["topic" "data"]))))
          (finally (bus/unsubscribe! :vis subscription))))))
