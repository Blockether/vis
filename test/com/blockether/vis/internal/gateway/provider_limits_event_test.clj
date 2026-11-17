(ns com.blockether.vis.internal.gateway.provider-limits-event-test
  (:require [com.blockether.vis.internal.gateway.bus :as bus]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.provider.limits :as limits]
            [lazytest.core :refer [around-each defdescribe expect it set-ns-context!]]
            [com.blockether.vis.internal.gateway.event-check :as event-check]))

;; Each published event payload must match its payload schema (#487).
(set-ns-context! [(around-each [f] (event-check/checked f))])

(defdescribe provider-limits-change-test
             ;; Issue #214: credential transitions must reach both SSE and polling clients.
             (it "publishes one provider invalidation in each registered session stream"
                 (let [registry (atom {"active" {:next-seq 0} "idle" {:next-seq 0}})]
                   (with-redefs-fn {#'state/registry registry
                                    #'bus/publish! (fn [& _])}
                     #(do (limits/auth-changed! :corp)
                          (doseq [sid ["active" "idle"]]
                            (let [events (state/events-since sid 0)]
                              (expect (= 1 (count events)))
                              (expect (= "provider.limits_changed" (get (first events) "type")))
                              (expect (= "corp" (get (first events) "provider_id")))
                              (expect (= sid (get (first events) "session_id"))))))))))
