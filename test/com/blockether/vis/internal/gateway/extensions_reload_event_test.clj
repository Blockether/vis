(ns com.blockether.vis.internal.gateway.extensions-reload-event-test
  (:require [com.blockether.vis.internal.gateway.bus :as bus]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.python.extensions :as python-extensions]
            [lazytest.core :refer [around-each defdescribe expect it set-ns-context!]]
            [com.blockether.vis.internal.gateway.event-check :as event-check]))

;; Each published event payload must match its payload schema (#487).
(set-ns-context! [(around-each [f] (event-check/checked f))])

(defdescribe
  extensions-reload-event-test
  ;; Issue #393: a reload from one client must reach every other client.
  (it "sends one reload id to the fleet feed and to each registered session stream"
      (let [registry
            (atom {"active" {:next-seq 0} "idle" {:next-seq 0}})

            frames
            (atom [])]

        (with-redefs-fn {#'state/registry registry
                         #'state/fleet-sinks (atom {"watch" #(swap! frames conj %)})
                         #'bus/publish! (fn [& _])}
          #(let [reload-id (python-extensions/announce-reload!)] (expect (string? reload-id))
             (expect (= [["extensions.reloaded" reload-id]]
                        (mapv (juxt (fn [frame]
                                      (get frame "type"))
                                    (fn [frame]
                                      (get frame "reload_id")))
                              @frames))) (doseq [sid ["active" "idle"]]
                                           (let [events (state/events-since sid 0)]
                                             (expect (= 1 (count events)))
                                             (expect (= "extensions.reloaded"
                                                        (get (first events) "type")))
                                             (expect (= reload-id (get (first events) "reload_id")))
                                             (expect (= sid
                                                        (get (first events) "session_id"))))))))))
