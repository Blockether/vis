(ns com.blockether.vis.internal.gateway.retention-test
  (:require [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.gateway.bus :as bus]
            [com.blockether.vis.internal.gateway.turn-archive :as turn-archive]
            [com.blockether.vis.internal.persistance.core :as persistence]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.session.cancellation :as cancellation]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  terminal-turn-releases-execution-payload-test
  (doseq [status ["completed" "failed" "cancelled" "suspended"]]
    (it
      status
      (let [sid (str (random-uuid))
            tid (str (random-uuid))
            token (cancellation/cancellation-token)
            payload (apply str (repeat 10000 "retained payload "))
            registry (atom {sid {:current-turn tid
                                 :turn-order [tid]
                                 :turns {tid {:turn_id tid
                                              :session_id sid
                                              :status "running"
                                              :request payload
                                              :messages [{:content payload}]
                                              :attachments [{:base64 payload}]
                                              :engine-opts {:hooks {:on-chunk identity}}
                                              :extra-body {:input payload}
                                              :workspace {:root "/tmp"}
                                              :cancel-token token
                                              :idempotency_key "once"}}}})]

        (with-redefs-fn {#'state/registry registry #'lp/db-info (constantly nil)}
          (fn []
            (try (#'state/finish-turn!
                  sid
                  tid
                  {:status status :content [{"type" "prose" "text" payload}]})
                 (let [stored (get-in @registry [sid :turns tid])]
                   ;; A lean wire projection is insufficient: check the actual registry root.
                   (expect (not-any? #(contains? stored %)
                                     [:request :messages :attachments :engine-opts :extra-body
                                      :workspace :cancel-token :content]))
                   (expect (= status (:status stored)))
                   (expect (nil? (get-in @registry [sid :current-turn])))
                   (expect (= payload (get (state/get-turn sid tid) "request")))
                   (expect (= payload (get-in (state/get-turn sid tid) ["content" 0 "text"])))
                   (expect (= "once" (get (state/get-turn sid tid) "idempotency_key"))))
                 (finally (#'state/drop-session! sid)))))))))

(defdescribe
  terminal-disposes-cancellation-registration-test
  (it "terminal disposes cancellation registration"
      (doseq [finish-first? [false true]]
        (let [sid (str (random-uuid))
              tid (str (random-uuid))
              token (cancellation/cancellation-token)
              registry (atom {sid {:turns {tid
                                           {:turn_id tid :status "running" :cancel-token token}}}})
              called (atom false)]

          (with-redefs-fn {#'state/registry registry #'lp/db-info (constantly nil)}
            (fn []
              (try (let [dispose (cancellation/on-cancel! token #(reset! called true))
                         finish! #(#'state/finish-turn! sid tid {:status "completed"})
                         install! #(#'state/install-turn-cancel-disposer! sid tid token dispose)]

                     (expect (#'state/claim-turn-terminal! sid tid token))
                     (if finish-first? (do (finish!) (install!)) (do (install!) (finish!)))
                     (expect (empty? @(::cancellation/callbacks token)))
                     (expect (not (#'state/claim-turn-terminal! sid tid token)))
                     (cancellation/cancel! token :test)
                     (expect (false? @called)))
                   (finally (#'state/drop-session! sid)
                            (#'state/release-turn-terminal-claim! sid tid)))))))))

(defdescribe mirrored-terminal-releases-payload-test
             (it "mirrored terminal releases payload"
                 (let [sid
                       (str (random-uuid))

                       tid
                       (str (random-uuid))

                       registry
                       (atom {sid {:next-seq 40 :turns {} :turn-order []}})]

                   (with-redefs-fn {#'state/registry registry #'lp/db-info (constantly nil)}
                     (fn []
                       (try
                         (state/ingest-mirrored-event!
                           sid
                           true
                           {"type" "turn.started" "seq" 1 "turn_id" tid "request" "mirror request"})
                         (state/ingest-mirrored-event! sid
                                                       true
                                                       {"type" "turn.completed"
                                                        "seq" 2
                                                        "turn_id" tid
                                                        "content" [{"type" "prose"
                                                                    "markdown" "mirror answer"}]})
                         (expect (not (contains? (get-in @registry [sid :turns tid]) :request)))
                         (expect (not (contains? (get-in @registry [sid :turns tid]) :content)))
                         (expect (= "mirror request" (get (state/get-turn sid tid) "request")))
                         (expect (= "mirror answer" (state/turn-answer-text sid tid)))
                         (expect (= 42 (:next-seq (get @registry sid))))
                         (finally (#'state/drop-session! sid))))))))

(defdescribe
  terminal-archive-failure-does-not-pin-execution-test
  (it
    "terminal archive failure does not pin execution"
    (let [sid
          (str (random-uuid))

          tid
          (str (random-uuid))

          token
          (cancellation/cancellation-token)

          registry
          (atom {sid {:current-turn tid
                      :turns
                      {tid
                       {:turn_id tid :request "payload" :status "running" :cancel-token token}}}})]

      (with-redefs-fn {#'state/registry registry
                       #'lp/db-info (constantly nil)
                       #'turn-archive/write! (fn [_]
                                               (throw (java.io.IOException. "disk full")))}
        (fn []
          (try (#'state/install-turn-cancel-disposer!
                sid
                tid
                token
                (cancellation/on-cancel! token
                                         (fn [])))
               (#'state/finish-turn! sid tid {:status "completed"})
               (expect (= "completed" (get-in @registry [sid :turns tid :status])))
               (expect (nil? (get-in @registry [sid :current-turn])))
               (expect (not (contains? (get-in @registry [sid :turns tid]) :request)))
               (expect (empty? @(::cancellation/callbacks token)))
               (expect (= "Terminal turn history is unavailable"
                          (get (state/get-turn sid tid) "error")))
               (expect (= [{"type" "prose" "markdown" "preserved live answer"}]
                          (:content (#'state/turn-terminal-payload
                                     sid
                                     tid
                                     "completed"
                                     {:content [{"type" "prose"
                                                 "markdown" "preserved live answer"}]}))))
               (finally (#'state/drop-session! sid))))))))

(defdescribe
  terminal-archive-falls-back-to-one-canonical-turn-test
  (it
    "terminal archive falls back to one canonical turn"
    (doseq [header [{} {:position 42 :created-at (java.util.Date. 1234)}]]
      (let [sid (str (random-uuid))
            tid (str (random-uuid))
            registry (atom {sid {:turn-order [tid]
                                 :turns {tid {:turn_id tid
                                              :session_id sid
                                              :status "completed"
                                              ::state/archive-error true}}}})
            calls (atom [])]

        (with-redefs-fn {#'state/registry registry
                         #'lp/db-info (constantly nil)
                         #'persistence/db-read-session-turn
                         (fn [_ s t]
                           (swap! calls conj [s t])
                           (merge {:status :done
                                   :user-request "canonical request"
                                   :content [{"type" "prose" "markdown" "canonical answer"}]}
                                  header))}
          (fn []
            (let [turn (state/get-turn sid tid)]
              (expect (= "canonical request" (get turn "request")))
              (expect (= (:position header) (get turn "position")))
              (expect (= (some-> ^java.util.Date (:created-at header)
                                 .getTime)
                         (get turn "created_at"))))
            (expect (= [[sid tid]] @calls)
                    "Archive recovery and header hydration share one canonical row.")
            (with-redefs [persistence/db-read-session-turn (fn [& _]
                                                             (throw (java.io.IOException.)))]
              (expect (= "completed" (get (first (state/list-turns sid)) "status")))
              (expect (= "Terminal turn history is unavailable"
                         (get (first (state/list-turns sid)) "error"))))))))))

(defdescribe unavailable-terminal-archive-reads-canonical-turn-once-test
             (doseq [failure [:missing :running :exception]]
               (it (name failure)
                   (let [sid (str (random-uuid))
                         tid (str (random-uuid))
                         registry (atom {sid {:turns {tid {:turn_id tid
                                                           :session_id sid
                                                           :status "completed"
                                                           ::state/archive-error true}}}})
                         calls (atom [])]

                     (with-redefs-fn {#'state/registry registry
                                      #'lp/db-info (constantly nil)
                                      #'persistence/db-read-session-turn
                                      (fn [_ s t]
                                        (swap! calls conj [s t])
                                        (case failure
                                          :missing
                                          nil

                                          :running
                                          {:status :running
                                           :content [{"type" "prose" "markdown" "pending"}]}

                                          :exception
                                          (throw (java.io.IOException.))))}
                       (fn []
                         (expect (= "Terminal turn history is unavailable"
                                    (get (state/get-turn sid tid) "error")))
                         (expect (= [[sid tid]] @calls))))))))

(defdescribe
  stale-archive-write-does-not-replace-new-run-test
  (it
    "stale archive write does not replace new run"
    (let [sid
          (str (random-uuid))

          tid
          (str (random-uuid))

          old
          {:turn_id tid
           :status "running"
           :request "old"
           :cancel-token (cancellation/cancellation-token)}

          replacement
          (assoc old
            :request "new"
            :cancel-token (cancellation/cancellation-token))

          registry
          (atom {sid {:turns {tid old} :current-turn tid}})

          removed
          (atom [])]

      (with-redefs-fn {#'state/registry registry
                       #'lp/db-info (constantly nil)
                       #'turn-archive/write!
                       (fn [_]
                         (swap! registry assoc-in [sid :turns tid] replacement)
                         "unused-archive")
                       #'turn-archive/delete! #(swap! removed conj %)}
        (fn []
          (#'state/finish-turn! sid tid {:status "completed"})
          (expect (= replacement (get-in @registry [sid :turns tid])))
          (expect (= tid (get-in @registry [sid :current-turn])))
          (expect (= ["unused-archive"] @removed))
          (let [token (:cancel-token old)]
            (#'state/install-turn-cancel-disposer!
             sid
             tid
             token
             (cancellation/on-cancel! token
                                      (fn [])))
            (expect (empty? @(::cancellation/callbacks token)))
            (expect (= replacement (get-in @registry [sid :turns tid])))))))))

(defdescribe
  archive-completion-does-not-resurrect-forgotten-session-test
  (it
    "archive completion does not resurrect forgotten session"
    (let [sid
          (str (random-uuid))

          tid
          (str (random-uuid))

          token
          (cancellation/cancellation-token)

          registry
          (atom {sid {:turns {tid {:turn_id tid :status "running" :cancel-token token}}}})

          removed
          (atom [])]

      (with-redefs-fn {#'state/registry registry
                       #'lp/db-info (constantly nil)
                       #'turn-archive/write! (fn [_]
                                               (swap! registry dissoc sid)
                                               "unused-archive")
                       #'turn-archive/delete! #(swap! removed conj %)}
        (fn []
          (#'state/finish-turn! sid tid {:status "completed"})
          (expect (not (contains? @registry sid)))
          (#'state/install-turn-cancel-disposer!
           sid
           tid
           token
           (cancellation/on-cancel! token
                                    (fn [])))
          (expect (not (contains? @registry sid)))
          (expect (empty? @(::cancellation/callbacks token)))
          (expect (= ["unused-archive"] @removed)))))))

(defdescribe
  replay-registry-retains-only-descriptors-test
  (it "replay registry retains only descriptors"
      (let [registry
            (atom {})

            payload
            (apply str (repeat 10000 "large event payload "))

            sessions
            (mapv (fn [_]
                    (str (random-uuid)))
                  (range 12))]

        (with-redefs-fn {#'state/registry registry
                         #'lp/db-info (constantly nil)
                         #'bus/publish! (fn [& _])}
          (fn []
            (try (doseq [sid sessions]
                   (state/append-event! sid "block.output" {:turn_id "turn" :output payload}))
                 (expect (every? #(not (contains? % "output")) (mapcat :events (vals @registry))))
                 (doseq [sid sessions]
                   (expect (= payload (get (first (state/events-since sid 0)) "output"))))
                 (finally (doseq [sid sessions]
                            (#'state/drop-session! sid)))))))))
