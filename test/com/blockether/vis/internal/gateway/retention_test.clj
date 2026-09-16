(ns com.blockether.vis.internal.gateway.retention-test
  (:require [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.gateway.bus :as bus]
            [com.blockether.vis.internal.gateway.turn-archive :as turn-archive]
            [com.blockether.vis.internal.persistance.core :as persistence]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.session.cancellation :as cancellation]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]))

(deftest terminal-turn-releases-execution-payload-test
  (doseq [status ["completed" "failed" "cancelled" "suspended"]]
    (testing status
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
                   (is (not-any? #(contains? stored %)
                                 [:request :messages :attachments :engine-opts :extra-body
                                  :workspace :cancel-token :content]))
                   (is (= status (:status stored)))
                   (is (nil? (get-in @registry [sid :current-turn])))
                   (is (= payload (get (state/get-turn sid tid) "request")))
                   (is (= payload (get-in (state/get-turn sid tid) ["content" 0 "text"])))
                   (is (= "once" (get (state/get-turn sid tid) "idempotency_key"))))
                 (finally (#'state/drop-session! sid)))))))))

(deftest terminal-disposes-cancellation-registration-test
  (doseq [finish-first? [false true]]
    (let [sid (str (random-uuid))
          tid (str (random-uuid))
          token (cancellation/cancellation-token)
          registry (atom {sid {:turns {tid {:turn_id tid :status "running" :cancel-token token}}}})
          called (atom false)]

      (with-redefs-fn {#'state/registry registry #'lp/db-info (constantly nil)}
        (fn []
          (try (let [dispose (cancellation/on-cancel! token #(reset! called true))
                     finish! #(#'state/finish-turn! sid tid {:status "completed"})
                     install! #(#'state/install-turn-cancel-disposer! sid tid token dispose)]

                 (is (#'state/claim-turn-terminal! sid tid token))
                 (if finish-first? (do (finish!) (install!)) (do (install!) (finish!)))
                 (is (empty? @(::cancellation/callbacks token)))
                 (is (not (#'state/claim-turn-terminal! sid tid token)))
                 (cancellation/cancel! token :test)
                 (is (false? @called)))
               (finally (#'state/drop-session! sid)
                        (#'state/release-turn-terminal-claim! sid tid))))))))

(deftest mirrored-terminal-releases-payload-test
  (let [sid
        (str (random-uuid))

        tid
        (str (random-uuid))

        registry
        (atom {sid {:next-seq 40 :turns {} :turn-order []}})]

    (with-redefs-fn {#'state/registry registry #'lp/db-info (constantly nil)}
      (fn []
        (try (state/ingest-mirrored-event!
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
             (is (not (contains? (get-in @registry [sid :turns tid]) :request)))
             (is (not (contains? (get-in @registry [sid :turns tid]) :content)))
             (is (= "mirror request" (get (state/get-turn sid tid) "request")))
             (is (= "mirror answer" (state/turn-answer-text sid tid)))
             (is (= 42 (:next-seq (get @registry sid))))
             (finally (#'state/drop-session! sid)))))))

(deftest terminal-archive-failure-does-not-pin-execution-test
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
             (is (= "completed" (get-in @registry [sid :turns tid :status])))
             (is (nil? (get-in @registry [sid :current-turn])))
             (is (not (contains? (get-in @registry [sid :turns tid]) :request)))
             (is (empty? @(::cancellation/callbacks token)))
             (is (= "Terminal turn history is unavailable" (get (state/get-turn sid tid) "error")))
             (is (= [{"type" "prose" "markdown" "preserved live answer"}]
                    (:content (#'state/turn-terminal-payload
                               sid
                               tid
                               "completed"
                               {:content [{"type" "prose" "markdown" "preserved live answer"}]}))))
             (finally (#'state/drop-session! sid)))))))

(deftest terminal-archive-falls-back-to-one-canonical-turn-test
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
            (is (= "canonical request" (get turn "request")))
            (is (= (:position header) (get turn "position")))
            (is (= (some-> ^java.util.Date (:created-at header)
                           .getTime)
                   (get turn "created_at"))))
          (is (= [[sid tid]] @calls)
              "Archive recovery and header hydration share one canonical row.")
          (with-redefs [persistence/db-read-session-turn (fn [& _]
                                                           (throw (java.io.IOException.)))]
            (is (= "completed" (get (first (state/list-turns sid)) "status")))
            (is (= "Terminal turn history is unavailable"
                   (get (first (state/list-turns sid)) "error")))))))))

(deftest unavailable-terminal-archive-reads-canonical-turn-once-test
  (doseq [failure [:missing :running :exception]]
    (testing (name failure)
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
                             {:status :running :content [{"type" "prose" "markdown" "pending"}]}

                             :exception
                             (throw (java.io.IOException.))))}
          (fn []
            (is (= "Terminal turn history is unavailable" (get (state/get-turn sid tid) "error")))
            (is (= [[sid tid]] @calls))))))))

(deftest stale-archive-write-does-not-replace-new-run-test
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
                     #'turn-archive/write! (fn [_]
                                             (swap! registry assoc-in [sid :turns tid] replacement)
                                             "unused-archive")
                     #'turn-archive/delete! #(swap! removed conj %)}
      (fn []
        (#'state/finish-turn! sid tid {:status "completed"})
        (is (= replacement (get-in @registry [sid :turns tid])))
        (is (= tid (get-in @registry [sid :current-turn])))
        (is (= ["unused-archive"] @removed))
        (let [token (:cancel-token old)]
          (#'state/install-turn-cancel-disposer!
           sid
           tid
           token
           (cancellation/on-cancel! token
                                    (fn [])))
          (is (empty? @(::cancellation/callbacks token)))
          (is (= replacement (get-in @registry [sid :turns tid]))))))))

(deftest archive-completion-does-not-resurrect-forgotten-session-test
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
        (is (not (contains? @registry sid)))
        (#'state/install-turn-cancel-disposer!
         sid
         tid
         token
         (cancellation/on-cancel! token
                                  (fn [])))
        (is (not (contains? @registry sid)))
        (is (empty? @(::cancellation/callbacks token)))
        (is (= ["unused-archive"] @removed))))))

(deftest replay-registry-retains-only-descriptors-test
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
             (is (every? #(not (contains? % "output")) (mapcat :events (vals @registry))))
             (doseq [sid sessions]
               (is (= payload (get (first (state/events-since sid 0)) "output"))))
             (finally (doseq [sid sessions]
                        (#'state/drop-session! sid))))))))
