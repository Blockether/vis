(ns com.blockether.vis.internal.gateway.turn-metadata-test
  (:require [com.blockether.vis.internal.gateway.bus :as bus]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.gateway.turn-archive :as archive]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.persistance.core :as persistance]
            [com.blockether.vis.internal.session.model :as smodel]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(deftest turn-headers-use-durable-metadata-after-allocation
  (let [sid
        (str (random-uuid))

        tid
        (str (random-uuid))

        registry
        @#'state/registry

        row
        (atom nil)

        reads
        (atom 0)]

    (try
      (swap! registry assoc
        sid
        {:next-seq 0
         :current-turn tid
         :turn-order [tid]
         :turns
         {tid
          {:turn_id tid :session_id sid :status "running" :request "Inspect" :started_at 1000}}})
      (with-redefs [lp/db-info
                    (constantly ::db)

                    lp/by-id
                    (constantly {:id sid :channel :tui})

                    persistance/db-read-session-turn
                    (fn [_ s t]
                      (is (= [sid tid] [s t]))
                      (swap! reads inc)
                      @row)

                    persistance/db-list-session-turns
                    (fn [_ _]
                      (if-let [turn @row]
                        [turn]
                        []))

                    persistance/db-list-turns-attachments
                    (constantly {})

                    persistance/db-session-turn-stats
                    (constantly {:turn-count 41})

                    bus/publish!
                    (fn [& _])

                    bus/live-turn-id
                    (constantly nil)

                    bus/session-waiting?
                    (constantly false)

                    smodel/pending-pref
                    (constantly [false nil])]

        ;; The engine allocates its row after turn.started. Never invent T1 here.
        (let [started (state/append-event! sid "turn.started" {:turn_id tid})]
          (is (not (contains? started "position"))))
        (reset! row {:id tid :position 42 :created-at (java.util.Date. 1234) :status :running})
        (let [progress
              (state/append-event! sid "block.started" {:turn_id tid :iteration 7 :form_index 3})

              terminal
              (state/append-event! sid "turn.completed" {:turn_id tid})

              turn
              (state/get-turn sid tid)

              soul
              (state/soul sid)]

          (is (= {"position" 42 "created_at" 1234 "iteration" 7 "form_index" 3}
                 (select-keys progress ["position" "created_at" "iteration" "form_index"])))
          (is (= {"position" 42 "created_at" 1234}
                 (select-keys terminal ["position" "created_at"])))
          (is (= {"position" 42 "created_at" 1234} (select-keys turn ["position" "created_at"])))
          (is (= {"position" 42 "created_at" 1234}
                 (select-keys (first (state/list-turns sid)) ["position" "created_at"])))
          (is (= {"running_position" 42 "running_created_at" 1234 "running_started_at" 1000}
                 (select-keys soul ["running_position" "running_created_at" "running_started_at"])))
          (is (= [[42 1234] [42 1234]]
                 (mapv (juxt #(get % "position") #(get % "created_at"))
                       (rest (state/events-since sid 0)))))
          (is (= 2 @reads) "Known metadata is cached, not queried for every streamed event.")
          ;; Reopening a running session need not wait for another stream event.
          (swap! registry update-in [sid :turns tid] dissoc :position :created_at)
          (is (= {"running_position" 42 "running_created_at" 1234}
                 (select-keys (state/soul sid) ["running_position" "running_created_at"])))
          (is (= 3 @reads))))
      (finally (swap! registry dissoc sid)))))

(deftest persisted-turn-list-retains-the-canonical-position
  (let [turn (#'state/persisted-turn->wire
              "session"
              {:id "turn" :position 42 :created-at (java.util.Date. 1234) :status :success})]
    (is (= 42 (:position turn)))
    (is (= 1234 (:created_at turn)))))

(deftest metadata-lookup-does-not-resurrect-a-forgotten-session
  (let [sid
        (str (random-uuid))

        tid
        (str (random-uuid))

        registry
        @#'state/registry]

    (try (swap! registry assoc sid {:turns {tid {:turn_id tid :session_id sid}}})
         (with-redefs [lp/db-info
                       (constantly ::db)

                       persistance/db-read-session-turn
                       (fn [& _]
                         (swap! registry dissoc sid)
                         {:position 42 :created-at (java.util.Date. 1234)})]

           (is (= 42 (get (state/get-turn sid tid) "position")))
           (is (not (contains? @registry sid))))
         (finally (swap! registry dissoc sid)))))

(deftest terminal-before-progress-keeps-canonical-header-through-archive
  (let [sid
        (str (random-uuid))

        tid
        (str (random-uuid))

        registry
        @#'state/registry

        stored
        (atom nil)]

    (try (swap! registry assoc
           sid
           {:next-seq 0
            :current-turn tid
            :turns {tid {:turn_id tid :session_id sid :status "running"}}})
         (with-redefs [lp/db-info
                       (constantly ::db)

                       persistance/db-read-session-turn
                       (constantly {:position 42 :created-at (java.util.Date. 1234)})

                       archive/write!
                       (fn [turn]
                         (reset! stored turn)
                         "test-archive")

                       archive/read-turn
                       (fn [_]
                         @stored)

                       archive/delete!
                       (fn [_])

                       bus/publish!
                       (fn [& _])]

           ;; No progress event ran before the terminal archived the engine result.
           (#'state/archive-terminal-turn! sid tid {:status "completed"})
           (let [event
                 (state/append-event! sid "turn.completed" {:turn_id tid})

                 turn
                 (state/get-turn sid tid)]

             (is (= {"position" 42 "created_at" 1234}
                    (select-keys event ["position" "created_at"])))
             (is (= {"position" 42 "created_at" 1234} (select-keys turn ["position" "created_at"])))
             (is (= {:position 42 :created_at 1234}
                    (select-keys (get-in @registry [sid :turns tid]) [:position :created_at])))))
         (finally (swap! registry dissoc sid)))))
