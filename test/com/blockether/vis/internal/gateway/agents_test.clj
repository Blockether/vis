(ns com.blockether.vis.internal.gateway.agents-test
  (:require [charred.api :as json]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.gateway :as contract]
            [com.blockether.vis.internal.gateway.bus :as bus]
            [com.blockether.vis.internal.gateway.server :as server]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.gateway.wiring :as wiring]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.loop.router :as loop-router]
            [com.blockether.vis.internal.session.agents :as agents]
            [com.blockether.vis.internal.session.cancellation :as cancellation]
            [com.blockether.vis.internal.session.model :as smodel]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [lazytest.core :refer [defdescribe expect it]]
            [reitit.core :as r])
  (:import [java.io ByteArrayInputStream]))

(wiring/install!)

(h/use-mem-store! {"subagents" true})

(defn- await-count
  "Wait up to 3 s until `events` holds `n` items. The push thread sends team events."
  [events n]
  (loop [left 300]
    (when (and (< (count @events) (long n)) (pos? left)) (Thread/sleep 10) (recur (dec left)))))

(defn- request
  [method path body]
  (let [match
        (r/match-by-path (#'server/router "test-token" []) path)

        handler
        (get-in match [:data method :handler])]

    (expect (some? handler))
    (handler {:request-method method
              :uri path
              :path-params (:path-params match)
              :body (when body (ByteArrayInputStream. (.getBytes ^String body "UTF-8")))})))

(defdescribe
  canonical-agent-routes-and-validation-test
  (it "canonical agent routes and validation"
      (doseq [[path methods]
              [["/v1/sessions/:sid/agents" [:get :post]] ["/v1/sessions/:sid/agents/answer" [:post]]
               ["/v1/sessions/:sid/agents/cancel" [:post]] ["/v1/sessions/:sid/agents/deny" [:post]]
               ["/v1/sessions/:sid/agents/extend" [:post]]
               ["/v1/sessions/:sid/agents/grant" [:post]] ["/v1/sessions/:sid/agents/route" [:post]]
               ["/v1/sessions/:sid/agents/run" [:post]] ["/v1/sessions/:sid/agents/wait" [:post]]
               ["/v1/sessions/:sid/agents/:child-id/timeline" [:get]]]

              method
              methods]

        (expect (some? (contract/operation method path))))
      ;; A follow-up task is a Council ping now, so the contract has no resume route.
      (expect (nil? (contract/operation :post "/v1/sessions/:sid/agents/resume")))
      (let [db
            (h/store)

            leader
            (str (h/store-session! db {:channel :api}))

            path
            (str "/v1/sessions/" leader "/agents")]

        (with-redefs [lp/db-info
                      (constantly db)

                      lp/env-for
                      (fn [_]
                        (expect false "Listing a team must not bootstrap a session"))]

          (expect (= [] (json/read-json (:body (request :get path nil)))))
          (doseq [body ["[]" "not json" "{}" (json/write-json-str {:task " "})
                        "{\"task\":\"Work\",\"iteration_budget\":0}"]]
            (expect (= 400 (:status (request :post path body))) body))
          (expect (= 404 (:status (request :get "/v1/sessions/unknown/agents" nil))))
          (expect (= 404 (:status (request :get (str path "/missing/timeline") nil)))
                  "A timeline needs a subagent of the session")))))

;; Regression for #405: every managed-child operation has a gateway route.
(defdescribe
  managed-child-control-routes-test
  (it
    "validates each control before it changes state and refuses a foreign child"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          other
          (str (h/store-session! db {:channel :api}))

          post
          (fn [operation body]
            (request :post (str "/v1/sessions/" leader "/agents/" operation) body))]

      (with-redefs [lp/db-info (constantly db)]
        (doseq [[operation body] [["run" "{}"] ["run" "{\"tasks\":[]}"]
                                  ["run" "{\"tasks\":[\"Work\"],\"timeout_secs\":0}"]
                                  ["wait" "{\"session_ids\":\"x\"}"]
                                  ["wait" "{\"timeout_secs\":1801}"]
                                  ["extend" "{\"session_id\":\"x\"}"] ["deny" "{}"]
                                  ["grant" "{\"session_id\":\"x\"}"] ["wait" "not json"]]]
          (expect (= 400 (:status (post operation body))) (pr-str [operation body])))
        (doseq [[operation body] [["extend" {:session_id other :iterations 5}]
                                  ["deny" {:session_id other :reason "No"}]
                                  ["grant" {:session_id other :files {:read_only ["/tmp/x"]}}]]]
          (let [response (post operation (json/write-json-str body))]
            (expect (= 409 (:status response)) operation)
            (expect (= "not-owner" (get-in (json/read-json (:body response)) ["error" "type"]))
                    operation)))
        (expect (= [] (json/read-json (:body (post "wait" "{\"timeout_secs\":1}")))))
        (expect (= [{"session_id" "nobody" "status" "unknown"}]
                   (mapv #(select-keys % ["session_id" "status"])
                         (json/read-json
                           (:body (post
                                    "wait"
                                    "{\"session_ids\":[\"nobody\"],\"timeout_secs\":1}"))))))))))

(defdescribe
  routing-is-local-and-respects-human-locks-through-http-test
  (it
    "routing is local and respects human locks through http"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          other
          (str (h/store-session! db {:channel :api}))

          path
          (str "/v1/sessions/" leader "/agents/route")

          router
          {:providers [{:id :p :models [{:name "small"} {:name "large"}] :root "large"}]}]

      (with-redefs [lp/db-info
                    (constantly db)

                    loop-router/get-router
                    (constantly router)

                    lp/env-for
                    (fn [_]
                      (expect false "Routing a team must not bootstrap a session"))]

        (expect (= 200 (:status (request :post path "{\"provider\":\"p\",\"model\":\"small\"}"))))
        (expect (= {:provider "p" :model "small"} (smodel/model-of db leader)))
        (expect (nil? (smodel/model-of db other)))
        (expect (= 409
                   (:status (request :post
                                     path
                                     (json/write-json-str
                                       {:provider "p" :model "large" :session_id other})))))
        (smodel/set-model! db leader "p" "small")
        (expect (ps/db-routing-locked? db leader) "An idempotent human pick locks agent routing")
        (expect (= 409
                   (:status (request :post path "{\"provider\":\"p\",\"model\":\"large\"}"))))))))

(defdescribe
  agent-activity-test
  ;; The replay ring holds event descriptors, not events. A test that put whole events
  ;; in the ring passed while the now column of every live team row stayed empty.
  (it
    "shows the latest Activity line of an agent, a running row first"
    (let [sid
          "agent-activity-child"

          tid
          "agent-activity-turn"

          registry
          @#'state/registry

          saved
          @registry

          row
          (fn [state headline summary]
            {:state state :presentation {:headline headline :summary summary}})

          activity!
          (fn [form-index rows store?]
            (state/append-event!
              sid
              "block.activity"
              {:turn_id tid :iteration 1 :form_index form-index :activity {:rows rows}}
              {:store? store?}))]

      (with-redefs [bus/publish! (fn [& _])]
        (try (reset! registry {sid {:next-seq 0 :current-turn tid :turns {tid {:turn_id tid}}}})
             (expect (nil? (#'state/agent-activity sid)))
             (activity! 0 [(row "succeeded" "Read" "~/a.clj")] true)
             (state/append-event! sid
                                  "block.output"
                                  {:turn_id tid :iteration 1 :form_index 0 :stdout "ok"})
             (expect (= "Read ~/a.clj" (#'state/agent-activity sid)))
             (activity! 1
                        [(row "running" "Run tests" "agents-test")
                         (row "succeeded" "Patched" "~/b.clj")]
                        false)
             (expect (= "Run tests agents-test" (#'state/agent-activity sid)))
             (activity! 1 [(row "succeeded" "Patched" "~/b.clj")] false)
             (expect (= "Patched ~/b.clj" (#'state/agent-activity sid)))
             (finally (reset! registry saved)))))))

;; Regression: the Companion app showed a running team only after its 3 s team poll, and
;; after a session switch only after a new read. The session row and a live event now
;; carry the team summary, so each client knows at once whether subagents work.
(defdescribe
  team-summary-test
  (it
    "puts the team summary in the leader row and sends it live when a child changes"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          turn
          (ps/db-store-session-turn! db {:parent-session-id leader :user-request "Parent"})

          child
          (str (h/fork-session-at-turn! db
                                        leader
                                        {:through-turn-id turn
                                         :agent {:parent_id leader
                                                 :leader_id leader
                                                 :team_id (str turn)
                                                 :task "Check evidence"
                                                 :iteration_budget 4
                                                 :spawn_key "k"
                                                 :spawn_fingerprint "f"
                                                 :checkpoint [{:role :user :content "Context"}]}}))

          events
          (atom [])

          registry
          @#'state/registry

          saved
          @registry]

      (with-redefs [lp/db-info
                    (constantly db)

                    bus/publish!
                    (fn [& _])

                    bus/note-unjournaled!
                    (fn [& _])]

        (try (expect (= {"active" 1 "total" 1} (get (state/soul leader) "team")))
             (expect (= {"active" 0 "total" 0} (get (state/soul child) "team"))
                     "A subagent leads no team")
             (state/add-event-tap! ::spy
                                   (fn [sid event]
                                     (when (= "session.team_updated" (get event "type"))
                                       (swap! events conj [sid event]))))
             (ps/db-agent-update! db child {:status "completed"})
             (state/append-event! child "turn.completed" {:turn_id "child-turn"})
             (await-count events 1)
             (let [[sid event] (last @events)]
               (expect (= leader sid) "The leader stream gets the event")
               (expect (= {"active" 0 "total" 1} (get event "team")))
               (expect (= child (get event "child_id"))))
             (expect (= {"active" 0 "total" 1} (get (state/soul leader) "team")))
             (finally (state/remove-event-tap! ::spy) (reset! registry saved)))))))

;; The TUI polled the team every 5 s and the Companion every 3 s, and each poll built every row
;; again. A step pushes the row instead, at most once every two seconds for each child, and the last
;; step of a burst is always sent.
(defdescribe
  team-step-rate-test
  (it
    "sends the first step at once, holds the next ones in one timer and lets a lifecycle pass"
    (let [mark
          #'state/team-mark

          due
          #'state/team-due

          take
          #'state/team-take

          q0
          @#'state/empty-team-queue

          [q1 a1]
          (mark q0 "lead" "c1" 0 false)

          q2
          (take q1 "lead" 0)

          [q3 a3]
          (mark q2 "lead" "c1" 200 false)

          [q4 a4]
          (mark q3 "lead" "c1" 400 false)

          [q5 a5]
          (due q4 "lead" "c1")]

      (expect (= :push a1))
      (expect (= #{"c1"} (get-in q1 [:pending "lead"])))
      (expect (= [:timer 1800] a3) "A step inside two seconds waits for the rest of them")
      (expect (nil? a4) "A second step in the same wait adds no work")
      (expect (= q3 q4))
      (expect (= :push a5) "The timer sends the last step")
      (expect (= #{"c1"} (get-in q5 [:pending "lead"])))
      (let [[q a] (mark q3 "lead" "c1" 300 true)]
        (expect (= :push a) "A lifecycle or request change goes at once")
        (expect (empty? (:timers q)))
        (expect (= [q nil] (due q "lead" "c1"))
                "The timer that the urgent push replaced sends nothing"))
      (expect (= [(update q2 :pending assoc "lead" #{"c1"}) :push] (mark q2 "lead" "c1" 2000 false))
              "A step two seconds after the last push goes at once")
      (expect (empty? (:sent (take q2 "lead" 5000))) "An old send time is dropped"))))

;; The clients rendered the team from `GET /agents` reads. Each change now pushes the changed rows
;; in `session.team_updated`, so the payload must be a valid `team_update` for each trigger.
(defdescribe
  team-push-test
  (it
    "pushes the changed rows for a start, a step, a request and both cancel paths"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          turn
          (ps/db-store-session-turn! db {:parent-session-id leader :user-request "Parent"})

          child!
          (fn []
            (str (h/fork-session-at-turn! db
                                          leader
                                          {:through-turn-id turn
                                           :agent {:parent_id leader
                                                   :leader_id leader
                                                   :team_id (str turn)
                                                   :task "Check evidence"
                                                   :status "running"
                                                   :iteration_budget 4
                                                   :spawn_key (str (random-uuid))
                                                   :spawn_fingerprint "f"
                                                   :checkpoint [{:role :user
                                                                 :content "Context"}]}})))

          [a b c]
          (repeatedly 3 child!)

          events
          (atom [])

          registry
          @#'state/registry

          saved
          @registry

          questions
          @#'agents/questions

          push!
          (fn [n f]
            (reset! events [])
            (f)
            (await-count events n)
            (expect (= n (count @events)) (pr-str @events))
            (doseq [[sid event] @events]
              (expect (= leader sid))
              (expect (document/valid-json? "gateway"
                                            "team_update"
                                            (select-keys event ["team" "agents" "child_id"]))
                      (pr-str event)))
            (mapv second @events))

          rows
          (fn [event]
            (mapv #(select-keys % ["session_id" "status" "question"]) (get event "agents")))]

      (with-redefs [lp/db-info
                    (constantly db)

                    bus/publish!
                    (fn [& _])

                    bus/note-unjournaled!
                    (fn [& _])]

        (try (state/add-event-tap! ::spy
                                   (fn [sid event]
                                     (when (= "session.team_updated" (get event "type"))
                                       (swap! events conj [sid event]))))
             (let [[event] (push! 1 #(state/append-event! a "turn.started" {:turn_id "a-turn"}))]
               (expect (= a (get event "child_id")))
               (expect (= [{"session_id" a "status" "running"}] (rows event))))
             ;; The start was the last push of `a`, so this step waits for its timer.
             (push! 1 #(state/append-event! a "iteration.completed" {:turn_id "a-turn"}))
             (let [[opened]
                   (push! 1 #(swap! questions assoc b {:question "May I?" :entry_id 7}))

                   [closed]
                   (push! 1 #(swap! questions dissoc b))]

               (expect (= [{"session_id" b "status" "running" "question" "May I?"}] (rows opened)))
               (expect (= [{"session_id" b "status" "running"}] (rows closed))
                       "A closed question leaves the row"))
             (let [[event] (push! 1 #(state/agents-operation! leader :cancel {:session_id a}))]
               (expect (= [{"session_id" a "status" "cancelled"}] (rows event)))
               (expect (= {"active" 2 "total" 3} (get event "team"))))
             (swap! registry assoc
               leader
               {:turns {"lt" {:turn_id "lt"
                              :session_id leader
                              :status "running"
                              :cancel-token (cancellation/cancellation-token)}}
                :current-turn "lt"})
             (let [[event] (push! 1 #(state/cancel-turn! leader "lt"))]
               (expect (= #{{"session_id" b "status" "cancelled"}
                            {"session_id" c "status" "cancelled"}}
                          (set (rows event)))
                       "The leader stop pushes each child that it cancelled")
               (expect (= {"active" 0 "total" 3} (get event "team")))
               (expect (not (contains? event "child_id"))))
             (finally (state/remove-event-tap! ::spy) (reset! registry saved)))))))
