(ns com.blockether.vis.internal.gateway.agents-test
  (:require [charred.api :as json]
            [clojure.string :as str]
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
            [com.blockether.vis.internal.sandbox.jail :as jail]
            [com.blockether.vis.internal.sandbox.policy :as policy]
            [com.blockether.vis.test-temp :as test-temp]
            [lazytest.core :refer [defdescribe expect it]]
            [next.jdbc :as jdbc]
            [reitit.core :as r])
  (:import [java.io ByteArrayInputStream]))

(wiring/install!)

(h/use-mem-store! {"subagents" true})

(defn- await-count
  "Wait up to 3 s until `events` holds `n` items. The push thread sends team events."
  [events n]
  (loop [left 300]
    (when (and (< (count @events) (long n)) (pos? left)) (Thread/sleep 10) (recur (dec left)))))

(defn- drain!
  "Wait until the push thread ran each job that is queued now and each job that those jobs queue."
  []
  (dotimes [_ 3]
    (let [done (promise)]
      (#'state/run-team! #(deliver done true))
      (deref done 3000 nil))))

(defn- push-statements
  "Run `f` and answer the SQL statements that the push thread ran meanwhile."
  [f]
  (let [n
        (atom 0)

        count!
        (fn [run]
          (fn [& args]
            (when (str/starts-with? (.getName (Thread/currentThread)) "vis-team-push")
              (swap! n inc))
            (apply run args)))]

    (with-redefs [jdbc/execute!
                  (count! jdbc/execute!)

                  jdbc/execute-one!
                  (count! jdbc/execute-one!)]

      (f)
      (drain!))
    @n))

(defn- with-team
  "Run `f` with a leader and one child for each status of `statuses`, in a store and a journal
   directory of this test. `f` gets `{:db :leader :children :events}`. `events` collects each
   `session.team_updated` as `[sid event]`. With `watched?`, a client stream watches the leader.
   Steps are not rate limited."
  [{:keys [statuses watched?]} f]
  (let [db
        (h/store)

        leader
        (str (h/store-session! db {:channel :api}))

        turn
        (ps/db-store-session-turn! db {:parent-session-id leader :user-request "Parent"})

        children
        (mapv (fn [status]
                (str (h/fork-session-at-turn! db
                                              leader
                                              {:through-turn-id turn
                                               :agent {:parent_id leader
                                                       :leader_id leader
                                                       :team_id (str turn)
                                                       :task "Check evidence"
                                                       :status status
                                                       :iteration_budget 4
                                                       :spawn_key (str (random-uuid))
                                                       :spawn_fingerprint "f"
                                                       :checkpoint [{:role :user
                                                                     :content "Context"}]}})))
              statuses)

        events
        (atom [])

        registry
        @#'state/registry

        saved
        @registry

        journal
        (.toPath (java.io.File. (test-temp/temp-dir "team-push") "events"))]

    (with-redefs [lp/db-info
                  (constantly db)

                  bus/publish!
                  (fn [& _])

                  bus/note-unjournaled!
                  (fn [& _])

                  bus/events-dir
                  (constantly journal)

                  state/team-step-ms
                  0]

      (try (reset! @#'bus/watch-cache {})
           (state/add-event-tap! ::spy
                                 (fn [sid event]
                                   (when (= "session.team_updated" (get event "type"))
                                     (swap! events conj [sid event]))))
           (when watched?
             (swap! registry assoc-in
               [leader :subscribers "client"]
               (fn [_])))
           (f {:db db :leader leader :children children :events events})
           (finally (state/remove-event-tap! ::spy)
                    (state/stop-team-pusher!)
                    (reset! registry saved)
                    (reset! @#'bus/watch-cache {}))))))

(defn- last-row
  "The row of `child` in the last team event, or nil."
  [events child]
  (some #(when (= child (get % "session_id")) %) (get (second (last @events)) "agents")))

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
              [["/v1/sessions/:sid/agents" [:post]] ["/v1/sessions/:sid/agents/answer" [:post]]
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
      ;; The subscription.ready frame carries the team, so the contract has no team read.
      (expect (nil? (contract/operation :get "/v1/sessions/:sid/agents")))
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
                        (expect false "A refused request must not bootstrap a session"))]

          (doseq [body ["[]" "not json" "{}" (json/write-json-str {:task " "})
                        "{\"task\":\"Work\",\"iteration_budget\":0}"]]
            (expect (= 400 (:status (request :post path body))) body))
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
             ;; A client watches the leader, so the gateway builds and sends rows.
             (swap! registry assoc-in
               [leader :subscribers "client"]
               (fn [_]))
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
          (mark q0 "lead" "c1" 0 false :full)

          q2
          (take q1 "lead" 0)

          [q3 a3]
          (mark q2 "lead" "c1" 200 false :activity)

          [q4 a4]
          (mark q3 "lead" "c1" 400 false :activity)

          [q5 a5]
          (due q4 "lead" "c1")]

      (expect (= :push a1))
      (expect (= {"c1" :full} (get-in q1 [:pending "lead"])))
      (expect (= [:timer 1800] a3) "A step inside two seconds waits for the rest of them")
      (expect (nil? a4) "A second step in the same wait adds no work")
      (expect (= q3 q4))
      (expect (= :push a5) "The timer sends the last step")
      (expect (= {"c1" :activity} (get-in q5 [:pending "lead"])))
      (let [[q a] (mark q3 "lead" "c1" 300 true :activity)]
        (expect (= :push a) "A lifecycle or request change goes at once")
        (expect (= {"c1" :full} (get-in q [:pending "lead"])) "An urgent change rebuilds the row")
        (expect (empty? (:timers q)))
        (expect (= [q nil] (due q "lead" "c1"))
                "The timer that the urgent push replaced sends nothing"))
      (expect (= [(update q2 :pending assoc "lead" {"c1" :activity}) :push]
                 (mark q2 "lead" "c1" 2000 false :activity))
              "A step two seconds after the last push goes at once")
      (expect (empty? (:sent (take q2 "lead" 5000))) "An old send time is dropped"))))

;; Most child steps are `block.activity`, which change only the Activity line of a row. Each
;; one rebuilt the whole row with 17 statements. Only a step that changes more rebuilds it.
(defdescribe
  team-change-kind-test
  (it "keeps the strongest change kind of a child until its push"
      (let [mark
            #'state/team-mark

            due
            #'state/team-due

            q0
            (first (mark @#'state/empty-team-queue "lead" "c1" 0 false :activity))]

        (expect (= {"c1" :activity} (get-in q0 [:pending "lead"])))
        (expect (= {"c1" :full}
                   (get-in (first (mark q0 "lead" "c1" 1 false :full)) [:pending "lead"]))
                "A full change wins over an Activity change")
        (expect (= {"c1" :full}
                   (-> q0
                       (mark "lead" "c1" 1 false :full)
                       first
                       (mark "lead" "c1" 2 false :activity)
                       first
                       (get-in [:pending "lead"])))
                "An Activity change does not weaken a pending one")
        (let [timed (-> @#'state/empty-team-queue
                        (assoc :sent {"c1" 0})
                        (mark "lead" "c1" 100 false :activity)
                        first
                        (mark "lead" "c1" 200 false :full)
                        first)]
          (expect (= {"c1" :full} (:timers timed)) "A timer keeps the strongest step of its wait")
          (expect (= {"c1" :full} (get-in (first (due timed "lead" "c1")) [:pending "lead"]))))))
  (it
    "reuses only a cached row for an Activity change and keeps rows of live children"
    (let [plan
          #'state/team-plan

          remember
          #'state/team-remember

          running
          {:session_id "c1" :status "running" :created_at 1}

          done
          {:session_id "c2" :status "completed" :created_at 2}

          cache
          (remember @#'state/empty-team-cache
                    "lead"
                    ["c1" "c2"]
                    [running done]
                    {:active 1 :total 2})]

      (expect (= {:rows {"c1" running} :teams {"lead" {:active 1 :total 2}}}
                 (select-keys cache [:rows :teams]))
              "A finished row and a team without active children are not kept")
      (expect (= {:full ["c2"] :reuse [running]} (plan cache {"c1" :activity "c2" :activity}))
              "An Activity change without a cached row rebuilds the row")
      (expect (= {:full ["c1"] :reuse []} (plan cache {"c1" :full})))
      (expect (= {:rows {} :teams {}}
                 (select-keys
                   (remember cache "lead" ["c1"] [(assoc running :status "completed")] {:active 0})
                   [:rows :teams]))
              "A child that ended leaves the cache")
      (expect (= {:rows {} :teams {}}
                 (select-keys (#'state/team-forget cache "lead" ["c1"]) [:rows :teams])))))
  (it "keeps each full change of the last 10 s for a new watcher"
      (let [note
            #'state/team-note

            cache
            (note @#'state/empty-team-cache "lead" {"c1" :full "c2" :activity} 1000)]

        (expect (= {"c1" ["lead" 1000]} (:recent cache)) "An Activity change is not an end")
        (expect (= [["lead" "c1"]] (#'state/team-recent cache #{"lead"} 5000)))
        (expect (empty? (#'state/team-recent cache #{"other"} 5000)) "Only the watched leader")
        (expect (= {"c3" ["lead" 12000]} (:recent (note cache "lead" {"c3" :full} 12000)))
                "A change older than 10 s goes"))))

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
             ;; A client watches the leader, so the gateway builds and sends rows.
             (swap! registry assoc-in
               [leader :subscribers "client"]
               (fn [_]))
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
             (swap! registry update
               leader
               merge
               {:turns {"lt" {:turn_id "lt"
                              :session_id leader
                              :status "running"
                              :cancel-token (cancellation/cancellation-token)}}
                :current-turn "lt"})
             ;; The two cancels can reach the push thread apart, so a client may get one event or
             ;; two. It merges the rows by session id, and so does this check (CI run 41449961621).
             (reset! events [])
             (state/cancel-turn! leader "lt")
             (let [merged (fn []
                            (into {}
                                  (for [[_ event] @events
                                        row (get event "agents")]

                                    [(get row "session_id") (get row "status")])))]
               (loop [left 300]
                 (when (and (not= {b "cancelled" c "cancelled"} (select-keys (merged) [b c]))
                            (pos? left))
                   (Thread/sleep 10)
                   (recur (dec left))))
               (expect (= {b "cancelled" c "cancelled"} (select-keys (merged) [b c]))
                       "The leader stop pushes each child that it cancelled")
               (doseq [[sid event] @events]
                 (expect (= leader sid))
                 (expect (document/valid-json? "gateway"
                                               "team_update"
                                               (select-keys event ["team" "agents" "child_id"])))
                 (expect (= (= 1 (count (get event "agents"))) (contains? event "child_id"))
                         "Only an event of one child names it"))
               (expect (= {"active" 0 "total" 3} (get (second (peek @events)) "team"))))
             (finally (state/remove-event-tap! ::spy) (reset! registry saved)))))))

;; With the client polls gone, these child changes reached no client: a model or routing-lock
;; change, an open or closed form for the person, a file grant and a plain budget extend.
(defdescribe
  team-trigger-test
  (it "pushes the child row when its model or routing lock changes"
      (with-team {:statuses ["running"] :watched? true}
                 (fn [{:keys [db children events]}]
                   (let [[child] children]
                     (smodel/set-model! db child "p" "small" :agent-routing)
                     (drain!)
                     (expect (= "small" (get (last-row events child) "model")))
                     (expect (false? (get (last-row events child) "routing_locked")))
                     (reset! events [])
                     (state/set-session-model! child "p" "small")
                     (drain!)
                     (expect (true? (get (last-row events child) "routing_locked"))
                             "A human pick of the same model locks routing")))))
  (it "pushes the child row when a form for the person opens and closes"
      (let [open (atom 0)]
        (with-redefs [bus/session-waiting? (fn [_]
                                             (pos? (long @open)))
                      bus/session-waiting-count (fn [_]
                                                  @open)]

          (with-team {:statuses ["running"] :watched? true}
                     (fn [{:keys [children events]}]
                       (let [[child] children]
                         (reset! open 1)
                         (state/append-event! child
                                              "view.open"
                                              {:kind "input" :view_id "req" :view {:id "req"}})
                         (drain!)
                         (expect (= 1 (get (last-row events child) "pending_input_count")))
                         (reset! open 0)
                         (state/append-event! child "view.close" {:kind "input" :view_id "req"})
                         (drain!)
                         (expect (false? (get (last-row events child) "pending_input")))))))))
  (it "pushes the child row after a file grant and a plain extend"
      (with-team {:statuses ["running"] :watched? true}
                 (fn [{:keys [db leader children events]}]
                   (let [[child] children]
                     (with-redefs [state/agent-files (fn [_ _ files]
                                                       files)
                                   jail/grant-jail! (fn [& _])
                                   policy/restrict (fn [& _]
                                                     {})]

                       (#'state/agent-grant
                        {:db-info db :session-id leader :security-policy {}}
                        {:session_id child :files {:read_only ["/tmp/team-grant"]}}))
                     (drain!)
                     (expect (= ["/tmp/team-grant"]
                                (get-in (last-row events child) ["files" "read_only"])))
                     (state/agents-operation! leader :extend {:session_id child :iterations 3})
                     (drain!)
                     (expect (= 7 (get (last-row events child) "iteration_budget"))))))))

;; Most child steps are `block.activity`. Each rebuilt the row with 17 statements, but it changes
;; only the Activity line, which is in memory.
(defdescribe
  team-activity-push-test
  (it "reuses the cached row for an Activity step and rebuilds it for an iteration"
      (with-team
        {:statuses ["running"] :watched? true}
        (fn [{:keys [children events]}]
          (let [[child]
                children

                activity!
                (fn [headline]
                  (state/append-event! child
                                       "block.activity"
                                       {:turn_id "t"
                                        :iteration 1
                                        :form_index 0
                                        :activity {:rows [{:state "running"
                                                           :presentation {:headline headline}}]}}
                                       {:store? false}))]

            (expect (pos? (long (push-statements
                                  #(state/append-event! child "turn.started" {:turn_id "t"})))))
            (expect (zero? (long (push-statements #(activity! "Read")))))
            (expect (= "Read" (get (last-row events child) "activity")))
            (expect (= {"active" 1 "total" 1} (get (second (last @events)) "team")))
            (expect (pos? (long (push-statements #(state/append-event! child
                                                                       "iteration.completed"
                                                                       {:turn_id "t"})))))
            (expect (= "Read" (get (last-row events child) "activity"))))))))

(defn- ready-frame
  "The `subscription.ready` frame that a new stream of session `sid` gets first."
  [sid]
  (with-open [out (java.io.ByteArrayOutputStream.)]
    (#'server/sse-ready! out sid 0 [] (state/soul sid))
    (->> (str/split-lines (.toString out "UTF-8"))
         (some #(when (str/starts-with? % "data:") (subs % 5)))
         json/read-json)))

;; A team pushed rows to nobody: without an open client, each child step still read the store.
;; The clients then read the team with GET /agents. The ready frame of each stream holds it now.
(defdescribe
  team-watcher-test
  (it "builds nothing without a watcher and gives the whole team in the ready frame of a stream"
      (with-team
        {:statuses ["running" "completed"]}
        (fn [{:keys [db leader children events]}]
          (let [[running] children]
            (state/append-event! running "turn.started" {:turn_id "t"})
            (drain!)
            (expect (zero? (long (push-statements #(state/append-event! running
                                                                        "iteration.completed"
                                                                        {:turn_id "t"})))))
            (expect (empty? @events))
            (state/subscribe! leader
                              "client-1"
                              (fn [_])
                              0)
            (let [frame (ready-frame leader)]
              (expect (document/valid-json? "gateway" "subscription_ready" frame))
              (expect (= (set children) (set (map #(get % "session_id") (get frame "agents"))))
                      "The frame has each child, the ended one too")
              (expect (= #{leader} (set (map #(get % "parent_id") (get frame "agents"))))))
            (expect (= (set children)
                       (set (map #(get % "session_id") (get (ready-frame running) "agents"))))
                    "A subagent stream gets its team")
            (expect (not (contains? (ready-frame (str (h/store-session! db {:channel :api})))
                                    "agents"))
                    "A session without a team has no rows")
            (expect (.exists (#'bus/watch-file leader)) "Other processes see the watcher")
            (state/unsubscribe! leader "client-1")
            (expect (not (.exists (#'bus/watch-file leader))))))))
  (it "counts the watch marker of another live process and deletes one of a dead process"
      (with-team
        {:statuses ["running"]}
        (fn [{:keys [leader children events]}]
          (let [[child]
                children

                dir
                (#'bus/watch-dir)

                marker!
                (fn [pid]
                  (let [f (.toFile (.resolve dir (str leader "." pid ".json")))]
                    (java.nio.file.Files/createDirectories
                      dir
                      (make-array java.nio.file.attribute.FileAttribute 0))
                    (spit f (json/write-json-str {"schema" 1 "session_id" leader "owner_pid" pid}))
                    (reset! @#'bus/watch-cache {})
                    f))

                dead
                (marker! 2147483646)]

            (state/append-event! child "turn.started" {:turn_id "t"})
            (drain!)
            (expect (empty? @events) "A marker of a dead process does not count")
            (expect (not (.exists dead)))
            (marker! (.pid (.get (.parent (java.lang.ProcessHandle/current)))))
            (state/append-event! child "turn.completed" {:turn_id "t"})
            (drain!)
            (expect (= [child]
                       (mapv #(get % "session_id") (get (second (last @events)) "agents"))))))))
  ;; Leader follow-up #9234: a push in the second before a scan saw a new watcher in another
  ;; process was skipped, so a child that then waited for the person kept a stale row there.
  (it
    "pushes the open rows once when a live process starts to watch the leader"
    (with-team
      {:statuses ["running"]}
      (fn [{:keys [leader children events]}]
        (let [[child]
              children

              dir
              (#'bus/watch-dir)]

          (reset! @#'bus/watch-seen #{})
          (state/append-event! child "turn.started" {:turn_id "t"})
          (swap! @#'state/registry assoc-in [child :current-turn] "t")
          (drain!)
          (#'bus/watch-tick!)
          (drain!)
          (expect (empty? @events) "Nobody watches the leader yet")
          (java.nio.file.Files/createDirectories dir
                                                 (make-array java.nio.file.attribute.FileAttribute
                                                             0))
          (spit (.toFile (.resolve dir
                                   (str leader
                                        "."
                                        (.pid (.get (.parent (java.lang.ProcessHandle/current))))
                                        ".json")))
                (json/write-json-str {"schema" 1
                                      "session_id" leader
                                      "owner_pid"
                                      (.pid (.get (.parent (java.lang.ProcessHandle/current))))}))
          (reset! @#'bus/watch-cache {})
          (#'bus/watch-tick!)
          (drain!)
          (expect (= [[child]]
                     (mapv #(mapv (fn [row]
                                    (get row "session_id"))
                                  (get (second %) "agents"))
                           @events))
                  "The new watcher gets the row without another change")
          (reset! @#'bus/watch-cache {})
          (#'bus/watch-tick!)
          (drain!)
          (expect (= 1 (count @events)) "A watcher that stays gets no second push")))))
  ;; Leader follow-up #9243: a child that ended in the second before a new watcher in another
  ;; process stayed running there for ever, because nothing changed it again.
  (it
    "pushes a child that ended just before a live process starts to watch, not an older one"
    (with-team
      {:statuses ["completed" "completed"]}
      (fn [{:keys [leader children events]}]
        (let [[fresh old]
              children

              dir
              (#'bus/watch-dir)

              pid
              (.pid (.get (.parent (java.lang.ProcessHandle/current))))]

          (reset! @#'bus/watch-seen #{})
          (doseq [child children]
            (state/append-event! child "turn.completed" {:turn_id "t"}))
          (drain!)
          (expect (empty? @events) "Nobody watches the leader when the children end")
          ;; The end of `old` is long before the watcher comes; the end of `fresh` is 2 s before.
          (swap! @#'state/team-cache
            (fn [cache]
              (-> cache
                  (assoc-in [:recent fresh] [leader (- (System/currentTimeMillis) 2000)])
                  (assoc-in [:recent old] [leader (- (System/currentTimeMillis) 11000)]))))
          (java.nio.file.Files/createDirectories dir
                                                 (make-array java.nio.file.attribute.FileAttribute
                                                             0))
          (spit (.toFile (.resolve dir (str leader "." pid ".json")))
                (json/write-json-str {"schema" 1 "session_id" leader "owner_pid" pid}))
          (reset! @#'bus/watch-cache {})
          (#'bus/watch-tick!)
          (drain!)
          (expect (= [[fresh]]
                     (mapv #(mapv (fn [row]
                                    (get row "session_id"))
                                  (get (second %) "agents"))
                           @events))
                  "Only the child that ended recently is pushed")
          (expect (= "completed" (get (last-row events fresh) "status"))))))))

;; Council run dependencies: a task with after starts only when its after tasks complete, once.
(defdescribe
  task-dependencies-test
  (it
    "starts a waiting task once when its after tasks complete, and blocks and re-arms it"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          turn
          (ps/db-store-session-turn! db {:parent-session-id leader :user-request "Parent"})

          spawn!
          (fn [status after]
            (str (h/fork-session-at-turn! db
                                          leader
                                          {:through-turn-id turn
                                           :agent {:parent_id leader
                                                   :leader_id leader
                                                   :team_id (str turn)
                                                   :task "Check evidence"
                                                   :status status
                                                   :after after
                                                   :iteration_budget 4
                                                   :spawn_key (str (random-uuid))
                                                   :spawn_fingerprint "f"
                                                   :checkpoint [{:role :user
                                                                 :content "Context"}]}})))

          a1
          (spawn! "running" nil)

          a2
          (spawn! "waiting" [1])

          a3
          (spawn! "waiting" [2])

          delivered
          (atom [])

          status
          #(:status (agents/info db %))

          settle!
          #(#'state/settle-dependents! db leader)

          post
          (fn [operation body]
            (json/read-json (:body (request :post
                                            (str "/v1/sessions/" leader "/agents/" operation)
                                            (json/write-json-str body)))))]

      (with-redefs-fn {#'lp/db-info (constantly db)
                       #'state/broadcast-team! (fn [& _])
                       #'state/deliver-task! (fn [_ _ child-id _]
                                               (swap! delivered conj child-id))}
        (fn []
          (expect (= ["a1" "a2" "a3"] (mapv #(:id (agents/info db %)) [a1 a2 a3])))
          (expect (= ["a2"] (:after (agents/info db a3))))
          (settle!)
          (expect (= ["running" "waiting" "waiting"] (mapv status [a1 a2 a3])))
          (expect (empty? @delivered) "A task does not start before its after task completes")
          (ps/db-agent-update! db a1 {:status "completed"})
          (settle!)
          (settle!)
          (expect (= [a2] @delivered) "The task starts once, also after a repeated settle")
          (expect (= ["queued" "waiting"] (mapv status [a2 a3])))
          (ps/db-agent-update! db a2 {:status "failed"})
          (settle!)
          (expect (= "blocked" (status a3)))
          (let [[record] (post "wait" {:session_ids ["a3"] :timeout_secs 1})]
            (expect (= [a3 "a3" "blocked" ["a2"] "dependency_failed"]
                       (mapv #(get-in record %)
                             [["session_id"] ["id"] ["status"] ["after"] ["error" "code"]]))))
          (expect (= [{"session_id" "a9" "status" "unknown"}]
                     (mapv #(select-keys % ["session_id" "status"])
                           (post "wait" {:session_ids ["a9"] :timeout_secs 1}))))
          ;; A follow-up of the failed task re-arms the blocked task.
          (ps/db-agent-update! db a2 {:status "queued"})
          (settle!)
          (expect (= "waiting" (status a3)))
          (ps/db-agent-update! db a2 {:status "completed"})
          (settle!)
          (expect (= [a2 a3] @delivered))
          (expect (= "queued" (status a3))))))))

(defdescribe
  task-id-control-test
  (it
    "takes a task id wherever a child id goes, and blocks the tasks after a cancelled task"
    (with-team
      {:statuses ["running"]}
      (fn [{:keys [db leader children]}]
        (let [[a1]
              children

              turn
              (:team_id (agents/info db a1))

              a2
              (str (h/fork-session-at-turn! db
                                            leader
                                            {:through-turn-id (parse-uuid turn)
                                             :agent {:parent_id leader
                                                     :leader_id leader
                                                     :team_id turn
                                                     :task "Verify"
                                                     :status "waiting"
                                                     :after [1]
                                                     :iteration_budget 4
                                                     :spawn_key (str (random-uuid))
                                                     :spawn_fingerprint "f"
                                                     :checkpoint [{:role :user
                                                                   :content "Context"}]}}))

              cancel
              (request :post
                       (str "/v1/sessions/" leader "/agents/cancel")
                       (json/write-json-str {:session_id "a1"}))]

          (expect (= 200 (:status cancel)) (pr-str cancel))
          (expect (= [[a1 "a1"]]
                     (mapv #(mapv % ["session_id" "id"]) (json/read-json (:body cancel)))))
          (expect (= ["cancelled" "blocked"] (mapv #(:status (agents/info db %)) [a1 a2])))
          (expect (= 400
                     (:status (request :post
                                       (str "/v1/sessions/" leader "/agents/cancel")
                                       (json/write-json-str {:session_id "a7"}))))
                  "An unknown task id names no child"))))))

;; User request 2026-11-07: a person stops subagents only through the leader. The stop of the
;; leader turn stops its children; a subagent session itself has no stop.
(defdescribe
  subagent-turn-cancel-test
  (it "refuses a turn cancel in a subagent session and keeps the child running"
      (with-team
        {:statuses ["running"]}
        (fn [{:keys [children]}]
          (let [[child]
                children

                token
                (cancellation/cancellation-token)

                registry
                @#'state/registry]

            (swap! registry assoc
              child
              {:current-turn "child-turn"
               :turns {"child-turn" {:status "running" :cancel-token token :idempotency_key "k"}}})
            (let [by-id
                  (request :post (str "/v1/sessions/" child "/turns/child-turn/cancel") nil)

                  current
                  (request :post
                           (str "/v1/sessions/" child "/cancel-current")
                           (json/write-json-str {:idempotency_key "k"}))]

              (expect (= [409 409] (mapv :status [by-id current])))
              (expect (= ["subagent-stop" "subagent-stop"]
                         (mapv #(get-in (json/read-json (:body %)) ["error" "type"])
                               [by-id current])))
              (expect (not (cancellation/cancelled? token)) "The child keeps working")
              (expect (= "running" (:status (agents/info (lp/db-info) child))))))))))

;; User report 2026-11-05: the team block of a turn came after the rest of the transcript. The
;; transcript had no team rows, so each client read them with a second request.
(defdescribe
  transcript-team-test
  (it
    "puts the team rows of a turn into that turn of each transcript read"
    (with-team
      {:statuses ["completed" "running"]}
      (fn [{:keys [db leader children]}]
        (let [status
              (zipmap children ["completed" "running"])

              ;; One spawn batch can share a millisecond. Spawn the children against their id
              ;; order, so that an order by id alone shows the wrong order.
              spawned
              (vec (sort #(compare %2 %1) children))

              _
              (doseq [[i sid] (map-indexed vector spawned)]
                (h/raw-query db
                             {:update :session_agent
                              ;; New task numbers above the stored ones: (parent_id, ordinal) is unique.
                              :set {:created_at 1 :ordinal (+ 10 i)}
                              :where [:= :session_id sid]}))

              quiet
              (str (ps/db-store-session-turn! db
                                              {:parent-session-id leader :user-request "No team"}))

              reads
              [(:turns (state/transcript-page leader {:limit 10 :iteration-limit 8}))
               (:turns (state/transcript-page leader {})) (state/transcript leader)]]

          (doseq [turns
                  reads

                  :let [by-id
                        (into {} (map (juxt #(get % "turn_id") identity)) turns)

                        [team-row]
                        (remove #(= quiet (get % "turn_id")) turns)]]

            (expect (= 2 (count turns)))
            (expect (= spawned (mapv #(get % "session_id") (get team-row "agents"))))
            (expect (= (mapv status spawned) (mapv #(get % "status") (get team-row "agents"))))
            (expect (not (contains? (get by-id quiet) "agents"))
                    "A turn without subagents has no agents key"))
          (expect (every? #(not (contains? % "agents")) (state/transcript (first children)))
                  "A child session shows no team in its own turns"))))))
