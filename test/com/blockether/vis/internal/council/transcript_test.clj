(ns com.blockether.vis.internal.council.transcript-test
  "Council requests retain their explicit provenance across the loop, SQLite and gateway."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.context.loop :as ctx-loop]
            [com.blockether.vis.internal.council.core :as council]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.gateway.wiring :as wiring]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.loop.iteration :as iteration]
            [com.blockether.vis.internal.loop.turn :as turn]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [com.blockether.vis.internal.session.agents :as agents]
            [com.blockether.vis.internal.session.cancellation :as cancellation]
            [com.blockether.vis.internal.session.titling :as titling]
            [com.blockether.vis.internal.task :as task]
            [lazytest.core :refer [defdescribe expect it]]))

(wiring/install!)

(h/use-mem-store!)

(defn- fixture
  []
  (let [db
        (h/store)

        gid
        (str (:id (ps/db-create-project! db {:name "Council transcript"})))

        sid
        (str (h/store-session! db {:channel :api}))

        activation
        (str (random-uuid))]

    (ps/db-set-session-project! db sid gid)
    (letfn [(publish! [title content]
              (with-redefs [toggles/enabled? (constantly true)]
                (council/publish!
                  db
                  (constantly {sid {:activation-id activation :group-id gid :state "running"}})
                  {:session-id sid :activation-id activation :source "sdk"}
                  {:title title :content content})))]
      {:db db
       :sid sid
       :publish! publish!
       :entry
       (publish! "Review findings"
                 "Actual Council request.\nSecond line.\nThird line.\nFourth line.\nFifth line.")})))

(defdescribe
  council-request-provenance-survives-storage-and-fork
  (it
    "council request provenance survives storage and fork"
    (let [{:keys [db sid entry]}
          (fixture)

          prompt
          "Council notification. Read the attributed input."

          opts
          (#'turn/turn-store-opts
           {:session-id sid}
           prompt
           {:request-kind :council :council-entry-id (:entry_id entry)})

          tid
          (ps/db-store-session-turn! db opts)

          user-tid
          (ps/db-store-session-turn! db
                                     {:parent-session-id sid
                                      :user-request "Council wake — literal user text"})

          [turn user-turn]
          (ps/db-list-session-turns db sid)

          wire
          (#'state/persisted-turn->wire sid (delay (ps/db-agent-list db sid)) turn)

          transcript
          (with-redefs [lp/db-info (constantly db)]
            (state/transcript sid))

          fork
          (h/fork-session-at-turn! db sid {:through-turn-id user-tid})]

      (expect (= :council (:request-kind turn)))
      (expect (= prompt (:user-request turn))
              "Model instruction stays separate from the visible request")
      (expect (= (:entry_id entry) (get-in turn [:council :entry-id])))
      (expect (= (:content entry) (get-in turn [:council :content])))
      (expect (= (:content entry) (:request wire)))
      (expect (= "council" (get-in transcript [0 "request_kind"])))
      (expect (= (:content entry) (get-in transcript [0 "request"])))
      (expect (= :user (:request-kind user-turn)) "Never classify user text by its prefix")
      (expect (nil? (:council user-turn)))
      (expect (= [{:request_kind "council" :council_entry_id (:entry_id entry)}]
                 (h/raw-query db
                              {:select [:request_kind :council_entry_id]
                               :from :session_turn_soul
                               :where [:= :id (str tid)]})))
      (expect (some? fork))
      (expect (= (:council turn) (:council (first (ps/db-list-session-turns db fork))))))))

;; Regression: a turn that took several held Council entries kept only the first one.
(defdescribe
  council-batch-turn-keeps-every-entry
  (it "shows every entry of one Council turn in the transcript after storage"
      (let [{:keys [db sid entry publish!]} (fixture)
            later (publish! "Build" "Antaresquill passed")
            _ (ps/db-store-session-turn!
                db
                (#'turn/turn-store-opts {:session-id sid}
                                        "Council notifications #1, #2."
                                        {:request-kind :council
                                         :council-entry-id (:entry_id entry)
                                         :council-later-entry-ids [(:entry_id later)]}))
            turn (with-redefs [lp/db-info (constantly db)]
                   (first (state/transcript sid)))]
        (expect (= [(:entry_id entry) (:entry_id later)]
                   (mapv #(get % "entry_id") (get-in turn ["council" "entries"]))))
        (expect (= (str (:content entry) "\n\n" (:content later)) (get turn "request"))))))

;; Regression: completed Council wakes appeared as You after engine preparation
;; discarded their provenance. Exercise both engine phases before the real write.
(defdescribe
  council-provenance-survives-engine-phases
  (it
    "council provenance survives engine phases"
    (doseq [council? [false true]]
      (let [{:keys [db sid entry]} (fixture)
            tid (str (random-uuid))
            prompt
            (str "Council notification #" (:entry_id entry) ". Read the attributed Council input.")
            env {:db-info db
                 :session-id sid
                 :turn-state-atom (ctx-loop/make-turn-state-atom)
                 :router {:providers [{:id :openai-codex :models [{:name "shared"}]}]}}
            opts (cond-> {:model "shared" :session-turn-id tid}
                   council?
                   (assoc :request-kind
                     :council :council-entry-id
                     (:entry_id entry)))
            ctx (#'turn/prepare-turn-context env [{:role "user" :content prompt}] opts)
            phase (with-redefs [iteration/iteration-loop (fn [_ request _]
                                                           (expect (= prompt request))
                                                           {:status :success
                                                            :answer "Acknowledged."
                                                            :iteration-count 1
                                                            :duration-ms 0})
                                titling/maybe-auto-title! (fn [& _]
                                                            nil)
                                titling/after-turn-auto-title! (fn [& _]
                                                                 nil)]

                    (#'turn/run-iteration-phase ctx))
            [turn] (ps/db-list-session-turns db sid)
            [wire] (with-redefs [lp/db-info (constantly db)]
                     (state/transcript sid))]

        (expect (= tid (str (:session-turn-id phase)) (str (:id turn))))
        (expect (= prompt (:user-request turn)) "Keep the model instruction unchanged")
        (expect (= (if council? :council :user) (:request-kind turn)))
        (expect (= (if council? "council" "user") (get wire "request_kind")))
        (if council?
          (do (expect (= (:entry_id entry) (get-in wire ["council" "entry_id"])))
              (expect (= (:thread_id entry) (get-in wire ["council" "thread_id"])))
              (expect (= (:content entry) (get wire "request"))))
          (do (expect (nil? (:council turn))) (expect (= prompt (get wire "request")))))))))

(defn- managed-fixture
  []
  (let [{:keys [db sid] :as base}
        (fixture)

        turn
        (ps/db-store-session-turn! db {:parent-session-id sid :user-request "Delegate"})

        child
        (str (h/fork-session-at-turn! db
                                      sid
                                      {:through-turn-id turn
                                       :agent {:parent_id sid
                                               :leader_id sid
                                               :team_id (str turn)
                                               :depth 1
                                               :task "Check provenance"
                                               :iteration_budget 4
                                               :spawn_key "fixture"
                                               :spawn_fingerprint "fixture"
                                               :checkpoint [{:role :user :content "Delegate"}]}}))]

    (ps/db-set-session-project! db child (:project-id (ps/db-get-session db sid)))
    (assoc base :sid child)))

(defdescribe
  council-wake-keeps-display-content-out-of-the-short-instruction
  (it "council wake keeps peer content out of the short instruction"
      (let [{:keys [db sid entry]}
            (managed-fixture)

            submitted
            (atom nil)

            wake!
            state/council-wake!

            ;; The child itself is a peer author, not its parent.
            peer
            (assoc entry :author_session_id sid)]

        (doseq [required? [false true]]
          (with-redefs-fn {#'toggles/enabled? (constantly true)
                           #'state/council-wake-eligible? (constantly true)
                           #'state/submit-turn! (fn [_ opts]
                                                  (reset! submitted opts)
                                                  {:turn {}})}
            #(wake! db sid (assoc peer :reply_required required?)))
          (expect (= (:content entry) (:display-request @submitted)))
          (expect (= :council (get-in @submitted [:engine-opts :request-kind])))
          (expect (= (:entry_id entry) (get-in @submitted [:engine-opts :council-entry-id])))
          (expect (< (count (:request @submitted)) 360))
          (expect (str/includes? (:request @submitted) (str "council.get(" (:entry_id entry) ")")))
          (expect (= required? (str/includes? (:request @submitted) "before ending this turn")))
          (expect (not (str/includes? (:request @submitted) (:content entry))))))))

;; Regression for Council complaint #7930: a small model took its parent's task as a notification.
;; It replied and did no work, or did the work and ended with "Done." as its whole answer.
(defdescribe
  council-wake-gives-a-parent-task-as-the-instruction
  (it "gives the parent's entry as the child's task and asks for the whole result"
      (let [{:keys [db sid entry]}
            (managed-fixture)

            submitted
            (atom nil)]

        (doseq [required? [false true]]
          (with-redefs-fn {#'toggles/enabled? (constantly true)
                           #'state/council-wake-eligible? (constantly true)
                           #'state/submit-turn! (fn [_ opts]
                                                  (reset! submitted opts)
                                                  {:turn {}})}
            #(state/council-wake! db sid (assoc entry :reply_required required?)))
          (let [request (:request @submitted)]
            (expect (= (:content entry) (:display-request @submitted)))
            (expect (str/includes? request (str "Your parent session " (:author_session_id entry))))
            (expect (str/includes? request "It is your own task, not peer data."))
            (expect (str/includes? request (str "Task:\n" (:content entry))))
            (expect (str/includes? request "complete result as your final answer"))
            (expect (not (str/includes? request "Council notification")))
            (expect (= required?
                       (str/includes? request (str "reply_to=" (:entry_id entry) ")")))))))
      (let [{:keys [db sid entry]} (fixture)]
        (expect (nil? (agents/task-request db sid entry)) "An independent session gets no task"))))

;; A ping from its parent to a finished subagent is its follow-up task, the same store step for each.
(defdescribe
  council-wake-continues-a-finished-subagent-like-resume
  (it "queues the child with its turn count and restores it when the turn cannot start"
      (let [{:keys [db sid entry]}
            (managed-fixture)

            owner
            (:author_session_id entry)

            resource
            [:subagent sid]

            submit
            (atom (fn [_]
                    (let [attempt (task/resource-task owner resource)]
                      (expect (= [:subagent :running] ((juxt :kind :state) attempt)))
                      (expect (= sid (:child-session-id (task/guest-task owner (:id attempt))))))
                    {:turn {}}))]

        (agents/finish! db sid "completed")
        (with-redefs-fn {#'toggles/enabled? (constantly true)
                         #'state/council-wake-eligible? (constantly true)
                         #'state/submit-turn! (fn [_ opts]
                                                (@submit opts))}
          (fn []
            (state/council-wake! db sid entry)
            (expect (= "queued" (:status (agents/info db sid))))
            (expect (= (count (ps/db-list-session-turns db sid))
                       (:resumed_turns (agents/info db sid))))
            (expect (some? (:resumed_at (agents/info db sid))))
            (agents/finish! db sid "completed")
            (let [before (select-keys (agents/info db sid) [:status :resumed_turns :resumed_at])
                  prior-id (:id (task/resource-task owner resource))]
              (reset! submit (fn [_]
                               {:error :busy :message "No start"}))
              (expect (= :busy
                         (try (state/council-wake! db sid entry)
                              nil
                              (catch clojure.lang.ExceptionInfo e (:error (ex-data e))))))
              (expect (= before
                         (select-keys (agents/info db sid) [:status :resumed_turns :resumed_at]))
                      "A failed start leaves the child as it was")
              (let [attempt (task/resource-task owner resource)]
                (expect (= [prior-id :done] ((juxt :id :state) attempt)))
                (task/discard! owner prior-id))))))))

(defdescribe cancelled-child-reply-test
  (it "stores a reply without starting another turn for a cancelled child"
      (let [{:keys [db sid entry]} (managed-fixture)
            leader (:author_session_id entry)
            gid (:group_id entry)
            fleet (atom {leader {:activation-id "leader" :group-id gid :state "running"}
                         sid {:activation-id "child" :group-id gid :state "running"}})
            submitted (atom [])]
        (with-redefs [toggles/enabled? (constantly true)
                      state/council-wake-eligible? (constantly true)
                      state/submit-turn! (fn [_ opts] (swap! submitted conj opts) {:turn {}})]
          (with-redefs-fn {#'agents/leader-active? #(= leader %)}
            (fn []
              (let [question (council/publish! db #(deref fleet)
                                               {:session-id sid :activation-id "child" :source "sdk"}
                                               {:content "Can I continue?" :ping [leader] :reply_required true})]
                (agents/finish! db sid "cancelled")
                (swap! fleet dissoc sid)
                (let [reply (council/publish! db #(deref fleet)
                                               {:session-id leader :activation-id "leader" :source "sdk"}
                                               {:content "No." :reply_to (:entry_id question)})]
                  (expect (= (:entry_id question) (:reply_to (ps/db-council-get db (:entry_id reply)))))
                  (expect (empty? @submitted))
                  (expect (= "cancelled" (:status (agents/info db sid))))))))))))

(defn- with-child-question
  [check]
  (let [{:keys [db sid entry]} (managed-fixture)
        owner (:author_session_id entry)
        gid (:group_id entry)
        leader {:activation-id "leader" :group-id gid :state "running"}]
    (with-redefs [toggles/enabled? (constantly true)
                  state/council-wake-eligible? (constantly true)]
      (let [question (council/publish! db (constantly {owner leader
                                                      sid {:activation-id "child" :group-id gid :state "running"}})
                                       {:session-id sid :activation-id "child" :source "sdk"}
                                       {:content "Can I continue?" :ping [owner] :reply_required true})
            reply! (fn [opts]
                     (council/publish! db (constantly {owner leader})
                                       {:session-id owner :activation-id "leader" :source "sdk"}
                                       (assoc opts :content "Continue." :reply_to (:entry_id question))))]
        (try (check db sid owner (:entry_id question) reply!)
             (finally
               (doseq [{:keys [id]} (task/tasks owner)] (task/discard! owner id))))))))

(defdescribe completed-child-reply-test
  (it "starts a new task with the file grant when a reply reaches a completed child"
      (with-child-question
        (fn [db sid owner question-id reply!]
          (let [files {:write ["e.txt"]}
                submitted (atom [])
                runtime @#'agents/runtime
                saved @runtime]
            (agents/finish! db sid "completed")
            (with-redefs [state/submit-turn! (fn [id _]
                                              (swap! submitted conj [id (:files (agents/info db id))])
                                              {:turn {}})]
              (try
                (swap! runtime assoc
                       :live? (constantly false)
                       :grant (fn [_ opts] (ps/db-agent-update! db sid {:files (:files opts)})))
                (let [reply (agents/publish-with-files! {:db-info db :session-id owner}
                                                       {:files files :reply_to question-id}
                                                       reply!)]
                  (expect (some? (:reply_to reply)))
                  (expect (= [[sid files]] @submitted))
                  (expect (= "queued" (:status (agents/info db sid))))
                  (expect (= :running (:state (task/resource-task owner [:subagent sid])))))
                (finally (reset! runtime saved)))))))))

(defdescribe merged-child-wake-test
  (it "does not leave a new task when the notification joins an active turn"
      (let [{:keys [db sid entry]} (managed-fixture)
            owner (:author_session_id entry)]
        (agents/finish! db sid "completed")
        (with-redefs [toggles/enabled? (constantly true)
                      state/council-wake-eligible? (constantly true)
                      state/submit-turn! (constantly {:council-active? true})]
          (try
            (expect (:council-active? (state/council-wake! db sid entry)))
            (expect (= ["completed" []] [(:status (agents/info db sid)) (task/tasks owner)]))
            (finally (doseq [{:keys [id]} (task/tasks owner)] (task/discard! owner id))))))))

(defdescribe child-cancel-request-reply-test
  (it "does not wake the child while its Task cancel callback is still pending"
      (with-child-question
        (fn [db sid owner _ reply!]
          (let [release (promise)
                {:keys [row]} (task/start-manual! owner {:kind :subagent :name "Child" :resource [:subagent sid]}
                                (fn [_] (deref release 5000 nil)))
                submitted (atom [])]
            (ps/db-agent-update! db sid {:status "running"})
            (with-redefs [state/submit-turn! (fn [id _] (swap! submitted conj id) {:turn {}})]
              (try
                (expect (task/cancel! owner (:id row) :agent))
                (expect (= ["running" :agent] [(:status (agents/info db sid))
                                               (:cancel-reason (task/state owner (:id row)))]))
                (let [reply (reply! {})]
                  (expect (some? (:reply_to reply)))
                  (expect (empty? @submitted)))
                (finally (deliver release true)))))))))

;; council.resume is gone: council.publish refuses a follow-up ping that cannot start, before the
;; entry exists, so the parent sees the cause at once.
(defdescribe
  council-publish-refuses-a-follow-up-that-cannot-start
  (it
    "refuses a ping to an exhausted or stopped child and stores a ping that can start"
    (let [{:keys [db sid]}
          (managed-fixture)

          leader
          (str (:parent_id (agents/info db sid)))

          gid
          (str (:project-id (ps/db-get-session db leader)))

          activation
          (str (random-uuid))

          saved
          @@#'agents/runtime

          ping!
          (fn []
            (try (some? (:entry_id (council/publish!
                                     db
                                     (constantly {leader {:activation-id activation
                                                          :group-id gid
                                                          :state "running"}})
                                     {:session-id leader :activation-id activation :source "sdk"}
                                     {:content "Next task" :ping [sid]})))
                 (catch clojure.lang.ExceptionInfo e (:error (ex-data e)))))]

      (with-redefs-fn {#'toggles/enabled? (constantly true)
                       #'state/council-wake-eligible? (constantly false)}
        (fn []
          (try (swap! @#'agents/runtime dissoc :leader-active?)
               (dotimes [_ 4]
                 (ps/db-agent-claim-iteration! db sid))
               (ps/db-agent-update! db sid {:status "budget_limited"})
               (expect (= :budget-exceeded (ping!)) "An exhausted child needs council.extend")
               (expect (some? (ps/db-agent-extend-budget! db sid 2)))
               (expect (true? (ping!)) "An extended child takes the ping")
               (ps/db-agent-update! db sid {:status "cancelled"})
               (expect (= :invalid-recipient (ping!))
                       "A stopped child stays stopped without a running leader turn")
               (swap! @#'agents/runtime assoc :leader-active? #(= leader %))
               (expect (true? (ping!)) "A running leader turn continues a stopped child")
               (finally (reset! @#'agents/runtime saved))))))))

(defdescribe request-provenance-is-enforced-by-sqlite
             (it "request provenance is enforced by sqlite"
                 (let [{:keys [db sid entry]} (fixture)]
                   (doseq [invalid [{:request-kind :assistant} {:request-kind :council}
                                    {:request-kind :user :council-entry-id (:entry_id entry)}
                                    {:request-kind :council :council-entry-id 99999}]]
                     (expect (try (ps/db-store-session-turn! db
                                                             (merge {:parent-session-id sid
                                                                     :user-request "Request"}
                                                                    invalid))
                                  false
                                  (catch Exception _ true)))))))

(defdescribe council-transcript-survives-database-reopen
             (it "council transcript survives database reopen"
                 (let [dir
                       (.toFile (java.nio.file.Files/createTempDirectory
                                  "vis-council-transcript"
                                  (make-array java.nio.file.attribute.FileAttribute 0)))

                       db
                       (ps/db-create-connection! (.getPath dir))]

                   (try
                     (let [{:keys [sid entry]}
                           (binding [h/*store* db]
                             (fixture))

                           tid
                           (ps/db-store-session-turn! db
                                                      {:parent-session-id sid
                                                       :request-kind :council
                                                       :council-entry-id (:entry_id entry)
                                                       :user-request "Short model instruction"})]

                       (ps/db-dispose-connection! db)
                       (let [reopened (ps/db-create-connection! (.getPath dir))]
                         (try (let [turn (first (ps/db-list-session-turns reopened sid))]
                                (expect (= (str tid) (str (:id turn))))
                                (expect (= :council (:request-kind turn)))
                                (expect (= (:content entry) (get-in turn [:council :content]))))
                              (finally (ps/db-dispose-connection! reopened)))))
                     (finally (ps/db-dispose-connection! db)
                              (doseq [^java.io.File f (reverse (file-seq dir))]
                                (.delete f)))))))

(defdescribe
  council-wake-live-event-and-early-terminal-share-durable-provenance
  (it
    "council wake live event and early terminal share durable provenance"
    (let [{:keys [db sid]}
          (managed-fixture)

          events
          (atom [])]

      (with-redefs-fn {#'toggles/enabled? (constantly true)
                       #'lp/db-info (constantly db)
                       #'state/session-model (constantly {:provider "fixture" :model "fixture"})
                       #'state/fresh-entry (fn [_]
                                             {:next-seq 0 :turns {} :turn-order []})
                       #'state/start-turn-stall-watchdog! (fn [& _]
                                                            nil)
                       #'state/append-event! (fn [_ type payload & _]
                                               (swap! events conj [type payload]))
                       ;; Hold execution before the worker starts; cancellation must still persist provenance.
                       #'cancellation/worker-future (fn [& _]
                                                      (future nil))}
        (fn []
          (try (let [entry
                     (council/wake! db
                                    #(council/runtime db)
                                    {:session-id sid :source "sdk"}
                                    {:content "Actual build result from the SDK"})

                     started
                     (some (fn [[type payload]]
                             (when (= "turn.started" type) payload))
                           @events)

                     tid
                     (:turn_id started)

                     live
                     (state/get-turn sid tid)]

                 (expect (some? started))
                 (expect (= "council" (:request_kind started)))
                 (expect (= (:entry_id entry) (get-in started [:council :entry_id])))
                 (expect (= (:content entry) (:request started) (get live "request")))
                 (expect (= "council" (get live "request_kind")))
                 (expect
                   (#'state/persist-forced-terminal! sid tid {:status :interrupted :content []}))
                 (let [turn (last (state/transcript sid))]
                   (expect (= "council" (get turn "request_kind")))
                   (expect (= (:entry_id entry) (get-in turn ["council" "entry_id"])))
                   (expect (= (:content entry) (get turn "request")))))
               (finally (#'state/drop-session! sid))))))))
