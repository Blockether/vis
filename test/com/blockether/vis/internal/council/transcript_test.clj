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
            [com.blockether.vis.internal.session.cancellation :as cancellation]
            [com.blockether.vis.internal.session.titling :as titling]
            [lazytest.core :refer [defdescribe expect it]]))

(wiring/install!)

(h/use-mem-store!)

(defn- fixture
  [kind]
  (let [db
        (h/store)

        gid
        (str (:id (ps/db-create-project! db {:name "Council transcript"})))

        sid
        (str (h/store-session! db {:channel :api}))

        activation
        (str (random-uuid))]

    (ps/db-set-session-project! db sid gid)
    (with-redefs [toggles/enabled? (constantly true)]
      {:db db
       :sid sid
       :entry
       (council/publish!
         db
         (constantly {sid {:activation-id activation :group-id gid :state "running"}})
         {:session-id sid :activation-id activation :source "sdk"}
         {:kind kind
          :title "Review findings"
          :content
          "Actual Council request.\nSecond line.\nThird line.\nFourth line.\nFifth line."})})))

(defdescribe
  council-request-provenance-survives-storage-and-fork
  (it
    "council request provenance survives storage and fork"
    (doseq [kind ["coordination" "informational" "complain"]]
      (let [{:keys [db sid entry]} (fixture kind)
            prompt "Council notification. Read the attributed input."
            opts (#'turn/turn-store-opts
                  {:session-id sid}
                  prompt
                  {:request-kind :council :council-entry-id (:entry_id entry)})
            tid (ps/db-store-session-turn! db opts)
            user-tid (ps/db-store-session-turn! db
                                                {:parent-session-id sid
                                                 :user-request "Council wake — literal user text"})
            [turn user-turn] (ps/db-list-session-turns db sid)
            wire (#'state/persisted-turn->wire sid turn)
            transcript (with-redefs [lp/db-info (constantly db)]
                         (state/transcript sid))
            fork (h/fork-session-at-turn! db sid {:through-turn-id user-tid})]

        (expect (= :council (:request-kind turn)))
        (expect (= prompt (:user-request turn))
                "Model instruction stays separate from the visible request")
        (expect (= (:entry_id entry) (get-in turn [:council :entry-id])))
        (expect (= kind
                   (some-> (get-in turn [:council :kind])
                           name)))
        (expect (= (:content entry) (get-in turn [:council :content])))
        (expect (= (:content entry) (:request wire)))
        (expect (= "council" (get-in transcript [0 "request_kind"])))
        (expect (= kind (get-in transcript [0 "council" "kind"])))
        (expect (= (:content entry) (get-in transcript [0 "request"])))
        (expect (= :user (:request-kind user-turn)) "Never classify user text by its prefix")
        (expect (nil? (:council user-turn)))
        (expect (= [{:request_kind "council" :council_entry_id (:entry_id entry)}]
                   (h/raw-query db
                                {:select [:request_kind :council_entry_id]
                                 :from :session_turn_soul
                                 :where [:= :id (str tid)]})))
        (expect (= [{:kind kind}]
                   (h/raw-query
                     db
                     {:select [:kind] :from :council_entry :where [:= :id (:entry_id entry)]})))
        (expect (some? fork))
        (expect (= (:council turn) (:council (first (ps/db-list-session-turns db fork)))))))))

;; Regression: completed Council wakes appeared as You after engine preparation
;; discarded their provenance. Exercise both engine phases before the real write.
(defdescribe
  council-provenance-survives-engine-phases
  (it
    "council provenance survives engine phases"
    (doseq [kind [nil "coordination" "informational" "complain"]]
      (let [{:keys [db sid entry]} (fixture (or kind "informational"))
            tid (str (random-uuid))
            prompt
            (str "Council notification #" (:entry_id entry) ". Read the attributed Council input.")
            env {:db-info db
                 :session-id sid
                 :turn-state-atom (ctx-loop/make-turn-state-atom)
                 :router {:providers [{:id :openai-codex :models [{:name "shared"}]}]}}
            opts (cond-> {:model "shared" :session-turn-id tid}
                   kind
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
        (expect (= (if kind :council :user) (:request-kind turn)))
        (expect (= (if kind "council" "user") (get wire "request_kind")))
        (if kind
          (do (expect (= (:entry_id entry) (get-in wire ["council" "entry_id"])))
              (expect (= (:thread_id entry) (get-in wire ["council" "thread_id"])))
              (expect (= kind (get-in wire ["council" "kind"])))
              (expect (= (:content entry) (get wire "request"))))
          (do (expect (nil? (:council turn))) (expect (= prompt (get wire "request")))))))))

(defn- managed-fixture
  [kind]
  (let [{:keys [db sid] :as base}
        (fixture kind)

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
  (it "council wake keeps display content out of the short instruction"
      (let [{:keys [db sid entry]}
            (managed-fixture "coordination")

            submitted
            (atom nil)

            wake!
            state/council-wake!]

        (doseq [required? [false true]]
          (with-redefs-fn {#'toggles/enabled? (constantly true)
                           #'state/council-wake-eligible? (constantly true)
                           #'state/submit-turn! (fn [_ opts]
                                                  (reset! submitted opts)
                                                  {:turn {}})}
            #(wake! db sid (assoc entry :reply_required required?)))
          (expect (= (:content entry) (:display-request @submitted)))
          (expect (= :council (get-in @submitted [:engine-opts :request-kind])))
          (expect (= (:entry_id entry) (get-in @submitted [:engine-opts :council-entry-id])))
          (expect (< (count (:request @submitted)) 360))
          (expect (str/includes? (:request @submitted) (str "council.get(" (:entry_id entry) ")")))
          (expect (= required? (str/includes? (:request @submitted) "before ending this turn")))
          (expect (not (str/includes? (:request @submitted) (:content entry))))))))

(defdescribe request-provenance-is-enforced-by-sqlite
             (it "request provenance is enforced by sqlite"
                 (let [{:keys [db sid entry]} (fixture "coordination")]
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
                             (fixture "complain"))

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
                                (expect (= :complain (get-in turn [:council :kind])))
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
          (managed-fixture "informational")

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
          (try
            (let [entry
                  (council/wake! db
                                 #(council/runtime db)
                                 {:session-id sid :source "sdk"}
                                 {:kind "coordination" :content "Actual build result from the SDK"})

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
              (expect (= "coordination" (get-in started [:council :kind])))
              (expect (= (:content entry) (:request started) (get live "request")))
              (expect (= "council" (get live "request_kind")))
              (expect (#'state/persist-forced-terminal! sid tid {:status :interrupted :content []}))
              (let [turn (last (state/transcript sid))]
                (expect (= "council" (get turn "request_kind")))
                (expect (= "coordination" (get-in turn ["council" "kind"])))
                (expect (= (:entry_id entry) (get-in turn ["council" "entry_id"])))
                (expect (= (:content entry) (get turn "request")))))
            (finally (#'state/drop-session! sid))))))))
