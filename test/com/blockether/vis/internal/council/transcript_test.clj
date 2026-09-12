(ns com.blockether.vis.internal.council.transcript-test
  "Council requests retain their explicit provenance across the loop, SQLite and gateway."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.council.core :as council]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [com.blockether.vis.internal.session.cancellation :as cancellation]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

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

(deftest council-request-provenance-survives-storage-and-fork
  (doseq [kind ["coordination" "informational" "complain"]]
    (let [{:keys [db sid entry]} (fixture kind)
          prompt "Council notification. Read the attributed input."
          opts (#'lp/turn-store-opts
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

      (is (= :council (:request-kind turn)))
      (is (= prompt (:user-request turn))
          "Model instruction stays separate from the visible request")
      (is (= (:entry_id entry) (get-in turn [:council :entry-id])))
      (is (= kind
             (some-> (get-in turn [:council :kind])
                     name)))
      (is (= (:content entry) (get-in turn [:council :content])))
      (is (= (:content entry) (:request wire)))
      (is (= "council" (get-in transcript [0 "request_kind"])))
      (is (= kind (get-in transcript [0 "council" "kind"])))
      (is (= (:content entry) (get-in transcript [0 "request"])))
      (is (= :user (:request-kind user-turn)) "Never classify user text by its prefix")
      (is (nil? (:council user-turn)))
      (is (= [{:request_kind "council" :council_entry_id (:entry_id entry)}]
             (h/raw-query db
                          {:select [:request_kind :council_entry_id]
                           :from :session_turn_soul
                           :where [:= :id (str tid)]})))
      (is (= [{:kind kind}]
             (h/raw-query
               db
               {:select [:kind] :from :council_entry :where [:= :id (:entry_id entry)]})))
      (is (some? fork))
      (is (= (:council turn) (:council (first (ps/db-list-session-turns db fork))))))))

(deftest council-wake-keeps-display-content-out-of-the-short-instruction
  (let [{:keys [db sid entry]}
        (fixture "coordination")

        submitted
        (atom nil)

        wake!
        (:wake! @(var-get #'council/runtime-waker))]

    (doseq [required? [false true]]
      (with-redefs-fn {#'toggles/enabled? (constantly true)
                       #'state/council-wake-eligible? (constantly true)
                       #'state/submit-turn! (fn [_ opts]
                                              (reset! submitted opts)
                                              {:turn {}})}
        #(wake! db sid (assoc entry :reply_required required?)))
      (is (= (:content entry) (:display-request @submitted)))
      (is (= :council (get-in @submitted [:engine-opts :request-kind])))
      (is (= (:entry_id entry) (get-in @submitted [:engine-opts :council-entry-id])))
      (is (< (count (:request @submitted)) 360))
      (is (str/includes? (:request @submitted) (str "council.get(" (:entry_id entry) ")")))
      (is (= required? (str/includes? (:request @submitted) "before ending this turn")))
      (is (not (str/includes? (:request @submitted) (:content entry)))))))

(deftest request-provenance-is-enforced-by-sqlite
  (let [{:keys [db sid entry]} (fixture "coordination")]
    (doseq [invalid [{:request-kind :assistant} {:request-kind :council}
                     {:request-kind :user :council-entry-id (:entry_id entry)}
                     {:request-kind :council :council-entry-id 99999}]]
      (is (try (ps/db-store-session-turn! db
                                          (merge {:parent-session-id sid :user-request "Request"}
                                                 invalid))
               false
               (catch Exception _ true))))))

(deftest council-transcript-survives-database-reopen
  (let [dir
        (.toFile (java.nio.file.Files/createTempDirectory
                   "vis-council-transcript"
                   (make-array java.nio.file.attribute.FileAttribute 0)))

        db
        (ps/db-create-connection! (.getPath dir))]

    (try (let [{:keys [sid entry]}
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
                    (is (= (str tid) (str (:id turn))))
                    (is (= :council (:request-kind turn)))
                    (is (= :complain (get-in turn [:council :kind])))
                    (is (= (:content entry) (get-in turn [:council :content]))))
                  (finally (ps/db-dispose-connection! reopened)))))
         (finally (ps/db-dispose-connection! db)
                  (doseq [^java.io.File f (reverse (file-seq dir))]
                    (.delete f))))))

(deftest council-wake-live-event-and-early-terminal-share-durable-provenance
  (let [{:keys [db sid]}
        (fixture "informational")

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
                                  {:kind "coordination"
                                   :content "Actual build result from the SDK"})

                   started
                   (some (fn [[type payload]]
                           (when (= "turn.started" type) payload))
                         @events)

                   tid
                   (:turn_id started)

                   live
                   (state/get-turn sid tid)]

               (is (some? started))
               (is (= "council" (:request_kind started)))
               (is (= (:entry_id entry) (get-in started [:council :entry_id])))
               (is (= "coordination" (get-in started [:council :kind])))
               (is (= (:content entry) (:request started) (get live "request")))
               (is (= "council" (get live "request_kind")))
               (is (#'state/persist-forced-terminal! sid tid {:status :interrupted :content []}))
               (let [turn (first (state/transcript sid))]
                 (is (= "council" (get turn "request_kind")))
                 (is (= "coordination" (get-in turn ["council" "kind"])))
                 (is (= (:entry_id entry) (get-in turn ["council" "entry_id"])))
                 (is (= (:content entry) (get turn "request")))))
             (finally (#'state/drop-session! sid)))))))
