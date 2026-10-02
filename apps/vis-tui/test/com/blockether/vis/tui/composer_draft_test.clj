(ns com.blockether.vis.tui.composer-draft-test
  "Regression coverage for unsent work that belongs to a session, not its tab."
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.client :as client]
            [com.blockether.vis.tui.input :as input]
            [com.blockether.vis.tui.projects :as projects]
            [com.blockether.vis.tui.screen :as screen]
            [com.blockether.vis.tui.state :as state]
            [lazytest.core :refer [defdescribe describe expect it]]))

(defn- step
  [db id & args]
  (let [handler
        (:fn (get @@#'state/event-registry id))

        result
        (handler db (into [id] args))]

    (#'state/finalize-db (if (contains? result :db) (:db result) result))))

(defn- draft-input [text] (input/paste-text (input/empty-input) text))

(defn- draft-db
  []
  {:active-project-id "p"
   :active-tab-id :tab-2
   :tabs [{:id :tab-1 :project-id "p"} {:id :tab-2 :project-id "p" :active? true}]
   :session {:id "b"}
   :input (input/empty-input)
   :tab-locals {:tab-1 {:session {:id "a"} :input (draft-input "Unsent first message")}}
   :project-sidebar
   {:open? true :expanded #{"p"} :items [{"id" "p" "name" "Project"}] :pages {"p" {}}}})

(defdescribe
  session-composer-drafts-test
  (describe
    "returning to unsent work"
    ;; User report: a new session holding unsent words must remain reachable as dirty.
    (it "restores words, collapsed pastes and staged files after closing and reopening a tab"
        (let [payload
              {:input (draft-input "Draft A [Pasted #1]")
               :pastes {1 {:text "Full paste body"}}
               :paste-counter 1
               :attachments [{:id "file" :filename "notes.txt" :base64 "bm90ZXM="}]
               :image-counter 2}

              a
              (-> {:active-project-id "p"}
                  (step :init-session {:id "a"} [] nil)
                  (merge payload))

              b
              (step a :open-session-tab {:id "b"} [] nil)

              a-tab
              (state/tab-id-for-session b "a")

              closed
              (step b :close-tab a-tab)

              reopened
              (step closed :open-session-tab {:id "a"} [] nil)]

          (expect (nil? (state/tab-id-for-session closed "a")))
          (expect (= payload (select-keys reopened (keys payload))))
          (expect (empty? (:messages reopened)))
          (expect (empty? (:input-history reopened)))
          (expect (empty? (:pending-sends reopened)))))
    (it "keeps an unsent first message when the new session finishes building in the background"
        (let [a
              (step {:active-project-id "p"} :init-session {:id "a"} [] nil)

              building
              (step a :open-building-tab "build")

              typed
              (step building :update-input (draft-input "New session draft"))

              away
              (step typed :select-tab-by-session "a")

              bound
              (step away :bind-built-session "build" {:id "new"} [] nil)

              returned
              (step bound :select-tab-by-session "new")]

          (expect (= "a" (get-in bound [:session :id])))
          (expect (= "New session draft" (input/input->text (:input returned)))))))
  (describe "gateway-owned dirty listing"
            (it "sends dirty session ids with both list windows and recent-session searches"
                (expect (str/includes? (#'client/session-window-path {:limit 10 :dirty ["a" "b"]})
                                       "dirty=a%2Cb"))
                (let [asked (atom nil)]
                  (with-redefs-fn {#'client/send-json! (fn [_ path]
                                                         (reset! asked path)
                                                         {"sessions" []})}
                    #(client/search-sessions "" {:dirty ["a" "b"]}))
                  (expect (str/includes? @asked "dirty=a%2Cb"))))
            (it "keeps an empty session with a parked unsent first message in the sidebar as Dirty"
                (let [asked (atom [])]
                  (with-redefs [state/app-db (atom (draft-db))
                                client/worker-future (fn [_ f]
                                                       (f))
                                client/gateway-list-session-groups-page (constantly {:groups []
                                                                                     :total 0})
                                client/gateway-list-sessions-page
                                (fn [opts]
                                  (swap! asked conj opts)
                                  {:sessions (cond-> [{"id" "b" "title" "Existing session"}]
                                               (some #{"a"} (:dirty opts))
                                               (conj {"id" "a" "title" "" "turn_count" 0}))
                                   :grouped []})]

                    (#'screen/load-project-page! "p")
                    (expect (= #{"a"} (set (:dirty (first @asked)))))
                    (let [row (some #(when (= "a" (get-in % [:session "id"])) %)
                                    (projects/sidebar-entries @state/app-db))]
                      (expect (= "Dirty" (:status row)))
                      (expect (= "Unsent first message" (:label row)))
                      (expect (= [:session "a"] (:action row)))))))
            (it "passes the same dirty identities to the recent-session picker"
                (let [asked (atom nil)]
                  (with-redefs [state/app-db (atom (draft-db))
                                client/gateway-search-sessions (fn [_ opts]
                                                                 (reset! asked opts)
                                                                 {:sessions []})]

                    (#'screen/tui-session-page {:limit 10})
                    (expect (= #{"a"} (set (:dirty @asked)))))))))

(defdescribe
  session-composer-draft-lifecycle-test
  (it "isolates drafts even when the current tab is rebound to another session"
      (let [a
            (-> {:active-project-id "p"}
                (step :init-session {:id "a"} [] nil)
                (step :update-input (draft-input "Draft A")))

            b
            (step a :init-session {:id "b"} [] nil)

            typed-b
            (step b :update-input (draft-input "Draft B"))

            returned-a
            (step typed-b :init-session {:id "a"} [] nil)

            returned-b
            (step returned-a :init-session {:id "b"} [] nil)]

        (expect (input/input-empty? (:input b)))
        (expect (= "Draft A" (input/input->text (:input returned-a))))
        (expect (= "Draft B" (input/input->text (:input returned-b))))
        (expect (= #{"a" "b"} (state/session-draft-ids returned-b)))))
  (it "keeps an attachment-only draft dirty after its tab closes"
      (let [file
            {:id "file" :filename "notes.txt" :base64 "bm90ZXM="}

            db
            (-> (draft-db)
                (assoc-in [:tab-locals :tab-1 :input] (input/empty-input))
                (assoc-in [:tab-locals :tab-1 :attachments] [file]))

            closed
            (step db :close-tab :tab-1)

            listed
            (assoc-in closed [:project-sidebar :pages "p" :sessions] [{"id" "a" "title" ""}])

            row
            (some #(when (= "a" (get-in % [:session "id"])) %) (projects/sidebar-entries listed))

            reopened
            (step closed :open-session-tab {:id "a"} [] nil)]

        (expect (= #{"a"} (state/session-draft-ids closed)))
        (expect (= "Dirty" (:status row)))
        (expect (= "1 unsent attachment" (:label row)))
        (expect (= [file] (:attachments reopened)))))
  (it "does not call an untouched or whitespace-only session dirty"
      (let [db (-> (draft-db)
                   (assoc-in [:tab-locals :tab-1 :input] (draft-input "  \n  ")))]
        (expect (empty? (state/session-draft-ids db)))
        (expect (empty? (:session-drafts (step db :close-tab :tab-1))))))
  (it "clears the saved draft when sending resets the composer"
      (let [a
            (-> {:active-project-id "p"}
                (step :init-session {:id "a"} [] nil)
                (step :update-input (draft-input "Draft A")))

            b
            (step a :open-session-tab {:id "b"} [] nil)

            closed
            (step b :close-tab (state/tab-id-for-session b "a"))

            reopened
            (step closed :open-session-tab {:id "a"} [] nil)

            cleared
            (step reopened :reset-input)

            closed-again
            (step cleared :close-tab (state/tab-id-for-session cleared "a"))

            returned
            (step closed-again :open-session-tab {:id "a"} [] nil)]

        (expect (empty? (state/session-draft-ids cleared)))
        (expect (empty? (:session-drafts cleared)))
        (expect (input/input-empty? (:input returned)))))
  (it "drops deleted sessions' drafts whether or not they still have an open tab"
      (doseq [close-first? [false true]]
        (let [db (draft-db)
              before (if close-first?
                       (step db :close-tab :tab-1)
                       (#'state/remember-session-draft db (get-in db [:tab-locals :tab-1])))
              deleted (step before :session-deleted "a")
              reopened (step deleted :open-session-tab {:id "a"} [] nil)]

          (expect (not (contains? (:session-drafts deleted) "a")))
          (expect (empty? (state/session-draft-ids deleted)))
          (expect (input/input-empty? (:input reopened))))))
  (it "shows a saved draft before a pending tab hydrates and preserves edits made during that read"
      (let [closed
            (step (draft-db) :close-tab :tab-1)

            allocated
            (step closed :preallocate-project-tabs [{:session-id "a" :label "A" :project-id "p"}])

            selected
            (step allocated :select-tab-by-session "a")

            edited
            (step selected :update-input (draft-input "Edited while loading"))

            hydrated
            (step edited :open-session-tab {:id "a"} [] nil)]

        (expect (= "Unsent first message" (input/input->text (:input selected))))
        (expect (= "Edited while loading" (input/input->text (:input hydrated))))))
  (it "restarts the gateway window when dirty membership changes, but keeps ordinary paging"
      (let [asked
            (atom [])

            db
            (assoc-in (draft-db)
              [:project-sidebar :pages "p"]
              {:after "old-cursor" :history [nil] :dirty #{}})]

        (with-redefs [state/app-db
                      (atom db)

                      client/worker-future
                      (fn [_ f]
                        (f))

                      client/gateway-list-session-groups-page
                      (constantly {:groups [] :total 0})

                      client/gateway-list-sessions-page
                      (fn [opts]
                        (swap! asked conj opts)
                        {:sessions [{"id" "b" "title" "Existing session"}] :grouped []})]

          (#'screen/load-project-page! "p" true)
          (expect (nil? (:after (first @asked))))
          (expect (empty? (get-in @state/app-db [:project-sidebar :pages "p" :history])))
          (swap! state/app-db assoc-in [:project-sidebar :pages "p" :after] "next-cursor")
          (#'screen/load-project-page! "p" true)
          (expect (= "next-cursor" (:after (last @asked)))))))
  (it "includes dirty identities in the separate group window when archive views differ"
      (let [asked
            (atom [])

            db
            (assoc-in (draft-db) [:project-sidebar :session-archived? "p"] true)]

        (with-redefs [state/app-db
                      (atom db)

                      client/worker-future
                      (fn [_ f]
                        (f))

                      client/gateway-list-session-groups-page
                      (constantly {:groups [] :total 0})

                      client/gateway-list-sessions-page
                      (fn [opts]
                        (swap! asked conj opts)
                        {:sessions [{"id" "b"}] :grouped []})]

          (#'screen/load-project-page! "p")
          (expect (= [:only :exclude] (mapv :archived @asked)))
          (expect (= [#{"a"} #{"a"}] (mapv (comp set :dirty) @asked))))))
  (it
    "refreshes when draft presence changes, not on every edit, and removes Dirty after clearing"
    (let [db
          (atom (update (draft-db) :tab-locals dissoc :tab-1))

          initial-read
          (promise)

          dirty-read
          (promise)

          clean-read
          (promise)

          reads
          (atom [])]

      (with-redefs-fn {#'state/app-db db
                       #'client/gateway-fleet-subscribe! (fn [_]
                                                           (fn []
                                                             nil))
                       #'screen/refresh-projects! (fn [_]
                                                    (let [ids (state/session-draft-ids @db)]
                                                      (swap! reads conj ids)
                                                      (cond (seq ids) (deliver dirty-read ids)
                                                            (realized? initial-read)
                                                            (deliver clean-read ids)
                                                            :else (deliver initial-read ids))))}
        (fn []
          (let [stop (#'screen/start-projects-refresh!)]
            (try (expect (= #{} (deref initial-read 4000 ::timeout)))
                 (state/dispatch [:update-input (draft-input "First thought")])
                 (expect (= #{"b"} (deref dirty-read 4000 ::timeout)))
                 (state/dispatch [:update-input (draft-input "More words, same draft")])
                 (Thread/sleep 1100)
                 (expect (= [#{} #{"b"}] @reads))
                 (state/dispatch [:reset-input])
                 (expect (= #{} (deref clean-read 4000 ::timeout)))
                 (expect (= [#{} #{"b"} #{}] @reads))
                 (finally (swap! db assoc :shutdown? true) (stop))))))))
  (it "omits empty dirty filters and encodes dirty sets consistently"
      (expect (= "/v1/sessions?limit=10" (#'client/session-window-path {:limit 10 :dirty #{}})))
      (expect (= (#'client/session-window-path {:dirty ["a" "b"]})
                 (#'client/session-window-path {:dirty #{"b" "a"}})))))

(defdescribe
  pending-session-composer-draft-test
  (it "records, labels and restores edits made before a pending session hydrates"
      (let [closed
            (step (draft-db) :close-tab :tab-1)

            pending
            (-> closed
                (step :preallocate-project-tabs [{:session-id "a" :label "A" :project-id "p"}])
                (step :select-tab-by-session "a"))

            edited
            (step pending :update-input (draft-input "Edited before hydration"))

            listed
            (assoc-in edited [:project-sidebar :pages "p" :sessions] [{"id" "a"}])

            row
            (some #(when (= "a" (get-in % [:session "id"])) %) (projects/sidebar-entries listed))

            closed-again
            (step edited :close-tab (state/tab-id-for-session edited "a"))

            reopened
            (step closed-again :open-session-tab {:id "a"} [] nil)]

        (expect (= "Edited before hydration" (:label row)))
        (expect (= "Dirty" (:status row)))
        (expect (= #{"a"} (state/session-draft-ids edited)))
        (expect (= "Edited before hydration" (input/input->text (:input reopened))))))
  (it "never resurrects a draft cleared before pending hydration finishes"
      (let [closed
            (step (draft-db) :close-tab :tab-1)

            pending
            (-> closed
                (step :preallocate-project-tabs [{:session-id "a" :label "A" :project-id "p"}])
                (step :select-tab-by-session "a"))

            cleared
            (step pending :reset-input)

            hydrated
            (step cleared :open-session-tab {:id "a"} [] nil)]

        (expect (empty? (state/session-draft-ids cleared)))
        (expect (input/input-empty? (:input hydrated))))))

(defdescribe draft-navigation-consistency-test
             (it "does not mark a session dirty because an unused paste body remains in memory"
                 (let [db (-> (draft-db)
                              (assoc-in [:tab-locals :tab-1 :input] (input/empty-input))
                              (assoc-in [:tab-locals :tab-1 :pastes] {1 {:text "Unused paste"}}))]
                   (expect (empty? (state/session-draft-ids db)))))
             (it "sends the same dirty overlay to project counts as to its session windows"
                 (let [asked (atom [])]
                   (with-redefs [state/app-db (atom (draft-db))
                                 client/worker-future (fn [_ f]
                                                        (f))
                                 client/gateway-list-projects (constantly [{"id" "p"
                                                                            "name" "Project"}])
                                 client/gateway-projects-overview (fn [& [opts]]
                                                                    (swap! asked conj opts)
                                                                    {"projects" []})
                                 screen/load-project-page! (fn [& _]
                                                             nil)]

                     (#'screen/refresh-projects! true)
                     (expect (= #{"a"} (set (:dirty (first @asked))))))))
             (it "encodes dirty session ids on the project overview route"
                 (let [asked (atom nil)]
                   (with-redefs-fn {#'client/send-json! (fn [_ path]
                                                          (reset! asked path)
                                                          {})}
                     (fn []
                       (client/projects-overview {:dirty ["b" "a"]})
                       (expect (= "/v1/projects/overview?dirty=a%2Cb" @asked)))))))
