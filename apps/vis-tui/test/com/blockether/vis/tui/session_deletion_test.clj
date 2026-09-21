(ns com.blockether.vis.tui.session-deletion-test
  (:require [com.blockether.vis.tui.chat :as chat]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.screen :as screen]
            [com.blockether.vis.tui.state :as state]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- delete-session
  [db sid]
  (if-let [handler (get-in @@#'state/event-registry [:session-deleted :fn])]
    (handler db [:session-deleted sid])
    {:db db}))

(defn- two-tabs
  []
  {:tabs [{:id :main :label "Deleted" :active? true :project-id "p"}
          {:id :tab-1 :label "Kept" :project-id "p"}]
   :active-tab-id :main
   :active-project-id "p"
   :project-active-tabs {"p" :main}
   :session {:id "gone"}
   :messages [{:text "Deleted transcript"}]
   :tab-locals {:tab-1 {:session {:id "kept"} :messages [{:text "Keep this"}]}}
   :render-version 0})

(defdescribe
  session-deletion-test
  (it
    "projects the gateway deletion and routes it through the idle tab subscription"
    (let [sink
          (atom nil)

          events
          (atom [])

          stopped
          (atom false)

          frame
          {"type" "session.deleted" "session_id" "gone" "seq" 12}]

      (expect (= {:phase :session-deleted :session-id "gone"} (#'chat/gateway-event->chunk frame)))
      (with-redefs [state/app-db
                    (atom (two-tabs))

                    state/dispatch
                    #(swap! events conj %)

                    vis/gateway-current-seq
                    (constantly 0)

                    vis/gateway-mux-subscribe!
                    (fn [_ callback _]
                      (reset! sink callback)
                      #(reset! stopped true))

                    vis/add-title-listener!
                    (fn [& _])

                    vis/add-title-pending-listener!
                    (fn [& _])

                    vis/remove-title-listener!
                    (fn [& _])

                    vis/remove-title-pending-listener!
                    (fn [& _])

                    vis/worker-future
                    (fn [& _])]

        (let [cleanup (#'screen/subscribe-session-live! "gone")]
          (try (@sink frame) (expect (= [[:session-deleted "gone"]] @events)) (finally (cleanup)))))
      (expect @stopped)))
  (it "removes the active session and restores its neighbor without server writes"
      (let [{:keys [db fx]} (delete-session (two-tabs) "gone")]
        (expect (= [:tab-1] (mapv :id (:tabs db))))
        (expect (= :tab-1 (:active-tab-id db)))
        (expect (= "kept" (get-in db [:session :id])))
        (expect (= [{:text "Keep this"}] (:messages db)))
        (expect (not (contains? (:tab-locals db) :main)))
        (expect (not-any? #{:main} (vals (:project-active-tabs db))))
        ;; The neighbor takes focus, so its session is read: that mark is what stops
        ;; every other surface from calling it new.
        (expect (= [[:release-session-listener "gone"] [:mark-session-read "kept"]] fx))))
  (it "removes every background view, preserving the focused session and its input"
      (let [before
            (-> (two-tabs)
                (assoc :session {:id "kept"}
                       :input {:text "Unsent"})
                (assoc-in [:tab-locals :tab-1 :session :id] "gone")
                (assoc-in [:tab-locals :tab-2] {:session {:id "gone"}})
                (update :tabs conj {:id :tab-2 :project-id "other"}))

            {:keys [db fx]}
            (delete-session before "gone")]

        (expect (= [:main] (mapv :id (:tabs db))))
        (expect (= :main (:active-tab-id db)))
        (expect (= {:text "Unsent"} (:input db)))
        (expect (= "kept" (get-in db [:session :id])))
        (expect (not (contains? (:tab-locals db) :tab-1)))
        (expect (not (contains? (:tab-locals db) :tab-2)))
        (expect (= [[:release-session-listener "gone"]] fx))))
  (it "replaces the last project tab with a clean tab and cancels only local work"
      (let [before
            (-> (two-tabs)
                (assoc :loading? true
                       :cancel-token :local-token
                       :gateway-turn-id "turn"
                       :pending-sends [{:text "Do not resubmit"}]
                       :workspace/root "/deleted-draft"
                       :human-input {:id "question"}
                       :live-views [{:id "view"}])
                (assoc-in [:tabs 1 :project-id] "other"))

            {:keys [db fx]}
            (delete-session before "gone")

            fresh
            (some #(when (= "p" (:project-id %)) %) (:tabs db))]

        (expect (some? fresh))
        (expect (not= :main (:id fresh)))
        (expect (= (:id fresh) (:active-tab-id db)))
        (expect (nil? (:session db)))
        (expect (= [] (:messages db)))
        (expect (= [] (:pending-sends db)))
        (expect (false? (:loading? db)))
        (expect (nil? (:workspace/root db)))
        (expect (nil? (:human-input db)))
        (expect (= [] (:live-views db)))
        (expect (= "kept" (get-in db [:tab-locals :tab-1 :session :id])))
        (expect (= [[:release-session-listener "gone"] [:cancel-local-turn :local-token]] fx))
        (expect (= {:db db} (delete-session db "gone")))))
  (it "keeps the terminal usable when its only tab is deleted"
      (let [before
            (assoc (two-tabs)
              :tabs [{:id :main :active? true :project-id "p"}]
              :tab-locals {})

            {:keys [db fx]}
            (delete-session before "gone")]

        (expect (= 1 (count (:tabs db))))
        (expect (not= :main (:active-tab-id db)))
        (expect (nil? (:session db)))
        (expect (= [] (:messages db)))
        (expect (= [[:release-session-listener "gone"]] fx))))
  (it "ignores unknown and repeated deletions"
      (let [db (two-tabs)]
        (expect (= {:db db} (delete-session db "unknown"))))))
