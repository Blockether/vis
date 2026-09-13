(ns com.blockether.vis.internal.council.leadership-test
  "Independent leaders exchange messages without waking one another."
  (:require [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.council.core :as council]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(h/use-mem-store!)

(deftest independent-leaders-never-wake-test
  ;; Regression: coordination between independent leaders created new user-facing turns.
  (let [db
        (h/store)

        gid
        (str (:id (ps/db-create-project! db {:name "Leaders"})))

        a
        (str (h/store-session! db {:channel :api}))

        b
        (str (h/store-session! db {:channel :api}))

        fleet
        (atom {a {:activation-id "a" :group-id gid}})

        wakes
        (atom [])

        actor
        {:session-id a :activation-id "a" :source "host"}]

    (doseq [sid [a b]]
      (ps/db-set-session-project! db sid gid))
    (with-redefs-fn {#'toggles/enabled? (constantly true)
                     (ns-resolve 'com.blockether.vis.internal.council.core 'runtime-waker)
                     (atom {:eligible? (constantly true)
                            :wake! (fn [_ sid _]
                                     (swap! wakes conj sid))})}
      (fn []
        (let [entry (council/publish! db
                                      #(deref fleet)
                                      actor
                                      {:kind "coordination"
                                       :content "Existing findings?"
                                       :ping [b]
                                       :reply_required true})]
          (is (empty? @wakes))
          (is (= "unavailable" (get-in entry [:replies 0 :state]))))
        (swap! fleet assoc b {:activation-id "b" :group-id gid})
        (let [request (council/publish! db
                                        #(deref fleet)
                                        actor
                                        {:kind "coordination"
                                         :content "While both are active"
                                         :ping [b]
                                         :reply_required true})]
          (swap! fleet dissoc a)
          (council/publish!
            db
            #(deref fleet)
            {:session-id b :activation-id "b" :source "host"}
            {:kind "informational" :content "Findings" :reply_to (:entry_id request)})
          (is (empty? @wakes) "A reply is not permission to wake another leader"))))))

(deftest council-turns-do-not-count-as-human-answers-test
  ;; Regression: unread badges counted settled Council turns as new human answers.
  (let [db
        (h/store)

        sid
        (h/store-session! db {:channel :api})

        entry
        (ps/db-council-insert! db
                               {:group_id (council/default-group db sid)
                                :author_sid (str sid)
                                :activation_id "fixture"
                                :source "host"
                                :kind "coordination"
                                :title "Work"
                                :content "Work"
                                :created_at 1
                                :idempotency_key "seed"
                                :fingerprint "seed"}
                               []
                               false)]

    (doseq [[kind status] [[:user :success] [:council :success] [:user :running]]]
      (ps/db-store-session-turn!
        db
        (cond-> {:parent-session-id sid :user-request "Work" :request-kind kind :status status}
          (= :council kind)
          (assoc :council-entry-id (get-in entry [:entry :entry_id])))))
    (is (= 3 (:turn-count (ps/db-session-turn-stats db sid))))
    (is (= 1 (:answer-count (ps/db-session-turn-stats db sid))))
    (is (= 1 (get-in (ps/db-session-turn-stats db) [(str sid) :answer-count])))))
