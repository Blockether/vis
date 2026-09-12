(ns com.blockether.vis.internal.activity.history-fork-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.activity.event :as event]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.persistance.core :as db]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [com.blockether.vis.internal.python.env :as env]
            [com.blockether.vis.test-python-context :as tpc]
            [lazytest.core :refer [defdescribe expect it]]))

(h/use-mem-store!)

(defn- emit-operation!
  [ctx n]
  (let [inv
        (event/invocation ctx nil)

        details
        {:operation :read-record
         :presenter :generic
         :args [{:path (str "record-" n)}]
         :activity {:headline "Read record" :show-start false}}]

    (extension/*tool-event-sink* (event/start-event ctx inv details))
    (extension/*tool-event-sink*
      (event/content-event
        ctx
        inv
        details
        {"headline" "Read record"
         "summary" (str "Record " n)
         "content" [{"type" "text" "text" (str "Record " n " " (apply str (repeat 2000 "界")))}]}))
    (extension/*tool-event-sink* (event/terminal-event ctx
                                                       inv
                                                       (assoc details
                                                         :outcome :succeeded
                                                         :started-at-ms
                                                         (System/currentTimeMillis))))
    (:invocation-id inv)))

(defn- all-rows
  [store sid aid opts]
  (loop [after
         0

         rows
         []]

    (let [page
          (db/db-activity-page store sid aid (assoc opts :after after))

          rows
          (into rows (:rows page))]

      (if-let [next-after (get-in page [:history :next-after])]
        (recur next-after rows)
        rows))))

(defn- run-stored-turn!
  "Run one Python block whose Activity history has `n` durable rows and store it as
   the single form of a new turn of `sid`; returns [turn-id history-id]."
  [store pc sid request n]
  (with-redefs [env/run-python-block (fn [_ _ _]
                                       (let [ctx (event/context)]
                                         (doseq [i (range n)]
                                           (emit-operation! ctx i)))
                                       {:stdout "done"})]
    (let [result (#'lp/run-python-code pc "pass" :env {:db-info store :session-id sid})
          tid (db/db-store-session-turn! store
                                         {:parent-session-id (str sid) :user-request request})]

      (expect (nil? (:error result)))
      (h/store-iteration!
        store
        {:session-turn-id tid
         :code "pass"
         :forms
         [{:scope nil :tag :observation :src "pass" :stdout "done" :activity (:activity result)}]})
      [tid (get-in result [:activity :history :id])])))

(defn- form-history-id
  [store tid]
  (-> (db/db-list-session-turn-iterations store tid)
      first
      :forms
      first
      (get-in [:activity :history :id])))

(defn- fork-turn-ids [store fork-sid] (mapv :id (db/db-list-session-turns store fork-sid)))

(defdescribe
  fork-preserves-activity-history-test
  ;; Regression #212: forking a session at a turn copied the iteration blobs verbatim, so
  ;; the fork's forms pointed at Activity histories owned by the SOURCE soul, which the
  ;; owner check in db-activity-page rejects and which vanish with the source session.
  (it
    "gives the fork its own durable copy of every history through the cutoff and nothing after"
    (let [store
          (h/store)

          sid
          (h/store-session! store {:channel :tui :title "Src"})]

      (tpc/with-own
        [pc {}]
        (let [[t1 aid1]
              (run-stored-turn! store pc sid "Q1" 145)

              [_t2 aid2]
              (run-stored-turn! store pc sid "Q2" 3)

              source-rows
              (all-rows store sid aid1 {})

              source-search
              (all-rows store sid aid1 {:q "Record 14"})

              fork-sid
              (h/fork-session-at-turn! store sid {:through-turn-id t1 :title "Fork"})

              [ft1 :as fork-turns]
              (fork-turn-ids store fork-sid)

              fork-aid
              (form-history-id store ft1)]

          ;; A real >128-row, >64KiB history stood behind the copied form.
          (expect (= 145 (count source-rows)))
          (expect (> (count (pr-str source-rows)) 65536))
          (expect (= 1 (count fork-turns)))
          ;; The fork's form names a NEW history the fork soul owns, not the source's.
          (expect (string? fork-aid))
          (expect (not= aid1 fork-aid))
          (expect (nil? (db/db-activity-page store fork-sid aid1 {})))
          (expect (nil? (db/db-activity-page store sid fork-aid {})))
          ;; Every page, detail and search result reads back identically from the fork.
          (expect (= source-rows (all-rows store fork-sid fork-aid {})))
          (expect (= source-search (all-rows store fork-sid fork-aid {:q "Record 14"})))
          ;; "Record 14" matches records 14 and 140-144.
          (expect (= 6 (count source-search)))
          (let [source-page
                (db/db-activity-page store sid aid1 {})

                fork-page
                (db/db-activity-page store fork-sid fork-aid {})]

            (expect (= (dissoc source-page :history) (dissoc fork-page :history)))
            (expect (= (dissoc (:history source-page) :id) (dissoc (:history fork-page) :id)))
            (expect (= fork-aid (get-in fork-page [:history :id]))))
          ;; Source untouched: same form reference, same rows, and the post-cutoff turn's
          ;; history was not copied (source t1, source t2, fork t1).
          (expect (= aid1 (form-history-id store t1)))
          (expect (= source-rows (all-rows store sid aid1 {})))
          (expect (= 3 (count (all-rows store sid aid2 {}))))
          (expect (= 3 (h/raw-count store :activity_history)))
          (expect (= 2 (h/raw-count store :activity_history [:= :session_soul_id (str sid)])))
          (expect (= 1 (h/raw-count store :activity_history [:= :session_soul_id (str fork-sid)])))
          (expect (= (+ 145 3 145) (h/raw-count store :activity_invocation)))
          ;; No orphan reference to the original history id anywhere in the fork's blobs.
          (let [blobs (h/raw-query store
                                   {:select [:i.tool_calls]
                                    :from [[:session_turn_iteration :i]]
                                    :join
                                    [[:session_turn_state :ts] [:= :ts.id :i.session_turn_state_id]
                                     [:session_turn_soul :t] [:= :t.id :ts.session_turn_soul_id]
                                     [:session_state :s] [:= :s.id :t.session_state_id]]
                                    :where [:= :s.session_soul_id (str fork-sid)]})]
            (expect (= 1 (count blobs)))
            (expect (not (str/includes? (pr-str (h/thaw-blob (:tool_calls (first blobs)))) aid1))))
          ;; The fork outlives the source: deleting the source keeps every fork row.
          (db/db-delete-session-tree! store sid)
          (expect (nil? (db/db-activity-page store sid aid1 {})))
          (expect (= 0 (h/raw-count store :activity_invocation [:= :history_id aid1])))
          (expect (= source-rows (all-rows store fork-sid fork-aid {})))
          (expect (= 145
                     (get-in (db/db-activity-page store fork-sid fork-aid {})
                             [:history :total]))))))))
