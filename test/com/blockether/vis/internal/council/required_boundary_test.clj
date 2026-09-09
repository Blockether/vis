(ns com.blockether.vis.internal.council.required-boundary-test
  "Required replies cross the real loop, Python host, session projection and SQLite boundary."
  (:require [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.council.core :as council]
            [com.blockether.vis.internal.council.host :as host]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.session.cancellation :as cancellation]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(deftest required-reply-model-loop-test
  (doseq [first-action [:final :tool]]
    (let [router (svar/make-router [{:id :lmstudio
                                     :api-key "test"
                                     :base-url "http://127.0.0.1:1234/v1"
                                     :models [{:name "model"}]}])
          a (lp/create-environment router {:db :memory})
          db (:db-info a)
          b (lp/create-environment router {:db db})
          aid (str (:session-id a))
          bid (str (:session-id b))
          calls (atom 0)
          prompts (atom [])]

      (try
        (doseq [sid [aid bid]]
          (#'state/update-session!
           sid
           (constantly {:current-turn "fixture"
                        :turns {"fixture" {:status "running"
                                           :cancel-token (cancellation/cancellation-token)}}})))
        (with-redefs [toggles/enabled? (fn [id]
                                         (= "council" id))
                      vis/toggle-enabled? (fn [id]
                                            (= "council" id))]

          (let [active (get (council/runtime db) aid)
                request (council/publish!
                          db
                          #(council/runtime db)
                          {:session-id aid :activation-id (:activation-id active) :source "sdk"}
                          {:content "Do you have evidence for the reported issue?"
                           :ping [bid]
                           :reply_required true})
                tid (ps/db-store-session-turn! db
                                               {:parent-session-id (:session-id b)
                                                :user-request "Continue your task"})
                reply-code (str "pending = session['council']['pending_replies']\n"
                                "assert len(pending) == 1\n"
                                "assert pending[0]['entry_id'] == "
                                (:id request)
                                "\n"
                                "assert pending[0]['due_iteration'] == 1\n"
                                "await council.publish('I do not have evidence.', reply_to="
                                (:id request)
                                ")\n")
                result
                (with-redefs
                  [svar/ask-code!
                   (fn [_ opts]
                     (swap! prompts conj (:messages opts))
                     (let [n (swap! calls inc)
                           code (cond
                                  (= n 2) reply-code
                                  (and (= n 1) (= first-action :tool))
                                  "session['council']['pending_replies'] = []\nprint('unrelated')")]

                       (if code
                         {:stop-reason :tool-calls
                          :tokens {}
                          :tool-calls
                          [{:id (str "reply-" n) :name "python_execution" :input {:code code}}]}
                         {:stop-reason :end :tokens {} :content "done" :tool-calls []})))]
                  (lp/iteration-loop b "Continue your task" {:session-turn-id tid}))
                iterations (ps/db-list-session-turn-iterations db tid)
                request-now (council/get-entry db aid {:entry_id (:id request)})
                reply-id (get-in request-now [:replies 0 :reply_entry_id])]

            (is (= {:answer "done"} (:answer result)))
            (is (true? (:final? (last (:trace result)))))
            (is (= 3 @calls)
                "Neither premature final prose nor an unrelated tool block is accepted")
            (is (str/includes? (pr-str (:forms (first iterations))) "Council reply required"))
            (is (= [(:id request)]
                   (mapv :entry_id (:pending_replies (:council-input (first iterations))))))
            (is (str/includes? (pr-str (first @prompts)) "pending_replies"))
            (is (= "replied" (get-in request-now [:replies 0 :state])))
            (is (pos-int? reply-id))
            (is (= [aid] (:ping (council/get-entry db bid {:entry_id reply-id}))))
            (is (empty? (council/session-pending-replies b)))
            (is (document/valid-json? "council" "session_context" (host/context b)))
            ;; No reply ping was supplied by Python; the requester's next input receives it.
            (let [sender (get (council/runtime db) aid)
                  notification (council/prepare-input! db
                                                       aid
                                                       (:activation-id sender)
                                                       (:group-id sender)
                                                       (:input-state sender)
                                                       ["sender" 0]
                                                       8192)]

              (is (= [reply-id] (mapv :id (:entries notification)))))))
        (finally (#'state/drop-session! aid)
                 (#'state/drop-session! bid)
                 (lp/dispose-environment! b)
                 (lp/dispose-environment! a))))))
