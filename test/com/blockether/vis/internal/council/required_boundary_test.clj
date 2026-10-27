(ns com.blockether.vis.internal.council.required-boundary-test
  "Required replies cross the real loop, Python host, session projection and SQLite boundary."
  (:require [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.council.core :as council]
            [com.blockether.vis.internal.council.host :as host]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.gateway.wiring :as wiring]
            [com.blockether.vis.internal.loop.environment :as loop-env]
            [com.blockether.vis.internal.loop.iteration :as iteration]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.session.agents :as agents]
            [com.blockether.vis.internal.session.cancellation :as cancellation]
            [lazytest.core :refer [defdescribe expect it]]))

(wiring/install!)

(defdescribe
  required-reply-model-loop-test
  (it
    "required reply model loop"
    ;; #182: a plain thread reply must also resolve the obligation across the Python boundary.
    ;; Session 29ac86ff t4: reading and calculating before a reply must not fail tool iterations.
    ;; The reply mode only selects the publish argument, so the modes alternate over the actions.
    (doseq [[first-action reply-mode] (map vector
                                           [:final :tool :read :research :read-final]
                                           (cycle ["reply_to" "thread_id"]))]
      (let [router
            ;; Exercise reply enforcement with room for the full system/Council prompt.
            (svar/make-router [{:id :lmstudio
                                :api-key "test"
                                :base-url "http://127.0.0.1:1234/v1"
                                :models [{:name "model" :context 200000}]}])
            a (loop-env/create-environment router {:db :memory})
            db (:db-info a)
            b (loop-env/create-environment router {:db db})
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
                                           (= "council" id))]
            (let
              [active (get (council/runtime db) aid)
               request (council/publish!
                         db
                         #(council/runtime db)
                         {:session-id aid :activation-id (:activation-id active) :source "sdk"}
                         {:kind "coordination"
                          :content "Do you have evidence for the reported issue?"
                          :ping [bid]
                          :reply_required true})
               tid (ps/db-store-session-turn! db
                                              {:parent-session-id (:session-id b)
                                               :user-request "Continue your task"})
               reply-at (if (#{:research :read-final} first-action) 3 2)
               reply-code
               (str "pending = session['council']['pending_replies']\n"
                    "assert len(pending) == 1\n"
                    "assert pending[0]['entry_id'] == "
                    (:entry_id request)
                    "\n"
                    "assert pending[0]['due_iteration'] == 1\n"
                    "await council.publish('I do not have evidence.', kind='informational', "
                    reply-mode
                    "="
                    (:entry_id request)
                    ")\n")
               result
               (with-redefs
                 [svar/ask-code!
                  (fn [_ opts]
                    (swap! prompts conj (:messages opts))
                    (let
                      [n (swap! calls inc)
                       code
                       (cond
                         (= n reply-at) reply-code
                         (and (= n 1) (= first-action :tool))
                         "session['council']['pending_replies'] = []\nprint('unrelated')"
                         (and (= n 1) (#{:read :research :read-final} first-action))
                         (str "entry = await council.get("
                              (:entry_id request)
                              ")\nprint(entry['content'])")
                         (and (= n 2) (= first-action :research))
                         "assert len(session['council']['pending_replies']) == 1\nprint(sum(range(4)))")]

                      (if code
                        {:stop-reason :tool-calls
                         :tokens {}
                         :tool-calls
                         [{:id (str "reply-" n) :name "python_execution" :input {:code code}}]}
                        {:stop-reason :end :tokens {} :content "done" :tool-calls []})))]
                 (iteration/iteration-loop b "Continue your task" {:session-turn-id tid}))
               iterations (ps/db-list-session-turn-iterations db tid)
               request-now (council/get-entry db aid {:entry_id (:entry_id request)})
               reply-id (get-in request-now [:replies 0 :reply_entry_id])]

              (expect (= {:answer "done"} (:answer result)))
              (expect (true? (:final? (last (:trace result)))))
              (expect (= (inc reply-at) @calls))
              (doseq [[index iteration] (map-indexed vector iterations)]
                (expect
                  (= (or (and (= first-action :final) (zero? index))
                         (and (= first-action :read-final) (= index 1)))
                     (str/includes? (pr-str (:forms iteration)) "Council reply required"))
                  "Only an unanswered final answer is rejected, not read/research/tool iterations"))
              (expect (= [(:entry_id request)]
                         (mapv :entry_id (:pending_replies (:council-input (first iterations))))))
              (expect (str/includes? (pr-str (first @prompts)) "pending_replies"))
              (expect (= "replied" (get-in request-now [:replies 0 :state])))
              (expect (pos-int? reply-id))
              (expect (= [aid] (:ping (council/get-entry db bid {:entry_id reply-id}))))
              (expect (empty? (council/session-pending-replies b)))
              (expect (document/valid-json? "council" "session_context" (host/context b)))
              ;; No reply ping was supplied by Python; the requester's next input receives it.
              (let [sender (get (council/runtime db) aid)
                    notification (council/prepare-input! db
                                                         aid
                                                         (:activation-id sender)
                                                         (:group-id sender)
                                                         (:input-state sender)
                                                         ["sender" 0]
                                                         8192)]

                (expect (= [reply-id] (mapv :entry_id (:entries notification)))))))
          (finally (#'state/drop-session! aid)
                   (#'state/drop-session! bid)
                   (loop-env/dispose-environment! b)
                   (loop-env/dispose-environment! a)))))))

;; A subagent question that only the person can decide: an interactive leader asks the person
;; and its turn ends suspended. A one-shot `:cli` leader still has to answer the question itself.
(defdescribe
  person-decides-child-question-test
  (doseq [[channel interactive?] [[nil true] [:cli false]]]
    (it
      (str "ends the turn " (if interactive? "suspended for the person" "only after a reply"))
      (let [router (svar/make-router [{:id :lmstudio
                                       :api-key "test"
                                       :base-url "http://127.0.0.1:1234/v1"
                                       :models [{:name "model" :context 200000}]}])
            leader (loop-env/create-environment router
                                                (cond-> {:db :memory}
                                                  channel
                                                  (assoc :channel channel)))
            db (:db-info leader)
            child (loop-env/create-environment router {:db db})
            lid (str (:session-id leader))
            cid (str (:session-id child))
            calls (atom 0)]

        (try
          (doseq [sid [lid cid]]
            (#'state/update-session!
             sid
             (constantly {:current-turn "fixture"
                          :turns {"fixture" {:status "running"
                                             :cancel-token (cancellation/cancellation-token)}}})))
          (with-redefs [toggles/enabled? (fn [id]
                                           (= "council" id))]
            (let [active (get (council/runtime db) cid)
                  question (council/publish!
                             db
                             #(council/runtime db)
                             {:session-id cid :activation-id (:activation-id active) :source "sdk"}
                             {:kind "coordination"
                              :content "Keep the old API or remove it?"
                              :ping [lid]
                              :reply_required true})
                  entry-id (:entry_id question)
                  tid (ps/db-store-session-turn! db
                                                 {:parent-session-id (:session-id leader)
                                                  :user-request "Lead the team"})
                  result
                  (with-redefs [agents/open-questions
                                (fn [_ sid]
                                  (if (= lid (str sid)) [{:session_id cid :entry_id entry-id}] []))
                                svar/ask-code!
                                (fn [_ _]
                                  (let [n (swap! calls inc)]
                                    (if (and (not interactive?) (= n 2))
                                      {:stop-reason :tool-calls
                                       :tokens {}
                                       :tool-calls
                                       [{:id "reply"
                                         :name "python_execution"
                                         :input {:code (str "await council.publish('Keep it.', "
                                                            "kind='informational', reply_to="
                                                            entry-id
                                                            ")")}}]}
                                      {:stop-reason :end
                                       :tokens {}
                                       :content
                                       (if interactive? "Keep the old API or remove it?" "done")
                                       :tool-calls []})))]

                    (iteration/iteration-loop leader "Lead the team" {:session-turn-id tid}))
                  replies (:replies (council/get-entry db lid {:entry_id entry-id}))]

              (if interactive?
                (do
                  (expect (= 1 @calls) "The gate accepts the question for the person")
                  (expect (= {:vis/answer-mode :needs-input
                              :answer/text "Keep the old API or remove it?"}
                             (:answer result))
                          "A needs-input answer ends the gateway turn as suspended")
                  (expect (nil? (some :reply_entry_id replies)))
                  (expect (= [entry-id]
                             (mapv :entry_id
                                   (with-redefs [agents/open-questions (constantly [{:session_id cid
                                                                                     :entry_id
                                                                                     entry-id}])]
                                     (council/session-pending-replies leader))))
                          "The question stays pending")
                  ;; The next turn has a new activation: the ended one interrupted its pings.
                  (ps/db-council-interrupt! db lid (:activation-id (get (council/runtime db) lid)))
                  (let [batch (with-redefs [agents/open-questions
                                            (constantly [{:session_id cid :entry_id entry-id}])]
                                (council/prepare-input! db
                                                        lid
                                                        "next-turn"
                                                        (council/default-group db lid)
                                                        (atom {})
                                                        ["next" 0]
                                                        8192))]
                    (expect (= [entry-id] (mapv :entry_id (:pending_replies batch)))
                            "The next turn of the leader still has to answer the question")))
                (do (expect (= 3 @calls) "A :cli leader must reply before it ends its turn")
                    (expect (= {:answer "done"} (:answer result)))
                    (expect (some :reply_entry_id replies))))))
          (finally (#'state/drop-session! lid)
                   (#'state/drop-session! cid)
                   (loop-env/dispose-environment! child)
                   (loop-env/dispose-environment! leader)))))))
