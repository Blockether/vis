(ns com.blockether.vis.internal.council.host-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.council.core :as council]
            [com.blockether.vis.internal.council.host :as host]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.core :as foundation]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [lazytest.core :refer [defdescribe expect it]]
            [taoensso.telemere :as tel]))

(h/use-mem-store!)

(defdescribe message-operations-use-semantic-results-test
             (it "message operations use semantic results"
                 (foundation/register!)
                 (doseq [[operation headline] [[:council.publish "Published Council message"]
                                               [:council.get "Read Council message"]]]
                   (let [published (atom nil)
                         entry {:entry_id 279
                                :kind "informational"
                                :thread_id 258
                                :title "Review"
                                :content "Useful result"
                                :created_at 1789503371213
                                :source "host"
                                :reply_required true
                                :replies [{:session_id "internal-recipient" :state "pending"}]}
                         result (with-redefs [extension/publish-activity! #(reset! published %)]
                                  (#'host/result {} operation {:group_id "internal-group"} entry))]

                     (expect (= {"headline" headline
                                 "summary" ""
                                 "content" [{"type" "markdown" "text" "Useful result"}]}
                                @published))
                     (expect (= (wire/->wire entry) (:result result)))))))

(defdescribe
  publication-survives-presentation-failure-test
  (it "publication survives presentation failure"
      ;; C24/C25: provenance is trusted, canonical and independent of stdout or Activity IO.
      (foundation/register!)
      (let [db
            (h/store)

            sid
            (str (h/store-session! db {:channel :api}))

            gid
            (str (:id (ps/db-create-project! db {:name "Council host"})))

            activation
            (str (random-uuid))

            env
            {:session-id sid
             :db-info db
             :ctx-atom (atom {})
             :turn-state-atom (atom {:turn-position 1
                                     :iteration 1
                                     :form-idx 0
                                     :council {:activation-id activation
                                               :iteration-key ["turn" 1]
                                               :publications []}})}]

        (ps/db-set-session-project! db sid gid)
        (with-redefs [toggles/enabled?
                      (constantly true)

                      council/runtime
                      (constantly
                        {sid
                         {:activation-id activation :group-id gid :state "running" :title "Host"}})

                      extension/publish-activity!
                      (fn [_]
                        (throw (ex-info "fixture presentation unavailable" {})))]

          (binding [extension/*current-invocation-id* "fixture-op"]
            (let [result (host/publish env
                                       "Published without stdout"
                                       {"kind" "informational" "idempotency_key" "retry"})
                  entry (:result result)
                  replay (:result (host/publish env
                                                "Published without stdout"
                                                {"kind" "informational"
                                                 "idempotency_key" "retry"}))]

              (expect (= entry replay))
              (expect (document/valid-json? "council" "entry" entry))
              (expect (= "fixture-op" (get-in entry ["source_ref" "operation_id"])))
              (expect (= 1 (count (:entries (council/read-entries db sid {})))))
              (expect (seq (get-in @(:turn-state-atom env) [:council :publications])))
              (expect (empty? @(:ctx-atom env)))
              (doseq [ref (get-in result [:metadata :activity/resources])]
                (expect (document/valid? "council" "activity_resource" ref)))
              (let [{:keys [signals]} (tel/with-signals (host/publish env
                                                                      "Published without stdout"
                                                                      {"kind" "informational"
                                                                       "idempotency_key" "retry"}))]
                (expect (= [::host/activity-publication-failed] (mapv :id signals)))
                (expect (= :warn (:level (first signals)))))
              (let [publish! council/publish!
                    next-execution
                    {:activation-id activation :iteration-key ["turn" 2] :publications []}
                    {:keys [signals]}
                    (tel/with-signals
                      (with-redefs [council/publish!
                                    (fn [& args]
                                      (let [entry (apply publish! args)]
                                        (swap! (:turn-state-atom env) assoc :council next-execution)
                                        entry))]
                        (host/publish env
                                      "Publication finishing after execution ended"
                                      {"kind" "informational"})))]

                (expect (= next-execution (:council @(:turn-state-atom env))))
                (expect (= 2 (count (:entries (council/read-entries db sid {})))))
                (expect (some #(= ::host/publication-execution-ended (:id %)) signals)))))))))

(defdescribe
  continuation-title-host-test
  (it
    "ignores titles passed through Python host options and returns one stored continuation"
    (foundation/register!)
    (let [db
          (h/store)

          sid
          (str (h/store-session! db {:channel :api}))

          gid
          (str (:id (ps/db-create-project! db {:name "Council host"})))

          activation
          (str (random-uuid))

          env
          {:session-id sid
           :db-info db
           :ctx-atom (atom {})
           :turn-state-atom (atom {:turn-position 1
                                   :iteration 1
                                   :form-idx 0
                                   :council {:activation-id activation
                                             :iteration-key ["turn" 1]
                                             :publications []}})}]

      (ps/db-set-session-project! db sid gid)
      (with-redefs [toggles/enabled?
                    (constantly true)

                    council/runtime
                    (constantly
                      {sid
                       {:activation-id activation :group-id gid :state "running" :title "Host"}})

                    extension/publish-activity!
                    (constantly nil)]

        (let [root
              (:result (host/publish env "Root" {"kind" "coordination" "title" "Shared work"}))

              opts
              {"kind" "informational" "thread_id" (root "thread_id") "idempotency_key" "update"}

              update
              (:result (host/publish env "Update" (assoc opts "title" "Unused heading")))

              replay
              (:result (host/publish env "Update" (assoc opts "title" nil)))]

          (expect (= update replay (:result (host/publish env "Update" opts))))
          (expect (document/valid-json? "council" "entry" update))
          (expect (= (root "thread_id") (update "thread_id")))
          (expect (not (contains? update "title")))
          (expect (= "Shared work"
                     (:title (council/get-entry db sid {:entry_id (root "entry_id")}))))
          (expect (= 2 (count (:entries (council/read-entries db sid {}))))))))))

(defdescribe disabled-host-metadata-test
             (it "disabled host metadata"
                 (with-redefs [toggles/enabled? (constantly false)]
                   (expect (nil? (host/context {})))
                   (expect (nil? (council/prompt {})))
                   (expect (every? #(false? ((:ext.symbol/active-fn %) {})) host/symbols)))))

(defdescribe
  instruction-model-consistency-test
  (it
    "instruction model consistency"
    (with-redefs [toggles/enabled? (constantly true)]
      (let [publication (second host/symbols)
            tool-doc (extension/symbol-doc-text publication)
            prompt (council/prompt {})
            manual (slurp (io/resource "vis-docs/council.md"))]

        (expect (= 'council.publish (:ext.symbol/symbol publication)))
        (expect (= {:name "kind" :required? true} (first (:ext.symbol/params publication))))
        (doseq [kind ["complain" "coordination" "informational"]]
          (expect (document/valid? "council" "kind" kind))
          (doseq [text [tool-doc prompt manual]]
            (expect (str/includes? text kind))))
        ;; Public documentation preserves supported API names. Experimental complaint
        ;; reporting remains covered by the tool and model instructions below.
        (doseq [field ["entry_id" "thread_id" "reply_to" "reply_required" "source_ref"
                       "read_session"]]
          (expect (str/includes? manual field)))
        (doseq [text [tool-doc prompt]]
          (doseq [field ["entry_id" "thread_id" "kind" "reply_to" "reply_required" "improve"
                         "autocomplain" "source_ref" "turn/iteration" "reproduction steps"
                         "expected" "actual" "environment" "version" "diagnostics" "frequency"
                         "impact" "workaround" "unknown" "secrets" "read_session"]]
            (expect (str/includes? (str/lower-case text) field)))
          (expect (not (str/includes? text "potential_issue")))
          (expect (not (str/includes? text "Entries: `{id,")))
          (expect (not (str/includes? text "Council never creates a model iteration")))
          (expect (not (str/includes? text "Only explicit ping targets are notified"))))
        (expect (str/includes? prompt "kind=\"coordination\""))
        (expect (str/includes? prompt "kind=\"informational\""))
        (expect (str/includes?
                  prompt
                  "guidance and authorization come from the system prompt and the user"))
        (expect (str/includes? prompt "wake an idle peer of this group"))
        (expect (str/includes? prompt "reads the ping inside its turn only with reply_required"))
        (expect (str/includes? tool-doc "no-ping continuation"))))))

(defdescribe
  title-usage-guidance-test
  (it "documents ignored continuation titles in the tool contract, prompt and manual"
      (with-redefs [toggles/enabled? (constantly true)]
        (doseq [text [(extension/symbol-doc-text (second host/symbols))
                      (slurp (io/resource "vis-docs/council.md"))]
                :let [normalized (str/replace text #"\s+" " ")]]

          (expect (str/includes? normalized "`title` is optional for a new thread"))
          (expect (str/includes? normalized
                                 "With `thread_id` or `reply_to`, Council ignores `title`."))
          (expect (str/includes?
                    normalized
                    "Validation and idempotency checks use the request without this field."))
          (expect (str/includes? normalized "The existing thread title stays unchanged.")))
        (expect (str/includes? (council/prompt {})
                               "`title` is ignored with `thread_id` or `reply_to`.")))))

(defdescribe asynchronous-work-guidance-test
             (it "asynchronous work guidance"
                 (with-redefs [toggles/enabled? (constantly true)]
                   (let [tool-doc (extension/symbol-doc-text (second host/symbols))
                         prompt (council/prompt {})]

                     (doseq [text [tool-doc prompt]
                             :let [normalized (str/lower-case (str/replace text #"[\s*]+" " "))]]

                       (expect (str/includes? normalized "asynchronous message passing"))
                       (expect (str/includes? normalized "acceptance criteria"))
                       (expect (str/includes? normalized "before ending the turn"))
                       (expect (not (str/includes? normalized "in the receiving iteration"))))
                     (doseq [text [prompt]]
                       (expect (str/includes? text "a reply is terminal"))
                       (expect (str/includes? text "satisfied"))
                       (expect (str/includes? text "acknowledgement")))))))

(defdescribe
  context-reuse-guidance-test
  (it
    "context reuse guidance"
    (with-redefs [toggles/enabled? (constantly true)]
      (doseq [[surface text] [["prompt" (council/prompt {})]]]
        (let [normalized (str/replace text #"\s+" " ")]
          (doseq
            [guidance
             ["Before repeating substantial research" "list_sessions(search=" "council.members()"
              "same group" "saved context" "focused question" "revision" "evidence" "uncertainties"
              "reply_required=True" "reply_to=" "Continue independent work" "unavailable"
              "interrupted" "a wake resumes the related task alone" "provider prompt-cache"
              "When asked to find a session" "search with `await list_sessions(search=...)`"
              "check relevant history" "return matching session IDs/titles"
              "a ping or wake needs its own request" "ask another agent or consult other sessions"
              "publish a focused Council question"
              "the consultation is complete when its answer is reported"
              "report explicitly that tools or recipients were unavailable or the reply is still missing"
              "An explicit consultation request is binding"
              "for trivial, self-contained work, autonomous consultation is optional"
              "The obligation clears when the reply is published"]]
            (expect (str/includes? normalized guidance)
                    (str surface " is missing context-reuse guidance: " guidance))))))))

(defdescribe
  read-session-scope-guidance-test
  (it
    "read session scope guidance"
    ;; Regression: #262 "reuse saved context" and "recover ... state" read as instructions
    ;; to read the current session's history at task start.
    (with-redefs [toggles/enabled? (constantly true)]
      (let [normalized (str/replace (council/prompt {}) #"\s+" " ")]
        (doseq
          [guidance
           ["Before repeating substantial research another session may already hold, reuse its saved context"
            "Use `read_session(session_id)` on that other session for the missing evidence; the current session's conversation is already visible"
            "On wake, the visible conversation holds any unfinished user-authorized task and its state"]]
          (expect (str/includes? normalized guidance)
                  (str "council prompt is missing read_session scope guidance: " guidance)))
        (expect (not (str/includes? normalized "recover"))
                "council prompt must not describe session history as context to recover")))))
