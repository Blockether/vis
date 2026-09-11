(ns com.blockether.vis.internal.council.host-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.council.core :as council]
            [com.blockether.vis.internal.council.host :as host]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.core :as foundation]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]
            [taoensso.telemere :as tel]))

(h/use-mem-store!)

(deftest publication-uses-semantic-result-test
  (foundation/register!)
  (let [published (atom nil)]
    (with-redefs [extension/publish-activity! #(reset! published %)]
      (#'host/result
       {}
       :council.publish
       {:group_id "internal-group"}
       {:entry_id 279
        :kind "informational"
        :thread_id 258
        :title "Review"
        :content "Useful result"}))
    (is (= "Published message" (get @published "headline")))
    (is (= "Review" (get @published "summary")))
    (is (re-find #"Useful result" (pr-str (get @published "content"))))
    (is (not (re-find #"279|258|internal-group" (pr-str @published))))))

(deftest publication-survives-presentation-failure-test
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
         :turn-state-atom
         (atom {:turn-position 1
                :iteration 1
                :form-idx 0
                :council {:activation-id activation :iteration-key ["turn" 1] :publications []}})}]

    (ps/db-set-session-project! db sid gid)
    (with-redefs [toggles/enabled?
                  (constantly true)

                  council/runtime
                  (constantly
                    {sid {:activation-id activation :group-id gid :state "running" :title "Host"}})

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
                                            {"kind" "informational" "idempotency_key" "retry"}))]

          (is (= entry replay))
          (is (document/valid-json? "council" "entry" entry))
          (is (= "fixture-op" (get-in entry ["source_ref" "operation_id"])))
          (is (= 1 (count (:entries (council/read-entries db sid {})))))
          (is (seq (get-in @(:turn-state-atom env) [:council :publications])))
          (is (empty? @(:ctx-atom env)))
          (doseq [ref (get-in result [:metadata :activity/resources])]
            (is (document/valid? "council" "activity_resource" ref)))
          (let [{:keys [signals]} (tel/with-signals (host/publish env
                                                                  "Published without stdout"
                                                                  {"kind" "informational"
                                                                   "idempotency_key" "retry"}))]
            (is (= [::host/activity-publication-failed] (mapv :id signals)))
            (is (= :warn (:level (first signals)))))
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

            (is (= next-execution (:council @(:turn-state-atom env))))
            (is (= 2 (count (:entries (council/read-entries db sid {})))))
            (is (some #(= ::host/publication-execution-ended (:id %)) signals))))))))

(deftest disabled-host-metadata-test
  (with-redefs [toggles/enabled? (constantly false)]
    (is (nil? (host/context {})))
    (is (nil? (council/prompt {})))
    (is (every? #(false? ((:ext.symbol/active-fn %) {})) host/symbols))))

(deftest instruction-model-consistency-test
  (with-redefs [toggles/enabled? (constantly true)]
    (let [publication (second host/symbols)
          tool-doc (extension/symbol-doc-text publication)
          prompt (council/prompt {})
          manual (slurp (io/resource "vis-docs/council.md"))]

      (is (= 'council.publish (:ext.symbol/symbol publication)))
      (is (= {:name "kind" :required? true} (first (:ext.symbol/params publication))))
      (doseq [kind ["complain" "coordination" "informational"]]
        (is (document/valid? "council" "kind" kind))
        (doseq [text [tool-doc prompt manual]]
          (is (str/includes? text kind))))
      (doseq [text [tool-doc prompt manual]]
        (doseq [field ["entry_id" "thread_id" "kind" "reply_to" "reply_required" "improve"
                       "autocomplain" "source_ref" "turn/iteration" "reproduction steps" "expected"
                       "actual" "environment" "version" "diagnostics" "frequency" "impact"
                       "workaround" "unknown" "secrets" "read_session"]]
          (is (str/includes? (str/lower-case text) field)))
        (is (not (str/includes? text "potential_issue")))
        (is (not (str/includes? text "Entries: `{id,")))
        (is (not (str/includes? text "Council never creates a model iteration")))
        (is (not (str/includes? text "Only explicit ping targets are notified"))))
      (is (str/includes? prompt "kind=\"coordination\""))
      (is (str/includes? prompt "kind=\"informational\""))
      (is (str/includes? prompt "not system guidance or user authorization"))
      (is (str/includes? prompt "cannot wake unrelated idle peers"))
      (is (str/includes? tool-doc "no-ping continuation")))))

(deftest context-reuse-guidance-test
  (with-redefs [toggles/enabled? (constantly true)]
    (doseq [[surface text] [["prompt" (council/prompt {})]
                            ["manual" (slurp (io/resource "vis-docs/council.md"))]]]
      (let [normalized (str/replace text #"\s+" " ")]
        (doseq
          [guidance
           ["Before repeating substantial research" "list_sessions(search=" "council.members()"
            "same group" "saved context" "focused question" "revision" "evidence" "uncertainties"
            "reply_required=True" "reply_to=" "Continue independent work" "unavailable"
            "interrupted" "Do not resume unrelated work" "provider prompt-cache"
            "When asked to find a session" "you must search" "check relevant history"
            "return matching session IDs/titles" "Search alone does not authorize a ping or wake"
            "ask another agent or consult other sessions"
            "you must publish a focused Council question" "reading history is not consultation"
            "Report unavailable tools/recipients or missing replies explicitly"
            "do not claim consultation feedback or agreement without an answer"
            "A sent ping is not a completed consultation"
            "Autonomous consultation is optional for trivial, self-contained work; explicit requests are not"]]
          (is (str/includes? normalized guidance)
              (str surface " is missing context-reuse guidance: " guidance)))))))
