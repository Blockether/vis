(ns com.blockether.vis.tui.automations-test
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.tui.automations :as automations]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.input :as input]
            [lazytest.core :refer [defdescribe describe expect it]])
  (:import))

(def ^:private webhook-automation
  {"id" "a-1"
   "name" "Triage issues"
   "enabled" true
   "triggers" [{"kind" "webhook" "signature" "github"}
               {"kind" "cron" "expression" "0 8 * * 1-5" "timezone" "Europe/Warsaw"}]
   "prompt" "Label the new issue."
   "target" {"mode" "new"}
   "delivery" {"push" true "callback" {"url" "https://gateway.example.com/done"}}
   "deliver_only" false
   "next_run_at" 1767250800000
   "webhook" {"path" "/v1/hooks/a-1"}
   "secrets" {"webhook" true "callback" false}
   "last_run" {"status" "completed" "created_at" 1767250800000}})

(def ^:private plain-automation
  {"id" "a-2"
   "name" "Daily digest"
   "enabled" false
   "triggers" [{"kind" "every" "seconds" 3600}]
   "prompt" "Summarize the day."
   "target" {"mode" "temporary"}
   "delivery" {"push" false "callback" nil}
   "next_run_at" nil
   "webhook" nil
   "secrets" {"webhook" false "callback" false}
   "last_run" nil})

(defn- response [status body] {:status status :body (wire/json-str body)})

(defdescribe presentation-test
             (describe "actions"
                       (it "offers secrets only for a webhook trigger or a callback"
                           (expect (= [:details :run :toggle :runs :webhook-secret :callback-secret
                                       :delete]
                                      (mapv :id (automations/actions webhook-automation))))
                           (expect (= [:details :run :toggle :runs :delete]
                                      (mapv :id (automations/actions plain-automation))))))
             (describe "runs-markdown"
                       (it "puts one run on each table row and escapes pipes"
                           (let [md (automations/runs-markdown [{"created_at" 1767250800000
                                                                 "trigger" "manual"
                                                                 "status" "completed"
                                                                 "answer" "a | b\nc"}])]
                             (expect (str/includes? md "| manual | completed | a \\| b c |"))))))

(defdescribe gateway-test
             (it "reads the automations and reports the gateway error message"
                 (with-redefs [vis/request! (fn [method path _]
                                              (expect (= [:get "/v1/automations"] [method path]))
                                              (response 200 {"automations" []}))]
                   (expect (= {:body {"automations" []}} (automations/fetch))))
                 (with-redefs [vis/request!
                               (fn [& _]
                                 (response 409 {"error" {"type" "off" "message" "Turned off"}}))]
                   (expect (= {:error "Turned off"} (automations/fetch))))
                 (with-redefs [vis/request! (fn [& _]
                                              (throw (ex-info "down" {})))]
                   (expect (= {:error "The gateway did not answer"} (automations/fetch))))))

(defn- view-run
  "Run the view with a scripted list choice and action; return the gateway calls,
   the notices and the viewer texts."
  [automation action]
  (let [calls
        (atom [])

        notices
        (atom [])

        viewed
        (atom [])

        lists
        (atom 0)]

    (with-redefs [vis/request!
                  (fn [method path opts]
                    (swap! calls conj [method path (:body opts)])
                    (cond (= [:get "/v1/automations"] [method path])
                          (response 200 {"automations" [automation]})
                          (str/ends-with? path "/secrets")
                          (response 201 {"kind" "webhook" "secret" "whsec_new"})
                          (str/starts-with? path "/v1/automations/runs") (response 200 {"runs" []})
                          :else (response 200 automation)))

                  vis/notify!
                  (fn [text & _]
                    (swap! notices conj text))

                  dlg/list-dialog!
                  (fn [_ title items _]
                    (expect (= "Automations" title))
                    (when (= 1 (swap! lists inc)) (first items)))

                  dlg/select-dialog!
                  (fn [_ _ items]
                    (some #(when (= action (:id %)) %) items))

                  dlg/confirm-dialog!
                  (constantly true)

                  dlg/markdown-viewer-dialog!
                  (fn [_ title md]
                    (swap! viewed conj [title md]))

                  input/clipboard-copy!
                  (constantly true)]

      (automations/show! nil))
    {:calls @calls :notices @notices :viewed @viewed}))

(defdescribe
  view-test
  (it "pauses an automation and reads the list again"
      (let [{:keys [calls]} (view-run webhook-automation :toggle)]
        (expect (= [[:get "/v1/automations" nil] [:patch "/v1/automations/a-1" {"enabled" false}]
                    [:get "/v1/automations" nil]]
                   calls))))
  (it "starts a run now"
      (let [{:keys [calls]} (view-run plain-automation :run)]
        (expect (some #{[:post "/v1/automations/a-2/run" {}]} calls))))
  (it "shows a new secret only in the viewer"
      (let [{:keys [calls notices viewed]} (view-run webhook-automation :webhook-secret)]
        (expect (some #{[:post "/v1/automations/a-1/secrets" {"kind" "webhook"}]} calls))
        (expect (= [] notices))
        (expect (str/includes? (second (first viewed)) "`whsec_new`"))))
  (it "deletes an automation after the confirmation"
      (let [{:keys [calls]} (view-run plain-automation :delete)]
        (expect (some #{[:delete "/v1/automations/a-2" nil]} calls))))
  (it "shows the runs of an automation"
      (let [{:keys [calls viewed]} (view-run plain-automation :runs)]
        (expect (some #{[:get "/v1/automations/runs?automation_id=a-2&limit=20" nil]} calls))
        (expect (= [["Daily digest · Runs" "No runs yet."]] viewed)))))
