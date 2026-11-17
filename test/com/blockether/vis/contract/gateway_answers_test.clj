(ns com.blockether.vis.contract.gateway-answers-test
  "Real gateway answers in `fixtures/gateway-answers.json` match the response
   schemas of their operations (#487)."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.gateway :as gateway]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private fixture
  (json/read-str (slurp (io/resource "vis-contract/fixtures/gateway-answers.json"))))

(defn- operation-of
  "The contract operation of a fixture key such as `GET /v1/projects`."
  [k]
  (let [[method path] (str/split k #" " 2)]
    (gateway/operation (keyword (str/lower-case method)) path)))

(def ^:private answers-without-fixture
  "JSON operations that have no recorded real answer yet. This set can only get
   smaller: add a real answer to the fixture, then remove its key here."
  #{"DELETE /v1/automations/:automation-id" "DELETE /v1/clients/:cid"
    "DELETE /v1/decisions/training/jobs/:job-id" "DELETE /v1/devices/:token"
    "DELETE /v1/mcp/servers/:name" "DELETE /v1/providers/:provider-id"
    "DELETE /v1/sessions/:sid/client-extensions" "DELETE /v1/sessions/:sid/speech/jobs/:job-id"
    "DELETE /v1/sessions/:sid/turns/:tid" "DELETE /v1/sessions/:sid/voice/jobs/:job-id"
    "DELETE /v1/speech/jobs/:job-id" "DELETE /v1/speech/voices/:voice-id"
    "DELETE /v1/voice/jobs/:job-id" "GET /healthz" "GET /openapi.json" "GET /readyz"
    "GET /v1/apps/:view-id" "GET /v1/automations" "GET /v1/automations/:automation-id"
    "GET /v1/automations/runs" "GET /v1/automations/runs/:run-id" "GET /v1/decisions/aliases/:alias"
    "GET /v1/decisions/models/:model-ref" "GET /v1/decisions/training/jobs/:job-id"
    "GET /v1/session-groups" "GET /v1/sessions" "GET /v1/sessions/:sid"
    "GET /v1/sessions/:sid/activity/:aid" "GET /v1/sessions/:sid/agents/:child-id/timeline"
    "GET /v1/sessions/:sid/client-calls" "GET /v1/sessions/:sid/context"
    "GET /v1/sessions/:sid/council/entries/:entry-id" "GET /v1/sessions/:sid/fs/file"
    "GET /v1/sessions/:sid/iterations/:iid/attachments/:idx/link"
    "GET /v1/sessions/:sid/speech/jobs/:job-id" "GET /v1/sessions/:sid/views/live/:view-id"
    "GET /v1/sessions/:sid/views/live/:view-id/log" "GET /v1/sessions/:sid/voice/jobs/:job-id"
    "GET /v1/settings" "GET /v1/settings/:id" "GET /v1/speech/jobs/:job-id"
    "GET /v1/voice/jobs/:job-id" "GET /v1/web/brand" "PATCH /v1/automations/:automation-id"
    "PATCH /v1/router" "PATCH /v1/sessions/:sid" "PATCH /v1/sessions/:sid/turns/:tid"
    "PATCH /v1/settings" "PATCH /v1/speech/voices/:voice-id" "POST /v1/admin/drain"
    "POST /v1/admin/stop" "POST /v1/apps" "POST /v1/apps/:view-id/actions" "POST /v1/automations"
    "POST /v1/automations/:automation-id/run" "POST /v1/automations/:automation-id/secrets"
    "POST /v1/decisions/models" "POST /v1/decisions/training/jobs" "POST /v1/devices"
    "POST /v1/devices/actions/test" "POST /v1/evals/agreement" "POST /v1/evals/compare"
    "POST /v1/evals/diagnose" "POST /v1/evals/labels" "POST /v1/evals/leaderboard"
    "POST /v1/evals/report" "POST /v1/extensions/reload" "POST /v1/fs/actions/mkdir"
    "POST /v1/hooks/:automation-id" "POST /v1/mcp/servers"
    "POST /v1/mcp/servers/:name/actions/enable" "POST /v1/mcp/servers/:name/actions/kill"
    "POST /v1/mcp/servers/:name/actions/start" "POST /v1/mcp/servers/:name/auth/cancel"
    "POST /v1/mcp/servers/:name/auth/complete" "POST /v1/mcp/servers/:name/auth/logout"
    "POST /v1/mcp/servers/:name/auth/poll" "POST /v1/mcp/servers/:name/auth/start"
    "POST /v1/mcp/servers/actions/test" "POST /v1/providers"
    "POST /v1/providers/:provider-id/auth/cancel" "POST /v1/providers/:provider-id/auth/complete"
    "POST /v1/providers/:provider-id/auth/poll" "POST /v1/providers/:provider-id/auth/start"
    "POST /v1/providers/:provider-id/logout" "POST /v1/providers/:provider-id/reset-credits/consume"
    "POST /v1/sessions" "POST /v1/sessions/:sid/agents" "POST /v1/sessions/:sid/agents/answer"
    "POST /v1/sessions/:sid/agents/cancel" "POST /v1/sessions/:sid/agents/deny"
    "POST /v1/sessions/:sid/agents/extend" "POST /v1/sessions/:sid/agents/grant"
    "POST /v1/sessions/:sid/agents/route" "POST /v1/sessions/:sid/agents/run"
    "POST /v1/sessions/:sid/apps" "POST /v1/sessions/:sid/attachments"
    "POST /v1/sessions/:sid/cancel-current" "POST /v1/sessions/:sid/client-calls/:call_id/activity"
    "POST /v1/sessions/:sid/client-calls/:call_id/result" "POST /v1/sessions/:sid/council/entries"
    "POST /v1/sessions/:sid/council/wake" "POST /v1/sessions/:sid/fs/actions/open"
    "POST /v1/sessions/:sid/keybindings/run" "POST /v1/sessions/:sid/resources/stop"
    "POST /v1/sessions/:sid/speech" "POST /v1/sessions/:sid/turns"
    "POST /v1/sessions/:sid/turns/:tid/cancel" "POST /v1/sessions/:sid/views/:view-id/actions"
    "POST /v1/sessions/:sid/voice" "POST /v1/sessions/actions/import" "POST /v1/settings"
    "POST /v1/settings/:id/preview" "POST /v1/speech" "POST /v1/speech/model"
    "POST /v1/speech/voices" "POST /v1/systemone" "POST /v1/voice" "POST /v1/voice/model"
    "POST /v1/web/launches" "POST /v1/web/launches/redeem" "PUT /v1/decisions/aliases/:alias"
    "PUT /v1/mcp/servers/:name" "PUT /v1/sessions/:sid/client-extensions"
    "PUT /v1/sessions/:sid/iterations/:iid/attachments/:idx/agent-note" "PUT /v1/web/brand"})

(defdescribe recorded-answers-match-their-response-schema
             (it "validates each recorded 2xx answer against its operation schema"
                 (doseq [[k body] (get fixture "answers")]
                   (let [schema-name (some-> (operation-of k)
                                             :response-schema
                                             name)]
                     (expect (some? schema-name) k)
                     (expect (nil? (document/explain-json "gateway" schema-name body))
                             (pr-str k (document/explain-json "gateway" schema-name body))))))
             (it "validates each recorded 4xx answer against the shared error schema"
                 (doseq [[k {:strs [status body]}] (get fixture "errors")]
                   (expect (<= 400 (long status) 599) k)
                   (expect (document/valid-json? "gateway" "error_response" body) k))))

(defdescribe each-json-operation-has-a-recorded-answer
             (it "has a recorded answer for each JSON operation, except the shrinking list"
                 (let [json-operations
                       (set (for [{:keys [path operations]}
                                  gateway/route-table

                                  [method {:keys [response]}]
                                  operations

                                  :when (= :json response)]

                              (str (str/upper-case (name method)) " " path)))

                       recorded
                       (set (keys (get fixture "answers")))]

                   (expect (empty? (remove json-operations recorded))
                           (pr-str (remove json-operations recorded)))
                   (expect (= answers-without-fixture (set (remove recorded json-operations)))
                           (pr-str (set/difference (set (remove recorded json-operations))
                                                   answers-without-fixture))))))
