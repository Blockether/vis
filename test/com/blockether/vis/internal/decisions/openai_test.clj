(ns com.blockether.vis.internal.decisions.openai-test
  "OpenAI decision models behind the one `/v1/systemone` contract.

   Every request goes to a real local HTTP server that stands in for the
   OpenAI Decisions API, so no test needs a key or makes a paid call."
  (:require [charred.api :as json]
            [lazytest.core :refer [defdescribe describe expect it]]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.decisions.core :as decisions]
            [com.blockether.vis.internal.decisions.openai :as openai]
            [com.blockether.vis.internal.provider.service :as provider-service])
  (:import [com.sun.net.httpserver HttpExchange HttpHandler HttpServer]
           [java.net InetSocketAddress]
           [java.nio.charset StandardCharsets]))

(defn- start-openai!
  "A real HTTP server for `/v1/decisions`. `respond` sees the recorded request
   and returns `[status body-map]`. Returns `{:base-url :requests :stop!}`."
  [respond]
  (let [requests
        (atom [])

        server
        (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)]

    (.createContext
      server
      "/v1/decisions"
      (reify
        HttpHandler
          (handle [_ e]
            (let [^HttpExchange exchange
                  e

                  request
                  {:method (.getRequestMethod exchange)
                   :path (.getPath (.getRequestURI exchange))
                   :authorization (.getFirst (.getRequestHeaders exchange) "Authorization")
                   :body (json/read-json (String. (.readAllBytes (.getRequestBody exchange))
                                                  StandardCharsets/UTF_8))}

                  _
                  (swap! requests conj request)

                  [status body]
                  (respond request)

                  bytes
                  (.getBytes ^String (json/write-json-str body) StandardCharsets/UTF_8)]

              (.add (.getResponseHeaders exchange) "Content-Type" "application/json")
              (.sendResponseHeaders exchange (int status) (alength bytes))
              (with-open [out (.getResponseBody exchange)]
                (.write out bytes))))))
    (.start server)
    {:base-url (str "http://127.0.0.1:" (.getPort (.getAddress server)) "/v1")
     :requests requests
     :stop! #(.stop server 0)}))

(defn- with-openai*
  "Run `f` with a fake OpenAI server and a test API key."
  [respond f]
  (let [{:keys [base-url stop!] :as fake} (start-openai! respond)]
    (try (with-redefs [openai/credentials (constantly {:api-key "test-key" :base-url base-url})]
           (f fake))
         (finally (stop!)))))

(def ^:private questions
  (doto (java.util.LinkedHashMap.)
    (.put "intent"
          {"type" "choice"
           "instructions" "Choose a request"
           "criteria" {"refund" "Money back" "repair" ""}})
    (.put "urgency"
          {"type" "score" "instructions" "Rate urgency" "criteria" ["low" "medium" "high"]})
    (.put "refundable"
          {"type" "noul"
           "instructions" "Can the item be refunded?"
           "criteria" {"true" "The policy allows a refund"}})
    (.put "secret" {"type" "choice" "instructions" "Pick" "criteria" ["a" "b"]})))

(def ^:private openai-answers
  {"model" "gpt-6-luna-2026-10-01"
   "answers"
   [{"type" "choice"
     "name" "intent"
     "choice" "refund"
     "confidence" 0.9
     "probabilities" [{"value" "refund" "probability" 0.9} {"value" "repair" "probability" 0.1}]}
    {"type" "score"
     "name" "urgency"
     "score" 1.6
     "confidence" 0.7
     "probabilities" [{"label" "low" "value" 1 "probability" 0.1}
                      {"label" "medium" "value" 2 "probability" 0.2}
                      {"label" "high" "value" 3 "probability" 0.7}]}
    {"type" "predicate" "name" "refundable" "probability" 0.25} {"type" "refusal" "name" "secret"}]
   "usage" {"input_tokens" 42
            "input_tokens_details" {"cached_tokens" 0 "cache_write_tokens" 0}
            "output_tokens" 4
            "output_tokens_details" {"reasoning_tokens" 0}
            "total_tokens" 46}})

(defn- infer
  [model]
  (decisions/infer! {"model" model "state" {"item" "broken lamp"} "questions" questions}))

(defn- failure
  [f]
  (try (f)
       nil
       (catch clojure.lang.ExceptionInfo e {:type (:type (ex-data e)) :message (ex-message e)})))

(defdescribe
  openai-decisions
  (describe
    "one /v1/systemone contract"
    (it
      "sends OpenAI's request shape and returns the local answer shapes"
      (with-openai*
        (constantly [200 openai-answers])
        (fn [{:keys [requests]}]
          (let [result
                (infer "openai/gpt-6-luna")

                request
                (first @requests)]

            (expect (= 1 (count @requests)))
            (expect (= "POST" (:method request)))
            (expect (= "/v1/decisions" (:path request)))
            (expect (= "Bearer test-key" (:authorization request)))
            (expect
              (= {"model" "gpt-6-luna"
                  "input" "{\"item\": \"broken lamp\"}"
                  "questions" [{"type" "choice"
                                "name" "intent"
                                "instructions" "Choose a request"
                                "choices" [{"value" "refund" "description" "Money back"}
                                           {"value" "repair"}]}
                               {"type" "score"
                                "name" "urgency"
                                "instructions" "Rate urgency"
                                "levels" [{"label" "low"} {"label" "medium"} {"label" "high"}]}
                               {"type" "predicate"
                                "name" "refundable"
                                "instructions"
                                "Can the item be refunded?\nTrue means: The policy allows a refund"}
                               {"type" "choice"
                                "name" "secret"
                                "instructions" "Pick"
                                "choices" [{"value" "a"} {"value" "b"}]}]}
                 (:body request)))
            (expect
              (= {"model" "gpt-6-luna-2026-10-01"
                  "routing"
                  {"model" "openai/gpt-6-luna" "model_ref" "openai/gpt-6-luna" "provider" "openai"}
                  "answers" {"intent" {"type" "choice"
                                       "choice" "refund"
                                       "probabilities" {"refund" 0.9 "repair" 0.1}
                                       "confidence" 0.9}
                             "urgency" {"type" "score"
                                        "score" 1.6
                                        "legend" {"0" "low" "1" "medium" "2" "high"}
                                        "probabilities" {"0" 0.1 "1" 0.2 "2" 0.7}
                                        "confidence" 0.7}
                             "refundable" {"type" "noul" "noul" 0.25 "confidence" 0.75}
                             "secret" {"type" "refusal"}}
                  "usage" {"input_tokens" 42 "output_tokens" 4}}
                 result))))))
    (it "keeps the local validation and sends nothing for an invalid request"
        (with-openai* (constantly [200 openai-answers])
                      (fn [{:keys [requests]}]
                        (expect (= :decisions/invalid-request
                                   (:type (failure #(decisions/infer!
                                                      {"model" "openai/gpt-6-luna"
                                                       "state" "x"
                                                       "questions" {"q" {"type" "bool"
                                                                         "instructions" "?"}}})))))
                        (expect (empty? @requests)))))
    (it "rejects answers that do not match the question order"
        (with-openai* (constantly [200 (update openai-answers "answers" (comp vec reverse))])
                      (fn [_]
                        (expect (= :decisions/unavailable
                                   (:type (failure #(infer "openai/gpt-6-luna")))))))))
  (describe "errors"
            (it "needs an OpenAI API key and never uses the Codex sign-in"
                (with-redefs [openai/credentials (constantly nil)]
                  (let [{:keys [type message]} (failure #(infer "openai/gpt-6-luna"))]
                    (expect (= :decisions/provider-unavailable type))
                    (expect (re-find #"OPENAI_API_KEY" message)))))
            (it "maps OpenAI HTTP failures to decision error types without the key"
                (doseq [[status type] [[400 :decisions/invalid-request]
                                       [401 :decisions/provider-unavailable]
                                       [404 :decisions/unknown-model] [429 :decisions/unavailable]
                                       [500 :decisions/unavailable]]]
                  (with-openai* (constantly [status {"error" {"message" "upstream detail"}}])
                                (fn [_]
                                  (let [{actual :type message :message}
                                        (failure #(infer "openai/gpt-6-luna"))]
                                    (expect (= type actual))
                                    (expect (not (re-find #"test-key" message)))))))))
  (describe "credentials and catalog"
            (it "uses the configured openai provider, not openai-codex"
                (with-redefs [config/command-token
                              (constantly nil)

                              provider-service/configured-providers-cached
                              (constantly [{:id :openai-codex :api-key "codex-token"}
                                           {:id :openai
                                            :api-key "sk-config"
                                            :base-url "https://example.test/v1/"}])]

                  (expect (= {:api-key "sk-config" :base-url "https://example.test/v1"}
                             (openai/credentials)))))
            (it "ignores an unresolved ${NAME} key"
                (with-redefs [config/command-token
                              (constantly nil)

                              provider-service/configured-providers-cached
                              (constantly [{:id :openai :api-key "${OPENAI_API_KEY}"}])]

                  (expect (= (not-empty (System/getenv "OPENAI_API_KEY"))
                             (:api-key (openai/credentials))))))
            (it "lists remote rows without an installed key, so gateway warm-up skips them"
                (with-redefs [openai/credentials (constantly nil)]
                  (let [row (some #(when (= "openai/gpt-6-luna" (get % "model_ref")) %)
                                  (decisions/models-status))]
                    (expect (= {"model_ref" "openai/gpt-6-luna"
                                "provider" "openai"
                                "residency" "remote"
                                "available" false}
                               row)))))))
