(ns com.blockether.vis.internal.loop.trajectory-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.loop.trajectory :as trajectory]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.io File)
           (java.nio.file Files LinkOption Path)
           (java.nio.file.attribute FileAttribute PosixFilePermissions)))

(def ^:private result
  "One CLI result in the engine shape: a code iteration, then the final answer."
  {:session-id "session-1"
   :content [{"id" "block_1" "type" "prose" "markdown" "The answer is 2."}]
   :trace
   [{:iteration 0
     :thinking "Add the numbers."
     :assistant-prose "I compute it."
     :blocks [{:role "tool"
               :tool-name "python_execution"
               :tool-call-id "toolu_1"
               :code "print(1 + 1)"
               :stdout "2\n"
               :error nil
               :repaired? false
               :timeout? false
               :envelope {:status :done
                          :ref "turn/t1/iteration/1/block/1"
                          :started-at-ms 1000
                          :finished-at-ms 1500
                          :repaired? false
                          :timeout? false}
               :activity {:counts {:running 0 :succeeded 1 :failed 0 :cancelled 0}}}]
     :reasoning-effort {:effective "medium" :selected {:provider "anthropic" :model "claude-test"}}
     :final? false}
    {:iteration 1
     :thinking nil
     :assistant-prose nil
     :blocks []
     :reasoning-effort {:effective "medium" :selected {:provider "anthropic" :model "claude-test"}}
     :final? true}]
   :iteration-count 2
   :duration-ms 2500
   :tokens {"input" 1200 "output" 80 "cached" 900 "cache_created" 250 "total" 1280}
   :cost {"total_cost" 0.25 "model" "claude-test" "provider" "anthropic-coding-plan"}
   :eval {:valid? true :invalid-reasons []}})

(def ^:private opts {:prompt "What is 1 + 1?" :started-at-ms 500 :profile {:name "cli"}})

(def ^:private document (trajectory/build result opts))

(defn- temp-dir
  ^Path []
  (Files/createTempDirectory "vis-trajectory-test" (make-array FileAttribute 0)))

(defn- block-step
  "The agent step of one iteration that runs `blocks`."
  [blocks]
  (second (:steps (trajectory/build (assoc result :trace [{:iteration 3 :blocks blocks}]) opts))))

(defdescribe
  build-test
  (it
    "starts with the prompt as the user step"
    (expect
      (= {:step-id 1 :source "user" :message "What is 1 + 1?" :timestamp "1970-01-01T00:00:00.500Z"}
         (first (:steps document)))))
  (it "maps each iteration to one agent step with its model, effort and thinking"
      (let [[_ step final-step] (:steps document)]
        (expect (= 3 (count (:steps document))))
        (expect (= ["agent" "agent"] [(:source step) (:source final-step)]))
        (expect (= 2 (:step-id step)))
        (expect (= "claude-test" (:model-name step)))
        (expect (= "medium" (:reasoning-effort step)))
        (expect (= "Add the numbers." (:reasoning-content step)))
        (expect (= "I compute it." (:message step)))
        (expect (= "1970-01-01T00:00:01Z" (:timestamp step)))
        (expect (= {:iteration 0 :is-final false :provider "anthropic"} (:extra step)))))
  (it "maps each code block to one python_execution call and its output"
      (let [step (second (:steps document))]
        (expect (= [{:tool-call-id "toolu_1"
                     :function-name "python_execution"
                     :arguments {"code" "print(1 + 1)"}
                     :extra {:status "done"
                             :ref "turn/t1/iteration/1/block/1"
                             :started-at-ms 1000
                             :finished-at-ms 1500
                             :is-repaired false
                             :is-timeout false
                             :activity {"running" 0 "succeeded" 1 "failed" 0 "cancelled" 0}}}]
                   (:tool-calls step)))
        (expect (= {:results [{:source-call-id "toolu_1" :content "2\n"}]} (:observation step)))))
  (it "puts the final answer on the final agent step"
      (let [final-step (last (:steps document))]
        (expect (= "The answer is 2." (:message final-step)))
        (expect (= {:iteration 1 :is-final true :provider "anthropic"} (:extra final-step)))
        (expect (nil? (:tool-calls final-step)))
        (expect (nil? (:reasoning-content final-step)))))
  (it "copies the token and cost totals of the result"
      (expect
        (= {:total-prompt-tokens 1200
            :total-completion-tokens 80
            :total-cached-tokens 900
            :total-cost-usd 0.25
            :total-steps 3
            :extra
            {:cache-creation-tokens 250 :total-tokens 1280 :iteration-count 2 :duration-ms 2500}}
           (:final-metrics document))))
  (it "names the agent, its tool, its profile and the session"
      (expect (= "ATIF-v1.8" (:schema-version document)))
      (expect (= "session-1" (:session-id document)))
      (expect (= "vis" (get-in document [:agent :name])))
      (expect (= "claude-test" (get-in document [:agent :model-name])))
      (expect (= ["python_execution"]
                 (mapv #(get-in % [:function :name]) (get-in document [:agent :tool-definitions]))))
      (expect (= {:name "cli"} (get-in document [:agent :extra :profile])))
      (expect (= "anthropic-coding-plan" (get-in document [:agent :extra :provider])))
      (expect (= {"is_valid" true "invalid_reasons" []} (get-in document [:extra :eval]))))
  (it "takes the agent model and provider from the steps when the cost has none"
      (let [agent (:agent (trajectory/build (update result :cost dissoc "model" "provider") opts))]
        (expect (= "claude-test" (:model-name agent)))
        (expect (= "anthropic" (get-in agent [:extra :provider])))))
  (it "reads the snake_case trace of a persisted run the same way"
      (let [persisted (update result :trace wire/canonical)]
        (expect (= (:steps document) (:steps (trajectory/build persisted opts))))))
  (it "adds the error of a failed block to its observation"
      (expect (= [{:source-call-id "vis-3-1"
                   :content "partial\n✗ runtime error: name 'x' is not defined"}]
                 (get-in (block-step [{:code "print('partial'); x"
                                       :stdout "partial"
                                       :error {:message "name 'x' is not defined"
                                               :data {:phase :runtime}}}])
                         [:observation :results]))))
  (it "gives a block without its own tool call id a new id"
      (expect (= ["vis-3-1" "vis-3-2" "toolu_9"]
                 (mapv :tool-call-id
                       (:tool-calls (block-step [{:code "a" :tool-call-id "toolu_1"}
                                                 {:code "b" :tool-call-id "toolu_1"}
                                                 {:code "c" :tool-call-id "toolu_9"}]))))))
  (it "keeps only the user step when the run has no iteration"
      (let [failed (trajectory/build {:status :error :error {:message "No provider."}} opts)]
        (expect (= ["user"] (mapv :source (:steps failed))))
        (expect (= {:status "error" :error "ERROR: No provider."} (:extra failed))))))

(defdescribe
  known-secrets-test
  (it "takes environment values whose names contain a credential word"
      (let [secrets (trajectory/known-secrets {:env {"OPENAI_API_KEY" "sk-env-1234"
                                                     "AWS_SECRET_ACCESS_KEY" "aws-secret-1234"
                                                     "DB_PASSWORD" "db-pass-1234"
                                                     "GH_CREDENTIAL" "gh-cred-123"
                                                     "GITHUB_TOKEN" "short"
                                                     "HOME" "/home/someone"}})]
        (expect (= #{"aws-secret-1234" "db-pass-1234" "sk-env-1234" "gh-cred-123"} (set secrets)))
        (expect (= "aws-secret-1234" (first secrets)))))
  (it "takes each string under a credential key of a credential file"
      (let [file (.toFile (.resolve (temp-dir) "anthropic-auth.json"))]
        (spit file
              (str "{\"access_token\":\"at-12345678\",\"refresh_token\":\"rt-12345678\","
                   "\"token_type\":\"Bearer\",\"expires_at\":123,\"email\":\"user@example.com\","
                   "\"coding-plan\":{\"api_key\":\"plan-key-12345678\"}}"))
        (expect (= #{"at-12345678" "rt-12345678" "plan-key-12345678"}
                   (set (trajectory/known-secrets {:files [file]}))))))
  (it "takes the API keys of the provider configuration"
      (expect (= ["sk-config-12345678"]
                 (trajectory/known-secrets {:providers [{:id "openai"
                                                         :api-key "sk-config-12345678"
                                                         :base-url
                                                         "https://gateway.example.com"}]}))))
  (it "ignores a credential file that is not JSON"
      (let [file (.toFile (.resolve (temp-dir) "broken-auth.json"))]
        (spit file "not json")
        (expect (= [] (trajectory/known-secrets {:files [file]}))))))

(defdescribe
  redact-test
  (it "replaces each known value in every string, map keys included"
      (expect (= {:message "key [REDACTED] and [REDACTED]" "[REDACTED]" ["[REDACTED]x" 3]}
                 (trajectory/redact {:message "key sk-123456789 and sk-123456789-long"
                                     "sk-123456789" ["sk-123456789x" 3]}
                                    ["sk-123456789" "sk-123456789-long"]))))
  (it "leaves the data as it is without known values"
      (expect (= {:message "sk-123456789"} (trajectory/redact {:message "sk-123456789"} [])))))

(defn- permissions
  [^File file]
  (PosixFilePermissions/toString (Files/getPosixFilePermissions (.toPath file)
                                                                (make-array LinkOption 0))))

(defdescribe write-test
             (it "writes the ATIF JSON to a file that only its owner can read"
                 (let [path
                       (str (.resolve (temp-dir) "nested/trajectory.json"))

                       _
                       (trajectory/write! path document)

                       file
                       (io/file path)

                       parsed
                       (wire/parse-json (slurp file))]

                   (expect (= "rw-------" (permissions file)))
                   (expect (= "ATIF-v1.8" (get parsed "schema_version")))
                   (expect (= "toolu_1" (get-in parsed ["steps" 1 "tool_calls" 0 "tool_call_id"])))
                   (expect (= 1200 (get-in parsed ["final_metrics" "total_prompt_tokens"])))))
             (it "replaces an existing file and makes it private"
                 (let [file (.toFile (.resolve (temp-dir) "trajectory.json"))]
                   (spit file "old")
                   (.setReadable file true false)
                   (trajectory/write! (str file) document)
                   (expect (= "rw-------" (permissions file)))
                   (expect (= "ATIF-v1.8" (get (wire/parse-json (slurp file)) "schema_version")))))
             (it "redacts the known credentials before the write"
                 (let [path (str (.resolve (temp-dir) "trajectory.json"))]
                   (with-redefs-fn {#'trajectory/run-secrets (fn []
                                                               ["What is 1 + 1?"])}
                     #(trajectory/export! path result opts))
                   (expect (= "[REDACTED]"
                              (get-in (wire/parse-json (slurp path)) ["steps" 0 "message"]))))))
