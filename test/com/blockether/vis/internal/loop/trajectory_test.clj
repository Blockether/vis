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
               :activity {:state :done
                          :counts {:running 0 :succeeded 1 :failed 0 :cancelled 0}
                          :rows [{:id "activity-1"
                                  :operation "cat"
                                  :state :succeeded
                                  :evidence [{:kind "arguments" :text "README.md"}
                                             {:kind "result" :text "# Vis"}]}]
                          :history {:cursor 4}}}]
     :reasoning-effort {:effective "medium" :selected {:provider "anthropic" :model "claude-test"}}
     :usage {:tokens {"input" 600 "output" 40 "cached" 450 "cache_created" 100 "reasoning" 12}
             :cost-usd 0.125}
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
   :eval {:valid? true :invalid-reasons [] :notes ["fixture"]}})

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
        (expect (= {:iteration 0 :is-final false :provider "anthropic"} (:extra step)))
        (expect (= {:prompt-tokens 600
                    :completion-tokens 40
                    :cached-tokens 450
                    :cost-usd 0.125
                    :extra {:cache-creation-tokens 100 :reasoning-tokens 12}}
                   (:metrics step)))))
  (it "maps each code block to one python_execution call and its output"
      (let [step (second (:steps document))]
        (expect
          (= [{:tool-call-id "toolu_1"
               :function-name "python_execution"
               :arguments {"code" "print(1 + 1)"}
               :extra {:status "done"
                       :ref "turn/t1/iteration/1/block/1"
                       :started-at-ms 1000
                       :finished-at-ms 1500
                       :is-repaired false
                       :is-timeout false
                       :activity {"state" "done"
                                  "counts" {"running" 0 "succeeded" 1 "failed" 0 "cancelled" 0}
                                  "rows" [{"id" "activity-1"
                                           "operation" "cat"
                                           "state" "succeeded"
                                           "evidence" [{"kind" "arguments" "text" "README.md"}
                                                       {"kind" "result" "text" "# Vis"}]}]}
                       :stdout-chars 2}}]
             (:tool-calls step)))
        (expect (= {:results [{:source-call-id "toolu_1" :content "2\n"}]} (:observation step)))))
  (it "puts the final answer on the final agent step"
      (let [final-step (last (:steps document))]
        (expect (= "The answer is 2." (:message final-step)))
        (expect (= {:iteration 1 :is-final true :provider "anthropic"} (:extra final-step)))
        (expect (nil? (:tool-calls final-step)))
        (expect (nil? (:metrics final-step)))
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
      (expect (= {"is_valid" true "invalid_reasons" [] "notes" ["fixture"]}
                 (get-in document [:extra :eval]))))
  (it "adds the session usage and the saved goal of a persisted run"
      (let [usage
            {"turns" 1 "cache_read_share_percent" 75}

            goal
            {"text" "Add the numbers." "status" "complete"}

            extra
            (:extra (trajectory/build result
                                      (assoc opts
                                        :session-usage usage
                                        :goal goal)))]

        (expect (= usage (:session-usage extra)))
        (expect (= goal (:goal extra)))
        (expect (nil? (:session-usage (:extra document))))
        (expect (nil? (:goal (:extra document))))))
  (it "takes the agent model and provider from the steps when the cost has none"
      (let [agent (:agent (trajectory/build (update result :cost dissoc "model" "provider") opts))]
        (expect (= "claude-test" (:model-name agent)))
        (expect (= "anthropic" (get-in agent [:extra :provider])))))
  (it "reads the snake_case trace of a persisted run the same way"
      (let [persisted (update result :trace wire/canonical)]
        (expect (= (:steps document) (:steps (trajectory/build persisted opts))))))
  (it "keeps the stdout size and the raw error of a block in its call"
      (let [extra (get-in (block-step [{:code "x" :stdout "partial" :error {:message "boom"}}])
                          [:tool-calls 0 :extra])]
        (expect (= 7 (:stdout-chars extra)))
        (expect (= {"message" "boom"} (:error extra)))))
  (it "counts the stdout size in Unicode code points, as Python does"
      (expect (= 2
                 (get-in (block-step [{:code "x" :stdout "😀\n"}])
                         [:tool-calls 0 :extra :stdout-chars]))))
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

(def ^:private child
  "One Council child of `result` as the gateway gives it: an agent row with one
   persisted turn and its iterations."
  {:session_id "child-1"
   :parent_id "session-1"
   :task "Count the files."
   :status "completed"
   :depth 1
   :created_at 1200
   :model "claude-child"
   :provider "anthropic"
   :turns [{"turn_id" "turn-1"
            "position" 2
            "request" "Count the files."
            "status" "completed"
            "created_at" 1200
            "content" [{"id" "block_1" "type" "prose" "markdown" "There are 3 files."}]
            "tokens" {"input" 300 "output" 20 "cached" 100}
            "cost" {"total_cost" 0.5}
            "trace" [{"position" 1
                      "created_at" 1300
                      "finished_at" 1400
                      "thinking" "List the directory."
                      "model" "claude-child"
                      "provider" "anthropic"
                      "input_tokens" 300
                      "output_tokens" 20
                      "input_cache_read_tokens" 100
                      "cost_usd" 0.5
                      "forms" [{"src" "print(len(ls()))"
                                "stdout" "3\n"
                                "tool_call_id" "toolu_c1"
                                "tool_name" "python_execution"
                                "duration_ms" 12}]}]}]})

(def ^:private grandchild
  "A child of `child` that has no persisted turn yet."
  {:session_id "child-2"
   :parent_id "child-1"
   :task "Check one file."
   :status "failed"
   :created_at 1350})

(def ^:private team-document
  (trajectory/build result (assoc opts :subagents [child grandchild child])))

(defdescribe
  subagent-test
  (it "embeds each Council child once, as a trajectory with its own id"
      (expect (= "session-1" (:trajectory-id team-document)))
      (expect (= ["child-1"] (mapv :trajectory-id (:subagent-trajectories team-document)))))
  (it "maps the persisted turn of a child to steps with metrics"
      (let [[request step] (:steps (first (:subagent-trajectories team-document)))]
        (expect (= {:step-id 1
                    :source "user"
                    :message "Count the files."
                    :timestamp "1970-01-01T00:00:01.200Z"}
                   request))
        (expect (= "There are 3 files." (:message step)))
        (expect (true? (get-in step [:extra :is-final])))
        (expect (= "t2/i1/f1" (get-in step [:tool-calls 0 :extra :ref])))
        (expect (= "claude-child" (:model-name step)))
        (expect (= "List the directory." (:reasoning-content step)))
        (expect (= {:prompt-tokens 300 :completion-tokens 20 :cached-tokens 100 :cost-usd 0.5}
                   (:metrics step)))
        (expect (= [{"code" "print(len(ls()))"}] (mapv :arguments (:tool-calls step))))
        (expect (= "3\n" (get-in step [:observation :results 0 :content])))))
  (it "refers to the child from the call that started it"
      (expect (= [{:trajectory-id "child-1" :session-id "child-1" :extra {:status "completed"}}]
                 (get-in team-document
                         [:steps 1 :observation :results 0 :subagent-trajectory-ref]))))
  (it "embeds a child of a child in that child"
      (let [sub
            (first (:subagent-trajectories team-document))

            nested
            (first (:subagent-trajectories sub))]

        (expect (= "child-2" (:trajectory-id nested)))
        (expect (= [{:step-id 1 :source "user" :message "Check one file."}] (:steps nested)))
        (expect (= "child-2"
                   (get-in sub
                           [:steps 1 :observation :results 0 :subagent-trajectory-ref 0
                            :trajectory-id])))))
  (it "adds the cost of the children to the total cost of the run"
      (expect (= 0.75 (get-in team-document [:final-metrics :total-cost-usd])))
      (expect (= 0.5 (get-in team-document [:final-metrics :extra :subagent-cost-usd])))
      (expect (= {:total-prompt-tokens 300
                  :total-completion-tokens 20
                  :total-cached-tokens 100
                  :total-cost-usd 0.5
                  :total-steps 2}
                 (:final-metrics (first (:subagent-trajectories team-document)))))))

(def ^:private persisted-iterations
  "The iterations of the turn of a persisted run. The answer step also runs code."
  [{"position" 1
    "created_at" 1000
    "finished_at" 1500
    "thinking" "Add the numbers."
    "model" "claude-test"
    "provider" "anthropic"
    "input_tokens" 1200
    "output_tokens" 80
    "forms"
    [{"src" "print(1 + 1)" "stdout" "2\n" "tool_call_id" "toolu_1" "tool_name" "python_execution"}]}
   {"position" 2
    "created_at" 2000
    "forms" [{"src" "print('done')"
              "stdout" "done\n"
              "tool_call_id" "toolu_2"
              "tool_name" "python_execution"}]}])

(def ^:private persisted-document
  "A persisted run: the result has no trace, so the iterations of its turn come
   from the session."
  (trajectory/build (dissoc result :trace)
                    (assoc opts
                      :iterations persisted-iterations
                      :turn-position 3)))

(defdescribe
  persisted-run-test
  (it "maps the persisted iterations of a run without a trace to agent steps"
      (let [[request step final-step] (:steps persisted-document)]
        (expect (= 3 (count (:steps persisted-document))))
        (expect (= "What is 1 + 1?" (:message request)))
        (expect (= [2 3] [(:step-id step) (:step-id final-step)]))
        (expect (= "claude-test" (:model-name step)))
        (expect (= [{"code" "print(1 + 1)"}] (mapv :arguments (:tool-calls step))))
        (expect (= "2\n" (get-in step [:observation :results 0 :content])))
        (expect (= "The answer is 2." (:message final-step)))))
  (it "keeps the totals of the result as the final metrics"
      (expect (= (:final-metrics document) (:final-metrics persisted-document))))
  (it "marks only the answer step as final, also when it runs code"
      (expect (= [false true]
                 (mapv #(get-in % [:extra :is-final]) (rest (:steps persisted-document))))))
  (it "names each persisted form with its turn, iteration and form"
      (expect (= ["t3/i1/f1" "t3/i2/f1"]
                 (mapv #(get-in % [:tool-calls 0 :extra :ref])
                       (rest (:steps persisted-document))))))
  (it "leaves out the ref of a persisted form when the turn position is unknown"
      (expect (nil? (get-in (trajectory/build (dissoc result :trace)
                                              (assoc opts :iterations persisted-iterations))
                            [:steps 1 :tool-calls 0 :extra :ref])))))

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
