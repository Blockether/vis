(ns com.blockether.vis.internal.context.prompt-test
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.svar.internal.llm :as svar-llm]
            [com.blockether.svar.core :as svar]
            [com.blockether.svar.internal.router :as svar-router]
            [com.blockether.vis.internal.attachment.core :as attachments]
            [com.blockether.vis.internal.context.agents :as agents]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.python.env :as env-python]
            [com.blockether.vis.internal.python.runtime :as python-runtime]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.extension.manifest :as manifest]
            [com.blockether.vis.internal.context.prompt :as prompt]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  build-metadata-prompt-test
  (it "advertises the same build globals as Python without elevating metadata to instructions"
      (let [metadata {"VIS_PYTHON_RUNTIME_VERSION" "0.5.10"
                      "VIS_SHA_RELEASE" nil
                      "VIS_VERSION" "dev"
                      "VIS_PYTHON_SDK_VERSION" "dev"}]
        (with-redefs [python-runtime/version-globals (constantly metadata)]
          (let [text (#'prompt/sandbox-shims-prompt-block [])]
            (doseq [[name value] metadata]
              (expect (str/includes?
                        text
                        (str "`" name " = " (if (nil? value) "None" (pr-str value)) "`"))))
            (expect (str/includes? text "it wins over pip copies"))
            ;; Issue #253: importable declarations do not imply extension host access.
            (expect (str/includes? text "`blockether.vis.extension` is the bundled"))
            (expect (str/includes? text "import it for types only"))
            ;; User report: the bundled SDK is frozen into the runtime, so
            ;; `blockether.vis.__file__` raises AttributeError and reads as a broken import.
            (expect (str/includes? text "frozen SDK (no `__file__`"))
            (expect (str/includes? text "cannot register extensions or use extension host APIs"))
            (expect (str/includes? text "Build globals, for diagnostics only")))))))

(defdescribe
  request-health-test
  ;; Regression: Blockether/vis#174 omitted nested tool payloads from the breakdown.
  (it "uses the shared tokenizer for dense text, tool calls and nested results"
      (let [payload
            (apply str (repeat 1000 "ą中42={x:17};\n"))

            messages
            [{:role "user" :content "question"}
             {:role "assistant"
              :content
              [{:type "tool_use" :id "call-1" :name "python_execution" :input {"code" payload}}]}
             {:role "user"
              :content [{:type "tool_result"
                         :tool_use_id "call-1"
                         :content [{:type "text" :text payload}]}]}]

            health
            (prompt/request-health {} messages [])]

        (expect (= (svar/count-messages "gpt-4o" messages)
                   (reduce + (map :tokens (:breakdown health)))))
        (expect (not (str/includes? (pr-str health) payload)))))
  ;; #186: a logical breakdown is not the prepared Responses request or measured usage.
  (it "labels and totals the logical projection without claiming a prepared count"
      (let [model
            "gpt-6-astra"

            signature
            (json/write-json-str
              {:type "reasoning" :id "not_rs" :encrypted_content (apply str (repeat 10000 "A"))})

            messages
            [{:role "user" :content "go"}
             {:role "assistant"
              :model model
              :content [{:type "thinking" :thinking "brief" :thinking-signature signature}
                        {:type "text" :text "done"}]} {:role "user" :content "continue"}]

            tools
            [{:name "run" :description "Run code" :schema {:type "object"}}]

            health
            (prompt/request-health {} messages tools model)

            prepared
            (#'svar-llm/build-openai-responses-request-body messages model {:svar/tools tools})]

        (expect (= :logical-request (:counted-projection health)))
        (expect (= (reduce + (map :tokens (:breakdown health))) (:estimated-input-tokens health)))
        (expect (< (+ 1000 (svar-router/count-responses-request model prepared))
                   (reduce + (map :tokens (:breakdown health)))))
        (expect (not (str/includes? (pr-str health) signature)))))
  (it "uses Svar's prepared components without recounting logical messages or tools"
      ;; #186: the prepared request can omit replay that the logical tokenizer counts.
      (let [accounting {:source :svar-estimate
                        :projection :prepared-request
                        :model "gpt-6-astra"
                        :api-style :openai-compatible-responses
                        :input-tokens 123
                        :components
                        {:messages 90 :instructions 20 :tools 8 :output-format 2 :reply-priming 3}}]
        (with-redefs [svar/count-messages (fn [& _]
                                            (throw (ex-info "Must not recount" {})))
                      svar/count-tokens (fn [& _]
                                          (throw (ex-info "Must not recount" {})))]

          (let [health (prompt/request-health {}
                                              [{:role "user" :content "private"}]
                                              [{:name "private-tool"}]
                                              "gpt-6-astra"
                                              accounting)]
            (expect (= :prepared-request (:counted-projection health)))
            (expect (= :svar-estimate (:token-count-source health)))
            (expect (= "gpt-6-astra" (:token-count-model health)))
            (expect (= 123
                       (:estimated-input-tokens health)
                       (reduce + 0 (map :tokens (:breakdown health)))))
            (expect (= [20 90 8 2 3] (mapv :tokens (:breakdown health))))
            (expect (not (str/includes? (pr-str health) "private")))))))
  (it "does not accept accounting for a different model or inconsistent components"
      (doseq [accounting [{:model "other" :input-tokens 3 :components {:reply-priming 3}}
                          {:model "gpt-6-astra" :input-tokens 5 :components {:reply-priming 3}}]]
        (let [health (prompt/request-health
                       {}
                       []
                       []
                       "gpt-6-astra"
                       (merge {:source :svar-estimate :projection :prepared-request} accounting))]
          (expect (= :unavailable (:token-count-source health)))
          (expect (empty? (:breakdown health)))
          (expect (nil? (:estimated-input-tokens health))))))
  (it "labels a prior turn's request like the current one"
      (let [messages
            (into (:request (prompt/prior-turn-messages {:turn 1 :request "prior dense input"}))
                  (prompt/assemble-initial-messages {:initial-user-content "current input"}))

            health
            (prompt/request-health {} messages [] "gpt-4")

            labels
            (set (map :label (:breakdown health)))]

        (expect (= 2 (count messages)))
        (expect (contains? labels "User requests"))
        (expect (not-any? #(str/includes? (str %) "recap") labels))
        (expect (= :svar-estimate (:token-count-source health)))
        (expect (= "gpt-4" (:token-count-model health)))
        (expect (= (svar/count-messages "gpt-4" messages)
                   (reduce + (map :tokens (:breakdown health)))))))
  (it "keeps unavailable token counts diagnostic-only and never exposes the failing content"
      (with-redefs [svar/count-messages (fn [_ _]
                                          (throw (ex-info "private request content" {})))]
        (expect (= {:token-count-source :unavailable
                    :token-count-model "gpt-4"
                    :counted-projection :logical-request
                    :breakdown []
                    :roots []}
                   (prompt/request-health {}
                                          [{:role "user" :content "private request content"}]
                                          []
                                          "gpt-4")))))
  (it "attributes sent primary guidance once without rereading it"
      (with-redefs [agents/primary-instructions
                    (constantly {:files [{:scope :project
                                          :source :agents-md
                                          :path "/work/AGENTS.md"
                                          :content "abcdabcd"}]})

                    agents/added-root-guidance-index
                    (constantly [])]

        (let [messages
              (prompt/assemble-stable-prompt-messages {} {:active-extensions []})

              health
              (prompt/request-health {} (conj messages {:role "user" :content "abcdefgh"}) [])

              rows
              (:breakdown health)]

          (expect (= {:label "Main AGENTS.md" :tokens 2 :path "/work/AGENTS.md"}
                     (first (filter #(= "Main AGENTS.md" (:label %)) rows))))
          (expect
            (= 6 (:tokens (first (filter #(= "Conversation and tool results" (:label %)) rows)))))
          (expect (not (contains? health :last-request-tokens)))
          (expect (every? #(not (contains? % :content)) rows)))))
  (it "ignores image bytes and keeps unknown guidance status unknown"
      (let [health (prompt/request-health
                     {:workspace {:root "/work"}
                      :filesystem-roots [{:trunk "/linked" :clone "/linked" :draft :shared}]}
                     [{:role "user"
                       :content [{:type "text" :text "abcd"}
                                 {:type "image_url"
                                  :image_url {:url "data:image/png;base64,AAAA" :detail "low"}}]}]
                     [])]
        (expect (= [{:label "Conversation and tool results" :tokens 91}
                    {:label "Message framing" :tokens 3}]
                   (:breakdown health)))
        (expect (every? #(not (contains? % :instructions-loaded)) (:roots health))))))

(defdescribe
  prepared-instruction-attribution-test
  ;; Regression: Blockether/vis#268 billed the core prompt, the injected
  ;; AGENTS.md, runtime/extension prompts and the session context as one opaque
  ;; "System instructions" row whenever Svar's prepared accounting was present.
  (it
    "drills Svar's prepared instructions down to guidance, runtime and session rows"
    (with-redefs [agents/primary-instructions
                  (constantly {:files [{:scope :project
                                        :source :agents-md
                                        :path "/work/AGENTS.md"
                                        :content (apply str (repeat 40 "house rule "))}]})

                  agents/added-root-guidance-index
                  (constantly [])]

      (let [model
            "gpt-6-astra"

            built-in
            {:ext/name "test.kernel"
             :ext/engine {:ext.engine/alias 'kernel :ext.engine/builtin? true}
             :ext/prompt-fn (constantly "Kernel rules that ride in the core surface.")}

            installed
            {:ext/name "test.drilldown"
             :ext/engine {:ext.engine/alias 'demo}
             :ext/prompt-fn (constantly "Demo extension guidance, pushed on every request.")}

            stable
            (prompt/assemble-stable-prompt-messages {:extensions (atom [built-in installed])}
                                                    {:active-extensions [built-in installed]
                                                     :session-context "session = {\"turn\": 1}"})

            messages
            (conj (vec stable) {:role "user" :content "Which fixed context can I cut?"})

            tools
            [{:name "run" :description "Run code" :schema {:type "object"}}]

            accounting
            (svar-router/responses-request-accounting
              model
              (#'svar-llm/build-openai-responses-request-body messages model {:svar/tools tools}))

            health
            (prompt/request-health {} messages tools model accounting)

            rows
            (:breakdown health)

            row
            (fn [label]
              (first (filter #(= label (:label %)) rows)))]

        (expect (= :prepared-request (:counted-projection health)))
        (expect (= (:input-tokens accounting)
                   (:estimated-input-tokens health)
                   (reduce + 0 (map :tokens rows))))
        (expect (= {:label "Main AGENTS.md" :path "/work/AGENTS.md"}
                   (select-keys (row "Main AGENTS.md") [:label :path])))
        (doseq [label ["Vis core system prompt" "Main AGENTS.md" "Built-in tools and rules"
                       "Extension: demo" "Sandbox and Python runtime"
                       "Session and environment context" "Conversation and tool results"
                       "Tool declarations"]]
          (expect (pos? (long (:tokens (row label) 0)))))
        ;; What is left over is message framing, not the aggregate #268 reported.
        (expect (< (long (:tokens (row "System instructions") 0))
                   (long (:tokens (row "Vis core system prompt")))))
        (expect (not (str/includes? (pr-str health) "house rule")))))))

(defdescribe
  request-health-tokenization-test
  (it "reuses a whole-message token count for an identical attribution"
      ;; JVM dogfooding: runtime/tool attribution tokenized the same large block twice.
      (doseq [content [(apply str (repeat 50 "ą中42={x:17};\n"))
                       [{:type "text" :text "Runtime instructions"}
                        {:type "text" :text "Tool declarations"}]]]
        (let [model "gpt-4o"
              count-messages svar/count-messages
              message {:role "system" :content content}
              total (count-messages model [message])
              overhead (count-messages model [(assoc message :content "")])
              calls (atom [])]

          (with-redefs [svar/count-messages (fn (^long [model messages] (swap! calls conj messages)
                                                 (count-messages model messages {}))
                                              (^long [model messages opts] (swap! calls conj
                                                                             messages)
                                               (count-messages model messages opts)))]
            (let [health (prompt/request-health {}
                                                [(with-meta message
                                                   {::prompt/parts [{:label "Attributed content"
                                                                     :content content}]})]
                                                []
                                                model)]
              (expect (= 1 (count (filter #(= [message] %) @calls))))
              (expect (= total (:estimated-input-tokens health)))
              (expect (= (- total overhead)
                         (:tokens (first (filter #(= "Attributed content" (:label %))
                                                 (:breakdown health))))))))))))

(defdescribe
  request-token-counter-test
  (it "preserves Svar counts for framing, repeated messages and structured content"
      (let [messages
            [{:role "system" :name "guide" :content "Zażółć 中\nStable instructions"}
             {:role "assistant"
              :content
              [{:type "thinking" :thinking "Consider the result"}
               {:type "tool_use" :id "call-1" :name "python_execution" :input {:code "print(42)"}}]}
             {:role "user"
              :content
              [{:type "tool_result" :tool_use_id "call-1" :content [{:type "text" :text "42"}]}
               {:type "image_url" :image_url {:url "data:image/png;base64,AAAA" :detail "low"}}]}]

            counter
            (prompt/request-token-counter)]

        (doseq [model
                ["gpt-4" "gpt-4o"]

                selected
                [nil [] [(first messages)] messages (conj messages (first messages))
                 (reverse messages)]]

          (expect (= (svar/count-messages model selected) (counter model selected))))))
  (it "reuses complete content across counters and separates model changes"
      ;; The message cache is process-wide, so content that other tests count must not hit it.
      (let [nonce
            (random-uuid)

            message
            {:role "user" :content (str "Stable request " nonce)}

            changed
            (assoc message :content (str "Changed request " nonce))

            count-messages
            svar/count-messages

            calls
            (atom [])]

        (with-redefs [svar/count-messages
                      (fn (^long [model messages] (swap! calls conj [model (vec messages)])
                           (count-messages model messages {})) (^long [model messages opts]
                                                                (swap! calls conj [model
                                                                                   (vec messages)])
                                                                (count-messages model messages
                                                                  opts)))]
          (let [counter (prompt/request-token-counter)]
            (doseq [messages [[message] [(with-meta message {:source :health})] [message message]
                              [changed]]]
              (counter "gpt-4o" messages))
            (counter "gpt-4" [message])
            (expect (= {["gpt-4o" []] 1
                        ["gpt-4o" [message]] 1
                        ["gpt-4o" [changed]] 1
                        ["gpt-4" []] 1
                        ["gpt-4" [message]] 1}
                       (frequencies @calls)))
            ((prompt/request-token-counter) "gpt-4o" [message])
            (expect (= 1 (get (frequencies @calls) ["gpt-4o" [message]])))
            (expect (= 2 (get (frequencies @calls) ["gpt-4o" []]))))))))

(defdescribe
  linked-guidance-estimates-test
  (it "estimates available guidance without adding it to sent context or recording a model read"
      (with-redefs [workspace/env-filesystem-roots
                    :filesystem-roots

                    agents/scan-in
                    (constantly {:result
                                 {:found? true :path "/linked/AGENTS.md" :content "ąbcde"}})]

        (let [health (prompt/request-health {:workspace {:root "/work"}
                                             :filesystem-roots
                                             [{:trunk "/linked" :clone "/linked" :draft :shared}]}
                                            [{:role "user" :content "abcd"}]
                                            [])]
          (expect (= [{:label "Conversation and tool results" :tokens 6}
                      {:label "Message framing" :tokens 3}]
                     (:breakdown health)))
          (expect (= [{:path "/linked"
                       :guidance {:status "available"
                                  :path "/linked/AGENTS.md"
                                  :tokens (svar/count-tokens "unknown" "ąbcde")}}]
                     (:roots health))))))
  (it "distinguishes missing guidance from read failures and never scans denied roots"
      (doseq [[scan status] [[{:result {:found? false}} "missing"]
                             [{:warnings [{:reason "unreadable"}]} "error"]]]
        (let [calls (atom 0)]
          (with-redefs [workspace/env-filesystem-roots :filesystem-roots
                        agents/scan-in (fn [_]
                                         (swap! calls inc)
                                         scan)]

            (let [health (prompt/request-health
                           {:workspace {:root "/work"}
                            :filesystem-roots [{:trunk "/linked" :clone "/linked" :draft :shared}
                                               {:trunk "/denied" :clone "/denied" :denied? true}]}
                           []
                           [])]
              (expect (= 1 @calls))
              (expect (= status (get-in health [:roots 0 :guidance :status])))
              (expect (nil? (get-in health [:roots 0 :guidance :tokens])))))))))

(defdescribe prompt-assembly-test
             (it "normalizes core addendum and extension prompt text"
                 (let [ext
                       {:ext/name "test.prompt"
                        :ext/engine {:ext.engine/alias 't}
                        :ext/prompt-fn
                        (fn [_]
                          "\n\n    Extension line\n\n\n\n      Nested extension line\n")}

                       env
                       {:extensions (atom [ext])}

                       messages
                       (prompt/assemble-stable-prompt-messages
                         env
                         {:system-prompt "\n\n    Addendum line\n\n\n\n      Nested addendum line\n"
                          :active-extensions [ext]})

                       text
                       (prompt/stable-prompt-text messages)]

                   (expect (str/includes? text "Addendum line\n\n  Nested addendum line"))
                   (expect (str/includes? text "Extension line\n\n  Nested extension line"))
                   (expect (not (str/includes? text "\n\n\n"))))))

(defdescribe core-prompt-grep-regex-test
             ;; Regression, user report: §3 ordered grep but omitted the content-regex switch.
             (it "names grep's regex mode where it teaches the call"
                 (let [text (prompt/build-system-prompt {})]
                   (expect (str/includes? text "`is_regex: True`")))))

(defdescribe
  one-system-prompt-test
  ;; ctb vis-edits: a separate CLI prompt split the cached prefix by channel. One prompt
  ;; serves every channel; `session["agent"]["interactive"]` carries the difference.
  ;; User decision: a one-shot `:cli` run has no reader for progress notes. With only the
  ;; interactive flag, GPT still wrote 2.7 notes per task in ctb edits11, so the `:cli`
  ;; prompt leaves out the progress-note rules and nothing else.
  (it "sends one stable prompt to every channel, without progress-note rules for :cli"
      (let [text-for
            (fn [ch]
              (-> (prompt/assemble-stable-prompt-messages {:channel ch} {:active-extensions []})
                  prompt/stable-prompt-text))

            text
            (text-for :tui)

            cli
            (text-for :cli)]

        (doseq [ch [:web nil]]
          (expect (= text (text-for ch)) (str ch)))
        (expect (= cli (str/replace text (var-get #'prompt/PROGRESS_NOTE_RULES) "")))
        (expect (str/includes? text "Next to each `python_execution` call"))
        (doseq [rule ["Next to each `python_execution` call" "a finding that changes the plan"]]
          (expect (not (str/includes? cli rule)) rule))
        (expect (str/includes? cli "results that you have.\n\n## Calls"))
        (expect (str/includes? cli "`session[\"agent\"][\"interactive\"]` is False"))
        (expect (not (str/includes? cli "NON-INTERACTIVE ONE-SHOT RUN"))))))

(defdescribe
  core-prompt-project-style-test
  ;; Regression: Blockether/vis#188 reported missing spacing between definitions and YAML resources.
  ;; These assertions pin the prompt contract, not model compliance.
  (it
    "makes project style and logical spacing part of editing correctness"
    (let [text
          (var-get #'prompt/CORE_SYSTEM_PROMPT)

          edit-section
          (second (re-find #"(?s)- EDIT: (.*?)\n- CHECK: " text))]

      (expect (some? edit-section))
      (doseq
        [rule
         ["Ship the smallest in-scope edit that fixes the cause, in the style of the code nearby"]]
        (expect (str/includes? (str/replace (or edit-section "") #"\s+" " ") rule) rule))))
  (it "owns scope and verification once, separately from response style"
      (let [text
            (var-get #'prompt/CORE_SYSTEM_PROMPT)

            edit-section
            (second (re-find #"(?s)- EDIT: (.*?)\n- FINISH: " text))]

        (doseq [rule ["Ship the smallest in-scope edit" "Test the changed code"
                      "Run the affected tests, format and lint, and review the diff"]]
          (let [pattern (re-pattern (java.util.regex.Pattern/quote rule))
                normalized (str/replace text #"\s+" " ")]

            (expect (= 1 (count (re-seq pattern normalized))) rule)
            (expect (str/includes? (str/replace (or edit-section "") #"\s+" " ") rule) rule)))
        (expect (str/includes? text "- FINISH: Start with the answer"))
        (expect (not (str/includes? text "## 7. Style and finish"))))))

(defdescribe core-prompt-scoped-discovery-test
             ;; Regression, user report: agents guessed paths and repeated broad searches after finding owners.
             ;; These assertions pin the instructions, not model compliance.
             (it "confirms filesystem paths before locating unknown code"
                 (let [text
                       (var-get #'prompt/CORE_SYSTEM_PROMPT)

                       steps
                       (mapv #(str/index-of text %)
                             ["`ls` the nearest confirmed parent" "`grep({\"query\": [needles]"])]

                   (expect (every? some? steps))
                   (when (every? some? steps) (expect (apply < steps)))
                   (expect (str/includes? text "first `project_root_path`"))
                   (expect (not (str/includes? text "`grep(...)` FIRST")))))
             (it "reads known regions directly instead of rediscovering them"
                 (let [text (var-get #'prompt/CORE_SYSTEM_PROMPT)]
                   (expect (str/includes? text "Read known regions directly"))))
             (it "limits further reads to unresolved questions and scopes searches to the owner"
                 (let [text (var-get #'prompt/CORE_SYSTEM_PROMPT)]
                   (doseq [rule ["Read only for the open question that controls the next step"]]
                     (expect (str/includes? text rule) rule)))))

(defdescribe
  core-prompt-demand-driven-discovery-test
  ;; Regression: #231 repeated discovery after /reload despite an already known contract.
  ;; These assertions pin the base prompt's decision rules, not model compliance.
  (it "reuses facts from system instructions and the visible conversation"
      (let [text (str/replace (var-get #'prompt/CORE_SYSTEM_PROMPT) #"\s+" " ")]
        (doseq
          [rule
           ["Facts in the system prompt and the conversation, with its fold gists, stay known"]]
          (expect (str/includes? text rule) rule))))
  (it "does not refresh known signatures to resolve a missing semantic detail"
      (let [text (str/replace (var-get #'prompt/CORE_SYSTEM_PROMPT) #"\s+" " ")]
        (expect (str/includes? text "For a call, `print(inspect.signature(fn))`"))
        (expect (not (str/includes? text "Still missing after inspection")))))
  (it "consolidates the decision matrix with discovery contracts in the base prompt"
      (let [text
            (prompt/build-system-prompt {})

            section
            (second (str/split text #"## States" 2))

            discovery
            (some-> section
                    (str/split #"## Calls" 2)
                    first)]

        (expect (some? discovery))
        (when discovery
          (doseq [rule ["Read only for the open question that controls the next step"
                        "For a call, `print(inspect.signature(fn))`"
                        "Find a name with `apropos(pattern)`"
                        "`doc(name)` returns the full contract"]]
            (expect (str/includes? discovery rule) rule)))
        (expect (not (str/includes? text "use the first row that matches")))
        (expect (= 1 (count (re-seq #"Read only for the open question" text))))
        (expect (= 1 (count (re-seq #"controls the next step" text))))
        (expect (not (str/includes? text "Unknown call shape: use narrow `doc(name)`"))))))

(defdescribe
  core-prompt-prior-turn-context-test
  ;; Regression: #262 "prior work or recovered context" read as an instruction to fetch
  ;; the current session's history at task start although the conversation held the task.
  (it "treats prior-turn context as visible conversation, not session history to fetch"
      (let [text (str/replace (prompt/build-system-prompt {}) #"\s+" " ")]
        (doseq
          [rule
           ["Facts in the system prompt and the conversation, with its fold gists, stay known"]]
          (expect (str/includes? text rule) rule))
        (expect (not (str/includes? text "recovered context"))))))

(defdescribe core-prompt-registered-python-contract-test
             ;; #232: pin invocation authority without encouraging repeated discovery or copied schemas.
             (it "keeps semantic documentation without duplicating structure"
                 (let [text (str/replace (var-get #'prompt/CORE_SYSTEM_PROMPT) #"\s+" " ")]
                   (doseq [rule ["`doc(name)` returns the full contract"]]
                     (expect (str/includes? text rule) rule))))
             (it "filters available full metadata before printing omitted schema details"
                 ;; #234: a full contract dump erased compact-doc savings in real-model E2E.
                 (let [text (str/replace (prompt/build-system-prompt {}) #"\s+" " ")]
                   (doseq [rule ["`print(inspect.signature(fn))`" "print only the fields you need"]]
                     (expect (str/includes? text rule) rule)))))

(defdescribe
  core-prompt-execution-invariants-test
  ;; Compression must retain executable contracts, not just capability names.
  (it "keeps independent batching separate from dependent observations"
      (let [text (var-get #'prompt/CORE_SYSTEM_PROMPT)]
        (doseq [rule ["Put independent calls in one block with `await gather(...)`"
                      "Keep results in variables and print only the fields you need"]]
          (expect (str/includes? text rule) rule))))
  ;; User decision: the `python_execution` tool contract owns the print channel, so the core
  ;; prompt does not repeat it.
  (it "leaves the output channel to the tool contract"
      (let [text (var-get #'prompt/CORE_SYSTEM_PROMPT)]
        (expect (str/includes? text "Keep results in variables"))
        (expect (not (str/includes? text "`print()` is the one channel back")))))
  ;; #239: pin type-directed access and recovery without adding another discovery preflight.
  ;; #259: a record's fields come from its model and its errors, never from guessed synonyms.
  ;; User review: the runtime accepts both access spellings and `dict(r)`, so no rule names them.
  (it "inspects only unknown shapes"
      (let [text (str/replace (var-get #'prompt/CORE_SYSTEM_PROMPT) #"\s+" " ")]
        (expect (not (str/includes? text "dataclasses.asdict")))
        (expect (not (str/includes? text "`dict(r)`")))
        (doseq [rule ["A wrong field name raises an error that lists the real fields"]]
          (expect (str/includes? text rule) rule))))
  (it
    "recovers a successful mutation's saved result rather than issuing it again"
    (let [text (str/replace (var-get #'prompt/CORE_SYSTEM_PROMPT) #"\s+" " ")]
      (expect
        (str/includes?
          text
          "When a write succeeded but its print or access failed, read back its saved result. Do not write again")))))

(defdescribe
  prompt-core-test
  ;; Regression: plain-text guidance turned response lists into literal bullet paragraphs.
  (it "does not force plain-text responses"
      (expect (not (str/includes? (prompt/build-system-prompt {}) "Respond in plain text"))))
  ;; LintLang H5/H6: pin the conditions for patch retry and completion.
  (it "states exact retry and completion conditions"
      (let [text (var-get #'prompt/CORE_SYSTEM_PROMPT)]
        (expect (not (str/includes? text "Prompt v1.")))
        (expect (str/includes?
                  text
                  "read the window that the error names, then retry with the current version"))
        (expect (str/includes? text "Exit: EDIT on a failure, else FINISH"))
        ;; User report: a working turn ran silently between calls, where opencode asks for
        ;; short progress notes; the answer must still read on its own without them.
        (expect (str/includes?
                  text
                  "Next to each `python_execution` call, write one sentence for the user"))
        (expect (str/includes? text "it must stand alone without the progress notes"))))
  ;; #303: §7 owns when and where progress notes appear, so a project need not copy that
  ;; policy into AGENTS.md. User decision: the prompt sets no prose style. The removed style
  ;; rule ("short sentences", "a clear actor") added tokens to every request, and a named
  ;; standard drew Claude Opus 5.5 into English and German notes in Polish turns.
  (it "owns progress-note timing without a prose style rule"
      (let [text (str/replace (var-get #'prompt/CORE_SYSTEM_PROMPT) #"\s+" " ")]
        (doseq [banned ["ASD-STE100" "Simplified" "another style" "short sentences" "a clear actor"
                        "one name for each thing"]]
          (expect (not (str/includes? text banned)) banned))
        (doseq [rule ["Next to each `python_execution` call, write one sentence for the user"]]
          (expect (str/includes? text rule) rule))))
  ;; User reports: Claude Opus 5.5 wrote German, then Russian, notes in Polish conversations.
  ;; After the first fix, 934 of 1 084 mid-turn notes in Polish turns were still English. The
  ;; rule named the latest message, which is tool output during a loop, and its fallback counted
  ;; the model's own notes. §7 now lets only the user's requests set the reply language. The core
  ;; prompt names no natural language: even an excluded name draws the model toward it.
  ;; User review: the reply-language rule left the core prompt.
  (it "names no natural language"
      (let [text (str/replace (var-get #'prompt/CORE_SYSTEM_PROMPT) #"\s+" " ")]
        (doseq [stale ["reply language" "keep the language of the conversation" "English"]]
          (expect (not (str/includes? text stale)) stale))))
  ;; Each capability owns its contract; doc() renders Python metadata and semantics.
  ;; The core prompt must point there instead of encouraging invented call shapes.
  (it "points authority at the document a capability carries"
      (let [text (prompt/build-system-prompt {})]
        (expect (str/includes? text "`doc(name)` returns"))
        (expect (str/includes? text "the full contract"))
        (expect (not (str/includes? text "Session titles are host-generated")))))
  (it
    "keeps the sectioned core contract explicit and non-contradictory"
    (let [text (var-get (ns-resolve 'com.blockether.vis.internal.context.prompt
                                    'CORE_SYSTEM_PROMPT))]
      ;; Context safety is worth a small fixed prompt cost; keep the whole core below 4.7k.
      ;; The ratchet must never squeeze out §7's teardown rule again: compressing it to a
      ;; bare "finish clean" is how sessions started leaking background shells. The budget
      ;; moved 4.5k → 4.7k exactly once, when reproduce-first debugging and the "unverified
      ;; covers it" rule landed: those rules pay for themselves, and paying for them by
      ;; shaving other rules' wording is the squeeze this lock exists to stop.
      ;; 4.7k → 4.75k exactly once more, when the merged `shell`/`fs` mega-tools split into
      ;; named verbs: §2's non-blocking rule and §3's five filesystem names are what stop the
      ;; model guessing an `op` discriminator that no longer exists.
      ;; The ceiling has never moved UP for a rewrite: when eighteen tools became one,
      ;; stale discovery and JSON-Schema prose left §1, §2 lost native-vs-Python routing,
      ;; and what replaced them — "ONE call exists", the discovery contract and the folding
      ;; truth — had to fit UNDER the existing budget. It does, at 4 729 chars.
      ;; The win of that change is not here; it is the provider `:tools` payload, which
      ;; went from eighteen JSON Schemas to one.
      ;; 4.75k → 4.8k exactly once more, for the fifty characters that make §6 executable:
      ;; the section ordered a fold and named no callable, so `fold_session`'s NAME and call
      ;; shape now ride inline. A rule the model cannot execute costs its whole section.
      ;; 4.8k → 5k exactly once more, for the budget line: `session_utilization` reports
      ;; `saturation`/`headroom_tokens` against the HARD per-call limit, while every fold
      ;; trigger — the `hint` ladder, the breadcrumb's `% of budget` — is priced against
      ;; `auto_compress_above`. On a 1M-window model 150k of the 200k operating budget reads
      ;; as `saturation 15%, headroom 850k`, so "watch `session[\"utilization\"]`" pointed the
      ;; model at the one pair of numbers that stays calm while the budget empties.
      ;; 5k → 5.5k exactly once more, for the two verbs that give an edit a COORDINATE:
      ;; `cat` mints a line address and `patch` spends it. §3 previously ordered the opposite —
      ;; "CHANGING the tree is plain Python" — and that sentence is gone, but naming both
      ;; verbs, the address format the model has to recognize, and the batch shape ONE
      ;; call takes does not fit in what it freed. Measured against 40 real
      ;; sessions, the instruction it replaces cost 48% of ALL block characters in blocks
      ;; that write a file, 80% of it the old text quoted back; this is the cheaper order.
      ;; 5.5k → 5.9k exactly once more, for §2's shape rule. The sandbox is a PROGRAM the
      ;; session keeps: roots bound once off `session`, results in named variables, one helper
      ;; called again instead of a near-identical block pasted twice, and — when the chore
      ;; outlives the turn — a proposed Python extension in `.vis/extensions/*.py`. Sessions
      ;; that lacked it retyped absolute paths per block, redefined the same helper each block,
      ;; and re-derived what an earlier block had already computed. Naming the `session` key,
      ;; the extension path and `doc("extending")` is what makes the rule executable rather
      ;; than a slogan; the paragraph it replaced ("A result is an ordinary Python value") is
      ;; folded into it. The budget did NOT move for the session-scope correction that
      ;; followed (a `def` lives for the whole session; `session` itself is rebuilt before
      ;; every block and cannot hold one): it was paid for by deleting the glossary
      ;; parenthetical after "higher-order helper", and lands at 5 871.
      ;; 5.9k → 6.05k for the fold KEY grammar. `fold_session` takes a key and a gist and
      ;; nothing else, but §6 named only the verb: the key shape had to be remembered, and a
      ;; guessed one folds nothing while the card still says `folded …`. Spelling the six key
      ;; forms inline is what makes the ordered fold executable; it is paid for by dropping
      ;; "Folding changes rendering, not storage" (the folded-step clause carries it), and
      ;; lands at 5 985.
      ;; 6.05k → 6.1k for the call shapes §3 was missing. The section named its code
      ;; verbs but spelled a callable form for only two (`cat`, `patch`); the ones whose
      ;; contract is a single options dict were named bare, so their shape had to be
      ;; recalled or pulled mid-edit. Measured over 179 gateway journals (1 006 sandbox
      ;; blocks, 326 of them calling a code verb): `grep` called with a bare string,
      ;; `cat` with a dict, and `patch` edits keyed `from_anchor`/`to_anchor` — a key no
      ;; release ever had.
      ;; Each is a refused call and a wasted round trip; the literal dicts cost 71 characters
      ;; and land at 6 056.
      ;; 6.1k → 6.2k for the one line that makes a helper DESCRIBE itself. §2 already ordered the
      ;; model to keep helpers and read them back, but 45 of the 146 documents `apropos` could
      ;; answer were its own `def`s carrying no text at all: an empty gist, a `doc(name)` page
      ;; that was a bare header, and nothing a described ask could match. A docstring is the whole
      ;; of that fix — first line to the listing, the rest to the page — and the rule had to name
      ;; where that line SHOWS UP, or it reads as style advice. It lands at 6 152.
      ;; 6.2k → 6.35k to distinguish questions from implementation requests before any tool rule.
      ;; Answering directly avoids turning an informational question into an unsolicited code change.
      ;; 6.35k → 6.65k for measured semantic-fold cadence. The first benchmark caught a vague gist;
      ;; a real Z.ai GLM-5.3 Flash A/B then showed that forcing a 4k-token fold doubled cost, while
      ;; the exact-route benchmark proved one canonical prior-prefix fold and continuation. The runtime's
      ;; measured 75% hint remains the default threshold instead of inventing an unverified lower one.
      ;; 6.65k → 6.7k because the sandbox has ONE success channel now. The runtime used to hand a
      ;; bare trailing expression's value back as a second result, so "print only what the answer
      ;; needs" read as advice about cost; with that channel deleted an unprinted value is simply
      ;; GONE, and the shape rule in §2 is where a model reads what a block gives back. It lands
      ;; at 6 687.
      ;; 6.7k → 6.8k to state that project_root_path is always available and root is not prebound.
      ;; 6.8k → 7.4k for the read/decision boundary, exact hashline endpoints and parse retries.
      ;; 7.4k → 7.7k for the ls signature, batching and hidden alias contract.
      ;; 7.7k → 8.2k for project-style correctness (#188); scope and verification stay single-owned.
      ;; 8.2k → 8.3k for confirmed paths, direct reads and question-driven discovery.
      ;; 8.3k → 8.5k for optional ls glob filtering and per-path overrides.
      ;; Deduplication keeps the same contracts below the previous 8.5k ceiling.
      ;; 8.1k → 8.7k for bounded helper discovery, explicit cleanup and verified Improve proposals.
      ;; 8.7k → 8.5k: fingerprints and Improve proposals moved to the `doc("defs")` page;
      ;; the prompt keeps helper policy and how the saved definitions follow the namespace.
      ;; 8.5k → 9.1k for #231: one demand-driven discovery policy, including recovery and reuse.
      ;; 9.1k → 9.7k for #232: registered call shape, semantic prose and withheld defaults.
      ;; 9.7k → 10k for #239: type-directed recovery and explicit issue-tracker routing.
      ;; 10k → 10.3k for #259: extension results are field records whose errors list the real fields.
      ;; 10.3k → 10.6k for #262: prior-turn context is the visible conversation, not session history to fetch.
      ;; 10.6k → 10.4k: positive wording — goals instead of prohibitions, one statement per duplicated rule.
      ;; 10.4k → 10.5k: user report — §2 named the shell handle's ops and not the ONE map
      ;; they answer, so a session wrote `sh.wait(60).out` and read AttributeError instead
      ;; of `r["out"]`. The keys cost 67 characters and land at 10 435.
      ;; 10.5k → 10.7k for #271: `fold_count` ships in every context block and was defined
      ;; nowhere, so folded work read as context the conversation lacks; §6 now names it as
      ;; budget telemetry, never a reason to re-read a fold. The clause lands at 10 594.
      ;; 10.6k → 10.7k: user report — §2 only told the model to REUSE helpers, so blocks
      ;; retyped the same steps and `defs()` stayed empty. Factoring one out on the second
      ;; occurrence is a rule again, and it lands at 10 680.
      ;; 10.7k → 10.8k: user report — "you can delete the helper and it is not available in
      ;; `defs` then" was not what §2 said. Deletion now names what it removes and that a
      ;; restart restores only what is still defined. The lifecycle lands at 10 754.
      ;; Frozen-record access and mutation recovery now take 10 881 characters.
      ;; 10.9k → 11.4k: user report — a working turn showed code and results with nothing
      ;; saying why, while opencode's prompts ask for short progress notes. §7 now asks for
      ;; them beside the call, where the transcript already renders prose above each step,
      ;; and says the answer stands alone because a finished turn folds the notes away.
      ;; It lands at 11 378.
      ;; A finished turn now keeps its notes and Activity, so §7 no longer says the notes
      ;; fold away; the rule is 26 characters shorter.
      ;; 11.4k → 11.7k for #303: §7 states when, where and how to write progress notes, so
      ;; projects stop copying that policy into AGENTS.md, and it makes ASD-STE100 the default
      ;; prose standard unless the user or project asks for another style.
      ;; 11.7k → 12.1k for #305: a restart silently lost every variable while the transcript
      ;; still showed it. §2 now says helpers AND variables are saved within stated limits,
      ;; `del` frees memory and a `[Sandbox restarted]` notice names what came back. It lands
      ;; at 12 018.
      ;; 12.1k → 11.55k for the ASD-STE100 rewrite: every rule stays, in short plain sentences
      ;; without hard-wrapped continuation lines, and duplicated wording is merged. It lands
      ;; at 11 499.
      ;; 11.55k → 11.65k: e2e runs called `dict(r)` on a record, so a rule names
      ;; `dataclasses.asdict(r)`, and the discovery table says "Skip discovery." again.
      ;; It lands at 11 584.
      ;; 11.65k → 11.95k: user report — Claude Opus 5.5 wrote German notes in Polish
      ;; conversations, because no rule named the reply language. §7 now names it, the
      ;; fallback for a short or mixed message and the text that must not change it. It lands
      ;; at 11 886.
      ;; 11.95k → 12.15k: user report — after that fix, Claude Opus 5.5 still wrote 934 of 1 084
      ;; mid-turn notes in Polish turns in English, and once Russian. The rule named the latest
      ;; message, which is tool output during a loop, and its fallback counted the model's own
      ;; notes. §7 now lets only the user's requests set the reply language, and the core prompt
      ;; names no natural language. It lands at 12 070.
      ;; #349: the shell handle line says that `sh.type` adds Enter and how to omit it.
      ;; It lands at 12 138.
      ;; `ls` gains `is_ignored`, which lists gitignored entries. It lands at 12 183.
      ;; User report: the style rule named ASD-STE100 (Simplified Technical English), and Claude
      ;; Opus 5.5 wrote English and German notes in Polish turns. §7 names no standard now.
      ;; User decision: §7 drops the prose style rule. It lands about 150 characters lower.
      ;; 12.2k → 12.35k: `ls` pages long listings, so the rule names `limit` and `offset`.
      ;; One rule states the paging that `ls` and `grep` share. It lands at 12 339.
      ;; 12.35k → 12.4k: an eval showed that `r.all()` includes the current page. It lands at 12 378.
      ;; 12.4k → 12.45k: in 357 session turns, 31% of patch calls patched a file again. One rule
      ;; asks to find every change of a file first. It lands at 12 431.
      ;; One system prompt for every channel: §5 tells a non-interactive run not to ask, and the
      ;; CLI-only prompt is gone. It lands at 12 448.
      ;; 12.45k → 12.6k: in a LongMemEval run, Opus kept a chat of 35 turns unfolded, because
      ;; it read folds as tool-step folds only. §6 says that chat turns fold too. It lands at 12 579.
      ;; 12.6k → 13.25k: four §4 rules on the class of a bug, related cases, exact-case measurement
      ;; and live systems. In a CTB screen on GPT-6.1 Sol, the same rules as a task note took solo
      ;; Vis from 3/9 to 7/9 on the same build: fixes reached only the reported case before.
      ;; 13.25k → 13.4k: in CTB, Vis measured a speedup on mismatched graph pairs and a baseline
      ;; with its own client, while the checkers used matching pairs and the running load. It
      ;; claimed 12x to 67000x and got 1.75x and 940x. With per-symptom reproduction, it lands
      ;; at 13 394.
      ;; 13.4k → 13.45k: Vis named a listed symptom, then kept it because DESIGN.md seemed to
      ;; require it. A reported symptom now stays a bug. It lands at 13 434.
      ;; 13.45k → 13.5k: Vis still timed only non-isomorphic graph pairs, which fail fast, and
      ;; the checker timed isomorphic pairs. Each reading is now measured. It lands at 13 490.
      ;; 13.5k → 13.65k: Vis read data files next to a named script, and only the script reached
      ;; the checker. Each named output file now works alone. A later script imported numpy, and
      ;; the checker installed no packages. It lands at 13 636.
      ;; 13.65k → 11.3k: user review of the prompt. The opening, the issue-tracker rule, the
      ;; signature and placeholder rules and the record rule go; `dict(r)` works now. The shell
      ;; handle and the file-work rule move to the shell-gated `sandbox-shims` block, the snapshot
      ;; limits to `doc("python-sandbox")`, and the checker-package rule to the benchmark. It
      ;; lands at 11 290.
      ;; 11.3k → 8.45k: second user review. Option details of `ls`, `grep`, `cat` and `patch` go to
      ;; their docs; the measurement, live-system, draft, commit, release and question rules go;
      ;; the discovery table becomes one signature-first rule, and §6 merges into four rules. It
      ;; lands at 8 423.
      ;; 8.45k → 7.5k: third user review. The result-access, reply-language, service, detach
      ;; and confirmation rules go. It lands at 7 489.
      ;; 7.5k → 7.4k: user decision. The core becomes a state machine: six states with their
      ;; exits, the rules that hold in every state, the call shapes and the context budget. It
      ;; lands at 7 364.
      ;; 7.4k → 3.6k: user decision. The state machine keeps only the minimum: state rules,
      ;; sandbox facts, call shapes and the fold rule. It lands at 3546.
      ;; 3.6k → 4.7k: user decision after CTB round m11. The engineering rules of ub-mode and the
      ;; v7 measurement rule return to EDIT and CHECK.
      (expect (< (count text) 4700))
      (let [steps (mapv #(str/index-of text %) ["`N│ text` rows" "`grep({" "`patch(path, edits)`"])]
        (expect (every? some? steps))
        (expect (apply < steps)))
      ;; User report: a model asked the user a question in a progress note and kept working.
      ;; A question now ends the turn.
      ;; User review: the rule that a question ends the turn moved out of the core prompt.
      (expect (not (str/includes? text "Ask one question only")))
      ;; Regression, user report: a section that ORDERS a verb has to say how it is CALLED.
      ;; Every code verb §3 names carries its literal call shape, and the options-dict ones
      ;; name the keys inside it — the shape is read, never remembered or looked up.
      (doseq [shape ["`grep({\"query\": [needles], \"paths\": [scopes], \"context\": 3})`"
                     "`patch(path, edits)`" "`{\"from\": a, \"to\": b, \"replace\": text}`"]]
        (expect (str/includes? text shape)))
      (expect (str/includes? text "For a question, an analysis or a diff preview, change no files"))
      (expect (< (str/index-of text "change no files") (str/index-of text "## Calls")))
      ;; A helper the model wrote is the only document it can author mid-session, so the rule that
      ;; orders one has to name what its docstring BECOMES — a gist, a page, and a way to be found.
      (expect (str/includes? text "a named helper with a one-line docstring"))
      ;; Session introspection is toggle-gated in foundation-core's dynamic fragment,
      ;; never copied into the static engine prompt.
      (expect (not (str/includes? text "`~/.vis/gateway/events/<id>.ndjson`")))
      ;; File work and the shell handle live in the shell-gated `sandbox-shims` block.
      (expect (not (str/includes? text "only to run programs")))
      ;; Regression, issue #126: list a known parent rather than inventing source roots.
      (expect (str/includes? text "nearest confirmed parent"))
      (expect (str/includes? text "A namespace or package name is only a lead"))
      ;; Regression, issue #267: the batch rule must rule a FILE path out up front —
      ;; one mixed into a batch of directories aborts the listing, it is not skipped.
      ;; User review: the rule lives in the `ls` docstring, which `doc("ls")` serves.
      (expect (not (str/includes? text "Give one `ls` call only confirmed directories")))
      (expect (str/includes? (slurp (io/resource "vis-host-python/ls.py")) "missing path"))
      ;; The routing rule sends every filesystem CHANGE to Python; naming the retired
      ;; verbs again would re-open the `mkdir -p`/`test -f` reflex it exists to close.
      (doseq [verb ["`copy`" "`move`" "`delete`" "`create_directory`" "`file_exists`"]]
        (expect (not (str/includes? text verb))))
      ;; The routing rule survived the arrival of `patch`: a filesystem CHANGE that is not
      ;; an ADDRESSED edit — create, move, delete — is still plain Python.
      (expect (str/includes? text "Create, move and delete files with plain Python"))
      (expect (not (str/includes? text "returns a handle")))
      ;; User decision: the core prompt is a state machine. Each state names its rules and exit.
      (doseq [heading ["## States" "## Always" "## Calls" "## Context"]]
        (expect (str/includes? text heading)))
      (doseq [state ["- ANSWER: " "- ORIENT: " "- REPRODUCE: " "- EDIT: " "- CHECK: " "- FINISH: "]]
        (expect (str/includes? text state) state))
      (expect (= 5 (count (re-seq #"Exit: " text))) "FINISH ends the turn")
      (doseq [required
              ["`apropos(pattern)`" "`doc(name)` returns" "runtime, source, docs, assumption"
               "`doc()` is the index" "Put independent calls in one block" "`await gather(...)`"
               "Keep results in" "Reproduce each symptom before you edit"
               "Show that each symptom is gone" "Name unrelated failures; do not fix them"
               "Write only production code and tests"
               ;; Regression, CTB round m11: without these rules, agents left probe and
               ;; test files in the project root and failed a structural gate.
               "Add a test only where nearby tests show a place"
               "Keep scratch work, probes and benchmarks in sandbox variables, not in files"
               ;; #423: an outer sandbox can deny /tmp; a fixed /tmp path then fails.
               "never a fixed `/tmp` path"
               ;; User decision after CTB round m11: ub-mode rules. Agents called a missed
               ;; latency target a limitation and measured an easier case than the checker.
               "See it fail for the reason that you expect"
               "A guard that only hides a failure is not a fix"
               "Give shared mutable state one writer"
               "again after a crash halfway, must end in the same state" "the way its user calls it"
               "Drop a test that still passes when every function returns nothing"
               "Measure a stated target on the exact stated case in a fresh process"
               "use the slowest" "Keep a wide margin"
               "A missed target is a failure, not a limitation"
               ;; Regression, user report: sessions stopped folding when §6 named no callable.
               ;; Real A/B data showed that a forced early fold doubled cost, so the fold
               ;; waits for the measured hint and a large phase.
               "When `hint` shows and a large phase follows"
               "make `print(fold_session(\"-tN/iK\", gist))` the only call of a step"
               "The gist keeps conclusions, exact paths, evidence, test state and dirty files"]]
        (expect (str/includes? text required)))
      ;; A bare return value produces no stdout: every fold example must print its receipt.
      (doseq [example (re-seq #"`[^`]*fold_session\([^`]*`" text)]
        (expect (str/starts-with? example "`print(fold_session("))
        (expect (str/ends-with? example "))`")))
      ;; These assertions pin prompt content, not model compliance.
      (doseq [required ["Exit: EDIT on a failure, else FINISH"]]
        (expect (str/includes? text required)))
      ;; Regression: a blanket CORE prohibition overrode repository Git opt-in.
      (expect
        (not
          (str/includes?
            text
            "Commit, push, publish, message people, or mutate external systems only when explicitly requested")))
      ;; Regression, user report: blanket resource cleanup stopped a healthy dev server
      ;; that the user had explicitly asked the agent to open and keep available.
      ;; User review: the service, detach and confirmation rules left the core prompt.
      (doseq [required ["Stop a background shell only if it was temporary test machinery"]]
        (expect (str/includes? text required)))
      (expect (not (str/includes? text "A healthy service that the user asked you to run")))
      (expect (not (str/includes? text "stop every session resource you started")))
      ;; `ntr` is gone: with `python_execution` the only call, nothing stores a
      ;; result the model could re-read by coordinate, so the prompt must never
      ;; promise one again.
      (doseq [surplus ["Keep managed REPLs across turns" "ntr[" "# saved:" "ntr.describe()"
                       ;; Regression, issue #ctx-resources: live shells/REPLs left ctx entirely,
                       ;; so no prompt may send the model to a `session["resources"]` that is gone.
                       "session[\"resources\"]"
                       ;; The vocabulary of eighteen doors: naming any of it again re-opens
                       ;; the routing question that having ONE call exists to close.
                       "native tool" "Native tool" "JSON Schema" "advertised" "Direct native tools"
                       "Raise vis bugs/issues" "After 3 failures" "Complete tasks autonomously"
                       "canonical decision table" "anything complicated"
                       ;; schema-owned or removed contracts stay out of the core prompt
                       "stales anchors" "benchmark/profile" "Route vis issues upstream"
                       "Before every `fold_session`" "`await read_session" "≤120 words"
                       "never offer a menu"
                       ;; The sleep/poll prohibition is OWNED by `python_execution`'s own
                       ;; description (pinned in loop_test). §1 already makes native
                       ;; descriptions authoritative, so a core copy is dead weight: the
                       ;; core keeps only the routing rule (background shells → `shell`
                       ;; op `wait`), never the tool-local prohibition.
                       "`time.sleep`" "`asyncio.sleep`" "poll in Python"]]
        (expect (not (str/includes? text surplus))))))
  (it
    "advertises exact model-facing Python capabilities, never internal shim ids"
    (let [shims [{:shim/name "attachments"
                  :shim/globals ["attach" "list_attachments" "get_attachment" "read_attachment"
                                 "show_attachment"]}
                 {:shim/name "fonttools" :shim/imports ["brotli" "fontTools"]}
                 {:shim/name "numpy" :shim/imports ["numpy"]}
                 {:shim/name "pil" :shim/imports ["PIL"]}
                 {:shim/name "tzdata" :shim/imports ["zoneinfo"]}]]
      (with-redefs [extension/sandbox-shims (constantly shims)]
        (let [text (#'prompt/sandbox-shims-prompt-block nil)]
          (expect (< (count text) 1800))
          (expect (not (str/includes? text "apropos")))
          (expect (not (str/includes? text "doc(name)")))
          (expect (str/includes? text "Already imported: `json`"))
          (expect (str/includes? text "real CPython"))
          (expect (str/includes? text "same `~/.vis/python/packages`"))
          (expect (str/includes? text "Imports never install packages"))
          ;; Regression, CTB round m11: an agent probed lxml with a sandbox import, saw
          ;; it missing and avoided a library that the project interpreter had.
          (expect (str/includes? text "not the interpreter of the project"))
          (expect (not (str/includes? text "fetched once")))
          (expect (str/includes? text "Modules from Vis"))
          (doseq [module ["PIL" "brotli" "fontTools" "numpy" "zoneinfo"]]
            (expect (str/includes? text (str "`" module "`"))))
          (expect (str/includes? text "Prebound globals"))
          (doseq [global ["attach" "list_attachments" "get_attachment" "read_attachment"
                          "show_attachment"]]
            (expect (str/includes? text (str "`" global "`"))))
          (expect (not (str/includes? text "`attachments`")))
          ;; NAMES ARE A TRAP IN BOTH DIRECTIONS — the sandbox is real CPython, so
          ;; `numpy` is numpy, while a name Vis publishes itself reaches the HOST and
          ;; never PyPI — so the block says which is which and POINTS at the page
          ;; instead of pushing one hand-written bullet per door into every request.
          (expect (str/includes? text "never PyPI"))
          (expect (str/includes? text "doc(\"<name>\")"))
          (expect (not (str/includes? text "- `numpy`")))
          (expect (not (str/includes? text "- `brotli`")))
          ;; With no shell layer active the block must SAY the process surface is
          ;; gone: silence read as "maybe try `subprocess`", and every attempt then
          ;; died on an opaque spawn failure instead of being ruled out up front.
          ;; Verbatim from the ONE source, so the prompt cannot drift from what
          ;; `subprocess` raises and what an undriveable handle reports.
          (expect (str/includes? text (get env-python/PROCESS_SURFACE "off")))
          (expect (str/includes? text "Shell commands are off"))
          (expect (str/includes? text "Nothing here can start a process"))
          (doseq [banned ["subprocess" "os.system" "os.popen"]]
            (expect (str/includes? text banned)))
          (expect (not (str/includes? text "route through the active")))
          (doseq [name env-python/AUTO_IMPORTED_PYTHON_NAMES]
            (expect (str/includes? text (str "`" name "`"))))))))
  (it "bans subprocess and states the shell handle only when shell is active"
      ;; User review: shell rules cost every request, so they ride only with an active shell.
      (let [text
            (#'prompt/sandbox-shims-prompt-block
             [{:ext/engine {:ext.engine/symbols [{:ext.symbol/symbol 'shell}]}}])

            off
            (#'prompt/sandbox-shims-prompt-block [])]

        (expect (str/includes? text (get env-python/PROCESS_SURFACE "ban")))
        (expect (str/includes? text "cannot start a process"))
        (expect (str/includes? text "`shell` verb"))
        (expect (not (str/includes? text "Shell commands are off")))
        (expect (str/includes? text "subprocess"))
        (expect (str/includes? text "os.system"))
        (expect (str/includes? text "os.popen"))
        (doseq [rule ["Use `shell` only to run programs" "returns a handle"
                      "`logs(-n)` (last n lines)" "each returns `out`, `exit` and `status`"
                      "`r = (await shell(cmd)).wait(300)`"]]
          (expect (str/includes? text rule) rule)
          (expect (not (str/includes? off rule)) rule))
        (expect (not (str/includes? text "\"id\"")))))
  (it "pushes every REGISTERED shim's names, and its prose nowhere"
      ;; The registry itself, not a fixture: the model gets exactly these names. The
      ;; prose that used to ride along — one hand-written bullet per shim — now lives
      ;; in each module's own Python `__doc__`, is harvested into
      ;; the manifest-listed shim apropos resource, and is PULLED by `doc(name)`. A block
      ;; that grows back into prose is a context regression, not documentation.
      (let [text
            (#'prompt/sandbox-shims-prompt-block
             [{:ext/engine {:ext.engine/symbols [{:ext.symbol/symbol 'shell}]}}])

            shims
            (extension/sandbox-shims)]

        (expect (seq shims))
        (doseq [nm (mapcat #(concat (:shim/imports %) (:shim/globals %)) shims)]
          (expect (str/includes? text (str "`" nm "`"))
                  (str nm " is installed but the prompt never names it")))
        (doseq [shim (filter :shim/docs shims)]
          (expect (not (str/includes? text (subs (:shim/docs shim) 0 40)))
                  (str (:shim/name shim) " pushes its pulled page into every request")))
        ;; 1.5k → 1.55k: user report — a session read `blockether.vis.__file__` as proof the
        ;; import was broken, so the frozen SDK's missing on-disk path is named here.
        ;; 1.55k → 1.75k: the shell handle and the file-work rule moved here from the core.
        ;; 1.75k → 1.95k: CTB round m11 — name the sandbox as the Vis environment and send
        ;; project package checks to the project interpreter through `shell`.
        ;; 1.95k → 1.6k: user review cut the build-global, install and shell prose.
        (expect (< (count text) 1600)))))

(defdescribe
  project-instructions-hoist-test
  (it "injects primary guidance as a dedicated PROJECT-INSTRUCTIONS system block"
      (with-redefs [agents/primary-instructions
                    (constantly
                      {:found? true
                       :source :repo
                       :path (str (System/getProperty "user.home") "/repo/AGENTS.md")
                       :content
                       "PROJECT-RULE-FROM-AGENTS-MD\nreproduce -> inspect -> minimal change"})

                    agents/added-root-guidance-index
                    (constantly [])]

        (let [env
              {:extensions (atom [])}

              messages
              (prompt/assemble-stable-prompt-messages env {:active-extensions []})

              text
              (prompt/stable-prompt-text messages)]

          (expect (str/includes? text "PROJECT-INSTRUCTIONS"))
          (expect (str/includes? text "PROJECT-RULE-FROM-AGENTS-MD"))
          (expect (str/includes? text "~/repo/AGENTS.md"))
          (expect (not (str/includes? text
                                      (str (System/getProperty "user.home") "/repo/AGENTS.md"))))
          (expect (str/includes? text "CORE wins"))
          (expect (< (str/index-of text "SYSTEM-PROMPT")
                     (str/index-of text "PROJECT-INSTRUCTIONS"))))))
  (it "indexes added-root guidance without injecting its contents"
      (with-redefs [agents/primary-instructions
                    (constantly {:found? true
                                 :files [{:scope :project
                                          :source :agents-md
                                          :path (str (System/getProperty "user.home")
                                                     "/vis/AGENTS.md")
                                          :content "VIS-RULE"}]})

                    agents/added-root-guidance-index
                    (constantly [{:root (str (System/getProperty "user.home") "/demo")
                                  :path (str (System/getProperty "user.home") "/demo/AGENTS.md")
                                  :source :agents-md}])]

        (let [env
              {:extensions (atom [])}

              messages
              (prompt/assemble-stable-prompt-messages env {:active-extensions []})

              text
              (prompt/stable-prompt-text messages)]

          (expect (str/includes? text "VIS-RULE"))
          (expect (str/includes? text "~/demo — guidance: ~/demo/AGENTS.md"))
          (expect (str/includes? text "guidance is not loaded yet"))
          (expect (str/includes? text "read its exact guidance path in `python_execution`"))
          (expect (not (str/includes? text "DEMO-RULE"))))))
  ;; Regression, #399: a glob-loaded extension of a nested project must bring that
  ;; project's rules, as a session opened at the project would read them.
  (it "indexes the guidance of a nested extension project without its contents"
      (let [root
            (.getCanonicalPath (.toFile (java.nio.file.Files/createTempDirectory
                                          "vis-nested-rules"
                                          (make-array java.nio.file.attribute.FileAttribute 0))))

            project
            (java.io.File. root "repos/a")]

        (.mkdirs (java.io.File. project ".vis"))
        (spit (java.io.File. project "AGENTS.md") "REPO-A-RULE")
        (with-redefs [agents/primary-instructions
                      (constantly {:found? false})

                      agents/added-root-guidance-index
                      (constantly [])]

          (binding [workspace/*workspace-root* root]
            (let [ext
                  {:ext/name "glob-a" :ext/description "Nested tools." :ext/project-path "repos/a"}
                  text (prompt/stable-prompt-text (prompt/assemble-stable-prompt-messages
                                                    {:extensions (atom [])}
                                                    {:active-extensions [ext]}))]

              (expect (str/includes?
                        text
                        (str (.getPath project) " — guidance: " (.getPath project) "/AGENTS.md")))
              (expect (str/includes? text "guidance is not loaded yet"))
              (expect (not (str/includes? text "REPO-A-RULE"))))))))
  (it "falls back to CLAUDE.md when primary AGENTS.md is absent"
      (with-redefs [agents/primary-instructions
                    (constantly {:found? true
                                 :source :repo:claude-md-fallback
                                 :path "/tmp/repo/CLAUDE.md"
                                 :content "CLAUDE-FALLBACK-RULE"})

                    agents/added-root-guidance-index
                    (constantly [])]

        (let [text (-> (prompt/assemble-stable-prompt-messages {:extensions (atom [])}
                                                               {:active-extensions []})
                       prompt/stable-prompt-text)]
          (expect (str/includes? text "CLAUDE-FALLBACK-RULE"))
          (expect (str/includes? text "CLAUDE.md")))))
  (it "emits no PROJECT-INSTRUCTIONS block when no guidance is available"
      (with-redefs [agents/primary-instructions
                    (constantly {:found? false})

                    agents/added-root-guidance-index
                    (constantly [])]

        (let [text (-> (prompt/assemble-stable-prompt-messages {:extensions (atom [])}
                                                               {:active-extensions []})
                       prompt/stable-prompt-text)]
          (expect (not (str/includes? text "PROJECT-INSTRUCTIONS")))))))

(defdescribe prompt-names-one-tool-test
             ;; The whole surface is ONE call now. Any sentence that still names a second
             ;; door, a schema or a stored result re-opens a routing question the model can
             ;; no longer act on, so the assembled prompt is checked for the old vocabulary
             ;; rather than only the core string.
             (it "names `python_execution`, and nothing from the eighteen-door vocabulary"
                 (manifest/initialize!)
                 (let [text (prompt/stable-prompt-text
                              (prompt/assemble-stable-prompt-messages
                                {}
                                {:active-extensions (vec (extension/registered-extensions))}))]
                   (expect (str/includes? text "python_execution"))
                   (doseq [gone ["ntr[" "# saved:" "native tool" "Native tool" "native tools"
                                 "advertised tool"]]
                     (expect (not (str/includes? text gone)))))))

(defdescribe extension-fragments-do-not-restate-doc-text-test
             ;; An `:ext/prompt-fn` fragment is PUSHED into every request; a symbol's own
             ;; document is PULLED once with `doc(name)`. Pasting a signature, a return
             ;; shape or a description up here is exactly how the tokens the one-tool
             ;; surface saved come straight back — and it makes a second contract that
             ;; drifts from the one that runs.
             (it "keeps every active fragment free of the text `doc(name)` already answers"
                 (manifest/initialize!)
                 (let [exts
                       (vec (extension/registered-extensions))

                       block
                       (str (#'prompt/extensions-prompt-block {} exts))

                       entries
                       (vec (for [ext
                                  exts

                                  entry
                                  (extension/ext-symbols ext)]

                              entry))

                       doc-first-lines
                       (into #{}
                             (comp (keep extension/symbol-doc-text)
                                   (map #(str/trim (first (str/split-lines %))))
                                   (remove str/blank?))
                             entries)

                       fragment-lines
                       (into [] (comp (map str/trim) (remove str/blank?)) (str/split-lines block))]

                   (expect (seq entries))
                   (expect (seq doc-first-lines))
                   (expect (not (str/blank? block)))
                   ;; a doc's own first line, copied into the prompt
                   (expect (empty? (filter doc-first-lines fragment-lines)))
                   ;; the raw-result contract belongs to `doc(name)`, never to a fragment
                   (expect (not (str/includes? block "Raw result:")))
                   ;; a declared signature: `name(args) -> shape`
                   (expect (nil? (re-find #"\w\([^)\n]*\)\s*->" block))))))

(defdescribe extension-activation-test
             (it "assembles from precomputed active extensions without activating again"
                 (let [calls
                       (atom 0)

                       ext
                       {:ext/name "test.activation"
                        :ext/activation-fn (fn [_]
                                             (swap! calls inc)
                                             true)
                        :ext/prompt-fn (constantly "Active prompt")}

                       env
                       {:extensions (atom [ext])}

                       active
                       (prompt/active-extensions env)]

                   (expect (= 1 @calls))
                   (prompt/assemble-stable-prompt-messages env {:active-extensions active})
                   (expect (= 1 @calls)))))

;; 1x1 red PNG — REAL pixels: the send gate decodes every image block it emits,
;; so a fake base64 payload is (correctly) refused and never reaches the wire.
(def ^:private tiny-png-b64
  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg==")

(defdescribe
  assemble-initial-messages-images-test
  "Image attachments turn the initial user message multimodal."
  (it "keeps text-only messages as a plain content string"
      (let [msgs
            (prompt/assemble-initial-messages {:initial-user-content "hello"})

            user
            (last msgs)]

        (expect (= 1 (count msgs)))
        (expect (= "user" (:role user)))
        (expect (string? (:content user)))
        (expect (str/includes? (:content user) "USER-REQUEST"))
        (expect (not (str/includes? (:content user) "ATTACHED-IMAGES")))))
  (it "rides svar image blocks ahead of the text block and lists a manifest"
      (let [msgs
            (prompt/assemble-initial-messages
              {:initial-user-content "what is on /tmp/shot.png?"
               :user-images [{:path "/tmp/shot.png"
                              :media-type "image/png"
                              :base64 tiny-png-b64
                              :size 5
                              :size-label "5B"}]
               :skipped-images [{:path "/tmp/huge.png"
                                 :reason "6.0MB exceeds the 5.0MB attachment limit"}]})

            user
            (last msgs)

            blocks
            (:content user)]

        (expect (= "user" (:role user)))
        (expect (vector? blocks))
        ;; image block first (svar/user contract), text block last
        (expect (= "image_url" (:type (first blocks))))
        (expect (str/includes? (get-in (first blocks) [:image_url :url])
                               (str "data:image/png;base64," tiny-png-b64)))
        (let [text (:text (last blocks))]
          (expect (str/includes? text "USER-REQUEST"))
          (expect (str/includes? text "ATTACHED-IMAGES"))
          (expect (str/includes? text "/tmp/shot.png (image/png,"))
          (expect (str/includes? text "NOT attached"))
          (expect (str/includes? text "/tmp/huge.png")))))
  (it "maps stable composer references to image block order, including skipped images"
      (let [prepared
            (attachments/prepare-inline-attachments
              [{:base64 tiny-png-b64 :filename "same.png" :reference "[IMAGE #7]"}
               {:base64 tiny-png-b64 :filename "same.png" :reference "[IMAGE #2]"}])

            messages
            (prompt/assemble-initial-messages
              {:initial-user-content "Compare [IMAGE #2] with [IMAGE #7]"
               :user-images (:attached prepared)
               :skipped-images
               [{:path "missing.png" :reference "[IMAGE #5]" :reason "attachment limit reached"}]})

            blocks
            (:content (last messages))

            text
            (:text (last blocks))]

        (expect (= 2 (count (filter #(= "image_url" (:type %)) blocks))))
        (expect (str/includes? text "- image 1: [IMAGE #7] — same.png"))
        (expect (str/includes? text "- image 2: [IMAGE #2] — same.png"))
        (expect (str/includes? text "- [IMAGE #5] — missing.png — NOT attached"))
        (let [blind (:content (last (prompt/assemble-initial-messages
                                      {:initial-user-content "Compare [IMAGE #2] with [IMAGE #7]"
                                       :user-images (:attached prepared)
                                       :vision? false})))]
          (expect (str/includes? blind "[IMAGE #7] — same.png — NOT attached"))
          (expect (str/includes? blind "[IMAGE #2] — same.png — NOT attached")))))
  (it "drops an image no decoder can read and NAMES it instead of sending a 400"
      ;; A perfect PNG signature + IHDR over an unreadable stream: wire-legal to
      ;; any sniff, and a `Could not process image` 400 that would replay on
      ;; every later turn of the session.
      (let [corrupt
            (.encodeToString (java.util.Base64/getEncoder)
                             (byte-array (concat (take 33
                                                       (.decode (java.util.Base64/getDecoder)
                                                                ^String tiny-png-b64))
                                                 (repeat 24 0))))

            msgs
            (prompt/assemble-initial-messages {:initial-user-content "look"
                                               :user-images [{:path "/tmp/dot.png"
                                                              :media-type "image/png"
                                                              :base64 corrupt
                                                              :size 57
                                                              :size-label "57B"}]})

            user
            (last msgs)]

        (expect (string? (:content user)))
        (expect (str/includes? (:content user) "NOT attached"))
        (expect (str/includes? (:content user) "/tmp/dot.png"))
        (expect (str/includes? (:content user) "could not be decoded"))))
  (it "omits image blocks for a text-only model and demotes them to the manifest"
      (let [msgs
            (prompt/assemble-initial-messages {:initial-user-content "what is on /tmp/shot.png?"
                                               :vision? false
                                               :user-images [{:path "/tmp/shot.png"
                                                              :media-type "image/png"
                                                              :base64 "aGVsbG8="
                                                              :size 5
                                                              :size-label "5B"}]})

            user
            (last msgs)]

        ;; text-only target: plain string content, NO image_url block
        (expect (= "user" (:role user)))
        (expect (string? (:content user)))
        (expect (not (str/includes? (:content user) "image_url")))
        ;; the image is not silently dropped — it is demoted with a reason
        (expect (str/includes? (:content user) "ATTACHED-IMAGES"))
        (expect (str/includes? (:content user) "/tmp/shot.png"))
        (expect (str/includes? (:content user) "NOT attached"))
        (expect (str/includes? (:content user) "no vision"))))
  (it "omits the manifest when there is no user content at all"
      (let [msgs (prompt/assemble-initial-messages
                   {:user-images
                    [{:path "p" :media-type "image/png" :base64 "eA==" :size 1 :size-label "1B"}]})]
        ;; no user message without initial-user-content — images can't ride alone
        (expect (= [] msgs)))))

(defdescribe
  attached-images-descriptions-test
  "When the active model cannot see, a sighted model's report stands in for the
   pixels. The manifest must carry that report AND label it second-hand: an agent
   that thinks it saw the image will testify about detail no one described."
  (let [assemble (fn [descriptions]
                   (:content (last (prompt/assemble-initial-messages
                                     {:initial-user-content "what is on /tmp/shot.png?"
                                      :vision? false
                                      :user-images [{:path "/tmp/shot.png"
                                                     :media-type "image/png"
                                                     :base64 tiny-png-b64
                                                     :size 5
                                                     :size-label "5B"}]
                                      :image-descriptions descriptions}))))]
    (it "quotes the description, names the model and marks it second-hand"
        (let [content (assemble {"/tmp/shot.png" {:text "a red 1x1 pixel" :model "pricey-seer"}})]
          (expect (str/includes? content "/tmp/shot.png"))
          (expect (str/includes? content "a red 1x1 pixel"))
          (expect (str/includes? content "pricey-seer"))
          (expect (str/includes? content "second-hand"))
          ;; Nothing to open with PIL — the content is already here.
          (expect (not (str/includes? content "the ONLY way to see them here")))
          ;; Still no image blocks on a blind wire.
          (expect (string? content))))
    (it "keeps the PIL directive when nothing described the image"
        ;; Toggle off, no sighted model in the fleet, or a refused ask: unchanged.
        (let [content (assemble nil)]
          (expect (str/includes? content "NOT attached"))
          (expect (str/includes? content "PIL"))
          (expect (not (str/includes? content "second-hand")))))
    (it "ignores a description that names a different image"
        (let [content (assemble {"/tmp/other.png" {:text "not this one" :model "seer"}})]
          (expect (not (str/includes? content "not this one")))
          (expect (str/includes? content "PIL"))))
    (it "carries both directives when only some images were described"
        (let [content (:content (last (prompt/assemble-initial-messages
                                        {:initial-user-content "look"
                                         :vision? false
                                         :user-images [{:path "/tmp/a.png"
                                                        :media-type "image/png"
                                                        :base64 tiny-png-b64
                                                        :size 5
                                                        :size-label "5B"}
                                                       {:path "/tmp/b.png"
                                                        :media-type "image/png"
                                                        :base64 tiny-png-b64
                                                        :size 5
                                                        :size-label "5B"}]
                                         :image-descriptions {"/tmp/a.png" {:text "a red pixel"
                                                                            :model "seer"}}})))]
          (expect (str/includes? content "a red pixel"))
          (expect (str/includes? content "NO description"))
          (expect (str/includes? content "PIL"))))
    (it "never lets a description displace pixels a SIGHTED model can read"
        (let [msgs (prompt/assemble-initial-messages
                     {:initial-user-content "look"
                      :user-images [{:path "/tmp/shot.png"
                                     :media-type "image/png"
                                     :base64 tiny-png-b64
                                     :size 5
                                     :size-label "5B"}]
                      :image-descriptions {"/tmp/shot.png" {:text "a red pixel" :model "seer"}}})
              blocks (:content (last msgs))]

          ;; The image rides; the stale description is not printed anywhere.
          (expect (vector? blocks))
          (expect (not (str/includes? (pr-str blocks) "a red pixel")))))))

(defdescribe
  prior-turn-messages-test
  (it "sends a prior turn's request in the shape its own turn sent it"
      (expect (= (mapv :content
                       (prompt/assemble-initial-messages {:turn-context "session[\"turn\"] = 3"
                                                          :initial-user-content "q3"}))
                 (mapv :content (:request (prompt/prior-turn-messages {:turn 3 :request "q3"}))))))
  ;; A replayed request is not the current one. Every request has the same USER-REQUEST
  ;; label, so an earlier request never claims to be the latest.
  (it "labels every request USER-REQUEST, never CURRENT"
      (let [content (-> {:turn 1 :request "q1"}
                        prompt/prior-turn-messages
                        :request
                        first
                        :content)]
        (expect (str/includes? content ";; -- USER-REQUEST --"))
        (expect (not (str/includes? content "CURRENT")))))
  (it "keeps the turn marker when the request is blank"
      (let [[message :as request] (:request (prompt/prior-turn-messages {:turn 2 :request "  "}))]
        (expect (= 1 (count request)))
        (expect (str/includes? (:content message) "session[\"turn\"] = 2"))
        (expect (not (str/includes? (:content message) "USER-REQUEST")))))
  (it "closes a finished turn with its answer, or with nothing when it has none"
      (expect (= [{:role "assistant" :content "a1"}]
                 (:closing (prompt/prior-turn-messages {:turn 1 :request "q1" :answer " a1 "}))))
      (expect (= [] (:closing (prompt/prior-turn-messages {:turn 1 :request "q1"})))))
  (it "lets a real answer replace any partial answer and notice"
      (expect
        (= [{:role "assistant" :content "done"}]
           (:closing
             (prompt/prior-turn-messages
               {:turn 1 :request "q1" :answer "done" :partial-answer "half" :interrupted? true})))))
  (it "closes cancelled work with a model-visible abort marker"
      (let [[notice :as closing] (:closing (prompt/prior-turn-messages {:turn 1
                                                                        :request "inspect and fix"
                                                                        :cancelled? true}))]
        (expect (= 1 (count closing)))
        (expect (= "user" (:role notice)))
        (expect (str/includes? (:content notice) "<turn_cancelled>"))
        (expect (str/includes? (:content notice)
                               "persisted results remain valid; do not repeat settled work"))
        (expect (not (str/includes? (:content notice) "INTERRUPTED before it finished")))))
  ;; Regression, user report in this Vis session: a fork of a running turn reached the
  ;; next turn as a user cancel. The copy closes with a fork marker instead.
  (it "closes a turn that a fork settled with a fork marker, not a cancel"
      (let [[notice :as closing]
            (:closing (prompt/prior-turn-messages
                        {:turn 1 :request "inspect and fix" :cancelled? true :forked? true}))]
        (expect (= 1 (count closing)))
        (expect (= "user" (:role notice)))
        (expect (str/includes? (:content notice) "<turn_forked>"))
        (expect (not (str/includes? (:content notice) "<turn_cancelled>")))))
  ;; Reported from the app: cancelling a turn also dropped the prose the model
  ;; had already sent, so the next turn resumed as if it had said nothing.
  (it "keeps the partial answer a cancelled turn had already produced"
      (let [[said notice] (:closing (prompt/prior-turn-messages
                                      {:turn 1
                                       :request "inspect and fix"
                                       :cancelled? true
                                       :partial-answer
                                       "I patched ChatContent.tsx and was checking the TUI"}))]
        (expect (= {:role "assistant" :content "I patched ChatContent.tsx and was checking the TUI"}
                   said))
        (expect (str/includes? (:content notice) "<turn_cancelled>"))))
  (it "tells an interrupted turn whether its answer is partial or absent"
      (let [partial
            (:closing (prompt/prior-turn-messages {:turn 1
                                                   :request "inspect and fix"
                                                   :interrupted? true
                                                   :partial-answer "found the cause in loop.clj"}))

            absent
            (:closing (prompt/prior-turn-messages
                        {:turn 1 :request "inspect and fix" :interrupted? true}))]

        (expect (= "found the cause in loop.clj" (:content (first partial))))
        (expect (str/includes? (:content (second partial))
                               "the answer above is only what you had said by then"))
        (expect (not (str/includes? (:content (second partial)) "you produced NO answer")))
        (expect (= 1 (count absent)))
        (expect (str/includes? (:content (first absent)) "you produced NO answer")))))

(defdescribe core-prompt-routes-text-edits-to-patch-test
             ;; The verbs exist only if the prompt spends them. Before this, the core
             ;; prompt told the model to CHANGE the tree with `Path.write_text` — which is
             ;; how a 2 KB block that restates the old text becomes the normal way to edit.
             (it "names both versioned verbs and the address they speak"
                 (let [text (prompt/build-system-prompt {})]
                   (expect (str/includes? text "patch(path, edits)"))
                   (expect (str/includes? text "\"replace\""))))
             (it "never tells the model to write a text EDIT in plain Python"
                 (let [text (prompt/build-system-prompt {})]
                   (expect (not (str/includes? text "CHANGING the tree is plain Python")))
                   (expect (not (str/includes? text "are edited in plain Python")))
                   (expect (str/includes? text "Use one `patch(path, edits)` call for each file"))))
             ;; Regression: after grep started answering ONE numbered TEXT block the
             ;; prompt still said only "hits arrive ANCHORED", never WHAT arrives, so the
             ;; model kept treating the answer as a keyed map.
             (it "says grep answers numbered TEXT, models context, and spends several hits at once"
                 (let [text (prompt/build-system-prompt {})]
                   (expect (str/includes? text
                                          "returns a `path @version` line, then `N│ text` rows"))
                   (expect (not (str/includes? text "returns a MAP")))
                   ;; Several hits in ONE file are ONE patch call now, so there is no
                   ;; order left for the caller to compute.
                   (expect (not (str/includes? text "bottom-up")))
                   (expect (str/includes? text "Use one `patch(path, edits)` call for each file"))
                   (expect (str/includes?
                             text
                             "`grep({\"query\": [needles], \"paths\": [scopes], \"context\": 3})`"))
                   ;; User review: paging, `is_files_only` and `offset` live in the grep and ls docs
                   ;; and in the page header of a capped result, not in every request.
                   (expect (not (str/includes? text "`next(r)` continues a capped page")))))
             ;; Regression: `sh.logs` grew the same negative tail `cat` has, and the
             ;; prompt named the method with no arguments at all, so a watcher still
             ;; paged bytes to answer "what did it just print".
             (it "says a shell handle reads its last n LINES"
                 (let [text (#'prompt/sandbox-shims-prompt-block
                             [{:ext/engine {:ext.engine/symbols [{:ext.symbol/symbol 'shell}]}}])]
                   (expect (str/includes? text "`logs(-n)` (last n lines)")))))

;; Regression, user report: cat and a patch with an unseen or placeholder version
;; ran in one block; printing the read cannot inform a prewritten replacement.
(defdescribe
  core-prompt-grounds-patch-retries-test
  (it "waits for an observed version before generating a dependent patch"
      (let [text (prompt/build-system-prompt {})]
        (doseq [rule ["the `@` value of the read" "which must match once"]]
          (expect (str/includes? text rule) rule))))
  ;; Editing e2e used source text instead of the address that a read printed.
  (it "defines both endpoints as line numbers of the version that a read showed"
      (let [text (prompt/build-system-prompt {})]
        (doseq [rule ["For long spans"]]
          (expect (str/includes? text rule) rule))))
  (it "distinguishes changed-lines recovery from invalid replacement syntax"
      (let [text (prompt/build-system-prompt {})]
        (doseq [rule ["A refused patch writes nothing"
                      "read the window that the error names, then retry with the current version"]]
          (expect (str/includes? text rule) rule)))))

(defdescribe core-prompt-ls-contract-test
             ;; Regression: the prompt omitted ls keywords, including the hidden alias.
             (it "names the ls entry point and leaves its signature and options to its doc"
                 (let [text
                       (prompt/build-system-prompt {})

                       ls-doc
                       (slurp (io/resource "vis-host-python/ls.py"))]

                   (expect (str/includes? text "`ls` the nearest confirmed parent"))
                   ;; Eval regression: the rule said `r.all()` gives "the rest", and Codex joined
                   ;; `str(r) + r.all()`, so the first page of a grep was patched twice. User review:
                   ;; the paging and option rules live in the `ls` docstring.
                   (doseq [rule ["`r.all()`" "basename glob" "ancestors"]]
                     (expect (not (str/includes? text rule)) rule)
                     (expect (str/includes? ls-doc rule) rule)))))

(defdescribe core-prompt-helper-lifecycle-test
             ;; Council report 4353: unbounded helper catalogs and versioned names hid reusable work.
             (it "searches before creating and refines the existing binding"
                 (let [text (prompt/build-system-prompt {})]
                   (doseq [rule ["`defs()` lists them" "with a one-line docstring"]]
                     (expect (str/includes? text rule) rule))))
             ;; User report: the goal-form rewrite left only a rule to REUSE helpers, so blocks
             ;; retyped the same steps and `defs()` stayed empty. Keep the rule that creates one.
             (it "factors a repeated block into a named helper"
                 (let [text (prompt/build-system-prompt {})]
                   (expect (str/includes? text "Make a repeated block a named helper"))))
             ;; #305: a recycle lost every variable while the transcript still showed it. The limits,
             ;; the restart notice and the deletion rules live on the `python-sandbox` page.
             (it "states what stays across blocks"
                 (let [text (prompt/build-system-prompt {})]
                   (expect (str/includes? text
                                          "Helpers and variables stay across blocks and turns"))))
             ;; User report: three of seven helper lines described source fingerprints and
             ;; Improve proposals, a rare workflow paid for in every request. That contract
             ;; stays on the `doc("defs")` page and in the context-management guide.
             (it "keeps fingerprints out of the per-request prompt"
                 (let [text (prompt/build-system-prompt {})]
                   (doseq [rule ["SHA-256" "liveness unknown"]]
                     (expect (not (str/includes? text rule)) rule)))))

;; Regression: name the prebound paths and lifetime of reusable helpers so blocks
;; do not redefine paths or helpers that the session already provides.
(defdescribe core-prompt-steers-python-shape-test
             (it "uses the advertised prebound paths instead of defining or guessing aliases"
                 (let [text (prompt/build-system-prompt {})]
                   ;; User report: the runtime always binds project_root_path, never a root alias.
                   (expect (str/includes? text "`project_root_path`"))
                   (expect (str/includes? text "`session[\"workspace\"][\"filesystem_roots\"]`"))
                   (expect (str/includes? text "`python_name`"))
                   (expect (not (str/includes? text "path_globals")))
                   (expect (str/includes? text "Prebound `Path` objects"))
                   (expect (not (str/includes? text "prebound `root`")))
                   (expect (not (str/includes? text "root = Path(session")))
                   (expect (str/includes? text "`await gather(...)`"))))
             ;; Regression, user report ("can I write function definitions into the session
             ;; object and refine them over time?"): §2 said "definitions persist between
             ;; blocks", which reads as within-turn scratch, and called `session` a "read-only
             ;; map" — a write there SUCCEEDS and is erased before the next block, so the
             ;; obvious place to keep a helper is the one place that silently loses it.
             (it "scopes a definition to the whole session and refuses `session` as storage"
                 (let [text (prompt/build-system-prompt {})]
                   (expect (str/includes? text "`defs()`"))
                   (expect (str/includes? text
                                          "Helpers and variables stay across blocks and turns"))
                   (expect (str/includes? text "The host builds `session` again before each block"))
                   (expect (not (str/includes? text "definitions persist between blocks")))
                   (expect (not (str/includes? text "live read-only map"))))))

(defdescribe planning-prompt-test
             (it "adds no planning block, also when a retired plans value is on"
                 (with-redefs [toggles/enabled? (constantly true)]
                   (doseq [channel [:web :tui :cli]]
                     (let [text (prompt/stable-prompt-text (prompt/assemble-stable-prompt-messages
                                                             {:channel channel}
                                                             {:active-extensions []}))]
                       (expect (not (str/includes? text ";; -- PLANS --")))
                       (expect (not (str/includes? text "PLANNING WORKFLOW"))))))))

(defdescribe request-tokenizer-test
             (it "uses a declared tokenizer consistently for priming and marginal message counts"
                 (let [opts
                       {:tokenizer "cl100k_base"}

                       counter
                       (prompt/request-token-counter opts)

                       messages
                       [{:role "user" :content "antidisestablishmentarianism"}
                        {:role "assistant" :content "Measured response"}]]

                   (expect (= (svar/count-messages "gpt-4o" messages opts)
                              (counter "gpt-4o" messages)))
                   (expect (= (svar/count-messages "gpt-4o" [] opts) (counter "gpt-4o" []))))))
