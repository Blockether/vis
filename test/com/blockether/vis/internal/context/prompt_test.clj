(ns com.blockether.vis.internal.context.prompt-test
  (:require [charred.api :as json]
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
            (expect (str/includes? text "bundled SDK takes precedence"))
            ;; Issue #253: importable declarations do not imply extension host access.
            (expect (str/includes? text "Import `blockether.vis.extension`"))
            ;; User report: the bundled SDK is frozen into the runtime, so
            ;; `blockether.vis.__file__` raises AttributeError and reads as a broken import.
            (expect (str/includes? text "frozen: no on-disk `__file__`"))
            (expect (str/includes? text "no registration or extension host APIs"))
            (expect (str/includes? text "do not change instruction priority")))))))

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
  (it "names foldable prior-turn recaps separately from user requests"
      (let [messages
            (prompt/assemble-initial-messages
              {:previous-turn-context
               [{:turn 1 :user-request "prior dense input" :interrupted? true}]
               :initial-user-content "current input"})

            health
            (prompt/request-health {} messages [] "gpt-4")

            labels
            (set (map :label (:breakdown health)))]

        (expect (contains? labels "Turn t1 recap (fold t1)"))
        (expect (contains? labels "User requests"))
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
  (it
    "scopes reuse by model and message, and drops it with the counter"
    (let [message
          {:role "user" :content "Stable request"}

          changed
          (assoc message :content "Changed request")

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
          (expect (= 2 (get (frequencies @calls) ["gpt-4o" [message]])))
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
  cli-autonomous-override-test
  (it "drops the candidate approval STOP for the non-interactive :cli channel only"
      (let [text-for
            (fn [ch]
              (-> (prompt/assemble-stable-prompt-messages {:channel ch} {:active-extensions []})
                  prompt/stable-prompt-text))

            marker
            "NON-INTERACTIVE ONE-SHOT RUN"]

        ;; :cli (headless one-shot — no approver) gets the override
        (expect (str/includes? (text-for :cli) marker))
        (expect (str/includes? (text-for :cli) "Keep working to a finished prose answer"))
        (expect (str/includes? (text-for :cli) "Leave destructive or irreversible work"))
        (expect (not (str/includes? (text-for :cli) "big, risky")))
        ;; interactive / card-bearing channels keep the approval flow
        (expect (not (str/includes? (text-for :tui) marker)))
        (expect (not (str/includes? (text-for :web) marker)))
        (expect (not (str/includes? (text-for nil) marker))))))

(defdescribe
  core-prompt-project-style-test
  ;; Regression: Blockether/vis#188 reported missing spacing between definitions and YAML resources.
  ;; These assertions pin the prompt contract, not model compliance.
  (it "makes project style and logical spacing part of editing correctness"
      (let [text
            (var-get #'prompt/CORE_SYSTEM_PROMPT)

            edit-section
            (second (re-find #"(?s)## 4\. Edit \+ verify\n(.*?)\n## 5\." text))]

        (expect (some? edit-section))
        (doseq [rule ["Treat code/config style as correctness"
                      "project rules and formatter/linter config"
                      "then the consistent nearby examples" "naming, indentation, logical grouping"
                      "blank-line separation between definitions and configuration resources"
                      "whitespace-sensitive values and required document separators"
                      "also in a minimal diff" "YAML `---`"]]
          (expect (str/includes? (str/replace (or edit-section "") #"\s+" " ") rule) rule))))
  (it "owns scope and verification once, separately from response style"
      (let [text
            (var-get #'prompt/CORE_SYSTEM_PROMPT)

            edit-section
            (second (re-find #"(?s)## 4\. Edit \+ verify\n(.*?)\n## 5\." text))]

        (doseq [rule ["preserve unrelated work and formatting" "Cover changed behavior with tests"
                      "run applicable project formatting/lint checks"
                      "review the final diff, including edit boundaries"]]
          (let [pattern (re-pattern (java.util.regex.Pattern/quote rule))
                normalized (str/replace text #"\s+" " ")]

            (expect (= 1 (count (re-seq pattern normalized))) rule)
            (expect (str/includes? (str/replace (or edit-section "") #"\s+" " ") rule) rule)))
        (expect (str/includes? text "## 7. Response and finish"))
        (expect (not (str/includes? text "## 7. Style and finish"))))))

(defdescribe
  core-prompt-scoped-discovery-test
  ;; Regression, user report: agents guessed paths and repeated broad searches after finding owners.
  ;; These assertions pin the instructions, not model compliance.
  (it "confirms filesystem paths before locating unknown code"
      (let [text
            (var-get #'prompt/CORE_SYSTEM_PROMPT)

            steps
            (mapv #(str/index-of text %)
                  ["`ls` the nearest confirmed parent"
                   "`grep` locates unknown code in confirmed paths"])]

        (expect (every? some? steps))
        (when (every? some? steps) (expect (apply < steps)))
        (expect (str/includes? text "initially `project_root_path`"))
        (expect (str/includes?
                  text
                  "A path is confirmed by a listing, a hit or an explicit project/user reference"))
        (expect (not (str/includes? text "`grep(...)` FIRST")))))
  (it "reads known regions directly instead of rediscovering them"
      (let [text (var-get #'prompt/CORE_SYSTEM_PROMPT)]
        (expect (str/includes? text "read known regions directly, without rediscovery"))))
  (it
    "limits further reads to unresolved questions and scopes searches to the owner"
    (let [text (var-get #'prompt/CORE_SYSTEM_PROMPT)]
      (doseq
        [rule
         ["identify the unresolved question affecting the next step" "if none, stop reading"
          "Search the known owner; broaden only for an unresolved caller, dependency or contract"]]
        (expect (str/includes? text rule) rule)))))

(defdescribe
  core-prompt-demand-driven-discovery-test
  ;; Regression: #231 repeated discovery after /reload despite an already known contract.
  ;; These assertions pin the base prompt's decision rules, not model compliance.
  (it "reuses facts from system instructions and the visible conversation"
      (let [text (str/replace (var-get #'prompt/CORE_SYSTEM_PROMPT) #"\s+" " ")]
        (doseq
          [rule
           ["Reuse signatures and preconditions from the system prompt and the visible conversation"
            "so `apropos()`, `doc()` and `inspect.signature()` serve new facts only"
            "a known fact stays known across turns, `/reload` and repeated calls"
            "None | Call directly; skip discovery"]]
          (expect (str/includes? text rule) rule))))
  (it "requires a missing fact or evidence of a changed contract before rediscovery"
      (let [text (str/replace (var-get #'prompt/CORE_SYSTEM_PROMPT) #"\s+" " ")]
        (expect (str/includes? text "Refresh on contract-change evidence"))))
  (it "uses known recovery without treating every operational failure as a discovery failure"
      (let [text (str/replace (var-get #'prompt/CORE_SYSTEM_PROMPT) #"\s+" " ")]
        (expect (str/includes? text "operational failures use known recovery"))))
  (it
    "does not refresh known signatures to resolve a missing semantic detail"
    (let [text (str/replace (var-get #'prompt/CORE_SYSTEM_PROMPT) #"\s+" " ")]
      (expect
        (str/includes?
          text
          "Semantics | Name the missing precondition/effect/unit/retry/limit, then `doc(name)` for that one contract"))
      (expect (not (str/includes? text "Still missing after inspection")))))
  (it
    "consolidates the decision matrix with discovery contracts in the base prompt"
    (let [text
          (prompt/build-system-prompt {})

          section
          (second (str/split text #"## 1\. Identity \+ Epistemic stance" 2))

          discovery
          (some-> section
                  (str/split #"## 2\. Execution surfaces" 2)
                  first)]

      (expect (some? discovery))
      (when discovery
        (doseq
          [rule
           ["Discovery is demand-driven"
            "identify the unresolved question affecting the next step; if none, stop reading"
            "Discovery matrix: first matching row, then reassess"
            "Symbol name | One narrow `apropos(pattern)` in the known namespace; broaden only after no useful match"
            "Arguments | `import inspect; print(inspect.signature(fn))`."
            "Semantics | Name the missing precondition/effect/unit/retry/limit"
            "Result shape | `doc(name)` lists return-model fields under Model schemas"
            "`apropos(pattern)` filters SYMBOL names" "`doc(name)` returns"
            "obey its stated preconditions"]]
          (expect (str/includes? discovery rule) rule)))
      (expect (= 1 (count (re-seq #"Discovery matrix:" text))))
      (expect (= 1 (count (re-seq #"Discovery is demand-driven" text))))
      (expect (= 1
                 (count (re-seq #"identify the unresolved question affecting the next step" text))))
      (expect (not (str/includes? text "Unknown call shape: use narrow `doc(name)`"))))))

(defdescribe
  core-prompt-prior-turn-context-test
  ;; Regression: #262 "prior work or recovered context" read as an instruction to fetch
  ;; the current session's history at task start although the conversation held the task.
  (it
    "treats prior-turn context as visible conversation, not session history to fetch"
    (let [text (str/replace (prompt/build-system-prompt {}) #"\s+" " ")]
      (doseq
        [rule
         ["Reuse signatures and preconditions from the system prompt and the visible conversation"
          "Prior-turn context | Already in the visible conversation, fold gists included"
          "continue from it, also when the request reads like a continuation (\"now…\", \"taking into account…\")"
          "Session history serves a named question the conversation leaves open"]]
        (expect (str/includes? text rule) rule))
      (expect (not (str/includes? text "recovered context"))))))

(defdescribe
  core-prompt-registered-python-contract-test
  ;; #232: pin invocation authority without encouraging repeated discovery or copied schemas.
  (it "prefers signature inspection without claiming it exposes types or effects"
      (let [text (str/replace (prompt/build-system-prompt {}) #"\s+" " ")]
        (doseq
          [rule
           ["Registered signatures/types own kinds, requiredness/defaults, returns and mutation tag"
            "inspection may omit types/effects"]]
          (expect (str/includes? text rule) rule))))
  (it
    "keeps semantic documentation and default safety without duplicating structure"
    (let [text (str/replace (var-get #'prompt/CORE_SYSTEM_PROMPT) #"\s+" " ")]
      (doseq
        [rule
         ["Name the missing precondition/effect/unit/retry/limit, then `doc(name)` for that one contract"
          "obey its stated preconditions"
          "A docstring adds intent and preconditions; the registry already carries signature, defaults and schema"
          "Omit optional arguments to take their defaults; a `...` shown in a signature is a placeholder"]]
        (expect (str/includes? text rule) rule))))
  (it "filters available full metadata before printing omitted schema details"
      ;; #234: a full contract dump erased compact-doc savings in real-model E2E.
      (let [text (str/replace (prompt/build-system-prompt {}) #"\s+" " ")]
        (doseq [rule ["Traverse available `fn.contract` in memory"
                      "`fields` is a list of `{name, type}`" "Print the matching leaves"
                      "Inspect unknown shapes"]]
          (expect (str/includes? text rule) rule)))))

(defdescribe
  core-prompt-execution-invariants-test
  ;; Compression must retain executable contracts, not just capability names.
  (it "keeps independent batching separate from dependent observations"
      (let [text (var-get #'prompt/CORE_SYSTEM_PROMPT)]
        (doseq [rule ["plural arguments first" "`await gather(...)` for" "independent calls"
                      "Reuse results" "print the needed fields or keys/types"
                      "END the block, then decide in the NEXT block"]]
          (expect (str/includes? text rule) rule))))
  ;; Regression, user report: the handle line named the ops and not the ONE map they
  ;; answer, so a session wrote `sh.wait(60).out` and read AttributeError, not `r["out"]`.
  (it "preserves output and watched shell handles"
      (let [text (var-get #'prompt/CORE_SYSTEM_PROMPT)]
        (doseq [rule ["keep results in variables" "`print()` is the ONE channel back"
                      "what you print is what returns" "answers a HANDLE" "`sh.logs(-50)`"
                      "`sh.wait(s)`" "`sh.stop()`" "every op answers the SAME map" "`r[\"out\"]`"
                      "`r[\"exit\"]`"]]
          (expect (str/includes? text rule) rule))))
  ;; #239: pin type-directed access and recovery without adding another discovery preflight.
  (it "names both result access spellings and inspects only unknown shapes"
      (let [text (str/replace (var-get #'prompt/CORE_SYSTEM_PROMPT) #"\s+" " ")]
        (doseq [rule ["every result answers BOTH spellings"
                      "`r['key']` and `r.key` on a result map or `session`"
                      "`r.field` and `r['field']` on a record"
                      "Inspect unknown shapes via keys/types or `dir(value)`"
                      "Use the keys and fields an error lists"]]
          (expect (str/includes? text rule) rule))))
  ;; #259: a record's fields come from its model and its errors, never from guessed synonyms.
  (it "reads extension records through their public fields and error-listed names"
      (let [text (str/replace (var-get #'prompt/CORE_SYSTEM_PROMPT) #"\s+" " ")]
        (doseq [rule ["An extension result is a frozen record of its public fields"
                      "methods excluded: its declared sequences iterate"
                      "a wrong name raises KeyError/AttributeError listing the real fields"]]
          (expect (str/includes? text rule) rule))))
  (it
    "recovers a successful mutation's saved result rather than issuing it again"
    (let [text (str/replace (var-get #'prompt/CORE_SYSTEM_PROMPT) #"\s+" " ")]
      (expect
        (str/includes?
          text
          "After a successful write whose print or access failed, read back its saved result; the write already happened"))))
  (it "honors the requested issue tracker without assuming a project-specific extension exists"
      (let [text (var-get #'prompt/CORE_SYSTEM_PROMPT)]
        (expect (str/includes?
                  text
                  "Route issues to the named repository/tracker via installed tools or its CLI"))
        (expect (str/includes? text "GitHub slugs are not Jira project keys"))
        (expect (not (str/includes? text "vis.issue_create"))))))

(defdescribe
  prompt-core-test
  ;; LintLang H5/H6: pin response defaults and the conditions for patch retry and completion.
  (it "states the output default and exact retry and completion conditions"
      (let [text (var-get #'prompt/CORE_SYSTEM_PROMPT)]
        (expect (str/includes? text "Prompt v1."))
        (expect (str/includes?
                  text
                  "Respond in plain text unless the user or tool requires another format."))
        (expect (str/includes? text "for stale anchors, read only the indicated region"))
        (expect (str/includes? text "After changed-file checks pass"))))
  ;; Each capability owns its contract; doc() renders Python metadata and semantics.
  ;; The core prompt must point there instead of encouraging invented call shapes.
  (it "points authority at the document a capability carries"
      (let [text (prompt/build-system-prompt {})]
        (expect (str/includes? text "`doc(name)` returns"))
        (expect (str/includes? text "the authoritative contract"))
        (expect (str/includes? text "obey its stated preconditions"))
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
      ;; `cat` mints `line:hash` and `patch` spends it. §3 previously ordered the opposite —
      ;; "CHANGING the tree is plain Python" — and that sentence is gone, but naming both
      ;; verbs, the anchor format the model has to recognize, and the batch shape ONE
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
      (expect (< (count text) 10900))
      (let [steps (mapv #(str/index-of text %)
                        ["`grep` locates unknown code" "a hit IS a `patch` argument"
                         "`patch(path, edits)`"])]
        (expect (every? some? steps))
        (expect (apply < steps)))
      ;; Regression, user report: a section that ORDERS a verb has to say how it is CALLED.
      ;; Every code verb §3 names carries its literal call shape, and the options-dict ones
      ;; name the keys inside it — the shape is read, never remembered or looked up.
      (doseq [shape ["`grep({\"query\": [needles], \"paths\": [scopes], \"context\": 3})`"
                     "`cat(path, start, end)`" "`patch(path, edits)`"
                     "`[{\"from\": a, \"to\": b, \"replace\": text}]`"]]
        (expect (str/includes? text shape)))
      (expect (str/includes?
                text
                "Answer questions without coding; use tools only for missing information."))
      (expect (< (str/index-of text "Answer questions without coding")
                 (str/index-of text "## 3. Inspect")))
      ;; A helper the model wrote is the only document it can author mid-session, so the rule that
      ;; orders one has to name what its docstring BECOMES — a gist, a page, and a way to be found.
      (expect (str/includes? text "one-line docstring supplies its `defs()` gist"))
      ;; Session introspection is toggle-gated in foundation-core's dynamic fragment,
      ;; never copied into the static engine prompt.
      (expect (not (str/includes? text "`~/.vis/gateway/events/<id>.ndjson`")))
      (expect (str/includes? text "locates unknown code"))
      (expect (str/includes? text "**Filesystem and data work (YAML/JSON/TOML/CSV) are Python**"))
      ;; Regression, issue #126: list a known parent rather than inventing source roots.
      (expect (str/includes? text "confirmed directories"))
      (expect (str/includes? text "a namespace or package name is a lead to confirm"))
      ;; Regression, issue #267: the batch rule must rule a FILE path out up front —
      ;; one mixed into a batch of directories aborts the listing, it is not skipped.
      (expect (str/includes? text "Batch confirmed directories only"))
      (expect (str/includes? text "one file or missing path aborts the call"))
      (expect (str/includes? text "`cat` reads a file"))
      ;; The routing rule sends every filesystem CHANGE to Python; naming the retired
      ;; verbs again would re-open the `mkdir -p`/`test -f` reflex it exists to close.
      (doseq [verb ["`copy`" "`move`" "`delete`" "`create_directory`" "`file_exists`"]]
        (expect (not (str/includes? text verb))))
      ;; The routing rule survived the arrival of `patch`: a filesystem CHANGE that is not
      ;; an ADDRESSED edit — create, move, delete — is still plain Python.
      (expect (str/includes? text "creating/moving/deleting is plain Python"))
      ;; Shell is a Python call, so the core must say WHERE it lives.
      (expect (str/includes? text "`shell(...)` runs programs"))
      (expect (str/includes? text "answers a HANDLE"))
      (doseq [heading ["## 1. Identity + Epistemic stance" "## 2. Execution surfaces"
                       "## 3. Inspect" "## 4. Edit + verify" "## 5. Act autonomously"
                       "## 6. Manage context" "## 7. Response and finish"]]
        (expect (str/includes? text heading)))
      (doseq [required
              ["Host project default" "`apropos(pattern)` filters SYMBOL names"
               "`doc(name)` returns" "runtime > source > docs > assumption"
               "obey its stated preconditions" "the curated index"
               "A skill is one of those documents" "`python_execution`" "ONE call exists"
               "there is no tool to choose" "Batch independent work in ONE block"
               "`await gather(...)` for"
               ;; No tool blocks on the model's behalf: the old `shell` op `wait`/`until`
               ;; is gone, so core routes to background + a poll the model can read.
               ;; Regression, issue #137: the handle line spelled `sh.type()` among the
               ;; status accessors, so following it verbatim raised a TypeError —
               ;; `type` SENDS keystrokes and its text argument is required.
               "answers a HANDLE" "`sh.logs(-50)`" "`sh.wait(s)`" "`sh.type(\"y\")`"
               "Factor a repeated loop or block into a small named helper" "keep results in"
               ;; The sandbox has ONE success channel: `print()`. Naming it is what makes
               ;; "print only what the answer needs" a contract instead of cost advice.
               "`print()` is the ONE channel back" "what you print is what returns"
               "Inspect unknown shapes" "keep the reproduction as a suite test"
               "rerun it after the fix" "Cover changed behavior with tests"
               "Write only files the task asked" "Commit and push" "Treat context as a budget"
               ;; The model must know this is the latest available provider measurement,
               ;; not a live token count or cumulative turn usage. Fold pressure
               ;; compares it to the soft budget, not to the hard input ceiling.
               "`latest_measured_input_tokens`" "not a live count" "`auto_compress_above`"
               "`model_input_limit`" "`hint` arms at 75%"
               "provider-cache metrics are available in diagnostics"
               ;; `session_drop` is gone: omitting the gist IS the discard, and a model
               ;; that does not know that writes a useless gist instead of dropping.
               "the gist discards outright"
               ;; Regression, user report: sessions stopped folding. §6 ORDERED the fold
               ;; but named no callable, so `fold_session` had to be remembered or
               ;; rediscovered through `doc()` — every other verb in the core is named.
               "Fold obsolete settled work: always `print(fold_session(key, gist))`"
               ;; Regression, user report: a fold that "saved 0 tokens". §6 named the verb
               ;; but not the KEY it takes, so the shape was guessed — a selector structure
               ;; or a bare id that resolved to nothing. The key grammar is in the core now.
               "STRING key" "`\"-t2/i9\"` everything through it"
               ;; Nothing stores a folded step for later: the gist is the whole survivor,
               ;; and a prompt that hints otherwise buys a fold the model regrets.
               "a folded step leaves the context, so the gist is what the conversation keeps"
               ;; Regression, user report: the benchmark said folding worked only because the task
               ;; ordered it. Real Z.ai GLM-5.3 Flash A/B data showed a forced 4k settled-prefix fold
               ;; doubled cost. Use the runtime's measured hint, require enough future work to amortize
               ;; the cache reset, and pin the exact oldest-prefix call instead of an arbitrary trigger.
               "research-to-implementation boundary" "`hint` as the default fold threshold"
               "Require a substantial next" "repeated large/clipped results"
               "clearly worth one cache reset" "beat append-only history"
               "Make the next iteration only" "`print(fold_session(\"-tN/iK\", gist))`"
               "last completed research step" "oldest settled prefix folds" "live step stays out"
               "one cache discontinuity" "One broad fold"
               ;; Regression, user report: Anthropic, OpenAI and Z.ai all continued from one
               ;; multi-turn fold without a read/fold loop, but transcript-like gists retained
               ;; raw logs and complete tests. Pin minimum sufficient narrowing, not just survival.
               "continue append-only from its gist, which already covers the settled work"
               "minimum sufficient checkpoint" "not a transcript" "conclusions, unknowns"
               "exact paths/symbols" "decisive evidence"
               "verification, edit/test state and dirty files"
               "omit raw outputs and full files/tests" "confirm reduction"]]
        (expect (str/includes? text required)))
      ;; A bare return value produces no stdout: every fold example must print its receipt.
      (doseq [example (re-seq #"`[^`]*fold_session\([^`]*`" text)]
        (expect (str/starts-with? example "`print(fold_session("))
        (expect (str/ends-with? example "))`")))
      ;; These assertions pin prompt content, not model compliance.
      (doseq
        [required
         ["Analysis-only and diff-preview requests end in findings or a proposed diff, and leave the tree untouched"
          "scratch and debugging stay in sandbox variables, findings in the answer"
          "checkout or enabled draft workflow" "other worktrees/clones need an explicit request"
          "Commit and push require an explicit request"
          "or explicit authorization in applicable project instructions"
          "Honor narrower user requests"
          "Other external actions (releases, messages, deployments, live service restarts) require an explicit request"
          "After changed-file checks pass, finish the authorized workflow"
          "only for new edits, failures, or a concrete unresolved risk"]]
        (expect (str/includes? text required)))
      ;; Regression: a blanket CORE prohibition overrode repository Git opt-in.
      (expect
        (not
          (str/includes?
            text
            "Commit, push, publish, message people, or mutate external systems only when explicitly requested")))
      ;; Regression, user report: blanket resource cleanup stopped a healthy dev server
      ;; that the user had explicitly asked the agent to open and keep available.
      (doseq [required
              ["Finish clean: stop a background shell before final answer only"
               "temporary implementation or test machinery"
               "healthy service the user asked you to run is persistent user infrastructure"
               "leave it running" "across turns and final answers" "Confirm destructive actions."]]
        (expect (str/includes? text required)))
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
          (expect (str/includes? text "Auto-imported by `python_execution`"))
          (expect (str/includes? text "REAL CPython"))
          (expect (str/includes? text "same `~/.vis/python/packages`"))
          (expect (str/includes? text "Imports never install packages"))
          (expect (not (str/includes? text "fetched once")))
          (expect (str/includes? text "Modules Vis publishes ITSELF"))
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
          (expect (str/includes? text "Shell commands are DISABLED"))
          (expect (str/includes? text "nothing here can start a process"))
          (doseq [banned ["subprocess" "os.system" "os.popen"]]
            (expect (str/includes? text banned)))
          (expect (not (str/includes? text "route through the active")))
          (doseq [name env-python/AUTO_IMPORTED_PYTHON_NAMES]
            (expect (str/includes? text (str "`" name "`"))))))))
  (it "bans subprocess even with shell active, without duplicating the shell contract"
      ;; Invocation syntax belongs to the shell symbol docs; this supplemental
      ;; block only says that `subprocess` is not a second door to a process.
      (let [text (#'prompt/sandbox-shims-prompt-block
                  [{:ext/engine {:ext.engine/symbols [{:ext.symbol/symbol 'shell}]}}])]
        (expect (str/includes? text (get env-python/PROCESS_SURFACE "ban")))
        (expect (str/includes? text "never spawn"))
        (expect (str/includes? text "`shell` verb"))
        (expect (not (str/includes? text "DISABLED")))
        (expect (str/includes? text "subprocess"))
        (expect (str/includes? text "os.system"))
        (expect (str/includes? text "os.popen"))
        (expect (not (str/includes? text "shell(")))
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
        (expect (< (count text) 1550)))))

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
            (prompt/assemble-initial-messages {:stable-prompt-messages [{:role "system"
                                                                         :content "sys"}]
                                               :initial-user-content "hello"})

            user
            (last msgs)]

        (expect (= "user" (:role user)))
        (expect (string? (:content user)))
        (expect (str/includes? (:content user) "CURRENT-USER-MESSAGE"))
        (expect (not (str/includes? (:content user) "ATTACHED-IMAGES")))))
  (it "rides svar image blocks ahead of the text block and lists a manifest"
      (let [msgs
            (prompt/assemble-initial-messages
              {:stable-prompt-messages []
               :initial-user-content "what is on /tmp/shot.png?"
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
          (expect (str/includes? text "CURRENT-USER-MESSAGE"))
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
            (prompt/assemble-initial-messages {:stable-prompt-messages []
                                               :initial-user-content "look"
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
            (prompt/assemble-initial-messages {:stable-prompt-messages []
                                               :initial-user-content "what is on /tmp/shot.png?"
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
                   {:stable-prompt-messages [{:role "system" :content "sys"}]
                    :user-images
                    [{:path "p" :media-type "image/png" :base64 "eA==" :size 1 :size-label "1B"}]})]
        ;; no user message without initial-user-content — images can't ride alone
        (expect (= 1 (count msgs)))
        (expect (= "system" (:role (first msgs)))))))

(defdescribe
  attached-images-descriptions-test
  "When the active model cannot see, a sighted model's report stands in for the
   pixels. The manifest must carry that report AND label it second-hand: an agent
   that thinks it saw the image will testify about detail no one described."
  (let [assemble (fn [descriptions]
                   (:content (last (prompt/assemble-initial-messages
                                     {:stable-prompt-messages []
                                      :initial-user-content "what is on /tmp/shot.png?"
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
                                        {:stable-prompt-messages []
                                         :initial-user-content "look"
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
                     {:stable-prompt-messages []
                      :initial-user-content "look"
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
  resume-message-cache-stability-test
  (it "appends each completed turn as its own stable message"
      (let [entry
            (fn [n]
              {:turn n :user-request (str "q" n) :answer (str "a" n) :results []})

            assemble
            (fn [prior current turn]
              (prompt/assemble-initial-messages {:stable-prompt-messages [{:role "system"
                                                                           :content "stable"}]
                                                 :previous-turn-context prior
                                                 :turn-context (str "session[\"turn\"] = " turn)
                                                 :initial-user-content current}))

            t3
            (assemble [(entry 1) (entry 2)] "q3" 3)

            t4
            (assemble [(entry 1) (entry 2) (entry 3)] "q4" 4)]

        (expect (= (vec (butlast t3)) (subvec t4 0 (dec (count t3)))))
        (expect (str/includes? (:content (last t4)) ";; -- TURN-SYSTEM-CONTEXT --"))
        (expect (str/includes? (:content (last t4)) "session[\"turn\"] = 4"))))
  (it "renders one checkpoint message without covered Q/A"
      (let [messages
            (prompt/assemble-initial-messages
              {:previous-turn-context [{:checkpoint? true :turns [1 2] :gist "durable state"}]
               :turn-context "session[\"turn\"] = 3"
               :initial-user-content "continue"})

            prior
            (:content (first messages))]

        (expect (= 2 (count messages)))
        (expect (str/includes? prior "folded turns 1, 2"))
        (expect (str/includes? prior "durable state"))
        (expect (not (str/includes? prior "user asked:")))))
  (it "renders cancelled work as settled history with a model-visible abort marker"
      (let [block (prompt/previous-turn-context-block [{:turn 1
                                                        :user-request "inspect and fix"
                                                        :cancelled? true
                                                        :results [{:scope "t1/i1/f1"
                                                                   :src "cat(src)"}]}])]
        (expect (str/includes? block "cat(src)"))
        (expect (str/includes? block "<turn_cancelled>"))
        (expect (str/includes? block "persisted results remain valid; do not repeat settled work"))
        (expect (not (str/includes? block "INTERRUPTED before it finished")))))
  ;; Reported from the app: cancelling a turn also dropped the prose the model
  ;; had already sent, so the next turn resumed as if it had said nothing.
  (it "keeps the partial answer a cancelled turn had already produced"
      (let [block (prompt/previous-turn-context-block
                    [{:turn 1
                      :user-request "inspect and fix"
                      :cancelled? true
                      :partial-answer "I patched ChatContent.tsx and was checking the TUI"
                      :results [{:scope "t1/i1" :src "patch(\"ChatContent.tsx\", edits)"}]}])]
        (expect (str/includes? block "you answered so far"))
        (expect (str/includes? block "I patched ChatContent.tsx and was checking the TUI"))
        (expect (str/includes? block "<turn_cancelled>"))))
  (it "tells an interrupted turn its answer is partial, not absent"
      (let [block (prompt/previous-turn-context-block [{:turn 1
                                                        :user-request "inspect and fix"
                                                        :interrupted? true
                                                        :partial-answer
                                                        "found the cause in loop.clj"
                                                        :results []}])]
        (expect (str/includes? block "found the cause in loop.clj"))
        (expect (str/includes? block "the answer above is only what you had said by then"))
        (expect (not (str/includes? block "you produced NO answer"))))))

(defdescribe core-prompt-routes-text-edits-to-patch-test
             ;; The verbs exist only if the prompt spends them. Before this, the core
             ;; prompt told the model to CHANGE the tree with `Path.write_text` — which is
             ;; how a 2 KB block that restates the old text becomes the normal way to edit.
             (it "names both anchored verbs and the address they speak"
                 (let [text (prompt/build-system-prompt {})]
                   (expect (str/includes? text "cat(path, start, end)"))
                   (expect (str/includes? text "patch(path, edits)"))
                   (expect (str/includes? text "\"replace\""))
                   (expect (str/includes? text "line:hash"))
                   (expect (str/includes? text "the write lands on exactly those lines"))))
             (it "never tells the model to write a text EDIT in plain Python"
                 (let [text (prompt/build-system-prompt {})]
                   (expect (not (str/includes? text "CHANGING the tree is plain Python")))
                   (expect (not (str/includes? text "are edited in plain Python")))
                   (expect (str/includes? text "Use `patch(path, edits)`"))))
             ;; Regression: `cat` grew a negative endpoint and the prompt kept quiet, so
             ;; the tail of a file still cost a line count first and then a read.
             (it "says a negative `start` counts from the end"
                 (let [text (prompt/build-system-prompt {})]
                   (expect (str/includes? text "a negative"))
                   (expect (str/includes? text "`start` counts from the end"))))
             ;; Regression: after grep started answering ONE anchored TEXT block the
             ;; prompt still said only "hits arrive ANCHORED", never WHAT arrives, so the
             ;; model kept treating the answer as a keyed map.
             (it
               "says grep answers anchored TEXT, models context, and spends several hits at once"
               (let [text (prompt/build-system-prompt {})]
                 (expect (str/includes? text "answers an anchored STRING"))
                 (expect (not (str/includes? text "returns a MAP")))
                 ;; Several hits in ONE file are ONE patch call now, so there is no
                 ;; order left for the caller to compute.
                 (expect (not (str/includes? text "bottom-up")))
                 (expect (str/includes? text "`patch(path, edits)`, ONE call per file"))
                 (expect (str/includes? text "FRESH ANCHOR"))
                 ;; User reports: recent sessions copied the primary grep example without
                 ;; context even though nearby lines often answered the question outright.
                 ;; Naming the default was still ambiguous: context is counted independently
                 ;; above and below every match, not split between the two sides.
                 (expect (str/includes?
                           text
                           "`grep({\"query\": [needles], \"paths\": [scopes], \"context\": 3})`"))
                 (expect (str/includes? text "`context`: lines per side (default 3)"))
                 ;; User report: §3 taught the grep call and stopped — a capped page and a
                 ;; query that matches everywhere had no next step, so the same search ran
                 ;; again instead of paging or asking WHICH files match.
                 (expect (str/includes? text "A capped page continues itself with `next(r)`"))
                 (expect (str/includes?
                           text
                           "`is_files_only: True` answers one row per matching file and its count"))
                 ;; Regression, user report: a capped page taught `offset` and stopped
                 ;; there, so the next call carried that offset onto a DIFFERENT query
                 ;; and read the empty page as proof the symbol does not exist.
                 (expect (str/includes? text "`offset` resumes THAT SAME query, never a new one"))))
             ;; Regression: `sh.logs` grew the same negative tail `cat` has, and the
             ;; prompt named the method with no arguments at all, so a watcher still
             ;; paged bytes to answer "what did it just print".
             (it "says a shell handle reads its last n LINES"
                 (let [text (prompt/build-system-prompt {})]
                   (expect (str/includes? text "`sh.logs(-50)` (last n LINES)")))))

;; Regression, user report: cat and a patch with unseen or placeholder hashes
;; ran in one block; printing the read cannot inform a prewritten replacement.
(defdescribe
  core-prompt-grounds-patch-retries-test
  (it "waits for observed anchors before generating a dependent patch"
      (let [text (prompt/build-system-prompt {})]
        (doseq [rule ["END the block, then decide in the NEXT block"
                      "copied verbatim from a read in an earlier block"
                      "each endpoint's line number and full three-character hash checked"
                      "the write lands on exactly those lines"]]
          (expect (str/includes? text rule) rule))))
  ;; Editing e2e used source text or bare line numbers despite the anchor instruction.
  (it "defines both endpoints as hashline strings rather than text or line numbers"
      (let [text (prompt/build-system-prompt {})]
        (doseq
          [rule
           ["`Path.read_text` suits whole-file processing" "`from`/`to` are `line:hash` anchors"
            "Given `12:abc│ old`, a one-line edit is `{\"from\": \"12:abc\", \"replace\": \"new\"}`"
            "`replace` is new file text without hash gutters"]]
          (expect (str/includes? text rule) rule))))
  (it "distinguishes stale-anchor recovery from invalid replacement syntax"
      (let [text (prompt/build-system-prompt {})]
        (doseq [rule
                ["use a FRESH ANCHOR from the last result or re-read the target"
                 "A refused patch writes nothing"
                 "for stale anchors, read only the indicated region"
                 "confirm the intended target before retrying"
                 "For parse errors, fix the replacement syntax and retry with the same anchors"]]
          (expect (str/includes? text rule) rule)))))

(defdescribe
  core-prompt-ls-contract-test
  ;; Regression: the prompt omitted ls keywords, including the hidden alias.
  (it "states the ls signature, batching, return type, and hidden precedence"
      (let [text (prompt/build-system-prompt {})]
        (doseq
          [rule
           ["ls(paths='.', depth=1, is_hidden=False, *, hidden=None, pattern=None, as_paths=False)"
            "`pattern`: case-sensitive basename glob (not regex), None disables"
            "applies at each depth, keeps ancestors; per-path specs override it"
            "accepts str/Path or a list"
            "returns STRING, or a flat list of paths with `as_paths=True`"
            "Non-None `hidden` overrides `is_hidden`" "gitignored entries stay excluded"]]
          (expect (str/includes? text rule) rule)))))

(defdescribe core-prompt-helper-lifecycle-test
             ;; Council report 4353: unbounded helper catalogs and versioned names hid reusable work.
             (it "searches before creating and refines the existing binding from its source"
                 (let [text (prompt/build-system-prompt {})]
                   (doseq [rule ["Before a new helper, search `defs(pattern=\"...\")`"
                                 "read `defs(name)` and refine a stable name"
                                 "`defs(name, details=True)` lists a" "whether each is present"]]
                     (expect (str/includes? text rule) rule))))
             ;; User report: the goal-form rewrite left only a rule to REUSE helpers, so blocks
             ;; retyped the same steps and `defs()` stayed empty. Keep the rule that creates one.
             (it "factors a repeated block into a named helper on its second occurrence"
                 (let [text (prompt/build-system-prompt {})]
                   (doseq [rule ["Factor a repeated loop or block into a small named helper"
                                 "on its second occurrence, then call it"
                                 "Reuse helpers instead of retyping" "`defs()` lists them"]]
                     (expect (str/includes? text rule) rule))))
             ;; User report: no rule said whether redefining or deleting a helper changes the
             ;; saved definitions, so the model had to read the host to answer that.
             (it "states that the saved set follows redefinition and explicit deletion"
                 (let [text (prompt/build-system-prompt {})]
                   (doseq [rule ["mirror the namespace after each block"
                                 "redefining replaces the saved source"
                                 "`del obsolete_name` drops the helper from `defs()` for good"
                                 "a restart restores only what is still defined"
                                 "callers, aliases and captured defaults confirm it is unused"]]
                     (expect (str/includes? text rule) rule))))
             ;; User report: three of seven helper lines described source fingerprints and
             ;; Improve proposals, a rare workflow paid for in every request. That contract
             ;; stays on the `doc("defs")` page and in the token-optimization guide.
             (it "keeps fingerprints and Improve proposals out of the per-request prompt"
                 (let [text (prompt/build-system-prompt {})]
                   (doseq [rule ["SHA-256" "propose to Improve" "liveness unknown"]]
                     (expect (not (str/includes? text rule)) rule))
                   (expect (str/includes? text "Create Python extensions only when asked")))))

;; Regression: name the prebound paths and lifetime of reusable helpers so blocks
;; do not redefine paths or helpers that the session already provides.
(defdescribe core-prompt-steers-python-shape-test
             (it "uses the advertised prebound paths instead of defining or guessing aliases"
                 (let [text (prompt/build-system-prompt {})]
                   (expect (str/includes? text "every action is sandbox Python"))
                   ;; User report: the runtime always binds project_root_path, never a root alias.
                   (expect (str/includes? text "`project_root_path` (workspace, always available)"))
                   (expect (str/includes? text "the complete alias set (`root` is not prebound)"))
                   (expect (str/includes? text "`session[\"workspace\"][\"filesystem_roots\"]`"))
                   (expect (str/includes? text "`python_name`"))
                   (expect (str/includes? text "`cwd`"))
                   (expect (not (str/includes? text "path_globals")))
                   (expect (str/includes? text "Prebound `Path` objects"))
                   (expect (str/includes? text "use them under exactly these names"))
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
                   (expect (str/includes? text "Reuse helpers"))
                   (expect (str/includes? text "`defs()`"))
                   (expect (str/includes? text "`defs(name)` reads one"))
                   ;; Regression: the prompt promised a `def` only "persists for the whole
                   ;; session" — true of the interpreter, false of the PROCESS, so a restart
                   ;; silently emptied the sandbox the transcript still described.
                   (expect (str/includes? text "survives blocks, turns and gateway restarts"))
                   (expect (str/includes? text "gateway restart"))
                   (expect (str/includes? text
                                          "rebuilt before every block, so writes to it vanish"))
                   (expect (str/includes? text "your own state lives in variables and helpers"))
                   (expect (not (str/includes? text "definitions persist between blocks")))
                   (expect (not (str/includes? text "live read-only map")))))
             ;; Creating extensions requires a request and reading their contract first.
             (it "creates extensions only when asked and reads their contract"
                 (let [text (prompt/build-system-prompt {})]
                   (expect (str/includes? text "Create Python extensions only when asked"))
                   (expect (str/includes? text "first read `doc(\"extending\")`")))))

(defdescribe
  planning-prompt-test
  (it "adds exactly one planning block to interactive channels only when enabled"
      (doseq [channel
              [:web :tui :cli]

              enabled?
              [false true]]

        (with-redefs [toggles/enabled? (fn [id]
                                         (and (= "plans" id) enabled?))]
          (let [messages (prompt/assemble-stable-prompt-messages {:channel channel}
                                                                 {:active-extensions []})
                text (prompt/stable-prompt-text messages)]

            (expect (= (if (and enabled? (not= :cli channel)) 1 0)
                       (count (re-seq #";; -- PLANS --" text))))))))
  (it "keeps review versioned and makes approval authorize implementation"
      (with-redefs [toggles/enabled? (constantly true)]
        (let [build #(prompt/assemble-stable-prompt-messages {:channel :tui}
                                                             {:active-extensions []})]
          (expect (= (build) (build)))
          (doseq [instruction ["Blocked by" "end-to-end" "authorizes implementation immediately"
                               "A document status is not permission"
                               "explicitly says not to implement"
                               "comments never authorize project edits" "newer revision exists"
                               "complete review round" "## Implementation plan" "read_attachment"
                               "SAME filename" "## Resolved comments" "IMPLEMENTATION-<feature>.md"
                               "Do not publish tracker tickets" "Markdown is the source of truth"
                               "normal chat" "workflow controls are optional shortcuts"]]
            (expect (str/includes? prompt/planning-rules instruction))))))
  (it "publishes reviewable specifications and diffs, but read-only implementation records"
      (doseq [instruction ["commentable=True" "commentable=False" "read-only execution record"
                           "kind=\"diff\"" "application/vnd.vis.diff+json" "draft_diff"
                           "after each completed task" "final cumulative diff"
                           "unrelated or pre-existing changes" "patch bytes unchanged"]]
        (expect (str/includes? prompt/planning-rules instruction)))))

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
