(ns com.blockether.vis.internal.context.prompt-test
  (:require [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.svar.internal.llm :as svar-llm]
            [com.blockether.svar.core :as svar]
            [com.blockether.svar.internal.router :as svar-router]
            [com.blockether.vis.internal.attachment.core :as attachments]
            [com.blockether.vis.internal.context.agents :as agents]
            [com.blockether.vis.internal.python.env :as env-python]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.context.prompt :as prompt]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [lazytest.core :refer [defdescribe expect it]]))

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

                   (expect (not (str/includes? text "\n\n\n"))))))

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
        (doseq [rule ["Next to each `python_execution` call" "a finding that changes the plan"]]
          (expect (not (str/includes? cli rule)) rule)))))

(defdescribe prompt-core-test
             (it "keeps the core prompt under its fixed size budget"
                 (let [text (var-get (ns-resolve 'com.blockether.vis.internal.context.prompt
                                                 'CORE_SYSTEM_PROMPT))]
                   (expect (< (count text) 4700))))
             (it "bans subprocess and states the shell handle only when shell is active"
                 ;; User review: shell rules cost every request, so they ride only with an active shell.
                 (let [text
                       (#'prompt/sandbox-shims-prompt-block
                        [{:ext/engine {:ext.engine/symbols [{:ext.symbol/symbol 'shell}]}}])

                       off
                       (#'prompt/sandbox-shims-prompt-block [])]

                   (expect (str/includes? text (get env-python/PROCESS_SURFACE "ban")))
                   (expect (str/includes? text "`shell` verb"))
                   (expect (str/includes? text "subprocess"))
                   (expect (str/includes? text "os.system"))
                   (expect (str/includes? text "os.popen"))
                   (doseq [rule ["Use `shell` only to run programs" "returns a handle"
                                 "`logs(-n)` (last n lines)"
                                 "each returns `out`, `exit` and `status`"
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
        (expect (str/includes? text "- image 2: [IMAGE #2] — same.png"))))
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
        (expect (str/includes? (:content user) "/tmp/dot.png"))))
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
        (expect (str/includes? (:content user) "/tmp/shot.png"))))
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
          ;; Still no image blocks on a blind wire.
          (expect (string? content))))
    (it "keeps the PIL directive when nothing described the image"
        ;; Toggle off, no sighted model in the fleet, or a refused ask: unchanged.
        (let [content (assemble nil)]
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
        (expect (str/includes? (:content notice) "<turn_cancelled>"))))
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
        (expect (= 1 (count absent))))))

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
