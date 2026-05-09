(ns com.blockether.vis.internal.prompt-test
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.prompt :as prompt]
   [com.blockether.vis.internal.skills :as skills]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe core-system-prompt-test
  (it "states the GROUND RULE: generate -> evaluate -> populate -> observe -> decide"
    (let [p prompt/CORE_SYSTEM_PROMPT]
      (expect (str/includes? p "GROUND RULE"))
      (expect (str/includes? p "GENERATE one OR MORE fenced ```clojure blocks"))
      (expect (str/includes? p "AUTOMATICALLY populates results into <journal>"))
      (expect (str/includes? p "OBSERVE"))
      (expect (str/includes? p "DECIDE"))
      (expect (str/includes? p "(answer"))))

  (it "SILENT FORMS sub-rule explains def acquisition vs observation"
    (let [p prompt/CORE_SYSTEM_PROMPT]
      (expect (str/includes? p "SILENT FORMS"))
      (expect (str/includes? p "<bindings>"))
      (expect (str/includes? p "acquisition, not observation"))
      (expect (str/includes? p "contiguous"))))

  (it "BATCHING sub-rule documents (do ...) for one-entry observation"
    (let [p prompt/CORE_SYSTEM_PROMPT]
      (expect (str/includes? p "BATCHING"))
      (expect (str/includes? p "(do"))
      (expect (str/includes? p "ONE journal entry"))))

  (it "BINDINGS sub-rule documents *1/*2/*3/*e as escape hatches"
    (let [p prompt/CORE_SYSTEM_PROMPT]
      (expect (str/includes? p "BINDINGS"))
      (expect (str/includes? p "`*1`"))
      (expect (str/includes? p "`*e`"))
      (expect (str/includes? p "prefer durable names")))))

(defdescribe journal-rendering-test
  (it "omits silent def/acquisition blocks from <journal>"
    (let [out (prompt/build-iteration-context
                {:conversation-title-atom (atom "Preview diagnosis")}
                {:active-extensions []
                 :model "test-model"
                 :context-limit 4096
                 :iteration 1
                 :blocks-by-iteration [[1 {:blocks [{:code "(def xd \"XDDD\")"
                                                     :result "XDDD"
                                                     :rendering-kind :vis/silent}
                                                    {:code "(+ 1 1)"
                                                     :result 2}]}]]})]
      (expect (str/includes? out "<journal>"))
      (expect (not (str/includes? out "XDDD")))
      (expect (not (str/includes? out "(def xd")))
      (expect (str/includes? out "(+ 1 1) -> 2")))))

(defdescribe full-system-prompt-assembly-test
  (it "includes <skills> block when skills are available"
    (with-redefs [skills/list-all (fn [] [{:name "diagnose"
                                           :source :repo
                                           :description "Debug loop."}])]
      (let [out (prompt/assemble-system-prompt {}
                  {:active-extensions []
                   :system-prompt nil})]
        (expect (str/includes? out "<system_prompt>"))
        (expect (str/includes? out "</system_prompt>"))
        (expect (str/includes? out "<skills>"))
        (expect (str/includes? out "diagnose: Debug loop."))
        (expect (str/includes? out "</skills>")))))

  (it "omits <skills> when catalog is empty"
    (with-redefs [skills/list-all (fn [] [])]
      (let [out (prompt/assemble-system-prompt {}
                  {:active-extensions []
                   :system-prompt nil})]
        (expect (str/includes? out "<system_prompt>"))
        (expect (not (str/includes? out "<skills>"))))))

  (it "no extensions = no <extensions>/<environment-info> blocks"
    (with-redefs [skills/list-all (fn [] [])]
      (let [out (prompt/assemble-system-prompt {}
                  {:active-extensions []
                   :system-prompt nil})]
        (expect (not (str/includes? out "<extensions>")))
        (expect (not (str/includes? out "<environment-info>")))
        (expect (not (str/includes? out "<llm_model_prompt")))
        (expect (not (str/includes? out "<specific_provider_model_prompt"))))))

  (it "includes <extensions> when an active extension provides :ext/prompt"
    (with-redefs [skills/list-all (fn [] [])]
      (let [fake-ext {:ext/namespace 'com.test.ext
                      :ext/prompt    (constantly "Use test/ prefix for all test fns.")}
            out (prompt/assemble-system-prompt {}
                  {:active-extensions [fake-ext]
                   :system-prompt nil})]
        (expect (str/includes? out "<extensions>"))
        (expect (str/includes? out "Use test/ prefix for all test fns."))
        (expect (str/includes? out "</extensions>")))))

  (it "includes <environment-info> when an active extension provides :ext/environment-info-fn"
    (with-redefs [skills/list-all (fn [] [])]
      (let [fake-ext {:ext/namespace 'com.test.env
                      :ext/environment-info-fn (constantly "OS: macOS 15.1\nCWD: /home/user")}
            out (prompt/assemble-system-prompt {}
                  {:active-extensions [fake-ext]
                   :system-prompt nil})]
        (expect (str/includes? out "<environment-info>"))
        (expect (str/includes? out "OS: macOS 15.1"))
        (expect (str/includes? out "</environment-info>"))))))
