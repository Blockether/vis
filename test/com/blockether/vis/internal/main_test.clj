(ns com.blockether.vis.internal.main-test
  (:require
   [com.blockether.vis.internal.commandline :as commandline]
   [com.blockether.vis.internal.main :as main]
   [com.blockether.vis.internal.toggles :as toggles]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe root-help-test
  (it "describes Vis and root one-shot flags"
    (let [help (commandline/render-tree (#'main/root-command))]
      (expect (.contains help "Vis - persistent sandboxed Recursive Language Model powered by an embedded Python REPL."))
      (expect (.contains help "vis [FLAGS] \"prompt\""))
      (expect (.contains help "--full-trace-json-stream"))
      (expect (.contains help "--shell-tool"))
      (expect (.contains help "--provider PROVIDER"))
      (expect (.contains help "COMMANDS")))))

(defdescribe fast-help-test
  (it "does not swallow unknown root commands that also ask for help"
    (expect (nil? (#'main/fast-help-dispatched? false ["missing" "--help"]))))

  (it "still handles known built-in help without full extension discovery"
    (let [out (java.io.StringWriter.)]
      (binding [*out* out]
        (expect (true? (#'main/fast-help-dispatched? false ["providers" "--help"]))))
      (expect (.contains (str out) "vis providers")))))

(defdescribe parse-run-args-test
  (it "parses --shell-tool as a run-scoped capability"
    (expect (= {:shell-tool? true
                :prompt "run tests"}
              (#'main/parse-run-args ["--shell-tool" "run" "tests"]))))

  (it "parses --toggles as a run-scoped override list"
    (expect (= {:toggles "vis/show-timestamps=true,vis/reasoning-level=deep"
                :prompt "run tests"}
              (#'main/parse-run-args
               ["--toggles" "vis/show-timestamps=true,vis/reasoning-level=deep"
                "run" "tests"]))))

  (it "parses --session-id as persistent continuation"
    (expect (= {:session-id "abc123"
                :persist? true
                :provider "anthropic-coding-plan"
                :model "claude-sonnet-4-6"
                :prompt "what do I like?"}
              (#'main/parse-run-args
               ["--provider" "anthropic-coding-plan"
                "--model" "claude-sonnet-4-6"
                "--session-id" "abc123"
                "what" "do" "I" "like?"])))))

(defdescribe toggle-overrides-test
  (it "parses NAME=VALUE pairs against the registry"
    (expect (= {:vis/show-timestamps true :vis/reasoning-level :deep}
              (#'main/parse-toggle-overrides
               "vis/show-timestamps=true,vis/reasoning-level=deep"))))

  (it "rejects unknown toggles as user error"
    (try
      (#'main/parse-toggle-overrides "nope/missing=true")
      (expect false)
      (catch clojure.lang.ExceptionInfo e
        (expect (= :vis.cli/unknown-toggle (:type (ex-data e))))
        (expect (true? (:vis/user-error (ex-data e)))))))

  (it "rejects enum values outside the registered choices"
    (try
      (#'main/parse-toggle-overrides "vis/reasoning-level=bogus")
      (expect false)
      (catch clojure.lang.ExceptionInfo e
        (expect (= :vis.cli/invalid-toggle (:type (ex-data e)))))))

  (it "rejects non-boolean values on boolean toggles"
    (try
      (#'main/parse-toggle-overrides "vis/show-timestamps=maybe")
      (expect false)
      (catch clojure.lang.ExceptionInfo e
        (expect (= :vis.cli/invalid-toggle (:type (ex-data e)))))))

  (it "applies overrides only while the one-shot body runs"
    (toggles/set-enabled! :vis/show-timestamps false)
    (try
      (expect (= [true :deep]
                (#'main/call-with-toggle-overrides
                 {:vis/show-timestamps true :vis/reasoning-level :deep}
                 #(vector (toggles/enabled? :vis/show-timestamps)
                    (toggles/value-of :vis/reasoning-level)))))
      (expect (false? (toggles/enabled? :vis/show-timestamps)))
      (expect (= :balanced (toggles/value-of :vis/reasoning-level)))
      (finally
        (toggles/reset-to-default! :vis/show-timestamps)
        (toggles/reset-to-default! :vis/reasoning-level)))))

(defdescribe root-run-shortcut-test
  (it "treats bare prompt and run flags as root run shortcut"
    (let [root (#'main/root-command)]
      (expect (true? (#'main/root-run-shortcut? root ["fix tests"])))
      (expect (true? (#'main/root-run-shortcut? root ["--json" "summarize"])))))

  (it "keeps known commands and unknown help out of root run shortcut"
    (let [root (#'main/root-command)]
      (expect (false? (#'main/root-run-shortcut? root ["providers" "list"])))
      (expect (false? (#'main/root-run-shortcut? root ["sessions" "export" "42d580bb" "--md"])))
      (expect (false? (#'main/root-run-shortcut? root ["sessions" "--help"])))
      (expect (false? (#'main/root-run-shortcut? root ["--help"]))))))

(defdescribe sessions-command-test
  (it "registers canonical session verbs under host-owned sessions command"
    (let [{:keys [command]} (commandline/find-leaf (#'main/root-command) ["vis" "sessions"])
          help             (commandline/render-command command ["vis" "sessions"])]
      (expect (.contains help "vis sessions <list|show|fork|delete|search|export>"))
      (expect (.contains help "list"))
      (expect (.contains help "show"))
      (expect (.contains help "fork"))
      (expect (.contains help "delete"))
      (expect (.contains help "export")))))

(defdescribe provider-override-error-test
  (it "marks unknown --provider as user error"
    (try
      (#'main/config-with-provider-override {:providers []} :definitely-nope)
      (expect false)
      (catch clojure.lang.ExceptionInfo e
        (expect (= :vis.cli/unknown-provider (:type (ex-data e))))
        (expect (true? (:vis/user-error (ex-data e)))))))

  (it "marks unknown provider/model as user error"
    (try
      (#'main/config-with-model-override {:providers []} "definitely-nope/model")
      (expect false)
      (catch clojure.lang.ExceptionInfo e
        (expect (= :vis.cli/unknown-model-provider (:type (ex-data e))))
        (expect (true? (:vis/user-error (ex-data e))))))))

(defdescribe model-override-routing-test
  (it "routes a bare model to the configured provider that declares it"
    (let [config {:providers [{:id :openai-codex
                               :models [{:name "gpt-5.4"}]}
                              {:id :zai-coding-plan
                               :models [{:name "glm-5-turbo"}]}]}
          result (#'main/config-with-model-override config "glm-5-turbo")]
      (expect (= :zai-coding-plan (get-in result [:providers 0 :id])))
      (expect (= "glm-5-turbo" (get-in result [:providers 0 :models 0 :name])))
      (expect (= :openai-codex (get-in result [:providers 1 :id])))))

  (it "still synthesizes an unknown bare model on the active provider"
    (let [config {:providers [{:id :openai-codex
                               :models [{:name "gpt-5.4"}]}
                              {:id :zai-coding-plan
                               :models [{:name "glm-5-turbo"}]}]}
          result (#'main/config-with-model-override config "experimental-model")]
      (expect (= :openai-codex (get-in result [:providers 0 :id])))
      (expect (= "experimental-model" (get-in result [:providers 0 :models 0 :name])))
      (expect (= :zai-coding-plan (get-in result [:providers 1 :id]))))))

(defdescribe toggle-bare-names-test
  (it "resolves bare names when unambiguous across the registry"
    (expect (= {:vis/show-timestamps true :vis/reasoning-level :deep}
              (#'main/parse-toggle-overrides
               "show-timestamps=true,reasoning-level=deep"))))

  (it "still accepts the fully namespaced form"
    (expect (= {:vis/show-timestamps false}
              (#'main/parse-toggle-overrides "vis/show-timestamps=false"))))

  (it "rejects unknown bare names as user error"
    (try
      (#'main/parse-toggle-overrides "definitely-not-a-toggle=true")
      (expect false)
      (catch clojure.lang.ExceptionInfo e
        (expect (= :vis.cli/unknown-toggle (:type (ex-data e))))
        (expect (true? (:vis/user-error (ex-data e)))))))

  (it "rejects ambiguous bare names listing every candidate"
    (toggles/register-toggle! {:id      :main-test/show-timestamps
                               :label   "Collision probe"
                               :default false})
    (try
      (#'main/parse-toggle-overrides "show-timestamps=true")
      (expect false)
      (catch clojure.lang.ExceptionInfo e
        (expect (= :vis.cli/ambiguous-toggle (:type (ex-data e))))
        (expect (true? (:vis/user-error (ex-data e))))
        (expect (= #{:vis/show-timestamps :main-test/show-timestamps}
                  (set (:candidates (ex-data e))))))
      (finally
        (swap! @#'toggles/registry dissoc :main-test/show-timestamps)))))
