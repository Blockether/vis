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

(defdescribe shell-tool-scope-test
  (it "enables shell support only while the one-shot body runs"
    (toggles/set-enabled! :vis/shell-tool false)
    (try
      (expect (true?
                (#'main/call-with-shell-tool true
                                             #(toggles/enabled? :vis/shell-tool))))
      (expect (false? (toggles/enabled? :vis/shell-tool)))
      (finally
        (toggles/reset-to-default! :vis/shell-tool)))))

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
