(ns com.blockether.vis.internal.main-test
  (:require
   [com.blockether.vis.internal.commandline :as commandline]
   [com.blockether.vis.internal.main :as main]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe root-help-test
  (it "describes Vis and root one-shot flags"
    (let [help (commandline/render-tree (#'main/root-command))]
      (expect (.contains help "Vis - persistent sandboxed Recursive Language Model powered by Clojure-SCI REPL."))
      (expect (.contains help "vis [FLAGS] \"prompt\""))
      (expect (.contains help "--full-trace-json-stream"))
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

(defdescribe root-run-shortcut-test
  (it "treats bare prompt and run flags as root run shortcut"
    (let [root (#'main/root-command)]
      (expect (true? (#'main/root-run-shortcut? root ["fix tests"])))
      (expect (true? (#'main/root-run-shortcut? root ["--json" "summarize"])))))

  (it "keeps known commands and unknown help out of root run shortcut"
    (let [root (#'main/root-command)]
      (expect (false? (#'main/root-run-shortcut? root ["providers" "list"])))
      (expect (false? (#'main/root-run-shortcut? root ["sessions" "--help"])))
      (expect (false? (#'main/root-run-shortcut? root ["--help"]))))))

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
