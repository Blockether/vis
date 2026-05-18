(ns com.blockether.vis.internal.main-test
  (:require
   [com.blockether.vis.internal.main :as main]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe fast-help-test
  (it "does not swallow unknown root commands that also ask for help"
    (expect (nil? (#'main/fast-help-dispatched? false ["sessions" "--help"]))))

  (it "still handles known built-in help without full extension discovery"
    (let [out (java.io.StringWriter.)]
      (binding [*out* out]
        (expect (true? (#'main/fast-help-dispatched? false ["run" "--help"]))))
      (expect (.contains (str out) "vis run")))))

(defdescribe root-run-shortcut-test
  (it "treats bare prompt and run flags as root run shortcut"
    (let [root (#'main/root-command)]
      (expect (true? (#'main/root-run-shortcut? root ["fix tests"])))
      (expect (true? (#'main/root-run-shortcut? root ["--json" "summarize"])))))

  (it "keeps known commands and unknown help out of root run shortcut"
    (let [root (#'main/root-command)]
      (expect (false? (#'main/root-run-shortcut? root ["run" "fix tests"])))
      (expect (false? (#'main/root-run-shortcut? root ["sessions" "--help"])))
      (expect (false? (#'main/root-run-shortcut? root ["--help"]))))))
