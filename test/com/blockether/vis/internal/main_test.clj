(ns com.blockether.vis.internal.main-test
  (:require [com.blockether.vis.internal.commandline :as commandline]
            [com.blockether.vis.internal.main :as main]
            [com.blockether.vis.internal.render :as render]
            [com.blockether.vis.internal.registry :as registry]
            [com.blockether.vis.internal.toggles :as toggles]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  answer->ir-safe-test
  "The --raw/CLI render must NEVER die on an unexpected answer shape (a
   force-finalized answer, a provider-error `[:ir …]` fallback, a bare string).
   `answer->ir-safe` coerces any shape into something `render/render` accepts."
  (let [safe (deref #'main/answer->ir-safe)]
    (it "renders every answer shape without throwing"
        (doseq [[a expected] [[{:answer "hi"} "hi"] [nil ""] ["bare string" "bare string"]
                              [[:ir {} [:p {} "x"]] "x"] [{:weird 1} "{:weird 1}"]
                              [{:vis/answer-mode :needs-input :answer/text "q"} "q"]]]
          (expect (= expected (render/render (safe a) :plain)))))))

(defdescribe
  root-help-test
  (it
    "describes Vis and root one-shot flags"
    (let [help (commandline/render-tree (#'main/root-command))]
      (expect
        (.contains
          help
          "Vis - persistent sandboxed Recursive Language Model powered by an embedded Python REPL."))
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
                   (expect (.contains (str out) "vis providers"))))
             (it "loads channels before rendering channels parent help"
                 (let [out (java.io.StringWriter.)
                       discovered? (atom false)
                       fake-channel {:channel/id ::fast-help-test
                                     :channel/cmd "zzz-test"
                                     :channel/doc "Test channel for help."
                                     :channel/main-fn (fn [_args])}]
                   (try (with-redefs [main/discover-all! (fn []
                                                           (reset! discovered? true)
                                                           (registry/register-channel! fake-channel))]
                          (binding [*out* out]
                            (expect (true? (#'main/fast-help-dispatched? false ["channels" "--help"]))))
                          (expect (true? @discovered?))
                          (expect (.contains (str out) "zzz-test"))
                          (expect (.contains (str out) "Test channel for help.")))
                        (finally (registry/deregister-channel! (:channel/id fake-channel))))))
             (it "strips launcher selectors when they leak into JVM args"
                 (expect (= ["channels" "--help"]
                            (#'main/strip-global-args ["channels" "--jvm" "--help"])))))

(defdescribe parse-run-args-test
             (it "parses --toggles as a run-scoped override list"
                 (expect (= {:toggles "vis/mouse-selection-copy=true,vis/reasoning-level=deep"
                             :prompt "run tests"}
                            (#'main/parse-run-args
                             ["--toggles" "vis/mouse-selection-copy=true,vis/reasoning-level=deep"
                              "run" "tests"]))))
             (it "parses --session-id as persistent continuation"
                 (expect (= {:session-id "abc123"
                             :persist? true
                             :provider "anthropic-coding-plan"
                             :model "claude-sonnet-4-6"
                             :prompt "what do I like?"}
                            (#'main/parse-run-args
                             ["--provider" "anthropic-coding-plan" "--model" "claude-sonnet-4-6"
                              "--session-id" "abc123" "what" "do" "I" "like?"])))))

(defdescribe toggle-overrides-test
             (it "parses NAME=VALUE pairs against the registry"
                 (expect (= {:vis/mouse-selection-copy true :vis/reasoning-level :deep}
                            (#'main/parse-toggle-overrides
                             "vis/mouse-selection-copy=true,vis/reasoning-level=deep"))))
             (it "rejects unknown toggles as user error"
                 (try (#'main/parse-toggle-overrides "nope/missing=true")
                      (expect false)
                      (catch clojure.lang.ExceptionInfo e
                        (expect (= :vis.cli/unknown-toggle (:type (ex-data e))))
                        (expect (true? (:vis/user-error (ex-data e)))))))
             (it "rejects enum values outside the registered choices"
                 (try (#'main/parse-toggle-overrides "vis/reasoning-level=bogus")
                      (expect false)
                      (catch clojure.lang.ExceptionInfo e
                        (expect (= :vis.cli/invalid-toggle (:type (ex-data e)))))))
             (it "rejects non-boolean values on boolean toggles"
                 (try (#'main/parse-toggle-overrides "vis/mouse-selection-copy=maybe")
                      (expect false)
                      (catch clojure.lang.ExceptionInfo e
                        (expect (= :vis.cli/invalid-toggle (:type (ex-data e)))))))
             (it "applies overrides only while the one-shot body runs"
                 (toggles/set-enabled! :vis/mouse-selection-copy false)
                 (try (expect (= [true :deep]
                                 (#'main/call-with-toggle-overrides
                                  {:vis/mouse-selection-copy true :vis/reasoning-level :deep}
                                  #(vector (toggles/enabled? :vis/mouse-selection-copy)
                                           (toggles/value-of :vis/reasoning-level)))))
                      (expect (false? (toggles/enabled? :vis/mouse-selection-copy)))
                      (expect (= :balanced (toggles/value-of :vis/reasoning-level)))
                      (finally (toggles/reset-to-default! :vis/mouse-selection-copy)
                               (toggles/reset-to-default! :vis/reasoning-level)))))

(defdescribe
  root-run-shortcut-test
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
                 (let [{:keys [command]}
                       (commandline/find-leaf (#'main/root-command) ["vis" "sessions"])

                       help
                       (commandline/render-command command ["vis" "sessions"])]

                   (expect (.contains help "vis sessions <list|show|fork|delete|search|export>"))
                   (expect (.contains help "list"))
                   (expect (.contains help "show"))
                   (expect (.contains help "fork"))
                   (expect (.contains help "delete"))
                   (expect (.contains help "export")))))

(defdescribe provider-override-error-test
             (it "marks unknown --provider as user error"
                 (try (#'main/config-with-provider-override {:providers []} :definitely-nope)
                      (expect false)
                      (catch clojure.lang.ExceptionInfo e
                        (expect (= :vis.cli/unknown-provider (:type (ex-data e))))
                        (expect (true? (:vis/user-error (ex-data e)))))))
             (it "marks unknown provider/model as user error"
                 (try (#'main/config-with-model-override {:providers []} "definitely-nope/model")
                      (expect false)
                      (catch clojure.lang.ExceptionInfo e
                        (expect (= :vis.cli/unknown-model-provider (:type (ex-data e))))
                        (expect (true? (:vis/user-error (ex-data e))))))))

(defdescribe
  toggle-bare-names-test
  (it "resolves bare names when unambiguous across the registry"
      (expect (= {:vis/mouse-selection-copy true :vis/reasoning-level :deep}
                 (#'main/parse-toggle-overrides "mouse-selection-copy=true,reasoning-level=deep"))))
  (it "still accepts the fully namespaced form"
      (expect (= {:vis/mouse-selection-copy false}
                 (#'main/parse-toggle-overrides "vis/mouse-selection-copy=false"))))
  (it "rejects unknown bare names as user error"
      (try (#'main/parse-toggle-overrides "definitely-not-a-toggle=true")
           (expect false)
           (catch clojure.lang.ExceptionInfo e
             (expect (= :vis.cli/unknown-toggle (:type (ex-data e))))
             (expect (true? (:vis/user-error (ex-data e)))))))
  (it "rejects ambiguous bare names listing every candidate"
      (toggles/register-toggle!
        {:id :main-test/mouse-selection-copy :label "Collision probe" :default false})
      (try (#'main/parse-toggle-overrides "mouse-selection-copy=true")
           (expect false)
           (catch clojure.lang.ExceptionInfo e
             (expect (= :vis.cli/ambiguous-toggle (:type (ex-data e))))
             (expect (true? (:vis/user-error (ex-data e))))
             (expect (= #{:vis/mouse-selection-copy :main-test/mouse-selection-copy}
                        (set (:candidates (ex-data e))))))
           (finally (swap! @#'toggles/registry dissoc :main-test/mouse-selection-copy)))))
