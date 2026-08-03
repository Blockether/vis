(ns com.blockether.vis.internal.main-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.commandline :as commandline]
            [com.blockether.vis.internal.main :as main]
            [com.blockether.vis.internal.registry :as registry]
            [com.blockether.vis.internal.toggles :as toggles]
            [lazytest.core :refer [defdescribe expect it]]))

(toggles/register-toggle!
  {:id "main_test_flag" :label "CLI toggle test flag" :default false :settings? false})

(defn- gutter-columns
  "Column where the description starts for every two-column `  TOKEN   Doc.` row
   in `s`. A single entry means that block shares one description gutter."
  [^String s]
  (->> (str/split-lines s)
       (keep (fn [^String line]
               (when-let [[_ _ _ doc] (re-matches #"^ {2}(\S.*?)( {2,})(\S.*)$" line)]
                 (- (count line) (count doc)))))
       set))

(defdescribe
  root-help-test
  (it
    "describes Vis and root one-shot flags"
    (let [^String help (commandline/render-tree (#'main/root-command))]
      (expect
        (.contains
          help
          "Vis - a coding agent that edits, runs and verifies code in your repo, with a persistent sandboxed Python REPL."))
      (expect (.contains help "vis-agent [FLAGS] \"prompt\""))
      (expect (.contains help "--full-trace-json-stream"))
      (expect (.contains help "--provider PROVIDER"))
      (expect (.contains help "--reasoning-effort"))
      (expect (.contains help "COMMANDS"))))
  (it "aligns every block: headings at column 0, rows at column 2, one gutter each"
      (let
        [^String help
         (commandline/render-tree (#'main/root-command))

         commands-at
         (.indexOf help "\nCOMMANDS\n")]

        (expect (.contains help "\nUSAGE\n"))
        (expect (pos? commands-at))
        ;; The root doc used to be re-indented on top of its own layout, so its
        ;; headings and rows both sat two columns right of the generated
        ;; COMMANDS block. Nothing is indented twice any more.
        (expect (nil? (re-find #"(?m)^ {3,}\S" help)))
        (expect (= 1 (count (gutter-columns (subs help 0 commands-at)))))
        (expect (= 1 (count (gutter-columns (subs help commands-at)))))))
  (it "documents how to change the runtime distribution"
      (let [^String help (commandline/render-tree (#'main/root-command))]
        (expect (.contains help "RUNTIME (WHICH DISTRIBUTION RUNS)"))
        (doseq
          [row ["--native" "--jvm" "--dev" "VIS_RUNTIME=native|jvm|dev" "vis-agent runtime show"
                "vis-agent runtime use NAME" "vis-agent update [RUNTIME]"]]
          (expect (.contains help row)))))
  (it "points at the configuration a run reads"
      (let [^String help (commandline/render-tree (#'main/root-command))]
        (expect (.contains help "CONFIGURATION"))
        (expect (.contains help "~/.vis/config.yml"))
        (expect (.contains help "<project>/vis.yml")))))

(defdescribe
  fast-help-test
  (it "does not swallow unknown root commands that also ask for help"
      (expect (nil? (#'main/fast-help-dispatched? false ["missing" "--help"]))))
  (it "still handles known built-in help without full extension discovery"
      (let [out (java.io.StringWriter.)]
        (binding [*out* out]
          (expect (true? (#'main/fast-help-dispatched? false ["providers" "--help"]))))
        (expect (.contains (str out) "vis-agent providers"))))
  (it "loads channels before rendering channels parent help"
      (let
        [out
         (java.io.StringWriter.)

         discovered?
         (atom false)

         fake-channel
         {:channel/id ::fast-help-test
          :channel/cmd "zzz-test"
          :channel/doc "Test channel for help."
          :channel/main-fn (fn [_args])}]

        (try (with-redefs
               [main/discover-all! (fn []
                                     (reset! discovered? true)
                                     (registry/register-channel! fake-channel))]
               (binding [*out* out]
                 (expect (true? (#'main/fast-help-dispatched? false ["channels" "--help"]))))
               (expect (true? @discovered?))
               (expect (.contains (str out) "zzz-test"))
               (expect (.contains (str out) "Test channel for help.")))
             (finally (registry/deregister-channel! (:channel/id fake-channel))))))
  (it "strips launcher selectors when they leak into JVM args"
      (expect (= ["channels" "--help"] (#'main/strip-global-args ["channels" "--jvm" "--help"])))))

(defdescribe
  parse-run-args-test
  (it "parses --toggles as a run-scoped override list"
      (expect (= {:toggles "main_test_flag=true,reasoning_level=deep" :prompt "run tests"}
                 (#'main/parse-run-args
                  ["--toggles" "main_test_flag=true,reasoning_level=deep" "run" "tests"]))))
  (it "parses --session-id as persistent continuation"
      (expect (= {:session-id "abc123"
                  :persist? true
                  :provider "anthropic-coding-plan"
                  :model "claude-sonnet-4-6"
                  :prompt "what do I like?"}
                 (#'main/parse-run-args
                  ["--provider" "anthropic-coding-plan" "--model" "claude-sonnet-4-6" "--session-id"
                   "abc123" "what" "do" "I" "like?"]))))
  (it "refuses a flag typo instead of smuggling it into the prompt"
      (expect (= ["unknown flag --modle"]
                 (:flag-errors (#'main/parse-run-args ["--modle" "gpt" "fix" "tests"])))))
  (it "refuses a value flag left without a value"
      (expect (= ["--model needs a value"] (:flag-errors (#'main/parse-run-args ["--model"])))))
  (it "consumes --verbose / -v as debug rather than prompt text"
      (expect (= {:debug? true :prompt "fix"} (#'main/parse-run-args ["--verbose" "fix"])))
      (expect (= {:debug? true :prompt "fix"} (#'main/parse-run-args ["-v" "fix"]))))
  (it "treats everything after -- as prompt text"
      (expect (= {:prompt "--modle is a typo"}
                 (#'main/parse-run-args ["--" "--modle" "is" "a" "typo"]))))
  (it "keeps quoted prose that merely starts with dashes"
      (expect (= {:prompt "--json output is broken"}
                 (#'main/parse-run-args ["--json output is broken"]))))
  (it "refuses a value flag whose value is blank or another flag"
      (expect (= ["--model needs a value"]
                 (:flag-errors (#'main/parse-run-args ["--model" "" "hi"]))))
      (expect (= ["--model needs a value (got --json)"]
                 (:flag-errors (#'main/parse-run-args ["--model" "--json" "task"]))))
      (expect (= ["--toggles needs a value"]
                 (:flag-errors (#'main/parse-run-args ["--toggles" "" "hi"]))))
      (expect (= ["--model needs a value"]
                 (:flag-errors (#'main/parse-run-args ["--model" "--" "hi"]))))))

(defdescribe run-output-mode-conflict-test
             (it "refuses two output modes instead of silently picking one"
                 (expect (= ["name one output mode, not --code and --json"]
                            (:flag-errors (#'main/check-run-conflicts
                                           (#'main/parse-run-args ["--json" "--code" "hi"])))))
                 (expect (= ["name one output mode, not --full-trace-stream and --json"]
                            (:flag-errors (#'main/check-run-conflicts
                                           (#'main/parse-run-args ["--trace" "--json" "hi"]))))))
             (it "leaves a single output mode alone"
                 (expect (nil? (:flag-errors (#'main/check-run-conflicts
                                              (#'main/parse-run-args ["--json" "hi"])))))
                 (expect (nil? (:flag-errors (#'main/check-run-conflicts
                                              (#'main/parse-run-args ["--json" "--raw" "hi"])))))))

(defdescribe
  run-db-target-test
  (it
    "names the unusable --db path instead of failing inside the pool"
    (expect
      (=
        ["--db /nonexistent-dir-xyz/vis.mdb needs an existing directory; /nonexistent-dir-xyz does not exist"]
        (:flag-errors (#'main/check-db-target {:db "/nonexistent-dir-xyz/vis.mdb"}))))
    (expect (= ["--db /tmp is a directory, not a database file"]
               (:flag-errors (#'main/check-db-target {:db "/tmp"})))))
  (it "accepts :memory and a writable path"
      (expect (nil? (:flag-errors (#'main/check-db-target {:db ":memory"}))))
      (expect (nil? (:flag-errors (#'main/check-db-target
                                   {:db (str (System/getProperty "java.io.tmpdir")
                                             "/vis-check-db-target.mdb")}))))
      (expect (nil? (:flag-errors (#'main/check-db-target {}))))))

(defdescribe reasoning-effort-cli-parse-test
             (it "parses exact provider-native reasoning effort separately"
                 (expect (= {:provider "zai-coding-plan"
                             :model "glm-5.2"
                             :reasoning-effort "max"
                             :json? true
                             :prompt "task"}
                            (#'main/parse-run-args
                             ["--provider" "zai-coding-plan" "--model" "glm-5.2"
                              "--reasoning-effort" "max" "--json" "task"])))))

(defdescribe eval-exit-code-test
             (it "uses 0 for valid eval evidence"
                 (expect (= 0 (#'main/cli-result-exit-code {:eval {:valid? true}}))))
             (it "uses 2 for a completed run invalidated by fallback"
                 (expect (= 2
                            (#'main/cli-result-exit-code
                             {:answer {:answer "done"}
                              :eval {:valid? false
                                     :invalid-reasons [{:type :provider-model-fallback}]}}))))
             (it "uses 2 for unsupported preflight even though the result carries an error"
                 (expect (= 2
                            (#'main/cli-result-exit-code
                             {:error "unsupported"
                              :eval {:valid? false
                                     :invalid-reasons [{:type :unsupported-reasoning-effort}]}}))))
             (it "uses 1 for execution failure"
                 (expect (= 1 (#'main/cli-result-exit-code {:status :error})))
                 (expect (= 1 (#'main/cli-result-exit-code {:error "boom"})))))

(defdescribe toggle-overrides-test
             (it "parses NAME=VALUE pairs against the registry"
                 (expect (= {"main_test_flag" true "reasoning_level" "deep"}
                            (#'main/parse-toggle-overrides
                             "main_test_flag=true,reasoning_level=deep"))))
             (it "rejects unknown toggles as user error"
                 (try (#'main/parse-toggle-overrides "nope-missing=true")
                      (expect false)
                      (catch clojure.lang.ExceptionInfo e
                        (expect (= :vis.cli/unknown-toggle (:type (ex-data e))))
                        (expect (true? (:vis/user-error (ex-data e)))))))
             (it "rejects enum values outside the registered choices"
                 (try (#'main/parse-toggle-overrides "reasoning_level=bogus")
                      (expect false)
                      (catch clojure.lang.ExceptionInfo e
                        (expect (= :vis.cli/invalid-toggle (:type (ex-data e)))))))
             (it "rejects non-boolean values on boolean toggles"
                 (try (#'main/parse-toggle-overrides "main_test_flag=maybe")
                      (expect false)
                      (catch clojure.lang.ExceptionInfo e
                        (expect (= :vis.cli/invalid-toggle (:type (ex-data e)))))))
             (it "applies overrides only while the one-shot body runs"
                 (toggles/set-enabled! "main_test_flag" false)
                 (try (expect (= [true "deep"]
                                 (#'main/call-with-toggle-overrides
                                  {"main_test_flag" true "reasoning_level" "deep"}
                                  #(vector (toggles/enabled? "main_test_flag")
                                           (toggles/value-of "reasoning_level")))))
                      (expect (false? (toggles/enabled? "main_test_flag")))
                      (expect (= "balanced" (toggles/value-of "reasoning_level")))
                      (finally (toggles/reset-to-default! "main_test_flag")
                               (toggles/reset-to-default! "reasoning_level")))))

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
                 (let
                   [{:keys [command]}
                    (commandline/find-leaf (#'main/root-command) ["vis-agent" "sessions"])

                    ^String help
                    (commandline/render-command command ["vis-agent" "sessions"])]

                   (expect (.contains help
                                      "vis-agent sessions <list|show|fork|delete|search|export>"))
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
  model-override-slash-test
  "Model ids may contain a slash (`z-ai/glm-4.6v`). `--model` must not read that
   prefix as a provider tag when a configured provider lists the WHOLE name."
  (it "promotes the provider whose catalog owns the slash-containing model"
      (let
        [config
         {:providers [{:id :anthropic :models [{:name "claude-opus-5"}]}
                      {:id :openrouter :models [{:name "gpt-oss-120b"} {:name "z-ai/glm-4.6v"}]}]}

         out
         (#'main/config-with-model-override config "z-ai/glm-4.6v")

         [active]
         (:providers out)]

        (expect (= :openrouter (:id active)))
        (expect (= "z-ai/glm-4.6v" (:name (first (:models active)))))
        (expect (= [:openrouter :anthropic] (mapv :id (:providers out))))))
  (it "still tags a real provider prefix"
      (let
        [config
         {:providers [{:id :anthropic :models [{:name "claude-opus-5"}]}
                      {:id :openrouter :models [{:name "gpt-oss-120b"}]}]}

         out
         (#'main/config-with-model-override config "openrouter/gpt-oss-120b")

         [active]
         (:providers out)]

        (expect (= :openrouter (:id active)))
        (expect (= "gpt-oss-120b" (:name (first (:models active))))))))

(defdescribe
  toggle-name-parsing-test
  (it "accepts exact snake_case ids"
      (expect (= {"main_test_flag" true "reasoning_level" "deep"}
                 (#'main/parse-toggle-overrides "main_test_flag=true,reasoning_level=deep"))))
  (it "rejects leading-colon, kebab-case, and namespaced aliases"
      (doseq [input [":main_test_flag=true" "main-test-flag=true" "vis/reasoning_level=deep"]]
        (try (#'main/parse-toggle-overrides input)
             (expect false)
             (catch clojure.lang.ExceptionInfo e
               (expect (= :vis.cli/unknown-toggle (:type (ex-data e))))
               (expect (true? (:vis/user-error (ex-data e))))))))
  (it "rejects an unknown snake_case name as user error"
      (try (#'main/parse-toggle-overrides "definitely_not_a_toggle=true")
           (expect false)
           (catch clojure.lang.ExceptionInfo e
             (expect (= :vis.cli/unknown-toggle (:type (ex-data e))))
             (expect (true? (:vis/user-error (ex-data e))))))))

(defdescribe launcher-owned-commands-test
             (let [by-name (into {} (map (juxt :cmd/name identity)) (registry/registered-under []))]
               (it "lists the launcher's runtime and update commands in the binary's help"
                   (doseq [nm ["runtime" "update"]]
                     (let [cmd (get by-name nm)]
                       (expect (some? cmd))
                       (expect (not (str/blank? (:cmd/doc cmd))))
                       (expect (str/starts-with? (:cmd/usage cmd) (str "vis-agent " nm))))))
               (it "documents exactly the launcher's own words, not a source-checkout updater"
                   (expect (= "vis-agent runtime [show | use native|jvm|dev|auto]"
                              (:cmd/usage (get by-name "runtime"))))
                   (expect (= "vis-agent update [--native|--jvm|--dev] [--rebuild] [vX.Y.Z|<sha>]"
                              (:cmd/usage (get by-name "update"))))
                   (expect (not (str/includes? (:cmd/usage (get by-name "update")) "--reset"))))
               (it "refuses to run them inside the binary and names the launcher instead"
                   (doseq [nm ["runtime" "update"]]
                     (try ((:cmd/run-fn (get by-name nm)) {} [])
                          (expect false)
                          (catch clojure.lang.ExceptionInfo e
                            (expect (= :vis.cli/launcher-owned-command (:type (ex-data e))))
                            (expect (true? (:vis/user-error (ex-data e))))
                            (expect (str/includes? (ex-message e) "vis-agent launcher"))))))))
