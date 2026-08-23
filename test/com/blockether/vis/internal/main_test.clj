(ns com.blockether.vis.internal.main-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.commandline :as commandline]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.main :as main]
            [com.blockether.vis.internal.registry :as registry]
            [com.blockether.vis.internal.toggles :as toggles]
            [com.blockether.vis.internal.workspace :as workspace]
            [lazytest.core :refer [defdescribe expect it throws?]]))

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
      (let [^String help
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
  (it "documents the runtime, and offers nothing to select"
      (let [^String help (commandline/render-tree (#'main/root-command))]
        (expect (.contains help "RUNTIME (WHAT RUNS)"))
        (doseq [row ["vis-agent runtime" "vis-agent update"]]
          (expect (.contains help row)))
        ;; There is no runtime SELECTOR any more: vis-agent installs under
        ;; ~/.vis, what is installed is what runs, and help must not advertise
        ;; a switch that no longer exists.
        (doseq [gone ["--native" "--jvm" "--dev" "VIS_RUNTIME" "runtime use" "dev|auto"]]
          (expect (not (str/includes? help gone)) gone))))
  (it "points at the configuration a run reads"
      (let [^String help (commandline/render-tree (#'main/root-command))]
        (expect (.contains help "CONFIGURATION"))
        (expect (.contains help "~/.vis/config.yml"))
        (expect (.contains help "<project>/vis.yml"))))
  (it "documents the flags that pick WHICH gateway does the work, TUI included"
      (let [^String help (commandline/render-tree (#'main/root-command))]
        (expect (.contains help "GATEWAY (WHICH DAEMON RUNS THE WORK)"))
        (expect (.contains help "--gateway HOST[:PORT]|URL"))
        (expect (.contains help "--gateway-token TOKEN"))
        (expect (.contains help "VIS_GATEWAY_URL"))
        (expect (.contains help "VIS_GATEWAY_TOKEN"))
        (expect (.contains help "vis-agent --gateway 10.0.0.5 --gateway-token TOKEN tui")))))

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
      (let [out
            (java.io.StringWriter.)

            discovered?
            (atom false)

            fake-channel
            {:channel/id ::fast-help-test
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
  (it "strips launcher-owned flags when they leak into JVM args"
      (expect (= ["channels" "--help"] (#'main/strip-global-args ["channels" "--jfr" "--help"]))))
  (it "strips --stream-trace, which the wrapper consumes as a system property"
      (expect (= ["channels" "tui"]
                 (#'main/strip-global-args ["channels" "--stream-trace" "tui"])))))

(defdescribe
  gateway-flags-test
  (it "splits --gateway and --gateway-token out of the args, in either order"
      (expect (= {:gateway {:url "10.0.0.5:7890" :token "t"} :args ["tui"]}
                 (#'main/split-gateway-flags
                  ["--gateway" "10.0.0.5:7890" "tui" "--gateway-token" "t"]))))
  (it "accepts the =-joined form and stops at a bare --"
      (expect (= {:gateway {:url "gateway.example.com"} :args ["--" "--gateway" "prompt text"]}
                 (#'main/split-gateway-flags
                  ["--gateway=gateway.example.com" "--" "--gateway" "prompt text"]))))
  (it "leaves an invocation that names no gateway completely alone"
      (expect (= {:gateway nil :args ["channels" "tui"]}
                 (#'main/split-gateway-flags ["channels" "tui"]))))
  (it "refuses a token with no address instead of silently using the local daemon"
      (expect (throws? clojure.lang.ExceptionInfo #(#'main/connect-gateway! {:token "t"})))))

(defdescribe log-role-for-args-test
             (it "labels only the long-lived TUI and gateway server processes"
                 (expect (= "tui" (#'main/log-role-for-args ["channels" "tui"])))
                 (expect (= "gateway" (#'main/log-role-for-args ["gateway" "start"])))
                 (expect (= "vis" (#'main/log-role-for-args ["gateway" "status"])))
                 (expect (= "vis" (#'main/log-role-for-args ["python" "-c" "print(1)"])))))

(defdescribe
  gateway-command-help-test
  (it
    "says in `gateway` help which subcommands follow --gateway, and which never leave this machine"
    (let [subs
          (into {} (map (juxt :cmd/name identity)) (registry/registered-under ["gateway"]))

          parent
          (first (filter #(= "gateway" (:cmd/name %)) (registry/registered-under [])))

          says-remote?
          (fn [cmd-name]
            (str/includes? (str (:cmd/doc (get subs cmd-name))
                                " "
                                (str/join " " (:cmd/examples (get subs cmd-name))))
                           "--gateway"))]

      (expect (str/includes? (:cmd/usage parent) "--gateway HOST[:PORT] --gateway-token TOKEN"))
      ;; `status` and `pair` answer from the --gateway target, `stop` refuses one
      ;; outright, and `start` always runs a daemon HERE. Help that named none of
      ;; this read as if --gateway did nothing to the gateway commands themselves.
      (expect (says-remote? "status"))
      (expect (says-remote? "pair"))
      (expect (says-remote? "stop"))
      (expect (str/includes? (:cmd/doc (get subs "start")) "THIS machine"))
      ;; --db picks a LOCAL registry, so a remote target ignores it.
      (expect (str/includes? (->> (:cmd/args (get subs "status"))
                                  (map :doc)
                                  (str/join " "))
                             "ignored when --gateway")))))

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
                 (let [{:keys [command]}
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
      (let [config
            {:providers [{:id :anthropic :models [{:name "claude-opus-5"}]}
                         {:id :openrouter
                          :models [{:name "gpt-oss-120b"} {:name "z-ai/glm-4.6v"}]}]}

            out
            (#'main/config-with-model-override config "z-ai/glm-4.6v")

            [active]
            (:providers out)]

        (expect (= :openrouter (:id active)))
        (expect (= "z-ai/glm-4.6v" (:name (first (:models active)))))
        (expect (= [:openrouter :anthropic] (mapv :id (:providers out))))))
  (it "still tags a real provider prefix"
      (let [config
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
             (it "keeps the launcher's runtime and update out of the binary's command tree"
                 ;; The `vis-agent` wrapper runs both itself and never forwards them,
                 ;; so registering them here only advertised commands this runtime
                 ;; cannot execute.
                 (let [by-name
                       (into {} (map (juxt :cmd/name identity)) (registry/registered-under []))]
                   (doseq [nm ["runtime" "update"]]
                     (expect (nil? (get by-name nm))))))
             (it "still documents them where the launcher owns them: the RUNTIME help section"
                 (let [^String help (commandline/render-tree (#'main/root-command))]
                   (expect (str/includes? help "RUNTIME (WHAT RUNS)"))
                   (doseq [row ["vis-agent runtime" "vis-agent update"]]
                     (expect (str/includes? help row))))))

;;; ── `vis-agent projects` ──────────────────────────────────────────────────────
;;
;; `vis-agent sessions delete` has always existed; there was no way at all to
;; remove a PROJECT and the conversations in it, so the blast radius is opt-in
;; (`--with-sessions`) and the default stays the scatter delete the schema has
;; always had.

(def ^:private match-projects @#'main/match-projects)

(def ^:private delete-project-tree! @#'main/delete-project-tree!)

(defdescribe
  project-id-resolution-test
  (let [projects [{:id "9f2c1a44-0000-0000-0000-000000000001" :name "vis"}
                  {:id "9f2c1a44-0000-0000-0000-000000000002" :name "spel"}
                  {:id "b1000000-0000-0000-0000-000000000003" :name "svar"}]]
    (it "resolves a full id, and an unambiguous prefix"
        (expect (= ["svar"] (mapv :name (match-projects projects "b1000000"))))
        (expect (= ["vis"]
                   (mapv :name (match-projects projects "9F2C1A44-0000-0000-0000-000000000001")))))
    (it "reports EVERY candidate for an ambiguous prefix instead of picking one"
        ;; Deleting a project is irreversible: a prefix that fits two projects
        ;; must never silently resolve to the first one.
        (expect (= ["vis" "spel"] (mapv :name (match-projects projects "9f2c")))))
    (it "matches nothing for an unknown id or blank input"
        (expect (= [] (match-projects projects "zzzz")))
        (expect (= [] (match-projects projects "  ")))
        (expect (= [] (match-projects projects nil))))))

(defdescribe
  cli-project-delete-blast-radius-test
  (let [pid
        "9f2c1a44-0000-0000-0000-000000000001"

        exec
        (fn [opts]
          (let [log (atom [])]
            (with-redefs [lp/project-session-ids (fn [p]
                                                   (if (= pid p) ["s-a" "s-b"] []))
                          workspace/discard-session-clones! (fn [_d sid]
                                                              (swap! log conj [:drafts sid])
                                                              (future nil))
                          lp/delete! (fn [sid]
                                       (swap! log conj [:session sid]))
                          lp/delete-project! (fn [p]
                                               (swap! log conj [:project p]))]

              {:result (delete-project-tree! ::db pid opts) :log @log})))]

    (it "keeps the scatter default: the project row goes, not one conversation"
        (let [{:keys [result log]} (exec {})]
          (expect (= [[:project pid]] log))
          (expect (= [] (:deleted-session-ids result)))
          (expect (= ["s-a" "s-b"] (:kept-session-ids result)))))
    (it "--with-sessions deletes each member's drafts and tree BEFORE the project row"
        ;; Sessions first: an interrupted teardown must leave a project holding
        ;; survivors, never orphaned sessions with a dead parent.
        (let [{:keys [result log]} (exec {:with-sessions true})]
          (expect (= [[:drafts "s-a"] [:session "s-a"] [:drafts "s-b"] [:session "s-b"]
                      [:project pid]]
                     log))
          (expect (= ["s-a" "s-b"] (:deleted-session-ids result)))
          (expect (= [] (:kept-session-ids result)))
          (expect (= pid (:project-id result)))))))


(defdescribe
  gateway-status-staleness-test
  ;; `gateway status` is what gets asked when an update looks like it did nothing,
  ;; so it must answer with the same verdict an attach would reach - never promise
  ;; a replacement the next client would refuse to make.
  (let [ours
        {:version "0.1.40" :build "aaaaaaaaaaaa"}

        note
        (fn [status]
          (#'main/stale-daemon-note status ours))

        running
        (fn [m]
          (merge {"status" "running" "managed" true "clients" 0 "running_turns" 0} m))]

    (it "names a dev build by its commit, because \"dev\" alone names no code"
        (expect (= "dev (abc123abc123)"
                   (#'main/build-label {:version "dev" :build "abc123abc123"})))
        (expect (= "0.1.40" (#'main/build-label {:version "0.1.40" :build "abc123abc123"}))))
    (it "says what picks the new build up when nothing is using the old daemon"
        (let [s (note (running {"protocol" {"version" "0.1.39" "build" "bbbbbbbbbbbb"}}))]
          (expect (str/includes? s "this build is 0.1.40"))
          (expect (str/includes? s "next session starts on it"))))
    (it "counts what is holding the old daemon instead of promising a replacement"
        (let [s (note (running {"clients" 2 "running_turns" 1 "protocol" {"version" "0.1.39"}}))]
          (expect (str/includes? s "2 clients"))
          (expect (str/includes? s "1 running turn"))
          (expect (not (str/includes? s "next session")))))
    (it "hands a user-owned daemon back to whoever started it"
        (let [s (note (running {"managed" false "pid" 4242 "protocol" {"version" "0.1.39"}}))]
          (expect (str/includes? s "user-owned"))
          (expect (str/includes? s "4242"))))
    (it "counts nothing it could not read"
        (let [s (note (running {"clients" :two "protocol" {"version" "0.1.39"}}))]
          (expect (str/includes? s "no longer in use"))))
    (it "is silent about a daemon this build does not replace"
        (expect (nil? (note (running {"protocol" {"version" "0.1.40" "build" "aaaaaaaaaaaa"}}))))
        (expect (nil? (note (running {"protocol" {"version" "0.1.41" "build" "cccccccccccc"}}))))
        (expect (nil? (note (running {"protocol" {"version" "dev"}})))))))
