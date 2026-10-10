(ns com.blockether.vis.internal.main-test
  (:require [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis.internal.commandline :as commandline]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.decisions.assets :as decisions-assets]
            [com.blockether.vis.internal.gateway.cli :as gateway-cli]
            [com.blockether.vis.internal.gateway.client :as gateway-client]
            [com.blockether.vis.internal.gateway.state :as gateway-state]
            [com.blockether.vis.internal.loop.environment :as loop-env]
            [com.blockether.vis.internal.loop.router :as loop-router]
            [com.blockether.vis.internal.loop.turn :as turn]
            [com.blockether.vis.internal.main :as main]
            [com.blockether.vis.internal.extension.manifest :as manifest]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.python.extensions :as python-extensions]
            [com.blockether.vis.internal.python.runtime :as python-runtime]
            [com.blockether.vis.internal.extension.registry :as registry]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [lazytest.core :refer [defdescribe describe expect it throws?]]))

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
  extension-list-github-identity-test
  (it "shows GitHub identity while retaining the package name used by lifecycle commands"
      (with-redefs [extension/registered-extensions
                    (constantly [{:ext/name "vis-greeter"
                                  :ext/description "Greeting tools"
                                  :ext/repository "example/extensions"
                                  :ext/owner "unverified"}
                                 {:ext/name "local-tools" :ext/description "Local tools"}
                                 {:ext/name "bundled-tools"
                                  :ext/description "Bundled tools"
                                  :ext/owner "vis"}])]
        (expect (= [{:namespace "example/extensions (vis-greeter)" :owner "example"}
                    {:namespace "local-tools" :owner "-"} {:namespace "bundled-tools" :owner "vis"}]
                   (mapv #(select-keys % [:namespace :owner]) (main/list-extensions)))))))

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
            (binding [commandline/*color-enabled?* false]
              (commandline/render-tree (#'main/root-command)))

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
  (it "documents the per-launch JVM override separately from update tracks"
      (let [^String help (commandline/render-tree (#'main/root-command))]
        (expect (.contains help "UPDATES"))
        (doseq [row ["vis-agent update" "--track release|beta|dev" "default" "--jvm" "RUNTIME"
                     "vis-agent switch list" "vis-agent switch <identifier>"]]
          (expect (.contains help row)))
        (doseq [gone ["--native" "--dev" "VIS_RUNTIME" "vis-agent runtime" "--rebuild"]]
          (expect (not (str/includes? help gone)) gone))))
  (it "points at the configuration a run reads"
      (let [^String help (commandline/render-tree (#'main/root-command))]
        (expect (.contains help "CONFIGURATION"))
        (expect (.contains help "~/.vis/config.yml"))
        (expect (.contains help "<project>/vis.yml"))))
  (it "documents the flags that pick WHICH gateway does the work"
      (let [^String help (commandline/render-tree (#'main/root-command))]
        (expect (.contains help "GATEWAY (WHICH DAEMON RUNS THE WORK)"))
        (expect (.contains help "--gateway HOST[:PORT]|URL"))
        (expect (.contains help "--gateway-token TOKEN"))
        (expect (.contains help "VIS_GATEWAY_URL"))
        (expect (.contains help "VIS_GATEWAY_TOKEN"))
        (expect (.contains help
                           "vis-agent --gateway 10.0.0.5 --gateway-token TOKEN sessions list")))))

;; Regression, Vis session ae259fdd-2712-4591-8f12-e1cdff30b208: a client
;; synchronously initialized CPython before dispatch, then its gateway did it again.
(defdescribe
  dispatch-extension-initialization-test
  (it "skips extension catalogs for host-only commands (#300)"
      (doseq [args
              [["gateway"] ["gateway" "start"] ["gateway" "tui"] ["gateway" "stop"]
               ["gateway" "stop" "--if-idle"] ["gateway" "stop" "--db" "unused.sqlite" "--if-idle"]
               ["gateway" "status"] ["gateway" "pair"] ["gateway" "mcp" "list"] ["sessions" "list"]
               ["sessions" "export" "session-id" "--md"] ["projects" "list"]
               ["speech" "models" "status"] ["speech" "models" "download"]
               ["decisions" "models" "status"] ["decisions" "models" "download"]
               ["extension" "sync" "--dry-run"] ["extension" "sync" "--trust"]
               ["extension" "install" "./vis-greeter"] ["extension" "versions" "example/greeting"]
               ["extension" "update" "example/greeting"] ["extension" "rollback" "example/greeting"]
               ["python" "-c" "print(1)"] ["stdio"] ["web"]]]
        (let [calls (atom [])]
          (with-redefs [manifest/initialize! #(swap! calls conj :clojure)
                        python-extensions/load-python-extensions! #(swap! calls conj :python)]

            (#'main/initialize-for-dispatch! false args)
            (expect (= [:clojure] @calls) (pr-str args))))))
  (it "loads extension catalogs for commands that use registered extension surfaces"
      (doseq [args [["extension" "list"] ["extension" "vis-greeter" "greet"] ["channels" "tui"]
                    ["providers" "list"] ["doctor"] ["--json" "Summarize this project"]]]
        (let [calls (atom [])]
          (with-redefs [manifest/initialize! #(swap! calls conj :clojure)
                        python-extensions/load-python-extensions! #(swap! calls conj :python)]

            (#'main/initialize-for-dispatch! false args)
            (expect (= [:clojure :python] @calls) (pr-str args))))))
  (describe "gateway shutdown with unavailable extensions"
            ;; #300: shutdown must reach its handler without importing or preparing extensions.
            (it
              "dispatches stop and idle-stop for running and already-stopped gateways"
              (doseq [{:keys [args response idle-response expected-call expected-output]}
                      [{:args ["gateway" "stop"]
                        :response {:stopping true :pid 42 :clients 2 :running-turns 0}
                        :expected-call :stop
                        :expected-output "gateway stopping (pid 42) - releasing 2 clients\n"}
                       {:args ["gateway" "stop"]
                        :response {:status "stopped"}
                        :expected-call :stop
                        :expected-output "gateway stopped\n"}
                       {:args ["gateway" "stop" "--if-idle"]
                        :idle-response {:stopped? true}
                        :expected-call :stop-if-idle
                        :expected-output "gateway stopped - next session starts on 0.2.29\n"}
                       {:args ["gateway" "stop" "--if-idle"]
                        :idle-response {:reason :not-running}
                        :expected-call :stop-if-idle
                        :expected-output ""}]

                      measure?
                      [false true]]

                (let [calls
                      (atom [])

                      lines
                      (atom [])]

                  (with-redefs-fn {#'manifest/initialize! #(swap! calls conj :clojure)
                                   #'python-extensions/load-python-extensions!
                                   (fn [& _]
                                     (swap! calls conj :python)
                                     (throw (ex-info "Configured extension cannot be loaded" {})))
                                   #'python-runtime/ensure-project!
                                   (fn [_]
                                     (swap! calls conj :prepare)
                                     (throw (ex-info "Configured extension cannot be prepared" {})))
                                   #'config/init-cli! (constantly nil)
                                   #'commandline/stdout! #(swap! lines conj %)
                                   #'gateway-client/stop-daemon! (fn []
                                                                   (swap! calls conj :stop)
                                                                   response)
                                   #'gateway-client/stop-daemon-if-idle! (fn []
                                                                           (swap! calls conj
                                                                             :stop-if-idle)
                                                                           idle-response)
                                   #'gateway-cli/this-handshake (constantly {:version "0.2.29"})}
                    (fn []
                      (binding [*err* (java.io.StringWriter.)]
                        (#'main/initialize-for-dispatch! measure? args)
                        (expect (= :ok
                                   (:status (commandline/dispatch! (#'main/root-command)
                                                                   (into ["vis-agent"] args))))))
                      (expect (= [:clojure expected-call] @calls))
                      (expect (= expected-output (apply str (map #(str % "\n") @lines)))))))))))

(defdescribe one-shot-router-boundary-test
             (it "uses Vis's provider-enriching router builder for explicit overrides"
                 (let [config {:providers [{:id :lmstudio :models [{:name "meta/muse-glimmer"}]}]}]
                   (with-redefs [loop-router/build-router #(assoc % :enriched? true)
                                 loop-router/get-router (fn []
                                                          :shared)]

                     (expect (= (assoc config :enriched? true) (#'main/router-for-run config true)))
                     (expect (= :shared (#'main/router-for-run config false))))))
             (it "reads a newly created gateway session id from canonical wire data"
                 (let [submitted
                       (atom nil)

                       config
                       {:providers [{:id :lmstudio :models [{:name "meta/muse-glimmer"}]}]}]

                   (with-redefs [config/load-config
                                 (constantly nil)

                                 loop-router/rebuild-router!
                                 (constantly nil)

                                 gateway-state/create-session!
                                 (fn [_]
                                   {"id" "wire-session"})

                                 gateway-state/submit-turn-sync!
                                 (fn [sid _]
                                   (reset! submitted sid)
                                   {"content" []})]

                     (expect (= "wire-session"
                                (:session-id (main/run! {} "hi" {:config config :persist? true}))))
                     (expect (= "wire-session" @submitted))))))

(defdescribe
  persistent-one-shot-route-test
  (it "forwards explicit provider and model selection to gateway turn submission"
      (doseq [[opts expected] [[{:provider "github-copilot" :model "gpt-6-astra"}
                                {:provider :github-copilot :model "gpt-6-astra"}]
                               [{:provider "github-copilot"} {:provider :github-copilot}]
                               [{:model "gpt-6-astra"} {:model "gpt-6-astra"}]
                               [{:model "github-copilot/gpt-6-astra"}
                                {:provider :github-copilot :model "gpt-6-astra"}]]]
        (let [submitted (atom nil)
              config {:providers [{:id :openai-codex :models [{:name "gpt-6-astra"}]}
                                  {:id :github-copilot :models [{:name "gpt-6-astra"}]}]}]

          (with-redefs [loop-router/rebuild-router! (constantly nil)
                        gateway-state/create-session! (constantly {"id" "wire-session"})
                        gateway-state/submit-turn-sync! (fn [_ request]
                                                          (reset! submitted request)
                                                          {"content" []})]

            (main/run! {} "hi" (merge {:config config :persist? true :reasoning-effort "low"} opts))
            (expect (= expected (select-keys @submitted [:provider :model])) (pr-str opts))
            (expect (= "low" (get-in @submitted [:engine-opts :reasoning-effort])))
            (when (:model expected)
              (expect (= (:model expected) (get-in @submitted [:engine-opts :model])))))))))

(defdescribe
  fast-help-test
  (it "does not swallow unknown root commands that also ask for help"
      (expect (nil? (#'main/fast-help-dispatched? false ["missing" "--help"]))))
  (it "still handles known built-in help without initializing the distribution"
      (let [out (java.io.StringWriter.)]
        (binding [*out* out]
          (expect (true? (#'main/fast-help-dispatched? false ["providers" "--help"]))))
        (expect (.contains (str out) "vis-agent providers"))))
  (it "documents transparent bundled uv invocation through Python help (#183)"
      (let [out (java.io.StringWriter.)]
        (binding [*out* out]
          (expect (= :help
                     (:status (commandline/dispatch! (#'main/root-command)
                                                     ["vis-agent" "python" "--help"])))))
        (expect (str/includes? (str out) "pass commands unchanged to bundled uv"))))
  (it "loads channels before rendering channels parent help"
      (let [out
            (java.io.StringWriter.)

            initialized?
            (atom false)

            fake-channel
            {:channel/id ::fast-help-test
             :channel/cmd "zzz-test"
             :channel/doc "Test channel for help."
             :channel/main-fn (fn [_args])}]

        (try (with-redefs [main/initialize-all! (fn []
                                                  (reset! initialized? true)
                                                  (registry/register-channel! fake-channel))]
               (binding [*out* out]
                 (expect (true? (#'main/fast-help-dispatched? false ["channels" "--help"]))))
               (expect (true? @initialized?))
               (expect (.contains (str out) "zzz-test"))
               (expect (.contains (str out) "Test channel for help.")))
             (finally (registry/deregister-channel! (:channel/id fake-channel))))))
  (it "initializes the closed distribution before concrete channel help"
      (let [initialized? (atom false)]
        (with-redefs [main/initialize-all! #(reset! initialized? true)]
          (#'main/initialize-fast-help-deps! ["channels" "tui" "--help"])
          (expect (true? @initialized?)))))
  ;; #300: asking for package-management help must not prepare or import packages.
  (it "renders package-management help without loading Python extensions"
      (doseq [command ["install" "sync" "versions" "update" "rollback"]]
        (let [calls (atom [])]
          (with-redefs [manifest/initialize! #(swap! calls conj :clojure)
                        python-extensions/load-python-extensions!
                        (fn [& _]
                          (swap! calls conj :python)
                          (throw (ex-info "Configured extension cannot be loaded" {})))]

            (with-out-str
              (expect (true? (#'main/fast-help-dispatched? false ["extension" command "--help"]))))
            (expect (= [:clojure] @calls) command)))))
  (it "still loads extension catalogs for parent, list and contributed command help"
      (doseq [args [["extension" "--help"] ["extension" "list" "--help"]
                    ["extension" "vis-greeter" "--help"]]]
        (let [calls (atom [])]
          (with-redefs [main/initialize-all! #(swap! calls conj :all)]
            (#'main/initialize-fast-help-deps! args)
            (expect (= [:all] @calls) (pr-str args))))))
  (it "strips launcher-owned flags when they leak into JVM args"
      (expect (= ["channels" "--help"] (#'main/strip-global-args ["channels" "--jfr" "--help"]))))
  (it "strips --stream-trace, which the wrapper consumes as a system property"
      (expect (= ["channels" "tui"]
                 (#'main/strip-global-args ["channels" "--stream-trace" "tui"])))))

(defdescribe stdio-command-help-test
             (it "explains the SDK stdio mode without starting it"
                 (let [out (java.io.StringWriter.)]
                   (binding [*out* out]
                     (expect (true? (#'main/fast-help-dispatched? false ["stdio" "--help"]))))
                   (doseq [text ["vis-agent stdio" "Python SDK" "LocalEngine" "VIS_DB_PATH" "NDJSON"
                                 "stdin" "stdout"]]
                     (expect (str/includes? (str out) text) text))))
             (it "lists the new name at the root instead of the SDK-prefixed name"
                 (let [help (commandline/render-tree (#'main/root-command))]
                   (expect (str/includes? help "stdio"))
                   (expect (not (str/includes? help "sdk-stdio")))))
             (it "defers Python extension loading for stdio"
                 (expect (true? (#'main/deferred-python-dispatch? ["stdio"])))))

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
             (it "labels only the long-lived gateway server process"
                 (expect (= "gateway" (#'main/log-role-for-args ["gateway" "start"])))
                 (expect (= "vis" (#'main/log-role-for-args ["gateway" "status"])))
                 (expect (= "vis" (#'main/log-role-for-args ["python" "-c" "print(1)"])))))

(defdescribe extension-command-surface-test
             (it "keeps listing, installation and contributed commands under `vis-agent extension`"
                 (let [names (set (map :cmd/name (registry/registered-under ["extension"])))]
                   (expect (contains? names "list"))
                   (expect (every? #(not (contains? names %)) ["scaffold" "check" "test"])))))

(defdescribe
  extension-install-command-test
  (it
    "passes string-keyed CLI arguments to the package installer"
    ;; Native installation rejected --trust because keyword destructuring discarded parsed flags.
    (doseq [[flags base trusted folder revision]
            [[[] "user.home" false "" nil] [["--global"] "user.home" false "" nil]
             [["--trust" "--project" "--subdirectory" "plugins/greeting" "--revision"
               (apply str (repeat 40 "a"))] "user.dir" true "plugins/greeting"
              (apply str (repeat 40 "a"))]]]
      (let [calls (atom [])]
        (with-redefs [python-extensions/install-package!
                      (fn [source options]
                        (swap! calls conj [source options])
                        {"name" "vis-greeter" "version" "1.0.0" "mode" "source" "next" "/reload"})]
          (with-out-str (commandline/dispatch!
                          (#'main/root-command)
                          (into ["vis-agent" "extension" "install" "./vis-greeter"] flags))))
        (let [[source options] (first @calls)]
          (expect (= 1 (count @calls)))
          (expect (= "./vis-greeter" source))
          (expect (= trusted (boolean (:trust options))))
          (expect (= folder (:subdirectory options)))
          (expect (= revision (:revision options)))
          (expect (= (str (System/getProperty base) "/.vis/extensions") (:directory options)))))))
  (it "rejects conflicting scopes before installing"
      (let [calls (atom [])]
        (with-redefs [python-extensions/install-package! (fn [& args]
                                                           (swap! calls conj args))]
          (expect (throws? clojure.lang.ExceptionInfo
                           #(commandline/dispatch! (#'main/root-command)
                                                   ["vis-agent" "extension" "install"
                                                    "example/greeting" "--trust" "--project"
                                                    "--global"]))))
        (expect (empty? @calls)))))

(defdescribe
  extension-install-save-command-test
  (it
    "passes explicit save intent and scope without changing ordinary installation"
    (doseq [scope [nil "--project" "--global"]]
      (let [calls (atom [])]
        (with-redefs [python-extensions/install-package!
                      (fn [source options]
                        (swap! calls conj [source options])
                        {"name" "vis-greeter" "version" "1.0.0" "mode" "source" "next" "/reload"})]
          (with-out-str (commandline/dispatch! (#'main/root-command)
                                               (cond-> ["vis-agent" "extension" "install"
                                                        "./vis-greeter" "--trust" "--save"]
                                                 scope
                                                 (conj scope)))))
        (let [[_ options] (first @calls)]
          (expect (= 1 (count @calls)))
          (expect (true? (:save options)))
          (expect (= (= scope "--project") (boolean (:project options)))))))))

(defdescribe
  extension-version-commands-test
  (it "prints the repository slug rather than the Python distribution name"
      (let [lines (atom [])]
        (with-redefs [commandline/stdout! #(swap! lines conj %)]
          (#'main/print-package-result
           "Installed"
           {"name" "vis-greeter"
            "repository" "example/extensions"
            "version" "1.2.0"
            "mode" "github"
            "next" "/reload"}))
        (expect (= ["Installed example/extensions@1.2.0 (github). /reload"] @lines))))
  (it "passes version selection to install without losing the project scope"
      (let [calls (atom [])]
        (with-redefs [python-extensions/install-package!
                      (fn [source options]
                        (swap! calls conj [source options])
                        {"name" "vis-greeter" "version" "1.2.0" "mode" "github" "next" "/reload"})]
          (with-out-str (commandline/dispatch! (#'main/root-command)
                                               ["vis-agent" "extension" "install" "example/greeting"
                                                "--version" "1.2.0" "--trust" "--project"])))
        (expect (= "example/greeting" (get-in @calls [0 0])))
        (expect (= "1.2.0" (get-in @calls [0 1 :version])))
        (expect (= (str (System/getProperty "user.dir") "/.vis/extensions")
                   (get-in @calls [0 1 :directory])))))
  (it "dispatches approved version discovery with installed and update information"
      (let [lines (atom [])]
        (with-redefs [commandline/stdout! #(swap! lines conj %)
                      python-extensions/package-versions
                      (fn [source options]
                        (expect (= "example/extensions" source))
                        (expect (= "plugins/greeting" (:subdirectory options)))
                        {"installed" "1.0.0"
                         "latest" "1.2.0"
                         "update_available" true
                         "releases" [{"version" "1.2.0" "revision" (apply str (repeat 40 "a"))}]})]

          (commandline/dispatch! (#'main/root-command)
                                 ["vis-agent" "extension" "versions" "example/extensions"
                                  "--subdirectory" "plugins/greeting"]))
        (expect (some #(str/includes? % "Update available") @lines))
        (expect (some #(str/includes? % "1.2.0") @lines))))
  (it "passes explicit trust, version and project to lifecycle operations"
      (doseq [command ["update" "rollback"]]
        (let [calls (atom [])
              operation (fn [name options]
                          (swap! calls conj [name options])
                          {"name" name "version" "1.2.0" "mode" "github" "next" "/reload"})]

          (with-redefs [python-extensions/update-package! operation
                        python-extensions/rollback-package! operation]

            (with-out-str (commandline/dispatch!
                            (#'main/root-command)
                            ["vis-agent" "extension" command "example/extensions" "--trust"
                             "--version" "1.2.0" "--project" "--subdirectory" "plugins/greeting"])))
          (expect (= [["example/extensions"
                       {:trust true
                        :version "1.2.0"
                        :subdirectory "plugins/greeting"
                        :directory (str (System/getProperty "user.dir") "/.vis/extensions")}]]
                     @calls))))))

(defdescribe
  extension-sync-command-test
  (it "preserves explicit trust, scope, cache refresh and removal intent"
      (let [calls (atom [])]
        (with-redefs [python-extensions/sync-packages! (fn [options]
                                                         (swap! calls conj options)
                                                         [])]
          (with-out-str (commandline/dispatch! (#'main/root-command)
                                               ["vis-agent" "extension" "sync" "--trust" "--project"
                                                "--refresh" "--prune" "--dry-run"])))
        (expect (= [{:trust true :project true :global nil :refresh true :prune true :dry-run true}]
                   @calls))))
  (it "does not report a partially failed sync as success"
      (with-redefs [python-extensions/sync-packages! (constantly [{"scope" "project"
                                                                   "name" "fixture"
                                                                   "status" "failed"
                                                                   "error" "Fixture failure"}])]
        (expect (try (with-out-str (commandline/dispatch! (#'main/root-command)
                                                          ["vis-agent" "extension" "sync"
                                                           "--trust"]))
                     false
                     (catch clojure.lang.ExceptionInfo _ true))))))

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
                             "ignored when --gateway"))))
  (it "documents the gateway JVM override in command help"
      (let [parent
            (first (filter #(= "gateway" (:cmd/name %)) (registry/registered-under [])))

            start
            (first (filter #(= "start" (:cmd/name %)) (registry/registered-under ["gateway"])))]

        (doseq [[cmd path] [[parent ["vis-agent" "gateway"]]
                            [start ["vis-agent" "gateway" "start"]]]]
          (let [help (commandline/render-command cmd path)]
            (expect (str/includes? help "[--jvm]"))
            (expect (str/includes? help "vis-agent gateway start --jvm"))))
        (expect (str/includes? (:cmd/doc start) "without changing the installed track")))))

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
                 (try (expect (= [true "deep" true]
                                 (#'main/call-with-toggle-overrides
                                  {"main_test_flag" true "reasoning_level" "deep"}
                                  #(vector (toggles/enabled? "main_test_flag")
                                           (toggles/value-of "reasoning_level")
                                           ;; Every session started here merges the
                                           ;; flag over its scoped settings.
                                           (get toggles/*invocation-overrides* "main_test_flag")))))
                      (expect (false? (toggles/enabled? "main_test_flag")))
                      (expect (= "balanced" (toggles/value-of "reasoning_level")))
                      (finally (toggles/reset-to-default! "main_test_flag")
                               (toggles/reset-to-default! "reasoning_level")))))

(defdescribe
  cli-merged-toggle-config-test
  (it "hydrates merged configuration before running and restores CLI overrides without saving"
      ;; #241: one-shot native runs ignored an explicit worktree backend in YAML.
      (manifest/initialize!)
      (let [previous
            @#'toggles/state

            saved-state
            @previous]

        (try
          (doseq [[configured override expected]
                  [[nil nil "off"] ["off" nil "off"] ["worktree" nil "worktree"]
                   ["off" "worktree" "worktree"] ["worktree" "off" "off"]]]
            (let [seen (atom [])
                  writes (atom [])
                  merged (if configured {"toggles" {"draft_backend" configured}} {})]

              (toggles/reset-to-default! "draft_backend")
              (with-redefs-fn {#'config/init-cli! (fn []
                                                    nil)
                               #'config/load-config-raw (fn []
                                                          merged)
                               #'config/save-config! (fn [& _]
                                                       (swap! writes conj :config))
                               #'config/save-toggles! (fn [& _]
                                                        (swap! writes conj :toggles))
                               #'commandline/stdout! (fn [& _]
                                                       nil)
                               #'clojure.core/shutdown-agents (fn []
                                                                nil)
                               #'main/run! (fn [& _]
                                             (swap! seen conj (toggles/value-of "draft_backend"))
                                             {:content []})}
                #(#'main/cli-run!
                   {}
                   (cond-> ["--raw"]
                     override
                     (into ["--toggles" (str "draft_backend=" override)])

                     true
                     (conj "fixture"))))
              (expect (empty? @writes))
              (expect (= [expected] @seen))
              (expect (= (or configured "off") (toggles/value-of "draft_backend")))))
          (finally (reset! previous saved-state))))))

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
        (expect (false? (#'main/root-run-shortcut? root ["--help"])))))
  ;; `vis-agent upgrade` used to reach the engine, match no command and become a
  ;; PROMPT: a whole model turn spent on the word "upgrade" while nothing
  ;; updated. The launcher owns these words; one arriving here is a mistake.
  (it "never turns a lone wrapper word into a prompt"
      (let [root (#'main/root-command)]
        (doseq [word ["update" "upgrade" "switch" "desktop" "runtime"]]
          (expect (false? (#'main/root-run-shortcut? root [word])) word))))
  (it "still lets a real question that starts with one through"
      (let [root (#'main/root-command)]
        (expect (true? (#'main/root-run-shortcut? root ["update" "the" "readme"])))
        (expect (true? (#'main/root-run-shortcut? root ["switch" "to" "the" "other" "branch"])))))
  (it "recognizes exactly the words the launcher implements"
      (expect (true? (#'main/wrapper-owned-invocation? ["upgrade"])))
      (expect (false? (#'main/wrapper-owned-invocation? ["upgrades"])))
      (expect (false? (#'main/wrapper-owned-invocation? ["update" "--track" "beta"])))))

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
                   (expect (.contains help "export"))
                   (expect (not (.contains help "draft"))))))

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

(defdescribe
  decision-models-command-test
  (it "routes the model name and optional training flag through the built-in CLI"
      (let [seen (atom [])]
        (with-redefs [config/init-cli! (constantly nil)
                      decisions-assets/download-model! (fn [model training?]
                                                         (swap! seen conj [model training?])
                                                         {:inference "/tmp/test-model"
                                                          :training "/tmp/test-training"
                                                          :wheels "/tmp/test-wheels"})]

          (commandline/dispatch! (#'main/root-command)
                                 ["vis-agent" "decisions" "models" "download" "--model"
                                  "laya-typed-decisions"])
          (commandline/dispatch! (#'main/root-command)
                                 ["vis-agent" "decisions" "models" "download" "--model"
                                  "laya-typed-decisions" "--training"])
          (expect (= [["laya-typed-decisions" false] ["laya-typed-decisions" true]] @seen))))))

(defdescribe launcher-owned-commands-test
             (it "keeps launcher-owned commands out of the binary command tree"
                 ;; The `vis-agent` wrapper runs these itself and never forwards them,
                 ;; so registering them here only advertised commands this runtime
                 ;; cannot execute.
                 (let [by-name
                       (into {} (map (juxt :cmd/name identity)) (registry/registered-under []))]
                   (doseq [nm ["runtime" "update" "desktop"]]
                     (expect (nil? (get by-name nm))))))
             (it "documents launcher-owned desktop and update commands in help"
                 (let [^String help (commandline/render-tree (#'main/root-command))]
                   (expect (str/includes? help "DESKTOP APP"))
                   (expect (str/includes? help "vis-agent desktop --update"))
                   (expect (str/includes? help "vis-agent desktop --no-gateway"))
                   (expect (str/includes? help "vis-agent desktop --track dev"))
                   (expect (str/includes? help "UPDATES"))
                   (expect (str/includes? help "vis-agent update"))
                   (expect (not (str/includes? help "vis-agent runtime"))))))

(defdescribe pretty-trace-form-output-test
             (it "prints a completed Python form's stdout"
                 (let [lines (atom [])]
                   (with-redefs [commandline/stdout! #(swap! lines conj %)]
                     (#'main/print-pretty-trace-chunk!
                      {:phase :form-result :form-idx 0 :form-of 1 :stdout "hello\n"}))
                   (let [text (str/join "\n" @lines)]
                     (expect (str/includes? text "stdout"))
                     (expect (str/includes? text "hello"))
                     (expect (not (str/includes? text "nil")))))))

(def ^:private city-schema
  {"type" "object"
   "required" ["city" "population_millions"]
   "properties" {"city" {"type" "string"} "population_millions" {"type" "number"}}
   "additionalProperties" false})

(defn- temp-json-file
  [text]
  (let [f (java.io.File/createTempFile "vis-json-schema" ".json")]
    (.deleteOnExit f)
    (spit f text)
    (.getPath f)))

;; Issue #344: one-shot runs need a schema-checked JSON answer for scripts.
(defdescribe
  json-schema-flag-parse-test
  (it "reads --json-schema as a value flag"
      (expect (= {:json-schema "{}" :json? true :prompt "task"}
                 (#'main/parse-run-args ["--json-schema" "{}" "--json" "task"])))
      (expect (= {:json-schema "@s.json" :prompt "task"}
                 (#'main/parse-run-args ["--json-schema" "@s.json" "task"]))))
  (it "refuses a missing schema value"
      (expect (= ["--json-schema needs a value (got --json)"]
                 (:flag-errors (#'main/parse-run-args ["--json-schema" "--json" "task"])))))
  (it "refuses --json-schema with --code"
      (expect (= ["--json-schema does not work with --code; use --json"]
                 (:flag-errors (#'main/check-run-conflicts
                                (#'main/parse-run-args ["--json-schema" "{}" "--code" "hi"])))))))

(defdescribe
  launch-options-test
  (let [launch (fn [args env]
                 (#'main/check-launch-options (#'main/parse-run-args args) env))]
    (it "keeps every tier and extension without flags or environment"
        (expect (= {:prompt "task"} (launch ["task"] {}))))
    (it "turns --no-global, --no-project and --repro into source lists"
        (expect (= ["project"] (:sources (launch ["--no-global" "task"] {}))))
        (expect (= ["global"] (:sources (launch ["--no-project" "task"] {}))))
        (expect (= {:sources [] :json-schema "{}" :prompt "task"}
                   (launch ["--repro" "--json-schema" "{}" "task"] {}))))
    (it "splits --extensions and reads none as an empty list"
        (expect (= ["gh" "clj"] (:extensions (launch ["--extensions" "gh, clj" "task"] {}))))
        (expect (= ["-spel"] (:extensions (launch ["--extensions" "-spel" "task"] {}))))
        (expect (= [] (:extensions (launch ["--extensions" "none" "task"] {})))))
    (it "still refuses --extensions followed by a long flag"
        (expect (= ["--extensions needs a value (got --json)"]
                   (:flag-errors (launch ["--extensions" "--json" "task"] {})))))
    (it "reads VIS_SOURCES and VIS_EXTENSIONS without a flag, and a flag wins"
        (expect (= {:sources ["project"] :extensions ["gh"] :prompt "task"}
                   (launch ["task"] {"VIS_SOURCES" "project" "VIS_EXTENSIONS" "gh"})))
        (expect (= ["global"]
                   (:sources (launch ["--no-project" "task"] {"VIS_SOURCES" "project"})))))
    (it "refuses an unknown tier"
        (expect (= ["Unknown configuration source: home. Use global or project."]
                   (:flag-errors (launch ["task"] {"VIS_SOURCES" "home"})))))
    (it "refuses the flags with --session-id and ignores the variables there"
        (expect (= 1 (count (:flag-errors (launch ["--session-id" "abc" "--repro" "task"] {})))))
        (expect (= {:session-id "abc" :persist? true :prompt "task"}
                   (launch ["--session-id" "abc" "task"] {"VIS_SOURCES" "project"}))))))

(defdescribe
  json-schema-arg-test
  (it "reads an inline schema and a schema file"
      (expect (= city-schema
                 (:schema (#'main/read-json-schema-arg (json/write-json-str city-schema)))))
      (expect (= city-schema
                 (:schema (#'main/read-json-schema-arg
                           (str "@" (temp-json-file (json/write-json-str city-schema)))))))
      (expect (some? (:compiled (#'main/read-json-schema-arg "{}")))))
  (it "names a missing file, a directory and an empty path"
      (expect (= "--json-schema file /nonexistent-vis/s.json does not exist"
                 (:error (#'main/read-json-schema-arg "@/nonexistent-vis/s.json"))))
      (expect (str/ends-with? (:error (#'main/read-json-schema-arg
                                       (str "@" (System/getProperty "java.io.tmpdir"))))
                              "is a directory"))
      (expect (= "--json-schema @ needs a file path, as in @schema.json"
                 (:error (#'main/read-json-schema-arg "@")))))
  (it "refuses text that is not exactly one JSON document"
      (doseq [text ["{" "" "nul" "{} {}" "{} trailing" "null 1"]]
        (expect (str/starts-with? (str (:error (#'main/read-json-schema-arg text)))
                                  "--json-schema is not valid JSON: ")
                text)))
  (it "refuses a schema that breaks the 2020-12 meta-schema"
      (let [{:keys [error schema-errors]} (#'main/read-json-schema-arg "{\"type\": 5}")]
        (expect (= "--json-schema is not a usable JSON Schema" error))
        (expect (some #(str/starts-with? % "/type: ") schema-errors)))
      (expect (= ["/: expected object or boolean, got array"]
                 (:schema-errors (#'main/read-json-schema-arg "[]"))))
      (expect (= ["/maxProperties: -1 is less than the minimum 0"]
                 (:schema-errors (#'main/read-json-schema-arg "{\"maxProperties\": -1}")))))
  (it "refuses the false schema, a bad pattern and a reference that cannot resolve"
      (expect (= ["/: the schema false accepts no answer"]
                 (:schema-errors (#'main/read-json-schema-arg "false"))))
      (expect (str/starts-with? (first (:schema-errors (#'main/read-json-schema-arg
                                                        "{\"pattern\": \"([\"}")))
                                "invalid regular expression"))
      (expect (= ["cannot resolve $ref \"#/$defs/x\" (only references inside the schema resolve)"]
                 (:schema-errors (#'main/read-json-schema-arg "{\"$ref\": \"#/$defs/x\"}"))))
      (expect (= 1
                 (count (:schema-errors (#'main/read-json-schema-arg
                                         "{\"$ref\": \"https://example.com/s.json\"}"))))))
  (it "resolves local references and ignores $ref text inside data"
      (expect (some? (:compiled
                       (#'main/read-json-schema-arg
                        "{\"$defs\": {\"x\": {\"type\": \"string\"}}, \"$ref\": \"#/$defs/x\"}"))))
      (expect (some? (:compiled
                       (#'main/read-json-schema-arg
                        (str "{\"$id\": \"https://x.test/root.json\","
                             " \"$defs\": {\"a\": {\"$id\": \"a.json\", \"type\": \"string\"}},"
                             " \"properties\": {\"p\": {\"$ref\": \"a.json\"}}}")))))
      (expect (some? (:compiled (#'main/read-json-schema-arg
                                 "{\"enum\": [{\"$ref\": \"#/nope\"}]}"))))))

(defdescribe
  structured-answer-test
  (let [compiled (:compiled (main/compile-json-schema city-schema))]
    (it "finds the JSON document in bare, fenced and surrounded answers"
        (doseq [text
                ["{\"city\":\"Warsaw\",\"population_millions\":1.86}"
                 "Here:\n```json\n{\"city\":\"Warsaw\",\"population_millions\":1.86}\n```\nDone."
                 "Sure: {\"city\":\"Warsaw\",\"population_millions\":1.86} - done"]]
          (expect (= {:value {"city" "Warsaw" "population_millions" 1.86}}
                     (main/structured-answer compiled text))
                  text)))
    (it
      "takes the first document that validates"
      (expect
        (=
          {:value {"city" "Warsaw" "population_millions" 2}}
          (main/structured-answer
            compiled
            "Example: {\"city\":1}\n```json\n{\"city\":\"Warsaw\",\"population_millions\":2}\n```"))))
    (it "names the validation errors of an answer that does not validate"
        (expect (= {:errors ["/: additional property \"x\" is not allowed"
                             "/population_millions: expected number, got string"]}
                   (main/structured-answer
                     compiled
                     "{\"city\":\"Warsaw\",\"population_millions\":\"1.86\",\"x\":1}"))))
    (it "reports an answer without JSON"
        (doseq [text ["no json here" "" "{\"city\":\"W\",\"population_millions\":NaN}"]]
          (expect (= {:errors ["the answer holds no JSON document"]}
                     (main/structured-answer compiled text))
                  text))))
  (it "accepts null and false as answer documents"
      (expect (= {:value nil}
                 (main/structured-answer (:compiled (main/compile-json-schema {"type" "null"}))
                                         "null")))
      (expect (= {:value false}
                 (main/structured-answer (:compiled (main/compile-json-schema {"type" "boolean"}))
                                         "false"))))
  (it "asserts format"
      (let [compiled (:compiled (main/compile-json-schema {"type" "string" "format" "date"}))]
        (expect (= {:value "2024-01-31"} (main/structured-answer compiled "\"2024-01-31\"")))
        (expect (= {:errors ["/: the string is not a valid date"]}
                   (main/structured-answer compiled "\"31.01.2024\""))))))

(defn- run-with-answers!
  "Run `main/run!` on a stubbed ephemeral engine. Each turn answers the next
   text of `answers`; returns the result and the messages of each turn."
  [answers opts]
  (let [turns
        (atom [])

        remaining
        (atom answers)]

    (with-redefs [loop-router/build-router
                  identity

                  loop-env/create-environment
                  (constantly {:stub true})

                  loop-env/dispose-environment!
                  (constantly nil)

                  turn/turn!
                  (fn [_env messages _opts]
                    (swap! turns conj messages)
                    (let [answer (first @remaining)]
                      (swap! remaining rest)
                      (if (map? answer)
                        answer
                        {:answer answer
                         :iteration-count 1
                         :duration-ms 10
                         :tokens {:input 100 :output 5}
                         :cost {"total_cost" 0.5 "model" "m"}
                         :trace [{:turn (count @turns)}]})))]

      {:result (main/run! {} "Name the capital of Poland." (merge {:config {:providers []}} opts))
       :turns @turns})))

(defdescribe
  json-schema-run-test
  (it "puts the schema into the one request message and returns the validated value"
      (let [{:keys [result turns]}
            (run-with-answers! ["{\"city\":\"Warsaw\",\"population_millions\":1.86}"]
                               {:json-schema city-schema})

            request
            (:content (last (first turns)))]

        (expect (= 1 (count turns)))
        (expect (= 1 (count (first turns))))
        (expect (str/starts-with? request "Name the capital of Poland."))
        (expect (str/includes? request (json/write-json-str city-schema)))
        (expect (= {"city" "Warsaw" "population_millions" 1.86} (:structured result)))
        (expect (= 1 (:structured-attempts result)))
        (expect (= 0 (#'main/cli-result-exit-code result)))))
  (it "asks again with the validation errors and adds up the spend"
      (let [{:keys [result turns]}
            (run-with-answers! ["The capital is Warsaw."
                                "{\"city\":\"Warsaw\",\"population_millions\":1.86}"]
                               {:json-schema city-schema})

            correction
            (:content (first (second turns)))]

        (expect (= 2 (count turns)))
        (expect (str/includes? correction "the answer holds no JSON document"))
        (expect (str/includes? correction (json/write-json-str city-schema)))
        (expect (= {"city" "Warsaw" "population_millions" 1.86} (:structured result)))
        (expect (= 2 (:structured-attempts result)))
        (expect (= 2 (:iteration-count result)))
        (expect (= {:input 200 :output 10} (:tokens result)))
        (expect (= {"total_cost" 1.0 "model" "m"} (:cost result)))
        (expect (= [{:turn 1} {:turn 2}] (:trace result)))))
  (it "fails with the schema errors after the last attempt"
      (let [{:keys [result turns]} (run-with-answers! ["{\"city\":1}" "{\"city\":2}" "{\"city\":3}"]
                                                      {:json-schema city-schema})]
        (expect (= 3 (count turns)))
        (expect (= "The answer does not match --json-schema after 3 attempts." (:error result)))
        (expect (= :error (:status result)))
        (expect (= ["/city: expected string, got integer"
                    "/: missing required property \"population_millions\""]
                   (:schema-errors result)))
        (expect (not (contains? result :structured)))
        (expect (= 1 (#'main/cli-result-exit-code result)))
        (let [envelope (json/read-json (main/result->json result))]
          (expect (= ["/city: expected string, got integer"
                      "/: missing required property \"population_millions\""]
                     (get envelope "schema-errors")))
          (expect (= 3 (get envelope "structured-attempts"))))))
  (it "stops without a retry when the turn itself fails"
      (let [{:keys [result turns]} (run-with-answers! [{:answer "Cancelled." :status :cancelled}]
                                                      {:json-schema city-schema})]
        (expect (= 1 (count turns)))
        (expect (= :cancelled (:status result)))
        (expect (not (contains? result :schema-errors)))))
  (it "keeps a valid null answer in the envelope"
      (let [{:keys [result]} (run-with-answers! ["null"] {:json-schema {"type" "null"}})]
        (expect (contains? result :structured))
        (expect (= {"structured" nil}
                   (select-keys (json/read-json (main/result->json result)) ["structured"])))))
  (it "refuses an unusable schema before any turn"
      (let [turns (atom 0)]
        (with-redefs [turn/turn! (fn [& _]
                                   (swap! turns inc)
                                   {:answer "x"})]
          (expect (throws? clojure.lang.ExceptionInfo
                           #(main/run! {} "hi" {:config {:providers []} :json-schema {"type" 5}}))))
        (expect (= 0 @turns))))
  (it "leaves runs without a schema unchanged"
      (let [{:keys [result turns]} (run-with-answers! ["plain answer"] {})]
        (expect (= "Name the capital of Poland." (:content (first (first turns)))))
        (expect (not (contains? result :structured))))))

(defdescribe
  json-schema-persistent-run-test
  (it
    "sends the schema request first and the correction as the next request"
    (let [requests
          (atom [])

          answers
          (atom [[{"type" "prose" "markdown" "Warsaw"}]
                 [{"type" "prose"
                   "markdown" "{\"city\":\"Warsaw\",\"population_millions\":1.86}"}]])]

      (with-redefs [loop-router/rebuild-router!
                    (constantly nil)

                    gateway-state/create-session!
                    (constantly {"id" "wire-session"})

                    gateway-state/submit-turn-sync!
                    (fn [_ request]
                      (swap! requests conj request)
                      (let [content (first @answers)]
                        (swap! answers rest)
                        {"content" content "status" "success"}))]

        (let [result (main/run! {}
                                "Name the capital of Poland."
                                {:config {:providers []} :persist? true :json-schema city-schema})]
          (expect (= {"city" "Warsaw" "population_millions" 1.86} (:structured result)))
          (expect (= "wire-session" (:session-id result)))
          (expect (= 2 (count @requests)))
          (expect (str/includes? (:request (first @requests)) (json/write-json-str city-schema)))
          (expect (str/starts-with? (:request (second @requests))
                                    "Your final answer does not validate"))
          (expect (= (:request (second @requests))
                     (:content (first (:messages (second @requests)))))))))))

(defdescribe
  json-schema-output-test
  (it "prints only the validated JSON document on stdout"
      (let [out
            (atom [])

            err
            (atom [])]

        (with-redefs [commandline/stdout!
                      #(swap! out conj %)

                      commandline/stderr!
                      #(swap! err conj %)]

          (#'main/print-structured-result! {:structured {"city" "Warsaw"} :content []}))
        (expect (= ["{\"city\":\"Warsaw\"}"] @out))
        (expect (= [] @err))))
  (it "sends a failed run to stderr and leaves stdout empty"
      (let [out
            (atom [])

            err
            (atom [])]

        (with-redefs [commandline/stdout!
                      #(swap! out conj %)

                      commandline/stderr!
                      #(swap! err conj %)]

          (#'main/print-structured-result!
           {:error "The answer does not match --json-schema after 3 attempts."
            :schema-errors ["/city: expected string, got integer"]}))
        (expect (= [] @out))
        (expect (= "  /city: expected string, got integer" (last @err)))))
  (it "reports an unusable schema as a JSON envelope with --json"
      (let [out (atom [])]
        (with-redefs [commandline/stdout! #(swap! out conj %)]
          (#'main/print-json-schema-error! true (#'main/read-json-schema-arg "{\"type\": 5}")))
        (let [envelope (json/read-json (first @out))]
          (expect (= "error" (get envelope "status")))
          (expect (= "--json-schema is not a usable JSON Schema" (get envelope "error")))
          (expect (seq (get envelope "schema-errors"))))))
  (it "reports an unusable schema on stderr without --json"
      (let [out
            (atom [])

            err
            (atom [])]

        (with-redefs [commandline/stdout!
                      #(swap! out conj %)

                      commandline/stderr!
                      #(swap! err conj %)]

          (#'main/print-json-schema-error! false (#'main/read-json-schema-arg "{")))
        (expect (= [] @out))
        (expect (str/starts-with? (first @err) "vis-agent: --json-schema is not valid JSON"))))
  ;; Issue #351: a failed turn has `:status :error` and error blocks, but no
  ;; `:error`. Stdout got only `null` and the reason was lost.
  (it "sends the error blocks of a failed turn to stderr, not null to stdout"
      (let [out
            (atom [])

            err
            (atom [])]

        (with-redefs [commandline/stdout!
                      #(swap! out conj %)

                      commandline/stderr!
                      #(swap! err conj %)]

          (#'main/print-structured-result!
           {:status :error
            :content [{"id" "block_1"
                       "type" "error"
                       "code" "model_not_found"
                       "message" "Model no-such-model-x is not available."
                       "retryable" false}]
            :structured-attempts 1}))
        (expect (= [] @out))
        (expect (= ["ERROR: Model no-such-model-x is not available."] @err))))
  (it "prints a valid JSON null answer on stdout"
      (let [out (atom [])]
        (with-redefs [commandline/stdout! #(swap! out conj %)]
          (#'main/print-structured-result! {:structured nil :content []}))
        (expect (= ["null"] @out)))))

(defn- run-cli
  "Run the one-shot CLI on `args`, with `result` as the turn result. Returns the
   stdout lines, the stderr lines and the exit code (nil for exit 0)."
  [args result]
  (let [out
        (atom [])

        err
        (atom [])

        exit
        (atom nil)]

    (with-redefs-fn {#'config/init-cli! (fn []
                                          nil)
                     #'config/load-config-raw (fn []
                                                {})
                     #'commandline/stdout! #(swap! out conj %)
                     #'commandline/stderr! #(swap! err conj %)
                     #'clojure.core/shutdown-agents (fn []
                                                      nil)
                     #'main/run! (fn [& _]
                                   result)
                     #'main/exit-process! (fn [code]
                                            (throw (ex-info "exit" {::exit code})))}
      #(try (#'main/cli-run! {} args)
            (catch clojure.lang.ExceptionInfo e
              (if (contains? (ex-data e) ::exit) (reset! exit (::exit (ex-data e))) (throw e)))))
    {:out @out :err @err :exit @exit}))

(def ^:private answer-schema
  "{\"type\":\"object\",\"properties\":{\"answer\":{\"type\":\"string\"}},\"required\":[\"answer\"]}")

(defdescribe
  one-shot-error-stream-test
  ;; Issue #355: output-mode conflicts printed the usage error on stdout.
  (it "writes every output-mode conflict to stderr with exit 2"
      (doseq [args [["--code" "--json-schema" answer-schema "--" "Say hi."] ["--code" "--json" "hi"]
                    ["--json" "--full-trace-stream" "hi"] ["--json" "--full-trace-json-stream" "hi"]
                    ["--code" "--trace" "hi"]
                    ["--full-trace-stream" "--full-trace-json-stream-raw" "hi"]
                    ["--modle" "x" "hi"]]]
        (let [{:keys [out err exit]} (run-cli args {:content []})]
          (expect (= [] out) (pr-str args))
          (expect (= 2 exit) (pr-str args))
          (expect (str/starts-with? (first err) "vis-agent: ") (pr-str args)))))
  ;; Issue #351: `--json-schema` alone printed `null` and exit 1 for a failed turn.
  (it "sends a failed --json-schema turn to stderr with exit 1"
      (let [{:keys [out err exit]}
            (run-cli ["--json-schema" answer-schema "--model" "no-such-model-x" "--" "hi"]
                     {:status :error
                      :content [{"id" "block_1"
                                 "type" "error"
                                 "code" "model_not_found"
                                 "message" "Model no-such-model-x is not available."
                                 "retryable" false}]})]
        (expect (= [] out))
        (expect (= 1 exit))
        (expect (= ["ERROR: Model no-such-model-x is not available."] err))))
  (it "fails a --json-schema turn that ends without a structured answer"
      (let [{:keys [out err exit]}
            (run-cli ["--json-schema" answer-schema "hi"]
                     {:status :needs-input
                      :content [{"id" "block_1" "type" "prose" "markdown" "Which city?"}]})]
        (expect (= [] out))
        (expect (= 1 exit))
        (expect (= ["Which city?"] err))))
  (it "keeps stdout for the structured answer only"
      (let [{:keys [out err exit]} (run-cli ["--json-schema" answer-schema "hi"]
                                            {:structured {"answer" "hi"} :content []})]
        (expect (= ["{\"answer\":\"hi\"}"] out))
        (expect (= [] err))
        (expect (nil? exit))))
  (it "sends a --code run without code blocks to stderr"
      (let [{:keys [out err exit]}
            (run-cli ["--code" "hi"]
                     {:content [{"id" "block_1" "type" "prose" "markdown" "No code."}]})]
        (expect (= [] out))
        (expect (= 1 exit))
        (expect (str/starts-with? (first err) "Error: --code expects"))))
  (it "sends a user error to stderr with exit 2"
      (let [err
            (atom [])

            out
            (atom [])

            exit
            (atom nil)]

        (with-redefs [commandline/stdout!
                      #(swap! out conj %)

                      commandline/stderr!
                      #(swap! err conj %)

                      shutdown-agents
                      (fn []
                        nil)

                      main/exit-process!
                      (fn [code]
                        (reset! exit code))]

          (#'main/exit-with-user-error!
           (ex-info "Invalid value for --toggles draft_backend: x" {:vis/user-error true})))
        (expect (= [] @out))
        (expect (= 2 @exit))
        (expect (= ["vis-agent: Invalid value for --toggles draft_backend: x"] @err)))))
