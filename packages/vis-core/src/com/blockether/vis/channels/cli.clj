(ns com.blockether.vis.channels.cli
  ;; CLI dispatcher.
  ;;
  ;; The whole `vis xxx` family is modeled as a TREE of commands using
  ;; `com.blockether.vis.commandline`. Built-in commands (`run`, `auth`,
  ;; `doctor`, `conversations`, `extensions`, `help`) live here; the
  ;; `vis ext <cmd>` and `vis channel <name>` parents have DYNAMIC
  ;; subcommands sourced from the extension and channel registries
  ;; respectively. vis-core has ZERO direct reference to any concrete
  ;; channel namespace — channels register themselves into
  ;; `com.blockether.vis.channel` at load time, and we just adapt the
  ;; registry into commandline command maps.
  (:require [clojure.string :as str]
            [com.blockether.vis.channel :as channel]
            [com.blockether.vis.channels.cli.agent :as agent]
            [com.blockether.vis.channels.core :as channels]
            [com.blockether.vis.commandline :as cmd]
            [com.blockether.vis.config :as config]
            [com.blockether.vis.loop.runtime.conversation.core :as conv-core]
            [com.blockether.vis.loop.runtime.conversation.environment.query.core :as query]
            [com.blockether.vis.persistance.core :as db]
            [honey.sql :as sql]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [taoensso.telemere :as tel]))

;;; ── Output helpers ──────────────────────────────────────────────────────

(defn- core-fn [sym]
  (requiring-resolve (symbol "com.blockether.vis.core" (name sym))))

(defn- stdout!
  "Print to the real terminal via the saved original stdout. Other
   output (telemere, SLF4J) is redirected to the log file."
  [^String s]
  (.println ^java.io.PrintStream config/original-stdout s)
  (.flush ^java.io.PrintStream config/original-stdout))

(defn- truncate-str [s max-len]
  (let [s (str s)]
    (if (> (count s) max-len)
      (str (subs s 0 (- max-len 1)) "…")
      s)))

(defn- format-date [d] (channels/format-date d))

(defn- print-table!
  "Print a formatted table to stdout!.
   `cols` is `[{:key :k :label \"L\" :width N :align :left|:right}]`."
  [cols rows]
  (let [pad   (fn [v {:keys [width align]}]
                (let [s (truncate-str (str v) width)]
                  (if (= align :right)
                    (cmd/pad-left s width)
                    (cmd/pad-right s width))))
        sep    (str "─" (str/join "─┼─"
                          (map #(apply str (repeat (:width %) \─)) cols)) "─")
        header (str " " (str/join " │ "
                          (map #(cmd/pad-right (:label %) (:width %)) cols)) " ")]
    (stdout! header)
    (stdout! sep)
    (doseq [row rows]
      (stdout! (str " " (str/join " │ "
                          (map #(pad (get row (:key %)) %) cols)) " ")))))

;;; ── `vis run` — handler + bespoke arg parser ────────────────────────────

(defn- parse-run-args
  "Parse `vis run` arguments into {:prompt str :json? bool …}.

   Bespoke instead of `vis-commandline/parse-args` because everything
   that ISN'T a known flag is glued together as the prompt body."
  [args]
  (loop [args         (seq args)
         opts         {}
         prompt-parts []]
    (if-not args
      (assoc opts :prompt (str/join " " prompt-parts))
      (let [arg  (first args)
            more (next args)]
        (case arg
          "--json"           (recur more (assoc opts :json? true) prompt-parts)
          "--edn"            (recur more (assoc opts :edn? true) prompt-parts)
          "--trace"          (recur more (assoc opts :trace? true) prompt-parts)
          ("--help" "-h")    (assoc opts :help? true :prompt "")
          "--debug"          (recur more (assoc opts :debug? true) prompt-parts)
          "--model"          (recur (next more) (assoc opts :model (first more)) prompt-parts)
          "--max-iterations" (recur (next more)
                               (assoc opts
                                 :max-iterations (parse-long (first more))
                                 :max-iterations-raw (first more)
                                 :max-iterations-provided? true)
                               prompt-parts)
          "--name"           (recur (next more) (assoc opts :agent-name (first more)) prompt-parts)
          "--db"             (recur (next more) (assoc opts :db (first more)) prompt-parts)
          (recur more opts (conj prompt-parts arg)))))))

(defn- print-run-usage! []
  (stdout! "Usage: vis run [FLAGS] \"prompt\"")
  (stdout! "")
  (stdout! "Flags:")
  (stdout! "  --json              Output result as JSON")
  (stdout! "  --edn               Output result as EDN")
  (stdout! "  --trace             Show full execution trace")
  (stdout! "  --debug             Enable debug logging")
  (stdout! "  --model MODEL       Override LLM model")
  (stdout! "  --max-iterations N  Override iteration budget (default 50, min 1)")
  (stdout! "  --name NAME         Agent name")
  (stdout! "  --db PATH|:memory   Override DB target for this command")
  (stdout! "")
  (stdout! "Examples:")
  (stdout! "  vis run \"What is 2+2?\"")
  (stdout! "  vis run --json --model gpt-4o \"Explain auth flow\"")
  (stdout! "  vis run --model gpt-4o --max-iterations 10 \"Explain auth flow\""))

(defn- validate-run-opts!
  [{:keys [max-iterations max-iterations-raw max-iterations-provided?]}]
  (when max-iterations-provided?
    (when-not (and (integer? max-iterations) (pos? max-iterations))
      (throw (ex-info "--max-iterations must be an integer >= 1"
               {:type :cli/invalid-arg
                :arg "--max-iterations"
                :value max-iterations-raw
                :parsed max-iterations})))))

(defn- cli-run!
  "`vis run` handler. `_parsed` is unused — we re-parse the residual
   ourselves so anything that isn't a flag falls into the prompt."
  [_parsed residual]
  (config/init-cli!)
  (let [{:keys [prompt json? edn? trace? help? agent-name db] :as opts}
        (parse-run-args residual)]
    (try
      (validate-run-opts! opts)
      (catch Exception e
        (stdout! (str "Validation error: " (ex-message e)))
        (print-run-usage!)
        (shutdown-agents)
        (System/exit 1)))
    (when (or help? (str/blank? prompt))
      (print-run-usage!)
      (System/exit 0))
    (let [agent-def (agent/agent {:name (or agent-name "cli")})
          run-opts  (cond-> (dissoc opts :prompt :json? :edn? :trace? :compact?
                              :agent-name :max-iterations-raw
                              :max-iterations-provided? :db)
                      db (assoc :db (config/resolve-db-spec
                                      (if (= db ":memory") :memory
                                        {:backend :sqlite :path db}))))
          result    (agent/run! agent-def prompt run-opts)]
      (cond
        json? (stdout! (agent/result->json result))
        edn?  (stdout! (agent/result->edn result))

        trace?
        (do (tel/log! {:level :info :id ::cli-trace
                       :data  (select-keys result [:answer :trace :iterations
                                                   :duration-ms :tokens :cost
                                                   :error :type])}
              "CLI trace result")
          (stdout! (str (:answer result)))
          (when (:error result)
            (when-let [ex (:exception result)]
              (stdout! "\nStack trace:")
              (.printStackTrace ^Throwable ex config/original-stdout))
            (shutdown-agents)
            (System/exit 1)))

        (:error result)
        (do (stdout! (str "Error: " (:error result)))
          (shutdown-agents)
          (System/exit 1))

        :else
        (do (stdout! (str (:answer result)))
          (when (:duration-ms result)
            (let [tokens  (:tokens result)
                  ctx-in  (some-> tokens :input)
                  ctx-out (some-> tokens :output)
                  cost    (some-> result :cost :total-cost)]
              (stdout! (str "\n["
                         (:iterations result) " iterations"
                         (when ctx-in  (str ", ctx-in: "  ctx-in))
                         (when ctx-out (str ", ctx-out: " ctx-out))
                         (when cost
                           (str ", ~$"
                             (String/format java.util.Locale/US "%.6f"
                               (into-array Object [(double cost)]))))
                         ", " (:duration-ms result) "ms"
                         "]"))))))
      (shutdown-agents))))

;;; ── `vis conversations` ─────────────────────────────────────────────────

(defn- cli-conversations! [_parsed residual]
  (config/init-cli!)
  (let [channel (or (some #{"vis" "telegram" "cli"} residual) "vis")
        ch-kw   (keyword channel)
        convs   (conv-core/by-channel ch-kw)
        d       (conv-core/db-info)]
    (if (empty? convs)
      (stdout! (str "No " channel " conversations found."))
      (let [rows (mapv (fn [c]
                         (let [queries (db/db-list-conversation-queries d (:id c))
                               turns   (count queries)
                               last-q  (last queries)]
                           {:id        (str (:id c))
                            :title     (or (:title c) "—")
                            :turns     turns
                            :last-turn (or (some-> last-q :created-at format-date) "—")
                            :created   (or (format-date (:created-at c)) "—")}))
                   convs)]
        (stdout! (str "\n  " (str/upper-case channel) " Conversations\n"))
        (print-table!
          [{:key :id        :label "ID"        :width 36 :align :left}
           {:key :title     :label "Title"     :width 24 :align :left}
           {:key :turns     :label "Turns"     :width 5  :align :right}
           {:key :last-turn :label "Last Turn" :width 16 :align :left}
           {:key :created   :label "Created"   :width 16 :align :left}]
          rows)
        (stdout! (str "\n  " (count rows) " conversation(s)\n"))
        (stdout! "  Resume with: vis channel tui --conversation-id <ID>  (full or short)")
        (stdout! "  Or latest:   vis channel tui --resume"))))
  (shutdown-agents))

;;; ── `vis auth` ──────────────────────────────────────────────────────────

(defn- cli-auth! [_parsed residual]
  (config/init-cli!)
  (let [provider (first residual)
        flags    (set (rest residual))]
    (case provider
      "github-copilot"
      (let [copilot-status   (requiring-resolve 'com.blockether.vis.providers.github-copilot/status)
            copilot-logout   (requiring-resolve 'com.blockether.vis.providers.github-copilot/logout!)
            copilot-detect   (requiring-resolve 'com.blockether.vis.providers.github-copilot/detect-oauth-token)
            copilot-start    (requiring-resolve 'com.blockether.vis.providers.github-copilot/start-device-flow!)
            copilot-poll     (requiring-resolve 'com.blockether.vis.providers.github-copilot/poll-for-token!)
            copilot-exchange (requiring-resolve 'com.blockether.vis.providers.github-copilot/get-copilot-token!)]
        (cond
          (contains? flags "--status")
          (let [s (copilot-status)]
            (stdout! "\n  GitHub Copilot Auth Status")
            (stdout! "  ───────────────────────────")
            (if (:authenticated? s)
              (do (stdout! "  Authenticated: yes")
                (stdout! (str "  Source:        " (name (:source s))))
                (stdout! (str "  Token:         " (:oauth-token-preview s)))
                (when (contains? s :copilot-token-valid?)
                  (stdout! (str "  API token:     "
                             (if (:copilot-token-valid? s) "valid" "expired")))
                  (when (:copilot-token-valid? s)
                    (stdout! (str "  Expires in:    "
                               (int (/ (:expires-in-ms s) 60000)) " min")))))
              (stdout! "  Authenticated: no"))
            (stdout! ""))

          (contains? flags "--logout")
          (do (copilot-logout)
            (stdout! "  Logged out of GitHub Copilot. Tokens cleared."))

          :else
          (if (copilot-detect)
            (do (stdout! "  Already authenticated with GitHub Copilot.")
              (stdout! "  Run `vis auth github-copilot --status` for details.")
              (stdout! "  Run `vis auth github-copilot --logout` first to re-authenticate."))
            (do (stdout! "\n  GitHub Copilot — OAuth Device Flow")
              (stdout! "  ───────────────────────────────────")
              (let [{:keys [user-code verification-uri device-code interval expires-in]}
                    (copilot-start)]
                (stdout! "")
                (stdout! (str "  1. Open: " verification-uri))
                (stdout! (str "  2. Enter code: " user-code))
                (stdout! "")
                (stdout! "  Waiting for authorization...")
                (.flush ^java.io.PrintStream config/original-stdout)
                (try
                  (copilot-poll device-code interval expires-in)
                  (copilot-exchange)
                  (stdout! "  ✓ Authenticated! GitHub Copilot is ready.")
                  (catch Exception e
                    (stdout! (str "  ✗ Authentication failed: " (ex-message e))))))))))

      ;; Unknown provider
      (do (stdout! (str "Unknown auth provider: " (or provider "<none>")))
        (stdout! "Available: github-copilot"))))
  (shutdown-agents))

;;; ── `vis doctor` ────────────────────────────────────────────────────────

(defn- cli-doctor! [_parsed _residual]
  (config/init-cli!)
  (let [env ((core-fn 'create-environment) (query/get-router)
             {:db (config/resolve-db-spec)})]
    (try
      (let [db-info (:db-info env)]
        (stdout! "vis doctor")
        (stdout! "")
        (stdout! "  Environment")
        (stdout! "  ───────────")
        (stdout! (str "  OS:           " (System/getProperty "os.name") " "
                   (System/getProperty "os.arch")))
        (stdout! (str "  Java:         " (System/getProperty "java.version")
                   " (" (System/getProperty "java.vendor") ")"))
        (stdout! (str "  Clojure:      " (clojure-version)))
        (stdout! (str "  Memory:       "
                   (let [rt   (Runtime/getRuntime)
                         used (- (.totalMemory rt) (.freeMemory rt))
                         max  (.maxMemory rt)
                         mb   (fn [b] (format "%.0fMB" (/ (double b) 1048576)))]
                     (str (mb used) " / " (mb max)))))
        (stdout! (str "  DB path:      " (or (:path db-info) "none")))
        (let [active-envs (count @conv-core/cache)
              sidecar-ds  (some-> db-info :datasource)
              count-ch    (fn [ch]
                            (try
                              (when sidecar-ds
                                (count (jdbc/execute! sidecar-ds
                                         (sql/format {:select [:*]
                                                      :from   [:conversation]
                                                      :where  [:= :channel (name ch)]})
                                         {:builder-fn rs/as-unqualified-lower-maps})))
                              (catch Exception _ 0)))
              vis-n  (or (count-ch :vis) 0)
              cli-n  (or (count-ch :cli) 0)
              tg-n   (or (count-ch :telegram) 0)
              total  (+ vis-n cli-n tg-n)]
          (stdout! (str "  Conversations:  " total
                     " (" vis-n " vis, " cli-n " cli, " tg-n " telegram)"
                     " — " active-envs " active in memory"))))
      (finally
        ((core-fn 'dispose-environment!) env)
        (shutdown-agents)))))

;;; ── `vis extensions` ────────────────────────────────────────────────────

(defn- cli-extensions! [_parsed _residual]
  (config/init-cli!)
  (let [exts (channels/list-extensions)]
    (if (empty? exts)
      (stdout! "No extensions registered.")
      (do (stdout! "\n  Extensions\n")
        (print-table!
          [{:key :namespace :label "Namespace"   :width 24 :align :left}
           {:key :doc       :label "Description" :width 40 :align :left}
           {:key :group     :label "Group"       :width 14 :align :left}
           {:key :version   :label "Version"     :width 10 :align :left}
           {:key :cli-cmds  :label "CLI"         :width 20 :align :left}]
          exts)
        (stdout! (str "\n  " (count exts) " extension(s)\n")))))
  (shutdown-agents))

;;; ── Dynamic subcommand builders ────────────────────────────────────────

(defn- channel->command
  "Adapt a `:channel/…`-keyed channel descriptor into a vis-commandline
   command map. Channels parse their own raw args so we forward the
   residual untouched and ignore the parsed map."
  [c]
  {:cmd/name      (:channel/cmd c)
   :cmd/doc       (:channel/doc c)
   :cmd/usage     (or (:channel/usage c)
                    (str "vis channel " (:channel/cmd c)))
   :cmd/owns-tty? (boolean (:channel/owns-tty? c))
   :cmd/run-fn    (fn [_parsed residual]
                    ((:channel/main-fn c) (vec residual)))})

(defn- channel-subcommands
  "Snapshot the channel registry AND any commandline plug-in commands
   that asked to mount under `vis channel` (`:cmd/parent [\"channel\"]`).
   Called on every `dispatch!`/`render-help` walk so newly registered
   channels appear immediately."
  []
  (let [chs   (mapv channel->command (channel/registered-channels))
        regd  (cmd/registered-under ["channel"])
        ;; Channel-registry entries take precedence on name collision
        ;; (channels are first-class; commandline mounts under `channel`
        ;; are an escape hatch for non-channel adapters).
        names (set (map :cmd/name chs))]
    (vec (sort-by :cmd/name (concat chs (remove #(names (:cmd/name %)) regd))))))

(defn- ext->command
  "Adapt one extension `:ext/cli` entry into a vis-commandline command.
   `entry` shape is `{:cmd :doc :args :fn :ext-ns}` as produced by
   `channels.core/all-extension-cmds`."
  [{cmd-name :cmd, cmd-doc :doc, cmd-args :args, runner :fn, ext-ns :ext-ns}]
  {:cmd/name cmd-name
   :cmd/doc  (str (or cmd-doc "")
              (when ext-ns (str "  (" ext-ns ")")))
   :cmd/args (vec (or cmd-args []))
   :cmd/run-fn (fn [parsed _residual]
                 (config/init-cli!)
                 (let [r (try (runner parsed)
                           (catch Throwable t
                             (stdout! (str "Error: " (ex-message t)))
                             (System/exit 1)))]
                   (when (some? r) (stdout! (str r)))
                   (shutdown-agents)))})

(defn- ext-subcommands
  "Snapshot every extension's `:ext/cli` entries AND any commandline
   plug-in commands registered with `:cmd/parent [\"ext\"]`. Called on
   every dispatch/help walk so freshly loaded plug-ins appear without
   restart."
  []
  (let [legacy (mapv ext->command (channels/all-extension-cmds))
        regd   (cmd/registered-under ["ext"])
        names  (set (map :cmd/name legacy))]
    (vec (sort-by :cmd/name (concat legacy (remove #(names (:cmd/name %)) regd))))))

;;; ── Root command tree ──────────────────────────────────────────────────

(def ^:private builtin-subcommands
  "The fixed top-level commands that ship with vis-core itself. Plug-in
   jars can add more by calling `commandline/register-global!` with
   no `:cmd/parent` (or with `:cmd/parent []`) — those land alongside
   these via `top-level-subcommands`."
  [{:cmd/name  "run"
       :cmd/doc   "Run a one-shot agent query and print the answer."
       :cmd/usage "vis run [FLAGS] \"prompt\""
       :cmd/args  [{:name "json"           :kind :flag :type :boolean :doc "Output result as JSON."}
                   {:name "edn"            :kind :flag :type :boolean :doc "Output result as EDN."}
                   {:name "trace"          :kind :flag :type :boolean :doc "Show full execution trace."}
                   {:name "debug"          :kind :flag :type :boolean :doc "Enable svar debug logging."}
                   {:name "model"          :kind :flag :type :string  :doc "Override the LLM model."}
                   {:name "max-iterations" :kind :flag :type :int     :doc "Iteration budget (default 50, min 1)."}
                   {:name "name"           :kind :flag :type :string  :doc "Agent name."}
                   {:name "db"             :kind :flag :type :string  :doc "DB target: PATH or :memory."}]
       :cmd/examples ["vis run \"What is 2+2?\""
                      "vis run --json --model gpt-4o \"Explain the auth flow\""
                      "vis run --max-iterations 10 \"Refactor src/foo.clj\""]
       :cmd/run-fn cli-run!}

      {:cmd/name  "auth"
       :cmd/doc   "Authenticate with an LLM provider."
       :cmd/usage "vis auth <provider> [--status | --logout]"
       :cmd/examples ["vis auth github-copilot"
                      "vis auth github-copilot --status"
                      "vis auth github-copilot --logout"]
       :cmd/run-fn cli-auth!}

      {:cmd/name  "conversations"
       :cmd/doc   "List conversations stored on disk."
       :cmd/usage "vis conversations [vis|telegram|cli]"
       :cmd/examples ["vis conversations"
                      "vis conversations telegram"]
       :cmd/run-fn cli-conversations!}

      {:cmd/name  "doctor"
       :cmd/doc   "Show environment + DB diagnostics."
       :cmd/usage "vis doctor"
       :cmd/run-fn cli-doctor!}

      {:cmd/name  "extensions"
       :cmd/doc   "List every registered extension."
       :cmd/usage "vis extensions"
       :cmd/run-fn cli-extensions!}

      {:cmd/name  "ext"
       :cmd/doc   "Run an extension-provided CLI command."
       :cmd/usage "vis ext <cmd> [args…]"
       :cmd/subcommands ext-subcommands}

      {:cmd/name  "channel"
       :cmd/doc   "Run a registered channel (TUI, Telegram, …)."
       :cmd/usage "vis channel <name> [args…]"
       :cmd/subcommands channel-subcommands}])

(defn- top-level-subcommands
  "Built-in vis-core commands + every plug-in command registered with
   `:cmd/parent []` (or no parent at all). Built-ins win on name
   collision so a stray plug-in can't hijack `run` / `auth` / etc."
  []
  (let [regd  (cmd/registered-under [])
        names (set (map :cmd/name builtin-subcommands))]
    (vec (concat builtin-subcommands
           (remove #(names (:cmd/name %)) regd)))))

(defn- root-command
  "Build the root `vis` command tree on every call so newly registered
   plug-ins show up without restart. Cheap — it's a tiny map; the heavy
   lifting is in the dynamic `:cmd/subcommands` fns."
  []
  (cmd/command
    {:cmd/name "vis"
     :cmd/doc  (str/join "\n"
                 ["vis — SCI powered RLM iterative coding harness"
                  ""
                  "  No tool calls. No message accumulation. No compaction."
                  "  The model writes Clojure. A sandboxed SCI interpreter executes it."
                  "  Results flow back as a compact journal. State lives in named vars"
                  "  and SQLite, not in the token budget. One context message per"
                  "  iteration — constant size, never grows. Works with any model that"
                  "  outputs JSON."
                  ""
                  "  Run `vis run --help` for one-shot options."
                  "  Docs: cd docs && mdbook serve --open"])
     :cmd/subcommands top-level-subcommands}))

;;; ── Main ───────────────────────────────────────────────────────────────

(defn- pre-redirect-stderr!
  "Walk the command tree to see whether the resolved leaf owns the
   controlling terminal. If so, redirect stderr to ~/.vis/vis.log
   BEFORE any further class loading triggers JVM warnings (e.g. the
   sun.misc.Unsafe deprecation from aircompressor)."
  [args]
  (when-let [{:keys [command]} (cmd/find-leaf (root-command) (cons "vis" args))]
    (when (:cmd/owns-tty? command)
      (let [log-dir (java.io.File. (str (System/getProperty "user.home") "/.vis"))]
        (when-not (.exists log-dir) (.mkdirs log-dir))
        (System/setErr (java.io.PrintStream.
                         (java.io.FileOutputStream.
                           (str log-dir "/vis.log") true) true))))))

(defn -main [& args]
  ;; Discover plug-ins FIRST so dispatch + help see the full set:
  ;;   - channels register themselves into the channel registry
  ;;   - commandline plug-ins (top-level, ext-mounted, channel-mounted)
  ;;     register into `commandline/global-registry`
  (channel/discover-channels!)
  (cmd/discover-commands!)
  (pre-redirect-stderr! args)
  (let [root      (root-command)
        full-args (cons "vis" args)
        leaf      (cmd/find-leaf root full-args)
        unknown?  (and (seq args)
                    leaf
                    (= (:command leaf) root)
                    (not-any? #{"help" "--help" "-h"} args))]
    (cond
      ;; No args at all → top-level help (the same render the help
      ;; command would produce).
      (empty? args)
      (println (cmd/render-tree root))

      ;; `vis "what is 2+2?"` — first arg is not a command. Fall back
      ;; to `vis run` for ergonomic one-liners.
      unknown?
      (cli-run! {} (vec args))

      ;; Otherwise: dispatch through the tree. `dispatch!` handles
      ;; `--help`, parent-without-runner help rendering, arg parsing,
      ;; and the actual leaf invocation.
      :else
      (cmd/dispatch! root full-args))))
