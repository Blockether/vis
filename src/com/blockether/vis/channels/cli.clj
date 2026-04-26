(ns com.blockether.vis.channels.cli
  (:require [clojure.string :as str]
            [com.blockether.vis.channels.cli.agent :as agent]
            [com.blockether.vis.config :as config]
            [com.blockether.vis.channels.telegram.bot :as telegram]
            [com.blockether.vis.loop.runtime.conversation.core :as conv-core]
            [com.blockether.vis.channels.core :as channels]
            [com.blockether.vis.channels.tui.primitives :as p]
            [com.blockether.vis.persistance.core :as db]
            [honey.sql :as sql]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [com.blockether.vis.loop.runtime.conversation.environment.query.core :as query]
            [taoensso.telemere :as tel]))

;;; ── CLI Output ──────────────────────────────────────────────────────────

(defn- core-fn [sym]
  (requiring-resolve (symbol "com.blockether.vis.core" (name sym))))

(defn- stdout!
  "Print to the real terminal via the saved original stdout.
   All other output (telemere, SLF4J) is redirected to log file."
  [^String s]
  (.println ^java.io.PrintStream config/original-stdout s)
  (.flush ^java.io.PrintStream config/original-stdout))

;;; ── Argument Parsing ────────────────────────────────────────────────────

(defn- parse-run-args
  "Parse `vis run` arguments into {:prompt str, :json? bool, ...}.

   Flags:
     --json               output JSON
     --edn                output EDN
     --trace              show full execution trace
     --debug              enable svar debug logging
     --model MODEL        override LLM model
     --max-iterations N   override iteration budget
     --name NAME          agent name
     --db PATH|:memory    override DB target for this command

   Everything else is concatenated as the prompt."
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
          ;; Not a flag → part of the prompt
          (recur more opts (conj prompt-parts arg)))))))

;;; ── Help ────────────────────────────────────────────────────────────────

(defn- print-help! []
  (println "vis — SCI powered RLM iterative coding harness")
  (println)
  (println "  No tool calls. No message accumulation. No compaction.")
  (println "  The model writes Clojure. A sandboxed SCI interpreter executes it.")
  (println "  Results flow back as a compact journal. State lives in named vars")
  (println "  and SQLite, not in the token budget. One context message per")
  (println "  iteration — constant size, never grows. Works with any model that")
  (println "  outputs JSON.")
  (println)
  (println "Commands:")
  (println "  vis                                   Start new TUI chat")
  (println "  vis chat                              Start new TUI chat")
  (println "  vis chat --conversation-id ID          Resume a specific conversation")
  (println "  vis chat --resume                      Resume the latest conversation")
  (println "  vis conversations                     List all conversations")
  (println "  vis conversations telegram             List telegram conversations")
  (println "  vis run \"prompt\"                      Run a one-shot agent query")
  (println "  vis web [PORT]                        Start web server (default port 3000)")
  (println "  vis telegram                          Run as a Telegram bot (needs TELEGRAM_BOT_TOKEN)")
  (println "  vis extensions                        List registered extensions")
  (println "  vis ext <cmd> [args...]                Run an extension command")
  (println "  vis ext help                           Show extension commands")
  (println "  vis auth github-copilot               Authenticate with GitHub Copilot (OAuth device flow)")
  (println "  vis auth github-copilot --status       Show Copilot auth status")
  (println "  vis auth github-copilot --logout       Clear Copilot tokens")
  (println "  vis doctor                            Show environment diagnostics")
  (println "  vis help                              Show this help")
  (println)
  (println "Run `vis run --help` for options.")
  (println "Docs: cd resources/docs && mdbook serve --open")
  ;; Show extension commands if any are registered
  (let [cmds (channels/all-extension-cmds)]
    (when (seq cmds)
      (println)
      (println (channels/extension-help)))))

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

;;; ── Run Command ─────────────────────────────────────────────────────────

(defn- validate-run-opts!
  [{:keys [max-iterations max-iterations-raw max-iterations-provided?]}]
  (when max-iterations-provided?
    (when-not (and (integer? max-iterations) (pos? max-iterations))
      (throw (ex-info "--max-iterations must be an integer >= 1"
               {:type :cli/invalid-arg
                :arg "--max-iterations"
                :value max-iterations-raw
                :parsed max-iterations}))))
  true)

(defn- cli-run!
  "Entry point for `vis run`. Parses args, runs agent, prints result."
  [args]
  (let [{:keys [prompt json? edn? trace? help? agent-name db] :as opts} (parse-run-args args)]
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
    (let [agent-def  (agent/agent {:name (or agent-name "cli")})
          run-opts   (cond-> (dissoc opts :prompt :json? :edn? :trace? :compact? :agent-name
                                :max-iterations-raw :max-iterations-provided? :db)
                       db (assoc :db (config/resolve-db-spec (if (= db ":memory") :memory {:backend :sqlite :path db}))))
          result     (agent/run! agent-def prompt run-opts)]
      (cond
        json?
        (stdout! (agent/result->json result))

        edn?
        (stdout! (agent/result->edn result))

        ;; --trace: log full result via telemere
        trace?
        (do (tel/log! {:level :info :id ::cli-trace
                         :data (select-keys result [:answer :trace :iterations
                                                    :duration-ms :tokens :cost
                                                    :error :type])
                        }
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
            (let [tokens (:tokens result)
                  ctx-in  (some-> tokens :input)
                  ctx-out (some-> tokens :output)
                  cost    (some-> result :cost :total-cost)]
              (stdout! (str "\n["
                         (:iterations result) " iterations"
                         (when ctx-in  (str ", ctx-in: "  ctx-in))
                         (when ctx-out (str ", ctx-out: " ctx-out))
                         (when cost
                           (str ", ~$" (String/format java.util.Locale/US "%.6f" (into-array Object [(double cost)]))))
                         ", " (:duration-ms result) "ms"
                         "]"))))))
      (shutdown-agents))))

;;; ── CLI Presentation Helpers ───────────────────────────────────────────────
;;
;; String padding/truncation reuses com.blockether.vis.channels.tui.primitives
;; (pure functions, no Lanterna dependency at call site).

(defn- truncate-str [s max-len]
  (let [s (str s)]
    (if (> (count s) max-len)
      (str (subs s 0 (- max-len 1)) "…")
      s)))

(defn- format-date [d]
  (channels/format-date d))

(defn- print-table!
  "Print a formatted table to stdout!.
   `cols` is [{:key :k :label \"L\" :width N :align :left|:right}].
   `rows` is a seq of maps."
  [cols rows]
  (let [pad-r  (fn [s w] (p/pad-right (str s) w))
        pad-l  (fn [s w] (p/pad-left (str s) w))
        pad    (fn [v {:keys [width align]}]
                 (let [s (truncate-str (str v) width)]
                   (if (= align :right) (pad-l s width) (pad-r s width))))
        sep    (str "─" (str/join "─┼─" (map #(apply str (repeat (:width %) \─)) cols)) "─")
        header (str " " (str/join " │ " (map #(pad-r (:label %) (:width %)) cols)) " ")]
    (stdout! header)
    (stdout! sep)
    (doseq [row rows]
      (stdout! (str " " (str/join " │ " (map #(pad (get row (:key %)) %) cols)) " ")))))

;;; ── Conversations Command ─────────────────────────────────────────────────

(defn- cli-conversations!
  "List all conversations in a formatted table."
  [args]
  (let [channel (or (some #{"vis" "telegram" "cli"} args) "vis")
        ch-kw   (keyword channel)
        convs   (conv-core/by-channel ch-kw)
        d       (conv-core/db-info)]
    (if (empty? convs)
      (stdout! (str "No " channel " conversations found."))
      (let [rows (mapv (fn [c]
                         (let [queries (db/db-list-conversation-queries
                                         d (:id c))
                               turns   (count queries)
                               last-q  (last queries)]
                           {:id         (str (:id c))
                            :title      (or (:title c) "—")
                            :turns      turns
                            :last-turn  (or (some-> last-q :created-at format-date) "—")
                            :created    (or (format-date (:created-at c)) "—")}))
                   convs)]
        (stdout! (str "\n  " (str/upper-case channel) " Conversations\n"))
        (print-table!
          [{:key :id        :label "ID"         :width 36 :align :left}
           {:key :title     :label "Title"      :width 24 :align :left}
           {:key :turns     :label "Turns"      :width 5  :align :right}
           {:key :last-turn :label "Last Turn"   :width 16 :align :left}
           {:key :created   :label "Created"    :width 16 :align :left}]
          rows)
        (stdout! (str "\n  " (count rows) " conversation(s)\n"))
        (stdout! "  Resume with: vis chat --conversation-id <ID>")
        (stdout! "  Or latest:   vis chat --resume")))))

;;; ── Chat Argument Parsing ─────────────────────────────────────────────────

(defn- parse-chat-args
  "Parse `vis chat` arguments.
   Flags:
     --conversation-id ID   Resume a specific conversation
     --resume               Resume the latest :vis conversation"
  [args]
  (loop [args (seq args) opts {}]
    (if-not args
      opts
      (let [arg  (first args)
            more (next args)]
        (case arg
          "--conversation-id" (recur (next more) (assoc opts :conversation-id (first more)))
          "--resume"          (recur more (assoc opts :resume true))
          ;; skip unknown
          (recur more opts))))))

;;; ── Auth ────────────────────────────────────────────────────────────────

(defn- cli-auth!
  "Handle `vis auth <provider> [flags]`."
  [args]
  (let [provider (first args)
        flags    (set (rest args))]
    (case provider
      "github-copilot"
      (let [copilot-status (requiring-resolve 'com.blockether.vis.providers.github-copilot/status)
            copilot-logout (requiring-resolve 'com.blockether.vis.providers.github-copilot/logout!)
            copilot-detect (requiring-resolve 'com.blockether.vis.providers.github-copilot/detect-oauth-token)
            copilot-start  (requiring-resolve 'com.blockether.vis.providers.github-copilot/start-device-flow!)
            copilot-poll   (requiring-resolve 'com.blockether.vis.providers.github-copilot/poll-for-token!)
            copilot-exchange (requiring-resolve 'com.blockether.vis.providers.github-copilot/get-copilot-token!)]
        (cond
          (contains? flags "--status")
          (let [s (copilot-status)]
            (stdout! "\n  GitHub Copilot Auth Status")
            (stdout! "  ───────────────────────────")
            (if (:authenticated? s)
              (do
                (stdout! "  Authenticated: yes")
                (stdout! (str "  Source:        " (name (:source s))))
                (stdout! (str "  Token:         " (:oauth-token-preview s)))
                (when (contains? s :copilot-token-valid?)
                  (stdout! (str "  API token:     " (if (:copilot-token-valid? s) "valid" "expired")))
                  (when (:copilot-token-valid? s)
                    (stdout! (str "  Expires in:    " (int (/ (:expires-in-ms s) 60000)) " min")))))
              (stdout! "  Authenticated: no"))
            (stdout! ""))

          (contains? flags "--logout")
          (do (copilot-logout)
            (stdout! "  Logged out of GitHub Copilot. Tokens cleared."))

          :else
          ;; Interactive device flow
          (if (copilot-detect)
            (do
              (stdout! "  Already authenticated with GitHub Copilot.")
              (stdout! "  Run `vis auth github-copilot --status` for details.")
              (stdout! "  Run `vis auth github-copilot --logout` first to re-authenticate."))
            (do
              (stdout! "\n  GitHub Copilot — OAuth Device Flow")
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
                  (stdout! "")
                  (stdout! "  Add to ~/.vis/config.edn:")
                  (stdout! "    {:providers [{:id :github-copilot")
                  (stdout! "                  :models [{:name \"gpt-4o\"} {:name \"claude-sonnet-4-20250514\"}]}]}")
                  (stdout! "")
                  (stdout! "  The OAuth token is persisted in ~/.vis/github-copilot-auth.json")
                  (stdout! "  and Copilot API tokens are refreshed automatically.")
                  (catch Exception e
                    (stdout! (str "  ✗ Authentication failed: " (ex-message e))))))))))

      ;; Unknown provider
      (do
        (stdout! (str "Unknown auth provider: " (or provider "<none>")))
        (stdout! "Available: github-copilot")))))

;;; ── TUI ─────────────────────────────────────────────────────────────────

(defn- run-tui!
  "Start TUI chat with logging redirected to file (stdout reserved for Lanterna)."
  [opts]
  ;; Redirect stderr immediately — before any requires — so JVM warnings
  ;; (e.g. sun.misc.Unsafe deprecation from aircompressor) don't bleed
  ;; into the Lanterna TUI.
  (let [log-dir (java.io.File. (str (System/getProperty "user.home") "/.vis"))]
    (when-not (.exists log-dir) (.mkdirs log-dir))
    (System/setErr (java.io.PrintStream.
                     (java.io.FileOutputStream.
                       (str log-dir "/vis.log") true) true)))
  (config/init!)
  (try
    ((requiring-resolve 'com.blockether.vis.channels.tui.screen/run-chat!) opts)
    (catch Throwable t
      (.println config/original-stdout (str "vis: fatal error — " (.getMessage t)))
      (.printStackTrace t (java.io.PrintStream. @config/tty-out true))
      (throw t))
    (finally
      (config/shutdown!))))

;;; ── Main ────────────────────────────────────────────────────────────────

(defn -main [& args]
  ;; Redirect stderr immediately for TUI paths — before any class loading
  ;; triggers JVM warnings (e.g. sun.misc.Unsafe deprecation from aircompressor).
  (let [cmd (first args)]
    (when (or (nil? cmd) (= cmd "chat"))
      (let [log-dir (java.io.File. (str (System/getProperty "user.home") "/.vis"))]
        (when-not (.exists log-dir) (.mkdirs log-dir))
        (System/setErr (java.io.PrintStream.
                         (java.io.FileOutputStream.
                           (str log-dir "/vis.log") true) true))))
    (cond
      ;; Explicit run — stdout stays connected, logs go to file only
      (= cmd "run")
      (do (config/init-cli!)
        (cli-run! (rest args)))

      ;; Help
      (#{"help" "--help" "-h"} cmd)
      (print-help!)

      ;; Web server
      (= cmd "web")
      (do (config/init-cli!)
        (require 'com.blockether.vis.channels.web.app)
        (apply (resolve 'com.blockether.vis.channels.web.app/-main) (rest args)))

      ;; Telegram bot — stdout stays connected for startup logs
      (= cmd "telegram")
      (do (config/init-cli!)
        (telegram/-main))

      ;; Auth commands
      (= cmd "auth")
      (do (config/init-cli!)
        (cli-auth! (rest args))
        (shutdown-agents))

      ;; Doctor — environment diagnostics
      (= cmd "doctor")
      (do (config/init-cli!)
        (let [env ((core-fn 'create-environment) (query/get-router) {:db (config/resolve-db-spec)})]
          (try
            (let [db-info (:db-info env)]
              (stdout! "vis doctor")
              (stdout! "")
              (stdout! "  Environment")
              (stdout! "  ───────────")
              (stdout! (str "  OS:           " (System/getProperty "os.name") " " (System/getProperty "os.arch")))
              (stdout! (str "  Java:         " (System/getProperty "java.version") " (" (System/getProperty "java.vendor") ")"))
              (stdout! (str "  Clojure:      " (clojure-version)))
              (stdout! (str "  Memory:       " (let [rt (Runtime/getRuntime)
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

      ;; Conversations list
      (= cmd "conversations")
      (do (config/init-cli!)
        (cli-conversations! (rest args))
        (shutdown-agents))

      ;; Extensions list
      (= cmd "extensions")
      (do (config/init-cli!)
        (let [exts (channels/list-extensions)]
          (if (empty? exts)
            (stdout! "No extensions registered.")
            (do
              (stdout! "\n  Extensions\n")
              (print-table!
                [{:key :namespace :label "Namespace" :width 24 :align :left}
                 {:key :doc       :label "Description" :width 40 :align :left}
                 {:key :group     :label "Group"   :width 14 :align :left}
                 {:key :version   :label "Version" :width 10 :align :left}
                 {:key :cli-cmds  :label "CLI"     :width 20 :align :left}]
                exts)
              (stdout! (str "\n  " (count exts) " extension(s)\n")))))
        (shutdown-agents))

      ;; Extension CLI command
      (= cmd "ext")
      (do (config/init-cli!)
        (let [ext-cmd (second args)
              ext-args (vec (drop 2 args))]
          (if (or (nil? ext-cmd) (= ext-cmd "help"))
            (stdout! (channels/extension-help))
            (let [{:keys [ok error help]} (channels/run-extension-cmd! ext-cmd ext-args)]
              (cond
                help  (stdout! help)
                error (do (stdout! (str "Error: " error))
                        (System/exit 1))
                :else (when (some? ok)
                        (stdout! (str ok)))))))
        (shutdown-agents))

      ;; TUI chat (explicit or no args)
      (or (nil? cmd) (= cmd "chat"))
      (run-tui! (parse-chat-args (rest args)))

      ;; Unknown subcommand → treat all args as a run prompt
      :else
      (do (config/init-cli!)
        (cli-run! args)))))
