(ns com.blockether.vis.adapters.cli
  (:require [charred.api :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [com.blockether.vis.adapters.cli.agent :as agent]
            [com.blockether.vis.config :as config]
            [com.blockether.vis.adapters.telegram.bot :as telegram]
            [com.blockether.vis.core :as core]
            [com.blockether.vis.loop.storage.db :as rlm-db]
            [com.blockether.vis.loop.runtime.query.routing :as rlm-routing]
            [com.blockether.vis.adapters.tui.screen :as screen]
            [taoensso.trove :as trove]))

;;; ── CLI Output ──────────────────────────────────────────────────────────

(defn- stdout!
  "Print to the real terminal via the saved original stdout.
   All other output (telemere, trove, SLF4J) is redirected to log file."
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
     --name NAME          agent name (affects storage path)
     --system-prompt STR  system instructions for the agent

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
          "--system-prompt"  (recur (next more) (assoc opts :system-prompt (first more)) prompt-parts)
          ;; Not a flag → part of the prompt
          (recur more opts (conj prompt-parts arg)))))))

;;; ── Help ────────────────────────────────────────────────────────────────

(defn- print-help! []
  (println "vis — AI assistant with svar RLM")
  (println)
  (println "Commands:")
  (println "  vis                  Start interactive TUI chat")
  (println "  vis chat             Start interactive TUI chat")
  (println "  vis run \"prompt\"     Run a one-shot agent query")
  (println "  vis index FILE       Build/update a .pageindex directory")
  (println "  vis qa INDEX_PATH    Generate QA pairs from an indexed document")
  (println "  vis web [PORT]       Start web server (default port 3000)")
  (println "  vis telegram         Run as a Telegram bot (needs TELEGRAM_BOT_TOKEN)")
  (println "  vis doctor           Show tool registration diagnostics")
  (println "  vis help             Show this help")
  (println)
  (println "Run `vis run --help`, `vis index --help`, or `vis qa --help` for options."))

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
  (stdout! "  --name NAME         Agent name (affects storage path)")
  (stdout! "  --system-prompt STR System instructions for the agent")
  (stdout! "")
  (stdout! "Examples:")
  (stdout! "  vis run \"What is 2+2?\"")
  (stdout! "  vis run --json --model gpt-4o \"Explain auth flow\"")
  (stdout! "  vis run --system-prompt \"You are a code reviewer\" \"Review auth.clj\""))

(defn- print-index-usage! []
  (stdout! "Usage: vis index [FLAGS] FILE")
  (stdout! "")
  (stdout! "Flags:")
  (stdout! "  --output DIR         Output directory (default: <file>.pageindex)")
  (stdout! "  --pages EDN          Page selection EDN (example: '[1 2 3]' or '{:from 1 :to 10}')")
  (stdout! "  --force              Force full reindex even when unchanged")
  (stdout! "")
  (stdout! "Examples:")
  (stdout! "  vis index docs/spec.pdf")
  (stdout! "  vis index docs/spec.pdf --output docs/spec.pageindex --pages '[1 2 3]'"))

(defn- print-qa-usage! []
  (stdout! "Usage: vis qa [FLAGS] INDEX_PATH")
  (stdout! "")
  (stdout! "Flags:")
  (stdout! "  --count N            Target number of Q&A pairs (default 10)")
  (stdout! "  --model MODEL        Override generation model")
  (stdout! "  --output PATH        Save Q&A to PATH.edn and PATH.md")
  (stdout! "  --json               Output full result as JSON")
  (stdout! "  --edn                Output full result as EDN")
  (stdout! "")
  (stdout! "Examples:")
  (stdout! "  vis qa docs/spec.pageindex")
  (stdout! "  vis qa docs/spec.pageindex --count 30 --output out/spec-qa"))

(defn- parse-edn-arg
  [s flag]
  (try
    (edn/read-string s)
    (catch Exception e
      (throw (ex-info (str flag " must be valid EDN")
               {:type :cli/invalid-arg :arg flag :value s :cause (ex-message e)})))))

(defn- parse-index-args
  [args]
  (loop [args (seq args)
         opts {}]
    (if-not args
      opts
      (let [arg (first args)
            more (next args)]
        (case arg
          ("--help" "-h") (assoc opts :help? true)
          "--force"       (recur more (assoc opts :force? true))
          "--output"      (recur (next more) (assoc opts :output (first more)))
          "--pages"       (recur (next more) (assoc opts :pages (first more)))
          (if (str/starts-with? arg "--")
            (throw (ex-info (str "Unknown flag: " arg)
                     {:type :cli/invalid-arg :arg arg}))
            (if (:file-path opts)
              (throw (ex-info (str "Unexpected extra argument: " arg)
                       {:type :cli/invalid-arg :arg arg}))
              (recur more (assoc opts :file-path arg)))))))))

(defn- parse-qa-args
  [args]
  (loop [args (seq args)
         opts {}]
    (if-not args
      opts
      (let [arg (first args)
            more (next args)]
        (case arg
          ("--help" "-h") (assoc opts :help? true)
          "--json"        (recur more (assoc opts :json? true))
          "--edn"         (recur more (assoc opts :edn? true))
          "--count"       (recur (next more) (assoc opts :count (parse-long (first more))
                                               :count-raw (first more)
                                               :count-provided? true))
          "--model"       (recur (next more) (assoc opts :model (first more)))
          "--output"      (recur (next more) (assoc opts :output (first more)))
          (if (str/starts-with? arg "--")
            (throw (ex-info (str "Unknown flag: " arg)
                     {:type :cli/invalid-arg :arg arg}))
            (if (:index-path opts)
              (throw (ex-info (str "Unexpected extra argument: " arg)
                       {:type :cli/invalid-arg :arg arg}))
              (recur more (assoc opts :index-path arg)))))))))

(defn- validate-index-opts!
  [{:keys [file-path pages]}]
  (when (str/blank? file-path)
    (throw (ex-info "FILE is required"
             {:type :cli/invalid-arg :arg "FILE"})))
  (when pages
    (parse-edn-arg pages "--pages"))
  true)

(defn- validate-qa-opts!
  [{:keys [index-path count count-raw count-provided?]}]
  (when (str/blank? index-path)
    (throw (ex-info "INDEX_PATH is required"
             {:type :cli/invalid-arg :arg "INDEX_PATH"})))
  (when count-provided?
    (when-not (and (integer? count) (pos? count))
      (throw (ex-info "--count must be an integer >= 1"
               {:type :cli/invalid-arg :arg "--count" :value count-raw :parsed count}))))
  true)

(defn- render-qa-result!
  [{:keys [questions dropped-questions stats duration-ms iterations]} saved-files]
  (stdout! "QA generation complete")
  (stdout! (str "- Questions: " (count questions)))
  (stdout! (str "- Dropped: " (count dropped-questions)))
  (stdout! (str "- Iterations: " iterations))
  (stdout! (str "- Duration: " duration-ms "ms"))
  (stdout! (str "- Difficulty mix: " (pr-str (:by-difficulty stats))))
  (stdout! (str "- Category mix: " (pr-str (:by-category stats))))
  (when (seq saved-files)
    (stdout! (str "- Saved files: " (str/join ", " saved-files)))))

(defn- cli-index!
  [args]
  (let [{:keys [help? file-path output force? pages] :as opts} (parse-index-args args)]
    (when help?
      (print-index-usage!)
      (System/exit 0))
    (try
      (validate-index-opts! opts)
      (catch Exception e
        (stdout! (str "Validation error: " (ex-message e)))
        (print-index-usage!)
        (shutdown-agents)
        (System/exit 1)))
    (let [router (rlm-routing/get-router)
          result (core/pageindex-build-and-write! file-path
                   (cond-> {:router router}
                     output (assoc :output output)
                     force? (assoc :force? true)
                     pages  (assoc :pages (parse-edn-arg pages "--pages"))))]
      (stdout! "Index build complete")
      (stdout! (str "- Output path: " (:output-path result)))
      (stdout! (str "- Cached: " (:cached? result)))
      (stdout! (str "- Hash changed: " (:hash-changed? result)))
      (stdout! (str "- Pages processed: " (:pages-processed result)))
      (stdout! (str "- Errors: " (:errors-count result)))
      (shutdown-agents))))

(defn- cli-qa!
  [args]
  (let [{:keys [help? index-path count model output json? edn?]
         :as opts} (parse-qa-args args)]
    (when help?
      (print-qa-usage!)
      (System/exit 0))
    (try
      (validate-qa-opts! opts)
      (catch Exception e
        (stdout! (str "Validation error: " (ex-message e)))
        (print-qa-usage!)
        (shutdown-agents)
        (System/exit 1)))
    (let [router (rlm-routing/get-router)
          env    (core/create-env router {:db config/db-path})]
      (try
        (let [doc      (core/pageindex-load index-path)
              _        (core/ingest-to-env! env [doc])
              qa-opts  (cond-> {}
                         count         (assoc :count count)
                         model         (assoc :model model))
              result   (core/qa-generate! env qa-opts)
              saved    (when output (core/qa-save-results! result output))
              payload  (cond-> result
                         (seq (:files saved)) (assoc :saved-files (:files saved)))]
          (cond
            json? (stdout! (json/write-json-str payload))
            edn?  (stdout! (pr-str payload))
            :else (render-qa-result! result (:files saved))))
        (catch Exception e
          (stdout! (str "Error: " (ex-message e)))
          (shutdown-agents)
          (System/exit 1))
        (finally
          (core/dispose-env! env)
          (shutdown-agents))))))

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
  (let [{:keys [prompt json? edn? trace? help? agent-name] :as opts} (parse-run-args args)]
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
          run-opts   (dissoc opts :prompt :json? :edn? :trace? :compact? :agent-name
                       :max-iterations-raw :max-iterations-provided?)
          result     (agent/run! agent-def prompt run-opts)]
      (cond
        json?
        (stdout! (agent/result->json result))

        edn?
        (stdout! (agent/result->edn result))

        ;; --trace: log full result via trove
        trace?
        (do (trove/log! {:level :info :id ::cli-trace
                         :data (select-keys result [:answer :trace :iterations
                                                    :duration-ms :tokens :cost
                                                    :error :type])
                         :msg "CLI trace result"})
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
            (stdout! (str "\n[" (:iterations result) " iterations"
                       ", " (:duration-ms result) "ms"
                       (when-let [c (some-> result :cost :total-cost)]
                         (str ", $" (String/format java.util.Locale/US "%.4f" (into-array Object [(double c)]))))
                       "]")))))
      (shutdown-agents))))

;;; ── TUI ─────────────────────────────────────────────────────────────────

(defn- run-tui!
  "Start TUI chat with logging redirected to file (stdout reserved for Lanterna)."
  []
  (config/init!)
  (try
    (screen/run-chat!)
    (finally
      (config/shutdown!))))

;;; ── Main ────────────────────────────────────────────────────────────────

(defn -main [& args]
  (let [cmd (first args)]
    (cond
      ;; Explicit run — stdout stays connected, logs go to file only
      (= cmd "run")
      (do (config/init-cli!)
        (cli-run! (rest args)))

      ;; PageIndex build
      (= cmd "index")
      (do (config/init-cli!)
        (cli-index! (rest args)))

      ;; QA generation from indexed corpus
      (= cmd "qa")
      (do (config/init-cli!)
        (cli-qa! (rest args)))

      ;; Help
      (#{"help" "--help" "-h"} cmd)
      (print-help!)

      ;; Web server
      (= cmd "web")
      (do (config/init-cli!)
        (require 'com.blockether.vis.adapters.web.app)
        (apply (resolve 'com.blockether.vis.adapters.web.app/-main) (rest args)))

      ;; Telegram bot — stdout stays connected for startup logs
      (= cmd "telegram")
      (do (config/init-cli!)
        (telegram/-main))

      ;; Doctor — tool registration diagnostics
      (= cmd "doctor")
      (do (config/init-cli!)
        (let [env (core/create-env (rlm-routing/get-router) {:db config/db-path})]
          (try
            (let [db-info  (:db-info env)
                  registry @(:tool-registry-atom env)
                  tools    (mapv (fn [[sym {:keys [activation-fn group activation-doc]}]]
                                   (let [t0      (System/nanoTime)
                                         active? (boolean (when activation-fn (activation-fn env)))
                                         elapsed (- (System/nanoTime) t0)]
                                     {:sym           (str sym)
                                      :group         (or group "Other")
                                      :active?       active?
                                      :activation-ms (/ (double elapsed) 1e6)
                                      :activation-doc activation-doc}))
                             registry)
                  report   ((requiring-resolve
                              'com.blockether.vis.loop.runtime.tool-diagnostics/format-doctor-report)
                             tools)
                  ;; DB stats
                  docs     (when db-info (rlm-db/db-list-documents db-info))
                  repo-stats (when db-info (rlm-db/db-repo-stats db-info))
                  concepts   (when db-info (rlm-db/list-concepts db-info))]
              (stdout! "vis doctor")
              (stdout! "")
              ;; Environment summary
              (stdout! "  Environment")
              (stdout! "  ───────────")
              (stdout! (str "  DB path:      " (or (:path db-info) "none")))
              (stdout! (str "  Documents:    " (count docs)
                        (when (seq docs)
                          (str " (" (str/join ", " (map :name docs)) ")"))))
              (stdout! (str "  Concepts:     " (count concepts)
                        (when (seq concepts)
                          (let [groups (group-by :group_name concepts)]
                            (str " in " (count groups) " groups")))))
              (if-let [{:keys [repos total-commits unique-authors]} repo-stats]
                (if (seq repos)
                  (do
                    (stdout! (str "  Git:          " (count repos) " repo(s), "
                              total-commits " commits, "
                              unique-authors " authors"))
                    (doseq [r repos]
                      (stdout! (str "                └ " (:name r)
                                " [" (or (:branch r) "?") "]"
                                (when (:commits-ingested r)
                                  (str " " (:commits-ingested r) " commits"))
                                " " (:path r)))))
                  (stdout! "  Git:          none"))
                (stdout! "  Git:          none"))
              ;; Tools
              (stdout! (str "\n" (count tools) " tools registered"))
              (stdout! report))
            (finally
              (core/dispose-env! env)
              (shutdown-agents)))))

      ;; TUI chat (explicit or no args)
      (or (nil? cmd) (= cmd "chat"))
      (run-tui!)

      ;; Unknown subcommand → treat all args as a run prompt
      :else
      (do (config/init-cli!)
        (cli-run! args)))))
