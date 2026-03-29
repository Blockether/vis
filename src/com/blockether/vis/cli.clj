(ns com.blockether.vis.cli
  (:require [clojure.string :as str]
            [com.blockether.vis.agent :as agent]
            [com.blockether.vis.logging :as logging]
            [com.blockether.vis.trace :as trace]
            [com.blockether.vis.tui.screen :as screen]))

;;; ── CLI Output ──────────────────────────────────────────────────────────

(defn- stdout!
  "Print to the real terminal via the saved original stdout.
   All other output (telemere, trove, SLF4J) is redirected to log file."
  [^String s]
  (.println ^java.io.PrintStream logging/original-stdout s)
  (.flush ^java.io.PrintStream logging/original-stdout))

;;; ── Argument Parsing ────────────────────────────────────────────────────

(defn- parse-run-args
  "Parse `vis run` arguments into {:prompt str, :json? bool, ...}.

   Flags:
     --json               output JSON
     --edn                output EDN
     --trace              show full execution trace
     --debug              enable svar debug logging
     --verify             enable claim verification
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
          "--verify"         (recur more (assoc opts :verify? true) prompt-parts)
          "--model"          (recur (next more) (assoc opts :model (first more)) prompt-parts)
          "--max-iterations" (recur (next more) (assoc opts :max-iterations (parse-long (first more))) prompt-parts)
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
  (println "  vis help             Show this help")
  (println)
  (println "Run `vis run --help` for agent options."))

(defn- print-run-usage! []
  (stdout! "Usage: vis run [FLAGS] \"prompt\"")
  (stdout! "")
  (stdout! "Flags:")
  (stdout! "  --json              Output result as JSON")
  (stdout! "  --edn               Output result as EDN")
  (stdout! "  --trace             Show full execution trace")
  (stdout! "  --debug             Enable debug logging")
  (stdout! "  --verify            Enable claim verification")
  (stdout! "  --model MODEL       Override LLM model")
  (stdout! "  --max-iterations N  Override iteration budget (default 50)")
  (stdout! "  --name NAME         Agent name (affects storage path)")
  (stdout! "  --system-prompt STR System instructions for the agent")
  (stdout! "")
  (stdout! "Examples:")
  (stdout! "  vis run \"What is 2+2?\"")
  (stdout! "  vis run --json --model gpt-4o \"Explain auth flow\"")
  (stdout! "  vis run --system-prompt \"You are a code reviewer\" \"Review auth.clj\""))

;;; ── Run Command ─────────────────────────────────────────────────────────

(defn- cli-run!
  "Entry point for `vis run`. Parses args, runs agent, prints result."
  [args]
  (let [{:keys [prompt json? edn? trace? help? agent-name] :as opts} (parse-run-args args)]
    (when (or help? (str/blank? prompt))
      (print-run-usage!)
      (System/exit 0))
    (let [agent-def  (agent/agent {:name (or agent-name "cli")})
          run-opts   (dissoc opts :prompt :json? :edn? :trace? :compact? :agent-name)
          ;; When --trace, set up live streaming hooks
          live       (when trace? (trace/live-trace-hooks logging/original-stdout))
          _          (when (and live (:model run-opts))
                       (swap! (:state live) assoc :model (:model run-opts)))
          run-opts   (if live
                       (assoc run-opts :hooks (:hooks live))
                       run-opts)
          result     (agent/run! agent-def prompt run-opts)]
      (cond
        json?
        (stdout! (agent/result->json result))

        edn?
        (stdout! (agent/result->edn result))

        ;; --trace: live hooks already streamed iterations, now print summary + answer
        trace?
        (do (trace/print-live-summary! logging/original-stdout result (:state live))
            (when (:error result)
              (when-let [ex (:exception result)]
                (stdout! "\nStack trace:")
                (.printStackTrace ^Throwable ex logging/original-stdout))
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
  (logging/init!)
  (try
    (screen/run-chat!)
    (finally
      (logging/shutdown!))))

;;; ── Main ────────────────────────────────────────────────────────────────

(defn -main [& args]
  (let [cmd (first args)]
    (cond
      ;; Explicit run — stdout stays connected, logs go to file only
      (= cmd "run")
      (do (logging/init-cli!)
          (cli-run! (rest args)))

      ;; Help
      (#{"help" "--help" "-h"} cmd)
      (print-help!)

      ;; TUI chat (explicit or no args)
      (or (nil? cmd) (= cmd "chat"))
      (run-tui!)

      ;; Unknown subcommand → treat all args as a run prompt
      :else
      (do (logging/init-cli!)
          (cli-run! args)))))
