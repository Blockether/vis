(ns com.blockether.vis.channels.cli
  "vis-core's built-in CLI commands.

   This namespace is a PLUG-IN, not a dispatcher. Loading it at
   startup self-registers every built-in (`run`, `auth`, `doctor`,
   `conversations`, `extensions`) into `com.blockether.vis.commandline`
   so the dispatcher in `com.blockether.vis.commandline.main` can
   discover and invoke them.

   Things that live elsewhere now:

     - The dispatcher / `-main` / pre-stderr-redirect → vis-commandline
     - The `vis channel` parent + channel-registry adapter → vis-extension
     - The `vis ext` parent + `:ext/cli` adapter → vis-extension

   `META-INF/vis/commandline.edn` lists this namespace so dropping
   the vis-core jar onto the classpath is enough to get every
   built-in. No caller wiring required."
  (:require [borkdude.dynaload :as dl]
            [clojure.string :as str]
            [com.blockether.vis.channels.cli.agent :as agent]
            [com.blockether.vis.channels.core :as channels]
            [com.blockether.vis.commandline :as cmd]
            [com.blockether.vis.config :as config]
            [com.blockether.vis.core :as core]
            [com.blockether.vis.loop.runtime.conversation.core :as conv-core]
            [com.blockether.vis.loop.runtime.conversation.environment.query.core :as query]
            [com.blockether.vis.persistance.core :as db]
            [taoensso.telemere :as tel]))

;;; ── vis-provider lookup (dynaload — zero compile-time dep on it) ───────

(def ^:private provider-by-id
  (dl/dynaload 'com.blockether.vis.provider/by-id
    {:default (constantly nil)}))

(def ^:private registered-providers
  (dl/dynaload 'com.blockether.vis.provider/registered-providers
    {:default (constantly [])}))

;;; ── Output helpers ──────────────────────────────────────────────────────

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
        (stdout! "  Resume with: vis channels tui --conversation-id <ID>  (full or short)")
        (stdout! "  Or latest:   vis channels tui --resume"))))
  (shutdown-agents))

;;; ── `vis auth` ──────────────────────────────────────────────────────────

(defn- print-auth-status! [provider]
  (let [s ((:provider/status-fn provider))]
    (stdout! (str "\n  " (:provider/label provider) " Auth Status"))
    (stdout! "  ─────────────────────────────────")
    (if (:authenticated? s)
      (do (stdout! "  Authenticated: yes")
        (when-let [src (:source s)]
          (stdout! (str "  Source:        " (name src))))
        (when-let [tp (:oauth-token-preview s)]
          (stdout! (str "  Token:         " tp)))
        (when (contains? s :copilot-token-valid?)
          (stdout! (str "  API token:     "
                     (if (:copilot-token-valid? s) "valid" "expired")))
          (when (:copilot-token-valid? s)
            (stdout! (str "  Expires in:    "
                       (int (/ (:expires-in-ms s) 60000)) " min")))))
      (stdout! "  Authenticated: no"))
    (stdout! "")))

(defn- cli-auth!
  "Look the requested provider up through the registry and dispatch
   --status / --logout / interactive auth via its registered fns.
   Concrete providers live in their own packages
   (`vis-provider-github-copilot`, future `vis-provider-anthropic`,
   …); vis-core never references them by namespace."
  [_parsed residual]
  (config/init-cli!)
  (let [provider-id (some-> (first residual) keyword)
        flags       (set (rest residual))
        provider    (when provider-id (provider-by-id provider-id))
        all         (registered-providers)]
    (cond
      (nil? provider-id)
      (do (stdout! "Usage: vis auth <provider> [--status | --logout]")
        (stdout! "")
        (if (seq all)
          (do (stdout! "Available providers:")
            (doseq [p (sort-by :provider/id all)]
              (stdout! (str "  " (cmd/pad-right (name (:provider/id p)) 22)
                         (:provider/label p)))))
          (stdout! "No providers registered. Drop a vis-provider-* jar onto the classpath.")))

      (nil? provider)
      (do (stdout! (str "Unknown auth provider: " (name provider-id)))
        (when (seq all)
          (stdout! (str "Available: "
                     (str/join ", " (map (comp name :provider/id)
                                      (sort-by :provider/id all)))))))

      (contains? flags "--status")
      (when (:provider/status-fn provider)
        (print-auth-status! provider))

      (contains? flags "--logout")
      (when-let [f (:provider/logout-fn provider)]
        (f)
        (stdout! (str "  Logged out of " (:provider/label provider) ". Tokens cleared.")))

      :else
      (when-let [auth-fn (:provider/auth-fn provider)]
        (try (auth-fn stdout!)
          (catch Exception e
            (stdout! (str "  ✗ Authentication failed: " (ex-message e))))))))
  (shutdown-agents))

;;; ── `vis doctor` ────────────────────────────────────────────────────────

(defn- cli-doctor! [_parsed _residual]
  (config/init-cli!)
  (let [env (core/create-environment (query/get-router)
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
        ;; Use the conversation API — it owns the channel-metadata
        ;; storage layout (JSON-extracted from conversation_soul.metadata
        ;; via the persistence backend). Reaching into raw JDBC here used
        ;; to query a non-existent `conversation` table and silently
        ;; reported 0 for every channel.
        (let [active-envs (count @conv-core/cache)
              count-ch    (fn [ch]
                            (try (count (conv-core/by-channel ch))
                              (catch Exception _ 0)))
              vis-n  (count-ch :vis)
              cli-n  (count-ch :cli)
              tg-n   (count-ch :telegram)
              total  (+ vis-n cli-n tg-n)]
          (stdout! (str "  Conversations:  " total
                     " (" vis-n " vis, " cli-n " cli, " tg-n " telegram)"
                     " — " active-envs " active in memory"))))
      (finally
        (core/dispose-environment! env)
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

;;; ── Self-register every built-in into the commandline registry ─────────
;;
;; This is the entire wiring this namespace contributes to the CLI.
;; The dispatcher in `com.blockether.vis.commandline.main` finds
;; these via `cmd/registered-under []` and renders + dispatches them
;; alongside any other plug-in's commands.

(doseq [spec
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

         ;; `list` mounts UNDER `vis extensions` (the parent registered
         ;; by vis-extension/extension.clj). vis-core has no top-level
         ;; `extensions` command anymore — listing extensions is just
         ;; another subcommand of the extensions parent.
         {:cmd/name   "list"
          :cmd/parent ["extensions"]
          :cmd/doc    "List every registered extension with metadata."
          :cmd/usage  "vis extensions list"
          :cmd/run-fn cli-extensions!}]]
  (cmd/register-global! spec))
