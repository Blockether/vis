(ns com.blockether.vis.ext.channel-telegram.bot
  "Telegram frontend for vis — long-polling loop that hands each incoming
   message to the shared conversations API and ships the answer back.

   Each Telegram chat maps to a `:telegram` conversation via
   `vis/for-telegram-chat!` (find-or-create on chat-id). One process can
   serve many chats; svar serializes asks per-conversation via the
   conversation's lock in `com.blockether.vis.core`."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-telegram.api :as tg]
            [taoensso.telemere :as tel]))

(defonce ^:private running? (atom false))
(defonce ^:private poll-thread (atom nil))

(defonce ^:private chat-state
  ;; {chat-id {:settings {:reasoning-level :balanced
  ;;                      :openai-codex-verbosity :low}
  ;;           :model-cycle-order [...]
  ;;           :in-flight cancellation-token}}
  (atom {}))

(def ^:private poll-timeout-seconds 30)

(def ^:private reasoning-level-order [:quick :balanced :deep])
(def ^:private codex-verbosity-order [:low :medium :high])

(def ^:private default-chat-settings
  {:reasoning-level :balanced
   :openai-codex-verbosity :low})

(def ^:private bot-menu-commands
  [{"command" "start"     "description" "Show Vis help"}
   {"command" "help"      "description" "Show Vis help"}
   {"command" "status"    "description" "Show conversation/model/status"}
   {"command" "model"     "description" "Show current model"}
   {"command" "models"    "description" "List and choose models"}
   {"command" "reasoning" "description" "Show or set reasoning effort"}
   {"command" "verbosity" "description" "Show or set Codex verbosity"}
   {"command" "cancel"    "description" "Cancel current request"}
   {"command" "export"    "description" "Export conversation as Markdown"}])

(defn- extract-text [message] (or (:text message) (:caption message)))

(defn- extract-sender [message]
  (or (some-> message :from :username not-empty)
    (some-> message :from :first_name)
    "user"))

(defn- format-footer [result]
  ;; Canonical " · "-joined turn-summary line, decorated for Telegram.
  ;; Keep this RAW Markdown-ish text. `tg/send-message!` owns escaping.
  ;; Pre-escaping here made MarkdownV2 fallback leak literal backslashes
  ;; into chats when an answer body failed Telegram parsing.
  (let [line (vis/format-meta-line result)]
    (when (seq line)
      (str "\n\n_🤖 " line "_"))))

(defn- normalize-choice [allowed aliases default v]
  (let [k (cond
            (keyword? v) v
            (string? v)  (keyword (str/lower-case (str/trim v)))
            :else        nil)
        k (get aliases k k)]
    (if (some #{k} allowed) k default)))

(defn- normalize-reasoning-level [v]
  (normalize-choice reasoning-level-order
    {:low :quick :medium :balanced :high :deep}
    :balanced
    v))

(defn- normalize-codex-verbosity [v]
  (normalize-choice codex-verbosity-order {} :low v))

(defn- chat-settings [chat-id]
  (merge default-chat-settings (get-in @chat-state [chat-id :settings])))

(defn- update-chat-settings! [chat-id f & args]
  (:settings
   (get
     (swap! chat-state update chat-id
       (fn [state]
         (let [settings (apply f (chat-settings chat-id) args)]
           (assoc (or state {}) :settings settings))))
     chat-id)))

(defn- set-chat-setting! [chat-id k v]
  (update-chat-settings! chat-id assoc k v))

(defn- set-in-flight! [chat-id token]
  (swap! chat-state assoc-in [chat-id :in-flight] token)
  nil)

(defn- clear-in-flight! [chat-id token]
  (swap! chat-state update chat-id
    (fn [state]
      (if (= token (:in-flight state))
        (dissoc state :in-flight)
        state)))
  nil)

(defn- in-flight-token [chat-id]
  (get-in @chat-state [chat-id :in-flight]))

(defn- current-model-info []
  (when-let [router (try (vis/get-router) (catch Throwable _ nil))]
    (try (vis/resolve-effective-model router) (catch Throwable _ nil))))

(defn- current-provider-id []
  (:provider (current-model-info)))

(defn- reasoning-effort-configurable? []
  (let [info (current-model-info)]
    (or (nil? info)
      (and (boolean (:reasoning? info))
        (not= false (:reasoning-effort? info))
        (not= :zai-thinking (:reasoning-style info))))))

(defn- turn-extra-body [settings]
  (when (= :openai-codex (current-provider-id))
    {:text {:verbosity (name (or (:openai-codex-verbosity settings) :low))}}))

(defn- turn-reasoning-default [settings]
  (when (reasoning-effort-configurable?)
    (:reasoning-level settings)))

(defn- model-entry [provider model]
  (when-let [model-name (and model (vis/model-name model))]
    (when (:id provider)
      {:provider-id (:id provider)
       :model       model-name})))

(defn- model-cycle-entries [config]
  (->> (:providers config)
    (mapcat (fn [provider]
              (keep #(model-entry provider %) (:models provider))))
    vec))

(defn- active-model-entry [config]
  (when-let [provider (first (:providers config))]
    (model-entry provider (first (:models provider)))))

(defn- model-entry-label [{:keys [provider-id model]}]
  (str (some-> provider-id name) "/" model))

(defn- current-model-label []
  (if-let [entry (active-model-entry (or (vis/load-config) {:providers []}))]
    (model-entry-label entry)
    "unknown"))

(defn- move-to-front [pred coll]
  (let [items (vec (or coll []))
        idx   (first (keep-indexed (fn [idx item]
                                     (when (pred item) idx))
                       items))]
    (if (nil? idx)
      items
      (vec (concat [(nth items idx)]
             (subvec items 0 idx)
             (subvec items (inc idx)))))))

(defn- select-model-entry [config {:keys [provider-id model]}]
  (update config :providers
    (fn [providers]
      (mapv (fn [provider]
              (if (= provider-id (:id provider))
                (update provider :models
                  #(move-to-front (fn [candidate]
                                    (= model (vis/model-name candidate)))
                     %))
                provider))
        (move-to-front #(= provider-id (:id %)) providers)))))

(defn- apply-config! [config]
  (let [raw        (or (vis/load-config-raw) {})
        persistent (assoc raw :providers (vec (:providers config)))]
    (vis/save-config! persistent)
    (let [resolved (or (vis/reload-config!) persistent)
          router   (vis/rebuild-router! resolved)]
      (vis/refresh-cached-routers! router))
    persistent))

(defn- find-model-entry [entries selection]
  (let [selection (str/trim (or selection ""))]
    (or (when (re-matches #"\d+" selection)
          (nth entries (dec (Long/parseLong selection)) nil))
      (let [needle (str/lower-case selection)]
        (first
          (filter (fn [entry]
                    (let [label (str/lower-case (model-entry-label entry))
                          model (str/lower-case (:model entry))]
                      (or (= needle label) (= needle model))))
            entries))))))

(defn- select-model! [selection]
  (let [base-config (or (vis/load-config) {:providers []})
        entries     (model-cycle-entries base-config)]
    (cond
      (empty? entries)
      {:message "No models configured" :level :warn}

      (str/blank? (or selection ""))
      {:message "Missing model selection" :level :warn}

      :else
      (if-let [entry (find-model-entry entries selection)]
        (let [config' (select-model-entry base-config entry)]
          (apply-config! config')
          {:message (str "Model set: " (model-entry-label entry))
           :level :info
           :changed? true
           :entry entry})
        {:message (str "Unknown model: " selection) :level :warn}))))

(defn- model-list-lines [entries active]
  (map-indexed
    (fn [idx entry]
      (str (inc idx) ". " (when (= entry active) "✅ ")
        (model-entry-label entry)))
    entries))

(defn- model-inline-keyboard [entries active]
  {"inline_keyboard"
   (mapv (fn [[idx entry]]
           [{"text" (str (when (= entry active) "✅ ") (model-entry-label entry))
             "callback_data" (str "model:" idx)}])
     (map-indexed vector entries))})

(defn- command-model []
  (str "Current model: " (current-model-label)
    "\n\nUse /models to list and choose."))

(defn- command-models [arg]
  (let [base-config (or (vis/load-config) {:providers []})
        entries     (model-cycle-entries base-config)
        active      (active-model-entry base-config)]
    (if (str/blank? (or arg ""))
      {:message (if (seq entries)
                  (str "Models\nCurrent: " (or (some-> active model-entry-label) "unknown")
                    "\n\n" (str/join "\n" (model-list-lines entries active))
                    "\n\nTap a button, or send /models 2, or /models provider/model.")
                  "No models configured")
       :reply-markup (when (seq entries) (model-inline-keyboard entries active))}
      {:message (:message (select-model! arg))})))

(defn- model-label []
  (current-model-label))

(defn- command-help []
  (str "Vis Telegram commands:\n"
    "/start — show this help\n"
    "/status — show conversation, model, reasoning, verbosity\n"
    "/model — show current model\n"
    "/models — list models and choose with buttons\n"
    "/models <n|provider/model> — choose model\n"
    "/reasoning [quick|balanced|deep] — show/set reasoning effort, same as TUI Ctrl+R\n"
    "/verbosity [low|medium|high] — show/set OpenAI Codex verbosity, same as TUI Ctrl+L\n"
    "/cancel — cancel current request\n"
    "/export — export this conversation as Markdown\n"
    "/help — show this help"))

(defn- command-status [chat-id]
  (let [{:keys [id title]} (vis/for-telegram-chat! chat-id)
        settings (chat-settings chat-id)]
    (str "Conversation: " (subs (str id) 0 (min 8 (count (str id))))
      (when-not (str/blank? title) (str " — " title))
      "\nModel: " (model-label)
      "\nReasoning: " (name (:reasoning-level settings))
      "\nCodex verbosity: " (name (:openai-codex-verbosity settings))
      "\nIn flight: " (if (in-flight-token chat-id) "yes" "no"))))

(defn- command-reasoning [chat-id arg]
  (if (str/blank? (or arg ""))
    (str "Reasoning: " (name (:reasoning-level (chat-settings chat-id)))
      "\nUse /reasoning quick, /reasoning balanced, or /reasoning deep.")
    (let [level (normalize-reasoning-level arg)]
      (if-not (= (keyword (str/lower-case (str/trim arg))) level)
        (str "Unknown reasoning level: " arg
          "\nUse quick, balanced, or deep.")
        (do
          (set-chat-setting! chat-id :reasoning-level level)
          (str "Reasoning: " (name level)))))))

(defn- command-verbosity [chat-id arg]
  (if (str/blank? (or arg ""))
    (str "Codex verbosity: " (name (:openai-codex-verbosity (chat-settings chat-id)))
      "\nUse /verbosity low, /verbosity medium, or /verbosity high.")
    (let [verbosity (normalize-codex-verbosity arg)]
      (if-not (= (keyword (str/lower-case (str/trim arg))) verbosity)
        (str "Unknown verbosity: " arg
          "\nUse low, medium, or high.")
        (do
          (set-chat-setting! chat-id :openai-codex-verbosity verbosity)
          (str "Codex verbosity: " (name verbosity)))))))

(defn- command-cancel [chat-id]
  (if-let [token (in-flight-token chat-id)]
    (do
      (vis/cancel! token)
      "Cancelling current request…")
    "No request is currently running."))

(defn- command-export [chat-id]
  (let [{:keys [id]} (vis/for-telegram-chat! chat-id)
        env      (vis/env-for id)
        markdown (when env (vis/conversation->markdown (:db-info env) id))]
    (if (seq markdown)
      markdown
      "No persisted turns to export yet.")))

(defn- parse-command [text]
  (let [text (str/trim (or text ""))]
    (when (str/starts-with? text "/")
      (let [[raw & _] (str/split text #"\s+" 2)
            cmd       (-> raw
                        (str/replace #"@.*$" "")
                        str/lower-case)
            arg       (str/trim (subs text (min (count text) (count raw))))]
        {:cmd cmd :arg arg}))))

(defn- handle-command! [token chat-id text]
  (when-let [{:keys [cmd arg]} (parse-command text)]
    (let [{:keys [message reply-markup]}
          (case cmd
            "/start"     {:message (command-help)}
            "/help"      {:message (command-help)}
            "/status"    {:message (command-status chat-id)}
            "/model"     {:message (command-model)}
            "/models"    (command-models arg)
            "/reasoning" {:message (command-reasoning chat-id arg)}
            "/verbosity" {:message (command-verbosity chat-id arg)}
            "/cancel"    {:message (command-cancel chat-id)}
            "/export"    {:message (command-export chat-id)}
            {:message (str "Unknown command: " cmd "\n\n" (command-help))})]
      (tg/send-message! token chat-id message {:reply-markup reply-markup})
      true)))

(defn- answer-text [result]
  (let [answer (:answer result)]
    (if (string? answer) answer (pr-str answer))))

(defn- handle-user-text! [token chat-id text sender]
  (let [turn-token (vis/cancellation-token)
        settings   (chat-settings chat-id)
        opts       (cond-> {:cancel-atom (vis/cancellation-atom turn-token)}
                     (turn-reasoning-default settings)
                     (assoc :reasoning-default (turn-reasoning-default settings))

                     (turn-extra-body settings)
                     (assoc :extra-body (turn-extra-body settings)))]
    (set-in-flight! chat-id turn-token)
    (let [fut (future
                (try
                  (tg/send-chat-action! token chat-id "typing")
                  (let [{:keys [id]} (vis/for-telegram-chat! chat-id)
                        result       (vis/send! id text opts)]
                    (tg/send-message! token chat-id
                      (str (answer-text result) (format-footer result))))
                  (catch Exception e
                    (if (vis/cancellation? e)
                      (try (tg/send-message! token chat-id "Cancelled by user.")
                        (catch Exception _ nil))
                      (do
                        (tel/log! {:level :error :id ::handle-message
                                   :data {:sender sender :chat-id chat-id :error (ex-message e)}
                                   :msg (str "error handling msg from " sender " in chat " chat-id)})
                        (try (tg/send-message! token chat-id
                               (vis/format-error (vis/db-error->user-message e)))
                          (catch Exception _ nil)))))
                  (finally
                    (clear-in-flight! chat-id turn-token))))]
      (vis/cancellation-set-future! turn-token fut))))

(defn- handle-callback-query! [token callback]
  (let [callback-id (:id callback)
        data        (:data callback)
        chat-id     (-> callback :message :chat :id)]
    (when (and callback-id chat-id (string? data)
            (str/starts-with? data "model:"))
      (let [idx-str (subs data (count "model:"))
            result  (when (re-matches #"\d+" idx-str)
                      (select-model! (str (inc (Long/parseLong idx-str)))))]
        (tg/answer-callback-query! token callback-id (:message result))
        (tg/send-message! token chat-id (or (:message result) "Model selection failed."))
        true))))

(defn- handle-update! [token update]
  (or (when-let [callback (:callback_query update)]
        (handle-callback-query! token callback))
    (when-let [message (:message update)]
      (let [chat-id (-> message :chat :id)
            text    (extract-text message)
            sender  (extract-sender message)]
        (when (and chat-id text)
          (when-not (handle-command! token chat-id text)
            (handle-user-text! token chat-id text sender)))))))

(defn- poll-loop! [token]
  (loop [offset 0]
    (if-not @running?
      :stopped
      (let [updates (try
                      (tg/get-updates token offset poll-timeout-seconds)
                      (catch InterruptedException _ ::interrupted)
                      (catch Exception e
                        (tel/log! {:level :error :id ::poll-error
                                   :data {:error (ex-message e)}
                                   :msg "poll error"})
                        (Thread/sleep 2000)
                        []))]
        (cond
          (= updates ::interrupted) :stopped
          (seq updates) (do (doseq [u updates] (handle-update! token u))
                          (recur (inc (apply max (map :update_id updates)))))
          :else         (recur offset))))))

(defn- resolve-token! []
  (or (not-empty (System/getenv "TELEGRAM_BOT_TOKEN"))
    (throw (ex-info (str "TELEGRAM_BOT_TOKEN env var is not set. "
                      "Create a bot via @BotFather on Telegram, then:\n"
                      "  export TELEGRAM_BOT_TOKEN=<your-token>")
             {}))))

(defn- install-bot-menu! [token]
  (try
    (tg/set-my-commands! token bot-menu-commands)
    (tel/log! {:level :info :id ::bot-menu-installed}
      "Telegram bot command menu installed")
    (catch Exception e
      ;; Menu install is UX parity, not startup correctness. Keep the bot alive
      ;; and leave diagnostics in the log when Telegram rejects the command list
      ;; or the network is down during boot.
      (tel/log! {:level :warn :id ::bot-menu-install-failed
                 :data  {:error (ex-message e)}
                 :msg   "Telegram bot command menu install failed"}))))

(defn start! []
  (when (compare-and-set! running? false true)
    (let [token (resolve-token!)
          t     (Thread. ^Runnable #(poll-loop! token) "vis-channel-telegram-poll")]
      (install-bot-menu! token)
      (.setDaemon t true)
      (.start t)
      (reset! poll-thread t)
      (tel/log! {:level :info :id ::started}
        "Telegram bot running"))))

(defn stop! []
  (when (compare-and-set! running? true false)
    (when-let [t @poll-thread]
      (.interrupt ^Thread t)
      (reset! poll-thread nil))
    (vis/close-all!)
    (tel/log! {:level :info :id ::stopped}
      "Telegram bot stopped")))

(defn -main [& _]
  (.addShutdownHook (Runtime/getRuntime)
    (Thread. ^Runnable (fn []
                         (tel/log! {:level :info :id ::shutdown}
                           "Shutting down Telegram bot")
                         (stop!))
      "vis-channel-telegram-shutdown"))
  (start!)
  @(promise))

;;; ── Channel registration (auto-discovered via META-INF/vis-extension/vis.edn) ──

(defn- channel-main
  "Channel entry point. Boots the CLI runtime config, then starts the
   long-poll loop and blocks forever (until SIGTERM → shutdown hook)."
  [_args]
  (vis/init-cli!)
  (-main))

(vis/register-extension!
  (vis/extension
    {:ext/namespace 'com.blockether.vis.ext.channel-telegram.bot
     :ext/doc       "Telegram bot channel — long-poll loop wired into conversations."
     :ext/version   "0.3.0"
     :ext/author    "Blockether"
     :ext/owner     "vis"
     :ext/license   "Apache-2.0"
     :ext/channels  [{:channel/id      :telegram
                      :channel/cmd     "telegram"
                      :channel/doc     "Run as a Telegram bot (needs TELEGRAM_BOT_TOKEN)."
                      :channel/usage   "vis channels telegram"
                      :channel/main-fn #'channel-main}]}))
