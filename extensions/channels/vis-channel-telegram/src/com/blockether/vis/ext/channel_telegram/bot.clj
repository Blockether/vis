(ns com.blockether.vis.ext.channel-telegram.bot
  "Telegram frontend for vis — long-polling loop that hands each incoming
   message to the shared conversations API and ships the answer back.

   Each Telegram chat maps to a `:telegram` conversation via
   `sdk/for-telegram-chat!` (find-or-create on chat-id). One process can
   serve many chats; svar serializes asks per-conversation via the
   conversation's lock in `com.blockether.vis.core`."
  (:require [com.blockether.vis.core :as sdk]
            [com.blockether.vis.ext.channel-telegram.api :as tg]
            [taoensso.telemere :as tel]))

(defonce ^:private running? (atom false))
(defonce ^:private poll-thread (atom nil))

(def ^:private poll-timeout-seconds 30)

(defn- extract-text [message] (or (:text message) (:caption message)))

(defn- extract-sender [message]
  (or (some-> message :from :username not-empty)
    (some-> message :from :first_name)
    "user"))

(defn- format-footer [result]
  ;; Canonical " · "-joined turn-summary line, identical to the CLI
  ;; bracket and the TUI per-message footer. Provider + model
  ;; auto-extract from the result's `:cost :provider` / `:cost :model`,
  ;; rendering as `provider/model` (e.g. `blockether/glm-5.1`).
  (let [line (sdk/format-meta-line result)]
    (str "\n\n_" (tg/escape-markdown-v2 line) "_")))

(defn- handle-update! [token update]
  (when-let [message (:message update)]
    (let [chat-id (-> message :chat :id)
          text    (extract-text message)
          sender  (extract-sender message)]
      (when (and chat-id text)
        (future
          (try
            (tg/send-chat-action! token chat-id "typing")
            (let [{:keys [id]} (sdk/for-telegram-chat! chat-id)
                  result       (sdk/send! id text
                                 {:max-context-tokens 2200})
                  answer       (if (string? (:answer result)) (:answer result) (pr-str (:answer result)))]
              (tg/send-message! token chat-id
                (str answer (format-footer result))))
            (catch Exception e
              (tel/log! {:level :error :id ::handle-message
                         :data {:sender sender :chat-id chat-id :error (ex-message e)}
                         :msg (str "error handling msg from " sender " in chat " chat-id)})
              (try (tg/send-message! token chat-id
                     (sdk/format-error (sdk/db-error->user-message e)))
                (catch Exception _ nil)))))))))

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

(defn start! []
  (when (compare-and-set! running? false true)
    (let [token (resolve-token!)
          t     (Thread. ^Runnable #(poll-loop! token) "vis-channel-telegram-poll")]
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
    (sdk/close-all!)
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
  (sdk/init-cli!)
  (-main))

(sdk/register-extension!
  (sdk/extension
    {:ext/namespace 'com.blockether.vis.ext.channel-telegram.bot
     :ext/doc       "Telegram bot channel — long-poll loop wired into conversations."
     :ext/version   "0.3.0"
     :ext/author    "Blockether"
     :ext/owner     "vis"
     :ext/license   "Apache-2.0"
     :ext/channels  [{:channel/id      :telegram
                      :channel/cmd     "telegram"
                      :channel/doc     "Run as a Telegram bot (needs TELEGRAM_BOT_TOKEN)."
                      :channel/usage   "vis telegram"
                      :channel/main-fn #'channel-main}]}))

