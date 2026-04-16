(ns com.blockether.vis.adapters.telegram.bot
  "Telegram frontend for vis — long-polling loop that hands each incoming
   message to the shared conversations API and ships the answer back.

   Each Telegram chat maps to a `:telegram` conversation via
   `conversations/for-telegram-chat!` (find-or-create on chat-id). One process can
   serve many chats; svar serializes asks per-conversation via the
   conversation's lock in `com.blockether.vis.rlm.conversations.core`."
  (:require [com.blockether.vis.rlm.conversations.core :as conversations]
            [com.blockether.vis.rlm.routing :as routing]
            [com.blockether.vis.adapters.telegram.api :as tg]
            [com.blockether.vis.utils :as utils]))

(defonce ^:private running? (atom false))
(defonce ^:private poll-thread (atom nil))

(def ^:private poll-timeout-seconds 30)

(defn- telegram-system-prompt []
  "You are vis over Telegram. Keep replies short and direct. Use minimal markdown.")

(defn- extract-text [msg] (or (:text msg) (:caption msg)))

(defn- extract-sender [msg]
  (or (some-> msg :from :username not-empty)
    (some-> msg :from :first_name)
    "user"))

(defn- format-footer [result model-name]
  (let [{:keys [iterations duration-ms tokens cost]} result
        sec (if duration-ms (format "%.1f" (/ duration-ms 1000.0)) "?")
        ctx-in  (or (:input tokens) 0)
        ctx-out (or (:output tokens) 0)
        c       (or (:total-cost cost) 0)
        c-str   (if (pos? c) (format "$%.4f" c) "—")]
    (str "\n\n_"
      (tg/escape-markdown-v2 (str iterations " iters · " sec "s · " (or model-name "?")))
      "\n"
      (tg/escape-markdown-v2 (str "ctx " ctx-in "→" ctx-out " · " c-str))
      "_")))

(defn- handle-update! [token update]
  (when-let [msg (:message update)]
    (let [chat-id (-> msg :chat :id)
          text    (extract-text msg)
          sender  (extract-sender msg)]
      (when (and chat-id text)
        (future
          (try
            (tg/send-chat-action! token chat-id "typing")
            (let [{:keys [id]} (conversations/for-telegram-chat! chat-id)
                  result       (conversations/send! id text
                                 {:system-prompt (telegram-system-prompt)
                                  :max-context-tokens 2200
                                  :max-iterations 12})
                  answer       (if (string? (:answer result)) (:answer result) (pr-str (:answer result)))
                  env          (conversations/env-for id)
                  model-name   (routing/resolve-root-model (:router env))]
              (tg/send-message! token chat-id
                (str answer (format-footer result model-name))))
            (catch Exception e
              (println (str "[telegram] error handling msg from " sender
                         " in chat " chat-id ": " (ex-message e)))
              (try (tg/send-message! token chat-id
                     (str "⚠️ Error: " (ex-message e)))
                (catch Exception _ nil)))))))))

(defn- poll-loop! [token]
  (loop [offset 0]
    (if-not @running?
      :stopped
      (let [updates (try
                      (tg/get-updates token offset poll-timeout-seconds)
                      (catch InterruptedException _ ::interrupted)
                      (catch Exception e
                        (println (str "[telegram] poll error: " (ex-message e)))
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
          t     (Thread. ^Runnable #(poll-loop! token) "vis-telegram-poll")]
      (.setDaemon t true)
      (.start t)
      (reset! poll-thread t)
      (println "Telegram bot running. Message the bot to start chatting."))))

(defn stop! []
  (when (compare-and-set! running? true false)
    (when-let [t @poll-thread]
      (.interrupt ^Thread t)
      (reset! poll-thread nil))
    (conversations/close-all!)
    (println "Telegram bot stopped.")))

(defn -main [& _]
  (utils/add-shutdown-hook! (fn []
                              (println "Shutting down Telegram bot…")
                              (stop!)))
  (start!)
  @(promise))
