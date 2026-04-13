(ns com.blockether.vis.telegram.bot
  "Telegram frontend for vis — long-polling loop that hands each incoming
   message to `telegram.sessions/handle-message!` and ships the answer back.

   Usage:
     export TELEGRAM_BOT_TOKEN=<from @BotFather>
     clojure -M:run telegram    ;; or: vis telegram

   One process can serve many chats — each Telegram chat is a named
   `:conversation` inside the shared `~/.vis/vis.mdb` Datalevin DB (see
   `config/db-path`), managed in `telegram.sessions`."
  (:require [com.blockether.vis.telegram.api :as tg]
            [com.blockether.vis.telegram.sessions :as sessions]))

(defonce ^:private running? (atom false))
(defonce ^:private poll-thread (atom nil))

(def ^:private poll-timeout-seconds 30)

(defn- extract-text
  "Pull the user-facing text out of a Telegram Message. Covers plain text,
   captioned media, and the empty case. Commands like `/start` come through
   as normal text — we let the LLM see them verbatim."
  [msg]
  (or (:text msg) (:caption msg)))

(defn- extract-sender [msg]
  (or (some-> msg :from :username not-empty)
      (some-> msg :from :first_name)
      "user"))

(defn- handle-update!
  "Dispatch a single Telegram update. Each message runs in its own future so
   the poll loop never blocks on the LLM; `sessions/handle-message!` serializes
   per-chat, so two messages from the same chat still run in order."
  [token update]
  (when-let [msg (:message update)]
    (let [chat-id (-> msg :chat :id)
          text    (extract-text msg)
          sender  (extract-sender msg)]
      (when (and chat-id text)
        (future
          (try
            (tg/send-chat-action! token chat-id "typing")
            (let [result (sessions/handle-message! chat-id text)
                  answer (:answer result)]
              (tg/send-message! token chat-id
                                (if (string? answer)
                                  answer
                                  (pr-str answer))))
            (catch Exception e
              (println (str "[telegram] error handling msg from " sender
                            " in chat " chat-id ": " (ex-message e)))
              (try
                (tg/send-message! token chat-id
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
                        ;; Brief backoff on transient errors, then retry.
                        (Thread/sleep 2000)
                        []))]
        (cond
          (= updates ::interrupted) :stopped

          (seq updates)
          (do (doseq [u updates] (handle-update! token u))
              (recur (inc (apply max (map :update_id updates)))))

          :else
          (recur offset))))))

(defn- resolve-token! []
  (or (not-empty (System/getenv "TELEGRAM_BOT_TOKEN"))
      (throw (ex-info (str "TELEGRAM_BOT_TOKEN env var is not set. "
                           "Create a bot via @BotFather on Telegram, then:\n"
                           "  export TELEGRAM_BOT_TOKEN=<your-token>")
                      {}))))

(defn start!
  "Start the Telegram long-polling bot. Blocking unless a poll thread is
   already running."
  []
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
    (sessions/dispose-all!)
    (println "Telegram bot stopped.")))

(defn -main [& _]
  (.addShutdownHook (Runtime/getRuntime)
                    (Thread. ^Runnable (fn []
                                         (println "Shutting down Telegram bot…")
                                         (stop!))))
  (start!)
  @(promise))
