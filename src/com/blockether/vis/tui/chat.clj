(ns com.blockether.vis.tui.chat
  "Conversation state and LLM integration via SVAR RLM.
   Uses RLM's persistent environment for conversation history, learnings, and documents."
  (:require [clojure.string :as str]
            [com.blockether.svar.internal.rlm :as rlm]
            [taoensso.telemere :as t]))

(def ^:private rlm-path (str (System/getProperty "user.home") "/.vis/rlm"))

(defn user-msg
  "Create a structured user message with timestamp."
  ([text] (user-msg text (java.util.Date.)))
  ([text timestamp]
   {:role :user :text text :timestamp timestamp}))

(defn assistant-msg
  "Create a structured assistant (vis) message with timestamp."
  ([text] (assistant-msg text (java.util.Date.)))
  ([text timestamp]
   {:role :assistant :text (if (string? text) text (pr-str text)) :timestamp timestamp}))

(defn- load-history
  "Load previous conversation from RLM's Datalevin DB into structured messages."
  [env]
  (let [get-msgs (get-in env [:hooks :get-recent-messages])
        msgs          (when get-msgs (get-msgs 100))]
    (when (seq msgs)
      (->> (reverse msgs) ;; oldest first
           (keep (fn [{:keys [role content timestamp]}]
                   (when (and content (not (str/blank? content)))
                     (case role
                       :user      (user-msg content (or timestamp (java.util.Date.)))
                       :assistant (assistant-msg content (or timestamp (java.util.Date.)))
                       nil))))
           vec))))

(defn make-conversation
  "Create an RLM environment with persistent storage at ~/.vis/rlm.
   Returns {:env env, :history [structured-messages]}."
  [provider-config]
  (let [env        (rlm/create-env {:config provider-config :path rlm-path})
        history    (or (load-history env) [])]
    {:env env :history history}))

(defn query!
  "Send query to RLM. Blocking. Returns {:answer str} or {:error str}."
  [conv text]
  (try
    (let [result (rlm/query-env! (:env conv) text)
          answer (or (:answer result) "[empty response]")]
      {:answer (if (string? answer) answer (pr-str answer))})
    (catch Exception e
      (t/log! :error (str "Query failed: " (ex-message e)))
      {:error (str "error: " (ex-message e))})))

(defn dispose!
  "Dispose the RLM environment. Persistent data at ~/.vis/rlm is preserved."
  [{:keys [env]}]
  (rlm/dispose-env! env))
