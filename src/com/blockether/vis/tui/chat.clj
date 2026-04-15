(ns com.blockether.vis.tui.chat
  "TUI-side projections over the shared conversations API.

   On startup the TUI creates a fresh `:vis` conversation — history starts
   empty, and the new conversation shows up in the `:vis` channel alongside
   anything the web server has created. Switching between existing
   conversations is not wired into the TUI today; we just open a new one
   each boot."
  (:require [com.blockether.vis.conversations :as conv]
            [taoensso.telemere :as t]))

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

(defn make-conversation
  "Create a fresh `:vis` conversation for this TUI session. Returns
   `{:id conv-id :history []}`. History is empty — we always boot into a
   new chat. `provider-config` is kept for caller compat."
  [_provider-config]
  (let [{:keys [id]} (conv/create! :vis)]
    {:id id :history []}))

(defn query!
  "Send a user query through the shared conversations cache. Blocking.
   Returns `{:answer str}` or `{:error str}`."
  [{:keys [id]} text]
  (try
    (let [result (conv/send! id text)
          answer (or (:answer result) "[empty response]")]
      {:answer (if (string? answer) answer (pr-str answer))})
    (catch Exception e
      (t/log! :error (str "Query failed: " (ex-message e)))
      {:error (str "error: " (ex-message e))})))

(defn dispose!
  "Release the TUI's env handle. Conversation data stays in
   `~/.vis/vis.mdb` so the web server (same `:vis` channel) can still see
   it."
  [{:keys [id]}]
  (when id (conv/close! id)))
