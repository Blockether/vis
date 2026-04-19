(ns com.blockether.vis.adapters.tui.chat
  "TUI-side projections over the shared conversations API.

   On startup the TUI creates a fresh `:vis` conversation — history starts
   empty, and the new conversation shows up in the `:vis` channel alongside
   anything the web server has created. Switching between existing
   conversations is not wired into the TUI today; we just open a new one
   each boot."
  (:require [com.blockether.vis.loop.conversations.core :as conversations]
            [com.blockether.vis.loop.conversations.shared :as conv-shared]
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
  (let [{:keys [id]} (conversations/create! :vis)]
    {:id id :history []}))

(defn query!
  "Send a user query through the shared conversations cache. Blocking.
   Returns `{:answer str}` or `{:error str}`.

   `opts` may contain:
     :on-chunk — fn receiving `{:iteration :thinking :code :final :done?}`
                 on every streaming chunk from the RLM. The TUI uses this
                 to project a live per-iteration progress timeline into
                 the assistant placeholder bubble."
  ([conv text] (query! conv text {}))
  ([{:keys [id]} text {:keys [on-chunk]}]
   (try
     (let [send-opts (cond-> {}
                       on-chunk (assoc :hooks {:on-chunk on-chunk}))
           result (conversations/send! id text send-opts)
           answer (or (:answer result) "[empty response]")]
       {:answer (if (string? answer) answer (pr-str answer))})
     (catch Exception e
       (t/log! :error (str "Query failed: " (ex-message e)))
       {:error (conv-shared/error->user-message e)}))))

(defn dispose!
  "Release the TUI's env handle. Conversation data stays in
   `~/.vis/vis.mdb` so the web server (same `:vis` channel) can still see
   it."
  [{:keys [id]}]
  (when id (conversations/close! id)))
