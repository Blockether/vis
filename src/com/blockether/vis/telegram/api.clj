(ns com.blockether.vis.telegram.api
  "Thin wrapper around the Telegram Bot API using babashka/http-client.

   Telegram Bot API reference: https://core.telegram.org/bots/api

   Three primitives are enough for a chat bot:
   - `get-updates`      long-poll for incoming messages
   - `send-message!`    send text back (auto-splits at 4096-char Telegram limit)
   - `send-chat-action!`  show 'typing…' while the LLM works"
  (:require [babashka.http-client :as http]
            [charred.api :as json]
            [clojure.string :as str]))

(defn- base-url [token]
  (str "https://api.telegram.org/bot" token))

(defn- parse-body [resp]
  (json/read-json (:body resp) :key-fn keyword))

(defn get-updates
  "Long-poll Telegram for new updates.

   `offset`  — lowest update_id we haven't processed (0 on first call).
   `timeout` — seconds Telegram may hold the connection (up to 50). The HTTP
               client timeout is bumped by 10s to cover network slack.

   Returns a vector of update maps (Telegram's `Update` objects)."
  [token offset timeout]
  (let [resp (http/get (str (base-url token) "/getUpdates")
                       {:query-params {"offset"  (str offset)
                                       "timeout" (str timeout)}
                        :timeout      (* 1000 (+ timeout 10))})
        body (parse-body resp)]
    (when-not (:ok body)
      (throw (ex-info (str "Telegram getUpdates failed: " (:description body))
                      {:body body})))
    (:result body)))

(defn- post-json! [token path payload]
  (let [resp (http/post (str (base-url token) path)
                        {:headers {"content-type" "application/json"}
                         :body    (json/write-json-str payload)
                         :timeout 15000
                         :throw   false})]
    (parse-body resp)))

(defn- chunk-text
  "Split text into <=4000-char pieces along paragraph/line boundaries so we
   don't break Markdown fences mid-block. Telegram's hard limit is 4096."
  [text]
  (if (<= (count text) 4000)
    [text]
    (loop [remaining text
           chunks    []]
      (if (<= (count remaining) 4000)
        (conj chunks remaining)
        (let [cut-at (or (str/last-index-of remaining "\n\n" 4000)
                         (str/last-index-of remaining "\n" 4000)
                         (str/last-index-of remaining " " 4000)
                         4000)]
          (recur (subs remaining cut-at)
                 (conj chunks (subs remaining 0 cut-at))))))))

(defn send-message!
  "Send a text reply. Tries Markdown parse mode first, falls back to plain text
   when Telegram rejects the formatting (common for stray `_` / `*` characters
   in code output). Auto-splits at Telegram's 4096-char limit."
  [token chat-id text]
  (doseq [chunk (chunk-text (or text ""))]
    (let [payload {"chat_id"    chat-id
                   "text"       chunk
                   "parse_mode" "Markdown"}
          resp    (post-json! token "/sendMessage" payload)]
      (when-not (:ok resp)
        ;; Fallback: resend without Markdown if parsing failed.
        (post-json! token "/sendMessage" (dissoc payload "parse_mode"))))))

(defn send-chat-action!
  "Show a transient indicator in the chat (e.g. 'typing…'). Best-effort: errors
   are swallowed because it's UX decoration, not correctness."
  [token chat-id action]
  (try
    (post-json! token "/sendChatAction"
                {"chat_id" chat-id "action" action})
    (catch Exception _ nil)))
