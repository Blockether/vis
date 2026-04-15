(ns com.blockether.vis.telegram.api
  "Thin wrapper around the Telegram Bot API using babashka/http-client.

   Telegram Bot API reference: https://core.telegram.org/bots/api

   Three primitives are enough for a chat bot:
   - `get-updates`      long-poll for incoming messages
   - `send-message!`    send text back as HTML (auto-splits at 4096-char limit)
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

(def ^:private mdv2-specials
  #{\\ \_ \* \[ \] \( \) \~ \` \> \# \+ \- \= \| \{ \} \. \!})

(defn escape-markdown-v2
  "Escape Telegram MarkdownV2 special characters in plain text segments."
  [^String text]
  (let [len (.length text)]
    (loop [i  (int 0)
           sb (StringBuilder. (+ len 16))]
      (if (>= i len)
        (.toString sb)
        (let [ch (.charAt text i)]
          (when (contains? mdv2-specials ch)
            (.append sb \\))
          (.append sb ch)
          (recur (inc i) sb))))))

(defn- escape-markdown-v2-code
  "Escape code content inside MarkdownV2 inline/fenced code."
  [^String text]
  (-> text
    (str/replace "\\" "\\\\")
    (str/replace "`" "\\`")))

(defn- advance-to-line-end [^String text start]
  (or (str/index-of text \newline start) (.length text)))

(defn- convert-md-to-markdown-v2
  "Best-effort normalization of common markdown to Telegram MarkdownV2.

   Handles:
   - fenced code blocks (```...```)
   - inline code (`...`)
   - bold (**...**)
   - italic (_..._ or *...*)
   - strikethrough (~~...~~)
   - blockquote lines (> ...)
   - plain text escaped for MarkdownV2

   Only processes formatting outside fenced code blocks."
  [^String text]
  (let [len (.length text)]
    (loop [i   (int 0)
           sb  (StringBuilder. (* 2 len))]
      (if (>= i len)
        (.toString sb)
        (cond
          ;; Fenced code block: ```lang\n...\n```
          (.startsWith text "```" i)
          (let [after-fence (+ i 3)
                lang-end    (advance-to-line-end text after-fence)
                code-start  (if (< lang-end len) (inc lang-end) lang-end)
                close-idx   (str/index-of text "```" code-start)]
            (if close-idx
              (let [code (subs text code-start close-idx)]
                (.append sb "```\n")
                (.append sb (escape-markdown-v2-code code))
                (.append sb "\n```")
                (recur (+ close-idx 3) sb))
              (do
                (.append sb "```\n")
                (.append sb (escape-markdown-v2-code (subs text code-start)))
                (.append sb "\n```")
                (recur len sb))))

          ;; Inline code: `...`
          (= (.charAt text i) \`)
          (let [close (str/index-of text \` (inc i))]
            (if close
              (let [inner (subs text (inc i) close)]
                (.append sb "`")
                (.append sb (escape-markdown-v2-code inner))
                (.append sb "`")
                (recur (inc close) sb))
              (do (.append sb (escape-markdown-v2 "`"))
                (recur (inc i) sb))))

          ;; Strikethrough: ~~...~~
          (.startsWith text "~~" i)
          (let [close (str/index-of text "~~" (+ i 2))]
            (if (and close (not= close (+ i 2)))
              (let [inner (subs text (+ i 2) close)]
                (.append sb "~~")
                (.append sb (escape-markdown-v2 inner))
                (.append sb "~~")
                (recur (+ close 2) sb))
              (do (.append sb (escape-markdown-v2 "~"))
                (recur (inc i) sb))))

          ;; Bold: **...**
          (.startsWith text "**" i)
          (let [close (str/index-of text "**" (+ i 2))]
            (if (and close (not= close (+ i 2)))
              (let [inner (subs text (+ i 2) close)]
                (.append sb "**")
                (.append sb (escape-markdown-v2 inner))
                (.append sb "**")
                (recur (+ close 2) sb))
              (do (.append sb (escape-markdown-v2 "*"))
                (recur (inc i) sb))))

          ;; Italic: *...* (single star, not double)
          (= (.charAt text i) \*)
          (let [close (str/index-of text \* (inc i))]
            (if (and close (> close (inc i)))
              (let [inner (subs text (inc i) close)]
                (.append sb "*")
                (.append sb (escape-markdown-v2 inner))
                (.append sb "*")
                (recur (inc close) sb))
              (do (.append sb (escape-markdown-v2 "*"))
                (recur (inc i) sb))))

          ;; Italic: _..._ (single underscore)
          (= (.charAt text i) \_)
          (let [close (some-> (str/index-of text \_ (inc i)))]
            (if (and close (> close (inc i)))
              (let [inner (subs text (inc i) close)]
                (.append sb "_")
                (.append sb (escape-markdown-v2 inner))
                (.append sb "_")
                (recur (inc close) sb))
              (do (.append sb (escape-markdown-v2 "_"))
                (recur (inc i) sb))))

          ;; Blockquote line: > ...
          (and (= (.charAt text i) \>)
            (or (zero? i) (= (.charAt text (dec i)) \newline)))
          (let [line-end (advance-to-line-end text i)
                prefix-len (if (.startsWith text "> " i) 2 1)
                quote-text (subs text (+ i prefix-len) line-end)]
            (.append sb "> ")
            (.append sb (escape-markdown-v2 quote-text))
            (recur (if (< line-end len) (inc line-end) line-end) sb))

          :else
          (do (.append sb (escape-markdown-v2 (str (.charAt text i))))
            (recur (inc i) sb)))))))

(defn- chunk-text
  "Split text into <=4000-char pieces along paragraph/line boundaries so we
   don't break HTML tags mid-element. Telegram's hard limit is 4096."
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
  "Send a text reply using Telegram MarkdownV2 parse mode.
   Falls back to plain text if Telegram rejects the payload.
   Auto-splits at 4096 chars."
  [token chat-id text]
  (doseq [raw-chunk (chunk-text (or text ""))]
    (let [md-chunk (convert-md-to-markdown-v2 raw-chunk)
          payload {"chat_id"    chat-id
                   "text"       md-chunk
                   "parse_mode" "MarkdownV2"}
          resp    (post-json! token "/sendMessage" payload)]
      (when-not (:ok resp)
        (post-json! token "/sendMessage"
          {"chat_id" chat-id
           "text" raw-chunk})))))

(defn send-chat-action!
  "Show a transient indicator in the chat (e.g. 'typing…'). Best-effort: errors
   are swallowed because it's UX decoration, not correctness."
  [token chat-id action]
  (try
    (post-json! token "/sendChatAction"
      {"chat_id" chat-id "action" action})
    (catch Exception _ nil)))
