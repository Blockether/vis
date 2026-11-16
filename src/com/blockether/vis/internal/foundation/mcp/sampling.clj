(ns com.blockether.vis.internal.foundation.mcp.sampling
  "Sampling of MCP: in the middle of a request, a server asks the model of the
   client for one answer. The person of the calling session sees the request first
   and allows or refuses it; nothing goes to a model without that. Vis sends text
   only: it declares no tools and no context for sampling."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.view.core :as view]))

(def ^:private shown-chars "How much of one message the person sees." 2000)

(defonce ^:private model (atom nil))

(defn install-model!
  "Install the function of `{:prompt :system}` that gives one model answer as
   `{:text :model}`. The router is part of the agent loop, a higher layer, so the
   wiring fills this slot."
  [f]
  (reset! model f)
  nil)

(defn- text-of
  "The text of the `content` of one sampling message: one block or a list of
   blocks. A block that is not text is named, because the model does not get it."
  [content]
  (str/join "\n"
            (for [block (if (map? content) [content] content)]
              (if (= "text" (get block "type"))
                (str (get block "text"))
                (str "[" (get block "type") " content left out]")))))

(defn- turns
  "The messages of `params` as pairs of speaker and text."
  [params]
  (mapv (fn [message]
          [(if (= "assistant" (get message "role")) "Assistant" "User")
           (text-of (get message "content"))])
        (get params "messages")))

(defn- prompt
  "The one prompt for `turns`: the text of a single message, else the labelled
   conversation."
  [turns]
  (if (= 1 (count turns))
    (second (first turns))
    (str/join "\n\n"
              (map (fn [[speaker text]]
                     (str speaker ": " text))
                   turns))))

(defn- shown
  [text]
  (let [text (str/trim (str text))]
    (if (> (count text) (long shown-chars)) (str (subs text 0 shown-chars) " ...") text)))

(defn ask!
  "Show the sampling request `params` of MCP server `server` to the person of
   `session-id`. When the person allows it, a model answers and the value is the
   result of `sampling/createMessage`. Returns nil when the person refuses or
   nobody answers. Without an installed model the request is refused, and the
   person is not asked."
  [session-id server params]
  (when-let [answer-with @model]
    (let [system (str/trim (str (get params "systemPrompt")))
          turns (turns params)
          answer (view/request!
                   {:title (str "MCP server " server " asks the model for an answer")
                    :session-id (str session-id)
                    :submit-label "Allow"
                    :cancel-label "Decline"
                    :nodes (into (if (str/blank? system)
                                   []
                                   [{:type "paragraph" :text (str "System: " (shown system))}])
                                 (map (fn [[speaker text]]
                                        {:type "paragraph" :text (str speaker ": " (shown text))}))
                                 turns)})]

      (when (and (:is-submitted answer) (seq turns))
        (let [reply (answer-with (cond-> {:prompt (prompt turns)}
                                   (not (str/blank? system))
                                   (assoc :system system)))]
          {"role" "assistant"
           "content" {"type" "text" "text" (:text reply)}
           "model" (str (or (:model reply) "vis"))
           "stopReason" "endTurn"})))))
