(ns com.blockether.vis.channels.web.dictation
  "Dictation refinement for the web adapter.

   Browser-side captures speech via the Web Speech API and sends the raw
   transcript here. We make a single one-shot LLM call (via the shared
   router, NOT via `conversations/send!`) to clean up the transcript using
   the recent conversation as context — fixing speech-to-text errors,
   punctuation, and capitalization while preserving the user's intent.

   Hard guarantees:
   - This call NEVER creates a `:query` or `:iteration` entity.
   - It never mutates the conversation env or messages cache.
   - On any failure (LLM error, blank response, etc.) the raw transcript
     is returned unchanged so the user is never left empty-handed."
  (:require [clojure.string :as str]
            [com.blockether.vis.channels.web.conversations :as web-conversations]
            [com.blockether.vis.loop.runtime.conversation.environment.query.base :as rlm-routing]
            [taoensso.trove :as trove]))

(def ^:private CLEANUP_SYSTEM_PROMPT
  (str "You clean up text that was just dictated by speech-to-text. "
       "Fix obvious recognition errors, add punctuation, and fix capitalization. "
       "Use the prior conversation as context to resolve ambiguous references "
       "and to recognize domain-specific terms (file names, function names, "
       "proper nouns the user has just been discussing). "
       "Preserve the user's intent and voice. Do NOT answer, summarize, "
       "translate, or expand on the message. Do NOT add commentary, "
       "preamble, or quotes. Reply with ONLY the cleaned-up text."))

(def ^:private CONTEXT_MESSAGE_LIMIT
  "How many of the most recent conversation messages to include as context
   for the cleanup. Small on purpose — we only need enough recent history to
   bias the model toward the user's current vocabulary."
  6)

(defn- ->context-messages
  "Project the recent conversation messages into the OpenAI-style message
   maps that `rlm-routing/ask!` accepts. Empty / blank text is dropped so
   the LLM never sees `nil` content."
  [messages]
  (into []
    (keep (fn [{:keys [role text]}]
            (let [t (str text)]
              (when (seq (str/trim t))
                {:role    (case role
                            :user      "user"
                            :assistant "assistant"
                            "user")
                 :content t}))))
    messages))

(defn- recent-context
  "Last `CONTEXT_MESSAGE_LIMIT` messages from the cache, projected. Pure
   read — does not trigger an LLM call or env mutation."
  [conversation-id]
  (->> (web-conversations/messages-for conversation-id)
       (take-last CONTEXT_MESSAGE_LIMIT)
       ->context-messages))

(defn- build-messages
  "Assemble the full prompt: system instruction, recent conversation as
   context, then the dictated text wrapped with a clear marker so the LLM
   cannot confuse it with the conversation itself."
  [context-msgs raw-text]
  (-> [{:role "system" :content CLEANUP_SYSTEM_PROMPT}]
      (into context-msgs)
      (conj {:role    "user"
             :content (str "Dictated text to clean up (return only the cleaned version, "
                           "in the same language):\n\n" raw-text)})))

(defn cleanup-dictation
  "Take a raw speech-to-text transcript and return a refined version.

   Calls the shared LLM router with `:prefer :speed` so svar picks the
   cheapest/fastest configured chat model. Never creates an entity row;
   never goes through `conversations/send!`. On any failure returns the
   raw input unchanged.

   `conversation-id` may be nil — in that case no conversation context is
   attached and the LLM works from the dictated text alone."
  [conversation-id raw-text]
  (let [raw (str raw-text)]
    (if (str/blank? raw)
      raw
      (try
        (let [ctx      (when conversation-id (recent-context conversation-id))
              messages (build-messages (or ctx []) raw)
              {:keys [result]} (rlm-routing/ask!
                                {:messages     messages
                                 :spec         {:cleaned {:type        :string
                                                          :description "The cleaned-up dictated text, same language as input. No quotes, no commentary."}}
                                 :prefer       :speed
                                 :capabilities #{:chat}})
              cleaned  (str/trim (str (:cleaned result)))]
          (if (str/blank? cleaned)
            raw
            cleaned))
        (catch Exception e
          (trove/log! {:level :warn :id ::cleanup-failed
                       :data {:error (ex-message e)}
                       :msg "dictation cleanup failed"})
          raw)))))
