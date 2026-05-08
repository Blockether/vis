(ns com.blockether.vis.ext.voice.rewrite
  "Speech-transcript cleanup for Vis prompts. Uses Vis' dynamic helper LLM
   selector; no voice-specific rewrite model is persisted."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]))

(def ^:private rewrite-system
  "You rewrite speech transcripts into concise Vis user prompts. Use context only to resolve pronouns, filenames, symbols, and ambiguous terms. Do not solve the task. Do not add requirements. Preserve profanity/emphasis when it carries meaning. Return only the rewritten prompt.")

(defn rewrite-transcript!
  "Rewrite raw ASR text into a clean prompt. Blank input returns blank."
  [transcript]
  (if (str/blank? transcript)
    ""
    (:text (vis/llm-text!
             {:system rewrite-system
              :prompt (str "Transcript:\n" transcript "\n\nRewrite as the user's prompt.")
              :reasoning :off
              :temperature 0}))))
