(ns com.blockether.vis-extension.error
  "Shared error-message formatting across core + channels."
  (:require [clojure.string :as str]))

(def ^:private default-error-message
  "Unknown error")

(def ^:private standard-error-prefix
  "ERROR: ")

(defn error-message
  "Return a stable, non-blank, human-readable message for any error value."
  [error-value]
  (let [message (cond
                  (nil? error-value)
                  nil

                  (string? error-value)
                  error-value

                  (instance? Throwable error-value)
                  (or (ex-message error-value)
                    (.toString ^Throwable error-value))

                  (map? error-value)
                  (or (:message error-value)
                    (:error error-value)
                    (some-> (:type error-value) str))

                  :else
                  (str error-value))
        trimmed (some-> message str str/trim)]
    (if (str/blank? trimmed)
      default-error-message
      trimmed)))

(defn format-error
  "Render an error value using the standard channel-facing prefix.

   Idempotent: if the message is already prefixed with `ERROR:`,
   it is returned unchanged."
  [error-value]
  (let [message (error-message error-value)]
    (if (str/starts-with? message standard-error-prefix)
      message
      (str standard-error-prefix message))))

(defn final-answer-code-error-message
  "Validation message when code execution failed before a final answer."
  [error-value]
  (str "Cannot finalize because code execution failed: "
    (error-message error-value)))

(def ^:const final-answer-validation-code-placeholder
  "(final-answer-validation)")
