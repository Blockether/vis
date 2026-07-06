(ns com.blockether.vis.internal.error
  "Error formatting - leaf module.

   Pure functions for turning exceptions, anomaly maps, and ad-hoc
   error values into human-readable strings. Lives in its own
   namespace so any layer (SDK facade, channels, extensions, the
   iteration loop) can format an error without dragging in the rest
   of the SDK.

   Public API:
     `error-message`                 - raw text from a Throwable / map / string
     `format-error`                  - prefix \"ERROR: \" idempotently
     `final-answer-code-error-message` - the \"Final-answer code error: ...\" prefix
                                        used by the iteration loop when an
                                        `(done ...)` form's own code throws."
  (:require [clojure.string :as str]))

(defn error-message
  "Build an error message string from a Throwable, map, or string."
  [v]
  (cond (instance? Throwable v) (or (.getMessage ^Throwable v) (str v))
        (map? v) (or (:message v) (:msg v) (pr-str v))
        (string? v) v
        :else (pr-str v)))

(defn format-error
  "Add the standard `ERROR: ` prefix to an error message, idempotent."
  [v]
  (let [s (error-message v)]
    (if (str/starts-with? s "ERROR:") s (str "ERROR: " s))))

(defn final-answer-code-error-message
  [exception]
  (str "Final-answer code error: " (error-message exception)))
