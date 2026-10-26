(ns com.blockether.vis.contract.diff
  "Versioned code diffs: the exact patch text and the source it came from."
  (:require [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.wire :as wire]))

(def ^:private schema (delay (document/schema-document "diff")))

(def media-type (get-in @schema ["$defs" "attachment" "contentMediaType"]))

(defn valid?
  "True only for a complete, canonical JSON-shaped diff envelope."
  [envelope]
  (document/valid-json? "diff" "envelope" envelope))

(defn parse!
  "Parse JSON text to a validated string-keyed envelope; throw :diff/invalid otherwise."
  [text]
  (let [envelope (when (string? text) (wire/parse-json text))]
    (when-not (valid? envelope) (throw (ex-info "Invalid diff attachment" {:type :diff/invalid})))
    envelope))

(defn render
  "Encode a valid envelope without normalizing or trimming its patch string."
  [envelope]
  (when-not (valid? envelope) (throw (ex-info "Invalid diff attachment" {:type :diff/invalid})))
  (wire/json-str envelope))
