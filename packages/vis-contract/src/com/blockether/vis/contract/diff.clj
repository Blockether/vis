(ns com.blockether.vis.contract.diff
  "Versioned code diffs keep exact patch text separate from human review comments."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]
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

(defn with-comments
  "Replace only the comments; accepts keyword or string-keyed quote/body maps."
  [envelope comments]
  (let [updated (assoc envelope "comments" (wire/->wire comments))]
    (when-not (valid? updated) (throw (ex-info "Invalid diff comments" {:type :diff/invalid})))
    updated))

(defn review-request
  "Request review of one exact saved version, never authorize new implementation scope."
  [filename version]
  (when-not
    (and (string? filename) (not (str/blank? filename)) (integer? version) (pos? (long version)))
    (throw (ex-info "Invalid diff filename or version" {:type :diff/invalid})))
  (str "Read `" filename
       "` v" version
       " with read_attachment(" (pr-str filename)
       ", version=" version
       ").\n" (get-in @schema ["$defs" "envelope" "x-vis-review-request"])))
