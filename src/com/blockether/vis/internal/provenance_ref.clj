(ns com.blockether.vis.internal.provenance-ref
  "Pure canonical provenance reference parser/formatter.

   References are durable evidence addresses. Channels may render labels or
   Markdown from these helpers, but writer inputs and persisted proof refs use
   the canonical string returned by `format-ref`."
  (:require
   [clojure.string :as str]))

(def ^:private id-prefix-pattern #"(?i)[0-9a-f]{8}")
(def ^:private tool-id-pattern #"[A-Za-z0-9_.:-]+")

(def canonical-ref-pattern
  (re-pattern
    (str "(?i)^(?:conversation/([0-9a-f]{8})/)?"
      "turn/([0-9a-f]{8})/"
      "iteration/([1-9][0-9]*)/"
      "block/([1-9][0-9]*)"
      "(?:/(tool/([A-Za-z0-9_.:-]+)|error))?$")))

(defn canonical-ref?
  "True when `s` exactly matches the canonical provenance reference grammar."
  [s]
  (boolean (and (string? s) (re-matches canonical-ref-pattern s))))

(defn parse-ref
  "Parse a canonical provenance reference into data.

   Returns nil for non-canonical references. Compact aliases such as `i4.2`,
   `i4.2/tool`, `E1`, or `G1` are intentionally rejected."
  [s]
  (when-let [[_ conversation-prefix turn-prefix iteration block child tool-id]
             (and (string? s) (re-matches canonical-ref-pattern s))]
    (cond-> {:scope       (if conversation-prefix :conversation :turn)
             :turn-prefix (str/lower-case turn-prefix)
             :iteration   (parse-long iteration)
             :block       (parse-long block)}
      conversation-prefix (assoc :conversation-prefix (str/lower-case conversation-prefix))
      child (assoc :child (if (= "error" (str/lower-case child))
                            {:kind :error}
                            {:kind :tool :op tool-id})))))

(defn- required-prefix! [k v]
  (when-not (and (string? v) (re-matches id-prefix-pattern v))
    (throw (ex-info (str "Expected " (name k) " to be an 8-character hex prefix")
             {:key k :value v})))
  (str/lower-case v))

(defn- positive-int! [k v]
  (when-not (pos-int? v)
    (throw (ex-info (str "Expected " (name k) " to be a positive integer")
             {:key k :value v})))
  v)

(defn- tool-id! [tool-id]
  (when-not (and (string? tool-id) (re-matches tool-id-pattern tool-id))
    (throw (ex-info "Tool id must be one slash-free path segment"
             {:tool-id tool-id})))
  tool-id)

(defn format-ref
  "Format canonical provenance reference data.

   Required keys: `:turn-prefix`, `:iteration`, `:block`.
   Optional keys: `:conversation-prefix`, `:child`.
   Child forms: `{:kind :tool :op 'bash'}` or `{:kind :error}`."
  [{:keys [conversation-prefix turn-prefix iteration block child]}]
  (let [turn-prefix (required-prefix! :turn-prefix turn-prefix)
        iteration   (positive-int! :iteration iteration)
        block       (positive-int! :block block)
        base        (str (when conversation-prefix
                           (str "conversation/" (required-prefix! :conversation-prefix conversation-prefix) "/"))
                      "turn/" turn-prefix "/iteration/" iteration "/block/" block)]
    (case (:kind child)
      nil base
      :error (str base "/error")
      :tool  (str base "/tool/" (tool-id! (or (:op child) (:tool-id child))))
      (throw (ex-info "Unsupported provenance child kind" {:child child})))))

(defn display-ref
  "Return presentation data for a canonical provenance reference.

   The canonical reference remains the copy/export form. Labels are display-only
   and must not be persisted as provenance."
  [canonical-ref]
  (when-let [{:keys [turn-prefix iteration block child] :as parsed} (parse-ref canonical-ref)]
    (let [child-label (case (:kind child)
                        nil nil
                        :error "error"
                        :tool (:op child))
          label       (str "T" turn-prefix " · i" iteration "." block
                        (when child-label (str " · " child-label)))
          short       (str "i" iteration "." block
                        (when child-label (str "/" child-label)))]
      {:canonical (format-ref parsed)
       :label     label
       :short     short
       :markdown  (str "`" label "`")})))

(defn display-provenance
  "Display helper for a provenance map containing at least `:ref`.
   Returns nil when the ref is not canonical."
  [{:keys [ref op status duration-ms parent-ref]}]
  (when-let [display (display-ref ref)]
    (cond-> (assoc display :op op :status status)
      duration-ms (assoc :duration-ms duration-ms)
      parent-ref  (assoc :parent-ref parent-ref))))
