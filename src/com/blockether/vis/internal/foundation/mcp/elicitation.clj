(ns com.blockether.vis.internal.foundation.mcp.elicitation
  "Form elicitation of MCP: in the middle of a tool call, a server asks the person
   of the calling session for values. The requested schema becomes one input View,
   and the answer of the person becomes the `content` of the reply. The schema is
   flat: text, number, boolean and choice properties."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.view.core :as view]))

(defn- choices
  "The choices of `schema` as View options, or nil when it has none: `oneOf` or
   `anyOf` constants with titles, else `enum` with the optional `enumNames`."
  [schema]
  (or (seq (for [choice
                 (or (get schema "oneOf") (get schema "anyOf"))

                 :let [value
                       (get choice "const")]
                 :when (string? value)]

             {:value value :label (or (get choice "title") value)}))
      (seq (map (fn [value label]
                  {:value value :label (or label value)})
                (filter string? (get schema "enum"))
                (concat (get schema "enumNames") (repeat nil))))))

(defn- number-of
  "The number that `text` writes for a property of JSON `type`, or nil."
  [type text]
  (let [text (str/trim (str text))]
    (cond (re-matches #"[+-]?\d+" text) (parse-long text)
          (= "integer" type) nil
          (re-matches #"[+-]?(\d+\.?\d*|\.\d+)([eE][+-]?\d+)?" text) (parse-double text))))

(defn- number-check
  "The validator of a number field: nil, or what is wrong with the text."
  [{type "type" lowest "minimum" highest "maximum"}]
  (fn [text]
    (let [n (number-of type text)]
      (cond (nil? n) (if (= "integer" type) "must be a whole number" "must be a number")
            (and (number? lowest) (< (double n) (double lowest))) (str "must be at least " lowest)
            (and (number? highest) (> (double n) (double highest))) (str "must be at most "
                                                                         highest)))))

(def ^:private format-hints
  {"date" "YYYY-MM-DD"
   "date-time" "YYYY-MM-DDThh:mm:ssZ"
   "email" "name@example.com"
   "uri" "https://example.com"})

(defn- node
  "The View field for property `field` of the requested schema."
  [field schema required?]
  (let [type
        (get schema "type")

        options
        (vec (choices (if (= "array" type) (get schema "items") schema)))

        allowed
        (set (map :value options))

        default
        (get schema "default")

        base
        (cond-> {:name field :label (or (get schema "title") field) :is-required required?}
          (not (str/blank? (str (get schema "description"))))
          (assoc :description (get schema "description")))]

    (cond (and (= "array" type) (seq options)) (assoc base
                                                 :type "multiselect"
                                                 :options options
                                                 :default (filterv allowed
                                                            (if (sequential? default) default [])))
          (seq options) (cond-> (assoc base
                                  :type "select"
                                  :options options)
                          (contains? allowed default)
                          (assoc :default default))
          ;; A required boolean must be there, not be true: `false` is an answer.
          (= "boolean" type) (assoc base
                               :type "checkbox"
                               :is-required false
                               :default (true? default))
          (contains? #{"number" "integer"} type) (cond-> (assoc base
                                                           :type "plaintext"
                                                           :validate (number-check schema))
                                                   (number? default)
                                                   (assoc :default (str default)))
          :else (cond-> (assoc base :type "plaintext")
                  (pos-int? (get schema "minLength"))
                  (assoc :min-length (get schema "minLength"))

                  (pos-int? (get schema "maxLength"))
                  (assoc :max-length (get schema "maxLength"))

                  (contains? format-hints (get schema "format"))
                  (assoc :placeholder (get format-hints (get schema "format")))

                  (and (string? default) (not (str/blank? default)))
                  (assoc :default default)))))

(defn- content
  "The values of the person as the `content` of the reply, each in the JSON type
   of its property. `properties` holds the pairs of field and schema. A blank
   optional answer is left out."
  [properties values]
  (reduce (fn [content [field schema]]
            (let [value
                  (get values field)

                  type
                  (get schema "type")]

              (cond (= "boolean" type) (assoc content field (true? value))
                    (= "array" type) (assoc content field (vec value))
                    (contains? #{"number" "integer"} type) (if-some [n (number-of type value)]
                                                             (assoc content field n)
                                                             content)
                    (str/blank? (str value)) content
                    :else (assoc content field value))))
          {}
          properties))

(defn ask!
  "Show the form elicitation `params` of MCP server `server` to the person of
   `session-id`. Returns the result of the reply: `accept` with the content,
   `decline` when the person refused, `cancel` when nobody answered. An `accept`
   has `\"fields\"`, the names of the fields that the form showed: the client
   puts no default back into a field that the person saw and left empty."
  [session-id server params]
  (let [schema
        (get params "requestedSchema")

        ;; Pairs, not a map: a map of more than eight fields loses their order.
        properties
        (filterv (comp map? val) (get schema "properties"))

        required
        (set (get schema "required"))

        message
        (str/trim (str (get params "message")))

        answer
        (view/request! (cond-> {:title (str "MCP server " server " asks for input")
                                :session-id (str session-id)
                                :submit-label "Send"
                                :cancel-label "Decline"
                                :nodes (if (seq properties)
                                         (mapv (fn [[field property]]
                                                 (node field property (contains? required field)))
                                               properties)
                                         ;; A request with no field is a question: send or decline.
                                         [{:type "paragraph"
                                           :text (if (str/blank? message) "Continue?" message)}])}
                         (and (seq properties) (not (str/blank? message)))
                         (assoc :description message)))]

    (cond (:is-submitted answer) {"action" "accept"
                                  "content" (content properties (:values answer))
                                  "fields" (mapv key properties)}
          (= "cancelled" (:reason answer)) {"action" "decline"}
          :else {"action" "cancel"})))
