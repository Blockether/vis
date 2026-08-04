(ns com.blockether.vis.internal.human-input.validation
  "Field validation for human-input requests: RULES are DATA, a validator is a
   FUNCTION, and both compose.

   A field carries `:validate` — one rule or a list of them. Each rule is a small
   map with exactly one thing it checks plus an optional `:message`
   (`{\"pattern\" \"^[A-Z]{2}-\\\\d+$\"}`, `{\"type\" \"email\"}`, `{\"min_length\" 8}`,
   `{\"matches\" \"password\"}`), or the bare name of a [[value-types]] shape
   (`\"email\"`). Rules run in the order they were written and the FIRST one that
   fails is the field's error message, so a spec reads like a checklist.

   Rules are data because they have to cross the wire. The engine is the
   authority — it re-checks everything [[com.blockether.vis.internal.human-input/submit!]]
   receives — but the TUI band and the companion app run the SAME rules from the
   request view as the operator types, which is the only way a surface can show
   an error the engine agrees with instead of a green field it is about to
   reject.

   A rule NEVER fires on a blank value. Emptiness is `:is-required`'s single
   job; a pattern that also rejected \"\" would quietly make every optional field
   mandatory.

   A Clojure caller may hand a plain FUNCTION as a rule: it takes the coerced
   value and answers nil/true when the value is fine, or the error message as a
   string. Functions cannot be serialized, so they run engine-side only and are
   dropped from the request view by [[wire-rules]] — a surface simply has one
   fewer rule to run, and the engine still refuses the submit."
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def value-types
  "Named `{\"type\" …}` shapes — the everyday formats a spec should not have to
   spell as a regex. `:re` matches the WHOLE trimmed value; `:pred` sees the
   coerced value itself, so a number stays a number."
  {"email" {:re #"[^@\s]+@[^@\s]+\.[^@\s]+" :message "must be an email address"}
   "url" {:re #"(?i)https?://\S+" :message "must be a URL"}
   "uuid" {:re #"(?i)[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"
           :message "must be a UUID"}
   "digits" {:re #"[0-9]+" :message "must be digits only"}
   "alpha" {:re #"[A-Za-z]+" :message "must be letters only"}
   "alphanumeric" {:re #"[A-Za-z0-9]+" :message "must be letters and digits only"}
   "slug" {:re #"[a-z0-9]+(?:-[a-z0-9]+)*"
           :message "must be a slug — lowercase words joined by dashes"}
   "integer" {:pred #(if (number? %) (integer? %) (some? (parse-long (str/trim (str %)))))
              :message "must be a whole number"}
   "number" {:pred #(if (number? %) true (some? (parse-double (str/trim (str %)))))
             :message "must be a number"}})

(def ^:private rule-keys
  "Every key ONE rule map may carry, in its canonical snake_case spelling."
  #{"pattern" "type" "min_length" "max_length" "min" "max" "matches" "message"})

(defn- pick
  "First non-nil value among `ks` — the same string-or-keyword spec tolerance the
   rest of the human-input boundary has."
  [m & ks]
  (reduce (fn [_ k]
            (let [v (get m k)]
              (if (some? v) (reduced v) nil)))
          nil
          ks))

(defn- text-of [value] (str/trim (str value)))

(defn- trimmed
  [value]
  (when-not (coll? value)
    (some-> value
            str
            str/trim
            not-empty)))

(defn- number-of
  [fail! label value]
  (if (number? value)
    value
    (or (parse-long (text-of value))
        (parse-double (text-of value))
        (fail! (str label " must be a number")))))

(defn- normalize-map-rule
  [rule fail!]
  (doseq [k (keys rule)]
    (let [canonical (str/replace (name k) "-" "_")]
      (when-not (contains? rule-keys canonical)
        (fail! (str "unknown :validate rule key " (pr-str k)
                    " — expected one of " (str/join ", " (sort rule-keys)))))
      (when-not (or (= k canonical) (= k (keyword (str/replace canonical "_" "-"))))
        (fail! (str "spell the :validate rule key " (pr-str k)
                    " as " (pr-str canonical)
                    " or " (pr-str (keyword (str/replace canonical "_" "-"))))))))
  (let
    [message
     (trimmed (pick rule "message" :message))

     pattern
     (trimmed (pick rule "pattern" :pattern))

     type-name
     (some-> (trimmed (pick rule "type" :type))
             str/lower-case)

     min-length
     (pick rule "min_length" :min-length)

     max-length
     (pick rule "max_length" :max-length)

     lo
     (pick rule "min" :min)

     hi
     (pick rule "max" :max)

     matches
     (trimmed (pick rule "matches" :matches))

     length
     (fn [bound-key raw noun]
       (let [n (long (number-of fail! (str ":" (name bound-key) "_length") raw))]
         (when-not (pos? n) (fail! (str ":" (name bound-key) "_length must be positive")))
         {:kind :length
          bound-key n
          :message (or message (str "must be " noun " " n " character" (when (not= 1 n) "s")))}))

     chosen
     (remove nil?
       [(when pattern :pattern) (when type-name :type) (when (some? min-length) :min-length)
        (when (some? max-length) :max-length) (when (some? lo) :min) (when (some? hi) :max)
        (when matches :matches)])]

    (when-not (= 1 (count chosen))
      (fail! (str "each :validate rule checks exactly one thing — "
                  (if (empty? chosen)
                    "this one checks nothing"
                    (str "this one mixes "
                         (str/join " and " (map name chosen))
                         "; write them as separate rules")))))
    (case (first chosen)
      :pattern
      (do (try (re-pattern pattern)
               (catch java.util.regex.PatternSyntaxException e
                 (fail! (str "invalid :pattern " (pr-str pattern) " — " (ex-message e)))))
          {:kind :pattern :pattern pattern :message (or message "must match the expected format")})

      :type
      (let [known (get value-types type-name)]
        (when-not known
          (fail! (str "unknown :validate type " (pr-str type-name)
                      " — expected one of " (str/join ", " (sort (keys value-types))))))
        {:kind :type :type type-name :message (or message (:message known))})

      :min-length
      (length :min min-length "at least")

      :max-length
      (length :max max-length "at most")

      :min
      (let [n (number-of fail! ":min" lo)]
        {:kind :bounds :min n :message (or message (str "must be at least " n))})

      :max
      (let [n (number-of fail! ":max" hi)]
        {:kind :bounds :max n :message (or message (str "must be at most " n))})

      :matches
      {:kind :matches
       :field matches
       ;; Filled in with the other field's LABEL by [[resolve-matches]]
       ;; once the request knows every field; on its own a rule cannot
       ;; see its siblings.
       :message (or message (str "must match " matches))
       :is-message-default (nil? message)})))

(defn- normalize-rule
  [rule fail!]
  (cond (or (string? rule) (keyword? rule)) (normalize-map-rule {"type" (name rule)} fail!)
        ;; `#"^\d{3}$"` on its own is the shortest honest way to write a pattern
        ;; rule, and `str` on a Pattern is its source, so it never reaches the
        ;; wire as an object.
        (instance? java.util.regex.Pattern rule) (normalize-map-rule {"pattern" (str rule)} fail!)
        (map? rule) (normalize-map-rule rule fail!)
        (coll? rule) (fail! "a :validate rule is a rule map, a type name, or a function")
        (ifn? rule) {:kind :fn :fn rule}
        :else (fail! "a :validate rule is a rule map, a type name, or a function")))

(defn normalize-rules
  "`raw` — one rule or a list of them — as the field's normalized rule vector, or
   nil when the field declares none. `fail!` reports a bad rule the way the
   surrounding spec reports every other mistake."
  [raw fail!]
  (cond (nil? raw) nil
        (sequential? raw) (not-empty (mapv #(normalize-rule % fail!) raw))
        :else [(normalize-rule raw fail!)]))

(defn resolve-matches
  "Point every `:matches` rule in `rules` at a REAL field and name it the way the
   operator sees it. A confirmation field that guards a typo'd field name is the
   one validation bug nobody notices, so an unknown target is refused here
   instead of quietly never failing."
  [rules fields fail!]
  (when rules
    (mapv (fn [{:keys [kind field] :as rule}]
            (if (not= :matches kind)
              rule
              (let [target (first (filter #(= field (:name %)) fields))]
                (when-not target
                  (fail! (str ":matches names no field of this request: " (pr-str field))))
                (cond-> (dissoc rule :is-message-default)
                  (:is-message-default rule)
                  (assoc :message (str "must match " (:label target)))))))
          rules)))

(defn wire-rules
  "The rules a SURFACE can run: everything but the engine-only functions."
  [rules]
  (not-empty (into [] (remove #(= :fn (:kind %))) rules)))

(defn- blank-value?
  "Nothing to check. Rules describe the SHAPE of an answer; whether there is one
   at all is `:is-required`'s single job."
  [value]
  (cond (nil? value) true
        (string? value) (str/blank? value)
        (coll? value) (empty? value)
        :else false))

(defn- check-rule
  [{:keys [kind message] :as rule} value values]
  (case kind
    :type
    (let [{:keys [re pred]} (get value-types (:type rule))]
      (when-not (if pred (pred value) (boolean (re-matches re (text-of value)))) message))

    ;; `re-find`, not `re-matches`: a pattern is a CONSTRAINT the answer has to
    ;; satisfy somewhere, so `[0-9]` reads as "needs a digit" the way everyone
    ;; writing it means it. Anchor it with `^…$` when the whole value must match.
    :pattern
    (when-not (re-find (re-pattern (:pattern rule)) (text-of value)) message)

    :length
    (let [n (count (text-of value))]
      (when (or (and (:min rule) (< n (long (:min rule))))
                (and (:max rule) (> n (long (:max rule)))))
        message))

    :bounds
    (let
      [n (if (number? value)
           value
           (let [s (text-of value)]
             (or (parse-long s) (parse-double s))))]
      (cond (nil? n) "must be a number"
            (and (:min rule) (< (double n) (double (:min rule)))) message
            (and (:max rule) (> (double n) (double (:max rule)))) message))

    :matches
    (when-not (= (text-of value) (text-of (get values (:field rule)))) message)

    :fn
    (try (let [verdict ((:fn rule) value)]
           (cond (or (nil? verdict) (true? verdict)) nil
                 (false? verdict) (or message "is not valid")
                 (string? verdict) (not-empty (str/trim verdict))
                 :else (str verdict)))
         (catch Throwable t
           ;; A validator that throws still has to produce a verdict: swallowing
           ;; it would ACCEPT the value it was written to refuse.
           (str "could not be validated: " (or (ex-message t) (str t)))))

    nil))

(defn check
  "The first message a rule has against `value`, or nil when the value is fine.
   `values` is the whole coerced `field id -> value` map, which is what a
   `:matches` rule reads."
  [rules value values]
  (when-not (blank-value? value) (some #(check-rule % value values) rules)))
