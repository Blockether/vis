(ns com.blockether.vis.internal.human-input-test
  (:require [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis.internal.channel-events :as ce]
            [com.blockether.vis.internal.human-input :as hi]
            [com.blockether.vis.internal.human-input.spec :as hs]
            [com.blockether.vis.internal.runtime-settings :as rt]
            [lazytest.core :refer [defdescribe expect it throws?]]
            [taoensso.telemere :as tel]))

(defn- signal?
  "True when `signals` carries a signal of `level` and `id` naming `request-id`."
  [signals level id request-id]
  (boolean (some (fn [signal]
                   (and (= level (:level signal))
                        (= id (:id signal))
                        (= request-id (:request-id (:data signal)))))
                 signals)))

(defn- fresh-channel
  "A channel id no other test (or a mounted channel) can collide with — the
   channel event bus is a process-local singleton."
  []
  (keyword "vis-test" (str "human-input-" (random-uuid))))

(defn- spec [& fields] {:title "Deploy" :fields (vec fields) :timeout-ms 5000})

(defn- normalized-fields [& fields] (:fields (hi/normalize-request (apply spec fields))))

(defn- await-request-id
  "Block until `pred-free` sees the request event, then return its request id.
   The waiter runs on another thread, so the event may land a beat later."
  [events]
  (loop [attempts 0]
    (if-let [id (some #(when (= :human-input/request (:op %)) (:request-id %)) @events)]
      id
      (if (< attempts 200)
        (do (Thread/sleep 10) (recur (inc attempts)))
        (throw (ex-info "no :human-input/request event arrived" {}))))))

(defn- start-request!
  "Open `request` on a fresh channel from another thread. Returns
   `{:future … :events … :request-id … :detach! …}` once the dialog event lands."
  [request]
  (let
    [chan
     (fresh-channel)

     events
     (atom [])]

    (ce/add-channel-event-listener! chan ::collector #(swap! events conj %))
    (let
      [fut (future (hi/request! (merge {:session-id "test-session"}
                                       (assoc request :channel-ids [chan]))))]
      {:future fut
       :events events
       :request-id (await-request-id events)
       :detach! #(ce/remove-channel-event-listener! chan ::collector)})))

(defdescribe
  normalize-field-test
  (it "fills the documented defaults"
      (let [[field] (normalized-fields {:id "name"})]
        (expect (= "name" (:id field)))
        (expect (= "name" (:name field)))
        (expect (= :plaintext (:type field)))
        ;; No `:label` in the spec — the field still shows something a human can
        ;; read, never a blank heading.
        (expect (= "name" (:label field)))
        (expect (nil? (:description field)))
        (expect (false? (:is-required field)))
        (expect (false? (:is-secret field)))))
  (it "keys the answer by :name, shows :label, explains with :description"
      (let
        [[field] (normalized-fields {:name "api_key"
                                     :label "API key"
                                     :description "Found on the provider dashboard"})]
        (expect (= "api_key" (:name field)))
        ;; The same string under `:id`, so every surface that has always keyed
        ;; rows, values and errors by `:id` keeps working unchanged.
        (expect (= "api_key" (:id field)))
        (expect (= "API key" (:label field)))
        (expect (= "Found on the provider dashboard" (:description field)))))
  (it "accepts the :id spelling of a name"
      (let [[field] (normalized-fields {:id "note" :description "Free text"})]
        (expect (= "note" (:name field)))
        (expect (= "note" (:id field)))
        (expect (= "Free text" (:description field)))))
  (it "prefers :name when a spec carries both spellings"
      (let [[field] (normalized-fields {:id "old" :name "new"})]
        (expect (= "new" (:id field)))
        (expect (= "new" (:name field)))))
  (it "refuses a field that has a label but no name"
      (expect (throws? clojure.lang.ExceptionInfo
                       #(normalized-fields {:label "Just a label" :description "and prose"}))))
  (it "never draws a collection as prose"
      ;; A JSON client can post anything. `str` on a map or vector yields Clojure
      ;; source no operator can read, so such a label or description is dropped
      ;; like a blank and such a name is refused outright.
      (let [[field] (normalized-fields {:name "note" :label {} :description ["a" "b"]})]
        (expect (= "note" (:label field)))
        (expect (nil? (:description field))))
      (expect (throws? clojure.lang.ExceptionInfo #(normalized-fields {:name {"a" 1}}))))
  (it "keys coerced values by the field name"
      (let [fields (normalized-fields {:name "note" :is-required true})]
        (expect (= {:is-accepted true :values {"note" "hi"}}
                   (hi/coerce-values fields {"note" "hi"})))))
  (it "reads string keys from the Python boundary"
      (let
        [[field] (normalized-fields {"id" "token"
                                     "type" "password"
                                     "label" "API token"
                                     "is_required" true
                                     "max_length" 40
                                     "description" "from the dashboard"})]
        (expect (= "token" (:id field)))
        (expect (= :password (:type field)))
        (expect (= "API token" (:label field)))
        (expect (true? (:is-required field)))
        (expect (true? (:is-secret field)))
        (expect (= 40 (:max-length field)))
        (expect (= "from the dashboard" (:description field)))))
  (it "expands bare option strings into value/label pairs"
      (let [[field] (normalized-fields {:id "env" :type "select" :options ["staging" "prod"]})]
        (expect (= [{:value "staging" :label "staging"} {:value "prod" :label "prod"}]
                   (:options field)))))
  (it "keeps explicit option labels"
      (let
        [[field] (normalized-fields
                   {:id "env" :type "select" :options [{"value" "prod" "label" "Production"}]})]
        (expect (= [{:value "prod" :label "Production"}] (:options field)))))
  (it "validates a declared default against its own field"
      (let [[field] (normalized-fields {:id "env" :type "select" :options ["a" "b"] :default "b"})]
        (expect (= "b" (:default field))))
      (expect (throws? clojure.lang.ExceptionInfo
                       #(normalized-fields
                          {:id "env" :type "select" :options ["a" "b"] :default "zzz"}))))
  (it "rejects malformed field specs"
      (expect (throws? clojure.lang.ExceptionInfo #(normalized-fields {:id "  "})))
      (expect (throws? clojure.lang.ExceptionInfo #(normalized-fields {:id "x" :type "wat"})))
      (expect (throws? clojure.lang.ExceptionInfo #(normalized-fields {:id "x" :type "select"})))
      (expect (throws? clojure.lang.ExceptionInfo #(normalized-fields "not-a-map")))
      (expect (throws? clojure.lang.ExceptionInfo #(normalized-fields {:id "x" :max-length 0}))))
  (it "carries the offending field id in the exception data"
      (let
        [data (try (normalized-fields {:id "env" :type "wat"})
                   (catch clojure.lang.ExceptionInfo e (ex-data e)))]
        (expect (= :vis/human-input-invalid-field (:type data)))
        (expect (= "env" (:field-id data))))))

(defdescribe
  normalize-request-test
  (it "defaults the chrome, channels and timeout"
      (let [request (hi/normalize-request {:title "Deploy" :fields [{:id "name"}]})]
        (expect (string? (:id request)))
        (expect (= "Deploy" (:title request)))
        (expect (= "Submit" (:submit-label request)))
        (expect (= "Cancel" (:cancel-label request)))
        (expect (true? (:is-cancellable request)))
        ;; Both surfaces: the TUI dialog AND the gateway bridge that tells the
        ;; companion app a run is parked on a human.
        (expect (= [:tui :app] (:channel-ids request)))
        (expect (= hi/default-timeout-ms (:timeout-ms request)))))
  (it "rejects a request without a title or fields"
      (expect (throws? clojure.lang.ExceptionInfo #(hi/normalize-request {:fields [{:id "name"}]})))
      (expect (throws? clojure.lang.ExceptionInfo #(hi/normalize-request {:title "t"})))
      (expect (throws? clojure.lang.ExceptionInfo #(hi/normalize-request {:title "t" :fields []}))))
  (it "rejects duplicate field names, however they are spelled"
      (expect (throws? clojure.lang.ExceptionInfo
                       #(hi/normalize-request {:title "t" :fields [{:name "a"} {:name "a"}]})))
      ;; `:id` is the same identity under its legacy name — a spec cannot smuggle
      ;; a collision past this by mixing the two spellings.
      (expect (throws? clojure.lang.ExceptionInfo
                       #(hi/normalize-request {:title "t" :fields [{:name "a"} {:id "a"}]}))))
  (it "keeps 0 as the indefinite wait, never clamps it, and refuses a negative"
      ;; `0` is the one way an extension says WAIT FOR THE HUMAN, so it has to
      ;; survive normalization untouched instead of being read as a missing key.
      (expect (= hi/no-timeout-ms
                 (:timeout-ms (hi/normalize-request
                                {:title "t" :fields [{:id "a"}] :timeout-ms 0}))))
      (expect (true? (hi/indefinite-timeout? (:timeout-ms (hi/normalize-request {:title "t"
                                                                                 :fields [{:id "a"}]
                                                                                 :timeout-ms 0})))))
      (expect (false? (hi/indefinite-timeout? hi/default-timeout-ms)))
      ;; Nothing is CLAMPED: a caller who states a day-long wait means it, and
      ;; quietly shortening it would only lie about when the answer arrives.
      (expect (= (* 24 60 60 1000)
                 (:timeout-ms (hi/normalize-request
                                {:title "t" :fields [{:id "a"}] :timeout-ms (* 24 60 60 1000)}))))
      (expect (throws? clojure.lang.ExceptionInfo
                       #(hi/normalize-request {:title "t" :fields [{:id "a"}] :timeout-ms -1})))
      (expect (throws? clojure.lang.ExceptionInfo
                       #(hi/normalize-request {:title "t" :fields [{:id "a"}] :timeout-ms "soon"})))
      ;; A Python extension says the same thing over the strings boundary, where a
      ;; 0 is easy to mistake for a missing key and default back to five minutes.
      (expect (= hi/no-timeout-ms
                 (:timeout-ms (hi/normalize-request
                                {"title" "t" "fields" [{"name" "a"}] "timeout_ms" 0})))))
  (it "accepts a single channel id or a collection"
      (expect (= [:app]
                 (:channel-ids (hi/normalize-request
                                 {:title "t" :fields [{:id "a"}] :channel-id :app}))))
      (expect (= [:tui :app]
                 (:channel-ids (hi/normalize-request
                                 {:title "t" :fields [{:id "a"}] :channel-ids [:tui :app]})))))
  (it "keeps an explicit false — a false flag is a value, not a missing key"
      ;; `:is-cancellable false` is how an extension says THIS ONE MUST BE
      ;; ANSWERED. Dropping it defaulted the flag back to true and both surfaces
      ;; offered the escape hatch anyway: the TUI painted its Esc hint, the app
      ;; its Cancel button, and either could dismiss a request the extension had
      ;; forbidden dismissing.
      (expect (false? (:is-cancellable (hi/normalize-request
                                         {:title "t" :fields [{:id "a"}] :is-cancellable false}))))
      (expect (false? (:is-cancellable (hi/normalize-request {"title" "t"
                                                              "fields" [{"id" "a"}]
                                                              "is_cancellable" false}))))))

(defn- refusal
  "The message a spec the engine must refuse throws, or nil when it wrongly
   accepted the spec."
  [thunk]
  (try (thunk) nil (catch clojure.lang.ExceptionInfo e (ex-message e))))

(defn- wire-name
  "A normalized key in the snake_case spelling a Python/JSON spec writes."
  [k]
  (str/replace (name k) "-" "_"))

(defn- unknown-keys
  "Of the spec vocabulary `ks`, the ones `normalize` calls an UNKNOWN key when
   `base` is written with them. Always none: the snake_case keys the parser
   accepts are derived from these very sets. A key `base` already carries is
   proven by `base` parsing at all, and any other refusal is somebody else's
   business."
  [ks base normalize]
  (into #{}
        (comp (map wire-name)
              (remove #(contains? base %))
              (filter (fn [k]
                        (boolean (some-> (refusal #(normalize (assoc base k nil)))
                                         (str/includes? "unknown"))))))
        ks))

(defn- await-pending-id
  "Block until a request titled `title` is pending, then return its id."
  [title]
  (loop [attempts 0]
    (if-let [request (first (filter #(= title (:title %)) (hi/pending-requests)))]
      (:id request)
      (if (< attempts 400)
        (do (Thread/sleep 10) (recur (inc attempts)))
        (throw (ex-info "no pending human-input request" {:title title}))))))

(defdescribe
  spec-key-spelling-test
  (it
    "takes the snake_case string spelling a Python spec writes"
    (let
      [request
       (hi/normalize-request {"title" "Deploy"
                              "description" "Pick a target"
                              "submit_label" "Go"
                              "cancel_label" "Stop"
                              "is_cancellable" false
                              "timeout_ms" 20000
                              "session_id" "sid-1"
                              "source" "asker"
                              "fields"
                              [{"name" "env"
                                "label" "Target"
                                "description" "Where it lands."
                                "type" "select"
                                "options" [{"value" "prod" "label" "Production"}]
                                "is_required" true}
                               {"name" "note" "placeholder" "why" "max_length" 5 "default" "ok"}]})

       [env note]
       (:fields request)]

      (expect (= "Go" (:submit-label request)))
      (expect (= "Stop" (:cancel-label request)))
      (expect (false? (:is-cancellable request)))
      (expect (= 20000 (:timeout-ms request)))
      (expect (= "sid-1" (:session-id request)))
      (expect (= "asker" (:source request)))
      (expect (= {:id "env" :name "env" :label "Target" :description "Where it lands."}
                 (select-keys env [:id :name :label :description])))
      (expect (true? (:is-required env)))
      (expect (= [{:value "prod" :label "Production"}] (:options env)))
      (expect (= "why" (:placeholder note)))
      (expect (= 5 (:max-length note)))
      (expect (= "ok" (:default note)))))
  (it "refuses a camelCase key instead of quietly ignoring it"
      ;; This is the whole point. `{'isRequired': True}` from Python parsed as
      ;; clean JSON, matched no key at all, and left a MANDATORY field optional
      ;; on every surface — the human just skipped it and the run went on.
      (let [message (refusal #(normalized-fields {"name" "env" "isRequired" true}))]
        (expect (some? message))
        (expect (str/includes? message "is_required")))
      (expect (some? (refusal #(normalized-fields {"name" "env" "maxLength" 5}))))
      (expect (some? (refusal #(hi/normalize-request
                                 {"title" "t" "fields" [{"name" "a"}] "timeoutMs" 10})))))
  (it "refuses a kebab-case STRING and a snake_case KEYWORD"
      ;; Each half of the contract, spelled the other half's way: strings are
      ;; snake_case (Python/JSON), keywords are kebab-case (Clojure).
      (expect (some? (refusal #(normalized-fields {"name" "env" "is-required" true}))))
      (expect (some? (refusal #(normalized-fields {:name "env" :is_required true}))))
      (expect (some? (refusal #(hi/normalize-request
                                 {:title "t" :fields [{:name "a"}] :timeout_ms 10})))))
  (it "refuses an unknown key, in a field, a request or an option"
      (let [message (refusal #(normalized-fields {:name "env" :requried true}))]
        (expect (some? message))
        (expect (str/includes? message "unknown field key")))
      (expect (some? (refusal #(hi/normalize-request
                                 {:title "t" :fields [{:name "a"}] :retries 3}))))
      (expect (some? (refusal #(normalized-fields {:name "env"
                                                   :type "select"
                                                   :options [{"value" "a" "Label" "A"}]})))))
  (it "takes the `id` spelling of a name, and no longer a `help`"
      ;; `help` was `description`'s legacy alias; the vocabulary has one spelling
      ;; per key, so it is now as unknown as any other stray word.
      (let [[field] (normalized-fields {"id" "env" "description" "prose"})]
        (expect (= "env" (:name field)))
        (expect (= "prose" (:description field))))
      (let [message (refusal #(normalized-fields {"name" "env" "help" "prose"}))]
        (expect (some? message))
        (expect (str/includes? message "unknown field key"))))
  (it "calls no key of the spec's own vocabulary unknown"
      ;; The parser derives the snake_case keys it accepts from the spec's key
      ;; sets, so nothing declared there can come back as an unknown key here.
      (expect (= #{} (unknown-keys hs/field-keys {"name" "env"} normalized-fields)))
      (expect
        (= #{}
           (unknown-keys hs/group-keys {"type" "group" "fields" [{"name" "a"}]} normalized-fields)))
      (expect (= #{} (unknown-keys hs/decor-keys {"type" "heading"} normalized-fields)))
      (expect (= #{}
                 (unknown-keys hs/option-keys
                               {"value" "a"}
                               #(normalized-fields {"name" "env" "type" "select" "options" [%]}))))
      (expect (= #{}
                 (unknown-keys hs/request-keys
                               {"title" "t" "fields" [{"name" "a"}]}
                               hi/normalize-request)))))

(defdescribe
  python-json-seam-test
  (it "takes snake_case strings in and hands snake_case strings back"
      ;; A request is only pending while some channel is mounted to draw it, and a
      ;; bare JVM has none — stand one up on a default channel so the seam, not
      ;; the empty process, is what this test measures.
      (ce/add-channel-event-listener! :tui
                                      ::json-seam
                                      (fn [_]))
      (try
        (let
          [title
           (str "Deploy " (random-uuid))

           answer-json
           (future (hi/request-json! (json/write-json-str {"title" title
                                                           "session_id" "test-session"
                                                           "description" "Pick a target"
                                                           ;; Channel routing is host-side — a guest cannot aim the
                                                           ;; dialog anywhere, so this key is dropped, not honoured.
                                                           "channel_ids" ["nowhere"]
                                                           "timeout_ms" 20000
                                                           "fields"
                                                           [{"name" "env"
                                                             "label" "Target"
                                                             "description" "Where it lands."
                                                             "type" "select"
                                                             "options" ["staging" "prod"]
                                                             "is_required" true}
                                                            {"name" "note" "type" "plaintext"}]})))

           request-id
           (await-pending-id title)

           [env]
           (:fields (hi/pending-request request-id))]

          ;; The dialog's OWN description crosses the seam, not just its fields': an
          ;; ask says what it is about before the operator reads a single label.
          (expect (= "Pick a target" (:description (hi/pending-request request-id))))
          ;; The dialog sees exactly what the Python spec asked for.
          (expect (= "env" (:name env)))
          (expect (= "Target" (:label env)))
          (expect (= "Where it lands." (:description env)))
          (expect (true? (:is-required env)))
          ;; Required is enforced for a JSON caller too, not just a dialog.
          (expect (false? (:is-accepted (hi/submit! request-id {"note" "hi"}))))
          (expect (true? (:is-accepted (hi/submit! request-id {"env" "prod" "note" "hi"}))))
          (let [answer (json/read-json @answer-json :key-fn identity)]
            (expect (= #{"is_submitted" "reason" "request_id" "values"} (set (keys answer))))
            (expect (true? (get answer "is_submitted")))
            (expect (= "submitted" (get answer "reason")))
            (expect (= request-id (get answer "request_id")))
            (expect (= {"env" "prod" "note" "hi"} (get answer "values")))))
        (finally (ce/remove-channel-event-listener! :tui ::json-seam))))
  (it "reports a misspelled key from the JSON seam instead of dropping it"
      (let
        [message (refusal #(hi/request-json! (json/write-json-str
                                               {"title" "t"
                                                "fields" [{"name" "env" "isRequired" true}]})))]
        (expect (some? message))
        (expect (str/includes? message "is_required")))))

(defdescribe
  coerce-values-test
  (it "reports every failing field at once"
      (let
        [fields
         (normalized-fields {:id "name" :is-required true :max-length 5}
                            {:id "env" :type "select" :options ["a"] :is-required true})

         outcome
         (hi/coerce-values fields {"name" "abcdef" "env" "zzz"})]

        (expect (false? (:is-accepted outcome)))
        (expect (= #{"name" "env"} (set (keys (:errors outcome)))))))
  (it "treats blank and missing text alike for a required field"
      (let [fields (normalized-fields {:id "name" :is-required true})]
        (expect (= {"name" "is required"} (:errors (hi/coerce-values fields {}))))
        (expect (= {"name" "is required"} (:errors (hi/coerce-values fields {"name" "  "}))))))
  (it "leaves an optional text field nil"
      (let [fields (normalized-fields {:id "note" :type "multiline"})]
        (expect (= {:is-accepted true :values {"note" nil}} (hi/coerce-values fields {})))))
  (it "trims plaintext but preserves multiline bodies"
      (let
        [fields
         (normalized-fields {:id "name"} {:id "body" :type "multiline"})

         outcome
         (hi/coerce-values fields {"name" "  vis  " "body" "  line\n  two  "})]

        (expect (= "vis" (get-in outcome [:values "name"])))
        (expect (= "  line\n  two  " (get-in outcome [:values "body"])))))
  (it "rejects a value no dialog could ever type"
      ;; A JSON client (the companion app) can post an object or a list where a
      ;; terminal can only type characters. Stringifying it would hand the
      ;; extension a Clojure printing of the map and let the app submit an
      ;; answer the TUI can never produce.
      (let
        [fields
         (normalized-fields {:id "name"} {:id "body" :type "multiline"})

         outcome
         (hi/coerce-values fields {"name" {"a" 1} "body" ["x"]})]

        (expect (false? (:is-accepted outcome)))
        (expect (= {"name" "must be text" "body" "must be text"} (:errors outcome)))
        (expect (= "42" (get-in (hi/coerce-values fields {"name" 42}) [:values "name"])))))
  (it "accepts one value or many for a multiselect and rejects unknown options"
      (let [fields (normalized-fields {:id "tags" :type "multiselect" :options ["a" "b"]})]
        (expect (= ["a"] (get-in (hi/coerce-values fields {"tags" "a"}) [:values "tags"])))
        (expect (= ["a" "b"]
                   (get-in (hi/coerce-values fields {"tags" ["a" "b" "a"]}) [:values "tags"])))
        (expect (= [] (get-in (hi/coerce-values fields {}) [:values "tags"])))
        (expect (false? (:is-accepted (hi/coerce-values fields {"tags" ["z"]}))))))
  (it "requires at least one option for a required multiselect"
      (let
        [fields (normalized-fields
                  {:id "tags" :type "multiselect" :options ["a"] :is-required true})]
        (expect (false? (:is-accepted (hi/coerce-values fields {"tags" []}))))))
  (it "coerces checkbox strings and defaults to false"
      (let [fields (normalized-fields {:id "ok" :type "checkbox"})]
        (expect (true? (get-in (hi/coerce-values fields {"ok" "true"}) [:values "ok"])))
        (expect (false? (get-in (hi/coerce-values fields {"ok" "false"}) [:values "ok"])))
        (expect (false? (get-in (hi/coerce-values fields {}) [:values "ok"])))
        (expect (false? (:is-accepted (hi/coerce-values fields {"ok" "maybe"}))))))
  (it "refuses an unticked required checkbox"
      ;; A required checkbox is a consent box — "I agree", "yes, delete it". False
      ;; is not an answer to it, and both dialogs already refuse to submit one,
      ;; so anything posting JSON must be held to the same rule.
      (let [fields (normalized-fields {:id "agree" :type "checkbox" :is-required true})]
        (expect (= {"agree" "must be checked"} (:errors (hi/coerce-values fields {}))))
        (expect (= {"agree" "must be checked"} (:errors (hi/coerce-values fields {"agree" false}))))
        (expect (= {"agree" "must be checked"}
                   (:errors (hi/coerce-values fields {"agree" "false"}))))
        (expect (true? (get-in (hi/coerce-values fields {"agree" true}) [:values "agree"])))))
  (it "leaves an optional checkbox free to stay unticked"
      (let [fields (normalized-fields {:id "ok" :type "checkbox"})]
        (expect (true? (:is-accepted (hi/coerce-values fields {"ok" false}))))))
  (it "honours a checkbox default when the value is absent"
      (let [fields (normalized-fields {:id "ok" :type "checkbox" :default true})]
        (expect (true? (get-in (hi/coerce-values fields {}) [:values "ok"])))))
  (it "keeps only declared fields"
      (let
        [fields
         (normalized-fields {:id "name"})

         outcome
         (hi/coerce-values fields {"name" "vis" "smuggled" "nope"})]

        (expect (= {"name" "vis"} (:values outcome))))))

(defdescribe secret-vault-test
             (it "hands back an opaque handle instead of the plaintext"
                 (let
                   [fields
                    (normalized-fields {:id "token" :type "password" :is-required true})

                    handle
                    (get-in (hi/coerce-values fields {"token" "hunter2"}) [:values "token"])]

                   (expect (hs/secret-handle? handle))
                   (expect (not= "hunter2" handle))
                   (expect (not (str/includes? (pr-str handle) "hunter2")))
                   (expect (= "hunter2" (hi/reveal-secret handle)))
                   (expect (true? (hi/forget-secret! handle)))
                   (expect (nil? (hi/reveal-secret handle)))
                   (expect (false? (hi/forget-secret! handle)))))
             (it "does not mint a handle for an absent optional password"
                 (let [fields (normalized-fields {:id "token" :type "password"})]
                   (expect (nil? (get-in (hi/coerce-values fields {}) [:values "token"])))))
             (it "knows a handle from any other string"
                 (expect (not (hs/secret-handle? "hunter2")))
                 (expect (not (hs/secret-handle? nil)))
                 (expect (not (hs/secret-handle? 7)))))

(defdescribe
  request-lifecycle-test
  (it "publishes a dialog event, blocks, and returns the submitted values"
      (let
        [{:keys [future events request-id detach!]}
         (start-request! {:title "Deploy"
                          :description "Pick a target"
                          :fields
                          [{:id "env" :type "select" :options ["staging" "prod"] :is-required true}
                           {:id "token" :type "password" :is-required true}]})]
        (try (expect (= 1 (count (hi/pending-requests))))
             (expect (= {:is-accepted false :errors {"env" "must be one of prod, staging"}}
                        (hi/submit! request-id {"env" "nope" "token" "t"})))
             (expect (= 1 (count (hi/pending-requests))) "a rejected submit leaves the dialog open")
             (expect (= {:is-accepted true}
                        (hi/submit! request-id {"env" "prod" "token" "hunter2"})))
             (let [result (deref future 5000 ::blocked)]
               (expect (true? (:is-submitted result)))
               (expect (= "submitted" (:reason result)))
               (expect (= request-id (:request-id result)))
               (expect (= "prod" (get-in result [:values "env"])))
               (expect (= "hunter2" (hi/reveal-secret (get-in result [:values "token"])))))
             (expect (= [:human-input/request :human-input/close] (mapv :op @events)))
             (expect (zero? (count (hi/pending-requests))))
             (finally (detach!)))))
  (it "never shows a channel the plaintext or the waiting promise"
      (let
        [{:keys [future events request-id detach!]}
         (start-request! {:title "Deploy"
                          :fields [{:id "token" :type "password" :is-required true}]})]
        (try (let [view (:request (first (filterv #(= :human-input/request (:op %)) @events)))]
               (expect (= request-id (:id view)))
               (expect (= "Deploy" (:title view)))
               (expect (nil? (:promise view)))
               (expect (nil? (:channel-ids view)))
               (expect (= [:password] (mapv :type (:fields view))))
               (expect (= view (hi/pending-request request-id))))
             (hi/submit! request-id {"token" "hunter2"})
             (let [closed (first (filterv #(= :human-input/close (:op %)) @events))]
               (expect (= request-id (:request-id closed)))
               (expect (= "submitted" (:reason closed)))
               (expect (not (clojure.string/includes? (pr-str @events) "hunter2"))))
             (deref future 5000 ::blocked)
             (finally (detach!)))))
  (it "releases the waiter on cancel"
      (let
        [{:keys [future events request-id detach!]} (start-request! {:title "Deploy"
                                                                     :fields [{:id "env"}]})]
        (try (expect (true? (hi/cancel! request-id)))
             (expect (= {:is-submitted false :reason "cancelled" :request-id request-id}
                        (deref future 5000 ::blocked)))
             (expect (false? (hi/cancel! request-id)) "a second cancel is a no-op")
             (expect (= "cancelled"
                        (:reason (first (filterv #(= :human-input/close (:op %)) @events)))))
             (finally (detach!)))))
  (it "carries a custom cancel reason"
      (let
        [{:keys [future request-id detach!]} (start-request! {:title "Deploy"
                                                              :fields [{:id "env"}]})]
        (try (hi/cancel! request-id "channel detached")
             (expect (= "channel detached" (:reason (deref future 5000 ::blocked))))
             (finally (detach!)))))
  (it "releases the waiter when the timeout elapses"
      (let
        [{:keys [future request-id detach!]}
         (start-request! {:title "Deploy" :timeout-ms 120 :fields [{:id "env"}]})]
        (try (expect (= {:is-submitted false :reason "timeout" :request-id request-id}
                        (deref future 5000 ::blocked)))
             (expect (nil? (hi/pending-request request-id)))
             (finally (detach!)))))
  (it "waits indefinitely when the timeout is 0, and still hears a late answer"
      ;; The twin of the test above: 120ms is a deadline, 0 is none. An
      ;; extension that must not guess an answer parks until the operator is
      ;; back at the keyboard, so nothing may settle this request on its own.
      (let
        [{:keys [future request-id detach!]}
         (start-request! {:title "Deploy" :timeout-ms 0 :fields [{:id "env"}]})]
        (try (expect (= ::blocked (deref future 400 ::blocked))
                     "an indefinite request never gives up on the human")
             (expect (some? (hi/pending-request request-id)) "the dialog is still on every surface")
             (expect (= {:is-accepted true} (hi/submit! request-id {"env" "prod"})))
             (let [result (deref future 5000 ::blocked)]
               (expect (true? (:is-submitted result)))
               (expect (= "submitted" (:reason result)))
               (expect (= "prod" (get-in result [:values "env"]))))
             (finally (detach!)))))
  (it "rejects a submit or cancel for an unknown request"
      (expect (= {:is-accepted false :reason "unknown"} (hi/submit! "no-such-id" {})))
      (expect (false? (hi/cancel! "no-such-id"))))
  (it "releases every waiter on cancel-all!"
      (let
        [a
         (start-request! {:title "A" :fields [{:id "x"}]})

         b
         (start-request! {:title "B" :fields [{:id "x"}]})]

        (try (expect (<= 2 (hi/cancel-all! "shutting down")))
             (expect (= "shutting down" (:reason (deref (:future a) 5000 ::blocked))))
             (expect (= "shutting down" (:reason (deref (:future b) 5000 ::blocked))))
             (finally ((:detach! a)) ((:detach! b))))))
  (it "rejects an invalid spec before anything blocks"
      (expect (throws? clojure.lang.ExceptionInfo #(hi/request! {:title "t" :fields []}))))
  ;; Regression, issue #113: a request raised outside a session parked its caller
  ;; anyway — the gateway bridge dropped it with a `request-without-session`
  ;; warning, so the companion app never learned the run was blocked and only a
  ;; TUI mounted in this very process could release it.
  (it "refuses a request that names no session"
      (let
        [ex (try (hi/request! {:title "Deploy" :fields [{:id "env"}]})
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
        (expect (= :vis/human-input-invalid-request (:type (ex-data ex)))))))

(defdescribe
  non-cancellable-request-test
  (it "refuses every operator cancel and still yields to shutdown"
      ;; `:is-cancellable false` has to be enforced where BOTH surfaces meet —
      ;; the engine — or the TUI and the app each have to remember it, and the
      ;; one that forgets dismisses a request the extension declared mandatory.
      (let
        [rid
         (str "must-answer-" (random-uuid))

         chan
         (fresh-channel)

         _
         (ce/add-channel-event-listener! chan
                                         ::must-answer
                                         (fn [_]))

         answer
         (future (hi/request! {:id rid
                               :session-id "test-session"
                               :title "Deploy"
                               :is-cancellable false
                               :timeout-ms 3000
                               :channel-ids [chan]
                               :fields [{:id "note" :is-required true}]}))

         parked?
         (loop [attempts 200]
           (cond (some? (hi/pending-request rid)) true
                 (zero? attempts) false
                 :else (do (Thread/sleep 5) (recur (dec attempts)))))]

        (try (expect parked?)
             (expect (false? (hi/cancel! rid)))
             (expect (false? (hi/cancel! rid "operator")))
             (expect (some? (hi/pending-request rid)))
             (expect (false? (:is-cancellable (hi/pending-request rid))))
             ;; Shutdown is not the operator: nothing is left to answer with, so
             ;; the parked thread must be released anyway.
             (expect (= 1 (hi/cancel-all! "shutdown")))
             (expect (= {:is-submitted false :reason "shutdown" :request-id rid}
                        (deref answer 2000 ::stuck)))
             (finally (ce/remove-channel-event-listener! chan ::must-answer)
                      (hi/cancel-all! "cleanup")
                      (future-cancel answer))))))

(defn- await-true
  "Poll `pred` for up to ~2s; another thread settles these requests."
  [pred]
  (loop [n 0]
    (cond (pred) true
          (< n 200) (do (Thread/sleep 10) (recur (inc n)))
          :else false)))

(defdescribe blocking-wall-park-test
             ;; REGRESSION: HITL "is not working" — the dialog opened, the operator was
             ;; still typing, and the enclosing timeout wall (the Python eval watchdog, the
             ;; native-tool wall) billed that thinking time and killed the call with a bare
             ;; `Timeout (120s)` while the dialog was still on screen.
             (it
               "parks the enclosing timeout wall until the operator answers"
               (let
                 [chan
                  (fresh-channel)

                  events
                  (atom [])

                  depth
                  (atom 0)

                  entered
                  (promise)

                  park
                  (fn [thunk]
                    (swap! depth inc)
                    (deliver entered true)
                    (try (thunk) (finally (swap! depth dec))))]

                 (ce/add-channel-event-listener! chan ::park-collector #(swap! events conj %))
                 (try (let
                        [fut
                         (future (binding [rt/*blocking-wall-park* park]
                                   (hi/request! {:title "Login"
                                                 :session-id "test-session"
                                                 :fields [{:id "otp" :label "OTP"}]
                                                 :timeout-ms 5000
                                                 :channel-ids [chan]})))

                         request-id
                         (await-request-id events)]

                        ;; The wall is parked BEFORE anybody can possibly answer.
                        (expect (true? (deref entered 2000 false)))
                        (expect (= 1 @depth))
                        (expect (:is-accepted (hi/submit! request-id {"otp" "123456"})))
                        (let [answer (deref fut 5000 ::none)]
                          (expect (true? (:is-submitted answer)))
                          (expect (= "submitted" (:reason answer))))
                        ;; …and the clock restarts the moment the answer lands.
                        (expect (= 0 @depth)))
                      (finally (ce/remove-channel-event-listener! chan ::park-collector))))))

(defdescribe interrupted-request-test
             ;; REGRESSION: interrupting the turn that opened the dialog left the entry in
             ;; `pending` forever — a zombie dialog no channel could dismiss and no later
             ;; request could reuse.
             (it "an interrupted wait releases the request and closes the dialog"
                 (let
                   [{fut :future events :events request-id :request-id detach! :detach!}
                    (start-request! (spec {:id "otp"}))]
                   (try (expect (some? (hi/pending-request request-id)))
                        (future-cancel fut)
                        (expect (true? (await-true #(nil? (hi/pending-request request-id)))))
                        (expect (true? (await-true #(boolean
                                                      (some (fn [e]
                                                              (and (= :human-input/close (:op e))
                                                                   (= request-id (:request-id e))))
                                                            @events)))))
                        (finally (detach!))))))

(defdescribe undeliverable-request-test
             ;; Regression, issue #104: a request published to channels nobody is mounted on
             ;; parked the extension for its whole timeout and then answered `"timeout"`, as
             ;; if a human had ignored a dialog that was never drawn — and not one log line
             ;; said the request had reached zero surfaces.
             (it "fails fast, and loudly, when no channel is listening"
                 (let
                   [rid
                    (str "undeliverable-" (random-uuid))

                    {answer :value signals :signals}
                    (tel/with-signals (hi/request! {:id rid
                                                    :session-id "test-session"
                                                    :title "Deploy"
                                                    :timeout-ms 2000
                                                    :fields [{:id "env"}]
                                                    :channel-ids [(fresh-channel)]}))]

                   (expect (= {:is-submitted false :reason "undeliverable" :request-id rid} answer))
                   (expect (nil? (hi/pending-request rid)))
                   (expect (signal? signals :error ::hi/request-undeliverable rid)))))

(defdescribe
  range-field-test
  (it "defaults its bounds to 0..100 by 1"
      (let [[field] (normalized-fields {:id "pct" :type "range"})]
        (expect (= :range (:type field)))
        (expect (= 0 (:min field)))
        (expect (= 100 (:max field)))
        (expect (= 1 (:step field)))))
  (it "keeps the bounds it was given"
      (let [[field] (normalized-fields {:id "pct" :type "range" :min 10 :max 20 :step 0.5})]
        (expect (= 10 (:min field)))
        (expect (= 20 (:max field)))
        (expect (= 0.5 (:step field)))))
  (it "refuses bounds that describe no range at all"
      (expect (throws? clojure.lang.ExceptionInfo
                       #(normalized-fields {:id "pct" :type "range" :min 5 :max 5})))
      (expect (throws? clojure.lang.ExceptionInfo
                       #(normalized-fields {:id "pct" :type "range" :min 5 :max 1})))
      (expect (throws? clojure.lang.ExceptionInfo
                       #(normalized-fields {:id "pct" :type "range" :step 0})))
      (expect (throws? clojure.lang.ExceptionInfo
                       #(normalized-fields {:id "pct" :type "range" :min "low"}))))
  (it "answers with a number, not a string"
      (let [fields (normalized-fields {:id "pct" :type "range" :min 0 :max 100})]
        (expect (= 42 (get-in (hi/coerce-values fields {"pct" 42}) [:values "pct"])))
        (expect (= 42 (get-in (hi/coerce-values fields {"pct" "42"}) [:values "pct"])))))
  (it "stays a double when the bounds are fractional"
      (let [fields (normalized-fields {:id "pct" :type "range" :min 0 :max 1 :step 0.25})]
        (expect (= 0.25 (get-in (hi/coerce-values fields {"pct" 0.25}) [:values "pct"])))))
  (it "falls back to min when nothing was answered"
      (let [fields (normalized-fields {:id "pct" :type "range" :min 7 :max 9})]
        (expect (= 7 (get-in (hi/coerce-values fields {}) [:values "pct"])))))
  (it "honours a default"
      (let [fields (normalized-fields {:id "pct" :type "range" :min 0 :max 100 :default 30})]
        (expect (= 30 (get-in (hi/coerce-values fields {}) [:values "pct"])))))
  (it
    "refuses a value outside the bounds, and one that is not a number"
    (let [fields (normalized-fields {:id "pct" :type "range" :min 0 :max 10})]
      (expect (= {"pct" "must be between 0 and 10"} (:errors (hi/coerce-values fields {"pct" 11}))))
      (expect (= {"pct" "must be between 0 and 10"} (:errors (hi/coerce-values fields {"pct" -1}))))
      (expect (= {"pct" "must be a number"} (:errors (hi/coerce-values fields {"pct" "loud"})))))))

(defn- otp-field
  "A normalized `:otp` field from the spec keys a caller would actually write."
  [& {:as spec}]
  (hi/normalize-field (merge {"name" "code" "type" "otp"} spec)))

(defdescribe
  otp-field-test
  (it "is six digits when the spec says nothing"
      (let [field (otp-field)]
        (expect (= 6 (:min-length field)))
        (expect (= 6 (:max-length field)))
        (expect (= [:ok "123456"] (hi/coerce-value field "123456")))))
  (it "reads the code however the provider printed it"
      (let [field (otp-field)]
        (expect (= [:ok "123456"] (hi/coerce-value field "123 456")))
        (expect (= [:ok "123456"] (hi/coerce-value field "123-456")))
        (expect (= [:ok "123456"] (hi/coerce-value field " 12 34 56 ")))))
  (it "takes digits and nothing else"
      (expect (= [:error "must be digits only"] (hi/coerce-value (otp-field) "12a456")))
      (expect (= [:error "must be digits only"] (hi/coerce-value (otp-field) "12.456")))
      (expect (= [:error "must be a one-time code"] (hi/coerce-value (otp-field) ["1" "2"]))))
  (it "names the exact length when there is only one"
      (let [field (otp-field "max_length" 4)]
        (expect (= [:error "must be 4 digits"] (hi/coerce-value field "123")))
        (expect (= [:error "must be 4 digits"] (hi/coerce-value field "12345")))
        (expect (= [:ok "1234"] (hi/coerce-value field "1234")))))
  (it "accepts any length between :min_length and :max_length"
      (let [field (otp-field "min_length" 4 "max_length" 8)]
        (expect (= 4 (:min-length field)))
        (expect (= 8 (:max-length field)))
        (expect (= [:error "must be at least 4 digits"] (hi/coerce-value field "123")))
        (expect (= [:error "must be at most 8 digits"] (hi/coerce-value field "123456789")))
        (expect (= [:ok "12345"] (hi/coerce-value field "12345")))))
  (it "leaves an untouched optional code unanswered, and refuses a blank required one"
      (expect (= [:ok nil] (hi/coerce-value (otp-field) nil)))
      (expect (nil? (:default (otp-field))))
      (expect (= [:error "is required"] (hi/coerce-value (otp-field "is_required" true) "  "))))
  (it "refuses a spec whose boxes could never be filled"
      (expect (throws? clojure.lang.ExceptionInfo #(otp-field "max_length" 0)))
      (expect (throws? clojure.lang.ExceptionInfo #(otp-field "max_length" 99)))
      (expect (throws? clojure.lang.ExceptionInfo #(otp-field "min_length" 8 "max_length" 4)))
      (expect (throws? clojure.lang.ExceptionInfo #(otp-field "max_length" "six"))))
  (it "validates its own :default like any other answer"
      (expect (= "123456" (:default (otp-field "default" "123 456"))))
      (expect (throws? clojure.lang.ExceptionInfo #(otp-field "default" "12")))))

(defn- sign-in-request
  "Two passwords and an email — the form every validation feature is for."
  [& {:as overrides}]
  (hi/normalize-request
    (merge {"title" "Sign in"
            "fields" [{"name" "email"
                       "is_required" true
                       "validate" #(when-not (re-find #"@" (str %)) "must be an email address")}
                      {"name" "pw"
                       "type" "password"
                       "label" "Password"
                       "is_required" true
                       "validate" [#(when (< (count (str %)) 8) "at least 8 characters")
                                   #(when-not (re-find #"[0-9]" (str %)) "needs a digit")]}
                      {"name" "pw2"
                       "type" "password"
                       "is_required" true
                       "validate" (fn [value values]
                                    (when-not (= value (get values "pw")) "must match Password"))}]}
           overrides)))

(defn- errors
  "The `field id -> message` map a submission of `values` would be refused with."
  [request values]
  (or (:errors (hi/validate-values (:fields request) values)) {}))

(def ^:private good-sign-in {"email" "ada@example.com" "pw" "hunter42" "pw2" "hunter42"})

(defdescribe
  validation-test
  "A validator is a FUNCTION the engine runs when the form is CONFIRMED: it
   answers nil/true for a good value, or the message the field should show."
  (it "accepts the form it was written for"
      (let [answer (hi/validate-values (:fields (sign-in-request)) good-sign-in)]
        (expect (:is-accepted answer))
        (expect (= "ada@example.com" (get-in answer [:values "email"])))))
  (it "reports every bad field at once, one message each"
      (expect (= {"email" "must be an email address"
                  "pw" "at least 8 characters"
                  "pw2" "must match Password"}
                 (errors (sign-in-request) {"email" "ada" "pw" "short1" "pw2" "other"}))))
  (it "gives the first validator that has something to say"
      (expect (= {"pw" "needs a digit"}
                 (errors (sign-in-request)
                         (assoc good-sign-in
                           "pw" "hunterrr"
                           "pw2" "hunterrr")))))
  (it "hands a two-argument validator every other value, so a field can compare itself"
      ;; The confirmation field nobody notices is broken: it needs its SIBLING,
      ;; which is exactly what the second argument is for.
      (expect (= {} (errors (sign-in-request) good-sign-in)))
      (expect (= {"pw2" "must match Password"}
                 (errors (sign-in-request) (assoc good-sign-in "pw2" "hunter43")))))
  (it "checks the shape of an answer, never whether there IS one"
      ;; A validator and `:is-required` answer two different questions: an
      ;; optional email left blank is fine, a required one is refused as missing.
      (let
        [optional (hi/normalize-request {"title" "t"
                                         "fields" [{"name" "email"
                                                    "validate" #(when-not (re-find #"@" (str %))
                                                                  "must be an email address")}]})]
        (expect (= {} (errors optional {"email" ""})))
        (expect (= {} (errors optional {})))
        (expect (= {"email" "must be an email address"} (errors optional {"email" "nope"})))))
  (it "takes a validator's word for it, whatever shape that word arrives in"
      (let
        [request (hi/normalize-request {"title" "t"
                                        "fields" [{"name" "team"
                                                   :validate #(when-not (= "ops" %)
                                                                "must be an ops team")}
                                                  {"name" "quiet" :validate (constantly nil)}
                                                  {"name" "sure" :validate (constantly true)}
                                                  {"name" "flag" :validate (constantly false)}
                                                  {"name" "boom"
                                                   :validate (fn [_]
                                                               (throw (ex-info "nope" {})))}]})]
        (expect (= {} (errors request {"team" "ops" "flag" nil "boom" nil})))
        (expect (= {"team" "must be an ops team"
                    "flag" "is not valid"
                    "boom" "could not be validated: nope"}
                   (errors request {"team" "sre" "quiet" "x" "sure" "x" "flag" "x" "boom" "x"})))))
  (it "runs each validator exactly once per confirmation"
      ;; Validation is CODE: an extension's function may be slow and may talk to
      ;; something, so it is never run speculatively while the human types. One
      ;; confirmation, one call per field with a value.
      (let
        [calls
         (atom 0)

         request
         (hi/normalize-request {"title" "t"
                                "fields" [{"name" "a"
                                           :validate (fn [_]
                                                       (swap! calls inc)
                                                       nil)}
                                          {"name" "b"
                                           :validate (fn [_]
                                                       (swap! calls inc)
                                                       "no")}]})]

        (expect (= {"b" "no"} (errors request {"a" "x" "b" "y"})))
        (expect (= 2 @calls))
        (expect (= {"b" "no"} (errors request {"a" "x" "b" "y"})))
        (expect (= 4 @calls))))
  (it "refuses a validator that is not a function, or one it could never call"
      ;; Rules as DATA are gone: a rule map, a type name and a bare regex are all
      ;; refused where they used to be honoured, so a spec written against the
      ;; old DSL fails loudly instead of quietly checking nothing.
      (expect (throws? clojure.lang.ExceptionInfo
                       #(hi/normalize-field {"name" "a" "validate" {"type" "email"}})))
      (expect (throws? clojure.lang.ExceptionInfo
                       #(hi/normalize-field {"name" "a" "validate" "email"})))
      (expect (throws? clojure.lang.ExceptionInfo
                       #(hi/normalize-field {"name" "a" "validate" #"^[0-9]+$"})))
      (expect (throws? clojure.lang.ExceptionInfo
                       #(hi/normalize-field {"name" "a"
                                             "validate" (fn []
                                                          nil)})))
      (expect (throws? clojure.lang.ExceptionInfo
                       #(hi/normalize-field {"name" "a"
                                             "validate" (fn [_ _ _]
                                                          nil)}))))
  (it "never lets a validator near the wire"
      ;; A function cannot be serialized and a surface has no business owning a
      ;; rule: the view carries the field, never the check.
      (let
        [request
         (hi/normalize-request {"title" "t" "fields" [{"name" "a" "validate" (constantly "no")}]})

         view-field
         (first (:fields (hi/request->view request)))]

        (expect (not (contains? view-field :validate)))
        (expect (= {"a" "no"} (errors request {"a" "ada@example.com"})))))
  (it "never mints a vault handle just to answer whether a form is valid"
      ;; `validate-values` is pure: only a real submission fills the vault, so a
      ;; refused confirmation never leaves a password behind it.
      (let
        [request
         (sign-in-request)

         _
         (hi/forget-secrets!)

         answer
         (hi/validate-values (:fields request) good-sign-in)]

        (expect (= "hunter42" (get-in answer [:values "pw"])))
        (expect (zero? (long (hi/forget-secrets!))))
        (expect (hs/secret-handle? (get-in (hi/coerce-values (:fields request) good-sign-in)
                                           [:values "pw"])))
        (expect (pos? (long (hi/forget-secrets!)))))))

(defn- grouped-request
  "One row holding two fields, beside a plain stacked field — the smallest form
   that can tell a layout tree from a flat list."
  [& {:as overrides}]
  (hi/normalize-request (merge {"title" "Deploy"
                                "fields" [{"type" "group"
                                           "direction" "row"
                                           "fields"
                                           [{"name" "host" "label" "Host" "is_required" true}
                                            {"name" "port"
                                             "label" "Port"
                                             "validate" #(when-not (re-matches #"[0-9]+" (str %))
                                                           "must be a whole number")}]}
                                          {"name" "note" "type" "multiline" "label" "Note"}]}
                               overrides)))

(defdescribe
  group-layout-test
  "A `group` is layout only: it holds fields and the direction they run in, it
   holds no answer, and it nests — which is the whole composability claim."
  (it "keeps the tree on the request and flattens it for the answer"
      (let
        [{:keys [fields]}
         (grouped-request)

         [group note]
         fields]

        (expect (= :group (:type group)))
        (expect (= :row (:direction group)))
        (expect (= ["host" "port"] (mapv :id (:fields group))))
        (expect (= :multiline (:type note)))
        (expect (= ["host" "port" "note"] (mapv :id (hi/input-fields fields))))))
  (it "is control flow, not a kind of field"
      ;; A group answers nothing, so it has no place in the vocabulary of
      ;; ANSWERS: with `:group` among the field types, every value path carried a
      ;; branch for a node that can never take one and a field spec happily
      ;; accepted layout.
      (let [[group note] (:fields (grouped-request))]
        (expect (not (contains? (set (vals hs/field-types)) hs/group-type)))
        (expect (= hs/group-type (:type group)))
        ;; Two contracts, one tree:
        (expect (nil? (hs/group-error group)))
        (expect (nil? (hs/field-error note)))
        (expect (some? (hs/field-error group)))
        (expect (some? (hs/group-error note)))
        ;; and the fork is taken above both, before a value key is parsed.
        (expect (str/includes? (refusal #(hi/normalize-field {"type" "group"
                                                              "fields" [{"name" "a"}]}))
                               "not a field"))))
  (it "stacks by default — a group without a :direction is a column"
      (let [[group] (normalized-fields {"type" "group" "fields" [{"name" "a"} {"name" "b"}]})]
        (expect (= :column (:direction group)))))
  (it "nests, so a row of stacks needs no new key"
      (let
        [[outer] (normalized-fields {"type" "group"
                                     "direction" "row"
                                     "fields" [{"type" "group" "fields" [{"name" "a"} {"name" "b"}]}
                                               {"name" "c"}]})]
        (expect (= :row (:direction outer)))
        (expect (= :column (:direction (first (:fields outer)))))
        (expect (= ["a" "b" "c"] (mapv :id (hi/input-fields [outer]))))))
  (it "names itself from its children when the spec does not name it"
      (let
        [[outer] (normalized-fields {"type" "group"
                                     "fields" [{"type" "group" "fields" [{"name" "a"}]}]})]
        (expect (= "group:group:a" (:id outer)))
        (expect (= "group:a" (:id (first (:fields outer)))))
        (expect (= (:id outer) (:name outer))))
      (let [[outer] (normalized-fields {"name" "when" "type" "group" "fields" [{"name" "a"}]})]
        (expect (= "when" (:id outer)))))
  (it "refuses a direction that is not a flexbox direction"
      (expect (throws? clojure.lang.ExceptionInfo
                       #(normalized-fields
                          {"type" "group" "direction" "diagonal" "fields" [{"name" "a"}]}))))
  (it "refuses a group with nothing in it"
      (expect (throws? clojure.lang.ExceptionInfo #(normalized-fields {"type" "group"})))
      (expect (throws? clojure.lang.ExceptionInfo
                       #(normalized-fields {"type" "group" "fields" []}))))
  (it "refuses a key that only an answerable field could use"
      (expect (throws? clojure.lang.ExceptionInfo
                       #(normalized-fields
                          {"type" "group" "is_required" true "fields" [{"name" "a"}]})))
      (expect (throws? clojure.lang.ExceptionInfo
                       #(normalized-fields
                          {"type" "group" "default" "x" "fields" [{"name" "a"}]}))))
  (it "refuses a layout key on a field that could never lay anything out"
      ;; The mirror of the rule above: `fields`/`direction` describe an
      ;; arrangement, and a leaf arranges nothing — silently ignoring them would
      ;; drop half the form the caller wrote.
      (expect (throws? clojure.lang.ExceptionInfo
                       #(normalized-fields {"name" "host" "fields" [{"name" "a"}]})))
      (expect (throws? clojure.lang.ExceptionInfo
                       #(normalized-fields {"name" "host" "direction" "row"})))
      (expect (str/includes? (try (normalized-fields {"name" "host" "direction" "row"})
                                  nil
                                  (catch clojure.lang.ExceptionInfo e (ex-message e)))
                             "only exists on a group")))
  (it "still refuses two fields with the same name across different groups"
      (expect (throws? clojure.lang.ExceptionInfo
                       #(normalized-fields {"type" "group" "fields" [{"name" "a"}]}
                                           {"type" "group" "fields" [{"name" "a"}]}))))
  (it "validates and answers the nested fields as if the tree were flat"
      (let [request (grouped-request)]
        (expect (= {"host" "is required" "port" "must be a whole number"}
                   (errors request {"port" "8o8o"})))
        (expect (= {"host" "vis.example.com" "port" "8080" "note" nil}
                   (:values (hi/coerce-values (:fields request)
                                              {"host" "vis.example.com" "port" "8080"}))))))
  (it "reaches a sibling in another group, because `values` is flat"
      (let
        [request (hi/normalize-request
                   {"title" "Sign in"
                    "fields" [{"type" "group"
                               "direction" "row"
                               "fields" [{"name" "pw" "type" "password" "label" "Password"}]}
                              {"name" "pw2"
                               "type" "password"
                               "validate" (fn [value values]
                                            (when-not (= value (get values "pw"))
                                              "must match Password"))}]})]
        (expect (= {} (errors request {"pw" "hunter42" "pw2" "hunter42"})))
        (expect (= {"pw2" "must match Password"} (errors request {"pw" "hunter42" "pw2" "nope"})))))
  (it "projects the tree onto the wire, direction and all, and never a validator"
      (let
        [view
         (hi/request->view (hi/normalize-request {"title" "Deploy"
                                                  "fields" [{"type" "group"
                                                             "direction" "row"
                                                             "label" "Target"
                                                             "fields" [{"name" "host"
                                                                        :validate [(fn [_]
                                                                                     true)]}]}]}))

         [group]
         (:fields view)]

        (expect (= :group (:type group)))
        (expect (= :row (:direction group)))
        (expect (= "Target" (:label group)))
        (expect (not (contains? (first (:fields group)) :validate)))
        (expect (not (contains? (first (:fields group)) :is-secret))))))

(defn- refusal-type
  "The `:type` of the `ex-info` a refusal carries, or nil when `thunk` wrongly
   went through."
  [thunk]
  (try (thunk) nil (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

;; The declared contract. `human-input` PARSES — either spelling of every key,
;; a type name looked up in a closed vocabulary — and `human-input.spec`
;; DECLARES what parsing has to produce. These tests hold the two together: the
;; vocabulary has one home, real normalized output satisfies the spec, and a
;; form no surface could paint is refused even when the engine itself built it.
(defdescribe
  declared-contract-test
  (it
    "every type a request can carry normalizes into the declared form"
    (let
      [request (hi/normalize-request
                 {:title "Deploy"
                  :description "everything at once"
                  :source "test"
                  :fields [{:id "name" :placeholder "who" :default "ada"}
                           {:name "pw" :type "password"}
                           {:name "notes" :type "multiline" :min-length 2 :max-length 40}
                           {:name "env" :type "select" :options ["dev" "prod"] :default "prod"}
                           {:name "tags"
                            :type "multiselect"
                            :options [{:value "a" :label "A"} {:value "b" :label "B"}]
                            :default ["a"]} {:name "confirm" :type "checkbox" :default true}
                           {:name "pct" :type "range" :min 0 :max 100 :step 5 :default 25}
                           {:name "code" :type "otp"}
                           {:name "grp"
                            :type "group"
                            :direction "row"
                            :fields [{:name "a"} {:name "b" :type "checkbox"}]}]
                  :timeout-ms 0})]
      (expect (nil? (hs/request-error request)))
      ;; The tree is checked as the two contracts it is: every leaf a field,
      ;; the group that arranges them a group, and its children part of the
      ;; same field contract — a nested field with no `:label` breaks the same
      ;; painter.
      (let
        [nodes (:fields request)
         group (last nodes)]

        (expect (every? #(nil? (hs/field-error %)) (butlast nodes)))
        (expect (nil? (hs/group-error group)))
        (expect (every? #(nil? (hs/field-error %)) (:fields group))))))
  (it "a request spelled the Python way lands in the very same form"
      (let
        [request (hi/normalize-request {"title" "Deploy"
                                        "fields"
                                        [{"name" "env" "type" "select" "options" ["dev" "prod"]}]
                                        "timeout_ms" 1000})]
        (expect (nil? (hs/request-error request)))))
  (it "the closed vocabulary has ONE home"
      ;; Two copies of the type table drift, and the copy the normalizer reads
      ;; is the one that decides what a surface is asked to paint.
      (expect (nil? (ns-resolve 'com.blockether.vis.internal.human-input 'field-types)))
      (expect (= #{:plaintext :password :multiline :select :multiselect :checkbox :range :otp}
                 (set (vals hs/field-types))))
      (expect (str/includes? (refusal #(hi/normalize-field {:name "a" :type "slider"}))
                             "multiselect")))
  (it "refuses a field the engine itself mis-built"
      ;; Nothing an extension writes reaches these: they are what a bug in the
      ;; normalizer would hand a dialog — a picker with no options, one identity
      ;; whose two spellings disagree, a password no longer marked secret, a
      ;; slider whose knob starts outside its own track.
      (let
        [legal {:id "env"
                :name "env"
                :type :select
                :label "Env"
                :is-required false
                :is-secret false
                :options [{:value "dev" :label "dev"}]}]
        (expect (nil? (hs/field-error legal)))
        (expect (str/includes? (refusal #(#'hi/checked-field "env" (dissoc legal :options)))
                               "options"))
        (expect (= :vis/human-input-invalid-field
                   (refusal-type #(#'hi/checked-field "env" (assoc legal :name "environment")))))
        (expect (some? (hs/field-error (assoc legal :is-secret true))))
        (expect (some? (hs/field-error (-> legal
                                           (assoc :type :range
                                                  :min 0
                                                  :max 10
                                                  :step 1)
                                           (dissoc :options)
                                           (assoc :default 50)))))
        ;; Closed in both directions: a wire key that survived normalization is
        ;; a bug, not a harmless extra.
        (expect (some? (hs/field-error (assoc legal "is_required" false))))))
  (it "refuses a request whose chrome went missing"
      (let [request (hi/normalize-request {:title "t" :fields [{:id "a"}]})]
        (expect (nil? (hs/request-error request)))
        (expect (str/includes? (refusal #(#'hi/checked-request (dissoc request :submit-label)))
                               "submit-label"))
        (expect (= :vis/human-input-invalid-request
                   (refusal-type #(#'hi/checked-request (assoc request :channel-ids [])))))))
  (it "checks every answer on its way out, whatever settled it"
      ;; `settle!` is the one funnel — submitted, cancelled, timed out,
      ;; undeliverable — so an answer missing `:request-id`, or claiming a
      ;; submission it has no `:values` for, never reaches the parked thread.
      (expect (= :vis/human-input-invalid-answer
                 (refusal-type #(#'hi/checked-answer
                                  "r1"
                                  nil
                                  {:is-submitted true :reason "submitted" :request-id "r1"}))))
      (expect (= :vis/human-input-invalid-answer
                 (refusal-type
                   #(#'hi/checked-answer "r1" nil {:is-submitted false :reason "cancelled"}))))
      (let
        [answer (hi/request! {:title "unmounted"
                              :session-id "test-session"
                              :fields [{:id "a"}]
                              :channel-ids [(fresh-channel)]
                              :timeout-ms 1000})]
        (expect (= "undeliverable" (:reason answer)))
        (expect (nil? (hs/answer-error nil answer)))))
  (it
    "checks a submitted answer against the very fields it answers"
    ;; The values ARE the inputs' data, so they are declared like the inputs:
    ;; a picker can only come back on an option it offered, a slider inside
    ;; its own track, a code at its own width, and a `password` as a vault
    ;; handle — the plaintext in an answer map is a leak, not a value. Nothing
    ;; an extension writes reaches these: this is what a coercion bug would
    ;; hand a blocked caller.
    (let
      [request
       (hi/normalize-request
         {:title "Deploy"
          :fields [{:name "env" :type "select" :options ["dev" "prod"] :is-required true}
                   {:name "pw" :type "password"} {:name "pct" :type "range" :min 0 :max 100 :step 5}
                   {:name "grp"
                    :type "group"
                    :direction "row"
                    :fields [{:name "code" :type "otp" :max-length 6}]}]
          :timeout-ms 0})

       fields
       (:fields request)

       answered
       (fn [values]
         {:is-submitted true :reason "submitted" :request-id "r1" :values values})

       legal
       {"env" "dev" "pw" nil "pct" 25 "code" nil}]

      (expect (nil? (hs/answer-error fields (answered legal))))
      ;; A value outside the domain its OWN field declared, named in the
      ;; reason so the bug is findable.
      (expect (str/includes? (hs/answer-error fields (answered (assoc legal "env" "staging")))
                             "env"))
      (expect (str/includes? (hs/answer-error fields (answered (assoc legal "pct" 500))) "pct"))
      (expect (some? (hs/answer-error fields (answered (assoc legal "code" "12")))))
      ;; The leak this seam exists to catch, and the handle that is fine.
      (expect (some? (hs/answer-error fields (answered (assoc legal "pw" "hunter2")))))
      (expect (nil? (hs/answer-error fields
                                     (answered (assoc legal
                                                 "pw" (str hs/secret-handle-prefix "abc"))))))
      ;; A field nobody asked about, and a question left unanswered: both lose
      ;; a value silently once an extension reads the map.
      (expect (some? (hs/answer-error fields (answered (assoc legal "sudo" "yes")))))
      (expect (some? (hs/answer-error fields (answered (dissoc legal "pct")))))
      ;; A layout group answers nothing, so naming one is naming no field.
      (expect (some? (hs/answer-error fields (answered (assoc legal "grp" "x")))))
      ;; Not every settlement carries values, and an answer whose request
      ;; already settled has no fields left to check against.
      (expect (nil? (hs/answer-error fields
                                     {:is-submitted false :reason "cancelled" :request-id "r1"})))
      (expect (nil? (hs/answer-error nil (answered {"whatever" "x"}))))
      (expect (= :vis/human-input-invalid-answer
                 (refusal-type
                   #(#'hi/checked-answer "r1" fields (answered (assoc legal "env" "staging")))))))))

;; A form is more than its questions: a `heading` names a section and a
;; `paragraph` explains one. Neither can live in the vocabulary of ANSWERS and
;; neither is layout — a decoration arranges nothing — so it is the third node
;; contract, and the three refuse each other.
(defdescribe
  decoration-test
  "A `heading` and a `paragraph` are pure DECORATION: ink on the form, with no
   answer, no children and no identity."
  (it "normalizes to just its type and the words it paints"
      (let
        [[head para field] (normalized-fields {"type" "heading" "text" "Connection"}
                                              {"type" "paragraph" "text" "Where it runs."}
                                              {"name" "host" "label" "Host"})]
        (expect (= {:type :heading :text "Connection"} head))
        (expect (= {:type :paragraph :text "Where it runs."} para))
        (expect (= "host" (:name field)))))
  (it "answers nothing, so it never keys the values map — not even inside a group"
      (let
        [fields (normalized-fields {"type" "heading" "text" "H"}
                                   {"name" "host"}
                                   {"type" "group"
                                    "name" "g"
                                    "fields" [{"type" "paragraph" "text" "P"}
                                              {"name" "pw" "type" "password"}]})]
        (expect (= ["host" "pw"] (mapv :name (hi/input-fields fields))))))
  (it "is neither a field nor a group, and the three contracts refuse each other"
      (let
        [[head field group] (normalized-fields {"type" "heading" "text" "H"}
                                               {"name" "host"}
                                               {"type" "group" "name" "g" "fields" [{"name" "a"}]})]
        ;; One home for the vocabulary, as with the field and group tables.
        (expect (= #{:heading :paragraph} (set (vals hs/decor-types))))
        (expect (not (contains? (set (vals hs/field-types)) :heading)))
        (expect (nil? (ns-resolve 'com.blockether.vis.internal.human-input 'decor-types)))
        (expect (nil? (hs/decor-error head)))
        (expect (some? (hs/field-error head)))
        (expect (some? (hs/group-error head)))
        (expect (some? (hs/decor-error field)))
        (expect (some? (hs/decor-error group)))))
  (it "refuses a decoration that tries to ask something"
      ;; A `:name` would make it keyed, a `:default` would make it answerable:
      ;; both are a spec that meant to add a field.
      (expect (str/includes? (refusal #(hi/normalize-node {"type" "heading" "text" "H" "name" "h"}))
                             "name"))
      (expect (str/includes? (refusal #(hi/normalize-node {"type" "paragraph"})) "text"))
      ;; And it can never arrive on the FIELD path: the fork is taken once, above.
      (expect (str/includes? (refusal #(hi/normalize-field {"type" "heading" "text" "H"}))
                             "decoration")))
  (it "leaves a decorated request satisfying the declared contract"
      (let
        [request (hi/normalize-request (spec {"type" "heading" "text" "H"}
                                             {"type" "paragraph" "text" "P"}
                                             {"name" "host"}))]
        (expect (nil? (hs/request-error request)))
        ;; Two headings reading the same words are two decorations, never a name
        ;; collision: there is no identity to collide.
        (expect (nil? (hs/request-error (hi/normalize-request (spec {"type" "heading" "text" "H"}
                                                                    {"type" "heading" "text" "H"}
                                                                    {"name" "host"}))))))))
