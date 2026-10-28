(ns com.blockether.vis.internal.view.core-test
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.attachment.storage :as attachment-storage]
            [com.blockether.vis.internal.channel.events :as ce]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.mpl-capture :as mpl]
            [com.blockether.vis.internal.view.core :as hi]
            [com.blockether.vis.internal.view.materializer :as live]
            [com.blockether.vis.internal.view.sink :as live-sink]
            [com.blockether.vis.contract.view :as hs]
            [com.blockether.vis.internal.config.runtime-settings :as rt]
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

(defn- spec [& fields] {:title "Deploy" :nodes (vec fields) :timeout-ms 5000})

(defn- normalized-fields [& fields] (:nodes (hi/normalize-request (apply spec fields))))

(defn- await-request-id
  "Block until `pred-free` sees the request event, then return its request id.
   The waiter runs on another thread, so the event may land a beat later."
  [events]
  (loop [attempts 0]
    (if-let [id (some #(when (= :view/open (:op %)) (:view-id %)) @events)]
      id
      (if (< attempts 200)
        (do (Thread/sleep 10) (recur (inc attempts)))
        (throw (ex-info "no :view/open event arrived" {}))))))

(defn- start-request!
  "Open `request` on a fresh channel from another thread. Returns
   `{:future … :events … :request-id … :detach! …}` once the dialog event lands."
  [request]
  (let [chan
        (fresh-channel)

        events
        (atom [])]

    (ce/add-channel-event-listener! chan ::collector #(swap! events conj %))
    (let [fut (future (hi/request! (merge {:session-id "test-session"}
                                          (assoc request :channel-ids [chan]))))]
      {:future fut
       :events events
       :request-id (await-request-id events)
       :detach! #(ce/remove-channel-event-listener! chan ::collector)})))

(defdescribe
  input-secret-presentation-test
  (it "refuses password and OTP defaults without echoing their value"
      (doseq [kind
              ["password" "otp"]

              value
              ["fixture-default-secret" "123456" ""]]

        (let [error (try (normalized-fields {:id "credential" :type kind :default value})
                         nil
                         (catch clojure.lang.ExceptionInfo e (ex-message e)))]
          (expect (some? error))
          (expect (not (str/includes? (or error "") "fixture-default-secret")))
          (expect (not (str/includes? (or error "") "123456"))))))
  (it "omits secret defaults from nested public projections even for constructed requests"
      (let [request
            {:nodes
             [{:id "group"
               :type :group
               :direction :column
               :children
               [{:id "pw" :type :password :is-secret true :default "fixture-projection-secret"}
                {:id "code" :type :otp :is-secret true :default "123456"}
                {:id "name" :type :text :default "Ada"}]}]}

            fields
            (get-in (hi/request->view request) [:nodes 0 :children])]

        (expect (not (contains? (first fields) :default)))
        (expect (not (contains? (second fields) :default)))
        (expect (= "Ada" (:default (nth fields 2))))))
  (it "scrubs raw and coerced form secrets from every validator error"
      (let [raw
            "  fixture-submitted-secret  "

            fields
            (normalized-fields {:id "pw"
                                :type "password"
                                :validate (fn [value]
                                            (throw (ex-info (str "Rejected " value) {})))}
                               {:id "code" :type "otp"}
                               {:id "name"
                                :validate (fn [_ values]
                                            (str "Try again " raw
                                                 " / " (get values "pw")
                                                 " / 123 456 / " (get values "code")))}
                               {:id "choices" :type "multiselect" :options ["safe"]})

            result
            (hi/validate-values
              fields
              {:pw raw :code "123 456" :name "Ada" :choices ["fixture-submitted-secret"]})

            public
            (pr-str (:errors result))]

        (expect (false? (:is-accepted result)))
        (expect (= #{"pw" "name" "choices"} (set (keys (:errors result)))))
        (doseq [secret ["fixture-submitted-secret" "123 456" "123456"]]
          (expect (not (str/includes? public secret))))
        (expect (str/includes? public "[REDACTED]")))))

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
      (let [[field] (normalized-fields {:name "api_key"
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
      (let [[field] (normalized-fields (hi/spec<-json {"id" "token"
                                                       "type" "password"
                                                       "label" "API token"
                                                       "is_required" true
                                                       "max_length" 40
                                                       "description" "from the dashboard"}))]
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
      (let [[field] (normalized-fields
                      {:id "env" :type "select" :options [{:value "prod" :label "Production"}]})]
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
      (let [data (try (normalized-fields {:id "env" :type "wat"})
                      (catch clojure.lang.ExceptionInfo e (ex-data e)))]
        (expect (= :vis/view-invalid-field (:type data)))
        (expect (= "env" (:field-id data))))))

(defdescribe
  normalize-request-test
  (it "defaults the chrome, channels and timeout"
      (let [request (hi/normalize-request {:title "Deploy" :nodes [{:id "name"}]})]
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
      (expect (throws? clojure.lang.ExceptionInfo #(hi/normalize-request {:nodes [{:id "name"}]})))
      (expect (throws? clojure.lang.ExceptionInfo #(hi/normalize-request {:title "t"})))
      (expect (throws? clojure.lang.ExceptionInfo #(hi/normalize-request {:title "t" :nodes []}))))
  (it "rejects duplicate field names, however they are spelled"
      (expect (throws? clojure.lang.ExceptionInfo
                       #(hi/normalize-request {:title "t" :nodes [{:name "a"} {:name "a"}]})))
      ;; `:id` is the same identity under its legacy name — a spec cannot smuggle
      ;; a collision past this by mixing the two spellings.
      (expect (throws? clojure.lang.ExceptionInfo
                       #(hi/normalize-request {:title "t" :nodes [{:name "a"} {:id "a"}]}))))
  (it "keeps 0 as the indefinite wait, never clamps it, and refuses a negative"
      ;; `0` is the one way an extension says WAIT FOR THE HUMAN, so it has to
      ;; survive normalization untouched instead of being read as a missing key.
      (expect (= hi/no-timeout-ms
                 (:timeout-ms (hi/normalize-request
                                {:title "t" :nodes [{:id "a"}] :timeout-ms 0}))))
      (expect (true? (hi/indefinite-timeout? (:timeout-ms (hi/normalize-request {:title "t"
                                                                                 :nodes [{:id "a"}]
                                                                                 :timeout-ms 0})))))
      (expect (false? (hi/indefinite-timeout? hi/default-timeout-ms)))
      ;; Nothing is CLAMPED: a caller who states a day-long wait means it, and
      ;; quietly shortening it would only lie about when the answer arrives.
      (expect (= (* 24 60 60 1000)
                 (:timeout-ms (hi/normalize-request
                                {:title "t" :nodes [{:id "a"}] :timeout-ms (* 24 60 60 1000)}))))
      (expect (throws? clojure.lang.ExceptionInfo
                       #(hi/normalize-request {:title "t" :nodes [{:id "a"}] :timeout-ms -1})))
      (expect (throws? clojure.lang.ExceptionInfo
                       #(hi/normalize-request {:title "t" :nodes [{:id "a"}] :timeout-ms "soon"})))
      ;; A Python extension says the same thing over the strings boundary, where a
      ;; 0 is easy to mistake for a missing key and default back to five minutes.
      (expect (= hi/no-timeout-ms
                 (:timeout-ms (hi/normalize-request (hi/spec<-json {"title" "t"
                                                                    "nodes" [{"name" "a"}]
                                                                    "timeout_ms" 0}))))))
  (it "accepts a single channel id or a collection"
      (expect (= [:app]
                 (:channel-ids (hi/normalize-request
                                 {:title "t" :nodes [{:id "a"}] :channel-id :app}))))
      (expect (= [:tui :app]
                 (:channel-ids (hi/normalize-request
                                 {:title "t" :nodes [{:id "a"}] :channel-ids [:tui :app]})))))
  (it "keeps an explicit false — a false flag is a value, not a missing key"
      ;; `:is-cancellable false` is how an extension says THIS ONE MUST BE
      ;; ANSWERED. Dropping it defaulted the flag back to true and both surfaces
      ;; offered the escape hatch anyway: the TUI painted its Esc hint, the app
      ;; its Cancel button, and either could dismiss a request the extension had
      ;; forbidden dismissing.
      (expect (false? (:is-cancellable (hi/normalize-request
                                         {:title "t" :nodes [{:id "a"}] :is-cancellable false}))))
      (expect (false? (:is-cancellable (hi/normalize-request (hi/spec<-json {"title" "t"
                                                                             "nodes" [{"id" "a"}]
                                                                             "is_cancellable"
                                                                             false})))))))

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
   `base` is written with them the way a Python spec writes them. Always none:
   the keys the parser accepts are derived from these very sets. A key `base` already carries is
   proven by `base` parsing at all, and any other refusal is somebody else's
   business."
  [ks base normalize]
  (into #{}
        (comp (map wire-name)
              (remove #(contains? base %))
              (filter (fn [k]
                        (boolean (some-> (refusal #(normalize (hi/spec<-json (assoc base k nil))))
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
        (throw (ex-info "no pending input View" {:title title}))))))

(defdescribe
  spec-key-spelling-test
  (it
    "takes the snake_case string spelling a Python spec writes"
    (let [request
          (hi/normalize-request
            (hi/spec<-json {"title" "Deploy"
                            "description" "Pick a target"
                            "submit_label" "Go"
                            "cancel_label" "Stop"
                            "is_cancellable" false
                            "timeout_ms" 20000
                            "session_id" "sid-1"
                            "source" "asker"
                            "nodes"
                            [{"name" "env"
                              "label" "Target"
                              "description" "Where it lands."
                              "type" "select"
                              "options" [{"value" "prod" "label" "Production"}]
                              "is_required" true}
                             {"name" "note" "placeholder" "why" "max_length" 5 "default" "ok"}]}))

          [env note]
          (:nodes request)]

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
      (let [message (refusal #(normalized-fields (hi/spec<-json {"name" "env" "isRequired" true})))]
        (expect (some? message))
        (expect (str/includes? message "is_required")))
      (expect (some? (refusal #(normalized-fields (hi/spec<-json {"name" "env" "maxLength" 5})))))
      (expect (some? (refusal #(hi/normalize-request (hi/spec<-json {"title" "t"
                                                                     "nodes" [{"name" "a"}]
                                                                     "timeoutMs" 10}))))))
  (it "refuses a kebab-case STRING and a snake_case KEYWORD"
      ;; Each half of the contract, spelled the other half's way: strings are
      ;; snake_case (Python/JSON), keywords are kebab-case (Clojure).
      (expect (some? (refusal #(normalized-fields (hi/spec<-json {"name" "env"
                                                                  "is-required" true})))))
      (expect (some? (refusal #(normalized-fields {:name "env" :is_required true}))))
      (expect (some? (refusal #(hi/normalize-request
                                 {:title "t" :nodes [{:name "a"}] :timeout_ms 10})))))
  (it "refuses an unknown key, in a field, a request or an option"
      (let [message (refusal #(normalized-fields {:name "env" :requried true}))]
        (expect (some? message))
        (expect (str/includes? message "unknown field key")))
      (expect (some? (refusal #(hi/normalize-request
                                 {:title "t" :nodes [{:name "a"}] :retries 3}))))
      (expect (some? (refusal #(normalized-fields {:name "env"
                                                   :type "select"
                                                   :options [{"value" "a" "Label" "A"}]})))))
  (it "takes the `id` spelling of a name, and no longer a `help`"
      ;; `help` was `description`'s legacy alias; the vocabulary has one spelling
      ;; per key, so it is now as unknown as any other stray word.
      (let [[field] (normalized-fields (hi/spec<-json {"id" "env" "description" "prose"}))]
        (expect (= "env" (:name field)))
        (expect (= "prose" (:description field))))
      (let [message (refusal #(normalized-fields (hi/spec<-json {"name" "env" "help" "prose"})))]
        (expect (some? message))
        (expect (str/includes? message "unknown field key"))))
  (it "calls no key of the spec's own vocabulary unknown"
      ;; The parser derives the snake_case keys it accepts from the spec's key
      ;; sets, so nothing declared there can come back as an unknown key here.
      (expect (= #{} (unknown-keys hs/field-keys {"name" "env"} normalized-fields)))
      (expect (= #{}
                 (unknown-keys hs/group-keys
                               {"type" "group" "children" [{"name" "a"}]}
                               normalized-fields)))
      (expect (= #{}
                 (unknown-keys hs/option-keys
                               {"value" "a"}
                               #(normalized-fields
                                  (hi/spec<-json {"name" "env" "type" "select" "options" [%]})))))
      (expect (= #{}
                 (unknown-keys hs/request-keys
                               {"title" "t" "nodes" [{"name" "a"}]}
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
        (let [title
              (str "Deploy " (random-uuid))

              answer-json
              (future (hi/request-json! (json/write-json-str
                                          {"title" title
                                           "session_id" "test-session"
                                           "description" "Pick a target"
                                           ;; Channel routing is host-side — a guest cannot aim the
                                           ;; dialog anywhere, so this key is dropped, not honoured.
                                           "channel_ids" ["nowhere"]
                                           "timeout_ms" 20000
                                           "nodes" [{"name" "env"
                                                     "label" "Target"
                                                     "description" "Where it lands."
                                                     "type" "select"
                                                     "options" ["staging" "prod"]
                                                     "is_required" true}
                                                    {"name" "note" "type" "plaintext"}]})))

              request-id
              (await-pending-id title)

              [env]
              (:nodes (hi/pending-request request-id))]

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
      (let [message (refusal #(hi/request-json! (json/write-json-str
                                                  {"title" "t"
                                                   "nodes" [{"name" "env" "isRequired" true}]})))]
        (expect (some? message))
        (expect (str/includes? message "is_required")))))

(defdescribe
  coerce-values-test
  (it "reports every failing field at once"
      (let [fields
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
      (let [fields
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
      (let [fields
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
      (let [fields (normalized-fields
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
      (let [fields
            (normalized-fields {:id "name"})

            outcome
            (hi/coerce-values fields {"name" "vis" "smuggled" "nope"})]

        (expect (= {"name" "vis"} (:values outcome))))))

(defdescribe secret-vault-test
             (it "hands back an opaque handle instead of the plaintext"
                 (let [fields
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
             ;; Regression, issue #128: an `:otp` answered with the digits the human
             ;; typed, so a one-time code travelled as plaintext through the answer
             ;; map, the transcript and the logs.
             (it "hands a one-time code back as a handle too"
                 (let [fields
                       (normalized-fields {:id "code" :type "otp" :is-required true})

                       handle
                       (get-in (hi/coerce-values fields {"code" "123456"}) [:values "code"])]

                   (expect (hs/secret-handle? handle))
                   (expect (not (str/includes? (pr-str handle) "123456")))
                   (expect (= "123456" (hi/reveal-secret handle)))
                   (expect (true? (hi/forget-secret! handle)))))
             (it "does not mint a handle for an absent optional password"
                 (let [fields (normalized-fields {:id "token" :type "password"})]
                   (expect (nil? (get-in (hi/coerce-values fields {}) [:values "token"])))))
             (it "knows a handle from any other string"
                 (expect (not (hs/secret-handle? "hunter2")))
                 (expect (not (hs/secret-handle? nil)))
                 (expect (not (hs/secret-handle? 7)))))

(defdescribe
  request-lifecycle-test
  (it
    "publishes the input close before releasing its waiter"
    ;; Regression: SDK consumers could receive turn.completed before view.close.
    (doseq [action [:submit :cancel]]
      (let [{:keys [future request-id detach!]} (start-request! (spec {:id "name"}))
            answer (:promise (get @(var-get #'hi/pending) request-id))
            delivered (atom [])
            publish ce/publish-channel-event!]

        (try
          (with-redefs [ce/publish-channel-event! (fn [channel event]
                                                    (when (= :view/close (:op event))
                                                      (swap! delivered conj (realized? answer)))
                                                    (publish channel event))]
            (if (= :submit action) (hi/submit! request-id {"name" "Ada"}) (hi/cancel! request-id)))
          (expect (= [false] @delivered))
          (expect (map? (deref future 5000 ::blocked)))
          (finally (detach!))))))
  (it "publishes a dialog event, blocks, and returns the submitted values"
      (let [{:keys [future events request-id detach!]}
            (start-request!
              {:title "Deploy"
               :description "Pick a target"
               :nodes [{:id "env" :type "select" :options ["staging" "prod"] :is-required true}
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
             (expect (= [:view/open :view/close] (mapv :op @events)))
             (expect (zero? (count (hi/pending-requests))))
             (finally (detach!)))))
  (it "never shows a channel the plaintext or the waiting promise"
      (let [{:keys [future events request-id detach!]}
            (start-request! {:title "Deploy"
                             :nodes [{:id "token" :type "password" :is-required true}]})]
        (try (let [view (:view (first (filterv #(= :view/open (:op %)) @events)))]
               (expect (= request-id (:id view)))
               (expect (= "Deploy" (:title view)))
               (expect (nil? (:promise view)))
               (expect (nil? (:channel-ids view)))
               (expect (= [:password] (mapv :type (:nodes view))))
               (expect (= view (hi/pending-request request-id))))
             (hi/submit! request-id {"token" "hunter2"})
             (let [closed (first (filterv #(= :view/close (:op %)) @events))]
               (expect (= request-id (:view-id closed)))
               (expect (= "submitted" (get-in closed [:result :reason])))
               (expect (not (clojure.string/includes? (pr-str @events) "hunter2"))))
             (deref future 5000 ::blocked)
             (finally (detach!)))))
  (it "releases the waiter on cancel"
      (let [{:keys [future events request-id detach!]} (start-request! {:title "Deploy"
                                                                        :nodes [{:id "env"}]})]
        (try (expect (true? (hi/cancel! request-id)))
             (expect (= {:is-submitted false :reason "cancelled" :request-id request-id}
                        (deref future 5000 ::blocked)))
             (expect (false? (hi/cancel! request-id)) "a second cancel is a no-op")
             (expect (= "cancelled"
                        (get-in (first (filterv #(= :view/close (:op %)) @events))
                                [:result :reason])))
             (finally (detach!)))))
  (it "carries a custom cancel reason"
      (let [{:keys [future request-id detach!]} (start-request! {:title "Deploy"
                                                                 :nodes [{:id "env"}]})]
        (try (hi/cancel! request-id "channel detached")
             (expect (= "channel detached" (:reason (deref future 5000 ::blocked))))
             (finally (detach!)))))
  (it "releases the waiter when the timeout elapses"
      (let [{:keys [future request-id detach!]}
            (start-request! {:title "Deploy" :timeout-ms 120 :nodes [{:id "env"}]})]
        (try (expect (= {:is-submitted false :reason "timeout" :request-id request-id}
                        (deref future 5000 ::blocked)))
             (expect (nil? (hi/pending-request request-id)))
             (finally (detach!)))))
  (it "waits indefinitely when the timeout is 0, and still hears a late answer"
      ;; The twin of the test above: 120ms is a deadline, 0 is none. An
      ;; extension that must not guess an answer parks until the operator is
      ;; back at the keyboard, so nothing may settle this request on its own.
      (let [{:keys [future request-id detach!]}
            (start-request! {:title "Deploy" :timeout-ms 0 :nodes [{:id "env"}]})]
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
      (let [a
            (start-request! {:title "A" :nodes [{:id "x"}]})

            b
            (start-request! {:title "B" :nodes [{:id "x"}]})]

        (try (expect (<= 2 (hi/cancel-all! "shutting down")))
             (expect (= "shutting down" (:reason (deref (:future a) 5000 ::blocked))))
             (expect (= "shutting down" (:reason (deref (:future b) 5000 ::blocked))))
             (finally ((:detach! a)) ((:detach! b))))))
  (it "rejects an invalid spec before anything blocks"
      (expect (throws? clojure.lang.ExceptionInfo #(hi/request! {:title "t" :nodes []}))))
  ;; Regression, issue #113: a request raised outside a session parked its caller
  ;; anyway — the gateway bridge dropped it with a `request-without-session`
  ;; warning, so the companion app never learned the run was blocked and only a
  ;; TUI mounted in this very process could release it.
  (it "refuses a request that names no session"
      (let [ex (try (hi/request! {:title "Deploy" :nodes [{:id "env"}]})
                    nil
                    (catch clojure.lang.ExceptionInfo e e))]
        (expect (= :vis/view-invalid-request (:type (ex-data ex)))))))

(defdescribe
  non-cancellable-request-test
  (it "refuses every operator cancel and still yields to shutdown"
      ;; `:is-cancellable false` has to be enforced where BOTH surfaces meet —
      ;; the engine — or the TUI and the app each have to remember it, and the
      ;; one that forgets dismisses a request the extension declared mandatory.
      (let [rid
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
                                  :nodes [{:id "note" :is-required true}]}))

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
             ;; still typing, and the enclosing timeout wall (the Python eval watchdog)
             ;; billed that thinking time and killed the call with a bare
             ;; `Timeout (120s)` while the dialog was still on screen.
             (it
               "parks the enclosing timeout wall until the operator answers"
               (let [chan
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
                 (try (let [fut
                            (future (binding [rt/*blocking-wall-park* park]
                                      (hi/request! {:title "Login"
                                                    :session-id "test-session"
                                                    :nodes [{:id "otp" :label "OTP"}]
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
                 (let [{fut :future events :events request-id :request-id detach! :detach!}
                       (start-request! (spec {:id "otp"}))]
                   (try (expect (some? (hi/pending-request request-id)))
                        (future-cancel fut)
                        (expect (true? (await-true #(nil? (hi/pending-request request-id)))))
                        (expect (true? (await-true #(boolean (some (fn [e]
                                                                     (and (= :view/close (:op e))
                                                                          (= request-id
                                                                             (:view-id e))))
                                                                   @events)))))
                        (finally (detach!))))))

(defdescribe undeliverable-request-test
             ;; Regression, issue #104: a request published to channels nobody is mounted on
             ;; parked the extension for its whole timeout and then answered `"timeout"`, as
             ;; if a human had ignored a dialog that was never drawn — and not one log line
             ;; said the request had reached zero surfaces.
             (it "fails fast, and loudly, when no channel is listening"
                 (let [rid
                       (str "undeliverable-" (random-uuid))

                       {answer :value signals :signals}
                       (tel/with-signals (hi/request! {:id rid
                                                       :session-id "test-session"
                                                       :title "Deploy"
                                                       :timeout-ms 2000
                                                       :nodes [{:id "env"}]
                                                       :channel-ids [(fresh-channel)]}))]

                   (expect (= {:is-submitted false :reason "undeliverable" :request-id rid} answer))
                   (expect (nil? (hi/pending-request rid)))
                   (expect (signal? signals :error ::hi/request-undeliverable rid)))))

(defdescribe ask-gate-test
             ;; A managed subagent may not ask the person: its leader answers for it. The gate
             ;; refuses its form before any surface sees it.
             (it "refuses a session that may not ask, at once and without a trace on any channel"
                 (let [rid
                       (str "refused-" (random-uuid))

                       chan
                       (fresh-channel)

                       events
                       (atom [])

                       previous
                       @(var-get #'hi/ask-gate)]

                   (ce/add-channel-event-listener! chan ::refused-collector #(swap! events conj %))
                   (hi/set-ask-gate! #(not= "subagent-session" %))
                   (try (let [{answer :value signals :signals}
                              (tel/with-signals (hi/request! {:id rid
                                                              :session-id "subagent-session"
                                                              :title "Deploy"
                                                              :timeout-ms 2000
                                                              :nodes [{:id "env"}]
                                                              :channel-ids [chan]}))]
                          (expect (= {:is-submitted false :reason "undeliverable" :request-id rid}
                                     answer))
                          (expect (empty? @events))
                          (expect (nil? (hi/pending-request rid)))
                          (expect (signal? signals :warn ::hi/request-refused rid)))
                        (finally (hi/set-ask-gate! previous)
                                 (ce/remove-channel-event-listener! chan ::refused-collector)))))
             (it "opens the form of a session that may ask, and also when the gate fails"
                 (let [previous @(var-get #'hi/ask-gate)]
                   (doseq [gate [#(not= "subagent-session" %)
                                 (fn [_]
                                   (throw (ex-info "store down" {})))]]
                     (hi/set-ask-gate! gate)
                     (try (let [{:keys [future request-id detach!]}
                                (start-request! {:title "Deploy" :nodes [{:id "env"}]})]
                            (try (expect (some? (hi/pending-request request-id)))
                                 (expect (true? (hi/cancel! request-id)))
                                 (expect (= "cancelled" (:reason (deref future 5000 ::blocked))))
                                 (finally (detach!))))
                          (finally (hi/set-ask-gate! previous)))))))

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
  (hi/normalize-field (hi/spec<-json (merge {"name" "code" "type" "otp"} spec))))

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
  (it "refuses secret defaults even when they have the right number of digits"
      (expect (throws? clojure.lang.ExceptionInfo #(otp-field "default" "123 456")))
      (expect (throws? clojure.lang.ExceptionInfo #(otp-field "default" "12")))))

(defn- sign-in-request
  "Two passwords and an email — the form every validation feature is for."
  [& {:as overrides}]
  (hi/normalize-request
    (hi/spec<-json
      (merge {"title" "Sign in"
              "nodes" [{"name" "email"
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
                                     (when-not (= value (get values "pw"))
                                       "must match Password"))}]}
             overrides))))

(defn- errors
  "The `field id -> message` map a submission of `values` would be refused with."
  [request values]
  (or (:errors (hi/validate-values (:nodes request) values)) {}))

(def ^:private good-sign-in {"email" "ada@example.com" "pw" "hunter42" "pw2" "hunter42"})

(defdescribe
  validation-test
  "A validator is a FUNCTION the engine runs when the form is CONFIRMED: it
   answers nil/true for a good value, or the message the field should show."
  (it "accepts the form it was written for"
      (let [answer (hi/validate-values (:nodes (sign-in-request)) good-sign-in)]
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
      (let [optional (hi/normalize-request
                       (hi/spec<-json {"title" "t"
                                       "nodes" [{"name" "email"
                                                 "validate" #(when-not (re-find #"@" (str %))
                                                               "must be an email address")}]}))]
        (expect (= {} (errors optional {"email" ""})))
        (expect (= {} (errors optional {})))
        (expect (= {"email" "must be an email address"} (errors optional {"email" "nope"})))))
  (it "takes a validator's word for it, whatever shape that word arrives in"
      (let [request (hi/normalize-request
                      (hi/spec<-json {"title" "t"
                                      "nodes" [{"name" "team"
                                                :validate #(when-not (= "ops" %)
                                                             "must be an ops team")}
                                               {"name" "quiet" :validate (constantly nil)}
                                               {"name" "sure" :validate (constantly true)}
                                               {"name" "flag" :validate (constantly false)}
                                               {"name" "boom"
                                                :validate (fn [_]
                                                            (throw (ex-info "nope" {})))}]}))]
        (expect (= {} (errors request {"team" "ops" "flag" nil "boom" nil})))
        (expect (= {"team" "must be an ops team"
                    "flag" "is not valid"
                    "boom" "could not be validated: nope"}
                   (errors request {"team" "sre" "quiet" "x" "sure" "x" "flag" "x" "boom" "x"})))))
  (it "runs each validator exactly once per confirmation"
      ;; Validation is CODE: an extension's function may be slow and may talk to
      ;; something, so it is never run speculatively while the human types. One
      ;; confirmation, one call per field with a value.
      (let [calls
            (atom 0)

            request
            (hi/normalize-request (hi/spec<-json {"title" "t"
                                                  "nodes" [{"name" "a"
                                                            :validate (fn [_]
                                                                        (swap! calls inc)
                                                                        nil)}
                                                           {"name" "b"
                                                            :validate (fn [_]
                                                                        (swap! calls inc)
                                                                        "no")}]}))]

        (expect (= {"b" "no"} (errors request {"a" "x" "b" "y"})))
        (expect (= 2 @calls))
        (expect (= {"b" "no"} (errors request {"a" "x" "b" "y"})))
        (expect (= 4 @calls))))
  (it "refuses a validator that is not a function, or one it could never call"
      ;; Rules as DATA are gone: a rule map, a type name and a bare regex are all
      ;; refused where they used to be honoured, so a spec written against the
      ;; old DSL fails loudly instead of quietly checking nothing.
      (expect (throws? clojure.lang.ExceptionInfo
                       #(hi/normalize-field (hi/spec<-json {"name" "a"
                                                            "validate" {"type" "email"}}))))
      (expect (throws? clojure.lang.ExceptionInfo
                       #(hi/normalize-field (hi/spec<-json {"name" "a" "validate" "email"}))))
      (expect (throws? clojure.lang.ExceptionInfo
                       #(hi/normalize-field (hi/spec<-json {"name" "a" "validate" #"^[0-9]+$"}))))
      (expect (throws? clojure.lang.ExceptionInfo
                       #(hi/normalize-field (hi/spec<-json {"name" "a"
                                                            "validate" (fn []
                                                                         nil)}))))
      (expect (throws? clojure.lang.ExceptionInfo
                       #(hi/normalize-field (hi/spec<-json {"name" "a"
                                                            "validate" (fn [_ _ _]
                                                                         nil)})))))
  (it "never lets a validator near the wire"
      ;; A function cannot be serialized and a surface has no business owning a
      ;; rule: the view carries the field, never the check.
      (let [request
            (hi/normalize-request
              (hi/spec<-json {"title" "t" "nodes" [{"name" "a" "validate" (constantly "no")}]}))

            view-field
            (first (:nodes (hi/request->view request)))]

        (expect (not (contains? view-field :validate)))
        (expect (= {"a" "no"} (errors request {"a" "ada@example.com"})))))
  (it "never mints a vault handle just to answer whether a form is valid"
      ;; `validate-values` is pure: only a real submission fills the vault, so a
      ;; refused confirmation never leaves a password behind it.
      (let [request
            (sign-in-request)

            _
            (hi/forget-secrets!)

            answer
            (hi/validate-values (:nodes request) good-sign-in)]

        (expect (= "hunter42" (get-in answer [:values "pw"])))
        (expect (zero? (long (hi/forget-secrets!))))
        (expect (hs/secret-handle? (get-in (hi/coerce-values (:nodes request) good-sign-in)
                                           [:values "pw"])))
        (expect (pos? (long (hi/forget-secrets!)))))))

(defn- grouped-request
  "One row holding two fields, beside a plain stacked field — the smallest form
   that can tell a layout tree from a flat list."
  [& {:as overrides}]
  (hi/normalize-request (hi/spec<-json
                          (merge {"title" "Deploy"
                                  "nodes" [{"type" "group"
                                            "direction" "row"
                                            "children"
                                            [{"name" "host" "label" "Host" "is_required" true}
                                             {"name" "port"
                                              "label" "Port"
                                              "validate" #(when-not (re-matches #"[0-9]+" (str %))
                                                            "must be a whole number")}]}
                                           {"name" "note" "type" "multiline" "label" "Note"}]}
                                 overrides))))

(defdescribe
  group-layout-test
  "A `group` is layout only: it holds fields and the direction they run in, it
   holds no answer, and it nests — which is the whole composability claim."
  (it "keeps the tree on the request and flattens it for the answer"
      (let [{fields :nodes}
            (grouped-request)

            [group note]
            fields]

        (expect (= :group (:type group)))
        (expect (= :row (:direction group)))
        (expect (= ["host" "port"] (mapv :id (:children group))))
        (expect (= :multiline (:type note)))
        (expect (= ["host" "port" "note"] (mapv :id (hi/input-fields fields))))))
  (it "is control flow, not a kind of field"
      ;; A group answers nothing, so it has no place in the vocabulary of
      ;; ANSWERS: with `:group` among the field types, every value path carried a
      ;; branch for a node that can never take one and a field spec happily
      ;; accepted layout.
      (let [[group note] (:nodes (grouped-request))]
        (expect (not (contains? (set (vals hs/field-types)) hs/group-type)))
        (expect (= hs/group-type (:type group)))
        ;; Two contracts, one tree:
        (expect (nil? (hs/group-error group)))
        (expect (nil? (hs/field-error note)))
        (expect (some? (hs/field-error group)))
        (expect (some? (hs/group-error note)))
        ;; and the fork is taken above both, before a value key is parsed.
        (expect (str/includes? (refusal #(hi/normalize-field (hi/spec<-json {"type" "group"
                                                                             "children" [{"name"
                                                                                          "a"}]})))
                               "not a field"))))
  (it "stacks by default — a group without a :direction is a column"
      (let [[group] (normalized-fields (hi/spec<-json {"type" "group"
                                                       "children" [{"name" "a"} {"name" "b"}]}))]
        (expect (= :column (:direction group)))))
  (it "nests, so a row of stacks needs no new key"
      (let [[outer] (normalized-fields (hi/spec<-json {"type" "group"
                                                       "direction" "row"
                                                       "children" [{"type" "group"
                                                                    "children" [{"name" "a"}
                                                                                {"name" "b"}]}
                                                                   {"name" "c"}]}))]
        (expect (= :row (:direction outer)))
        (expect (= :column (:direction (first (:children outer)))))
        (expect (= ["a" "b" "c"] (mapv :id (hi/input-fields [outer]))))))
  (it "takes a positional id when the spec does not give one"
      ;; A form never patches a node, so a group without an `id` gets one from its
      ;; place in the tree, numbered depth first, and carries no `name`.
      (let [[outer] (normalized-fields (hi/spec<-json {"type" "group"
                                                       "children" [{"type" "group"
                                                                    "children" [{"name" "a"}]}]}))]
        (expect (= "group-2" (:id outer)))
        (expect (= "group-1" (:id (first (:children outer)))))
        (expect (not (contains? outer :name))))
      (let [[outer] (normalized-fields (hi/spec<-json
                                         {"id" "when" "type" "group" "children" [{"name" "a"}]}))]
        (expect (= "when" (:id outer)))))
  (it "takes a grid with a column cap and a wrap row"
      (let [[grid wrap] (normalized-fields (hi/spec<-json {"type" "group"
                                                           "direction" "grid"
                                                           "max_columns" 3
                                                           "children" [{"name" "a"} {"name" "b"}]})
                                           (hi/spec<-json {"type" "group"
                                                           "direction" "wrap"
                                                           "children" [{"name" "c"}]}))]
        (expect (= :grid (:direction grid)))
        (expect (= 3 (:max-columns grid)))
        (expect (= :wrap (:direction wrap)))
        (expect (not (contains? wrap :max-columns)))))
  (it "refuses a column cap outside a grid or outside its bounds"
      (let [message (fn [spec]
                      (try (normalized-fields (hi/spec<-json spec))
                           nil
                           (catch clojure.lang.ExceptionInfo e (ex-message e))))]
        (expect (str/includes?
                  (message
                    {"type" "group" "direction" "row" "max_columns" 2 "children" [{"name" "a"}]})
                  "only applies to a group with direction \"grid\""))
        (expect (str/includes?
                  (message
                    {"type" "group" "direction" "grid" "max_columns" 13 "children" [{"name" "a"}]})
                  "from 1 to 12"))
        (expect (str/includes?
                  (message
                    {"type" "group" "direction" "grid" "max_columns" 1.5 "children" [{"name" "a"}]})
                  "whole number"))))
  (it "refuses a direction that is not a flexbox direction"
      (expect (throws? clojure.lang.ExceptionInfo
                       #(normalized-fields (hi/spec<-json {"type" "group"
                                                           "direction" "diagonal"
                                                           "children" [{"name" "a"}]})))))
  (it "refuses a group with nothing in it"
      (expect (throws? clojure.lang.ExceptionInfo
                       #(normalized-fields (hi/spec<-json {"type" "group"}))))
      (expect (throws? clojure.lang.ExceptionInfo
                       #(normalized-fields (hi/spec<-json {"type" "group" "children" []})))))
  (it "refuses a key that only an answerable field could use"
      (expect (throws? clojure.lang.ExceptionInfo
                       #(normalized-fields (hi/spec<-json {"type" "group"
                                                           "is_required" true
                                                           "children" [{"name" "a"}]}))))
      (expect (throws? clojure.lang.ExceptionInfo
                       #(normalized-fields (hi/spec<-json {"type" "group"
                                                           "default" "x"
                                                           "children" [{"name" "a"}]})))))
  (it "refuses a layout key on a field that could never lay anything out"
      ;; The mirror of the rule above: `fields`/`direction` describe an
      ;; arrangement, and a leaf arranges nothing — silently ignoring them would
      ;; drop half the form the caller wrote.
      (expect (throws? clojure.lang.ExceptionInfo
                       #(normalized-fields (hi/spec<-json {"name" "host"
                                                           "children" [{"name" "a"}]}))))
      (expect (throws? clojure.lang.ExceptionInfo
                       #(normalized-fields (hi/spec<-json {"name" "host" "direction" "row"}))))
      (expect (str/includes? (try (normalized-fields (hi/spec<-json {"name" "host"
                                                                     "direction" "row"}))
                                  nil
                                  (catch clojure.lang.ExceptionInfo e (ex-message e)))
                             "only exists on a group")))
  (it "still refuses two fields with the same name across different groups"
      (expect (throws? clojure.lang.ExceptionInfo
                       #(normalized-fields
                          (hi/spec<-json {"type" "group" "children" [{"name" "a"}]})
                          (hi/spec<-json {"type" "group" "children" [{"name" "a"}]})))))
  (it "validates and answers the nested fields as if the tree were flat"
      (let [request (grouped-request)]
        (expect (= {"host" "is required" "port" "must be a whole number"}
                   (errors request {"port" "8o8o"})))
        (expect (= {"host" "vis.example.com" "port" "8080" "note" nil}
                   (:values (hi/coerce-values (:nodes request)
                                              {"host" "vis.example.com" "port" "8080"}))))))
  (it "reaches a sibling in another group, because `values` is flat"
      (let [request (hi/normalize-request
                      (hi/spec<-json
                        {"title" "Sign in"
                         "nodes" [{"type" "group"
                                   "direction" "row"
                                   "children" [{"name" "pw" "type" "password" "label" "Password"}]}
                                  {"name" "pw2"
                                   "type" "password"
                                   "validate" (fn [value values]
                                                (when-not (= value (get values "pw"))
                                                  "must match Password"))}]}))]
        (expect (= {} (errors request {"pw" "hunter42" "pw2" "hunter42"})))
        (expect (= {"pw2" "must match Password"} (errors request {"pw" "hunter42" "pw2" "nope"})))))
  (it "projects the tree onto the wire, direction and all, and never a validator"
      (let [view
            (hi/request->view (hi/normalize-request
                                (hi/spec<-json {"title" "Deploy"
                                                "nodes" [{"type" "group"
                                                          "direction" "row"
                                                          "label" "Target"
                                                          "children" [{"name" "host"
                                                                       :validate [(fn [_]
                                                                                    true)]}]}]})))

            [group]
            (:nodes view)]

        (expect (= :group (:type group)))
        (expect (= :row (:direction group)))
        (expect (= "Target" (:label group)))
        (expect (not (contains? (first (:children group)) :validate)))
        (expect (not (contains? (first (:children group)) :is-secret))))))

(defn- refusal-type
  "The `:type` of the `ex-info` a refusal carries, or nil when `thunk` wrongly
   went through."
  [thunk]
  (try (thunk) nil (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

;; The declared contract. `view` PARSES — either spelling of every key, a type
;; name looked up in a closed vocabulary — and `view.spec` DECLARES what parsing
;; has to produce. These tests hold the two together: the vocabulary has one
;; home, real normalized output satisfies the spec, and a form no surface could
;; paint is refused even when the engine itself built it.
(defdescribe
  declared-contract-test
  (it "every type a request can carry normalizes into the declared form"
      (let [request (hi/normalize-request
                      {:title "Deploy"
                       :description "everything at once"
                       :source "test"
                       :nodes [{:id "name" :placeholder "who" :default "ada"}
                               {:name "pw" :type "password"}
                               {:name "notes" :type "multiline" :min-length 2 :max-length 40}
                               {:name "env" :type "select" :options ["dev" "prod"] :default "prod"}
                               {:name "tags"
                                :type "multiselect"
                                :options [{:value "a" :label "A"} {:value "b" :label "B"}]
                                :default ["a"]} {:name "confirm" :type "checkbox" :default true}
                               {:name "pct" :type "range" :min 0 :max 100 :step 5 :default 25}
                               {:name "code" :type "otp"}
                               {:id "grp"
                                :type "group"
                                :direction "row"
                                :children [{:name "a"} {:name "b" :type "checkbox"}]}]
                       :timeout-ms 0})]
        (expect (nil? (hs/request-error request)))
        ;; The tree is checked as the two contracts it is: every leaf a field,
        ;; the group that arranges them a group, and its children part of the
        ;; same field contract — a nested field with no `:label` breaks the same
        ;; painter.
        (let [nodes (:nodes request)
              group (last nodes)]

          (expect (every? #(nil? (hs/field-error %)) (butlast nodes)))
          (expect (nil? (hs/group-error group)))
          (expect (every? #(nil? (hs/field-error %)) (:children group))))))
  (it "a request spelled the Python way lands in the very same form"
      (let [request (hi/normalize-request (hi/spec<-json {"title" "Deploy"
                                                          "nodes" [{"name" "env"
                                                                    "type" "select"
                                                                    "options" ["dev" "prod"]}]
                                                          "timeout_ms" 1000}))]
        (expect (nil? (hs/request-error request)))))
  (it "the closed vocabulary has ONE home"
      ;; Two copies of the type table drift, and the copy the normalizer reads
      ;; is the one that decides what a surface is asked to paint.
      (expect (nil? (ns-resolve 'com.blockether.vis.internal.view.core 'field-types)))
      (expect (= #{:plaintext :password :multiline :select :multiselect :checkbox :range :otp}
                 (set (vals hs/field-types))))
      (expect (str/includes? (refusal #(hi/normalize-field {:name "a" :type "slider"}))
                             "multiselect")))
  (it "refuses a field the engine itself mis-built"
      ;; Nothing an extension writes reaches these: they are what a bug in the
      ;; normalizer would hand a dialog — a picker with no options, one identity
      ;; whose two spellings disagree, a password no longer marked secret, a
      ;; slider whose knob starts outside its own track.
      (let [legal {:id "env"
                   :name "env"
                   :type :select
                   :label "Env"
                   :is-required false
                   :is-secret false
                   :options [{:value "dev" :label "dev"}]}]
        (expect (nil? (hs/field-error legal)))
        (expect (str/includes? (refusal #(#'hi/checked-field "env" (dissoc legal :options)))
                               "options"))
        (expect (= :vis/view-invalid-field
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
      (let [request (hi/normalize-request {:title "t" :nodes [{:id "a"}]})]
        (expect (nil? (hs/request-error request)))
        (expect (str/includes? (refusal #(#'hi/checked-request (dissoc request :submit-label)))
                               "submit-label"))
        (expect (= :vis/view-invalid-request
                   (refusal-type #(#'hi/checked-request (assoc request :channel-ids [])))))))
  (it "checks every answer on its way out, whatever settled it"
      ;; `settle!` is the one funnel — submitted, cancelled, timed out,
      ;; undeliverable — so an answer missing `:request-id`, or claiming a
      ;; submission it has no `:values` for, never reaches the parked thread.
      (expect (= :vis/view-invalid-answer
                 (refusal-type #(#'hi/checked-answer
                                  "r1"
                                  nil
                                  {:is-submitted true :reason "submitted" :request-id "r1"}))))
      (expect (= :vis/view-invalid-answer
                 (refusal-type
                   #(#'hi/checked-answer "r1" nil {:is-submitted false :reason "cancelled"}))))
      (let [answer (hi/request! {:title "unmounted"
                                 :session-id "test-session"
                                 :nodes [{:id "a"}]
                                 :channel-ids [(fresh-channel)]
                                 :timeout-ms 1000})]
        (expect (= "undeliverable" (:reason answer)))
        (expect (nil? (hs/answer-error nil answer)))))
  (it "checks a submitted answer against the very fields it answers"
      ;; The values ARE the inputs' data, so they are declared like the inputs:
      ;; a picker can only come back on an option it offered, a slider inside
      ;; its own track, a code at its own width, and a `password` as a vault
      ;; handle — the plaintext in an answer map is a leak, not a value. Nothing
      ;; an extension writes reaches these: this is what a coercion bug would
      ;; hand a blocked caller.
      (let [request
            (hi/normalize-request
              {:title "Deploy"
               :nodes [{:name "env" :type "select" :options ["dev" "prod"] :is-required true}
                       {:name "pw" :type "password"}
                       {:name "pct" :type "range" :min 0 :max 100 :step 5}
                       {:id "grp"
                        :type "group"
                        :direction "row"
                        :children [{:name "code" :type "otp" :max-length 6}]}]
               :timeout-ms 0})

            fields
            (:nodes request)

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
        (expect
          (= :vis/view-invalid-answer
             (refusal-type
               #(#'hi/checked-answer "r1" fields (answered (assoc legal "env" "staging")))))))))

;; A form is more than its questions: a `heading` names a section and a
;; `paragraph` explains one. Both are display components of the one catalog that
;; live views use. Neither can live in the vocabulary of ANSWERS and neither is
;; layout, so the field, group and display contracts refuse each other.
(defdescribe
  decoration-test
  "A `heading` and a `paragraph` are DISPLAY components: ink on the form, with no
   answer and no children. A form gives each one a positional id."
  (it "normalizes to its type, a positional id and the words it paints"
      (let [[head para field] (normalized-fields
                                (hi/spec<-json {"type" "heading" "text" "Connection"})
                                (hi/spec<-json {"type" "paragraph" "text" "Where it runs."})
                                (hi/spec<-json {"name" "host" "label" "Host"}))]
        (expect (= {:id "heading-1" :type :heading :text "Connection" :level 2} head))
        (expect (= {:id "paragraph-2" :type :paragraph :text "Where it runs."} para))
        (expect (= "host" (:name field)))))
  (it "answers nothing, so it never keys the values map — not even inside a group"
      (let [fields (normalized-fields (hi/spec<-json {"type" "heading" "text" "H"})
                                      (hi/spec<-json {"name" "host"})
                                      (hi/spec<-json {"type" "group"
                                                      "id" "g"
                                                      "children" [{"type" "paragraph" "text" "P"}
                                                                  {"name" "pw"
                                                                   "type" "password"}]}))]
        (expect (= ["host" "pw"] (mapv :name (hi/input-fields fields))))))
  (it "is neither a field nor a group, and the three contracts refuse each other"
      (let [[head field group]
            (normalized-fields (hi/spec<-json {"type" "heading" "text" "H"})
                               (hi/spec<-json {"name" "host"})
                               (hi/spec<-json {"type" "group" "id" "g" "children" [{"name" "a"}]}))]
        ;; One home for the vocabulary, as with the field and group tables.
        (expect (contains? (set (vals hs/live-node-types)) :heading))
        (expect (not (contains? (set (vals hs/field-types)) :heading)))
        (expect (nil? (hs/live-node-error head)))
        (expect (some? (hs/field-error head)))
        (expect (some? (hs/group-error head)))
        (expect (nil? (hs/group-error group)))
        (expect (some? (hs/group-error field)))))
  (it "refuses a decoration that tries to ask something"
      ;; A `:name` would make it keyed, a `:default` would make it answerable:
      ;; both are a spec that meant to add a field.
      (expect (str/includes? (refusal #(hi/normalize-node
                                         (hi/spec<-json {"type" "heading" "text" "H" "name" "h"})))
                             "name"))
      (expect (str/includes? (refusal #(hi/normalize-node (hi/spec<-json {"type" "paragraph"})))
                             "text"))
      ;; And it can never arrive on the FIELD path: the fork is taken once, above.
      (expect (str/includes? (refusal #(hi/normalize-field (hi/spec<-json {"type" "heading"
                                                                           "text" "H"})))
                             "display component")))
  (it "leaves a decorated request satisfying the declared contract"
      (let [request (hi/normalize-request (spec (hi/spec<-json {"type" "heading" "text" "H"})
                                                (hi/spec<-json {"type" "paragraph" "text" "P"})
                                                (hi/spec<-json {"name" "host"})))]
        (expect (nil? (hs/request-error request)))
        ;; Two headings reading the same words are two decorations, never a name
        ;; collision: there is no identity to collide.
        (expect (nil? (hs/request-error (hi/normalize-request
                                          (spec (hi/spec<-json {"type" "heading" "text" "H"})
                                                (hi/spec<-json {"type" "heading" "text" "H"})
                                                (hi/spec<-json {"name" "host"})))))))))

(defn- live-view
  "A legal normalized live view carrying one node of every keyed kind, so a test
   changes exactly the one thing it is about."
  [& node-overrides]
  {:id "view-1"
   :title "Deploying"
   :session-id "session-1"
   :channel-ids [:tui]
   :seq 0
   :created-at 1
   :timeout-ms 0
   :nodes (into [{:id "state" :type :status :text "connecting" :tone :running}
                 {:id "bar" :type :progress :value 0.25}
                 {:id "score" :type :stat :stats [{:id "failed" :label "failed" :value-text "0"}]}
                 {:id "plan" :type :steps :steps [{:id "build" :label "build" :tone :running}]}
                 {:id "out" :type :log :lines ["compiling"] :window-lines 2000}
                 {:id "hosts"
                  :type :table
                  :max-rows 100
                  :order :insertion
                  :columns [{:id "host" :label "Host"} {:id "state" :label "State"}]
                  :rows [{:id "h1" :cells ["alpha" "ok"] :tone :ok}]}
                 {:id "docs"
                  :type :link
                  :links [{:id "report" :label "Report" :target-kind :attachment :target "att-1"}]}]
                node-overrides)})

(defdescribe
  live-view-spec-test
  "The live vocabulary is closed in the same directions the form's is: an
   unknown node type is refused BY NAME, an id is an ADDRESS unique inside the
   view, and a form field is not a node of a view."
  (it "accepts a view holding one node of every kind"
      (expect (nil? (hs/live-view-error (live-view)))))
  (it "refuses a node type nobody can paint, rather than minting the keyword"
      (expect (some? (hs/live-view-error (assoc-in (live-view) [:nodes 0 :type] :marquee)))))
  (it "refuses two nodes sharing one id, because an id is the address a patch names"
      (expect (some? (hs/live-view-error (live-view
                                           {:id "state" :type :status :text "b" :tone :ok})))))
  (it "refuses a key no live node declares"
      (expect (some? (hs/live-view-error (assoc-in (live-view) [:nodes 0 :options] [])))))
  (it "takes the FORM's own row as a live node, and refuses layout declared on a leaf"
      (expect (nil? (hs/live-view-error
                      (live-view {:id "reading"
                                  :type :group
                                  :direction :row
                                  :children [{:id "beside" :type :status :text "b" :tone :ok}]}))))
      (expect (some? (hs/live-view-error
                       (live-view {:id "reading"
                                   :type :group
                                   :direction :sideways
                                   :children [{:id "beside" :type :status :text "b" :tone :ok}]}))))
      (expect (some? (hs/live-view-error
                       (live-view {:id "reading" :type :group :direction :row :children []})))
              "a row holding nothing is a layout with no content")
      (expect (some? (hs/live-view-error (assoc-in (live-view) [:nodes 0 :direction] :row)))
              "only a group arranges")
      (expect (some? (hs/live-view-error
                       (live-view {:id "reading"
                                   :type :group
                                   :direction :row
                                   :children [{:id "state" :type :status :text "b" :tone :ok}]})))
              "and an id is unique across the whole tree, because a patch names it alone"))
  (it "takes a form field as a live input, and refuses a live node offered as a field"
      ;; One catalog: a live view can hold an input, but an input is not a status.
      (expect (nil? (hs/live-node-error {:id "host"
                                         :name "host"
                                         :type :plaintext
                                         :label "Host"
                                         :is-required false
                                         :is-secret false})))
      (expect (some? (hs/field-error {:id "state" :type :status :text "x" :tone :running}))))
  (it "refuses a progress value outside its own fraction, and takes nil as indeterminate"
      (expect (some? (hs/live-view-error (assoc-in (live-view) [:nodes 1 :value] 4))))
      (expect (nil? (hs/live-view-error (assoc-in (live-view) [:nodes 1 :value] nil)))))
  (it "refuses a sorted order naming a column the table never declared"
      (expect (some? (hs/live-view-error (assoc-in (live-view) [:nodes 5 :order] {:by "nope"}))))
      (expect (nil? (hs/live-view-error
                      (assoc-in (live-view) [:nodes 5 :order] {:by "host" :dir :desc})))))
  (it "accepts a non-blank parent on rows and refuses an empty one"
      (expect (nil? (hs/live-view-error
                      (assoc-in (live-view) [:nodes 5 :rows 0 :parent] "Release apps"))))
      (expect (some? (hs/live-view-error (assoc-in (live-view) [:nodes 5 :rows 0 :parent] "")))))
  (it "keeps a table row's declared parent through normalization"
      (expect (= "tests · 3 variants"
                 (get-in (hi/normalize-live-node
                           {:id "jobs"
                            :type "table"
                            :columns [{:id "job" :label "Job"}]
                            :rows [{:id "linux" :cells ["linux"] :parent "tests · 3 variants"}]})
                         [:rows 0 :parent]))))
  (it "refuses a table row whose id is missing, because a row with no id cannot be upserted"
      (expect (some? (hs/live-view-error
                       (assoc-in (live-view) [:nodes 5 :rows] [{:cells ["a" "b"]}])))))
  (it "accepts declared table groups and refuses one without an id"
      (expect (nil? (hs/live-view-error
                      (assoc-in (live-view)
                        [:nodes 5 :groups]
                        [{:id "tests" :label "Tests" :tone :error :order 1 :is-open true}]))))
      (expect (some? (hs/live-view-error
                       (assoc-in (live-view) [:nodes 5 :groups] [{:label "Tests"}]))))
      (expect (some? (hs/live-view-error
                       (assoc-in (live-view) [:nodes 5 :groups] [{:id "tests" :open true}])))))
  (it "keeps a declared group's label, tone, order and open state through normalization"
      (expect (= [{:id "tests" :label "Tests" :tone :error :order 1 :is-open true}]
                 (:groups (hi/normalize-live-node
                            {:id "jobs"
                             :type "table"
                             :columns [{:id "job" :label "Job"}]
                             :groups
                             [{:id "tests" :label "Tests" :tone "error" :order 1 :is-open true}]
                             :rows [{:id "linux" :cells ["linux"] :parent "tests"}]})))))
  (it "leaves a row pointing at an undeclared group alone — an undeclared parent is still a group"
      (let [node (hi/normalize-live-node {:id "jobs"
                                          :type "table"
                                          :columns [{:id "job" :label "Job"}]
                                          :rows [{:id "linux" :cells ["linux"] :parent "tests"}]})]
        (expect (= "tests" (get-in node [:rows 0 :parent])))
        (expect (not (contains? node :groups))))))

(defdescribe
  live-patch-spec-test
  "A patch is vocabulary only: which node, which operation, which items. Whether
   the node EXISTS and whether a bound would be crossed is the materializer's
   answer, not the spec's."
  (it "accepts each of the six operations"
      (let [patch (fn [op]
                    (hs/live-patch-error {:view-id "view-1" :seq 1 :ops [op]}))]
        (expect (nil? (patch {:op :set :node-id "state" :text "done" :tone :ok})))
        (expect (nil? (patch {:op :append :node-id "out" :lines ["one" ""]})))
        (expect (nil? (patch
                        {:op :append :node-id "hosts" :rows [{:id "h2" :cells ["beta" "ok"]}]})))
        (expect (nil? (patch {:op :remove :node-id "hosts" :item-ids ["h1"]})))
        (expect (nil? (patch {:op :clear :node-id "out"})))
        (expect (nil? (patch {:op :add-node
                              :node-spec {:id "extra" :type :status :text "x" :tone :idle}})))
        (expect (nil? (patch {:op :remove-node :node-id "extra"})))))
  (it "accepts a blank :detail on a set, because clearing one is not a missing value"
      (expect (nil? (hs/live-patch-error
                      {:view-id "view-1" :seq 1 :ops [{:op :set :node-id "state" :detail ""}]})))
      (expect (= {:op :set :node-id "state" :detail ""}
                 (hi/normalize-live-op {:op :set :node-id "state" :detail "  "}))))
  (it "refuses an operation nobody implements, and a key no operation declares"
      (expect (some? (hs/live-patch-error
                       {:view-id "view-1" :seq 1 :ops [{:op :nudge :node-id "out"}]})))
      (expect (some? (hs/live-patch-error {:view-id "view-1"
                                           :seq 1
                                           :ops [{:op :clear :node-id "out" :lines ["x"]}]}))))
  (it "refuses a patch that names no node and one that carries no operation"
      (expect (some? (hs/live-patch-error {:view-id "view-1" :seq 1 :ops [{:op :clear}]})))
      (expect (some? (hs/live-patch-error {:view-id "view-1" :seq 1 :ops []}))))
  (it "refuses an op that would MOVE a node, because where it stands is declared once"
      (expect (some? (hs/live-patch-error {:view-id "view-1"
                                           :seq 1
                                           :ops [{:op :set :node-id "state" :direction :row}]}))))
  (it
    "refuses an `add-node` whose node is itself illegal, so a view cannot grow a shape it would have refused at declaration"
    (expect (some? (hs/live-patch-error {:view-id "view-1"
                                         :seq 1
                                         :ops [{:op :add-node
                                                :node-spec {:id "x" :type :marquee}}]})))))

(def ^:private done-picture
  "The one finished picture the verdict specs speak about."
  {:title "Deploy" :nodes [{:id "now" :type :status :text "done" :tone :ok}]})

(defdescribe
  live-result-spec-test
  "The verdict is the ONE thing the model reads back, and it reads DATA: the
   picture with its ids and tones, never the document the human reads. It is not
   an answer either — a verdict carries no field values, and an answer carries no
   artifact. It always says whether a PERSON ended it, and carries the words they
   left when they did."
  (it "accepts each ending, and refuses one nobody may branch on"
      (expect (nil? (hs/live-result-error {:view-id "v"
                                           :is-completed true
                                           :reason :completed
                                           :is-from-human false
                                           :view done-picture})))
      (expect (nil? (hs/live-result-error {:view-id "v"
                                           :is-completed false
                                           :reason :interrupted
                                           :is-from-human true
                                           :note "wrong subnet — I will re-run it against staging"
                                           :view done-picture
                                           :elided [{:node-id "now" :items 12}]
                                           :summary "12 of 40 hosts"
                                           :artifact-id "att-1"})))
      (expect (some? (hs/live-result-error {:view-id "v"
                                            :is-completed false
                                            :reason :bored
                                            :is-from-human false
                                            :view done-picture}))))
  (it
    "refuses a verdict carrying no picture: what the human watched reaches the model as data or the view did not end"
    (expect (some? (hs/live-result-error
                     {:view-id "v" :is-completed true :reason :completed :is-from-human false}))))
  (it "refuses the document in place of the picture, because prose is not a contract"
      (expect (some? (hs/live-result-error {:view-id "v"
                                            :is-completed true
                                            :reason :completed
                                            :is-from-human false
                                            :markdown "# Deploy\n\n[ok] **done**"}))))
  (it "refuses a picture carrying the mount, so a verdict cannot smuggle a session"
      (expect (some? (hs/live-result-error {:view-id "v"
                                            :is-completed true
                                            :reason :completed
                                            :is-from-human false
                                            :view (assoc done-picture :session-id "s")}))))
  (it "refuses a verdict claiming completion while naming why it stopped"
      (expect (some? (hs/live-result-error {:view-id "v"
                                            :is-completed true
                                            :reason :interrupted
                                            :is-from-human false
                                            :view done-picture})))
      (expect (some? (hs/live-result-error {:view-id "v"
                                            :is-completed false
                                            :reason :completed
                                            :is-from-human false
                                            :view done-picture}))))
  (it "refuses a verdict that will not say whether a person ended it"
      (expect (some? (hs/live-result-error
                       {:view-id "v" :is-completed true :reason :completed :view done-picture}))))
  (it "refuses a note nobody left, and a human's stop that does not read as interrupted"
      (expect (some? (hs/live-result-error {:view-id "v"
                                            :is-completed false
                                            :reason :failed
                                            :is-from-human false
                                            :note "wrong subnet"
                                            :view done-picture})))
      (expect (some? (hs/live-result-error {:view-id "v"
                                            :is-completed false
                                            :reason :timeout
                                            :is-from-human true
                                            :view done-picture}))))
  (it "refuses a note longer than the field the human typed it into"
      (expect (some? (hs/live-result-error {:view-id "v"
                                            :is-completed false
                                            :reason :interrupted
                                            :is-from-human true
                                            :note (apply str (repeat (inc (long hs/note-chars)) \x))
                                            :view done-picture}))))
  (it "refuses field values, because a live view never asked a question"
      (expect (some? (hs/live-result-error {:view-id "v"
                                            :is-completed true
                                            :reason :completed
                                            :is-from-human false
                                            :view done-picture
                                            :values {}}))))
  (it "leaves the form's answer contract alone: its reason is still the line a human reads"
      (expect (nil? (hs/answer-error nil
                                     {:is-submitted false :reason "cancelled" :request-id "r"})))
      (expect (some?
                (hs/answer-error nil {:is-submitted false :reason :interrupted :request-id "r"})))))

;; A live view — the interaction the human WATCHES. Nothing blocks, the record on
;; disk is the store of record, and the model reads ONE verdict at the end.

(def ^:private live-views-dir
  "The private var every view record hangs under, redefined per test so nothing
   here writes anywhere near the developer's own `~/.vis`."
  (requiring-resolve 'com.blockether.vis.internal.view.sink/views-dir))

(defn- temp-views-dir
  []
  (io/file (System/getProperty "java.io.tmpdir") (str "vis-views-" (random-uuid))))

(defn- live-spec
  "A live view in a session of its own, so its record shares a directory with
   nothing else."
  [& nodes]
  {:title "CI"
   :description "Blockether/vis · 42"
   :session-id (str "vis-test-" (random-uuid))
   :nodes (vec nodes)})

(defn- recorded
  "Run `f` with every view record under a temp directory of its own."
  [f]
  (with-redefs-fn {live-views-dir (constantly (temp-views-dir))} f))

(defn- watching
  "Open `spec` into a temp record, hand the mounted view to `f`, and close it —
   so a failing expectation never leaves a view pending for the next test."
  [spec f]
  (recorded (fn []
              (let [view (hi/open-live! spec)]
                (try (f view) (finally (hi/close-live! (:id view))))))))

(defn- live-refusal
  "The one-line reason `f` was refused, or nil when it was accepted."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-message e))))

(defdescribe
  live-secret-presentation-test
  (it
    "redacts open, patch, close and snapshot text before publishing or persistence"
    (recorded
      (fn []
        (let [chan
              (fresh-channel)

              events
              (atom [])

              spec
              (assoc (live-spec {:id "tail" :type "log" :lines ["password=fixture-open-secret"]})
                :channel-ids [chan]
                :title "token=fixture-title-secret")]

          (ce/add-channel-event-listener! chan ::secret-collector #(swap! events conj %))
          (try
            (let [view
                  (hi/open-live! spec)

                  view-id
                  (:id view)]

              (try
                (let [patched
                      (hi/patch-live! view-id
                                      [{:op "append"
                                        :node-id "tail"
                                        :lines ["Authorization: Bearer fixture-patch-secret"]}])

                      snapshot
                      {:node-id "tail"
                       :selected-ids ["first"]
                       :view (assoc view :description "private_key=fixture-snapshot-secret")}

                      result
                      (hi/close-live! view-id
                                      {:summary "password=fixture-close-secret"
                                       :reason :interrupted
                                       :selection-snapshots [snapshot]}
                                      {:note "secret=fixture-note-secret"})

                      file
                      (live-sink/view-file (:session-id spec) view-id)

                      lines
                      (live-sink/read-range file 0 10)]

                  (doseq [public [(pr-str view) (pr-str patched) (pr-str result) (pr-str @events)
                                  (slurp file) (live/->markdown (:view result) {:result result})]]
                    (doseq [secret ["fixture-open-secret" "fixture-title-secret"
                                    "fixture-patch-secret" "fixture-snapshot-secret"
                                    "fixture-close-secret" "fixture-note-secret"]]
                      (expect (not (str/includes? public secret)))))
                  (expect (= ["open" "patch" "close"] (mapv :kind lines)))
                  (expect (= [:view/open :view/patch :view/close] (mapv :op @events)))
                  (expect (= "tail" (get-in patched [:nodes 0 :id])))
                  (expect (= 1 (:seq patched)))
                  (expect (= view-id (:view-id result)))
                  (expect (str/includes? (slurp file) "[REDACTED]")))
                (finally (hi/close-live! view-id))))
            (finally (ce/remove-channel-event-listener! chan ::secret-collector)))))))
  (it "redacts the compact model result as well as direct sink callers"
      (recorded (fn []
                  (let [spec
                        (live-spec {:id "now" :type "status" :text "Ready"})

                        view
                        (hi/open-live! spec)]

                    (try (let [result (hi/close-live! (:id view)
                                                      {:model-result "token=fixture-model-secret"})]
                           (expect (= "token=[REDACTED]" result)))
                         (finally (hi/close-live! (:id view))))
                    (live-sink/open! (assoc view :title "password=fixture-direct-secret"))
                    (let [public (slurp (live-sink/view-file (:session-id spec) (:id view)))]
                      (expect (not (str/includes? public "fixture-direct-secret")))))))))

(defdescribe
  live-lifecycle-test
  (it
    "mounts a view, patches it BY NODE ID, and ends in the picture the model reads"
    (watching
      (live-spec {:id "now" :type "status" :text "Polling…" :tone "running"}
                 {:id "tail" :type "log" :window-lines 50})
      (fn [view]
        (let [view-id (:id view)]
          (expect (= 0 (:seq view)))
          (expect (= ["now" "tail"] (mapv :id (:nodes (hi/live-view view-id)))))
          (let [patched (hi/patch-live! view-id
                                        [{:op "set" :node-id "now" :text "18 jobs" :tone "ok"}
                                         {:op "append"
                                          :node-id "tail"
                                          :lines ["+ clojure -M:test" "18 passed"]}])]
            (expect (= 1 (:seq patched)))
            (expect (= "18 jobs" (:text (first (:nodes patched)))))
            (expect (= ["+ clojure -M:test" "18 passed"] (:lines (second (:nodes patched)))))
            (expect (= patched (hi/live-view view-id))))
          (let [result (hi/close-live! view-id {:summary "18 of 18 jobs finished"})]
            (expect (true? (:is-completed result)))
            (expect (= :completed (:reason result)))
            (expect (= view-id (:view-id result)))
            ;; The verdict carries the picture as DATA — the ids it declared and
            ;; the values it set — so the model acts on it instead of reading it
            ;; back out of prose.
            (expect (nil? (:markdown result)))
            (expect (= "18 of 18 jobs finished" (:summary result)))
            (expect (= ["now" "tail"] (mapv :id (:nodes (:view result)))))
            (expect (= {:id "now" :type :status :text "18 jobs" :tone :ok}
                       (first (:nodes (:view result)))))
            (expect (= ["+ clojure -M:test" "18 passed"] (:lines (second (:nodes (:view result))))))
            ;; …and the document the human reopens renders from that same picture.
            (expect (str/includes? (live/->markdown (:view result) {:result result})
                                   "18 of 18 jobs finished")))))))
  (it "opens a ROW that stands two nodes side by side, in the spelling the wire writes"
      (watching (live-spec
                  (hi/spec<-json
                    {"id" "reading"
                     "type" "group"
                     "direction" "row"
                     "children"
                     [{"id" "hosts" "type" "table" "columns" [{"id" "host" "label" "Host"}]}
                      {"id" "why" "type" "status" "text" "`db-2` failed the `openssl` check"}]}))
                (fn [view]
                  ;; A view lays itself out with the FORM's own vocabulary — one `group`
                  ;; carrying the nodes that stand together, in the direction it names.
                  (expect (= ["reading"] (mapv :id (:nodes view))))
                  (expect (= :row (:direction (first (:nodes view)))))
                  (expect (= ["hosts" "why"] (mapv :id (:children (first (:nodes view))))))
                  ;; No op carries layout, so a run cannot rearrange what a human is
                  ;; already reading — the attempt is refused BY NAME, not ignored.
                  (let [refused (live-refusal #(hi/patch-live!
                                                 (:id view)
                                                 [{:op "set" :node-id "why" :direction "column"}]))]
                    (expect (some? refused))
                    (expect (str/includes? refused "direction")))
                  ;; …and a patch still finds a node INSIDE the row by its id alone: the
                  ;; tree is how it is PAINTED, never how it is addressed.
                  (hi/patch-live! (:id view) [{:op "set" :node-id "why" :tone "error"}])
                  (let [row (first (:nodes (hi/live-view (:id view))))]
                    (expect (= :error (:tone (second (:children row)))))))))
  (it
    "keeps every ACCEPTED patch on disk, in the order the engine accepted them"
    (recorded
      (fn []
        (let [spec
              (live-spec {:id "now" :type "status" :text "Polling…" :tone "running"})

              view
              (hi/open-live! spec)

              view-id
              (:id view)]

          (hi/patch-live! view-id [{:op "set" :node-id "now" :text "18 jobs" :tone "ok"}])
          (hi/close-live! view-id {:summary "done"})
          (let [lines (live-sink/read-range (live-sink/view-file (:session-id spec) view-id) 0 10)]
            (expect (= ["open" "patch" "close"] (mapv :kind lines)))
            (expect (= view-id (:id (:view (first lines)))))
            (expect (= [1] (mapv :seq (keep :patch lines))))
            (expect (= "18 jobs"
                       (-> lines
                           second
                           :patch
                           :ops
                           first
                           :text)))
            (expect (true? (:is-completed (:result (last lines)))))
            ;; APPEND mode: a process that reopens a record adds to it. Truncating
            ;; here would drop a run somebody is still scrolling back through.
            (live-sink/open! view)
            (expect (= 4
                       (count (live-sink/read-range (live-sink/view-file (:session-id spec) view-id)
                                                    0
                                                    10)))))))))
  (it "returns an explicit model result while preserving the full artifact verdict"
      (recorded
        (fn []
          (let [spec
                (live-spec {:id "now" :type "status" :text "Done"})

                view
                (hi/open-live! spec)

                result
                (hi/close-live! (:id view)
                                {:summary "Human-facing close metadata"
                                 :model-result "Build failed: linux-tests · Run tests"})

                trailer
                (:result (last (live-sink/read-range (live-sink/view-file (:session-id spec)
                                                                          (:id view))
                                                     0
                                                     10)))]

            (expect (= "Build failed: linux-tests · Run tests" result))
            (expect (= "Human-facing close metadata" (:summary trailer)))
            (expect (= "Done"
                       (-> trailer
                           :view
                           :nodes
                           first
                           :text)))
            (expect (nil? (:model-result trailer)))))))
  ;; Regression, session a64d44c2-8228-455f-926e-b3381f19a93b: finished
  ;; selectable rows lost their alternate detail pictures when the live writer exited.
  (it
    "seals selection snapshots into the artifact record but not the model verdict"
    (recorded
      (fn []
        (let [spec
              (live-spec {:id "jobs"
                          :type "table"
                          :columns [{:id "job" :label "Job"}]
                          :rows [{:id "linux" :cells ["Linux"]}]
                          :is-selectable true
                          :selected-ids ["linux"]})

              view
              (hi/open-live! spec)

              view-id
              (:id view)

              snapshot
              {:node-id "jobs" :selected-ids ["linux"] :view view}

              result
              (hi/close-live! view-id {:selection-snapshots [snapshot]})

              trailer
              (:result (last (live-sink/read-range (live-sink/view-file (:session-id spec) view-id)
                                                   0
                                                   10)))]

          (expect (nil? (:selection-snapshots result)))
          (expect (= ["linux"]
                     (-> trailer
                         :selection-snapshots
                         first
                         :selected-ids)))
          (expect (= view-id
                     (-> trailer
                         :selection-snapshots
                         first
                         :view
                         :id)))))))
  ;; Regression, session f8115c8c-b997-49bf-a22b-81816d961fe3: the only picture an
  ;; extension can hand back is the snake_case one `state` ANSWERED it with, and the engine
  ;; held that to its own kebab-case vocabulary — so every archive snapshot was refused
  ;; ("invalid selection snapshot") and a watch that ran to the end died at its own close
  ;; instead of sealing what the human had been reading.
  (it
    "reads an archive snapshot spelled the way the extension holding it reads a view"
    (recorded
      (fn []
        (let [spec
              (live-spec {:id "jobs"
                          :type "table"
                          :columns [{:id "job" :label "Job"}]
                          :rows [{:id "linux" :cells ["Linux"] :tone "ok"}]
                          :is-selectable true
                          :selected-ids ["linux"]})

              view-id
              (:id (hi/open-live! spec))

              picture
              (-> {"op" "state" "view_id" view-id}
                  json/write-json-str
                  hi/live-json!
                  (json/read-json :key-fn identity)
                  (get "view"))

              _
              (hi/live-json! (json/write-json-str {"op" "close"
                                                   "view_id" view-id
                                                   "ending" {"selection_snapshots"
                                                             [{"node_id" "jobs"
                                                               "selected_ids" ["linux"]
                                                               "view" picture}]}}))

              trailer
              (-> (live-sink/read-range (live-sink/view-file (:session-id spec) view-id) 0 10)
                  last
                  :result)

              snapshot
              (first (:selection-snapshots trailer))]

          (expect (= "jobs" (:node-id snapshot)))
          (expect (= ["linux"] (:selected-ids snapshot)))
          (expect (= view-id
                     (-> snapshot
                         :view
                         :id)))
          ;; ONE spelling per record: the archived picture leaves through the same encoder as
          ;; the verdict's own, so a reader of either sees a live view rather than raw JSON
          ;; the engine never read.
          (expect (= (-> trailer
                         :view
                         :nodes
                         first
                         :type)
                     (-> snapshot
                         :view
                         :nodes
                         first
                         :type)))
          (expect (= (-> trailer
                         :view
                         :nodes
                         first
                         :rows
                         first
                         :tone)
                     (-> snapshot
                         :view
                         :nodes
                         first
                         :rows
                         first
                         :tone)))))))
  (it "refuses to mount a view that names no session"
      (recorded (fn []
                  (expect (re-find #"names no session"
                                   (live-refusal #(hi/open-live! (dissoc (live-spec {:id "now"
                                                                                     :type "status"
                                                                                     :text "Hi"})
                                                                   :session-id))))))))
  (it "refuses a patch for a view that is not open, and names it"
      (expect (re-find #"no live view"
                       (live-refusal #(hi/patch-live! "not-a-view"
                                                      [{:op "clear" :node-id "tail"}])))))
  (it "closes ONCE: a second close is a no-op, not a second verdict"
      (recorded (fn []
                  (let [view
                        (hi/open-live!
                          (live-spec {:id "now" :type "status" :text "Sweeping" :tone "running"}))

                        view-id
                        (:id view)]

                    (expect (true? (:is-completed (hi/close-live! view-id))))
                    (expect (nil? (hi/close-live! view-id)))
                    (expect (nil? (hi/live-view view-id)))))))
  (it "routes select and interrupt through the one closed View action seam"
      (recorded
        (fn []
          (let [view
                (hi/open-live! (live-spec {:id "jobs"
                                           :type "table"
                                           :is-selectable true
                                           :selected-ids ["a"]
                                           :columns [{:id "job" :label "Job"}]
                                           :rows [{:id "a" :cells ["A"]} {:id "b" :cells ["B"]}]}))

                view-id
                (:id view)

                selected
                (hi/action! view-id {:action "select" :node-id "jobs" :item-ids ["b"]})]

            (expect
              (=
                {:action :select :view-id view-id :is-accepted true :node-id "jobs" :item-ids ["b"]}
                selected))
            (expect (= ["b"] (get-in (hi/live-view view-id) [:nodes 0 :selected-ids])))
            (expect (throws? clojure.lang.ExceptionInfo
                             #(hi/action! view-id {:action :submit :values {}})))
            (expect (= {:action :interrupt :view-id view-id :is-accepted true}
                       (hi/action! view-id {:action :interrupt :note "enough"})))
            (expect (nil? (hi/live-view view-id)))))))
  (it "interrupts into a verdict that carries what the human watched, and the words they left"
      (recorded
        (fn []
          (let [view
                (hi/open-live! (live-spec
                                 {:id "now" :type "status" :text "Sweeping" :tone "running"}))

                view-id
                (:id view)]

            (hi/patch-live! view-id
                            [{:op "set" :node-id "now" :text "12 of 40 hosts" :tone "warn"}])
            (let [result (hi/interrupt-live! view-id "wrong subnet — re-run it against staging")]
              (expect (false? (:is-completed result)))
              (expect (= :interrupted (:reason result)))
              (expect (true? (:is-from-human result)))
              (expect (= "wrong subnet — re-run it against staging" (:note result)))
              (expect (= "12 of 40 hosts" (:text (first (:nodes (:view result)))))))))))
  (it "stops a view that left no words, and one nobody may refuse to stop"
      (recorded
        (fn []
          (let [view
                (hi/open-live! (live-spec {:id "now" :type "status" :text "Sweeping"}))

                result
                (hi/interrupt-live! (:id view))]

            (expect (true? (:is-from-human result))
                    "a person stopped it whether or not they said why")
            (expect (not (contains? result :note)))
            (expect (= :interrupted (:reason result))))
          ;; A form may be declared unanswerable-by-cancel; a view never can be,
          ;; so there is no shape of view a stop can bounce off.
          (expect (throws? clojure.lang.ExceptionInfo
                           #(hi/open-live! (assoc (live-spec
                                                    {:id "now" :type "status" :text "Sweeping"})
                                             :is-cancellable false)))
                  "the flag is not even a word the vocabulary still knows"))))
  (it "cuts a note longer than the field it was typed into instead of refusing the stop"
      (recorded (fn []
                  (let [view
                        (hi/open-live! (live-spec {:id "now" :type :status :text "Sweeping"}))

                        result
                        (hi/interrupt-live! (:id view)
                                            (apply str (repeat (+ 40 (long hs/note-chars)) \x)))]

                    (expect (= (long hs/note-chars) (count (:note result))))
                    (expect (true? (:is-from-human result)))))))
  (it "hands a body the view id and answers the verdict"
      (recorded (fn []
                  (let [result (hi/with-live!
                                 (live-spec
                                   {:id "now" :type "status" :text "Polling…" :tone "running"})
                                 (fn [view-id]
                                   (hi/patch-live!
                                     view-id
                                     [{:op "set" :node-id "now" :text "18 jobs" :tone "ok"}])))]
                    (expect (= :completed (:reason result)))
                    (expect (= "18 jobs" (:text (first (:nodes (:view result))))))))))
  (it "closes what a body opened even when the body throws"
      (recorded
        (fn []
          (let [spec
                (live-spec {:id "now" :type "status" :text "Polling…" :tone "running"})

                opened
                (atom nil)]

            (expect (throws? clojure.lang.ExceptionInfo
                             #(hi/with-live! spec
                                             (fn [view-id]
                                               (reset! opened view-id)
                                               (throw (ex-info "the poll died" {}))))))
            (expect (nil? (hi/live-view @opened)))
            (let [result (:result (last (live-sink/read-range
                                          (live-sink/view-file (:session-id spec) @opened)
                                          0
                                          10)))]
              (expect (= "failed" (:reason result)))
              (expect (= "the poll died" (:error result)))
              (expect (false? (:is-completed result))))))))
  (it "keeps forms and live views in ONE registry without confusing the two"
      (watching (live-spec {:id "now" :type "status" :text "Polling…" :tone "running"})
                (fn [view]
                  (let [view-id (:id view)]
                    ;; One id space, one cancel path — but a view is not a question, so it
                    ;; is never listed among the forms a run is BLOCKED on.
                    (expect (contains? (set (map :id (hi/live-views))) view-id))
                    (expect (not (contains? (set (map :id (hi/pending-requests))) view-id)))
                    ;; A pending view IS its materialized state: its live nodes.
                    (expect (some? (:nodes (hi/pending-request view-id))))
                    (expect (re-find #"live view, not a form"
                                     (live-refusal #(hi/submit! view-id {})))))))))

;; A settled view: Phase 5 of the live-view plan. Before it, dismissing the pane
;; lost the log the human had been watching — the only way back would have been
;; dumping it into the transcript, which is the cost the whole design removes.

(defn- filed
  "Open `spec`, run `f` against its id, then close it with `ending` while the
   per-block artifact sink is bound — the very collector a producing python block
   binds around `attach`. Answers `{:result … :rows … :file …}`: the verdict, every
   artifact the close filed, and the record they point at."
  ([spec]
   (filed spec
          {}
          (fn [_])))
  ([spec ending f]
   (recorded (fn []
               (let [sink
                     (atom [])

                     view
                     (hi/open-live! spec)

                     file
                     (live-sink/view-file (:session-id spec) (:id view))]

                 (f (:id view))
                 (let [result (binding [mpl/*attachment-sink* sink]
                                (hi/close-live! (:id view) ending))]
                   {:result result :rows @sink :file file}))))))

(defdescribe
  live-activity-owner-test
  ;; Regression #222: live frames and durable receipts name their actual Activity row.
  (it "captures the observed invocation and preserves it in the record and attachment"
      (let [invocation-id
            (str (random-uuid))

            activity-id
            (str (random-uuid))

            owner
            {:invocation-id invocation-id :activity-id activity-id}]

        (binding [extension/*current-invocation-id*
                  invocation-id

                  extension/*activity-history-id*
                  activity-id]

          (let [{:keys [rows file]} (filed (live-spec {:id "now" :type "status" :text "Running"})
                                           {}
                                           (fn [id]
                                             (expect (= owner (:owner (hi/live-view id))))))]
            (expect (= owner (:owner (first rows))))
            (expect (= {"invocation_id" invocation-id "activity_id" activity-id}
                       (get-in (json/read-json (first (str/split-lines (slurp file))))
                               ["view" "owner"])))))))
  (it "leaves unobserved views ownerless and refuses caller-provided ownership"
      (let [spec (live-spec {:id "now" :type "status" :text "Ready"})]
        (binding [extension/*activity-history-id* (str (random-uuid))]
          (expect (nil? (:owner (hi/normalize-live-view spec)))))
        (binding [extension/*current-invocation-id* (str (random-uuid))]
          (expect (= #{:invocation-id} (set (keys (:owner (hi/normalize-live-view spec)))))))
        (expect (some? (live-refusal #(hi/normalize-live-view (assoc spec
                                                                :owner {:invocation-id
                                                                        (str (random-uuid))}))))))))

(defdescribe
  live-artifact-test
  (it "settles a finished view into ONE artifact this session owns, and names it in the verdict"
      (let [{:keys [result rows]} (filed (live-spec {:id "tail" :type "log" :window-lines 50})
                                         {:summary "18 of 18 jobs finished"}
                                         (fn [view-id]
                                           (hi/patch-live! view-id
                                                           [{:op "append"
                                                             :node-id "tail"
                                                             :lines ["+ clojure -M:test"]}])))]
        (expect (= 1 (count rows)))
        (let [row (first rows)]
          (expect (= (:artifact-id result) (:id row)))
          (expect (= hs/live-artifact-media-type (:media-type row)))
          ;; Human-only: the model is TOLD the artifact exists and reads the picture
          ;; the verdict already carries, never the log.
          (expect (= "user" (:audience row)))
          (expect (= "ci.live.ndjson" (:filename row)))
          ;; ADDRESSED, never re-encoded: the row points at the very file the run has
          ;; been writing since `open`, by IDENTITY — a path only the machine that
          ;; wrote it could resolve is not an address a row gets to outlive.
          (expect (= (live-sink/record-uri (:session-id row) (:view-id row)) (:storage-uri row)))
          (expect (= (:view-id result) (:view-id row)))
          ;; The picture stays in the VERDICT — a row is a row.
          (expect (nil? (:view row))))))
  (it "seals the record with the artifact it filed, so a view read back off disk names it too"
      (let [{:keys [result file]}
            (filed (live-spec {:id "now" :type "status" :text "Polling…"}))

            sealed
            (:result (last (live-sink/read-range file 0 100)))]

        (expect (string? (:artifact-id result)))
        (expect (= (:artifact-id result) (:artifact-id sealed)))))
  (it "counts the RUN it recorded, not the verdict that sealed it"
      (let [{:keys [rows file]}
            (filed (live-spec {:id "tail" :type "log" :window-lines 50})
                   {}
                   (fn [view-id]
                     (dotimes [n 3]
                       (hi/patch-live! view-id
                                       [{:op "append" :node-id "tail" :lines [(str "line " n)]}]))))

            row
            (first rows)]

        ;; The declared view plus three accepted patches. The trailer the close then
        ;; writes states the verdict, and the verdict is already `:reason` here.
        (expect (= 4 (:line-count row)))
        (expect (= 5 (count (live-sink/read-range file 0 100))))
        (expect (pos? (long (:size row))))
        (expect (< (long (:size row)) (.length ^java.io.File file)))))
  (it "files an artifact however the view ended — the human's stop and the run's own death included"
      (doseq [[ending reason] [[{} :completed] [{:reason "failed" :error "the poll died"} :failed]
                               [{:reason "interrupted"} :interrupted]]]
        (let [{:keys [result rows]} (filed (live-spec {:id "now" :type "status" :text "Polling…"})
                                           ending
                                           (fn [_]))]
          (expect (= reason (:reason result)))
          (expect (= 1 (count rows)))
          (expect (= reason (:reason (first rows))))
          (expect (= (:artifact-id result) (:id (first rows)))))))
  (it "inlines a small record on top of the file, and leaves a big one where it is"
      (let [spec (live-spec {:id "now" :type "status" :text "Polling…"})]
        (expect (some? (:base64 (first (:rows (filed spec))))))
        ;; Past the floor the bytes stay on disk: holding a build log in memory as
        ;; base64 is exactly what addressing the record avoids.
        (expect (nil? (with-redefs [hs/live-artifact-inline-bytes 8]
                        (:base64 (first (:rows (filed spec)))))))))
  (it "reads the record back through the very rail that addressed it"
      (recorded
        (fn []
          (let [sink
                (atom [])

                spec
                (live-spec {:id "now" :type "status" :text "Polling…"})

                view
                (hi/open-live! spec)

                _
                (binding [mpl/*attachment-sink* sink]
                  (with-redefs [hs/live-artifact-inline-bytes 8]
                    (hi/close-live! (:id view))))

                row
                (first @sink)

                file
                (live-sink/view-file (:session-id spec) (:id view))]

            ;; Past the inline floor the row carries no bytes at all — only the
            ;; address — and an address no rail owns is a 404 on exactly the runs a
            ;; settled artifact exists for: the long ones.
            (expect (nil? (:base64 row)))
            (expect (= (slurp file)
                       (String. ^bytes (attachment-storage/resolve-bytes (:storage-uri row))
                                "UTF-8")))))))
  (it "files the artifact into the block that opened the view when the stop lands elsewhere"
      (recorded (fn []
                  (let [sink
                        (atom [])

                        view
                        (binding [mpl/*attachment-sink* sink]
                          (hi/open-live! (live-spec {:id "now" :type "status" :text "Polling…"})))

                        ;; The human's stop: a gateway thread with no collector of its own. The
                        ;; row still belongs to the block whose run produced it.
                        result
                        @(future (hi/interrupt-live! (:id view) "enough"))]

                    (expect (= :interrupted (:reason result)))
                    (expect (= 1 (count @sink)))
                    (expect (= (:artifact-id result) (:id (first @sink))))))))
  (it "ends a view opened where nothing collects artifacts without inventing one"
      (recorded (fn []
                  (let [spec
                        (live-spec {:id "now" :type "status" :text "Polling…"})

                        view
                        (hi/open-live! spec)

                        ;; Opened outside any block, so there is no collector to file
                        ;; into: the view still ends and the record is still sealed, the
                        ;; verdict simply names no artifact.
                        result
                        (hi/interrupt-live! (:id view))]

                    (expect (= :interrupted (:reason result)))
                    (expect (not (contains? result :artifact-id)))
                    (expect (some? (live-sink/verdict (live-sink/view-file (:session-id spec)
                                                                           (:id view)))))))))
  (it "files ONE artifact even when the view is closed twice"
      (recorded (fn []
                  (let [sink
                        (atom [])

                        view
                        (hi/open-live! (live-spec {:id "now" :type "status" :text "Polling…"}))]

                    (binding [mpl/*attachment-sink* sink]
                      (expect (some? (hi/close-live! (:id view))))
                      (expect (nil? (hi/close-live! (:id view)))))
                    (expect (= 1 (count @sink)))))))
  ;; A run the human stops LONG after the block returned: that block's collector was
  ;; drained the instant it returned, so a row filed into it is listed nowhere and the
  ;; picture the human watched is lost at the moment they stop it.
  (it
    "files a stop that lands after the block ended onto the iteration instead"
    (recorded
      (fn []
        (let [sink
              (atom [])

              filed
              (atom [])

              view
              (binding [mpl/*attachment-sink* sink]
                (hi/open-live! (live-spec {:id "now" :type "status" :text "Polling…"})))

              ;; The block ENDS: its iteration is nameable now, and from here the
              ;; record belongs to it.
              adopted
              (hi/adopt-open-views! (:session-id view) "iteration-7")

              ;; Restore the filer that the gateway wiring installed, not nil: later
              ;; tests in this JVM read it.
              previous
              @(var-get #'hi/late-artifact-filer)

              result
              (try (hi/set-late-artifact-filer! (fn [home att]
                                                  (swap! filed conj [home att])
                                                  {:id "row-1"}))
                   @(future (hi/interrupt-live! (:id view) "enough"))
                   (finally (hi/set-late-artifact-filer! previous)))]

          (expect (= [(:id view)] adopted))
          (expect (= :interrupted (:reason result)))
          (expect (empty? @sink) "the collector the block already drained is never written again")
          (expect (= ["iteration-7"] (mapv first @filed)))
          (expect (str/ends-with? (:filename (second (first @filed))) ".live.ndjson"))
          (expect (string? (:artifact-id result))))))))

(defdescribe
  live-view-holds-the-watchers-wall-test
  ;; Regression, reported from the phone: a `gh` watch of a CI run was killed at
  ;; `Timeout (300s)` while the build was still going, and the pane it left
  ;; behind never closed — hours later it still painted the last poll and still
  ;; offered a Stop nobody was listening to.
  (it "lifts the wall of the block that opened it, and hands it back at the close"
      (recorded (fn []
                  (let [{:keys [deadline hold]}
                        (rt/parkable-wall (System/currentTimeMillis) 20)

                        view
                        (binding [rt/*blocking-wall-hold* hold]
                          (hi/open-live! (live-spec {:id "now" :type "status" :text "Polling…"})))]

                    ;; Not a bigger number — NO number. A run whose picture a human
                    ;; is watching is billed nothing at all.
                    (expect (nil? @deadline))
                    (hi/close-live! (:id view))
                    (expect (some? @deadline))
                    (expect (< @deadline (+ (System/currentTimeMillis) 1000)))))))
  (it "stays lifted while the view is open, patched or not"
      (recorded
        (fn []
          (let [{:keys [deadline hold]} (rt/parkable-wall (System/currentTimeMillis) 20)]
            (binding [rt/*blocking-wall-hold* hold]
              (let [view (hi/open-live! (live-spec {:id "now" :type "status" :text "Polling…"}))]
                ;; Far past the 20ms budget, and nothing had to be re-bought to
                ;; get here: a picture that stands still is still being watched.
                (Thread/sleep 30)
                (hi/patch-live! (:id view) {:ops [{:op "set" :node-id "now" :text "Polling… 2"}]})
                (Thread/sleep 30)
                (expect (nil? @deadline))
                (hi/close-live! (:id view))
                (expect (< @deadline (+ (System/currentTimeMillis) 1000)))))))))
  (it "stays lifted while a SECOND view is still open"
      (recorded
        (fn []
          (let [{:keys [deadline hold]} (rt/parkable-wall (System/currentTimeMillis) 20)]
            (binding [rt/*blocking-wall-hold* hold]
              (let [one (hi/open-live! (live-spec {:id "now" :type "status" :text "One…"}))
                    two (hi/open-live! (live-spec {:id "now" :type "status" :text "Two…"}))]

                (hi/close-live! (:id one))
                ;; One watcher left is still a watcher.
                (expect (nil? @deadline))
                (hi/close-live! (:id two))
                (expect (< @deadline (+ (System/currentTimeMillis) 1000))))))))))

(defdescribe
  live-blocking-state-test
  (it "waits without publishing and omits an unchanged picture"
      (let [events
            (atom [])

            chan
            (fresh-channel)]

        (ce/add-channel-event-listener! chan ::wait-test #(swap! events conj %))
        (try
          (watching
            (assoc (live-spec {:id "now" :type "status" :text "Ready"}) :channel-ids [chan])
            (fn [view]
              (let [begin
                    (System/nanoTime)

                    before
                    @events

                    answer
                    (hi/live-dispatch
                      {"op" "state" "view_id" (:id view) "after_seq" (:seq view) "timeout_ms" 30})]

                (expect (true? (:timed-out answer)))
                (expect (not (contains? answer :view)))
                (expect (= before @events))
                (expect (>= (- (System/nanoTime) begin) 30000000)))))
          (finally (ce/remove-channel-event-listener! chan ::wait-test)))))
  (it "wakes for a patch or close, including a change before registration"
      (recorded
        (fn []
          (doseq [action
                  [:patch :close]

                  before?
                  (if (= action :patch) [false true] [false])]

            (let [view
                  (hi/open-live! (live-spec {:id "now" :type "status" :text "Ready"}))

                  id
                  (:id view)

                  change!
                  #(case action :patch (hi/patch-live! id
                                                       [{:op :set :node-id "now" :text "Changed"}])
                     :close (hi/interrupt-live! id "Stop"))]

              (try (when before? (change!))
                   (let [waiting (future (hi/live-dispatch {"op" "state"
                                                            "view_id" id
                                                            "after_seq" (:seq view)
                                                            "timeout_ms" 1000}))]
                     (when-not before? (Thread/sleep 30) (change!))
                     (let [answer (deref waiting 500 ::timeout)]
                       (expect (not= ::timeout answer))
                       (expect (= (= action :patch) (:is-open answer)))
                       (when (= action :patch)
                         (expect (= "Changed" (get-in answer [:view :nodes 0 :text]))))))
                   (finally (hi/close-live! id)))))))))

(defdescribe
  live-wait-validation-test
  (it "refuses invalid wait options before touching a view"
      (doseq [options [{"timeout_ms" -1} {"timeout_ms" 86400001} {"timeout_ms" 1}
                       {"timeout_ms" 1 "after_seq" -1} {"timeout_ms" 1 "after_seq" true}]]
        (expect (str/includes? (live-refusal #(hi/live-dispatch
                                                (merge {"op" "state" "view_id" "unused"} options)))
                               "timeout_ms")))))

(defdescribe
  live-view-block-ownership-test
  ;; Regression: a failed block closed another session's view and captured its artifact.
  (it
    "closes only its own views on failure or timeout, even within one session"
    (doseq [a-session
            ["session-A" "session-B"]

            reason
            [:failed :timeout]]

      (recorded
        (fn []
          (let [a-owner
                (atom false)

                b-owner
                (atom false)

                a-sink
                (atom [])

                b-sink
                (atom [])

                spec
                {:title "Watching" :nodes [{:id "status" :type "status" :text "Watching"}]}

                a
                (binding [hi/*block-owner*
                          a-owner

                          mpl/*attachment-sink*
                          a-sink]

                  (hi/open-live! (assoc spec :session-id a-session)))

                b
                (binding [hi/*block-owner*
                          b-owner

                          mpl/*attachment-sink*
                          b-sink]

                  (hi/open-live! (assoc spec :session-id "session-B")))]

            (try (let [swept (binding [mpl/*attachment-sink* b-sink]
                               (hi/close-abandoned! b-owner {:reason reason :error "B stopped"}))]
                   (expect (= [(:id b)] (mapv :view-id swept)))
                   (expect (= [reason] (mapv :reason swept)))
                   (expect (some? (hi/live-view (:id a))))
                   (expect (nil? (hi/live-view (:id b))))
                   (expect (empty? @a-sink))
                   (expect (= [(:id b)] (mapv :view-id @b-sink)))
                   (expect (empty? (hi/close-abandoned! nil {:reason reason}))))
                 (finally (hi/close-live! (:id a)) (hi/close-live! (:id b)))))))))
  (it "adopts only ended blocks in the owning session and never changes their home"
      (recorded
        (fn []
          (let [a-owner
                (atom false)

                b-owner
                (atom true)

                spec
                {:title "Watching" :nodes [{:id "status" :type "status" :text "Watching"}]}

                a
                (binding [hi/*block-owner* a-owner]
                  (hi/open-live! (assoc spec :session-id "session-A")))

                b
                (binding [hi/*block-owner* b-owner]
                  (hi/open-live! (assoc spec :session-id "session-B")))]

            (try (expect (empty? (hi/adopt-open-views! "session-A" "A-iteration")))
                 (expect (= [(:id b)] (hi/adopt-open-views! "session-B" "B-iteration")))
                 (reset! a-owner true)
                 (expect (empty? (hi/adopt-open-views! "session-B" "B-next")))
                 (expect (= [(:id a)] (hi/adopt-open-views! "session-A" "A-iteration")))
                 (expect (empty? (hi/adopt-open-views! "session-A" "A-next")))
                 (finally (hi/close-live! (:id a)) (hi/close-live! (:id b)))))))))

(defdescribe
  live-view-human-stop-test
  ;; Regression: a model loop around a watch tool opened the view again after each human
  ;; stop, in the same block, and the person's note never reached the model.
  (it "refuses a new view in the block that a person stopped and answers each stop once"
      (recorded
        (fn []
          (let [owner
                (atom false)

                other
                (atom false)

                spec
                (live-spec {:id "status" :type "status" :text "Watching"})

                first-view
                (binding [hi/*block-owner* owner]
                  (hi/open-live! spec))]

            @(future (hi/interrupt-live! (:id first-view) "I stop it for a reason"))
            (let [refusal (try (binding [hi/*block-owner* owner]
                                 (hi/open-live! spec))
                               nil
                               (catch clojure.lang.ExceptionInfo e e))]
              (expect (= :vis/live-stopped-by-human (:type (ex-data refusal))))
              (expect (str/includes? (ex-message refusal) "I stop it for a reason")))
            (let [elsewhere (binding [hi/*block-owner* other]
                              (hi/open-live! spec))]
              (expect (some? (hi/live-view (:id elsewhere))))
              (hi/close-live! (:id elsewhere)))
            (expect (= [{:title "CI" :note "I stop it for a reason"}] (hi/take-human-stops! owner)))
            (expect (empty? (hi/take-human-stops! owner)))
            (expect (empty? (hi/take-human-stops! nil)))))))
  (it "names a stop without a note and ignores a stop after its block ended"
      (recorded
        (fn []
          (let [owner
                (atom false)

                spec
                (live-spec {:id "status" :type "status" :text "Watching"})

                quiet
                (binding [hi/*block-owner* owner]
                  (hi/open-live! spec))

                late
                (binding [hi/*block-owner* owner]
                  (hi/open-live! spec))]

            (hi/interrupt-live! (:id quiet))
            (let [stops (hi/take-human-stops! owner)]
              (expect (= [{:title "CI"}] stops))
              (expect (str/includes? (hi/human-stop-text (first stops)) "without a note")))
            (reset! owner true)
            (hi/interrupt-live! (:id late) "too late")
            (expect (empty? (hi/take-human-stops! owner))))))))

(defdescribe
  live-patch-close-race-test
  (it "returns the ended verdict when close wins after the first registry lookup"
      (recorded
        (fn []
          (let [spec
                (live-spec {:id "status" :type "status" :text "Watching"})

                view
                (hi/open-live! spec)

                lookup
                #'hi/live-entry

                original
                @lookup

                close-once
                (atom true)]

            (try
              (let [answer
                    (with-redefs-fn {lookup
                                     (fn [id]
                                       (let [entry (original id)]
                                         (when (and entry (compare-and-set! close-once true false))
                                           (hi/close-live! id {:reason :failed :error "B failed"}))
                                         entry))}
                      #(binding [extension/*current-environment* {:session-id (:session-id spec)}]
                         (hi/live-dispatch
                           {"op" "patch" "view_id" (:id view) "patch" {"ops" []}})))]
                (expect (false? (:is-open answer)))
                (expect (= "failed" (get-in answer [:result :reason]))))
              (finally (hi/close-live! (:id view)))))))))

(defdescribe
  live-input-events-test
  "A live view holds inputs. A change sets one value, and a submit button sends
   the inputs of its nearest group as one event. A secret stays out of the record."
  (it
    "queues change and submit events, refuses invalid input, and keeps secrets out"
    (recorded
      (fn []
        (let [spec
              (assoc (live-spec {:id "outside" :type "plaintext" :label "Outside"}
                                {:id "actions"
                                 :type "group"
                                 :direction "column"
                                 :children
                                 [{:id "target" :type "plaintext" :label "Target" :is-required true}
                                  {:id "pct" :type "range" :label "Pct" :min 0 :max 10}
                                  {:id "pw" :type "password" :label "Token"}
                                  {:id "apply" :type "button" :label "Apply" :action "submit"}]})
                :title "Form")

              view
              (hi/open-live! spec)

              view-id
              (:id view)

              act!
              #(hi/action! view-id %)]

          (try
            (expect (= {"pct" "must be a number"}
                       (:errors (act! {:action :change :node-id "pct" :value "abc"}))))
            (expect (= {"target" "is required"}
                       (:errors (act! {:action :submit :node-id "apply" :values {}}))))
            (expect (some? (hi/live-view view-id)) "an invalid value never closes the view")
            (expect (true? (:is-accepted (act! {:action :change :node-id "target" :value "db-2"}))))
            (expect (true? (:is-accepted
                             (act! {:action :change :node-id "pw" :value "fixture-live-secret"}))))
            (expect (true? (:is-accepted (act! {:action :submit
                                                :node-id "apply"
                                                :values {"pw" "fixture-live-secret"}}))))
            (let [{:keys [events cursor is-open]}
                  (hi/live-events view-id 0 0)

                  [changed secret submitted]
                  events

                  nodes
                  (tree-seq map? :children {:children (:nodes (hi/live-view view-id))})

                  node
                  (fn [id]
                    (first (filter #(= id (:id %)) nodes)))]

              (expect (true? is-open))
              (expect (= 3 cursor))
              (expect (= {:type :change :node-id "target" :value "db-2"}
                         (select-keys changed [:type :node-id :value])))
              (expect (= {:type :change :node-id "pw" :is-set true}
                         (select-keys secret [:type :node-id :value :is-set])))
              (expect (= :submit (:type submitted)))
              (expect (= #{"target" "pct" "pw"} (set (keys (:values submitted))))
                      "the submit collects only its nearest group")
              (expect (str/starts-with? (get-in submitted [:values "pw"]) "vis-secret:"))
              (expect (= "db-2" (:value (node "target"))))
              (expect (= 1 (:clicks (node "apply"))))
              (expect (not (contains? (node "pw") :value)))
              (expect (empty? (:events (hi/live-events view-id 3 0))))
              (doseq [text [(pr-str events)
                            (slurp (live-sink/view-file (:session-id spec) view-id))]]
                (expect (not (str/includes? text "fixture-live-secret")))))
            (finally (hi/close-live! view-id))))))))

(defn- owned-open!
  "Open a live view through the extension door of `owner` and answer its wire map."
  [owner view]
  (json/read-json (hi/live-json! (json/write-json-str {"op" "open" "view" view}) owner)
                  :key-fn
                  identity))

(defn- panel-view
  [key]
  {"title" "Build"
   "placement" "panel"
   "session_id" "forge-session"
   "key" key
   "nodes" [{"id" "state" "type" "status" "text" "Build passed" "tone" "ok"}
            {"id" "go" "type" "button" "label" "Rerun" "action" "activate"}]})

(defn- owned
  "Run `f` with temp records and no stale views, and save nothing outside the test."
  [f]
  (recorded (fn []
              (try (f) (finally (hi/flush-owned!) (reset! @#'hi/stale-views {}))))))

(defdescribe
  owned-live-view-test
  "A panel view belongs to the extension that opened it. One key holds one view,
   the last picture comes back stale after a restart, and an unload closes the
   views of the extension."
  (it "refuses a panel view outside extension code, and one without a key"
      (owned (fn []
               (expect (str/includes? (live-refusal #(owned-open! nil (panel-view "build")))
                                      "only extension code"))
               (expect (str/includes? (live-refusal #(owned-open! {:extension "forge"}
                                                                  (dissoc (panel-view "build")
                                                                    "key")))
                                      "needs a :key")))))
  (it
    "keeps one view for each key, refuses other extensions and sends events to the listener"
    (owned
      (fn []
        (let [ctx
              (Object.)

              events
              (atom [])

              owner
              {:extension "forge"
               :context ctx
               :listener (fn [event _session-id]
                           (swap! events conj event))}

              first-id
              (get (owned-open! owner (panel-view "build")) "view_id")

              second-id
              (get (owned-open! owner (panel-view "build")) "view_id")

              other-id
              (get (owned-open! owner (panel-view "deploy")) "view_id")

              view
              (hi/live-view second-id)]

          (expect (nil? (hi/live-view first-id)) "a second view of a key supersedes the first")
          (expect (= "superseded"
                     (get-in (hi/live-dispatch {"op" "state" "view_id" first-id})
                             [:result :reason])))
          (expect (= [:panel "forge" "build" "forge-session"]
                     [(:placement view) (:extension view) (:key view) (:session-id view)]))
          (expect (not (contains? view :owner)))
          (expect (str/includes? (live-refusal #(hi/live-json! (json/write-json-str
                                                                 {"op" "close" "view_id" second-id})
                                                               {:extension "other"}))
                                 "belongs to extension forge"))
          (expect (true? (:is-accepted (hi/action! second-id {:action :activate :node-id "go"}))))
          (expect (await-true #(= 1 (count @events))) "the listener gets the pushed event")
          (expect (= [:activate "go"] ((juxt :type :node-id) (first @events))))
          (expect (true? (hi/owner-context-open? ctx)) "the idle reaper keeps this context")
          (expect (false? (hi/owner-context-open? (Object.))))
          (expect (= #{second-id other-id} (set (hi/close-owned-by! ctx))))
          (expect (false? (hi/owner-context-open? ctx)))
          (expect (every? nil? (map hi/live-view [second-id other-id])))))))
  (it "keeps the sandbox of an extension that holds an app until the extension unloads"
      ;; Live apps: the idle sandbox reaper asks owner-context-open?. Without the
      ;; app, five idle minutes disconnected every app of the extension.
      (owned
        (fn []
          (let [ctx
                (Object.)

                view-id
                (get (owned-open! {:extension "forge" :context ctx}
                                  {"title" "Board"
                                   "placement" "attachment"
                                   "session_id" "forge-session"
                                   "key" "board"
                                   "nodes" [{"id" "go" "type" "button" "label" "Go"}]})
                     "view_id")]

            (try (expect (true? (hi/owner-context-open? ctx)) "the idle reaper keeps this context")
                 (expect (false? (hi/owner-context-open? (Object.))))
                 (hi/close-owned-by! ctx)
                 (expect (false? (hi/owner-context-open? ctx)))
                 (expect (true? (:is-stale (hi/app-view "forge-session" view-id)))
                         "the unloaded extension leaves its app disconnected")
                 (finally (hi/close-live! view-id)))))))
  (it "brings the saved picture back as a stale view that only a dismiss removes"
      (owned
        (fn []
          (let [owner
                {:extension "forge" :context (Object.)}

                view-id
                (get (owned-open! owner (panel-view "build")) "view_id")

                picture
                (hi/live-view view-id)]

            (live-sink/save-owned! [picture])
            (hi/close-live! view-id)
            (reset! @#'hi/save-due false)
            (expect (= 1 (hi/restore-owned!)))
            (let [stale (first (filter #(= view-id (:id %)) (hi/live-views)))]
              (expect (true? (:is-stale stale)))
              (expect (= "Build" (:title stale))))
            (expect (= "stale" (:reason (hi/action! view-id {:action :activate :node-id "go"}))))
            (expect (true? (hi/dismiss-stale! view-id)))
            (expect (not-any? #(= view-id (:id %)) (hi/live-views)))
            (hi/flush-owned!)
            (expect (empty? (live-sink/load-owned)) "a dismiss saves the pictures again")))))
  (it "closes a stale view when its extension loads again without its key"
      (owned (fn []
               (let [owner
                     {:extension "forge" :context (Object.)}

                     view-id
                     (get (owned-open! owner (panel-view "build")) "view_id")]

                 (live-sink/save-owned! [(hi/live-view view-id)])
                 (hi/close-live! view-id)
                 (reset! @#'hi/save-due false)
                 (expect (= 1 (hi/restore-owned!)))
                 (expect (= [] (hi/close-unclaimed-stale! "other")) "another extension keeps it")
                 (expect (= [view-id] (hi/close-unclaimed-stale! "forge")))
                 (expect (not-any? #(= view-id (:id %)) (hi/live-views)))
                 (hi/flush-owned!)
                 (expect (empty? (live-sink/load-owned))
                         "the closed stale view is not saved again"))))))

(defn- app-seam!
  "Cross one envelope through the sandbox app seam, inside session `sid`."
  [sid envelope]
  (binding [extension/*current-environment* {:session-id sid}]
    (json/read-json (hi/app-json! (json/write-json-str envelope)) :key-fn identity)))

(defn- sandbox-app
  [sid]
  {"op" "open"
   "view" {"title" "Review"
           "session_id" sid
           "placement" "attachment"
           "nodes" [{"id" "general" "type" "thread" "messages" []}]}})

(defdescribe
  app-sandbox-seam-test
  ;; #421: `attach(ui.app(...))` in the python_execution sandbox opens and drives
  ;; apps through `app-json!`, and only the apps of its own session.
  (it "opens and drives an app of its own session"
      (recorded
        (fn []
          (let [sid
                (str (random-uuid))

                view-id
                (get (app-seam! sid (sandbox-app sid)) "view_id")]

            (try (expect (string? (get (app-seam! sid
                                                  {"op" "reply"
                                                   "view_id" view-id
                                                   "node_id" "general"
                                                   "body" "Done."})
                                       "message_id")))
                 (expect (= ["Done."]
                            (mapv #(get % "body")
                                  (get-in (app-seam! sid {"op" "state" "view_id" view-id})
                                          ["view" "nodes" 0 "messages"]))))
                 (finally (hi/close-live! view-id)))))))
  (it "refuses to open a view that is no app"
      (let [sid (str (random-uuid))]
        (expect (str/includes?
                  (live-refusal
                    #(app-seam! sid (assoc-in (sandbox-app sid) ["view" "placement"] "inline")))
                  "opens only apps"))))
  (it "refuses to drive a live view that is no app"
      (watching (live-spec {:id "now" :type :status :text "running"})
                (fn [view]
                  (expect (str/includes? (live-refusal #(app-seam! (:session-id view)
                                                                   {"op" "patch"
                                                                    "view_id" (:id view)
                                                                    "patch" {"ops" []}}))
                                         "is no app")))))
  (it "refuses an app of another session"
      (recorded (fn []
                  (let [sid
                        (str (random-uuid))

                        view-id
                        (get (app-seam! sid (sandbox-app sid)) "view_id")]

                    (try (expect (str/includes? (live-refusal #(app-seam! (str (random-uuid))
                                                                          {"op" "state"
                                                                           "view_id" view-id}))
                                                "belongs to another session"))
                         (finally (hi/close-live! view-id))))))))

(defn- shortcut-app
  [sid shortcuts]
  {"op" "open"
   "view" {"title" "Board"
           "session_id" sid
           "placement" "attachment"
           "nodes" [{"id" "go" "type" "button" "label" "Go"}]
           "shortcuts" shortcuts}})

(defdescribe
  app-shortcut-test
  ;; Live apps C1: an app declares keyboard shortcuts; an action shortcut sends
  ;; one owner event, and a node shortcut presses its node.
  (it "keeps the shortcuts in the view state"
      (recorded (fn []
                  (let [sid
                        (str (random-uuid))

                        shortcuts
                        [{"key" "r" "action" "refresh" "label" "Refresh"}
                         {"key" "Enter" "node_id" "go"}]

                        view-id
                        (get (app-seam! sid (shortcut-app sid shortcuts)) "view_id")]

                    (try (expect (= shortcuts
                                    (get-in (app-seam! sid {"op" "state" "view_id" view-id})
                                            ["view" "shortcuts"])))
                         (finally (hi/close-live! view-id)))))))
  (it
    "pushes one owner event for an action shortcut"
    (recorded
      (fn []
        (let [sid
              (str (random-uuid))

              view-id
              (get (app-seam! sid (shortcut-app sid [{"key" "r" "action" "refresh"}])) "view_id")]

          (try
            (expect
              (= {:action :shortcut :view-id view-id :is-accepted true :key "r" :shortcut "refresh"}
                 (hi/action! view-id {:action :shortcut :key "r"})))
            (expect (= [{"type" "shortcut" "action" "refresh" "key" "r"}]
                       (mapv #(select-keys % ["type" "action" "key"])
                             (get (app-seam!
                                    sid
                                    {"op" "events" "view_id" view-id "after" 0 "timeout_ms" 0})
                                  "events"))))
            (finally (hi/close-live! view-id)))))))
  (it
    "answers unbound only for a free key"
    (recorded
      (fn []
        (let [sid
              (str (random-uuid))

              view-id
              (get (app-seam! sid (shortcut-app sid [{"key" "Enter" "node_id" "go"}])) "view_id")]

          (try (expect (= "unbound" (:reason (hi/action! view-id {:action :shortcut :key "x"}))))
               (finally (hi/close-live! view-id)))))))
  ;; Live apps D5: a client cannot tell a node shortcut from a free key, so the
  ;; engine presses the node itself, as a surface does.
  (it
    "presses the node of a node shortcut with one click and one activate event"
    (recorded
      (fn []
        (let [sid
              (str (random-uuid))

              view-id
              (get (app-seam!
                     sid
                     (-> (shortcut-app sid
                                       [{"key" "Enter" "node_id" "go"} {"key" "c" "node_id" "card"}
                                        {"key" "d" "node_id" "off"}])
                         (update-in
                           ["view" "nodes"]
                           into
                           [{"id" "card" "type" "card" "title" "C" "is_selectable" true}
                            {"id" "off" "type" "button" "label" "Off" "is_disabled" true}])))
                   "view_id")]

          (try
            (expect
              (= {:action :shortcut :view-id view-id :is-accepted true :key "Enter" :node-id "go"}
                 (hi/action! view-id {:action :shortcut :key "Enter"})))
            (expect
              (= {:action :shortcut :view-id view-id :is-accepted true :key "c" :node-id "card"}
                 (hi/action! view-id {:action :shortcut :key "c"})))
            (expect
              (= {:action :shortcut :view-id view-id :is-accepted false :key "d" :reason "disabled"}
                 (hi/action! view-id {:action :shortcut :key "d"})))
            (expect (= [{"type" "activate" "node_id" "go"} {"type" "activate" "node_id" "card"}]
                       (mapv #(select-keys % ["type" "node_id"])
                             (get (app-seam!
                                    sid
                                    {"op" "events" "view_id" view-id "after" 0 "timeout_ms" 0})
                                  "events"))))
            (expect (= {"go" 1 "card" 1 "off" 0}
                       (into {}
                             (map (juxt #(get % "id") #(get % "clicks")))
                             (get-in (app-seam! sid {"op" "state" "view_id" view-id})
                                     ["view" "nodes"]))))
            (finally (hi/close-live! view-id)))))))
  ;; Live apps D3: the owner hears an accepted selection.
  (it "pushes one select event with the selected row ids"
      (recorded
        (fn []
          (let [sid
                (str (random-uuid))

                view-id
                (get (app-seam! sid
                                (assoc-in (shortcut-app sid [])
                                  ["view" "nodes"]
                                  [{"id" "t"
                                    "type" "table"
                                    "is_selectable" true
                                    "columns" [{"id" "n" "label" "N"}]
                                    "rows" [{"id" "r1" "cells" ["1"]} {"id" "r2" "cells" ["2"]}]}]))
                     "view_id")]

            (try (expect (:is-accepted
                           (hi/action! view-id {:action :select :node-id "t" :item-ids ["r2"]})))
                 (expect (= [{"type" "select" "node_id" "t" "item_ids" ["r2"]}]
                            (mapv #(select-keys % ["type" "node_id" "item_ids"])
                                  (get (app-seam!
                                         sid
                                         {"op" "events" "view_id" view-id "after" 0 "timeout_ms" 0})
                                       "events"))))
                 (finally (hi/close-live! view-id)))))))
  ;; Live apps D2: the refusal names the count, the maximum and the remedy.
  (it "refuses 65 nodes with the count, the maximum 64 and a table remedy"
      (let [sid
            (str (random-uuid))

            fields
            (fn [n]
              (mapv #(hash-map "id" (str "f" %) "type" "plaintext" "label" "F") (range n)))

            reason
            (live-refusal
              #(app-seam! sid (assoc-in (shortcut-app sid []) ["view" "nodes"] (fields 65))))]

        (expect (str/includes?
                  reason
                  "live view has 65 nodes; the maximum is 64. Put a collection in a table")
                reason)))
  (it
    "refuses a key that is not valid and lists the valid keys"
    (recorded
      (fn []
        (let [sid
              (str (random-uuid))

              view-id
              (get (app-seam! sid (shortcut-app sid [{"key" "r" "action" "refresh"}])) "view_id")]

          (try (expect (str/includes? (live-refusal #(hi/action! view-id
                                                                 {:action :shortcut :key "Escape"}))
                                      "ArrowUp"))
               (finally (hi/close-live! view-id)))))))
  (it "refuses duplicate keys and a node that cannot run"
      (let [sid (str (random-uuid))]
        (expect (str/includes? (live-refusal #(app-seam! sid
                                                         (shortcut-app sid
                                                                       [{"key" "r" "action" "a"}
                                                                        {"key" "r" "action" "b"}])))
                               "r"))
        (expect (some? (live-refusal
                         #(app-seam! sid (shortcut-app sid [{"key" "r" "node_id" "missing"}])))))))
  (it "replaces the shortcuts with a patch and removes them with an empty list"
      (recorded
        (fn []
          (let [sid
                (str (random-uuid))

                view-id
                (get (app-seam! sid (shortcut-app sid [{"key" "r" "action" "refresh"}])) "view_id")

                shortcuts!
                (fn [shortcuts]
                  (app-seam! sid
                             {"op" "patch"
                              "view_id" view-id
                              "patch" {"ops" [{"op" "shortcuts" "shortcuts" shortcuts}]}})
                  (get-in (app-seam! sid {"op" "state" "view_id" view-id}) ["view" "shortcuts"]))]

            (try (expect (= [{"key" "n" "action" "next"}]
                            (shortcuts! [{"key" "n" "action" "next"}])))
                 (expect (nil? (shortcuts! [])))
                 (expect (= "unbound" (:reason (hi/action! view-id {:action :shortcut :key "n"}))))
                 (finally (hi/close-live! view-id)))))))
  (it "refuses to remove a node that a shortcut uses"
      (recorded
        (fn []
          (let [sid
                (str (random-uuid))

                view-id
                (get (app-seam! sid (shortcut-app sid [{"key" "g" "node_id" "go"}])) "view_id")]

            (try (expect (some? (live-refusal #(app-seam! sid
                                                          {"op" "patch"
                                                           "view_id" view-id
                                                           "patch" {"ops" [{"op" "remove-node"
                                                                            "node_id" "go"}]}}))))
                 (finally (hi/close-live! view-id))))))))

(defn- note-app
  [sid note]
  {"op" "open"
   "view" {"title" "Board"
           "session_id" sid
           "placement" "attachment"
           "nodes" [{"id" "go" "type" "button" "label" "Go"}]
           "agent_note" note}})

(defdescribe
  app-agent-note-test
  ;; An app and an HTML page keep a note of their state. The model reads the
  ;; latest notes of its session with the next user message.
  (it "keeps a trimmed note and lists it for the model"
      (recorded
        (fn []
          (let [sid
                (str (random-uuid))

                view-id
                (get (app-seam! sid (note-app sid "  Card 3 is done  ")) "view_id")]

            (try (expect (= "Card 3 is done"
                            (get-in (app-seam! sid {"op" "state" "view_id" view-id})
                                    ["view" "agent_note"])))
                 (expect
                   (= [{"id" view-id "kind" "app" "title" "Board" "agent_note" "Card 3 is done"}]
                      (hi/agent-notes sid)))
                 (expect (= [] (hi/agent-notes (str (random-uuid)))) "another session has no note")
                 (finally (hi/close-live! view-id)))
            (expect (= [] (hi/agent-notes sid)) "a closed app has no note")))))
  (it "replaces the note with a patch and removes it with a blank text"
      (recorded
        (fn []
          (let [sid
                (str (random-uuid))

                view-id
                (get (app-seam! sid (note-app sid "Card 1")) "view_id")

                note!
                (fn [text]
                  (app-seam! sid
                             {"op" "patch"
                              "view_id" view-id
                              "patch" {"ops" [{"op" "agent-note" "text" text}]}})
                  (get-in (app-seam! sid {"op" "state" "view_id" view-id}) ["view" "agent_note"]))]

            (try (expect (= "Card 2" (note! " Card 2 ")))
                 (expect (= "Card 2" (note! "Card 2")) "the same note again keeps the same state")
                 (expect (nil? (note! "  ")))
                 (expect (= [] (hi/agent-notes sid)))
                 (finally (hi/close-live! view-id)))))))
  (it "refuses a note on an inline view and a note that is too long"
      (recorded (fn []
                  (let [sid (str (random-uuid))]
                    (expect (str/includes? (str (live-refusal #(hi/open-live!
                                                                 (assoc (live-spec {:id "now"
                                                                                    :type "status"
                                                                                    :text "Ready"})
                                                                   :agent-note "x"))))
                                           "only an app"))
                    (expect (some?
                              (live-refusal
                                #(app-seam! sid (note-app sid (apply str (repeat 4001 "x")))))))))))
  (it "lists the latest note of each HTML page after the app notes"
      (recorded
        (fn []
          (let [sid
                (str (random-uuid))

                view-id
                (get (app-seam! sid (note-app sid "Card 1")) "view_id")]

            (try (live-sink/note-page! sid {:id "a1" :title "board.html" :agent-note " Row 1 "})
                 (live-sink/note-page! sid {:id "a1" :title "board.html" :agent-note "Row 2"})
                 (expect (= [{"id" view-id "kind" "app" "title" "Board" "agent_note" "Card 1"}
                             {"id" "a1" "kind" "page" "title" "board.html" "agent_note" "Row 2"}]
                            (hi/agent-notes sid)))
                 (live-sink/note-page! sid {:id "a1" :title "board.html" :agent-note " "})
                 (expect (= ["app"] (mapv #(get % "kind") (hi/agent-notes sid)))
                         "a blank text removes the page note")
                 (finally (hi/close-live! view-id)))))))
  (it "keeps only the newest page notes"
      (recorded (fn []
                  (let [sid (str (random-uuid))]
                    (doseq [n (range 10)]
                      (live-sink/note-page!
                        sid
                        {:id (str "p" n) :title (str "p" n ".html") :agent-note (str "Row " n)}))
                    (expect (= (mapv #(str "p" %) (range 2 10))
                               (mapv #(get % "id") (hi/agent-notes sid)))))))))

(defn- judge-next!
  "Answer the next `validate` question of app `view-id` with `errors`, or answer
   nothing for `:silent`. Answers the question."
  [sid view-id errors]
  (future
    (loop [after
           0

           tries
           0]

      (let [answer
            (app-seam! sid {"op" "events" "view_id" view-id "after" after "timeout_ms" 2000})

            question
            (first (filter #(= "validate" (get % "type")) (get answer "events")))]

        (cond question (do (when-not (= :silent errors)
                             (app-seam! sid
                                        {"op" "validated"
                                         "view_id" view-id
                                         "request_id" (get question "request_id")
                                         "errors" errors}))
                           question)
              (< tries 5) (recur (get answer "cursor") (inc tries)))))))

(defdescribe
  app-validate-test
  ;; Live apps D4: the owner judges the inputs that it gave validators before the
  ;; host accepts a submit. A function never crosses as JSON.
  (it
    "refuses with the owner's errors, accepts a clean answer and drops the question"
    (recorded
      (fn []
        (let [sid
              (str (random-uuid))

              view-id
              (get (app-seam! sid
                              {"op" "open"
                               "validators" ["port"]
                               "view"
                               {"title" "Ports"
                                "session_id" sid
                                "placement" "attachment"
                                "nodes"
                                [{"id" "port" "type" "plaintext" "label" "Port"}
                                 {"id" "send" "type" "button" "label" "Send" "action" "submit"}]}})
                   "view_id")]

          (try (let [judged
                     (judge-next! sid view-id {"port" "must be digits" "other" "ignored"})

                     refused
                     (hi/action! view-id
                                 {:action :submit
                                  :node-id "send"
                                  :values {"port" "abc"}
                                  :idempotency-key "key-0001"})]

                 (expect (= {"port" "must be digits"} (:errors refused)))
                 (expect (= "invalid" (:reason refused)))
                 (expect (= {"port" "abc"} (get @judged "values"))))
               (let [_
                     (judge-next! sid view-id {})

                     accepted
                     (hi/action! view-id
                                 {:action :submit
                                  :node-id "send"
                                  :values {"port" "80"}
                                  :idempotency-key "key-0002"})]

                 (expect (true? (:is-accepted accepted))))
               (expect (= ["submit"]
                          (mapv #(get % "type")
                                (get (app-seam!
                                       sid
                                       {"op" "events" "view_id" view-id "after" 0 "timeout_ms" 0})
                                     "events"))))
               (finally (hi/close-live! view-id)))))))
  (it
    "refuses a submit that the owner does not judge in time"
    (recorded
      (fn []
        ;; The host drops an unanswered question at the deadline, so the silent
        ;; judge needs time to read it first, also on a loaded machine.
        (with-redefs [hi/validate-wait-ms 1500]
          (let [sid (str (random-uuid))
                view-id (get (app-seam!
                               sid
                               {"op" "open"
                                "validators" ["port"]
                                "view"
                                {"title" "Ports"
                                 "session_id" sid
                                 "placement" "attachment"
                                 "nodes"
                                 [{"id" "port" "type" "plaintext" "label" "Port"}
                                  {"id" "send" "type" "button" "label" "Send" "action" "submit"}]}})
                             "view_id")]

            (try (let [question (judge-next! sid view-id :silent)
                       refused (hi/action! view-id
                                           {:action :submit
                                            :node-id "send"
                                            :values {"port" "80"}
                                            :idempotency-key "key-0003"})]

                   (expect (= {"send" "The app did not answer. Try again."} (:errors refused)))
                   (expect (false? (get (app-seam! sid
                                                   {"op" "validated"
                                                    "view_id" view-id
                                                    "request_id" (get @question "request_id")
                                                    "errors" {}})
                                        "is_accepted"))
                           "a late answer changes nothing"))
                 (finally (hi/close-live! view-id))))))))
  ;; Live apps D4b: validators follow the node tree on every door.
  (it
    "declares a validated input that a patch adds and drops it with its node"
    (recorded
      (fn []
        (let [sid
              (str (random-uuid))

              view-id
              (get (app-seam! sid
                              {"op" "open"
                               "view"
                               {"title" "Ports"
                                "session_id" sid
                                "placement" "attachment"
                                "nodes"
                                [{"id" "send" "type" "button" "label" "Send" "action" "submit"}]}})
                   "view_id")

              patch!
              (fn [ops & [validators]]
                (app-seam! sid
                           (cond-> {"op" "patch" "view_id" view-id "patch" {"ops" ops}}
                             validators
                             (assoc "validators" validators))))]

          (try (patch! [{"op" "add-node"
                         "node_spec" {"id" "port" "type" "plaintext" "label" "Port"}}]
                       ["port" "typo"])
               (expect (= #{"port"} (get @(var-get #'hi/validator-ids) view-id))
                       "an id that names no node is dropped")
               (let [_
                     (judge-next! sid view-id {"port" "must be digits"})

                     refused
                     (hi/action! view-id
                                 {:action :submit
                                  :node-id "send"
                                  :values {"port" "abc"}
                                  :idempotency-key "key-0101"})]

                 (expect (= {"port" "must be digits"} (:errors refused))))
               (patch! [{"op" "remove-node" "node_id" "port"}])
               (expect (nil? (get @(var-get #'hi/validator-ids) view-id)))
               (expect (true? (:is-accepted (hi/action! view-id
                                                        {:action :submit
                                                         :node-id "send"
                                                         :values {}
                                                         :idempotency-key "key-0102"}))))
               (finally (hi/close-live! view-id)))))))
  (it "refuses validators that name no node of the view"
      (expect (str/includes? (live-refusal #(app-seam! (str (random-uuid))
                                                       {"op" "open"
                                                        "validators" ["nope"]
                                                        "view" {"title" "Ports"
                                                                "placement" "attachment"
                                                                "nodes" [{"id" "port"
                                                                          "type" "plaintext"
                                                                          "label" "Port"}]}}))
                             "nope"))))

(defn- sdk-door!
  "Cross one envelope through the gateway app door of SDK owner `owner`."
  [sid owner envelope]
  (json/read-json (hi/owner-app-json! sid owner (json/write-json-str envelope)) :key-fn identity))

(defn- keyed-app
  [sid]
  {"op" "open"
   "view" {"title" "Board"
           "session_id" sid
           "placement" "attachment"
           "key" "board"
           "nodes" [{"id" "go" "type" "button" "label" "Go"}
                    {"id" "note" "type" "plaintext" "label" "Note"}]}})

(defn- restart-app!
  "Forget open app `view-id` as a gateway restart does: only its record stays."
  [view-id]
  (swap! (var-get #'hi/pending) dissoc view-id))

(defn- event-types
  [sid view-id after]
  (mapv #(get % "type")
        (get (sdk-door! sid "demo" {"op" "events" "view_id" view-id "after" after "timeout_ms" 0})
             "events")))

(defdescribe
  app-restart-test
  ;; Live apps D6 and D8: a restored app waits for its owner, and an owner that
  ;; opens it again reads only the events that come after its open.
  (it "starts an adopting owner at the current events cursor"
      (recorded
        (fn []
          (let [sid
                (str (random-uuid))

                opened
                (sdk-door! sid "demo" (keyed-app sid))

                view-id
                (get opened "view_id")]

            (try (expect (= 0 (get opened "events_cursor")))
                 (dotimes [_ 3]
                   (hi/action! view-id {:action :activate :node-id "go"}))
                 (let [adopted
                       (sdk-door! sid "demo" (keyed-app sid))

                       cursor
                       (get adopted "events_cursor")]

                   (expect (= view-id (get adopted "view_id")))
                   (expect (= 3 cursor))
                   (expect (= [] (event-types sid view-id cursor)) "no old event comes again")
                   (hi/action! view-id {:action :activate :node-id "go"})
                   (expect (= ["activate"] (event-types sid view-id cursor))))
                 (finally (hi/close-live! view-id)))))))
  (it
    "judges an input that names itself with name alone"
    (recorded
      (fn []
        (let [sid
              (str (random-uuid))

              view-id
              (get (sdk-door! sid
                              "demo"
                              {"op" "open"
                               "validators" ["port"]
                               "view"
                               {"title" "Ports"
                                "session_id" sid
                                "placement" "attachment"
                                "nodes"
                                [{"name" "port" "type" "plaintext" "label" "Port"}
                                 {"id" "send" "type" "button" "label" "Send" "action" "submit"}]}})
                   "view_id")]

          (try (expect (= #{"port"} (get @(var-get #'hi/validator-ids) view-id)))
               (let [_
                     (future
                       (loop [tries 0]
                         (let [question (->> (get (sdk-door! sid
                                                             "demo"
                                                             {"op" "events"
                                                              "view_id" view-id
                                                              "after" 0
                                                              "timeout_ms" 1000})
                                                  "events")
                                             (filter #(= "validate" (get % "type")))
                                             first)]
                           (cond question (sdk-door! sid
                                                     "demo"
                                                     {"op" "validated"
                                                      "view_id" view-id
                                                      "request_id" (get question "request_id")
                                                      "errors" {"port" "must be digits"}})
                                 (< tries 5) (recur (inc tries))))))

                     refused
                     (hi/action! view-id
                                 {:action :submit
                                  :node-id "send"
                                  :values {"port" "abc"}
                                  :idempotency-key "key-0201"})]

                 (expect (= {"port" "must be digits"} (:errors refused))))
               (finally (hi/close-live! view-id)))))))
  (it
    "refuses presses as disconnected until the owner opens the app again"
    (recorded
      (fn []
        (let [sid
              (str (random-uuid))

              view-id
              (get (sdk-door! sid "demo" (keyed-app sid)) "view_id")]

          (try
            (hi/action! view-id {:action :activate :node-id "go"})
            (restart-app! view-id)
            (expect (true? (:is-stale (hi/app-view sid view-id))))
            (expect (= [true] (mapv :is-stale (hi/session-apps sid))))
            (doseq [action
                    [{:action :activate :node-id "go"} {:action :select :node-id "go" :item-ids []}
                     {:action :change :node-id "note" :value "x"} {:action :shortcut :key "r"}]]
              (expect (= "disconnected" (:reason (hi/action! view-id action))) (str action)))
            (expect (= 1 (get-in (hi/app-view sid view-id) [:nodes 0 :clicks]))
                    "a refused press changes nothing")
            (expect
              (= {"is_stale" true "events" []}
                 (select-keys
                   (sdk-door! sid "demo" {"op" "events" "view_id" view-id "after" 7 "timeout_ms" 0})
                   ["is_stale" "events"]))
              "the door learns that it must open the app again")
            (expect (true? (:is-accepted (hi/action! view-id
                                                     {:action :submit
                                                      :values {"note" "ship it"}
                                                      :idempotency-key "send-0001"})))
                    "a Send waits for the owner")
            (let [adopted
                  (sdk-door! sid "demo" (keyed-app sid))

                  cursor
                  (get adopted "events_cursor")]

              (expect (nil? (get-in adopted ["view" "is_stale"])))
              (expect (nil? (:is-stale (hi/app-view sid view-id))))
              (expect (= ["submit"] (event-types sid view-id cursor))
                      "only the waiting Send comes, no press from before the restart")
              (expect (true? (:is-accepted (hi/action! view-id
                                                       {:action :activate :node-id "go"})))))
            (finally (hi/close-live! view-id))))))))
