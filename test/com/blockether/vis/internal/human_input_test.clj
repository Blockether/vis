(ns com.blockether.vis.internal.human-input-test
  (:require [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis.internal.channel-events :as ce]
            [com.blockether.vis.internal.human-input :as hi]
            [lazytest.core :refer [defdescribe expect it throws?]]))

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
    (let [fut (future (hi/request! (assoc request :channel-ids [chan])))]
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
  (it "accepts the legacy :id and :help spellings"
      (let [[field] (normalized-fields {:id "note" :help "Free text"})]
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
  (it "clamps and validates the timeout"
      (expect (= hi/max-timeout-ms
                 (:timeout-ms (hi/normalize-request {:title "t"
                                                     :fields [{:id "a"}]
                                                     :timeout-ms (* 10 hi/max-timeout-ms)}))))
      (expect (throws? clojure.lang.ExceptionInfo
                       #(hi/normalize-request {:title "t" :fields [{:id "a"}] :timeout-ms 0}))))
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
  (it "still takes both legacy spellings"
      (let [[field] (normalized-fields {"id" "env" "help" "legacy prose"})]
        (expect (= "env" (:name field)))
        (expect (= "legacy prose" (:description field))))))

(defdescribe
  python-json-seam-test
  (it
    "takes snake_case strings in and hands snake_case strings back"
    (let
      [title
       (str "Deploy " (random-uuid))

       answer-json
       (future (hi/request-json! (json/write-json-str {"title" title
                                                       "description" "Pick a target"
                                                       ;; Channel routing is host-side — a guest cannot aim the
                                                       ;; dialog anywhere, so this key is dropped, not honoured.
                                                       "channel_ids" ["nowhere"]
                                                       "timeout_ms" 20000
                                                       "fields" [{"name" "env"
                                                                  "label" "Target"
                                                                  "description" "Where it lands."
                                                                  "type" "select"
                                                                  "options" ["staging" "prod"]
                                                                  "is_required" true}
                                                                 {"name" "note"
                                                                  "type" "plaintext"}]})))

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
        (expect (= {"env" "prod" "note" "hi"} (get answer "values"))))))
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

                   (expect (hi/secret-handle? handle))
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
                 (expect (not (hi/secret-handle? "hunter2")))
                 (expect (not (hi/secret-handle? nil)))
                 (expect (not (hi/secret-handle? 7)))))

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
      (expect (throws? clojure.lang.ExceptionInfo #(hi/request! {:title "t" :fields []})))))

(defdescribe
  non-cancellable-request-test
  (it "refuses every operator cancel and still yields to shutdown"
      ;; `:is-cancellable false` has to be enforced where BOTH surfaces meet —
      ;; the engine — or the TUI and the app each have to remember it, and the
      ;; one that forgets dismisses a request the extension declared mandatory.
      (let
        [rid
         (str "must-answer-" (random-uuid))

         answer
         (future (hi/request! {:id rid
                               :title "Deploy"
                               :is-cancellable false
                               :timeout-ms 3000
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
             (finally (hi/cancel-all! "cleanup") (future-cancel answer))))))
