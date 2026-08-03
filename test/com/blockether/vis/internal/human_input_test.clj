(ns com.blockether.vis.internal.human-input-test
  (:require [clojure.string :as str]
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
        (expect (= :plaintext (:type field)))
        (expect (= "name" (:label field)))
        (expect (false? (:is-required field)))
        (expect (false? (:is-secret field)))))
  (it "reads string keys from the Python boundary"
      (let
        [[field] (normalized-fields {"id" "token"
                                     "type" "password"
                                     "label" "API token"
                                     "is_required" true
                                     "max_length" 40
                                     "help" "from the dashboard"})]
        (expect (= "token" (:id field)))
        (expect (= :password (:type field)))
        (expect (= "API token" (:label field)))
        (expect (true? (:is-required field)))
        (expect (true? (:is-secret field)))
        (expect (= 40 (:max-length field)))
        (expect (= "from the dashboard" (:help field)))))
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
  (it "rejects duplicate field ids"
      (expect (throws? clojure.lang.ExceptionInfo
                       #(hi/normalize-request {:title "t" :fields [{:id "a"} {:id "a"}]}))))
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
