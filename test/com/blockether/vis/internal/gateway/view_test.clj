(ns com.blockether.vis.internal.gateway.view-test
  "An extension blocked on a typed human-input request must reach the COMPANION
   APP, not only the TUI.

   These tests pin the whole app path: the request defaults to both surfaces,
   it names its session, the gateway bridge turns it into `view.open`
   / `view.close` session events, the REST endpoints the phone actually
   calls answer it, the push tap alerts a phone that a run is parked, and the
   JSON fixture the companion's own suite parses is the engine's own projection.

   The matching TUI half — one request driving the terminal dialog and this
   bridge at the same time — is
   `com.blockether.vis.ext.channel-tui.view-cross-channel-test`."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.activity :as activity]
            [com.blockether.vis.internal.gateway.view :as gw-hi]
            [com.blockether.vis.internal.gateway.push :as push]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [com.blockether.vis.internal.view :as hi]
            [com.blockether.vis.internal.view.materializer :as live]
            [com.blockether.vis.internal.view.spec :as hi-spec]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]))

(defn- spec
  [& {:as overrides}]
  (merge
    {:title "Deploy?" :fields [{:id "confirm" :type "checkbox" :label "Confirm"}] :timeout-ms 4000}
    overrides))

(defn- await-true
  "Poll `pred` for up to a second. Requests settle on another thread."
  [pred]
  (loop [attempts 200]
    (cond (pred) true
          (zero? attempts) false
          :else (do (Thread/sleep 5) (recur (dec attempts))))))

(defn- with-events
  "Call `f` with an atom collecting `[sid event]` for every session event
   appended while it runs."
  [f]
  (let [seen
        (atom [])

        k
        (keyword "human-input-test" (str (System/nanoTime)))]

    (state/add-event-tap! k
                          (fn [sid event]
                            (swap! seen conj [(str sid) event])))
    (try (f seen) (finally (state/remove-event-tap! k)))))

(defn- events-of
  "Every collected event of `type` naming `view-id`. Scoped by View id so a
   sibling test's traffic can never satisfy an assertion here."
  [seen type view-id]
  (filterv (fn [[_ event]]
             (and (= type (get event "type"))
                  (= view-id (or (get-in event ["view" "id"]) (get event "view_id")))))
    @seen))

(deftest human-input-request-shape-test
  (testing "a request reaches BOTH surfaces unless the caller narrows it"
    (is (= [:tui :app] (:channel-ids (hi/normalize-request (spec)))))
    (is (= [:tui] (:channel-ids (hi/normalize-request (spec :channel-ids [:tui])))))
    (is (= [:app] (:channel-ids (hi/normalize-request (spec :channel-id :app))))))
  (testing "the request names its session, from either key spelling"
    (is (= "sid-1" (:session-id (hi/normalize-request (spec :session-id "sid-1")))))
    (is (= "sid-2" (:session-id (hi/normalize-request (assoc (spec) "session_id" "sid-2")))))
    (is (nil? (:session-id (hi/normalize-request (spec))))))
  (testing "the channel/wire view keeps the session — the app routes on it"
    (is (= "sid-3"
           (:session-id (hi/request->view (hi/normalize-request (spec :session-id "sid-3"))))))))

(deftest app-sees-and-answers-a-blocked-run-test
  (gw-hi/install!)
  (let [sid
        (str (random-uuid))

        rid
        (str "req-" (random-uuid))]

    (with-events
      (fn [seen]
        (let [answer (future (hi/request! (spec :id rid :session-id sid)))]
          (try (testing "the pause becomes a session event, so SSE + replay carry it"
                 (is (await-true #(seq (events-of seen "view.open" rid))))
                 (let [[event-sid event] (first (events-of seen "view.open" rid))]
                   (is (= sid event-sid))
                   (is (= "input" (get event "kind")))
                   (is (= "Deploy?" (get-in event ["view" "title"])))
                   (is (= sid (get-in event ["view" "session_id"])))
                   (is (= ["confirm"] (mapv #(get % "id") (get-in event ["view" "fields"]))))))
               (testing "a client that connects later still finds the open form"
                 (let [view (first (filterv #(= rid (:id %)) (gw-hi/input-views sid)))]
                   (is (some? view))
                   (is (= "Deploy?" (:title view)))))
               (testing "an answer is scoped to the session that owns the request"
                 (is (some? (gw-hi/input-view-of sid rid)))
                 (is (nil? (gw-hi/input-view-of (str (random-uuid)) rid))))
               (testing "the app's answer releases the blocked extension"
                 (is (true? (:is-accepted
                              (gw-hi/action! rid {:action :submit :values {"confirm" true}}))))
                 (let [result (deref answer 2000 ::timeout)]
                   (is (true? (:is-submitted result)))
                   (is (= true (get-in result [:values "confirm"])))))
               (testing "the close event tells every OTHER client to drop the form"
                 (is (await-true #(seq (events-of seen "view.close" rid))))
                 (let [[event-sid event] (first (events-of seen "view.close" rid))]
                   (is (= sid event-sid))
                   (is (= "input" (get event "kind")))
                   (is (= "submitted" (get-in event ["result" "reason"])))))
               (finally (hi/cancel! rid "cleanup"))))))))

(deftest rejected-answer-keeps-the-request-open-test
  (gw-hi/install!)
  (let [sid
        (str (random-uuid))

        rid
        (str "req-" (random-uuid))

        answer
        (future (hi/request! {:title "Key?"
                              :id rid
                              :session-id sid
                              :timeout-ms 4000
                              :fields [{:id "key" :type "plaintext" :is-required true}]}))]

    (try (is (await-true #(some? (gw-hi/input-view-of sid rid))))
         (testing "validation is the engine's, so app and TUI accept the same answers"
           (let [outcome (gw-hi/action! rid {:action :submit :values {"key" "   "}})]
             (is (false? (:is-accepted outcome)))
             (is (contains? (:errors outcome) "key")))
           (is (some? (gw-hi/input-view-of sid rid))))
         (testing "cancelling releases the waiter"
           (is (true? (:is-accepted (gw-hi/action! rid {:action :cancel}))))
           (is (false? (:is-submitted (deref answer 2000 ::timeout))))
           (is (nil? (gw-hi/input-view-of sid rid))))
         (finally (hi/cancel! rid "cleanup")))))

(deftest sessionless-request-is-refused-test
  ;; Regression, issue #104: a request that named no session was dropped here in
  ;; silence — the companion app never learned the run was parked and nothing in
  ;; the logs said a request had been thrown away. Issue #113: it is refused at
  ;; the source now, so no caller ever parks on a dialog only this process could
  ;; answer.
  (gw-hi/install!)
  (let [rid (str "req-" (random-uuid))]
    (with-events
      (fn [seen]
        (let [ex (try (hi/request! (spec :id rid)) nil (catch clojure.lang.ExceptionInfo e e))]
          (testing "the engine refuses it before anything blocks"
            (is (= :vis/view-invalid-request (:type (ex-data ex))))
            (is (nil? (hi/pending-request rid)))
            (is (empty? (events-of seen "view.open" rid)))))))))

(deftest push-alerts-a-parked-run-test
  ;; The describer stays installed for every case below: a session title is
  ;; minted from whatever opened the session, so it must never reach the alert.
  (let [prev @@#'push/describe-session]
    (try (push/set-session-describer! (fn [_sid _tid]
                                        {:title "Ship the parser"}))
         (testing "the title demands action and then asks the question; the body is the detail"
           (let [n (#'push/input-view-notification
                    "sid-9"
                    {"type" "view.open"
                     "kind" "input"
                     "view" {"id" "req-1"
                             "title" "Approve the deploy"
                             "description" "v1.2.3 to production"}})]
             (is (= "Action needed — Approve the deploy" (:title n)))
             (is (= "v1.2.3 to production" (:body n)))
             (is (= "sid-9" (:thread-id n)))
             (is (= "sid-9:input-view" (:collapse-id n)))
             (is (= "view.open" (get-in n [:data :type])))
             (is (= "req-1" (get-in n [:data :view_id])))
             (is (= "sid-9" (get-in n [:data :session_id])))))
         (testing "a question with no detail under it is never repeated in the body"
           (let [n (#'push/input-view-notification
                    "sid-9"
                    {"type" "view.open"
                     "kind" "input"
                     "view" {"id" "req-2" "title" "Approve the deploy"}})]
             (is (= "Action needed — Approve the deploy" (:title n)))
             (is (= "Vis is waiting on your answer." (:body n)))))
         (testing "a request carrying only a description still says what it wants"
           (let [n (#'push/input-view-notification
                    "sid-9"
                    {"type" "view.open"
                     "kind" "input"
                     "view" {"id" "req-3" "description" "Approve the deploy"}})]
             (is (= "Action needed" (:title n)))
             (is (= "Approve the deploy" (:body n)))))
         (testing "an unlabelled request is still a demand, never blank"
           (let [n (#'push/input-view-notification
                    "sid-9"
                    {"type" "view.open" "kind" "input" "view" {"id" "req-4"}})]
             (is (= "Action needed" (:title n)))
             (is (= "Vis is waiting on your answer." (:body n)))))
         (finally (push/set-session-describer! prev)))))

;; The endpoints the phone actually calls
;;
;; `gw-hi/action!` is the in-process seam; the app only ever sees the shared HTTP
;; handler and its JSON. These drive the real ring handler so a routing, path-param
;; or encoding slip cannot hide behind a green in-process test.

(defn- rv
  "Resolve a (private) handler var in the gateway server namespace."
  [sym]
  (requiring-resolve (symbol "com.blockether.vis.internal.gateway.server" (name sym))))

(defn- body-stream
  [m]
  (java.io.ByteArrayInputStream. (.getBytes ^String (wire/json-str m) "UTF-8")))

(defn- json-body [response] (wire/parse-json (:body response)))

(defn- view-action-response
  "Apply one action through the exact shared HTTP handler the Companion calls."
  [sid view-id action]
  ((rv 'view-action-handler) {:path-params {:sid sid :view-id view-id} :body (body-stream action)}))

(deftest the-app-answers-a-parked-run-over-http-test
  (gw-hi/install!)
  (let [sid
        (str (random-uuid))

        rid
        (str "req-" (random-uuid))

        answer
        (future (hi/request!
                  (spec :id rid
                        :session-id sid
                        :fields [{:id "note" :type "plaintext" :label "Note" :is-required true}])))]

    (try (is (await-true #(some? (gw-hi/input-view-of sid rid))))
         (testing "a phone that starts cold still finds the open form, snake_case"
           (let [response
                 ((rv 'list-input-views-handler) {:path-params {:sid sid}})

                 request
                 (first (get (json-body response) "requests"))]

             (is (= 200 (:status response)))
             (is (= "application/json" (get-in response [:headers "Content-Type"])))
             (is (= rid (get request "id")))
             (is (= sid (get request "session_id")))
             (is (= "Deploy?" (get request "title")))
             (is (= ["note"] (mapv #(get % "id") (get request "fields"))))
             (is (true? (get-in request ["fields" 0 "is_required"])))))
         (testing "the engine's validation answers the app, and the run stays parked"
           (let [body (json-body
                        (view-action-response sid rid {:action "submit" :values {"note" "   "}}))]
             (is (false? (get body "is_accepted")))
             (is (= "submit" (get body "action")))
             (is (= rid (get body "view_id")))
             (is (contains? (get body "errors") "note"))
             (is (some? (gw-hi/input-view-of sid rid)))))
         (testing "another session may not answer this View"
           (is (= 404
                  (:status (view-action-response (str (random-uuid))
                                                 rid
                                                 {:action "submit" :values {"note" "ship"}})))))
         (testing "an accepted answer releases the blocked extension"
           (let [body
                 (json-body
                   (view-action-response sid rid {:action "submit" :values {"note" "ship it"}}))]
             (is (true? (get body "is_accepted")))
             (is (= "submit" (get body "action")))
             (is (= "ship it" (get-in (deref answer 2000 ::timeout) [:values "note"])))))
         (testing "a settled request is gone from the snapshot and answerable no more"
           (is (empty? (get (json-body ((rv 'list-input-views-handler) {:path-params {:sid sid}}))
                            "requests")))
           (is (= 404 (:status (view-action-response sid rid {:action "cancel"})))))
         (finally (hi/cancel! rid "cleanup")))))

(deftest the-app-cancels-a-parked-run-over-http-test
  (gw-hi/install!)
  (let [sid
        (str (random-uuid))

        rid
        (str "req-" (random-uuid))

        answer
        (future (hi/request! (spec :id rid :session-id sid)))]

    (try (is (await-true #(some? (gw-hi/input-view-of sid rid))))
         (let [body (json-body (view-action-response sid rid {:action "cancel"}))]
           (is (true? (get body "is_accepted")))
           (is (= "cancel" (get body "action")))
           (is (= rid (get body "view_id")))
           (is (false? (:is-submitted (deref answer 2000 ::timeout))))
           (is (empty? (gw-hi/input-views sid))))
         (finally (hi/cancel! rid "cleanup")))))

;; Cross-language contract
;;
;; The companion's own unit tests parse `human-input.fixture.json`. That file is
;; the engine's projection, byte for byte — so a change to `request->view` that
;; the app cannot read fails HERE, in Clojure, instead of silently shipping a
;; dialog the phone renders empty.

(def ^:private fixture-spec
  "The request whose wire projection the companion app's fixture holds."
  {:id "req-1"
   :session-id "sid-1"
   :title "Deploy?"
   :description "prod"
   :timeout-ms 300000
   ;; Two DECORATIONS lead the form: they answer nothing, so the app must render
   ;; them and keep them out of its values map.
   :fields
   [{:type "heading" :text "Target"} {:type "paragraph" :text "Staging pages nobody."}
    {:name "env"
     :type "select"
     :label "Env"
     :description "Where this deploy lands"
     :default "prod"
     :options [{:value "prod"} {:value "stg" :label "Staging"}]}
    {:name "key" :type "password" :is-required true :max-length 40}
    {:id "ok" :type "checkbox" :label "Confirm" :default true}
    {:id "tags" :type "multiselect" :options ["a" "b"] :default []}
    {:id "risk"
     :type "range"
     :label "Risk budget"
     :description "How much of the error budget this may spend"
     :min 0
     :max 10
     :step 0.5
     :default 2.5}
    {:id "code"
     :type "otp"
     :label "One-time code"
     :description "From the authenticator on your phone"
     :is-required true
     :min-length 4
     :max-length 6}
    {:id "notify"
     :type "plaintext"
     :label "Notify"
     :validate (fn [value]
                 (when (> (count value) 60) "keep it short"))}
    {:id "notes" :type "multiline" :label "Notes" :placeholder "Anything the on-call should know"}
    {:type "group"
     :direction "row"
     :label "Server"
     :description "Where the pool dials out"
     :fields [{:name "host" :label "Host" :is-required true}
              {:type "group"
               :direction "column"
               :fields [{:name "port"
                         :label "Port"
                         :validate (fn [value]
                                     (when-not (re-matches #"\d+" value) "digits only"))}
                        {:name "tls" :type "checkbox" :label "TLS"}]}]}]})

(defn- fixture-file
  "`apps/vis-companion/src/lib/human-input.fixture.json`, found from the working
   directory upwards so the test runs from the repo root or a sub-project."
  []
  (loop [dir (.getCanonicalFile (io/file (System/getProperty "user.dir")))]
    (when dir
      (let [f (io/file dir "apps/vis-companion/src/lib/human-input.fixture.json")]
        (if (.isFile f) f (recur (.getParentFile dir)))))))

(defn- node-types
  "Every `type` in a request view's field tree, groups and decorations included."
  [fields]
  (into #{}
        (mapcat (fn [{:keys [type fields]}]
                  (cons type (node-types fields))))
        fields))

(deftest the-app-fixture-is-the-engines-own-projection-test
  (let [view (hi/request->view (hi/normalize-request fixture-spec))]
    (testing "the companion parses engine bytes, not a hand-written lookalike"
      (let [file (fixture-file)]
        (is (some? file))
        (when file (is (= (wire/parse-json (slurp file)) (wire/parse-json (wire/json-str view)))))))
    ;; The app's own suite renders this fixture and asserts a control for every
    ;; node in it. That proof is only worth the vocabulary it covers, so the
    ;; fixture holds ONE OF EVERY KIND the engine can send: a type added to the
    ;; spec and left out of the fixture would otherwise ship an app that paints
    ;; a hole in a dialog which has already stopped somebody's run.
    (testing "and holds one node of every kind the engine can send"
      (is (= (conj (into (set (vals hi-spec/field-types)) (vals hi-spec/decor-types))
                   hi-spec/group-type)
             (node-types (:fields view)))))))

(deftest the-companion-urls-route-to-the-shared-view-action-handler-test
  (testing "the URLs `gateway.ts` builds are the URLs this router serves"
    (let [match-by-path
          (requiring-resolve 'reitit.core/match-by-path)

          router
          ((rv 'router) "token" [])

          sid
          (str (random-uuid))

          rid
          "req 1"

          ;; `encodeURIComponent`, exactly as the companion client escapes an id.
          encoded
          "req%201"

          match
          (fn [path]
            (match-by-path router path))]

      (is (= @(rv 'list-input-views-handler)
             (get-in (match (str "/v1/sessions/" sid "/views/input")) [:data :get :handler])))
      (let [m (match (str "/v1/sessions/" sid "/views/" encoded "/actions"))]
        (is (= @(rv 'view-action-handler) (get-in m [:data :post :handler])))
        (testing "and hand the shared handler the View id it acts on"
          (is (= sid (str (get-in m [:path-params :sid]))))
          (is (= rid (get-in m [:path-params :view-id]))))))))

(deftest a-hostile-body-cannot-park-or-settle-a-run-test
  (gw-hi/install!)
  (let [sid
        (str (random-uuid))

        ;; An extension may name its own request, including characters the app has
        ;; to `encodeURIComponent` before it can even build the URL.
        rid
        "req/one two"

        answer
        (future (hi/request! (spec :id rid
                                   :session-id sid
                                   :fields [{:id "note" :type "plaintext" :label "Note"}])))]

    (try (is (await-true #(some? (gw-hi/input-view-of sid rid))))
         (testing "a malformed body is a 400 — never a 500, never a settled run"
           (doseq [body [{:action "submit" :values "text"} {:action "submit" :values [1 2]}
                         {:action "submit" :values 42} {:action "submit" :values nil}
                         {:values {"note" "missing action"}} {:action "unknown"} nil]]
             (is (= 400 (:status (view-action-response sid rid body)))))
           (is (= 400 (:status ((rv 'view-action-handler) {:path-params {:sid sid :view-id rid}}))))
           (is (= 400
                  (:status ((rv 'view-action-handler)
                             {:path-params {:sid sid :view-id rid}
                              :body (java.io.ByteArrayInputStream. (.getBytes "not json"
                                                                              "UTF-8"))}))))
           (is (some? (gw-hi/input-view-of sid rid))))
         (testing "a structured value is rejected, not stringified into the answer"
           (let [body (json-body
                        (view-action-response sid rid {:action "submit" :values {"note" {"a" 1}}}))]
             (is (false? (get body "is_accepted")))
             (is (= "must be text" (get-in body ["errors" "note"])))
             (is (some? (gw-hi/input-view-of sid rid)))))
         (testing "the escaped id still routes, and the same handler answers it"
           (let [match ((requiring-resolve 'reitit.core/match-by-path)
                         ((rv 'router) "token" [])
                         (str "/v1/sessions/" sid "/views/req%2Fone%20two/actions"))]
             (is (= rid (get-in match [:path-params :view-id])))
             (is (= @(rv 'view-action-handler) (get-in match [:data :post :handler])))
             (is (true? (get (json-body ((rv 'view-action-handler)
                                          {:path-params (:path-params match)
                                           :body (body-stream {:action "submit"
                                                               :values {"note" "typed"}})}))
                             "is_accepted")))))
         (is (= {:is-submitted true :reason "submitted" :request-id rid :values {"note" "typed"}}
                (deref answer 2000 ::stuck)))
         (finally (hi/cancel! rid)))))

(deftest a-storm-of-answers-settles-a-parked-run-exactly-once-test
  (gw-hi/install!)
  (with-events
    (fn [seen]
      (let [sid
            (str (random-uuid))

            rid
            (str "storm-" (random-uuid))

            answer
            (future (hi/request! (spec :id rid
                                       :session-id sid
                                       :timeout-ms 10000
                                       :fields [{:id "note" :type "plaintext" :is-required true}])))

            _
            (is (await-true #(some? (gw-hi/input-view-of sid rid))))

            ;; Every surface fires at once: valid answers, blank ones, cancels.
            gate
            (java.util.concurrent.CountDownLatch. 1)

            racers
            (doall
              (concat
                (for [i (range 6)]
                  (future (.await gate)
                          [:submit
                           (gw-hi/action! rid {:action :submit :values {"note" (str "v" i)}})]))
                (for [_ (range 3)]
                  (future (.await gate)
                          [:blank (gw-hi/action! rid {:action :submit :values {"note" "   "}})]))
                (for [_ (range 3)]
                  (future (.await gate) [:cancel (gw-hi/action! rid {:action :cancel})]))))

            _
            (.countDown gate)

            results
            (mapv deref racers)

            winners
            (filterv (fn [[_ outcome]]
                       (true? (:is-accepted outcome)))
              results)

            final
            (deref answer 5000 ::stuck)]

        (testing "exactly one answer wins and the extension is released once"
          (is (= 1 (count winners)))
          (is (= rid (:request-id final)))
          (is (= (if (= :cancel (ffirst winners)) "cancelled" "submitted") (:reason final))))
        (testing "and every surface is told exactly once that the form is gone"
          (is (await-true #(= 1 (count (events-of seen "view.close" rid)))))
          (is (= 1 (count (events-of seen "view.open" rid))))
          (is (empty? (gw-hi/input-views sid)))
          (is (nil? (gw-hi/input-view-of sid rid)))
          (is (= {:action :submit :view-id rid :is-accepted false :reason "unknown"}
                 (gw-hi/action! rid {:action :submit :values {"note" "late"}})))
          (is (= 404
                 (:status
                   (view-action-response sid rid {:action "submit" :values {"note" "late"}})))))))))

(deftest a-request-that-refuses-cancellation-refuses-the-app-too-test
  (gw-hi/install!)
  (let [sid
        (str (random-uuid))

        rid
        (str "must-answer-" (random-uuid))

        answer
        (future (hi/request! (spec :id rid
                                   :session-id sid
                                   :is-cancellable false
                                   :fields [{:id "note" :type "plaintext" :is-required true}])))]

    (try (is (await-true #(some? (gw-hi/input-view-of sid rid))))
         (testing "the app is refused the escape hatch the TUI dialog also denies"
           (let [response (view-action-response sid rid {:action "cancel"})]
             (is (= 409 (:status response)))
             (is (= "view-action-refused" (get-in (json-body response) ["error" "type"])))
             (is (false? (:is-cancellable (gw-hi/input-view-of sid rid))))
             (is (some? (gw-hi/input-view-of sid rid)))))
         (testing "answering it is still the way out"
           (is (true? (get
                        (json-body
                          (view-action-response sid rid {:action "submit" :values {"note" "yes"}}))
                        "is_accepted")))
           (is (true? (:is-submitted (deref answer 2000 ::stuck)))))
         (finally (hi/cancel! rid)))))

;; -- A live view: the interaction the app WATCHES -----------------------------
;;
;; Nothing is parked here and nobody owes an answer, so none of this is a
;; question: what the app owes the operator is the PICTURE. These pin the three
;; events on the session stream, the resync a client reads after joining late,
;; the log page it scrolls back through, and the one button it has — stop.

(def ^:private live-views-dir
  "The private var every view record hangs under, redefined per test so nothing
   here writes anywhere near the developer's own `~/.vis`."
  (requiring-resolve 'com.blockether.vis.internal.view.sink/views-dir))

(defn- recorded
  "Run `f` with every view record under a temp directory of its own."
  [f]
  (with-redefs-fn {live-views-dir (constantly (io/file (System/getProperty "java.io.tmpdir")
                                                       (str "vis-views-" (random-uuid))))}
    f))

(defn- live-events-of
  "Every collected event of `type` naming `view-id`. Scoped by view id so a
   sibling test's traffic can never satisfy an assertion here."
  [seen type view-id]
  (filterv (fn [[_ event]]
             (and (= type (get event "type")) (= view-id (get event "view_id"))))
    @seen))

(def ^:private live-flush-ms
  "The bridge's own tick. Redefined per test through [[unhurried]], so what a view
   HOLDS stays held until something asks for it."
  (requiring-resolve 'com.blockether.vis.internal.gateway.view/live-flush-ms))

(defn- unhurried
  "Run `f` with the flush tick pushed out of reach. These pin WHO publishes a held
   patch; how long the window is, is [[gw-hi/live-flush-ms]]'s own business."
  [f]
  (with-redefs-fn {live-flush-ms (* 60 1000)} f))
(deftest the-app-watches-a-live-view-test
  (gw-hi/install!)
  (recorded
    (fn []
      (with-events
        (fn [seen]
          (let [sid
                (str (random-uuid))

                view
                (hi/open-live! {:title "CI"
                                :description "Blockether/vis · 42"
                                :session-id sid
                                :nodes [{:id "now" :type "status" :text "Polling…" :tone "running"}
                                        {:id "tail" :type "log"}]})

                view-id
                (:id view)]

            (try (testing "the open crosses as an ordinary session event, in snake_case"
                   (is (await-true #(seq (live-events-of seen gw-hi/view-open-event view-id))))
                   (let [[[event-sid event]] (live-events-of seen gw-hi/view-open-event view-id)]
                     (is (= sid event-sid))
                     (is (= "CI" (get-in event ["view" "title"])))
                     (is (= ["now" "tail"] (mapv #(get % "id") (get-in event ["view" "nodes"]))))))
                 (hi/patch-live! view-id [{:op "append" :node-id "tail" :lines ["one" "two"]}])
                 (hi/patch-live! view-id
                                 [{:op "append" :node-id "tail" :lines ["three"]}
                                  {:op "set" :node-id "now" :text "Building"}])
                 (testing "patches ride ONE coalesced frame that says which of them it carries"
                   (gw-hi/flush-live-patches!)
                   (is (await-true #(seq (live-events-of seen gw-hi/view-patch-event view-id))))
                   (let [frames
                         (live-events-of seen gw-hi/view-patch-event view-id)

                         [_ event]
                         (first frames)]

                     (is (= 1 (count frames)))
                     (is (= 1 (get event "first_seq")))
                     (is (= 2 (get-in event ["patch" "seq"])))
                     ;; Two appends on one node became one; the `set` on the OTHER node
                     ;; kept its place, because merging across nodes would reorder the run.
                     (is (= [["append" "tail"] ["set" "now"]]
                            (mapv (juxt #(get % "op") #(get % "node_id"))
                                  (get-in event ["patch" "ops"]))))
                     (is (= ["one" "two" "three"] (get-in event ["patch" "ops" 0 "lines"])))))
                 (finally (hi/close-live! view-id)))
            (testing "and the ending carries the picture the model reads, not a rendering of it"
              (is (await-true #(seq (live-events-of seen gw-hi/view-close-event view-id))))
              (let [[[_ event]] (live-events-of seen gw-hi/view-close-event view-id)]
                (is (true? (get-in event ["result" "is_completed"])))
                (is (= "completed" (get-in event ["result" "reason"])))
                (is (= ["now" "tail"]
                       (mapv #(get % "id") (get-in event ["result" "view" "nodes"]))))
                (is (nil? (get-in event ["result" "markdown"])))))))))))
;; Regression, session 3d6dc388-a21c-4005-b498-87c02668cb34: every Activity
;; replacement survived the gateway flush, duplicating the growing snapshot on SSE and
;; making the phone process and repaint obsolete intermediate pictures.
(deftest activity-flush-keeps-only-the-latest-picture-test
  (gw-hi/install!)
  (recorded
    (fn []
      (unhurried
        (fn []
          (with-events
            (fn [seen]
              (let [sid
                    (str (random-uuid))

                    state
                    (activity/empty-state
                      {:evaluation-id (str (random-uuid)) :iteration 1 :form-index 0})

                    view
                    (hi/open-activity! {:session-id sid :state state})

                    view-id
                    (:id view)]

                (try (hi/patch-activity! view-id state)
                     (hi/patch-activity! view-id state)
                     (gw-hi/flush-live-patches!)
                     (let [[[_ event]] (live-events-of seen gw-hi/view-patch-event view-id)]
                       (is (= 1 (count (live-events-of seen gw-hi/view-patch-event view-id))))
                       (is (= 1 (get event "first_seq")))
                       (is (= 2 (get-in event ["patch" "seq"])))
                       (is (= ["set-activity"]
                              (mapv #(get % "op") (get-in event ["patch" "ops"])))))
                     (finally (hi/close-live! view-id)))))))))))

;; The bridge holds a view's patches for one flush window, so a gateway that goes
;; away mid-stream would swallow whatever the window still had. `stop!` in
;; `com.blockether.vis.internal.gateway.server` is [[gw-hi/uninstall!]]'s one
;; caller, and this is what calling it buys.
(deftest a-gateway-going-away-publishes-what-it-still-holds-test
  (gw-hi/install!)
  (recorded
    (fn []
      (unhurried
        (fn []
          (with-events
            (fn [seen]
              (let [sid
                    (str (random-uuid))

                    view
                    (hi/open-live! {:title "CI" :session-id sid :nodes [{:id "tail" :type "log"}]})

                    view-id
                    (:id view)]

                (try (hi/patch-live! view-id [{:op "append" :node-id "tail" :lines ["one"]}])
                     (testing "a patch waits for the tick, and the tick is nowhere near due"
                       (is (empty? (live-events-of seen gw-hi/view-patch-event view-id))))
                     (testing "so the gateway leaving is what publishes it"
                       ;; Subscribe again at once: the bus is process-local, so this one
                       ;; listener is also serving every sibling test's view.
                       (gw-hi/uninstall!)
                       (gw-hi/install!)
                       (let [frames
                             (live-events-of seen gw-hi/view-patch-event view-id)

                             [[_ event]]
                             frames]

                         (is (= 1 (count frames)))
                         (is (= ["one"] (get-in event ["patch" "ops" 0 "lines"])))))
                     (finally (hi/close-live! view-id)))))))))))
(deftest the-app-reads-a-live-view-back-over-http-test
  (gw-hi/install!)
  (recorded
    (fn []
      (let [sid
            (str (random-uuid))

            view
            (hi/open-live! {:title "CI" :session-id sid :nodes [{:id "tail" :type "log"}]})

            view-id
            (:id view)]

        (try (hi/patch-live!
               view-id
               [{:op "append" :node-id "tail" :lines (mapv #(str "line " %) (range 1 21))}])
             (testing "a phone that starts cold reads the CURRENT picture, not a stream it missed"
               (let [response
                     ((rv 'list-live-views-handler) {:path-params {:sid sid}})

                     answered
                     (first (get (json-body response) "views"))]

                 (is (= 200 (:status response)))
                 (is (= "application/json" (get-in response [:headers "Content-Type"])))
                 (is (= view-id (get answered "id")))
                 (is (= sid (get answered "session_id")))
                 (is (= ["tail"] (mapv #(get % "id") (get answered "nodes"))))))
             (testing "and scrolls back through output whose patches it never received"
               (let [body (json-body ((rv 'live-view-log-handler)
                                       {:path-params {:sid sid :view-id view-id :node-id "tail"}
                                        :query-params {"from" "5" "limit" "3"}}))]
                 (is (= "tail" (get body "node_id")))
                 (is (= 5 (get body "from")))
                 (is (= 20 (get body "total")))
                 (is (= ["line 6" "line 7" "line 8"] (get body "lines")))))
             (testing "a view id belonging to another session is not stoppable from here"
               (is (= 404
                      (:status
                        (view-action-response (str (random-uuid)) view-id {:action "interrupt"})))))
             (testing "the app's stop action carries the words typed with it"
               (let [body (json-body (view-action-response sid
                                                           view-id
                                                           {:action "interrupt"
                                                            :note "wrong subnet"}))]
                 (is (true? (get body "is_accepted")))
                 (is (= "interrupt" (get body "action")))
                 (is (= view-id (get body "view_id")))
                 (is (nil? (gw-hi/live-view-of sid view-id)))
                 (is (empty? (gw-hi/live-views sid)))))
             (testing "a view that already ended answers 404 instead of pretending to stop again"
               (is (= 404 (:status (view-action-response sid view-id {:action "interrupt"})))))
             (testing "and its record still answers, which is what makes a finished log readable"
               (let [body (json-body ((rv 'live-view-log-handler)
                                       {:path-params {:sid sid :view-id view-id :node-id "tail"}}))]
                 (is (= 20 (get body "total")))
                 (is (= 20 (count (get body "lines"))))))
             (finally (hi/close-live! view-id)))))))

;; Regression, session a64d44c2-8228-455f-926e-b3381f19a93b: tapping a CI job
;; had no engine action, so the visible selection and the log could never follow the tap.
(deftest the-app-selects-a-live-table-row-over-http-test
  (gw-hi/install!)
  (let [sid
        (str (random-uuid))

        view
        (hi/open-live! {:title "CI"
                        :session-id sid
                        :nodes [{:id "jobs"
                                 :type "table"
                                 :is-selectable true
                                 :selected-ids ["a"]
                                 :columns [{:id "job" :label "Job"}]
                                 :rows [{:id "a" :cells ["A"]} {:id "b" :cells ["B"]}]}]})

        view-id
        (:id view)]

    (try
      (testing "the selected ids become ordinary durable live state"
        (let [response
              (view-action-response sid view-id {:action "select" :node_id "jobs" :item_ids ["b"]})

              body
              (json-body response)]

          (is (= 200 (:status response)))
          (is (true? (get body "is_accepted")))
          (is (= "select" (get body "action")))
          (is (= ["b"] (get body "item_ids")))
          (is (= ["b"] (get-in (gw-hi/live-view-of sid view-id) [:nodes 0 :selected-ids])))))
      (testing "a stale row id is refused without moving the selection"
        (let [response (view-action-response
                         sid
                         view-id
                         {:action "select" :node_id "jobs" :item_ids ["missing"]})]
          (is (= 400 (:status response)))
          (is (= ["b"] (get-in (gw-hi/live-view-of sid view-id) [:nodes 0 :selected-ids])))))
      (testing "another session cannot select in this view"
        (is (= 404
               (:status (view-action-response
                          (str (random-uuid))
                          view-id
                          {:action "select" :node_id "jobs" :item_ids ["a"]})))))
      (finally (hi/close-live! view-id)))))

(deftest a-live-view-is-always-stoppable-and-the-stop-carries-its-words-test
  (gw-hi/install!)
  (recorded
    (fn []
      (with-events
        (fn [seen]
          (let [sid
                (str (random-uuid))

                view
                (hi/open-live! {:title "Migration"
                                :session-id sid
                                :nodes
                                [{:id "now" :type "status" :text "Writing rows" :tone "running"}]})

                view-id
                (:id view)]

            (try (testing "no view refuses the stop: it asks nothing, so nothing is left unanswered"
                   (let [body (json-body (view-action-response
                                           sid
                                           view-id
                                           {:action "interrupt"
                                            :note "wrong subnet — I will re-run it"}))]
                     (is (true? (get body "is_accepted")))
                     (is (= "interrupt" (get body "action")))
                     (is (nil? (gw-hi/live-view-of sid view-id)))))
                 (testing "and the run reads WHO stopped it, and why, before it reads the picture"
                   (is (await-true #(seq (live-events-of seen gw-hi/view-close-event view-id))))
                   (let [[[_ event]] (live-events-of seen gw-hi/view-close-event view-id)]
                     (is (= "interrupted" (get-in event ["result" "reason"])))
                     (is (true? (get-in event ["result" "is_from_human"])))
                     (is (= "wrong subnet — I will re-run it" (get-in event ["result" "note"])))
                     (is (= ["now"]
                            (mapv #(get % "id") (get-in event ["result" "view" "nodes"]))))))
                 (finally (hi/close-live! view-id))))
          (let [sid
                (str (random-uuid))

                bare
                (:id (hi/open-live! {:title "Sweep"
                                     :session-id sid
                                     :nodes [{:id "now" :type "status" :text "Sweeping"}]}))]

            (try (testing "a stop with no note still says a person sent it"
                   (let [body (json-body (view-action-response sid bare {:action "interrupt"}))]
                     (is (true? (get body "is_accepted")))
                     (is (= "interrupt" (get body "action"))))
                   (is (await-true #(seq (live-events-of seen gw-hi/view-close-event bare))))
                   (let [[[_ event]] (live-events-of seen gw-hi/view-close-event bare)]
                     (is (true? (get-in event ["result" "is_from_human"])))
                     (is (nil? (get-in event ["result" "note"])))))
                 (finally (hi/close-live! bare)))))))))

(deftest view-actions-use-one-kind-independent-route-test
  (testing "a View kind is policy, not part of the action resource address"
    (let [match-by-path
          (requiring-resolve 'reitit.core/match-by-path)

          router
          ((rv 'router) "token" [])

          sid
          (str (random-uuid))

          view-id
          (str (random-uuid))

          match
          (fn [path]
            (match-by-path router path))

          action-handler
          (rv 'view-action-handler)

          action-match
          (match (str "/v1/sessions/" sid "/views/" view-id "/actions"))]

      (is (some? action-handler))
      (is (some? action-match))
      (is (= (some-> action-handler
                     deref)
             (get-in action-match [:data :post :handler])))
      (testing "the obsolete kind/action-specific endpoints are gone"
        (is (nil? (match (str "/v1/sessions/" sid "/views/input/" view-id "/actions/submit"))))
        (is (nil? (match (str "/v1/sessions/" sid "/views/live/" view-id "/actions/focus"))))
        (is (nil? (match
                    (str "/v1/sessions/" sid "/views/live/" view-id "/actions/interrupt"))))))))

;; The companion's own unit tests parse `live-view.fixture.json`, exactly as they
;; parse the form fixture above. That file is not written by hand either: it is
;; what the engine actually projects onto the wire for a view holding ONE OF EVERY
;; node kind, so a node type added to the spec and left out of the app ships as a
;; failure here rather than as a hole in somebody's running build.

(def ^:private live-fixture-spec
  "The live view whose wire projection the companion app's fixture holds."
  {:title "Fleet scan"
   :description "3 hosts · started 12:04"
   :session-id "5f0f4d0e-2f6f-4c8e-9a0e-2f9a1c0b7d31"
   :source "vis-fleet"
   :nodes
   [{:id "now" :type "status" :text "Scanning db-2" :detail "host 2 of 3" :tone "running"}
    {:id "swept" :type "progress" :label "Swept" :done 2 :total 3}
    {:id "score"
     :type "stat"
     :label "Findings"
     :stats [{:id "critical" :label "Critical" :value-text "1" :tone "error"}
             {:id "warnings" :label "Warnings" :value-text "4" :tone "warn"}]}
    {:id "phases"
     :type "steps"
     :label "Phases"
     :steps [{:id "collect" :label "Collect inventory" :tone "ok" :detail "3 hosts"}
             {:id "scan" :label "Scan packages" :tone "running"}
             {:id "report" :label "Write report" :tone "idle"}]}
    {:id "tail"
     :type "log"
     :label "Output"
     :lines ["db-1 · 0 critical" "db-2 · 1 critical (openssl)"]}
    ;; The one ROW in the fixture: the table and the paragraph that explains it
    ;; stand together, so a surface's layout is the ENGINE's projection and not
    ;; the app's guess — and the sentence carries the inline marks a human
    ;; string may hold.
    {:id "reading"
     :type "group"
     :direction "row"
     :fields
     [{:id "hosts"
       :type "table"
       :label "Hosts"
       :columns [{:id "host" :label "Host"} {:id "state" :label "State"}
                 {:id "findings" :label "Findings" :align "right"}]
       :rows [{:id "db-1" :cells ["db-1" "clean" "0"] :tone "ok"}
              {:id "db-2" :cells ["db-2" "critical" "1"] :tone "error"}]}
      {:id "why"
       :type "status"
       :label "Why"
       :tone "warn"
       :text
       "`db-2` needs **openssl 3.0.13**: its `libssl` is two releases behind the rest of the fleet, so the scan stopped short of writing the report."}]}
    {:id "links"
     :type "link"
     :label "Elsewhere"
     :links [{:id "run" :label "The run on GitHub" :target "https://example.com/run/42"}
             {:id "report" :label "report.md" :target-kind "path" :target "/tmp/report.md"}]}]})

(defn- companion-fixture-file
  "A Companion engine fixture, found from the repository root or a sub-project."
  [filename]
  (loop [dir (.getCanonicalFile (io/file (System/getProperty "user.dir")))]
    (when dir
      (let [f (io/file dir "apps/vis-companion/src/lib" filename)]
        (if (.isFile f) f (recur (.getParentFile dir)))))))

(defn- live-fixture-file [] (companion-fixture-file "live-view.fixture.json"))

(defn- without-mint
  "The view without the two values every view mints for ITSELF."
  [m]
  (dissoc m "id" "created_at"))

(deftest the-app-live-fixture-is-the-engines-own-projection-test
  (let [view
        (live/materialize (hi/normalize-live-view live-fixture-spec))

        file
        (live-fixture-file)

        fixture
        (some-> file
                slurp
                wire/parse-json)]

    (is (some? file))
    (when fixture
      (testing "the companion parses engine bytes, not a hand-written lookalike"
        (is (= (without-mint (wire/parse-json (wire/json-str view))) (without-mint fixture))))
      (testing "the two minted values are still there, because the app keys on them"
        (is (some? (parse-uuid (get fixture "id"))))
        (is (pos-int? (get fixture "created_at"))))
      (testing "and it holds one node of every kind the engine can send, wherever it stands"
        (is (= (conj (set (keys hi-spec/live-node-types)) hi-spec/group-type-name)
               (set (map #(get % "type")
                         (mapcat #(tree-seq map?
                                            (fn [node]
                                              (get node "fields"))
                                            %)
                                 (get fixture "nodes"))))))))))

(deftest the-app-activity-fixture-is-the-host-projection-test
  (let [state
        {:schema-version 1
         :anchor {:evaluation-id "fixture-evaluation" :iteration 0 :form-index 0}
         :state :running
         :counts {:running 1 :succeeded 1 :failed 0 :cancelled 0}
         :rows [{:id "call-1"
                 :sequence 1
                 :operation :grep
                 :presenter :observation
                 :classification :observation
                 :state :succeeded
                 :summary "18 matches"
                 :duration-ms 41
                 :resources []
                 :evidence [{:kind :arguments :text "[{query: needle}]"}
                            {:kind :result :text "18 matches"}]}
                {:id "call-2"
                 :sequence 2
                 :operation :run_tests
                 :presenter :tests
                 :classification :verification
                 :state :running
                 :summary "suite"
                 :result-summary "24 passed"
                 :resources []
                 :evidence [{:kind :arguments :text "suite"}]}]
         :omitted {:rows 0 :by-classification {}}}

        file
        (companion-fixture-file "activity.fixture.json")

        fixture
        (some-> file
                slurp
                wire/parse-json)]

    (is (some? file))
    (when fixture
      (is (= "activity" (get fixture "classification")))
      (is (= [] (get fixture "nodes")))
      (is (= (wire/parse-json (wire/json-str (activity/presentation state)))
             (get fixture "activity"))))))
