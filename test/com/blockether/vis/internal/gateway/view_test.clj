(ns com.blockether.vis.internal.gateway.view-test
  "An extension blocked on a typed human-input request must reach the COMPANION
   APP, not only the TUI.

   These tests pin the whole app path: the request defaults to both surfaces,
   it names its session, the gateway bridge turns it into `view.open`
   / `view.close` session events, the REST endpoints the phone actually
   calls answer it, the push tap alerts a phone that a run is parked, and the
   JSON fixture the companion's own suite parses is the engine's own projection.

   The matching TUI half — one request driving the terminal dialog and this
   bridge at the same time — is the standalone app suite under `apps/vis-tui/test`."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.gateway :as gateway-contract]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.activity.core :as activity]
            [com.blockether.vis.internal.gateway.client :as client]
            [com.blockether.vis.internal.gateway.push :as push]
            [com.blockether.vis.internal.gateway.server.views :as views-api]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.gateway.view :as gw-hi]
            [com.blockether.vis.internal.view.core :as hi]
            [com.blockether.vis.internal.view.materializer :as live]
            [com.blockether.vis.contract.view :as hi-spec]
            [lazytest.core :refer [defdescribe expect it]]
            [reitit.ring :as ring]
            [ring.adapter.jetty9 :as jetty])
  (:import [java.net URLEncoder]
           [org.eclipse.jetty.server Server ServerConnector]))

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

(defdescribe
  human-input-request-shape-test
  (it "a request reaches BOTH surfaces unless the caller narrows it"
      (expect (= [:tui :app] (:channel-ids (hi/normalize-request (spec)))))
      (expect (= [:tui] (:channel-ids (hi/normalize-request (spec :channel-ids [:tui])))))
      (expect (= [:app] (:channel-ids (hi/normalize-request (spec :channel-id :app))))))
  (it "the request names its session, in Clojure or as converted JSON"
      (expect (= "sid-1" (:session-id (hi/normalize-request (spec :session-id "sid-1")))))
      (expect (= "sid-2"
                 (:session-id (hi/normalize-request (hi/spec<-json (assoc (spec)
                                                                     "session_id" "sid-2"))))))
      (expect (nil? (:session-id (hi/normalize-request (spec))))))
  (it "the channel/wire view keeps the session — the app routes on it"
      (expect (= "sid-3"
                 (:session-id (hi/request->view (hi/normalize-request (spec :session-id
                                                                            "sid-3"))))))))

(defdescribe
  app-sees-and-answers-a-blocked-run-test
  (it "app sees and answers a blocked run"
      (gw-hi/install!)
      (let [sid
            (str (random-uuid))

            rid
            (str "req-" (random-uuid))]

        (with-events
          (fn [seen]
            (let [answer (future (hi/request! (spec :id rid :session-id sid)))]
              (try
                ;; the pause becomes a session event, so SSE + replay carry it
                (expect (await-true #(seq (events-of seen "view.open" rid))))
                (let [[event-sid event] (first (events-of seen "view.open" rid))]
                  (expect (= sid event-sid))
                  (expect (= "input" (get event "kind")))
                  (expect (= "Deploy?" (get-in event ["view" "title"])))
                  (expect (= sid (get-in event ["view" "session_id"])))
                  (expect (= ["confirm"] (mapv #(get % "id") (get-in event ["view" "fields"])))))
                ;; a client that connects later still finds the open form
                (let [view (first (filterv #(= rid (:id %)) (gw-hi/input-views sid)))]
                  (expect (some? view))
                  (expect (= "Deploy?" (:title view))))
                ;; an answer is scoped to the session that owns the request
                (expect (some? (gw-hi/input-view-of sid rid)))
                (expect (nil? (gw-hi/input-view-of (str (random-uuid)) rid)))
                ;; the app's answer releases the blocked extension
                (expect (true? (:is-accepted
                                 (gw-hi/action! rid {:action :submit :values {"confirm" true}}))))
                (let [result (deref answer 2000 ::timeout)]
                  (expect (true? (:is-submitted result)))
                  (expect (= true (get-in result [:values "confirm"]))))
                ;; the close event tells every OTHER client to drop the form
                (expect (await-true #(seq (events-of seen "view.close" rid))))
                (let [[event-sid event] (first (events-of seen "view.close" rid))]
                  (expect (= sid event-sid))
                  (expect (= "input" (get event "kind")))
                  (expect (= "submitted" (get-in event ["result" "reason"]))))
                (finally (hi/cancel! rid "cleanup")))))))))

(defdescribe rejected-answer-keeps-the-request-open-test
             (it "rejected answer keeps the request open"
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
                                             :fields
                                             [{:id "key" :type "plaintext" :is-required true}]}))]

                   (try (expect (await-true #(some? (gw-hi/input-view-of sid rid))))
                        ;; validation is the engine's, so app and TUI accept the same answers
                        (let [outcome (gw-hi/action! rid {:action :submit :values {"key" "   "}})]
                          (expect (false? (:is-accepted outcome)))
                          (expect (contains? (:errors outcome) "key")))
                        (expect (some? (gw-hi/input-view-of sid rid)))
                        ;; cancelling releases the waiter
                        (expect (true? (:is-accepted (gw-hi/action! rid {:action :cancel}))))
                        (expect (false? (:is-submitted (deref answer 2000 ::timeout))))
                        (expect (nil? (gw-hi/input-view-of sid rid)))
                        (finally (hi/cancel! rid "cleanup"))))))

(defdescribe sessionless-request-is-refused-test
             (it "the engine refuses it before anything blocks"
                 ;; Regression, issue #104: a request that named no session was dropped here in
                 ;; silence — the companion app never learned the run was parked and nothing in
                 ;; the logs said a request had been thrown away. Issue #113: it is refused at
                 ;; the source now, so no caller ever parks on a dialog only this process could
                 ;; answer.
                 (gw-hi/install!)
                 (let [rid (str "req-" (random-uuid))]
                   (with-events (fn [seen]
                                  (let [ex (try (hi/request! (spec :id rid))
                                                nil
                                                (catch clojure.lang.ExceptionInfo e e))]
                                    (expect (= :vis/view-invalid-request (:type (ex-data ex))))
                                    (expect (nil? (hi/pending-request rid)))
                                    (expect (empty? (events-of seen "view.open" rid)))))))))

(defdescribe
  push-alerts-a-parked-run-test
  (it "push alerts a parked run"
      ;; The describer stays installed for every case below: a session title is
      ;; minted from whatever opened the session, so it must never reach the alert.
      (let [prev @@#'push/describe-session]
        (try (push/set-session-describer! (fn [_sid _tid]
                                            {:title "Ship the parser"}))
             ;; the title demands action and then asks the question; the body is the detail
             (let [n (#'push/input-view-notification
                      "sid-9"
                      {"type" "view.open"
                       "kind" "input"
                       "view" {"id" "req-1"
                               "title" "Approve the deploy"
                               "description" "v1.2.3 to production"}})]
               (expect (= "Action needed — Approve the deploy" (:title n)))
               (expect (= "v1.2.3 to production" (:body n)))
               (expect (= "sid-9" (:thread-id n)))
               (expect (= "sid-9:input-view" (:collapse-id n)))
               (expect (= "view.open" (get-in n [:data :type])))
               (expect (= "req-1" (get-in n [:data :view_id])))
               (expect (= "sid-9" (get-in n [:data :session_id]))))
             ;; a question with no detail under it is never repeated in the body
             (let [n (#'push/input-view-notification
                      "sid-9"
                      {"type" "view.open"
                       "kind" "input"
                       "view" {"id" "req-2" "title" "Approve the deploy"}})]
               (expect (= "Action needed — Approve the deploy" (:title n)))
               (expect (= "Vis is waiting on your answer." (:body n))))
             ;; a request carrying only a description still says what it wants
             (let [n (#'push/input-view-notification
                      "sid-9"
                      {"type" "view.open"
                       "kind" "input"
                       "view" {"id" "req-3" "description" "Approve the deploy"}})]
               (expect (= "Action needed" (:title n)))
               (expect (= "Approve the deploy" (:body n))))
             ;; an unlabelled request is still a demand, never blank
             (let [n (#'push/input-view-notification
                      "sid-9"
                      {"type" "view.open" "kind" "input" "view" {"id" "req-4"}})]
               (expect (= "Action needed" (:title n)))
               (expect (= "Vis is waiting on your answer." (:body n))))
             (finally (push/set-session-describer! prev))))))

;; The endpoints the phone actually calls
;;
;; `gw-hi/action!` is the in-process seam; the app only ever sees the shared HTTP
;; handler and its JSON. These drive the real ring handler so a routing, path-param
;; or encoding slip cannot hide behind a green in-process test.

(defn- rv
  "Resolve a (private) var in the gateway server namespace."
  [sym]
  (requiring-resolve (symbol "com.blockether.vis.internal.gateway.server" (name sym))))

(defn- body-stream
  [m]
  (java.io.ByteArrayInputStream. (.getBytes ^String (wire/json-str m) "UTF-8")))

(defn- json-body [response] (wire/parse-json (:body response)))

(defn- view-action-response
  "Apply one action through the exact shared HTTP handler the Companion calls."
  [sid view-id action]
  (#'views-api/view-action-handler
   {:path-params {:sid sid :view-id view-id} :body (body-stream action)}))

(defdescribe
  the-app-answers-a-parked-run-over-http-test
  (it
    "the app answers a parked run over http"
    (gw-hi/install!)
    (let [sid
          (str (random-uuid))

          rid
          (str "req-" (random-uuid))

          answer
          (future (hi/request!
                    (spec :id rid
                          :session-id sid
                          :fields
                          [{:id "note" :type "plaintext" :label "Note" :is-required true}])))]

      (try (expect (await-true #(some? (gw-hi/input-view-of sid rid))))
           ;; a phone that starts cold still finds the open form, snake_case
           (let [response
                 (#'views-api/list-input-views-handler {:path-params {:sid sid}})

                 request
                 (first (get (json-body response) "requests"))]

             (expect (= 200 (:status response)))
             (expect (= "application/json" (get-in response [:headers "Content-Type"])))
             (expect (= rid (get request "id")))
             (expect (= sid (get request "session_id")))
             (expect (= "Deploy?" (get request "title")))
             (expect (= ["note"] (mapv #(get % "id") (get request "fields"))))
             (expect (true? (get-in request ["fields" 0 "is_required"]))))
           ;; the engine's validation answers the app, and the run stays parked
           (let [body (json-body
                        (view-action-response sid rid {:action "submit" :values {"note" "   "}}))]
             (expect (false? (get body "is_accepted")))
             (expect (= "submit" (get body "action")))
             (expect (= rid (get body "view_id")))
             (expect (contains? (get body "errors") "note"))
             (expect (some? (gw-hi/input-view-of sid rid))))
           ;; another session may not answer this View
           (expect (= 404
                      (:status (view-action-response (str (random-uuid))
                                                     rid
                                                     {:action "submit" :values {"note" "ship"}}))))
           ;; an accepted answer releases the blocked extension
           (let [body
                 (json-body
                   (view-action-response sid rid {:action "submit" :values {"note" "ship it"}}))]
             (expect (true? (get body "is_accepted")))
             (expect (= "submit" (get body "action")))
             (expect (= "ship it" (get-in (deref answer 2000 ::timeout) [:values "note"]))))
           ;; a settled request is gone from the snapshot and answerable no more
           (expect (empty? (get (json-body (#'views-api/list-input-views-handler
                                            {:path-params {:sid sid}}))
                                "requests")))
           (expect (= 404 (:status (view-action-response sid rid {:action "cancel"}))))
           (finally (hi/cancel! rid "cleanup"))))))

(defdescribe the-app-cancels-a-parked-run-over-http-test
             (it "the app cancels a parked run over http"
                 (gw-hi/install!)
                 (let [sid
                       (str (random-uuid))

                       rid
                       (str "req-" (random-uuid))

                       answer
                       (future (hi/request! (spec :id rid :session-id sid)))]

                   (try (expect (await-true #(some? (gw-hi/input-view-of sid rid))))
                        (let [body (json-body (view-action-response sid rid {:action "cancel"}))]
                          (expect (true? (get body "is_accepted")))
                          (expect (= "cancel" (get body "action")))
                          (expect (= rid (get body "view_id")))
                          (expect (false? (:is-submitted (deref answer 2000 ::timeout))))
                          (expect (empty? (gw-hi/input-views sid))))
                        (finally (hi/cancel! rid "cleanup"))))))

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

(defdescribe
  the-app-fixture-is-the-engines-own-projection-test
  (it "the app fixture is the engines own projection"
      (let [view (hi/request->view (hi/normalize-request fixture-spec))]
        ;; the companion parses engine bytes, not a hand-written lookalike
        (let [file (fixture-file)]
          (expect (some? file))
          (when file
            (expect (= (wire/parse-json (slurp file)) (wire/parse-json (wire/json-str view))))))
        ;; The app's own suite renders this fixture and asserts a control for every
        ;; node in it. That proof is only worth the vocabulary it covers, so the
        ;; fixture holds ONE OF EVERY KIND the engine can send: a type added to the
        ;; spec and left out of the fixture would otherwise ship an app that paints
        ;; a hole in a dialog which has already stopped somebody's run.
        ;; and holds one node of every kind the engine can send
        (expect (= (conj (into (set (vals hi-spec/field-types)) (vals hi-spec/decor-types))
                         hi-spec/group-type)
                   (node-types (:fields view)))))))

(defdescribe the-companion-urls-route-to-the-shared-view-action-handler-test
             (it "the URLs `gateway.ts` builds are the URLs this router serves"
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

                   (expect (= @#'views-api/list-input-views-handler
                              (get-in (match (str "/v1/sessions/" sid "/views/input"))
                                      [:data :get :handler])))
                   (let [m (match (str "/v1/sessions/" sid "/views/" encoded "/actions"))]
                     (expect (= @#'views-api/view-action-handler (get-in m [:data :post :handler])))
                     ;; and hand the shared handler the View id it acts on
                     (expect (= sid (str (get-in m [:path-params :sid]))))
                     (expect (= rid (get-in m [:path-params :view-id])))))))

(defdescribe
  a-hostile-body-cannot-park-or-settle-a-run-test
  (it
    "a hostile body cannot park or settle a run"
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

      (try (expect (await-true #(some? (gw-hi/input-view-of sid rid))))
           ;; a malformed body is a 400 — never a 500, never a settled run
           (doseq [body [{:action "submit" :values "text"} {:action "submit" :values [1 2]}
                         {:action "submit" :values 42} {:action "submit" :values nil}
                         {:values {"note" "missing action"}} {:action "unknown"} nil]]
             (expect (= 400 (:status (view-action-response sid rid body)))))
           (expect (= 400
                      (:status (#'views-api/view-action-handler
                                {:path-params {:sid sid :view-id rid}}))))
           (expect (= 400
                      (:status (#'views-api/view-action-handler
                                {:path-params {:sid sid :view-id rid}
                                 :body (java.io.ByteArrayInputStream. (.getBytes "not json"
                                                                                 "UTF-8"))}))))
           (expect (some? (gw-hi/input-view-of sid rid)))
           ;; a structured value is rejected, not stringified into the answer
           (let [body (json-body
                        (view-action-response sid rid {:action "submit" :values {"note" {"a" 1}}}))]
             (expect (false? (get body "is_accepted")))
             (expect (= "must be text" (get-in body ["errors" "note"])))
             (expect (some? (gw-hi/input-view-of sid rid))))
           ;; the escaped id still routes, and the same handler answers it
           (let [match ((requiring-resolve 'reitit.core/match-by-path)
                         ((rv 'router) "token" [])
                         (str "/v1/sessions/" sid "/views/req%2Fone%20two/actions"))]
             (expect (= rid (get-in match [:path-params :view-id])))
             (expect (= @#'views-api/view-action-handler (get-in match [:data :post :handler])))
             (expect (true? (get (json-body (#'views-api/view-action-handler
                                             {:path-params (:path-params match)
                                              :body (body-stream {:action "submit"
                                                                  :values {"note" "typed"}})}))
                                 "is_accepted"))))
           (expect
             (= {:is-submitted true :reason "submitted" :request-id rid :values {"note" "typed"}}
                (deref answer 2000 ::stuck)))
           (finally (hi/cancel! rid))))))

(defdescribe
  a-storm-of-answers-settles-a-parked-run-exactly-once-test
  (it
    "a storm of answers settles a parked run exactly once"
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
                                         :fields
                                         [{:id "note" :type "plaintext" :is-required true}])))

              _
              (expect (await-true #(some? (gw-hi/input-view-of sid rid))))

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

          ;; exactly one answer wins and the extension is released once
          (expect (= 1 (count winners)))
          (expect (= rid (:request-id final)))
          (expect (= (if (= :cancel (ffirst winners)) "cancelled" "submitted") (:reason final)))
          ;; and every surface is told exactly once that the form is gone
          (expect (await-true #(= 1 (count (events-of seen "view.close" rid)))))
          (expect (= 1 (count (events-of seen "view.open" rid))))
          (expect (empty? (gw-hi/input-views sid)))
          (expect (nil? (gw-hi/input-view-of sid rid)))
          (expect (= {:action :submit :view-id rid :is-accepted false :reason "unknown"}
                     (gw-hi/action! rid {:action :submit :values {"note" "late"}})))
          (expect
            (= 404
               (:status
                 (view-action-response sid rid {:action "submit" :values {"note" "late"}})))))))))

(defdescribe
  a-request-that-refuses-cancellation-refuses-the-app-too-test
  (it
    "a request that refuses cancellation refuses the app too"
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

      (try (expect (await-true #(some? (gw-hi/input-view-of sid rid))))
           ;; the app is refused the escape hatch the TUI dialog also denies
           (let [response (view-action-response sid rid {:action "cancel"})]
             (expect (= 409 (:status response)))
             (expect (= "view-action-refused" (get-in (json-body response) ["error" "type"])))
             (expect (false? (:is-cancellable (gw-hi/input-view-of sid rid))))
             (expect (some? (gw-hi/input-view-of sid rid))))
           ;; answering it is still the way out
           (expect
             (true? (get (json-body
                           (view-action-response sid rid {:action "submit" :values {"note" "yes"}}))
                         "is_accepted")))
           (expect (true? (:is-submitted (deref answer 2000 ::stuck))))
           (finally (hi/cancel! rid))))))

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

(defdescribe
  the-app-watches-a-live-view-test
  (it
    "the app watches a live view"
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
                                  :nodes
                                  [{:id "now" :type "status" :text "Polling…" :tone "running"}
                                   {:id "tail" :type "log"}]})

                  view-id
                  (:id view)]

              (try
                ;; the open crosses as an ordinary session event, in snake_case
                (expect (await-true
                          #(seq (live-events-of seen gateway-contract/view-open-event view-id))))
                (let [[[event-sid event]]
                      (live-events-of seen gateway-contract/view-open-event view-id)]
                  (expect (= sid event-sid))
                  (expect (= "CI" (get-in event ["view" "title"])))
                  (expect (= ["now" "tail"] (mapv #(get % "id") (get-in event ["view" "nodes"])))))
                (hi/patch-live! view-id [{:op "append" :node-id "tail" :lines ["one" "two"]}])
                (hi/patch-live! view-id
                                [{:op "append" :node-id "tail" :lines ["three"]}
                                 {:op "set" :node-id "now" :text "Building"}])
                ;; patches ride ONE coalesced frame that says which of them it carries
                (gw-hi/flush-live-patches!)
                (expect (await-true
                          #(seq (live-events-of seen gateway-contract/view-patch-event view-id))))
                (let [frames
                      (live-events-of seen gateway-contract/view-patch-event view-id)

                      [_ event]
                      (first frames)]

                  (expect (= 1 (count frames)))
                  (expect (= 1 (get event "first_seq")))
                  (expect (= 2 (get-in event ["patch" "seq"])))
                  ;; Two appends on one node became one; the `set` on the OTHER node
                  ;; kept its place, because merging across nodes would reorder the run.
                  (expect (= [["append" "tail"] ["set" "now"]]
                             (mapv (juxt #(get % "op") #(get % "node_id"))
                                   (get-in event ["patch" "ops"]))))
                  (expect (= ["one" "two" "three"] (get-in event ["patch" "ops" 0 "lines"]))))
                (finally (hi/close-live! view-id)))
              ;; and the ending carries the picture the model reads, not a rendering of it
              (expect (await-true
                        #(seq (live-events-of seen gateway-contract/view-close-event view-id))))
              (let [[[_ event]] (live-events-of seen gateway-contract/view-close-event view-id)]
                (expect (true? (get-in event ["result" "is_completed"])))
                (expect (= "completed" (get-in event ["result" "reason"])))
                (expect (= ["now" "tail"]
                           (mapv #(get % "id") (get-in event ["result" "view" "nodes"]))))
                (expect (nil? (get-in event ["result" "markdown"])))))))))))

;; The bridge holds a view's patches for one flush window, so a gateway that goes
;; away mid-stream would swallow whatever the window still had. `stop!` in
;; `com.blockether.vis.internal.gateway.server` is [[gw-hi/uninstall!]]'s one
;; caller, and this is what calling it buys.
(defdescribe
  a-gateway-going-away-publishes-what-it-still-holds-test
  (it "a gateway going away publishes what it still holds"
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
                        (hi/open-live!
                          {:title "CI" :session-id sid :nodes [{:id "tail" :type "log"}]})

                        view-id
                        (:id view)]

                    (try (hi/patch-live! view-id [{:op "append" :node-id "tail" :lines ["one"]}])
                         ;; a patch waits for the tick, and the tick is nowhere near due
                         (expect (empty?
                                   (live-events-of seen gateway-contract/view-patch-event view-id)))
                         ;; so the gateway leaving is what publishes it
                         ;; Subscribe again at once: the bus is process-local, so this one
                         ;; listener is also serving every sibling test's view.
                         (gw-hi/uninstall!)
                         (gw-hi/install!)
                         (let [frames
                               (live-events-of seen gateway-contract/view-patch-event view-id)

                               [[_ event]]
                               frames]

                           (expect (= 1 (count frames)))
                           (expect (= ["one"] (get-in event ["patch" "ops" 0 "lines"]))))
                         (finally (hi/close-live! view-id))))))))))))

(defdescribe
  the-app-reads-a-live-view-back-over-http-test
  (it
    "the app reads a live view back over http"
    (gw-hi/install!)
    (recorded
      (fn []
        (let [sid
              (str (random-uuid))

              view
              (hi/open-live! {:title "CI" :session-id sid :nodes [{:id "tail" :type "log"}]})

              view-id
              (:id view)]

          (try
            (hi/patch-live!
              view-id
              [{:op "append" :node-id "tail" :lines (mapv #(str "line " %) (range 1 21))}])
            ;; a phone that starts cold reads the CURRENT picture, not a stream it missed
            (let [response
                  (#'views-api/list-live-views-handler {:path-params {:sid sid}})

                  answered
                  (first (get (json-body response) "views"))]

              (expect (= 200 (:status response)))
              (expect (= "application/json" (get-in response [:headers "Content-Type"])))
              (expect (= view-id (get answered "id")))
              (expect (= sid (get answered "session_id")))
              (expect (= ["tail"] (mapv #(get % "id") (get answered "nodes")))))
            ;; and scrolls back through output whose patches it never received
            (let [body (json-body (#'views-api/live-view-log-handler
                                   {:path-params {:sid sid :view-id view-id}
                                    :query-params {"node" "tail" "from" "5" "limit" "3"}}))]
              (expect (= "tail" (get body "node_id")))
              (expect (= 5 (get body "from")))
              (expect (= 20 (get body "total")))
              (expect (= ["line 6" "line 7" "line 8"] (get body "lines"))))
            ;; search pages use match offsets and retain original line numbers
            (let [body (json-body (#'views-api/live-view-log-handler
                                   {:path-params {:sid sid :view-id view-id}
                                    :query-params
                                    {"node" "tail" "query" "LINE 1" "from" "1" "limit" "2"}}))]
              (expect (= 11 (get body "matched")))
              (expect (= 20 (get body "total")))
              (expect (= [10 11] (get body "line_numbers")))
              (expect (= ["line 10" "line 11"] (get body "lines"))))
            ;; a view id belonging to another session is not stoppable from here
            (expect (= 404
                       (:status
                         (view-action-response (str (random-uuid)) view-id {:action "interrupt"}))))
            ;; the app's stop action carries the words typed with it
            (let [body
                  (json-body
                    (view-action-response sid view-id {:action "interrupt" :note "wrong subnet"}))]
              (expect (true? (get body "is_accepted")))
              (expect (= "interrupt" (get body "action")))
              (expect (= view-id (get body "view_id")))
              (expect (nil? (gw-hi/live-view-of sid view-id)))
              (expect (empty? (gw-hi/live-views sid))))
            ;; a view that already ended answers 404 instead of pretending to stop again
            (expect (= 404 (:status (view-action-response sid view-id {:action "interrupt"}))))
            ;; and its record still answers, which is what makes a finished log readable
            (let [body (json-body (#'views-api/live-view-log-handler
                                   {:path-params {:sid sid :view-id view-id}
                                    :query-params {"node" "tail"}}))]
              (expect (= 20 (get body "total")))
              (expect (= 20 (count (get body "lines")))))
            ;; a closed record remains searchable
            (let [body (json-body (#'views-api/live-view-log-handler
                                   {:path-params {:sid sid :view-id view-id}
                                    :query-params {"node" "tail" "query" "LINE 20"}}))]
              (expect (= ["line 20"] (get body "lines")))
              (expect (= [20] (get body "line_numbers")))
              (expect (= 1 (get body "matched"))))
            (finally (hi/close-live! view-id))))))))

(defdescribe
  a-log-node-named-like-a-path-still-answers-over-http-test
  (it
    "the node rides the query string: a `/` in a node id is no route business"
    (gw-hi/install!)
    (recorded
      (fn []
        (let [sid
              (str (random-uuid))

              ;; A Jenkins job is `folder/job`; a surface that keys its log by the job
              ;; name used to leave the record unreachable — the gateway refused the
              ;; encoded separator inside a path segment before any route matched.
              node-id
              "glms-tests/glms-test-data#6064 · console"

              view
              (hi/open-live! {:title "CI" :session-id sid :nodes [{:id node-id :type "log"}]})

              view-id
              (:id view)

              ;; The real server's query parsing around the real router: the whole
              ;; HTTP path, with Jetty's own URI checks in front of it.
              gateway
              (jetty/run-jetty
                ((rv 'wrap-scoped-params) (ring/ring-handler ((rv 'router) nil [])) [])
                {:host "127.0.0.1" :port 0 :join? false})

              port
              (.getLocalPort ^ServerConnector (first (.getConnectors ^Server gateway)))

              log-page
              (fn [& {:as params}]
                (let [response (client/request!
                                 :get
                                 (str "/v1/sessions/" sid
                                      "/views/live/" view-id
                                      "/log?"
                                      (str/join
                                        "&"
                                        (map (fn [[k v]]
                                               (str k "=" (URLEncoder/encode (str v) "UTF-8")))
                                             params)))
                                 {:timeout-ms 2000})]
                  {:status (:status response) :json (wire/parse-json (:body response))}))]

          (try (hi/patch-live! view-id
                               [{:op "append"
                                 :node-id node-id
                                 :lines ["Started by user" "Building" "Finished: SUCCESS"]}])
               (with-redefs-fn {#'client/ensure-gateway! (constantly {:host "127.0.0.1" :port port})
                                #'client/ensure-client! (constantly "test-client")}
                 (fn []
                   (let [{:keys [status json]} (log-page "node" node-id "query" "finished")]
                     (expect (= 200 status) (pr-str json))
                     (expect (= node-id (get json "node_id")))
                     (expect (= ["Finished: SUCCESS"] (get json "lines")))
                     (expect (= [3] (get json "line_numbers")))
                     (expect (= 3 (get json "total"))))
                   ;; a page without a node is a bad request, not a mystery 404
                   (expect (= 400 (:status (log-page "query" "finished"))))))
               (finally (.stop ^Server gateway) (hi/close-live! view-id))))))))

;; Regression, session a64d44c2-8228-455f-926e-b3381f19a93b: tapping a CI job
;; had no engine action, so the visible selection and the log could never follow the tap.
(defdescribe
  the-app-selects-a-live-table-row-over-http-test
  (it
    "the app selects a live table row over http"
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
        ;; the selected ids become ordinary durable live state
        (let [response
              (view-action-response sid view-id {:action "select" :node_id "jobs" :item_ids ["b"]})

              body
              (json-body response)]

          (expect (= 200 (:status response)))
          (expect (true? (get body "is_accepted")))
          (expect (= "select" (get body "action")))
          (expect (= ["b"] (get body "item_ids")))
          (expect (= ["b"] (get-in (gw-hi/live-view-of sid view-id) [:nodes 0 :selected-ids]))))
        ;; a stale row id is refused without moving the selection
        (let [response (view-action-response
                         sid
                         view-id
                         {:action "select" :node_id "jobs" :item_ids ["missing"]})]
          (expect (= 400 (:status response)))
          (expect (= ["b"] (get-in (gw-hi/live-view-of sid view-id) [:nodes 0 :selected-ids]))))
        ;; another session cannot select in this view
        (expect (= 404
                   (:status (view-action-response
                              (str (random-uuid))
                              view-id
                              {:action "select" :node_id "jobs" :item_ids ["a"]}))))
        (finally (hi/close-live! view-id))))))

(defdescribe
  a-live-view-is-always-stoppable-and-the-stop-carries-its-words-test
  (it
    "a live view is always stoppable and the stop carries its words"
    (gw-hi/install!)
    (recorded
      (fn []
        (with-events
          (fn [seen]
            (let [sid
                  (str (random-uuid))

                  view
                  (hi/open-live!
                    {:title "Migration"
                     :session-id sid
                     :nodes [{:id "now" :type "status" :text "Writing rows" :tone "running"}]})

                  view-id
                  (:id view)]

              (try
                ;; no view refuses the stop: it asks nothing, so nothing is left unanswered
                (let [body (json-body (view-action-response sid
                                                            view-id
                                                            {:action "interrupt"
                                                             :note
                                                             "wrong subnet — I will re-run it"}))]
                  (expect (true? (get body "is_accepted")))
                  (expect (= "interrupt" (get body "action")))
                  (expect (nil? (gw-hi/live-view-of sid view-id))))
                ;; and the run reads WHO stopped it, and why, before it reads the picture
                (expect (await-true
                          #(seq (live-events-of seen gateway-contract/view-close-event view-id))))
                (let [[[_ event]] (live-events-of seen gateway-contract/view-close-event view-id)]
                  (expect (= "interrupted" (get-in event ["result" "reason"])))
                  (expect (true? (get-in event ["result" "is_from_human"])))
                  (expect (= "wrong subnet — I will re-run it" (get-in event ["result" "note"])))
                  (expect (= ["now"]
                             (mapv #(get % "id") (get-in event ["result" "view" "nodes"])))))
                (finally (hi/close-live! view-id))))
            (let [sid
                  (str (random-uuid))

                  bare
                  (:id (hi/open-live! {:title "Sweep"
                                       :session-id sid
                                       :nodes [{:id "now" :type "status" :text "Sweeping"}]}))]

              (try
                ;; a stop with no note still says a person sent it
                (let [body (json-body (view-action-response sid bare {:action "interrupt"}))]
                  (expect (true? (get body "is_accepted")))
                  (expect (= "interrupt" (get body "action"))))
                (expect (await-true
                          #(seq (live-events-of seen gateway-contract/view-close-event bare))))
                (let [[[_ event]] (live-events-of seen gateway-contract/view-close-event bare)]
                  (expect (true? (get-in event ["result" "is_from_human"])))
                  (expect (nil? (get-in event ["result" "note"]))))
                (finally (hi/close-live! bare))))))))))

(defdescribe
  view-actions-use-one-kind-independent-route-test
  (it
    "a View kind is policy, not part of the action resource address"
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
          #'views-api/view-action-handler

          action-match
          (match (str "/v1/sessions/" sid "/views/" view-id "/actions"))]

      (expect (some? action-handler))
      (expect (some? action-match))
      (expect (= (some-> action-handler
                         deref)
                 (get-in action-match [:data :post :handler])))
      ;; the obsolete kind/action-specific endpoints are gone
      (expect (nil? (match (str "/v1/sessions/" sid "/views/input/" view-id "/actions/submit"))))
      (expect (nil? (match (str "/v1/sessions/" sid "/views/live/" view-id "/actions/focus"))))
      (expect (nil? (match
                      (str "/v1/sessions/" sid "/views/live/" view-id "/actions/interrupt")))))))

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

(defdescribe
  the-app-live-fixture-is-the-engines-own-projection-test
  (it "the app live fixture is the engines own projection"
      (let [view
            (live/materialize (hi/normalize-live-view live-fixture-spec))

            file
            (live-fixture-file)

            fixture
            (some-> file
                    slurp
                    wire/parse-json)]

        (expect (some? file))
        (when fixture
          ;; the companion parses engine bytes, not a hand-written lookalike
          (expect (= (without-mint (wire/parse-json (wire/json-str view))) (without-mint fixture)))
          ;; the two minted values are still there, because the app keys on them
          (expect (some? (parse-uuid (get fixture "id"))))
          (expect (pos-int? (get fixture "created_at")))
          ;; the engine and shared presentation fixtures cover every node type
          (let [presentation (wire/parse-json
                               (slurp (io/resource "vis-contract/fixtures/live-primitives.json")))]
            (expect (= (conj (set (keys hi-spec/live-node-types)) hi-spec/group-type-name)
                       (set (map #(get % "type")
                                 (mapcat #(tree-seq map?
                                                    (fn [node]
                                                      (get node "fields"))
                                                    %)
                                         (concat (get fixture "nodes")
                                                 (get presentation "nodes"))))))))))))

(defdescribe
  the-app-activity-fixture-is-the-host-projection-test
  (it
    "the app activity fixture is the host projection"
    (let [state
          {:state :running
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
                   :operation :suite
                   :presenter :tests
                   :classification :verification
                   :state :running
                   :summary "suite"
                   :result-summary "24 passed"
                   :resources []
                   :evidence [{:kind :arguments :text "suite"}]}]}

          file
          (io/resource "vis-contract/fixtures/activity.json")

          fixture
          (some-> file
                  slurp
                  wire/parse-json)]

      (expect (some? file))
      (when fixture
        ;; Activity does not know its owner: the fixture IS the whole snapshot.
        (expect (= (wire/parse-json (wire/json-str (activity/presentation state))) fixture))))))

(defdescribe
  live-buttons-use-the-shared-http-action-test
  (it "live buttons use the shared http action"
      (gw-hi/install!)
      (recorded
        (fn []
          (let [sid
                (str (random-uuid))

                view
                (hi/open-live! {:title "Review"
                                :session-id sid
                                :nodes
                                [{:id "go" :type :button :label "Continue"}
                                 {:id "off" :type :button :label "Unavailable" :is-disabled true}]})

                id
                (:id view)]

            (try
              (expect (= 404
                         (:status (view-action-response (str (random-uuid))
                                                        id
                                                        {:action "activate" :node_id "go"}))))
              (expect (= 0 (get-in (gw-hi/live-view-of sid id) [:nodes 0 :clicks])))
              (let [response (view-action-response sid id {:action "activate" :node_id "go"})]
                (expect (= 200 (:status response)))
                (expect (true? (get (json-body response) "is_accepted")))
                (expect (= 1 (get-in (gw-hi/live-view-of sid id) [:nodes 0 :clicks]))))
              (let [response (view-action-response sid id {:action "activate" :node_id "off"})]
                (expect (= 200 (:status response)))
                (expect (false? (get (json-body response) "is_accepted")))
                (expect (= 0 (get-in (gw-hi/live-view-of sid id) [:nodes 1 :clicks]))))
              (expect (= 400
                         (:status
                           (view-action-response sid id {:action "activate" :node_id "missing"}))))
              (hi/close-live! id)
              (expect
                (= 404 (:status (view-action-response sid id {:action "activate" :node_id "go"}))))
              (finally (hi/close-live! id))))))))
