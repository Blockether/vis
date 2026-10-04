(ns com.blockether.vis.internal.automation.runner-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.automation.core :as automation]
            [com.blockether.vis.internal.automation.runner :as runner]
            [com.blockether.vis.internal.automation.webhook :as webhook]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.util :as util]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (com.sun.net.httpserver HttpExchange HttpHandler HttpServer)
           (java.net InetSocketAddress)
           (java.nio.charset StandardCharsets)
           (java.util HexFormat)))

(def ^:private terminal #{"completed" "failed" "cancelled" "skipped" "unknown"})

(defn- eventually
  "Poll `f` for up to five seconds and answer its first truthy value."
  [f]
  (loop [n 0]
    (or (f) (when (< n 500) (Thread/sleep 10) (recur (inc n))))))

(defn- settled
  [db run-id]
  (eventually #(let [run (ps/db-automation-run db run-id)] (when (terminal (:status run)) run))))

(defn- utf8 ^bytes [^String text] (.getBytes text StandardCharsets/UTF_8))

(defn- install!
  "Install a fake runtime. `submit!` answers the turn result; every call is recorded."
  [calls submit!]
  (runner/install-runtime! {:submit! (fn [sid opts]
                                       (swap! calls conj [:submit sid opts])
                                       (submit! sid opts))
                            :create-session! (fn [opts]
                                               (swap! calls conj [:create opts])
                                               {"id" "s-new"})
                            :delete-session! (fn [sid]
                                               (swap! calls conj [:delete sid]))
                            :session? (fn [sid]
                                        (= "s-old" sid))
                            :notify! (fn [alert]
                                       (swap! calls conj [:notify alert]))}))

(defn- answer
  [text]
  (constantly {"status" "done" "turn_id" "t-1" "content" [{"type" "prose" "markdown" text}]}))

(defn- calls-of [calls kind] (filter #(= kind (first %)) @calls))

(defn- with-runner
  "Run `f` with a fresh store, a fake runtime and the setting `allowed`."
  [allowed submit! f]
  (let [db
        (ps/db-create-connection! :memory)

        calls
        (atom [])]

    (install! calls submit!)
    (try (with-redefs [runner/allowed? (constantly allowed)]
           (f db calls))
         (finally (ps/db-dispose-connection! db)))))

(def ^:private input
  {"name" "Nightly check"
   "triggers" [{"kind" "every" "seconds" 60}]
   "prompt" "Check the build."
   "target" {"mode" "temporary"}})

(defn- create!
  [db changes]
  (get (automation/create! db (merge input changes) (System/currentTimeMillis)) "id"))

(defdescribe
  run-test
  (it "runs a temporary session, deletes it and sends one alert"
      (with-runner true
                   (answer "The build is green.")
                   (fn [db calls]
                     (let [id
                           (create! db {})

                           run
                           (settled db (get (runner/run-now! db id) "id"))

                           [_ sid opts]
                           (first (calls-of calls :submit))]

                       (expect (= "completed" (:status run)))
                       (expect (= "The build is green." (:answer run)))
                       (expect (= "t-1" (:turn_id run)))
                       (expect (nil? (:session_id run)))
                       (expect (= "s-new" sid))
                       (expect (= "Automation: Nightly check"
                                  (get-in (first (calls-of calls :create)) [1 :title])))
                       (expect (= "Check the build." (:display-request opts)))
                       (expect (str/starts-with? (:request opts)
                                                 "Automation \"Nightly check\" started this turn"))
                       (expect (str/ends-with? (:request opts) "\n\nCheck the build."))
                       (expect (= (str "automation-" (:id run)) (:idempotency-key opts)))
                       (expect (= [[:delete "s-new"]] (calls-of calls :delete)))
                       (expect (eventually #(seq (calls-of calls :notify))))
                       (expect (= "Nightly check"
                                  (get-in (first (calls-of calls :notify)) [1 :automation-name])))
                       (expect (runner/quiet-turn? "s-other" {"turn_id" "t-1"}))))))
  (it
    "keeps a silent answer quiet and always reports a failure"
    (with-runner
      true
      (fn [_ opts]
        (if (str/includes? (:request opts) "fail")
          {"status" "needs_input" "turn_id" "t-2" "content" []}
          {"status" "done"
           "turn_id" "t-3"
           "content" [{"type" "prose" "markdown" "[SILENT] nothing"}]}))
      (fn [db calls]
        (let [quiet
              (settled db (get (runner/run-now! db (create! db {})) "id"))

              failed
              (settled db (get (runner/run-now! db (create! db {"prompt" "Please fail."})) "id"))]

          (expect (true? (:is_silent quiet)))
          (expect (= "failed" (:status failed)))
          (expect (str/includes? (:error failed) "asked for input"))
          (expect (eventually #(seq (calls-of calls :notify))))
          (expect (= [(:id failed)] (map #(get-in % [1 :run "id"]) (calls-of calls :notify))))))))
  (it "uses an existing session and refuses a missing one"
      (with-runner
        true
        (answer "Done.")
        (fn [db calls]
          (let [kept
                (settled db
                         (get (runner/run-now!
                                db
                                (create! db {"target" {"mode" "session" "session_id" "s-old"}}))
                              "id"))

                missing
                (settled db
                         (get (runner/run-now!
                                db
                                (create! db {"target" {"mode" "session" "session_id" "s-gone"}}))
                              "id"))]

            (expect (= ["completed" "s-old"] [(:status kept) (:session_id kept)]))
            (expect (= "failed" (:status missing)))
            (expect (empty? (calls-of calls :create)))
            (expect (empty? (calls-of calls :delete)))))))
  (it "marks the session of a running turn as an automation session"
      (let [during (atom nil)]
        (with-runner true
                     (fn [sid _]
                       (reset! during (runner/automation-session? sid))
                       ((answer "Done.") sid nil))
                     (fn [db _]
                       (let [run (settled db
                                          (get (runner/run-now! db
                                                                (create! db
                                                                         {"target" {"mode" "session"
                                                                                    "session_id"
                                                                                    "s-old"}}))
                                               "id"))]
                         (expect (= "completed" (:status run)))
                         (expect (true? @during))
                         (expect (false? (runner/automation-session? "s-old"))))))))
  (it "skips a run that the setting blocks and sends deliver-only text without a model"
      (with-runner false
                   (answer "unused")
                   (fn [db _]
                     (let [blocked (settled db (get (runner/run-now! db (create! db {})) "id"))]
                       (expect (= ["skipped" "settings"] [(:status blocked) (:reason blocked)])))))
      (with-runner
        true
        (answer "unused")
        (fn [db calls]
          (let [sent (settled db
                              (get (runner/run-now! db (create! db {"deliver_only" true})) "id"))]
            (expect (= ["completed" "Check the build."] [(:status sent) (:answer sent)]))
            (expect (empty? (calls-of calls :submit)))))))
  (it "marks the open runs of a stopped gateway as unknown"
      (with-runner true
                   (answer "unused")
                   (fn [db _]
                     (let [id
                           (create! db {})

                           run-id
                           (str (random-uuid))]

                       (ps/db-automation-claim-run! db
                                                    {:id run-id
                                                     :automation_id id
                                                     :trigger_kind "manual"
                                                     :trigger_key "manual:lost"
                                                     :status "running"
                                                     :created_at 1
                                                     :owner_pid Integer/MAX_VALUE})
                       (runner/recover! db)
                       (expect (= "unknown" (:status (ps/db-automation-run db run-id)))))))))

(defdescribe schedule-test
             (it "claims each due occurrence once and skips an overlap"
                 (let [release (promise)]
                   (with-runner
                     true
                     (fn [_ _]
                       @release
                       {"status" "done" "turn_id" "t" "content" []})
                     (fn [db _]
                       (let [t0 (System/currentTimeMillis)
                             id (get (automation/create! db input t0) "id")
                             runs #(ps/db-automation-runs db {:automation-id id :limit 10})]

                         (runner/fire-schedules! db t0 (+ t0 60000) t0)
                         (runner/fire-schedules! db t0 (+ t0 60000) t0)
                         (expect (= 1 (count (runs))))
                         (expect (eventually #(runner/busy? id)))
                         (runner/fire-schedules! db (+ t0 60000) (+ t0 120000) t0)
                         (expect (= ["skipped" "overlap"] ((juxt :status :reason) (first (runs)))))
                         (deliver release true)
                         (expect (eventually #(= "completed" (:status (last (runs))))))
                         (expect (eventually #(not (runner/busy? id))))))))))

(defn- github-headers
  [secret body delivery event]
  {"x-github-event" event
   "x-github-delivery" delivery
   "x-hub-signature-256"
   (str "sha256=" (.formatHex (HexFormat/of) (util/hmac-sha256 (utf8 secret) (utf8 body))))})

(defdescribe
  webhook-test
  (it
    "checks, filters, renders and deduplicates deliveries"
    (with-runner
      true
      (answer "Reviewed.")
      (fn [db calls]
        (let
          [id
           (create! db
                    {"triggers" [{"kind" "webhook"
                                  "signature" "github"
                                  "events" ["pull_request.opened"]
                                  "filters" [{"field" "pull_request.base.ref" "equals" "main"}]}]
                     "prompt" "Review {pull_request.title}."})

           body
           "{\"action\":\"opened\",\"pull_request\":{\"title\":\"Fix parser\",\"base\":{\"ref\":\"main\"}}}"

           accept
           (fn [headers text]
             (runner/accept-webhook! db id {:headers headers :body (utf8 text)}))]

          (expect (= 401 (:status (accept (github-headers "x" body "d0" "pull_request") body))))
          (let [secret
                (get (automation/rotate-secret! db id "webhook" 1) "secret")

                accepted
                (accept (github-headers secret body "d1" "pull_request") body)

                run
                (settled db (get-in accepted [:body "run_id"]))

                [_ _ opts]
                (first (calls-of calls :submit))]

            (expect (= 401
                       (:status (accept (github-headers "wrong" body "d1" "pull_request") body))))
            (expect (= [202 "accepted"] [(:status accepted) (get-in accepted [:body "status"])]))
            (expect (= "completed" (:status run)))
            (expect (= "Review Fix parser." (:display-request opts)))
            (expect (str/includes? (:request opts) "untrusted content"))
            (expect (= [200 "duplicate"]
                       ((juxt :status #(get-in % [:body "status"]))
                         (accept (github-headers secret body "d1" "pull_request") body))))
            (expect (= "event"
                       (get-in (accept (github-headers secret body "d2" "push") body)
                               [:body "reason"])))
            (let [other (str/replace body "main" "dev")]
              (expect (= "filter"
                         (get-in (accept (github-headers secret other "d3" "pull_request") other)
                                 [:body "reason"]))))
            (automation/update! db id {"enabled" false} 2)
            (expect (= "disabled"
                       (get-in (accept (github-headers secret body "d4" "pull_request") body)
                               [:body "reason"])))
            (expect (= 404
                       (:status (runner/accept-webhook! db
                                                        (create! db {})
                                                        {:headers {} :body (utf8 "{}")})))))))))
  (it "limits the requests of one automation in one minute"
      (with-runner true
                   (answer "unused")
                   (fn [db _]
                     (let [id (create! db {"triggers" [{"kind" "webhook" "signature" "token"}]})]
                       (automation/rotate-secret! db id "webhook" 1)
                       (expect (= 429
                                  (:status (last (repeatedly 31
                                                             #(runner/accept-webhook!
                                                                db
                                                                id
                                                                {:headers {}
                                                                 :body (utf8 "{}")})))))))))))

(defn- receiver
  "A local callback receiver that records each request. It answers `statuses` in
   order and then repeats the last one."
  [& statuses]
  (let [requests
        (atom [])

        server
        (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)]

    (.createContext server
                    "/"
                    (reify
                      HttpHandler
                        (handle [_ exchange]
                          (let [^HttpExchange exchange
                                exchange

                                body
                                (slurp (.getRequestBody exchange))

                                headers
                                (into {}
                                      (map (fn [[k v]]
                                             [(str/lower-case k) (first v)]))
                                      (.getRequestHeaders exchange))]

                            (let [n (count (swap! requests conj {:headers headers :body body}))]
                              (.sendResponseHeaders exchange
                                                    (int (nth statuses
                                                              (min (dec n) (dec (count statuses)))))
                                                    -1))
                            (.close exchange)))))
    (.start server)
    {:url (str "http://127.0.0.1:" (.getPort (.getAddress server)) "/hook")
     :requests requests
     :stop #(.stop server 0)}))

(defdescribe
  callback-test
  (it "signs the callback with Standard Webhooks headers"
      (let [{:keys [url requests stop]} (receiver 204)]
        (try (with-runner
               true
               (answer "Green.")
               (fn [db _]
                 (let [id (create! db {"delivery" {"push" false "callback" {"url" url}}})
                       secret (get (automation/rotate-secret! db id "callback" 1) "secret")
                       run (settled db (get (runner/run-now! db id) "id"))
                       {:keys [headers body]} (eventually #(first @requests))]

                   (expect (nil? (webhook/verify "standard"
                                                 secret
                                                 {:headers headers
                                                  :body (utf8 body)
                                                  :now (System/currentTimeMillis)
                                                  :skew-seconds 300})))
                   (expect (str/includes? body "\"type\":\"run.completed\""))
                   (expect (str/includes? body (:id run)))
                   (expect (eventually
                             #(empty? (ps/db-automation-due-deliveries db Long/MAX_VALUE 10)))))))
             (finally (stop)))))
  (it "retries a failed callback and stops after the last attempt"
      (let [{:keys [url requests stop]} (receiver 500)]
        (try
          (with-runner
            true
            (answer "Green.")
            (fn [db _]
              (let [id (create! db {"delivery" {"push" false "callback" {"url" url}}})]
                (settled db (get (runner/run-now! db id) "id"))
                (expect (eventually
                          #(= 1
                              (:attempts
                                (first (ps/db-automation-due-deliveries db Long/MAX_VALUE 10))))))
                (expect (empty? (ps/db-automation-due-deliveries db (System/currentTimeMillis) 10)))
                (dotimes [_ 5]
                  (#'runner/send-callback!
                   db
                   (first (ps/db-automation-due-deliveries db Long/MAX_VALUE 10))))
                (expect (= 6 (count @requests)))
                (expect (empty? (ps/db-automation-due-deliveries db Long/MAX_VALUE 10))))))
          (finally (stop)))))
  (it "retries a failed callback and then delivers it once"
      (let [{:keys [url requests stop]} (receiver 500 204)]
        (try (with-runner
               true
               (answer "Green.")
               (fn [db _]
                 (let [id (create! db {"delivery" {"push" false "callback" {"url" url}}})]
                   (settled db (get (runner/run-now! db id) "id"))
                   (expect (eventually #(= 1
                                           (:attempts (first (ps/db-automation-due-deliveries
                                                               db
                                                               Long/MAX_VALUE
                                                               10))))))
                   (#'runner/send-callback!
                    db
                    (first (ps/db-automation-due-deliveries db Long/MAX_VALUE 10)))
                   (expect (= 2 (count @requests)))
                   ;; The retry keeps the message ID, so a receiver can drop a repeat.
                   (expect
                     (= 1 (count (distinct (map #(get-in % [:headers "webhook-id"]) @requests)))))
                   (expect (= 1 (count (distinct (map :body @requests)))))
                   (expect (empty? (ps/db-automation-due-deliveries db Long/MAX_VALUE 10))))))
             (finally (stop))))))

(defdescribe start-test
             (it "opens the shared connection for the gateway database spec"
                 ;; The gateway gives start! its database spec, not an open connection.
                 (let [db
                       (ps/db-create-connection! :memory)

                       seen
                       (atom nil)

                       spec
                       {:backend :sqlite :path "/tmp/vis-automation-test/vis.db"}]

                   (try (with-redefs [ps/db-shared-connection! (fn [s]
                                                                 (reset! seen s)
                                                                 db)]
                          ((runner/start! spec)))
                        (expect (= spec @seen))
                        (finally (ps/db-dispose-connection! db)))))
             (it "keeps the gateway running when the recovery fails"
                 (let [db (ps/db-create-connection! :memory)]
                   (try
                     (with-redefs [ps/db-shared-connection! (constantly db)
                                   runner/recover! (fn [_]
                                                     (throw (ex-info "The store is locked" {})))]

                       (expect (fn? (runner/start! {:backend :sqlite
                                                    :path "/tmp/vis-automation-test/vis.db"})))
                       (runner/stop!))
                     (finally (ps/db-dispose-connection! db))))))
