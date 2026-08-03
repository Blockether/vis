(ns com.blockether.vis.internal.gateway.human-input-test
  "An extension blocked on a typed human-input request must reach the COMPANION
   APP, not only the TUI.

   These tests pin the whole app path: the request defaults to both surfaces,
   it names its session, the gateway bridge turns it into `human_input.request`
   / `human_input.close` session events, the REST helpers scope every answer to
   the owning session, and the push tap alerts a phone that a run is parked."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.gateway.human-input :as gw-hi]
            [com.blockether.vis.internal.gateway.push :as push]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.human-input :as hi]
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
  (let
    [seen
     (atom [])

     k
     (keyword "human-input-test" (str (System/nanoTime)))]

    (state/add-event-tap! k
                          (fn [sid event]
                            (swap! seen conj [(str sid) event])))
    (try (f seen) (finally (state/remove-event-tap! k)))))

(defn- events-of
  "Every collected event of `type` naming `request-id`. Scoped by request id so
   a sibling test's traffic can never satisfy an assertion here."
  [seen type request-id]
  (filterv (fn [[_ event]]
             (and (= type (get event "type"))
                  (= request-id (or (get-in event ["request" "id"]) (get event "request_id")))))
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
  (let
    [sid
     (str (random-uuid))

     rid
     (str "req-" (random-uuid))]

    (with-events
      (fn [seen]
        (let [answer (future (hi/request! (spec :id rid :session-id sid)))]
          (try (testing "the pause becomes a session event, so SSE + replay carry it"
                 (is (await-true #(seq (events-of seen "human_input.request" rid))))
                 (let [[event-sid event] (first (events-of seen "human_input.request" rid))]
                   (is (= sid event-sid))
                   (is (= "Deploy?" (get-in event ["request" "title"])))
                   (is (= sid (get-in event ["request" "session_id"])))
                   (is (= ["confirm"] (mapv #(get % "id") (get-in event ["request" "fields"]))))))
               (testing "a client that connects later still finds the open form"
                 (let [view (first (filterv #(= rid (:id %)) (gw-hi/pending sid)))]
                   (is (some? view))
                   (is (= "Deploy?" (:title view)))))
               (testing "an answer is scoped to the session that owns the request"
                 (is (some? (gw-hi/request-of sid rid)))
                 (is (nil? (gw-hi/request-of (str (random-uuid)) rid))))
               (testing "the app's answer releases the blocked extension"
                 (is (= {:is-accepted true} (gw-hi/submit! rid {"confirm" true})))
                 (let [result (deref answer 2000 ::timeout)]
                   (is (true? (:is-submitted result)))
                   (is (= true (get-in result [:values "confirm"])))))
               (testing "the close event tells every OTHER client to drop the form"
                 (is (await-true #(seq (events-of seen "human_input.close" rid))))
                 (let [[event-sid event] (first (events-of seen "human_input.close" rid))]
                   (is (= sid event-sid))
                   (is (= "submitted" (get event "reason")))))
               (finally (hi/cancel! rid "cleanup"))))))))

(deftest rejected-answer-keeps-the-request-open-test
  (gw-hi/install!)
  (let
    [sid
     (str (random-uuid))

     rid
     (str "req-" (random-uuid))

     answer
     (future (hi/request! {:title "Key?"
                           :id rid
                           :session-id sid
                           :timeout-ms 4000
                           :fields [{:id "key" :type "plaintext" :is-required true}]}))]

    (try (is (await-true #(some? (gw-hi/request-of sid rid))))
         (testing "validation is the engine's, so app and TUI accept the same answers"
           (let [outcome (gw-hi/submit! rid {"key" "   "})]
             (is (false? (:is-accepted outcome)))
             (is (contains? (:errors outcome) "key")))
           (is (some? (gw-hi/request-of sid rid))))
         (testing "cancelling releases the waiter"
           (is (true? (gw-hi/cancel! rid)))
           (is (false? (:is-submitted (deref answer 2000 ::timeout))))
           (is (nil? (gw-hi/request-of sid rid))))
         (finally (hi/cancel! rid "cleanup")))))

(deftest sessionless-request-is-not-appended-test
  (gw-hi/install!)
  (let [rid (str "req-" (random-uuid))]
    (with-events (fn [seen]
                   (let [answer (future (hi/request! (spec :id rid)))]
                     (try (testing "a request outside a gateway session has nowhere to land"
                            (is (await-true #(some? (hi/pending-request rid))))
                            (is (empty? (gw-hi/pending (str (random-uuid)))))
                            (is (true? (hi/cancel! rid)))
                            (is (false? (:is-submitted (deref answer 2000 ::timeout))))
                            (is (empty? (events-of seen "human_input.request" rid)))
                            (is (empty? (events-of seen "human_input.close" rid))))
                          (finally (hi/cancel! rid "cleanup"))))))))

(deftest push-alerts-a-parked-run-test
  (testing "a blocked run pushes on its own collapse lane"
    (let
      [n (#'push/human-input-notification
          "sid-9"
          {"type" "human_input.request"
           "request" {"id" "req-1" "title" "API key" "description" "production"}})]
      (is (= "sid-9" (:thread-id n)))
      (is (= "sid-9:human-input" (:collapse-id n)))
      (is (str/includes? (:body n) "API key"))
      (is (str/includes? (:body n) "production"))
      (is (= "human_input.request" (get-in n [:data :type])))
      (is (= "req-1" (get-in n [:data :request_id])))
      (is (= "sid-9" (get-in n [:data :session_id]))))))
