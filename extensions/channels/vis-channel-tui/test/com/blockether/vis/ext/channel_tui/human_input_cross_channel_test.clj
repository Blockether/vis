(ns com.blockether.vis.ext.channel-tui.human-input-cross-channel-test
  "CROSS-CHANNEL proof that ONE blocked extension drives BOTH surfaces.

   `human-input-test` covers the TUI dialog in isolation (the engine stubbed
   out) and the engine's own `gateway.human-input-test` covers the app path
   (no terminal in sight). Neither shows the two surfaces on the SAME request,
   which is exactly where a channel-routing or validation drift would hide.

   Here the REAL screen listener and the REAL gateway bridge are both
   subscribed to the real channel bus around a real `human-input/request!`, so
   every assertion below is about one parked run: it opens on both surfaces,
   either surface can answer it, the answer releases the extension, and the
   OTHER surface drops its form instead of leaving a dead dialog behind."
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.state :as state]
            [com.blockether.vis.ext.channel-tui.screen :as screen]
            [com.blockether.vis.ext.channel-tui.human-input :as tui-hi]
            [com.blockether.vis.internal.gateway.human-input :as gw]
            [com.blockether.vis.internal.gateway.state :as gw-state]
            [com.blockether.vis.internal.human-input :as engine]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]])
  (:import [com.googlecode.lanterna.input KeyStroke KeyType]))

(set! *warn-on-reflection* true)

(defn- await-true
  "Poll `pred` for up to a second. Requests are answered from another thread."
  [pred]
  (loop [attempts 200]
    (cond (pred) true
          (zero? attempts) false
          :else (do (Thread/sleep 5) (recur (dec attempts))))))

(defn- with-surfaces!
  "Call `f` with an atom collecting `[sid event]` for every session event, with
   BOTH surfaces live: the screen's own channel listener on `:tui` and the
   gateway bridge on `:app`."
  [f]
  (let
    [seen
     (atom [])

     tap-key
     (keyword "human-input-cross-channel-test" (str (System/nanoTime)))]

    (reset! state/app-db {:render-version 0})
    (gw/install!)
    (vis/add-channel-event-listener! :tui ::screen #'screen/handle-channel-event!)
    (gw-state/add-event-tap! tap-key
                             (fn [sid event]
                               (swap! seen conj [(str sid) event])))
    (try (f seen)
         (finally (gw-state/remove-event-tap! tap-key)
                  (vis/remove-channel-event-listener! :tui ::screen)
                  (reset! state/app-db {:render-version 0})))))

(defn- events-of
  "Every collected event of `type` naming `request-id`. Scoped by request id so
   a sibling test's traffic can never satisfy an assertion here."
  [seen type request-id]
  (filterv (fn [[_ event]]
             (and (= type (get event "type"))
                  (= request-id (or (get-in event ["request" "id"]) (get event "request_id")))))
    @seen))

(defn- ask!
  "Park an extension on a typed request, exactly like an extension would."
  [sid request-id fields]
  (future (engine/request! {:id request-id
                            :session-id sid
                            :title "Deploy?"
                            :description "production"
                            :timeout-ms 5000
                            :fields fields})))

(defn- tui-open? [request-id] (= request-id (get-in @state/app-db [:human-input :request :id])))

(defn- press!
  "Feed keystrokes to the open dialog through the screen's real key handler —
   no engine stub, so every submit round-trips through `human-input/submit!`."
  [& keys]
  (doseq [^KeyStroke key keys]
    (#'screen/human-input-key! @state/app-db key)))

(defn- stroke [c] (KeyStroke. (Character/valueOf ^Character c) false false))

(deftest one-request-opens-on-the-tui-and-the-app-test
  (with-surfaces!
    (fn [seen]
      (let
        [sid
         (str (random-uuid))

         rid
         (str "req-" (random-uuid))

         answer
         (ask! sid
               rid
               [{:id "note" :type "plaintext" :label "Note" :is-required true}
                {:id "confirm" :type "checkbox" :label "Confirm"}])]

        (try (is (await-true #(tui-open? rid)))
             (is (await-true #(seq (events-of seen "human_input.request" rid))))
             (testing "both surfaces render the SAME form, from one engine projection"
               (let
                 [[event-sid event]
                  (first (events-of seen "human_input.request" rid))

                  view
                  (get-in @state/app-db [:human-input :request])]

                 (is (= sid event-sid))
                 (is (= (:title view) (get-in event ["request" "title"])))
                 (is (= (:description view) (get-in event ["request" "description"])))
                 ;; Same fields, same order — the TUI reads keyword types, the
                 ;; app reads the snake_case wire encoding of that very view.
                 (is (= ["note" "confirm"] (mapv :id (:fields view))))
                 (is (= (mapv :id (:fields view))
                        (mapv #(get % "id") (get-in event ["request" "fields"]))))
                 (is (= (mapv (comp name :type) (:fields view))
                        (mapv #(get % "type") (get-in event ["request" "fields"]))))
                 (is (= (mapv :is-required (:fields view))
                        (mapv #(get % "is_required") (get-in event ["request" "fields"]))))))
             (testing "a client that connects late finds the same open request"
               (is (= [rid] (mapv :id (gw/pending sid)))))
             (testing "the APP's answer releases the run"
               (is (= {:is-accepted true} (gw/submit! rid {"note" "ship it" "confirm" true})))
               (let [result (deref answer 2000 ::timeout)]
                 (is (true? (:is-submitted result)))
                 (is (= "ship it" (get-in result [:values "note"])))
                 (is (= true (get-in result [:values "confirm"])))))
             (testing "and the TUI dialog closes instead of hanging on a dead request"
               (is (await-true #(nil? (:human-input @state/app-db))))
               (is (false? (#'screen/overlay-locked? @state/app-db)))
               (is (empty? (gw/pending sid)))
               (is (seq (events-of seen "human_input.close" rid))))
             (finally (engine/cancel! rid "cleanup")))))))

(deftest an-answer-typed-in-the-tui-releases-the-app-test
  (with-surfaces!
    (fn [seen]
      (let
        [sid
         (str (random-uuid))

         rid
         (str "req-" (random-uuid))

         answer
         (ask! sid rid [{:id "user" :type "plaintext" :label "User" :is-required true}])]

        (try (is (await-true #(tui-open? rid)))
             (is (await-true #(seq (gw/pending sid))))
             (testing "keys typed into the terminal answer the very same request"
               (press! (stroke \o) (stroke \k) (KeyStroke. KeyType/Enter))
               (let [result (deref answer 2000 ::timeout)]
                 (is (true? (:is-submitted result)))
                 (is (= "ok" (get-in result [:values "user"])))))
             (testing "the app is told to drop its form — it never shows a stale dialog"
               (is (await-true #(seq (events-of seen "human_input.close" rid))))
               (is (= "submitted"
                      (get (second (first (events-of seen "human_input.close" rid))) "reason")))
               (is (empty? (gw/pending sid)))
               (is (nil? (gw/request-of sid rid)))
               (is (nil? (:human-input @state/app-db))))
             (finally (engine/cancel! rid "cleanup")))))))

(deftest a-rejected-answer-reads-the-same-on-both-surfaces-test
  (with-surfaces!
    (fn [_seen]
      (let
        [sid
         (str (random-uuid))

         rid
         (str "req-" (random-uuid))

         answer
         (ask! sid rid [{:id "key" :type "plaintext" :label "API key" :is-required true}])]

        (try (is (await-true #(tui-open? rid)))
             (let [app-outcome (gw/submit! rid {"key" "   "})]
               (testing "the app's blank answer is rejected and the request stays parked"
                 (is (false? (:is-accepted app-outcome)))
                 (is (contains? (:errors app-outcome) "key"))
                 (is (some? (gw/request-of sid rid))))
               (testing "the TUI's blank answer is rejected with the SAME message"
                 ;; One validator lives in the engine, so neither surface can
                 ;; drift into accepting what the other refuses. The TUI runs it
                 ;; LOCALLY and never sends the blank answer at all, so the
                 ;; parity to prove is between the app's rejection and the
                 ;; message the band puts under the field.
                 (press! (KeyStroke. KeyType/Enter))
                 (let [form (:human-input @state/app-db)]
                   (is (true? (:is-submit-attempted form)))
                   (is (= (:errors app-outcome) (tui-hi/visible-errors form))))
                 (is (tui-open? rid))))
             (testing "an accepted answer still ends the pause everywhere"
               (press! (stroke \k) (KeyStroke. KeyType/Enter))
               (is (= "k" (get-in (deref answer 2000 ::timeout) [:values "key"])))
               (is (await-true #(nil? (:human-input @state/app-db))))
               (is (empty? (gw/pending sid))))
             (finally (engine/cancel! rid "cleanup")))))))

(deftest either-surface-can-cancel-the-other-test
  (with-surfaces!
    (fn [seen]
      (let
        [sid
         (str (random-uuid))

         from-app
         (str "req-" (random-uuid))

         from-tui
         (str "req-" (random-uuid))

         fields
         [{:id "ok" :type "checkbox" :label "Confirm"}]]

        (testing "the app dismisses the request and the terminal dialog goes away"
          (let [answer (ask! sid from-app fields)]
            (try (is (await-true #(tui-open? from-app)))
                 (is (true? (gw/cancel! from-app)))
                 (is (false? (:is-submitted (deref answer 2000 ::timeout))))
                 (is (await-true #(nil? (:human-input @state/app-db))))
                 (finally (engine/cancel! from-app "cleanup")))))
        (testing "Escape in the terminal dismisses it for the app too"
          (let [answer (ask! sid from-tui fields)]
            (try (is (await-true #(tui-open? from-tui)))
                 (press! (KeyStroke. KeyType/Escape))
                 (is (false? (:is-submitted (deref answer 2000 ::timeout))))
                 (is (await-true #(seq (events-of seen "human_input.close" from-tui))))
                 (is (empty? (gw/pending sid)))
                 (is (nil? (:human-input @state/app-db)))
                 (finally (engine/cancel! from-tui "cleanup")))))))))

(deftest a-second-parked-request-queues-behind-the-first-test
  (with-surfaces!
    (fn [seen]
      (let
        [sid
         (str (random-uuid))

         open-id
         (str "open-" (random-uuid))

         queued-id
         (str "queued-" (random-uuid))

         open-answer
         (ask! sid open-id [{:id "user" :type "plaintext" :label "User" :is-required true}])

         _
         (is (await-true #(tui-open? open-id)))

         queued-answer
         (ask! sid queued-id [{:id "why" :type "plaintext" :label "Why" :is-required true}])]

        (try (testing "the terminal shows one form at a time, the app is offered both"
               (is (await-true #(= 1 (count (:human-input-queue @state/app-db)))))
               (is (tui-open? open-id))
               (is (await-true #(= [open-id queued-id] (mapv :id (gw/pending sid)))))
               ;; The bridge publishes from another thread: poll, never race it.
               (is (await-true #(= 1 (count (events-of seen "human_input.request" queued-id))))))
             (testing "the app may answer the QUEUED one without disturbing the open dialog"
               (is (:is-accepted (gw/submit! queued-id {"why" "because"})))
               (is (true? (:is-submitted (deref queued-answer 2000 ::timeout))))
               (is (await-true #(empty? (:human-input-queue @state/app-db))))
               (is (tui-open? open-id))
               (is (= [open-id] (mapv :id (gw/pending sid)))))
             (testing "and the still-open one answers from the terminal as if nothing happened"
               (press! (stroke \o) (stroke \k) (KeyStroke. KeyType/Enter))
               (is (= "ok" (get-in (deref open-answer 2000 ::timeout) [:values "user"])))
               (is (await-true #(nil? (:human-input @state/app-db))))
               (is (empty? (gw/pending sid))))
             (finally (engine/cancel! open-id "cleanup") (engine/cancel! queued-id "cleanup")))))))

(deftest a-timeout-clears-the-form-from-both-surfaces-test
  (with-surfaces!
    (fn [seen]
      (let
        [sid
         (str (random-uuid))

         rid
         (str "expired-" (random-uuid))

         answer
         (future (engine/request! {:id rid
                                   :session-id sid
                                   :title "Deploy?"
                                   :timeout-ms 400
                                   :fields [{:id "user" :type "plaintext" :label "User"}]}))]

        (is (await-true #(tui-open? rid)))
        (testing "nobody answered: the extension resumes and no surface keeps a dead form"
          (is (= "timeout" (:reason (deref answer 3000 ::timeout))))
          (is (await-true #(nil? (:human-input @state/app-db))))
          (is (await-true #(seq (events-of seen "human_input.close" rid))))
          (is (= ["timeout"]
                 (mapv #(get (second %) "reason") (events-of seen "human_input.close" rid))))
          (is (empty? (gw/pending sid)))
          (is (nil? (gw/request-of sid rid))))))))

(deftest both-surfaces-answering-at-once-settles-the-run-once-test
  (with-surfaces!
    (fn [seen]
      (let
        [sid
         (str (random-uuid))

         rid
         (str "storm-" (random-uuid))

         answer
         (ask! sid rid [{:id "user" :type "plaintext" :label "User" :is-required true}])

         _
         (is (await-true #(and (tui-open? rid) (seq (gw/pending sid)))))

         gate
         (java.util.concurrent.CountDownLatch. 1)

         ;; Six phones and a terminal answer the same form in the same instant.
         racers
         (doall (conj (vec (for [i (range 6)]
                             (future (.await gate) (gw/submit! rid {"user" (str "app-" i)}))))
                      (future (.await gate) (press! (KeyStroke. KeyType/Escape)))))

         _
         (.countDown gate)

         _
         (run! deref racers)

         result
         (deref answer 5000 ::timeout)]

        (testing "the extension is released exactly once, by exactly one surface"
          (is (not= ::timeout result))
          (is (= rid (:request-id result)))
          (is (contains? #{"submitted" "cancelled"} (:reason result))))
        (testing "and both surfaces end with no form and no way to answer it again"
          (is (await-true #(= 1 (count (events-of seen "human_input.close" rid)))))
          (is (= 1 (count (events-of seen "human_input.request" rid))))
          (is (await-true #(nil? (:human-input @state/app-db))))
          (is (empty? (gw/pending sid)))
          (is (= {:is-accepted false :reason "unknown"} (gw/submit! rid {"user" "late"})))
          (is (false? (gw/cancel! rid))))))))

(deftest a-request-that-forbids-cancelling-forbids-it-on-both-surfaces-test
  (with-surfaces!
    (fn [seen]
      (let
        [sid
         (str (random-uuid))

         rid
         (str "must-answer-" (random-uuid))

         answer
         (future (engine/request!
                   {:id rid
                    :session-id sid
                    :title "Deploy?"
                    :is-cancellable false
                    :timeout-ms 5000
                    :fields [{:id "note" :type "plaintext" :label "Note" :is-required true}]}))]

        (try (is (await-true #(tui-open? rid)))
             (is (await-true #(seq (events-of seen "human_input.request" rid))))
             (testing "both surfaces are told the escape hatch is closed"
               (is (false? (:is-cancellable (gw/request-of sid rid))))
               (is (false? (get-in (second (first (events-of seen "human_input.request" rid)))
                                   ["request" "is_cancellable"])))
               (is (false? (get-in @state/app-db [:human-input :request :is-cancellable]))))
             (testing "Escape in the terminal does not dismiss it"
               (press! (KeyStroke. KeyType/Escape))
               (is (tui-open? rid))
               (is (not (realized? answer)))
               (is (empty? (events-of seen "human_input.close" rid))))
             (testing "and the app is refused the same way"
               (is (false? (gw/cancel! rid)))
               (is (some? (gw/request-of sid rid)))
               (is (not (realized? answer)))
               (is (tui-open? rid)))
             (testing "answering is the only way out, and it clears both surfaces"
               (press! (stroke \o) (stroke \k) (KeyStroke. KeyType/Enter))
               (is (= "ok" (get-in (deref answer 2000 ::timeout) [:values "note"])))
               (is (await-true #(= 1 (count (events-of seen "human_input.close" rid)))))
               (is (= "submitted"
                      (get (second (first (events-of seen "human_input.close" rid))) "reason")))
               (is (await-true #(nil? (:human-input @state/app-db))))
               (is (empty? (gw/pending sid))))
             (finally (engine/cancel-all! "cleanup") (future-cancel answer)))))))

;; A one-time code and a field's rules are ENGINE data. The app answers over
;; HTTP and the terminal answers with keystrokes, so the only way neither can
;; drift is that both are judged by the same sentence, from the same request.
(deftest an-otp-and-its-rules-hold-on-both-surfaces-test
  (with-surfaces!
    (fn [seen]
      (let
        [sid
         (str (random-uuid))

         rid
         (str "req-" (random-uuid))

         answer
         (ask! sid
               rid
               [{:id "code" :type "otp" :label "One-time code" :is-required true :max-length 4}
                {:id "notify"
                 :type "plaintext"
                 :label "Notify"
                 :default "ops@example.com"
                 :validate :email}])]

        (try (is (await-true #(tui-open? rid)))
             (is (await-true #(seq (events-of seen "human_input.request" rid))))
             (testing "the rules reach the app as DATA, so it can refuse before it sends"
               (let
                 [fields
                  (get-in (second (first (events-of seen "human_input.request" rid)))
                          ["request" "fields"])

                  by-id
                  (into {} (map (juxt #(get % "id") identity)) fields)]

                 (is (= "otp" (get-in by-id ["code" "type"])))
                 ;; One `max_length` and no `min_length` means a FIXED length:
                 ;; four boxes, and four digits is the only answer that fits.
                 (is (= 4 (get-in by-id ["code" "min_length"])))
                 (is (= 4 (get-in by-id ["code" "max_length"])))
                 (is (= [{"kind" "type" "type" "email" "message" "must be an email address"}]
                        (get-in by-id ["notify" "validate"])))))
             (testing "the app's bad answer is refused field by field"
               (let [outcome (gw/submit! rid {"code" "12ab" "notify" "nope"})]
                 (is (false? (:is-accepted outcome)))
                 (is (= {"code" "must be digits only" "notify" "must be an email address"}
                        (:errors outcome)))
                 (is (some? (gw/request-of sid rid)))))
             (testing "digits typed into the terminal fill the boxes and release the run"
               ;; The band drops everything that is not a digit, so a fat-fingered
               ;; letter never even reaches the engine.
               (press! (stroke \1)
                       (stroke \x)
                       (stroke \2)
                       (stroke \3)
                       (stroke \4)
                       (KeyStroke. KeyType/Enter))
               (let [result (deref answer 2000 ::timeout)]
                 (is (true? (:is-submitted result)))
                 (is (= "1234" (get-in result [:values "code"])))
                 (is (= "ops@example.com" (get-in result [:values "notify"])))))
             (finally (engine/cancel! rid "cleanup")))))))
