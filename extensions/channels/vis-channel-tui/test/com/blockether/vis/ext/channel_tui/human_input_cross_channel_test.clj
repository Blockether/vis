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
                 ;; drift into accepting what the other refuses.
                 (press! (KeyStroke. KeyType/Enter))
                 (is (= (:errors app-outcome) (get-in @state/app-db [:human-input :errors])))
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
