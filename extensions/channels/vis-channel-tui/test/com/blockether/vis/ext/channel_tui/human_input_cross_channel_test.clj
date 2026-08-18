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
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.chat :as chat]
            [com.blockether.vis.ext.channel-tui.dialogs :as dialogs]
            [com.blockether.vis.ext.channel-tui.human-input :as hi]
            [com.blockether.vis.ext.channel-tui.state :as state]
            [com.blockether.vis.ext.channel-tui.screen :as screen]
            [com.blockether.vis.internal.gateway.human-input :as gw]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [com.blockether.vis.internal.gateway.state :as gw-state]
            [com.blockether.vis.internal.human-input :as engine]
            [com.blockether.vis.internal.human-input.spec :as hi-spec]
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
  (let [seen
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

(defn- attach!
  "Point the terminal at `sid`.

   A form is shown on the tab whose session it parks and on no other, so a
   surface test is attached to that session before anything asks."
  [sid]
  (swap! state/app-db assoc :session {:id sid}))

(defn- ask!
  "Park an extension on a typed request, exactly like an extension would."
  [sid request-id fields]
  (attach! sid)
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
      (let [sid
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
               (let [[event-sid event]
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
      (let [sid
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
      (let [sid
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
               (testing "the TUI's blank answer is refused by the SAME engine"
                 ;; The band keeps no rules of its own: Enter SENDS, the engine
                 ;; refuses, and the message printed under the field is the very
                 ;; one the app got back.
                 (press! (KeyStroke. KeyType/Enter))
                 (is (await-true #(seq (:errors (:human-input @state/app-db)))))
                 (is (= (:errors app-outcome) (:errors (:human-input @state/app-db))))
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
      (let [sid
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
      (let [sid
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
      (let [sid
            (str (random-uuid))

            rid
            (str "expired-" (random-uuid))

            answer
            (do (attach! sid)
                (future (engine/request! {:id rid
                                          :session-id sid
                                          :title "Deploy?"
                                          :timeout-ms 400
                                          :fields
                                          [{:id "user" :type "plaintext" :label "User"}]})))]

        (is (await-true #(tui-open? rid)))
        (testing "nobody answered: the extension resumes and no surface keeps a dead form"
          (is (= "timeout" (:reason (deref answer 3000 ::timeout))))
          (is (await-true #(nil? (:human-input @state/app-db))))
          (is (await-true #(seq (events-of seen "human_input.close" rid))))
          (is (= ["timeout"]
                 (mapv #(get (second %) "reason") (events-of seen "human_input.close" rid))))
          (is (empty? (gw/pending sid)))
          (is (nil? (gw/request-of sid rid))))))))

;; The twin of the test above: `:timeout-ms 0` is the extension that must NOT
;; guess. No clock may take this dialog off either surface.
(deftest an-indefinite-request-parks-on-both-surfaces-test
  (with-surfaces!
    (fn [seen]
      (let [sid
            (str (random-uuid))

            rid
            (str "forever-" (random-uuid))

            answer
            (do (attach! sid)
                (future (engine/request! {:id rid
                                          :session-id sid
                                          :title "Deploy?"
                                          :timeout-ms 0
                                          :fields
                                          [{:id "user" :type "plaintext" :label "User"}]})))]

        (try (is (await-true #(tui-open? rid)))
             (is (await-true #(seq (gw/pending sid))))
             (testing "long past the deadline a defaulted ask would have had, nobody gave up"
               (Thread/sleep 700)
               (is (= ::timeout (deref answer 1 ::timeout)))
               (is (tui-open? rid))
               (is (some? (gw/request-of sid rid)))
               (is (empty? (events-of seen "human_input.close" rid))))
             (testing "and the operator who finally shows up is still heard, from the terminal"
               (press! (stroke \o) (stroke \k) (KeyStroke. KeyType/Enter))
               (let [result (deref answer 3000 ::timeout)]
                 (is (= "submitted" (:reason result)))
                 (is (= "ok" (get-in result [:values "user"]))))
               (is (await-true #(nil? (:human-input @state/app-db))))
               (is (await-true #(empty? (gw/pending sid)))))
             (finally (engine/cancel! rid "cleanup")))))))

(deftest both-surfaces-answering-at-once-settles-the-run-once-test
  (with-surfaces!
    (fn [seen]
      (let [sid
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
      (let [sid
            (str (random-uuid))

            rid
            (str "must-answer-" (random-uuid))

            answer
            (do (attach! sid)
                (future (engine/request!
                          {:id rid
                           :session-id sid
                           :title "Deploy?"
                           :is-cancellable false
                           :timeout-ms 5000
                           :fields
                           [{:id "note" :type "plaintext" :label "Note" :is-required true}]})))]

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
      (let [sid
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
                    :validate (fn [value]
                                (when-not (re-find #"@" value) "must be an email address"))}])]

        (try (is (await-true #(tui-open? rid)))
             (is (await-true #(seq (events-of seen "human_input.request" rid))))
             (testing "not one validator crosses the wire"
               (let [fields
                     (get-in (second (first (events-of seen "human_input.request" rid)))
                             ["request" "fields"])

                     by-id
                     (into {} (map (juxt #(get % "id") identity)) fields)]

                 (is (= "otp" (get-in by-id ["code" "type"])))
                 ;; One `max_length` and no `min_length` means a FIXED length:
                 ;; four boxes, and four digits is the only answer that fits.
                 (is (= 4 (get-in by-id ["code" "min_length"])))
                 (is (= 4 (get-in by-id ["code" "max_length"])))
                 ;; A validator is a FUNCTION that runs in the engine, once, on a
                 ;; confirmation. There is nothing to serialize, so no surface
                 ;; can hold a second copy of the rules and drift from it.
                 (is (not (contains? (get by-id "notify") "validate")))))
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
                 ;; Issue #128: a one-time code is a credential, so the answer
                 ;; carries a vault HANDLE and only the extension can read it.
                 (is (not= "1234" (get-in result [:values "code"])))
                 (is (= "1234" (engine/reveal-secret (get-in result [:values "code"]))))
                 (is (= "ops@example.com" (get-in result [:values "notify"])))))
             (finally (engine/cancel! rid "cleanup")))))))

;; One vocabulary, three surfaces
;;
;; The terminal reads the engine's own tables (`human-input.spec`) at runtime,
;; so it cannot drift. The APP cannot: TypeScript needs its literals at compile
;; time, so `apps/vis-companion/src/lib/human-input.ts` declares the same closed
;; vocabulary a second time. That copy is checked HERE, against the engine's
;; tables and the terminal's own glyphs — a type the engine gains and the app
;; never learns fails this test instead of shipping a form the phone renders as
;; a hole.

(defn- repo-file
  "A path under the repository root, found from the working directory upwards so
   the test runs from this extension or from the root project."
  [rel]
  (loop [dir (.getCanonicalFile (io/file (System/getProperty "user.dir")))]
    (when dir
      (let [f (io/file dir rel)]
        (if (.isFile f) f (recur (.getParentFile dir)))))))

(defn- app-source
  "One companion source file as text, or nil when it cannot be found."
  [rel]
  (some-> (repo-file (str "apps/vis-companion/src/" rel))
          slurp))

(defn- ts-strings
  "The string literals of `export const NAME = [ ... ]` in a TypeScript source."
  [source const-name]
  (some->> (re-find (re-pattern (str "(?s)export const " const-name " = \\[(.*?)\\]")) source)
           second
           (re-seq #"'([^']*)'")
           (mapv second)))

(defn- ts-numbers
  "The `key: number` entries of `export const NAME = { ... }`, as a map."
  [source const-name]
  (some->> (re-find (re-pattern (str "(?s)export const " const-name " = \\{(.*?)\\}")) source)
           second
           (re-seq #"(\w+):\s*(-?[\d.]+)")
           (into {}
                 (map (fn [[_ k v]]
                        [(keyword k) (parse-long v)])))))

(defn- ts-marks
  "The `key: 'glyph'` entries of `export const NAME = { ... }`, as a map."
  [source const-name]
  (some->> (re-find (re-pattern (str "(?s)export const " const-name " = \\{(.*?)\\}")) source)
           second
           (re-seq #"(\w+):\s*'([^']*)'")
           (into {}
                 (map (fn [[_ k v]]
                        [(keyword k) v])))))

(defn- ts-literal
  "The string of `export const NAME = '…';` in a TypeScript source."
  [source const-name]
  (second (re-find (re-pattern (str "export const " const-name " = '([^']*)'")) source)))

(defn- ts-number
  "The number of `export const NAME = 123;` in a TypeScript source."
  [source const-name]
  (some-> (re-find (re-pattern (str "export const " const-name " = (-?\\d+)")) source)
          second
          parse-long))
(deftest the-app-declares-the-engines-own-node-vocabulary-test
  (let [source (app-source "lib/human-input.ts")]
    (is (some? source))
    (when source
      (testing "every answerable field type, and not one the engine cannot parse"
        (is (= (set (keys hi-spec/field-types))
               (set (ts-strings source "HUMAN_INPUT_FIELD_TYPES")))))
      (testing "ink is ink on both surfaces"
        (is (= (set (keys hi-spec/decor-types))
               (set (ts-strings source "HUMAN_INPUT_DECOR_TYPES")))))
      (testing "and the layout group is the one node that is neither"
        (is (contains? (set (ts-strings source "HUMAN_INPUT_NODE_TYPES")) hi-spec/group-type-name)))
      (testing "a group runs in the directions the engine accepts"
        (is (= (set (keys hi-spec/group-directions))
               (set (map second
                         (re-seq #"'(row|column)'"
                                 (or (second (re-find #"direction\?: ([^;]+);" source)) "")))))))
      (testing "an unbounded slider is the same percentage, and a code the same width"
        (is (= hi-spec/range-defaults (ts-numbers source "HUMAN_INPUT_RANGE_DEFAULTS")))
        (is (= hi-spec/otp-defaults (ts-numbers source "HUMAN_INPUT_OTP_DEFAULTS")))))))

(deftest both-surfaces-mark-a-choice-the-same-way-test
  (let [source
        (app-source "lib/human-input.ts")

        sheet
        (app-source "components/HumanInputPrompt.tsx")]

    (is (some? source))
    (is (some? sheet))
    (when (and source sheet)
      (testing "`choose one` and `choose any` look alike on neither surface"
        (is (= {:exclusiveOn (str/trim (dialogs/choice-mark true true))
                :exclusiveOff (str/trim (dialogs/choice-mark true false))
                :inclusiveOn (str/trim (dialogs/choice-mark false true))
                :inclusiveOff (str/trimr (dialogs/choice-mark false false))}
               (ts-marks source "HUMAN_INPUT_CHOICE_MARKS"))))
      (testing "and the sheet paints them from that table rather than its own"
        (is (not (str/includes? sheet "'[x]'")))
        (is (str/includes? sheet "HUMAN_INPUT_CHOICE_MARKS"))))))

;; A request parked in the SERVE DAEMON (issue #122)
;;
;; `internal/human-input` publishes on the IN-PROCESS channel bus. That bus does
;; not cross a JVM, so when the extension parks inside `vis-agent serve` the terminal
;; is a different process entirely and the `:tui` publication reaches nobody.
;; The gateway bridge above still turns the request into a `human_input.request`
;; SESSION event — which the TUI simply dropped: `chat/gateway-event->chunk`,
;; the ONE projection of that stream into the terminal, had no case for it. The
;; operator watched a turn that never moved, and `publish!` counting the gateway
;; listener meant the engine's "undeliverable" escape hatch never fired either.

(defn- daemon-view
  "A request VIEW built the way the engine builds one, without parking a thread."
  ([rid] (daemon-view rid "s1"))
  ([rid sid]
   (engine/request->view (engine/normalize-request
                           {:id rid
                            :session-id sid
                            :title "Deploy?"
                            :fields [{:id "note" :type "plaintext" :label "Note"}]}))))

;; Regression, issue #122: a run parked by an extension running in the serve
;; daemon never surfaced in the terminal at all — the persisted
;; `human_input.request` / `human_input.close` session events projected to nil,
;; so the tab sat on a turn that never moved and no dialog was ever drawn.
(deftest a-daemon-side-request-reaches-the-terminal-test
  (with-surfaces!
    (fn [seen]
      (let [sid
            (str (random-uuid))

            rid
            (str "req-" (random-uuid))

            answer
            (ask! sid
                  rid
                  [{:id "note" :type "plaintext" :label "Note" :is-required true}
                   {:id "env"
                    :type "select"
                    :label "Env"
                    :options [{:value "prod" :label "prod"} {:value "dev" :label "dev"}]}])]

        (try (is (await-true #(seq (events-of seen "human_input.request" rid))))
             (testing "the session event PROJECTS — the only route out of the daemon"
               (let [[_ event]
                     (first (events-of seen "human_input.request" rid))

                     chunk
                     (#'chat/gateway-event->chunk event)]

                 (is (= :human-input-open (:phase chunk)))
                 (is (= rid (get-in chunk [:request "id"])))
                 (testing "and rehydrates the ENGINE's own view, not a second field table"
                   (let [view (hi/request<-wire (:request chunk))]
                     (is (= sid (hi/session-id {:request view})))
                     ;; Byte-for-byte the view the in-process `:tui` channel
                     ;; event carried, so the same dialog is drawn either way.
                     (is (= (get-in @state/app-db [:human-input :request]) view))
                     (is (= (:human-input @state/app-db) (hi/init-form view)))))))
             (testing "the APP answering it closes every terminal's form too"
               (is (= {:is-accepted true} (gw/submit! rid {"note" "ship it" "env" "prod"})))
               (is (true? (:is-submitted (deref answer 2000 ::timeout))))
               (is (await-true #(seq (events-of seen "human_input.close" rid))))
               (let [[_ event]
                     (first (events-of seen "human_input.close" rid))

                     chunk
                     (#'chat/gateway-event->chunk event)]

                 (is (= :human-input-close (:phase chunk)))
                 (is (= rid (:request-id chunk)))
                 (is (= "submitted" (:reason chunk)))))
             (finally (engine/cancel! rid "cleanup")))))))

;; Regression, issue #122: with the daemon route wired, a request parked in THIS
;; process arrives twice — once on the in-process `:tui` channel bus and once as
;; the gateway's own session event — and the second arrival queued a duplicate
;; dialog behind the first, leaving a zombie form open after the answer.
(deftest a-request-that-arrives-on-both-routes-opens-one-dialog-test
  (reset! state/app-db {:render-version 0 :session {:id "s1"}})
  (try (let [form (hi/init-form (daemon-view "req-dup"))]
         (state/dispatch [:human-input-open form])
         (state/dispatch [:human-input-open form])
         (is (= form (:human-input @state/app-db)))
         (is (empty? (:human-input-queue @state/app-db)))
         (testing "a DIFFERENT request still queues behind it"
           (let [other (hi/init-form (daemon-view "req-other"))]
             (state/dispatch [:human-input-open other])
             (is (= [other] (vec (:human-input-queue @state/app-db))))
             (testing "and answering the first promotes exactly that one"
               (state/dispatch [:human-input-close "req-dup"])
               (is (= "req-other" (get-in @state/app-db [:human-input :request :id])))
               (is (empty? (:human-input-queue @state/app-db)))))))
       (finally (reset! state/app-db {:render-version 0}))))

;; Regression, issue #122: the terminal answered every request through the
;; in-process registry, so a request parked in the daemon could not be answered
;; at all — its id resolves to nothing here, and the parked run stayed blocked
;; whatever the operator typed.
(deftest a-daemon-side-answer-goes-over-the-gateway-test
  (let [calls
        (atom [])

        form
        (hi/init-form (daemon-view "req-remote"))]

    (with-redefs [vis/gateway-submit-human-input!
                  (fn [sid rid values]
                    (swap! calls conj [:submit sid rid values])
                    {:is-accepted true})

                  vis/gateway-cancel-human-input!
                  (fn [sid rid]
                    (swap! calls conj [:cancel sid rid])
                    true)]

      (is (= {:is-accepted true} (#'screen/human-input-answer! form :submit {"note" "hi"})))
      (is (true? (#'screen/human-input-answer! form :cancel nil)))
      (is (= [[:submit "s1" "req-remote" {"note" "hi"}] [:cancel "s1" "req-remote"]] @calls)))))

;; Regression, issue #122: a session can be blocked on MORE than one request at
;; a time — several extensions parked in the daemon, or one that asks again
;; while an earlier form is still unanswered. A tab that attaches late saw NONE
;; of them; it must now surface EVERY one, in the daemon's own order, and every
;; answer must name its own request over the gateway that owns it.
(deftest every-daemon-side-request-is-replayed-on-attach-test
  (reset! state/app-db {:render-version 0})
  (let [sid
        (str "s-" (random-uuid))

        rids
        ["replay-one" "replay-two" "replay-three"]

        calls
        (atom [])]

    ;; The tab is attached to `sid` — a replayed form belongs to that session.
    (swap! state/app-db assoc :session {:id sid})
    (try
      (with-redefs [vis/gateway-human-input-requests
                    (fn [asked]
                      (is (= sid asked))
                      (mapv #(wire/->wire (daemon-view % sid)) rids))

                    vis/gateway-submit-human-input!
                    (fn [answered-sid rid values]
                      (swap! calls conj [answered-sid rid values])
                      {:is-accepted true})]

        (#'screen/replay-human-input! sid)
        (testing "all of them, oldest first: one open dialog and the rest queued"
          (is (= "replay-one" (get-in @state/app-db [:human-input :request :id])))
          (is (= ["replay-two" "replay-three"]
                 (mapv #(get-in % [:request :id]) (:human-input-queue @state/app-db)))))
        (testing "attaching twice does not stack a second copy of any of them"
          (#'screen/replay-human-input! sid)
          (is (= "replay-one" (get-in @state/app-db [:human-input :request :id])))
          (is (= 2 (count (:human-input-queue @state/app-db)))))
        (testing "and each one is answered over the gateway, against its OWN id"
          (doseq [rid rids]
            (let [form (:human-input @state/app-db)]
              (is (= rid (hi/request-id form)))
              (is (= {:is-accepted true} (#'screen/human-input-answer! form :submit {"note" rid})))
              (state/dispatch [:human-input-close rid])))
          (is (= (mapv (fn [rid]
                         [sid rid {"note" rid}])
                       rids)
                 @calls))
          (is (nil? (:human-input @state/app-db)))
          (is (empty? (:human-input-queue @state/app-db)))))
      (finally (reset! state/app-db {:render-version 0})))))


;; A live view crosses the same border and hits the same compile-time wall:
;; `apps/vis-companion/src/lib/live-view.ts` declares the engine's closed tables a
;; second time so the phone can FOLD patches itself. A node type the engine gains
;; and the app never learns fails here, instead of shipping a card the phone paints
;; as a hole while a run reports into it.
(deftest the-app-declares-the-engines-own-live-vocabulary-test
  (let [source (app-source "lib/live-view.ts")]
    (is (some? source))
    (when source
      (testing "what a view can be MADE of, and what one patch can do to it"
        (is (= (set (keys hi-spec/live-node-types)) (set (ts-strings source "LIVE_NODE_TYPES"))))
        (is (= (set (keys hi-spec/live-ops)) (set (ts-strings source "LIVE_OPS")))))
      (testing "and the layout it borrows from the form, spelled the same on both surfaces"
        (is (= hi-spec/group-type-name (ts-literal source "LIVE_GROUP_TYPE"))
            "a view arranges with the FORM's own group, never a second word for the same thing")
        (is (= (set (keys hi-spec/group-directions))
               (set (ts-strings source "LIVE_GROUP_DIRECTIONS")))))
      (testing "how a surface colours a line"
        (is (= (set (keys hi-spec/live-tones)) (set (ts-strings source "LIVE_TONES")))))
      (testing "what a link points at, and how a table is ordered under it"
        (is (= (set (keys hi-spec/link-targets)) (set (ts-strings source "LIVE_LINK_TARGETS"))))
        (is (= (set (keys hi-spec/live-orders)) (set (ts-strings source "LIVE_ORDERS"))))
        (is (= (set (keys hi-spec/live-aligns)) (set (ts-strings source "LIVE_ALIGNS"))))
        (is (= (set (keys hi-spec/live-sort-dirs)) (set (ts-strings source "LIVE_SORT_DIRS")))))
      (testing "the bounds the phone holds a node to are the engine's own"
        (is (= (:window-lines hi-spec/log-defaults) (ts-number source "LIVE_LOG_WINDOW")))
        (is (= (:max-rows hi-spec/table-defaults) (ts-number source "LIVE_TABLE_MAX_ROWS")))
        (is (= hi-spec/note-chars (ts-number source "LIVE_NOTE_CHARS"))
            "the field the phone types a stop note into ends where the engine cuts it"))
      (testing "and it listens for the three events the bridge actually publishes"
        (is (= [gw/live-open-event gw/live-patch-event gw/live-close-event]
               (mapv (partial ts-literal source)
                     ["LIVE_VIEW_OPEN_EVENT" "LIVE_VIEW_PATCH_EVENT" "LIVE_VIEW_CLOSE_EVENT"])))))))
