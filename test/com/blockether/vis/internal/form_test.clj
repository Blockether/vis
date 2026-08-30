(ns com.blockether.vis.internal.form-test
  "Guard: the canonical per-form DISPLAY set survives the gateway round-trip.

   This is the regression net for the whole class of bug that motivated
   `internal/form.clj` — a boundary (the gateway `block.output` payload, the
   client projection) silently dropping a display field so the live channel lost
   the card / badge while persisted ones kept it. If a NEW key is added to
   `form/display-keys` but a boundary stops carrying it, `survives-the-gateway`
   fails — no more chasing it through tmux."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.form :as form]
            [com.blockether.vis.internal.gateway.state :as gw]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private chunk->event @#'gw/chunk->event)

(defn- sentinel
  "A non-nil sentinel value per display key, shaped like the real field."
  [k]
  (case k
    :result-kind
    :tool

    :result-detail
    {:op :rg :hit-count 9}

    :render-segments
    [{:kind :code}]

    (:timeout? :repaired?)
    true

    ;; everything else: a distinctive string
    (str "sentinel-" (namespace k) "-" (name k))))

(defn- simulate-wire
  "The REAL wire hop: `wire/canonical` is by construction what a client holds
   after `parse-json` ∘ `json-str` — snake_case STRING keys, keyword values
   stringified, namespaces dropped. Using the real codec keeps this guard
   honest (a hand-rolled mimic drifted from the actual key munge)."
  [payload]
  (wire/canonical payload))

;; ONE form body, ONE ceiling. The same printed output reaches three surfaces — the
;; model's tool result, the card a channel paints, and the gateway's copy of it on
;; `block.output` — and each used to hand-roll its own cut at its own size.
(defdescribe
  clip-to-wire-test
  (it "keeps a body under the ceiling verbatim and answers nil for a blank one"
      (expect (= "printed" (form/clip-to-wire "printed  \n")))
      (expect (nil? (form/clip-to-wire "   \n")))
      (expect (nil? (form/clip-to-wire nil))))
  (it "announces what it dropped"
      (let [body
            (apply str (repeat (* 2 (long form/MAX_FORM_WIRE_CHARS)) "x"))

            clipped
            (form/clip-to-wire body)]

        (expect (str/includes?
                  clipped
                  (str "output clipped at " form/MAX_FORM_WIRE_CHARS "/" (count body) " chars")))
        (expect (< (count clipped) (count body)))))
  (it "carries the calling surface's own advice in the marker"
      (let [body (apply str (repeat (* 2 (long form/MAX_FORM_WIRE_CHARS)) "x"))]
        (expect (str/ends-with? (form/clip-to-wire body "narrow next time.")
                                " chars — narrow next time.")))))
(defdescribe
  form-gateway-roundtrip-test
  (it "every display key survives loop chunk -> gateway block.output -> wire -> <-wire"
      (let [chunk
            (into {:phase :form-result :iteration 1 :position 0}
                  (map (fn [k]
                         [k (sentinel k)]))
                  form/display-keys)

            [type _store payload]
            (chunk->event chunk)

            back
            (form/<-wire (simulate-wire payload))]

        (expect (= "block.output" type))
        ;; The gateway carried, and <-wire recovered, EVERY canonical display key.
        (doseq [k form/display-keys]
          (expect
            (some? (get back k))
            (str k " was dropped on the gateway round-trip — add it to a boundary projection")))))
  (it
    "result-card is the ONE projection: a card for a form that shows something, none for one that shows nothing"
    ;; a block's own printed output → exactly one card, carrying no op: a card
    ;; descriptor mints no display NAME of its own.
    (let [one (form/result-card {:result-render "```\nprinted\n```"})]
      (expect (some? one))
      (expect (nil? (:op one))))
    ;; the op is carried VERBATIM — an op nothing registered still reaches a channel.
    (expect (= "unheard_of" (:op (form/result-card {:op "unheard_of" :result-summary "1 row"}))))
    ;; a block that printed nothing and returned nothing → no card at all.
    (expect (nil? (form/result-card {:result {:k 1}}))))
  (it "->display drops nils so a merge never stamps empty keys"
      (expect (= {} (form/->display {:result nil :op nil})))
      (expect (= {:op "grep"} (form/->display {:op "grep" :result-render nil})))))

(defdescribe form-authored-display-code-test
             (it "keeps an authored display instead of re-deriving it from the source"
                 ;; A form that already carries the surface a channel must paint keeps it;
                 ;; `with-display` must not overwrite it with its own formatting.
                 (let [form (form/with-display {:code "shell({\"commands\": [\"sleep 30\"]})"
                                                :display-code "sleep 30"
                                                :display-language "bash"})]
                   (expect (= "sleep 30" (:display-code form)))
                   (expect (= "bash" (:display-language form)))
                   ;; the raw invocation is still carried for the model-facing surfaces
                   (expect (= "shell({\"commands\": [\"sleep 30\"]})" (:code form)))))
             (it "still derives the display for a form that authored none"
                 (let [form (form/with-display {:code "x=1"})]
                   (expect (seq (:display-code form)))
                   (expect (nil? (:display-language form))))))

;; Regression: `:result-render` used to be STORED on every persisted form — the
;; rendered body sat in the store right next to the `:result`/`:stdout` it is a
;; pure projection of, 32% of the whole `tool_calls` blob. A restored envelope
;; now carries no render at all, so these derivations ARE the display: if one
;; drifts, a reopened session paints a different card than the live stream did.
(defdescribe
  result-render-derivation-test
  (it "derives a native result's body from the value alone"
      (let [body (form/result-render {:src "grep({})" :result {"files" ["a.clj"]}})]
        (expect (some? body))
        (expect (str/includes? body "files"))
        (expect (str/includes? body "a.clj"))))
  (it "derives a printed block's body from its stdout"
      (expect (str/includes? (form/result-render {:src "print(1)" :stdout "hello"}) "hello")))
  (it "has nothing to show for a form that returned and printed nothing"
      (expect (nil? (form/result-render {:src "x = 1"}))))
  (it "passes a vis-image fence through verbatim so the channel paints it inline"
      (expect (= "````vis-image\nx\n````" (form/result-render {:stdout "````vis-image\nx\n````"}))))
  ;; Regression: a printed tool result was captured on the side and painted as its
  ;; OWN card, replacing the stdout — so ONE block that printed twice read as TWO
  ;; results.
  (it "keeps the WHOLE stdout as the one body, however many results the block printed"
      (let [body (form/result-render {:stdout "first\nsecond"})]
        (expect (str/includes? body "first"))
        (expect (str/includes? body "second"))))
  ;; Regression: a timed-out block used to paint a second ⧖ card under its error
  ;; line, whose body re-showed the FORM already rendered above it, the STDOUT and
  ;; the very same timeout message — three sections saying what the error line had
  ;; already said. A timeout is an error like every other one.
  (it "gives a wall-clock timeout no card of its own"
      (expect (nil? (form/result-display {:src "time.sleep(99)"
                                          :timeout? true
                                          :error {:message "Timeout (30s)"
                                                  :type :vis/eval-timeout
                                                  :data {:timeout-ms 30000}}}))))
  (it "keeps what a timed-out block printed as its ordinary stdout body"
      (let [card (form/result-display {:src "time.sleep(99)"
                                       :timeout? true
                                       :stdout "partial"
                                       :error {:message "Timeout (30s)"
                                               :data {:timeout-ms 30000}}})]
        (expect (nil? (:summary card)))
        (expect (str/includes? (:body card) "partial"))
        (expect (not (str/includes? (:body card) "time.sleep(99)"))))))

(defdescribe with-display-derives-the-body-test
             (it "fills the body a restored envelope no longer carries"
                 (let [restored
                       {:src "grep({})" :code "grep({})" :result {"files" ["a.clj"]}}

                       form
                       (form/with-display restored)]

                   (expect (= (form/result-render restored) (:result-render form)))
                   (expect (str/includes? (:result-render form) "a.clj"))))
             (it "never overwrites a render no projection reproduces"
                 ;; A `!cmd` bubble: the body is the shell layer's own card markdown,
                 ;; not a rendering of `:result`, so it is authored and kept verbatim.
                 (let [form (form/with-display {:code "await shell({\"command\": \"ls\"})"
                                                :result {"ok" true}
                                                :result-render "**SHELL**\nls"})]
                   (expect (= "**SHELL**\nls" (:result-render form))))))

(defdescribe
  envelope-duration-test
  ;; The loop, the CLI trace, the progress projection, the ctx envelope and a
  ;; DB-restored transcript each carried a private copy of this arithmetic.
  (it "reads the pair the envelope already carries"
      (expect (= 250 (form/envelope-duration-ms {:started-at-ms 1000 :finished-at-ms 1250}))))
  (it "answers nil for an envelope that was never timed, not zero"
      (expect (nil? (form/envelope-duration-ms {:started-at-ms 1000})))
      (expect (nil? (form/envelope-duration-ms nil))))
  (it "never answers a negative duration for a clock that went backwards"
      (expect (= 0 (form/envelope-duration-ms {:started-at-ms 1250 :finished-at-ms 1000})))))
