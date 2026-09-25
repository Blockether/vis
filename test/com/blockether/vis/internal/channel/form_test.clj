(ns com.blockether.vis.internal.channel.form-test
  "Guard: the canonical per-form DISPLAY set survives the gateway round-trip.

   This is the regression net for the whole class of bug that motivated
   `internal/form.clj` — a boundary (the gateway `block.output` payload, the
   client projection) silently dropping a display field so the live channel lost
   the card / badge while persisted ones kept it. If a NEW key is added to
   `form/display-keys` but a boundary stops carrying it, `survives-the-gateway`
   fails — no more chasing it through tmux."
  (:require [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.channel.form :as form]
            [com.blockether.vis.internal.gateway.state :as gw]
            [com.blockether.vis.contract.wire :as wire]
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

(defdescribe
  clip-to-wire-test
  (it "keeps short output verbatim and returns nil for a blank body"
      (expect (= "printed" (form/clip-to-wire "printed  \n")))
      (expect (nil? (form/clip-to-wire "   \n")))
      (expect (nil? (form/clip-to-wire nil))))
  (it "includes the recovery address inside the token budget"
      (let [f
            {:scope "t1/i2" :svar/tool-call-id "call_A" :llm-model "glm-5.3"}

            body
            (str (apply str (repeat 6000 "source path: \"quoted\"\n")) "LAST")

            projected
            (form/clip-to-wire body f)]

        (expect (str/starts-with? projected "source"))
        (expect (str/ends-with? projected "LAST"))
        (expect (str/includes? projected "kept "))
        (expect (str/includes? projected "r = await read_session()"))
        (expect (str/includes? projected "b.get(\"scope\") == \"t1/i2\""))
        (expect (<= (svar/count-tokens "glm-5.3" projected) form/MAX_FORM_OUTPUT_TOKENS))))
  (it "uses the persisted iteration scope for a live /fN result"
      (let [body
            (apply str (repeat 6000 "printed token "))

            live
            {:scope "t1/i2/f1" :svar/tool-call-id "call_A" :llm-model "glm-5.3"}

            projected
            (form/clip-to-wire body live)]

        (expect (str/includes? projected "b.get(\"scope\") == \"t1/i2\""))
        (expect (not (str/includes? projected "b.get(\"scope\") == \"t1/i2/f1\"")))))
  (it "does not let an oversized call id exceed the whole token ceiling"
      (let [body
            (apply str (repeat 6000 "printed token "))

            call-id
            (apply str (repeat 10000 "q_"))

            projected
            (form/clip-to-wire body
                               {:scope "t1/i2" :svar/tool-call-id call-id :llm-model "glm-5.3"})]

        (expect (<= (svar/count-tokens "glm-5.3" projected) form/MAX_FORM_OUTPUT_TOKENS))
        (expect (not (str/includes? projected "read_session()")))))
  (it "bounds oversized artifact fences before rendering them as a card"
      (let [f
            {:scope "t1/i2"
             :svar/tool-call-id "call_A"
             :llm-model "glm-5.3"
             :stdout (str "````vis-doc\n" (apply str (repeat 30000 "content\n")) "````")}

            body
            (:body (form/stdout-display f))]

        (expect (str/includes? body "stdout clipped"))
        (expect (not (str/starts-with? body "````vis-doc")))
        (expect (<= (svar/count-tokens "glm-5.3" (form/clip-to-wire (:stdout f) f))
                    form/MAX_FORM_OUTPUT_TOKENS)))))

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
        (expect (not (some #{:result :result-summary} form/display-keys)))
        ;; The gateway carried, and <-wire recovered, EVERY canonical display key.
        (doseq [k form/display-keys]
          (expect
            (some? (get back k))
            (str k " was dropped on the gateway round-trip — add it to a boundary projection")))))
  (it "result-card derives one body from stdout, and none from metadata alone"
      ;; A block's own printed output becomes exactly one card carrying no invented op or headline.
      (let [one (form/result-card {:stdout "printed"})]
        (expect (some? one))
        (expect (nil? (:op one)))
        (expect (not (contains? one :summary)))
        (expect (str/includes? (:body one) "printed")))
      ;; Neither operation metadata nor the retired summary field can manufacture output.
      (expect (nil? (form/result-card {:op "unheard_of" :result-summary "1 row"})))
      (expect (nil? (form/result-card {}))))
  (it "->display projects only declared form fields"
      (expect (= {:op "grep"} (form/->display {:op "grep" :internal true})))))

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

;; The display is always derived from canonical facts. No rendered copy is stored,
;; transported, or accepted as authored input, so live and restored forms use the
;; same projection.
(defdescribe stdout-display-derivation-test
             (it "derives a printed block's body from stdout"
                 (expect (str/includes? (:body (form/stdout-display {:src "print(1)"
                                                                     :stdout "hello"}))
                                        "hello")))
             (it "has nothing to show for a form that printed nothing"
                 (expect (nil? (form/stdout-display {:src "x = 1"}))))
             (it "passes a vis-image fence through verbatim so the channel paints it inline"
                 (expect (= "````vis-image\nx\n````"
                            (:body (form/stdout-display {:stdout "````vis-image\nx\n````"})))))
             ;; Regression: printed output was captured on the side and painted as its own
             ;; card, replacing stdout, so one block that printed twice read as two cards.
             (it "keeps the whole stdout as one body, however many lines the block printed"
                 (let [body (:body (form/stdout-display {:stdout "first\nsecond"}))]
                   (expect (str/includes? body "first"))
                   (expect (str/includes? body "second"))))
             ;; Regression: a timed-out block used to paint a second card under its error line,
             ;; whose body re-showed the form, stdout and the same timeout message.
             (it "gives a wall-clock timeout no card of its own"
                 (expect (nil? (form/stdout-display {:src "time.sleep(99)"
                                                     :timeout? true
                                                     :error {:message "Timeout (30s)"
                                                             :type :vis/eval-timeout
                                                             :data {:timeout-ms 30000}}}))))
             (it "keeps what a timed-out block printed as its ordinary stdout body"
                 (let [card (form/stdout-display {:src "time.sleep(99)"
                                                  :timeout? true
                                                  :stdout "partial"
                                                  :error {:message "Timeout (30s)"
                                                          :data {:timeout-ms 30000}}})]
                   (expect (nil? (:summary card)))
                   (expect (str/includes? (:body card) "partial"))
                   (expect (not (str/includes? (:body card) "time.sleep(99)"))))))

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
