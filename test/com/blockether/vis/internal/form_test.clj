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

    :cards
    [{:op "cat" :result-summary "read 3 lines" :result-render "```\nx\n```"}
     {:op "grep" :result-summary "5 results" :result-render "a.clj:1: x"}]

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
  form-gateway-roundtrip-test
  (it "every display key survives loop chunk -> gateway block.output -> wire -> <-wire"
      (let
        [chunk
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
          (expect (some? (get back k))
                  (str k
                       " was dropped on the gateway round-trip — add it to a boundary projection")))
        ;; Nested cards survive the same wire hop as the singular display fields.
        (let [cards (:cards back)]
          (expect (= 2 (count cards)))
          (expect (= "cat" (:op (first cards)))))))
  (it
    "result-cards is the ONE projection: N cards for a print-many block, 1 for its own output, none for neither"
    ;; print-many: each :cards mini-form → its own card descriptor, in order,
    ;; carrying the op the PRINTED value itself brought out of the sandbox.
    (let
      [multi (form/result-cards {:cards
                                 [{:op "cat" :result-summary "read 3 lines" :result-render "x"}
                                  {:op "grep" :result-summary "12 results" :result-render "y"}]})]
      (expect (= 2 (count multi)))
      (expect (= ["cat" "grep"] (mapv :op multi))))
    ;; a block's own printed output (no :cards) → exactly one card, carrying no op:
    ;; a card descriptor mints no display NAME of its own.
    (let [one (form/result-cards {:result-render "```\nprinted\n```"})]
      (expect (= 1 (count one)))
      (expect (nil? (:op (first one)))))
    ;; the op is carried VERBATIM — an op nothing registered still reaches a channel.
    (expect (= "unheard_of" (:op (form/result-card {:op "unheard_of" :result-summary "1 row"}))))
    ;; a block that printed nothing and returned nothing → no card at all.
    (expect (= [] (form/result-cards {:result {:k 1}}))))
  (it "->display drops nils so a merge never stamps empty keys"
      (expect (= {} (form/->display {:result nil :op nil})))
      (expect (= {:op "grep"} (form/->display {:op "grep" :result-render nil})))))

(defdescribe form-authored-display-code-test
             (it "keeps an authored display instead of re-deriving it from the source"
                 ;; A form that already carries the surface a channel must paint keeps it;
                 ;; `with-display` must not overwrite it with its own formatting.
                 (let
                   [form (form/with-display {:code "shell({\"commands\": [\"sleep 30\"]})"
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
  (it "suppresses the stdout body once a printed card carries one"
      (expect (nil? (form/result-render {:stdout "raw"
                                         :cards [{:op "grep"
                                                  :result-summary "3 files"
                                                  :result-render "```python\n[]\n```"}]}))))
  (it "keeps stdout when the printed cards are headline-only"
      (expect (str/includes? (form/result-render {:stdout "raw"
                                                  :cards [{:op "grep" :result-summary "3 files"}]})
                             "raw")))
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
      (let
        [card (form/result-display {:src "time.sleep(99)"
                                    :timeout? true
                                    :stdout "partial"
                                    :error {:message "Timeout (30s)" :data {:timeout-ms 30000}}})]
        (expect (nil? (:summary card)))
        (expect (str/includes? (:body card) "partial"))
        (expect (not (str/includes? (:body card) "time.sleep(99)"))))))

(defdescribe with-display-derives-the-body-test
             (it "fills the body a restored envelope no longer carries"
                 (let
                   [restored
                    {:src "grep({})" :code "grep({})" :result {"files" ["a.clj"]}}

                    form
                    (form/with-display restored)]

                   (expect (= (form/result-render restored) (:result-render form)))
                   (expect (str/includes? (:result-render form) "a.clj"))))
             (it "never overwrites a render no projection reproduces"
                 ;; A `!cmd` bubble: the body is the shell layer's own card markdown,
                 ;; not a rendering of `:result`, so it is authored and kept verbatim.
                 (let
                   [form (form/with-display {:code "await shell({\"command\": \"ls\"})"
                                             :result {"ok" true}
                                             :result-render "**SHELL**\nls"})]
                   (expect (= "**SHELL**\nls" (:result-render form))))))
