(ns com.blockether.vis.internal.form-test
  "Guard: the canonical per-form DISPLAY set survives the gateway round-trip.

   This is the regression net for the whole class of bug that motivated
   `internal/form.clj` — a boundary (the gateway `block.output` payload, the
   client projection) silently dropping a display field so the live channel lost
   the card / badge while persisted ones kept it. If a NEW key is added to
   `form/display-keys` but a boundary stops carrying it, `survives-the-gateway`
   fails — no more chasing it through tmux."
  (:require [com.blockether.vis.internal.form :as form]
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
    ;; print-many: each :cards mini-form → its own card descriptor, in order, titled
    ;; by the op the PRINTED value itself carried.
    (let
      [multi (form/result-cards {:cards
                                 [{:op "cat" :result-summary "read 3 lines" :result-render "x"}
                                  {:op "grep" :result-summary "12 results" :result-render "y"}]})]
      (expect (= 2 (count multi)))
      (expect (= ["CAT" "GREP"] (mapv :label multi))))
    ;; a block's own printed output (no :cards) → exactly one card, titled RESULT.
    (let [one (form/result-cards {:result-render "```\nprinted\n```"})]
      (expect (= 1 (count one)))
      (expect (= "RESULT" (:label (first one)))))
    ;; an op nothing registered still titles itself — the badge is the value's data.
    (expect (= "UNHEARD_OF" (:label (form/result-card {:op "unheard_of" :result-summary "1 row"}))))
    ;; a block that printed nothing and returned nothing → no card at all.
    (expect (= [] (form/result-cards {:result {:k 1}}))))
  (it "falls back to the PENDING card while a block is still running"
      ;; A running block has no result yet, but it is the SAME card: it wears the
      ;; pending headline AND the pending body instead of collapsing to a bare band.
      (let
        [running (form/result-card {:pending-summary "$ npm test (running)"
                                    :pending-render "**COMMAND**\n```bash\nnpm test\n```"})]
        (expect (= "$ npm test (running)" (:summary running)))
        (expect (= "**COMMAND**\n```bash\nnpm test\n```" (:body running)))
        (expect (:collapsible? running)))
      ;; Summary-only pending card: nothing to fold under it yet.
      (let [bare (form/result-card {:pending-summary "◷ `dev` reading logs"})]
        (expect (nil? (:body bare)))
        (expect (not (:collapsible? bare))))
      ;; The moment the result lands it wins — a finished card never says "running".
      (let
        [done (form/result-card {:pending-summary "$ npm test (running)"
                                 :pending-render "**COMMAND**\n```bash\nnpm test\n```"
                                 :result-summary "exit 0 · 12 lines"
                                 :result-render "**OUTPUT**\n```\nok\n```"})]
        (expect (= "exit 0 · 12 lines" (:summary done)))
        (expect (= "**OUTPUT**\n```\nok\n```" (:body done)))))
  (it "->display drops nils so a merge never stamps empty keys"
      (expect (= {} (form/->display {:result nil :op nil})))
      (expect (= {:op "grep"} (form/->display {:op "grep" :result-render nil})))))

(defdescribe form-authored-display-code-test
             (it "keeps an authored display instead of re-deriving it from the source"
                 ;; A form that already carries the surface a channel must paint keeps it;
                 ;; `with-display-code` must not overwrite it with its own formatting.
                 (let
                   [form (form/with-display-code {:code "shell({\"commands\": [\"sleep 30\"]})"
                                                  :display-code "sleep 30"
                                                  :display-language "bash"})]
                   (expect (= "sleep 30" (:display-code form)))
                   (expect (= "bash" (:display-language form)))
                   ;; the raw invocation is still carried for the model-facing surfaces
                   (expect (= "shell({\"commands\": [\"sleep 30\"]})" (:code form)))))
             (it "still derives the display for a form that authored none"
                 (let [form (form/with-display-code {:code "x=1"})]
                   (expect (seq (:display-code form)))
                   (expect (nil? (:display-language form))))))
