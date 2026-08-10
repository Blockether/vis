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
    [{:vis/tool-name "cat" :result-summary "read 3 lines" :result-render "```\nx\n```"}
     {:vis/tool-name "rg" :result-summary "5 hits in 1 file" :result-render "a.clj:1: x"}]

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
          (expect (= "cat" (:vis/tool-name (first cards)))))))
  (it
    "result-cards is the ONE projection: N cards for a print-many form, 1 for a native form, none for a non-tool"
    ;; print-many: each :cards mini-form → its own op-card descriptor, in order.
    (let
      [multi (form/result-cards
               {:vis/tool-name "python_execution"
                :cards [{:vis/tool-name "cat" :result-summary "read 3 lines" :result-render "x"}
                        {:vis/tool-name "rg" :result-summary "5 hits" :result-render "y"}]})]
      (expect (= 2 (count multi)))
      (expect (= ["CAT" "RG"] (mapv :label multi)))
      (expect (every? :tool? multi)))
    ;; single native form (no :cards) → exactly its own card.
    (let [one (form/result-cards {:vis/tool-name "rg" :result-summary "5 hits" :result-render "y"})]
      (expect (= 1 (count one)))
      (expect (= "RG" (:label (first one)))))
    ;; non-tool form → no card at all (its body stays channel-specific).
    (expect (= [] (form/result-cards {:result {:k 1}}))))
  (it "hide-tool-code? drops only redundant successful native invocations"
      ;; A successful native tool (cat/rg/patch/…) already renders as an op-card, so
      ;; its synthesized `name(args)` source is redundant chrome — hide it.
      (expect (form/hide-tool-code? {:vis/tool-name "cat"}))
      (expect (form/hide-tool-code? {:vis/tool-name "rg" :success? true}))
      ;; Python always keeps the actual submitted program, including on failure.
      (expect (not (form/hide-tool-code? {:vis/tool-name "python_execution"})))
      (expect (not (form/hide-tool-code? {:vis/tool-name "python_execution"
                                          :error "1 | boom()\n    ^"})))
      ;; A non-tool form has no card, so there's nothing to hide behind.
      (expect (not (form/hide-tool-code? {:result {:k 1}})))
      ;; Failed native tools keep source context for channels that need it.
      (expect (not (form/hide-tool-code? {:vis/tool-name "cat" :error "boom"})))
      (expect (not (form/hide-tool-code? {:vis/tool-name "cat" :success? false})))
      ;; A long-running tool needs no exception here: `python_execution` keeps its
      ;; program above, and shell authors its own pending card
      ;; body, so nothing has to re-show raw invocation JSON while a call runs.
      (expect (form/hide-tool-code? {:vis/tool-name "shell"}))
      (expect (form/hide-tool-code? {:vis/tool-name "shell"})))
  (it "removes redundant mutation verbs from new and persisted tool summaries"
      (doseq
        [[tool summary expected] [["patch" "update `a.clj` · add `b.clj`" "`a.clj`, `b.clj`"]
                                  ["struct_patch" "update `src/app.clj`" "`src/app.clj`"]
                                  ["struct_patch" "(no change) `README.md`" "`README.md`"]
                                  ["cat" "update `literal.txt`" "update `literal.txt`"]]]
        (expect (= expected
                   (:summary (form/result-card {:vis/tool-name tool :result-summary summary}))))))
  (it "falls back to the tool-authored PENDING card while a call is still running"
      ;; A running call has no result yet, but it is the SAME card: it wears the
      ;; headline AND the body its own `:render-start-call-fn` authored (`$ npm test
      ;; (running)` over a COMMAND section) instead of collapsing to a bare band.
      (let
        [running (form/result-card {:vis/tool-name "shell"
                                    :pending-summary "$ npm test (running)"
                                    :pending-render "**COMMAND**\n```bash\nnpm test\n```"})]
        (expect (= "$ npm test (running)" (:summary running)))
        (expect (= "**COMMAND**\n```bash\nnpm test\n```" (:body running)))
        (expect (:collapsible? running)))
      ;; Summary-only pending card: nothing to fold under it yet.
      (let
        [bare (form/result-card {:vis/tool-name "shell_logs"
                                 :pending-summary "◷ `dev` reading logs"})]
        (expect (nil? (:body bare)))
        (expect (not (:collapsible? bare))))
      ;; The moment the result lands it wins — a finished card never says "running".
      (let
        [done (form/result-card {:vis/tool-name "shell"
                                 :pending-summary "$ npm test (running)"
                                 :pending-render "**COMMAND**\n```bash\nnpm test\n```"
                                 :result-summary "exit 0 · 12 lines"
                                 :result-render "**OUTPUT**\n```\nok\n```"})]
        (expect (= "exit 0 · 12 lines" (:summary done)))
        (expect (= "**OUTPUT**\n```\nok\n```" (:body done)))))
  (it "->display drops nils so a merge never stamps empty keys"
      (expect (= {} (form/->display {:result nil :vis/tool-name nil})))
      (expect (= {:vis/tool-name "rg"} (form/->display {:vis/tool-name "rg" :result-render nil}))))
  (it "labels the streaming placeholder as a native call"
      (expect (= "NATIVE CALL" (#'form/tool-label "native_call")))))

(defdescribe form-authored-display-code-test
             (it "keeps a tool-authored pending display instead of re-deriving it from the call"
                 ;; `shell`'s `:render-start-call-fn` already rendered the bash this call is about to run;
                 ;; `with-display-code` must not overwrite it with the invocation's own formatting.
                 (let
                   [form (form/with-display-code {:code "shell({\"commands\": [\"sleep 30\"]})"
                                                  :display-code "sleep 30"
                                                  :display-language "bash"
                                                  :vis/tool-name "shell"})]
                   (expect (= "sleep 30" (:display-code form)))
                   (expect (= "bash" (:display-language form)))
                   ;; the raw invocation is still carried for the model-facing surfaces
                   (expect (= "shell({\"commands\": [\"sleep 30\"]})" (:code form)))))
             (it "still derives the display for a form that authored none"
                 (let [form (form/with-display-code {:code "x=1"})]
                   (expect (seq (:display-code form)))
                   (expect (nil? (:display-language form))))))
