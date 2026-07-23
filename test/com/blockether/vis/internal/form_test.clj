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
  "A non-nil sentinel VALUE per display key, shaped like the real thing so the
   round-trip exercises keyword-valued fields (`:tool-color-role`) and scalars."
  [k]
  (case k
    :tool-color-role
    :tool-color/search

    :result-kind
    :tool

    :result-detail
    {:op :rg :hit-count 9}

    :render-segments
    [{:kind :code}]

    ;; A print-many block: each card is a canonical MINI-FORM carrying a NESTED
    ;; keyword colour the JSON wire stringifies — the exact hazard `<-wire` recurses
    ;; over so the per-card colour survives like the top-level one.
    :cards
    [{:vis/tool-name "cat"
      :result-summary "read 3 lines"
      :result-render "```\nx\n```"
      :tool-color-role :tool-color/read}
     {:vis/tool-name "rg"
      :result-summary "5 hits in 1 file"
      :result-render "a.clj:1: x"
      :tool-color-role :tool-color/search}]

    (:timeout? :repaired? :auto-repaired?)
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
  (it
    "every display key survives loop chunk -> gateway block.output -> wire -> <-wire"
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
                (str k " was dropped on the gateway round-trip — add it to a boundary projection")))
      ;; Keyword-valued fields come back as KEYWORDS (not the wire's strings), or
      ;; the channel's keyword dispatch (badge colour) silently misses.
      (expect (= :tool-color/search (:tool-color-role back)))
      (expect (keyword? (:tool-color-role back)))
      ;; NESTED: each card is a mini-form whose snake_case keys + stringified colour
      ;; were recovered by `<-wire` recursing — so a print-many block's per-card
      ;; colours survive the JSON hop the same way the singular badge does.
      (let [cards (:cards back)]
        (expect (= 2 (count cards)))
        (expect (= "cat" (:vis/tool-name (first cards))))
        (expect (= :tool-color/read (:tool-color-role (first cards))))
        (expect (= :tool-color/search (:tool-color-role (second cards))))
        (expect (every? (comp keyword? :tool-color-role) cards)))))
  (it
    "result-cards is the ONE projection: N cards for a print-many form, 1 for a native form, none for a non-tool"
    ;; print-many: each :cards mini-form → its own op-card descriptor, in order.
    (let
      [multi (form/result-cards {:vis/tool-name "python_execution"
                                 :cards [{:vis/tool-name "cat"
                                          :result-summary "read 3 lines"
                                          :result-render "x"
                                          :tool-color-role :tool-color/read}
                                         {:vis/tool-name "rg"
                                          :result-summary "5 hits"
                                          :result-render "y"
                                          :tool-color-role :tool-color/search}]})]
      (expect (= 2 (count multi)))
      (expect (= ["CAT" "RG"] (mapv :label multi)))
      (expect (= [:tool-color/read :tool-color/search] (mapv :color-role multi)))
      (expect (every? :tool? multi)))
    ;; single native form (no :cards) → exactly its own card.
    (let
      [one (form/result-cards {:vis/tool-name "rg"
                               :result-summary "5 hits"
                               :result-render "y"
                               :tool-color-role :tool-color/search})]
      (expect (= 1 (count one)))
      (expect (= "RG" (:label (first one)))))
    ;; non-tool form → no card at all (its body stays channel-specific).
    (expect (= [] (form/result-cards {:result {:k 1}}))))
  (it "hide-tool-code? drops the redundant source for a successful native non-python tool only"
      ;; A successful native tool (cat/rg/patch/…) already renders as an op-card, so
      ;; its synthesized `name(args)` source is redundant chrome — hide it.
      (expect (form/hide-tool-code? {:vis/tool-name "cat"}))
      (expect (form/hide-tool-code? {:vis/tool-name "rg" :success? true}))
      ;; python_execution is the model's OWN program — always keep its code.
      (expect (not (form/hide-tool-code? {:vis/tool-name "python_execution"})))
      ;; A non-tool form has no card, so there's nothing to hide behind.
      (expect (not (form/hide-tool-code? {:result {:k 1}})))
      ;; An errored tool form keeps its code so the inline error has context.
      (expect (not (form/hide-tool-code? {:vis/tool-name "cat" :error "boom"})))
      (expect (not (form/hide-tool-code? {:vis/tool-name "cat" :success? false}))))
  (it "removes redundant mutation verbs from new and persisted tool summaries"
      (doseq
        [[tool summary expected] [["patch" "update `a.clj` · add `b.clj`" "`a.clj`, `b.clj`"]
                                  ["struct_patch" "update `src/app.clj`" "`src/app.clj`"]
                                  ["write" "(no change) `README.md`" "`README.md`"]
                                  ["cat" "update `literal.txt`" "update `literal.txt`"]]]
        (expect (= expected
                   (:summary (form/result-card {:vis/tool-name tool :result-summary summary}))))))
  (it "->display drops nils so a merge never stamps empty keys"
      (expect (= {} (form/->display {:result nil :vis/tool-name nil})))
      (expect (= {:vis/tool-name "rg"} (form/->display {:vis/tool-name "rg" :result-render nil}))))
  (it "labels the streaming placeholder as a native call"
  (expect (= "NATIVE CALL" (form/tool-label "native_call")))))
