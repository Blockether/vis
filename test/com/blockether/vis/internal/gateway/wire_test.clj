(ns com.blockether.vis.internal.gateway.wire-test
  "THE canonical-shape invariant. `wire/canonical` is DEFINED as what a remote
   client holds after `parse-json` ∘ `json-str`; this gate keeps the two in
   lockstep. A facade that serves `(canonical x)` (the gateway transcript)
   therefore feeds in-process readers EXACTLY what an HTTP client parses —
   one shape, every transport, every channel."
  (:require [com.blockether.vis.internal.gateway.wire :as wire]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private rich-fixture
  "One value exercising every wire conversion: kebab + namespaced keyword
   keys, keyword/symbol/ratio/uuid/date values, string keys, nesting."
  [{:id #uuid "00000000-0000-0000-0000-000000000042"
    :user-request "hello"
    :answer-markdown "**hi**"
    :created-at (java.util.Date. 1700000000000)
    :status :completed
    :prior-outcome :cancelled
    :duration-ms 812
    :total-cost 1/4
    :iterations [{:thinking "t"
                  :llm-routing-trace [{:provider "x" :ok? true}]
                  :forms [{:scope "t1/i1/f1"
                           :tag :observation
                           :src "(cat \"x\")"
                           :vis/tool-name "cat"
                           :tool-color-role :tool-color/read
                           :result-summary "`x` · 3 lines"
                           :result {:anchors {"1:abc" "line one"}}
                           :duration-ms 5}]}]}])

(defdescribe
  canonical-roundtrip-test
  (it "canonical == parse-json ∘ json-str for a rich engine value"
      (expect (= (wire/canonical rich-fixture) (wire/parse-json (wire/json-str rich-fixture)))))
  (it "namespaced keyword VALUES keep their namespace across the wire"
      ;; the TUI badge colour (`:tool-color/search`) used to degrade to a bare
      ;; `:search` on the remote wire while the in-process web kept the full
      ;; keyword — the exact two-shapes class of bug `canonical` exists to kill.
      (expect (= {:tool_color_role "tool-color/search"}
                 (wire/canonical {:tool-color-role :tool-color/search}))))
  (it "map keys canonicalize to snake keywords, namespaces dropped"
      (expect (= {:tool_name "rg" :duration_ms 5 :llm_fallback? true}
                 (wire/canonical {:vis/tool-name "rg" :duration-ms 5 :llm-fallback? true})))))

(def ^:private workspace-fixture
  "A gateway workspace record as `state/session-workspace-info` emits it —
   every hyphenated key the TUI picker / web footer read back kebab."
  {:id "ws-1"
   :draft? true
   :root "/clones/app"
   :repo-root "/real/app"
   :label "app"
   :fork-ms 1700000000000
   :filesystem-roots
   [{:trunk "/real/docs" :clone "/clones/docs" :fork-ms 1700000000001 :backend :graalpy}
    {:trunk "/real/lib" :clone "/real/lib" :fork-ms nil :backend :live}]})

(defdescribe kebab-keys-inverts-the-wire-munge-test
             ;; The two-shapes bug the workspace picker hit: the wire munges kebab keys
             ;; `-`->`_`, `parse-json` keywordizes verbatim, so `:filesystem-roots` came
             ;; back `:filesystem_roots` and every kebab reader saw nil. `kebab-keys` is
             ;; the codec's documented inverse — a GENERIC round-trip guard so a new
             ;; hyphenated key can never silently reintroduce it.
             (it "kebab-keys ∘ parse-json ∘ json-str is identity for a workspace record"
                 (expect (= workspace-fixture
                            (-> workspace-fixture
                                wire/json-str
                                wire/parse-json
                                wire/kebab-keys
                                ;; :backend rides as a keyword VALUE, stringified by the
                                ;; wire — the one field the client re-coerces on top.
                                (update :filesystem-roots
                                        (fn [rs]
                                          (mapv #(update % :backend keyword) rs)))))))
             (it "the raw hop (no kebab-keys) loses the hyphenated keys — the bug"
                 (let [raw (-> workspace-fixture
                               wire/json-str
                               wire/parse-json)]
                   (expect (nil? (:filesystem-roots raw)))
                   (expect (nil? (:repo-root raw)))
                   (expect (some? (:filesystem_roots raw)))))
             (it "kebab-keys is idempotent on already-kebab input and nil-safe"
                 (expect (= workspace-fixture (wire/kebab-keys workspace-fixture)))
                 (expect (nil? (wire/kebab-keys nil)))))
