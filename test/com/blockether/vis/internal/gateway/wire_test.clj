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
