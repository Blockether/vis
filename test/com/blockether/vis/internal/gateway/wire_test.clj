(ns com.blockether.vis.internal.gateway.wire-test
  "THE canonical-shape invariant. `wire/canonical` is DEFINED as what a remote
   client holds after `parse-json` ∘ `json-str`; this gate keeps the two in
   lockstep. Canonical map keys are snake_case STRINGS — never keywords, never
   kebab, never a trailing `?` (boolean-style keys become `is_*`)."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private rich-fixture
  "One value exercising every wire conversion: kebab + namespaced keyword
   keys, `?` booleans, keyword/symbol/ratio/uuid/date values, string keys,
   nesting — including the string-keyed tokens/cost/utilization maps the
   engine now emits canonically at construction."
  [{:id #uuid "00000000-0000-0000-0000-000000000042"
    :user-request "hello"
    :answer-markdown "**hi**"
    :created-at (java.util.Date. 1700000000000)
    :status :completed
    :needs-input? false
    :duration-ms 812
    :tokens {"input" 11461 "output" 35 "cached" 4096 "cache_created" 12}
    :cost {"total_cost" 0.006954 "model" "gpt-4o" "provider" "openai"}
    :utilization {"last_request_tokens" 100 "saturation" 4 "headroom_tokens" 900}
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

(defn- all-map-keys
  [x]
  (let [acc (atom [])]
    (walk/postwalk (fn [v]
                     (when (map? v) (swap! acc into (keys v)))
                     v)
                   x)
    @acc))

(defdescribe
  canonical-roundtrip-test
  (it "canonical == parse-json ∘ json-str for a rich engine value"
      (expect (= (wire/canonical rich-fixture) (wire/parse-json (wire/json-str rich-fixture)))))
  (it "namespaced keyword VALUES keep their namespace across the wire"
      (expect (= {"tool_color_role" "tool-color/search"}
                 (wire/canonical {:tool-color-role :tool-color/search}))))
  (it "map keys canonicalize to snake STRINGS, namespaces dropped, `?` -> `is_`"
      (expect (= {"tool_name" "rg" "duration_ms" 5 "is_llm_fallback" true}
                 (wire/canonical {:vis/tool-name "rg" :duration-ms 5 :llm-fallback? true}))))
  (it "boolean-style keyword keys become `is_*` (never a trailing `?`)"
      (expect (= {"is_draft" true "is_redacted" false "is_answer_present" true}
                 (wire/canonical {:draft? true :redacted? false :answer-present? true}))))
  (it "already-canonical string keys pass through untouched"
      (expect (= {"total_cost" 0.1 "cache_created" 2}
                 (wire/canonical {"total_cost" 0.1 "cache_created" 2})))))

(defdescribe
  no-legacy-key-survives-test
  (it "every canonical map key is a snake_case STRING — no keyword, kebab, or `?`"
      (let [ks (all-map-keys (wire/canonical rich-fixture))]
        (expect (seq ks))
        (expect (every? string? ks))
        (expect (not-any? #(str/includes? % "-") ks))
        (expect (not-any? #(str/ends-with? % "?") ks))))
  (it
    "the string-keyed tokens/cost/utilization maps ride the wire IDENTICAL (no re-keying anywhere)"
    (let [m
          {:tokens {"input" 1 "cache_created" 2}
           :cost {"total_cost" 0.1 "model" "m"}
           :utilization {"saturation" 9 "headroom_tokens" 10}}

          c
          (wire/canonical m)]

      (expect (= (:tokens m) (get c "tokens")))
      (expect (= (:cost m) (get c "cost")))
      (expect (= (:utilization m) (get c "utilization"))))))
