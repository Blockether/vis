(ns com.blockether.vis.contract.wire-test
  "THE canonical-shape invariant. `wire/canonical` is DEFINED as what a remote
   client holds after `parse-json` ∘ `json-str`; this gate keeps the two in
   lockstep. Canonical map keys are snake_case STRINGS — never keywords, never
   kebab, never a trailing `?` (boolean-style keys become `is_*`)."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [com.blockether.vis.contract.wire :as wire]
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
                           :src "print('line one')"
                           :vis/tool-name "python_execution"
                           :stdout "line one
"
                           :activity {:rows [{:operation :print :summary "line one"}]}
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
  (it "namespaced keyword values keep their namespace across the wire"
      (expect (= {"status_code" "http/not-found"} (wire/canonical {:status-code :http/not-found}))))
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
  canonical-key-test
  (it "every canonical map key is a snake_case string"
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

(defdescribe
  search-match-wire-test
  "Transcript-search matches carry a NESTED `hits` vector. Nesting is exactly
   where a hand-rolled encoder drifts back into keywords, so pin it."
  (it "encodes a match and its nested hits as snake_case STRING keys"
      (let [match
            {:session_id "abc"
             ;; The gateway's own relevance band travels with the match: clients
             ;; paint this order, they do not re-derive one from the flags.
             :rank 1
             :is_in_title false
             :is_in_request true
             :is_in_reply false
             :is_in_thinking false
             :request_snippet "…needle…"
             :reply_snippet nil
             :hits [{:side "request" :snippet "…needle…" :at 1700000000000}]}

            w
            (wire/canonical [match])

            hit
            (-> w
                first
                (get "hits")
                first)]

        (expect (= #{"session_id" "rank" "is_in_title" "is_in_request" "is_in_reply"
                     "is_in_thinking" "request_snippet" "reply_snippet" "hits"}
                   (set (keys (first w)))))
        (expect (= #{"side" "snippet" "at"} (set (keys hit))))
        ;; The SIDE tag is a plain string on the wire — a keyword here would
        ;; serialize as ":request" and every client would have to strip the colon.
        (expect (= "request" (get hit "side")))
        (expect (every? string? (all-map-keys w))))))

(defdescribe
  unencodable-value-test
  "Every canonical value MUST survive `json-str`. The encoder throws per FRAME
   at the transport, and `append-event!` stores the canonical event in the
   replay ring BEFORE anything encodes it — so one unencodable value inside a
   tool result silently kills the SSE connection and the `/poll` batch for the
   whole session, then again on every replay."
  (it "non-string map keys (decoded JSON / Python dicts) render as JSON keys"
      (expect (= {"1" "a" "2" "b"} (wire/canonical {1 :a 2 :b})))
      (expect (= {"null" 1 "true" 2 "2.5" 3} (wire/canonical {nil 1 true 2 2.5 3}))))
  (it "a non-finite double becomes null instead of throwing, like JSON.stringify"
      (expect (= {"v" nil} (wire/canonical {:v (/ 0.0 0.0)})))
      (expect (= {"v" nil} (wire/canonical {:v Double/POSITIVE_INFINITY})))
      (expect (= {"v" nil} (wire/canonical {:v Double/NEGATIVE_INFINITY}))))
  (it "a BigDecimal keeps the canonical == roundtrip invariant"
      (expect (= {"v" 1.5} (wire/canonical {:v 1.5M}))))
  (it "a poll batch carrying such values remains encodable"
      (let [event (wire/canonical {:seq 1
                                   :type "iteration.completed"
                                   :tool-result {:counts {1 "a"} :ratio (/ 0.0 0.0)}})]
        (expect (= [event] (wire/parse-json (wire/json-str [event]))))))
  (it "the invariant holds for every awkward scalar"
      (doseq [x [{1 :a} {nil 1} {true 1} {[1 2] :k} {:v 1.0M} {:v (/ 0.0 0.0)}
                 {:v Double/POSITIVE_INFINITY} {:v (float 0.5)} {:v (/ 1 3)} {:v (biginteger 10)}
                 {:v #{1 2}} {:v \c} {:v Long/MAX_VALUE}]]
        (expect (= (wire/canonical x) (wire/parse-json (wire/json-str x)))
                (str "roundtrip differs for " (pr-str x))))))
