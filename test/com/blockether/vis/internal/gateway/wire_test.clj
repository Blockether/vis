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
  no-legacy-key-survives-test
  (it "every canonical map key is a snake_case STRING — no keyword, kebab, or `?`"
      (let [ks (all-map-keys (wire/canonical rich-fixture))]
        (expect (seq ks))
        (expect (every? string? ks))
        (expect (not-any? #(str/includes? % "-") ks))
        (expect (not-any? #(str/ends-with? % "?") ks))))
  (it
    "the string-keyed tokens/cost/utilization maps ride the wire IDENTICAL (no re-keying anywhere)"
    (let
      [m
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
      (let
        [match
         {:session_id "abc"
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

        (expect (= #{"session_id" "is_in_request" "is_in_reply" "is_in_thinking" "request_snippet"
                     "reply_snippet" "hits"}
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
  (it "an event carrying such values still renders an SSE frame and a poll batch"
      (let
        [event (wire/canonical {:seq 1
                                :type "iteration.completed"
                                :tool-result {:counts {1 "a"} :ratio (/ 0.0 0.0)}})]
        (expect (str/starts-with? (wire/sse-frame event)
                                  "id: 1\nevent: iteration.completed\ndata: "))
        (expect (= [event] (wire/parse-json (wire/json-str [event]))))))
  (it "the invariant holds for every awkward scalar"
      (doseq
        [x [{1 :a} {nil 1} {true 1} {[1 2] :k} {:v 1.0M} {:v (/ 0.0 0.0)}
            {:v Double/POSITIVE_INFINITY} {:v (float 0.5)} {:v (/ 1 3)} {:v (biginteger 10)}
            {:v #{1 2}} {:v \c} {:v Long/MAX_VALUE}]]
        (expect (= (wire/canonical x) (wire/parse-json (wire/json-str x)))
                (str "roundtrip differs for " (pr-str x))))))

(defdescribe
  bounded-clamp-test
  "Truncation must never emit a LONE surrogate: half an emoji is not valid text
   and corrupts every UTF-8 consumer downstream."
  (it "a cut landing inside a surrogate pair steps back instead of splitting it"
      (let
        [s
         (str "okxxxxxxxxxx" "😀😀")

         ;; A LONE surrogate cannot be encoded: the UTF-8 round-trip
         ;; replaces it with `?`, which is exactly how it reaches a client.
         utf8-clean?
         (fn [^String t]
           (= t (String. (.getBytes t "UTF-8") "UTF-8")))]

        (doseq [limit [11 12 13 14 15]]
          (expect (utf8-clean? (wire/bounded-str s limit))
                  (str "lone surrogate at limit " limit)))))
  (it "a non-positive limit clamps instead of throwing"
      (expect (= " …[truncated]" (wire/bounded-str "abc" 0)))
      (expect (= " …[truncated]" (wire/bounded-str "abc" -1))))
  (it "a short string is returned verbatim"
      (expect (= "abc" (wire/bounded-str "abc" 10)))
      (expect (= "\"abc\"" (wire/bounded-pr "abc" 10)))))
