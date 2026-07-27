(ns com.blockether.vis.internal.persistance-test
  (:require [com.blockether.vis.internal.gateway.wire :as wire]
            [com.blockether.vis.internal.persistance :as persistance]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  canonical-assistant-restore-test
  (it
    "restores canonical assistant keys while leaving tool input opaque"
    (let
      [message
       {:role :assistant
        :content
        [{:type :thinking :thinking "analysis" :thinking-signature "signature" :redacted? true}
         {:type :tool_use
          :id "call-1"
          :name "grep"
          :input {"thinking_signature" "user-owned" "is_redacted" false}}
         {:type :tool_result :tool-use-id "call-1" :is-error false :content "ok"}]}

       restored
       @(-> message
            wire/canonical
            persistance/->json
            persistance/<-json-canonical-lazy)]

      (expect
        (= {:role "assistant"
            :content
            [{:type "thinking" :thinking "analysis" :thinking-signature "signature" :redacted? true}
             {:type "tool_use"
              :id "call-1"
              :name "grep"
              :input {"thinking_signature" "user-owned" "is_redacted" false}}
             {:type "tool_result" :tool-use-id "call-1" :is-error false :content "ok"}]}
           restored)))))

(defdescribe
  json-column-totality-test
  "`->json` is THE column codec for every backend. Charred REFUSES four things —
   non-string map keys, a nil key, NaN and ±Infinity — and a throw here does not
   degrade a field, it loses the whole column: `content_json` on the final
   outcome row IS the settled answer. A Python `Counter` (int keys) or a pandas
   NaN riding a tool result is enough to trigger it."
  (it "encodes what charred refuses, instead of throwing"
      (expect (= "{\"1\":\"a\"}" (persistance/->json {1 :a})))
      (expect (= "{\"null\":1}" (persistance/->json {nil 1})))
      (expect (= "{\"v\":null}" (persistance/->json {:v (/ 0.0 0.0)})))
      (expect (= "{\"v\":null}" (persistance/->json {:v Double/POSITIVE_INFINITY})))
      (expect (= "{\"v\":null}" (persistance/->json {:v Double/NEGATIVE_INFINITY}))))
  (it "reaches nested tool-result content, not just the top level"
      (expect (= "{\"content\":[{\"type\":\"tool_result\",\"counts\":{\"1\":null}}]}"
                 (persistance/->json {:content [{:type "tool_result" :counts {1 (/ 0.0 0.0)}}]}))))
  (it "leaves every already-encodable spelling BYTE-identical (persisted data must not shift)"
      (expect (= "{\"a-b\":1}" (persistance/->json {:a-b 1})))
      (expect (= "{\"vis\\/x\":1}" (persistance/->json {:vis/x 1})))
      (expect (= "{\"s\":1}" (persistance/->json {"s" 1})))
      (expect (= "{\"v\":\"1970-01-01T00:00:00Z\"}" (persistance/->json {:v (java.util.Date. 0)})))
      (expect (= "{\"v\":[1]}" (persistance/->json {:v #{1}})))
      (expect (= "{\"v\":1.5}" (persistance/->json {:v 1.5}))))
  (it "nil in, nil out" (expect (nil? (persistance/->json nil)))))
