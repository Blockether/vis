(ns com.blockether.vis.internal.persistance.codec-test
  (:require [com.blockether.vis.internal.persistance.codec :as codec]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  json-column-totality-test
  "`->json` is THE column codec for every backend. Charred REFUSES four things —
   non-string map keys, a nil key, NaN and ±Infinity — and a throw here does not
   degrade a field, it loses the whole column: `content_json` on the final
   outcome row IS the settled answer. A Python `Counter` (int keys) or a pandas
   NaN riding a tool result is enough to trigger it."
  (it "encodes what charred refuses, instead of throwing"
      (expect (= "{\"1\":\"a\"}" (codec/->json {1 :a})))
      (expect (= "{\"null\":1}" (codec/->json {nil 1})))
      (expect (= "{\"v\":null}" (codec/->json {:v (/ 0.0 0.0)})))
      (expect (= "{\"v\":null}" (codec/->json {:v Double/POSITIVE_INFINITY})))
      (expect (= "{\"v\":null}" (codec/->json {:v Double/NEGATIVE_INFINITY}))))
  (it "reaches nested tool-result content, not just the top level"
      (expect (= "{\"content\":[{\"type\":\"tool_result\",\"counts\":{\"1\":null}}]}"
                 (codec/->json {:content [{:type "tool_result" :counts {1 (/ 0.0 0.0)}}]}))))
  (it "leaves every already-encodable spelling BYTE-identical (persisted data must not shift)"
      (expect (= "{\"a-b\":1}" (codec/->json {:a-b 1})))
      (expect (= "{\"vis\\/x\":1}" (codec/->json {:vis/x 1})))
      (expect (= "{\"s\":1}" (codec/->json {"s" 1})))
      (expect (= "{\"v\":\"1970-01-01T00:00:00Z\"}" (codec/->json {:v (java.util.Date. 0)})))
      (expect (= "{\"v\":[1]}" (codec/->json {:v #{1}})))
      (expect (= "{\"v\":1.5}" (codec/->json {:v 1.5}))))
  (it "nil in, nil out" (expect (nil? (codec/->json nil)))))
