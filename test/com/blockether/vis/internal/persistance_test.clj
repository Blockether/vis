(ns com.blockether.vis.internal.persistance-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.gateway.wire :as wire]
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

;; Regression (session 4b6897d4): a runtime error quoted the entire document
;; that broke it, so the turn's terminal write bound a value past
;; SQLITE_MAX_LENGTH, threw `[SQLITE_TOOBIG]`, and the turn stayed `running`
;; for good -- no status, no error, no iteration count -- inside a session that
;; had already finished it.
(defdescribe
  bounded-turn-diagnostics-test
  "`db-update-session-turn!` writes the row that says HOW a turn ended, and its
   DIAGNOSTIC fields are bounded here in the facade so every backend gets the
   same guarantee. The answer's own content is DATA and is persisted verbatim."
  (it "truncates one oversized diagnostic string and names what it cut"
      (let
        [huge
         (apply str (repeat (+ persistance/max-persisted-error-chars 500) "x"))

         bounded
         (persistance/bounded-error-text huge)]

        ;; The marker counts itself in: bounding NEVER grows a string.
        (expect (<= (count bounded) persistance/max-persisted-error-chars))
        (expect (str/starts-with? bounded (subs huge 0 1000)))
        (expect (str/ends-with? bounded " chars truncated>"))))
  (it "leaves a diagnostic already within the cap byte-identical"
      (expect (= "boom" (persistance/bounded-error-text "boom")))
      (expect (= "" (persistance/bounded-error-text ""))))
  (it "reaches every string at any depth of a structured error"
      (let
        [huge
         (apply str (repeat (inc persistance/max-persisted-error-chars) "y"))

         bounded
         (persistance/bound-error-data
           {"type" "error" "code" "python_runtime" "detail" {"message" huge "frames" [huge]}})]

        (expect (= "error" (get bounded "type")))
        (expect (= "python_runtime" (get bounded "code")))
        (expect (<= (count (get-in bounded ["detail" "message"]))
                    persistance/max-persisted-error-chars))
        (expect (str/ends-with? (get-in bounded ["detail" "message"]) " chars truncated>"))
        (expect (<= (count (first (get-in bounded ["detail" "frames"])))
                    persistance/max-persisted-error-chars)))
      (expect (nil? (persistance/bound-error-data nil))))
  (it "bounds the error and the ERROR content blocks before the backend sees them, and nothing else"
      (let
        [huge
         (apply str (repeat (+ persistance/max-persisted-error-chars 7) "z"))

         seen
         (atom nil)]

        (with-redefs-fn {#'persistance/resolve-impl (fn [_ _]
                                                      (atom (fn [_ _ opts]
                                                              (reset! seen opts))))}
          #(persistance/db-update-session-turn! {}
                                                "turn-1"
                                                {:status :error
                                                 :iteration-count 33
                                                 :error {"type" "error" "message" huge}
                                                 :content [{"type" "error" "message" huge}
                                                           {"type" "text" "text" huge}]}))
        (expect (= :error (:status @seen)))
        (expect (= 33 (:iteration-count @seen)))
        (expect (<= (count (get (:error @seen) "message")) persistance/max-persisted-error-chars))
        (expect (<= (count (get (first (:content @seen)) "message"))
                    persistance/max-persisted-error-chars))
        ;; An answer is DATA: the facade never truncates it.
        (expect (= huge (get (second (:content @seen)) "text"))))))
