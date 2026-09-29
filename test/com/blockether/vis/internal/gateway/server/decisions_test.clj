(ns com.blockether.vis.internal.gateway.server.decisions-test
  (:require [lazytest.core :refer [defdescribe expect it]]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.decisions.core :as decisions]
            [com.blockether.vis.internal.gateway.server.decisions :as api])
  (:import [java.io ByteArrayInputStream]
           [java.nio.charset StandardCharsets]))

(defdescribe
  decision-input-limit-response
  (it "returns a distinct HTTP 400 with safe token counts"
      ;; #295: carry the encoder budget across the real gateway error envelope.
      (with-redefs [decisions/infer! (fn [_]
                                       (throw (ex-info
                                                "Input exceeds the model limit. Shorten the state."
                                                {:type :decisions/input-too-long
                                                 :input-tokens 780
                                                 :max-input-tokens 512
                                                 :state "private state"})))]
        (let [response ((get api/handlers [:post "/v1/systemone"])
                         {:body (ByteArrayInputStream. (.getBytes "{}" StandardCharsets/UTF_8))})
              body (wire/parse-json (:body response))]

          (expect (= 400 (:status response)))
          (expect (= {"error" {"type" "input-too-long"
                               "message" "Input exceeds the model limit. Shorten the state."
                               "input_tokens" 780
                               "max_input_tokens" 512}}
                     body))
          (expect (document/valid-json? "gateway" "error_response" body))))))
