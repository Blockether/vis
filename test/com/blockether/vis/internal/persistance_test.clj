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
