(ns com.blockether.vis.internal.gateway.server.transport.sse-test
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.gateway :as gateway-contract]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.gateway.server.transport.sse :as sse]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe sse-framing-test
             (it "frames a canonical session event with its replay cursor"
                 (let [event
                       (wire/canonical {:seq 7
                                        :type "iteration.completed"
                                        :tool-result {:counts {1 "a"} :ratio (/ 0.0 0.0)}})

                       frame
                       (sse/sse-frame event)

                       data
                       (->> (str/split-lines frame)
                            (filter #(str/starts-with? % "data: "))
                            first
                            (#(subs % 6))
                            wire/parse-json)]

                   (expect (str/starts-with? frame "id: 7\nevent: iteration.completed\ndata: "))
                   (expect (= event data))))
             (it "frames current job state without a session cursor"
                 (let [frame (sse/job-sse-frame gateway-contract/voice-job-event
                                                {"id" "vj_1" "phase" "transcribing"})]
                   (expect (str/starts-with? frame "event: voice.job\ndata: {"))
                   (expect (str/ends-with? frame "\n\n"))
                   (expect (not (str/includes? frame "id: "))))))
