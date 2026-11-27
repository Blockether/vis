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
             (it "keeps an oversized UTF-8 event's cursor and omits its body"
                 (let [event {"schema" 1 "seq" 7 "ts" 1 "type" "block.output"
                              "session_id" "session-1" "turn_id" "turn-1"
                              "text" (apply str (repeat 350000 "界"))}
                       frame (sse/sse-frame event)
                       payload (wire/parse-json (subs (nth (str/split-lines frame) 2) 6))]
                   (expect (<= (alength (.getBytes frame java.nio.charset.StandardCharsets/UTF_8))
                               1048576))
                   (expect (str/starts-with? frame "id: 7\nevent: event.elided\ndata: "))
                   (expect (= {"schema" 1 "seq" 7 "ts" 1 "type" "event.elided"
                               "session_id" "session-1" "turn_id" "turn-1"
                               "event_type" "block.output" "reason" "frame_size"}
                              payload))))
             (it "keeps a bounded session repair marker when a view ID is too large"
                 (let [event {"seq" 7 "type" "view.patch" "session_id" "s"
                              "view_id" (apply str (repeat sse/max-frame-bytes "v"))}
                       frame (sse/sse-frame event)
                       payload (wire/parse-json (subs (nth (str/split-lines frame) 2) 6))]
                   (expect (<= (count frame) sse/max-frame-bytes))
                   (expect (= [7 "s" "view.patch" nil]
                              (mapv payload ["seq" "session_id" "event_type" "view_id"])))))
             (it "bounds an elided job frame even when its ID is too large"
                 (let [frame (sse/job-sse-frame gateway-contract/voice-job-event
                                                {"id" (apply str (repeat sse/max-frame-bytes "v"))})]
                   (expect (<= (count frame) sse/max-frame-bytes))
                   (expect (= {"is_elided" true} (wire/parse-json (subs (second (str/split-lines frame)) 6))))))
             (it "frames current job state without a session cursor"
                 (let [frame (sse/job-sse-frame gateway-contract/voice-job-event
                                                {"id" "vj_1" "phase" "transcribing"})]
                   (expect (str/starts-with? frame "event: voice.job\ndata: {"))
                   (expect (str/ends-with? frame "\n\n"))
                   (expect (not (str/includes? frame "id: "))))))
