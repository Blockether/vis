(ns com.blockether.vis.internal.gateway.server.transport.sse-test
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]
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
             (it "describes the repair frames that voice and speech clients receive"
                 (let [large (apply str (repeat sse/max-frame-bytes "v"))]
                   (doseq [[direction event-name] [["voice" gateway-contract/voice-job-event]
                                                   ["speech" gateway-contract/speech-job-event]]
                           [job expected] [[{:id "job-1" :is-done true :text large}
                                            {"id" "job-1" "is_elided" true}]
                                           [{:id large :is-done true} {"is_elided" true}]
                                           [{:id "job-1" :is-done true}
                                            {"id" "job-1" "is_done" true}]]]
                     (let [route (str "/v1/sessions/:sid/" direction "/jobs/:job-id/events")
                           schema (some-> (gateway-contract/operation :get route) :response-schema name)
                           frame (sse/job-sse-frame event-name job)
                           payload (wire/parse-json (subs (second (str/split-lines frame)) 6))]
                       (expect (<= (count frame) sse/max-frame-bytes))
                       (expect (= expected payload))
                       (expect (and schema (document/valid-json? "gateway" schema payload)))))))
             (it "frames current job state without a session cursor"
                 (let [frame (sse/job-sse-frame gateway-contract/voice-job-event
                                                {"id" "vj_1" "phase" "transcribing"})]
                   (expect (str/starts-with? frame "event: voice.job\ndata: {"))
                   (expect (str/ends-with? frame "\n\n"))
                   (expect (not (str/includes? frame "id: "))))))
