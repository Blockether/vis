(ns com.blockether.vis.internal.loop-recording-test
  "Recording upload, prompt, durable transcript and resumed-context regression coverage."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.attachment.audio-transcribe :as at]
            [com.blockether.vis.internal.context.prompt :as prompt]
            [com.blockether.vis.internal.gateway.server :as server]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.loop.iteration :as iteration]
            [com.blockether.vis.internal.loop.transcript :as transcript]
            [com.blockether.vis.internal.loop.turn :as turn]
            [com.blockether.vis.internal.persistance.core :as persistence]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [com.blockether.vis.internal.speech.core :as speech]
            [lazytest.core :refer [defdescribe expect it]]))

(h/use-mem-store!)

(def ^:private recording
  {:filename "meeting.m4a" :media-type "audio/mp4" :source "user" :base64 "YXVkaW8=" :size 5})

;; Regression: session 388aa964-07cf-498a-a6d7-71c49c86b491 finished its model
;; response before the audio worker. The words never reached storage or reopening.
(defdescribe
  recording-lifecycle-test
  (it
    "joins upload work once, stores its words, and includes them after a cold resume"
    (let [db
          (h/store)

          sid
          (h/store-session! db {:channel :api})

          hold
          (promise)

          entered
          (promise)

          calls
          (atom 0)

          submitted
          (atom nil)

          chunk
          (promise)

          words
          "We agreed to meet on Friday."

          engine
          {:id :recording-test
           :label "Test recording"
           :transcribe (fn [_]
                         (swap! calls inc)
                         (deliver entered true)
                         (deref hold 5000 nil)
                         words)}]

      (at/clear-cache!)
      (try
        (with-redefs [at/engine
                      (constantly engine)

                      at/enabled?
                      (constantly true)

                      at/available?
                      (constantly true)

                      speech/resolve-engine
                      (fn [& _]
                        engine)

                      state/soul
                      (constantly {:id sid})

                      state/submit-turn!
                      (fn [_ opts]
                        (reset! submitted opts)
                        {:turn {:turn_id "recorded-turn"}})

                      lp/db-info
                      (constantly db)]

          (let [uploaded
                (#'server/upload-attachment-handler
                 {:path-params {:sid (str sid)}
                  :query-params {"filename" "meeting.m4a" "media_type" "audio/mp4"}
                  :headers {"content-length" "5"}
                  :body (java.io.ByteArrayInputStream. (.getBytes "audio" "UTF-8"))})

                upload-id
                (get (wire/parse-json (:body uploaded)) "upload_id")

                accepted
                (#'server/submit-turn-handler
                 {:path-params {:sid (str sid)}
                  :body (java.io.ByteArrayInputStream.
                          (.getBytes (wire/json-str {:request "Summarize this"
                                                     :attachments [{:upload_id upload-id}]})
                                     "UTF-8"))})

                staged
                (:attachments @submitted)

                tid
                (persistence/db-store-session-turn!
                  db
                  {:parent-session-id sid :user-request "Summarize this" :attachments staged})

                stored
                (#'turn/store-transcripts! db tid staged)

                request
                (future (prompt/assemble-initial-messages
                          {:stable-prompt-messages []
                           :initial-user-content "Summarize this"
                           :vision? false
                           :user-images (#'iteration/transcribe-turn-attachments
                                         staged
                                         {:hooks {:on-chunk #(deliver chunk %)}})}))]

            (try (expect (= 201 (:status uploaded)))
                 (expect (= 202 (:status accepted)))
                 (expect (= true (deref entered 5000 ::timeout)))
                 (expect (= at/PENDING (:transcription-status (first staged))))
                 (expect (= :attachment-transcription (:phase (deref chunk 5000 {}))))
                 (expect (not (realized? request)))
                 (deliver hold true)
                 (expect (str/includes? (str (deref request 5000 ::timeout)) words))
                 (expect (nil? (deref stored 5000 ::timeout)))
                 (expect (= 1 @calls))
                 (persistence/db-update-session-turn!
                   db
                   tid
                   {:status :done :content [{:id "summary" :type "prose" :markdown "Summary"}]})
                 ;; Drop the worker cache: the database, not process memory, must own it.
                 (at/clear-cache!)
                 (expect (= words
                            (get-in (first (state/transcript sid))
                                    ["attachments" 0 "transcription"])))
                 (expect (str/includes?
                           (prompt/previous-turn-context-block
                             (#'transcript/previous-turn-context {:db-info db :session-id sid} nil))
                           words))
                 (finally (deliver hold true) (future-cancel request) (future-cancel stored)))))
        (finally (deliver hold true) (at/clear-cache!)))))
  (it
    "persists a late cached result when the recording is reopened"
    (let [db
          (h/store)

          sid
          (h/store-session! db {:channel :api})

          tid
          (persistence/db-store-session-turn!
            db
            {:parent-session-id sid :user-request "Listen" :attachments [recording]})

          words
          "A completed recording recovered on reopen."]

      (with-redefs [lp/db-info
                    (constantly db)

                    state/soul
                    (constantly {:id sid})

                    at/request-attachments!
                    (fn [rows]
                      (mapv #(assoc % :transcription words) rows))]

        (let [state-id
              (persistence/db-latest-session-state-id db sid)

              _
              (persistence/db-set-session-prompt-cache-state! db state-id {:without-recording true})

              response
              (#'server/turn-attachments-handler
               {:path-params {:sid (str sid) :tid (str tid)}
                :query-params {"transcription_only" "true"}})

              attachment
              (get-in (wire/parse-json (:body response)) ["attachments" 0])]

          (expect (= 200 (:status response)))
          (expect (= words (get attachment "transcription")))
          (expect (not (contains? attachment "base64")))
          (expect (nil? (persistence/db-get-session-prompt-cache-state db state-id)))
          (expect (= words
                     (:transcription (first (persistence/db-list-turn-attachments db tid))))))))))

(defdescribe recording-watchdog-test
             (it "does not mistake a long recording join for a stalled provider or model output"
                 (let [waiting (#'state/advance-turn-stall-state
                                {:started? true :produced? false}
                                {:phase :attachment-transcription}
                                0)]
                   (expect (false? (:produced? waiting)))
                   (expect (not (:tripped? (#'state/turn-stall-decision waiting 600000))))
                   (let [calling
                         (#'state/advance-turn-stall-state waiting {:phase :provider-call} 600000)]
                     (expect (:tripped? (#'state/turn-stall-decision calling 600000)))))))

(defdescribe
  recording-cache-invalidation-test
  (it
    "invalidates only changed words and only the recording's session"
    (let [db
          (h/store)

          sid
          (h/store-session! db {:channel :api})

          other-sid
          (h/store-session! db {:channel :api})

          tid
          (persistence/db-store-session-turn!
            db
            {:parent-session-id sid :user-request "Listen" :attachments [recording]})

          state-id
          (persistence/db-latest-session-state-id db sid)

          other-state-id
          (persistence/db-latest-session-state-id db other-sid)

          checkpoint
          {:prefix "saved"}]

      (persistence/db-set-session-prompt-cache-state! db state-id checkpoint)
      (persistence/db-set-session-prompt-cache-state! db other-state-id checkpoint)
      (expect (false? (persistence/db-set-turn-attachment-transcription! db tid 9 "words" nil)))
      (expect (= checkpoint (persistence/db-get-session-prompt-cache-state db state-id)))
      (expect (true? (persistence/db-set-turn-attachment-transcription! db tid 0 "words" nil)))
      (expect (nil? (persistence/db-get-session-prompt-cache-state db state-id)))
      (expect (= checkpoint (persistence/db-get-session-prompt-cache-state db other-state-id)))
      (persistence/db-set-session-prompt-cache-state! db state-id checkpoint)
      (expect (true? (persistence/db-set-turn-attachment-transcription! db tid 0 "words" nil)))
      (expect (= checkpoint (persistence/db-get-session-prompt-cache-state db state-id))))))

(defdescribe
  recording-position-test
  (it
    "persists by the durable position rather than the order of a filtered response"
    (let [db
          (h/store)

          sid
          (h/store-session! db {:channel :api})

          tid
          (persistence/db-store-session-turn! db
                                              {:parent-session-id sid
                                               :user-request "Listen"
                                               :attachments [recording
                                                             (assoc recording
                                                               :filename "second.m4a"
                                                               :base64 "b3RoZXI=")]})

          rows
          (persistence/db-list-turn-attachments db tid)]

      (with-redefs [lp/db-info
                    (constantly db)

                    state/soul
                    (constantly {:id sid})

                    state/turn-attachments
                    (fn [& _]
                      [(second rows)])

                    at/request-attachments!
                    (fn [rows]
                      (mapv #(assoc % :transcription "second words") rows))]

        (#'server/turn-attachments-handler
         {:path-params {:sid (str sid) :tid (str tid)}
          :query-params {"transcription_only" "true"}}))
      (let [saved (persistence/db-list-turn-attachments db tid)]
        (expect (nil? (:transcription (first saved))))
        (expect (= "second words" (:transcription (second saved))))))))

(defdescribe
  recording-wait-limit-test
  (it "stops the turn after a five-minute join instead of sending pending audio to the model"
      (let [seen
            (atom nil)

            chunks
            (atom [])

            failure
            (with-redefs [at/transcribe-attachments
                          (fn [rows opts]
                            (reset! seen opts)
                            (mapv #(assoc % :transcription-status at/PENDING) rows))]
              (try (#'iteration/transcribe-turn-attachments
                    [recording]
                    {:hooks {:on-chunk #(swap! chunks conj %)}})
                   nil
                   (catch clojure.lang.ExceptionInfo e e)))]

        (expect (= 300000 (:timeout-ms @seen)))
        (expect (= [{:phase :attachment-transcription :iteration 1}] @chunks))
        (expect (= :com.blockether.vis.internal.loop.iteration/recording-transcription-timeout
                   (:type (ex-data failure))))
        (expect (str/includes? (str (some-> failure
                                            ex-message))
                               "5 minutes")))))

(defdescribe
  recording-progress-event-test
  (it "publishes the transcription phase to the live ticker without storing a model delta"
      (expect (= ["turn.progress" false {:progress "attachment-transcription" :iteration 1}]
                 (#'state/progress-chunk->event {:phase :attachment-transcription :iteration 1})))))
