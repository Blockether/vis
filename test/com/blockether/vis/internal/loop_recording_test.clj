(ns com.blockether.vis.internal.loop-recording-test
  "Recording upload, prompt, durable transcript and resumed-context regression coverage."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.attachment.audio-transcribe :as at]
            [com.blockether.vis.internal.attachment.codec :as codec]
            [com.blockether.vis.internal.attachment.storage :as attachment-storage]
            [com.blockether.vis.internal.context.prompt :as prompt]
            [com.blockether.vis.internal.gateway.server.turns :as turns-api]
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
                (#'turns-api/upload-attachment-handler
                 {:path-params {:sid (str sid)}
                  :query-params {"filename" "meeting.m4a" "media_type" "audio/mp4"}
                  :headers {"content-length" "5"}
                  :body (java.io.ByteArrayInputStream. (.getBytes "audio" "UTF-8"))})

                upload-id
                (get (wire/parse-json (:body uploaded)) "upload_id")

                accepted
                (#'turns-api/submit-turn-handler
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
                          {:initial-user-content "Summarize this"
                           :vision? false
                           :user-images (#'iteration/transcribe-turn-attachments
                                         staged
                                         {:hooks {:on-chunk #(deliver chunk %)}})}))]

            (try
              (expect (= 201 (:status uploaded)))
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
                         (get-in (first (state/transcript sid)) ["attachments" 0 "transcription"])))
              ;; A later turn reads the recording's words from this turn's request.
              (expect
                (str/includes?
                  (pr-str
                    (keep (comp :turn/messages second)
                          (transcript/prior-turn-trailer {:db-info db :session-id sid} nil nil [])))
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
                    (fn [rows & _]
                      (mapv #(assoc % :transcription words) rows))]

        (let [state-id
              (persistence/db-latest-session-state-id db sid)

              _
              (persistence/db-set-session-prompt-cache-state! db state-id {:without-recording true})

              response
              (#'turns-api/turn-attachments-handler
               {:path-params {:sid (str sid) :tid (str tid)}
                :query-params {"transcription_only" "true"}})

              attachment
              (get-in (wire/parse-json (:body response)) ["attachments" 0])]

          (expect (= 200 (:status response)))
          (expect (= words (get attachment "transcription")))
          (expect (not (contains? attachment "base64")))
          (expect (nil? (persistence/db-get-session-prompt-cache-state db state-id)))
          (expect (= words
                     (:transcription (first (persistence/db-list-turn-attachments db tid)))))))))
  ;; Session 475b983a-62be-400c-aaf7-ff7bb4822c88: a transcript leaves a large payload
  ;; in the store. A client that shows one picture must not also read a 32 MiB recording.
  (it
    "answers only the attachment that the request names"
    (let [db
          (h/store)

          sid
          (h/store-session! db {:channel :api})

          picture
          {:filename "shot.png"
           :media-type "image/png"
           :source "user"
           :base64 "cGljdHVyZQ=="
           :size 7}

          tid
          (persistence/db-store-session-turn!
            db
            {:parent-session-id sid :user-request "Look" :attachments [recording picture]})

          id
          (str (:id (second (persistence/db-list-turn-attachments db tid))))]

      (with-redefs [lp/db-info
                    (constantly db)

                    state/soul
                    (constantly {:id sid})

                    at/request-attachments!
                    identity]

        (let [response
              (#'turns-api/turn-attachments-handler
               {:path-params {:sid (str sid) :tid (str tid)} :query-params {"id" id}})

              rows
              (get (wire/parse-json (:body response)) "attachments")]

          (expect (= 200 (:status response)))
          (expect (= [["shot.png" "cGljdHVyZQ=="]]
                     (mapv (juxt #(get % "filename") #(get % "base64")) rows))))))))

(defdescribe
  turn-attachment-bytes-test
  (it "serves only the selected payload, including after a sent turn is forgotten"
      (let [db (h/store)
            sid (h/store-session! db {:channel :api})
            tid (persistence/db-store-session-turn!
                  db {:parent-session-id sid :user-request "Listen" :status :done})
            qtid (str (random-uuid))
            text (apply str (repeat 4096 "hello "))
            data (.getBytes ^String text "UTF-8")
            packed (codec/pack data)
            image {:media-type "image/png" :filename "image.png" :base64 "aW1hZ2U="}
            html {:media-type "text/html" :filename "page.html"
                  :base64 (.encodeToString (java.util.Base64/getEncoder) data)}
            [raw-id packed-id corrupt-id]
            (persistence/db-store-queued-turn-attachments! db tid qtid [image html recording])
            iid (h/store-iteration!
                  db {:session-turn-id tid :status :done :code "pass" :attachments [image html]})
            packed-tool (:id (second (persistence/db-list-iteration-attachments-meta db iid)))
            _ (h/fork-session-at-turn! db sid {:through-turn-id tid :title "Forked"})
            fork-id (:id (first (h/raw-query
                                  db {:select [:id] :from [:session_attachment]
                                      :where [:and [:= :queued_turn_id qtid]
                                              [:not= :session_turn_soul_id (str tid)]]})))
            registry @#'state/registry
            request {:path-params {:sid (str sid) :tid qtid}}
            binary (fn [id]
                     (#'turns-api/turn-attachment-bytes-handler
                      (assoc-in request [:path-params :aid] id)))
            json (fn [id]
                   (get (wire/parse-json
                          (:body (#'turns-api/turn-attachments-handler
                                  (assoc request :query-params {"id" id}))))
                        "attachments"))]
        (expect (= "zstd" (:codec packed)))
        (doseq [id [packed-id packed-tool corrupt-id]]
          (h/raw-query db {:update :session_attachment
                           :set {:storage_codec "zstd"
                                 :bytes (if (= id corrupt-id) (byte-array [1]) (:bytes packed))}
                           :where [:= :id id]}))
        (with-redefs [lp/db-info (constantly db)
                      persistence/db-read-attachment (fn [& _]
                                                       (throw (ex-info "Legacy envelope read" {})))]
          (try
            (doseq [sent? [true false]]
              (if sent?
                (swap! registry assoc-in [(str sid) :turns qtid] {:status "sent"})
                (swap! registry dissoc (str sid)))
              (expect (= [{"filename" "image.png" "base64" "aW1hZ2U="}]
                         (mapv #(select-keys % ["filename" "base64"]) (json raw-id))))
              (expect (= [] (json fork-id)))
              (expect (= 404 (:status (binary fork-id))))
              (expect (= 404 (:status (binary "missing"))))
              (doseq [[id media expected] [[raw-id "image/png" "image"]
                                           [packed-id "text/html" text]]]
                (let [response (binary id)]
                  (expect (= 200 (:status response)))
                  (expect (= media (get-in response [:headers "Content-Type"])))
                  (expect (= expected (slurp (:body response)))))))
            (finally (swap! registry dissoc (str sid))))
          (doseq [[idx expected] [["0" "image"] ["1" text]]]
            (let [response (#'turns-api/attachment-bytes-handler
                            {:path-params {:sid (str sid) :iid (str iid) :idx idx}})]
              (expect (= 200 (:status response)))
              (expect (= expected (slurp (:body response))))))))))

(defdescribe
  archived-attachment-id-test
  (it "serves an archived upload by its original ID after persistence assigns a new ID"
      (let [db (h/store)
            sid (h/store-session! db {:channel :api})
            old-id (str (random-uuid))
            image {:filename "upload.png" :media-type "image/png" :base64 "aW1hZ2U="}
            tid (str (persistence/db-store-session-turn!
                       db {:parent-session-id sid :user-request "Look" :attachments [image]}))
            durable-id (:id (first (persistence/db-list-turn-attachments db tid)))
            registry @#'state/registry
            route (fn [id]
                    (#'turns-api/turn-attachment-bytes-handler
                     {:path-params {:sid (str sid) :tid tid :aid id}}))]
        (try
          (swap! registry assoc-in [(str sid) :turns tid] {::state/archive :test})
          (with-redefs-fn {#'lp/db-info (constantly db)
                           #'state/read-turn-record (fn [turn]
                                                       (assoc turn :attachments
                                                         [(assoc image :id old-id)]))}
            (fn []
              (let [response (#'turns-api/turn-attachments-handler
                              {:path-params {:sid (str sid) :tid tid}})
                    rows (get (wire/parse-json (:body response)) "attachments")]
                (expect (= [old-id] (mapv #(get % "id") rows))))
              (expect (= "image" (slurp (:body (route durable-id)))))
              (let [old (route old-id)]
                (expect (= 200 (:status old)))
                (expect (= "image" (slurp (:body old)))))))
          (finally (swap! registry dissoc (str sid)))))))

(defdescribe
  offloaded-recording-poll-test
  (it "does not fetch external storage for speech in a metadata poll"
      (let [db (h/store)
            sid (h/store-session! db {:channel :api})
            tid (persistence/db-store-session-turn!
                  db {:parent-session-id sid :user-request "Listen"
                      :attachments [(-> recording (dissoc :base64)
                                        (assoc :storage-uri "fixture://recording"))]})
            reads (atom [])
            engine {:id :recording-test :label "Test" :transcribe (constantly "Remote words")}]
        (with-redefs [lp/db-info (constantly db)
                      at/engine (constantly engine)
                      speech/resolve-engine (constantly engine)
                      at/enabled? (constantly true)
                      at/available? (constantly true)
                      attachment-storage/resolve-bytes (fn [uri] (swap! reads conj uri) nil)]
          (let [response (#'turns-api/turn-attachments-handler
                          {:path-params {:sid (str sid) :tid (str tid)}
                           :query-params {"transcription_only" "true"}})]
            (expect (= at/UNAVAILABLE
                       (get-in (wire/parse-json (:body response))
                               ["attachments" 0 "transcription_status"])))
            (expect (empty? @reads)))))))

(defdescribe
  recording-metadata-poll-test
  (it "loads and hashes only the unresolved recording across repeated polls"
      (let [db (h/store)
            sid (h/store-session! db {:channel :api})
            tid (persistence/db-store-session-turn!
                  db {:parent-session-id sid
                      :user-request "Listen"
                      :attachments [recording
                                    (assoc recording :filename "done.m4a" :transcription "Known")
                                    {:media-type "image/png" :base64 "aW1hZ2U=" :filename "image.png"}]})
            [id] (mapv :id (persistence/db-list-turn-attachments db tid))
            audio-read persistence/db-read-attachment-bytes
            digest @#'at/content-digest
            reads (atom [])
            hashes (atom 0)
            entered (promise)
            finish (promise)
            worker {:id :recording-test
                    :label "Test recording"
                    :transcribe (fn [_]
                                  (deliver entered true)
                                  (deref finish 5000 nil)
                                  "New words")}
            poll (fn []
                   (get (wire/parse-json
                          (:body (#'turns-api/turn-attachments-handler
                                  {:path-params {:sid (str sid) :tid (str tid)}
                                   :query-params {"transcription_only" "true"}})))
                        "attachments"))]
        (at/clear-cache!)
        (try
          (with-redefs-fn {#'lp/db-info (constantly db)
                           #'at/engine (constantly worker)
                           #'speech/resolve-engine (constantly worker)
                           #'at/enabled? (constantly true)
                           #'at/available? (constantly true)
                           #'at/content-digest (fn [att] (swap! hashes inc) (digest att))
                           #'persistence/db-read-attachment-bytes
                           (fn [db id]
                             (swap! reads conj id)
                             (audio-read db id))}
            (fn []
              (expect (= "pending" (get-in (poll) [0 "transcription_status"])))
              (expect (true? (deref entered 5000 false)))
              (let [[pending known image] (poll)]
                (expect (= "pending" (get pending "transcription_status")))
                (expect (= "Known" (get known "transcription")))
                (expect (every? #(not (contains? % "base64")) [pending known image])))
              (deliver finish true)
              (loop [attempts 200]
                (when (and (pos? attempts) (not= "New words" (get-in (poll) [0 "transcription"])))
                  (Thread/sleep 10)
                  (recur (dec attempts))))
              (expect (= [id] @reads))
              (expect (= 1 @hashes))
              (expect (= "New words" (:transcription (persistence/db-read-attachment db id))))))
          (finally
            (deliver finish true)
            (at/clear-cache!))))))

(defdescribe
  inline-path-image-poll-test
  (it "keeps inline image bytes when the row has no binary route"
      (let [sid (random-uuid)
            image {:filename "path.png" :media-type "image/png" :base64 "aW1hZ2U="}
            rows [image (assoc image :id "stored-id" :filename "stored.png")]]
        (with-redefs [state/turn-attachments (constantly (wire/canonical rows))]
          (let [response (#'turns-api/turn-attachments-handler
                          {:path-params {:sid (str sid) :tid "turn"}
                           :query-params {"transcription_only" "true"}})
                [path stored] (get (wire/parse-json (:body response)) "attachments")]
            (expect (= 200 (:status response)))
            (expect (= "aW1hZ2U=" (get path "base64")))
            (expect (= "stored-id" (get stored "id")))
            (expect (not (contains? stored "base64"))))))))

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
                      (with-meta [(second rows)] {::state/stored? true}))

                    at/request-attachments!
                    (fn [rows & _]
                      (mapv #(assoc % :transcription "second words") rows))]

        (#'turns-api/turn-attachments-handler
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
