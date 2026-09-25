(ns com.blockether.vis.internal.loop-memory-test
  (:require [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.loop.environment :as loop-env]
            [com.blockether.vis.internal.loop.iteration :as iteration]
            [com.blockether.vis.internal.loop.transcript :as transcript]
            [com.blockether.vis.internal.content :as content]
            [com.blockether.vis.internal.persistance.core :as db]
            [com.blockether.vis.internal.attachment.storage :as storage]
            [com.blockether.vis.internal.attachment.core :as attachments]
            [lazytest.core :refer [defdescribe it expect]]))

(defdescribe folded-carry-retention-test
             (it "releases folded payloads rather than only hiding their forms from the wire"
                 (let [payload
                       (apply str (repeat 10000 "retained"))

                       record
                       {:forms-vec [{:scope "t1/i1/f1" :stdout payload}]
                        :blocks [{:stdout payload}]
                        :thinking payload
                        :attachments [{:base64 payload}]
                        :reinspect-attachments [{:base64 payload}]
                        :assistant-message {:role "assistant" :content payload}
                        :tool-calls [{:arguments payload}]
                        :council-input {:content payload}
                        :ctx-diff payload
                        :goal-continuation payload}

                       summaries
                       [{"through" "t1/i1" "at_turn" 1 "gist" "settled"}]

                       compacted
                       (#'transcript/apply-summaries [[1 record]] summaries)

                       kept
                       (second (first compacted))]

                   (expect (= "t1/i1" (:iteration-scope kept)))
                   (expect (not-any? #(contains? kept %)
                                     [:blocks :thinking :attachments :reinspect-attachments
                                      :assistant-message :tool-calls :council-input :ctx-diff
                                      :goal-continuation]))
                   (expect (= compacted (#'transcript/apply-summaries compacted summaries)))))
             (it "keeps visible provider-native replay and forms unchanged"
                 (let [record
                       {:forms-vec [{:scope "t1/i2/f1" :stdout "answer"}]
                        :assistant-message {:role "assistant"
                                            :content [{:type "thinking" :signature "opaque"}]}
                        :tool-calls [{:id "call"}]}

                       result
                       (#'transcript/apply-summaries
                        [[2 record]]
                        [{"through" "t1/i1" "at_turn" 1 "gist" "settled"}])]

                   (expect (= record (second (first result)))))))

(defdescribe disk-trace-retention-test
             (it "retains offsets during a turn and reconstructs exact terminal trace from disk"
                 (let [payload
                       (apply str (repeat 10000 "trace-body"))

                       entry
                       {:iteration 3
                        :blocks [{:stdout payload :error {:message "failed"}}]
                        :thinking payload
                        :final? false}

                       result
                       (#'iteration/with-trace-store
                        (fn [journal]
                          (let [a
                                (#'iteration/store-trace! journal entry)

                                b
                                (#'iteration/store-trace! journal {:iteration 4 :final? true})]

                            (expect (and (integer? a) (integer? b) (< a b)))
                            {:trace [a b] :answer "done"})))]

                   (expect (= {:trace [entry {:iteration 4 :final? true}] :answer "done"} result))))
             (it "closes the journal on successful and exceptional exits"
                 (doseq [fail? [false true]]
                   (let [journal-ref (atom nil)]
                     (try (#'iteration/with-trace-store
                           (fn [journal]
                             (reset! journal-ref journal)
                             (if fail? (throw (ex-info "fixture" {})) {:trace []})))
                          (catch clojure.lang.ExceptionInfo _ nil))
                     (expect (try (.length ^java.io.RandomAccessFile @journal-ref)
                                  false
                                  (catch java.io.IOException _ true))))))
             (it "keeps the transient terminal entry exact even with native error callbacks"
                 (let [callback
                       (fn []
                         :native)

                       terminal
                       {:iteration 2 :error {:data {:on-chunk callback}}}

                       result
                       (#'iteration/with-trace-store
                        (fn [journal]
                          {:trace [(#'iteration/store-trace! journal {:iteration 1}) terminal]}))]

                   (expect (= [{:iteration 1} terminal] (:trace result)))
                   (expect (identical? callback
                                       (get-in result [:trace 1 :error :data :on-chunk]))))))

(defdescribe
  metadata-first-seed-test
  (it
    "loads only visible incomplete bodies, no completed or folded bytes"
    (let [body-reads
          (atom [])

          artifact-reads
          (atom [])

          turns
          [{:id "done" :position 1 :status :done} {:id "error" :position 2 :status :error}
           {:id "current" :position 3 :status :running}]

          metadata
          {"done" [{:id "a" :position 1 :status :done}]
           "error" [{:id "b" :position 1 :status :done} {:id "c" :position 2 :status :done}
                    {:id "d" :position 3 :status :error}]}

          summaries
          [{"through" "t2/i1" "at_turn" 3 "gist" "settled"}]]

      (with-redefs [db/db-list-session-turns-meta
                    (fn [& _]
                      turns)

                    db/db-list-session-turns-iterations-meta
                    (fn [_ ids]
                      (expect (= ["done" "error"] (vec ids)))
                      metadata)

                    db/db-list-iterations
                    (fn [_ ids]
                      (reset! body-reads (vec ids))
                      {"c" {:forms [{:scope "t2/i2/f1" :stdout "kept"}]}})

                    db/db-list-iterations-attachments-meta
                    (fn [_ ids]
                      (reset! artifact-reads (vec ids))
                      {"c" [{:id "image" :media-type "image/png" :size 9}]})

                    db/db-list-session-turns-iterations
                    (fn [& _]
                      (throw (ex-info "raw scan" {})))

                    db/db-list-iterations-attachments
                    (fn [& _]
                      (throw (ex-info "byte scan" {})))

                    storage/hydrate
                    (fn [& _]
                      (throw (ex-info "eager hydration" {})))]

        (let [entries
              (#'iteration/seed-trailer-iters
               {:session-id "s" :db-info :fixture}
               "current"
               summaries)

              visible
              (second (last entries))

              context
              (atom {})]

          (expect (= ["c"] @body-reads @artifact-reads))
          (expect (= [{:scope "t2/i2/f1" :stdout "kept"}] (:forms-vec visible)))
          (expect (false? (:preserved-thinking/replay? visible)))
          (expect (= 2 (count (filter (comp :collapsed? second) entries))))
          (#'transcript/stamp-iter-universe! context entries entries)
          (expect (= ["t1/i1" "t2/i1" "t2/i2"] (get @context "engine_iter_universe")))
          (expect (zero? (get-in @context ["engine_iter_weights" "t1/i1"])))))))
  (it "does not read bodies from completed turns and supports sessions without persistence"
      (with-redefs [db/db-list-session-turns-meta
                    (fn [& _]
                      [{:id "done" :position 1 :status :done}])

                    db/db-list-session-turns-iterations-meta
                    (fn [& _]
                      {"done" [{:id "a" :position 1 :status :done}]})

                    db/db-list-iterations
                    (fn [_ ids]
                      (expect (empty? ids))
                      {})

                    db/db-list-iterations-attachments-meta
                    (fn [& _]
                      {})]

        (expect (nil? (#'iteration/seed-trailer-iters {} nil [])))
        (expect (= "t1/i1"
                   (get-in (#'iteration/seed-trailer-iters {:session-id "s"} nil [])
                           [0 1 :iteration-scope]))))))

(defdescribe
  metadata-first-image-budget-test
  (it
    "reads and hydrates only budget-selected metadata, never folded or non-image artifacts"
    (let [reads
          (atom [])

          hydrated
          (atom [])

          entries
          (mapv (fn [n]
                  [n
                   {:attachments
                    [{:id n :media-type "image/png" :size 3 ::transcript/attachment-db :fixture}]}])
                (range 30))]

      (with-redefs [db/db-read-attachment
                    (fn [_ id]
                      (swap! reads conj id)
                      {:id id :media-type "image/png" :base64 "AAAA"})

                    storage/hydrate
                    (fn [image]
                      (swap! hydrated conj (:id image))
                      image)

                    attachments/wire-image
                    identity]

        (let [plan (#'transcript/replay-image-plan
                    (into entries
                          [[30 {:collapsed? true :attachments [{:id "folded"}]}]
                           [31 {:attachments [{:id "text" :media-type "text/plain"}]}]]))]
          (expect (= (vec (range 29 13 -1)) @reads @hydrated))
          (expect (= 16 (count (mapcat :images (vals plan)))))
          (expect (= 14 (count (mapcat :dropped (vals plan)))))
          (expect (every? #(not (contains? % :base64)) (mapcat :dropped (vals plan)))))))))

(defdescribe
  active-fold-memory-test
  (it
    "releases folded active carry before the next request while disk trace and DB stay exact"
    (let [router
          (svar/make-router [{:id :lmstudio
                              :base-url "http://127.0.0.1:1234/v1"
                              :api-key "test"
                              :models [{:name "model" :input-limit 1000000}]}])

          env
          (loop-env/create-environment router {:db :memory})

          store
          (:db-info env)

          tid
          (db/db-store-session-turn! store
                                     {:parent-session-id (:session-id env)
                                      :user-request "fold large output"})

          requests
          (atom 0)

          carry-verdicts
          (atom [])

          offsets
          (atom [])

          stamp
          @#'transcript/stamp-iter-universe!

          spill
          @#'iteration/store-trace!]

      (try
        (let [result
              (with-redefs-fn {#'transcript/stamp-iter-universe!
                               (fn [ctx entries & rest-args]
                                 (doseq [[_ rec]
                                         entries

                                         :when (:collapsed? rec)]

                                   (swap! carry-verdicts conj
                                     (not-any? #(contains? rec %)
                                               [:blocks :thinking :attachments
                                                :assistant-message])))
                                 (apply stamp ctx entries rest-args))
                               #'iteration/store-trace! (fn [journal entry]
                                                          (let [offset (spill journal entry)]
                                                            (swap! offsets conj offset)
                                                            offset))
                               #'svar/ask-code!
                               (fn [_ _]
                                 (let [i
                                       (swap! requests inc)

                                       code
                                       (case i
                                         1
                                         "print('large-payload-' * 10000)"

                                         2
                                         "fold_session('-t1/i1', 'settled output')"

                                         nil)]

                                   (merge {:api-usage {:input-tokens 1000 :output-tokens 1}
                                           :routed/provider-id :lmstudio
                                           :routed/model "model"
                                           :tokens {}}
                                          (if code
                                            {:stop-reason :tool-calls
                                             :tool-calls [{:id (str "call-" i)
                                                           :name "python_execution"
                                                           :input {:code code}}]}
                                            {:stop-reason :end :tool-calls [] :content "done"}))))}
                #(iteration/iteration-loop env "fold large output" {:session-turn-id tid}))

              persisted
              (db/db-list-session-turn-iterations store tid)

              stdout
              (get-in result [:trace 0 :blocks 0 :stdout])]

          (expect (= 3 @requests))
          (expect (and (seq @carry-verdicts) (every? true? @carry-verdicts)))
          (expect (and (= 2 (count @offsets)) (every? integer? @offsets)))
          (expect (= (str (apply str (repeat 10000 "large-payload-")) "\n") stdout))
          (expect (= stdout (get-in persisted [0 :forms 0 :stdout]))))
        (finally (loop-env/dispose-environment! env))))))

(defdescribe
  metadata-first-recap-test
  (it
    "selects unfolded Q/A before reading content, without completed iteration bodies"
    (let [reads
          (atom [])

          body-reads
          (atom [])

          turns
          [{:id "folded" :position 1 :status :done} {:id "visible" :position 2 :status :done}
           {:id "unfinished" :position 3 :status :cancelled}
           {:id "current" :position 4 :status :running}]

          metadata
          {"folded" [{:id "a" :position 1 :status :done}]
           "visible" [{:id "b" :position 1 :status :done}]
           "unfinished" [{:id "c" :position 1 :status :done}]}

          env
          {:session-id "session"
           :db-info :fixture
           :ctx-atom (atom {"session_summaries"
                            [{"through" "t1" "at_turn" 4 "issued_turn" 4 "gist" "settled"}]})}]

      (with-redefs [db/db-list-session-turns-meta
                    (fn [& _]
                      turns)

                    db/db-list-session-turns-iterations-meta
                    (fn [& _]
                      metadata)

                    db/db-list-session-turns
                    (fn [& _]
                      (throw (ex-info "full turns" {})))

                    db/db-list-session-turn-iterations
                    (fn [& _]
                      (throw (ex-info "raw forms" {})))

                    db/db-list-iterations
                    (fn [_ ids]
                      (swap! body-reads into (map str ids))
                      {"c" {:id "c"
                            :forms [{:scope "t3/i1" :src "grep({\"query\": [\"cancel\"]})"}]}})

                    db/db-list-iterations-attachments-meta
                    (fn [& _]
                      {})

                    db/db-read-session-turn
                    (fn [_ _ id]
                      (swap! reads conj id)
                      (expect (not= "folded" id))
                      (assoc (first (filter #(= id (:id %)) turns))
                        :user-request "question"
                        :content [(content/prose "answer")]))]

        (let [result (#'transcript/previous-turn-context env "current")]
          (expect (= ["visible" "unfinished"] @reads))
          ;; ONLY the cancelled turn's iterations: an answered turn's recap still
          ;; costs nothing but its stored-iteration line.
          (expect (= ["c"] @body-reads))
          (expect (= [2 3] (mapv :turn result)))
          (expect (= [{:scope "t2/i1" :src "t2/i1 (stored iteration)"}] (:results (first result))))
          (expect (= [{:scope "t3/i1" :src "grep({\"query\": [\"cancel\"]})"}]
                     (:results (second result))))
          (expect (true? (:cancelled? (second result)))))))))

(defdescribe
  local-command-metadata-test
  (it
    "validates only local command candidates and excludes confirmed slash turns"
    (let [reads
          (atom [])

          metadata
          {"slash" [{:id "local" :position 1 :status :done :local-command-candidate? true}]
           "provider" [{:id "remote"
                        :position 1
                        :status :done
                        :provider :fixture
                        :local-command-candidate? false}]}]

      (with-redefs [db/db-list-session-turns-meta
                    (fn [& _]
                      [{:id "slash" :position 1 :status :done}
                       {:id "provider" :position 2 :status :done}])

                    db/db-list-session-turns-iterations-meta
                    (fn [& _]
                      metadata)

                    db/db-list-iterations
                    (fn [_ ids]
                      (swap! reads into ids)
                      (expect (every? #{"local"} ids))
                      (if (seq ids) {"local" {:forms [{:tag :user-slash :src "/help"}]}} {}))

                    db/db-list-iterations-attachments-meta
                    (fn [& _]
                      {})]

        (let [entries (#'iteration/seed-trailer-iters {:session-id "s"} nil [])]
          (expect (= ["local"] @reads))
          (expect (= ["t2/i1"] (mapv (comp :iteration-scope second) entries))))))))
