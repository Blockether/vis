(ns com.blockether.vis.internal.persistance.sqlite.memory-test
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.persistance.core :as persistence]
            [com.blockether.vis.internal.persistance.sqlite.core :as sqlite]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [lazytest.core :refer [defdescribe it expect]]
            [next.jdbc :as jdbc]))

(h/use-mem-store!)

(defdescribe
  selective-iteration-readers-test
  (it
    "selects latest iteration metadata without reading any payload column or thawing blobs"
    (let [s
          (h/store)

          sid
          (h/store-session! s {:channel :api})

          tid
          (vis/db-store-session-turn! s {:parent-session-id sid :user-request "history"})

          empty-tid
          (vis/db-store-session-turn! s {:parent-session-id sid :user-request "empty"})

          payload
          (apply str (repeat 10000 "history"))

          store!
          #(h/store-iteration! s
                               {:session-turn-id tid
                                :code payload
                                :stdout %
                                :thinking payload
                                :llm-routing {:actual {:provider :fixture :model "fixture-model"}}})

          obsolete
          (store! "obsolete")

          _
          (vis/db-retry-session-turn! s tid {:status :running})

          current
          [(store! "first") (store! "second")]

          f
          persistence/db-list-session-turns-iterations-meta

          statements
          (atom [])

          execute!
          jdbc/execute!]

      (expect (some? f))
      (when f
        (let [actual
              (with-redefs-fn {#'sqlite/<-blob (fn [_]
                                                 (throw (ex-info "metadata thawed a blob" {})))
                               #'jdbc/execute! (fn [db statement & options]
                                                 (swap! statements conj statement)
                                                 (apply execute! db statement options))}
                #(f s [tid empty-tid nil (str tid) "missing"]))

              rows
              (get actual (str tid))]

          (expect (= current (mapv :id rows)))
          (expect (not-any? #{obsolete} (map :id rows)))
          (expect (= [1 2] (mapv :position rows)))
          (expect (= [:done :done] (mapv :status rows)))
          (expect (= [:fixture :fixture] (mapv :provider rows)))
          (expect (= ["fixture-model" "fixture-model"] (mapv :model rows)))
          (expect (every? #(= #{:id :position :status :created-at :provider :model
                                :local-command-candidate?}
                              (set (keys %)))
                          rows))
          (expect (= [] (get actual (str empty-tid))))
          (expect (= [] (get actual "missing")))
          (expect (= 1 (count @statements)))
          (expect (not (re-find #"(?i)tool_calls|llm_thinking|council|SELECT i\.\*"
                                (ffirst @statements))))))))
  (it
    "loads exact chosen payloads, including old states, without loading unrequested iterations"
    (let [s
          (h/store)

          sid
          (h/store-session! s {:channel :api})

          tid
          (vis/db-store-session-turn! s {:parent-session-id sid :user-request "history"})

          forms
          [{:scope "t1/i1/f1" :src "print(1)" :stdout "full original output"}]

          first-id
          (vis/db-store-iteration!
            s
            {:session-turn-id tid :code "print(1)" :forms forms :thinking "reasoning"})

          _
          (vis/db-retry-session-turn! s tid {:status :running})

          second-id
          (h/store-iteration! s {:session-turn-id tid :code "print(2)" :stdout "next"})

          f
          persistence/db-list-iterations]

      (expect (some? f))
      (when f
        (let [actual (f s [first-id nil (str first-id) "missing"])]
          (expect (= #{(str first-id)} (set (keys actual))))
          (expect (= forms (:forms (get actual (str first-id)))))
          (expect (= "reasoning" (:thinking (get actual (str first-id)))))
          (expect (= "print(1)" (:code (get actual (str first-id)))))
          (expect (not (contains? actual (str second-id))))))))
  (it "does no work without ids or a datasource, and binds bounded batches"
      (doseq [f [persistence/db-list-session-turns-iterations-meta persistence/db-list-iterations]]
        (let [s (h/store)
              statements (atom [])
              execute! jdbc/execute!
              ids (mapv #(str "missing-" %) (range 1001))
              unsafe "missing' OR 1=1 --"]

          (expect (some? f))
          (when f
            (with-redefs [jdbc/execute! (fn [db statement & options]
                                          (swap! statements conj statement)
                                          (apply execute! db statement options))]
              (expect (= {} (f s [nil])))
              (expect (= {} (f nil ["unused"])))
              (expect (empty? @statements))
              (f s (conj ids unsafe))
              (expect (= 3 (count @statements)))
              (expect (every? #(<= (count (rest %)) 500) @statements))
              (expect (= (conj ids unsafe) (vec (mapcat rest @statements))))
              (expect (not-any? #(str/includes? (first %) unsafe) @statements)))))))
  (it
    "reads forked iteration identities without reintroducing source or post-fork rows"
    (let [s
          (h/store)

          sid
          (h/store-session! s {:channel :api})

          tid
          (vis/db-store-session-turn! s {:parent-session-id sid :user-request "fork point"})

          source-id
          (h/store-iteration! s {:session-turn-id tid :code "print(1)" :stdout "copied"})

          fork-id
          (h/fork-session-at-turn! s sid {:through-turn-id tid})

          fork-turn-id
          (:id (first (vis/db-list-session-turns s fork-id)))

          f
          persistence/db-list-session-turns-iterations-meta

          payload-f
          persistence/db-list-iterations]

      (expect (and f payload-f))
      (when (and f payload-f)
        (let [rows
              (get (f s [fork-turn-id]) (str fork-turn-id))

              copied-id
              (:id (first rows))]

          (expect (= 1 (count rows)))
          (expect (not= source-id copied-id))
          (expect (= "copied"
                     (get-in (payload-f s [copied-id]) [(str copied-id) :forms 0 :stdout]))))))))

(defdescribe
  selective-turn-metadata-test
  (it
    "preserves latest and fork ordering without selecting turn payloads"
    (let [s
          (h/store)

          sid
          (h/store-session! s {:channel :api})

          tid
          (vis/db-store-session-turn! s {:parent-session-id sid :user-request "private payload"})

          _
          (h/store-iteration! s {:session-turn-id tid :code "print(1)" :stdout "payload"})

          _
          (vis/db-retry-session-turn! s tid {:status :running})

          fork-id
          (h/fork-session-at-turn! s sid {:through-turn-id tid})

          f
          (ns-resolve 'com.blockether.vis.internal.persistance.core 'db-list-session-turns-meta)

          execute!
          jdbc/execute!

          statements
          (atom [])

          fields
          [:id :position :status :created-at :request-kind]]

      (expect (some? f))
      (when f
        (doseq [session-id [sid fork-id]]
          (let [expected (mapv #(select-keys % fields)
                               (persistence/db-list-session-turns s session-id))
                actual (with-redefs-fn {#'sqlite/<-blob (fn [_]
                                                          (throw (ex-info "metadata thawed" {})))
                                        #'jdbc/execute! (fn [db statement & options]
                                                          (swap! statements conj statement)
                                                          (apply execute! db statement options))}
                         #(f s session-id))]

            (expect (= expected actual))))
        (expect (= [] (f nil sid)))
        (expect (= [] (f s nil)))
        (expect (not-any? #(re-find #"(?i)user_request|content_json|council|tool_calls|qst.error"
                                    (first %))
                          @statements))))))

(defdescribe
  local-command-candidate-test
  (it "marks only unrouted slash-prefixed stored iteration code as a candidate"
      (let [s
            (h/store)

            sid
            (h/store-session! s {:channel :api})

            tid
            (vis/db-store-session-turn! s {:parent-session-id sid :user-request "ordinary text"})]

        (doseq [opts [{:code "/help"} {:code "  /help"}
                      {:code "/help" :llm-routing {:actual {:provider :fixture :model "model"}}}
                      {:code "print(1)"}]]
          (h/store-iteration! s
                              (assoc opts
                                :session-turn-id tid
                                :stdout "payload")))
        (expect (= [true true false false]
                   (mapv :local-command-candidate?
                         (get (persistence/db-list-session-turns-iterations-meta s [tid])
                              (str tid))))))))
