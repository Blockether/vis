(ns com.blockether.vis.internal.loop-cache-memory-test
  (:require [com.blockether.vis.internal.loop.environment :as loop-env]
            [com.blockether.vis.internal.loop.transcript :as transcript]
            [com.blockether.vis.internal.persistance.core :as persistance]
            [com.blockether.vis.internal.util :as util]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(deftest request-cache-does-not-retain-message-payloads-test
  (let [history
        (atom {})

        text
        (apply str (repeat 100000 "payload"))]

    (#'transcript/note-prompt-cache-request!
     history
     :openai
     "model"
     {:id "cache-context" :fixed-prefix-weight 0}
     [{:role "user" :content text}]
     1000
     0
     1000)
    (is (not-any? #(and (string? %) (= text %)) (tree-seq coll? seq @history)))
    (is (nil? (get-in @history [[:openai "model"] :messages])))))

(deftest request-cache-bounds-route-history-test
  (let [history (atom {})]
    (dotimes [i 100]
      (#'transcript/note-prompt-cache-request!
       history
       :openai
       (str "model-" i)
       {:id "cache-context" :fixed-prefix-weight 0}
       [{:role "user" :content "hello"}]
       1000
       0
       1000))
    (is (<= (count @history) 8))))

(deftest request-cache-weak-hints-are-optional-test
  (let [history
        (atom {})

        context
        {:id "cache-context" :fixed-prefix-weight 0}

        messages
        [{:role "system" :content "stable"} {:role "user" :content "question"}]]

    (#'transcript/note-prompt-cache-request! history :openai "model" context messages 1000 0 1000)
    (doseq [reference (get-in @history [[:openai "model"] :message-refs])]
      (.clear ^java.lang.ref.WeakReference reference))
    (let [sample (#'transcript/note-prompt-cache-request!
                  history
                  :openai
                  "model"
                  context
                  (conj messages {:role "assistant" :content "answer"})
                  1100
                  500
                  1001)]
      (is (= :append-only (:continuity sample)))
      (is (= 1000 (:reusable-tokens sample))))))

(deftest request-cache-retains-expiry-denominator-across-route-switches-test
  (let [history
        (atom {})

        context
        {:id "cache-context" :fixed-prefix-weight 0}

        messages
        [{:role "user" :content "question"}]]

    (#'transcript/note-prompt-cache-request! history :openai "old" context messages 1000 0 1)
    (#'transcript/note-prompt-cache-request! history :openai "new" context messages 1000 0 400000)
    (let [sample (#'transcript/note-prompt-cache-request!
                  history
                  :openai
                  "old"
                  context
                  messages
                  1100
                  0
                  400001)]
      (is (= :expired (:continuity sample)))
      (is (= 1000 (:reusable-tokens sample))))))

(deftest request-cache-message-budget-does-not-disable-disk-checkpoints-test
  (let [history
        (atom {})

        messages
        (vec (repeat 5 {:role "user" :content "small"}))]

    (with-redefs-fn {#'transcript/PROMPT_CACHE_MESSAGE_LIMIT 2}
      (fn []
        (#'transcript/note-prompt-cache-request!
         history
         :openai
         "model"
         {:id "cache-context" :fixed-prefix-weight 0}
         messages
         1000
         0
         1000)
        (let [entry
              (get @history [:openai "model"])

              completed
              (#'transcript/completed-prompt-cache-entry
               entry
               messages
               1
               []
               1
               {:role "assistant" :content "answer"})]

          (is (empty? (:fingerprints entry)))
          (is (= messages (:messages completed)))
          (is (= 5 (count (:weights completed))))
          (is (empty? (:fingerprints (#'transcript/compact-prompt-cache-entry completed)))))))))

(deftest exact-checkpoint-survives-restart-without-live-payload-retention-test
  (let [dir
        (.toFile (java.nio.file.Files/createTempDirectory
                   "vis-cache-memory"
                   (make-array java.nio.file.attribute.FileAttribute 0)))

        db-path
        (.getPath (java.io.File. dir "vis.mdb"))

        route
        [:openai "model"]

        context
        {:id "cache-context" :fixed-prefix-weight 0}

        stable
        [{:role "system" :content "stable"}]

        request
        (conj stable
              {:role "user" :content "question"}
              {:role "assistant"
               :content
               [{:type "thinking" :thinking "reasoning" :signature "exact-provider-signature"}]})

        answer
        {:role "assistant" :content "answer"}

        session-id
        (atom nil)]

    (try
      (let [environment (loop-env/create-environment ::router {:db db-path})]
        (try (reset! session-id (:session-id environment))
             (reset! (:standing-ctx-atom environment) {:block "standing" :baseline {}})
             (#'transcript/note-prompt-cache-request!
              (:prompt-cache-history-atom environment)
              :openai
              "model"
              context
              request
              1000
              0
              (util/now-ms))
             (#'transcript/persist-prompt-cache-state!
              environment
              :openai
              "model"
              {:messages request
               :turn-position 1
               :summaries []
               :stable-message-count 1
               :assistant-message answer})
             (is (nil? (get-in @(:prompt-cache-history-atom environment) [route :messages])))
             (is (nil? (get-in @(:prompt-cache-history-atom environment) [route :completed-turn])))
             (finally (loop-env/dispose-environment! environment))))
      (let [environment (loop-env/create-environment ::router {:db db-path :session @session-id})]
        (try (let [history @(:prompt-cache-history-atom environment)
                   state (#'transcript/load-prompt-cache-state
                          (:db-info environment)
                          (:session/state-id environment))
                   user [{:role "user" :content "follow-up"}]
                   base (#'transcript/resumable-prompt-message-base
                         state
                         :openai
                         "model"
                         context
                         2
                         []
                         stable
                         user)]

               (is (nil? (get-in history [route :messages])))
               (is (nil? (get-in history [route :completed-turn])))
               (is (= (into (conj request answer) user) (:messages base)))
               (persistance/db-set-session-prompt-cache-state! (:db-info environment)
                                                               (:session/state-id environment)
                                                               nil)
               (is (nil? (#'transcript/load-prompt-cache-state
                          (:db-info environment)
                          (:session/state-id environment)))))
             (finally (loop-env/dispose-environment! environment))))
      (finally (doseq [file (reverse (file-seq dir))]
                 (.delete ^java.io.File file))))))
