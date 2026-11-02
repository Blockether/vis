(ns com.blockether.vis.internal.anthropic-threads-test
  "Anthropic continuation across the Vis and Svar boundary."
  (:require [babashka.http-client :as http]
            [charred.api :as json]
            [clojure.walk :as walk]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.loop.transcript :as transcript]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- router
  [body]
  (svar/make-router [{:id :anthropic
                      :api-key "test-key"
                      :base-url "https://example.invalid/v1"
                      :extra-body body
                      :models [{:name "claude-sonnet-4-6" :context 200000}]}]))

(defn- fixture
  [seen]
  (fn [_url opts]
    (let [request
          (json/read-json (:body opts) :key-fn keyword)

          n
          (count (swap! seen conj request))]

      {:status 200
       :headers {}
       :body (json/write-json-str {:id (str "msg_" n)
                                   :type "message"
                                   :role "assistant"
                                   :content [{:type "text" :text (str "answer" n)}]
                                   :stop_reason "end_turn"
                                   :usage {:input_tokens 10 :output_tokens 2}})})))

(defn- cache-markers
  [body]
  (let [markers (atom [])]
    (walk/postwalk (fn [value]
                     (when (and (map? value) (:cache_control value))
                       (swap! markers conj (:cache_control value)))
                     value)
                   body)
    @markers))

;; User request: use Message Threads in Vis without coupling sessions to prompt caching.
(defdescribe
  anthropic-vis-thread-test
  (it
    "continues cached turns and closes the thread when the call opts out"
    (let [seen
          (atom [])

          session-atom
          (atom nil)

          environment
          {:router (router {:thread {:type "create"}}) :llm-session-atom session-atom}

          resolved
          {:provider :anthropic :name "claude-sonnet-4-6"}

          initial
          [(svar/system "Stable instructions.") (svar/user "one")]]

      (with-redefs [http/post (fixture seen)]
        (try
          (let [first-result
                (transcript/ask-code-with-session! environment resolved {:messages initial})
                history (conj initial (:assistant-message first-result) (svar/user "two"))
                second-result
                (transcript/ask-code-with-session! environment resolved {:messages history})
                third-history (conj history (:assistant-message second-result) (svar/user "three"))
                third-result
                (transcript/ask-code-with-session! environment resolved {:messages third-history})
                next-history
                (conj third-history (:assistant-message third-result) (svar/user "four"))
                disabled (transcript/ask-code-with-session! environment
                                                            resolved
                                                            {:messages next-history
                                                             :extra-body {:thread nil}})]

            (expect (= ["answer1" "answer2" "answer3" "answer4"]
                       (mapv :content [first-result second-result third-result disabled])))
            (expect (= ["create" "continue" "continue"]
                       (mapv #(get-in % [:thread :type]) (take 3 @seen))))
            (expect (= [1 1] (mapv #(count (:messages %)) (take 2 (rest @seen)))))
            (expect (every? seq (map cache-markers (take 3 @seen))))
            (expect (every? #(<= (count (cache-markers %)) 4) @seen))
            (expect (nil? (:thread (last @seen))))
            (expect (= 7 (count (:messages (last @seen)))))
            (expect (nil? @session-atom)))
          (finally (transcript/close-llm-session! session-atom))))))
  (it "keeps Anthropic unthreaded without explicit opt-in"
      (let [seen
            (atom [])

            session-atom
            (atom nil)]

        (with-redefs [http/post (fixture seen)]
          (let [result (transcript/ask-code-with-session!
                         {:router (router {}) :llm-session-atom session-atom}
                         {:provider :anthropic :name "claude-sonnet-4-6"}
                         {:messages [(svar/user "one")]})]
            (expect (= "answer1" (:content result)))
            (expect (nil? (:thread (first @seen))))
            (expect (nil? @session-atom)))))))
