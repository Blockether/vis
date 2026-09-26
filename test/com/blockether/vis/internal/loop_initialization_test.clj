(ns com.blockether.vis.internal.loop-initialization-test
  "Recovery after an interrupted lazy Python sandbox initialization."
  (:require [com.blockether.vis.internal.content :as content]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.loop.environment :as loop-env]
            [com.blockether.vis.internal.loop.iteration :as iteration]
            [com.blockether.vis.internal.loop.transcript :as transcript]
            [com.blockether.vis.internal.loop.turn :as turn]
            [com.blockether.vis.internal.persistance.core :as persistance]
            [com.blockether.vis.internal.python.env :as env]
            [com.blockether.vis.internal.session.titling :as titling]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- interrupted-environment
  []
  (let [entered
        (promise)

        release
        (promise)

        failed
        (promise)

        environment
        {:python-sandbox (atom (delay (deliver entered true) @release))
         :python-context-retired-atom (atom false)}

        builder
        (Thread. ^Runnable
                 (fn []
                   (try (env/sandbox environment) (catch Throwable error (deliver failed error)))))]

    (.start builder)
    (try (expect (= true (deref entered 5000 ::timeout)))
         (.interrupt builder)
         (.join builder 5000)
         (expect (not (.isAlive builder)))
         (expect (instance? InterruptedException (deref failed 5000 ::timeout)))
         (expect (nil? (ex-message @failed)))
         (expect (realized? @(:python-sandbox environment)))
         environment
         (finally (deliver release nil) (.interrupt builder) (.join builder 5000)))))

(defdescribe
  failed-sandbox-inspection-test
  ;; Issue #252: a cancelled initializer caches a nil-message interruption in its Delay.
  (it "treats a failed initializer as absent for teardown and unsafe for another turn"
      (let [environment (interrupted-environment)]
        (expect (nil? (env/sandbox-if-built environment)))
        (expect (nil? (env/python-context-if-built environment)))
        (dotimes [_ 2]
          (expect (false? (env/context-enterable? environment))))
        (expect (not (.isInterrupted (Thread/currentThread))))
        (expect (nil? (env/dispose-sandbox! environment)))
        (expect (true? @(:python-context-retired-atom environment)))))
  (it "keeps an unstarted sandbox cold and a completed sandbox available"
      (let [cold
            {:python-sandbox (atom (delay (throw (ex-info "must stay cold" {}))))}

            built
            {:python-context "initialized"}

            pending
            (delay built)]

        (expect (env/context-enterable? cold))
        (expect (not (realized? @(:python-sandbox cold))))
        (expect (= built @pending))
        (expect (= built (env/sandbox-if-built {:python-sandbox (atom pending)})))
        (expect (= "initialized" (env/python-context-if-built {:python-sandbox (atom pending)})))))
  (it "does not retry a failed initializer when entering Python directly"
      (let [error
            (ex-info "initialization failed" {})

            environment
            {:python-sandbox (atom (delay (throw error)))}]

        (dotimes [_ 2]
          (expect (identical? error
                              (try (env/sandbox environment) (catch Throwable caught caught)))))
        (expect (false? (env/context-enterable? environment)))
        (expect (nil? (env/sandbox-if-built environment))))))

(defdescribe
  interrupted-sandbox-turn-recovery-test
  ;; Issue #252: both later requests used to rethrow before the first iteration.
  (it
    "rebuilds once and completes both later submissions in the same session"
    (let [id
          (str (random-uuid))

          k
          (#'loop-env/cache-key id)

          cache
          @#'loop-env/cache

          entry
          (#'loop-env/new-cache-entry (interrupted-environment))

          fresh-env
          {:db-info ::db
           :session-id id
           :router {:providers [{:id :openai-codex :models [{:name "fixture-model"}]}]}
           :python-sandbox (atom (delay {:python-context (str "recovered-" id)}))
           :python-context-retired-atom (atom false)}

          answer
          [(content/prose "Recovered")]

          opened
          (atom 0)

          iterations
          (atom 0)

          writes
          (atom [])]

      (swap! cache assoc k entry)
      (try
        (with-redefs-fn {#'loop-env/open-env! (fn [_ _]
                                                (swap! opened inc)
                                                fresh-env)
                         #'loop-env/ensure-env-reaper! (constantly nil)
                         #'turn/turn! (fn [environment _ opts]
                                        (#'turn/run-normal-turn! environment "continue" opts))
                         #'iteration/iteration-loop
                         (fn [environment _ _]
                           (expect (identical? fresh-env environment))
                           (expect (some? (env/python-context environment)))
                           (swap! iterations inc)
                           {:status :success :answer answer :iteration-count 1 :duration-ms 1})
                         #'transcript/session-turn-position (fn [& _]
                                                              (inc @iterations))
                         #'persistance/db-store-session-turn! (fn [& _]
                                                                (str (random-uuid)))
                         #'persistance/db-update-session-turn! (fn [_ _ opts]
                                                                 (swap! writes conj opts)
                                                                 true)
                         #'titling/maybe-auto-title! (constantly nil)
                         #'titling/after-turn-auto-title! (constantly nil)}
          (fn []
            (dotimes [_ 2]
              (let [result (lp/send! id "continue")]
                (expect (= :success (:status result)))
                (expect (= :complete (:prior-outcome result)))
                (expect (= answer (:answer result)))))
            (expect (= 1 @opened))
            (expect (= 2 @iterations))
            (expect (= 2 (count @writes)))
            (expect (every? #(= :success (:status %)) @writes))
            (expect (every? #(= :complete (:prior-outcome %)) @writes))
            (expect (every? #(= answer (:content %)) @writes))
            (expect (not (identical? entry (get @cache k))))
            (expect (not (.isLocked ^java.util.concurrent.locks.ReentrantLock
                                    (:lock (get @cache k)))))))
        (finally (swap! cache dissoc k))))))
