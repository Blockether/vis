(ns com.blockether.vis.internal.python-worker-test
  "The session-worker process boundary: control messages are bounded and a
   retired interpreter can never be entered again."
  (:require [com.blockether.vis.internal.env-python :as env]
            [com.blockether.vis.internal.loop :as loop]
            [com.blockether.vis.internal.python-worker :as worker]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.util.concurrent.atomic AtomicLong)))

(defdescribe
  worker-control-plane-test
  (it "bounds an interrupt whose child never replies"
      (let [pending
            (atom {})

            peer
            {:pending pending :seq (AtomicLong. 0)}]

        (with-redefs-fn {#'worker/live (fn [_]
                                         {:peer peer})
                         #'worker/send-line! (fn [_ _]
                                               nil)}
          (fn []
            (let [call
                  (future (try (worker/interrupt! "session" "sandbox")
                               (catch clojure.lang.ExceptionInfo e (ex-data e))))

                  observed
                  (deref call 1500 ::blocked)]

              ;; Release the old, unbounded implementation so a failing run leaves
              ;; no parked future behind.
              (doseq [[_ waiting] @pending]
                (deliver waiting {"value" false}))
              (deref call 1000 nil)
              (expect (= :vis/python-worker-timeout (:type observed))))))))
  (it "refuses an environment whose worker was retired"
      (expect (false? (env/context-enterable? {:python-context "retired-session"
                                               :python-context-retired-atom (atom true)}))))
  (it "kills only a worker whose interrupt control plane failed"
      (let [retired
            (atom false)

            stopped
            (atom [])

            release-task
            (promise)

            exec-task
            (future @release-task)]

        (try (with-redefs-fn {#'env/interrupt-guest! (fn [_]
                                                       (throw (ex-info "worker unavailable" {})))
                              #'env/retire-python-context! (fn [session]
                                                             (swap! stopped conj session))}
               (fn []
                 (expect (false? ((deref #'loop/interrupt-block!)
                                   "broken-session"
                                   exec-task
                                   {:python-context-retired-atom retired})))
                 (expect (true? @retired))
                 (expect (= ["broken-session"] @stopped))))
             (finally (deliver release-task true)))))
  (it "retires a worker that accepts an interrupt but never unwinds"
      (let [retired
            (atom false)

            stopped
            (promise)

            release-task
            (promise)

            exec-task
            (future @release-task)]

        (try (with-redefs-fn {#'loop/INTERRUPT_UNWIND_MS 25
                              #'env/interrupt-guest! (fn [_]
                                                       true)
                              #'env/retire-python-context! (fn [session]
                                                             (deliver stopped session))}
               (fn []
                 (expect (true? ((deref #'loop/interrupt-block!)
                                  "stuck-native-session"
                                  exec-task
                                  {:python-context-retired-atom retired})))
                 (expect (= "stuck-native-session" (deref stopped 1000 ::not-stopped)))
                 (expect (true? @retired))))
             (finally (deliver release-task true)))))
  (it "includes every worker process in the runtime RSS ownership set"
      (let [self (.pid (java.lang.ProcessHandle/current))]
        (with-redefs-fn {#'env/python-worker-pids (fn []
                                                    [self 424242 424242])}
          #(expect (= [self 424242] (vec ((deref #'loop/runtime-pids))))))))
  (it
    "kills the real session process when native sleep does not unwind"
    (let [entered
          (promise)

          made
          (env/create-python-context {(symbol "entered") (fn []
                                                           (deliver entered true))}
                                     (constantly [])
                                     {:worker? true
                                      :jail-enabled? false
                                      :enabled? false
                                      :allowed-domains []
                                      :denied-domains []
                                      :exclude-domains []}
                                     nil)

          session
          (:python-context made)

          ^Process process
          (:process (get @(var-get #'worker/workers) session))

          retired
          (atom false)

          execution
          (future (try (env/run-python-block session "entered()
import time
time.sleep(30)")
                       (catch Throwable _ :stopped)))]

      (try (expect (true? (deref entered 5000 false)))
           (expect (true? ((deref #'loop/interrupt-block!)
                            session
                            execution
                            {:python-context-retired-atom retired})))
           (Thread/sleep 2500)
           (expect (true? @retired))
           (expect (false? (worker/worker-live? session)))
           (expect (false? (.isAlive process)))
           (finally (try (env/dispose-python-context! session) (catch Throwable _ nil))))))
  (it "reclaims a condemned environment's worker before detaching it"
      (let [retired
            (atom false)

            stopped
            (atom [])

            key
            (random-uuid)

            entry
            {:environment {:python-context "abandoned-session"
                           :python-context-retired-atom retired}}]

        (with-redefs-fn {#'loop/cache (atom {key entry})
                         #'env/retire-python-context! (fn [session]
                                                        (swap! stopped conj session))}
          (fn []
            (expect (true? ((deref #'loop/detach-entry!) key entry)))
            (expect (true? @retired))
            (expect (= ["abandoned-session"] @stopped))))))
  (it "keeps a healthy worker when interrupt lost a completion race"
      (let [retired
            (atom false)

            stopped
            (atom [])

            release-task
            (promise)

            exec-task
            (future @release-task)]

        (try (with-redefs-fn {#'env/interrupt-guest! (fn [_]
                                                       false)
                              #'env/retire-python-context! (fn [session]
                                                             (swap! stopped conj session))}
               (fn []
                 (expect (false? ((deref #'loop/interrupt-block!)
                                   "healthy-session"
                                   exec-task
                                   {:python-context-retired-atom retired})))
                 (expect (false? @retired))
                 (expect (empty? @stopped))))
             (finally (deliver release-task true))))))
