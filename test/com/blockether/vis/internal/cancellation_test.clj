(ns com.blockether.vis.internal.cancellation-test
  "Verify the on-cancel! callback contract: registered thunks fire on
   `cancel!`, late registration after cancellation still triggers
   immediately, and `dispose!` removes the hook cleanly without
   side-effects."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.cancellation :as cancellation]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe on-cancel-callback-test
             (it "fires every registered thunk when cancel! is called"
                 (let
                   [token
                    (cancellation/cancellation-token)

                    rang-1
                    (atom 0)

                    rang-2
                    (atom 0)]

                   (cancellation/on-cancel! token #(swap! rang-1 inc))
                   (cancellation/on-cancel! token #(swap! rang-2 inc))
                   (expect (= 0 @rang-1))
                   (expect (= 0 @rang-2))
                   (cancellation/cancel! token)
                   (expect (= 1 @rang-1))
                   (expect (= 1 @rang-2))
                   ;; Idempotent flag: second cancel! does NOT re-fire callbacks
                   ;; that already disposed themselves, but the cooperative flag
                   ;; stays true.
                   (expect (cancellation/cancelled? token))))
             (it "fires immediately when the token was already cancelled"
                 (let
                   [token
                    (cancellation/cancellation-token)

                    rang
                    (atom 0)]

                   (cancellation/cancel! token)
                   (cancellation/on-cancel! token #(swap! rang inc))
                   (expect (= 1 @rang))))
             (it "dispose! removes the hook so a later cancel! does not fire it"
                 (let
                   [token
                    (cancellation/cancellation-token)

                    rang
                    (atom 0)

                    dispose
                    (cancellation/on-cancel! token #(swap! rang inc))]

                   (dispose)
                   (cancellation/cancel! token)
                   (expect (= 0 @rang))))
             (it "isolates a throwing callback from the others"
                 (let
                   [token
                    (cancellation/cancellation-token)

                    rang
                    (atom 0)]

                   (cancellation/on-cancel! token
                                            (fn []
                                              (throw (RuntimeException. "boom"))))
                   (cancellation/on-cancel! token #(swap! rang inc))
                   (cancellation/cancel! token)
                   (expect (= 1 @rang))))
             (it "cancellation-set-future! routes through on-cancel! and cancels the future"
                 (let
                   [token
                    (cancellation/cancellation-token)

                    task
                    (java.util.concurrent.FutureTask. ^java.util.concurrent.Callable
                                                      (reify
                                                        java.util.concurrent.Callable
                                                          (call [_] (Thread/sleep 5000) :done)))

                    thread
                    (Thread. ^Runnable task "vis-cancellation-test")]

                   (.setDaemon thread true)
                   (.start thread)
                   (cancellation/cancellation-set-future! token task)
                   (cancellation/cancel! token)
                   ;; Future was hard-cancelled — isCancelled flips true.
                   (expect (true? (.isCancelled task))))))

(defdescribe cancel-reason-test
             ;; Downstream a cancel is just a thread interrupt, so the token is the only
             ;; place that can remember WHO fired it. Without this the daemon could not
             ;; tell its own stall/shutdown cancel from a user pressing Esc.
             (it "records the reason of the FIRST cancel! and never rewrites it"
                 (let [token (cancellation/cancellation-token)]
                   (expect (nil? (cancellation/cancel-reason token)))
                   (cancellation/cancel! token :stall-watchdog)
                   (expect (= :stall-watchdog (cancellation/cancel-reason token)))
                   ;; A shutdown sweep landing on an already-cancelled turn must not
                   ;; overwrite the origin that actually stopped it.
                   (cancellation/cancel! token :gateway-shutdown)
                   (expect (= :stall-watchdog (cancellation/cancel-reason token)))))
             (it "records :unspecified for an unattributed cancel"
                 (let [token (cancellation/cancellation-token)]
                   (cancellation/cancel! token)
                   (expect (= :unspecified (cancellation/cancel-reason token)))
                   (expect (true? (cancellation/cancelled? token)))))
             (it "still cancels a hand-built token that carries no reason atom"
                 (let
                   [token {:com.blockether.vis.internal.cancellation/flag (atom false)
                           :com.blockether.vis.internal.cancellation/callbacks (atom [])}]
                   (cancellation/cancel! token :client-cancel-turn)
                   (expect (true? (cancellation/cancelled? token)))
                   (expect (nil? (cancellation/cancel-reason token)))))
             (it "is nil for a token nobody cancelled"
                 (expect (nil? (cancellation/cancel-reason (cancellation/cancellation-token))))
                 (expect (nil? (cancellation/cancel-reason nil)))))

;; Regression, issue #130: repeated cancellation re-fired the same terminal hook,
;; and blocked turn work could pin every virtual-thread carrier.
(defdescribe
  cancel-worker-resilience-test
  (it "drains callbacks after the first cancel while preserving its reason"
      (let
        [token
         (cancellation/cancellation-token)

         fired
         (atom 0)]

        (cancellation/on-cancel! token #(swap! fired inc))
        (cancellation/cancel! token :client-cancel-turn)
        (cancellation/cancel! token :stall-watchdog)
        (expect (= 1 @fired))
        (expect (= :client-cancel-turn (cancellation/cancel-reason token)))))
  (it "runs an explicitly platform worker on its named thread"
      (let
        [thread-name
         (promise)

         fut
         (cancellation/worker-future "vis-test-platform"
                                     #(do (deliver thread-name (.getName (Thread/currentThread)))
                                          :done)
                                     {:platform? true})]

        (expect (= :done (deref fut 5000 :timeout)))
        (expect (= "vis-test-platform" (deref thread-name 5000 :timeout)))))
  (it "keeps the default worker virtual when supported"
      (let
        [virtual?
         (promise)

         fut
         (cancellation/worker-future
           "vis-test-default"
           #(do (deliver virtual? (str/includes? (str (Thread/currentThread)) "VirtualThread"))
                :done))]

        (expect (= :done (deref fut 5000 :timeout)))
        (expect (= (cancellation/virtual-threads-available?) (deref virtual? 5000 :timeout))))))
