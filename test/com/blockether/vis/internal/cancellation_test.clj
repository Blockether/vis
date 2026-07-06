(ns com.blockether.vis.internal.cancellation-test
  "Verify the on-cancel! callback contract: registered thunks fire on
   `cancel!`, late registration after cancellation still triggers
   immediately, and `dispose!` removes the hook cleanly without
   side-effects."
  (:require [com.blockether.vis.internal.cancellation :as cancellation]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe on-cancel-callback-test
             (it "fires every registered thunk when cancel! is called"
                 (let [token
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
                 (let [token
                       (cancellation/cancellation-token)

                       rang
                       (atom 0)]

                   (cancellation/cancel! token)
                   (cancellation/on-cancel! token #(swap! rang inc))
                   (expect (= 1 @rang))))
             (it "dispose! removes the hook so a later cancel! does not fire it"
                 (let [token
                       (cancellation/cancellation-token)

                       rang
                       (atom 0)

                       dispose
                       (cancellation/on-cancel! token #(swap! rang inc))]

                   (dispose)
                   (cancellation/cancel! token)
                   (expect (= 0 @rang))))
             (it "isolates a throwing callback from the others"
                 (let [token
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
                 (let [token
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
