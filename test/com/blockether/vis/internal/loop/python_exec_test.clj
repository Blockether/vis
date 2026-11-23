(ns com.blockether.vis.internal.loop.python-exec-test
  (:require [com.blockether.vis.internal.loop.python-exec :as python-exec]
            [com.blockether.vis.internal.python.env :as python-env]
            [com.blockether.vis.internal.session.cancellation :as cancellation]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- failed-block
  [end!]
  (let [token (cancellation/cancellation-token)
        retired? (atom false)
        observed (atom [])
        env {:cancel-token token
             :python-context-retired-atom retired?
             :extensions (atom [{:ext/name "test-observer"
                                 :ext/op-hooks [{:op :py3
                                                 :phase :after
                                                 :fn (fn [_ _ _ result]
                                                       (swap! observed conj (:error result))
                                                       result)}]}])}]
    (with-redefs-fn {#'python-env/run-python-block (fn [& _]
                                                     (end! token retired?)
                                                     {:stdout "Output before failure"
                                                      :error {:message "Python error"}})
                     #'python-exec/interrupt-block! (constantly true)}
      #(let [result (#'python-exec/run-python-code (Object.) "pass" :env env)]
         {:result (select-keys result [:stdout :error]) :observed @observed}))))

(defdescribe after-hook-lifecycle-test
  (it "does not run an after hook when the turn is cancelled or the context is retired"
      (doseq [[end! error] [[(fn [token _] (cancellation/cancel! token :gateway-drain))
                             {:message "Python execution was interrupted" :type :vis/interrupted}]
                            [(fn [_ retired?] (reset! retired? true)) {:message "Python error"}]]]
        (expect (= {:result {:stdout "Output before failure" :error error} :observed []}
                   (failed-block end!)))))
  (it "passes the block failure to the after hook while the session is live"
      (expect (= {:result {:stdout "Output before failure" :error {:message "Python error"}}
                  :observed [{:message "Python error"}]}
                 (failed-block (constantly nil))))))
