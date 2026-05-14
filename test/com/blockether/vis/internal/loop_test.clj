(ns com.blockether.vis.internal.loop-test
  (:require
   [com.blockether.vis.internal.loop :as lp]
   [com.blockether.vis.internal.persistance :as persistance]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- locked-cache-entry []
  {:environment {:id :env}
   :lock        (java.util.concurrent.locks.ReentrantLock.)})

(defdescribe close-locking-test
  (it "close! waits for the per-env turn lock before disposing"
    (let [{:keys [lock] :as entry} (locked-cache-entry)
          disposed? (atom false)]
      (reset! @#'lp/cache {"c1" entry})
      (.lock lock)
      (try
        (with-redefs [lp/dispose-environment! (fn [_] (reset! disposed? true))]
          (let [f (future (lp/close! "c1"))]
            (Thread/sleep 50)
            (expect (false? @disposed?))
            (.unlock lock)
            (expect (= {} (deref f 1000 :timeout)))
            (expect (true? @disposed?))))
        (finally
          (when (.isHeldByCurrentThread lock)
            (.unlock lock))
          (reset! @#'lp/cache {})))))

  (it "close-all! waits for per-env locks before disposing"
    (let [{:keys [lock] :as entry} (locked-cache-entry)
          disposed? (atom false)]
      (reset! @#'lp/cache {"c1" entry})
      (.lock lock)
      (try
        (with-redefs [lp/dispose-environment! (fn [_] (reset! disposed? true))
                      persistance/db-dispose-shared-connection! (fn [] nil)]
          (let [f (future (lp/close-all!))]
            (Thread/sleep 50)
            (expect (false? @disposed?))
            (.unlock lock)
            (expect (nil? (deref f 1000 :timeout)))
            (expect (true? @disposed?))))
        (finally
          (when (.isHeldByCurrentThread lock)
            (.unlock lock))
          (reset! @#'lp/cache {}))))))
