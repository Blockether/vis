(ns com.blockether.vis.internal.config.runtime-settings-test
  (:require [com.blockether.vis.internal.config.runtime-settings :as rt]
            [com.blockether.vis.internal.util :as util]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe concurrent-wall-release-test
             ;; Issue #187: a release computing its reset must not overwrite a new hold.
             (it "keeps a newly entered call parked while the previous call is returning"
                 (let [{:keys [deadline hold]}
                       (rt/parkable-wall 0 300000)

                       release
                       (hold)

                       resetting
                       (promise)

                       finish-reset
                       (promise)

                       entering
                       (promise)]

                   (with-redefs [util/now-ms (fn ^long []
                                               (deliver resetting true)
                                               (deref finish-reset 3000 nil)
                                               900000)]
                     (let [leaver (future (release))]
                       (try (expect (= true (deref resetting 3000 :timeout)))
                            (let [entrant (future (deliver entering true) (hold))]
                              (expect (= true (deref entering 3000 :timeout)))
                              ;; Old code enters here while the previous release is still resetting.
                              (deref entrant 100 nil)
                              (deliver finish-reset true)
                              (deref leaver 3000 nil)
                              (let [release-new (deref entrant 3000 nil)]
                                (try (expect (nil? @deadline))
                                     (finally (when release-new (release-new))))))
                            (finally (deliver finish-reset true) (deref leaver 3000 nil))))))))

(defdescribe await-wall-entry-race-test
             ;; Issue #187: entering an extension between deadline reads cancels expiry.
             (it "does not time out a call that parked an apparently expired deadline"
                 (let [{:keys [deadline hold]}
                       (rt/parkable-wall 0 300000)

                       result
                       (promise)

                       release
                       (atom nil)]

                   (with-redefs [util/now-ms (fn ^long []
                                               (reset! release (hold))
                                               (deliver result :finished)
                                               300001)]
                     (try (expect (= :finished (rt/await-wall result deadline :timeout)))
                          (finally (when-let [done @release]
                                     (with-redefs [util/now-ms (fn ^long []
                                                                 900000)]
                                       (done)))))))))

(defn- with-clock
  [f]
  (let [now (atom 0)]
    (with-redefs [util/now-ms (fn ^long []
                                (long @now))]
      (f now))))

(defdescribe
  parkable-execution-budget-test
  ;; Issue #187: restore the full configured budget, not the unused remainder.
  (it "starts with the configured deadline"
      (expect (= 301000 @(:deadline (rt/parkable-wall 1000 300000)))))
  (it "leaves unbound calls usable outside an execution watchdog"
      (binding [rt/*blocking-wall-park*
                nil

                rt/*blocking-wall-hold*
                nil]

        (expect (= :done (rt/park-blocking-wall (constantly :done))))
        (expect (nil? ((rt/hold-blocking-wall!))))))
  (it "resets all five minutes after a successful call near the old deadline"
      (with-clock (fn [now]
                    (let [{:keys [deadline park]} (rt/parkable-wall 0 300000)]
                      (reset! now 299999)
                      (expect (= :done
                                 (park #(do (expect (nil? @deadline)) (reset! now 900000) :done))))
                      (expect (= 1200000 @deadline))))))
  (it "restores the entire budget when the call throws"
      (with-clock (fn [now]
                    (let [{:keys [deadline park]}
                          (rt/parkable-wall 0 300000)

                          failure
                          (ex-info "extension failed" {})]

                      (expect (identical? failure
                                          (try (park #(do (reset! now 900000) (throw failure)))
                                               (catch Exception e e))))
                      (expect (= 1200000 @deadline))))))
  (it "resets independently after every sequential call"
      (with-clock (fn [now]
                    (let [{:keys [deadline park]} (rt/parkable-wall 0 300000)]
                      (doseq [finished [900000 1900000 2900000]]
                        (park #(reset! now finished))
                        (expect (= (+ finished 300000) @deadline)))))))
  (it "keeps nested parks suspended until the outermost return"
      (with-clock (fn [now]
                    (let [{:keys [deadline park]} (rt/parkable-wall 0 300000)]
                      (park #(do (park (fn []
                                         (reset! now 900000)))
                                 (expect (nil? @deadline))
                                 (reset! now 1900000)))
                      (expect (= 2200000 @deadline))))))
  (it "combines parks and holds without an early reset"
      (with-clock (fn [now]
                    (let [{:keys [deadline park hold]}
                          (rt/parkable-wall 0 300000)

                          release
                          (hold)]

                      (park #(reset! now 900000))
                      (expect (nil? @deadline))
                      (release)
                      (expect (= 1200000 @deadline))
                      (park #(do (let [release-inner (hold)]
                                   (release-inner))
                                 (expect (nil? @deadline))
                                 (reset! now 1900000)))
                      (expect (= 2200000 @deadline))))))
  (it "makes repeated release idempotent even after another call starts"
      (with-clock
        (fn [now]
          (let [{:keys [deadline hold]}
                (rt/parkable-wall 0 300000)

                release
                (hold)]

            (reset! now 900000)
            (release)
            (let [release-next (hold)]
              (release)
              (expect (nil? @deadline))
              (reset! now 1900000)
              (release-next)
              (expect (= 2200000 @deadline))
              (reset! now 2900000)
              (release-next)
              (expect (= 2200000 @deadline)))))))
  (it "resets only once when many concurrent holds finish"
      (with-clock (fn [now]
                    (let [{:keys [deadline hold]}
                          (rt/parkable-wall 0 300000)

                          releases
                          (mapv (fn [_]
                                  (hold))
                                (range 32))]

                      (reset! now 900000)
                      (doseq [f (mapv #(future (%)) releases)]
                        (expect (not= ::timeout (deref f 3000 ::timeout))))
                      (expect (= 1200000 @deadline))))))
  (it "parks and resets inherited execution clocks together"
      (with-clock (fn [now]
                    (let [outer (rt/parkable-wall 0 300000)]
                      (binding [rt/*blocking-wall-park* (:park outer)]
                        (let [inner (rt/parkable-wall 0 1000)]
                          (binding [rt/*blocking-wall-park* (:park inner)]
                            (rt/park-blocking-wall #(do (expect (nil? @(:deadline outer)))
                                                        (expect (nil? @(:deadline inner)))
                                                        (reset! now 900000))))
                          (expect (= 901000 @(:deadline inner)))
                          (expect (= 1200000 @(:deadline outer)))))))))
  (it "composes inherited holds with the dynamic hold API"
      (with-clock (fn [now]
                    (let [outer (rt/parkable-wall 0 300000)]
                      (binding [rt/*blocking-wall-hold* (:hold outer)]
                        (let [inner (rt/parkable-wall 0 1000)
                              release (binding [rt/*blocking-wall-hold* (:hold inner)]
                                        (rt/hold-blocking-wall!))]

                          (expect (nil? @(:deadline outer)))
                          (expect (nil? @(:deadline inner)))
                          (reset! now 900000)
                          (release)
                          (expect (= 901000 @(:deadline inner)))
                          (expect (= 1200000 @(:deadline outer)))
                          (reset! now 1900000)
                          (release)
                          (expect (= 1200000 @(:deadline outer)))))))))
  (it "returns a completed call even long after the original deadline"
      (with-clock (fn [now]
                    (let [{:keys [deadline park]}
                          (rt/parkable-wall 0 300000)

                          result
                          (promise)]

                      (park #(do (reset! now 900000)
                                 (deliver result :done)
                                 (expect (= :done (rt/await-wall result deadline :timeout)))))))))
  (it "times out runaway execution after the restored budget expires"
      (with-clock (fn [now]
                    (let [{:keys [deadline park]} (rt/parkable-wall 0 300000)]
                      (park #(reset! now 900000))
                      (reset! now 1200001)
                      (expect (= :timeout (rt/await-wall (promise) deadline :timeout))))))))
