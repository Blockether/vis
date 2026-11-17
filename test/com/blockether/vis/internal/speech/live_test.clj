(ns com.blockether.vis.internal.speech.live-test
  "Stream windowing, ownership, cancellation and native callback contracts."
  (:require [com.blockether.vis.internal.speech.live :as live]
            [com.blockether.vis.internal.speech.tts :as tts]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.blockether.vis.internal.speech.tts LiveGenerationCallback]))

(defn- error-type [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

(defn- fake-decoder
  ([] (fake-decoder {}))
  ([overrides]
   (let [windows
         (atom [])

         closes
         (atom 0)

         aborts
         (atom 0)]

     (merge {:sample-rate 24000
             :window 6
             :chunk 4
             :windows windows
             :closes closes
             :aborts aborts
             :step! (fn [samples]
                      (swap! windows conj (vec samples))
                      " word")
             :close! #(swap! closes inc)
             :abort! #(swap! aborts inc)}
            overrides))))

(defn- opened
  [decoder opts]
  (binding [live/*open-decoder* (constantly decoder)]
    (live/open-asr! opts)))

(defdescribe
  live-asr-test
  (it "preserves batch windows for every input split and pads only the tail"
      (doseq [n
              (range 21)

              split
              (range (inc n))]

        (let [d
              (fake-decoder)

              h
              (opened d {})

              samples
              (float-array (range n))

              expected
              (mapv (fn [start]
                      (mapv #(float (if (< % n) % 0)) (range start (+ start 6))))
                    (range 0 n 4))]

          (try (live/feed-asr! h (java.util.Arrays/copyOfRange samples 0 split) 0)
               (live/feed-asr! h (java.util.Arrays/copyOfRange samples split n) split)
               (let [final (live/finish-asr! h)]
                 (expect (= expected @(:windows d)))
                 (expect (= (count expected) (:revision final)))
                 (expect (= final (live/finish-asr! h)))
                 (expect (= 1 @(:closes d))))
               (finally (live/abort-asr! h))))))
  (it "returns replacement partials only after complete windows"
      (let [d
            (fake-decoder)

            partials
            (atom [])

            h
            (opened d {:on-partial #(swap! partials conj %)})]

        (try (expect (nil? (live/feed-asr! h (float-array 5) 0)))
             (expect (= {:text "word" :revision 1} (live/feed-asr! h (float-array 1) 5)))
             (expect (= [{:text "word" :revision 1}] @partials))
             (expect (= {:text "word" :revision 2} (live/finish-asr! h)))
             (finally (live/abort-asr! h)))))
  (it "rejects overlapping ownership and releases the permit after close"
      (let [d
            (fake-decoder)

            h
            (opened d {})]

        (expect (= :speech-live/busy (error-type #(opened d {}))))
        (live/abort-asr! h)
        (let [h2 (opened d {})]
          (expect (= :acknowledged (:state (live/abort-asr! h2)))))))
  (it "rejects invalid limits before opening a native decoder"
      (doseq [limit [0 -1 720001 1.2]]
        (expect (= :speech-live/sample-limit
                   (error-type #(opened (fake-decoder) {:max-samples limit}))))))
  (it "closes once on invalid offsets, oversized input and non-finite samples"
      (doseq [[samples offset limit expected]
              [[(float-array 1) 1 10 :speech-live/input-limit]
               [(float-array 11) 0 10 :speech-live/input-limit]
               [(float-array [Float/NaN]) 0 10 :speech-live/invalid-sample]
               [(float-array [Float/POSITIVE_INFINITY]) 0 10 :speech-live/invalid-sample]]]
        (let [d (fake-decoder)
              h (opened d {:max-samples limit})]

          (expect (= expected (error-type #(live/feed-asr! h samples offset))))
          (expect (= :failed (:state (live/asr-status h))))
          (live/abort-asr! h)
          (expect (= 1 @(:closes d))))))
  (it "cleans up when native inference or the partial consumer fails"
      (doseq [native? [true false]]
        (let [fail #(throw (ex-info "injected" {:type :injected}))
              d (fake-decoder (when native?
                                {:step! (fn [_]
                                          (fail))}))
              h (opened d
                        (when-not native?
                          {:on-partial (fn [_]
                                         (fail))}))]

          (expect (= :injected (error-type #(live/feed-asr! h (float-array 6) 0))))
          (expect (= 1 @(:closes d)))
          (expect (= :failed (:state (live/asr-status h)))))))
  (it "acknowledges abort only after active inference exits"
      (let [entered
            (promise)

            exit
            (promise)

            d
            (fake-decoder {:step! (fn [_]
                                    (deliver entered true)
                                    @exit
                                    "obsolete")})

            h
            (opened d {})

            worker
            (future (error-type #(live/feed-asr! h (float-array 6) 0)))]

        (try (expect (= true (deref entered 3000 :timeout)))
             (expect (= {:state :requested} (live/abort-asr! h)))
             (expect (= 0 @(:closes d)))
             (expect (= :requested (:abort-state (live/asr-status h))))
             (deliver exit true)
             (expect (= :speech-live/closed (deref worker 3000 :timeout)))
             (expect (= :acknowledged (:abort-state (live/asr-status h))))
             (expect (= 1 @(:closes d)))
             (expect (= {:state :acknowledged} (live/abort-asr! h)))
             (expect (= 1 @(:closes d)))
             (finally (deliver exit true) (live/abort-asr! h)))))
  (it "does not acknowledge an idle abort before resource cleanup exits"
      (let [entered
            (promise)

            exit
            (promise)

            d
            (fake-decoder {:close! (fn []
                                     (deliver entered true)
                                     @exit)})

            h
            (opened d {})

            worker
            (future (error-type #(live/finish-asr! h)))]

        (try (expect (= true (deref entered 3000 :timeout)))
             (expect (= {:state :requested} (live/abort-asr! h)))
             (deliver exit true)
             (expect (= :speech-live/closed (deref worker 3000 :timeout)))
             (expect (= :acknowledged (:abort-state (live/asr-status h))))
             (finally (deliver exit true) (live/abort-asr! h)))))
  (it "rejects unsupported model metadata and returns the permit"
      (let [d (fake-decoder {:sample-rate 16000})]
        (expect (= :speech-live/model-contract (error-type #(opened d {}))))
        (expect (= 1 @(:closes d)))
        (let [h (opened (fake-decoder) {})]
          (live/abort-asr! h)))))

(defdescribe live-callback-test
             (it "passes samples and their actual rate without copying or writing WAV"
                 (let [samples
                       (float-array [0.1])

                       got
                       (atom nil)

                       cb
                       (LiveGenerationCallback. (fn [s r]
                                                  (reset! got [s r]))
                                                (constantly false)
                                                (atom nil)
                                                24000)]

                   (expect (= 1 (.invoke cb samples)))
                   (expect (identical? samples (first @got)))
                   (expect (= 24000 (second @got)))))
             (it "returns native stop on cancellation or exhausted playback credit"
                 (doseq [[cancel? consume] [[(constantly true)
                                             (fn [_ _]
                                               (throw (Exception. "not called")))]
                                            [(constantly false)
                                             (fn [_ _]
                                               false)]]]
                   (let [cb (LiveGenerationCallback. consume cancel? (atom nil) 24000)]
                     (expect (= 0 (.invoke cb (float-array 1)))))))
             (it "checks cancellation again after a blocked consumer returns"
                 (let [cancelled?
                       (atom false)

                       cb
                       (LiveGenerationCallback. (fn [_ _]
                                                  (reset! cancelled? true)
                                                  true)
                                                #(deref cancelled?)
                                                (atom nil)
                                                24000)]

                   (expect (= 0 (.invoke cb (float-array 1))))))
             (it "captures callback exceptions and never throws through JNI"
                 (let [error
                       (Exception. "credit failed")

                       failure
                       (atom nil)

                       cb
                       (LiveGenerationCallback. (fn [_ _]
                                                  (throw error))
                                                (constantly false)
                                                failure
                                                24000)]

                   (expect (= 0 (.invoke cb (float-array 1))))
                   (expect (identical? error @failure))
                   (expect (= 0 (.invoke cb (float-array 1))))))
             (it "checks text limits before any model loading"
                 (doseq [text ["" (apply str (repeat (inc tts/live-max-characters) "a"))]]
                   (expect (= :speech-live/segment-limit
                              (error-type #(live/synthesize-stream! {:text text
                                                                     :on-pcm (fn [_ _])})))))))

(defdescribe live-startup-test
             (it "does not construct models when readiness is missing"
                 (with-redefs [live/capabilities
                               (constantly {:available false})

                               live/open-asr!
                               (fn [_]
                                 (throw (Exception. "must not load")))]

                   (expect (= :speech-live/unavailable (error-type #(live/prepare!))))))
             (it "checks cancellation before and between native model constructors"
                 (doseq [stop [0 1 2]]
                   (let [calls (atom [])
                         cancelled? #(>= (count @calls) stop)]

                     (with-redefs [live/capabilities (constantly {:available true})
                                   live/open-asr! (fn [_]
                                                    (swap! calls conj :asr)
                                                    :handle)
                                   live/abort-asr! (fn [_]
                                                     nil)
                                   tts/prepare-live! #(swap! calls conj :tts)]

                       (expect (= {:ready false :state :aborted}
                                  (live/prepare! {:cancelled? cancelled?})))
                       (expect (= stop (count @calls)))))))
             (it "advertises readiness only after both constructors return"
                 (let [calls (atom [])]
                   (with-redefs [live/capabilities (constantly {:available true})
                                 live/open-asr! (fn [_]
                                                  (swap! calls conj :asr)
                                                  :handle)
                                 live/abort-asr! (fn [_]
                                                   (swap! calls conj :close))
                                 tts/prepare-live! #(swap! calls conj :tts)]

                     (expect (= {:available true :ready true} (live/prepare!)))
                     (expect (= [:asr :close :tts] @calls))))))
