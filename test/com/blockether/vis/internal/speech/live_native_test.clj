(ns com.blockether.vis.internal.speech.live-native-test
  "Opt-in native qualification without microphone or playback. Set VIS_TEST_LIVE_SPEECH=1."
  (:require [clojure.java.shell]
            [clojure.string]
            [com.blockether.vis.internal.speech.assets :as assets]
            [com.blockether.vis.internal.speech.engine :as engine]
            [com.blockether.vis.internal.speech.live :as live]
            [com.blockether.vis.internal.speech.tts :as tts]
            [com.blockether.vis.internal.speech.vibevoice :as vibevoice]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- enabled? [] (= "1" (System/getenv "VIS_TEST_LIVE_SPEECH")))

(defn- qualify!
  [f]
  (if (enabled?)
    (do (when-not (:available (live/capabilities))
          (throw (ex-info "Native qualification requires installed ASR and Pocket assets"
                          {:capabilities (live/capabilities)})))
        (f))
    (println
      "SKIP live native qualification: set VIS_TEST_LIVE_SPEECH=1 with pinned assets installed.")))

(defn- elapsed-ms [start] (/ (- (System/nanoTime) (long start)) 1e6))

(def ^:private ^:const abort-deadline-ms 30000)

(defn- join-samples
  ^floats [chunks]
  (let [out (float-array (reduce + (map #(alength ^floats %) chunks)))]
    (loop [remaining chunks
           offset 0]

      (when-let [^floats samples (first remaining)]
        (System/arraycopy samples 0 out offset (alength samples))
        (recur (next remaining) (+ offset (alength samples)))))
    out))

(defn- spoken-samples
  []
  (let [chunks
        (atom [])

        start
        (System/nanoTime)

        first-ms
        (atom nil)

        result
        (live/synthesize-stream!
          {:text "The local speech test is ready. Please read these words clearly."
           :on-pcm (fn [^floats samples rate]
                     (when-not @first-ms (reset! first-ms (elapsed-ms start)))
                     (expect (= 24000 rate))
                     (swap! chunks conj (aclone samples))
                     true)})]

    (println "NATIVE Pocket"
             {:first-pcm-ms @first-ms
              :total-ms (elapsed-ms start)
              :chunks (count @chunks)
              :state (:state result)})
    (expect (= :finished (:state result)))
    (expect (> (count @chunks) 1))
    (join-samples @chunks)))

(defdescribe
  native-live-qualification
  (it "loads installed models and streams real PCM before native completion"
      (qualify! (fn []
                  (let [start (System/nanoTime)]
                    (expect (:ready (live/prepare!)))
                    (println "NATIVE prepare" {:elapsed-ms (elapsed-ms start)}))
                  (expect (pos? (alength ^floats (spoken-samples)))))))
  (it
    "matches batch ASR with arbitrarily split PCM and fixed noise ordering"
    (qualify!
      (fn []
        (let [samples
              (spoken-samples)

              dir
              (assets/install-dir (vibevoice/asr-asset))

              start
              (System/nanoTime)

              batch
              (vibevoice/transcribe-samples dir samples)

              expected
              (engine/words->text (engine/clean-words (vibevoice/chunk-words batch)))

              _
              (println "NATIVE batch ASR" {:elapsed-ms (elapsed-ms start)})

              first-ms
              (atom nil)

              start
              (System/nanoTime)

              h
              (live/open-asr! {:on-partial (fn [_]
                                             (when-not @first-ms
                                               (reset! first-ms (elapsed-ms start))))})]

          (try (loop [offset 0]
                 (when (< offset (alength ^floats samples))
                   (let [end (min (alength ^floats samples) (+ offset 480))]
                     (live/feed-asr!
                       h
                       (java.util.Arrays/copyOfRange ^floats samples (int offset) (int end))
                       offset)
                     (recur end))))
               (let [final (live/finish-asr! h)]
                 (println "NATIVE stream ASR"
                          {:first-partial-ms @first-ms
                           :total-ms (elapsed-ms start)
                           :samples (alength ^floats samples)
                           :text (:text final)})
                 (expect (seq expected))
                 (expect (= expected (:text final)))
                 (expect (= final (live/finish-asr! h))))
               (finally (live/abort-asr! h)))))))
  (it
    "stops Pocket after a callback returns zero and propagates callback failures safely"
    (qualify!
      (fn []
        (let [calls
              (atom 0)

              stopped
              (atom nil)

              result
              (live/synthesize-stream!
                {:text "The first part is enough. Do not generate the rest of this longer sentence."
                 :on-pcm (fn [_ _]
                           (swap! calls inc)
                           (reset! stopped (System/nanoTime))
                           false)})]

          (println "NATIVE Pocket callback abort" {:ack-ms (elapsed-ms @stopped) :callbacks @calls})
          (expect (= :aborted (:state result)))
          (expect (= 1 @calls)))
        (let [error
              (ex-info "injected consumer failure" {})

              caught
              (try (live/synthesize-stream! {:text "This tests the callback failure."
                                             :on-pcm (fn [_ _]
                                                       (throw error))})
                   nil
                   (catch Throwable e e))]

          (expect (identical? error caught)))
        (expect (:ready (tts/prepare-live!))))))
  (it
    "acknowledges ONNX termination after native inference exits"
    (qualify!
      (fn []
        (let [h
              (live/open-asr! {})

              entered
              (promise)

              worker
              (future (deliver entered true)
                      (try (live/feed-asr! h (float-array 83200) 0)
                           :returned
                           (catch Throwable _ :aborted)))]

          (try
            (expect (= :speech-live/busy
                       (try (vibevoice/release!)
                            nil
                            (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
            (expect (= true (deref entered 3000 :timeout)))
            (Thread/sleep 50)
            (let [start
                  (System/nanoTime)

                  request
                  (live/abort-asr! h)

                  terminal
                  (deref worker abort-deadline-ms :timeout)

                  ack-ms
                  (elapsed-ms start)]

              (println
                "NATIVE ASR abort"
                {:ack-ms ack-ms :deadline-ms abort-deadline-ms :request request :worker terminal})
              (expect (<= ack-ms abort-deadline-ms))
              (expect (= :aborted terminal))
              (expect (= :requested (:state request)))
              (expect (= :acknowledged (:abort-state (live/asr-status h)))))
            (finally (live/abort-asr! h)))))))
  (it "releases the native models after qualification"
      (qualify! (fn []
                  (tts/release-live!)
                  (vibevoice/release!)
                  (expect true)))))

(def ^:private corpus
  [{:kind :short :text "Yes."} {:kind :short :text "No."} {:kind :short :text "Ready."}
   {:kind :short :text "Please continue."} {:kind :negation :text "Do not delete the file."}
   {:kind :negation :text "I did not approve that change."}
   {:kind :negation :text "The answer is not seven."}
   {:kind :negation :text "Do not send this message."}
   {:kind :uncertainty :text "I am not sure yet."}
   {:kind :uncertainty :text "That might be correct."}
   {:kind :uncertainty :text "Please check before you decide."}
   {:kind :uncertainty :text "I cannot confirm the result."} {:kind :control :text "Stop speaking."}
   {:kind :control :text "Cancel the current task."} {:kind :control :text "End voice mode."}
   {:kind :control :text "Keep the task running but stop the audio."}
   {:kind :ordinary :text "The local speech test is ready."}
   {:kind :ordinary :text "Please summarize the next step."}
   {:kind :ordinary :text "There are three items on the list."}
   {:kind :ordinary :text "Save the draft without publishing it."}])

(defn- percentile
  [values fraction]
  (let [values
        (vec (sort values))

        n
        (count values)]

    (when (pos? n) (nth values (max 0 (dec (long (Math/ceil (* (double fraction) n)))))))))

(defn- distribution
  [values]
  {:n (count values) :p50-ms (percentile values 0.50) :p95-ms (percentile values 0.95)})

(defn- normalized
  [text]
  (-> text
      clojure.string/lower-case
      (clojure.string/replace #"[^a-z0-9 ]" "")
      (clojure.string/replace #"\s+" " ")
      clojure.string/trim))

(defn- qualification-cycle
  [text compare-batch?]
  (let [chunks
        (atom [])

        first-pcm
        (atom nil)

        start
        (System/nanoTime)

        tts-result
        (live/synthesize-stream! {:text text
                                  :on-pcm (fn [^floats samples _]
                                            (when-not @first-pcm
                                              (reset! first-pcm (elapsed-ms start)))
                                            (swap! chunks conj (aclone samples))
                                            true)})

        tts-ms
        (elapsed-ms start)

        samples
        (join-samples @chunks)

        start
        (System/nanoTime)

        h
        (live/open-asr! {})]

    (try
      (loop [offset 0]
        (when (< offset (alength samples))
          (let [end (min (alength samples) (+ offset 480))]
            (live/feed-asr! h (java.util.Arrays/copyOfRange samples (int offset) (int end)) offset)
            (recur end))))
      (let [final
            (live/finish-asr! h)

            asr-ms
            (elapsed-ms start)

            batch-start
            (System/nanoTime)

            batch
            (when compare-batch?
              (engine/words->text (engine/clean-words (vibevoice/chunk-words
                                                        (vibevoice/transcribe-samples
                                                          (assets/install-dir (vibevoice/asr-asset))
                                                          samples)))))]

        (when-not (and (= :finished (:state tts-result)) (= :finished (:state (live/asr-status h))))
          (throw (ex-info "Native stream did not finish" {})))
        {:tts-first-pcm-ms @first-pcm
         :tts-total-ms tts-ms
         :asr-buffered-final-ms asr-ms
         :batch-asr-ms (when compare-batch? (elapsed-ms batch-start))
         :batch-equal? (when compare-batch? (= batch (:text final)))
         :reference-equal? (= (normalized text) (normalized (:text final)))
         :reference text
         :transcript (:text final)
         :empty? (clojure.string/blank? (:text final))})
      (finally (live/abort-asr! h)))))

(defn- memory-snapshot
  []
  (System/gc)
  (let [heap
        (.getHeapMemoryUsage (java.lang.management.ManagementFactory/getMemoryMXBean))

        os
        (java.lang.management.ManagementFactory/getOperatingSystemMXBean)

        rss
        (clojure.java.shell/sh "ps"
                               "-o" "rss="
                               "-p" (str (.pid (java.lang.ProcessHandle/current))))]

    {:heap-used-bytes (.getUsed heap)
     :rss-bytes (when (zero? (long (:exit rss)))
                  (* 1024 (Long/parseLong (clojure.string/trim (:out rss)))))
     :threads (.getThreadCount (java.lang.management.ManagementFactory/getThreadMXBean))
     :open-file-descriptors (when (instance? com.sun.management.UnixOperatingSystemMXBean os)
                              (.getOpenFileDescriptorCount
                                ^com.sun.management.UnixOperatingSystemMXBean os))
     :asr-model-leases @(var-get #'vibevoice/live-pins)
     :asr-permits (.availablePermits ^java.util.concurrent.Semaphore (var-get #'live/asr-permit))
     :tts-permits (.availablePermits ^java.util.concurrent.Semaphore (var-get #'tts/live-permit))}))

(defn- interrupt-cycle!
  []
  (let [callbacks
        (atom 0)

        tts-result
        (live/synthesize-stream!
          {:text "Stop this speech after the first audio chunk. The task must remain separate."
           :on-pcm (fn [_ _]
                     (swap! callbacks inc)
                     false)})

        h
        (live/open-asr! {})

        entered
        (promise)

        worker
        (future (deliver entered true)
                (try (live/feed-asr! h (float-array 83200) 0) (catch Throwable _ nil)))]

    (try (when-not (= true (deref entered 30000 :timeout))
           (throw (ex-info "ASR worker did not start" {})))
         (Thread/sleep 20)
         (live/abort-asr! h)
         (when (= :timeout (deref worker 30000 :timeout))
           (throw (ex-info "ASR abort did not exit" {})))
         (when-not (and (= :aborted (:state tts-result))
                        (= 1 @callbacks)
                        (= :acknowledged (:abort-state (live/asr-status h))))
           (throw (ex-info "Native interruption failed" {})))
         (finally (live/abort-asr! h)))))

(defn- assert-idle!
  [snapshot]
  (when-not (and (= 0 (:asr-model-leases snapshot))
                 (= 1 (:asr-permits snapshot))
                 (= 1 (:tts-permits snapshot)))
    (throw (ex-info "Native ownership leaked" {:snapshot snapshot}))))

(defdescribe
  extended-native-qualification
  (it
    "measures a synthetic English corpus without executing transcripts"
    (if-not (= "1" (System/getenv "VIS_TEST_LIVE_CORPUS"))
      (println
        "SKIP extended native corpus: set VIS_TEST_LIVE_CORPUS=1 and VIS_TEST_LIVE_SPEECH=1.")
      (do
        (when-not (enabled?) (throw (ex-info "Native corpus requires explicit speech opt-in" {})))
        (qualify!
          (fn []
            (live/prepare!)
            (dotimes [i 5]
              (qualification-cycle (:text (nth corpus i)) false))
            (let [rows
                  (mapv (fn [i]
                          (let [entry
                                (nth corpus (mod i (count corpus)))

                                result
                                (try (assoc (qualification-cycle (:text entry) true)
                                       :kind (:kind entry))
                                     (catch Throwable e
                                       {:kind (:kind entry) :error (ex-message e)}))]

                            (when (zero? (mod (inc i) 25)) (println "CORPUS progress" (inc i)))
                            result))
                        (range 200))

                  good
                  (remove :error rows)

                  report
                  {:utterances 200
                   :unique-prompts (count corpus)
                   :concurrency 1
                   :categories (frequencies (map :kind rows))
                   :failures (count (filter :error rows))
                   :failure-reasons (frequencies (keep :error rows))
                   :batch-consistent (count (filter :batch-equal? good))
                   :reference-normalized-exact (count (filter :reference-equal? good))
                   :reference-mismatches-by-category (frequencies
                                                       (map :kind (remove :reference-equal? good)))
                   :reference-mismatch-examples
                   (vec (take 20
                              (distinct (map #(select-keys % [:kind :reference :transcript])
                                             (remove :reference-equal? good)))))
                   :intent-sensitive-mismatch-examples
                   (vec (take 80
                              (distinct (map #(select-keys % [:kind :reference :transcript])
                                             (filter #(and (#{:negation :control} (:kind %))
                                                           (not (:reference-equal? %)))
                                                     good)))))
                   :empty-transcripts (count (filter :empty? good))
                   :stages (into {}
                                 (for [stage [:tts-first-pcm-ms :tts-total-ms :asr-buffered-final-ms
                                              :batch-asr-ms]]
                                   [stage (distribution (keep stage good))]))}]

              (println "CORPUS RESULT" (pr-str report))
              (assert-idle! (memory-snapshot))
              (expect (zero? (:failures report)))
              (expect (= 200 (:batch-consistent report)))))))))
  (it
    "soaks real native streams and interruptions for an explicit duration"
    (if-let [duration (System/getenv "VIS_TEST_LIVE_SOAK_SECONDS")]
      (do
        (when-not (enabled?) (throw (ex-info "Native soak requires explicit speech opt-in" {})))
        (let [seconds (Long/parseLong duration)]
          (when-not (<= 10 seconds 7200)
            (throw (ex-info "Soak duration must be 10..7200 seconds" {})))
          (qualify!
            (fn []
              (live/prepare!)
              (dotimes [i 5]
                (qualification-cycle (:text (nth corpus i)) false))
              (interrupt-cycle!)
              (let [baseline (memory-snapshot)
                    start (System/nanoTime)
                    result (loop [cycles 0
                                  interruptions 0
                                  snapshots [baseline]
                                  next-report 60]

                             (if (>= (elapsed-ms start) (* seconds 1000))
                               {:cycles cycles :interruptions interruptions :snapshots snapshots}
                               (do (qualification-cycle (:text (nth corpus
                                                                    (mod cycles (count corpus))))
                                                        false)
                                   (when (zero? (mod cycles 5)) (interrupt-cycle!))
                                   (let [report? (>= (elapsed-ms start) (* next-report 1000))
                                         sample (when report? (memory-snapshot))]

                                     (when sample
                                       (assert-idle! sample)
                                       (println "SOAK progress"
                                                (pr-str (assoc sample
                                                          :seconds (/ (elapsed-ms start) 1000)
                                                          :cycles (inc cycles)))))
                                     (recur (inc cycles)
                                            (+ interruptions (if (zero? (mod cycles 5)) 1 0))
                                            (cond-> snapshots
                                              sample
                                              (conj sample))
                                            (if report? (+ next-report 60) next-report))))))
                    final (memory-snapshot)]

                (assert-idle! final)
                (println "SOAK RESULT"
                         (pr-str {:duration-seconds (/ (elapsed-ms start) 1000)
                                  :requested-seconds seconds
                                  :concurrency 1
                                  :failures 0
                                  :cycles (:cycles result)
                                  :interruptions (:interruptions result)
                                  :baseline baseline
                                  :final final
                                  :heap-delta-bytes (- (:heap-used-bytes final)
                                                       (:heap-used-bytes baseline))
                                  :rss-delta-bytes (when (and (:rss-bytes final)
                                                              (:rss-bytes baseline))
                                                     (- (:rss-bytes final) (:rss-bytes baseline)))
                                  :samples (count (:snapshots result))}))
                (expect (pos? (:cycles result)))
                (tts/release-live!)
                (vibevoice/release!))))))
      (println "SKIP native soak: set VIS_TEST_LIVE_SOAK_SECONDS and VIS_TEST_LIVE_SPEECH=1."))))

(defn- profile-asr-final
  [^floats samples]
  (let [phase
        (atom :feed)

        timings
        (atom {})

        run
        @#'vibevoice/run-session

        h
        (live/open-asr! {})]

    (try (with-redefs [vibevoice/run-session
                       (fn [session inputs]
                         (let [start (System/nanoTime)
                               kind
                               (if (.containsKey ^java.util.Map inputs "audio") :encoder :decoder)]

                           (try (run session inputs)
                                (finally (swap! timings update
                                           [@phase kind]
                                           (fnil conj [])
                                           (elapsed-ms start))))))]
           (loop [offset 0]
             (when (< offset (alength samples))
               (let [end (min (alength samples) (+ offset 480))]
                 (live/feed-asr! h
                                 (java.util.Arrays/copyOfRange samples (int offset) (int end))
                                 offset)
                 (recur end))))
           (reset! phase :finish)
           (let [pending (:pending-count @(:state h))
                 start (System/nanoTime)
                 final (live/finish-asr! h)]

             {:finish-ms (elapsed-ms start)
              :pending-samples pending
              :text (:text final)
              :native (into {}
                            (for [[stage values] @timings]
                              [stage {:calls (count values) :total-ms (reduce + values)}]))}))
         (finally (live/abort-asr! h)))))

(defdescribe
  native-finalization-profile
  (it
    "profiles native finalization with one fixed synthetic fixture and explicit thread counts"
    (if-not (= "1" (System/getenv "VIS_TEST_LIVE_PROFILE"))
      (println "SKIP finalization profile: set VIS_TEST_LIVE_PROFILE=1 and VIS_TEST_LIVE_SPEECH=1.")
      (do
        (when-not (enabled?) (throw (ex-info "Profiling requires native speech opt-in" {})))
        (qualify!
          (fn []
            (let
              [chunks
               (atom [])

               _
               (live/synthesize-stream!
                 {:text
                  "Please explain how the local speech test checks a useful answer. Read these words clearly and keep the answer short."
                  :on-pcm (fn [^floats samples _]
                            (swap! chunks conj (aclone samples))
                            true)})

               raw
               (join-samples @chunks)

               samples
               (java.util.Arrays/copyOf
                 raw
                 (int (* 480 (long (Math/ceil (/ (double (max 96000 (alength raw))) 480.0))))))

               baseline
               (atom nil)]

              (try (doseq [threads [0 8 0 8]]
                     (vibevoice/release!)
                     (with-redefs [vibevoice/open-session
                                   (fn [^ai.onnxruntime.OrtEnvironment env ^java.io.File file]
                                     (with-open [options
                                                 (ai.onnxruntime.OrtSession$SessionOptions.)]
                                       (when (pos? threads)
                                         (.setIntraOpNumThreads options (int threads)))
                                       (.createSession env (.getAbsolutePath file) options)))]
                       (dotimes [trial 3]
                         (let [result (profile-asr-final samples)
                               batch (engine/words->text (engine/clean-words
                                                           (vibevoice/chunk-words
                                                             (vibevoice/transcribe-samples
                                                               (assets/install-dir
                                                                 (vibevoice/asr-asset))
                                                               samples))))]

                           (expect (= batch (:text result)))
                           (when-not @baseline (reset! baseline (:text result)))
                           (println "FINALIZATION PROFILE"
                                    (pr-str (assoc result
                                              :threads threads
                                              :trial trial
                                              :samples (alength samples))))
                           (expect (= @baseline (:text result)))))))
                   (finally (vibevoice/release!) (tts/release-live!))))))))))

(defn- pocket-chunk-trial
  [text seed chunk-size stop-after-first?]
  (let [configure
        @#'tts/pocket-generation-config

        chunks
        (atom [])

        first-pcm
        (atom nil)

        start
        (System/nanoTime)

        terminal
        (with-redefs [tts/pocket-generation-config (fn [clip transcript]
                                                     (let [^com.k2fsa.sherpa.onnx.GenerationConfig
                                                           config
                                                           (configure clip transcript)
                                                           ^java.util.Map extra (.getExtra config)]

                                                       (.put extra "seed" (str seed))
                                                       (.put extra "chunk_size" (str chunk-size))
                                                       config))]
          (live/synthesize-stream!
            {:text text
             :on-pcm (fn [^floats samples rate]
                       (when-not @first-pcm (reset! first-pcm (elapsed-ms start)))
                       (when-not (= 24000 rate) (throw (ex-info "Unexpected sample rate" {})))
                       (swap! chunks conj (aclone samples))
                       (not stop-after-first?))}))]

    {:samples (join-samples @chunks)
     :callback-sizes (mapv #(alength ^floats %) @chunks)
     :first-pcm-ms @first-pcm
     :total-ms (elapsed-ms start)
     :state (:state terminal)}))

(defn- pcm-comparison
  [^floats reference ^floats candidate callback-sizes]
  (let [n
        (min (alength reference) (alength candidate))

        nonfinite
        (fn [^floats samples]
          (loop [i
                 0

                 count
                 0]

            (if (= i (alength samples))
              count
              (recur (inc i) (if (Float/isFinite (aget samples i)) count (inc count))))))

        summary
        (loop [i
               0

               different
               0

               maximum
               0.0

               squared
               0.0]

          (if (= i n)
            {:different-samples different
             :max-absolute-error maximum
             :rmse (if (pos? n) (Math/sqrt (/ squared (double n))) 0.0)}
            (let [a
                  (aget reference i)

                  b
                  (aget candidate i)

                  delta
                  (if (and (Float/isFinite a) (Float/isFinite b))
                    (Math/abs (- (double a) (double b)))
                    0.0)]

              (recur (inc i)
                     (if (= (Float/floatToRawIntBits a) (Float/floatToRawIntBits b))
                       different
                       (inc different))
                     (max maximum delta)
                     (+ squared (* delta delta))))))

        joins
        (filter #(< (long %) n) (butlast (rest (reductions + 0 callback-sizes))))]

    (assoc summary
      :reference-samples (alength reference)
      :candidate-samples (alength candidate)
      :bit-equal? (and (= (alength reference) (alength candidate))
                       (zero? (long (:different-samples summary))))
      :reference-nonfinite (nonfinite reference)
      :candidate-nonfinite (nonfinite candidate)
      :join-boundaries
      (mapv (fn [offset]
              (let [i (int offset)]
                {:sample i
                 :reference-jump (- (double (aget reference i)) (double (aget reference (dec i))))
                 :candidate-jump (- (double (aget candidate i)) (double (aget candidate (dec i))))
                 :left-error (Math/abs (- (double (aget reference (dec i)))
                                          (double (aget candidate (dec i)))))
                 :right-error (Math/abs (- (double (aget reference i))
                                           (double (aget candidate i))))}))
            joins))))

(defdescribe
  pocket-chunk-paired-qualification
  (it
    "compares seeded decoder chunks without changing production settings"
    (if-not (= "1" (System/getenv "VIS_TEST_POCKET_CHUNKS"))
      (println
        "SKIP Pocket chunk comparison: set VIS_TEST_POCKET_CHUNKS=1 and VIS_TEST_LIVE_SPEECH=1.")
      (do
        (when-not (enabled?) (throw (ex-info "Chunk comparison requires native speech opt-in" {})))
        (qualify!
          (fn []
            (try
              (tts/prepare-live!)
              (pocket-chunk-trial "The local draft is ready." 42 15 false)
              (doseq
                [[kind text]
                 [[:short "The local draft is ready."]
                  [:multiple
                   "The local draft is ready. Review the changes before publishing the final version."]
                  [:control
                   "Do not delete the file. Stop speaking, but keep the current task running."]]

                 seed
                 [42 43 44]]

                (let [a
                      (pocket-chunk-trial text seed 15 false)

                      b
                      (pocket-chunk-trial text seed 15 false)

                      control
                      (pcm-comparison (:samples a) (:samples b) (:callback-sizes b))]

                  (println "POCKET CHUNK CONTROL"
                           (pr-str {:kind kind
                                    :seed seed
                                    :comparison control
                                    :runs (mapv #(dissoc % :samples) [a b])}))
                  (when-not (and (:bit-equal? control)
                                 (zero? (long (:reference-nonfinite control)))
                                 (= :finished (:state a) (:state b)))
                    (throw (ex-info
                             "Seeded baseline is not deterministic; candidate comparison is invalid"
                             {:kind kind :seed seed})))
                  (doseq [trial (range 2)]
                    (let [candidate (pocket-chunk-trial text seed 5 false)
                          comparison (pcm-comparison (:samples a)
                                                     (:samples candidate)
                                                     (:callback-sizes candidate))]

                      (println "POCKET CHUNK CANDIDATE"
                               (pr-str {:kind kind
                                        :seed seed
                                        :trial trial
                                        :comparison comparison
                                        :run (dissoc candidate :samples)
                                        :sample-preserving? (:bit-equal? comparison)}))
                      ;; A waveform difference rejects an unchanged-samples claim, not this measurement test.
                      (expect (= :finished (:state candidate)))
                      (expect (zero? (long (:candidate-nonfinite comparison))))))))
              (doseq [chunk-size [15 5]]
                (let [result (pocket-chunk-trial "Stop speaking, but keep the task running."
                                                 42
                                                 chunk-size
                                                 true)]
                  (println "POCKET CHUNK CALLBACK STOP"
                           (pr-str (assoc (dissoc result :samples) :chunk-size chunk-size)))
                  ;; Callback cancellation cannot interrupt the preceding latent-generation loop.
                  (expect (= :aborted (:state result)))
                  (expect (= 1 (count (:callback-sizes result))))))
              (finally (tts/release-live!)))))))))

(defn- pre-pcm-abort-trial
  [text seed]
  (let [configure
        @#'tts/pocket-generation-config

        abort
        (promise)

        outcome
        (promise)

        first-pcm
        (atom nil)

        callbacks
        (atom 0)]

    (with-redefs [tts/pocket-generation-config
                  (fn [clip transcript]
                    (let [^com.k2fsa.sherpa.onnx.GenerationConfig config (configure clip transcript)
                          ^java.util.Map extra (.getExtra config)]

                      (.put extra "seed" (str seed))
                      config))]
      (let [^Runnable task
            (bound-fn []
                      (try (let [value (live/synthesize-stream!
                                         {:text text
                                          :register-abort! #(deliver abort %)
                                          :on-pcm
                                          (fn [_ _]
                                            (compare-and-set! first-pcm nil (System/nanoTime))
                                            (swap! callbacks inc)
                                            true)})]
                             (deliver outcome {:value value :returned-at (System/nanoTime)}))
                           (catch Throwable error
                             (deliver outcome {:error error :returned-at (System/nanoTime)}))))
            worker (Thread. task "vis-tts-pre-pcm-probe")]

        ;; A failed probe must not keep its owned test JVM alive.
        (.setDaemon worker true)
        (.start worker)
        (try
          (let [deadline (+ (System/nanoTime) 5000000000)
                native-frame
                (loop []

                  (when (realized? outcome)
                    (throw (ex-info "Synthesis ended before the native abort probe"
                                    {:result (:value @outcome)}
                                    (:error @outcome))))
                  (when @first-pcm
                    (throw (ex-info "The abort probe missed the first audio callback" {})))
                  (if-let [frame (some (fn [^java.lang.StackTraceElement frame]
                                         (when (and (.isNativeMethod frame)
                                                    (= "com.k2fsa.sherpa.onnx.OfflineTts"
                                                       (.getClassName frame))
                                                    (.startsWith (.getMethodName frame) "generate"))
                                           {:class (.getClassName frame)
                                            :method (.getMethodName frame)}))
                                       (.getStackTrace worker))]
                    frame
                    (if (< (System/nanoTime) deadline)
                      (do (Thread/sleep 1) (recur))
                      (throw (ex-info "No active native generation frame was observed" {})))))
                abort! (deref abort 0 nil)
                _ (when-not (ifn? abort!)
                    (throw (ex-info "Native generation has no registered abort" {})))
                requested-at (System/nanoTime)
                _ (abort!)
                abort-returned-at (System/nanoTime)
                answer (deref outcome abort-deadline-ms ::timeout)]

            (when (= ::timeout answer)
              (throw (ex-info "Native synthesis did not release within the probe deadline"
                              {:deadline-ms abort-deadline-ms :native-frame native-frame})))
            (when-let [error (:error answer)]
              (throw error))
            {:characters (count text)
             :seed seed
             :native-frame native-frame
             :state (get-in answer [:value :state])
             :callbacks @callbacks
             :first-pcm-before-request? (boolean (and @first-pcm
                                                      (< (long @first-pcm) requested-at)))
             :abort-call-ms (/ (- abort-returned-at requested-at) 1000000.0)
             :request-to-release-ms (/ (- (long (:returned-at answer)) requested-at) 1000000.0)
             :available-permits (.availablePermits ^java.util.concurrent.Semaphore
                                                   @#'tts/live-permit)})
          (finally (when (realized? abort) (@abort)) (.join worker abort-deadline-ms)))))))

(defdescribe
  live-native-pre-pcm-abort-qualification
  (it
    "measures native release when cancellation precedes the first audio callback"
    (if (not= "1" (System/getenv "VIS_TEST_LIVE_PRE_PCM_ABORT"))
      (println "SKIP pre-PCM abort qualification: set VIS_TEST_LIVE_PRE_PCM_ABORT=1.")
      (qualify!
        (fn []
          (try (tts/prepare-live!)
               (pocket-chunk-trial "The local draft is ready." 42 15 false)
               (let [maximum
                     (long @#'tts/live-max-characters)

                     long-text
                     (str (clojure.string/trim
                            (subs (clojure.string/join " " (repeat maximum "the local voice check"))
                                  0
                                  (dec maximum)))
                          ".")]

                 (doseq [[kind text]
                         [[:short "The local draft is ready."] [:engine-limit long-text]]

                         seed
                         [42 43 44]]

                   (let [result (pre-pcm-abort-trial text seed)]
                     (println "PRE-PCM NATIVE ABORT"
                              (pr-str (assoc result
                                        :kind kind
                                        :deadline-ms abort-deadline-ms)))
                     (expect (<= (:request-to-release-ms result) abort-deadline-ms))
                     (expect (= :aborted (:state result)))
                     (expect (zero? (:callbacks result)))
                     (expect (false? (:first-pcm-before-request? result)))
                     (expect (= 1 (:available-permits result))))))
               (finally
                 ;; Keep a timed-out native model alive until this owned JVM exits.
                 (when (pos? (.availablePermits ^java.util.concurrent.Semaphore @#'tts/live-permit))
                   (tts/release-live!)))))))))

(defn- paced-asr-profile
  [^floats samples concurrent-tts?]
  (let [timings
        (atom {})

        phase
        (atom :feed)

        run
        @#'vibevoice/run-session

        step
        @#'vibevoice/asr-step

        measure
        (fn [stage f]
          (let [start (System/nanoTime)]
            (try (f)
                 (finally
                   (swap! timings update [@phase stage] (fnil conj []) (elapsed-ms start))))))

        stop?
        (atom false)

        started
        (promise)

        tts-completed
        (atom 0)

        h
        (live/open-asr! {})

        tts-worker
        (when concurrent-tts?
          (future (deliver started true)
                  (loop [calls 0]
                    (if @stop?
                      calls
                      (do (live/synthesize-stream!
                            {:text
                             "The local speech test is ready. Please read these words clearly."
                             :on-pcm (fn [_ _]
                                       true)})
                          (swap! tts-completed inc)
                          (recur (inc calls)))))))]

    (try
      (when tts-worker
        (when-not (= true (deref started 30000 :timeout))
          (throw (ex-info "TTS worker did not start" {}))))
      (with-redefs [vibevoice/run-session
                    (fn [session inputs]
                      (if (.containsKey ^java.util.Map inputs "audio")
                        (measure :encoder #(run session inputs))
                        (run session inputs)))

                    vibevoice/asr-step
                    (fn [bundle ids speech kv]
                      (let [stage (cond (= [(get-in bundle [:meta "ids" "text_chunk_end"])]
                                           (vec ids))
                                        :chunk-finalize
                                        (= 1 (count ids)) :token-decode
                                        (= (first ids) (get-in bundle [:meta "ids" "speech_start"]))
                                        :speech-prefill
                                        :else :prompt-prefill)]
                        (measure stage #(step bundle ids speech kv))))]

        (let [start
              (System/nanoTime)

              waits
              (atom [])

              feeds
              (atom [])]

          ;; Scheduled arrivals model an unbounded FIFO, not the gateway implementation.
          (loop [offset 0]
            (when (< offset (alength samples))
              (let [arrival-ms (/ offset 24.0)
                    delay-ms (- arrival-ms (elapsed-ms start))]

                (when (pos? delay-ms) (Thread/sleep (long (Math/ceil delay-ms))))
                (swap! waits conj (max 0.0 (- (elapsed-ms start) arrival-ms)))
                (let [end (min (alength samples) (+ offset 480))
                      feed-start (System/nanoTime)]

                  (live/feed-asr! h
                                  (java.util.Arrays/copyOfRange samples (int offset) (int end))
                                  offset)
                  (swap! feeds conj (elapsed-ms feed-start))
                  (recur end)))))
          (reset! phase :finish)
          (let [finish-start
                (System/nanoTime)

                final
                (live/finish-asr! h)

                finish-ms
                (elapsed-ms finish-start)]

            {:text (:text final)
             :audio-ms (/ (alength samples) 24.0)
             :total-ms (elapsed-ms start)
             :finish-ms finish-ms
             :audio-end-to-final-ms (max 0.0 (- (elapsed-ms start) (/ (alength samples) 24.0)))
             :tts-completed @tts-completed
             :queue-wait (assoc (distribution @waits) :max-ms (reduce max 0.0 @waits))
             :feed (assoc (distribution @feeds) :max-ms (reduce max 0.0 @feeds))
             :stages (into {}
                           (for [[stage values] @timings]
                             [stage (assoc (distribution values) :total-ms (reduce + values))]))})))
      (finally (reset! stop? true)
               (live/abort-asr! h)
               (when tts-worker
                 (when (= :timeout (deref tts-worker 30000 :timeout))
                   (throw (ex-info "Concurrent TTS did not stop" {}))))))))

(defdescribe
  native-paced-profile
  (it
    "profiles paced ASR stages alone and with concurrent TTS"
    (if-not (= "true" (System/getProperty "vis.test.paced-speech-profile"))
      (println "SKIP paced profile: set JVM property vis.test.paced-speech-profile=true.")
      (do
        (when-not (:available (live/capabilities))
          (throw (ex-info "Paced profile requires installed native speech assets" {})))
        (try (live/prepare!)
             (let [fixture
                   (pocket-chunk-trial
                     "The local speech test is ready. Please read these words clearly." 42
                     15 false)

                   samples
                   (:samples fixture)

                   baseline
                   (:text (paced-asr-profile samples false))]

               (expect (= :finished (:state fixture)))
               (expect (seq baseline))
               (println "PACED PROFILE CONFIG"
                        (pr-str {:fixture-seed 42
                                 :samples (alength ^floats samples)
                                 :frame-ms 20
                                 :queue-model :unbounded-scheduled-arrivals
                                 :asr-threads :runtime-default
                                 :tts-threads (@#'tts/pocket-num-threads)
                                 :warmup-trials 1
                                 :java (System/getProperty "java.version")
                                 :processors (.availableProcessors (Runtime/getRuntime))}))
               (doseq [concurrent?
                       [false true]

                       trial
                       (range 3)]

                 (let [result (paced-asr-profile samples concurrent?)]
                   (println "PACED PROFILE"
                            (pr-str (assoc result
                                      :concurrent-tts? concurrent?
                                      :trial trial)))
                   (expect (= baseline (:text result)))
                   (expect (= concurrent? (pos? (:tts-completed result))))
                   (expect (pos? (get-in result [:stages [:feed :encoder] :n] 0)))
                   (expect (pos? (get-in result [:stages [:finish :chunk-finalize] :n] 0))))))
             (finally (vibevoice/release!) (tts/release-live!)))))))

(defdescribe
  native-thread-profile
  (it
    "compares bounded inference threads with one fixed paced fixture"
    (if-not (= "true" (System/getProperty "vis.test.thread-speech-profile"))
      (println "SKIP thread profile: set JVM property vis.test.thread-speech-profile=true.")
      (do
        (when-not (:available (live/capabilities))
          (throw (ex-info "Thread profile requires installed native speech assets" {})))
        (try
          (live/prepare!)
          (let [fixture
                (pocket-chunk-trial
                  "The local speech test is ready. Please read these words clearly." 42
                  15 false)

                samples
                (:samples fixture)

                baseline
                (:text (paced-asr-profile samples false))

                default-tts
                (@#'tts/num-threads)]

            (expect (= :finished (:state fixture)))
            (expect (seq baseline))
            (println "THREAD PROFILE CONFIG"
                     (pr-str {:fixture-seed 42
                              :samples (alength ^floats samples)
                              :frame-ms 20
                              :warmup-trials-per-setting 1
                              :queue-model :unbounded-scheduled-arrivals
                              :java (System/getProperty "java.version")
                              :processors (.availableProcessors (Runtime/getRuntime))}))
            (doseq [[setting [asr-threads tts-threads]]
                    (map-indexed vector
                                 [[0 default-tts] [2 default-tts] [4 default-tts] [2 2] [4 2] [0 2]
                                  [0 default-tts]])]
              (vibevoice/release!)
              (tts/release-live!)
              (with-redefs [vibevoice/open-session
                            (fn [^ai.onnxruntime.OrtEnvironment env ^java.io.File file]
                              (with-open [options (ai.onnxruntime.OrtSession$SessionOptions.)]
                                (when (pos? asr-threads)
                                  (.setIntraOpNumThreads options (int asr-threads)))
                                (.createSession env (.getAbsolutePath file) options)))
                            tts/pocket-num-threads (fn ^long []
                                                     (long tts-threads))]

                (live/prepare!)
                (expect (= baseline (:text (paced-asr-profile samples false))))
                (doseq [concurrent? [false true]
                        trial (range 3)]

                  (let [result (paced-asr-profile samples concurrent?)]
                    (println "THREAD PROFILE"
                             (pr-str (assoc (dissoc result :text)
                                       :setting setting
                                       :asr-threads asr-threads
                                       :tts-threads tts-threads
                                       :concurrent-tts? concurrent?
                                       :trial trial)))
                    (expect (= baseline (:text result)))
                    (expect (= concurrent? (pos? (:tts-completed result)))))))))
          (finally (vibevoice/release!) (tts/release-live!)))))))

(defdescribe
  pocket-thread-paired-qualification
  (it
    "compares seeded speech output and latency across thread counts"
    (if-not (= "true" (System/getProperty "vis.test.pocket-thread-profile"))
      (println
        "SKIP Pocket thread comparison: set JVM property vis.test.pocket-thread-profile=true.")
      (do
        (when-not (:available (live/capabilities))
          (throw (ex-info "Pocket thread comparison requires installed native speech assets" {})))
        (let [maximum
              (long @#'tts/live-max-characters)

              sentence
              "The local draft is ready. Review the changes before publishing the final version. "

              long-text
              (str (clojure.string/trim
                     (subs (apply str (repeat maximum sentence)) 0 (dec maximum)))
                   ".")

              fixtures
              [[:short "The local draft is ready."] [:multiple (apply str (repeat 3 sentence))]
               [:engine-limit long-text]]

              references
              (atom {})

              default-threads
              (@#'tts/num-threads)]

          (try
            (doseq [[setting threads] (map-indexed vector [default-threads 2 default-threads])]
              (tts/release-live!)
              (with-redefs [tts/pocket-num-threads (fn ^long []
                                                     (long threads))]
                (tts/prepare-live!)
                (pocket-chunk-trial "The local draft is ready." 42 15 false)
                (doseq [[kind text] fixtures
                        seed [42 43 44]]

                  (let [result (pocket-chunk-trial text seed 15 false)
                        key [kind seed]
                        reference (get @references key result)
                        comparison (pcm-comparison (:samples reference)
                                                   (:samples result)
                                                   (:callback-sizes result))]

                    (when (zero? setting) (swap! references assoc key result))
                    (println "POCKET THREAD PROFILE"
                             (pr-str {:setting setting
                                      :threads threads
                                      :kind kind
                                      :seed seed
                                      :characters (count text)
                                      :first-pcm-ms (:first-pcm-ms result)
                                      :total-ms (:total-ms result)
                                      :audio-ms (/ (alength ^floats (:samples result)) 24.0)
                                      :callbacks (count (:callback-sizes result))
                                      :callback-sizes-equal? (= (:callback-sizes reference)
                                                                (:callback-sizes result))
                                      :comparison (dissoc comparison :join-boundaries)}))
                    (expect (= :finished (:state result)))
                    (expect (pos? (alength ^floats (:samples result))))
                    (expect (pos? (double (:first-pcm-ms result))))
                    (expect (zero? (long (:candidate-nonfinite comparison))))
                    ;; Default repeats must be deterministic before comparing candidate output.
                    (when (= 2 setting) (expect (:bit-equal? comparison)))))))
            (finally (tts/release-live!))))))))
