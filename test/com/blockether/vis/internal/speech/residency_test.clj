(ns com.blockether.vis.internal.speech.residency-test
  "The gateway releases idle speech models and keeps them under one memory limit
   (#448), and it never closes a model that a request uses."
  (:require [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.speech.residency :as residency]
            [com.blockether.vis.internal.speech.vibevoice :as vv]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.io File]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- fake!
  "Register model `id` of `bytes`, used `ago-ms` before `now`. Returns an atom that
   counts its releases."
  [id bytes now ago-ms & {:keys [busy? keep-toggle]}]
  (let [released (atom 0)]
    (residency/loaded! id
                       {:token id
                        :bytes bytes
                        :in-use? (constantly (boolean busy?))
                        :release! #(do (swap! released inc) true)
                        :keep-toggle keep-toggle})
    (swap! residency/*registry* assoc-in [id :used-at] (- now ago-ms))
    released))

(defdescribe idle-models
             (it "releases a model that nothing used for the idle time, and keeps a busy one"
                 (binding [residency/*registry* (atom {})]
                   (let [now (System/currentTimeMillis)
                         old (fake! "old" 10 now 400000)
                         busy (fake! "busy" 10 now 400000 :busy? true)
                         recent (fake! "recent" 10 now 1000)]

                     (expect (= ["old"] (residency/sweep! {:now now :limit nil :idle-ms 300000})))
                     (expect (= [1 0 0] [@old @busy @recent]))
                     (expect (= #{"busy" "recent"} (set (keys @residency/*registry*)))))))
             (it "keeps an idle model while its keep setting is on"
                 (binding [residency/*registry* (atom {})]
                   (let [now (System/currentTimeMillis)
                         kept (fake! "parakeet" 10 now 400000 :keep-toggle "speech_preload_model")]

                     (with-redefs [toggles/enabled? #(= "speech_preload_model" %)]
                       (expect (= [] (residency/sweep! {:now now :limit nil :idle-ms 300000}))))
                     (with-redefs [toggles/enabled? (constantly false)]
                       (expect (= ["parakeet"]
                                  (residency/sweep! {:now now :limit nil :idle-ms 300000}))))
                     (expect (= 1 @kept))))))

(defdescribe memory-limit
             (it "releases the least recently used free models until the rest fit, never the newest"
                 (binding [residency/*registry* (atom {})]
                   (let [now (System/currentTimeMillis)]
                     (fake! "a" 30 now 5000)
                     (fake! "b" 30 now 4000 :busy? true)
                     (fake! "c" 30 now 3000)
                     (fake! "d" 30 now 2000)
                     (fake! "newest" 30 now 0)
                     ;; 150 bytes over a limit of 90: "a" and "c" go, "b" is busy.
                     (expect (= ["a" "c"] (residency/sweep! {:now now :limit 90 :idle-ms nil}))))))
             (it "keeps one model that is larger than the limit"
                 (binding [residency/*registry* (atom {})]
                   (fake! "big" 100 (System/currentTimeMillis) 0)
                   (expect (= [] (residency/sweep! {:limit 10 :idle-ms nil})))))
             (it "reads the limits from the setting choices"
                 (expect (= (* 4 1024 1024 1024) (residency/choice-bytes "4 GB")))
                 (expect (nil? (residency/choice-bytes "no limit")))
                 (expect (= 60000 (residency/choice-ms "1 minute")))
                 (expect (nil? (residency/choice-ms "never")))))

(defdescribe
  vibevoice-bundles
  (it "never closes a bundle in use, and opens a released bundle again"
      (binding [residency/*registry* (atom {})]
        (let [dir (.toFile (Files/createTempDirectory "vis-residency" (make-array FileAttribute 0)))
              opened (atom 0)
              spec [:residency-test dir
                    (fn [_ _]
                      {:n (swap! opened inc)})]
              sweep #(residency/sweep! {:limit nil :idle-ms 0})]

          (try (expect (= 1
                          (vv/call-loaded
                            spec
                            (fn [bundle]
                              ;; In use: neither a sweep nor a direct release frees it.
                              (expect (= [] (sweep)))
                              (expect (false? ((get-in @residency/*registry*
                                                       ["residency-test" :release!]))))
                              (expect (true? (get-in (residency/status) [:models 0 :in_use])))
                              (:n bundle)))))
               (expect (= ["residency-test"] (sweep)))
               (expect (= [] (:models (residency/status))))
               (expect (= 2 (vv/call-loaded spec :n)))
               (expect (= 2 @opened))
               (finally (sweep) (.delete ^File dir)))))))

(defdescribe live-asr-leases
             ;; Rebase regression: #448 idle eviction must not close a live decoder.
             (it
               "keeps a live decoder through eviction and closes its lease on another thread"
               (binding [residency/*registry* (atom {})]
                 (let [dir (.toFile (Files/createTempDirectory "vis-live-lease"
                                                               (make-array FileAttribute 0)))
                       tensors (atom 0)
                       closed (atom 0)
                       meta {"sample_rate" 24000
                             "layers" 1
                             "kv_heads" 1
                             "head_dim" 1
                             "hidden" 1
                             "samples_per_frame" 480
                             "chunk_frames" 1
                             "lookahead_frames" 0
                             "acoustic_noise_std" 0.0
                             "ids" {"text_chunk_end" 1 "eos" 2}}
                       sweep #(residency/sweep! {:limit nil :idle-ms 0})]

                   (try (with-redefs-fn {#'vv/open-asr (fn [_ _]
                                                         {:meta meta})
                                         #'vv/float-tensor (fn [& _]
                                                             (swap! tensors inc)
                                                             (reify
                                                               java.lang.AutoCloseable
                                                                 (close [_] (swap! closed inc))))}
                          (fn []
                            (let [decoder (vv/open-live-asr! dir)]
                              (try (expect (true? (get-in (residency/status) [:models 0 :in_use])))
                                   (expect (false? ((get-in @residency/*registry*
                                                            ["vibevoice-asr" :release!]))))
                                   (expect (= [] (sweep)))
                                   (let [finished (future ((:close! decoder)) true)]
                                     (expect (= true (deref finished 3000 :timeout))))
                                   ((:close! decoder))
                                   (expect (= @tensors @closed))
                                   (expect (= ["vibevoice-asr"] (sweep)))
                                   (finally ((:close! decoder)))))))
                        (finally (sweep) (.delete ^File dir))))))
             (it "releases the model lease when decoder setup fails"
                 (binding [residency/*registry* (atom {})]
                   (let [dir (.toFile (Files/createTempDirectory "vis-live-lease-fail"
                                                                 (make-array FileAttribute 0)))
                         sweep #(residency/sweep! {:limit nil :idle-ms 0})]

                     (try (with-redefs-fn {#'vv/open-asr (fn [_ _]
                                                           {:meta {"sample_rate" 16000
                                                                   "layers" 1
                                                                   "kv_heads" 1
                                                                   "head_dim" 1
                                                                   "hidden" 1
                                                                   "samples_per_frame" 480
                                                                   "chunk_frames" 1
                                                                   "lookahead_frames" 0}})}
                            (fn []
                              (expect (try (vv/open-live-asr! dir)
                                           false
                                           (catch clojure.lang.ExceptionInfo e
                                             (= :speech-live/model-contract (:type (ex-data e))))))
                              (expect (false? (get-in (residency/status) [:models 0 :in_use])))
                              (expect (= ["vibevoice-asr"] (sweep)))))
                          (finally (sweep) (.delete ^File dir)))))))
