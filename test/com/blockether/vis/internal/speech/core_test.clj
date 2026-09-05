(ns com.blockether.vis.internal.speech.core-test
  "The fixed gateway speech engines and their shared job lifecycle."
  (:require [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]
            [com.blockether.vis.internal.speech.core :as speech]))

(defn- with-only-engines!
  "Run `f` against a fixed test engine set without adding a production registry."
  [by-direction f]
  (with-redefs-fn {#'speech/engines (fn [direction]
                                      (get by-direction direction []))
                   #'speech/env-engine-id (constantly nil)}
    f))

(defn- refusal [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-message e))))

(defn- echo-engine
  [id text & [steps]]
  {:id id
   :label (name id)
   :transcribe (fn [{:keys [on-progress]}]
                 (doseq [step (or steps [])]
                   (on-progress step))
                 text)})

(defn- speaker-engine
  [id & [steps]]
  {:id id
   :label (name id)
   :voices (fn []
             [{:id :alba :label "Alba" :language :en} {:id :javert}])
   :synthesize (fn [{:keys [text voice-id on-progress]}]
                 (doseq [step (or steps [])]
                   (on-progress step))
                 (let [f (java.io.File/createTempFile "vis-speech-test" ".wav")]
                   (.deleteOnExit f)
                   (spit f
                         (str (some-> voice-id
                                      name)
                              "|"
                              text))
                   {:audio-path (str f) :sample-rate 24000}))})

(deftest the-gateway-has-one-fixed-engine-set
  (is (= [:parakeet-local] (mapv :id (speech/engines :transcribe))))
  (is (= [:piper-local :pocket-tts-local] (mapv :id (speech/engines :synthesize))))
  (is (= [:uploading :queued :preparing :transcribing :done :failed]
         (speech/direction-phases :transcribe)))
  (is (= [:uploading :queued :preparing :synthesizing :done :failed]
         (speech/direction-phases :synthesize)))
  (is (= "Unknown speech direction: :speak" (refusal #(speech/engines :speak)))))

(deftest engine-selection-is-a-lookup-not-a-registry
  (with-only-engines!
    {:transcribe [(echo-engine :parakeet-local "local") (echo-engine :other "other")]}
    (fn []
      (is (= :parakeet-local (:id (speech/default-engine :transcribe))))
      (is (= "other" (speech/transcribe! {:audio-path "/tmp/a.wav" :engine-id :other})))
      (with-redefs-fn {#'speech/env-engine-id (constantly :other)}
        #(is (= :other (:id (speech/default-engine :transcribe)))))
      (is (= "Unknown speech transcription engine: missing"
             (refusal #(speech/resolve-engine :transcribe :missing)))))))

(deftest speaking-and-listening-remain-independent
  (with-only-engines! {:transcribe [(echo-engine :listener "heard")]
                       :synthesize [(speaker-engine :speaker)]}
                      (fn []
                        (is (= {:engines [{:id "listener" :label "listener"}] :selected "listener"}
                               (speech/engines-info :transcribe)))
                        (is (= "speaker" (get-in (speech/engines-info :synthesize) [:selected]))))))

(deftest readiness-is-the-engines-own-question
  (testing "an engine that needs no preparation is simply ready"
    (is (= {:state :ready} (speech/readiness (echo-engine :remote "hi"))))
    (is (true? (speech/ready? (echo-engine :remote "hi")))))
  (testing "an engine that downloads a model reports its own progress"
    (let [engine (assoc (echo-engine :local "hi")
                   :model-state (constantly {:state :downloading :progress 42}))]
      (is (= {:state :downloading :progress 42} (speech/readiness engine)))
      (is (false? (speech/ready? engine)))))
  (testing "a readiness call that throws is a FAILED engine, never a broken gateway"
    (let [engine (assoc (echo-engine :local "hi")
                   :model-state (fn []
                                  (throw (ex-info "disk is gone" {}))))]
      (is (= :failed (:state (speech/readiness engine))))
      (is (= "disk is gone" (:error (speech/readiness engine))))))
  (testing "prepare! is the engine's own hook and answers with readiness"
    (let [started
          (atom 0)

          engine
          (assoc (echo-engine :local "hi")
            :start-download (fn []
                              (swap! started inc)
                              {:state :downloading :progress 0}))]

      (is (= {:state :downloading :progress 0} (speech/prepare! engine)))
      (is (= 1 @started))
      ;; an engine with no hook is prepared by definition
      (is (= {:state :ready} (speech/prepare! (echo-engine :remote "hi")))))))

(deftest a-voice-is-the-speaking-engines-own-catalogue
  (testing "the voices come out in the shape every surface reports"
    (is (= [{:id "alba" :label "Alba" :language "en"} {:id "javert" :label "javert"}]
           (speech/voices (speaker-engine :pocket-tts)))))
  (testing "an engine with ONE fixed voice offers no choice at all"
    (is (= [] (speech/voices (dissoc (speaker-engine :fixed) :voices)))))
  (testing "a voice a user has to install deliberately says so, and no other voice does"
    ;; A picker that cannot tell offers that voice like any other and finds out on the
    ;; click, which is the one refusal a client could have shown up front.
    (is (= [{:id "alba" :label "Alba" :language "en"} {:id "ryan" :label "Ryan" :is-opt-in true}]
           (speech/voices (assoc (speaker-engine :piper)
                            :voices (fn []
                                      [{:id :alba :label "Alba" :language :en}
                                       {:id :ryan :label "Ryan" :is-opt-in true}]))))))
  (testing "and a catalogue that refuses says why instead of looking empty"
    (is (= "the voice list is gone"
           (refusal #(speech/voices (assoc (speaker-engine :broken)
                                      :voices (fn []
                                                (throw (ex-info "the voice list is gone"
                                                                {}))))))))))

(deftest a-job-walks-the-shared-phase-vocabulary
  (testing "the phases are ordered and shared by every surface"
    (is (= [:uploading :queued :preparing :transcribing :synthesizing :done :failed] speech/phases))
    (is (speech/phase? :transcribing))
    (is (not (speech/phase? :almost-there))))
  (let [seen
        (atom [])

        engine
        {:id :fake
         :label "fake"
         :transcribe (fn [{:keys [on-progress job-id]}]
                       (on-progress {:phase :preparing :progress 50})
                       (swap! seen conj (select-keys (speech/job job-id) [:phase :progress]))
                       (on-progress {:phase :transcribing :progress 10})
                       (swap! seen conj (select-keys (speech/job job-id) [:phase :progress]))
                       "hello world")}]

    (with-only-engines!
      {:transcribe [engine]}
      (fn []
        (speech/reset-jobs!)
        (let [done (speech/submit-sync! :transcribe {:audio-path "/tmp/a.wav"})]
          (is (= [{:phase "preparing" :progress 50} {:phase "transcribing" :progress 10}] @seen))
          (is (= "done" (:phase done)))
          (is (= "transcribe" (:direction done)))
          (is (= 100 (:progress done)))
          (is (true? (:is-done done)))
          (is (= "hello world" (:text done)))
          (is (= "fake" (:engine done)))
          (is (= done (speech/job (:id done))))
          (is (nil? (:audio-path (speech/job (:id done)))))
          (speech/forget! (:id done))
          (is (nil? (speech/job (:id done)))))))))

(deftest a-spoken-reply-is-a-job-like-any-other
  (with-only-engines!
    {:synthesize [(speaker-engine :pocket-tts [{:phase :synthesizing :progress 40}])]}
    (fn []
      (speech/reset-jobs!)
      (let [done (speech/submit-sync! :synthesize {:text "the build is green" :voice-id :alba})]
        (testing "it walks the same lifecycle, in its own working phase"
          (is (= "done" (:phase done)))
          (is (= "synthesize" (:direction done)))
          (is (= "pocket-tts" (:engine done)))
          (is (= "alba" (:voice done)))
          (is (= 100 (:progress done)))
          (is (true? (:is-done done)))
          (is (nil? (:error done))))
        (testing "the answer is a FILE, described by the facts a player needs"
          (is (= "audio/wav" (get-in done [:audio :media-type])))
          (is (= 24000 (get-in done [:audio :sample-rate])))
          (is (pos? (long (get-in done [:audio :bytes])))))
        (testing "and the path stays host-side, exactly like a recording's"
          (is (nil? (:audio-path done)))
          (is (nil? (get-in done [:audio :audio-path])))
          (let [f (java.io.File. (str (speech/job-audio-path (:id done))))]
            (is (.isFile f))
            (is (= "alba|the build is green" (slurp f)))
            (.delete f)))
        (testing "a spoken job is forgotten like any other"
          (speech/forget! (:id done))
          (is (nil? (speech/job (:id done))))
          (is (nil? (speech/job-audio-path (:id done))))))))
  (with-only-engines! {:synthesize [{:id :mute
                                     :synthesize (fn [{:keys [on-progress]}]
                                                   (on-progress {:phase :synthesizing :progress 30})
                                                   (throw (ex-info "the vocoder gave up" {})))}]}
                      (fn []
                        (speech/reset-jobs!)
                        (let [failed (speech/submit-sync! :synthesize {:text "anything"})]
                          (testing "a speaking engine that dies fails the job with a readable line"
                            (is (= "failed" (:phase failed)))
                            (is (= "synthesize" (:direction failed)))
                            (is (= 30 (:progress failed)))
                            (is (= "the vocoder gave up" (:error failed)))
                            (is (nil? (:audio failed)))))))
  (with-only-engines! {:synthesize [{:id :empty-handed :synthesize (constantly nil)}]}
                      (fn []
                        (speech/reset-jobs!)
                        (testing
                          "and an engine that answers with no file is a failure, never silent :done"
                          (is (= "Synthesis engine returned no audio file"
                                 (:error (speech/submit-sync! :synthesize {:text "anything"}))))))))

(deftest synthesis-answers-whoever-asks-for-it
  ;; The `speech` feature toggle is gone. Whether a reply is spoken belongs to the
  ;; SURFACE's voice conversation (the TUI mode, the app's armed conversation); a
  ;; global flag in front of every synthesis said "off" in a place nobody was speaking
  ;; from and could not say "on, for this conversation only".
  (with-only-engines! {:synthesize [(speaker-engine :pocket-tts)]}
                      (fn []
                        (speech/reset-jobs!)
                        (testing
                          "a line asked for is a line spoken - nothing global stands in front of it"
                          (let [spoken (speech/synthesize! {:text "out loud" :voice-id :javert})]
                            (is (= "audio/wav" (:media-type spoken)))
                            (let [f (java.io.File. (str (:audio-path spoken)))]
                              (is (= "javert|out loud" (slurp f)))
                              (.delete f))))
                        (testing "listening is a different direction and is unaffected"
                          (is (= [] (speech/engines :transcribe)))))))

(deftest progress-only-ever-moves-forward
  (with-only-engines!
    {:transcribe [{:id :jumpy
                   :transcribe (fn [{:keys [on-progress job-id]}]
                                 (on-progress {:phase :transcribing :progress 80})
                                 (on-progress {:phase :transcribing :progress 5})
                                 (on-progress {:progress 4000})
                                 (str (:progress (speech/job job-id))))}]}
    (fn []
      (speech/reset-jobs!)
      ;; a chunked engine that restarts its counter must never make the bar go
      ;; backwards in front of a human, and a bad percentage is clamped, not shown
      (is (= "100" (:text (speech/submit-sync! :transcribe {:audio-path "/tmp/a.wav"})))))))

(deftest a-failing-engine-fails-the-job-with-a-readable-line
  (with-only-engines! {:transcribe [{:id :broken
                                     :transcribe (fn [_]
                                                   (throw (ex-info "model file is corrupt" {})))}]}
                      (fn []
                        (speech/reset-jobs!)
                        (let [collected
                              (atom nil)

                              job
                              (speech/submit-sync! :transcribe
                                                   {:audio-path "/tmp/a.wav"
                                                    :on-done #(reset! collected %)})]

                          (is (= "failed" (:phase job)))
                          (is (true? (:is-done job)))
                          (is (= "model file is corrupt" (:error job)))
                          (is (nil? (:text job)))
                          (testing "on-done still runs, so a temp recording is always deleted"
                            (is (= (:id job) (:id @collected)))))))
  (testing "an unknown engine is refused BEFORE a job exists"
    (with-only-engines! {:transcribe [(echo-engine :fake "hi")]}
                        (fn []
                          (speech/reset-jobs!)
                          (is (= "Unknown speech transcription engine: nope"
                                 (refusal #(speech/submit! :transcribe
                                                           {:audio-path "/tmp/a.wav"
                                                            :engine-id :nope}))))))))

(deftest submit-answers-immediately-and-the-job-finishes-on-its-own-thread
  (let [release (promise)]
    (with-only-engines! {:transcribe [{:id :slow
                                       :transcribe (fn [_]
                                                     @release
                                                     "eventually")}]}
                        (fn []
                          (speech/reset-jobs!)
                          (let [queued (speech/submit! :transcribe {:audio-path "/tmp/a.wav"})]
                            (testing "the caller can answer 202 without waiting for a single word"
                              (is (string? (:id queued)))
                              (is (false? (:is-done queued)))
                              (is (nil? (:text queued)))
                              (is (contains? #{"queued" "preparing"} (:phase queued))))
                            (deliver release :go)
                            (let [deadline (+ (System/currentTimeMillis) 5000)]
                              (while (and (not (:is-done (speech/job (:id queued))))
                                          (< (System/currentTimeMillis) deadline))
                                (Thread/sleep 5)))
                            (is (= "eventually" (:text (speech/job (:id queued))))))))))

(deftest a-finished-job-is-swept-when-its-ttl-runs-out
  (let [ttl
        (long @#'speech/job-ttl-ms)

        t
        (System/currentTimeMillis)

        stale
        (- t ttl 60000)

        kept
        (#'speech/sweep
         {"spoken" {:id "spoken"
                    :direction :synthesize
                    :phase :done
                    :progress 100
                    :created-at stale
                    :updated-at stale}
          "heard" {:id "heard"
                   :direction :transcribe
                   :phase :done
                   :progress 100
                   :created-at t
                   :updated-at t}
          "running" {:id "running"
                     :direction :transcribe
                     :phase :transcribing
                     :progress 10
                     :created-at stale
                     :updated-at stale}})]

    (testing "a job nobody collected is dropped once its TTL passed - in both directions"
      (is (= #{"heard" "running"} (set (keys kept)))))
    (testing "and work still RUNNING is never swept, however long it has taken"
      (is (= :transcribing (get-in kept ["running" :phase]))))))

(deftest error-message-reads-like-a-sentence
  (is (= "boom" (speech/error-message (ex-info "boom" {}))))
  (is (= "root cause"
         (speech/error-message (java.io.IOException. (RuntimeException. "root cause")))))
  (is (string? (speech/error-message (NullPointerException.)))))

;; A job PUSHES - watchers, not polls

(defn- wait-done!
  "Block until `job-id` is terminal (or 5s pass) and return its public job."
  [job-id]
  (let [deadline (+ (System/currentTimeMillis) 5000)]
    (while (and (not (:is-done (speech/job job-id))) (< (System/currentTimeMillis) deadline))
      (Thread/sleep 5))
    (speech/job job-id)))

(deftest a-watcher-is-told-every-step-as-it-happens
  ;; The percentage is only worth showing while the work is happening, so a
  ;; surface must never have to ASK for it: the gateway's SSE body is one of
  ;; these watchers, and it writes a frame the instant the engine reports.
  (let [armed
        (promise)

        seen
        (atom [])]

    (with-only-engines!
      {:transcribe [{:id :steps
                     :transcribe (fn [{:keys [on-progress]}]
                                   (on-progress {:phase :preparing :progress 50})
                                   @armed
                                   (on-progress {:phase :transcribing :progress 20})
                                   (on-progress {:phase :transcribing :progress 80})
                                   "the words")}]}
      (fn []
        (speech/reset-jobs!)
        (let [job
              (speech/submit! :transcribe {:audio-path "/tmp/a.wav"})

              unwatch
              (speech/watch! (:id job)
                             (fn [j]
                               (swap! seen conj [(:phase j) (:progress j)])))]

          (deliver armed :go)
          (let [final (wait-done! (:id job))]
            (unwatch)
            (testing "each step arrives on its own, in order, ending at the transcript"
              ;; whatever the engine had already reported before the watcher
              ;; arrived is the SNAPSHOT's job, not the stream's
              (is (= [["transcribing" 20] ["transcribing" 80] ["done" 100]]
                     (vec (remove (comp #{"queued" "preparing"} first) @seen)))
                  (pr-str @seen))
              (testing "and no filler frame that repeats what DONE already says"
                (is (not-any? #{["transcribing" 100]} @seen))))
            (testing "the terminal step carries the text, so nothing follows it"
              (is (= "the words" (:text final))))))))))

(deftest a-spoken-job-streams-its-own-phase-too
  (let [armed
        (promise)

        seen
        (atom [])]

    (with-only-engines!
      {:synthesize [{:id :streamer
                     :synthesize (fn [{:keys [on-progress text]}]
                                   @armed
                                   (on-progress {:phase :synthesizing :progress 25})
                                   (on-progress {:phase :synthesizing :progress 75})
                                   (let [f (java.io.File/createTempFile "vis-speech-test" ".wav")]
                                     (.deleteOnExit f)
                                     (spit f text)
                                     (str f)))}]}
      (fn []
        (speech/reset-jobs!)
        (let [job
              (speech/submit! :synthesize {:text "spoken aloud"})

              unwatch
              (speech/watch! (:id job)
                             (fn [j]
                               (swap! seen conj [(:phase j) (:progress j)])))]

          (deliver armed :go)
          (let [final (wait-done! (:id job))]
            (unwatch)
            (testing "the human watching a reply being spoken sees SYNTHESIZING, not transcribing"
              (is (= [["synthesizing" 25] ["synthesizing" 75] ["done" 100]]
                     (vec (remove (comp #{"queued" "preparing"} first) @seen)))
                  (pr-str @seen)))
            (testing "and a bare path from the engine is still a described file"
              (is (= "audio/wav" (get-in final [:audio :media-type])))
              (let [f (java.io.File. (str (speech/job-audio-path (:id final))))]
                (is (= "spoken aloud" (slurp f)))
                (.delete f)))))))))

(deftest a-watcher-that-let-go-is-never-called-again
  (let [gate
        (promise)

        heard
        (atom [])]

    (with-only-engines! {:transcribe [{:id :gated
                                       :transcribe (fn [_]
                                                     @gate
                                                     "late")}]}
                        (fn []
                          (speech/reset-jobs!)
                          (let [job
                                (speech/submit! :transcribe {:audio-path "/tmp/a.wav"})

                                unwatch
                                (speech/watch! (:id job)
                                               (fn [j]
                                                 (swap! heard conj (:phase j))))]

                            (unwatch)
                            (deliver gate :go)
                            (is (= "done" (:phase (wait-done! (:id job)))))
                            (testing "a disconnected client costs the engine nothing"
                              (is (not (some #{"done"} @heard)))))))))

(deftest a-failed-job-keeps-the-percentage-it-died-at
  ;; :failed is not a new scale to start over on - where the engine gave up is
  ;; part of the report.
  (with-only-engines! {:transcribe [{:id :breaks
                                     :transcribe (fn [{:keys [on-progress]}]
                                                   (on-progress {:phase :transcribing :progress 60})
                                                   (throw (ex-info "the decoder died" {})))}]}
                      (fn []
                        (speech/reset-jobs!)
                        (let [job (speech/submit-sync! :transcribe {:audio-path "/tmp/a.wav"})]
                          (is (= "failed" (:phase job)))
                          (is (= 60 (:progress job)))
                          (is (= "the decoder died" (:error job)))))))

;; A voice can be BROUGHT: the engine that clones learns one from a recording

(defn- cloning-engine
  "A speaking engine that learns a voice from a recording, as a local cloning model
   does: the clip IS the voice, so an import lands in the same catalogue a caller
   picks from."
  [store]
  (assoc (speaker-engine :cloner)
    :voices (fn []
              (vec (vals @store)))
    :import-voice (fn [{:keys [path voice-name language text]}]
                    (let [voice {:id (.replace (.toLowerCase (str voice-name)) " " "-")
                                 :label voice-name
                                 :language language
                                 :clip path
                                 :clip-text text
                                 :is-imported true}]
                      (swap! store assoc (:id voice) voice)
                      voice))
    :forget-voice (fn [id]
                    (let [had? (contains? @store id)]
                      (swap! store dissoc id)
                      had?))))

(deftest a-voice-can-be-brought-instead-of-shipped
  ;; A cloning engine's catalogue is not fixed at build time. A surface that offers
  ;; "add a voice" has to know THAT it may, hand over a recording, see what it became
  ;; and be able to take it back - and an engine that cannot clone must refuse by
  ;; name instead of accepting the upload and doing nothing with it.
  (let [store (atom {})]
    (with-only-engines!
      {:synthesize [(cloning-engine store)]}
      (fn []
        (let [engine (first (speech/engines :synthesize))]
          (testing "the capability is advertised, so nothing offers what cannot work"
            (is (= {:id "cloner" :label "cloner" :is-voice-import true}
                   (speech/public-engine engine)))
            (is (true? (:is-voice-import (first (:engines (speech/engines-info :synthesize)))))))
          (testing "the recording becomes a voice in the catalogue a caller picks from"
            (is (= {:id "my-own" :label "My Own" :language "en-GB" :is-imported true}
                   (speech/import-voice! engine
                                         {:path "/tmp/clip.wav"
                                          :voice-name "My Own"
                                          :language "en-GB"
                                          :text "what the clip says"})))
            (is (= [{:id "my-own" :label "My Own" :language "en-GB" :is-imported true}]
                   (speech/voices engine)))
            ;; which file backs a voice stays the engine's business
            (is (not-any? :clip (speech/voices engine))))
          (testing "forgetting twice is the same outcome, never an error"
            (is (true? (speech/forget-voice! engine "my-own")))
            (is (empty? (speech/voices engine)))
            (is (false? (speech/forget-voice! engine "my-own"))))))))
  (testing "an engine that cannot learn a voice refuses by name"
    (let [plain (speaker-engine :pocket-tts)]
      (is (nil? (:is-voice-import (speech/public-engine plain))))
      (is (= "pocket-tts cannot learn a voice from a recording"
             (refusal #(speech/import-voice! plain {:path "/tmp/clip.wav" :voice-name "Mine"}))))
      (is (= "pocket-tts does not keep voices of its own"
             (refusal #(speech/forget-voice! plain "mine")))))))

;; A voice is a NAME until you hear it

(defn- sampled-engine
  "A speaking engine that can play a voice back: one voice already sampled, one
   that could be cheaply, one that could not be at all."
  [prepared]
  (assoc (speaker-engine :sampler)
    :voices (constantly [{:id :kristin :label "Kristin"} {:id :cori :label "Cori"}
                         {:id :ryan :label "Ryan"}])
    :voice-sample (fn [id]
                    (case (keyword id)
                      :kristin
                      {:audio-path "/tmp/kristin.wav" :media-type "audio/wav"}

                      :cori
                      {:is-preparable true}

                      nil))
    :prepare-voice-sample (fn [id]
                            (swap! prepared conj id)
                            {:audio-path (str "/tmp/" (name id) ".wav") :media-type "audio/wav"})))

(deftest a-voice-can-be-heard-before-it-is-chosen
  ;; A list of names cannot say what a voice sounds like, and the surfaces must not
  ;; guess: a play button that turns into a 116 MB download is a trap, so the
  ;; catalogue says per voice whether it can be played now, played after something
  ;; small, or not at all.
  (let [prepared
        (atom [])

        engine
        (sampled-engine prepared)]

    (testing "the catalogue says what a play button may promise, per voice"
      (is (= [{:id "kristin" :label "Kristin" :is-sample-ready true}
              {:id "cori" :label "Cori" :is-sample-preparable true} {:id "ryan" :label "Ryan"}]
             (speech/voices engine))))
    (testing "a sample that exists is handed over without preparing anything"
      (is (= {:audio-path "/tmp/kristin.wav" :media-type "audio/wav"}
             (speech/voice-sample! engine "kristin")))
      (is (= [] @prepared)))
    (testing "a preparable one is made on the spot"
      (is (= {:audio-path "/tmp/cori.wav" :media-type "audio/wav"}
             (speech/voice-sample! engine "cori")))
      (is (= ["cori"] @prepared)))
    (testing "a voice with no sample answers nothing, and prepares nothing"
      (is (nil? (speech/voice-sample! engine "ryan")))
      (is (= ["cori"] @prepared))))
  (testing "an engine that declares no sample seam offers no play button at all"
    (let [plain (assoc (speaker-engine :fixed) :voices (constantly [{:id :one :label "One"}]))]
      (is (= [{:id "one" :label "One"}] (speech/voices plain)))
      (is (nil? (speech/voice-sample! plain "one")))))
  (testing "a sample lookup that throws is a voice without a sample, not a broken catalogue"
    (let [angry (assoc (speaker-engine :angry)
                  :voices (constantly [{:id :one :label "One"}])
                  :voice-sample (fn [_]
                                  (throw (ex-info "the model store is gone" {}))))]
      (is (= [{:id "one" :label "One"}] (speech/voices angry)))
      (is (nil? (speech/voice-sample! angry "one"))))))
