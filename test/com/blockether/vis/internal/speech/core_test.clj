(ns com.blockether.vis.internal.speech.core-test
  "The fixed gateway speech engines and their shared job lifecycle."
  (:require [lazytest.core :refer [defdescribe expect it]]
            [com.blockether.vis.internal.speech.assets :as assets]
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

(defdescribe the-gateway-has-one-fixed-engine-set
             (it
               "the gateway has one fixed engine set"
               (expect (= [:parakeet-local] (mapv :id (speech/engines :transcribe))))
               (expect (= [:piper-local :pocket-tts-local] (mapv :id (speech/engines :synthesize))))
               (expect (= [:uploading :queued :preparing :transcribing :done :failed]
                          (speech/direction-phases :transcribe)))
               (expect (= [:uploading :queued :preparing :synthesizing :done :failed]
                          (speech/direction-phases :synthesize)))
               (expect (= "Unknown speech direction: :speak" (refusal #(speech/engines :speak))))))

(defdescribe
  engine-selection-is-a-lookup-not-a-registry
  (it "engine selection is a lookup not a registry"
      (with-only-engines!
        {:transcribe [(echo-engine :parakeet-local "local") (echo-engine :other "other")]}
        (fn []
          (expect (= :parakeet-local (:id (speech/default-engine :transcribe))))
          (expect (= "other"
                     (:text (speech/transcribe! {:audio-path "/tmp/a.wav" :engine-id :other}))))
          (with-redefs-fn {#'speech/env-engine-id (constantly :other)}
            #(expect (= :other (:id (speech/default-engine :transcribe)))))
          (expect (= "Unknown speech transcription engine: missing"
                     (refusal #(speech/resolve-engine :transcribe :missing))))))))

(defdescribe
  speaking-and-listening-remain-independent
  (it "speaking and listening remain independent"
      (with-only-engines!
        {:transcribe [(echo-engine :listener "heard")] :synthesize [(speaker-engine :speaker)]}
        (fn []
          (expect (= {:engines [{:id "listener" :label "listener"}] :selected "listener"}
                     (speech/engines-info :transcribe)))
          (expect (= "speaker" (get-in (speech/engines-info :synthesize) [:selected])))))))

(defdescribe readiness-is-the-engines-own-question
             (it "an engine that needs no preparation is simply ready"
                 (expect (= {:state :ready} (speech/readiness (echo-engine :remote "hi"))))
                 (expect (true? (speech/ready? (echo-engine :remote "hi")))))
             (it "an engine that downloads a model reports its own progress"
                 (let [engine (assoc (echo-engine :local "hi")
                                :model-state (constantly {:state :downloading :progress 42}))]
                   (expect (= {:state :downloading :progress 42} (speech/readiness engine)))
                   (expect (false? (speech/ready? engine)))))
             (it "a readiness call that throws is a FAILED engine, never a broken gateway"
                 (let [engine (assoc (echo-engine :local "hi")
                                :model-state (fn []
                                               (throw (ex-info "disk is gone" {}))))]
                   (expect (= :failed (:state (speech/readiness engine))))
                   (expect (= "disk is gone" (:error (speech/readiness engine))))))
             (it "prepare! is the engine's own hook and answers with readiness"
                 (let [started
                       (atom 0)

                       engine
                       (assoc (echo-engine :local "hi")
                         :start-download (fn []
                                           (swap! started inc)
                                           {:state :downloading :progress 0}))]

                   (expect (= {:state :downloading :progress 0} (speech/prepare! engine)))
                   (expect (= 1 @started))
                   ;; an engine with no hook is prepared by definition
                   (expect (= {:state :ready} (speech/prepare! (echo-engine :remote "hi")))))))

(defdescribe
  a-voice-is-the-speaking-engines-own-catalogue
  (it "the voices come out in the shape every surface reports"
      (expect (= [{:id "alba" :label "Alba" :language "en"} {:id "javert" :label "javert"}]
                 (speech/voices (speaker-engine :pocket-tts)))))
  (it "an engine with ONE fixed voice offers no choice at all"
      (expect (= [] (speech/voices (dissoc (speaker-engine :fixed) :voices)))))
  (it "a voice a user has to install deliberately says so, and no other voice does"
      ;; A picker that cannot tell offers that voice like any other and finds out on the
      ;; click, which is the one refusal a client could have shown up front.
      (expect (= [{:id "alba" :label "Alba" :language "en"}
                  {:id "ryan" :label "Ryan" :is-opt-in true}]
                 (speech/voices (assoc (speaker-engine :piper)
                                  :voices (fn []
                                            [{:id :alba :label "Alba" :language :en}
                                             {:id :ryan :label "Ryan" :is-opt-in true}]))))))
  (it "and a catalogue that refuses says why instead of looking empty"
      (expect (= "the voice list is gone"
                 (refusal #(speech/voices (assoc (speaker-engine :broken)
                                            :voices (fn []
                                                      (throw (ex-info "the voice list is gone"
                                                                      {}))))))))))

(defdescribe
  a-job-walks-the-shared-phase-vocabulary
  (it "a job walks the shared phase vocabulary"
      ;; the phases are ordered and shared by every surface
      (expect (= [:uploading :queued :preparing :transcribing :synthesizing :done :failed]
                 speech/phases))
      (expect (speech/phase? :transcribing))
      (expect (not (speech/phase? :almost-there)))
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
              (expect (= [{:phase "preparing" :progress 50} {:phase "transcribing" :progress 10}]
                         @seen))
              (expect (= "done" (:phase done)))
              (expect (= "transcribe" (:direction done)))
              (expect (= 100 (:progress done)))
              (expect (true? (:is-done done)))
              (expect (= "hello world" (:text done)))
              (expect (= "fake" (:engine done)))
              (expect (= done (speech/job (:id done))))
              (expect (nil? (:audio-path (speech/job (:id done)))))
              (speech/forget! (:id done))
              (expect (nil? (speech/job (:id done))))))))))

(defdescribe
  a-spoken-reply-is-a-job-like-any-other
  (it "a spoken reply is a job like any other"
      (with-only-engines!
        {:synthesize [(speaker-engine :pocket-tts [{:phase :synthesizing :progress 40}])]}
        (fn []
          (speech/reset-jobs!)
          (let [done (speech/submit-sync! :synthesize {:text "the build is green" :voice-id :alba})]
            ;; it walks the same lifecycle, in its own working phase
            (expect (= "done" (:phase done)))
            (expect (= "synthesize" (:direction done)))
            (expect (= "pocket-tts" (:engine done)))
            (expect (= "alba" (:voice done)))
            (expect (= 100 (:progress done)))
            (expect (true? (:is-done done)))
            (expect (nil? (:error done)))
            ;; the answer is a FILE, described by the facts a player needs
            (expect (= "audio/wav" (get-in done [:audio :media-type])))
            (expect (= 24000 (get-in done [:audio :sample-rate])))
            (expect (pos? (long (get-in done [:audio :bytes]))))
            ;; and the path stays host-side, exactly like a recording's
            (expect (nil? (:audio-path done)))
            (expect (nil? (get-in done [:audio :audio-path])))
            (let [f (java.io.File. (str (speech/job-audio-path (:id done))))]
              (expect (.isFile f))
              (expect (= "alba|the build is green" (slurp f)))
              (.delete f))
            ;; a spoken job is forgotten like any other
            (speech/forget! (:id done))
            (expect (nil? (speech/job (:id done))))
            (expect (nil? (speech/job-audio-path (:id done)))))))
      (with-only-engines! {:synthesize [{:id :mute
                                         :synthesize
                                         (fn [{:keys [on-progress]}]
                                           (on-progress {:phase :synthesizing :progress 30})
                                           (throw (ex-info "the vocoder gave up" {})))}]}
                          (fn []
                            (speech/reset-jobs!)
                            (let [failed (speech/submit-sync! :synthesize {:text "anything"})]
                              ;; a speaking engine that dies fails the job with a readable line
                              (expect (= "failed" (:phase failed)))
                              (expect (= "synthesize" (:direction failed)))
                              (expect (= 30 (:progress failed)))
                              (expect (= "the vocoder gave up" (:error failed)))
                              (expect (nil? (:audio failed))))))
      (with-only-engines! {:synthesize [{:id :empty-handed :synthesize (constantly nil)}]}
                          (fn []
                            (speech/reset-jobs!)
                            ;; and an engine that answers with no file is a failure, never silent :done
                            (expect (= "Synthesis engine returned no audio file"
                                       (:error (speech/submit-sync! :synthesize
                                                                    {:text "anything"}))))))))

(defdescribe synthesis-answers-whoever-asks-for-it
             (it "synthesis answers whoever asks for it"
                 ;; The `speech` feature toggle is gone. Whether a reply is spoken belongs to the
                 ;; SURFACE's voice conversation (the TUI mode, the app's armed conversation); a
                 ;; global flag in front of every synthesis said "off" in a place nobody was speaking
                 ;; from and could not say "on, for this conversation only".
                 (with-only-engines! {:synthesize [(speaker-engine :pocket-tts)]}
                                     (fn []
                                       (speech/reset-jobs!)
                                       ;; a line asked for is a line spoken - nothing global stands in front of it
                                       (let [spoken (speech/synthesize! {:text "out loud"
                                                                         :voice-id :javert})]
                                         (expect (= "audio/wav" (:media-type spoken)))
                                         (let [f (java.io.File. (str (:audio-path spoken)))]
                                           (expect (= "javert|out loud" (slurp f)))
                                           (.delete f)))
                                       ;; listening is a different direction and is unaffected
                                       (expect (= [] (speech/engines :transcribe)))))))

(defdescribe progress-only-ever-moves-forward
             (it "progress only ever moves forward"
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
                     (expect (= "100"
                                (:text (speech/submit-sync! :transcribe
                                                            {:audio-path "/tmp/a.wav"}))))))))

(defdescribe
  a-failing-engine-fails-the-job-with-a-readable-line
  (it "a failing engine fails the job with a readable line"
      (with-only-engines!
        {:transcribe [{:id :broken
                       :transcribe (fn [_]
                                     (throw (ex-info "model file is corrupt" {})))}]}
        (fn []
          (speech/reset-jobs!)
          (let [collected
                (atom nil)

                job
                (speech/submit-sync! :transcribe
                                     {:audio-path "/tmp/a.wav" :on-done #(reset! collected %)})]

            (expect (= "failed" (:phase job)))
            (expect (true? (:is-done job)))
            (expect (= "model file is corrupt" (:error job)))
            (expect (nil? (:text job)))
            ;; on-done still runs, so a temp recording is always deleted
            (expect (= (:id job) (:id @collected))))))
      ;; an unknown engine is refused BEFORE a job exists
      (with-only-engines! {:transcribe [(echo-engine :fake "hi")]}
                          (fn []
                            (speech/reset-jobs!)
                            (expect (= "Unknown speech transcription engine: nope"
                                       (refusal #(speech/submit! :transcribe
                                                                 {:audio-path "/tmp/a.wav"
                                                                  :engine-id :nope}))))))))

(defdescribe submit-answers-immediately-and-the-job-finishes-on-its-own-thread
             (it "submit answers immediately and the job finishes on its own thread"
                 (let [release (promise)]
                   (with-only-engines!
                     {:transcribe [{:id :slow
                                    :transcribe (fn [_]
                                                  @release
                                                  "eventually")}]}
                     (fn []
                       (speech/reset-jobs!)
                       (let [queued (speech/submit! :transcribe {:audio-path "/tmp/a.wav"})]
                         ;; the caller can answer 202 without waiting for a single word
                         (expect (string? (:id queued)))
                         (expect (false? (:is-done queued)))
                         (expect (nil? (:text queued)))
                         (expect (contains? #{"queued" "preparing"} (:phase queued)))
                         (deliver release :go)
                         (let [deadline (+ (System/currentTimeMillis) 5000)]
                           (while (and (not (:is-done (speech/job (:id queued))))
                                       (< (System/currentTimeMillis) deadline))
                             (Thread/sleep 5)))
                         (expect (= "eventually" (:text (speech/job (:id queued)))))))))))

(defdescribe
  a-finished-job-is-swept-when-its-ttl-runs-out
  (it
    "a finished job is swept when its ttl runs out"
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

      ;; a job nobody collected is dropped once its TTL passed - in both directions
      (expect (= #{"heard" "running"} (set (keys kept))))
      ;; and work still RUNNING is never swept, however long it has taken
      (expect (= :transcribing (get-in kept ["running" :phase]))))))

(defdescribe error-message-reads-like-a-sentence
             (it "error message reads like a sentence"
                 (expect (= "boom" (speech/error-message (ex-info "boom" {}))))
                 (expect (= "root cause"
                            (speech/error-message (java.io.IOException. (RuntimeException.
                                                                          "root cause")))))
                 (expect (string? (speech/error-message (NullPointerException.))))))

;; A job PUSHES - watchers, not polls

(defn- wait-done!
  "Block until `job-id` is terminal (or 5s pass) and return its public job."
  [job-id]
  (let [deadline (+ (System/currentTimeMillis) 5000)]
    (while (and (not (:is-done (speech/job job-id))) (< (System/currentTimeMillis) deadline))
      (Thread/sleep 5))
    (speech/job job-id)))

(defdescribe
  a-watcher-is-told-every-step-as-it-happens
  (it "a watcher is told every step as it happens"
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
                ;; each step arrives on its own, in order, ending at the transcript
                ;; whatever the engine had already reported before the watcher
                ;; arrived is the SNAPSHOT's job, not the stream's
                (expect (= [["transcribing" 20] ["transcribing" 80] ["done" 100]]
                           (vec (remove (comp #{"queued" "preparing"} first) @seen)))
                        (pr-str @seen))
                ;; and no filler frame that repeats what DONE already says
                (expect (not-any? #{["transcribing" 100]} @seen))
                ;; the terminal step carries the text, so nothing follows it
                (expect (= "the words" (:text final))))))))))

(defdescribe
  a-spoken-job-streams-its-own-phase-too
  (it
    "a spoken job streams its own phase too"
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
              ;; the human watching a reply being spoken sees SYNTHESIZING, not transcribing
              (expect (= [["synthesizing" 25] ["synthesizing" 75] ["done" 100]]
                         (vec (remove (comp #{"queued" "preparing"} first) @seen)))
                      (pr-str @seen))
              ;; and a bare path from the engine is still a described file
              (expect (= "audio/wav" (get-in final [:audio :media-type])))
              (let [f (java.io.File. (str (speech/job-audio-path (:id final))))]
                (expect (= "spoken aloud" (slurp f)))
                (.delete f)))))))))

(defdescribe a-watcher-that-let-go-is-never-called-again
             (it "a watcher that let go is never called again"
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
                                               (speech/submit! :transcribe
                                                               {:audio-path "/tmp/a.wav"})

                                               unwatch
                                               (speech/watch! (:id job)
                                                              (fn [j]
                                                                (swap! heard conj (:phase j))))]

                                           (unwatch)
                                           (deliver gate :go)
                                           (expect (= "done" (:phase (wait-done! (:id job)))))
                                           ;; a disconnected client costs the engine nothing
                                           (expect (not (some #{"done"} @heard)))))))))

(defdescribe a-failed-job-keeps-the-percentage-it-died-at
             (it "a failed job keeps the percentage it died at"
                 ;; :failed is not a new scale to start over on - where the engine gave up is
                 ;; part of the report.
                 (with-only-engines!
                   {:transcribe [{:id :breaks
                                  :transcribe (fn [{:keys [on-progress]}]
                                                (on-progress {:phase :transcribing :progress 60})
                                                (throw (ex-info "the decoder died" {})))}]}
                   (fn []
                     (speech/reset-jobs!)
                     (let [job (speech/submit-sync! :transcribe {:audio-path "/tmp/a.wav"})]
                       (expect (= "failed" (:phase job)))
                       (expect (= 60 (:progress job)))
                       (expect (= "the decoder died" (:error job))))))))

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

(defdescribe
  a-voice-can-be-brought-instead-of-shipped
  (it "a voice can be brought instead of shipped"
      ;; A cloning engine's catalogue is not fixed at build time. A surface that offers
      ;; "add a voice" has to know THAT it may, hand over a recording, see what it became
      ;; and be able to take it back - and an engine that cannot clone must refuse by
      ;; name instead of accepting the upload and doing nothing with it.
      (let [store (atom {})]
        (with-only-engines!
          {:synthesize [(cloning-engine store)]}
          (fn []
            (let [engine (first (speech/engines :synthesize))]
              ;; the capability is advertised, so nothing offers what cannot work
              (expect (= {:id "cloner" :label "cloner" :is-voice-import true}
                         (speech/public-engine engine)))
              (expect (true? (:is-voice-import (first (:engines (speech/engines-info
                                                                  :synthesize))))))
              ;; the recording becomes a voice in the catalogue a caller picks from
              (expect (= {:id "my-own" :label "My Own" :language "en-GB" :is-imported true}
                         (speech/import-voice! engine
                                               {:path "/tmp/clip.wav"
                                                :voice-name "My Own"
                                                :language "en-GB"
                                                :text "what the clip says"})))
              (expect (= [{:id "my-own" :label "My Own" :language "en-GB" :is-imported true}]
                         (speech/voices engine)))
              ;; which file backs a voice stays the engine's business
              (expect (not-any? :clip (speech/voices engine)))
              ;; forgetting twice is the same outcome, never an error
              (expect (true? (speech/forget-voice! engine "my-own")))
              (expect (empty? (speech/voices engine)))
              (expect (false? (speech/forget-voice! engine "my-own")))))))
      ;; an engine that cannot learn a voice refuses by name
      (let [plain (speaker-engine :pocket-tts)]
        (expect (nil? (:is-voice-import (speech/public-engine plain))))
        (expect (= "pocket-tts cannot learn a voice from a recording"
                   (refusal #(speech/import-voice! plain
                                                   {:path "/tmp/clip.wav" :voice-name "Mine"}))))
        (expect (= "pocket-tts does not keep voices of its own"
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

(defdescribe
  a-voice-can-be-heard-before-it-is-chosen
  (it "a voice can be heard before it is chosen"
      ;; A list of names cannot say what a voice sounds like, and the surfaces must not
      ;; guess: a play button that turns into a 116 MB download is a trap, so the
      ;; catalogue says per voice whether it can be played now, played after something
      ;; small, or not at all.
      (let [prepared
            (atom [])

            engine
            (sampled-engine prepared)]

        ;; the catalogue says what a play button may promise, per voice
        (expect (= [{:id "kristin" :label "Kristin" :is-sample-ready true}
                    {:id "cori" :label "Cori" :is-sample-preparable true}
                    {:id "ryan" :label "Ryan"}]
                   (speech/voices engine)))
        ;; a sample that exists is handed over without preparing anything
        (expect (= {:audio-path "/tmp/kristin.wav" :media-type "audio/wav"}
                   (speech/voice-sample! engine "kristin")))
        (expect (= [] @prepared))
        ;; a preparable one is made on the spot
        (expect (= {:audio-path "/tmp/cori.wav" :media-type "audio/wav"}
                   (speech/voice-sample! engine "cori")))
        (expect (= ["cori"] @prepared))
        ;; a voice with no sample answers nothing, and prepares nothing
        (expect (nil? (speech/voice-sample! engine "ryan")))
        (expect (= ["cori"] @prepared)))
      ;; an engine that declares no sample seam offers no play button at all
      (let [plain (assoc (speaker-engine :fixed) :voices (constantly [{:id :one :label "One"}]))]
        (expect (= [{:id "one" :label "One"}] (speech/voices plain)))
        (expect (nil? (speech/voice-sample! plain "one"))))
      ;; a sample lookup that throws is a voice without a sample, not a broken catalogue
      (let [angry (assoc (speaker-engine :angry)
                    :voices (constantly [{:id :one :label "One"}])
                    :voice-sample (fn [_]
                                    (throw (ex-info "the model store is gone" {}))))]
        (expect (= [{:id "one" :label "One"}] (speech/voices angry)))
        (expect (nil? (speech/voice-sample! angry "one"))))))

(defdescribe preload-transcription-only-loads-a-model-that-is-already-installed
             ;; #275 follow-up: the gateway preloads the transcription model at startup. The
             ;; manifest answers first so a machine that never installed one stays untouched.
             (it "an absent model neither warms nor even resolves the backend engine"
                 (let [resolved (atom 0)]
                   (with-redefs [assets/installed? (fn [_entry]
                                                     false)
                                 speech/default-engine (fn [_direction]
                                                         (swap! resolved inc)
                                                         nil)]

                     (expect (false? (speech/preload-transcription!)))
                     (expect (zero? @resolved)))))
             (it "an installed model warms the default engine"
                 (let [warmed (atom 0)]
                   (with-redefs [assets/installed? (fn [_entry]
                                                     true)
                                 speech/default-engine (fn [_direction]
                                                         {:id :test-engine
                                                          :warm (fn []
                                                                  (swap! warmed inc)
                                                                  true)})]

                     (expect (true? (speech/preload-transcription!)))
                     (expect (= 1 @warmed)))))
             (it "an engine with nothing to warm is not an error"
                 (with-redefs [assets/installed?
                               (fn [_entry]
                                 true)

                               speech/default-engine
                               (fn [_direction]
                                 {:id :test-engine})]

                   (expect (false? (speech/preload-transcription!))))))
