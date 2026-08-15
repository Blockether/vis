(ns com.blockether.vis.internal.voice-test
  "The speech seam, in BOTH directions: WHICH engine runs, and WHERE the work is right
   now.

   Both halves are the product promise. A different engine — a remote whisper.cpp
   server, another speaker — must be zero lines away, so nothing here may know what a
   model file is; and every phase and percentage a human reads comes out of the job
   store, so nothing may be invented by a surface. The two directions share that
   machinery and NOTHING else: listening can never answer with the engine chosen for
   speaking."
  (:require [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]
            [com.blockether.vis.internal.toggles :as toggles]
            [com.blockether.vis.internal.voice :as voice]))

(defn- with-only-engines!
  "Run `f` with EXACTLY these engines registered — `{:transcribe [...] :synthesize [...]}`
   — then restore both registries and both pins."
  [by-direction f]
  (let
    [before
     (into {} (map (juxt identity voice/engines)) voice/directions)

     clear!
     (fn []
       (doseq
         [d
          voice/directions

          e
          (voice/engines d)]

         (voice/unregister-engine! d (:id e)))
       (doseq [d voice/directions]
         (voice/set-default-engine! d nil)))]

    (clear!)
    (try (doseq
           [d
            voice/directions

            e
            (get by-direction d)]

           (voice/register-engine! d e))
         (f)
         (finally (clear!)
                  (doseq
                    [d
                     voice/directions

                     e
                     (get before d)]

                    (voice/register-engine! d e))))))

(defn- refusal
  "The one-line reason `f` refused, or nil when it did not refuse at all."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-message e))))

(defn- echo-engine
  "A transcription engine that reports the progress it is told to and returns `text`."
  [id text & [steps]]
  {:id id
   :label (name id)
   :transcribe (fn [{:keys [on-progress]}]
                 (doseq [s (or steps [])]
                   (on-progress s))
                 text)})

(defn- speaker-engine
  "A synthesis engine that writes what it was asked to say into a temp file — the file
   IS the answer, exactly as a real one hands back the WAV it rendered."
  [id & [steps]]
  {:id id
   :label (name id)
   :voices (fn []
             [{:id :alba :label "Alba" :language :en} {:id :javert}])
   :synthesize (fn [{:keys [text voice-id on-progress]}]
                 (doseq [s (or steps [])]
                   (on-progress s))
                 (let [f (java.io.File/createTempFile "vis-voice-test" ".wav")]
                   (.deleteOnExit f)
                   (spit f
                         (str (some-> voice-id
                                      name)
                              "|"
                              text))
                   {:audio-path (str f) :sample-rate 24000}))})

;; =============================================================================
;; The engine is a REGISTRATION, not a hardcoded namespace
;; =============================================================================

(deftest an-engine-is-registered-not-hardcoded
  (with-only-engines!
    {}
    (fn []
      (testing "a gateway with no engine says so instead of throwing something opaque"
        (is (empty? (voice/engines :transcribe)))
        (is (nil? (voice/default-engine :transcribe)))
        (is (= {:engines [] :selected nil} (voice/engines-info :transcribe)))
        (is (= "No voice transcription engine is registered"
               (refusal #(voice/resolve-engine :transcribe nil))))
        (is (= "No speech synthesis engine is registered"
               (refusal #(voice/resolve-engine :synthesize nil)))))
      (testing "anything with an :id and a :transcribe fn is a listening engine"
        (voice/register-engine! :transcribe (echo-engine :whisper-server "from the server"))
        (is (= [:whisper-server] (mapv :id (voice/engines :transcribe))))
        (is (= "from the server" (voice/transcribe! {:audio-path "/tmp/does-not-matter.wav"})))
        (is (= {:engines [{:id "whisper-server" :label "whisper-server"}]
                :selected "whisper-server"}
               (voice/engines-info :transcribe))))
      (testing "a malformed engine is refused AT registration, not at the microphone"
        (is (nil? (voice/engine-error :transcribe (echo-engine :ok "hi"))))
        (is (= "engine :id must be a keyword"
               (voice/engine-error :transcribe {:id "str" :transcribe str})))
        (is (= "engine :transcribe must be a function" (voice/engine-error :transcribe {:id :x})))
        (is (= "Invalid voice transcription engine: engine :transcribe must be a function"
               (refusal #(voice/register-engine! :transcribe {:id :x})))))
      (testing "and a direction nobody defined is refused with the two that exist"
        (is (= "Unknown speech direction: :speak" (refusal #(voice/engines :speak))))
        (is (= [:transcribe :synthesize]
               (:available (try (voice/engines :speak)
                                (catch clojure.lang.ExceptionInfo e (ex-data e))))))))))

(deftest the-selected-engine-can-be-swapped-without-touching-a-caller
  (with-only-engines! {:transcribe [(echo-engine :parakeet-local "local words")
                                    (echo-engine :whisper-server "remote words")]}
                      (fn []
                        (testing "the first registered engine wins by default"
                          (is (= "local words" (voice/transcribe! {:audio-path "/tmp/a.wav"}))))
                        (testing "pinning one changes every caller that names none"
                          (voice/set-default-engine! :transcribe :whisper-server)
                          (is (= :whisper-server (:id (voice/default-engine :transcribe))))
                          (is (= "remote words" (voice/transcribe! {:audio-path "/tmp/a.wav"}))))
                        (testing "a caller may still name an engine per request"
                          (is (= "local words"
                                 (voice/transcribe! {:audio-path "/tmp/a.wav"
                                                     :engine-id :parakeet-local}))))
                        (testing "a typo names the engines that DO exist"
                          (let
                            [e (try (voice/resolve-engine :transcribe :wisper)
                                    (catch clojure.lang.ExceptionInfo e e))]
                            (is (= :vis/voice-engine-unavailable (:type (ex-data e))))
                            (is (= [:parakeet-local :whisper-server] (:available (ex-data e))))))
                        (testing "unregistering the pinned engine falls back instead of dangling"
                          (voice/unregister-engine! :transcribe :whisper-server)
                          (is (= :parakeet-local (:id (voice/default-engine :transcribe))))))))

;; =============================================================================
;; Two directions, one registry - and neither may answer for the other
;; =============================================================================

(deftest speaking-and-listening-are-separate-entries-in-one-registry
  (with-only-engines!
    {:transcribe [(echo-engine :parakeet-local "heard it")]
     :synthesize [(speaker-engine :pocket-tts)]}
    (fn []
      (testing "each direction lists only its own engines"
        (is (= [:parakeet-local] (mapv :id (voice/engines :transcribe))))
        (is (= [:pocket-tts] (mapv :id (voice/engines :synthesize))))
        ;; a SPEAKING engine carries its catalogue in the same answer, so a picker is
        ;; populated by the one capabilities request instead of a call per engine
        (is (= {:engines [{:id "pocket-tts"
                           :label "pocket-tts"
                           :voices [{:id "alba" :label "Alba" :language "en"}
                                    {:id "javert" :label "javert"}]}]
                :selected "pocket-tts"}
               (voice/engines-info :synthesize)))
        ;; and a LISTENING engine has none: voices are not a fact about transcription
        (is (= {:engines [{:id "parakeet-local" :label "parakeet-local"}]
                :selected "parakeet-local"}
               (voice/engines-info :transcribe))))
      (testing "an engine that only listens can never be registered as one that speaks"
        (is (= "engine :synthesize must be a function"
               (voice/engine-error :synthesize (echo-engine :parakeet-local "heard it"))))
        (is (= "Invalid speech synthesis engine: engine :synthesize must be a function"
               (refusal #(voice/register-engine! :synthesize (echo-engine :x "hi"))))))
      (testing "the same id in both directions is two independent entries"
        (voice/register-engine! :synthesize (speaker-engine :parakeet-local))
        (is (= [:pocket-tts :parakeet-local] (mapv :id (voice/engines :synthesize))))
        (voice/unregister-engine! :synthesize :parakeet-local)
        (is (= [:parakeet-local] (mapv :id (voice/engines :transcribe))))
        (is (= [:pocket-tts] (mapv :id (voice/engines :synthesize)))))
      (testing "each direction walks its own phases and never promises the other's"
        (is (= [:uploading :queued :preparing :transcribing :done :failed]
               (voice/direction-phases :transcribe)))
        (is (= [:uploading :queued :preparing :synthesizing :done :failed]
               (voice/direction-phases :synthesize)))))))

(deftest each-direction-resolves-only-its-own-default
  (with-only-engines!
    {:transcribe [(echo-engine :parakeet-local "heard it") (echo-engine :whisper-server "remote")]
     :synthesize [(speaker-engine :pocket-tts) (speaker-engine :piper)]}
    (fn []
      (testing "a pin on one side leaves the other side where it was"
        (voice/set-default-engine! :synthesize :piper)
        (is (= :piper (:id (voice/default-engine :synthesize))))
        (is (= :parakeet-local (:id (voice/default-engine :transcribe))))
        (voice/set-default-engine! :transcribe :whisper-server)
        (is (= :whisper-server (:id (voice/default-engine :transcribe))))
        (is (= :piper (:id (voice/default-engine :synthesize)))))
      (testing "and clearing one pin never clears the other"
        (voice/set-default-engine! :synthesize nil)
        (is (= :pocket-tts (:id (voice/default-engine :synthesize))))
        (is (= :whisper-server (:id (voice/default-engine :transcribe)))))
      (testing "an operator names each direction with its OWN variable"
        (is (= {:transcribe "VIS_VOICE_ENGINE" :synthesize "VIS_SPEECH_ENGINE"}
               voice/engine-env-vars))
        (with-redefs-fn {#'voice/env-engine-id
                         (fn [direction]
                           (get {:transcribe :parakeet-local :synthesize :piper} direction))}
          (fn []
            ;; the environment outranks the pin above, in each direction independently
            (is (= :parakeet-local (:id (voice/default-engine :transcribe))))
            (is (= :piper (:id (voice/default-engine :synthesize)))))))
      (testing "an environment variable naming an engine of the OTHER direction resolves nothing"
        (with-redefs-fn {#'voice/env-engine-id (constantly :pocket-tts)}
          (fn []
            ;; falls through to the pin, never across the two registries
            (is (= :whisper-server (:id (voice/default-engine :transcribe))))
            (is (= :pocket-tts (:id (voice/default-engine :synthesize))))))))))

(deftest an-operator-outranks-a-pin-outranks-registration-order
  ;; The precedence itself, as a pure question, so the ORDER is provable without
  ;; touching the environment of a running process.
  (let [es [{:id :first} {:id :second} {:id :third}]]
    (is (= :first (:id (voice/choose-engine {:engines es}))))
    (is (= :second (:id (voice/choose-engine {:pinned-id :second :engines es}))))
    (is (= :third (:id (voice/choose-engine {:env-id :third :pinned-id :second :engines es}))))
    (testing "a name nobody registered is skipped, not fatal"
      (is (= :second (:id (voice/choose-engine {:env-id :nope :pinned-id :second :engines es}))))
      (is (nil? (voice/choose-engine {:env-id :nope :engines []}))))))

(deftest readiness-is-the-engines-own-question
  (testing "an engine that needs no preparation is simply ready"
    (is (= {:state :ready} (voice/readiness (echo-engine :remote "hi"))))
    (is (true? (voice/ready? (echo-engine :remote "hi")))))
  (testing "an engine that downloads a model reports its own progress"
    (let
      [engine (assoc (echo-engine :local "hi")
                :model-state (constantly {:state :downloading :progress 42}))]
      (is (= {:state :downloading :progress 42} (voice/readiness engine)))
      (is (false? (voice/ready? engine)))))
  (testing "a readiness call that throws is a FAILED engine, never a broken gateway"
    (let
      [engine (assoc (echo-engine :local "hi")
                :model-state (fn []
                               (throw (ex-info "disk is gone" {}))))]
      (is (= :failed (:state (voice/readiness engine))))
      (is (= "disk is gone" (:error (voice/readiness engine))))))
  (testing "prepare! is the engine's own hook and answers with readiness"
    (let
      [started
       (atom 0)

       engine
       (assoc (echo-engine :local "hi")
         :start-download (fn []
                           (swap! started inc)
                           {:state :downloading :progress 0}))]

      (is (= {:state :downloading :progress 0} (voice/prepare! engine)))
      (is (= 1 @started))
      ;; an engine with no hook is prepared by definition
      (is (= {:state :ready} (voice/prepare! (echo-engine :remote "hi")))))))

(deftest a-voice-is-the-speaking-engines-own-catalogue
  (testing "the voices come out in the shape every surface reports"
    (is (= [{:id "alba" :label "Alba" :language "en"} {:id "javert" :label "javert"}]
           (voice/voices (speaker-engine :pocket-tts)))))
  (testing "an engine with ONE fixed voice offers no choice at all"
    (is (= [] (voice/voices (dissoc (speaker-engine :fixed) :voices)))))
  (testing "a voice a user has to install deliberately says so, and no other voice does"
    ;; A picker that cannot tell offers that voice like any other and finds out on the
    ;; click, which is the one refusal a client could have shown up front.
    (is (= [{:id "alba" :label "Alba" :language "en"} {:id "ryan" :label "Ryan" :is-opt-in true}]
           (voice/voices (assoc (speaker-engine :piper)
                           :voices (fn []
                                     [{:id :alba :label "Alba" :language :en}
                                      {:id :ryan :label "Ryan" :is-opt-in true}]))))))
  (testing "and a catalogue that refuses says why instead of looking empty"
    (is (= "the voice list is gone"
           (refusal #(voice/voices (assoc (speaker-engine :broken)
                                     :voices (fn []
                                               (throw (ex-info "the voice list is gone" {}))))))))))

;; =============================================================================
;; The job: where the work IS
;; =============================================================================

(deftest a-job-walks-the-shared-phase-vocabulary
  (testing "the phases are ordered and the surfaces share them"
    (is (= [:uploading :queued :preparing :transcribing :synthesizing :done :failed] voice/phases))
    (is (voice/phase? :transcribing))
    (is (voice/phase? :synthesizing))
    (is (not (voice/phase? :almost-there))))
  (with-only-engines!
    {:transcribe [(echo-engine :fake
                               "hello world"
                               [{:phase :preparing :progress 50} {:phase :transcribing :progress 10}
                                {:phase :transcribing :progress 90}])]}
    (fn []
      (voice/reset-jobs!)
      (let
        [seen
         (atom [])

         engine
         (assoc (voice/engine :transcribe :fake)
           :transcribe (fn [{:keys [on-progress job-id]}]
                         (on-progress {:phase :preparing :progress 50})
                         (swap! seen conj (select-keys (voice/job job-id) [:phase :progress]))
                         (on-progress {:phase :transcribing :progress 10})
                         (swap! seen conj (select-keys (voice/job job-id) [:phase :progress]))
                         "hello world"))

         _
         (voice/register-engine! :transcribe engine)

         done
         (voice/submit-sync! :transcribe {:audio-path "/tmp/a.wav"})]

        (testing "the engine's own progress is READABLE while it runs"
          (is (= [{:phase "preparing" :progress 50} {:phase "transcribing" :progress 10}] @seen)))
        (testing "the finished job carries the transcript and nothing else moves"
          (is (= "done" (:phase done)))
          (is (= "transcribe" (:direction done)))
          (is (= 100 (:progress done)))
          (is (true? (:is-done done)))
          (is (= "hello world" (:text done)))
          (is (= "fake" (:engine done)))
          (is (nil? (:error done))))
        (testing "the job is readable again by id, and the audio path never leaves"
          (is (= done (voice/job (:id done))))
          (is (nil? (:audio-path (voice/job (:id done))))))
        (testing "a collected job can be forgotten"
          (voice/forget! (:id done))
          (is (nil? (voice/job (:id done)))))))))

(deftest a-spoken-reply-is-a-job-like-any-other
  (with-only-engines!
    {:synthesize [(speaker-engine :pocket-tts [{:phase :synthesizing :progress 40}])]}
    (fn []
      (voice/reset-jobs!)
      (let [done (voice/submit-sync! :synthesize {:text "the build is green" :voice-id :alba})]
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
          (let [f (java.io.File. (str (voice/job-audio-path (:id done))))]
            (is (.isFile f))
            (is (= "alba|the build is green" (slurp f)))
            (.delete f)))
        (testing "a spoken job is forgotten like any other"
          (voice/forget! (:id done))
          (is (nil? (voice/job (:id done))))
          (is (nil? (voice/job-audio-path (:id done))))))))
  (with-only-engines! {:synthesize [{:id :mute
                                     :synthesize (fn [{:keys [on-progress]}]
                                                   (on-progress {:phase :synthesizing :progress 30})
                                                   (throw (ex-info "the vocoder gave up" {})))}]}
                      (fn []
                        (voice/reset-jobs!)
                        (let [failed (voice/submit-sync! :synthesize {:text "anything"})]
                          (testing "a speaking engine that dies fails the job with a readable line"
                            (is (= "failed" (:phase failed)))
                            (is (= "synthesize" (:direction failed)))
                            (is (= 30 (:progress failed)))
                            (is (= "the vocoder gave up" (:error failed)))
                            (is (nil? (:audio failed)))))))
  (with-only-engines! {:synthesize [{:id :empty-handed :synthesize (constantly nil)}]}
                      (fn []
                        (voice/reset-jobs!)
                        (testing
                          "and an engine that answers with no file is a failure, never silent :done"
                          (is (= "Synthesis engine returned no audio file"
                                 (:error (voice/submit-sync! :synthesize {:text "anything"}))))))))

(deftest spoken-replies-obey-their-feature-toggle
  (with-only-engines!
    {:synthesize [(speaker-engine :pocket-tts)]}
    (fn []
      (voice/reset-jobs!)
      (try (toggles/set-enabled! voice/speech-toggle-id false)
           (testing "with `speech` off nothing speaks, and no job is created to explain it"
             (is (false? (voice/speech-enabled?)))
             (is (= "Spoken replies are turned off"
                    (refusal #(voice/submit! :synthesize {:text "quiet please"}))))
             (is (= "Spoken replies are turned off"
                    (refusal #(voice/synthesize! {:text "quiet please"})))))
           (testing "listening is untouched by the SPEAKING toggle"
             (is (= [] (voice/engines :transcribe))))
           (finally (toggles/reset-to-default! voice/speech-toggle-id)))
      (testing "and with the toggle back at its default the same call speaks"
        (is (true? (voice/speech-enabled?)))
        (let [spoken (voice/synthesize! {:text "out loud" :voice-id :javert})]
          (is (= "audio/wav" (:media-type spoken)))
          (let [f (java.io.File. (str (:audio-path spoken)))]
            (is (= "javert|out loud" (slurp f)))
            (.delete f)))))))

(deftest progress-only-ever-moves-forward
  (with-only-engines!
    {:transcribe [{:id :jumpy
                   :transcribe (fn [{:keys [on-progress job-id]}]
                                 (on-progress {:phase :transcribing :progress 80})
                                 (on-progress {:phase :transcribing :progress 5})
                                 (on-progress {:progress 4000})
                                 (str (:progress (voice/job job-id))))}]}
    (fn []
      (voice/reset-jobs!)
      ;; a chunked engine that restarts its counter must never make the bar go
      ;; backwards in front of a human, and a bad percentage is clamped, not shown
      (is (= "100" (:text (voice/submit-sync! :transcribe {:audio-path "/tmp/a.wav"})))))))

(deftest a-failing-engine-fails-the-job-with-a-readable-line
  (with-only-engines!
    {:transcribe [{:id :broken
                   :transcribe (fn [_]
                                 (throw (ex-info "model file is corrupt" {})))}]}
    (fn []
      (voice/reset-jobs!)
      (let
        [collected
         (atom nil)

         job
         (voice/submit-sync! :transcribe {:audio-path "/tmp/a.wav" :on-done #(reset! collected %)})]

        (is (= "failed" (:phase job)))
        (is (true? (:is-done job)))
        (is (= "model file is corrupt" (:error job)))
        (is (nil? (:text job)))
        (testing "on-done still runs, so a temp recording is always deleted"
          (is (= (:id job) (:id @collected)))))))
  (testing "an unknown engine is refused BEFORE a job exists"
    (with-only-engines! {:transcribe [(echo-engine :fake "hi")]}
                        (fn []
                          (voice/reset-jobs!)
                          (is (= "Unknown voice transcription engine: nope"
                                 (refusal #(voice/submit! :transcribe
                                                          {:audio-path "/tmp/a.wav"
                                                           :engine-id :nope}))))))))

(deftest submit-answers-immediately-and-the-job-finishes-on-its-own-thread
  (let [release (promise)]
    (with-only-engines! {:transcribe [{:id :slow
                                       :transcribe (fn [_]
                                                     @release
                                                     "eventually")}]}
                        (fn []
                          (voice/reset-jobs!)
                          (let [queued (voice/submit! :transcribe {:audio-path "/tmp/a.wav"})]
                            (testing "the caller can answer 202 without waiting for a single word"
                              (is (string? (:id queued)))
                              (is (false? (:is-done queued)))
                              (is (nil? (:text queued)))
                              (is (contains? #{"queued" "preparing"} (:phase queued))))
                            (deliver release :go)
                            (let [deadline (+ (System/currentTimeMillis) 5000)]
                              (while (and (not (:is-done (voice/job (:id queued))))
                                          (< (System/currentTimeMillis) deadline))
                                (Thread/sleep 5)))
                            (is (= "eventually" (:text (voice/job (:id queued))))))))))

(deftest a-finished-job-is-swept-when-its-ttl-runs-out
  (let
    [ttl
     (long @#'voice/job-ttl-ms)

     t
     (System/currentTimeMillis)

     stale
     (- t ttl 60000)

     kept
     (#'voice/sweep
      {"spoken" {:id "spoken"
                 :direction :synthesize
                 :phase :done
                 :progress 100
                 :created-at stale
                 :updated-at stale}
       "heard"
       {:id "heard" :direction :transcribe :phase :done :progress 100 :created-at t :updated-at t}
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
  (is (= "boom" (voice/error-message (ex-info "boom" {}))))
  (is (= "root cause"
         (voice/error-message (java.io.IOException. (RuntimeException. "root cause")))))
  (is (string? (voice/error-message (NullPointerException.)))))

;; =============================================================================
;; A job PUSHES - watchers, not polls
;; =============================================================================

(defn- wait-done!
  "Block until `job-id` is terminal (or 5s pass) and return its public job."
  [job-id]
  (let [deadline (+ (System/currentTimeMillis) 5000)]
    (while (and (not (:is-done (voice/job job-id))) (< (System/currentTimeMillis) deadline))
      (Thread/sleep 5))
    (voice/job job-id)))

(deftest a-watcher-is-told-every-step-as-it-happens
  ;; The percentage is only worth showing while the work is happening, so a
  ;; surface must never have to ASK for it: the gateway's SSE body is one of
  ;; these watchers, and it writes a frame the instant the engine reports.
  (let
    [armed
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
        (voice/reset-jobs!)
        (let
          [job
           (voice/submit! :transcribe {:audio-path "/tmp/a.wav"})

           unwatch
           (voice/watch! (:id job)
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
  (let
    [armed
     (promise)

     seen
     (atom [])]

    (with-only-engines!
      {:synthesize [{:id :streamer
                     :synthesize (fn [{:keys [on-progress text]}]
                                   @armed
                                   (on-progress {:phase :synthesizing :progress 25})
                                   (on-progress {:phase :synthesizing :progress 75})
                                   (let [f (java.io.File/createTempFile "vis-voice-test" ".wav")]
                                     (.deleteOnExit f)
                                     (spit f text)
                                     (str f)))}]}
      (fn []
        (voice/reset-jobs!)
        (let
          [job
           (voice/submit! :synthesize {:text "spoken aloud"})

           unwatch
           (voice/watch! (:id job)
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
              (let [f (java.io.File. (str (voice/job-audio-path (:id final))))]
                (is (= "spoken aloud" (slurp f)))
                (.delete f)))))))))

(deftest a-watcher-that-let-go-is-never-called-again
  (let
    [gate
     (promise)

     heard
     (atom [])]

    (with-only-engines! {:transcribe [{:id :gated
                                       :transcribe (fn [_]
                                                     @gate
                                                     "late")}]}
                        (fn []
                          (voice/reset-jobs!)
                          (let
                            [job
                             (voice/submit! :transcribe {:audio-path "/tmp/a.wav"})

                             unwatch
                             (voice/watch! (:id job)
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
                        (voice/reset-jobs!)
                        (let [job (voice/submit-sync! :transcribe {:audio-path "/tmp/a.wav"})]
                          (is (= "failed" (:phase job)))
                          (is (= 60 (:progress job)))
                          (is (= "the decoder died" (:error job)))))))

;; =============================================================================
;; A voice can be BROUGHT: the engine that clones learns one from a recording
;; =============================================================================

(defn- cloning-engine
  "A speaking engine that learns a voice from a recording, as a local cloning model
   does: the clip IS the voice, so an import lands in the same catalogue a caller
   picks from."
  [store]
  (assoc (speaker-engine :cloner)
    :voices (fn []
              (vec (vals @store)))
    :import-voice (fn [{:keys [path voice-name language text]}]
                    (let
                      [voice {:id (.replace (.toLowerCase (str voice-name)) " " "-")
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
        (let [engine (first (voice/engines :synthesize))]
          (testing "the capability is advertised, so nothing offers what cannot work"
            (is (= {:id "cloner" :label "cloner" :is-voice-import true}
                   (voice/public-engine engine)))
            (is (true? (:is-voice-import (first (:engines (voice/engines-info :synthesize)))))))
          (testing "the recording becomes a voice in the catalogue a caller picks from"
            (is (= {:id "my-own" :label "My Own" :language "en-GB" :is-imported true}
                   (voice/import-voice! engine
                                        {:path "/tmp/clip.wav"
                                         :voice-name "My Own"
                                         :language "en-GB"
                                         :text "what the clip says"})))
            (is (= [{:id "my-own" :label "My Own" :language "en-GB" :is-imported true}]
                   (voice/voices engine)))
            ;; which file backs a voice stays the engine's business
            (is (not-any? :clip (voice/voices engine))))
          (testing "forgetting twice is the same outcome, never an error"
            (is (true? (voice/forget-voice! engine "my-own")))
            (is (empty? (voice/voices engine)))
            (is (false? (voice/forget-voice! engine "my-own"))))))))
  (testing "an engine that cannot learn a voice refuses by name"
    (let [plain (speaker-engine :pocket-tts)]
      (is (nil? (:is-voice-import (voice/public-engine plain))))
      (is (= "pocket-tts cannot learn a voice from a recording"
             (refusal #(voice/import-voice! plain {:path "/tmp/clip.wav" :voice-name "Mine"}))))
      (is (= "pocket-tts does not keep voices of its own"
             (refusal #(voice/forget-voice! plain "mine")))))))
