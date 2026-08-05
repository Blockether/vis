(ns com.blockether.vis.internal.voice-test
  "The transcription seam: WHICH engine runs, and WHERE the work is right now.

   Both halves are the product promise. A different engine — a remote
   whisper.cpp server, a cloud API — must be zero lines away, so nothing here
   may know what a model file is; and every phase and percentage a human reads
   comes out of the job store, so nothing may be invented by a surface."
  (:require [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]
            [com.blockether.vis.internal.voice :as voice]))

(defn- with-only-engines!
  "Run `f` with EXACTLY `engines` registered, then restore the registry."
  [engines f]
  (let [before (voice/engines)]
    (doseq [e before]
      (voice/unregister-engine! (:id e)))
    (voice/set-default-engine! nil)
    (try (doseq [e engines]
           (voice/register-engine! e))
         (f)
         (finally (doseq [e (voice/engines)]
                    (voice/unregister-engine! (:id e)))
                  (voice/set-default-engine! nil)
                  (doseq [e before]
                    (voice/register-engine! e))))))

(defn- refusal
  "The one-line reason `f` refused, or nil when it did not refuse at all."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-message e))))

(defn- echo-engine
  "An engine that reports the progress it is told to and returns `text`."
  [id text & [steps]]
  {:id id
   :label (name id)
   :transcribe (fn [{:keys [on-progress]}]
                 (doseq [s (or steps [])]
                   (on-progress s))
                 text)})

;; =============================================================================
;; The engine is a REGISTRATION, not a hardcoded namespace
;; =============================================================================

(deftest an-engine-is-registered-not-hardcoded
  (with-only-engines!
    []
    (fn []
      (testing "a gateway with no engine says so instead of throwing something opaque"
        (is (empty? (voice/engines)))
        (is (nil? (voice/default-engine)))
        (is (= {:engines [] :selected nil} (voice/engines-info)))
        (is (= "No voice transcription engine is registered"
               (refusal #(voice/resolve-engine nil)))))
      (testing "anything with an :id and a :transcribe fn is a voice engine"
        (voice/register-engine! (echo-engine :whisper-server "from the server"))
        (is (= [:whisper-server] (mapv :id (voice/engines))))
        (is (= "from the server" (voice/transcribe! {:audio-path "/tmp/does-not-matter.wav"})))
        (is (= {:engines [{:id "whisper-server" :label "whisper-server"}]
                :selected "whisper-server"}
               (voice/engines-info))))
      (testing "a malformed engine is refused AT registration, not at the microphone"
        (is (nil? (voice/engine-error (echo-engine :ok "hi"))))
        (is (= "engine :id must be a keyword" (voice/engine-error {:id "str" :transcribe str})))
        (is (= "engine :transcribe must be a function" (voice/engine-error {:id :x})))
        (is (= "Invalid voice engine: engine :transcribe must be a function"
               (refusal #(voice/register-engine! {:id :x}))))))))

(deftest the-selected-engine-can-be-swapped-without-touching-a-caller
  (with-only-engines!
    [(echo-engine :parakeet-local "local words") (echo-engine :whisper-server "remote words")]
    (fn []
      (testing "the first registered engine wins by default"
        (is (= "local words" (voice/transcribe! {:audio-path "/tmp/a.wav"}))))
      (testing "pinning one changes every caller that names none"
        (voice/set-default-engine! :whisper-server)
        (is (= :whisper-server (:id (voice/default-engine))))
        (is (= "remote words" (voice/transcribe! {:audio-path "/tmp/a.wav"}))))
      (testing "a caller may still name an engine per request"
        (is (= "local words"
               (voice/transcribe! {:audio-path "/tmp/a.wav" :engine-id :parakeet-local}))))
      (testing "a typo names the engines that DO exist"
        (let [e (try (voice/resolve-engine :wisper) (catch clojure.lang.ExceptionInfo e e))]
          (is (= :vis/voice-engine-unavailable (:type (ex-data e))))
          (is (= [:parakeet-local :whisper-server] (:available (ex-data e))))))
      (testing "unregistering the pinned engine falls back instead of dangling"
        (voice/unregister-engine! :whisper-server)
        (is (= :parakeet-local (:id (voice/default-engine))))))))

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

;; =============================================================================
;; The job: where the transcription IS
;; =============================================================================

(deftest a-job-walks-the-shared-phase-vocabulary
  (testing "the phases are ordered and the surfaces share them"
    (is (= [:uploading :queued :preparing :transcribing :done :failed] voice/phases))
    (is (voice/phase? :transcribing))
    (is (not (voice/phase? :almost-there))))
  (with-only-engines!
    [(echo-engine :fake
                  "hello world"
                  [{:phase :preparing :progress 50} {:phase :transcribing :progress 10}
                   {:phase :transcribing :progress 90}])]
    (fn []
      (voice/reset-jobs!)
      (let
        [seen
         (atom [])

         engine
         (assoc (voice/engine :fake)
           :transcribe (fn [{:keys [on-progress job-id]}]
                         (on-progress {:phase :preparing :progress 50})
                         (swap! seen conj (select-keys (voice/job job-id) [:phase :progress]))
                         (on-progress {:phase :transcribing :progress 10})
                         (swap! seen conj (select-keys (voice/job job-id) [:phase :progress]))
                         "hello world"))

         _
         (voice/register-engine! engine)

         done
         (voice/submit-sync! {:audio-path "/tmp/a.wav"})]

        (testing "the engine's own progress is READABLE while it runs"
          (is (= [{:phase "preparing" :progress 50} {:phase "transcribing" :progress 10}] @seen)))
        (testing "the finished job carries the transcript and nothing else moves"
          (is (= "done" (:phase done)))
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

(deftest progress-only-ever-moves-forward
  (with-only-engines! [{:id :jumpy
                        :transcribe (fn [{:keys [on-progress job-id]}]
                                      (on-progress {:phase :transcribing :progress 80})
                                      (on-progress {:phase :transcribing :progress 5})
                                      (on-progress {:progress 4000})
                                      (str (:progress (voice/job job-id))))}]
                      (fn []
                        (voice/reset-jobs!)
                        ;; a chunked engine that restarts its counter must never make the bar go
                        ;; backwards in front of a human, and a bad percentage is clamped, not shown
                        (is (= "100" (:text (voice/submit-sync! {:audio-path "/tmp/a.wav"})))))))

(deftest a-failing-engine-fails-the-job-with-a-readable-line
  (with-only-engines! [{:id :broken
                        :transcribe (fn [_]
                                      (throw (ex-info "model file is corrupt" {})))}]
                      (fn []
                        (voice/reset-jobs!)
                        (let
                          [collected
                           (atom nil)

                           job
                           (voice/submit-sync! {:audio-path "/tmp/a.wav"
                                                :on-done #(reset! collected %)})]

                          (is (= "failed" (:phase job)))
                          (is (true? (:is-done job)))
                          (is (= "model file is corrupt" (:error job)))
                          (is (nil? (:text job)))
                          (testing "on-done still runs, so a temp recording is always deleted"
                            (is (= (:id job) (:id @collected)))))))
  (testing "an unknown engine is refused BEFORE a job exists"
    (with-only-engines! [(echo-engine :fake "hi")]
                        (fn []
                          (voice/reset-jobs!)
                          (is (= "Unknown voice engine: nope"
                                 (refusal #(voice/submit! {:audio-path "/tmp/a.wav"
                                                           :engine-id :nope}))))))))

(deftest submit-answers-immediately-and-the-job-finishes-on-its-own-thread
  (let [release (promise)]
    (with-only-engines! [{:id :slow
                          :transcribe (fn [_]
                                        @release
                                        "eventually")}]
                        (fn []
                          (voice/reset-jobs!)
                          (let [queued (voice/submit! {:audio-path "/tmp/a.wav"})]
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

(deftest error-message-reads-like-a-sentence
  (is (= "boom" (voice/error-message (ex-info "boom" {}))))
  (is (= "root cause"
         (voice/error-message (java.io.IOException. (RuntimeException. "root cause")))))
  (is (string? (voice/error-message (NullPointerException.)))))
