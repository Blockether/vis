(ns com.blockether.vis.ext.foundation-voice.core-test
  (:require [com.blockether.vis.ext.foundation-voice.core :as voice]
            [lazytest.core :refer [defdescribe it expect]]))

(defdescribe
  voice-config-test
  (it "mounts voice model and voice-import commands under vis-agent extension voice"
      (let
        [cli
         (-> voice/voice-extension
             :ext/cli
             first)

         by-name
         (into {} (map (juxt :cmd/name identity)) (:cmd/subcommands cli))]

        (expect (= "voice" (:cmd/name cli)))
        (expect (= ["models" "voices" "import" "forget" "say" "transcribe"]
                   (mapv :cmd/name (:cmd/subcommands cli))))
        (expect (= ["status" "download" "licenses"]
                   (mapv :cmd/name (:cmd/subcommands (get by-name "models")))))
        ;; a recording is the whole of "add a voice", so the clip is POSITIONAL and
        ;; everything that merely describes it is optional
        (expect (= ["file" "name" "lang" "text"] (mapv :name (:cmd/args (get by-name "import")))))
        (expect (= [:positional :flag :flag :flag] (mapv :kind (:cmd/args (get by-name "import")))))
        (expect (true? (:required (first (:cmd/args (get by-name "import"))))))
        (expect (= ["name"] (mapv :name (:cmd/args (get by-name "forget")))))
        ;; `say` and `transcribe` are the two one-line checks a human runs when voice
        ;; "does nothing": speak a line, then read that recording back.
        (expect (= ["text" "voice" "pocket-tts" "out"]
                   (mapv :name (:cmd/args (get by-name "say")))))
        (expect (= [:positional :flag :flag :flag] (mapv :kind (:cmd/args (get by-name "say")))))
        (expect (= ["file"] (mapv :name (:cmd/args (get by-name "transcribe")))))
        ;; every leaf runs something: a subcommand that only prints help is a dead end
        (expect (every? #(or (:cmd/run-fn %) (seq (:cmd/subcommands %))) (:cmd/subcommands cli)))))
  (it "downloads what Vis fetches by itself unless an opt-in model is NAMED"
      ;; pocket-tts ships as an engine without its weights: neither `--all` nor
      ;; a bare `download` may accept its terms for the user.
      (expect (= [:parakeet :piper] (#'voice/download-families {})))
      (expect (= [:parakeet :piper] (#'voice/download-families {"all" true})))
      (expect (= [:pocket-tts] (#'voice/download-families {"pocket-tts" true})))
      (expect (= [:parakeet :piper] (#'voice/download-families {"parakeet" true "piper" true}))))
  (it "contributes voice-specific doctor diagnostics"
      (with-redefs
        [voice/model-status
         (constantly {:parakeet {:installed? true}
                      :espeak {:is-installed true}
                      :speech {:piper {:state :ready} :pocket-tts {:state :absent}}})

         com.blockether.vis.ext.foundation-voice.core/executable?
         (constantly true)

         clojure.core/requiring-resolve
         (fn [sym]
           (case sym
             com.blockether.vis.ext.foundation-voice.asr/transcribe-file!
             identity))]

        (let [msgs ((:ext/doctor-fn voice/voice-extension) {})]
          (expect (= [::voice/runtime ::voice/ffmpeg ::voice/parakeet ::voice/espeak ::voice/speech]
                     (mapv :check-id msgs)))
          (expect (every? #(= :info (:level %)) msgs)))))
  (it "warns about the speech voice it installs, never about the opt-in one"
      ;; pocket-tts staying absent is the DESIGNED state, and a warning nobody
      ;; is meant to act on teaches the user to ignore doctor.
      (with-redefs
        [voice/model-status
         (constantly {:parakeet {:installed? true}
                      :espeak {:is-installed true}
                      :speech {:piper {:state :absent} :pocket-tts {:state :absent}}})

         com.blockether.vis.ext.foundation-voice.core/executable?
         (constantly true)

         clojure.core/requiring-resolve
         (fn [sym]
           (case sym
             com.blockether.vis.ext.foundation-voice.asr/transcribe-file!
             identity))]

        (let
          [by-id
           (into {} (map (juxt :check-id identity)) ((:ext/doctor-fn voice/voice-extension) {}))]
          (expect (= :warn (:level (::voice/speech by-id))))
          (expect (re-find #"--piper" (:remediation (::voice/speech by-id))))
          (expect (= 5 (count by-id))))))
  (it
    "says WHY a speech family failed, not just that it did"
    ;; A voice downloaded onto a machine without espeak-ng printed a bare
    ;; `Speech (piper): failed`, hiding the one line that names the fix.
    (let [lines (atom [])]
      (with-redefs
        [voice/model-status
         (constantly
           {:parakeet {:installed? true}
            :speech
            {:piper
             {:state :failed
              :error
              "espeak-ng's phoneme tables are not on this system, and Piper cannot speak without them. Install espeak-ng: `brew install espeak-ng`."}
             :pocket-tts {:state :absent}}
            :assets []})
         com.blockether.vis.ext.foundation-voice.core/cli-out! (fn [s]
                                                                 (swap! lines conj (str s)))]

        (#'voice/print-status!)
        (expect (some #(= "Speech (piper): failed" %) @lines))
        (expect (some #(re-find #"brew install espeak-ng" %) @lines))
        ;; an absent opt-in model has no reason to give and must not grow a blank line
        (expect (some #(= "Speech (pocket-tts): absent" %) @lines)))))
  (it "defers voice input namespace until the /voice slash run-fn fires (K10)"
      ;; The declarative `/voice` slash spec lazily requiring-resolves
      ;; `toggle-recording!` from the input ns so the host doesn't pay
      ;; the audio stack cost until the user actually toggles voice.
      (let
        [voice-slash
         (first (filter #(= "voice" (:slash/name %)) (:ext/slash-commands voice/voice-extension)))

         calls
         (atom [])]

        (with-redefs
          [clojure.core/requiring-resolve
           (fn [sym]
             (swap! calls conj sym)
             (expect (= 'com.blockether.vis.ext.foundation-voice.input/toggle-recording! sym))
             (fn [ctx]
               (swap! calls conj [:invoked ctx])
               :toggled))]
          (let [result ((:slash/run-fn voice-slash) {:source :test})]
            (expect (= :ok (:slash/status result)))
            (expect (= [:invoked {:source :test}] (last @calls)))
            (expect (= 'com.blockether.vis.ext.foundation-voice.input/toggle-recording!
                       (first @calls))))))))
