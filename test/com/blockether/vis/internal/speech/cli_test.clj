(ns com.blockether.vis.internal.speech.cli-test
  (:require [com.blockether.vis.internal.speech.cli :as speech]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  speech-command-test
  (it "is a built-in top-level command, not an extension contribution"
      (let [cli
            speech/command

            by-name
            (into {} (map (juxt :cmd/name identity)) (:cmd/subcommands cli))]

        (expect (= "speech" (:cmd/name cli)))
        (expect (= ["models" "voices" "import" "forget" "say" "transcribe"]
                   (mapv :cmd/name (:cmd/subcommands cli))))
        (expect (= ["status" "download" "licenses"]
                   (mapv :cmd/name (:cmd/subcommands (get by-name "models")))))
        (expect (= ["file" "name" "lang" "text"] (mapv :name (:cmd/args (get by-name "import")))))
        (expect (= ["text" "voice" "pocket-tts" "out"]
                   (mapv :name (:cmd/args (get by-name "say")))))
        (expect (every? #(or (:cmd/run-fn %) (seq (:cmd/subcommands %))) (:cmd/subcommands cli)))))
  (it "downloads every family it may fetch unless specific families were named"
      (expect (= [:parakeet :piper :pocket-tts] (#'speech/download-families {})))
      (expect (= [:parakeet :piper :pocket-tts] (#'speech/download-families {"all" true})))
      (expect (= [:pocket-tts] (#'speech/download-families {"pocket-tts" true}))))
  (it "runs diagnostics from one gateway-owned model snapshot"
      (let [calls (atom 0)]
        (with-redefs [speech/model-status (fn []
                                            (swap! calls inc)
                                            {:parakeet {:state :ready :installed? true}
                                             :speech {:piper {:state :ready}
                                                      :pocket-tts {:state :ready}}})]
          (let [msgs (speech/doctor-fn {})]
            (expect (= 1 @calls))
            (expect (= [::speech/runtime ::speech/parakeet ::speech/speech ::speech/pocket-speech]
                       (mapv :check-id msgs)))
            (expect (every? #(= :info (:level %)) msgs))))))
  (it "warns for every absent gateway speech model"
      (with-redefs [speech/model-status (constantly {:parakeet {:state :ready :installed? true}
                                                     :speech {:piper {:state :absent}
                                                              :pocket-tts {:state :absent}}})]
        (let [by-id (into {} (map (juxt :check-id identity)) (speech/doctor-fn {}))]
          (expect (= :warn (:level (::speech/speech by-id))))
          (expect (re-find #"--piper" (:remediation (::speech/speech by-id))))
          (expect (= :warn (:level (::speech/pocket-speech by-id))))
          (expect (re-find #"--pocket-tts" (:remediation (::speech/pocket-speech by-id))))))))
