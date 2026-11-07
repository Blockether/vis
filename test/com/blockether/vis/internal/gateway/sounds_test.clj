(ns com.blockether.vis.internal.gateway.sounds-test
  "Sounds: which session events play a cue, and which file a cue plays."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.test-temp :as test-temp]
            [lazytest.core :refer [defdescribe expect it]]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.gateway.push :as push]
            [com.blockether.vis.internal.gateway.sounds :as sounds]
            [com.blockether.vis.internal.sounds :as internal-sounds]))

(defn- played
  "How often each cue plays for `events` with `settings`. Each cue plays in its own
   future, so the order is not fixed."
  [settings events]
  (let [cues (atom [])]
    (with-redefs [sounds/play! (fn [cue]
                                 (swap! cues conj cue)
                                 true)]
      (binding [toggles/*overrides* settings]
        (doseq [event events]
          (sounds/on-event! "s1" event))
        (loop [attempts 100]
          (when (and (pos? attempts) (< (count @cues) (count (keep #(sounds/cue "s1" %) events))))
            (Thread/sleep 10)
            (recur (dec attempts))))))
    (frequencies @cues)))

(defdescribe cues-test
             (it "plays a cue only for turns that the person waits on, and only with sounds on"
                 (let [events [{"type" "turn.completed" "turn_id" "child" "subagent" true}
                               {"type" "turn.failed" "turn_id" "child-failed" "subagent" true}
                               {"type" "turn.completed" "turn_id" "wake" "request_kind" "council"}
                               {"type" "content.block.delta" "turn_id" "t1"}
                               {"type" "view.open" "kind" "display"}
                               {"type" "turn.completed" "turn_id" "t1" "status" "completed"}
                               {"type" "view.open" "kind" "input"}
                               {"type" "turn.failed" "turn_id" "t2" "status" "failed"}
                               {"type" "turn.cancelled" "turn_id" "t3" "status" "cancelled"}]]
                   (expect (= {:finish 1 :input 1 :error 2} (played {"sounds" true} events)))
                   (expect (= {} (played {"sounds" false} events)))
                   (push/set-quiet-filter! (fn [_ event]
                                             (= "automation" (get event "turn_id"))))
                   (try (expect (= {}
                                   (played {"sounds" true}
                                           [{"type" "turn.completed" "turn_id" "automation"}])))
                        (finally (push/set-quiet-filter! nil))))))

(defdescribe
  play-test
  (it "plays the file of the active preset in the sound_presets setting"
      (let [own
            (doto (java.io.File/createTempFile "own-sound-" ".wav") (test-temp/delete-at-exit!))

            files
            (atom [])]

        (with-redefs [internal-sounds/play-file! (fn [file]
                                                   (swap! files conj file)
                                                   true)]
          (binding [toggles/*overrides*
                    {"sound_presets" {"active" "Mine"
                                      "custom" [{"name" "Mine"
                                                 "slots" {"finish" (.getPath own) "input" ""}}]}}]
            (sounds/play! :finish)
            (sounds/play! :input))
          (binding [toggles/*overrides* {"sound_presets" {"active" "glass" "custom" []}}]
            (sounds/play! :error)))
        (expect (= own (first @files)))
        (expect (nil? (second @files)))
        (expect
          (= (seq (.readAllBytes (io/input-stream (io/resource "vis-sounds/glass/error.wav"))))
             (seq (java.nio.file.Files/readAllBytes (.toPath ^java.io.File (nth @files 2)))))))))
