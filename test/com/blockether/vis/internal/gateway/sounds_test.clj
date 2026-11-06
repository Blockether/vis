(ns com.blockether.vis.internal.gateway.sounds-test
  "Sounds: which session events play a cue, and which file a cue plays."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.test-temp :as test-temp]
            [lazytest.core :refer [defdescribe expect it]]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.gateway.push :as push]
            [com.blockether.vis.internal.gateway.sounds :as sounds]))

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
  sound-file-test
  (it "plays the bundled sound for an empty setting, none for off and the named file for a path"
      (let [bundled (binding [toggles/*overrides* {"sound_finish" ""}]
                      (sounds/sound-file :finish))]
        (expect (.isFile bundled))
        (expect (= (seq (.readAllBytes (io/input-stream (io/resource "vis-sounds/finish.wav"))))
                   (seq (java.nio.file.Files/readAllBytes (.toPath bundled))))))
      (binding [toggles/*overrides* {"sound_input" "off"}]
        (expect (nil? (sounds/sound-file :input))))
      (let [own (doto (java.io.File/createTempFile "own-sound-" ".wav")
                  (test-temp/delete-at-exit!))]
        (binding [toggles/*overrides* {"sound_error" (str "  " (.getPath own) " ")}]
          (expect (= own (sounds/sound-file :error)))))
      (binding [toggles/*overrides* {"sound_error" "/no/such/sound.wav"}]
        (expect (= :vis.sounds/missing-file
                   (try (sounds/sound-file :error)
                        nil
                        (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))))
