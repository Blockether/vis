(ns com.blockether.vis.internal.gateway.sounds
  "Sounds on the machine that runs the gateway.

   ONE job: when a turn ends or a run waits for the person, play the sound of the
   active preset on this machine. There are three cues:

     :finish  a turn completed,
     :input   a run waits for the answer of the person (an input View),
     :error   a turn failed or stopped.

   Only a turn that the person waits on makes a sound. This is the rule of push
   alerts (`push/operator-turn?`), so subagent turns, Council wakes and
   automation runs stay silent.

   Machine settings, group `sounds`: the `sounds` switch turns the cues on, and
   `sound_presets` chooses the preset (see `internal.sounds`). The gateway
   process plays the sound, so a gateway on another machine plays it there."
  (:require [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.gateway.push :as push]
            [com.blockether.vis.internal.sounds :as sounds]
            [taoensso.telemere :as tel]))

(def ^:private TERMINAL_TYPES #{"turn.completed" "turn.failed" "turn.cancelled"})

(defn cue
  "The cue that `event` of session `sid` raises, or nil."
  [sid event]
  (let [event-type (get event "type")]
    (cond (and (= "view.open" event-type) (= "input" (get event "kind"))) :input
          (and (TERMINAL_TYPES event-type) (push/operator-turn? sid event))
          (if (= "turn.completed" event-type) :finish :error))))

(defn play!
  "Play the sound of `cue` in the active preset now. Returns true when a player
   starts, and false when the cue has no sound, a sound plays already or this
   machine has no player."
  [cue]
  (sounds/play-file! (sounds/cue-file (or (toggles/value-of "sound_presets") sounds/default-value)
                                      cue)))

(defn on-event!
  "Play the cue of one session event when the `sounds` setting is on. Never throws."
  [sid event]
  (try (when (toggles/enabled? "sounds")
         (when-let [c (cue sid event)]
           (future (try (play! c)
                        (catch Throwable t
                          (tel/log! {:level :warn
                                     :id ::play-failed
                                     :data {:cue c :session-id sid :error (ex-message t)}}
                                    (str "Sound failed: " (ex-message t))))))))
       (catch Throwable t
         (tel/log! {:level :warn :id ::sound-tap-failed :data {:error (ex-message t)}})))
  nil)
