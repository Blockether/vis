(ns com.blockether.vis.internal.gateway.sounds
  "Sounds on the machine that runs the gateway.

   ONE job: when a turn ends or a run waits for the person, play a short sound
   on this machine. There are three cues:

     :finish  a turn completed,
     :input   a run waits for the answer of the person (an input View),
     :error   a turn failed or stopped.

   Only a turn that the person waits on makes a sound. This is the rule of push
   alerts (`push/operator-turn?`), so subagent turns, Council wakes and
   automation runs stay silent.

   Machine settings, group `sounds`: the `sounds` switch turns the cues on.
   `sound_finish`, `sound_input` and `sound_error` each hold an empty value for
   the bundled sound, `off` for no sound, or the path of a sound file on this
   machine. A path that names no file logs a warning and plays nothing.

   The bundled sounds are CC0 sounds from Kenney Interface Sounds, in
   `resources/vis-sounds/`. A player program of the platform plays the file:
   `afplay` on macOS, `pw-play`, `paplay` or `aplay` on Linux, and PowerShell
   `Media.SoundPlayer` on Windows (WAV files only). The gateway process plays the
   sound, so a gateway on another machine plays it there. One sound plays at a
   time: a cue while a sound plays makes no sound."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.gateway.push :as push]
            [com.blockether.vis.internal.paths :as paths]
            [taoensso.telemere :as tel])
  (:import [java.io File]
           [java.lang ProcessBuilder$Redirect]
           [java.nio.file CopyOption Files OpenOption StandardCopyOption]
           [java.util List]
           [java.util.concurrent TimeUnit]))

(def cue-settings
  "The setting that holds the sound of each cue."
  {:finish "sound_finish" :input "sound_input" :error "sound_error"})

(def ^:private ^:const MAX_PLAY_MS
  "The longest time that one sound plays. A longer file stops here."
  10000)

(def ^:private TERMINAL_TYPES #{"turn.completed" "turn.failed" "turn.cancelled"})

(defn cue
  "The cue that `event` of session `sid` raises, or nil."
  [sid event]
  (let [event-type (get event "type")]
    (cond (and (= "view.open" event-type) (= "input" (get event "kind"))) :input
          (and (TERMINAL_TYPES event-type) (push/operator-turn? sid event))
          (if (= "turn.completed" event-type) :finish :error))))

(defn- bundled-file
  "A file copy of the bundled sound of `cue` in the temporary directory. The copy
   is written again when it is absent or has another size, so a cleaned or broken
   temporary directory repairs itself."
  ^File [cue]
  (let [resource
        (or (io/resource (str "vis-sounds/" (name cue) ".wav"))
            (throw (ex-info "Bundled sound is missing"
                            {:type :vis.sounds/missing-bundle :cue cue})))

        content
        (with-open [in (io/input-stream resource)]
          (.readAllBytes in))

        target
        (io/file (paths/temp-dir) "vis-sounds" (str (name cue) ".wav"))]

    (when-not (and (.isFile target) (= (.length target) (alength ^bytes content)))
      (io/make-parents target)
      (let [temporary (File/createTempFile "sound-" ".wav" (.getParentFile target))]
        (try (Files/write (.toPath temporary)
                          ^bytes content
                          ^"[Ljava.nio.file.OpenOption;" (make-array OpenOption 0))
             (Files/move (.toPath temporary)
                         (.toPath target)
                         (into-array CopyOption
                                     [StandardCopyOption/ATOMIC_MOVE
                                      StandardCopyOption/REPLACE_EXISTING]))
             (finally (.delete temporary)))))
    target))

(defn sound-file
  "The file that `cue` plays, or nil when its setting is `off`. Throws when the
   setting names a path that is not a file."
  ^File [cue]
  (let [value (str/trim (str (toggles/value-of (cue-settings cue))))]
    (cond (str/blank? value) (bundled-file cue)
          (= "off" (str/lower-case value)) nil
          :else (let [file (io/file (paths/expand-home value))]
                  (when-not (.isFile file)
                    (throw (ex-info (str "Sound file not found: " value)
                                    {:type :vis.sounds/missing-file :cue cue :path value})))
                  file))))

(defn- os-name [] (str/lower-case (str (System/getProperty "os.name"))))

(defn- on-path
  "The executable `program` on PATH, or nil. Resolved on each call, so a player
   that is installed while Vis runs plays the next cue."
  [program]
  (some #(let [f (io/file % program)] (when (.canExecute f) (str f)))
        (str/split (str (System/getenv "PATH")) (re-pattern File/pathSeparator))))

(defn player-command
  "The command that plays `path` on this platform, or nil when no player exists."
  [^String path]
  (let [os (os-name)]
    (cond (str/includes? os "mac") ["afplay" path]
          (str/includes? os "win")
          ["powershell" "-NoProfile" "-NonInteractive" "-Command"
           (str "(New-Object Media.SoundPlayer '" (str/replace path "'" "''") "').PlaySync()")]
          :else (some #(when-let [program (on-path %)] [program path])
                      ["pw-play" "paplay" "aplay"]))))

(defonce ^:private playing
  ;; The player process of the sound that plays now, or nil. Only `play!` writes it.
  (atom nil))

(defn play!
  "Play the sound of `cue` now. Returns true when a player starts, and false
   when the cue is off, a sound plays already or this machine has no player."
  [cue]
  (if-let [file (sound-file cue)]
    (if-let [command (player-command (.getPath file))]
      (locking playing
        (let [^Process current @playing]
          (if (and current (.isAlive current))
            false
            (let [process (-> (ProcessBuilder. ^List command)
                              (.redirectOutput ProcessBuilder$Redirect/DISCARD)
                              (.redirectError ProcessBuilder$Redirect/DISCARD)
                              (.start))]
              (.close (.getOutputStream process))
              (reset! playing process)
              (future (when-not (.waitFor process MAX_PLAY_MS TimeUnit/MILLISECONDS)
                        (.destroyForcibly process)))
              true))))
      (do (tel/log! {:level :debug :id ::no-player :data {:cue cue :os (os-name)}}
                    "No sound player on this machine")
          false))
    false))

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
