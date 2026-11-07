(ns com.blockether.vis.internal.sounds
  "Sound presets and the player of the machine that runs the gateway.

   A preset gives one value to each slot. The slots are the cues:

     finish  a turn completed,
     input   a run waits for the answer of the person,
     error   a turn failed or stopped.

   The `sound_presets` setting holds `{\"active\" name \"custom\" [preset ...]}`.
   A custom preset is `{\"name\" text \"slots\" {slot value}}`. A slot value is
   empty for no sound, the id of a built-in preset for the sound of that preset,
   or the path of a sound file on this machine. A built-in preset plays its own
   sound for each slot.

   The built-in presets are CC0 sounds from Kenney Interface Sounds, in
   `resources/vis-sounds/<preset>/<slot>.wav`. A player program of the platform
   plays the file: `afplay` on macOS, `pw-play`, `paplay` or `aplay` on Linux,
   and PowerShell `Media.SoundPlayer` on Windows (WAV files only). One sound
   plays at a time: a request while a sound plays makes no sound.

   This namespace reads no setting. Callers give it the preset value."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.internal.paths :as paths]
            [taoensso.telemere :as tel])
  (:import [java.io File]
           [java.lang ProcessBuilder$Redirect]
           [java.nio.file CopyOption Files OpenOption StandardCopyOption]
           [java.util List]
           [java.util.concurrent TimeUnit]))

(def slots
  "The slots of a sound preset, in display order."
  [{"id" "finish" "label" "Finish" "description" "A turn completed."}
   {"id" "input" "label" "Question" "description" "Vis waits for your answer."}
   {"id" "error" "label" "Error" "description" "A turn failed or stopped."}])

(def builtin
  "The built-in presets, in display order. Each has one bundled file per slot."
  [{"id" "classic" "label" "Classic"} {"id" "glass" "label" "Glass"} {"id" "soft" "label" "Soft"}
   {"id" "minimal" "label" "Minimal"} {"id" "bright" "label" "Bright"}])

(def ^:private slot-ids (set (map #(get % "id") slots)))

(def ^:private builtin-ids (set (map #(get % "id") builtin)))

(def ^:const MAX_CUSTOM "The most custom presets that the setting keeps." 50)

(def default-value
  "The value of a machine without its own presets."
  {"active" "classic" "custom" []})

(def editor-options
  "The static data of the presets editor of the `sound_presets` setting."
  {"slots" slots "builtin" builtin "is_previewable" true "max_custom" MAX_CUSTOM})

(def ^:private ^:const MAX_PLAY_MS
  "The longest time that one sound plays. A longer file stops here."
  10000)

(defn- folded [s] (str/lower-case (str/trim (str s))))

(defn- slot-problem
  "Nil when `slot` is a slot id and `value` is text, else a message."
  [preset slot value]
  (cond (not (slot-ids slot)) (str "Preset " preset " has an unknown sound: " slot ".")
        (not (string? value)) (str "Preset " preset " needs text for " slot ".")))

(defn problem
  "Nil when `value` has the shape of a `sound_presets` value, else a message for the
   person. It reads no file, so a saved value stays valid after a sound file moves."
  [value]
  (let [custom
        (get value "custom")

        names
        (map #(get % "name") custom)

        taken
        (into builtin-ids (map #(folded (get % "label"))) builtin)]

    (cond (not (document/valid-json? "gateway" "presets_value" value))
          "A preset needs a name of 1 to 60 characters and text for each sound."
          (< MAX_CUSTOM (count custom)) (str "Keep at most " MAX_CUSTOM " presets.")
          :else
          (or (some #(when (taken (folded %)) (str "A built-in preset is already called " % "."))
                    names)
              (some (fn [[k n]]
                      (when (< 1 n) (str "Two presets are called " k ".")))
                    (frequencies (map folded names)))
              (some (fn [{:strs [name] preset-slots "slots"}]
                      (some (fn [[slot v]]
                              (slot-problem name slot v))
                            preset-slots))
                    custom)
              (when-not (or (builtin-ids (get value "active")) (some #{(get value "active")} names))
                (str "No preset is called " (get value "active") "."))))))

(defn file-problem
  "Nil when each path in the `sound_presets` value `value` names a file on this
   machine, else a message. Settings check this only when a person writes."
  [value]
  (some (fn [{:strs [name] preset-slots "slots"}]
          (some (fn [[_ v]]
                  (when-not (or (str/blank? v)
                                (builtin-ids v)
                                (.isFile (io/file (paths/expand-home v))))
                    (str "Preset " name ": no sound file at " v ".")))
                preset-slots))
        (get value "custom")))

(defn- bundled-file
  "A file copy of the bundled sound of `preset` and `slot` in the temporary
   directory. The copy is written again when it is absent or has another size,
   so a cleaned or broken temporary directory repairs itself."
  ^File [preset slot]
  (let [resource
        (or (io/resource (str "vis-sounds/" preset "/" slot ".wav"))
            (throw (ex-info "Bundled sound is missing"
                            {:type :vis.sounds/missing-bundle :preset preset :slot slot})))

        content
        (with-open [in (io/input-stream resource)]
          (.readAllBytes in))

        target
        (io/file (paths/temp-dir) "vis-sounds" preset (str slot ".wav"))]

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

(defn slot-file
  "The file that slot value `value` plays in slot `slot`, or nil for no sound.
   Throws when `value` names a path that is not a file."
  ^File [slot value]
  (let [value (str/trim (str value))]
    (cond (str/blank? value) nil
          (builtin-ids value) (bundled-file value slot)
          :else (let [file (io/file (paths/expand-home value))]
                  (when-not (.isFile file)
                    (throw (ex-info (str "Sound file not found: " value)
                                    {:type :vis.sounds/missing-file :slot slot :path value})))
                  file))))

(defn cue-file
  "The file that `cue` plays under the `sound_presets` value `presets`, or nil.
   An active name that no preset carries falls back to the default preset."
  ^File [presets cue]
  (let [slot
        (name cue)

        active
        (get presets "active")]

    (if (builtin-ids active)
      (bundled-file active slot)
      (if-let [preset (some #(when (= active (get % "name")) %) (get presets "custom"))]
        (slot-file slot (get-in preset ["slots" slot] ""))
        (bundled-file (get default-value "active") slot)))))

(defn- os-name [] (str/lower-case (str (System/getProperty "os.name"))))

(defn- on-path
  "The executable `program` on PATH, or nil. Resolved on each call, so a player
   that is installed while Vis runs plays the next sound."
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
  ;; The player process of the sound that plays now, or nil. Only `play-file!` writes it.
  (atom nil))

(defn play-file!
  "Play `file` now. Returns true when a player starts, and false when `file` is
   nil, a sound plays already or this machine has no player."
  [^File file]
  (if-let [command (some-> file
                           .getPath
                           player-command)]
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
    (do (when file
          (tel/log! {:level :debug :id ::no-player :data {:os (os-name)}}
                    "No sound player on this machine"))
        false)))

(defn preview!
  "Play slot value `value` in slot `slot`, before the person saves it. Returns
   true when a player starts. Throws with a message for an unknown slot or a
   path that is not a file."
  [slot value]
  (when-not (slot-ids slot)
    (throw (ex-info (str "Unknown sound: " slot) {:type :vis.sounds/unknown-slot :slot slot})))
  (play-file! (slot-file slot value)))
