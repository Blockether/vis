(ns com.blockether.vis.ext.voice.core
  "Local voice output through sherpa-onnx Piper/VITS TTS."
  (:require [babashka.process :as process]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis])
  (:import [java.io File FileInputStream FileOutputStream]
           [java.net URL]
           [org.apache.commons.compress.archivers.tar TarArchiveInputStream]
           [org.apache.commons.compress.compressors.bzip2 BZip2CompressorInputStream]))

(def model-dir-env "VIS_PIPER_MODEL_DIR")
(def voice-env "VIS_PIPER_VOICE")
(def player-env "VIS_VOICE_PLAYER")
(def parakeet-model-dir-env "VIS_PARAKEET_MODEL_DIR")

(def default-piper-voice
  "Sharp default Piper voice selected for clarity and pleasant prosody."
  "en_US-ryan-high")

(def model-url
  "https://github.com/k2-fsa/sherpa-onnx/releases/download/tts-models/vits-piper-en_US-ryan-high.tar.bz2")

(def default-model-dir
  "~/.vis model path used when no env/config override is set."
  (str (System/getProperty "user.home")
    "/.vis/models/sherpa-onnx-vits-piper-en_US-ryan-high"))

(defn piper-voice
  []
  (or (some-> (vis/extension-env-value voice-env) str str/trim not-empty)
    (some-> (System/getenv voice-env) str str/trim not-empty)
    default-piper-voice))

(defn model-dir
  []
  (or (some-> (vis/extension-env-value model-dir-env) str str/trim not-empty)
    (some-> (System/getenv model-dir-env) str str/trim not-empty)
    default-model-dir))

(defn- existing-file-path
  [files fallback]
  (or (some #(when (.isFile (io/file %)) (str (io/file %))) files)
    (str (io/file fallback))))

(defn model-files
  ([] (model-files (model-dir)))
  ([dir]
   (let [canonical-model (io/file dir "model.onnx")
         piper-model     (io/file dir (str (piper-voice) ".onnx"))]
     {:model  (existing-file-path [canonical-model piper-model] piper-model)
      :tokens (str (io/file dir "tokens.txt"))
      :data   (str (io/file dir "espeak-ng-data"))})))

(defn model-installed?
  ([] (model-installed? (model-dir)))
  ([dir]
   (let [{:keys [model tokens data]} (model-files dir)]
     (and (.isFile (io/file model))
       (.isFile (io/file tokens))
       (.isDirectory (io/file data))))))

(defn- progress-text
  [label bytes-read bytes-total]
  (if (pos? (long (or bytes-total 0)))
    (let [pct (long (Math/floor (* 100.0 (/ (double bytes-read) (double bytes-total)))))]
      (str label " " pct "%"))
    label))

(defn- notify-progress!
  [phase label bytes-read bytes-total]
  (let [event {:op :status/set
               :id :voice/piper
               :text (progress-text label bytes-read bytes-total)
               :level (if (= :error phase) :error :info)
               :model :voice/piper
               :phase phase
               :bytes-read bytes-read
               :bytes-total bytes-total}]
    (vis/publish-channel-event! :tui event)
    (vis/notify! (:text event) :level (:level event) :ttl-ms 3000)))

(defn- download!
  ([url path] (download! url path notify-progress!))
  ([url path progress-fn]
   (.mkdirs (.getParentFile (io/file path)))
   (let [conn (.openConnection (URL. url))
         total (.getContentLengthLong conn)]
     (with-open [in  (.getInputStream conn)
                 out (FileOutputStream. (io/file path))]
       (let [buf (byte-array (* 64 1024))]
         (loop [read-total 0 last-pct -1]
           (let [n (.read in buf)]
             (when (pos? n)
               (.write out buf 0 n)
               (let [read-total' (+ read-total n)
                     pct (if (pos? total)
                           (long (Math/floor (* 100.0 (/ (double read-total') (double total)))))
                           -1)]
                 (when (or (neg? pct) (>= (- pct last-pct) 5) (= pct 100))
                   (progress-fn :download "Downloading Piper TTS model" read-total' total))
                 (recur read-total' pct)))))))
     path)))

(defn- safe-entry-name
  [entry-name]
  (let [parts (->> (str/split entry-name #"/")
                (remove str/blank?)
                ;; Release archives contain a top-level directory. Strip it so
                ;; custom VIS_PIPER_MODEL_DIR targets get the files directly.
                rest)]
    (when (and (seq parts)
            (not-any? #(or (= % "..") (str/includes? % "\\")) parts))
      (str/join File/separator parts))))

(defn- extract-tar-bz2!
  [archive-path target-dir]
  (.mkdirs (io/file target-dir))
  (with-open [fis (FileInputStream. (io/file archive-path))
              bz  (BZip2CompressorInputStream. fis)
              tar (TarArchiveInputStream. bz)]
    (loop []
      (when-let [entry (.getNextTarEntry tar)]
        (when-let [relative (safe-entry-name (.getName entry))]
          (let [out-file (io/file target-dir relative)]
            (if (.isDirectory entry)
              (.mkdirs out-file)
              (do
                (.mkdirs (.getParentFile out-file))
                (with-open [out (FileOutputStream. out-file)]
                  (io/copy tar out))))))
        (recur))))
  target-dir)

(defn ensure-model!
  "Download and extract the default Piper model if missing. Returns model dir."
  ([] (ensure-model! (model-dir)))
  ([dir] (ensure-model! dir notify-progress!))
  ([dir progress-fn]
   (if (model-installed? dir)
     dir
     (let [archive (File/createTempFile "vis-piper-model-" ".tar.bz2")]
       (try
         (progress-fn :download "Downloading Piper TTS model" 0 -1)
         (download! model-url archive progress-fn)
         (progress-fn :extract "Extracting Piper TTS model" 0 -1)
         (extract-tar-bz2! archive dir)
         (when-not (model-installed? dir)
           (throw (ex-info "Piper model download did not produce expected files"
                    {:type :voice/download-incomplete
                     :model-dir dir
                     :expected (model-files dir)})))
         (progress-fn :ready "Piper TTS model ready" 1 1)
         dir
         (catch Throwable t
           (progress-fn :error (str "Piper model failed: " (or (ex-message t) t)) 0 -1)
           (throw t))
         (finally
           (try (.delete archive) (catch Throwable _))))))))

(defn- new-instance
  [class-name & args]
  (clojure.lang.Reflector/invokeConstructor
    (Class/forName class-name)
    (object-array args)))

(defn- call!
  [target method & args]
  (clojure.lang.Reflector/invokeInstanceMethod
    target method (object-array args)))

(defn- static-call!
  [class-name method & args]
  (clojure.lang.Reflector/invokeStaticMethod
    (Class/forName class-name) method (object-array args)))

(defn- tts-config
  [dir]
  (let [{:keys [model tokens data]} (model-files dir)
        vits-model (-> (static-call! "com.k2fsa.sherpa.onnx.OfflineTtsVitsModelConfig" "builder")
                     (call! "setModel" model)
                     (call! "setTokens" tokens)
                     (call! "setDataDir" data)
                     (call! "build"))
        model-cfg  (-> (static-call! "com.k2fsa.sherpa.onnx.OfflineTtsModelConfig" "builder")
                     (call! "setVits" vits-model)
                     (call! "setNumThreads" (int (.. Runtime getRuntime availableProcessors)))
                     (call! "setDebug" false)
                     (call! "build"))]
    (-> (static-call! "com.k2fsa.sherpa.onnx.OfflineTtsConfig" "builder")
      (call! "setModel" model-cfg)
      (call! "build"))))

(defn synthesize-file!
  "Synthesizes text to a WAV file with local Piper/VITS TTS.

  Options:
  - :out-file required output WAV path
  - :speaker-id defaults to 0
  - :speed defaults to 1.0
  - :model-dir overrides VIS_PIPER_MODEL_DIR/default-model-dir"
  [text {:keys [out-file speaker-id speed]
         model-dir-option :model-dir
         :or {speaker-id 0 speed 1.0}}]
  (let [dir (or model-dir-option (model-dir))
        ;; Vis answers travel as raw Markdown source. The TTS engine
        ;; wants plain prose with no Markdown markup, so lift the
        ;; Markdown to IR via `vis/markdown->ir` and walk it to plain
        ;; text. The Markdown-answer pipeline produces exactly two
        ;; shapes: `{:answer string}` and the needs-input map; a raw
        ;; string is accepted as the prompt-side caller convention.
        md     (cond
                 (nil? text)                                        nil
                 (and (map? text) (string? (:answer text)))         (:answer text)
                 (and (map? text) (string? (:answer/text text)))    (:answer/text text)
                 (string? text)                                     text
                 :else
                 (throw (ex-info "voice/synthesize-file! accepts Markdown answers only"
                          {:type :voice/invalid-text
                           :got-type (some-> text class .getName)})))
        spoken (if (str/blank? (str md))
                 ""
                 (vis/extract-text (vis/markdown->ir md)))]
    (ensure-model! dir)
    (when (str/blank? spoken)
      (throw (ex-info "TTS text is blank" {:type :voice/blank-tts-text})))
    (when (str/blank? (str out-file))
      (throw (ex-info "TTS output file is required" {:type :voice/missing-output-file})))
    (let [parent (.getParentFile (io/file out-file))]
      (when parent (.mkdirs parent)))
    (let [tts   (new-instance "com.k2fsa.sherpa.onnx.OfflineTts" (tts-config dir))
          audio (call! tts "generate" spoken (int speaker-id) (float speed))]
      (call! audio "save" (str out-file))
      {:voice (piper-voice)
       :model-dir dir
       :out-file (str (.getAbsoluteFile (io/file out-file)))})))

(defn- executable-success?
  [cmd]
  (try
    (zero? (.waitFor (.start (ProcessBuilder. ["sh" "-c" (str "command -v " cmd)]))))
    (catch Throwable _ false)))

(defn- player-argv
  [wav-file]
  (if-let [custom (some-> (or (vis/extension-env-value player-env)
                            (System/getenv player-env)) str str/trim not-empty)]
    ["sh" "-c" (str custom " " (pr-str (str wav-file)))]
    (cond
      (executable-success? "afplay") ["afplay" (str wav-file)]
      (executable-success? "paplay") ["paplay" (str wav-file)]
      (executable-success? "aplay")  ["aplay" (str wav-file)]
      (executable-success? "ffplay") ["ffplay" "-nodisp" "-autoexit" "-loglevel" "quiet" (str wav-file)]
      :else nil)))

(defn play-file!
  [wav-file]
  (if-let [argv (player-argv wav-file)]
    (let [p (.start (ProcessBuilder. ^java.util.List argv))]
      {:process p :argv argv})
    (throw (ex-info "No voice playback command found"
             {:type :voice/missing-player
              :tried ["VIS_VOICE_PLAYER" "afplay" "paplay" "aplay" "ffplay"]}))))

(defn voice-response-prompt
  [env]
  (when (true? (get-in env [:turn/features :voice-response?]))
    (str "<voice_response_mode>\n"
      "The user may receive your final answer through text-to-speech audio.\n\n"
      "Still produce the canonical final answer as plain text. This text is saved to the session database.\n\n"
      "Optimize the final answer for spoken delivery:\n"
      "- speak like a manager update, not a developer handoff;\n"
      "- be concise, direct, and outcome-focused;\n"
      "- say what went wrong and what changed, but skip implementation details unless explicitly requested;\n"
      "- do not include extra trails, info refs, verification logs, stack traces, diffs, or code blocks;\n"
      "- do not read code aloud; summarize code changes in plain business language;\n"
      "- avoid huge tables unless explicitly requested;\n"
      "- when code/files changed, summarize what changed and name files clearly;\n"
      "- do not mention that you are producing audio;\n"
      "- do not emit SSML unless the user explicitly asks.\n"
      "</voice_response_mode>")))

(defn speak-answer-async!
  "Synthesize and play `answer` after the caller has persisted the text answer.
   `answer` may be a plain string or canonical `[:ir & nodes]`; the
   downstream `synthesize-file!` projects to plain text on its own."
  [answer]
  (when-not (or (nil? answer)
              (and (string? answer) (str/blank? answer)))
    (future
      (try
        (notify-progress! :synthesize "Synthesizing voice response" 0 -1)
        (let [wav (File/createTempFile "vis-voice-response-" ".wav")]
          (synthesize-file! answer {:out-file wav})
          (notify-progress! :play "Speaking..." 0 -1)
          (let [{:keys [process]} (play-file! wav)]
            (future
              (try
                (.waitFor ^Process process)
                (notify-progress! :ready "Voice response complete" 1 1)
                (finally (try (.delete wav) (catch Throwable _)))))))
        (catch Throwable t
          (notify-progress! :error (str "Voice response failed: " (or (ex-message t) t)) 0 -1))))))

(defn- cli-out!
  [s]
  (.println ^java.io.PrintStream vis/original-stdout (str s)))

(defn- voice-asr-var
  [sym]
  (or (requiring-resolve (symbol "com.blockether.vis.ext.voice.asr" (name sym)))
    (throw (ex-info "Voice ASR namespace did not expose expected var"
             {:type :voice-asr/missing-var
              :var sym}))))

(defn- voice-asr-call!
  [sym & args]
  (apply (voice-asr-var sym) args))

(defn- parakeet-status
  []
  {:installed? (boolean (voice-asr-call! 'model-installed?))})

(defn model-status
  []
  {:piper {:installed? (model-installed?)
           :dir (model-dir)
           :files (model-files)}
   :parakeet (parakeet-status)})

(defn- executable? [cmd]
  (try
    (zero? (:exit (process/sh {:out :string :err :string :continue true}
                    "command" "-v" cmd)))
    (catch Throwable _ false)))

(defn- resolved? [sym]
  (boolean (requiring-resolve sym)))

(defn- voice-runtime-message []
  (let [asr? (resolved? 'com.blockether.vis.ext.voice.asr/transcribe-file!)
        tts? (resolved? 'com.blockether.vis.ext.voice.core/synthesize-file!)]
    {:level (if (or asr? tts?) :info :warn)
     :check-id ::runtime
     :message (str "Voice runtime: input=" (if asr? "loaded" "missing")
                ", output=" (if tts? "loaded" "missing"))
     :remediation (when-not (or asr? tts?)
                    "Add/load vis-voice, then restart the channel.")}))

(defn- ffmpeg-message []
  (if (executable? "ffmpeg")
    {:level :info
     :check-id ::ffmpeg
     :message "ffmpeg: installed"}
    {:level :warn
     :check-id ::ffmpeg
     :message "ffmpeg: missing; Telegram voice input cannot convert .oga/.opus to WAV for ASR."
     :remediation "Install ffmpeg and ensure it is on PATH for the Vis/Telegram process."}))

(defn- piper-message []
  (try
    (let [dir      (model-dir)
          files    (model-files dir)
          data-dir (:data files)]
      (if (model-installed? dir)
        {:level :info
         :check-id ::piper
         :message (str "Piper model: installed - " dir)}
        {:level :warn
         :check-id ::piper
         :message (str "Piper model: missing - " dir)
         :remediation (str "Run `vis extensions voice models download --piper` or set "
                        model-dir-env " to a complete Piper model directory. Expected espeak-ng-data: "
                        data-dir)}))
    (catch Throwable t
      {:level :warn
       :check-id ::piper
       :message (str "Piper model: check failed: " (or (ex-message t) t))
       :remediation "Run `vis ext voice models status` for detailed voice model diagnostics."})))

(defn- parakeet-message []
  (try
    (if (:installed? (:parakeet (model-status)))
      {:level :info
       :check-id ::parakeet
       :message "Parakeet ASR model: installed"}
      {:level :warn
       :check-id ::parakeet
       :message "Parakeet ASR model: missing"
       :remediation "Run `vis extensions voice models download --parakeet` or set VIS_PARAKEET_MODEL_DIR."})
    (catch Throwable t
      {:level :warn
       :check-id ::parakeet
       :message (str "Parakeet ASR model: check failed: " (or (ex-message t) t))
       :remediation "Run `vis ext voice models status` for detailed voice model diagnostics."})))

(defn doctor-fn [_environment]
  [(voice-runtime-message)
   (ffmpeg-message)
   (piper-message)
   (parakeet-message)])

(defn- print-status! []
  (let [{:keys [piper parakeet]} (model-status)]
    (cli-out! (str "Piper: " (if (:installed? piper) "installed" "missing") " - " (:dir piper)))
    (cli-out! (str "Parakeet: " (if (:installed? parakeet) "installed" "missing")))))

(defn- ensure-parakeet! []
  (voice-asr-call! 'ensure-model!))

(defn- voice-input-tui-commands
  [ctx]
  ((or (requiring-resolve 'com.blockether.vis.ext.voice.input/tui-commands)
     (throw (ex-info "Voice input namespace did not expose TUI commands"
              {:type :voice-input/missing-tui-commands})))
   ctx))

(defn- voice-models-status-command [_parsed _residual]
  (vis/init-cli!)
  (print-status!))

(defn- voice-models-download-command [{piper? "piper" parakeet? "parakeet" all? "all"} residual]
  (vis/init-cli!)
  (let [words (set (map str/lower-case residual))
        piper? (or piper? all? (contains? words "piper") (contains? words "all"))
        parakeet? (or parakeet? all? (contains? words "parakeet") (contains? words "all"))
        any? (or piper? parakeet?)]
    (when (or piper? (not any?))
      (cli-out! "Downloading/checking Piper TTS model...")
      (ensure-model!)
      (cli-out! (str "Piper ready: " (model-dir))))
    (when (or parakeet? (not any?))
      (cli-out! "Downloading/checking Parakeet ASR model...")
      (cli-out! (str "Parakeet ready: " (ensure-parakeet!))))
    (print-status!)))

(def voice-extension
  (vis/extension
    {:ext/name      "voice"
     :ext/description "Native local voice extension: Piper TTS output and Parakeet ASR input through sherpa-onnx."
     :ext/version   "0.1.0"
     :ext/author    "Blockether"
     :ext/owner     "vis"
     :ext/license   "Apache-2.0"
     :ext/kind      "voice"
     :ext/prompt    voice-response-prompt
     :ext/doctor-fn doctor-fn
     :ext/settings  [{:key :voice/respond?
                      :type :toggle
                      :label "Voice responses"
                      :description "Speak assistant final answers with Piper TTS. The answer is still saved as text."}
                     {:key :voice/telegram-send-transcript?
                      :type :toggle
                      :label "Telegram transcript text"
                      :description "In Telegram duplex mode, send the transcribed user request before the answer audio."}
                     {:key :voice/telegram-send-answer-text?
                      :type :toggle
                      :label "Telegram answer text"
                      :description "In Telegram duplex mode, send the plain text answer after the answer audio in a collapsible block."}
                     {:key :voice/tui-auto-read?
                      :type :toggle
                      :label "TUI auto-read answers"
                      :description "Read TUI answers aloud when voice responses are enabled. TUI still always shows text."}]
     :ext/env       [{:name model-dir-env
                      :label "Piper model directory"
                      :description "Directory containing model.onnx, tokens.txt, and espeak-ng-data. Missing files can be downloaded with vis extensions voice models download --piper."
                      :required? false}
                     {:name voice-env
                      :label "Piper voice"
                      :description "Logical Piper voice id. Defaults to en_US-ryan-high."
                      :required? false}
                     {:name player-env
                      :label "Voice playback command"
                      :description "Optional local audio playback command. Defaults to afplay/paplay/aplay/ffplay discovery."
                      :required? false}
                     {:name parakeet-model-dir-env
                      :label "Parakeet model directory"
                      :description "Directory containing encoder.int8.onnx, decoder.int8.onnx, joiner.int8.onnx, and tokens.txt. Missing files can be downloaded with vis extensions voice models download --parakeet."
                      :required? false}]
     :ext/cli       [{:cmd/name "voice"
                      :cmd/doc "Voice extension commands."
                      :cmd/usage "vis extensions voice <models>"
                      :cmd/subcommands
                      [{:cmd/name "models"
                        :cmd/doc "Manage local voice models."
                        :cmd/usage "vis extensions voice models <status|download>"
                        :cmd/subcommands
                        [{:cmd/name "status"
                          :cmd/doc "Show local Piper and Parakeet model status."
                          :cmd/usage "vis extensions voice models status"
                          :cmd/run-fn #'voice-models-status-command}
                         {:cmd/name "download"
                          :cmd/doc "Download local voice models. Defaults to all when no flag is supplied."
                          :cmd/usage "vis extensions voice models download [--piper|--parakeet|--all]"
                          :cmd/args [{:name "piper" :kind :flag :type :boolean
                                      :doc "Download/check Piper TTS model."}
                                     {:name "parakeet" :kind :flag :type :boolean
                                      :doc "Download/check Parakeet ASR model."}
                                     {:name "all" :kind :flag :type :boolean
                                      :doc "Download/check both voice models."}]
                          :cmd/run-fn #'voice-models-download-command}]}]}]
     :ext/channel-contributions
     {:tui.slot/commands
      [{:id :voice/input
        :fn #'voice-input-tui-commands}]}}))

(vis/register-extension! voice-extension)
