(ns com.blockether.vis.ext.foundation-voice.core
  "Local voice output through sherpa-onnx Piper/VITS TTS."
  (:require [babashka.process :as process]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.paths :as paths])
  (:import [java.io File FileInputStream FileOutputStream]
           [java.lang ProcessBuilder$Redirect]
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
  (str (System/getProperty "user.home") "/.vis/models/sherpa-onnx-vits-piper-en_US-ryan-high"))

(defn piper-voice
  []
  (or (some-> (vis/extension-env-value voice-env)
              str
              str/trim
              not-empty)
      (some-> (System/getenv voice-env)
              str
              str/trim
              not-empty)
      default-piper-voice))

(defn model-dir
  []
  (or (some-> (vis/extension-env-value model-dir-env)
              str
              str/trim
              not-empty)
      (some-> (System/getenv model-dir-env)
              str
              str/trim
              not-empty)
      default-model-dir))

(defn- existing-file-path
  [files fallback]
  (or (some #(when (.isFile (io/file %)) (str (io/file %))) files) (str (io/file fallback))))

(defn model-files
  ([] (model-files (model-dir)))
  ([dir]
   ;; `/`-separated on every OS (Windows native loaders accept `/`).
   (let [canonical-model
         (io/file dir "model.onnx")

         piper-model
         (io/file dir (str (piper-voice) ".onnx"))]

     {:model (paths/unixify (existing-file-path [canonical-model piper-model] piper-model))
      :tokens (paths/unixify (io/file dir "tokens.txt"))
      :data (paths/unixify (io/file dir "espeak-ng-data"))})))

(defn model-installed?
  ([] (model-installed? (model-dir)))
  ([dir]
   (let [{:keys [model tokens data]} (model-files dir)]
     (and (.isFile (io/file model)) (.isFile (io/file tokens)) (.isDirectory (io/file data))))))

(defn- progress-text
  [label bytes-read bytes-total]
  (if (pos? (long (or bytes-total 0)))
    (let [pct (long (Math/floor (* 100.0 (/ (double bytes-read) (double bytes-total)))))]
      (str label " " pct "%"))
    label))

(defn- notify-progress!
  [phase label bytes-read bytes-total]
  (let [text
        (progress-text label bytes-read bytes-total)

        level
        (if (= :error phase) :error :info)

        event
        (cond-> {:op (if (= :ready phase)
                       :status/clear
                       :status/set)
                 :id :voice/piper
                 :text text
                 :level level
                 :model :voice/piper
                 :phase phase
                 :bytes-read bytes-read
                 :bytes-total bytes-total})]

    (vis/publish-channel-event! :tui event)
    (vis/notify! text :level level :ttl-ms 3000)))

(defn- download!
  ([url path] (download! url path notify-progress!))
  ([url path progress-fn]
   (.mkdirs (.getParentFile (io/file path)))
   (let [conn
         (.openConnection (URL. url))

         total
         (.getContentLengthLong conn)]

     (with-open [in
                 (.getInputStream conn)

                 out
                 (FileOutputStream. (io/file path))]

       (let [buf (byte-array (* 64 1024))]
         (loop [read-total 0
                last-pct -1]

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
    (when (and (seq parts) (not-any? #(or (= % "..") (str/includes? % "\\")) parts))
      (str/join File/separator parts))))

(defn- extract-tar-bz2!
  [archive-path target-dir]
  (.mkdirs (io/file target-dir))
  (with-open [fis
              (FileInputStream. (io/file archive-path))

              bz
              (BZip2CompressorInputStream. fis)

              tar
              (TarArchiveInputStream. bz)]

    (loop []

      (when-let [entry (.getNextTarEntry tar)]
        (when-let [relative (safe-entry-name (.getName entry))]
          (let [out-file (io/file target-dir relative)]
            (if (.isDirectory entry)
              (.mkdirs out-file)
              (do (.mkdirs (.getParentFile out-file))
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
       (try (progress-fn :download "Downloading Piper TTS model" 0 -1)
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
            (finally (try (.delete archive) (catch Throwable _))))))))

(defn- new-instance
  [class-name & args]
  (clojure.lang.Reflector/invokeConstructor (Class/forName class-name) (object-array args)))

(defn- call!
  [target method & args]
  (clojure.lang.Reflector/invokeInstanceMethod target method (object-array args)))

(defn- static-call!
  [class-name method & args]
  (clojure.lang.Reflector/invokeStaticMethod (Class/forName class-name) method (object-array args)))

(defn- tts-config
  [dir]
  (let [{:keys [model tokens data]}
        (model-files dir)

        vits-model
        (-> (static-call! "com.k2fsa.sherpa.onnx.OfflineTtsVitsModelConfig" "builder")
            (call! "setModel" model)
            (call! "setTokens" tokens)
            (call! "setDataDir" data)
            (call! "build"))

        model-cfg
        (-> (static-call! "com.k2fsa.sherpa.onnx.OfflineTtsModelConfig" "builder")
            (call! "setVits" vits-model)
            (call! "setNumThreads" (int (.. Runtime getRuntime availableProcessors)))
            (call! "setDebug" false)
            (call! "build"))]

    (-> (static-call! "com.k2fsa.sherpa.onnx.OfflineTtsConfig" "builder")
        (call! "setModel" model-cfg)
        (call! "build"))))

(defn- speech-node-tag [node] (when (vector? node) (first node)))

(defn- speech-has-attrs? [node] (and (vector? node) (map? (second node))))

(defn- speech-node-attrs [node] (if (speech-has-attrs? node) (second node) {}))

(defn- speech-node-children [node] (if (speech-has-attrs? node) (drop 2 node) (rest node)))

(defn- clean-speech-text
  [s]
  (-> (str s)
      (str/replace #"(?m)^\s*(```|~~~).*$" "")
      (str/replace #"(?m)^\s*\|?\s*:?-{3,}:?\s*(\|\s*:?-{3,}:?\s*)+\|?\s*$" "")
      (str/replace #"(?m)^\s*[-*+•]\s+" "")
      (str/replace #"(?m)^\s*\d+[.)]\s+" "")
      (str/replace #"\s*\|\s*" ", ")
      (str/replace #"[ \t]+" " ")
      (str/replace #"\s+\n" "\n")
      (str/replace #"\n{3,}" "\n\n")
      str/trim))

(defn- answer->speech-text
  [text]
  (let [ir-answer?
        (and (vector? text) (= :ir (first text)))

        md
        (cond (nil? text) nil
              ir-answer? nil
              (and (map? text) (string? (:answer text))) (:answer text)
              (and (map? text) (string? (:answer/text text))) (:answer/text text)
              (string? text) text
              :else (throw (ex-info
                             "voice/synthesize-file! accepts Markdown answers or canonical IR only"
                             {:type :voice/invalid-text
                              :got-type (some-> text
                                                class
                                                .getName)})))

        ir
        (cond ir-answer? (vis/->ast text)
              (str/blank? (str md)) nil
              :else (vis/markdown->ir md))]

    (letfn
      [(children->text
         ([children] (children->text children ""))
         ([children sep]
          (->> children
               (map node->text)
               (remove str/blank?)
               (str/join sep)
               clean-speech-text)))
       (table->text [node]
         (let [rows
               (mapv (fn [row]
                       {:header? (some #(= :th (speech-node-tag %)) (speech-node-children row))
                        :cells (mapv #(children->text (speech-node-children %))
                                     (speech-node-children row))})
                     (speech-node-children node))

               header
               (when (:header? (first rows)) (:cells (first rows)))

               data-rows
               (if header (subvec rows 1) rows)]

           (->> data-rows
                (map (fn [{:keys [cells]}]
                       (if (and (seq header) (= (count header) (count cells)))
                         (->> (map vector header cells)
                              (map (fn [[h c]]
                                     (str h ": " c)))
                              (str/join "; "))
                         (str/join "; " cells))))
                (remove str/blank?)
                (str/join "\n")
                clean-speech-text)))
       (node->text [node]
         (cond (string? node) node
               (not (vector? node)) ""
               :else (let [tag
                           (speech-node-tag node)

                           attrs
                           (speech-node-attrs node)

                           children
                           (speech-node-children node)]

                       (case tag
                         :ir
                         (children->text children "\n\n")

                         (:p :h :quote :li :tr :th :td :strong :em :mark :sup :sub)
                         (children->text children " ")

                         (:ul :ol)
                         (children->text children "\n")

                         :table
                         (table->text node)

                         :code
                         ""

                         :span
                         (children->text children "")

                         :br
                         "\n"

                         :c
                         (children->text children "")

                         :kbd
                         (children->text children "")

                         :a
                         (children->text children "")

                         :img
                         (or (:alt attrs) "")

                         (children->text children " ")))))]
      (clean-speech-text (when ir (node->text ir))))))

(defn synthesize-file!
  "Synthesizes text to a WAV file with local Piper/VITS TTS.

  Options:
  - :out-file required output WAV path
  - :speaker-id defaults to 0
  - :speed defaults to 1.0
  - :model-dir overrides VIS_PIPER_MODEL_DIR/default-model-dir"
  [text
   {:keys [out-file speaker-id speed] model-dir-option :model-dir :or {speaker-id 0 speed 1.0}}]
  (let [dir
        (or model-dir-option (model-dir))

        ;; Vis answers can reach this boundary as raw Markdown (CLI /
        ;; Telegram) or canonical IR (TUI, after response rendering). The
        ;; TTS engine wants spoken prose, not Markdown syntax.
        spoken
        (answer->speech-text text)]

    (ensure-model! dir)
    (when (str/blank? spoken) (throw (ex-info "TTS text is blank" {:type :voice/blank-tts-text})))
    (when (str/blank? (str out-file))
      (throw (ex-info "TTS output file is required" {:type :voice/missing-output-file})))
    (let [parent (.getParentFile (io/file out-file))]
      (when parent (.mkdirs parent)))
    (let [tts
          (new-instance "com.k2fsa.sherpa.onnx.OfflineTts" (tts-config dir))

          audio
          (call! tts "generate" spoken (int speaker-id) (float speed))]

      (call! audio "save" (str out-file))
      {:voice (piper-voice) :model-dir dir :out-file (str (.getAbsoluteFile (io/file out-file)))})))

(defn- executable-success?
  [cmd]
  (try (zero? (.waitFor (.start (ProcessBuilder. ["sh" "-c" (str "command -v " cmd)]))))
       (catch Throwable _ false)))

(defn- player-argv
  [wav-file]
  (if-let [custom (some-> (or (vis/extension-env-value player-env) (System/getenv player-env))
                          str
                          str/trim
                          not-empty)]
    ["sh" "-c" (str custom " " (pr-str (str wav-file)))]
    (cond (executable-success? "afplay") ["afplay" (str wav-file)]
          (executable-success? "paplay") ["paplay" (str wav-file)]
          (executable-success? "aplay") ["aplay" (str wav-file)]
          (executable-success? "ffplay") ["ffplay" "-nodisp" "-autoexit" "-loglevel" "quiet"
                                          (str wav-file)]
          :else nil)))

(defn play-file!
  [wav-file]
  (if-let [argv (player-argv wav-file)]
    (let [p (.start (ProcessBuilder. ^java.util.List argv))]
      {:process p :argv argv})
    (throw (ex-info "No voice playback command found"
                    {:type :voice/missing-player
                     :tried ["VIS_VOICE_PLAYER" "afplay" "paplay" "aplay" "ffplay"]}))))

(def ^:private tts-worker-code
  (str
    "(require '[clojure.edn :as edn] " "'[clojure.string :as str] "
    "'[com.blockether.vis.ext.foundation-voice.core :as voice]) "
    "(let [reader (java.io.BufferedReader. *in*)] "
    "(doseq [line (line-seq reader)] " "(when-not (str/blank? line) "
    "(let [{:keys [answer-file out-file response-file]} (edn/read-string line)] "
    "(try (voice/synthesize-file! (edn/read-string (slurp answer-file)) "
    "{:out-file (java.io.File. out-file)}) " "(spit response-file (pr-str {:ok? true})) "
    "(catch Throwable t "
    "(spit response-file (pr-str {:ok? false :message (or (ex-message t) (str t)) :data (ex-data t)}))))))))"))

(defn- java-bin
  []
  (str (io/file (System/getProperty "java.home")
                "bin"
                (if (str/starts-with? (str/lower-case (System/getProperty "os.name" "")) "win")
                  "java.exe"
                  "java"))))

(defn- tts-worker-argv
  []
  [(java-bin) "-XX:+IgnoreUnrecognizedVMOptions" "--enable-native-access=ALL-UNNAMED"
   "--sun-misc-unsafe-memory-access=allow" "-cp" (System/getProperty "java.class.path")
   "clojure.main" "-e" tts-worker-code])

(defn- voice-log-file
  []
  (let [log-file (io/file (System/getProperty "user.home") ".vis" "vis.log")]
    (.mkdirs (.getParentFile log-file))
    log-file))

(defn- start-tts-worker!
  [log-file]
  (let [pb (ProcessBuilder. ^java.util.List (tts-worker-argv))]
    (.redirectErrorStream pb true)
    (.redirectOutput pb (ProcessBuilder$Redirect/appendTo (io/file log-file)))
    (.start pb)))

(defonce ^:private tts-worker-state (atom nil))

(defn- live-process? [process] (and process (.isAlive ^Process process)))

(defn- stop-tts-worker!
  []
  (when-let [{:keys [process writer]} @tts-worker-state]
    (try (.close ^java.io.Writer writer) (catch Throwable _))
    (try (.destroy ^Process process) (catch Throwable _)))
  (reset! tts-worker-state nil))

(defonce ^:private tts-worker-shutdown-hook
  (delay (.addShutdownHook (Runtime/getRuntime)
                           (Thread. ^Runnable stop-tts-worker! "vis-voice-tts-worker-shutdown"))))

(defn- ensure-tts-worker!
  [log-file]
  (let [{:keys [process] :as existing} @tts-worker-state]
    (if (live-process? process)
      existing
      (let [_ @tts-worker-shutdown-hook
            process (start-tts-worker! log-file)
            writer (java.io.BufferedWriter. (java.io.OutputStreamWriter. (.getOutputStream
                                                                           ^Process process)))]

        (reset! tts-worker-state {:process process :writer writer :log-file (str log-file)})))))

(defn- wait-for-response-file!
  [response-file process log-file]
  (loop [remaining-ms 120000]
    (cond (.isFile ^File response-file)
          (let [{:keys [ok? message data]} (edn/read-string (slurp response-file))]
            (when-not ok?
              (throw (ex-info (str "Voice synthesis worker failed; log: " log-file)
                              (assoc (or data {})
                                :type :voice/tts-worker-failed
                                :log-file (str log-file)
                                :message message))))
            true)
          (not (live-process? process))
          (do (reset! tts-worker-state nil)
              (throw (ex-info (str "Voice synthesis worker exited before response; log: " log-file)
                              {:type :voice/tts-worker-exited :log-file (str log-file)})))
          (not (pos? remaining-ms))
          (throw (ex-info (str "Voice synthesis worker timed out; log: " log-file)
                          {:type :voice/tts-worker-timeout :log-file (str log-file)}))
          :else (do (Thread/sleep 25) (recur (- remaining-ms 25))))))

(defn- synthesize-file-in-worker!
  "Run sherpa-onnx TTS in a persistent child JVM so native stdout/stderr goes to ~/.vis/vis.log, never Lanterna."
  [answer {:keys [out-file]}]
  (when (str/blank? (str out-file))
    (throw (ex-info "TTS output file is required" {:type :voice/missing-output-file})))
  (let [out-file
        (io/file out-file)

        parent
        (.getParentFile out-file)

        answer-file
        (File/createTempFile "vis-voice-answer-" ".edn")

        response-file
        (File/createTempFile "vis-voice-response-" ".edn")

        log-file
        (voice-log-file)]

    (when parent (.mkdirs parent))
    (spit answer-file (pr-str answer))
    (.delete response-file)
    (try
      (let [{:keys [process writer]}
            (ensure-tts-worker! log-file)

            command
            {:answer-file (str answer-file)
             :out-file (str out-file)
             :response-file (str response-file)}]

        (locking writer
          (.write ^java.io.Writer writer (pr-str command))
          (.write ^java.io.Writer writer "\n")
          (.flush ^java.io.Writer writer)
          (wait-for-response-file! response-file process log-file))
        {:voice (piper-voice) :model-dir (model-dir) :out-file (str (.getAbsoluteFile out-file))})
      (finally (try (.delete answer-file) (catch Throwable _))
               (try (.delete response-file) (catch Throwable _))))))

(defn voice-response-prompt
  [env]
  (when (true? (get-in env [:turn/features :voice-response?]))
    (str
      "<voice_response_mode>\n"
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
      "- do not emit SSML unless the user explicitly asks.\n" "</voice_response_mode>")))

(defn speak-answer-async!
  "Synthesize and play `answer` after the caller has persisted the text answer.
   `answer` may be a plain string or canonical `[:ir & nodes]`; the
   downstream `synthesize-file!` projects to plain text on its own."
  [answer]
  (when-not (or (nil? answer) (and (string? answer) (str/blank? answer)))
    (future (try (notify-progress! :synthesize "Synthesizing voice response" 0 -1)
                 (let [wav (File/createTempFile "vis-voice-response-" ".wav")]
                   (synthesize-file-in-worker! answer {:out-file wav})
                   (notify-progress! :play "Speaking..." 0 -1)
                   (let [{:keys [process]} (play-file! wav)]
                     (future (try (.waitFor ^Process process)
                                  (notify-progress! :ready "Voice response complete" 1 1)
                                  (finally (try (.delete wav) (catch Throwable _)))))))
                 (catch Throwable t
                   (notify-progress! :error (str "Voice response failed: " (or (ex-message t) t))
                                     0 -1))))))

(defn- cli-out! [s] (.println ^java.io.PrintStream vis/original-stdout (str s)))

(defn- voice-asr-var
  [sym]
  (or (requiring-resolve (symbol "com.blockether.vis.ext.foundation-voice.asr" (name sym)))
      (throw (ex-info "Voice ASR namespace did not expose expected var"
                      {:type :voice-asr/missing-var :var sym}))))

(defn- voice-asr-call! [sym & args] (apply (voice-asr-var sym) args))

(defn- parakeet-status [] {:installed? (boolean (voice-asr-call! 'model-installed?))})

(defn model-status
  []
  {:piper {:installed? (model-installed?) :dir (model-dir) :files (model-files)}
   :parakeet (parakeet-status)})

(defn- executable?
  [cmd]
  (try (zero? (:exit (process/sh {:out :string :err :string :continue true} "command" "-v" cmd)))
       (catch Throwable _ false)))

(defn- resolved? [sym] (boolean (requiring-resolve sym)))

(defn- voice-runtime-message
  []
  (let [asr?
        (resolved? 'com.blockether.vis.ext.foundation-voice.asr/transcribe-file!)

        tts?
        (resolved? 'com.blockether.vis.ext.foundation-voice.core/synthesize-file!)]

    {:level (if (or asr? tts?) :info :warn)
     :check-id ::runtime
     :message (str "Voice runtime: input=" (if asr? "loaded" "missing")
                   ", output=" (if tts? "loaded" "missing"))
     :remediation (when-not (or asr? tts?)
                    "Add/load vis-foundation-voice, then restart the channel.")}))

(defn- ffmpeg-message
  []
  (if (executable? "ffmpeg")
    {:level :info :check-id ::ffmpeg :message "ffmpeg: installed"}
    {:level :warn
     :check-id ::ffmpeg
     :message "ffmpeg: missing; Telegram voice input cannot convert .oga/.opus to WAV for ASR."
     :remediation "Install ffmpeg and ensure it is on PATH for the Vis/Telegram process."}))

(defn- piper-message
  []
  (try (let [dir
             (model-dir)

             files
             (model-files dir)

             data-dir
             (:data files)]

         (if (model-installed? dir)
           {:level :info :check-id ::piper :message (str "Piper model: installed - " dir)}
           {:level :warn
            :check-id ::piper
            :message (str "Piper model: missing - " dir)
            :remediation
            (str "Run `vis extensions voice models download --piper` or set " model-dir-env
                 " to a complete Piper model directory. Expected espeak-ng-data: " data-dir)}))
       (catch Throwable t
         {:level :warn
          :check-id ::piper
          :message (str "Piper model: check failed: " (or (ex-message t) t))
          :remediation "Run `vis ext voice models status` for detailed voice model diagnostics."})))

(defn- parakeet-message
  []
  (try (if (:installed? (:parakeet (model-status)))
         {:level :info :check-id ::parakeet :message "Parakeet ASR model: installed"}
         {:level :warn
          :check-id ::parakeet
          :message "Parakeet ASR model: missing"
          :remediation
          "Run `vis extensions voice models download --parakeet` or set VIS_PARAKEET_MODEL_DIR."})
       (catch Throwable t
         {:level :warn
          :check-id ::parakeet
          :message (str "Parakeet ASR model: check failed: " (or (ex-message t) t))
          :remediation "Run `vis ext voice models status` for detailed voice model diagnostics."})))

(defn doctor-fn
  [_environment]
  [(voice-runtime-message) (ffmpeg-message) (piper-message) (parakeet-message)])

(defn- print-status!
  []
  (let [{:keys [piper parakeet]} (model-status)]
    (cli-out! (str "Piper: " (if (:installed? piper) "installed" "missing") " - " (:dir piper)))
    (cli-out! (str "Parakeet: " (if (:installed? parakeet) "installed" "missing")))))

(defn- ensure-parakeet! [] (voice-asr-call! 'ensure-model!))

(defn- voice-toggle-recording!
  "Slash run-fn body for `/voice`. Resolves the input ns lazily so the
   host doesn't pay the audio stack cost until the user actually
   triggers voice."
  [ctx]
  (let [toggle (or (requiring-resolve
                     'com.blockether.vis.ext.foundation-voice.input/toggle-recording!)
                   (throw (ex-info "Voice input namespace did not expose toggle-recording!"
                                   {:type :voice-input/missing-toggle})))]
    (toggle ctx)
    {:slash/status :ok :slash/title "Voice recording toggled"}))

(defn- voice-models-status-command [_parsed _residual] (vis/init-cli!) (print-status!))

(defn- voice-models-download-command
  [{piper? "piper" parakeet? "parakeet" all? "all"} residual]
  (vis/init-cli!)
  (let [words
        (set (map str/lower-case residual))

        piper?
        (or piper? all? (contains? words "piper") (contains? words "all"))

        parakeet?
        (or parakeet? all? (contains? words "parakeet") (contains? words "all"))

        any?
        (or piper? parakeet?)]

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
    {:ext/name "foundation-voice"
     :ext/description
     "Native local voice extension: Piper TTS output and Parakeet ASR input through sherpa-onnx."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "voice"
     :ext/prompt-fn voice-response-prompt
     :ext/doctor-fn doctor-fn
     :ext/settings
     [{:key :voice/telegram-send-transcript?
       :type :toggle
       :label "Telegram transcript text"
       :description
       "In Telegram duplex mode, send the transcribed user request before the answer audio."}
      {:key :voice/telegram-send-answer-text?
       :type :toggle
       :label "Telegram answer text"
       :description
       "In Telegram duplex mode, send the plain text answer after the answer audio in a collapsible block."}
      {:key :voice/tui-auto-read?
       :type :toggle
       :label "TUI auto-read answers"
       :description
       "Read TUI answers aloud when voice responses are enabled. TUI still always shows text."}]
     :ext/env
     [{:name model-dir-env
       :label "Piper model directory"
       :description
       "Directory containing model.onnx, tokens.txt, and espeak-ng-data. Missing files can be downloaded with vis extensions voice models download --piper."
       :required? false}
      {:name voice-env
       :label "Piper voice"
       :description "Logical Piper voice id. Defaults to en_US-ryan-high."
       :required? false}
      {:name player-env
       :label "Voice playback command"
       :description
       "Optional local audio playback command. Defaults to afplay/paplay/aplay/ffplay discovery."
       :required? false}
      {:name parakeet-model-dir-env
       :label "Parakeet model directory"
       :description
       "Directory containing encoder.int8.onnx, decoder.int8.onnx, joiner.int8.onnx, and tokens.txt. Missing files can be downloaded with vis extensions voice models download --parakeet."
       :required? false}]
     :ext/cli
     [{:cmd/name "voice"
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
           :cmd/args
           [{:name "piper" :kind :flag :type :boolean :doc "Download/check Piper TTS model."}
            {:name "parakeet" :kind :flag :type :boolean :doc "Download/check Parakeet ASR model."}
            {:name "all" :kind :flag :type :boolean :doc "Download/check both voice models."}]
           :cmd/run-fn #'voice-models-download-command}]}]}]
     ;; Declarative slash registration: both TUI and Telegram channels render
     ;; the same surface via the engine slash registry. The TUI variant of
     ;; /voice toggles recording via input/toggle-recording!. The Telegram
     ;; channel has different /voice semantics (mode picker + inline keyboard)
     ;; and registers its own /voice spec on vis-channel-telegram.
     :ext/slash-commands [{:slash/name "voice"
                           :slash/doc "Toggle voice recording (TUI)."
                           :slash/usage "/voice"
                           ;; HIDDEN from the TUI slash-suggestion box (above the input): voice
                           ;; recording is driven by the C-x v keymap hint + the header status
                           ;; banner, not a typed slash command. Keep the spec registered (the
                           ;; keymap action dispatches it) but off the suggestion list.
                           :slash/hidden? true
                           :slash/requires #{:channel}
                           :slash/availability-fn (fn [{ch :channel/id}]
                                                    (= :tui ch))
                           :slash/run-fn voice-toggle-recording!}]}))

(vis/register-extension! voice-extension)
