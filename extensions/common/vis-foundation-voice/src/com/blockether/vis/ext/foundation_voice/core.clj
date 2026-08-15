(ns com.blockether.vis.ext.foundation-voice.core
  "Local voice: Parakeet ASR in, Piper or pocket-tts speech out."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]))

(defn- cli-out! [s] (.println ^java.io.PrintStream vis/original-stdout (str s)))

(defn- ext-var
  "A var from one of the voice namespaces, resolved on FIRST USE. Nothing here
   requires `asr`, `tts` or `assets` at load time: this extension is registered
   during startup and the audio stack - sherpa's natives included - costs far
   too much to pay for on a run that never speaks."
  [ns-name sym]
  (or (requiring-resolve (symbol ns-name (name sym)))
      (throw (ex-info (str "Voice namespace " ns-name " did not expose " sym)
                      {:type :voice-ext/missing-var :ns ns-name :var sym}))))

(defn- asr-call!
  [sym & args]
  (apply (ext-var "com.blockether.vis.ext.foundation-voice.asr" sym) args))

(defn- tts-call!
  [sym & args]
  (apply (ext-var "com.blockether.vis.ext.foundation-voice.tts" sym) args))

(defn- assets-call!
  [sym & args]
  (apply (ext-var "com.blockether.vis.ext.foundation-voice.assets" sym) args))

(defn- voices-call!
  [sym & args]
  (apply (ext-var "com.blockether.vis.ext.foundation-voice.voices" sym) args))

(defn- attribution-call!
  [sym & args]
  (apply (ext-var "com.blockether.vis.ext.foundation-voice.attribution" sym) args))
(defn- parakeet-status [] {:installed? (boolean (asr-call! 'model-installed?))})

(defn- speech-status
  []
  (into {}
        (map (fn [family]
               [family (tts-call! 'model-state family)]))
        [:piper :pocket-tts]))

(defn- espeak-status
  "Piper's phonemizer data, which the SYSTEM owns. Vis does not ship espeak-ng
   (GPL-3.0-or-later, and in every package manager), so this reports what is
   on the machine and, when nothing is, the command that puts it there."
  []
  (let [dir (tts-call! 'espeak-data-dir)]
    {:dir dir :is-installed (some? dir) :remediation (tts-call! 'espeak-install-hint)}))

(defn- asset-status
  [entry]
  {:id (:id entry)
   :engine (:engine entry)
   :license (:license entry)
   :attribution (:attribution entry)
   :notice (:notice entry)
   :dir (assets-call! 'install-dir entry)
   :is-installed (boolean (assets-call! 'installed? entry))
   :is-opt-in (boolean (:is-opt-in entry))})

(defn model-status
  "Every local voice model in one map - the shape doctor and the CLI both read:

     :parakeet  the ASR model, which honours VIS_PARAKEET_MODEL_DIR
     :espeak    the system's espeak-ng tables, without which Piper cannot speak
     :speech    per speaking family, the readiness a UI polls
     :assets    every manifest entry, with its licence and whether it is here"
  []
  {:parakeet (parakeet-status)
   :espeak (espeak-status)
   :speech (speech-status)
   :assets (mapv asset-status (assets-call! 'manifest))})

(defn- executable?
  "True when `cmd` resolves to an executable file on PATH.

  A pure PATH walk on purpose. This used to shell out to `command -v`, but
  `command` is a POSIX *shell builtin* with no binary behind it: exec'ing it
  directly throws ENOENT on Debian and macOS alike, the catch swallowed it, and
  doctor reported every tool as missing — `ffmpeg` warned even in the container
  that build-asserts `ffmpeg -version`. Staying in-process also avoids paying a
  fork per check."
  [cmd]
  (try (boolean (some (fn [dir]
                        (let [f (java.io.File. ^String dir ^String cmd)]
                          (and (.isFile f) (.canExecute f))))
                      (str/split (or (System/getenv "PATH") "")
                                 (re-pattern (java.util.regex.Pattern/quote
                                               (System/getProperty "path.separator" ":"))))))
       (catch Throwable _ false)))

(defn- resolved? [sym] (boolean (requiring-resolve sym)))

(defn- voice-runtime-message
  []
  (let [asr? (resolved? 'com.blockether.vis.ext.foundation-voice.asr/transcribe-file!)]
    {:level (if asr? :info :warn)
     :check-id ::runtime
     :message (str "Voice runtime: input=" (if asr? "loaded" "missing"))
     :remediation (when-not asr? "Add/load vis-foundation-voice, then restart the channel.")}))

(defn- ffmpeg-message
  []
  (if (executable? "ffmpeg")
    {:level :info :check-id ::ffmpeg :message "ffmpeg: installed"}
    {:level :warn
     :check-id ::ffmpeg
     :message "ffmpeg: missing; voice input cannot convert .oga/.opus to WAV for ASR."
     :remediation "Install ffmpeg and ensure it is on PATH for the Vis process."}))

(defn- model-message
  "One doctor line for a model that has to be ON DISK. A check that THROWS warns
   too: a diagnostic that dies is the one nobody can act on."
  [check-id label is-ready remediation]
  (try
    (if (is-ready (model-status))
      {:level :info :check-id check-id :message (str label ": installed")}
      {:level :warn :check-id check-id :message (str label ": missing") :remediation remediation})
    (catch Throwable t
      {:level :warn
       :check-id check-id
       :message (str label ": check failed: " (or (ex-message t) t))
       :remediation
       "Run `vis-agent extension voice models status` for detailed voice model diagnostics."})))

(defn- parakeet-message
  []
  (model-message
    ::parakeet
    "Parakeet ASR model"
    #(:installed? (:parakeet %))
    "Run `vis-agent extension voice models download --parakeet` or set VIS_PARAKEET_MODEL_DIR."))

(defn- speech-message
  "Piper only, because it is the family Vis installs by itself. pocket-tts
   staying absent is the DESIGNED state - see its manifest entry - so warning
   about it would only teach the user to ignore doctor."
  []
  (model-message ::speech
                 "Piper speech voice"
                 #(= :ready (:state (:piper (:speech %))))
                 "Run `vis-agent extension voice models download --piper`."))

(defn- espeak-message
  "A machine without espeak-ng gets NO Piper voice, however many are installed,
   so this is a check of its own rather than a footnote on the voice line."
  []
  (model-message ::espeak
                 "espeak-ng phoneme data"
                 #(:is-installed (:espeak %))
                 (try (tts-call! 'espeak-install-hint)
                      (catch Throwable _ "Install espeak-ng from your package manager."))))

(defn doctor-fn
  [_environment]
  [(voice-runtime-message) (ffmpeg-message) (parakeet-message) (espeak-message) (speech-message)])

(defn- status-word
  [{:keys [is-installed is-opt-in]}]
  (cond is-installed "installed"
        is-opt-in "opt-in, not installed"
        :else "missing"))

(defn- print-status!
  []
  (let [{:keys [parakeet speech assets]} (model-status)]
    (cli-out! (str "Parakeet ASR model: " (if (:installed? parakeet) "installed" "missing")))
    (doseq [family [:piper :pocket-tts]]
      (cli-out! (str "Speech (" (name family) "): " (name (:state (get speech family))))))
    (cli-out! "")
    (doseq [asset assets]
      (cli-out! (format "  %-30s %-22s %s" (:id asset) (status-word asset) (:license asset))))))


(defn- voice-toggle-recording!
  "Slash run-fn body for `/voice`. Resolves the input ns lazily so the
   host doesn't pay the audio stack cost until the user actually
   triggers voice."
  [ctx]
  (let
    [toggle (or (requiring-resolve 'com.blockether.vis.ext.foundation-voice.input/toggle-recording!)
                (throw (ex-info "Voice input namespace did not expose toggle-recording!"
                                {:type :voice-input/missing-toggle})))]
    (toggle ctx)
    {:slash/status :ok :slash/title "Voice recording toggled"}))

(defn- voice-models-status-command [_parsed _residual] (vis/init-cli!) (print-status!))

(defn- voice-models-licenses-command
  "What Vis puts on your machine and under what terms, without reading the
   manifest: the answer to \"may I ship this?\". `--markdown` prints
   `THIRD_PARTY_MODELS.md` itself - the same manifest, rendered."
  [parsed _residual]
  (vis/init-cli!)
  (if (get parsed "markdown")
    (cli-out! (str/trimr (attribution-call! 'markdown)))
    (doseq [asset (:assets (model-status))]
      (cli-out! (str (:id asset) "  [" (:license asset) "]"))
      (cli-out! (str "  " (:attribution asset)))
      (when (:notice asset) (cli-out! (str "  Note: " (:notice asset))))
      (cli-out! (str "  " (:dir asset)))
      (cli-out! ""))))

(defn- download-families
  "Which families the flags asked for. `--all` and a bare `download` both mean
   everything Vis fetches on its own; an opt-in model is only ever NAMED."
  [parsed]
  (let [named (filterv #(get parsed (name %)) [:parakeet :piper :pocket-tts])]
    (if (or (get parsed "all") (empty? named)) [:parakeet :piper] named)))

(defn- download-family!
  [family voice]
  (if (= :parakeet family)
    (do (cli-out! "Parakeet ASR model...") (cli-out! (str "  ready: " (asr-call! 'ensure-model!))))
    (do (cli-out! (str (name family) " speech model..."))
        (let [installed (tts-call! 'install-model! family voice nil)]
          (if (seq installed)
            (doseq [dir installed]
              (cli-out! (str "  installed: " dir)))
            (cli-out! "  already installed"))))))

(defn- voice-models-download-command
  [parsed _residual]
  (vis/init-cli!)
  (let [voice (get parsed "voice")]
    (doseq [family (download-families parsed)]
      (download-family! family voice)))
  (print-status!))

(defn- voice-line
  [voice]
  (str "  "
       (:id voice)
       (when-let [language (:language voice)]
         (str " [" language "]"))
       (when (:is-imported voice) " (imported)")
       "  "
       (:label voice)))

(defn- voice-list-command
  "Every voice this machine can speak in, by family, and which of them came from
   a recording somebody imported rather than one that shipped."
  [_parsed _residual]
  (vis/init-cli!)
  (doseq
    [[family voices] [[:piper (tts-call! 'piper-voices)] [:pocket-tts (tts-call! 'pocket-voices)]]]
    (cli-out! (name family))
    (if (seq voices)
      (doseq [voice voices]
        (cli-out! (voice-line voice)))
      (cli-out! "  (none)"))
    (cli-out! "")))

(defn- voice-import-command
  "A pocket voice IS a reference clip, so importing a recording is the whole of
   \"add a voice\" - no training, no account, nothing leaves the machine."
  [parsed residual]
  (vis/init-cli!)
  (let [path (or (get parsed "file") (first residual))]
    (try
      (let
        [voice (voices-call! 'import!
                             {:path path
                              :voice-name (or (get parsed "name") path)
                              :language (get parsed "lang")
                              :text (get parsed "text")})]
        (cli-out!
          (str "imported " (:id voice) " (" (:seconds voice) "s at " (:sample-rate voice) " Hz)"))
        (cli-out! (str "  " (:clip voice)))
        (when-not (:clip-text voice)
          (cli-out! (str "  no transcript: --text \"<what the clip says>\" makes the clone"
                         " track the voice far more closely"))))
      (catch clojure.lang.ExceptionInfo e (cli-out! (ex-message e))))))

(defn- voice-forget-command
  [parsed residual]
  (vis/init-cli!)
  (let
    [named
     (or (get parsed "name") (first residual))

     id
     (voices-call! 'voice-id named)]

    (if (and id (voices-call! 'forget! id))
      (cli-out! (str "forgot " id))
      (cli-out! (str "no imported voice named " (pr-str named))))))

(defn- speech-failure!
  "Says WHY speaking or listening failed and what the machine has to do about it,
   then leaves a non-zero status behind - a check that fails quietly is worse than
   no check at all."
  [^Throwable e]
  (cli-out! (ex-message e))
  (when-let [remediation (:remediation (ex-data e))]
    (cli-out! (str "  " remediation)))
  (System/exit 1))

(defn- voice-say-command
  "Speak one line on THIS machine and say where the audio landed: the shortest
   whole-path proof that the speaking half works here - model, native runtime,
   voice and encoder, end to end."
  [parsed residual]
  (vis/init-cli!)
  (let
    [text
     (str/trim (str (or (get parsed "text") (str/join " " residual))))

     family
     (if (get parsed "pocket-tts") :pocket-tts :piper)]

    (try (let
           [spoken
            (tts-call! 'synthesize! family {:text text :voice-id (get parsed "voice")})

            ^java.io.File out
            (some-> (get parsed "out")
                    io/file)]

           (when out
             (io/copy (io/file (:audio-path spoken)) out)
             (io/delete-file (io/file (:audio-path spoken)) true))
           (cli-out! (str "spoke " (:duration-ms spoken) " ms at " (:sample-rate spoken) " Hz"))
           (cli-out! (str "  " (if out (.getAbsolutePath out) (:audio-path spoken)))))
         (catch Throwable e (speech-failure! e)))))

(defn- voice-transcribe-command
  "Read a recording back as text with the local ASR model - the listening half of
   the same proof, and the command to run when the microphone \"does nothing\"."
  [parsed residual]
  (vis/init-cli!)
  (let [path (or (get parsed "file") (first residual))]
    (try (cli-out! (str/trim (str (asr-call! 'transcribe-file! path))))
         (catch Throwable e (speech-failure! e)))))

(def voice-extension
  (vis/extension
    {:ext/name "foundation-voice"
     :ext/description
     "Native local voice: Parakeet ASR and Piper/pocket-tts speech through sherpa-onnx."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "voice"
     :ext/doctor-fn doctor-fn
     :ext/cli
     [{:cmd/name "voice"
       :cmd/doc "Voice extension commands."
       :cmd/usage "vis-agent extension voice <models>"
       :cmd/subcommands
       [{:cmd/name "models"
         :cmd/doc "Manage local voice models."
         :cmd/usage "vis-agent extension voice models <status|download|licenses>"
         :cmd/subcommands
         [{:cmd/name "status"
           :cmd/doc "Show which local voice models are installed."
           :cmd/usage "vis-agent extension voice models status"
           :cmd/run-fn #'voice-models-status-command}
          {:cmd/name "download"
           :cmd/doc "Download local voice models."
           :cmd/usage
           "vis-agent extension voice models download [--parakeet|--piper|--pocket-tts|--all] [--voice ID]"
           :cmd/args
           [{:name "parakeet"
             :kind :flag
             :type :boolean
             :doc "Download/check the Parakeet ASR model."}
            {:name "piper" :kind :flag :type :boolean :doc "Download/check the Piper speech voice."}
            {:name "pocket-tts"
             :kind :flag
             :type :boolean
             :doc "Download/check pocket-tts (opt-in; see `models licenses`)."}
            {:name "all"
             :kind :flag
             :type :boolean
             :doc "Download/check every model Vis fetches by itself."}
            {:name "voice"
             :kind :flag
             :type :string
             :doc "Which Piper voice, when the catalogue lists more than one."}]
           :cmd/run-fn #'voice-models-download-command}
          {:cmd/name "licenses"
           :cmd/doc "Show what each voice model is licensed under and who to credit."
           :cmd/usage "vis-agent extension voice models licenses [--markdown]"
           :cmd/args [{:name "markdown"
                       :kind :flag
                       :type :boolean
                       :doc "Print THIRD_PARTY_MODELS.md, this manifest rendered."}]
           :cmd/run-fn #'voice-models-licenses-command}]}
        {:cmd/name "voices"
         :cmd/doc "List the voices this machine can speak in."
         :cmd/usage "vis-agent extension voice voices"
         :cmd/run-fn #'voice-list-command}
        {:cmd/name "import"
         :cmd/doc "Turn a recording into a voice pocket-tts can speak in."
         :cmd/usage
         "vis-agent extension voice import <clip.wav> --name NAME [--lang en] [--text \"…\"]"
         :cmd/args
         [{:name "file"
           :kind :positional
           :type :string
           :required true
           :doc "The recording to learn the voice from (WAV; anything else needs ffmpeg)."}
          {:name "name" :kind :flag :type :string :doc "What to call it; the id is made from this."}
          {:name "lang"
           :kind :flag
           :type :string
           :doc "Language tag the clip speaks, e.g. en or en-GB."}
          {:name "text"
           :kind :flag
           :type :string
           :doc
           "What the clip says. Optional, and worth giving: the clone tracks the voice better."}]
         :cmd/run-fn #'voice-import-command}
        {:cmd/name "forget"
         :cmd/doc "Delete a voice imported with `voice import`."
         :cmd/usage "vis-agent extension voice forget <name>"
         :cmd/args [{:name "name"
                     :kind :positional
                     :type :string
                     :required true
                     :doc "The imported voice to delete."}]
         :cmd/run-fn #'voice-forget-command}
        {:cmd/name "say"
         :cmd/doc "Speak a line on this machine, to prove that it can."
         :cmd/usage
         "vis-agent extension voice say \"<text>\" [--voice ID] [--pocket-tts] [--out FILE.wav]"
         :cmd/args
         [{:name "text" :kind :positional :type :string :required true :doc "What to say."}
          {:name "voice" :kind :flag :type :string :doc "Which voice; see `voice voices`."}
          {:name "pocket-tts"
           :kind :flag
           :type :boolean
           :doc "Speak with pocket-tts instead of Piper."}
          {:name "out"
           :kind :flag
           :type :string
           :doc "Where to write the WAV. A temporary file otherwise."}]
         :cmd/run-fn #'voice-say-command}
        {:cmd/name "transcribe"
         :cmd/doc "Read a recording back as text with the local ASR model."
         :cmd/usage "vis-agent extension voice transcribe <clip.wav>"
         :cmd/args [{:name "file"
                     :kind :positional
                     :type :string
                     :required true
                     :doc "The WAV to read back."}]
         :cmd/run-fn #'voice-transcribe-command}]}]
     ;; Declarative slash registration: the TUI renders /voice via the
     ;; engine slash registry, toggling recording through
     ;; input/toggle-recording!.
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
