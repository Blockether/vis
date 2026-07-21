(ns com.blockether.vis.ext.foundation-voice.core
  "Local voice input through sherpa-onnx Parakeet ASR."
  (:require [babashka.process :as process]
            [com.blockether.vis.core :as vis]))

(def parakeet-model-dir-env "VIS_PARAKEET_MODEL_DIR")

(defn- cli-out! [s] (.println ^java.io.PrintStream vis/original-stdout (str s)))

(defn- voice-asr-var
  [sym]
  (or (requiring-resolve (symbol "com.blockether.vis.ext.foundation-voice.asr" (name sym)))
      (throw (ex-info "Voice ASR namespace did not expose expected var"
                      {:type :voice-asr/missing-var :var sym}))))

(defn- voice-asr-call! [sym & args] (apply (voice-asr-var sym) args))

(defn- parakeet-status [] {:installed? (boolean (voice-asr-call! 'model-installed?))})

(defn model-status [] {:parakeet (parakeet-status)})

(defn- executable?
  [cmd]
  (try (zero? (long (:exit
                      (process/sh {:out :string :err :string :continue true} "command" "-v" cmd))))
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

(defn doctor-fn [_environment] [(voice-runtime-message) (ffmpeg-message) (parakeet-message)])

(defn- print-status!
  []
  (let [{:keys [parakeet]} (model-status)]
    (cli-out! (str "Parakeet: " (if (:installed? parakeet) "installed" "missing")))))

(defn- ensure-parakeet! [] (voice-asr-call! 'ensure-model!))

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

(defn- voice-models-download-command
  [_parsed _residual]
  (vis/init-cli!)
  (cli-out! "Downloading/checking Parakeet ASR model...")
  (cli-out! (str "Parakeet ready: " (ensure-parakeet!)))
  (print-status!))

(def voice-extension
  (vis/extension
    {:ext/name "foundation-voice"
     :ext/description "Native local voice input: Parakeet ASR through sherpa-onnx."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "voice"
     :ext/doctor-fn doctor-fn
     :ext/env
     [{:name parakeet-model-dir-env
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
           :cmd/doc "Show local Parakeet model status."
           :cmd/usage "vis extensions voice models status"
           :cmd/run-fn #'voice-models-status-command}
          {:cmd/name "download"
           :cmd/doc "Download the local Parakeet ASR model."
           :cmd/usage "vis extensions voice models download [--parakeet|--all]"
           :cmd/args
           [{:name "parakeet" :kind :flag :type :boolean :doc "Download/check Parakeet ASR model."}
            {:name "all" :kind :flag :type :boolean :doc "Download/check the voice model."}]
           :cmd/run-fn #'voice-models-download-command}]}]}]
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
