(ns com.blockether.vis.internal.speech.cli
  "Commands for the gateway-owned speech subsystem. This namespace never loads a
   model or Sherpa: every runtime operation crosses the canonical gateway client."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.gateway.client :as gateway-client]
            [com.blockether.vis.internal.speech.assets :as assets]
            [com.blockether.vis.internal.speech.attribution :as attribution])
  (:import [java.net URLEncoder]
           [java.nio.charset StandardCharsets]))

(defn- cli-out! [s] (.println ^java.io.PrintStream config/original-stdout (str s)))

(defn- enc [x] (URLEncoder/encode (str x) StandardCharsets/UTF_8))

(defn- response-text
  [body]
  (if (bytes? body) (String. ^bytes body StandardCharsets/UTF_8) (str body)))

(defn- response-json!
  ([method path] (response-json! method path {}))
  ([method path opts]
   (let [response
         (gateway-client/request! method path opts)

         body
         (or (wire/parse-json (response-text (:body response))) {})]

     (when (>= (long (:status response)) 400)
       (throw (ex-info (or (get body "error") (str "gateway HTTP " (:status response)))
                       {:http-status (:status response) :body body})))
     body)))

(def ^:private model-targets
  {:parakeet {:direction :transcribe :engine-id "parakeet-local" :label "Parakeet ASR model"}
   :piper {:direction :synthesize :engine-id "piper-local" :label "Piper speech voice"}
   :pocket-tts
   {:direction :synthesize :engine-id "pocket-tts-local" :label "pocket-tts speech voice"}})

(defn- model-path
  [{:keys [direction engine-id]}]
  (str "/v1/" (if (= :transcribe direction) "voice" "speech") "/model?engine=" (enc engine-id)))

(defn- model-state
  [target]
  (let [m (response-json! :get (model-path target))]
    {:state (keyword (or (get m "status") "unavailable"))
     :progress (get m "progress")
     :error (get m "error")}))

(defn model-status
  "Gateway-reported speech readiness. No model state is duplicated in this process."
  []
  (let [parakeet
        (model-state (:parakeet model-targets))

        piper
        (model-state (:piper model-targets))

        pocket
        (model-state (:pocket-tts model-targets))]

    {:parakeet (assoc parakeet :installed? (= :ready (:state parakeet)))
     :speech {:piper piper :pocket-tts pocket}}))

(defn- runtime-message
  []
  {:level :info :check-id ::runtime :message "Speech runtime: one built-in gateway subsystem"})

(defn- model-message
  [check-id family state]
  (let [{:keys [label]} (get model-targets family)]
    (if (= :ready (:state state))
      {:level :info :check-id check-id :message (str label ": ready in gateway")}
      {:level :warn
       :check-id check-id
       :message (str label
                     ": "
                     (or (some-> (:state state)
                                 name)
                         "gateway unavailable"))
       :remediation (str "Run `vis-agent speech models download --" (name family) "`.")})))

(defn doctor-fn
  [_environment]
  (let [status (try (model-status) (catch Throwable t {:error (or (ex-message t) (str t))}))]
    [(runtime-message) (model-message ::parakeet :parakeet (:parakeet status))
     (model-message ::speech :piper (get-in status [:speech :piper]))
     (model-message ::pocket-speech :pocket-tts (get-in status [:speech :pocket-tts]))]))

(defn- print-status!
  []
  (let [{:keys [parakeet speech]} (model-status)]
    (cli-out! (str "Parakeet ASR model: " (name (:state parakeet))))
    (doseq [family [:piper :pocket-tts]]
      (let [{:keys [state progress error]} (get speech family)]
        (cli-out!
          (str "Speech (" (name family) "): " (name state) (when progress (str " " progress "%"))))
        (when error (cli-out! (str "  " error)))))))

(defn- speech-models-status-command [_parsed _residual] (config/init-cli!) (print-status!))

(defn- speech-models-licenses-command
  [parsed _residual]
  (config/init-cli!)
  (if (get parsed "markdown")
    (cli-out! (str/trimr (attribution/markdown)))
    (doseq [asset (assets/manifest)]
      (cli-out! (str (:id asset) "  [" (:license asset) "]"))
      (cli-out! (str "  " (:attribution asset)))
      (when (:notice asset) (cli-out! (str "  Note: " (:notice asset))))
      (cli-out! ""))))

(defn- download-families
  [parsed]
  (let [named (filterv #(get parsed (name %)) [:parakeet :piper :pocket-tts])]
    (if (or (get parsed "all") (empty? named)) [:parakeet :piper :pocket-tts] named)))

(defn- download-family!
  [family voice-id]
  (let [{:keys [direction engine-id label]} (get model-targets family)]
    (cli-out! (str label "..."))
    (gateway-client/prepare-speech-model! direction
                                          {:engine-id engine-id
                                           :voice-id (when (= family :piper) voice-id)
                                           :on-progress (fn [m]
                                                          (when-let [progress (get m "progress")]
                                                            (cli-out! (str "  " progress "%"))))})
    (cli-out! "  ready")))

(defn- speech-models-download-command
  [parsed _residual]
  (config/init-cli!)
  (doseq [family (download-families parsed)]
    (download-family! family (get parsed "voice"))))

(defn- voices-for
  [engine-id]
  (get (response-json! :get (str "/v1/speech/voices?engine=" (enc engine-id))) "voices"))

(defn- voice-line
  [voice]
  (str "  "
       (get voice "id")
       (when-let [language (get voice "language")]
         (str " [" language "]"))
       (when (get voice "is_imported") " (imported)")
       "  "
       (get voice "label")))

(defn- voice-list-command
  [_parsed _residual]
  (config/init-cli!)
  (doseq [[family engine-id] [[:piper "piper-local"] [:pocket-tts "pocket-tts-local"]]]
    (cli-out! (name family))
    (let [voices (voices-for engine-id)]
      (if (seq voices)
        (doseq [voice voices]
          (cli-out! (voice-line voice)))
        (cli-out! "  (none)")))
    (cli-out! "")))

(defn- raw-upload!
  [path file]
  (with-open [in (io/input-stream (io/file file))]
    (response-json!
      :post
      path
      {:body in :raw-body? true :headers {"Content-Type" "audio/wav"} :timeout-ms 120000})))

(defn- voice-import-command
  [parsed residual]
  (config/init-cli!)
  (let [file
        (or (get parsed "file") (first residual))

        params
        (cond-> [(str "engine=" (enc "pocket-tts-local"))
                 (str "name=" (enc (or (get parsed "name") file)))]
          (get parsed "lang")
          (conj (str "lang=" (enc (get parsed "lang"))))

          (get parsed "text")
          (conj (str "text=" (enc (get parsed "text")))))

        voice
        (get (raw-upload! (str "/v1/speech/voices?" (str/join "&" params)) file) "voice")]

    (cli-out! (str "imported " (get voice "id")))))

(defn- voice-forget-command
  [parsed residual]
  (config/init-cli!)
  (let [id (or (get parsed "name") (first residual))]
    (response-json! :delete (str "/v1/speech/voices/" (enc id) "?engine=" (enc "pocket-tts-local")))
    (cli-out! (str "forgot " id))))

(defn- with-temporary-session
  [f]
  (let [session
        (gateway-client/create-session!
          {:channel "cli" :title "Speech diagnostic" :root (System/getProperty "user.dir")})

        sid
        (get session "id")]

    (try (f sid) (finally (gateway-client/request! :delete (str "/v1/sessions/" (enc sid)))))))

(defn- speech-say-command
  [parsed residual]
  (config/init-cli!)
  (let [text
        (str/trim (str (or (get parsed "text") (str/join " " residual))))

        engine-id
        (if (get parsed "pocket-tts") "pocket-tts-local" "piper-local")]

    (with-temporary-session
      (fn [sid]
        (let [audio
              (gateway-client/synthesize-speech! sid
                                                 text
                                                 {:engine-id engine-id
                                                  :voice-id (get parsed "voice")})

              out
              (some-> (get parsed "out")
                      io/file)]

          (if out
            (do (io/copy audio out)
                (io/delete-file audio true)
                (cli-out! (str "spoke to " (.getAbsolutePath ^java.io.File out))))
            (cli-out! (str "spoke to " (.getAbsolutePath ^java.io.File audio)))))))))

(defn- speech-transcribe-command
  [parsed residual]
  (config/init-cli!)
  (let [path (or (get parsed "file") (first residual))]
    (with-temporary-session #(cli-out! (str/trim
                                         (str (gateway-client/transcribe-audio! % path {})))))))

(def command
  {:cmd/name "speech"
   :cmd/doc "Manage the gateway's built-in local speech models and voices."
   :cmd/usage "vis-agent speech <models|voices|import|forget|say|transcribe>"
   :cmd/subcommands
   [{:cmd/name "models"
     :cmd/doc "Manage local speech models."
     :cmd/usage "vis-agent speech models <status|download|licenses>"
     :cmd/subcommands
     [{:cmd/name "status"
       :cmd/doc "Show gateway speech model readiness."
       :cmd/usage "vis-agent speech models status"
       :cmd/run-fn #'speech-models-status-command}
      {:cmd/name "download"
       :cmd/doc "Prepare gateway speech models."
       :cmd/usage
       "vis-agent speech models download [--parakeet|--piper|--pocket-tts|--all] [--voice ID]"
       :cmd/args [{:name "parakeet" :kind :flag :type :boolean :doc "Prepare Parakeet ASR."}
                  {:name "piper" :kind :flag :type :boolean :doc "Prepare Piper speech."}
                  {:name "pocket-tts" :kind :flag :type :boolean :doc "Prepare pocket-tts speech."}
                  {:name "all" :kind :flag :type :boolean :doc "Prepare every model."}
                  {:name "voice" :kind :flag :type :string :doc "Piper voice id."}]
       :cmd/run-fn #'speech-models-download-command}
      {:cmd/name "licenses"
       :cmd/doc "Show voice model licences and attribution."
       :cmd/usage "vis-agent speech models licenses [--markdown]"
       :cmd/args [{:name "markdown"
                   :kind :flag
                   :type :boolean
                   :doc "Print THIRD_PARTY_MODELS.md from the model manifest."}]
       :cmd/run-fn #'speech-models-licenses-command}]}
    {:cmd/name "voices"
     :cmd/doc "List voices in the gateway."
     :cmd/usage "vis-agent speech voices"
     :cmd/run-fn #'voice-list-command}
    {:cmd/name "import"
     :cmd/doc "Import a pocket-tts reference recording."
     :cmd/usage "vis-agent speech import <clip.wav> --name NAME [--lang en] [--text TEXT]"
     :cmd/args
     [{:name "file" :kind :positional :type :string :required true :doc "Recording to learn from."}
      {:name "name" :kind :flag :type :string :doc "Imported voice name."}
      {:name "lang" :kind :flag :type :string :doc "Language tag."}
      {:name "text" :kind :flag :type :string :doc "What the clip says."}]
     :cmd/run-fn #'voice-import-command}
    {:cmd/name "forget"
     :cmd/doc "Delete an imported voice."
     :cmd/usage "vis-agent speech forget <name>"
     :cmd/args [{:name "name" :kind :positional :type :string :required true :doc "Voice id."}]
     :cmd/run-fn #'voice-forget-command}
    {:cmd/name "say"
     :cmd/doc "Synthesize a line in the gateway."
     :cmd/usage "vis-agent speech say <text> [--voice ID] [--pocket-tts] [--out FILE.wav]"
     :cmd/args [{:name "text" :kind :positional :type :string :required true :doc "What to say."}
                {:name "voice" :kind :flag :type :string :doc "Voice id."}
                {:name "pocket-tts" :kind :flag :type :boolean :doc "Use pocket-tts."}
                {:name "out" :kind :flag :type :string :doc "Output WAV path."}]
     :cmd/run-fn #'speech-say-command}
    {:cmd/name "transcribe"
     :cmd/doc "Transcribe a WAV in the gateway."
     :cmd/usage "vis-agent speech transcribe <clip.wav>"
     :cmd/args
     [{:name "file" :kind :positional :type :string :required true :doc "WAV to transcribe."}]
     :cmd/run-fn #'speech-transcribe-command}]})
