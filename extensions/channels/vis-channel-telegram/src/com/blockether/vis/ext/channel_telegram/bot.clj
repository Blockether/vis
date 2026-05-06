(ns com.blockether.vis.ext.channel-telegram.bot
  "Telegram frontend for vis — long-polling loop that hands each incoming
   message to the shared conversations API and ships the answer back.

   Each Telegram chat maps to a `:telegram` conversation via
   `vis/for-telegram-chat!` (find-or-create on chat-id). One process can
   serve many chats; svar serializes asks per-conversation via the
   conversation's lock in `com.blockether.vis.core`."
  (:require [babashka.process :as process]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-telegram.api :as tg]
            [taoensso.telemere :as tel])
  (:import
   (java.lang ProcessBuilder$Redirect ProcessHandle)
   (java.lang.management ManagementFactory)))

(defonce ^:private running? (atom false))
(defonce ^:private poll-thread (atom nil))
(defonce ^:private restart-requested? (atom false))

(defonce ^:private chat-state
  ;; {chat-id {:settings {:reasoning-level :balanced
  ;;                      :openai-codex-verbosity :low}
  ;;           :model-cycle-order [...]
  ;;           :in-flight cancellation-token}}
  (atom {}))

(def ^:private poll-timeout-seconds 30)
(def ^:private telegram-restart-cmd-env "VIS_TELEGRAM_RESTART_CMD")
(def ^:private telegram-allowed-chat-ids-env "TELEGRAM_ALLOWED_CHAT_IDS")

(def ^:private reasoning-level-order [:quick :balanced :deep])
(def ^:private codex-verbosity-order [:low :medium :high])
(def ^:private voice-mode-order [:off :input :output :duplex])

(def ^:private default-chat-settings
  {:reasoning-level :balanced
   :openai-codex-verbosity :low
   :voice-mode :off})

(def ^:private bot-menu-commands-before-voice
  [{"command" "help"      "description" "Show Vis help"}
   {"command" "status"    "description" "Show conversation/model/status"}
   {"command" "model"     "description" "Show current model"}
   {"command" "models"    "description" "List and choose models"}
   {"command" "reasoning" "description" "Show or set reasoning effort"}
   {"command" "verbosity" "description" "Show or set Codex verbosity"}])

(def ^:private bot-menu-command-voice
  {"command" "voice" "description" "Show or set voice mode"})

(def ^:private bot-menu-commands-after-voice
  [{"command" "cancel"  "description" "Cancel current request"}
   {"command" "restart" "description" "Restart the bot in a fresh JVM"}
   {"command" "export"  "description" "Export conversation as Markdown"}])

(defn- voice-input-extension? []
  (boolean (requiring-resolve 'com.blockether.vis.ext.voice-parakeet.sherpa/transcribe-file!)))

(defn- voice-output-extension? []
  (boolean (requiring-resolve 'com.blockether.vis.ext.voice.core/synthesize-file!)))

(defn- voice-extension? []
  (or (voice-input-extension?) (voice-output-extension?)))

(defn- bot-menu-commands []
  (vec
    (concat bot-menu-commands-before-voice
      (when (voice-extension?) [bot-menu-command-voice])
      bot-menu-commands-after-voice)))

(defn- extract-text [message] (or (:text message) (:caption message)))

(defn- extract-sender [message]
  (or (some-> message :from :username not-empty)
    (some-> message :from :first_name)
    "user"))

(defn- extract-voice-file-id [message]
  (get-in message [:voice :file_id]))

(defn- split-csv [s]
  (when (seq (str/trim (or s "")))
    (->> (str/split s #",")
      (map str/trim)
      (remove str/blank?)
      vec)))

(defn- normalize-chat-id [chat-id]
  (when-not (nil? chat-id)
    (str (if (string? chat-id) (str/trim chat-id) chat-id))))

(defn- parse-allowed-chat-ids [v]
  (cond
    (nil? v) #{}
    (string? v) (set (split-csv v))
    (sequential? v) (set (keep normalize-chat-id v))
    :else #{}))

(defn- configured-allowed-chat-ids []
  (let [config-ids (parse-allowed-chat-ids
                     (get-in (or (vis/load-config-raw) {}) [:telegram :allowed-chat-ids]))
        env-ids    (parse-allowed-chat-ids (System/getenv telegram-allowed-chat-ids-env))]
    (set (concat config-ids env-ids))))

(defn- chat-approved? [chat-id]
  (let [allowed (configured-allowed-chat-ids)]
    (or (empty? allowed)
      (contains? allowed (normalize-chat-id chat-id)))))

(defn- unauthorized-message [chat-id]
  (str "Telegram chat is not approved: " chat-id "\n\n"
    "Approve it from a trusted shell:\n"
    "vis channels telegram approve --chat-id " chat-id "\n\n"
    "Then restart the bot with /restart or:\n"
    "vis channels telegram restart"))

(defn- approve-chat-id! [chat-id]
  (let [chat-id (normalize-chat-id chat-id)
        raw     (or (vis/load-config-raw) {})
        ids     (-> (parse-allowed-chat-ids (get-in raw [:telegram :allowed-chat-ids]))
                  (conj chat-id)
                  sort
                  vec)
        saved   (assoc-in raw [:telegram :allowed-chat-ids] ids)]
    (vis/save-config! saved)
    (vis/reload-config!)
    ids))

(defn- vis-home-dir []
  (let [dir (io/file (System/getProperty "user.home") ".vis")]
    (.mkdirs dir)
    dir))

(defn- telegram-pid-file []
  (io/file (vis-home-dir) "telegram.pid"))

(defn- telegram-log-file []
  (io/file (vis-home-dir) "telegram.log"))

(defn- current-pid []
  (.pid (ProcessHandle/current)))

(defn- read-pid-file []
  (try
    (some-> (telegram-pid-file) slurp str/trim parse-long)
    (catch Throwable _ nil)))

(defn- write-pid-file! []
  (spit (telegram-pid-file) (str (current-pid))))

(defn- delete-pid-file! []
  (try
    (let [file (telegram-pid-file)]
      (when (and (.exists file) (= (read-pid-file) (current-pid)))
        (.delete file)))
    (catch Throwable _ nil)))

(defn- java-bin []
  (str (System/getProperty "java.home") java.io.File/separator "bin"
    java.io.File/separator "java"))

(defn- default-restart-argv []
  (vec
    (concat [(java-bin)]
      (.getInputArguments (ManagementFactory/getRuntimeMXBean))
      ["-cp" (System/getProperty "java.class.path")
       "clojure.main" "-m" "com.blockether.vis.core"
       "channels" "telegram"])))

(defn- restart-process-builder []
  (if-let [cmd (not-empty (str/trim (or (System/getenv telegram-restart-cmd-env) "")))]
    (ProcessBuilder. ["sh" "-c" cmd])
    (ProcessBuilder. ^java.util.List (default-restart-argv))))

(defn- start-fresh-telegram-process! []
  (let [log-file (telegram-log-file)
        pb       (doto (restart-process-builder)
                   (.redirectInput ProcessBuilder$Redirect/INHERIT)
                   (.redirectOutput (ProcessBuilder$Redirect/appendTo log-file))
                   (.redirectError (ProcessBuilder$Redirect/appendTo log-file)))]
    (.start pb)))

(defn- destroy-telegram-pid! [pid]
  (when (and pid (not= pid (current-pid)))
    (let [handle (ProcessHandle/of (long pid))]
      (when (.isPresent handle)
        (.destroy (.get handle))))))

(defn- stop-polling! []
  (when (compare-and-set! running? true false)
    (when-let [t @poll-thread]
      (.interrupt ^Thread t)
      (reset! poll-thread nil))
    true))

(defn- schedule-self-restart! []
  (when (compare-and-set! restart-requested? false true)
    (future
      (Thread/sleep 500)
      (try
        (start-fresh-telegram-process!)
        (catch Throwable t
          (reset! restart-requested? false)
          (tel/log! {:level :error :id ::restart-failed
                     :data {:error (ex-message t)}
                     :msg "Telegram restart failed"})
          (throw t)))
      (stop-polling!)
      (vis/close-all!)
      (shutdown-agents)
      (System/exit 0))))

(defn- executable? [cmd]
  (try
    (zero? (:exit (process/sh {:out :string :err :string :continue true}
                    "command" "-v" cmd)))
    (catch Throwable _ false)))

(defn- audio-extension [audio-file]
  (let [name (.getName (io/file audio-file))]
    (when-let [idx (str/last-index-of name ".")]
      (str/lower-case (subs name idx)))))

(defn- wav-audio? [audio-file]
  (contains? #{".wav" ".wave"} (audio-extension audio-file)))

(defn- ffmpeg-audio->wav! [audio-file wav-file]
  (when-not (executable? "ffmpeg")
    (throw (ex-info "ffmpeg is required to convert Telegram voice audio for transcription"
             {:type :telegram-voice/missing-ffmpeg
              :source-extension (audio-extension audio-file)})))
  (let [{:keys [exit err]} (process/sh {:out :string :err :string :continue true}
                             "ffmpeg" "-y" "-i" (str audio-file)
                             "-ac" "1" "-ar" "16000" "-f" "wav" (str wav-file))]
    (when-not (zero? exit)
      (throw (ex-info "ffmpeg failed to convert Telegram voice audio for transcription"
               {:type :telegram-voice/ffmpeg-failed
                :source-extension (audio-extension audio-file)
                :exit-code exit
                :stderr (some-> err str/trim not-empty)})))
    wav-file))

(defn- asr-ready-audio-file! [audio-file]
  (if (wav-audio? audio-file)
    {:file (io/file audio-file) :delete? false}
    (let [wav (java.io.File/createTempFile "vis-telegram-voice-asr-" ".wav")]
      (ffmpeg-audio->wav! audio-file wav)
      {:file wav :delete? true})))

(defn- transcribe-audio-file! [audio-file]
  (let [asr-fn     (or (requiring-resolve 'com.blockether.vis.ext.voice-parakeet.sherpa/transcribe-file!)
                     (throw (ex-info "Parakeet ASR extension is not on the classpath" {})))
        rewrite-fn (requiring-resolve 'com.blockether.vis.ext.voice-parakeet.rewrite/rewrite-transcript!)
        {:keys [file delete?]} (asr-ready-audio-file! audio-file)]
    (try
      (let [raw       (asr-fn file)
            rewritten (when rewrite-fn (rewrite-fn raw))]
        (if (str/blank? rewritten) raw rewritten))
      (finally
        (when delete?
          (try (.delete ^java.io.File file) (catch Throwable _)))))))

(defn- download-and-transcribe-voice! [token file-id]
  (let [{:keys [file_path]} (tg/get-file token file-id)
        suffix (if (and file_path (str/includes? file_path "."))
                 (subs file_path (str/last-index-of file_path "."))
                 ".ogg")
        tmp    (java.io.File/createTempFile "vis-telegram-voice-" suffix)]
    (try
      (tg/download-file! token file_path tmp)
      (transcribe-audio-file! tmp)
      (finally
        (.delete tmp)))))

(defn- format-footer [result]
  ;; Canonical " · "-joined turn-summary line, decorated for Telegram.
  ;; Keep this RAW Markdown-ish text. `tg/send-message!` owns escaping.
  ;; Pre-escaping here made MarkdownV2 fallback leak literal backslashes
  ;; into chats when an answer body failed Telegram parsing.
  (let [line (vis/format-meta-line result)]
    (when (seq line)
      (str "\n\n_🤖 " line "_"))))

(defn- normalize-choice [allowed aliases default v]
  (let [k (cond
            (keyword? v) v
            (string? v)  (keyword (str/lower-case (str/trim v)))
            :else        nil)
        k (get aliases k k)]
    (if (some #{k} allowed) k default)))

(defn- normalize-reasoning-level [v]
  (normalize-choice reasoning-level-order
    {:low :quick :medium :balanced :high :deep}
    :balanced
    v))

(defn- normalize-codex-verbosity [v]
  (normalize-choice codex-verbosity-order {} :low v))

(defn- normalize-voice-mode [v]
  (normalize-choice voice-mode-order
    {:on :duplex :voice :duplex :audio :duplex}
    :off
    v))

(defn- persisted-chat-settings [chat-id]
  (let [raw (try (vis/load-config-raw) (catch Throwable _ nil))
        m   (get-in raw [:telegram :chat-settings (normalize-chat-id chat-id)])]
    (when (map? m)
      (select-keys m (keys default-chat-settings)))))

(defn- persist-chat-settings! [chat-id settings]
  (try
    (let [chat-id (normalize-chat-id chat-id)
          raw     (or (vis/load-config-raw) {})]
      (vis/save-config!
        (assoc-in raw [:telegram :chat-settings chat-id]
          (select-keys settings (keys default-chat-settings)))
        :telegram)
      (vis/reload-config!))
    (catch Throwable t
      (tel/log! {:level :warn :id ::persist-chat-settings-failed
                 :data {:chat-id chat-id :error (ex-message t)}
                 :msg "Failed to persist Telegram chat settings"}))))

(defn- chat-settings [chat-id]
  (merge default-chat-settings
    (persisted-chat-settings chat-id)
    (get-in @chat-state [chat-id :settings])))

(defn- update-chat-settings! [chat-id f & args]
  (let [settings (:settings
                  (get
                    (swap! chat-state update chat-id
                      (fn [state]
                        (let [settings (apply f (chat-settings chat-id) args)]
                          (assoc (or state {}) :settings settings))))
                    chat-id))]
    (persist-chat-settings! chat-id settings)
    settings))

(defn- set-chat-setting! [chat-id k v]
  (update-chat-settings! chat-id assoc k v))

(defn- set-in-flight! [chat-id token]
  (swap! chat-state assoc-in [chat-id :in-flight] token)
  nil)

(defn- clear-in-flight! [chat-id token]
  (swap! chat-state update chat-id
    (fn [state]
      (if (= token (:in-flight state))
        (dissoc state :in-flight)
        state)))
  nil)

(defn- in-flight-token [chat-id]
  (get-in @chat-state [chat-id :in-flight]))

(defn- current-model-info []
  (when-let [router (try (vis/get-router) (catch Throwable _ nil))]
    (try (vis/resolve-effective-model router) (catch Throwable _ nil))))

(defn- current-provider-id []
  (:provider (current-model-info)))

(defn- reasoning-effort-configurable? []
  (let [info (current-model-info)]
    (or (nil? info)
      (and (boolean (:reasoning? info))
        (not= false (:reasoning-effort? info))
        (not= :zai-thinking (:reasoning-style info))))))

(defn- turn-extra-body [settings]
  (when (= :openai-codex (current-provider-id))
    {:text {:verbosity (name (or (:openai-codex-verbosity settings) :low))}}))

(defn- turn-reasoning-default [settings]
  (when (reasoning-effort-configurable?)
    (:reasoning-level settings)))

(defn- model-entry [provider model]
  (when-let [model-name (and model (vis/model-name model))]
    (when (:id provider)
      {:provider-id (:id provider)
       :model       model-name})))

(defn- model-cycle-entries [config]
  (->> (:providers config)
    (mapcat (fn [provider]
              (keep #(model-entry provider %) (:models provider))))
    vec))

(defn- active-model-entry [config]
  (when-let [provider (first (:providers config))]
    (model-entry provider (first (:models provider)))))

(defn- model-entry-label [{:keys [provider-id model]}]
  (str (some-> provider-id name) "/" model))

(defn- current-model-label []
  (if-let [entry (active-model-entry (or (vis/load-config) {:providers []}))]
    (model-entry-label entry)
    "unknown"))

(defn- move-to-front [pred coll]
  (let [items (vec (or coll []))
        idx   (first (keep-indexed (fn [idx item]
                                     (when (pred item) idx))
                       items))]
    (if (nil? idx)
      items
      (vec (concat [(nth items idx)]
             (subvec items 0 idx)
             (subvec items (inc idx)))))))

(defn- select-model-entry [config {:keys [provider-id model]}]
  (update config :providers
    (fn [providers]
      (mapv (fn [provider]
              (if (= provider-id (:id provider))
                (update provider :models
                  #(move-to-front (fn [candidate]
                                    (= model (vis/model-name candidate)))
                     %))
                provider))
        (move-to-front #(= provider-id (:id %)) providers)))))

(defn- apply-config! [config]
  (let [raw        (or (vis/load-config-raw) {})
        persistent (assoc raw :providers (vec (:providers config)))]
    (vis/save-config! persistent)
    (let [resolved (or (vis/reload-config!) persistent)
          router   (vis/rebuild-router! resolved)]
      (vis/refresh-cached-routers! router))
    persistent))

(defn- find-model-entry [entries selection]
  (let [selection (str/trim (or selection ""))]
    (or (when (re-matches #"\d+" selection)
          (nth entries (dec (Long/parseLong selection)) nil))
      (let [needle (str/lower-case selection)]
        (first
          (filter (fn [entry]
                    (let [label (str/lower-case (model-entry-label entry))
                          model (str/lower-case (:model entry))]
                      (or (= needle label) (= needle model))))
            entries))))))

(defn- select-model! [selection]
  (let [base-config (or (vis/load-config) {:providers []})
        entries     (model-cycle-entries base-config)]
    (cond
      (empty? entries)
      {:message "No models configured" :level :warn}

      (str/blank? (or selection ""))
      {:message "Missing model selection" :level :warn}

      :else
      (if-let [entry (find-model-entry entries selection)]
        (let [config' (select-model-entry base-config entry)]
          (apply-config! config')
          {:message (str "Model set: " (model-entry-label entry))
           :level :info
           :changed? true
           :entry entry})
        {:message (str "Unknown model: " selection) :level :warn}))))

(defn- model-list-lines [entries active]
  (map-indexed
    (fn [idx entry]
      (str (inc idx) ". " (when (= entry active) "✅ ")
        (model-entry-label entry)))
    entries))

(defn- model-inline-keyboard [entries active]
  {"inline_keyboard"
   (mapv (fn [[idx entry]]
           [{"text" (str (when (= entry active) "✅ ") (model-entry-label entry))
             "callback_data" (str "model:" idx)}])
     (map-indexed vector entries))})

(defn- command-model []
  (str "Current model: " (current-model-label)
    "\n\nUse /models to list and choose."))

(defn- command-models [arg]
  (let [base-config (or (vis/load-config) {:providers []})
        entries     (model-cycle-entries base-config)
        active      (active-model-entry base-config)]
    (if (str/blank? (or arg ""))
      {:message (if (seq entries)
                  (str "Models\nCurrent: " (or (some-> active model-entry-label) "unknown")
                    "\n\n" (str/join "\n" (model-list-lines entries active))
                    "\n\nTap a button, or send /models 2, or /models provider/model.")
                  "No models configured")
       :reply-markup (when (seq entries) (model-inline-keyboard entries active))}
      {:message (:message (select-model! arg))})))

(defn- model-label []
  (current-model-label))

(defn- command-help []
  (str "Vis Telegram commands:\n"
    "/help — show this help\n"
    "/status — show conversation, model, reasoning, verbosity\n"
    "/model — show current model\n"
    "/models — list models and choose with buttons\n"
    "/models <n|provider/model> — choose model\n"
    "/reasoning [quick|balanced|deep] — show/set reasoning effort, same as TUI Ctrl+R\n"
    "/verbosity [low|medium|high] — show/set OpenAI Codex verbosity, same as TUI Ctrl+L\n"
    (when (voice-extension?)
      (str "/voice [off|input|output|duplex|on] — configure voice input/output for this chat\n"
        "Voice messages — transcribe with the Parakeet ASR extension, then send as text\n"))
    "/cancel — cancel current request\n"
    "/restart — restart the bot in a fresh Java process\n"
    "/export — export this conversation as Markdown"))

(defn- command-status [chat-id]
  (let [{:keys [id title]} (vis/for-telegram-chat! chat-id)
        settings (chat-settings chat-id)]
    (str "Conversation: " (subs (str id) 0 (min 8 (count (str id))))
      (when-not (str/blank? title) (str " — " title))
      "\nModel: " (model-label)
      "\nReasoning: " (name (:reasoning-level settings))
      "\nCodex verbosity: " (name (:openai-codex-verbosity settings))
      "\nVoice mode: " (name (:voice-mode settings))
      "\nIn flight: " (if (in-flight-token chat-id) "yes" "no"))))

(defn- command-reasoning [chat-id arg]
  (if (str/blank? (or arg ""))
    (str "Reasoning: " (name (:reasoning-level (chat-settings chat-id)))
      "\nUse /reasoning quick, /reasoning balanced, or /reasoning deep.")
    (let [level (normalize-reasoning-level arg)]
      (if-not (= (keyword (str/lower-case (str/trim arg))) level)
        (str "Unknown reasoning level: " arg
          "\nUse quick, balanced, or deep.")
        (do
          (set-chat-setting! chat-id :reasoning-level level)
          (str "Reasoning: " (name level)))))))

(defn- command-verbosity [chat-id arg]
  (if (str/blank? (or arg ""))
    (str "Codex verbosity: " (name (:openai-codex-verbosity (chat-settings chat-id)))
      "\nUse /verbosity low, /verbosity medium, or /verbosity high.")
    (let [verbosity (normalize-codex-verbosity arg)]
      (if-not (= (keyword (str/lower-case (str/trim arg))) verbosity)
        (str "Unknown verbosity: " arg
          "\nUse low, medium, or high.")
        (do
          (set-chat-setting! chat-id :openai-codex-verbosity verbosity)
          (str "Codex verbosity: " (name verbosity)))))))

(defn- voice-mode-available? [mode]
  (case mode
    :off true
    :input (voice-input-extension?)
    :output (voice-output-extension?)
    :duplex (and (voice-input-extension?) (voice-output-extension?))
    false))

(defn- available-voice-modes []
  (filterv voice-mode-available? voice-mode-order))

(defn- voice-mode-lines [active modes]
  (map-indexed
    (fn [idx mode]
      (str (inc idx) ". " (when (= mode active) "✅ ") (name mode)))
    modes))

(defn- voice-inline-keyboard [active modes]
  {"inline_keyboard"
   (mapv (fn [mode]
           [{"text" (str (when (= mode active) "✅ ") (name mode))
             "callback_data" (str "voice:" (name mode))}])
     modes)})

(defn- set-voice-mode! [chat-id mode]
  (cond
    (not (voice-extension?))
    "Voice extensions are not loaded. Install/load vis-voice or vis-voice-parakeet, then restart Telegram."

    (not (some #{mode} voice-mode-order))
    "Unknown voice mode. Use off, input, output, duplex, or on."

    (and (contains? #{:input :duplex} mode) (not (voice-input-extension?)))
    "Voice input is unavailable: vis-voice-parakeet is not loaded."

    (and (contains? #{:output :duplex} mode) (not (voice-output-extension?)))
    "Voice output is unavailable: vis-voice is not loaded."

    :else
    (do
      (set-chat-setting! chat-id :voice-mode mode)
      (str "Voice mode: " (name mode)))))

(defn- command-voice [chat-id arg]
  (if-not (voice-extension?)
    {:message "Voice extensions are not loaded. Install/load vis-voice or vis-voice-parakeet, then restart Telegram."}
    (if (str/blank? (or arg ""))
      (let [active (:voice-mode (chat-settings chat-id))
            modes  (available-voice-modes)]
        {:message (str "Voice modes\nCurrent: " (name active)
                    "\n\n" (str/join "\n" (voice-mode-lines active modes))
                    "\n\ninput = voice messages transcribe to text answers."
                    "\noutput = text prompts receive audio answers."
                    "\nduplex = voice in, audio out."
                    "\n\nTap a button, or send /voice duplex."
                    "\nAvailable: input=" (if (voice-input-extension?) "yes" "missing")
                    ", output=" (if (voice-output-extension?) "yes" "missing"))
         :reply-markup (voice-inline-keyboard active modes)})
      (let [raw  (keyword (str/lower-case (str/trim arg)))
            mode (normalize-voice-mode raw)]
        {:message (if (and (not= raw mode)
                        (not (contains? #{:on :voice :audio} raw)))
                    (str "Unknown voice mode: " arg
                      "\nUse off, input, output, duplex, or on.")
                    (set-voice-mode! chat-id mode))}))))

(defn- command-cancel [chat-id]
  (if-let [token (in-flight-token chat-id)]
    (do
      (vis/cancel! token)
      "Cancelling current request…")
    "No request is currently running."))

(defn- command-restart []
  (schedule-self-restart!)
  "Restarting Telegram bot in a fresh Java process…")

(defn- command-export [chat-id]
  (let [{:keys [id]} (vis/for-telegram-chat! chat-id)
        env      (vis/env-for id)
        markdown (when env (vis/conversation->markdown (:db-info env) id))]
    (if (seq markdown)
      markdown
      "No persisted turns to export yet.")))

(defn- parse-command [text]
  (let [text (str/trim (or text ""))]
    (when (str/starts-with? text "/")
      (let [[raw & _] (str/split text #"\s+" 2)
            cmd       (-> raw
                        (str/replace #"@.*$" "")
                        str/lower-case)
            arg       (str/trim (subs text (min (count text) (count raw))))]
        {:cmd cmd :arg arg}))))

(defn- handle-command! [token chat-id text]
  (when-let [{:keys [cmd arg]} (parse-command text)]
    (let [{:keys [message reply-markup]}
          (case cmd
            "/start"     {:message (command-help)}
            "/help"      {:message (command-help)}
            "/status"    {:message (command-status chat-id)}
            "/model"     {:message (command-model)}
            "/models"    (command-models arg)
            "/reasoning" {:message (command-reasoning chat-id arg)}
            "/verbosity" {:message (command-verbosity chat-id arg)}
            "/voice"     (command-voice chat-id arg)
            "/cancel"    {:message (command-cancel chat-id)}
            "/restart"   {:message (command-restart)}
            "/export"    {:message (command-export chat-id)}
            {:message (str "Unknown command: " cmd "\n\n" (command-help))})]
      (tg/send-message! token chat-id message {:reply-markup reply-markup})
      true)))

(defn- answer-text [result]
  (let [answer (:answer result)]
    (cond
      (and (map? answer)
        (= :needs-input (:vis/answer-mode answer))
        (string? (:answer/text answer)))
      (:answer/text answer)
      (string? answer) answer
      :else (pr-str answer))))

(defn- voice-output-mode? [settings]
  (contains? #{:output :duplex} (:voice-mode settings)))

(defn- synthesize-answer-wav! [answer]
  (let [synthesize-fn (or (requiring-resolve 'com.blockether.vis.ext.voice.core/synthesize-file!)
                        (throw (ex-info "Piper TTS extension is not on the classpath" {})))
        wav (java.io.File/createTempFile "vis-telegram-answer-" ".wav")]
    (synthesize-fn answer {:out-file wav})
    wav))

(defn- wav->ogg-opus! [wav]
  (when (executable? "ffmpeg")
    (let [ogg  (java.io.File/createTempFile "vis-telegram-answer-" ".ogg")
          exit (:exit (process/sh {:out :string :err :string :continue true}
                        "ffmpeg" "-y" "-i" (str wav)
                        "-c:a" "libopus" "-b:a" "32k" (str ogg)))]
      (if (zero? exit)
        ogg
        (do (.delete ogg) nil)))))

(defn- send-answer-audio! [token chat-id answer]
  (let [wav (synthesize-answer-wav! answer)
        ogg (try (wav->ogg-opus! wav) (catch Throwable _ nil))]
    (try
      (if ogg
        (tg/send-voice! token chat-id ogg)
        (tg/send-audio! token chat-id wav))
      (finally
        (try (.delete wav) (catch Throwable _))
        (when ogg (try (.delete ogg) (catch Throwable _)))))))

(defn- transcript-answer-text [transcript answer]
  (if (str/blank? (str transcript))
    answer
    (str "> Transcribed text\n> " (str/replace (str/trim (str transcript)) #"\n" "\n> ")
      "\n\n" answer)))

(defn- handle-user-text!
  ([token chat-id text sender]
   (handle-user-text! token chat-id text sender nil))
  ([token chat-id text sender {:keys [transcript]}]
   (let [turn-token (vis/cancellation-token)
         settings   (chat-settings chat-id)
         voice-response? (voice-output-mode? settings)
         opts       (cond-> {:cancel-atom (vis/cancellation-atom turn-token)}
                      (turn-reasoning-default settings)
                      (assoc :reasoning-default (turn-reasoning-default settings))

                      (turn-extra-body settings)
                      (assoc :extra-body (turn-extra-body settings))

                      voice-response?
                      (assoc :turn/features {:voice-response? true}))]
     (set-in-flight! chat-id turn-token)
     (let [fut (future
                 (try
                   (tg/send-chat-action! token chat-id "typing")
                   (let [{:keys [id]} (vis/for-telegram-chat! chat-id)
                         result       (vis/send! id text opts)
                         answer       (answer-text result)]
                     (if voice-response?
                       (do
                         (tg/send-chat-action! token chat-id "record_voice")
                         (send-answer-audio! token chat-id answer)
                         (when-not (str/blank? (str transcript))
                           (tg/send-message! token chat-id
                             (str "> Transcribed text\n> "
                               (str/replace (str/trim (str transcript)) #"\n" "\n> ")))))
                       (tg/send-message! token chat-id
                         (str (transcript-answer-text transcript answer)
                           (format-footer result)))))
                   (catch Exception e
                     (if (vis/cancellation? e)
                       (try (tg/send-message! token chat-id "Cancelled by user.")
                         (catch Exception _ nil))
                       (do
                         (tel/log! {:level :error :id ::handle-message
                                    :data {:sender sender :chat-id chat-id :error (ex-message e)}
                                    :msg (str "error handling msg from " sender " in chat " chat-id)})
                         (try (tg/send-message! token chat-id
                                (vis/format-error (vis/db-error->user-message e)))
                           (catch Exception _ nil)))))
                   (finally
                     (clear-in-flight! chat-id turn-token))))]
       (vis/cancellation-set-future! turn-token fut)))))

(defn- handle-user-voice! [token chat-id message sender]
  (when-let [file-id (extract-voice-file-id message)]
    (future
      (try
        (tg/send-chat-action! token chat-id "typing")
        (let [text (download-and-transcribe-voice! token file-id)]
          (if (str/blank? text)
            (tg/send-message! token chat-id "Voice transcription was blank.")
            (handle-user-text! token chat-id text sender {:transcript text})))
        (catch Exception e
          (tel/log! {:level :error :id ::voice-asr-failed
                     :data {:sender sender :chat-id chat-id :error (ex-message e)}
                     :msg (str "voice ASR failed for chat " chat-id)})
          (try (tg/send-message! token chat-id
                 (vis/format-error (str "Voice transcription failed: " (or (ex-message e) e))))
            (catch Exception _ nil))))))
  true)

(defn- handle-callback-query! [token callback]
  (let [callback-id (:id callback)
        data        (:data callback)
        chat-id     (-> callback :message :chat :id)]
    (cond
      (and chat-id (not (chat-approved? chat-id)))
      (do
        (tg/answer-callback-query! token callback-id "Chat is not approved")
        (tg/send-message! token chat-id (unauthorized-message chat-id))
        true)

      (and callback-id chat-id (string? data)
        (str/starts-with? data "model:"))
      (let [idx-str (subs data (count "model:"))
            result  (when (re-matches #"\d+" idx-str)
                      (select-model! (str (inc (Long/parseLong idx-str)))))]
        (tg/answer-callback-query! token callback-id (:message result))
        (tg/send-message! token chat-id (or (:message result) "Model selection failed."))
        true)

      (and callback-id chat-id (string? data)
        (str/starts-with? data "voice:"))
      (let [raw     (keyword (str/lower-case (subs data (count "voice:"))))
            message (if (some #{raw} voice-mode-order)
                      (set-voice-mode! chat-id raw)
                      "Voice selection failed.")]
        (tg/answer-callback-query! token callback-id message)
        (tg/send-message! token chat-id message)
        true))))

(defn- handle-update! [token update]
  (or (when-let [callback (:callback_query update)]
        (handle-callback-query! token callback))
    (when-let [message (:message update)]
      (let [chat-id (-> message :chat :id)
            text    (extract-text message)
            sender  (extract-sender message)]
        (cond
          (nil? chat-id) nil

          (not (chat-approved? chat-id))
          (do (tg/send-message! token chat-id (unauthorized-message chat-id)) true)

          text
          (do
            (when-not (handle-command! token chat-id text)
              (handle-user-text! token chat-id text sender))
            true)

          (extract-voice-file-id message)
          (handle-user-voice! token chat-id message sender))))))

(defn- poll-loop! [token]
  (loop [offset 0]
    (if-not @running?
      :stopped
      (let [updates (try
                      (tg/get-updates token offset poll-timeout-seconds)
                      (catch InterruptedException _ ::interrupted)
                      (catch Exception e
                        (tel/log! {:level :error :id ::poll-error
                                   :data {:error (ex-message e)}
                                   :msg "poll error"})
                        (Thread/sleep 2000)
                        []))]
        (cond
          (= updates ::interrupted) :stopped
          (seq updates) (do (doseq [u updates] (handle-update! token u))
                          (recur (inc (apply max (map :update_id updates)))))
          :else         (recur offset))))))

(defn- resolve-token! []
  (or (not-empty (System/getenv "TELEGRAM_BOT_TOKEN"))
    (throw (ex-info (str "TELEGRAM_BOT_TOKEN env var is not set. "
                      "Create a bot via @BotFather on Telegram, then:\n"
                      "  export TELEGRAM_BOT_TOKEN=<your-token>")
             {}))))

(defn- install-bot-menu! [token]
  (try
    (tg/set-my-commands! token (bot-menu-commands))
    (tel/log! {:level :info :id ::bot-menu-installed}
      "Telegram bot command menu installed")
    (catch Exception e
      ;; Menu install is UX parity, not startup correctness. Keep the bot alive
      ;; and leave diagnostics in the log when Telegram rejects the command list
      ;; or the network is down during boot.
      (tel/log! {:level :warn :id ::bot-menu-install-failed
                 :data  {:error (ex-message e)}
                 :msg   "Telegram bot command menu install failed"}))))

(defn start! []
  (when (compare-and-set! running? false true)
    (let [token (resolve-token!)
          t     (Thread. ^Runnable #(poll-loop! token) "vis-channel-telegram-poll")]
      (write-pid-file!)
      (install-bot-menu! token)
      (.setDaemon t true)
      (.start t)
      (reset! poll-thread t)
      (tel/log! {:level :info :id ::started}
        "Telegram bot running"))))

(defn stop! []
  (when (stop-polling!)
    (delete-pid-file!)
    (vis/close-all!)
    (tel/log! {:level :info :id ::stopped}
      "Telegram bot stopped")))

(defn -main [& _]
  (.addShutdownHook (Runtime/getRuntime)
    (Thread. ^Runnable (fn []
                         (tel/log! {:level :info :id ::shutdown}
                           "Shutting down Telegram bot")
                         (stop!))
      "vis-channel-telegram-shutdown"))
  (start!)
  @(promise))

;;; ── Channel registration (auto-discovered via META-INF/vis-extension/vis.edn) ──

(defn- channel-main
  "Channel entry point. Boots the CLI runtime config, then starts the
   long-poll loop and blocks forever (until SIGTERM → shutdown hook)."
  [_args]
  (vis/init-cli!)
  (-main))

(defn- cli-out! [s]
  (.println ^java.io.PrintStream vis/original-stdout (str s)))

(defn- telegram-approve-command [{chat-id "chat-id" restart? "restart"} _residual]
  (vis/init-cli!)
  (let [ids (approve-chat-id! chat-id)]
    (cli-out! (str "Approved Telegram chat-id " (normalize-chat-id chat-id)))
    (cli-out! (str "Allowed chat ids: " (str/join ", " ids)))
    (when restart?
      (let [old-pid (read-pid-file)]
        (start-fresh-telegram-process!)
        (destroy-telegram-pid! old-pid)
        (cli-out! (str "Restarted Telegram bot in a fresh Java process. Log: " (.getAbsolutePath (telegram-log-file))))))))

(defn- telegram-restart-command [_parsed _residual]
  (vis/init-cli!)
  (let [old-pid (read-pid-file)]
    (start-fresh-telegram-process!)
    (destroy-telegram-pid! old-pid)
    (cli-out! (str "Restarted Telegram bot in a fresh Java process. Log: " (.getAbsolutePath (telegram-log-file))))))

(defn- telegram-channel-subcommands []
  [{:cmd/name   "approve"
    :cmd/doc    "Approve a Telegram chat id for the bot allow-list."
    :cmd/usage  "vis channels telegram approve --chat-id ID [--restart]"
    :cmd/args   [{:name "chat-id" :kind :flag :type :string :required true
                  :doc "Telegram chat id to approve."}
                 {:name "restart" :kind :flag :type :boolean
                  :doc "Restart the Telegram bot after saving the allow-list."}]
    :cmd/run-fn #'telegram-approve-command}
   {:cmd/name   "restart"
    :cmd/doc    "Start the Telegram bot in a fresh Java process and stop the pid-file process."
    :cmd/usage  "vis channels telegram restart"
    :cmd/run-fn #'telegram-restart-command}])

(vis/register-extension!
  (vis/extension
    {:ext/namespace 'com.blockether.vis.ext.channel-telegram.bot
     :ext/doc       "Telegram bot channel — long-poll loop wired into conversations."
     :ext/version   "0.3.0"
     :ext/author    "Blockether"
     :ext/owner     "vis"
     :ext/license   "Apache-2.0"
     :ext/channels  [{:channel/id      :telegram
                      :channel/cmd     "telegram"
                      :channel/doc     "Run as a Telegram bot (needs TELEGRAM_BOT_TOKEN)."
                      :channel/usage   "vis channels telegram [approve|restart]"
                      :channel/main-fn #'channel-main
                      :channel/subcommands #'telegram-channel-subcommands}]}))
