(ns com.blockether.vis.ext.channel-telegram.bot
  "Telegram frontend for vis - long-polling loop that hands each incoming
   message to the shared conversations API and ships the answer back.

   Each Telegram chat maps to a `:telegram` conversation via
   `vis/for-telegram-chat!` (find-or-create on chat-id). One process can
   serve many chats; svar serializes asks per-conversation via the
   conversation's lock in `com.blockether.vis.core`."
  (:require [babashka.process :as process]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.svar.core :as svar]
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

;; ─── renderer chokepoint ────────────────────────────────────────────────
;;
;; Single point where answer-IR (or legacy strings) becomes
;; Telegram-HTML. Registered on the channel as
;; `:channel/messages-renderer-fn`. Bot calls it before every
;; `tg/send-message!` so the API helper receives ready-to-ship HTML.

(defn render-for-telegram
  "Render canonical answer-IR (`[:ir & nodes]`) to Telegram-flavored
   HTML.

   STRICT input contract: IR only, mirroring
   `channel-tui.core/render-for-tui`. `nil` is accepted as the empty
   placeholder. Strings, Hiccup vectors, EDN, etc. are programmer
   bugs and throw — lift to IR upstream. Use the `text->ir` helper
   below to build IR from plain command strings.

   `opts` forwarded to the IR walker."
  ([ir] (render-for-telegram ir nil))
  ([ir opts]
   (cond
     (nil? ir) ""
     (not (and (vector? ir) (= :ir (first ir))))
     (throw (ex-info "render-for-telegram requires canonical [:ir ...] input; build IR upstream, do not pass raw text or Hiccup"
              {:got-type (some-> ir class .getName)
               :got-preview (let [s (pr-str ir)]
                              (subs s 0 (min 200 (count s))))}))
     :else (vis/render ir :html opts))))

(defn- text->ir
  "Lift a plain-text command-response string into canonical IR.
   Blank lines split paragraphs (`:p`); intra-paragraph newlines
   become hard breaks (`:br`). Used at the boundary where legacy
   `command-*` helpers return strings — we never want soft-coerce in
   `render-for-telegram`."
  [s]
  (if (or (nil? s) (= "" s))
    [:ir {}]
    (let [paragraphs (clojure.string/split (str s) #"\n\n+")
          para->ir   (fn [p]
                       (let [lines (clojure.string/split-lines p)
                             runs  (vec (mapcat (fn [l]
                                                  (if (= "" l)
                                                    [[:br {}]]
                                                    [[:span {} l] [:br {}]]))
                                          lines))
                             ;; drop trailing :br
                             runs  (if (and (seq runs) (= :br (first (peek runs))))
                                     (subvec runs 0 (dec (count runs)))
                                     runs)]
                         (into [:p {}] runs)))]
      (into [:ir {}] (mapv para->ir paragraphs)))))

(defn- send! [token chat-id ir & [opts]]
  ;; STRICT IR rendering chokepoint. String callers must wrap via
  ;; `text->ir` before invoking; passing a raw string here throws.
  (tg/send-message! token chat-id
    (render-for-telegram ir)
    opts))

(defn- send-plain! [token chat-id text & [opts]]
  ;; Escape hatch for short status / cancellation lines that should
  ;; ship as plain text without parse_mode.
  (tg/send-message! token chat-id text (assoc opts :plain? true)))

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
  (boolean (requiring-resolve 'com.blockether.vis.ext.voice.asr/transcribe-file!)))

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
  (let [asr-fn     (or (requiring-resolve 'com.blockether.vis.ext.voice.asr/transcribe-file!)
                     (throw (ex-info "Parakeet ASR model is not on the classpath" {})))
        rewrite-fn (requiring-resolve 'com.blockether.vis.ext.voice.rewrite/rewrite-transcript!)
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

;; ─── streaming live-bubble ───────────────────────────────────────────────
;;
;; One Telegram message per turn, edited in place as the LLM thinks +
;; runs forms. Driven by `vis/make-progress-tracker`'s on-update
;; callback. Throttle: 1200ms / 40-char delta / newline; idle 3s
;; resets the clock; HTTP 429 doubles next two intervals; `message is
;; not modified` swallowed.

(def ^:private throttle-min-ms 1200)
(def ^:private throttle-min-delta 40)
(def ^:private thinking-window-chars 3500)

(defn- now-ms ^long [] (System/currentTimeMillis))

(defn- cancel-keyboard [chat-id]
  {:inline_keyboard [[{:text "⊘ Cancel" :callback_data (str "cancel:" chat-id)}]]})

(defn- thinking-html
  "Wrap the sliding-window reasoning text in an expandable blockquote.
   Bot API ≥ 7.0 renders `<blockquote expandable>` as collapsible;
   older clients show plain blockquote (full text visible) — degraded
   but functional."
  [thinking]
  (when-not (str/blank? thinking)
    (let [escaped (-> thinking
                    (str/replace "&" "&amp;")
                    (str/replace "<" "&lt;")
                    (str/replace ">" "&gt;"))
          windowed (if (> (count escaped) thinking-window-chars)
                     (str "…" (subs escaped (- (count escaped) thinking-window-chars)))
                     escaped)]
      (str "<blockquote expandable>" windowed "</blockquote>"))))

(defn- live-bubble-html
  "Compose the full bubble HTML from current state. Always starts with
   the lit thinking lamp + status line so the user sees activity
   immediately."
  [{:keys [thinking-acc status-line]}]
  (let [parts (cond-> ["💭 <b>Thinking…</b>"]
                (seq thinking-acc) (conj (thinking-html thinking-acc))
                (seq status-line)  (conj (str "<i>" status-line "</i>")))]
    (str/join "\n\n" parts)))

(defn- chunk-display-code
  [chunk]
  (let [segments (:render-segments chunk)
        code     (if (seq segments)
                   (some (fn [{:keys [kind source]}]
                           (when (and (= :code kind) (not (str/blank? (str source))))
                             source))
                     segments)
                   (:code chunk))]
    (when-not (str/blank? (str code))
      (let [first-line (-> code str/split-lines first str/trim)]
        (when (seq first-line)
          first-line)))))

(defn- bubble-form-status-line
  [chunk]
  (str "⏳ Running form #" (inc (or (:form-idx chunk) 0))
    (when-let [first-line (chunk-display-code chunk)]
      (str " — " (subs first-line 0 (min 60 (count first-line)))))))

(defn- bubble-state-for [chat-id]
  (get-in @chat-state [chat-id :live-bubble]))

(defn- update-bubble-state! [chat-id f & args]
  (apply swap! chat-state update-in [chat-id :live-bubble] f args))

(defn- start-live-bubble!
  "Post the initial '💭 Thinking…' bubble with cancel keyboard. Returns
   `:ok` on success, `:unavailable` when Telegram refuses.

   Stores `{:message-id, :pending-html, :last-edit-ms, :keyboard?,
   :backoff-ms, :thinking-acc, :status-line}` under
   `[chat-id :live-bubble]` so update-bubble!  / cancel callback can
   find it."
  [token chat-id]
  (let [initial-html "💭 <b>Thinking…</b>"
        resp (try (tg/post-draft-message! token chat-id initial-html
                    {:reply-markup (cancel-keyboard chat-id)})
               (catch Exception _ nil))]
    (if-let [mid (some-> resp :result :message_id)]
      (do (update-bubble-state! chat-id (constantly
                                          {:message-id     mid
                                           :pending-html   initial-html
                                           :last-edit-ms   (now-ms)
                                           :keyboard?      true
                                           :backoff-ms     0
                                           :thinking-acc   ""
                                           :status-line    nil}))
        :ok)
      :unavailable)))

(defn- should-edit?
  "Throttle predicate. `flush?` bypasses interval / delta checks (used
   on stream end and when the user-visible state must update now,
   e.g. status-line swap)."
  [{:keys [last-edit-ms backoff-ms pending-html]} new-html flush?]
  (cond
    flush?                                            true
    (= new-html pending-html)                         false
    (< (- (now-ms) last-edit-ms) (max throttle-min-ms (or backoff-ms 0)))
    (boolean (or (str/ends-with? new-html "\n")
               (>= (- (count new-html) (count (or pending-html "")))
                 throttle-min-delta)))
    :else                                             true))

(defn- apply-edit-result!
  "Patch live-bubble state with the response from a tg/edit-message!
   call. Handles `not modified` (no-op) and 429 (back off)."
  [chat-id new-html resp]
  (cond
    (and resp (:ok resp))
    (update-bubble-state! chat-id
      assoc :pending-html new-html :last-edit-ms (now-ms) :backoff-ms 0)

    (and resp (= 400 (:error_code resp))
      (str/includes? (str (:description resp)) "is not modified"))
    (update-bubble-state! chat-id
      assoc :last-edit-ms (now-ms))

    (and resp (= 429 (:error_code resp)))
    (let [retry-after (-> resp :parameters :retry_after (or 1))]
      (update-bubble-state! chat-id
        assoc :backoff-ms (* 1000 (max retry-after 1)) :last-edit-ms (now-ms)))

    :else
    (update-bubble-state! chat-id assoc :last-edit-ms (now-ms))))

(defn- update-live-bubble!
  "Compose new bubble HTML from current state and edit if the throttle
   allows (or `:flush? true`). No-op when no bubble has been posted."
  [token chat-id & {:keys [flush?]}]
  (when-let [{:keys [message-id keyboard?] :as state} (bubble-state-for chat-id)]
    (let [new-html (live-bubble-html state)]
      (when (should-edit? state new-html flush?)
        (let [resp (try (tg/edit-message! token chat-id message-id new-html
                          (when keyboard?
                            {:reply-markup (cancel-keyboard chat-id)}))
                     (catch Exception _ nil))]
          (apply-edit-result! chat-id new-html resp))))))

(defn- finalize-live-bubble!
  "Stream-end edit: collapse thinking, drop the cancel keyboard. The
   actual answer text ships AFTER, as a separate message."
  [token chat-id outcome]
  (when-let [{:keys [message-id thinking-acc]} (bubble-state-for chat-id)]
    (let [glyph     (case outcome
                      :cancelled "⊘ <b>Cancelled by user.</b>"
                      :error     "⚠️ <b>Error during thinking.</b>"
                      "💭 <b>Thinking complete.</b>")
          html (str glyph
                 (when (seq thinking-acc)
                   (str "\n\n" (thinking-html thinking-acc))))]
      (try
        (tg/edit-message! token chat-id message-id html
          {:reply-markup {:inline_keyboard []}})
        (catch Exception _ nil)))
    (update-bubble-state! chat-id (constantly nil))))

(defn- on-tracker-update!
  "Progress-tracker `:on-update` handler. Reads the timeline, projects
   it into the bubble state, and calls update-live-bubble!."
  [token chat-id _timeline chunk]
  (when (bubble-state-for chat-id)
    (let [phase (:phase chunk)]
      (cond
        ;; Reasoning text deltas accumulate into thinking-acc.
        (= phase :reasoning)
        (when-let [delta (or (:thinking chunk) (:reasoning chunk))]
          (update-bubble-state! chat-id update :thinking-acc str delta)
          (update-live-bubble! token chat-id))

        (= phase :provider-fallback)
        (do
          (update-bubble-state! chat-id assoc :status-line
            (let [failed (:failed-provider chunk)
                  target (:new-provider chunk)
                  from   (str (or (some-> (:id failed) name) (some-> (:provider-id failed) name) "provider")
                           (when-let [m (:model failed)] (str "/" m)))
                  to     (when (or (:id target) (:provider-id target) (:model target))
                           (str (or (some-> (:id target) name) (some-> (:provider-id target) name) "provider")
                             (when-let [m (:model target)] (str "/" m))))
                  why    (or (:error failed) (some-> (:reason chunk) name) "fallback")]
              (str "↪ Fallback " from (when to (str " → " to)) " — " why)))
          (update-live-bubble! token chat-id :flush? true))

        ;; Form starts swap the status line; flush so the user sees the
        ;; new step within a tick.
        (= phase :form-start)
        (do
          (update-bubble-state! chat-id assoc :status-line
            (bubble-form-status-line chunk))
          (update-live-bubble! token chat-id :flush? true))

        (= phase :form-result)
        (do
          (update-bubble-state! chat-id assoc :status-line "⏳ Thinking…")
          (update-live-bubble! token chat-id))

        (and (= phase :iteration-final) (not (:done? chunk)))
        (do
          (update-bubble-state! chat-id assoc :status-line
            (str "⏳ Iteration " (inc (or (:iteration chunk) 0))))
          (update-live-bubble! token chat-id :flush? true))

        ;; Stream-end is handled by handle-user-text! finally clause;
        ;; we still flush thinking so the final bubble carries a clean
        ;; collapsed view.
        (and (= phase :iteration-final) (:done? chunk))
        (update-live-bubble! token chat-id :flush? true)

        :else nil))))

(defn- format-footer [result]
  ;; Canonical " / "-joined turn-summary line, decorated for Telegram.
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

(def ^:private model-catalog-cache
  ;; Cache of `{provider-id [model-id ...]}` from `svar/models!`. The
  ;; live `/models` endpoint is rate-limited and adds latency to every
  ;; `/models` Telegram command, so we hold the response for a short
  ;; window. Cleared automatically on `apply-config!` to pick up new
  ;; releases without a process restart.
  (atom {:fetched-at-ms 0 :ttl-ms 60000 :by-provider {}}))

(defn- fetch-live-models
  "Return a vec of model id strings for `provider` via `svar/models!`,
   or nil on failure / unknown provider.

   Routes through svar so OAuth headers (`anthropic-version`,
   `chatgpt-account-id`, ...) and per-provider model endpoints
   (`/codex/models?client_version=1.0.0`, etc.) come from the
   platform-agnostic `KNOWN_PROVIDERS` registry, not from telegram."
  [provider]
  (try
    (let [probe (cond-> provider
                  (empty? (:models provider))
                  (assoc :models [{:name "probe"}]))
          svar-provider (vis/->svar-provider probe)
          router        (svar/make-router [svar-provider])
          raw           (svar/models! router)]
      (->> raw
        (map (fn [m] (or (:id m) (:name m) (:slug m))))
        (filter string?)
        (filter #(vis/provider-model-visible? (:id provider) %))
        distinct
        vec))
    (catch Throwable _ nil)))

(defn- live-models-for [provider]
  (let [{:keys [fetched-at-ms ttl-ms by-provider]} @model-catalog-cache
        now (System/currentTimeMillis)]
    (if (and (< (- now fetched-at-ms) (long ttl-ms))
          (contains? by-provider (:id provider)))
      (get by-provider (:id provider))
      (let [fresh (or (fetch-live-models provider) [])]
        (swap! model-catalog-cache
          (fn [state]
            (cond-> state
              (>= (- now (:fetched-at-ms state)) (:ttl-ms state))
              (assoc :fetched-at-ms now :by-provider {})
              :always
              (assoc-in [:by-provider (:id provider)] fresh))))
        fresh))))

(defn- invalidate-model-catalog! []
  (swap! model-catalog-cache assoc :fetched-at-ms 0 :by-provider {}))

(defn- model-cycle-entries
  "Build the model-cycle list shown by `/models`.

   Configured models (in `config.edn`) come first, in their persisted
   order, so the user's pinned defaults stay stable. Live models from
   `svar/models!` follow, deduped against the configured set, so newly
   released models (Claude Opus 4.7, GPT-5.5, ...) auto-appear in the
   menu without anyone editing config. Live-only entries are flagged
   with `:live-only? true` and get persisted into the provider on
   selection (see `select-model-entry`)."
  [config]
  (->> (:providers config)
    (mapcat (fn [provider]
              (let [configured     (->> (:models provider)
                                     (keep #(model-entry provider %))
                                     vec)
                    configured-ids (into #{} (map :model) configured)
                    live-only      (->> (live-models-for provider)
                                     (remove configured-ids)
                                     (mapv (fn [model-id]
                                             (-> (model-entry provider {:name model-id})
                                               (assoc :live-only? true)))))]
                (concat configured live-only))))
    vec))

(defn- active-model-entry [config]
  (when-let [provider (first (:providers config))]
    (model-entry provider (first (:models provider)))))

(defn- model-entry-label [{:keys [provider-id model live-only?]}]
  (str (some-> provider-id name) "/" model (when live-only? " (live)")))

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
                (let [models (or (:models provider) [])
                      has?   (some #(= model (vis/model-name %)) models)
                      ;; Append a live-only model the user just
                      ;; picked so it persists; `move-to-front`
                      ;; below promotes it to active.
                      models (cond-> models
                               (not has?) (-> vec (conj {:name model})))]
                  (assoc provider :models
                    (move-to-front (fn [candidate]
                                     (= model (vis/model-name candidate)))
                      models)))
                provider))
        (move-to-front #(= provider-id (:id %)) providers)))))

(defn- apply-config! [config]
  (let [raw        (or (vis/load-config-raw) {})
        persistent (assoc raw :providers (vec (:providers config)))]
    (vis/save-config! persistent)
    (let [resolved (or (vis/reload-config!) persistent)
          router   (vis/rebuild-router! resolved)]
      (vis/refresh-cached-routers! router))
    ;; Catalog is provider-keyed; provider rotation may change which
    ;; provider is active. Drop the cache so the next `/models` call
    ;; reflects the freshly-applied router state.
    (invalidate-model-catalog!)
    persistent))

(defn- find-model-entry [entries selection]
  (let [selection (str/trim (or selection ""))]
    (or (when (re-matches #"\d+" selection)
          (nth entries (dec (Long/parseLong selection)) nil))
      (let [needle (str/lower-case selection)]
        (first
          (filter (fn [entry]
                    (let [;; Strip optional " (live)" tag so users
                          ;; can paste either form.
                          label-with-tag (str/lower-case (model-entry-label entry))
                          label-bare     (str/replace label-with-tag #"\s*\(live\)$" "")
                          model          (str/lower-case (:model entry))]
                      (or (= needle label-with-tag)
                        (= needle label-bare)
                        (= needle model))))
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
  (vis/text->ir
    (str "Current model: " (current-model-label)
      "\n\nUse /models to list and choose.")))

(defn- command-models [arg]
  (let [base-config (or (vis/load-config) {:providers []})
        entries     (model-cycle-entries base-config)
        active      (active-model-entry base-config)]
    (if (str/blank? (or arg ""))
      {:message (vis/text->ir
                  (if (seq entries)
                    (str "Models\nCurrent: " (or (some-> active model-entry-label) "unknown")
                      "\n\n" (str/join "\n" (model-list-lines entries active))
                      "\n\nTap a button, or send /models 2, or /models provider/model.")
                    "No models configured"))
       :reply-markup (when (seq entries) (model-inline-keyboard entries active))}
      {:message (vis/text->ir (:message (select-model! arg)))})))

(defn- model-label []
  (current-model-label))

(defn- command-help []
  (vis/text->ir
    (str "Vis Telegram commands:\n"
      "/help - show this help\n"
      "/status - show conversation, model, reasoning, verbosity\n"
      "/model - show current model\n"
      "/models - list models and choose with buttons\n"
      "/models <n|provider/model> - choose model\n"
      "/reasoning [quick|balanced|deep] - show/set reasoning effort, same as TUI Ctrl+R\n"
      "/verbosity [low|medium|high] - show/set OpenAI Codex verbosity, same as TUI Ctrl+L\n"
      (when (voice-extension?)
        (str "/voice [off|input|output|duplex|on] - configure voice input/output for this chat\n"
          "Voice messages - transcribe with the Parakeet ASR model, then send as text\n"))
      "/cancel - cancel current request\n"
      "/restart - restart the bot in a fresh Java process\n"
      "/export - export this conversation as Markdown")))

(defn- command-status [chat-id]
  (let [{:keys [id title]} (vis/for-telegram-chat! chat-id)
        settings (chat-settings chat-id)]
    (vis/text->ir
      (str "Conversation: " (subs (str id) 0 (min 8 (count (str id))))
        (when-not (str/blank? title) (str " - " title))
        "\nModel: " (model-label)
        "\nReasoning: " (name (:reasoning-level settings))
        "\nCodex verbosity: " (name (:openai-codex-verbosity settings))
        "\nVoice mode: " (name (:voice-mode settings))
        "\nIn flight: " (if (in-flight-token chat-id) "yes" "no")))))

(defn- command-reasoning [chat-id arg]
  (vis/text->ir
    (if (str/blank? (or arg ""))
      (str "Reasoning: " (name (:reasoning-level (chat-settings chat-id)))
        "\nUse /reasoning quick, /reasoning balanced, or /reasoning deep.")
      (let [level (normalize-reasoning-level arg)]
        (if-not (= (keyword (str/lower-case (str/trim arg))) level)
          (str "Unknown reasoning level: " arg
            "\nUse quick, balanced, or deep.")
          (do
            (set-chat-setting! chat-id :reasoning-level level)
            (str "Reasoning: " (name level))))))))

(defn- command-verbosity [chat-id arg]
  (vis/text->ir
    (if (str/blank? (or arg ""))
      (str "Codex verbosity: " (name (:openai-codex-verbosity (chat-settings chat-id)))
        "\nUse /verbosity low, /verbosity medium, or /verbosity high.")
      (let [verbosity (normalize-codex-verbosity arg)]
        (if-not (= (keyword (str/lower-case (str/trim arg))) verbosity)
          (str "Unknown verbosity: " arg
            "\nUse low, medium, or high.")
          (do
            (set-chat-setting! chat-id :openai-codex-verbosity verbosity)
            (str "Codex verbosity: " (name verbosity))))))))

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
    "Voice is not loaded. Install/load vis-voice, then restart Telegram."

    (not (some #{mode} voice-mode-order))
    "Unknown voice mode. Use off, input, output, duplex, or on."

    (and (contains? #{:input :duplex} mode) (not (voice-input-extension?)))
    "Voice input is unavailable: voice ASR is not loaded."

    (and (contains? #{:output :duplex} mode) (not (voice-output-extension?)))
    "Voice output is unavailable: voice TTS is not loaded."

    :else
    (do
      (set-chat-setting! chat-id :voice-mode mode)
      (str "Voice mode: " (name mode)))))

(defn- command-voice [chat-id arg]
  (if-not (voice-extension?)
    {:message (vis/text->ir "Voice is not loaded. Install/load vis-voice, then restart Telegram.")}
    (if (str/blank? (or arg ""))
      (let [active (:voice-mode (chat-settings chat-id))
            modes  (available-voice-modes)]
        {:message (vis/text->ir
                    (str "Voice modes\nCurrent: " (name active)
                      "\n\n" (str/join "\n" (voice-mode-lines active modes))
                      "\n\ninput = voice messages transcribe to text answers."
                      "\noutput = text prompts receive audio answers."
                      "\nduplex = voice in, audio out."
                      "\n\nTap a button, or send /voice duplex."
                      "\nAvailable: input=" (if (voice-input-extension?) "yes" "missing")
                      ", output=" (if (voice-output-extension?) "yes" "missing")))
         :reply-markup (voice-inline-keyboard active modes)})
      (let [raw  (keyword (str/lower-case (str/trim arg)))
            mode (normalize-voice-mode raw)]
        {:message (vis/text->ir
                    (if (and (not= raw mode)
                          (not (contains? #{:on :voice :audio} raw)))
                      (str "Unknown voice mode: " arg
                        "\nUse off, input, output, duplex, or on.")
                      (set-voice-mode! chat-id mode)))}))))

(defn- command-cancel [chat-id]
  (vis/text->ir
    (if-let [token (in-flight-token chat-id)]
      (do
        (vis/cancel! token)
        "Cancelling current request...")
      "No request is currently running.")))

(defn- command-restart []
  (schedule-self-restart!)
  (vis/text->ir "Restarting Telegram bot in a fresh Java process..."))

(defn- command-export [chat-id]
  (let [{:keys [id]} (vis/for-telegram-chat! chat-id)
        env      (vis/env-for id)
        markdown (when env (vis/conversation->markdown (:db-info env) id))]
    (vis/text->ir
      (if (seq markdown) markdown "No persisted turns to export yet."))))

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
            ;; Unknown command — build IR directly to keep the strict
            ;; contract.
            {:message (vis/text->ir (str "Unknown command: " cmd))})]
      (send! token chat-id message {:reply-markup reply-markup})
      true)))

(defn- answer-text
  "Render the turn's answer to a Telegram-HTML string. `(:answer
   result)` from `vis/send!` is canonical IR `[:ir & nodes]`; passed
   directly to the strict renderer chokepoint."
  [result]
  (render-for-telegram (:answer result)))

(defn- answer-voice-text
  "Plain-text projection of the answer for voice TTS. Strips structure
   so the synth doesn't read code blocks / tables aloud verbatim.
   Falls back to a generic line when the answer carries no [:p]
   content (e.g., a code-only answer)."
  [result]
  (let [answer (:answer result)
        spoken (when answer (vis/extract-text answer))]
    (if (str/blank? spoken)
      "The assistant returned a structured answer (code or data). See the chat for details."
      spoken)))

(defn- voice-config-flag
  [k default]
  (let [raw       (try (vis/load-config-raw) (catch Throwable _ nil))
        qualified (keyword "voice" (name k))
        voice-map (:voice raw)]
    (cond
      (contains? raw qualified) (boolean (get raw qualified))
      (and (map? voice-map) (contains? voice-map k)) (boolean (get voice-map k))
      :else default)))

(defn- markdown-quote
  [s]
  (str "> " (str/replace (str/trim (str s)) #"\n" "\n> ")))

(defn- transcript-message
  [transcript]
  (str "*Transcription*\n" (markdown-quote transcript)))

(defn- voice-output-mode? [settings]
  (contains? #{:output :duplex} (:voice-mode settings)))

(defn- synthesize-answer-wav! [answer]
  (let [synthesize-fn (or (requiring-resolve 'com.blockether.vis.ext.voice.core/synthesize-file!)
                        (throw (ex-info "Voice TTS is not on the classpath" {})))
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

(defn- handle-user-text!
  ([token chat-id text sender]
   (handle-user-text! token chat-id text sender nil))
  ([token chat-id text sender {:keys [transcript]}]
   (let [turn-token (vis/cancellation-token)
         settings   (chat-settings chat-id)
         voice-response? (voice-output-mode? settings)
         tracker    (vis/make-progress-tracker
                      {:on-update (fn [timeline chunk]
                                    (try (on-tracker-update! token chat-id timeline chunk)
                                      (catch Exception _ nil)))})
         opts       (cond-> {:cancel-atom (vis/cancellation-atom turn-token)
                             :hooks       {:on-chunk (:on-chunk tracker)}}
                      (turn-reasoning-default settings)
                      (assoc :reasoning-default (turn-reasoning-default settings))

                      (turn-extra-body settings)
                      (assoc :extra-body (turn-extra-body settings))

                      voice-response?
                      (assoc :turn/features {:voice-response? true}))]
     (set-in-flight! chat-id turn-token)
     (let [bubble-status (start-live-bubble! token chat-id)
           fut (future
                 (try
                   (when (= :unavailable bubble-status)
                     ;; Pre-first-paint failure path — keep the legacy
                     ;; typing indicator so the chat doesn't look frozen.
                     (tg/send-chat-action! token chat-id "typing"))
                   (let [{:keys [id]} (vis/for-telegram-chat! chat-id)
                         result       (vis/send! id text opts)]
                     (finalize-live-bubble! token chat-id :collapse)
                     (if voice-response?
                       (let [voice-text (answer-voice-text result)]
                         (when (and (voice-config-flag :telegram-send-transcript? true)
                                 (not (str/blank? (str transcript))))
                           (send! token chat-id (vis/text->ir (transcript-message transcript))))
                         (tg/send-chat-action! token chat-id "record_voice")
                         (send-answer-audio! token chat-id voice-text)
                         (when (voice-config-flag :telegram-send-answer-text? true)
                           ;; Full HTML answer ships alongside the voice
                           ;; note so users see code/tables that TTS
                           ;; deliberately skipped.
                           (tg/send-message! token chat-id (answer-text result))))
                       (do
                         (when (and transcript (not (str/blank? (str transcript))))
                           (send! token chat-id (vis/text->ir (transcript-message transcript))))
                         (tg/send-message! token chat-id
                           (str (answer-text result) (format-footer result))))))
                   (catch Exception e
                     (if (vis/cancellation? e)
                       (do
                         (finalize-live-bubble! token chat-id :cancelled)
                         (when (= :unavailable bubble-status)
                           ;; Pre-first-paint cancel — fresh message.
                           (try (send-plain! token chat-id "Cancelled by user.")
                             (catch Exception _ nil))))
                       (do
                         (finalize-live-bubble! token chat-id :error)
                         (tel/log! {:level :error :id ::handle-message
                                    :data {:sender sender :chat-id chat-id :error (ex-message e)}
                                    :msg (str "error handling msg from " sender " in chat " chat-id)})
                         (try (send! token chat-id
                                (vis/text->ir (vis/format-error (vis/db-error->user-message e))))
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
            (send-plain! token chat-id "Voice transcription was blank.")
            (handle-user-text! token chat-id text sender {:transcript text})))
        (catch Exception e
          (tel/log! {:level :error :id ::voice-asr-failed
                     :data {:sender sender :chat-id chat-id :error (ex-message e)}
                     :msg (str "voice ASR failed for chat " chat-id)})
          (try (send! token chat-id
                 (vis/text->ir (vis/format-error (str "Voice transcription failed: " (or (ex-message e) e)))))
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
        (send! token chat-id (vis/text->ir (unauthorized-message chat-id)))
        true)

      (and callback-id chat-id (string? data)
        (str/starts-with? data "model:"))
      (let [idx-str (subs data (count "model:"))
            result  (when (re-matches #"\d+" idx-str)
                      (select-model! (str (inc (Long/parseLong idx-str)))))]
        (tg/answer-callback-query! token callback-id (:message result))
        (send! token chat-id (vis/text->ir (or (:message result) "Model selection failed.")))
        true)

      (and callback-id chat-id (string? data)
        (str/starts-with? data "voice:"))
      (let [raw     (keyword (str/lower-case (subs data (count "voice:"))))
            message (if (some #{raw} voice-mode-order)
                      (set-voice-mode! chat-id raw)
                      "Voice selection failed.")]
        (tg/answer-callback-query! token callback-id message)
        true)

      (and callback-id chat-id (string? data)
        (str/starts-with? data "cancel:"))
      (let [in-flight (in-flight-token chat-id)]
        (if in-flight
          (do
            (tg/answer-callback-query! token callback-id "Cancelling…")
            (try (vis/cancel! in-flight) (catch Exception _ nil)))
          (tg/answer-callback-query! token callback-id "Already cancelled."))
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
          (do (send! token chat-id (vis/text->ir (unauthorized-message chat-id))) true)

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
   long-poll loop and blocks forever (until SIGTERM -> shutdown hook)."
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
     :ext/doc       "Telegram bot channel - long-poll loop wired into conversations."
     :ext/version   "0.3.0"
     :ext/author    "Blockether"
     :ext/owner     "vis"
     :ext/license   "Apache-2.0"
     :ext/channels  [{:channel/id      :telegram
                      :channel/cmd     "telegram"
                      :channel/doc     "Run as a Telegram bot (needs TELEGRAM_BOT_TOKEN)."
                      :channel/usage   "vis channels telegram [approve|restart]"
                      :channel/main-fn #'channel-main
                      :channel/messages-renderer-fn #'render-for-telegram
                      :channel/subcommands #'telegram-channel-subcommands}]}))
