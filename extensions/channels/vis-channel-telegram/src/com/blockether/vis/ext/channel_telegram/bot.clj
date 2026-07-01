(ns com.blockether.vis.ext.channel-telegram.bot
  "Telegram frontend for vis - long-polling loop that hands each incoming
   message to the canonical in-process gateway and ships the answer back.

   Each Telegram chat maps to a `:telegram` gateway session by external chat id.
   One process can serve many chats; the gateway serializes asks per-session and
   emits the same event/turn machinery used by TUI, web, and transport clients."
  (:require [babashka.process :as process]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-telegram.api :as tg]
            [com.blockether.vis.internal.extension :as extension]
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
;; Single point where answer-IR becomes
;; Telegram-HTML. Registered on the channel as
;; `:channel/messages-renderer-fn`. Bot calls it before every
;; `tg/send-message!` so the API helper receives ready-to-ship HTML.

(defn render-for-telegram
  "Render canonical answer-IR (`[:ir & nodes]`) to Telegram-flavored
   HTML.

   STRICT input contract: IR only, mirroring
   `channel-tui.core/render-for-tui`. `nil` is accepted as the empty
   placeholder. Strings, Hiccup vectors, EDN, etc. are programmer
   bugs and throw — lift to IR upstream via `vis/markdown->ir`.

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
     :else (-> (vis/render ir :html opts)
             ;; Belt-and-suspenders: the block walkers fix the structural
             ;; spacing, but collapse any residual run of 3+ newlines to a
             ;; single blank line and trim the edges so stacked blocks read
             ;; cleanly and the footer attaches without a gap.
             (str/replace #"\n{3,}" "\n\n")
             str/trim))))

(defn- send! [token chat-id ir & [opts]]
  ;; STRICT IR rendering chokepoint. String callers must wrap via
  ;; `vis/markdown->ir` before invoking; passing a raw string here throws.
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

(defn- voice-input-extension? []
  (boolean (requiring-resolve 'com.blockether.vis.ext.foundation-voice.asr/transcribe-file!)))

(defn- voice-output-extension? []
  (boolean (requiring-resolve 'com.blockether.vis.ext.foundation-voice.core/synthesize-file!)))

(defn- voice-extension? []
  (or (voice-input-extension?) (voice-output-extension?)))

(defn- slash-available-for-telegram?
  "True when `slash-spec` is allowed to surface on the Telegram bot.
   Uses `:slash/availability-fn` when present (synthesising a
   `:channel/id :telegram` ctx) and falls back to allowing any spec
   without an availability gate."
  [slash-spec]
  (if-let [f (:slash/availability-fn slash-spec)]
    (try (boolean (f {:channel/id :telegram})) (catch Throwable _ false))
    true))

(defn- bot-menu-commands
  "Build the Telegram bot's `setMyCommands` payload from the engine
   slash registry. Only top-level slashes
   surface; Telegram's bot menu cannot render nested commands.
   Filtered by `:slash/availability-fn` so TUI-only entries don't
   leak into the Telegram menu; specs marked `:slash/hidden? true`
   (e.g. `/start` alias) are dispatched normally but excluded from
   the menu. The /voice entry is filtered out entirely when no
   voice extension is loaded (matches legacy bot-menu behaviour
   pre-step-9)."
  []
  (let [voice? (voice-extension?)]
    (vec
      (for [spec (vis/registered-slashes)
            :when (and (empty? (:slash/parent spec))
                    (not (:slash/hidden? spec))
                    (slash-available-for-telegram? spec)
                    (or voice? (not= "voice" (:slash/name spec))))]
        {"command"     (:slash/name spec)
         "description" (or (:slash/doc spec) (str "/" (:slash/name spec)))}))))

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
  (let [asr-fn (or (requiring-resolve 'com.blockether.vis.ext.foundation-voice.asr/transcribe-file!)
                 (throw (ex-info "Parakeet ASR model is not on the classpath" {})))
        {:keys [file delete?]} (asr-ready-audio-file! audio-file)]
    (try
      (asr-fn file)
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
;; Reasoning window. Kept well under Telegram's 4096 hard limit so the
;; bubble (reasoning + the code-block feed + chrome) can NEVER exceed it —
;; once an edit is too long, editMessageText 400s and the bubble freezes
;; mid-turn (never reaching the answer). Left room for the per-form code
;; blocks below.
(def ^:private thinking-window-chars 2200)
;; The live feed shows the last few forms as code blocks, each badged with
;; its run status. Each is a multi-line <pre> block; per-step code is
;; clamped (lines, then chars) so a stack of blocks stays comfortably
;; under the hard limit.
(def ^:private max-feed-steps 4)
(def ^:private step-code-max-lines 12)
(def ^:private step-code-max-chars 380)
(def ^:private telegram-msg-limit 4096)

(defn- esc-html
  "Escape the three HTML-significant chars for Telegram HTML parse mode."
  [s]
  (-> (str s)
    (str/replace "&" "&amp;")
    (str/replace "<" "&lt;")
    (str/replace ">" "&gt;")))

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
    ;; Window the RAW text (so we never split an HTML entity), opening at
    ;; the next word boundary so it doesn't start mid-word ("…dd a close
    ;; icon"), THEN escape.
    (let [windowed (if (> (count thinking) thinking-window-chars)
                     (let [tail (subs thinking (- (count thinking) thinking-window-chars))
                           cut  (or (str/index-of tail " ") 0)]
                       (str "…" (str/triml (subs tail cut))))
                     thinking)]
      (str "<blockquote expandable>" (esc-html windowed) "</blockquote>"))))

(defn- clip-chars
  "Hard char cap with ellipsis (operates on raw text, before escaping)."
  [s n]
  (let [s (str s)]
    (if (> (count s) n) (str (subs s 0 n) "…") s)))

(defn- clamp-step-code
  "Bound a live-feed code block: cap lines first, then total chars, so a
   stack of step blocks can never push the bubble past Telegram's limit."
  [code]
  (let [lines   (str/split-lines (str code))
        clipped (if (> (count lines) step-code-max-lines)
                  (str (str/join "\n" (take step-code-max-lines lines)) "\n…")
                  (str code))]
    (clip-chars clipped step-code-max-chars)))

(defn- step-badge
  "Run-status badge for a live-feed step: ⏳ running, ✅ ok, ❌ failed.
   A real emoji for the running state so it sits flush with the ✅/❌
   status emoji instead of the thin monochrome ▸ glyph."
  [status]
  (case status
    :ok    "✅"
    :error "❌"
    "⏳"))

(defn- render-step
  "One live-feed entry: a status badge + status word + the form's code as
   a <pre> block, so the user reads the actual form AND sees at a glance
   whether it is running, succeeded, or failed. Falls back to a one-line
   label when the form carried no code to show."
  [{:keys [code label status]}]
  (let [badge (step-badge status)
        word  (case status :ok "ok" :error "failed" "running")]
    (if (str/blank? (str code))
      (str badge " <i>" word "</i> " (esc-html (or label "form")))
      (str badge " <i>" word "</i>\n<pre>" (esc-html (clamp-step-code code)) "</pre>"))))

(defn- live-bubble-html
  "Compose the full bubble HTML from current state: a live FEED of the
   forms the agent is running — each rendered as a code block badged with
   its run status (⏳ running, ✅ ok, ❌ failed) — the current status line,
   and the sliding reasoning window. The whole bubble is transient — at
   turn end it is replaced in place by the final answer, so none of this
   chrome survives."
  [{:keys [thinking-acc status-line steps]}]
  (let [step-lines (when (seq steps)
                     (str/join "\n\n"
                       (map render-step (take-last max-feed-steps steps))))
        chrome (cond-> ["💭 <b>Thinking…</b>"]
                 (seq step-lines)  (conj step-lines)
                 (seq status-line) (conj (str "<i>" (esc-html status-line) "</i>")))
        full   (str/join "\n\n"
                 (cond-> chrome
                   (seq thinking-acc) (conj (thinking-html thinking-acc))))]
    ;; Hard guard: an over-long edit 400s ("message is too long") and
    ;; FREEZES the live bubble mid-turn. Shed weight in order of least
    ;; value: first drop the reasoning blockquote (the code feed is the
    ;; more useful live signal); if a big code feed still overflows, keep
    ;; only the header + the most recent block.
    (cond
      (<= (count full) telegram-msg-limit) full
      (<= (count (str/join "\n\n" chrome)) telegram-msg-limit) (str/join "\n\n" chrome)
      :else (str/join "\n\n"
              (cond-> ["💭 <b>Thinking…</b>"]
                (seq steps) (conj (render-step (last steps))))))))

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

(defn- form-step-label
  "Short label for a live-feed step: the form's first code line
   (truncated), or `form #N` when there's no code to show."
  [chunk]
  (if-let [code (chunk-display-code chunk)]
    (subs code 0 (min 72 (count code)))
    (str "form #" (inc (or (:form-idx chunk) 0)))))

(defn- chunk-code-block
  "Full code string for a live-feed step BLOCK: the code render-segments
   joined (or the raw `:code`), trimmed. nil when there's nothing to show.
   Unlike `chunk-display-code` this keeps the whole multi-line form, not
   just its first line."
  [chunk]
  (let [segments (:render-segments chunk)
        code     (if (seq segments)
                   (->> segments
                     (filter #(= :code (:kind %)))
                     (map :source)
                     (remove #(str/blank? (str %)))
                     (str/join "\n\n")
                     not-empty)
                   (:code chunk))]
    (when-not (str/blank? (str code))
      (str/trim (str code)))))

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
        ;; Plain sendMessage (NOT sendMessageDraft): the draft method
        ;; requires a `random_id` we don't supply and 400s with
        ;; RANDOM_ID_INVALID on every call, which silently disabled the
        ;; whole live bubble (no stream at all). A regular message + in-
        ;; place editMessageText is the standard streaming pattern and the
        ;; one the rest of this namespace already edits/replaces.
        resp (try (tg/post-message! token chat-id initial-html
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
                                           :status-line    nil
                                           :steps          []}))
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

(defn- drop-live-bubble!
  "Delete the live streaming bubble message + clear its state. Used when
   the answer can't reuse the bubble in place (too long, edit failed, or
   a transcript must sit above the answer)."
  [token chat-id]
  (when-let [{:keys [message-id]} (bubble-state-for chat-id)]
    (try (tg/delete-message! token chat-id message-id) (catch Exception _ nil))
    (update-bubble-state! chat-id (constantly nil))))

(defn- replace-bubble-with-answer!
  "Turn the live streaming bubble INTO the final answer — one message
   that streamed the activity and then BECOMES the answer, so none of the
   intermediate 'what happened' chrome lingers. Edits the bubble in place
   when `answer-html` fits a single Telegram message; otherwise (too long,
   edit rejected, or no bubble) it drops the bubble and returns false so
   the caller sends the answer as a fresh (auto-split) message.

   Returns true when it delivered the answer, false when the caller must."
  [token chat-id answer-html]
  (boolean
    (when (and (bubble-state-for chat-id)
            (not (str/blank? answer-html))
            (<= (count answer-html) telegram-msg-limit))
      (let [{:keys [message-id]} (bubble-state-for chat-id)
            resp (try (tg/edit-message! token chat-id message-id answer-html
                        {:reply-markup {:inline_keyboard []}})
                   (catch Exception _ nil))]
        (if (and resp (:ok resp))
          (do (update-bubble-state! chat-id (constantly nil)) true)
          ;; edit rejected — remove the stale streaming bubble so it
          ;; doesn't linger, and let the caller send the answer fresh.
          (do (drop-live-bubble! token chat-id) false))))))

(defn- on-tracker-update!
  "Progress-tracker `:on-update` handler. Reads the timeline, projects
   it into the bubble state, and calls update-live-bubble!."
  [token chat-id _timeline chunk]
  (when (bubble-state-for chat-id)
    (let [phase (:phase chunk)]
      (cond
        ;; The tracker chunk carries the CUMULATIVE reasoning so far (the
        ;; progress reducer REPLACES the entry's :thinking with the chunk's),
        ;; NOT a delta — so REPLACE thinking-acc, don't append. Appending
        ;; would restack the whole reasoning block on every tick.
        (= phase :reasoning)
        (when-let [thinking (or (:thinking chunk) (:reasoning chunk))]
          (update-bubble-state! chat-id assoc :thinking-acc thinking)
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

        ;; Each form the agent runs appends a code block to the live feed
        ;; (`▸` running). Keyed by the form's :position so the result phase
        ;; can flip the RIGHT block even if a late/re-emitted chunk arrives
        ;; out of order. Flush so the new step shows within a tick.
        (= phase :form-start)
        (do
          (update-bubble-state! chat-id update :steps
            (fn [steps]
              (let [pos   (:position chunk)
                    steps (or steps [])]
                (if (some #(= pos (:position %)) steps)
                  steps
                  (conj steps {:position pos
                               :label    (form-step-label chunk)
                               :code     (chunk-code-block chunk)
                               :status   :running})))))
          (update-bubble-state! chat-id assoc :status-line nil)
          (update-live-bubble! token chat-id :flush? true))

        ;; Result lands → badge the matching block ✅ ok / ❌ failed based on
        ;; whether the form errored. Flush so the pass/fail badge shows now.
        (= phase :form-result)
        (do
          (update-bubble-state! chat-id update :steps
            (fn [steps]
              (let [pos    (:position chunk)
                    status (if (:error chunk) :error :ok)
                    steps  (or steps [])
                    idx    (first (keep-indexed
                                    (fn [i s] (when (= pos (:position s)) i))
                                    steps))]
                (if idx
                  (assoc-in steps [idx :status] status)
                  ;; No matching start block (e.g. its :form-start was
                  ;; suppressed) — append the completed block so the result
                  ;; still surfaces.
                  (conj steps {:position pos
                               :label    (form-step-label chunk)
                               :code     (chunk-code-block chunk)
                               :status   status})))))
          (update-live-bubble! token chat-id :flush? true))

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
  ;; The SHARED, humanized turn-summary line — identical to the CLI bracket and
  ;; the TUI bubble footer (model · in→out (cached) · ~$cost · duration, plus the
  ;; routing fallback note folded inline). The humanized formatter already shows
  ;; the cost total and a compact cached count, so there's nothing to pre-trim.
  ;; Decorated for Telegram: appended to the already-rendered HTML answer body
  ;; and shipped with parse_mode=HTML, so use <i>…</i> tags (Markdown `_…_`
  ;; renders as literal underscores in HTML mode) and escape the dynamic line so
  ;; stray &/</> never break parsing.
  (let [line (vis/format-meta-line result)]
    (when (seq line)
      (str "\n\n<i>🤖 " (esc-html line) "</i>"))))

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
          ;; Honor `:router` opts from `~/.vis/config.edn` so /models
          ;; probes use the same retry/network policy as real turns.
          router        (svar/make-router [svar-provider]
                          (vis/router-opts (vis/current-config)))
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

(defn- model-inline-keyboard [entries active]
  {"inline_keyboard"
   (mapv (fn [[idx entry]]
           [{"text" (str (when (= entry active) "✅ ") (model-entry-label entry))
             "callback_data" (str "model:" idx)}])
     (map-indexed vector entries))})

(defn- command-model []
  (vis/markdown->ir
    (str "Current model: " (current-model-label)
      "\n\nUse /models to list and choose.")))

(defn- command-models [arg]
  (let [base-config (or (vis/load-config) {:providers []})
        entries     (model-cycle-entries base-config)
        active      (active-model-entry base-config)]
    (if (str/blank? (or arg ""))
      ;; The inline keyboard already lists every model (with ✅ on the
      ;; active one), so the text body is just a header — enumerating the
      ;; models in text too is redundant noise on Telegram.
      {:message (vis/markdown->ir
                  (if (seq entries)
                    (str "Models — current: " (or (some-> active model-entry-label) "unknown")
                      "\nTap a button to switch, or send /models 2 or /models provider/model.")
                    "No models configured"))
       :reply-markup (when (seq entries) (model-inline-keyboard entries active))}
      {:message (vis/markdown->ir (:message (select-model! arg)))})))

(defn- model-label []
  (current-model-label))

(defn- command-help []
  (vis/markdown->ir
    (str "Vis Telegram commands:\n"
      "/help - show this help\n"
      "/status - show session, model, reasoning, verbosity\n"
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
      "/export - export this session as Markdown")))

(defn- codex-model-active?
  "True when the active model belongs to the OpenAI Codex provider.
   `verbosity` is a Codex-only knob (OpenAI Responses API), so the
   `/status` line and `/verbosity` chatter are noise for any other
   provider. The active model is the first model of the first provider
   (see `active-model-entry`), so we inspect that provider directly."
  []
  (let [provider (first (:providers (or (vis/load-config) {})))]
    (or (= :openai-codex (:id provider))
      (= :openai-compatible-responses (:api-style provider)))))

(defn- gateway-session-for-telegram-chat!
  [chat-id]
  (let [external-id (normalize-chat-id chat-id)]
    (or (some #(when (= external-id (:external_id %)) %)
          (vis/gateway-list-sessions :telegram))
      (vis/gateway-create-session! {:channel :telegram :external-id external-id}))))

(defn- command-status [chat-id]
  (let [{:keys [id title]} (gateway-session-for-telegram-chat! chat-id)
        settings (chat-settings chat-id)]
    (vis/markdown->ir
      (str "Session: " (subs (str id) 0 (min 8 (count (str id))))
        (when-not (str/blank? title) (str " - " title))
        "\nModel: " (model-label)
        "\nReasoning: " (name (:reasoning-level settings))
        (when (codex-model-active?)
          (str "\nCodex verbosity: " (name (:openai-codex-verbosity settings))))
        "\nVoice mode: " (name (:voice-mode settings))
        "\nIn flight: " (if (in-flight-token chat-id) "yes" "no")))))

(defn- command-reasoning [chat-id arg]
  (vis/markdown->ir
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
  (vis/markdown->ir
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
    "Voice is not loaded. Install/load vis-foundation-voice, then restart Telegram."

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
    {:message (vis/markdown->ir "Voice is not loaded. Install/load vis-foundation-voice, then restart Telegram.")}
    (if (str/blank? (or arg ""))
      (let [active (:voice-mode (chat-settings chat-id))
            modes  (available-voice-modes)]
        {:message (vis/markdown->ir
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
        {:message (vis/markdown->ir
                    (if (and (not= raw mode)
                          (not (contains? #{:on :voice :audio} raw)))
                      (str "Unknown voice mode: " arg
                        "\nUse off, input, output, duplex, or on.")
                      (set-voice-mode! chat-id mode)))}))))

(defn- command-cancel [chat-id]
  (vis/markdown->ir
    (if-let [token (in-flight-token chat-id)]
      (do
        (vis/cancel! token)
        "Cancelling current request...")
      "No request is currently running.")))

(defn- command-restart []
  (schedule-self-restart!)
  (vis/markdown->ir "Restarting Telegram bot in a fresh Java process..."))

(defn- command-export [chat-id]
  (let [{:keys [id]} (gateway-session-for-telegram-chat! chat-id)
        markdown (vis/session->markdown (vis/db-info) id)]
    (vis/markdown->ir
      (if (seq markdown) markdown "No persisted turns to export yet."))))

(defn- command-clear [chat-id]
  ;; Tear down the chat's gateway session tree (turns + soul + workspace
  ;; links) and immediately recreate a fresh empty Telegram gateway session.
  (let [{:keys [id]} (gateway-session-for-telegram-chat! chat-id)]
    (vis/gateway-close-session! id)
    (vis/gateway-create-session! {:channel :telegram
                                  :external-id (normalize-chat-id chat-id)})
    (vis/markdown->ir "🧹 Session cleared - starting a fresh conversation.")))

;; ----------------------------------------------------------------------------
;; Slash dispatch
;;
;; Every Telegram slash lives as a declarative entry on the
;; vis-channel-telegram extension's `:ext/slash-commands` (built
;; further down). Incoming text is tokenised by `vis/slash-parse`
;; and dispatched through the engine `slash/dispatch` infrastructure
;; against the global registry. Channel-private commands
;; (`/help`, `/status`, ...) ship with `:slash/availability-fn`
;; restricted to `:channel/id :telegram` so they don't bleed into
;; the TUI palette.
;;
;; Reply markups (Telegram inline keyboards) flow back via the
;; `:slash/data {:reply-markup ...}` slot on the envelope; the
;; dispatcher below extracts and threads them into `send!`.
;; ----------------------------------------------------------------------------

(defn- slash-env []
  ;; A throwaway env shape that mirrors what `prompt/active-extensions`
  ;; reads: an `:extensions` atom holding every registered extension.
  ;; Activation-fn filtering passes through every registered extension
  ;; here (channels surface slash UX before a per-turn env exists).
  {:extensions (atom (vec (extension/registered-extensions)))})

(defn- handle-slash!
  "Cross-channel slash dispatch entry. Returns `true` when the engine
   handled the text as a slash (with the answer / error sent to the
   chat); `false` when the text is not a slash and should flow to
   `handle-user-text!` for a normal LLM turn."
  [token chat-id text]
  (when (vis/slash-parse text)
    ;; Session id + db-info are stamped on ctx for the cross-channel,
    ;; session-scoped slashes (`/rename`, `/workspace …`) that read them as
    ;; plain values. Chat-id-only slashes (`/help`, `/reasoning`, `/cancel`,
    ;; `/model`, …) resolve everything they need from `:telegram/chat-id`
    ;; and never look at the session.
    ;;
    ;; Resolving the session / db-info must NOT be able to abort the whole
    ;; dispatch: if persistence is unavailable (or a local `~/.vis/vis.mdb`
    ;; carries a stale schema), a chat-id-only slash should still run — it
    ;; touches no DB. So the resolution is wrapped: on failure the ctx keys
    ;; are simply absent, and the session-scoped slashes degrade to their
    ;; own "session not ready" branch (while `/status`, `/export`, `/clear`
    ;; re-resolve inside their run-fns and surface the real error there).
    (let [db-info       (try (vis/db-info) (catch Throwable _ nil))
          session-id    (try (:id (gateway-session-for-telegram-chat! chat-id))
                          (catch Throwable _ nil))
          ctx           (cond-> {:channel/id       :telegram
                                 :telegram/chat-id chat-id
                                 :command/raw      text}
                          session-id (assoc :session/id session-id)
                          db-info    (assoc :db-info db-info))
          result        (vis/slash-dispatch (slash-env) ctx text)]
      (cond
        (not (:handled? result))
        false

        (:error result)
        (do (send! token chat-id
              (vis/markdown->ir (str "**Slash error** (" (name (or (:reason result) :error)) ")\n\n"
                                  (:error result))))
          true)

        :else
        (let [r            (:result result)
              raw-body     (:slash/body r)
              ;; Cross-channel slash run-fns (e.g. /dir) may return a raw
              ;; string body; render-for-telegram only accepts canonical
              ;; [:ir ...]. Coerce strings to IR so non-Telegram-native
              ;; slashes render here instead of throwing.
              body         (cond
                             (string? raw-body) (vis/markdown->ir raw-body)
                             (some? raw-body)   raw-body
                             :else              (vis/markdown->ir (or (:slash/title r) "Slash handled")))
              reply-markup (get-in r [:slash/data :reply-markup])]
          (send! token chat-id body {:reply-markup reply-markup})
          true)))))

(defn- answer->markdown-string
  "Extract the Markdown string from a turn-result `:answer` field.
   The Markdown-answer pipeline produces exactly two shapes:
     - `{:answer string}`                                    -- canonical final answer
     - `{:vis/answer-mode :needs-input :answer/text string}` -- needs-input gate"
  [answer]
  (cond
    (and (map? answer) (string? (:answer answer)))      (:answer answer)
    (and (map? answer) (string? (:answer/text answer))) (:answer/text answer)
    :else                                               ""))

;; ─── provider-error: compact chat render ────────────────────────────────
;;
;; The engine's `provider-error-ir` is a rich, TUI-shaped block (banner +
;; WHAT-HAPPENED/NEXT-STEP prose + a Wrapper/HTTP/Request-id list + the raw
;; provider JSON). Dumped verbatim it's a wall of noise in a chat bubble.
;; Telegram instead reads the structured `:vis/provider-error-data` the
;; engine stashes on the IR attrs and renders: one headline + one action
;; line, with the mechanics tucked into a native expandable blockquote.

(defn- provider-error-data
  "Structured provider-error info the engine stashes on the answer IR
   attrs, or nil when the answer is not a provider error."
  [answer]
  (when (and (vector? answer) (= :ir (first answer)) (map? (second answer)))
    (get (second answer) :vis/provider-error-data)))

(defn- provider-id-str [provider-id]
  (when provider-id
    (if (keyword? provider-id) (name provider-id) (str provider-id))))

(defn- provider-error-html
  "Compact Telegram render of a provider error: headline + one action
   line, with HTTP status / request-id / raw provider message folded into
   an expandable blockquote so the chat stays readable."
  [{:keys [kind status request-id provider-message provider-id]}]
  (let [prov   (some-> (provider-id-str provider-id) esc-html)
        suffix (when prov (str " — <code>" prov "</code>"))
        head   (case kind
                 :auth          (str "🔐 <b>Authentication failed</b>" suffix)
                 :invalid-thinking-signature (str "🧠 <b>Invalid thinking signature</b>" suffix)
                 (str "⚠️ <b>Provider error</b>" suffix))
        lead   (case kind
                 :auth          "The provider rejected Vis's credentials, so the model never ran."
                 :invalid-thinking-signature
                 "The provider rejected a replayed thinking block, so the model never ran."
                 (or (some-> provider-message esc-html)
                   "The provider rejected the request before the model ran."))
        action (case kind
                 :auth (str "→ Re-authenticate: <code>vis providers auth"
                         (when-let [p (provider-id-str provider-id)] (str " " (esc-html p)))
                         "</code>, then resend.")
                 :invalid-thinking-signature
                 "→ Just resend — Vis won't replay the stale thinking state."
                 "→ Resend; if it keeps failing, check this provider's config.")
        ;; Mechanics only — never the headline copy. For :generic the
        ;; provider message is already the lead, so don't repeat it.
        detail (cond-> []
                 status     (conj (str "HTTP " (esc-html (str status))))
                 request-id (conj (esc-html (str request-id)))
                 (and provider-message (not= kind :generic)) (conj (esc-html provider-message)))
        block  (when (seq detail)
                 (str "<blockquote expandable>" (str/join "\n" detail) "</blockquote>"))]
    (str/join "\n\n" (cond-> [head lead action] block (conj block)))))

(defn- answer-text
  "Render the turn's answer to a Telegram-HTML string. The model usually
   writes Markdown via `(done {:answer ...})` (a `{:answer string}` map), but
   the engine can hand back canonical IR DIRECTLY — e.g. a
   `#:vis{:provider-error true}` block when a provider call fails (401, etc.).
   Render IR as-is (otherwise it falls through to a blank string and the user
   sees only the footer); lift a markdown string to IR first."
  [result]
  (let [answer (:answer result)]
    (cond
      ;; Provider errors render compact + chat-native (expandable details),
      ;; not as the TUI-shaped wall of JSON/HTTP the shared IR carries.
      (provider-error-data answer)
      (provider-error-html (provider-error-data answer))

      (and (vector? answer) (= :ir (first answer)))
      (render-for-telegram answer)

      :else
      (let [md (answer->markdown-string answer)]
        (if (str/blank? md)
          ""
          (render-for-telegram (vis/markdown->ir md)))))))

(defn- answer-voice-text
  "Plain-text projection of the answer for voice TTS. Strips structure
   so the synth doesn't read code blocks / tables aloud verbatim.
   Falls back to a generic line when the answer carries no prose."
  [result]
  (let [md     (answer->markdown-string (:answer result))
        spoken (when-not (str/blank? md) (vis/extract-text (vis/markdown->ir md)))]
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
  (let [synthesize-fn (or (requiring-resolve 'com.blockether.vis.ext.foundation-voice.core/synthesize-file!)
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
         opts       (cond-> {:cancel-token turn-token
                             :hooks        {:on-chunk (:on-chunk tracker)}}
                      (turn-reasoning-default settings)
                      (assoc :reasoning-default (turn-reasoning-default settings))

                      (turn-extra-body settings)
                      (assoc :extra-body (turn-extra-body settings))

                      voice-response?
                      (assoc :turn-features {:voice-response? true}))]
     (set-in-flight! chat-id turn-token)
     (let [bubble-status (start-live-bubble! token chat-id)
           fut (future
                 (try
                   (when (= :unavailable bubble-status)
                     ;; Pre-first-paint failure path — keep the legacy
                     ;; typing indicator so the chat doesn't look frozen.
                     (tg/send-chat-action! token chat-id "typing"))
                   (let [{:keys [id]} (gateway-session-for-telegram-chat! chat-id)
                         result*      (vis/gateway-submit-turn-sync!
                                        id
                                        (-> opts
                                          (dissoc :hooks)
                                          (assoc :request text
                                            :engine-opts (select-keys opts [:hooks]))))
                         result       (cond-> result*
                                        (:answer-ir result*) (assoc :answer (:answer-ir result*)))]
                     (if voice-response?
                       (do
                         ;; Voice answer is audio, so the streaming bubble
                         ;; can't become it — collapse the bubble instead.
                         (finalize-live-bubble! token chat-id :collapse)
                         (let [voice-text (answer-voice-text result)]
                           (when (and (voice-config-flag :telegram-send-transcript? true)
                                   (not (str/blank? (str transcript))))
                             (send! token chat-id (vis/markdown->ir (transcript-message transcript))))
                           (tg/send-chat-action! token chat-id "record_voice")
                           (send-answer-audio! token chat-id voice-text)
                           (when (voice-config-flag :telegram-send-answer-text? true)
                             ;; Full HTML answer ships alongside the voice
                             ;; note so users see code/tables that TTS
                             ;; deliberately skipped.
                             (tg/send-message! token chat-id (answer-text result)))))
                       ;; TEXT answer: the streaming bubble BECOMES the answer
                       ;; (no separate "thinking complete" remnant).
                       (let [answer-html (str (answer-text result) (format-footer result))]
                         (if (and transcript (not (str/blank? (str transcript))))
                           ;; Transcript must sit ABOVE the answer, but the
                           ;; bubble was posted first — so drop it and send
                           ;; transcript + answer fresh, in order.
                           (do
                             (drop-live-bubble! token chat-id)
                             (send! token chat-id (vis/markdown->ir (transcript-message transcript)))
                             (tg/send-message! token chat-id answer-html))
                           (when-not (replace-bubble-with-answer! token chat-id answer-html)
                             (tg/send-message! token chat-id answer-html))))))
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
                                (vis/markdown->ir (vis/format-error (vis/db-error->user-message e))))
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
                 (vis/markdown->ir (vis/format-error (str "Voice transcription failed: " (or (ex-message e) e)))))
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
        (send! token chat-id (vis/markdown->ir (unauthorized-message chat-id)))
        true)

      (and callback-id chat-id (string? data)
        (str/starts-with? data "model:"))
      (let [idx-str (subs data (count "model:"))
            result  (when (re-matches #"\d+" idx-str)
                      (select-model! (str (inc (Long/parseLong idx-str)))))]
        (tg/answer-callback-query! token callback-id (:message result))
        (send! token chat-id (vis/markdown->ir (or (:message result) "Model selection failed.")))
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
          (do (send! token chat-id (vis/markdown->ir (unauthorized-message chat-id))) true)

          text
          (do
            (when-not (handle-slash! token chat-id text)
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
          (seq updates) (do (doseq [u updates]
                              ;; A single malformed update or a throwing
                              ;; command handler must NOT kill the poll
                              ;; thread: the offset is only advanced after
                              ;; the doseq, so an escaping exception would
                              ;; freeze the whole queue on the bad update.
                              (try
                                (handle-update! token u)
                                (catch InterruptedException e (throw e))
                                (catch Throwable e
                                  (tel/log! {:level :error :id ::handle-update-error
                                             :error e
                                             :data {:update-id (:update_id u)
                                                    :text      (-> u :message :text)}
                                             :msg "handle-update! failed; skipping update"})
                                  (.println ^java.io.PrintStream vis/original-stdout
                                    (str "[telegram] handle-update! failed on update "
                                      (:update_id u) " (" (-> u :message :text) "):"))
                                  (.printStackTrace ^Throwable e
                                    ^java.io.PrintStream vis/original-stdout))))
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

;; ----------------------------------------------------------------------------
;; Telegram-private slash specs
;;
;; Each entry is a declarative `:slash/*` spec routed through the
;; engine slash registry. Run-fns delegate to the existing
;; command-* helpers and wrap their return in the canonical
;; envelope shape:
;;
;;   {:slash/status :ok
;;    :slash/body   <IR>
;;    :slash/data   {:reply-markup <inline-keyboard?>}}
;;
;; `handle-slash!` extracts `:reply-markup` from `:slash/data` so
;; the inline keyboards (models / voice mode pickers) survive the
;; engine round-trip. `:slash/availability-fn` pins every spec to
;; the Telegram channel so the entries never leak into TUI.
;; ----------------------------------------------------------------------------

(defn- telegram-only-channel? [ctx]
  (= :telegram (:channel/id ctx)))

(defn- ctx-chat-id [ctx]
  (or (:telegram/chat-id ctx)
    ;; Engine-side slash dispatch (no chat-id stamped). Resolve from
    ;; the session_soul.external_id column.
    (when-let [db (:db-info ctx)]
      (when-let [sid (:session/id ctx)]
        (try (some-> (vis/db-get-session db sid) :external-id)
          (catch Throwable _ nil))))))

(defn- argv-arg [ctx]
  (str/join " " (:command/argv ctx)))

(defn- ok-ir [ir]
  {:slash/status :ok :slash/body ir})

(defn- ok-msg [{:keys [message reply-markup]}]
  (cond-> {:slash/status :ok :slash/body message}
    reply-markup (assoc :slash/data {:reply-markup reply-markup})))

;; Slash specs in `setMyCommands` order: help, status, model, models,
;; reasoning, verbosity, voice, cancel, restart, export. `/start`
;; ships as a hidden alias for `/help` (no menu entry).
(def ^:private telegram-menu-slash-specs
  [{:slash/name "help"
    :slash/doc  "Show Vis help"
    :slash/availability-fn telegram-only-channel?
    :slash/run-fn (fn [_ctx] (ok-ir (command-help)))}
   {:slash/name "status"
    :slash/doc  "Show session/model/status"
    :slash/availability-fn telegram-only-channel?
    :slash/run-fn (fn [ctx] (ok-ir (command-status (ctx-chat-id ctx))))}
   {:slash/name "model"
    :slash/doc  "Show current model"
    :slash/availability-fn telegram-only-channel?
    :slash/run-fn (fn [_ctx] (ok-ir (command-model)))}
   {:slash/name "models"
    :slash/doc  "List and choose models"
    :slash/availability-fn telegram-only-channel?
    :slash/run-fn (fn [ctx] (ok-msg (command-models (argv-arg ctx))))}
   {:slash/name "reasoning"
    :slash/doc  "Show or set reasoning effort"
    :slash/availability-fn telegram-only-channel?
    :slash/run-fn (fn [ctx] (ok-ir (command-reasoning (ctx-chat-id ctx) (argv-arg ctx))))}
   {:slash/name "verbosity"
    :slash/doc  "Show or set Codex verbosity"
    :slash/availability-fn telegram-only-channel?
    :slash/run-fn (fn [ctx] (ok-ir (command-verbosity (ctx-chat-id ctx) (argv-arg ctx))))}
   ;; /voice has Telegram-specific semantics (mode picker +
   ;; inline keyboard); vis-foundation-voice registers a
   ;; separate /voice for the TUI. The two specs do NOT collide
   ;; because their `:slash/availability-fn` predicates partition
   ;; the channel set.
   ;; /voice is always reachable on Telegram — command-voice itself
   ;; surfaces the 'voice not loaded' message when the runtime
   ;; doesn't have ASR/TTS available. Restricting availability
   ;; here would render a confusing 'unavailable' envelope when
   ;; voice is genuinely off.
   {:slash/name "voice"
    :slash/doc  "Show or set voice mode"
    :slash/availability-fn telegram-only-channel?
    :slash/run-fn (fn [ctx] (ok-msg (command-voice (ctx-chat-id ctx) (argv-arg ctx))))}
   {:slash/name "cancel"
    :slash/doc  "Cancel current request"
    :slash/availability-fn telegram-only-channel?
    :slash/run-fn (fn [ctx] (ok-ir (command-cancel (ctx-chat-id ctx))))}
   {:slash/name "restart"
    :slash/doc  "Restart the bot in a fresh JVM"
    :slash/availability-fn telegram-only-channel?
    :slash/run-fn (fn [_ctx] (ok-ir (command-restart)))}
   {:slash/name "export"
    :slash/doc  "Export session as Markdown"
    :slash/availability-fn telegram-only-channel?
    :slash/run-fn (fn [ctx] (ok-ir (command-export (ctx-chat-id ctx))))}
   {:slash/name "clear"
    :slash/doc  "Clear this session and start fresh"
    :slash/availability-fn telegram-only-channel?
    :slash/run-fn (fn [ctx] (ok-ir (command-clear (ctx-chat-id ctx))))}])

;; Hidden Telegram slashes — dispatched normally but excluded from
;; `setMyCommands`. `/start` aliases `/help` so first-time users get
;; the help text without polluting the menu.
(def ^:private telegram-hidden-slash-specs
  [{:slash/name "start"
    :slash/doc  "Show Vis help (alias)"
    :slash/availability-fn telegram-only-channel?
    :slash/hidden? true
    :slash/run-fn (fn [_ctx] (ok-ir (command-help)))}])

(def ^:private telegram-slash-specs
  (vec (concat telegram-menu-slash-specs telegram-hidden-slash-specs)))

(vis/register-extension!
  (vis/extension
    {:ext/name      "channel-telegram"
     :ext/description "Telegram bot channel - long-poll loop wired into sessions."
     :ext/version   "0.3.0"
     :ext/author    "Blockether"
     :ext/owner     "vis"
     :ext/license   "Apache-2.0"
     :ext/slash-commands telegram-slash-specs
     :ext/channels  [{:channel/id      :telegram
                      :channel/cmd     "telegram"
                      :channel/doc     "Run as a Telegram bot (needs TELEGRAM_BOT_TOKEN)."
                      :channel/usage   "vis channels telegram [approve|restart]"
                      :channel/main-fn #'channel-main
                      :channel/messages-renderer-fn #'render-for-telegram
                      :channel/subcommands #'telegram-channel-subcommands}]}))
