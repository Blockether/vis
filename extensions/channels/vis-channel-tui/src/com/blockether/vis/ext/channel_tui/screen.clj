(ns com.blockether.vis.ext.channel-tui.screen
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.chat :as chat]
            [com.blockether.vis.ext.channel-tui.click-regions :as cr]
            [com.blockether.vis.ext.channel-tui.command-suggest :as slash]
            [com.blockether.vis.ext.channel-tui.components :as components]
            [com.blockether.vis.ext.channel-tui.file-suggest :as file-suggest]
            [com.blockether.vis.ext.channel-tui.footer :as footer]
            [com.blockether.vis.ext.channel-tui.header :as header]
            [com.blockether.vis.ext.channel-tui.input :as input]
            [com.blockether.vis.ext.channel-tui.provider :as provider]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.render :as render]
            [com.blockether.vis.ext.channel-tui.scroll :as scroll]
            [com.blockether.vis.ext.channel-tui.scrollbar :as scrollbar]
            [com.blockether.vis.ext.channel-tui.selection :as selection]
            [com.blockether.vis.ext.channel-tui.state :as state]
            [com.blockether.vis.ext.channel-tui.tabs :as tabs]
            [com.blockether.vis.ext.channel-tui.terminal-image :as timg]
            [com.blockether.vis.ext.channel-tui.theme :as t]
            [com.blockether.vis.ext.channel-tui.virtual :as virtual]
            [com.blockether.vis.ext.channel-tui.dialogs :as dlg]
            [com.blockether.vis.internal.external-opener :as opener]
            [com.blockether.vis.internal.workspace :as workspace]
            [com.blockether.vis.internal.prompt-templates :as prompt-templates]
            [taoensso.telemere :as tel])
  (:import [com.googlecode.lanterna SGR TerminalPosition TerminalSize]
           [com.googlecode.lanterna.input KeyStroke KeyType MouseAction MouseActionType]
           [com.googlecode.lanterna.screen TerminalScreen Screen$RefreshType]
           [com.googlecode.lanterna.terminal MouseCaptureMode]
           [com.googlecode.lanterna.terminal.ansi UnixLikeTerminal$CtrlCBehaviour UnixTerminal]
           [java.io PrintWriter StringWriter]
           [java.nio.charset Charset]
           [java.util.concurrent TimeUnit]
           [java.util.concurrent.locks ReentrantLock]
           [sun.misc Signal SignalHandler]))
;;; ── Threading model ─────────────────────────────────────────────────────────────
;;
;; The TUI now runs two threads against the Lanterna screen:
;;
;;   1. Input thread (the original main loop): polls keys, dispatches
;;      events into app-db, opens modal dialogs. Never draws the chat
;;      view itself - just mutates state.
;;
;;   2. Render thread: sleeps on `state/render-monitor`, wakes on
;;      every dispatched event, and only repaints if `:render-version`
;;      moved or the terminal got resized. While a dialog is up the
;;      input thread holds `draw-lock` for the dialog's whole session
;;      so the render thread cannot scribble underneath it.
;;
;; `screen-size`/`doResizeIfNecessary`, `setCharacter`, `refresh`, and
;; `setCursorPosition` are owned EXCLUSIVELY by the holder of
;; `draw-lock`. `pollInput` lives on its own input queue inside
;; Lanterna and is safe to call concurrently from the input thread.
(defonce ^:private ^ReentrantLock draw-lock
  ^{:doc
    "Single screen-mutation lock. Held by the render thread for the
          duration of one paint, and by `with-dialog-lock` for the
          duration of a modal dialog session."}
  (ReentrantLock.))
(def ^:private ^:dynamic *skip-frame-refresh?*
  "When true, `render-frame!` paints into the back-buffer but does NOT push
   the delta to the terminal. Bound by `repaint-chat-frame!` so a modal
   dialog can repaint the chat behind it (e.g. live theme preview) and let
   the dialog's OWN refresh flush background + dialog chrome together — one
   paint, no flicker."
  false)

(defonce ^:private dialog-closed-at
  ;; millis of the last modal dialog close. Used to swallow a DUPLICATE bare
  ;; Escape that leaks into the editor right after Esc closes a dialog (see
  ;; `swallow-post-dialog-escape?`).
  (atom 0))

(def ^:private post-dialog-escape-window-ms
  "A bare Escape arriving within this many millis of a modal dialog close is
   treated as the DUPLICATE of the Esc that closed the dialog and swallowed,
   so it can't fall through to `handle-key` and wipe the editor draft."
  250)
;; Input box auto-sizing: starts at one row when empty (the smallest
;; surface that still reads as an input field) and grows by one row
;; per soft-wrap line of typed content, capped at four rows. Beyond
;; the cap `draw-input-box!` keeps the cursor in view by scrolling
;; the visible window vertically over the underlying text. The cap
;; was 8 rows; four is the sweet spot - long enough to show a real
;; multi-paragraph prompt, short enough that the input never eats
;; more than ~25% of a 1080p terminal's chat area.
(def ^:private input-min-lines 1)
(def ^:private input-max-lines 4)
(def ^:private mouse-double-click-ms
  "Maximum time between two selection clicks on the same row for line-select."
  500)
(def ^:private prewarm-sync-tail-count
  "How many newest bubbles to warm synchronously before launching
   the background pre-warm worker. Covers the region users hit on
   the first wheel-up after opening/switching sessions."
  16)
(def ^:private prewarm-sync-budget-ms
  "Wall-clock budget for synchronous tail warm-up. Keeps startup and
   workspace-switch responsive while eliminating first-scroll cold stalls."
  120)
(defn- input-empty?
  "True when the input editor has no text. The empty editor is `{:lines [\"\"]
   :crow 0 :ccol 0}` - a one-element vec with the empty string - so we
   can't just `(empty? lines)`."
  [{:keys [lines]}]
  (or (empty? lines) (every? str/blank? lines)))
(def ^:private drag-autoscroll-max-coalesce-factor
  "Upper bound for per-loop drag auto-scroll amplification."
  8)
(defn- mouse-wheel-delta
  "Return wheel direction as -1 / +1 for a MouseAction, else nil."
  [key]
  (when (instance? MouseAction key)
    (let [atype (.getActionType ^MouseAction key)]
      (cond (= atype MouseActionType/SCROLL_UP) -1
        (= atype MouseActionType/SCROLL_DOWN) 1
        :else nil))))
(defn- drag-action?
  [key]
  (and (instance? MouseAction key) (= MouseActionType/DRAG (.getActionType ^MouseAction key))))
(defn- coalesce-wheel-input
  "Collapse one run of consecutive wheel events into a single delta.

   Input:
   - `key`: first event already read from the input queue
   - `poll-next`: zero-arg fn returning next queued event or nil

   Output map:
   - `:key`         original first key
   - `:wheel-delta` signed accumulated wheel steps (or nil when zero)
   - `:next-key`    first non-wheel event encountered while draining"
  [key poll-next]
  (if-let [delta (mouse-wheel-delta key)]
    (loop [acc (long delta)]
      (if-let [next-key (poll-next)]
        (if-let [next-delta (mouse-wheel-delta next-key)]
          (recur (+ acc (long next-delta)))
          {:key key, :wheel-delta (when-not (zero? acc) acc), :next-key next-key})
        {:key key, :wheel-delta (when-not (zero? acc) acc)}))
    {:key key}))
(defn- coalesce-drag-input
  "Collapse consecutive DRAG events into one terminal event.

   Keeps the LAST drag event (latest cursor position) and returns how
   many drag events were merged so selection auto-scroll can scale one
   dispatch instead of flooding the reducer/render loop."
  [key poll-next]
  (if (drag-action? key)
    (loop [last-key key
           n 1]
      (if-let [next-key (poll-next)]
        (if (drag-action? next-key)
          (recur next-key (inc n))
          {:key last-key, :drag-events n, :next-key next-key})
        {:key last-key, :drag-events n}))
    {:key key, :drag-events 1}))
(defn- coalesced-drag-scroll-amount
  [amount drag-events]
  (let [amount (long (or amount 0))
        drag-events (long (or drag-events 1))
        factor (long (max 1 (min drag-autoscroll-max-coalesce-factor drag-events)))]
    (long (* amount factor))))
(defn- pop-pending!
  "Pop the oldest stashed keystroke from the pending vector stash, or nil."
  [pending-keys]
  (let [s @pending-keys]
    (when (seq s)
      (vreset! pending-keys (subvec s 1))
      (nth s 0))))

(defn- coalesce-chat-input
  "Coalesce wheel/drag floods behind `first-key`; the first non-coalescible
   event drained is stashed back at the FRONT of `pending-keys` (it is older
   than anything still queued behind it)."
  [first-key poll-next pending-keys]
  (let [{:keys [key wheel-delta next-key], :as wheel-pass}
        (coalesce-wheel-input first-key poll-next)]
    (if wheel-delta
      (do (when next-key (vreset! pending-keys (into [next-key] @pending-keys)))
        {:key key, :wheel-delta wheel-delta, :drag-events 1})
      (let [{:keys [key drag-events next-key]} (coalesce-drag-input (:key wheel-pass)
                                                 poll-next)]
        (when next-key (vreset! pending-keys (into [next-key] @pending-keys)))
        {:key key, :wheel-delta nil, :drag-events drag-events}))))

(defn- swallow-post-dialog-escape?
  "True (and consumes the marker) when a bare Escape lands within
   `post-dialog-escape-window-ms` of a modal dialog close - i.e. it is the
   duplicate of the Esc that closed the dialog, not a fresh press. Only ONE
   escape is swallowed per close."
  []
  (let [closed @dialog-closed-at]
    (and (pos? closed)
      (<= (- (System/currentTimeMillis) closed) post-dialog-escape-window-ms)
      (compare-and-set! dialog-closed-at closed 0))))

(defn- read-chat-input!
  "Read one chat-loop input event, coalescing wheel and drag floods.

   `pending-keys` is a volatile VECTOR stash (oldest first) for events
   consumed by drain loops that belong to later iterations. A bare Escape
   first runs `input/drain-sgr-leak!` so a literal `<65;32;43M` tail that
   leaked past the decoder (split-read SGR mouse report) is swallowed
   together with its phantom Escape instead of being typed into the input."
  [^TerminalScreen screen pending-keys]
  (let [poll-next #(or (pop-pending! pending-keys) (.pollInput screen))]
    (loop []
      (let [first-key (poll-next)]
        (if-not (input/bare-escape? first-key)
          (coalesce-chat-input first-key poll-next pending-keys)
          ;; A bare Escape within `post-dialog-escape-window-ms` of a modal
          ;; dialog close is the DUPLICATE of the Esc that closed the dialog
          ;; (terminals/tmux/SSH can echo a second ESC after the modal already
          ;; consumed the first). Swallow it so it can't fall through to
          ;; `handle-key` and wipe the editor draft the user was typing.
          (if (swallow-post-dialog-escape?)
            (recur)
            (let [{:keys [swallowed? replay]} (input/drain-sgr-leak! poll-next)]
              (vreset! pending-keys (into (vec replay) @pending-keys))
              (if swallowed?
                (recur)
                (coalesce-chat-input first-key poll-next pending-keys)))))))))
(defn- throwable-log-data
  [^Throwable t]
  (let [sw (StringWriter.)]
    (.printStackTrace t (PrintWriter. sw))
    {:class (.getName (class t)),
     :message (or (ex-message t) (str t)),
     :ex-data (ex-data t),
     :stack (.toString sw)}))
(defn- submit-input!
  "Submit the current editor buffer, then clear it.

   Important: `:send-message` must run before `:reset-input`. Large
   paste placeholders expand from `app-db :pastes`, and `:reset-input`
   clears that registry. Resetting first sends the cosmetic
   `[Pasted #N: ...]` token to the provider instead of the payload."
  [db input-state]
  (let [text (input/input->text input-state)]
    ;; T-003: never silently drop a non-empty submission while a turn
    ;; is in flight. Idle -> send-message. Busy -> enqueue with visible
    ;; feedback; drained from `:message-received` once :loading? clears.
    (when (and (seq (str/trim text)) (:session db))
      (cond (state/transcript-dump-input? text)
        (vis/notify! "Input looks like copied assistant transcript; not sent"
          :level :warn
          :ttl-ms 4000)
        (:loading? db) (do (state/dispatch [:enqueue-message text])
                         (state/dispatch [:reset-input]))
        :else (do (state/dispatch [:send-message text]) (state/dispatch [:reset-input]))))))

(def ^:private copy-success-ttl-ms 1500)
(def ^:private status-error-ttl-ms 5000)
(defn- copy-session-id!
  [text]
  (vis/worker-future "vis-tui-copy-session-id"
    #(try (input/clipboard-copy! text) (catch Throwable _ nil)))
  (vis/notify! "✓ Copied session ID" :level :success :ttl-ms copy-success-ttl-ms))
(defn- copy-selection!
  ([text] (copy-selection! text :transcript))
  ([text source]
   (vis/worker-future "vis-tui-copy-selection"
     #(try (input/clipboard-copy! (or text "")) (catch Throwable _ nil)))
   (vis/notify! (if (= source :input) "✓ Copied input selection" "✓ Copied selection")
     :level :success
     :ttl-ms copy-success-ttl-ms)))
(defn- copy-bubble!
  [text]
  (let [text (selection/clean-copied-text text)]
    (vis/worker-future "vis-tui-copy-bubble"
      #(try (input/clipboard-copy! text) (catch Throwable _ nil)))
    (vis/notify! "✓ Copied bubble" :level :success :ttl-ms copy-success-ttl-ms)))
(defn- handle-channel-event!
  [{:keys [op id text level ttl-ms], :as event}]
  (case op
    :input/replace (state/dispatch [:external-input :replace text (:workspace-id event)])
    :input/append (state/dispatch [:external-input :append text (:workspace-id event)])
    :input/insert (state/dispatch [:external-input :insert text (:workspace-id event)])
    :status/set
    (let [status-id (or id (:source event) :external)]
      (cond (= :error level) (do (state/dispatch [:channel-status-clear status-id])
                               (vis/notify! (or text "")
                                 :level :error
                                 :ttl-ms (or ttl-ms status-error-ttl-ms)))
        (= :ready (:phase event)) (state/dispatch [:channel-status-clear status-id])
        :else (let [until (when ttl-ms (+ (System/currentTimeMillis) (long ttl-ms)))]
                (state/dispatch [:channel-status-set status-id
                                 (cond-> {:text text, :level (or level :info)}
                                   until (assoc :until until))])
                (when ttl-ms
                  (vis/worker-future "vis-tui-status-expire"
                    #(do (Thread/sleep (long ttl-ms))
                       (state/dispatch [:channel-status-clear-if-until
                                        status-id until])))))))
    :status/clear (state/dispatch [:channel-status-clear (or id (:source event) :external)])
    :notify
    (vis/notify! (or text "") :level (or level :info) :ttl-ms (or ttl-ms copy-success-ttl-ms))
    nil))
(defn- slash-spec->menu-command
  "Adapt a declarative slash spec into the legacy menu-command shape
   `command_suggest.clj` consumes. Both top-level and nested slashes
   get palette entries. The engine's `slash/dispatch` does longest-prefix
   resolution regardless of palette discoverability."
  [spec]
  (let [name   (:slash/name spec)
        parent (vec (:slash/parent spec))
        path   (into parent [name])
        path-s (str/join " " path)]
    {:id (keyword (str/join "." path)),
     :label (or (:slash/doc spec)
              (some-> (:slash/usage spec)
                (clojure.string/replace #"^/+" ""))
              name),
     :doc          (:slash/doc spec),
     :slash/spec   spec,
     :slash/name   path-s,
     :slash/path   path,
     :slash/text   (str "/" path-s),
     :slash/usage  (or (:slash/usage spec) (str "/" path-s))}))
(defn- slash-available-in-tui?
  "True when a slash spec is safe to expose in TUI slash UX.
   `registered-slashes` is env-less and includes Telegram-only specs,
   so the TUI must apply channel availability itself before rendering
   suggestions."
  [spec]
  (and (not (:slash/hidden? spec))
    (if-let [available? (:slash/availability-fn spec)]
      (try (boolean (available? {:channel/id :tui})) (catch Throwable _ false))
      true)))
(def ^:private registry-slash-commands-cache
  "Memo cell for the harvested registry slash commands. The engine slash
   registry is stable within a session, so harvest ONCE and reuse — the first
   `/` no longer pays a cold harvest + class-load + compile mid-frame (which
   dropped a frame and flickered the popup on first open). Only NON-empty
   results are cached so a transient registry hiccup (caught → []) can't
   poison the cell; the next call simply retries."
  (atom nil))
;; "Session-stable" has ONE exception: a `/reload` of Python extensions can
;; add/remove slash commands mid-session. Drop the memo when that happens so
;; the next `/` re-harvests and the new commands show up without a restart.
(defonce ^{:private true
           :clj-kondo/ignore [:unused-private-var]}
  python-extension-slash-cache-reset
  (vis/add-python-extension-change-listener!
    ::reset-tui-slash-cache
    (fn [_] (reset! registry-slash-commands-cache nil))))
(defn- registry-slash-commands
  "All slashes harvested from the engine registry for typed `/`
   suggestions / exact slash submission in the TUI. Both top-level
   and nested commands are included. Hidden specs and non-TUI channel
   specs stay out; this prevents Telegram menu commands (`/help`,
   `/models`, ...) from leaking into TUI slash UX. Memoized via
   `registry-slash-commands-cache` (registry is session-stable) so the
   first `/` is already warm."
  []
  (or @registry-slash-commands-cache
    (let [v (try
              (let [specs        (filter slash-available-in-tui? (vis/registered-slashes))
                      ;; A spec is a "group root" when some other visible spec
                      ;; names its path as `:slash/parent`. Its own `:slash/run-fn`
                      ;; only prints the subcommand list the palette already shows
                      ;; inline, so suppress the redundant root entry. The engine
                      ;; `slash/dispatch` still resolves a typed `/workspace`
                      ;; (handled as a raw message submission), so the root stays
                      ;; reachable — it just isn't a palette suggestion.
                    parent-paths (into #{}
                                   (keep (fn [s]
                                           (let [p (vec (:slash/parent s))]
                                             (when (seq p) p))))
                                   specs)
                    leaf?        (fn [s]
                                   (let [path (conj (vec (:slash/parent s)) (:slash/name s))]
                                     (not (contains? parent-paths path))))]
                (mapv slash-spec->menu-command (filter leaf? specs)))
              (catch Throwable _t []))]
      (when (seq v) (reset! registry-slash-commands-cache v))
      v)))
(defn- template-slash-commands
  "Prompt templates as typed-`/` palette entries: `.vis/prompts/*.md`,
   `~/.vis/prompts/*.md`, and provider-contributed dynamic templates
   (`/skill:<name>`, …). NOT memoized — the template registries are
   marker-cached internally (stat-only when unchanged) and templates can
   appear mid-session (a file dropped in, a skill added). Selecting one
   submits the plain `/name` text; the engine expands it (registered
   slashes always win over a same-named template, so shadowed names are
   filtered by the caller)."
  []
  (try
    (mapv (fn [{:keys [name description]}]
            (let [desc (when-not (str/blank? (str description)) (str description))]
              {:id           (keyword (str "template." name))
               :label        (or desc name)
               :doc          desc
               :slash/name   name
               :slash/path   [name]
               :slash/text   (str "/" name)
               :slash/usage  (str "/" name " [args]")}))
      (prompt-templates/templates))
    (catch Throwable _ [])))

(defn- command-palette-extra-commands
  "Extra commands appended to Ctrl+K.

   Keep this empty by default: typed slash suggestions already expose the
   slash registry, and duplicating top-level roots (`/workspace`, `/voice`,
   help-ish extension commands) bloats Ctrl+K. Extensions must not appear in
   Ctrl+K unless we add an explicit opt-in later."
  []
  [])
(defn- menu-commands
  "Command universe for typed slash suggestion/exact-match handling.

   Built-in palette commands stay here because input matching shares the
   legacy command shape; registered top-level slashes stay here so typing `/`
   discovers `/workspace`, `/voice`, etc. Ctrl+K itself uses
   `command-palette-extra-commands` and remains minimal."
  [_screen]
  (let [base   (vec (concat dlg/palette-commands (registry-slash-commands)))
        ;; A registered slash shadows a same-named template (engine
        ;; precedence) — drop the duplicate suggestion.
        taken  (into #{} (keep :slash/text) base)]
    (into base (remove #(contains? taken (:slash/text %)) (template-slash-commands)))))
(defn- slash-command-for-input
  [screen input-state]
  (slash/exact-command (input/input->text input-state) (menu-commands screen)))
(defn- navigator-slash-for-input
  "When the typed text names — by EXACT full slash path — a slash spec that
   declares `{:slash/ui {:kind :navigator}}`, return that spec. The channel
   realizes the navigator intent directly (opens the Ctrl+G session/workspace
   picker) instead of dispatching to the engine, so `/workspace` and
   `/workspace list` both land in the SAME unified list — no useless answer
   bubble, identical live vs resume.

   EXACT path only (no longest-prefix fallback): `/workspace new` must still
   dispatch to the engine even though its parent `/workspace` is a navigator
   slash. `exact-command` can't be reused here because it keys only on the
   first token, so nested commands like `workspace list` never match it."
  [input-state]
  (when-let [{:keys [path]} (vis/slash-parse (input/input->text input-state))]
    (let [target (vec path)
          spec   (some (fn [s]
                         (when (= target (vec (concat (:slash/parent s) [(:slash/name s)])))
                           s))
                   (vis/registered-slashes))]
      (when (#{:navigator :dir-picker :clear-session} (get-in spec [:slash/ui :kind]))
        spec))))
(defn- prompt-arg-slash-for-input
  "When the typed text is EXACTLY a registered slash that declares
   `:slash/prompt-arg` and carries NO argument, return {:slash-text :prompt}
   so the channel can pop a text-input for the missing argument instead of
   running the slash blank. A trailing argument (`/draft new x`) returns nil —
   that runs normally."
  [input-state]
  (let [text (str/trim (input/input->text input-state))]
    (some (fn [s]
            (when-let [prompt (:slash/prompt-arg s)]
              (let [full (str "/" (str/join " " (concat (:slash/parent s) [(:slash/name s)])))]
                (when (= text full)
                  {:slash-text full :prompt prompt}))))
      (vis/registered-slashes))))
(defn- slash-suggestions-for-input
  ([screen input-state] (slash-suggestions-for-input screen input-state 0))
  ([screen input-state selected-index]
   (let [slash (slash/suggestions (input/input->text input-state)
                 (menu-commands screen)
                 {:limit Integer/MAX_VALUE, :selected-index selected-index})]
     ;; When no slash command matches, the same overlay + key handling drive
     ;; the inline `@` file picker (shared with the web; see file-suggest).
     (if (seq slash)
       slash
       (file-suggest/suggestions input-state selected-index)))))
(defn- input-state-from-text [text] (input/paste-text (input/empty-input) (or text "")))
(defn- activate-tab-entry-hit!
  "Switch to the workspace represented by a header click region."
  [refresh-active-tab! hit]
  (let [before (:active-tab-id @state/app-db)]
    (state/dispatch [:select-tab-index (:index hit)])
    (when-not (= before (:active-tab-id @state/app-db)) (refresh-active-tab! false))))
(defn- close-tab-entry-hit!
  "Close the workspace behind a header ✕ click region. Mirrors the Ctrl+W
   keyboard path: refresh the active tab only when it actually changed, and
   persist only when a tab was really removed (closing the last tab is a
   no-op in the reducer)."
  [refresh-active-tab! persist-tabs! hit]
  (let [before-active (:active-tab-id @state/app-db)
        before-n      (count (:tabs @state/app-db))]
    (state/dispatch [:close-tab (:workspace-id hit)])
    (when (not= before-n (count (:tabs @state/app-db)))
      (when (not= before-active (:active-tab-id @state/app-db))
        (refresh-active-tab! false))
      (persist-tabs!))))
(defn- capture-screen-cells
  "Read the current Lanterna back-buffer as per-cell strings.

   The snapshot is captured before selection highlighting is overlaid, so the
   copy payload is the visible text, not an artifact of the reverse-video pass."
  [^TerminalScreen screen cols rows]
  (vec (for [row (range rows)]
         (vec (for [col (range cols)]
                (let [tc (.getBackCharacter screen (int col) (int row))]
                  (or (some-> tc
                        .getCharacterString)
                    " ")))))))
(defn- paint-selection!
  "Overlay reverse-video on the selected back-buffer cells."
  [^TerminalScreen screen selection cols rows selectable-ranges viewport]
  (let [screen-selection (selection/document->screen-selection selection viewport)]
    (doseq [{:keys [row col width]}
            (selection/selected-ranges screen-selection cols rows selectable-ranges)
            x (range col (+ col width))]
      (when-let [tc (.getBackCharacter screen (int x) (int row))]
        (.setCharacter screen (int x) (int row) (.withModifier tc SGR/REVERSE))))))

(def ^:private search-hits-cache
  "Memoizes `paint-search-hits!`'s per-row match scan.

   Building a `StringBuilder` across all columns + lowercase + `indexOf` for
   every visible row is O(rows*cols) and ran on every live tick while search
   is open during a stream. The painted glyphs are fully determined by
   `[needle eff-scroll text-region cols render-version active-band]` (every
   db-mutating event bumps `:render-version`), so when that key is unchanged
   we reuse the computed match spans and only re-apply the SGR modifiers - the
   back buffer is rebuilt by the bubble paint each frame, so re-application is
   always required, but the expensive scan is not."
  (atom {:key nil :spans nil}))

(defn- paint-search-hits!
  "Overlay reverse-video on every visible back-buffer cell that belongs to a
   substring match for the active in-session search.

   Scans the WHOLE visible text region [text-top, text-top+inner-h) directly
   off the back buffer - whatever is painted gets highlighted - so matches
   show regardless of virtualization or a live/streaming render, and even on
   content not (yet) in `:search :hits` (which only drives scroll + the
   current-match accent). Bubble label rows (the role title \"Vis\"/\"You\" +
   timestamp) are EXCLUDED: search highlights CONTENT only, never chrome.

   Sentinels are NOT in the back buffer (the painter already translated them
   into ANSI style modifiers on each cell), so the row's characters are the
   visible glyphs only. Applies `SGR/REVERSE` on every matched column, or a
   solid accent block on the message the current match is parked on.

   The per-row scan is memoized in `search-hits-cache`: the painted glyphs are
   fully determined by `[needle eff-scroll top-y bot-y cols render-version
   active-band]` (every db-mutating event bumps `:render-version`), so when the
   key is unchanged we reuse the computed spans and skip the O(rows*cols)
   StringBuilder rebuild. The modifiers are still re-applied every frame because
   the bubble paint rebuilds the back buffer."
  [^TerminalScreen screen layout text-top inner-h cols db]
  (when-let [{:keys [active? query hits index case?]} (:search db)]
    (when (and active? (not (str/blank? (str query))))
      (let [needle (cond-> (str query) (not case?) str/lower-case)
            n-len (count needle)
            top-y (long text-top)
            bot-y (+ top-y (long inner-h))
            visible (:visible layout)
            label-rows (into #{} (map (fn [{:keys [top]}] (+ top-y (long top)))) visible)
            active-msg (when (seq hits) (nth hits (mod (long (or index 0)) (count hits))))
            active-band (when active-msg
                          (some (fn [{:keys [idx top height]}]
                                  (when (= idx active-msg)
                                    [(+ top-y (long top)) (+ top-y (long top) (long height))]))
                            visible))
            cache-key [needle case? (:eff-scroll layout) top-y bot-y cols (:render-version db) active-band]
            spans (if (= cache-key (:key @search-hits-cache))
                    (:spans @search-hits-cache)
                    (let [computed
                          (persistent!
                            (reduce
                              (fn [acc row]
                                (if (contains? label-rows row)
                                  acc
                                  (let [sb (StringBuilder.)
                                        _ (dotimes [c cols]
                                            (let [tc (.getBackCharacter screen (int c) (int row))
                                                  s (or (some-> tc .getCharacterString) " ")]
                                              (.append sb ^String s)))
                                        lower (cond-> (.toString sb) (not case?) str/lower-case)
                                        current? (boolean (and active-band
                                                            (<= (long (first active-band)) row)
                                                            (< row (long (second active-band)))))]
                                    (loop [from 0 acc acc]
                                      (let [pos (.indexOf ^String lower ^String needle (int from))]
                                        (if (>= pos 0)
                                          (recur (+ pos n-len)
                                            (conj! acc {:row row :start pos :current? current?}))
                                          acc))))))
                              (transient [])
                              (range (max top-y 0) bot-y)))]
                      (reset! search-hits-cache {:key cache-key :spans computed})
                      computed))]
        (doseq [{:keys [row start current?]} spans
                x (range start (+ start n-len))]
          (when-let [tc (.getBackCharacter screen (int x) (int row))]
            (.setCharacter screen (int x) (int row)
              (if current?
                (-> tc (.withBackgroundColor t/header-active-tab-accent) (.withForegroundColor t/dialog-bg))
                (.withModifier tc SGR/REVERSE)))))))))
(def ^:private bubble-content-h-pad
  "Horizontal text inset inside `render/draw-chat-bubble!` user content rows."
  2)
(def ^:private assistant-code-text-inset-markers
  #{p/MARKER_CODE p/MARKER_CODE_OK p/MARKER_CODE_ERR p/MARKER_RESULT
    p/MARKER_ERR_RESULT p/MARKER_MD_CODE p/MARKER_TH_MD_CODE})
(def ^:private selection-output-indent "  ")
(def ^:private selection-output-indent-markers #{})
(defn- assistant-code-text-row?
  [line]
  (let [line (or line "")] (some #(str/starts-with? line %) assistant-code-text-inset-markers)))
(defn- output-indented-row?
  [line]
  (let [line (or line "")]
    (some #(str/starts-with? line (str % selection-output-indent))
      selection-output-indent-markers)))
(defn- bubble-line-text-col
  [role bubble-left line]
  (cond (= :user role) (+ bubble-left bubble-content-h-pad)
    (assistant-code-text-row? line)
    (+ bubble-left
      1
      (if (output-indented-row? line) (p/display-width selection-output-indent) 0))
    :else bubble-left))
(def ^:private transcript-copy-skip-markers
  "Line markers that paint TUI chrome rather than message content.

   Mouse-selection copy operates on rendered screen cells, so clipping these
   rows before extraction keeps copied transcript text free of role banners,
   answer dividers, padding bands, iteration labels, and provider/model footers."
  #{p/MARKER_ITERATION_HDR p/MARKER_DURATION p/MARKER_SEP p/MARKER_ANSWER_SEP
    p/MARKER_ANSWER_HDR p/MARKER_ANSWER_PAD p/MARKER_CODE_PAD p/MARKER_CODE_OK_PAD
    p/MARKER_CODE_ERR_PAD p/MARKER_ITERATION_PAD p/MARKER_QUEUE_HDR})
(defn- copyable-transcript-line?
  [line]
  (let [line (or line "")] (not-any? #(str/starts-with? line %) transcript-copy-skip-markers)))
(defn- projected-content-lines
  "Lines for clipboard / selection layout. Bubble messages always
   carry `:ir` (canonical answer-IR); we derive the markdown string
   on demand if the walker hasn't projected the bubble yet."
  [message content-w]
  (or (:prewrapped-lines message)
    (let [text (or (:text message)
                 (some-> (:ir message)
                   (vis/render :markdown)))]
      (render/wrap-text (or text "") content-w))))
(defn- bubble-selectable-ranges
  "Return absolute screen-cell ranges for visible transcript message content.

   Selection deliberately excludes the header, input box, footer, scrollbar
   gutter, message-area margins, role/timestamp row, final inter-bubble gap,
   assistant provider/model footer, and structural separator/padding rows.
   Dragging across those cells may continue a gesture, but highlight/copy is
   clipped back to user/model-authored text rows."
  [layout text-top inner-h cols]
  (let [bubble-left (long render/MESSAGE_MARGIN_LEFT)
        bubble-w (long (max 0 (- (long cols) render/MESSAGE_SIDE_PAD)))
        content-w (long (max 0 (- bubble-w (* 2 bubble-content-h-pad))))
        top-limit (long text-top)
        bottom-limit (+ top-limit (long (max 0 inner-h)))]
    (if (or (not (pos? content-w)) (<= bottom-limit top-limit))
      []
      (vec
        (for [{:keys [top projected]} (:visible layout)
              :let [message (or projected {})
                    sep-pad 0
                    top-pad (if (= :user (:role message)) 1 0)
                    content-top (+ top-limit (long top) sep-pad 1 top-pad)]
              [idx line] (map-indexed vector (projected-content-lines message content-w))
              :let [row (+ content-top (long idx))]
              :when (and (<= top-limit row) (< row bottom-limit) (copyable-transcript-line? line))]
          {:row row,
           :col (bubble-line-text-col (:role message) bubble-left line),
           :width content-w})))))
(defn- transcript-document-copy-lines
  "Return selectable transcript rows in document coordinates.

   Mouse drag anchors live in virtual transcript coordinates so selection can
   survive auto-scroll. The old copy path projected that selection back onto the
   current screen cells, losing rows that had scrolled off-screen before
   release. This builds the selected document rows from messages/layout instead."
  [messages layout cols settings copy-opts selection]
  (let [bubble-left (long render/MESSAGE_MARGIN_LEFT)
        bubble-w (long (max 0 (- (long cols) render/MESSAGE_SIDE_PAD)))
        content-w (long (max 0 (- bubble-w (* 2 bubble-content-h-pad))))
        offsets (vec (:offsets layout))
        heights (vec (:heights layout))
        visible-projected-by-idx (into {}
                                   (keep (fn [{:keys [idx projected]}]
                                           (when (some? idx) [idx projected]))
                                     (:visible layout)))
        {:keys [start end]} (selection/normalize selection)
        start-row (long (:row start))
        end-row (long (:row end))]
    (if (or (not (pos? content-w)) (empty? offsets))
      []
      (vec
        (for [[idx message] (map-indexed vector messages)
              :let [top (long (or (get offsets idx) 0))
                    bottom (long (or (get offsets (inc idx))
                                   (+ top (long (or (get heights idx) 0)))))]
              :when (and (<= top end-row) (>= (dec bottom) start-row))
              :let [visible-projected (get visible-projected-by-idx idx)
                    projected (cond (and (:pending? message) visible-projected) visible-projected
                                (:prewrapped-lines message) message
                                :else
                                (virtual/project-message message bubble-w settings copy-opts))
                    message (or projected message {})
                    top-pad (if (= :user (:role message)) 1 0)
                    content-top (+ top 1 top-pad)]
              [line-idx line] (map-indexed vector (projected-content-lines message content-w))
              :let [row (+ content-top (long line-idx))]
              :when (and (<= start-row row) (<= row end-row) (copyable-transcript-line? line))]
          (let [visible (selection/clean-copied-text line)
                visible (if (and (output-indented-row? line)
                              (str/starts-with? visible selection-output-indent))
                          (subs visible (count selection-output-indent))
                          visible)]
            {:row row,
             :col (bubble-line-text-col (:role message) bubble-left line),
             :width content-w,
             :text visible}))))))
(defn- selection-touches-pending-bubble?
  "True when the selection range overlaps a bubble whose message map
   is `:pending? true`. Live progress (thinking + iteration trace)
   is NOT carried on the message map — it lives in workspace
   `:progress` and is painted directly by the bubble renderer; the
   document-rows projection only sees the static placeholder IR
   (`\"Sending request to provider…\"`). When the user drag-selects
   across a live bubble, the document path therefore copies the
   placeholder instead of the visible trace. Falling back to the
   screen-cells path picks up exactly what's painted."
  [messages layout selection]
  (let [{:keys [start end]} (selection/normalize selection)
        start-row (long (:row start))
        end-row (long (:row end))
        offsets (vec (:offsets layout))
        heights (vec (:heights layout))]
    (boolean (some (fn [[idx message]]
                     (when (:pending? message)
                       (let [top (long (or (get offsets idx) 0))
                             bottom (long (or (get offsets (inc idx))
                                            (+ top (long (or (get heights idx) 0)))))]
                         (and (<= top end-row) (>= (dec bottom) start-row)))))
               (map-indexed vector messages)))))
(defn- selected-transcript-text
  "Extract selected transcript text from virtual document rows, not only
   current screen cells. Used when mouse selection auto-scroll moves earlier
   selected rows off-screen before release."
  [messages layout cols settings copy-opts selection]
  (let [doc-lines (transcript-document-copy-lines messages layout cols settings copy-opts selection)
        total-h (long (or (:total-h layout) (peek (vec (:offsets layout))) 0))
        by-row (into {} (map (juxt :row identity) doc-lines))
        ranges (selection/selected-ranges selection
                 cols
                 total-h
                 (mapv #(select-keys % [:row :col :width]) doc-lines))]
    (selection/clean-copied-text (str/join
                                   "\n"
                                   (map (fn [{:keys [row col width]}]
                                          (let [{line-col :col, text :text} (get by-row row)
                                                text (or text "")
                                                from (max 0 (- (long col) (long (or line-col 0))))
                                                to (min (count text) (+ from (long width)))]
                                            (if (< from to) (subs text from to) "")))
                                     ranges)))))
(defn- release-selection-focus
  "Return document-space focus for a mouse-selection release.

   Drag-copy must use the release event's current viewport so a selection that
   auto-scrolled past the original screen includes the newly exposed rows.
   Double-click line selection is pre-expanded at click-down, so keep that
   stored focus."
  [anchor stored-focus line-selection? screen-point viewport]
  (if line-selection?
    (or stored-focus anchor)
    (selection/screen->document-point screen-point viewport)))
(defn- copyable-bubble-text
  "Whole-bubble copy hands the user complete text, not the collapsed viewport.

   For trace bubbles, expand disclosure blocks for clipboard export so a copied
   bubble does not paste `N chars hidden` placeholders back into the prompt.
   Plain answer/user bubbles keep the projected text fallback used for live
   streaming, where the raw message can still be a placeholder."
  [message bubble-w settings {:keys [session-id detail-expansions]}]
  (if (and (= :assistant (:role message)) (:traces message))
    (let [opts {:session-id session-id,
                :session-turn-id (or (:turn-id message) (:session-turn-id message) (:id message)),
                :detail-expansions (assoc (or detail-expansions {})
                                     :vis.channel-tui/expand-all-details? true)}]
      (:text (render/format-answer-with-thinking-data (:ir message)
               (:traces message)
               bubble-w
               settings
               (:confidence message)
               (= :cancelled (:status message))
               opts)))
    (or (:text message)
      (some-> (:ir message)
        (vis/render :markdown))
      "")))
(defn- bubble-copy-regions
  "Return absolute screen-cell rectangles for single-click whole-bubble copy.

   Drag selection remains clipped to `bubble-selectable-ranges`; these wider
   regions only answer the release-time question: which message did this
   simple click land on? They cover the visible bubble rectangle except for
   the final inter-bubble gap row, so clicking the role row or bubble padding
   still copies the message while clicking between bubbles does nothing."
  [layout messages text-top inner-h cols settings copy-opts]
  (let [bubble-left (long render/MESSAGE_MARGIN_LEFT)
        bubble-w (long (max 0 (- (long cols) render/MESSAGE_SIDE_PAD)))
        top-limit (long text-top)
        bottom-limit (+ top-limit (long (max 0 inner-h)))]
    (if (or (not (pos? bubble-w)) (<= bottom-limit top-limit))
      []
      (vec
        (for [{:keys [idx top height projected]} (:visible layout)
              :let [;; Use the PROJECTED message, not the raw one from
                    ;; `messages`. For the live streaming bubble the raw
                    ;; message has `:ir` = pending placeholder ("Sending
                    ;; request to provider...") while `projected` has
                    ;; `:text` set by virtual/layout's loading branch
                    ;; with the actual streamed content. Falling back to
                    ;; the raw message would render the placeholder IR
                    ;; on copy, which is the bug fixed here.
                    raw-message (nth messages idx nil)
                    ;; Prefer the raw persisted message for trace bubbles so
                    ;; clipboard can expand hidden blocks. Keep projected text
                    ;; for live streaming/plain bubbles; it may carry fresher
                    ;; visible content than the raw placeholder.
                    message (if (:traces raw-message) raw-message (or projected raw-message))
                    text (copyable-bubble-text message bubble-w settings copy-opts)
                    sep-pad 0
                    bubble-top (+ top-limit (long top) sep-pad)
                    copy-height (max 1 (- (long height) sep-pad 1))
                    copy-bottom (min bottom-limit (+ bubble-top copy-height))
                    row (max top-limit bubble-top)
                    clipped-height (- copy-bottom row)]
              :when (and (pos? clipped-height) (not (str/blank? text)))]
          {:row row, :col bubble-left, :width bubble-w, :height clipped-height, :text text})))))
(defn- disclosure-copy-regions
  "Per-disclosure copy targets. Each visible row of an EXPANDED disclosure
   body carries `:meta {:kind :copy-block-body :node-id ... :text ...}`
   from the renderer (see `tag-copy-block-body`). On a plain (no-drag)
   click the screen handler picks these BEFORE the whole-bubble copy
   region, so a click under a `▾ RESULT` summary copies just that
   block's body, not the entire assistant message. Drag selection is
   unaffected - it operates on screen cells via `selectable-ranges`."
  [layout text-top inner-h cols]
  (let [bubble-left (long render/MESSAGE_MARGIN_LEFT)
        bubble-w (long (max 0 (- (long cols) render/MESSAGE_SIDE_PAD)))
        top-limit (long text-top)
        bottom-limit (+ top-limit (long (max 0 inner-h)))]
    (if (or (not (pos? bubble-w)) (<= bottom-limit top-limit))
      []
      (vec
        (for [{:keys [top projected]} (:visible layout)
              :let [line-meta (:line-meta projected)
                    sep-pad 0
                    bubble-top (+ top-limit (long top) sep-pad)]
              :when (sequential? line-meta)
              i (range (count line-meta))
              :let [m (nth line-meta i nil)
                    abs-row (+ bubble-top (long i))]
              :when (and (map? m)
                      (= :copy-block-body (:kind m))
                      (not (str/blank? (str (:text m))))
                      (>= abs-row top-limit)
                      (< abs-row bottom-limit))]
          {:row abs-row,
           :col bubble-left,
           :width bubble-w,
           :height 1,
           :text (:text m),
           :node-id (:node-id m)})))))
(defn- fitting-image-placements
  "Keep only inline-image placements whose full `:rows` box fits inside the
   transcript viewport `[messages-top, messages-bottom)`. The graphics layer
   can't be clipped to the scroll region, so a partially-scrolled image is
   dropped rather than drawn over the input/footer. `placements` are the exact
   painted positions `draw-chat-bubble!` recorded via `render/*image-placements*`."
  [placements messages-top messages-bottom]
  (let [top    (long messages-top)
        bottom (long messages-bottom)]
    (vec
      (for [{:keys [row img] :as p} placements
            :when (and (>= (long row) top)
                    (<= (+ (long row) (long (:rows img))) bottom))]
        p))))

(defonce ^:private image-paint-state
  ;; Signature of the images currently drawn on the terminal's graphics layer.
  ;; Guards the post-refresh emit so a multi-KB Kitty/iTerm2 sequence rides
  ;; the wire ONLY when the visible image set (path / position / box) changes,
  ;; not on every spinner heartbeat.
  (atom nil))

(defn- paint-terminal-images!
  "Draw every region from `image-regions` onto the terminal's graphics layer,
   AFTER Lanterna's delta refresh has painted the reserved blank cells. Wrapped
   in DECSC/DECRC so the shell cursor lands back where Lanterna left it. Kitty
   placements are cleared first (`d=A`) so scrolling doesn't stack ghosts. Skips
   the whole write when the signature matches the last paint."
  [regions]
  (let [proto     (timg/images-protocol)
        signature (mapv (fn [{:keys [row col img]}]
                          [row col (:path img) (:cols img) (:rows img)])
                    regions)]
    (when (and proto (not= signature @image-paint-state))
      (let [^java.io.OutputStream out @vis/tty-out
            sb (StringBuilder.)]
        (.append sb "\u001b7") ;; DECSC save cursor
        (when (and (= proto :kitty) (seq @image-paint-state))
          (.append sb "\u001b_Ga=d,d=A,q=2\u001b\\")) ;; drop prior kitty images
        (doseq [{:keys [row col img]} regions]
          (when-let [seqstr (timg/render-sequence (:path img) (:mime img)
                              {:cols (:cols img), :rows (:rows img)})]
            (.append sb (format "\u001b[%d;%dH" (inc (long row)) (inc (long col))))
            (.append sb seqstr)))
        (.append sb "\u001b8") ;; DECRC restore cursor
        (try
          (when out
            (.write out (.getBytes (.toString sb) "UTF-8"))
            (.flush out))
          (catch Throwable _ nil)))
      (reset! image-paint-state signature))))

(defn- bubble-copy-hit
  [point regions]
  (let [col (long (:col point))
        row (long (:row point))]
    (some (fn [{r :row, c :col, w :width, h :height, :as region}]
            (when (and (>= row (long r))
                    (< row (+ (long r) (long h)))
                    (>= col (long c))
                    (< col (+ (long c) (long w))))
              region))
      regions)))
(defn- input-selectable-ranges
  "Return absolute screen-cell ranges for the visible input editor text rows.

   The ranges start at the same horizontal text inset used by
   `render/draw-input-box!`, so selection copies the user's draft text without
   the input box padding or border chrome."
  [input-top text-rows cols]
  (let [cols (long (max 0 cols))
        text-w (long (render/input-text-w cols))
        left (long (max 0 (quot (- cols text-w) 2)))
        text-top (+ (long input-top) 1 (long render/input-pad-y))
        n (long (max 0 text-rows))]
    (if (or (not (pos? text-w)) (not (pos? n)))
      []
      (vec (for [row (range text-top (+ text-top n))] {:row row, :col left, :width text-w})))))
(defn- selectable-ranges-for-source
  [source transcript-ranges input-ranges]
  (if (= source :input) input-ranges transcript-ranges))
(defn- with-dialog-lock
  "Mark a dialog open in app-db AND grab `draw-lock` for the dialog's
   whole session. Holding the lock blocks the render thread cleanly
   regardless of timing: even if the version bump from
   `:set-dialog-open true` races a render in flight, the dialog can't
   start drawing until the render thread releases the lock, and once
   we hold it, the render thread's next attempt blocks until we're
   done. After the dialog returns we release the lock and the
   `:set-dialog-open false` dispatch wakes the render thread to
   repaint over the dialog area."
  [f]
  (.lock draw-lock)
  (try (state/dispatch [:set-dialog-open true])
    (try (f) (finally (reset! dialog-closed-at (System/currentTimeMillis))
               (state/dispatch [:set-dialog-open false])))
    (finally (.unlock draw-lock))))
(defn- paint-search-bar!
  "Call-site adapter for `components/find-bar!` — the reusable find bar and its
   `button!` widgets (paint + hover + click region, together) live in
   components.clj now, so this can't drift from the highlight/key controller.

   Returns the bar's cursor cell [col row] (nil when inactive) so callers can
   move the terminal cursor into the query field while the bar owns typing."
  [g cols text-top db]
  (components/find-bar! g cols text-top (:search db)))
(defn- paint-jump-bottom!
  "Floating \"jump to latest\" affordance — the TUI twin of the web's
   `#jump-bottom` button. Painted ONLY while the user has scrolled UP off the
   live bottom (`scroll/scrolled-up?`), bottom-right of the messages viewport
   just above the echo area. Clicking it (the `:jump-bottom` click region) — or
   pressing C-l / Ctrl+End — re-arms FOLLOW and eases to the newest content.
   Hidden while following, exactly like the web. Reuses `components/button!`
   so its look / hover / click region can't drift from the other TUI chips.

   Gated on `scroll/bottom-hidden?` (live bottom is OFF-SCREEN below), NOT on
   `scrolled-up?` — an empty/short session has nothing below, so a PageUp that
   parks `:at` offset 0 must NOT pop a chip pointing at nowhere."
  [g cols messages-bottom max-scroll db]
  (when (scroll/bottom-hidden? (:scroll db) max-scroll)
    (let [label " ↓ latest (C-x j) "
          w     (long (p/display-width label))
          ;; Horizontally CENTERED, floating just above the echo area — the
          ;; chat-app convention (a centered pill), not tucked in a corner.
          col   (max 0 (quot (- (long cols) w) 2))
          row   (max 0 (dec (long messages-bottom)))]
      (components/button! g col row label :jump-bottom))))
(defn- open-click-target!
  ([{:keys [kind url]}]
   (vis/worker-future "vis-tui-open-click-target"
     #(try (if (= :file kind) (opener/open-file-in-editor! url) (opener/open! url))
        (catch Throwable _ nil))))
  ([^TerminalScreen _screen ref] (open-click-target! ref)))
(defn- screen-size
  "Lanterna size + lazy resize handling. MUST be called with `draw-lock`
   held (or before the render thread is started) because
   `doResizeIfNecessary` reallocates the back buffer."
  ^com.googlecode.lanterna.TerminalSize [^TerminalScreen screen]
  (if-let [new-size (.doResizeIfNecessary screen)]
    (do (try (.refresh screen Screen$RefreshType/COMPLETE)
          (catch NullPointerException _
               ;; Lanterna buffer may have null cells after resize before first
               ;; full render.  DELTA is safe because it only touches dirty cells.
            (try (.refresh screen Screen$RefreshType/DELTA) (catch Exception _ nil))))
      new-size)
    (.getTerminalSize screen)))
;; `apply-settings` was retired in favour of
;; `com.blockether.vis.ext.channel-tui.virtual/layout`, which
;; projects ONLY the messages whose viewport interval is non-empty
;; (cold-open of long sessions no longer pays
;; `format-answer-with-thinking` for every off-screen bubble before
;; the first frame). The `:show-timestamps` projection moved into
;; `virtual/project-message`; the loading-bubble swap moved into
;; `virtual/layout`'s pass-2 logic. See `virtual.clj` for the why.
(defn- input-text-rows
  "Compute visible text rows for the input box based on content,
   counting SOFT-WRAPPED visual rows so the box grows as a single
   logical line overflows the box width. Capped between
   `input-min-lines` and `input-max-lines`; beyond the cap the box
   stops growing and `draw-input-box!` scrolls vertically to keep
   the cursor visible."
  [{:keys [lines]} cols]
  (let [text-w (render/input-text-w cols)
        n (render/input-visual-row-count lines text-w)]
    (min input-max-lines (max input-min-lines n))))
(defn- overlay-locked?
  "True when an F1 help / F2 context modal card owns the whole screen.
   While locked, the cheap render fast-paths are disabled and the cursor
   is hidden — the card is the only interactive surface, but the chrome
   underneath stays painted so the input is visible behind the overlay."
  [db]
  (boolean (or (:help-open? db) (:tasks-open? db))))

(defn- draw-bottom-chrome!
  "Paint the bottom screen chrome — input box, echo-area row, the two
   footer rows, and slash-command suggestions — then place the text cursor.

   Always draws the full chrome so the input remains visible behind F1/F2
   overlays (matching the Lanterna modal behaviour where the last frame
   persists underneath the dialog). When `overlay-locked?` the cursor is
   hidden so the overlay owns the interactive surface."
  [^TerminalScreen screen g db
   {:keys [input input-top text-rows cols now-ms echo-row footer-row
           slash-suggestions slash-command-index]}]
  (let [[cx cy]
        (render/draw-input-box! g input input-top text-rows cols nil)]
    (footer/draw-echo-area! g db echo-row cols now-ms)
    (footer/draw-footer! g db footer-row cols now-ms)
    (render/draw-slash-command-suggestions! g
      slash-suggestions
      input-top
      cols
      slash-command-index)
    (if (or (overlay-locked? db) (scroll/scrolled-up? (:scroll db)))
      (.setCursorPosition screen nil)
      (.setCursorPosition screen (TerminalPosition. cx cy)))))

(defn- render-frame!
  "Draw one frame: background, messages area (bubbles), input box,
   echo-area row, and two footer rows.

   Returns the layout map `{:total-h, :inner-h, :cols, :rows}` so the
   render thread can publish it back into app-db for the input thread's
   scroll handlers. `apply-settings` runs ONCE here and feeds both the
   layout calculation and the actual draw - the old code path computed
   it twice per frame, which doubled cost on long traces."
  [^TerminalScreen screen cols rows
   {:keys [messages input progress loading? cancelling? turn-start-ms settings
           slash-command-index],
    :as db} now-ms]
  (let [now-ms (long now-ms)
        g (.newTextGraphics screen)
        text-rows (input-text-rows input cols)
        input-box-h (+ text-rows 2 (* 2 render/input-pad-y))
        ;; Reserve bottom rows for footer proper (model/status + provider
        ;; limits). The Emacs echo area is a single flat row directly
        ;; above the editor, which now draws its own top border.
        header-top 0
        footer-row (- rows 2)
        input-top (- rows input-box-h 2)
        echo-row (- input-top 1)
        ;; Keep one empty terminal row between the header band (`Vis`/workspace
        ;; strip) and the first transcript bubble. `draw-messages-area!` then
        ;; applies its own internal MESSAGE_MARGIN_TOP inside this area; without
        ;; this outer gap the first recap/progress bubble visually hugs the
        ;; header bottom rule.
        messages-top (inc (header/header-rows db))
        messages-bottom echo-row
        ;; Single source of truth for the gutter math lives in `render.clj`
        ;; (`MESSAGE_SIDE_PAD`). Reference it directly; do
        ;; NOT inline a literal here. Two layers disagreeing by even
        ;; one column makes `format-iteration-entry` size labels for
        ;; one bubble-w while `draw-chat-bubble!` paints into a
        ;; different bubble-w - right-aligned labels (`BLOCK 3`,
        ;; `✓ 3ms`, `FINAL ANSWER`) wrap onto two lines from the
        ;; mismatch. Use the const, never the value.
        bubble-w (max 1 (- cols render/MESSAGE_SIDE_PAD))
        inner-h (max 0 (- messages-bottom messages-top 2)) ;; top + bottom margins
        ;; Derive the concrete layout offset from the `:scroll` variant. nil
        ;; = FOLLOW settled at the bottom (auto-bottom, never anchored); a
        ;; number = parked or mid-ease. `prev-max-s` from the previously
        ;; published layout is good enough to tell "settled" from "easing".
        prev-max-s (max 0 (- (long (or (:total-h (:layout db)) inner-h)) inner-h))
        messages-scroll (scroll/layout-offset (:scroll db) prev-max-s)
        ;; `:viewport-rows` lets the live progress bubble truncate its
        ;; iteration trace to what actually fits on screen instead of
        ;; formatting all 15-of-N iterations every spinner tick. Off-screen
        ;; iterations collapse under the existing `PROGRESS HISTORY`
        ;; toggle; clicking it expands the full trace on demand.
        progress-extra {:now-ms now-ms,
                        :turn-start-ms turn-start-ms,
                        :cancelling? (boolean cancelling?),
                        :viewport-rows inner-h,
                        :pending-sends (:pending-sends db)}
        ;; Single virtualized layout pass: cheap height estimate for
        ;; every message, full projection + real height ONLY for
        ;; messages whose viewport interval is non-empty. The
        ;; resulting `:total-h` feeds the scrollbar geometry +
        ;; gets published into app-db so input-thread scroll handlers
        ;; have an accurate ceiling.
        layout (virtual/layout
                 messages
                 bubble-w
                 settings
                 messages-scroll
                 inner-h
                 {:progress progress, :loading? loading?, :progress-extra progress-extra}
                 {:session-id (get-in db [:session :id]),
                  :detail-expansions (:detail-expansions db)
                  ;; Previous frame's cumulative offsets let `layout`
                  ;; anchor the scroll to the message that was at the
                  ;; top of the viewport, so estimate->real height
                  ;; corrections (which move `total-h`) don't lurch the
                  ;; viewport / scrollbar thumb mid-scroll.
                  :prev-offsets (get-in db [:layout :offsets])})
        ;; Persist the anchor-corrected scroll so the input thread's
        ;; wheel/drag math and the next layout share the same offset.
        ;; nil = auto-bottom (never written back). Skip the dispatch
        ;; when unchanged to avoid churning render versions. Pass the
        ;; delta so an in-flight scroll animation's target re-anchors
        ;; with the offset (otherwise leaving auto-bottom lurches).
        anchored-scroll (:anchored-scroll layout)
        _ (when (and (some? anchored-scroll)
                  (some? messages-scroll)
                  (not= anchored-scroll messages-scroll))
            (state/dispatch [:reanchor-scroll anchored-scroll
                             (- (long anchored-scroll) (long messages-scroll))]))
        total-h (long (:total-h layout))
        text-top (+ messages-top render/MESSAGE_MARGIN_TOP)
        transcript-selectable-ranges (bubble-selectable-ranges layout text-top inner-h cols)
        transcript-bubble-copy-regions (bubble-copy-regions layout
                                         messages
                                         text-top
                                         inner-h
                                         cols
                                         settings
                                         {:session-id (get-in db [:session :id]),
                                          :detail-expansions (:detail-expansions
                                                              db)})
        transcript-disclosure-copy-regions (disclosure-copy-regions layout text-top inner-h cols)
        input-selectable-ranges (input-selectable-ranges input-top text-rows cols)
        selectable-ranges (into transcript-selectable-ranges input-selectable-ranges)
        slash-suggestions (slash-suggestions-for-input screen input slash-command-index)
        ;; F2 context panel snapshot ({:tasks :facts}) — derived once here (like
        ;; slash-suggestions) from the active session's cache, refreshed at each
        ;; turn end. The paint body just reads it; no inline let, no DB hit.
        ctx-snapshot      (get-in db [:ctx-by-session (get-in db [:session :id])]
                            {:tasks {} :facts {} :archived {}})
        ;; Collector for inline-image placements. `draw-messages-area!` conj's
        ;; the exact painted position of each expanded `vis-image` row here;
        ;; the post-refresh graphics pass drains it below.
        image-sink        (atom [])]
    (render/fill-background! g cols rows)
    ;; Messages area draws FIRST. It opens a new click-region staging
    ;; pass via `cr/begin-frame!` and registers every painted chrome
    ;; row (links, image markers, file links). The header then
    ;; registers its :copy-id region. The published click-region
    ;; registry is unchanged until `cr/commit-frame!` runs at the end
    ;; of this fn - so the input thread can `cr/lookup` at any time
    ;; during the paint and still get a complete previous frame back
    ;; instead of a half-filled buffer (the bug that made the header
    ;; copy-id button feel \"sometimes broken\" when the spinner was
    ;; ticking).
    (binding [render/*image-placements* image-sink]
      (render/draw-messages-area! g layout messages-top messages-bottom cols))
    (header/draw-header! g db header-top cols)
    ;; Bottom band (input box + footer + slash suggestions) — always painted so
    ;; the input stays visible behind F1/F2 overlays (modal-like behaviour).
    (draw-bottom-chrome! screen g db
      {:input input, :input-top input-top, :text-rows text-rows, :cols cols,
       :now-ms now-ms, :echo-row echo-row, :footer-row footer-row,
       :slash-suggestions slash-suggestions, :slash-command-index slash-command-index})
    ;; Atomically publish every chrome region painted above. Until this swap runs the input
    ;; thread sees the PREVIOUS frame's regions, which is the correct fallback - the previous
    ;; frame matches what's actually still on the user's screen up to this instant.
    ;; capture-screen-cells walks cols×rows of the back buffer and allocates a 2D string vec.
    ;; On a 200×50 terminal that's 10000 .getBackCharacter calls + ~10000 string allocations
    ;; per frame. The cells are ONLY consumed by the mouse-selection clipboard copy path - we
    ;; read `(:screen-cells (:layout db))` to extract the text under the user's drag. When no
    ;; selection is active, nothing reads them. Skip the capture in that case.
    ;;
    ;; Safety: when a fresh selection starts (mouse press), the next
    ;; full render is dispatched (the input handler bumps
    ;; render-version on every mouse event); :mouse-selection becomes
    ;; non-nil in db, so we DO capture this frame. Selection content
    ;; uses these cells. The only edge case is a press+release within
    ;; the SAME render frame, which can't happen because each event
    ;; bumps version and the render loop processes one version at a
    ;; time. See autoresearch A11.
    (let [overlay-geom (when (:tasks-open? db)
                         (components/context-overlay! g cols rows ctx-snapshot (:ctx-scroll db) (:expanded-facts db)))
          overlay-selectable-ranges (:selectable-ranges overlay-geom)
          sel (:mouse-selection db)
          overlay-sel? (= :overlay (:source sel))
          need-cells? (boolean sel)
          ;; F2 context panel (W3) is painted by the `overlay-geom` binding ABOVE
          ;; this `screen-cells` capture, so its text lands in the back buffer in
          ;; time to be copyable. The panel used to paint LAST (after capture),
          ;; which is exactly why its cells were never selectable.
          screen-cells (when need-cells? (capture-screen-cells screen cols rows))
          viewport (if overlay-sel?
                     {:viewport-top 0, :eff-scroll 0}
                     {:viewport-top text-top, :eff-scroll (:eff-scroll layout)})]
      (when overlay-geom
        (when (not= (:max-scroll overlay-geom) (:ctx-scroll-max db))
          (state/dispatch [:set-ctx-scroll-max (:max-scroll overlay-geom)])))
      (when sel
        (paint-selection! screen
          sel
          cols
          rows
          (if overlay-sel?
            overlay-selectable-ranges
            (selectable-ranges-for-source (:source sel)
              transcript-selectable-ranges
              input-selectable-ranges))
          viewport))
      ;; Inline highlight of in-session search hits. Runs AFTER
      ;; the main paint and AFTER mouse-selection overlay so a search
      ;; hit inside an actively-selected range still shows reverse
      ;; (the modifier is idempotent — stacking it doesn't double-flip).
      (paint-search-hits! screen layout text-top inner-h cols db)
      ;; Find bar (top-right overlay) + its prev/next/close click regions.
      ;; While the bar is ACTIVE it owns the keyboard, so the terminal cursor
      ;; moves INTO its query field — overriding the prompt-input placement
      ;; from `draw-bottom-chrome!` above. The prompt's logical cursor state
      ;; is untouched while searching, so closing the bar restores the cursor
      ;; to exactly where it sat in the input box.
      (when-let [[sx sy] (paint-search-bar! g cols text-top db)]
        (when-not (overlay-locked? db)
          (.setCursorPosition screen (TerminalPosition. sx sy))))
      ;; "↓ latest" jump-to-bottom chip (bottom-right) — only when the live bottom
      ;; is actually off-screen below (never in an empty/short session).
      (paint-jump-bottom! g cols messages-bottom (max 0 (- (long total-h) (long inner-h))) db)
      ;; Ctrl+H / F1 shortcut overlay paints LAST, on top of everything. It
      ;; registers its dedicated close-button click region, which commit-frame!
      ;; below publishes so the locked-overlay mouse branch can dismiss on click.
      (when (:help-open? db)
        (let [help-geom (components/help-overlay! g cols rows (:help-scroll db))]
          (when (not= (:max-scroll help-geom) (:help-scroll-max db))
            (state/dispatch [:set-help-scroll-max (:max-scroll help-geom)]))))
      (cr/commit-frame!)
      ;; Vim-style jump-label overlay for disclosures (C-x t). Painted AFTER
      ;; the commit so `cr/current` holds this frame's fresh toggle regions and
      ;; each letter badge lands on the chevron the user sees. No-op when off.
      (render/draw-detail-labels! g (:detail-labels-active? db))
      (when-not *skip-frame-refresh?*
        (.refresh screen Screen$RefreshType/DELTA)
        ;; Inline images ride the terminal's graphics layer, which Lanterna
        ;; doesn't model. Draw them AFTER the delta so they sit on top of the
        ;; blank cells the renderer reserved for each expanded `vis-image`.
        ;; `draw-messages-area!` recorded the EXACT painted position of each
        ;; image row into `image-sink`; only those whose full box fits the
        ;; viewport are placed.
        (paint-terminal-images!
          (fitting-image-placements @image-sink messages-top messages-bottom)))
      {:cols cols,
       :rows rows,
       :total-h total-h,
       :inner-h inner-h,
       :messages-top messages-top,
       :text-top text-top,
       :eff-scroll (:eff-scroll layout),
       :heights (:heights layout),
       :offsets (:offsets layout),
       ;; Did this frame place any inline terminal images? The scroll fast
       ;; path repaints only the messages band and does NOT re-place images
       ;; (they ride the terminal's own graphics layer, outside Lanterna's
       ;; cell model), so it bails to a full frame whenever this is true and
       ;; lets the full painter re-anchor the images to the scrolled rows.
       :has-images? (boolean (seq @image-sink)),
       :screen-cells screen-cells,
       :selectable-ranges selectable-ranges,
       :transcript-selectable-ranges transcript-selectable-ranges,
       :transcript-bubble-copy-regions transcript-bubble-copy-regions,
       :transcript-disclosure-copy-regions transcript-disclosure-copy-regions,
       :input-selectable-ranges input-selectable-ranges,
       :overlay-selectable-ranges overlay-selectable-ranges})))
(defn- repaint-chat-frame!
  "Repaint the full chat frame (background, transcript, working-area outline,
   input, footer) into the screen back-buffer using the CURRENT theme, WITHOUT
   flushing to the terminal. Modal dialogs run their own blocking loop while
   holding `draw-lock`, so the render thread is parked and the area behind /
   around the dialog is frozen on the theme that was active when the dialog
   opened. Calling this after a live theme change repaints that background with
   the new colors; the dialog's next `.refresh` pushes background + chrome as a
   single delta (no flicker). Best-effort — never throws into the modal loop."
  [^TerminalScreen screen]
  (try
    (binding [*skip-frame-refresh?* true]
      (let [size (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
            cols (.getColumns size)
            rows (.getRows size)]
        (render-frame! screen cols rows @state/app-db (System/currentTimeMillis))))
    (catch Throwable _ nil)))
(defn- live-progress-only-change?
  "True when the next frame only changed live progress bookkeeping.
   Those frames should keep the 80ms heartbeat but not repaint the header,
   footer, input, or stable transcript bubbles. Full renders still happen for
   scroll/input/settings/notification/layout changes."
  [previous-db db]
  (and previous-db
    (:loading? db)
    ;; Exclude `:tab-locals` — a BACKGROUND tab's spinner tick mutates it every
    ;; frame but never touches the active view, so without this a turn that runs
    ;; while other tabs are open (or streaming) fell to a FULL repaint on every
    ;; 80ms tick instead of the cheap partial-live path. partial-live still
    ;; repaints the header, so the background tab's spinner keeps animating.
    ;; Mirrors `active-view-unchanged?` / `scroll-only-change?`.
    (= (dissoc previous-db :progress :tab-locals :render-version :layout)
      (dissoc db :progress :tab-locals :render-version :layout))))
(defn- partial-live-frame?
  "True when the render loop may use the live-bubble-only repaint path."
  [previous-db db same-size? last-layout]
  (and (:loading? db)
    (not (:cancelling? db))
    same-size?
    last-layout
    (not (:mouse-selection db))
    (live-progress-only-change? previous-db db)))
(defn- scroll-only-change?
  "True when the ONLY thing that changed vs the last painted frame is the
   scroll position, AND the view is parked ABOVE the live bottom. In that
   state a full frame would repaint header/footer/input to byte-identical
   cells (their inputs didn't change) and hide the input cursor (`scrolled-up?`
   ⇒ nil) — so a messages-only repaint is pixel-identical to a full frame at a
   fraction of the cost. Anything that could make the fast path diverge —
   loading (live bubble grows), a mouse selection, an open overlay / find bar /
   jump-label mode, or the settle back to FOLLOW (which must restore the input
   cursor) — fails the test and falls through to the full painter. The scroll
   value itself is diffed out; every other db key must be equal."
  [prev-db db]
  (and prev-db
    (scroll/scrolled-up? (:scroll db))
    (not (:loading? db))
    (not (:mouse-selection db))
    (not (:tasks-open? db))
    (not (:help-open? db))
    (not (:detail-labels-active? db))
    (not (get-in db [:search :active?]))
    (not= (:scroll prev-db) (:scroll db))
    ;; Exclude the same churn keys the sibling predicates do: :tab-locals is
    ;; per-background-tab state (spinners, cursor blink) that ticks every frame
    ;; and never affects the active scroll view; :render-version / :layout are
    ;; render-thread bookkeeping. Mirrors `active-view-unchanged?`.
    (= (dissoc prev-db :scroll :tab-locals :render-version :layout)
      (dissoc db :scroll :tab-locals :render-version :layout))))
(defn- active-view-unchanged?
  "True when two app-db snapshots paint the SAME active view — they differ only
   in background tab state (`:tab-locals`), the dirty counter
   (`:render-version`), or the published `:layout`. The header-spinner-only fast
   path guards on this so it fires ONLY for a genuine background-spinner tick:
   any real change to the active view (a tab switch, a new message, a scroll)
   fails the test and falls through to a full frame, instead of repainting just
   the header and leaving the previous tab's body frozen on screen. Mirrors
   `state/active-view-slice` — keep the excluded keys in sync."
  [a b]
  (= (dissoc a :tab-locals :render-version :layout)
    (dissoc b :tab-locals :render-version :layout)))
(def ^:private header-hover-kinds #{:copy-id :workspace-entry :header-help :header-tasks :header-search :header-new-session})
(defn- header-hover-region? [region] (contains? header-hover-kinds (:kind region)))
(defn- header-hover-only-change?
  "True when a render bump only exists to repaint header hover chrome.

   Header affordance (`<id>`) lives outside the
   transcript body. Repainting the whole scrollback when the mouse enters
   or leaves those cells makes the body visibly flash. Body link hovers
   still take the full path for now, because their highlight row lives
   inside the virtualized transcript."
  [previous-db db previous-hover current-hover]
  (and previous-db
    (= (dissoc previous-db :render-version :layout) (dissoc db :render-version :layout))
    (or (header-hover-region? current-hover)
      (and (nil? current-hover) (header-hover-region? previous-hover)))))
(defn- live-loading-idx
  [messages loading?]
  (when (and loading? (seq messages) (= :assistant (:role (peek messages))))
    (long (dec (count messages)))))
(defn- render-header-hover-frame!
  "Cheap repaint for header-only hover changes.

   Do not begin/commit click regions here: geometry did not change, and
   the previous full frame's published regions remain authoritative.
   Bind header registration off so header-only redraws do not fill the
   staging buffer."
  [^TerminalScreen screen cols _rows db]
  (let [g (.newTextGraphics screen)]
    (binding [header/*register-click-regions?* false] (header/draw-header! g db 0 cols))
    (.refresh screen Screen$RefreshType/DELTA)))
(defn- render-scrollbar!
  [g cols bar-top inner-h track-h total-h eff-scroll]
  (scrollbar/draw! g
    {:col (- cols 2),
     :top bar-top,
     :track-h track-h,
     :total-h total-h,
     :inner-h inner-h,
     :scroll eff-scroll,
     :track-fg t/border-fg,
     :track-bg t/terminal-bg,
     :thumb-fg t/dialog-hint-key,
     :thumb-bg t/terminal-bg}))
(defn- render-live-bubble-frame!
  "Fast path for 80ms live ticks. Recompute virtual layout, but only
   repaint the live assistant bubble + the chrome bands that the user
   can actually interact with mid-turn (header, footer, input,
   scrollbar). Stable transcript bubbles above the live one are left
   alone - that's the optimization that keeps long traces from
   re-rendering every 80ms.

   Why repaint chrome here even though it's nominally stable:
   `(vis/notify! ...)` from a header click pushes a banner into the
   LEFT slot of the header. The notifications watcher bumps
   `:render-version` but leaves app-db otherwise untouched, so the
   render loop classifies the next frame as `partial-live?`. If we
   skipped header paint there, every header copy / footer status /
   cursor blink during a turn would feel frozen until the next full
   render. Header + footer + input boxes are cheap text ops; do
   them every tick. Click regions stay valid because the prior full
   frame's `cr/commit-frame!` is still authoritative - we don't
   begin/commit a new region pass here, so transcript chrome stays
   clickable too."
  [^TerminalScreen screen cols rows
   {:keys [messages input progress loading? cancelling? turn-start-ms settings slash-command-index],
    :as db} now-ms previous-layout]
  (let [g (.newTextGraphics screen)
        text-rows (input-text-rows input cols)
        input-box-h (+ text-rows 2 (* 2 render/input-pad-y))
        input-top (- rows input-box-h 2)
        echo-row (- input-top 1)
        ;; Geometry MUST match `render-frame!` exactly. The full path
        ;; reserves one empty terminal row between the header band and
        ;; the first transcript bubble via `(inc (header/header-rows db))`.
        ;; Dropping the `inc` here shifts the live bubble + scrollbar
        ;; track + inner-h by one row vs the previous full frame, which
        ;; (a) leaves a stale row of bubble border under the header on
        ;; every live↔full flip (the `[]` artifacts visible when a turn
        ;; ends), (b) changes inner-h so the virtual layout clamps
        ;; `eff-scroll` against a different ceiling — content visibly
        ;; jumps down when new iterations arrive mid-scroll, and
        ;; (c) misaligns click regions published by the prior full frame
        ;; with the partial-live re-paint, so a second click on a
        ;; collapsible disclosure misses its toggle target.
        messages-top (inc (header/header-rows db))
        messages-bottom echo-row
        bubble-w (max 1 (- cols render/MESSAGE_SIDE_PAD))
        inner-h (max 0 (- messages-bottom messages-top 2))
        ;; Same `:scroll`-variant → concrete-offset derivation as the
        ;; full-frame path (see `render-frame!`).
        prev-max-s (max 0 (- (long (or (:total-h (:layout db)) inner-h)) inner-h))
        messages-scroll (scroll/layout-offset (:scroll db) prev-max-s)
        text-top (+ messages-top render/MESSAGE_MARGIN_TOP)
        header-top 0
        ;; Footer is two rows tall (model + limits). Shortcut chrome is a
        ;; closed helper cell above input; input top border is omitted. Must
        ;; match full render geometry.
        footer-row (- rows 2)
        progress-extra {:now-ms now-ms,
                        :turn-start-ms turn-start-ms,
                        :cancelling? (boolean cancelling?),
                        :viewport-rows inner-h,
                        :pending-sends (:pending-sends db)}
        layout (virtual/layout
                 messages
                 bubble-w
                 settings
                 messages-scroll
                 inner-h
                 {:progress progress, :loading? loading?, :progress-extra progress-extra}
                 {:session-id (get-in db [:session :id]),
                  :detail-expansions (:detail-expansions db)
                  ;; Anchor for paint parity with the full-frame path.
                  ;; This partial path doesn't republish `:offsets`, so
                  ;; the next full frame persists the corrected scroll.
                  :prev-offsets (get-in db [:layout :offsets])})
        ;; In-session search consumes its pending scroll target
        ;; here — the layout's `:offsets` vec gives the Y of any
        ;; message-idx in O(1). One-shot: clear pending so subsequent
        ;; frames don't re-scroll.
        _ (when-let [target (:scroll-to-message-pending db)]
            (let [offsets (:offsets layout)
                  total-h (long (:total-h layout))
                  max-s (max 0 (- total-h inner-h))]
              (when (and (vector? offsets) (< target (count offsets)))
                (state/dispatch [:set-scroll (max 0 (min max-s (long (nth offsets target))))]))
              (state/dispatch [:scroll-to-message-resolved])))
        live-idx (live-loading-idx messages loading?)
        live-entry (first (filter #(= live-idx (:idx %)) (:visible layout)))
        old-entry (first (filter #(= live-idx (:idx %)) (:visible previous-layout)))]
    ;; Diagnostic: capture per-tick geometry so we can see whether
    ;; iteration 2/3 actually reach the painter. Fires only when
    ;; the bubble's height changed since the last frame (keeps log
    ;; tight — spinner ticks without content change skip).
    (when (and live-entry (not= (long (or (:height old-entry) -1)) (long (:height live-entry))))
      ;; Per-tick geometry diagnostic. Fires every paint where the
      ;; live bubble's height changed — several times per second
      ;; while an iteration is streaming. At `:info` it dominated
      ;; vis.log alongside the mouse-event flood. Demote to `:debug`
      ;; so the default file handler drops it; flip min-level to
      ;; `:debug` to re-enable.
      (tel/log! {:level :debug,
                 :id ::live-bubble-tick,
                 :data {:iteration-count (count (or (:iterations progress) [])),
                        :total-h (long (:total-h layout)),
                        :inner-h inner-h,
                        :eff-scroll (long (:eff-scroll layout)),
                        :live-top (long (:top live-entry)),
                        :live-h (long (:height live-entry)),
                        :old-top (some-> old-entry
                                   :top
                                   long),
                        :old-h (some-> old-entry
                                 :height
                                 long),
                        :messages-scroll messages-scroll}}))
    ;; ── Click-region republish (live-aware) ────────────────────
    ;; The previous frame's `regions-atom` is used by `cr/lookup` while
    ;; partial frames tick. That works for STABLE bubbles (their pixels
    ;; aren't repainted, so old regions still match what's on screen),
    ;; but the LIVE bubble grows: every new iteration shifts its
    ;; toggle-details / link rows down, so a region registered at the
    ;; previous full frame ends up pointing at the wrong row. The user
    ;; clicks the disclosure they SEE and nothing happens.
    ;;
    ;; Fix: stage a fresh frame. Carry over every region NOT painted
    ;; by something we're about to repaint (header chrome + the live
    ;; bubble's previous row range). Then re-paint live bubble + header
    ;; and commit. Stable transcript bubbles keep their previous-frame
    ;; regions verbatim because we don't touch their rows here.
    (let [prev-live-entry old-entry
          prev-text-top   (long (or (:text-top previous-layout) text-top))
          live-row-band   (when prev-live-entry
                            (let [lo (+ prev-text-top (long (:top prev-live-entry)))
                                  hi (+ lo (long (:height prev-live-entry)))]
                              [lo hi]))
          header-rows-n   (long (header/header-rows db))
          carry-over      (vec
                            (remove
                              (fn [{:keys [bounds]}]
                                (let [row (long (:row bounds))]
                                  (or
                                    ;; header re-registers below
                                    (< row header-rows-n)
                                    ;; live bubble re-registers below
                                    (and live-row-band
                                      (>= row (first live-row-band))
                                      (< row (second live-row-band)))
                                    ;; footer re-registers below (its button
                                    ;; click-regions: dirs / resources) — drop
                                    ;; stale copies so the fresh ones win.
                                    (>= row (long footer-row)))))
                              (cr/current)))]
      (cr/begin-frame!)
      (doseq [r carry-over] (cr/register! r))
      (when live-entry
        (let [clip (.newTextGraphics g (TerminalPosition. 0 text-top) (TerminalSize. cols inner-h))
              y0 (max 0 (min (long (:top live-entry)) (long (or (:top old-entry) (:top live-entry)))))
              y1 (min inner-h
                   (max (+ (long (:top live-entry)) (long (:height live-entry)))
                     (+ (long (or (:top old-entry) (:top live-entry)))
                       (long (or (:height old-entry) (:height live-entry))))))]
          (when (< y0 y1)
            (p/set-colors! clip t/text-fg t/terminal-bg)
            (p/fill-rect! clip 0 y0 cols (- y1 y0)))
          (render/draw-chat-bubble! clip
            (:projected live-entry)
            (:top live-entry)
            render/MESSAGE_MARGIN_LEFT
            bubble-w
            {:viewport-top text-top, :viewport-h inner-h})))
      ;; Chrome refresh - cheap text writes, kept inside the partial
      ;; path so notification banners and footer status update on
      ;; every spinner tick instead of waiting for the next full
      ;; frame. Header re-registers its click rectangles into the
      ;; staged frame above so they survive the upcoming
      ;; `cr/commit-frame!`.
      (header/draw-header! g db header-top cols)
      ;; Find bar overlay + its prev/next/close click regions, staged into
      ;; THIS frame so they stay clickable while a turn streams.
      (paint-search-bar! g cols text-top db)
      ;; Footer painted INSIDE the staged frame so its button click-regions
      ;; (dirs / resources) are published by `commit-frame!` below. Painting it
      ;; AFTER the commit (as before) left those regions in an uncommitted
      ;; staging buffer that the next `begin-frame!` dropped — so the footer
      ;; buttons were never clickable on the live/partial render path.
      (footer/draw-echo-area! g db echo-row cols now-ms)
      (footer/draw-footer! g db footer-row cols now-ms)
      (cr/commit-frame!))
    (let [[cx cy]
          (render/draw-input-box! g input input-top text-rows cols nil)]
      (if-let [[sx sy] (components/find-bar-cursor cols text-top (:search db))]
        ;; Active find bar owns the keyboard — cursor sits in its query field
        ;; (see the full-frame path / `find-bar-cursor` for the why).
        (.setCursorPosition screen (TerminalPosition. sx sy))
        (if (scroll/scrolled-up? (:scroll db))
          (.setCursorPosition screen nil)
          (.setCursorPosition screen (TerminalPosition. cx cy)))))
    ;; Suggestion popup (slash commands / inline `@` file picker) is drawn
    ;; just above the input box, in rows the live-bubble repaint above
    ;; overdraws. Re-paint it here every tick so it stays on top instead of
    ;; flickering / vanishing under the streaming bubble; empty suggestions
    ;; no-op, and non-`/`/`@` input returns empty cheaply (no index crawl).
    (render/draw-slash-command-suggestions!
      g
      (slash-suggestions-for-input screen input slash-command-index)
      input-top
      cols
      slash-command-index)
    (render-scrollbar! g
      cols
      messages-top
      inner-h
      (- messages-bottom messages-top)
      (:total-h layout)
      (:eff-scroll layout))
    ;; Search hit highlights: painted on the live path too so they survive
    ;; streaming ticks instead of only appearing on full frames.
    (paint-search-hits! screen layout text-top inner-h cols db)
    (.refresh screen Screen$RefreshType/DELTA)
    (merge previous-layout
      {:cols cols,
       :rows rows,
       :total-h (long (:total-h layout)),
       :inner-h inner-h,
       :messages-top messages-top,
       :text-top text-top,
       :eff-scroll (:eff-scroll layout),
       :heights (:heights layout),
       :offsets (:offsets layout),
       :visible (:visible layout)})))
(defn- render-scroll-frame!
  "Fast path for a pure history scroll (see `scroll-only-change?`): the user is
   parked/easing ABOVE the live bottom and NOTHING but the scroll offset
   changed. Repaint ONLY the messages viewport + scrollbar + jump-to-bottom
   chip. `draw-messages-area!` fills its own band, opens a fresh click-region
   frame, draws the visible bubbles and the scrollbar, and re-registers the
   transcript click regions; we then carry over the previous frame's header /
   footer / input click regions (their rows aren't repainted, so their regions
   must survive `commit-frame!`) and hide the input cursor to match
   `draw-bottom-chrome!` under `scrolled-up?`.

   Header, footer, input box and the background OUTSIDE the messages band are
   left exactly as the previous frame painted them — their inputs didn't
   change, so a full repaint would produce byte-identical cells and the
   Lanterna delta emits nothing for them. That skips the bulk of a full scroll
   frame (static chrome rebuild) while staying pixel-identical to
   `render-frame!` for this state; force it off with `force-full-frame?`."
  [^TerminalScreen screen cols rows
   {:keys [messages input progress settings] :as db} now-ms previous-layout]
  (let [g (.newTextGraphics screen)
        ;; Geometry MUST match render-frame! / render-live-bubble-frame!
        ;; EXACTLY — a one-row slip shifts the viewport + scrollbar and reads
        ;; as a jump on the fast↔full flip (see render-live-bubble-frame!).
        text-rows (input-text-rows input cols)
        input-box-h (+ text-rows 2 (* 2 render/input-pad-y))
        input-top (- rows input-box-h 2)
        echo-row (- input-top 1)
        messages-top (inc (header/header-rows db))
        messages-bottom echo-row
        bubble-w (max 1 (- cols render/MESSAGE_SIDE_PAD))
        inner-h (max 0 (- messages-bottom messages-top 2))
        prev-max-s (max 0 (- (long (or (:total-h (:layout db)) inner-h)) inner-h))
        messages-scroll (scroll/layout-offset (:scroll db) prev-max-s)
        text-top (+ messages-top render/MESSAGE_MARGIN_TOP)
        progress-extra {:now-ms now-ms,
                        :turn-start-ms (:turn-start-ms db),
                        :cancelling? false,
                        :viewport-rows inner-h,
                        :pending-sends (:pending-sends db)}
        layout (virtual/layout messages bubble-w settings messages-scroll inner-h
                 {:progress progress, :loading? false, :progress-extra progress-extra}
                 {:session-id (get-in db [:session :id]),
                  :detail-expansions (:detail-expansions db),
                  :prev-offsets (get-in db [:layout :offsets])})
        anchored-scroll (:anchored-scroll layout)]
    ;; Anchor correction, exactly as render-frame! — estimate→real height fixes
    ;; keep the scrolled content visually put instead of lurching.
    (when (and (some? anchored-scroll) (some? messages-scroll)
            (not= anchored-scroll messages-scroll))
      (state/dispatch [:reanchor-scroll anchored-scroll
                       (- (long anchored-scroll) (long messages-scroll))]))
    (render/draw-messages-area! g layout messages-top messages-bottom cols)
    ;; Carry over chrome click regions (rows OUTSIDE the messages band). The
    ;; transcript regions in-band were just re-registered by draw-messages-area!
    ;; at their new scrolled rows, so they are deliberately NOT carried.
    (doseq [r (cr/current)]
      (let [row (long (:row (:bounds r)))]
        (when (or (< row messages-top) (>= row messages-bottom))
          (cr/register! r))))
    ;; Jump-to-bottom chip is scroll-dependent and registers its own click
    ;; region — repaint it every scroll frame, BEFORE commit.
    (paint-jump-bottom! g cols messages-bottom (max 0 (- (long (:total-h layout)) inner-h)) db)
    (cr/commit-frame!)
    ;; scrolled-up? ⇒ input cursor hidden (matches draw-bottom-chrome!).
    (.setCursorPosition screen nil)
    (.refresh screen Screen$RefreshType/DELTA)
    (merge previous-layout
      {:cols cols, :rows rows,
       :total-h (long (:total-h layout)), :inner-h inner-h,
       :messages-top messages-top, :text-top text-top,
       :eff-scroll (:eff-scroll layout),
       :heights (:heights layout), :offsets (:offsets layout),
       :visible (:visible layout)})))

;;; ── Render thread ───────────────────────────────────────────────────────────────
(def ^:private spinner-tick-ms
  "How often the live turn frame advances while a turn is in flight. Drives
   both the wait-timeout cap and the animate? predicate, so a quiet render
   thread still repaints on the live heartbeat. Keep this at 80ms: higher
   values make streamed reasoning feel frozen, lower values waste work."
  80)
(def ^:private scroll-anim-tick-ms
  "Frame budget for smooth-scroll interpolation: while an ease is in
   flight the render loop wakes every N ms to dispatch `:ease-scroll`
   (steps the on-screen `:scroll` `:pos` toward its desired row) and
   repaint. 16ms = ~60fps, the largest tick that still reads as
   continuous motion on the terminal cell grid. Bigger ticks feel
   chunky; smaller ticks burn CPU for no perceptual gain."
  16)
(defn- scroll-anim-active?
  "True when the `:scroll` ease hasn't reached its desired row yet, using
   the previously published layout's max-scroll. Drives the fast tick
   cadence; settles to false so an idle view stops repainting."
  [db]
  (let [ly (:layout db)
        total-h (long (or (:total-h ly) 0))
        inner-h (long (or (:inner-h ly) 0))
        max-s (max 0 (- total-h inner-h))]
    (scroll/animating? (:scroll db) max-s)))
(def ^:private force-full-frame?
  "When set (env `VIS_FORCE_FULL_FRAME`) every render takes the FULL frame
   path — all incremental fast paths (scroll, header-hover, partial-live,
   header-spinner) are disabled. A production escape hatch: if a fast path ever
   mis-paints on some terminal, this restores the always-full-repaint behaviour
   without a rebuild."
  (some? (System/getenv "VIS_FORCE_FULL_FRAME")))
(defn- render-loop!
  "The render thread's main loop. Sleeps on `state/render-monitor` and
   only paints when `:render-version` advances, the terminal gets
   resized, or - while loading - the spinner frame advances. Skips
   painting entirely while a dialog is up by failing to acquire
   `draw-lock`."
  [^TerminalScreen screen]
  (loop [last-v -1
         last-cols -1
         last-rows -1
         last-frame-ms 0
         last-db nil
         last-layout nil
         last-hover nil
         was-blocked? false]
    (let [db @state/app-db]
      ;; Advance the scroll ease BEFORE acquiring draw-lock so the re-read
      ;; inside the try/let below paints the freshly stepped offset. One
      ;; `:ease-scroll` covers BOTH smooth wheel/key animation AND
      ;; stick-to-bottom follow: in FOLLOW the desired row is the growing
      ;; bottom, so streamed content eases in; in AT the user's parked row
      ;; is fixed, so they're never yanked. Pulse it while a turn streams
      ;; (content grows every frame) OR whenever an ease is still in
      ;; flight; idle + settled dispatches nothing, so the view stops
      ;; repainting instead of livelocking on render-version bumps.
      (let [ly (:layout db)]
        (when (and ly (:total-h ly) (:inner-h ly)
                (or (:loading? db) (scroll-anim-active? db)))
          (state/dispatch [:ease-scroll (:total-h ly) (:inner-h ly)])))
      (when-not (:shutdown? db)
        (let [version (long (or (:render-version @state/app-db) 0))
              ;; tryLock so a dialog session (which holds the lock for
              ;; seconds) doesn't pin us. Time out fast and re-poll.
              got-lock? (.tryLock draw-lock 50 TimeUnit/MILLISECONDS)
              [rendered? new-cols new-rows new-frame-ms rendered-db rendered-layout rendered-hover
               new-was-blocked?]
              (if-not got-lock?
                  ;; Lock contention while a dialog session paints onto
                  ;; Lanterna's back buffer. Remember it so the next
                  ;; successful render forces a full repaint over the
                  ;; dialog cells - the partial-live / header-hover-only
                  ;; paths only touch a subset of rows and would leave
                  ;; the dialog overlay ghosted on screen.
                [false last-cols last-rows last-frame-ms last-db last-layout last-hover true]
                (try
                    ;; Re-read AFTER acquiring the lock - dialog state
                    ;; could have flipped while we were waiting.
                  (let [db @state/app-db
                        size (screen-size screen)
                        cols (.getColumns size)
                        rows (.getRows size)
                        now-ms (System/currentTimeMillis)
                        loading? (boolean (:loading? db))
                        any-loading? (or loading? (state/any-background-loading? db))
                        scroll-anim? (scroll-anim-active? db)
                          ;; F1 (help) / F2 (context) overlays LOCK the
                          ;; background: while one is up we suppress every
                          ;; incremental repaint path (spinner tick, scroll
                          ;; ease, header-hover, partial-live). The overlay
                          ;; paints once on the version bump that opened it
                          ;; and then sits still — no streaming bubble redraw
                          ;; underneath flickering through it. A real change
                          ;; (overlay toggle, live ctx update, resize) still
                          ;; bumps :render-version and forces ONE full frame
                          ;; that repaints the overlay cleanly on top.
                        overlay-open? (overlay-locked? db)
                        animate? (and (not overlay-open?)
                                   (or (and any-loading?
                                         (>= (- now-ms (long last-frame-ms)) spinner-tick-ms))
                                     scroll-anim?))
                        same-size? (and (= last-cols cols) (= last-rows rows))
                        current-hover (cr/hovered)
                          ;; Force a full repaint on the first iteration
                          ;; after a dialog session held draw-lock: see
                          ;; the no-got-lock branch above.
                        header-hover-only?
                        (and (not force-full-frame?)
                          (not overlay-open?)
                          same-size?
                          last-layout
                          (not animate?)
                          (not was-blocked?)
                          (header-hover-only-change? last-db db last-hover current-hover))
                        partial-live? (and (not force-full-frame?)
                                        (not overlay-open?)
                                        (not was-blocked?)
                                        (partial-live-frame? last-db
                                          db
                                          same-size?
                                          last-layout))
                        header-spinner-only?
                        (and (not force-full-frame?)
                          (not overlay-open?)
                          same-size?
                          last-layout
                          (not was-blocked?)
                          (not loading?)
                          (not scroll-anim?)
                          ;; ONLY a background spinner tick — the active view is
                          ;; byte-for-byte the last rendered one. Without this
                          ;; guard a tab switch (or any version bump) while a
                          ;; background tab streams would repaint just the header
                          ;; and leave the previous tab's body on screen.
                          (active-view-unchanged? last-db db)
                          (state/any-background-loading? db))
                        ;; Pure history scroll: repaint only the messages band
                        ;; + scrollbar, skip the static chrome. Excludes image
                        ;; sessions (images ride the terminal graphics layer and
                        ;; must be re-anchored by the full painter on scroll).
                        scroll-frame?
                        (and (not force-full-frame?)
                          (not overlay-open?)
                          same-size?
                          last-layout
                          (not was-blocked?)
                          (not (:has-images? last-layout))
                          (scroll-only-change? last-db db))]
                    (if (and (not (:shutdown? db))
                          (not (:dialog-open? db))
                          (or (not= last-v version)
                            (not= last-cols cols)
                            (not= last-rows rows)
                            animate?
                            was-blocked?))
                      (let [[layout publish-layout?]
                            (cond header-hover-only?
                              (do (render-header-hover-frame! screen cols rows db)
                                [last-layout false])
                              partial-live? [(render-live-bubble-frame! screen
                                               cols
                                               rows
                                               db
                                               now-ms
                                               last-layout) true]
                              header-spinner-only?
                              (do (render-header-hover-frame! screen cols rows db)
                                [last-layout false])
                              scroll-frame?
                              [(render-scroll-frame! screen cols rows db now-ms last-layout) true]
                              :else [(render-frame! screen cols rows db now-ms) true])]
                          ;; Publish layout back to app-db without bumping the version (see
                          ;; no-render-bump-events).
                        (when publish-layout? (state/dispatch [:set-layout layout]))
                          ;; Terminal WIDTH change: every sticky height is keyed
                          ;; by the old bubble-w, so the whole transcript just
                          ;; fell back to estimates. Re-warm in the background so
                          ;; total-h re-settles while the user is idle instead of
                          ;; correcting - and jumping the scrollbar thumb - on
                          ;; their next scroll-up. Skipped on the first frame
                          ;; (last-cols -1): the startup path already warms.
                        (when (and (not= last-cols cols) (pos? (long last-cols)))
                          (virtual/rewarm!
                            (:messages db)
                            (max 1 (- cols render/MESSAGE_SIDE_PAD))
                            (or (:settings db) {})
                            {:session-id (get-in db [:session :id]),
                             :detail-expansions (:detail-expansions db),
                             :on-warm #(state/dispatch [:bump-render-version])}))
                          ;; Cleared was-blocked? - the dialog overlay has just been painted
                          ;; over.
                        [true cols rows now-ms db layout current-hover false])
                        ;; Dialog still open (or nothing to paint) - keep
                        ;; the blocked flag sticky until we actually paint.
                      [false cols rows last-frame-ms last-db last-layout last-hover
                       (or was-blocked? (boolean (:dialog-open? db)))]))
                  (catch Throwable t
                      ;; Drawing must never crash the thread - a stray
                      ;; resize race or null cell will recover next frame.
                    (tel/log! {:level :warn,
                               :id ::render-frame-failed,
                               :data (throwable-log-data t),
                               :msg (str "render frame failed: " (or (ex-message t) (str t)))})
                    [false last-cols last-rows last-frame-ms last-db last-layout last-hover
                     was-blocked?])
                  (finally (.unlock draw-lock))))]
          (when-not rendered?
            ;; Park until the next dispatch wakes us, or until the
            ;; spinner needs to tick. Idle sessions sleep up to
            ;; ~250ms (defensive cap on lost wakeups); active queries
            ;; sleep no longer than the spinner tick so the animation
            ;; stays smooth without spamming repaints.
            (locking state/render-monitor
              (let [v-now (long (or (:render-version @state/app-db) 0))
                    loading? (or (boolean (:loading? @state/app-db))
                               (state/any-background-loading? @state/app-db))]
                (when (= v-now version)
                  (try (.wait ^Object state/render-monitor
                         (long (cond (scroll-anim-active? @state/app-db) scroll-anim-tick-ms
                                 loading? spinner-tick-ms
                                 :else 250)))
                    (catch InterruptedException _ nil))))))
          (recur (if rendered? version last-v)
            (long (or new-cols last-cols))
            (long (or new-rows last-rows))
            (long new-frame-ms)
            rendered-db
            rendered-layout
            rendered-hover
            (boolean new-was-blocked?)))))))
(defn- start-render-thread!
  "Spawn the render thread. Daemon so the JVM can still exit even if a
   bug ever traps it in the loop."
  ^Thread [^TerminalScreen screen]
  (let [t (Thread. ^Runnable (fn [] (render-loop! screen)) "vis-channel-tui-render")]
    (.setDaemon t true)
    (.start t)
    t))
(def ^:private provider-limits-refresh-ms 60000)
(defn- active-provider-id
  []
  (or
    ;; Poll the provider the ACTIVE SESSION routes through — the unified
    ;; per-session pref the footer model label and the engine already use.
    ;; Polling the global router default (resolve-effective-model) made the
    ;; usage row fetch the wrong plan's limits after a per-session switch.
    (when-let [sid (get-in @state/app-db [:session :id])]
      (some-> (vis/gateway-session-model sid) :provider not-empty keyword))
    (when-let [router (try (vis/get-router) (catch Throwable _ nil))]
      (some-> (try (vis/resolve-effective-model router) (catch Throwable _ nil))
        :provider))))
(defn- start-provider-limits-thread!
  "Refresh provider limit metadata outside the render thread."
  ^Thread []
  (let [t (Thread.
            ^Runnable
            (fn []
              (loop [last-provider-id nil
                     last-refresh-ms 0]
                (when-not (:shutdown? @state/app-db)
                  (let [now-ms (System/currentTimeMillis)
                        provider-id (active-provider-id)
                        changed? (not= provider-id last-provider-id)
                        stale? (>= (- now-ms (long last-refresh-ms)) provider-limits-refresh-ms)]
                    (try (cond (nil? provider-id) (when last-provider-id
                                                    (state/dispatch [:clear-provider-limits]))
                           (or changed? stale?) (state/dispatch
                                                  [:set-provider-limits provider-id
                                                   (vis/provider-limits provider-id)]))
                      (catch Throwable t
                        (tel/log! {:level :warn,
                                   :id ::provider-limits-refresh-failed,
                                   :data {:provider provider-id,
                                          :error (or (ex-message t) (str t))},
                                   :msg "Provider limits refresh failed"})))
                    (try (Thread/sleep 1000) (catch InterruptedException _ nil))
                    (recur provider-id (if (or changed? stale?) now-ms last-refresh-ms))))))
            "vis-channel-tui-provider-limits")]
    (.setDaemon t true)
    (.start t)
    t))
(defn- format-session-not-found
  "Build a friendly multi-line message for `--session-id` misses,
   listing the most recent sessions so the user has
   something to copy-paste."
  [cid]
  (let [available (try (vec (take 10 (vis/gateway-list-sessions :all))) (catch Throwable _ []))
        line (fn [c]
               (let [id-str (str (:id c))
                     id8 (if (>= (count id-str) 8) (subs id-str 0 8) id-str)
                     title (let [t (:title c)] (when-not (str/blank? t) t))]
                 (str "  " id8 "  " (or title "(untitled)"))))]
    (str "Session not found: "
      cid
      (if (seq available)
        (str "\n\nAvailable sessions (most recent first):\n"
          (str/join "\n" (map line available))
          "\n\nUse the 8-char prefix or full UUID with --session-id.")
        "\n\nNo sessions exist yet - run `vis channels tui` without --session-id first."))))
(defn- current-session-id
  []
  (some-> @state/app-db
    :session
    :id
    str))
(defn- workspace-sessions
  []
  (let [db @state/app-db]
    (->> (concat (keep :session (vals (:tab-locals db))) [(:session db)])
      (filter :id)
      (reduce (fn [{:keys [seen out], :as acc} session]
                (let [id (:id session)] ; UUID; hashes + equality work natively
                  (if (contains? seen id) acc {:seen (conj seen id), :out (conj out session)})))
        {:seen #{}, :out []})
      :out)))
(defn- register-shutdown-hook!
  "Thin wrapper over `Runtime/addShutdownHook` so call-sites read as
   plain Clojure instead of a `(Thread. ^Runnable (fn [] ...))` casting
   ritual. `f` is a zero-arg fn; thrown exceptions are swallowed (the
   hook chain MUST NOT propagate - a single noisy listener can hang
   the whole shutdown sequence). Returns the registered Thread so
   tests can deregister it via `.removeShutdownHook` if needed."
  [^Runnable f]
  (let [hook (Thread. ^Runnable (fn [] (try (f) (catch Throwable _ nil))))]
    (.addShutdownHook (Runtime/getRuntime) hook)
    hook))
(defn- terminal-interrupt-action
  "Action for terminal-level interrupts (SIGINT from Ctrl+C, SIGTSTP from
   Ctrl+Z) that never reach Lanterna as KeyStrokes on some terminals.
   Match the in-band Ctrl+C contract: first interrupt clears a draft; the
   next interrupt on an empty editor exits the TUI."
  [db]
  (if (input-empty? (:input db)) :quit :clear-input))
(defn- handle-terminal-interrupt!
  []
  (case (terminal-interrupt-action @state/app-db)
    :clear-input (state/dispatch [:reset-input])
    :quit (state/dispatch [:shutdown])))
(defn- register-terminal-signal-handler!
  [signal-name f]
  (try (let [signal (Signal. signal-name)
             previous (Signal/handle signal
                        (reify
                          SignalHandler
                          (handle [_ _signal] (try (f) (catch Throwable _ nil)))))]
         (fn [] (try (Signal/handle signal previous) (catch Throwable _ nil))))
    (catch IllegalArgumentException _ nil)
    (catch Throwable _ nil)))
(defn- register-terminal-interrupt-handlers!
  []
  (let [cleanups (keep #(register-terminal-signal-handler! % handle-terminal-interrupt!)
                   ["INT" "TSTP"])]
    (fn [] (doseq [cleanup cleanups] (cleanup)))))
(defn- subscribe-title-listener!
  "Wire host title writes (`titling/set-title-with-broadcast!` — async
   auto-title generation or a rename) for this session to the TUI header:
   every change dispatches `[:set-title]`
   into app-db so the next render frame paints the new title without
   polling. Returns a zero-arg cleanup fn.

   One listener stays subscribed per OPEN session (not just the active one),
   so a background tab's async auto-title lands live - `:set-title` carries
   the session-id and relabels the owning tab even while it's unfocused."
  [session-id]
  (let [session-id (str session-id)
        listener (vis/add-title-listener! session-id
                   (fn [new-title]
                     ;; Dispatch for EVERY session (focused or not), carrying our
                     ;; session-id so `:set-title` can relabel the owning tab even
                     ;; when it's in the background.
                     (state/dispatch [:set-title (or new-title "") session-id])))
        ;; Host signals when auto-title generation starts/ends so the
        ;; header can spinner the active tab. Scoped to the active session,
        ;; same as the value listener — a background generation must not
        ;; spinner the tab you're looking at.
        pending-listener (vis/add-title-pending-listener! session-id
                           (fn [pending?]
                             (when (= session-id (current-session-id))
                               (state/dispatch [:title-loading (boolean pending?)]))))
        cleanup #(do (vis/remove-title-listener! session-id listener)
                   (vis/remove-title-pending-listener! session-id pending-listener))]
    (register-shutdown-hook! cleanup)
    cleanup))
(defn- date->millis
  [v]
  (cond (instance? java.util.Date v) (.getTime ^java.util.Date v)
    (instance? java.time.Instant v) (.toEpochMilli ^java.time.Instant v)
    (number? v) (long v)
    :else nil))
(defn- latest-turn-created-at
  [turns]
  (->> turns
    (keep :created-at)
    (sort-by #(or (date->millis %) 0))
    last))
(defn- session-summary
  [_db-info session]
  (let [turns (try (vec (vis/gateway-list-turns (:id session))) (catch Throwable _ []))
        modified-at (or (latest-turn-created-at turns) (:created-at session))]
    (assoc session
      :turn-count (count turns)
      :modified-at modified-at)))
(defn- empty-untitled-session?
  [{:keys [title turn-count]}]
  (and (not (pos? (long (or turn-count 0))))
    (or (str/blank? (str title))
      (#{"untitled" "untitled session"}
       (str/lower-case (str/trim (str title)))))))
(defn- session-sort-key
  "Default session picker ordering.

   Prefer sessions with real turns, then newest latest-turn/modified time,
   then higher turn count. This keeps the latest active session first while
   still pushing empty/new shells behind sessions with history."
  [{:keys [turn-count modified-at created-at]}]
  [(if (pos? (long (or turn-count 0))) 1 0)
   (or (date->millis modified-at) (date->millis created-at) 0) (long (or turn-count 0))])
(defn- latest-modified-first
  [sessions]
  (sort-by session-sort-key (fn [a b] (compare b a)) sessions))
(defn- tui-session-summaries
  []
  (try (let [db-info (vis/db-info)]
         (->> (vis/gateway-list-sessions :all)
           (map #(session-summary db-info %))
           latest-modified-first
           vec))
    (catch Throwable _ [])))
(defn- session-db-title
  [session-id]
  (when-let [session (try (vis/gateway-soul session-id) (catch Throwable _ nil))]
    (let [title (:title session)] (when-not (str/blank? (str title)) (str title)))))
(defn- session-workspace
  "The rift draft pinned to `session-id` — a workspace record whose
   `:root` is the clone path. Lets the TUI display layer (footer / badge)
   reflect the actual draft instead of falling back to trunk."
  [session-id]
  (try
    (vis/gateway-session-workspace session-id)
    (catch Throwable _ nil)))

(defn- abbrev-home
  "Shorten an absolute path by replacing the user's home dir with `~`."
  [^String p]
  (let [home (System/getProperty "user.home")]
    (if (and p home (str/starts-with? p home)) (str "~" (subs p (count home))) (str p))))

(defn- short-dir
  "A compact, IDENTIFYING directory label: home-abbreviated, and when still
   long, keeping the last two path segments (`…/parent/dir`) so two projects
   never collapse to the same head-truncated prefix in the navigator."
  [^String p]
  (when p
    (let [p (abbrev-home p)]
      (if (<= (count p) 22)
        p
        (let [segs (remove str/blank? (str/split p #"/"))]
          (if (<= (count segs) 2) p (str "…/" (str/join "/" (take-last 2 segs)))))))))

(defn- enrich-session-row
  "Attach `:draft-label` + `:work-dir` to a session summary for the navigator,
   read from the session's pinned workspace. Working dir = the project root
   the session edits in: for a draft that's the trunk it was cloned from
   (`:repo-root`); for trunk it's the root itself. `:draft-label` is nil on
   trunk."
  [s]
  (let [ws     (session-workspace (:id s))
        draft? (some? (:fork-ms ws))]
    (assoc s
      :draft-label (when draft? (or (not-empty (:label ws)) "draft"))
      :work-dir    (short-dir (or (:repo-root ws) (:root ws))))))

(defn- persist-tabs!
  "Snapshot the current open-tab set + active tab and persist it for this
   launch directory, so the tabs come back next time vis opens here."
  []
  (tabs/save! (state/tab-session-snapshot @state/app-db)))

(defn- init-visible-session!
  "Install a session into app-db and repaint the workspace strip. Returns the\n   cleanup fn for that session's title listener."
  [{:keys [id history]}]
  (state/dispatch [:init-session {:id id} history (session-workspace id)])
  (state/dispatch [:set-title (or (session-db-title id) "")])
  (subscribe-title-listener! id))
(defn- terminal-ctrl-c-behaviour
  "Lanterna's 3-arg UnixTerminal constructor defaults to
   CTRL_C_KILLS_APPLICATION. In raw mode Ctrl+C is decoded as a
   KeyStroke, then UnixLikeTerminal calls System/exit from pollInput
   before our input handler can clear the draft. TRAP preserves the
   KeyStroke so `input/handle-key` owns the first-Ctrl+C contract."
  []
  UnixLikeTerminal$CtrlCBehaviour/TRAP)
(defn- create-terminal!
  [_opts]
  (UnixTerminal. @vis/tty-in @vis/tty-out (Charset/defaultCharset) (terminal-ctrl-c-behaviour)))
(defn- configure-terminal-input!
  [terminal _opts]
  (when (instance? UnixTerminal terminal)
    (input/register-custom-patterns! terminal)
    (try (.setMouseCaptureMode terminal MouseCaptureMode/CLICK_RELEASE_DRAG_MOVE)
      (catch Throwable _ nil))))
(defn- enable-terminal-escape-modes!
  [_opts]
  (input/enable-bracketed-paste! @vis/tty-out)
  (input/enable-sgr-mouse! @vis/tty-out)
  ;; Free Ctrl+V from the tty's "literal next" (VLNEXT) so Emacs `C-v` page-scroll
  ;; actually reaches the app — the same IEXTEN-off raw-mode move Emacs makes.
  (input/disable-literal-next!)
  ;; Match the emulator's window padding to the theme background (OSC 11):
  ;; without this the 1-cell rim around the Lanterna grid stays the user's
  ;; default (often white) instead of the theme background.
  (let [^com.googlecode.lanterna.TextColor$RGB c t/terminal-bg]
    (input/set-default-bg! @vis/tty-out (.getRed c) (.getGreen c) (.getBlue c))))
(defn- disable-terminal-escape-modes!
  [_opts]
  (try (input/disable-bracketed-paste! @vis/tty-out) (catch Throwable _ nil))
  (try (input/disable-sgr-mouse! @vis/tty-out) (catch Throwable _ nil))
  (try (input/reset-default-bg! @vis/tty-out) (catch Throwable _ nil))
  ;; Restore IEXTEN so the user's shell gets literal-next quoting back.
  (try (input/restore-literal-next!) (catch Throwable _ nil)))
;; ---------------------------------------------------------------------------
;; Encrypted SSH key passphrase prompt — retired.
;;
;; git auth (ssh keys, credential helpers, passphrase prompts) is now handled
;; entirely by the user's own `git` binary and its ssh-agent / askpass config,
;; so the TUI no longer registers an in-app passphrase provider. Kept as a
;; no-op returning nil so the screen lifecycle (which vresets a cleanup thunk
;; from this call) stays unchanged.
;; ---------------------------------------------------------------------------
(defn- install-ssh-passphrase-prompt!
  "No-op: the host `git` binary owns credential prompting now. Returns nil
   (no cleanup thunk to run)."
  [^TerminalScreen _screen]
  nil)
(defn- sweep-orphaned-running-turns!
  []
  (try (vis/gateway-reconcile-running-turns!) (catch Throwable _ nil)))
(defn- pre-resolve-session-id!
  "Sweep interrupted turns before any resume history rebuild. The
   `--session-id` path validates before Lanterna starts; if the sweep
   happens later, the precomputed history still contains stale `:running`
   turns with blank answers, which render as empty assistant bubbles."
  [opts]
  (sweep-orphaned-running-turns!)
  (when-let [cid (:session-id opts)]
    (or (chat/resume-session cid)
      (throw (ex-info (format-session-not-found cid) {:vis/user-error true, :id cid})))))
(defn run-chat!
  "Start the fullscreen chat TUI. Blocks until user quits.
   Optional `opts` map:
     :session-id uuid-string - resume a specific session
     :resume          true        - resume the latest :tui session"
  ([] (run-chat! {}))
  ([opts]
   ;; Validate --session-id BEFORE we boot Lanterna. A miss here
   ;; used to crash mid-screen-startup with a stack trace; now it
   ;; surfaces as a `:vis/user-error` and `channel-main` prints a
   ;; clean, actionable message + exit code 2 (no trace, no torn-down
   ;; terminal state). Sweep first so precomputed resume history never
   ;; includes orphaned `:running` turns from a killed prior process.
   (let [resumed-from-flag (pre-resolve-session-id! opts)]
     (state/init!)
     ;; Subscribe to host notifications so any (vis/notify! ...) push
     ;; - from anywhere: this channel's click handler, an extension,
     ;; the iteration loop - wakes the render thread immediately. The
     ;; header band reads `(vis/notifications)` on every paint, so we
     ;; only need a render bump here; no app-db copy.
     (vis/watch-notifications! :tui-screen (fn [_snapshot] (state/dispatch [:bump-render-version])))
     (vis/add-channel-event-listener! :tui :tui-screen handle-channel-event!)
     ;; Load persisted config
     (when-let [c (vis/load-config)] (state/dispatch [:set-config c]))
     ;; Hydrate feature toggles (`:toggles` slot in ~/.vis/config.edn).
     ;; Runs AFTER `vis/load-config` and before the input loop; the
     ;; render pipeline reads toggles per-paint, so the first frame
     ;; already reflects persisted overrides instead of falling back
     ;; to registry defaults for one tick. We also install a listener
     ;; that auto-persists every change — toggles flipped from a future
     ;; settings dialog land in config.edn without any per-callsite
     ;; save plumbing. The listener bumps `:render-version` so the
     ;; bubble repaints with the new value on the same tick.
     (try (vis/toggles-hydrate-from-config! (or (vis/load-config-raw) {}))
       ;; `state/init!` (above) already projected registry DEFAULTS into
       ;; `:settings`, and hydration mutates the toggles AFTER that and
       ;; BEFORE the auto-persist listener below exists — so no resync
       ;; fires for the hydrated values. Refresh the cached projection
       ;; ONCE here, otherwise the footer keeps showing the default (e.g.
       ;; reasoning `balanced`) while the real toggle holds the persisted
       ;; value, and the first Ctrl+X r cycle appears to "do nothing"
       ;; (it advances the toggle, but only up to the already-shown value).
       (state/dispatch [:resync-toggle-settings])
       (vis/toggle-add-listener!
         (fn [_event]
           (try (let [raw (or (vis/load-config-raw) {})]
                  (vis/save-config! (assoc raw :toggles (vis/toggles-snapshot))))
             (catch Throwable t
               (tel/log!
                 {:level :warn, :id ::toggle-persist-failed, :data {:error (ex-message t)}}
                 "Toggle persistence failed; in-memory value still applies.")))
              ;; Rebuild the cached `:settings` projection AND drop the
              ;; stale render/height caches so consumer code and the
              ;; painter observe the new value on the next paint.
              ;; `:resync-toggle-settings` busts both render caches (a
              ;; registry-only toggle like `:vis/show-thinking` isn't in
              ;; the height cache's `settings-fingerprint`, so without
              ;; the bust the flip only took effect after a restart).
              ;; `:bump-render-version` then wakes the render thread for
              ;; the actual redraw.
           (state/dispatch [:resync-toggle-settings])
           (state/dispatch [:bump-render-version])))
       (catch Throwable t
         (tel/log! {:level :warn, :id ::toggles-hydrate-failed, :data {:error (ex-message t)}}
           "Toggle hydration from config failed; defaults stand.")))
     (let [terminal (create-terminal! opts)
           _ (configure-terminal-input! terminal opts)
           screen (TerminalScreen. terminal)
           ;; Render thread handle is held in a volatile so the `finally`
           ;; clause can join it. (Locals from the `try` body aren't in
           ;; scope inside `finally`.)
           render-thread (volatile! nil)
           title-listeners (volatile! {})
           ;; Background bubble pre-warm is owned by `virtual/rewarm!` -
           ;; one managed worker process-wide; see its ns comment for the
           ;; invalidation events that restart it.
           provider-limits-thread (volatile! nil)
           terminal-signal-cleanup (volatile! nil)
           ;; On a plain launch, the saved tab snapshot for this place (set
           ;; while resolving the initial session); the extra tabs are reopened
           ;; once the loop closures exist.
           restore-plan (volatile! nil)]
       (.startScreen screen)
       (let [ssh-passphrase-cleanup (volatile! nil)]
         (try
           (vreset! terminal-signal-cleanup (register-terminal-interrupt-handlers!))
           (vreset! ssh-passphrase-cleanup (install-ssh-passphrase-prompt! screen))
           ;; No provider yet → onboard BEFORE any session is created. A genuine
           ;; first run (no config file ever) gets the branded welcome; a
           ;; returning user who simply has no provider right now goes straight
           ;; to the provider manager. Either returns {:providers [...]} or nil.
           (when-not (:config @state/app-db)
             (when (not (:dialog-open? @state/app-db))
               (when-let [c (with-dialog-lock
                              #(if (vis/first-run?)
                                 (provider/show-welcome! screen)
                                 (provider/show-provider-dialog! screen (:config @state/app-db))))]
                 (state/dispatch [:set-config c]))))
           ;; Init session: resume if --session-id given, else fresh. The --session-id case was
           ;; already validated above (before Lanterna started), so here we only need the
           ;; pre-resolved value.
           (when-let [config (:config @state/app-db)]
             ;; Start warming the empty-session pool the MOMENT a config exists —
             ;; BEFORE the (possibly slow) resume/restore below — so it is hot by
             ;; the first Ctrl+N. Idempotent; the post-open `prewarm-session!` just
             ;; tops it up toward the pool depth.
             (chat/prewarm-session! config)
             (let [{:keys [id history]}
                   (cond
                     (:session-id opts)
                     resumed-from-flag

                     ;; --continue: reopen the most-recent :tui session
                     (:continue opts)
                     (if-let [latest (first (remove empty-untitled-session?
                                              (tui-session-summaries)))]
                       (or (chat/resume-session (:id latest))
                         (chat/make-session config))
                       (chat/make-session config))

                     ;; --resume: start fresh; the session picker opens
                     ;; before the main loop (see below), like `pi -r`.
                     (:resume opts)
                     (chat/make-session config)

                     ;; Plain launch: restore this place's saved tabs. The
                     ;; first still-existing saved session becomes tab 1; the
                     ;; rest are reopened after the loop closures exist (see
                     ;; `restore-plan`). No saved tabs → a fresh session.
                     :else
                     (let [snap  (tabs/read-snapshot)
                           saved (filter some? (:sessions snap))]
                       (if-let [first-session (some chat/resume-session saved)]
                         (do (vreset! restore-plan snap) first-session)
                         (chat/make-session config))))]
               (vswap! title-listeners assoc (str id) (init-visible-session! {:id id, :history history}))
               ;; Record this place's tab set even when there's just the one
               ;; startup tab — otherwise a single-tab session never persists
               ;; and a plain relaunch wouldn't restore it. Skip when restoring
               ;; (the restore block below persists once it reopens every tab,
               ;; so we don't transiently truncate the saved set to tab 1).
               (when-not @restore-plan (persist-tabs!))
               ;; Warm a fresh empty session in the BACKGROUND now, so the FIRST
               ;; Ctrl+N / `+` new-session is instant instead of paying the full
               ;; cold DB+env build on the input thread. The startup tab is
               ;; usually RESUMED (not built via `make-session`), so nothing else
               ;; kicks the new-session prewarm — without this the first
               ;; new-session is slow. Safe to race the just-finished session
               ;; open: the migration lock serializes same-JVM env builds.
               ;; `discard-prewarmed-session!` (shutdown hook below) deletes it
               ;; if the user never opens another tab.
               (chat/prewarm-session! config)
               ;; Kick off background pre-warm of the LRU. Walks the
               ;; history bottom-up calling project + bubble-height,
               ;; so by the time the user scrolls UP the cache is
               ;; already hot. Empty sessions skip this entirely.
               ;; Cancelled in the shutdown hook below.
               (when (seq history)
                 (let [size (screen-size screen)
                       cols (.getColumns size)
                       bubble-w (max 1 (- cols render/MESSAGE_SIDE_PAD))
                       settings (or (:settings @state/app-db) {})
                       warm-opts {:session-id id,
                                  :detail-expansions (:detail-expansions @state/app-db)
                                  ;; Re-layout as background warm lands so
                                  ;; total-h SETTLES while idle at auto-bottom
                                  ;; (thumb pinned to bottom = invisible) instead
                                  ;; of snapping ~20% on the first wheel-up.
                                  :on-warm #(state/dispatch [:bump-render-version])}]
                   ;; Head-start warm on the input thread so immediate
                   ;; first-scroll doesn't hit a cold heavy trace bubble.
                   ;; Full history still warms async below.
                   (virtual/pre-warm-recent! history
                     bubble-w
                     settings
                     (assoc warm-opts
                       :count prewarm-sync-tail-count
                       :budget-ms prewarm-sync-budget-ms))
                   (virtual/rewarm! history bubble-w settings warm-opts)))))
           ;; Spawn the render thread BEFORE the input loop. It will paint
           ;; the first frame as soon as `:render-version` is non-zero (every
           ;; init dispatch above bumps it).
           (vreset! render-thread (start-render-thread! screen))
           ;; Prewarm the slash-command machinery OFF the hot path so the
           ;; FIRST `/` keystroke doesn't pay cold JIT + registry harvest
           ;; (registry-slash-commands → menu-commands → slash/suggestions →
           ;; fuzzy-score) mid-frame — that dropped a frame and flickered the
           ;; popup on first open. Also fills the registry memo cell. Fire-
           ;; and-forget; failure is harmless (the live path recomputes).
           (future
             (try (slash-suggestions-for-input screen (input-state-from-text "/"))
               (catch Throwable _ nil)))
           (vreset! provider-limits-thread (start-provider-limits-thread!))
           ;; Local UI state that lives only in the input thread.
           ;;
           ;; `scrollbar-drag-offset` is `nil` when no drag is in
           ;; progress; otherwise it carries the integer row offset
           ;; between the click row and the TOP of the thumb captured
           ;; at CLICK_DOWN time (i.e. "how many rows from the top of
           ;; the thumb did the user grab?").
           ;;
           ;; Drag math then becomes `new-thumb-top = my - offset`,
           ;; which keeps the grip-point fixed under the cursor for
           ;; the entire drag - same contract every GUI scroll thumb
           ;; honours. A simple boolean would force the thumb to snap
           ;; its TOP to the cursor on the first DRAG event, which is
           ;; what made the previous implementation "jump" the moment
           ;; the user started moving.
           ;;
           ;; Clicks anywhere outside the thumb itself - the messages area, the track
           ;; above/below the thumb, the right gutter columns - are deliberately ignored (no
           ;; jump-to-position). Wheel scroll, keyboard PageUp/PageDown and arrows remain the
           ;; supported ways to move the viewport without grabbing the thumb. Bracketed-paste
           ;; mode is opt-in per terminal. Send the `ESC[?2004h` enable sequence right after
           ;; the screen is up so xterm-class terminals (Apple Terminal, iTerm, Alacritty,
           ;; kitty, gnome-terminal, mintty, vscode) wrap subsequent pastes in `ESC[200~ ...
           ;; ESC[201~`. Disabling happens in the outer `finally` block, so a crashed TUI
           ;; can't leave the user's shell stuck with bracketing on.
           (enable-terminal-escape-modes! opts)
           ;; SGR mouse mode (1006). Lanterna's `setMouseCaptureMode`
           ;; above already enabled legacy 1003 on the native terminal
           ;; backend, but its parser only understands the X10 binary
           ;; encoding - which corrupts coordinates the moment
           ;; `col + 32` exceeds 0x7F (i.e. col >= 96), because the JVM
           ;; UTF-8 decoder replaces the high byte with U+FFFD. SGR sends
           ;; the same payload as pure ASCII text, so wide terminals (the
           ;; copy-id glyph lives near the right edge!) survive intact.
           ;; The custom pattern registered above turns SGR sequences into
           ;; `MouseAction` instances with correct integer mx/my.
           (let [scrollbar-drag-offset (volatile! nil)
                 ;; `click-action-fired?` is set to true when the
                 ;; CLICK_DOWN branch already handled a click region
                 ;; (copy / link / image). The CLICK_RELEASE branch
                 ;; reads it to decide whether to fire the fallback
                 ;; release-only path - needed for terminals that
                 ;; deliver clicks as a single CLICK_RELEASE event
                 ;; (X10-style mouse mode, some SSH-tunnelled
                 ;; setups). Without this guard a normal
                 ;; DOWN+RELEASE pair would double-fire (open the link twice, copy twice).
                 click-action-fired? (volatile! false)
                 ;; App-side drag selection. Native terminal selection is
                 ;; unavailable while mouse reporting is enabled, so Vis tracks
                 ;; drag coordinates, highlights the range during render, then
                 ;; copies the visible text when the button is released.
                 mouse-selection-anchor (volatile! nil)
                 mouse-selection-focus (volatile! nil)
                 mouse-selection-source (volatile! nil)
                 mouse-selection-line? (volatile! false)
                 last-selection-click (volatile! nil)
                 ;; `paste-buffer` accumulates every keystroke received
                 ;; between `paste-start?` and `paste-end?`. We treat
                 ;; the whole block as one paste - newlines included -
                 ;; so a multi-line clipboard payload doesn't fire
                 ;; `KeyType/Enter` -> send mid-paste. The buffer is
                 ;; kept in a StringBuilder so accumulation stays
                 ;; allocation-cheap even for kilobyte pastes.
                 paste-buffer (volatile! nil)
                 ;; One-event stash used by wheel coalescing. When
                 ;; `read-chat-input!` drains wheel floods and sees a
                 ;; non-wheel event, it parks it here for the next loop
                 ;; iteration instead of dropping it.
                 pending-input-key (volatile! [])
                 prewarm-session!
                 (fn [{:keys [id history]}]
                   (virtual/stop-rewarm!)
                   (when (seq history)
                     (let [size (screen-size screen)
                           cols (.getColumns size)
                           bubble-w (max 1 (- cols render/MESSAGE_SIDE_PAD))
                           settings (or (:settings @state/app-db) {})
                           warm-opts {:session-id id,
                                      :detail-expansions (:detail-expansions @state/app-db)
                                      ;; See init-path warm-opts: settle total-h
                                      ;; via render bumps as the background warm
                                      ;; lands, so a session/workspace switch
                                      ;; doesn't jump the thumb on first scroll.
                                      :on-warm #(state/dispatch [:bump-render-version])}]
                       (virtual/pre-warm-recent! history
                         bubble-w
                         settings
                         (assoc warm-opts
                           :count prewarm-sync-tail-count
                           :budget-ms prewarm-sync-budget-ms))
                       (virtual/rewarm! history bubble-w settings warm-opts))))
                 ;; Open (or focus) a TAB for this session. Unlike the old
                 ;; in-place install, this NEVER resets the active tab — so a
                 ;; turn streaming in another tab keeps running. Each open
                 ;; session keeps its own title listener (so background
                 ;; auto-titles land live); prewarm re-binds to the front tab.
                 ensure-title-listener!
                 (fn [id]
                   (let [sid (str id)]
                     (when-not (get @title-listeners sid)
                       (vswap! title-listeners assoc sid (subscribe-title-listener! id)))))
                 open-session-tab! (fn [{:keys [id history], :as session-result} notify?]
                                     (when (and id session-result)

                                       (state/dispatch [:open-session-tab {:id id} history
                                                        (session-workspace id)])
                                       ;; `:open-session-tab` already reset `:title nil`. Only
                                       ;; push a title when the DB actually has one — mirror
                                       ;; refresh-active-tab! and NEVER overwrite with "" (a
                                       ;; race where the background auto-title future hasn't
                                       ;; persisted yet would otherwise blank the tab).
                                       (when-let [title (session-db-title id)]
                                         (state/dispatch [:set-title title]))
                                       (ensure-title-listener! id)
                                       (prewarm-session! session-result)
                                       (persist-tabs!)
                                       (when notify?
                                         (vis/notify! "Opened session"
                                           :level :success
                                           :ttl-ms copy-success-ttl-ms))))
                 start-new-session!
                 ;; Ctrl+N / `+` / `/new-session`. NEVER blocks the input thread on
                 ;; the cold env/runtime build: a warm pool session opens instantly,
                 ;; and on a pool MISS we open an optimistic "Starting…" placeholder
                 ;; tab now and bind the real session once the background build lands
                 ;; (chat/make-session-async). Text typed meanwhile queues into the
                 ;; tab's `:pending-sends` and drains the moment it is bound.
                 (fn [config seed-text]
                   (let [seed   (some-> seed-text str/trim not-empty)
                         result (chat/make-session-async config)]
                     (if-let [session (:session result)]
                       (do
                         (open-session-tab! session true)
                         (when seed (state/dispatch [:send-message seed])))
                       (let [build-id (str (java.util.UUID/randomUUID))
                             fut      (:building result)]
                         (state/dispatch [:open-building-tab build-id])
                         (when seed (state/dispatch [:send-message seed]))
                         (vis/worker-future
                           "tui-new-session-bind"
                           (fn []
                             (try
                               (let [{:keys [id history]} @fut]
                                 (ensure-title-listener! id)
                                 (state/dispatch [:bind-built-session build-id
                                                  {:id id} history (session-workspace id)])
                                 (persist-tabs!)
                                 (vis/notify! "Opened session"
                                   :level :success
                                   :ttl-ms copy-success-ttl-ms))
                               (catch Throwable e
                                 (vis/notify! (str "Failed to open new session: " (ex-message e))
                                   :level :error
                                   :ttl-ms copy-success-ttl-ms)))))))))
                 refresh-active-tab!
                 (fn [notify?]

                   (when-let [id (current-session-id)]
                     (when-let [title (session-db-title id)] (state/dispatch [:set-title title]))
                     (ensure-title-listener! id)
                     (prewarm-session! {:id id, :history (:messages @state/app-db)}))
                   (persist-tabs!)
                   (when notify?
                     (vis/notify! "Switched workspace"
                       :level :success
                       :ttl-ms copy-success-ttl-ms)))
                 switch-session!
                 (fn [choice]
                   ;; No `:loading?` guard: opening or focusing a tab never
                   ;; disturbs a turn running in another tab, so there's
                   ;; nothing to wait for. Every action lands on a tab.
                   (cond
                     (= :new (:action choice)) (when-let [config (:config @state/app-db)]
                                                 (start-new-session! config (:seed-text choice)))
                     (= :fork (:action choice))
                     (if-let [current-id (or (:id choice) (current-session-id))]
                       (let [db (vis/db-info)
                                     ;; Each fork gets its own workspace pin (1:1).
                                     ;; Mint a fresh rift clone of cwd for the new state.
                             ws-id (try (:id (vis/workspace-ensure-workspace! db {}))
                                     (catch Throwable _ nil))
                             fork-state-id (try (vis/db-fork-session! db
                                                  current-id
                                                  {:workspace-id ws-id})
                                             (catch Throwable _ nil))]
                         (if fork-state-id
                           (if-let [session-result (chat/resume-session current-id)]
                             (do (open-session-tab! session-result false)
                               (vis/notify! "Forked current session"
                                 :level :success
                                 :ttl-ms copy-success-ttl-ms))
                             (vis/notify! "Forked, but failed to reload session"
                               :level :warn
                               :ttl-ms copy-success-ttl-ms))
                           (vis/notify! "Could not fork current session"
                             :level :warn
                             :ttl-ms copy-success-ttl-ms)))
                       (vis/notify! "No current session to fork"
                         :level :warn
                         :ttl-ms copy-success-ttl-ms))
                     (= :delete (:action choice))
                     (when-let [target-id (:id choice)]
                       (when (with-dialog-lock
                               #(dlg/confirm-dialog! screen
                                  "Delete session"
                                  "Permanently delete this session? This cannot be undone."))
                         (let [current? (= (str target-id) (current-session-id))]
                           (try (vis/gateway-close-session! target-id) (catch Throwable _ nil))
                           (if current?
                             ;; Deleting the active session: drop into a fresh tab.
                             (when-let [config (:config @state/app-db)]
                               (let [old-tab-id (:active-tab-id @state/app-db)]
                                 (open-session-tab! (chat/make-session config) true)
                                 (when old-tab-id (state/dispatch [:close-tab old-tab-id]))))
                             ;; Non-current: if it's open in a background tab, close
                             ;; that now-dangling tab so it doesn't linger.
                             (when-let [tab-id (state/tab-id-for-session @state/app-db target-id)]
                               (state/dispatch [:close-tab tab-id])))
                           (vis/notify! "Deleted session" :level :success :ttl-ms copy-success-ttl-ms))))

                     (= :switch (:action choice))
                     ;; Focus the tab already bound to this session, or open a
                     ;; new one — `:open-session-tab` decides. Switching to a
                     ;; session whose turn is mid-flight just brings its tab to
                     ;; the front; the turn was never paused.
                     (let [target-id (:id choice)]
                       (when-not (= (str target-id) (current-session-id))
                         (if-let [session-result (chat/resume-session target-id)]
                           (open-session-tab! session-result true)
                           (vis/notify! "Session no longer exists"
                             :level :warn
                             :ttl-ms copy-success-ttl-ms))))))
                 ;; `/clear` (a `:slash/ui {:kind :clear-session}` slash):
                 ;; tear down THIS session (turns + soul + workspace links)
                 ;; and open a fresh empty one in its place — like Telegram's
                 ;; /clear, but in the SAME tab slot (open a fresh focused tab,
                 ;; drop the old one) so you keep working right where you were.
                 clear-session!
                 (fn []
                   (when-not (:dialog-open? @state/app-db)
                     (when-let [config (:config @state/app-db)]
                       (let [old-id     (current-session-id)
                             old-tab-id (:active-tab-id @state/app-db)]
                         (open-session-tab! (chat/make-session config) true)
                         (when old-tab-id
                           (state/dispatch [:close-tab old-tab-id]))
                         (when old-id
                           (try (vis/gateway-close-session! old-id) (catch Throwable _ nil)))
                         (vis/notify! "Cleared session"
                           :level :success
                           :ttl-ms copy-success-ttl-ms)))))
                 ;; Mint a trunk workspace rooted at `d`, create a session
                 ;; pinned to it, and open it in a new tab — a session in
                 ;; another project, focused, alongside the current ones.
                 open-dir-tab!
                 (fn [d]
                   (let [f (java.io.File. ^String d)]
                     (if (and (.exists f) (.isDirectory f))
                       (when-let [config (:config @state/app-db)]
                         (let [ws (try (vis/workspace-create-trunk-at! (vis/db-info)
                                         (.getCanonicalPath f))
                                    (catch Throwable _ nil))
                               session-result (when ws
                                                (chat/make-session config
                                                  {:workspace-id (:id ws)}))]
                           (if session-result
                             (open-session-tab! session-result true)
                             (vis/notify! "Could not open a session there"
                               :level :warn :ttl-ms copy-success-ttl-ms))))
                       (vis/notify! (str "Not a directory: " d)
                         :level :warn :ttl-ms copy-success-ttl-ms))))
                 ;; `/fs` (a `:slash/ui {:kind :dir-picker}` slash): browse to
                 ;; a directory in the modal picker, then open a focused session
                 ;; tab there. Starts at the active tab's working dir.
                 pick-dir!
                 (fn pick-dir! [& [purpose]]
                   (when-not (:dialog-open? @state/app-db)
                     (let [start (or (:workspace/root @state/app-db)
                                   (System/getProperty "user.dir"))
                           sid   (current-session-id)
                           ws    (when sid (session-workspace sid))
                           ;; C-r (set as root) repoints the SESSION's pinned
                           ;; workspace, which needs the state id, not just the
                           ;; workspace id.
                           state-id (when sid
                                      (try (vis/db-latest-session-state-id (vis/db-info) (str sid))
                                        (catch Throwable _ nil)))]
                       (when-let [chosen (with-dialog-lock
                                           #(dlg/directory-picker-dialog! screen start
                                              :db-info (vis/db-info) :workspace-id (:id ws)
                                              :session-state-id state-id
                                              :purpose purpose))]
                         (open-dir-tab! chosen))
                        ;; A filesystem add/remove or root change may have happened
                        ;; inside the picker; re-sync the workspace so the footer
                        ;; dir count and header reflect it immediately.
                       (when sid
                         (try (state/dispatch [:set-workspace (session-workspace sid)])
                           (catch Throwable _ nil))))))
                 ;; Managed-resources dialog (C-x s + the footer's `res N`
                 ;; button). One dialog at a time: drop the F2/help overlays and
                 ;; any active search before the modal so nothing bleeds around it.
                 open-resources!
                 (fn open-resources! []
                   (when-not (:dialog-open? @state/app-db)
                     (state/dispatch [:close-overlays])
                     (when (get-in @state/app-db [:search :active?])
                       (state/dispatch [:search-clear]))
                     (with-dialog-lock
                       #(dlg/resources-dialog! screen (get-in @state/app-db [:session :id])))))
                 show-sessions! (fn show-sessions! []
                                  (when-not (:dialog-open? @state/app-db)
                                    (let [sessions (mapv enrich-session-row (tui-session-summaries))]
                                      (when-let [choice (with-dialog-lock
                                                          #(dlg/navigator-dialog!
                                                             screen
                                                             {:sessions sessions
                                                              :active-session-id (current-session-id)
                                                              :db @state/app-db}))]
                                        (switch-session! choice)
                                        ;; After a delete, reopen the picker on the
                                        ;; refreshed list so pruning can continue.
                                        (when (= :delete (:action choice))
                                          (show-sessions!))))))
                 ;; Per-session model PICKER (C-x o + palette "Choose Model…").
                 ;; Mirrors the web footer chooser: a searchable list of every
                 ;; configured model (active one marked) plus a "★ router
                 ;; default" reset. The choice flows through [:set-model …],
                 ;; the SAME per-session pref the C-x m cycle writes.
                 show-model-picker!
                 (fn show-model-picker! []
                   (when-not (:dialog-open? @state/app-db)
                     (let [sid     (current-session-id)
                           current (when sid (try (vis/gateway-session-model sid)
                                               (catch Throwable _ nil)))]
                       (when-let [choice (with-dialog-lock
                                           #(dlg/model-picker! screen current))]
                         (if (:reset? choice)
                           (state/dispatch [:set-model nil nil])
                           (state/dispatch [:set-model (:provider choice) (:model choice)]))))))]
             ;; Restore the rest of this place's saved tabs (tab 1 is already
             ;; the first saved session). Reopen each in order, then focus the
             ;; one that was active — `open-session-tab!` focuses an
             ;; already-open session rather than duplicating it. Sessions that
             ;; no longer exist are silently skipped (the next save self-heals).
             (when-let [snap @restore-plan]
               (doseq [sid (:sessions snap)]
                 (when-let [sr (chat/resume-session sid)]
                   (open-session-tab! sr false)))
               (when-let [active (:active snap)]
                 (when-let [sr (chat/resume-session active)]
                   (open-session-tab! sr false))))
             ;; --resume opens the session picker at startup, like `pi -r`.
             (when (and (:resume opts)
                     (not (:dialog-open? @state/app-db)))
               (show-sessions!))
             (loop []
               ;; Layout fields are populated by the render thread after the first paint. Until
               ;; then, scroll handlers fall back to safe defaults and act as a no-op. Pure
               ;; poll - no rendering on this thread anymore. The
               ;; render thread handles all screen output.
               (let [db @state/app-db
                     {:keys [cols total-h inner-h messages-top]} (:layout db)
                     cols (or cols 0)
                     total-h (or total-h 0)
                     inner-h (or inner-h 0)
                     messages-top (or messages-top 0)
                     {:keys [key wheel-delta drag-events]} (read-chat-input! screen
                                                             pending-input-key)]
                 (cond
                   (:shutdown? db) nil
                   (nil? key) (do (Thread/sleep 16) (recur))
                   ;; ── Bracketed paste ───────────────────────────────────────────────────
                   ;; Three-state machine sitting BEFORE the regular key dispatch:
                   ;;
                   ;;   START arrives  -> open a new StringBuilder,
                   ;;                     swallow the key.
                   ;;   any key while open -> append its char into the
                   ;;                     buffer, swallow.
                   ;;   END arrives    -> flush the buffered text into
                   ;;                     the input via `paste-text`,
                   ;;                     close the buffer.
                   ;;
                   ;; Mouse events are excluded from the paste
                   ;; state machine below - they take a separate cond
                   ;; branch that fires BEFORE this one (see the
                   ;; `(instance? MouseAction key)` clause). A stuck
                   ;; paste buffer therefore can't silently swallow
                   ;; clicks on the header copy affordance or the
                   ;; scrollbar.
                   (input/paste-start? key) (do (vreset! paste-buffer (StringBuilder.)) (recur))
                   (input/paste-end? key)
                   (let [^StringBuilder sb @paste-buffer]
                     (when sb
                       (let [text (.toString sb)]
                         (vreset! paste-buffer nil)
                         (when-not (.isEmpty text)
                           (if-let [image (timg/probe-paste-image text
                                            {:workspace-root (try (str (workspace/cwd))
                                                               (catch Throwable _ nil))})]
                               ;; Dropped image file: the terminal pasted its
                               ;; PATH. Always chip it (never inline the raw
                               ;; path) with an `[Image #N: ...]` placeholder
                               ;; carrying the sniffed file metadata; the send
                               ;; path expands it back to the path text so the
                               ;; engine attaches the pixels.
                             (do (state/dispatch [:add-paste text image])
                               (let [{:keys [paste-counter pastes]} @state/app-db
                                     entry (get pastes paste-counter)
                                     token (input/format-paste-placeholder entry)
                                     db' @state/app-db]
                                 (state/dispatch [:update-input
                                                  (input/paste-text (:input db') token)])))
                             (if (input/use-placeholder? text)
                               ;; Stash the payload, insert a
                               ;; one-line `[Pasted #N: ...]` placeholder.
                               ;; The send path expands every active
                               ;; placeholder back into its content via
                               ;; `expand-paste-placeholders`. Reading
                               ;; the new id back out of the atom right
                               ;; after the dispatch is safe: every db
                               ;; event handler runs on the dispatching
                               ;; thread, so the swap is already visible.
                               (do (state/dispatch [:add-paste text])
                                 (let [{:keys [paste-counter pastes]} @state/app-db
                                       entry (get pastes paste-counter)
                                       token (input/format-paste-placeholder entry)
                                       db' @state/app-db]
                                   (state/dispatch [:update-input
                                                    (input/paste-text (:input db') token)])))
                               ;; Short single-line paste: inline,
                               ;; matches the natural feel of
                               ;; `git rev-parse HEAD`-style copies.
                               (state/dispatch [:update-input
                                                (input/paste-text (:input db) text)]))))))
                     (recur))
                   ;; Mouse events: scrollbar grab/drag + wheel scroll.
                   ;; Bypass `input/handle-key` entirely - those events
                   ;; need access to the layout published by the render
                   ;; thread, which `handle-key` doesn't see. Placed
                   ;; BEFORE the paste-buffer clause so a stuck paste
                   ;; bracket can't silently swallow mouse events (mouse
                   ;; and paste are physically disjoint channels -
                   ;; nothing in this branch can mutate paste state).
                   (instance? MouseAction key)
                   (let [^MouseAction ma key
                         atype (.getActionType ma)
                         pos (.getPosition ma)
                         mx (.getColumn pos)
                         my (.getRow pos)
                         _ (when-not (or (= atype MouseActionType/MOVE)
                                       (= atype MouseActionType/DRAG))
                               ;; MOVE/DRAG fire dozens of times per
                               ;; second; CLICK_*/SCROLL_* can still
                               ;; spew tens of events per second
                               ;; while the user scroll-wheels through
                               ;; a long bubble — enough to make the
                               ;; file handler's IO the dominant cost.
                               ;; Keep the diagnostic ("my click did
                               ;; nothing" reports), but emit at
                               ;; `:debug` so the default `:info`-min
                               ;; file handler drops the line. Flip
                               ;; min-level to `:debug` (or attach a
                               ;; console handler) to get it back.
                             (try (let [hit-kind (some-> (cr/lookup mx my)
                                                   :kind)]
                                    (tel/log! {:level :debug,
                                               :id ::mouse-event,
                                               :data {:type (str atype),
                                                      :mx mx,
                                                      :my my,
                                                      :cols cols,
                                                      :hit hit-kind},
                                               :msg (str "tui mouse "
                                                      atype
                                                      " at ("
                                                      mx
                                                      ","
                                                      my
                                                      ")"
                                                      " cols=" cols
                                                      " hit=" hit-kind)}))
                               (catch Throwable _ nil)))
                         bar-top messages-top
                         track-h
                         (+ inner-h render/MESSAGE_MARGIN_TOP render/MESSAGE_MARGIN_BOTTOM)
                           ;; Single source of truth for thumb geometry
                           ;; lives in `scrollbar/geometry` so painter
                           ;; and hit-test cannot drift apart. A nil
                           ;; return means there is no overflow — no
                           ;; thumb is painted, and every click below is
                           ;; correctly classified as off-thumb.
                         geom (scrollbar/geometry total-h inner-h track-h
                                (scroll/layout-offset (:scroll db) (max 0 (- (long total-h) (long inner-h)))))
                         thumb-top (when geom (+ bar-top (long (:thumb-top-rel geom))))
                         thumb-h (long (or (:thumb-h geom) 0))
                           ;; Hit-zone: the thumb's actual rows, with a
                           ;; 3-column-wide x-band on the right gutter so
                           ;; the user doesn't need pixel-perfect aim.
                         on-thumb? (and (some? geom)
                                     (>= mx (- cols render/MESSAGE_MARGIN_RIGHT))
                                     (< mx cols)
                                     (>= my (long thumb-top))
                                     (< my (+ (long thumb-top) thumb-h)))
                         selection-copy? (true? (get-in db [:settings :mouse-selection-copy]))
                         transcript-selectable-ranges
                         (get-in db [:layout :transcript-selectable-ranges])
                         transcript-bubble-copy-regions
                         (get-in db [:layout :transcript-bubble-copy-regions])
                         transcript-disclosure-copy-regions
                         (get-in db [:layout :transcript-disclosure-copy-regions])
                         input-selectable-ranges (get-in db [:layout :input-selectable-ranges])
                         selection-viewport {:viewport-top (+ messages-top
                                                             render/MESSAGE_MARGIN_TOP),
                                             :eff-scroll (get-in db [:layout :eff-scroll])}
                         slash-suggestions (slash-suggestions-for-input screen
                                             (:input db)
                                             (:slash-command-index
                                              db))]
                     (cond
                       ;; F1 help / F2 task overlay is open: it LOCKS the
                       ;; screen. Route the wheel to the F2 panel's own
                       ;; :ctx-scroll and SWALLOW every other mouse event so
                       ;; clicks / drag never leak through to the tabs, input,
                       ;; or scrollbar painted behind the overlay (which made
                       ;; the dialog feel unfocusable - no scroll, no click).
                       (or (:tasks-open? db) (:help-open? db))
                       (let [overlay-ranges (get-in db [:layout :overlay-selectable-ranges])
                             f2-only? (and (:tasks-open? db) (not (:help-open? db)))
                             over-panel? (and f2-only? selection-copy?
                                           (selection/point-in-ranges?
                                             (selection/point mx my) overlay-ranges))]
                         (cond
                           ;; Mouse-wheel scrolls the F2 panel body.
                           (and wheel-delta (not (zero? (long wheel-delta))))
                           (do (state/dispatch [(if (:help-open? db) :help-scroll-by :ctx-scroll-by)
                                                (* 3 (long wheel-delta))])
                             (state/dispatch [:bump-render-version])
                             (recur))
                           ;; F2 text selection — arm on a press inside the panel body.
                           ;; Anchors live in SCREEN coords; the paint path uses an
                           ;; identity viewport for :overlay selections so screen==doc.
                           (and over-panel? (= atype MouseActionType/CLICK_DOWN)
                             (not (cr/lookup mx my)))
                           (let [p (selection/point mx my)]
                             (vreset! mouse-selection-anchor p)
                             (vreset! mouse-selection-focus p)
                             (vreset! mouse-selection-source :overlay)
                             (vreset! click-action-fired? true)
                             (state/dispatch [:set-mouse-selection
                                              {:anchor p, :focus p, :source :overlay}])
                             (state/dispatch [:bump-render-version])
                             (recur))
                           ;; F2 drag — extend the active overlay selection.
                           (and f2-only? selection-copy?
                             (= atype MouseActionType/DRAG)
                             (= :overlay @mouse-selection-source)
                             (some? @mouse-selection-anchor))
                           (let [p (selection/point mx my)]
                             (vreset! mouse-selection-focus p)
                             (state/dispatch [:set-mouse-selection
                                              {:anchor @mouse-selection-anchor,
                                               :focus p, :source :overlay}])
                             (state/dispatch [:bump-render-version])
                             (recur))
                           ;; Release — finish an F2 selection (copy), else fall back
                           ;; to the dedicated close-button dismissal + hover.
                           (= atype MouseActionType/CLICK_RELEASE)
                           (let [anchor @mouse-selection-anchor
                                 focus (or @mouse-selection-focus anchor)
                                 overlay-sel? (= :overlay @mouse-selection-source)
                                 already-handled? @click-action-fired?]
                             (vreset! click-action-fired? false)
                             (vreset! mouse-selection-anchor nil)
                             (vreset! mouse-selection-focus nil)
                             (vreset! mouse-selection-source nil)
                             (if (and overlay-sel? anchor (not= anchor focus))
                               (let [payload (selection/selected-text
                                               (get-in db [:layout :screen-cells])
                                               {:anchor anchor, :focus focus, :source :overlay}
                                               overlay-ranges)]
                                 (state/dispatch [:clear-mouse-selection])
                                 (when-not (str/blank? payload)
                                   (copy-selection! payload :overlay)))
                               (do (state/dispatch [:clear-mouse-selection])
                                 (when-let [hit (and (not already-handled?) (cr/lookup mx my))]
                                   (case (:kind hit)
                                     :toggle-help (state/dispatch [:toggle-help])
                                     :toggle-tasks (state/dispatch [:toggle-tasks])
                                     :toggle-fact-files (do (state/dispatch [:toggle-fact-files (:fact-key hit)])
                                                          (state/dispatch [:bump-render-version]))
                                     :search-case  (state/dispatch [:search-toggle-case])
                                     :search-prev  (state/dispatch [:search-prev])
                                     :search-next  (state/dispatch [:search-next])
                                     :search-close (state/dispatch [:search-clear])
                                     :header-help  (state/dispatch [:toggle-help])
                                     :header-tasks (state/dispatch [:toggle-tasks])
                                     :header-search (state/dispatch [:search-open])
                                     :header-new-session (do (state/dispatch [:reset-input])
                                                           (switch-session! {:action :new}))
                                     :footer-dirs (pick-dir!)
                                     :footer-resources (open-resources!)
                                     :footer-model (show-model-picker!)
                                       ;; "↓ latest" chip → re-arm FOLLOW + repaint
                                       ;; (the click twin of C-l / Ctrl+End).
                                     :jump-bottom (do (state/dispatch [:scroll-to-bottom])
                                                    (state/dispatch [:bump-render-version]))
                                     nil))))
                             (recur))
                           (= atype MouseActionType/MOVE)
                           (do (when (cr/set-hovered! (cr/lookup mx my))
                                 (state/dispatch [:bump-render-version]))
                             (recur))
                           :else (recur)))
                       (and (seq slash-suggestions) (neg? (long (or wheel-delta 0))))
                       (do (state/dispatch [:move-slash-command-selection (long wheel-delta)
                                            (count slash-suggestions)])
                         (recur))
                       (and (seq slash-suggestions) (pos? (long (or wheel-delta 0))))
                       (do (state/dispatch [:move-slash-command-selection (long wheel-delta)
                                            (count slash-suggestions)])
                         (recur))
                       (neg? (long (or wheel-delta 0)))
                       (do (state/dispatch [:scroll-up (* 3 (Math/abs (long wheel-delta)))
                                            total-h inner-h])
                         (recur))
                       (pos? (long (or wheel-delta 0)))
                       (do (state/dispatch [:scroll-down (* 3 (long wheel-delta)) total-h
                                            inner-h])
                         (recur))
                         ;; CLICK_DOWN on the thumb itself: arm a drag.
                         ;; Record the offset between the click row and
                         ;; the thumb's top so subsequent DRAG events can
                         ;; preserve the grip-point. Crucially we do NOT
                         ;; dispatch any scroll mutation here - a bare
                         ;; click without movement must not move content.
                       (and (= atype MouseActionType/CLICK_DOWN) on-thumb?)
                       (do (vreset! scrollbar-drag-offset (- my thumb-top)) (recur))
                         ;; CLICK_DOWN on the scrollbar TRACK (right
                         ;; gutter, anywhere in the messages-area rows,
                         ;; off-thumb): jump the thumb so it CENTERS on
                         ;; the cursor row, then arm a drag with the same
                         ;; centered grip offset so a follow-up motion
                         ;; tracks naturally. This is the modern macOS
                         ;; \"jump to spot\" behaviour - a click anywhere
                         ;; on the scrollbar moves you there, instead of
                         ;; the legacy paged \"click-above-thumb = page-up\"
                         ;; convention. The previous build silently
                         ;; ignored every off-thumb click in the gutter,
                         ;; which felt broken (\"I'm clicking on the
                         ;; scrollbar and nothing scrolls\").
                       (and (= atype MouseActionType/CLICK_DOWN)
                         (some? geom)
                         (>= mx (- cols render/MESSAGE_MARGIN_RIGHT))
                         (< mx cols)
                         (>= my bar-top)
                         (< my (+ bar-top inner-h)))
                       (let [grip (long (quot thumb-h 2))]
                         (vreset! scrollbar-drag-offset grip)
                         (state/dispatch [:scroll-to-y (- my grip) bar-top track-h total-h
                                          inner-h])
                         (recur))
                         ;; Drag continues to track the cursor's Y as
                         ;; long as the user is holding the button after
                         ;; a thumb grab. We feed `(my - drag-offset)` to
                         ;; `:scroll-to-y` so the row under the user's
                         ;; finger stays glued to the same point on the
                         ;; thumb - no jump, no snap. X is intentionally
                         ;; ignored once dragging starts so the thumb
                         ;; doesn't pop loose if the cursor strays out
                         ;; of the right gutter.
                       (and (= atype MouseActionType/DRAG) (some? @scrollbar-drag-offset))
                       (do (state/dispatch [:scroll-to-y (- my (long @scrollbar-drag-offset))
                                            bar-top track-h total-h inner-h])
                         (recur))
                       (and selection-copy?
                         (= atype MouseActionType/DRAG)
                         (some? @mouse-selection-anchor))
                       (let [screen-focus (selection/point mx my)
                             doc-focus (selection/screen->document-point screen-focus
                                         selection-viewport)
                             source @mouse-selection-source]
                         (vreset! mouse-selection-focus doc-focus)
                         (state/dispatch [:set-mouse-selection
                                          {:anchor @mouse-selection-anchor,
                                           :focus doc-focus,
                                           :source source}])
                         (when-not (= source :input)
                           (when-let [{:keys [direction amount]} (selection/auto-scroll-step
                                                                   screen-focus
                                                                   {:top bar-top,
                                                                    :bottom (+ bar-top inner-h),
                                                                    :edge-size 6,
                                                                    :max-step 6})]
                             (let [amount (coalesced-drag-scroll-amount amount drag-events)]
                               (case direction
                                 :up (state/dispatch [:scroll-up amount total-h inner-h])
                                 :down (state/dispatch [:scroll-down amount total-h inner-h])
                                 nil))))
                         (recur))
                         ;; CLICK_RELEASE - ends a drag, and serves as
                         ;; a FALLBACK click trigger for terminals that
                         ;; deliver clicks as a single CLICK_RELEASE
                         ;; (X10 mouse mode, some SSH-tunnelled
                         ;; sessions) instead of the standard
                         ;; CLICK_DOWN/CLICK_RELEASE pair. The fallback
                         ;; is gated on (a) no drag in progress, and
                         ;; (b) the corresponding CLICK_DOWN didn't
                         ;; already handle the same region - otherwise
                         ;; a normal terminal would double-fire (copy
                         ;; the id twice, open the link twice).
                       (= atype MouseActionType/CLICK_RELEASE)
                       (let [was-dragging? (some? @scrollbar-drag-offset)
                             already-handled? @click-action-fired?
                             anchor @mouse-selection-anchor
                             line-selection? @mouse-selection-line?
                             screen-point (selection/point mx my)
                             focus (release-selection-focus anchor
                                     @mouse-selection-focus
                                     line-selection?
                                     screen-point
                                     selection-viewport)
                             source @mouse-selection-source]
                         (vreset! scrollbar-drag-offset nil)
                         (vreset! click-action-fired? false)
                         (vreset! mouse-selection-anchor nil)
                         (vreset! mouse-selection-focus nil)
                         (vreset! mouse-selection-source nil)
                         (vreset! mouse-selection-line? false)
                         (if (and selection-copy? anchor)
                           (let [sel {:anchor anchor, :focus focus, :source source}
                                 simple-click? (= anchor (:focus sel))
                                 disclosure-hit (when (and simple-click? (not= source :input))
                                                  (bubble-copy-hit
                                                    screen-point
                                                    transcript-disclosure-copy-regions))
                                 bubble-hit (when (and simple-click?
                                                    (not= source :input)
                                                    (not disclosure-hit))
                                              (bubble-copy-hit screen-point
                                                transcript-bubble-copy-regions))
                                 screen-sel
                                 (selection/document->screen-selection sel selection-viewport)
                                     ;; Pending bubbles carry only the static
                                     ;; \"Sending request to provider…\"
                                     ;; placeholder on the message map; the
                                     ;; live trace (thinking + iteration
                                     ;; progress) is painted by the bubble
                                     ;; layer straight from workspace
                                     ;; `:progress`. When the user drag-selects
                                     ;; across a live bubble we have to fall
                                     ;; back to the SCREEN-cells path so the
                                     ;; copied payload matches what the user
                                     ;; sees — the document-rows projection
                                     ;; would otherwise leak the placeholder.
                                 pending-in-sel? (and (= source :transcript)
                                                   (selection-touches-pending-bubble?
                                                     (:messages db)
                                                     (:layout db)
                                                     sel))
                                 payload (if (and (= source :transcript) (not pending-in-sel?))
                                           (selected-transcript-text
                                             (:messages db)
                                             (:layout db)
                                             cols
                                             (:settings db)
                                             {:session-id (get-in db [:session :id]),
                                              :detail-expansions (:detail-expansions db)}
                                             sel)
                                           (selection/selected-text
                                             (get-in db [:layout :screen-cells])
                                             screen-sel
                                             (selectable-ranges-for-source
                                               source
                                               transcript-selectable-ranges
                                               input-selectable-ranges)))]
                             (state/dispatch [:clear-mouse-selection])
                             (cond disclosure-hit (copy-bubble! (:text disclosure-hit))
                               bubble-hit (copy-bubble! (:text bubble-hit))
                               (and (not simple-click?) (not (str/blank? payload)))
                               (copy-selection! payload source)))
                           (when (and (not was-dragging?) (not already-handled?))
                             (if-let [hit (cr/lookup mx my)]
                               (case (:kind hit)
                                 :copy-id (copy-session-id! (:text hit))
                                 :search-case  (state/dispatch [:search-toggle-case])
                                 :search-prev  (state/dispatch [:search-prev])
                                 :search-next  (state/dispatch [:search-next])
                                 :search-close (state/dispatch [:search-clear])
                                 :toggle-tasks (state/dispatch [:toggle-tasks])
                                 :toggle-help (state/dispatch [:toggle-help])
                                 :header-help  (state/dispatch [:toggle-help])
                                 :header-tasks (state/dispatch [:toggle-tasks])
                                 :header-search (state/dispatch [:search-open])
                                 :header-new-session (do (state/dispatch [:reset-input])
                                                       (switch-session! {:action :new}))
                                 :footer-dirs (pick-dir!)
                                 :footer-resources (open-resources!)
                                 :footer-model (show-model-picker!)
                                 :jump-bottom (do (state/dispatch [:scroll-to-bottom])
                                                (state/dispatch [:bump-render-version]))
                                 :switch-session (switch-session! {:action :switch,
                                                                   :id (:text hit)})
                                 :workspace-entry
                                 (activate-tab-entry-hit! refresh-active-tab! hit)
                                 :close-tab
                                 (close-tab-entry-hit! refresh-active-tab! persist-tabs! hit)
                                 ;; new expanded = current collapsed (flip).
                                 :toggle-details (state/dispatch [:toggle-detail
                                                                  (:session-id hit)
                                                                  (:node-id hit)
                                                                  (:collapsed? hit)])
                                 :preview-switcher (state/dispatch [:select-preview-mode
                                                                    (:session-id hit)
                                                                    (:node-id hit) (:mode hit)])
                                 (open-click-target! screen hit))
                               (let [point (selection/point mx my)
                                     disclosure-hit (bubble-copy-hit
                                                      point
                                                      transcript-disclosure-copy-regions)]
                                 (cond disclosure-hit (copy-bubble! (:text disclosure-hit))
                                   :else (when-let [bubble-hit
                                                    (bubble-copy-hit
                                                      point
                                                      transcript-bubble-copy-regions)]
                                           (copy-bubble! (:text bubble-hit))))))))
                         (recur))
                         ;; MOVE - hover. We want the chat link-chrome
                         ;; rows to highlight when the user hovers over
                         ;; them. Look up the click region under the
                         ;; cursor; if it changed, update the hover
                         ;; pointer and bump the render version so the
                         ;; renderer repaints the highlighted row. The
                         ;; bump is gated by `set-hovered!` returning
                         ;; true so a cursor twitch inside the same
                         ;; chrome row doesn't trigger a repaint storm.
                       (= atype MouseActionType/MOVE) (do
                                                        (when (cr/set-hovered! (cr/lookup mx my))
                                                          (state/dispatch [:bump-render-version]))
                                                        (recur))
                         ;; CLICK_DOWN that didn't grab the scrollbar:
                         ;; if it landed on a registered click region
                         ;; (a markdown link / image / file-link
                         ;; chrome row), hand the URL to the OS opener
                         ;; on a side thread - a slow `xdg-open` cannot
                         ;; freeze the input loop's redraw cadence.
                       (= atype MouseActionType/CLICK_DOWN)
                       (do
                         (if-let [hit (cr/lookup mx my)]
                           (do
                                 ;; Tell the matching CLICK_RELEASE in
                                 ;; the same gesture pair to skip the
                                 ;; fallback fire - we just handled it.
                             (vreset! click-action-fired? true)
                             (vreset! last-selection-click nil)
                             (vreset! mouse-selection-line? false)
                             (case (:kind hit)
                                   ;; Header copy-id affordance: drop the
                                   ;; FULL UUID onto the system clipboard,
                                   ;; then push a host notification - the
                                   ;; header band's LEFT slot subscribes to
                                   ;; `vis.core/notifications` and surfaces
                                   ;; `✓ Copied session ID` for ~1.5s
                                   ;; before the entry expires. No
                                   ;; TUI-specific flash state; the
                                   ;; cross-channel notifications system
                                   ;; carries the feedback.
                               :copy-id (copy-session-id! (:text hit))
                               :toggle-tasks (state/dispatch [:toggle-tasks])
                               :toggle-help (state/dispatch [:toggle-help])
                               ;; F1/F2 header BUTTON chips (`button!`-painted).
                               ;; CLICK_DOWN sets `click-action-fired?` for ANY
                               ;; hit, which makes the matching CLICK_RELEASE skip
                               ;; its own dispatch — so without these two branches
                               ;; the chips fell through to `open-click-target!`
                               ;; (a no-op) and never toggled their panel.
                               :header-help  (state/dispatch [:toggle-help])
                               :header-tasks (state/dispatch [:toggle-tasks])
                               :header-search (state/dispatch [:search-open])
                               :header-new-session (do (state/dispatch [:reset-input])
                                                     (switch-session! {:action :new}))
                               :footer-dirs (pick-dir!)
                               :footer-resources (open-resources!)
                               :footer-model (show-model-picker!)
                               ;; "↓ latest" chip → re-arm FOLLOW + repaint. MUST be
                               ;; here on CLICK_DOWN (the gesture's first event swallows
                               ;; the click via `click-action-fired?`), else it falls to
                               ;; the no-op `open-click-target!` default and feels dead.
                               :jump-bottom (do (state/dispatch [:scroll-to-bottom])
                                              (state/dispatch [:bump-render-version]))
                               ;; Find-bar buttons: same CLICK_DOWN swallow as the
                               ;; header chips above - without these the matching
                               ;; CLICK_RELEASE skips (already-handled?) and the
                               ;; prev/next/close glyphs never fire (dead during a
                               ;; live render that keeps the region freshly registered).
                               :search-case  (state/dispatch [:search-toggle-case])
                               :search-prev  (state/dispatch [:search-prev])
                               :search-next  (state/dispatch [:search-next])
                               :search-close (state/dispatch [:search-clear])
                               :switch-session (switch-session! {:action :switch,
                                                                 :id (:text hit)})
                               :workspace-entry
                               (activate-tab-entry-hit! refresh-active-tab! hit)
                               :close-tab
                               (close-tab-entry-hit! refresh-active-tab! persist-tabs! hit)
                               :toggle-details (state/dispatch [:toggle-detail (:session-id hit)
                                                                (:node-id hit)
                                                                (:collapsed? hit)])
                               :preview-switcher (state/dispatch [:select-preview-mode
                                                                  (:session-id hit)
                                                                  (:node-id hit) (:mode hit)])
                                   ;; Default: hand any direct-open hit to
                                   ;; the OS opener on a side thread - a
                                   ;; slow `xdg-open` cannot freeze the
                                   ;; input loop's redraw cadence.
                               (open-click-target! screen hit)))
                           (when selection-copy?
                             (let [screen-anchor (selection/point mx my)
                                   source (selection/source-at-point
                                            screen-anchor
                                            transcript-selectable-ranges
                                            input-selectable-ranges
                                            {:row-padding 2})]
                               (if-not source
                                 (do (vreset! last-selection-click nil)
                                   (vreset! mouse-selection-line? false))
                                 (let [now-ms (System/currentTimeMillis)
                                       source-ranges (selectable-ranges-for-source
                                                       source
                                                       transcript-selectable-ranges
                                                       input-selectable-ranges)
                                       line-sel (when (selection/double-click?
                                                        @last-selection-click
                                                        now-ms
                                                        source
                                                        screen-anchor
                                                        mouse-double-click-ms)
                                                  (selection/line-selection-at-point
                                                    screen-anchor
                                                    source-ranges
                                                    selection-viewport))
                                       doc-anchor (or (:anchor line-sel)
                                                    (selection/screen->document-point
                                                      screen-anchor
                                                      selection-viewport))
                                       doc-focus (or (:focus line-sel) doc-anchor)]
                                   (vreset!
                                     last-selection-click
                                     (when-not line-sel
                                       {:source source, :point screen-anchor, :time-ms now-ms}))
                                   (vreset! mouse-selection-anchor doc-anchor)
                                   (vreset! mouse-selection-focus (:focus line-sel))
                                   (vreset! mouse-selection-source source)
                                   (vreset! mouse-selection-line? (boolean line-sel))
                                   (state/dispatch [:set-mouse-selection
                                                    {:anchor doc-anchor,
                                                     :focus doc-focus,
                                                     :source source}]))))))
                         (recur))
                         ;; Every other click - inside the input box,
                         ;; on the footer, etc. - falls through here.
                         ;; The scrollbar branch above already covers
                         ;; the right-gutter \"click on track to jump\";
                         ;; this `:else` is effectively the no-op tail.
                       :else (recur)))
                   ;; Paste-buffer accumulator runs AFTER the mouse
                   ;; branch so a stuck paste bracket can't swallow
                   ;; clicks. The buffer only collects character-bearing
                   ;; KeyStrokes; a MouseAction would have matched above.
                   (some? @paste-buffer) (do (when-let [ch (input/keystroke->paste-char key)]
                                               (.append ^StringBuilder @paste-buffer ^String ch))
                                           (recur))
                   ;; Placeholder smart-delete: a single
                   ;; Backspace right after the closing `]` of a
                   ;; `[Pasted #N: ...]` token nukes the WHOLE token in
                   ;; one keystroke, and drops the matching entry from
                   ;; `:pastes` so memory tracks what the user can
                   ;; still see in their input. Without this, the user
                   ;; would have to mash Backspace 27+ times to remove
                   ;; one placeholder - the visual unit-of-edit is the
                   ;; whole token, not its individual characters.
                   ;; In-session search owns the keyboard while active: typing
                   ;; edits the query (incremental), F3/Shift+F3 walk matches,
                   ;; Enter = next, Esc closes. Sits above the placeholder/slash
                   ;; branches so Backspace + chars reach the query, not the draft.
                   (and (instance? KeyStroke key) (get-in db [:search :active?]))
                   (let [ks    ^KeyStroke key
                         ktype (.getKeyType ks)
                         chr   (when (= ktype KeyType/Character) (Character/toLowerCase (.getCharacter ks)))]
                     (cond
                       ;; Esc toggles the find mode OFF (drops query + highlight).
                       (= ktype KeyType/Escape)
                       (state/dispatch [:search-clear])
                       ;; Ctrl+N / Ctrl+P walk matches (next / prev).
                       (and chr (.isCtrlDown ks) (= chr \n)) (state/dispatch [:search-next])
                       (and chr (.isCtrlDown ks) (= chr \p)) (state/dispatch [:search-prev])
                       ;; Alt+C toggles case sensitivity (same as the find-bar Aa chip).
                       (and chr (.isAltDown ks) (= chr \c)) (state/dispatch [:search-toggle-case])
                       (= ktype KeyType/Enter)               (state/dispatch [:search-next])
                       (= ktype KeyType/Backspace)
                       (state/dispatch [:search-set-query
                                        (apply str (butlast (get-in @state/app-db [:search :query])))])
                       ;; Plain (no ctrl/alt) printable → edit the query incrementally.
                       (and chr (not (.isCtrlDown ks)) (not (.isAltDown ks)))
                       (state/dispatch [:search-set-query
                                        (str (get-in @state/app-db [:search :query]) (.getCharacter ks))])
                       :else nil)
                     (recur))
                   (and (instance? KeyStroke key)
                     (= KeyType/Backspace (.getKeyType ^KeyStroke key))
                     (input/placeholder-id-before-cursor (:input db)))
                   (let [paste-id (input/placeholder-id-before-cursor (:input db))]
                     (state/dispatch [:update-input
                                      (input/delete-placeholder-backward (:input db))])
                     (when paste-id (state/dispatch [:remove-paste paste-id]))
                     (recur))
                   (and (instance? KeyStroke key)
                     (seq (slash-suggestions-for-input screen
                            (:input db)
                            (:slash-command-index db)))
                     (#{KeyType/ArrowUp KeyType/ArrowDown KeyType/PageUp KeyType/PageDown
                        KeyType/Enter KeyType/Tab KeyType/ReverseTab}
                      (.getKeyType ^KeyStroke key)))
                   (let [suggestions (slash-suggestions-for-input screen
                                       (:input db)
                                       (:slash-command-index db))
                         ktype (.getKeyType ^KeyStroke key)]
                     (cond
                       (= ktype KeyType/ArrowUp) (state/dispatch [:move-slash-command-selection -1
                                                                  (count suggestions)])
                       (= ktype KeyType/ArrowDown) (state/dispatch [:move-slash-command-selection
                                                                    1 (count suggestions)])
                       (= ktype KeyType/PageUp) (state/dispatch [:move-slash-command-selection -6
                                                                 (count suggestions)])
                       (= ktype KeyType/PageDown) (state/dispatch [:move-slash-command-selection 6
                                                                   (count suggestions)])
                       (= ktype KeyType/ReverseTab) (state/dispatch [:move-slash-command-selection
                                                                     -1 (count suggestions)])
                       (or (= ktype KeyType/Enter) (= ktype KeyType/Tab))
                       (when-let [suggestion (slash/selected-suggestion suggestions)]
                         (if (:file/mention? suggestion)
                           (state/dispatch
                             [:update-input (file-suggest/apply-mention (:input db)
                                              (:file/path suggestion))])
                           (do
                             (state/dispatch
                               [:update-input (input-state-from-text
                                                (slash/completion-text suggestion))])
                             ;; Enter = complete AND run in one keystroke: re-inject
                             ;; the Enter at the FRONT of the pending stash so the
                             ;; next loop iteration (overlay now gone — the completed
                             ;; token ends in a space) routes it through the normal
                             ;; `:send` path, which resolves prompt-arg / navigator /
                             ;; exact-slash execution. Tab stays completion-only so
                             ;; the user can keep typing arguments.
                             (when (= ktype KeyType/Enter)
                               (vreset! pending-input-key
                                 (into [key] @pending-input-key)))))))
                     (recur))
                   ;; Vim-style jump labels own the keyboard while active: a
                   ;; letter toggles the fold under its badge, Esc / C-g / any
                   ;; other key cancels. Sits above the fall-through so the
                   ;; label keys never reach the draft or the app-verb dispatch.
                   (and (instance? KeyStroke key) (:detail-labels-active? db))
                   (let [ks    ^KeyStroke key
                         ktype (.getKeyType ks)
                         chr   (when (= ktype KeyType/Character) (.getCharacter ks))
                         hit   (when (and chr (not (.isCtrlDown ks)) (not (.isAltDown ks)))
                                 (get (into {} (cr/assign-labels (cr/current)))
                                   (str (Character/toLowerCase ^char chr))))]
                     (when hit
                       (state/dispatch [:toggle-detail (:session-id hit) (:node-id hit) (:collapsed? hit)]))
                     (state/dispatch [:set-detail-labels false])
                     (state/dispatch [:bump-render-version])
                     (recur))
                   :else
                   (let [{:keys [action state workspace-index]} (input/handle-key key
                                                                  (:input db))]
                     (state/dispatch [:update-input state])
                     (let [run-command!
                               ;; The extension-contributed `:tui.slot/commands` slot is GONE;
                               ;; dispatchable command maps are either built-in palette ids
                               ;; (`:new-session`, `:fork-session`, ...) or typed
                               ;; slash suggestions from `vis/registered-slashes`.
                               ;; Slash entries carry a dotted `:id` (path)
                               ;; + `:slash/text`; we detect them by `:slash/text`
                               ;; and resubmit as a user message so
                               ;; `run-turn!` handles them through canonical
                               ;; `slash/dispatch`. Ctrl+K no longer includes those
                               ;; slash roots by default.
                           (fn [cmd & [args]]
                             (let [cmd-id (if (map? cmd) (:id cmd) cmd)
                                   args (or args (:slash/args cmd) "")
                                   cmd-map (when (map? cmd) cmd)]
                               (cond
                                 ;; A slash may declare a channel UI intent
                                 ;; instead of an engine round-trip. `:navigator`
                                 ;; opens the Ctrl+G session/workspace picker
                                 ;; (session == workspace), so `/workspace` and
                                 ;; `/workspace list` land in the SAME unified
                                 ;; list — no useless text bubble, and identical
                                 ;; live vs. resume.
                                 (= :navigator (get-in cmd-map [:slash/spec :slash/ui :kind]))
                                 (when-not (:dialog-open? @state/app-db)
                                   (show-sessions!))

                                 ;; `/fs`: open the directory picker, then a
                                 ;; focused session tab in the chosen directory.
                                 (= :dir-picker (get-in cmd-map [:slash/spec :slash/ui :kind]))
                                 (pick-dir! (get-in cmd-map [:slash/spec :slash/ui :purpose]))

                                 ;; `/clear`: wipe this session and start a
                                 ;; fresh one in the same tab.
                                 (= :clear-session (get-in cmd-map [:slash/spec :slash/ui :kind]))
                                 (clear-session!)

                                 (and cmd-map (:slash/text cmd-map))
                                 (when-not (:dialog-open? @state/app-db)
                                   (let [text (cond-> (:slash/text cmd-map)
                                                (not (str/blank? args))
                                                (str " " (str/trim args)))]
                                     (state/dispatch [:send-message text])
                                     (state/dispatch [:reset-input])))
                                 :else (when-not (:dialog-open? @state/app-db)
                                         (case cmd-id
                                           :new-session (let [seed (some-> (not-empty (str/trim (str args)))
                                                                     (input/expand-paste-placeholders (:pastes @state/app-db)))]
                                                          ;; Expand `[Pasted #N: ...]` against the ORIGINATING
                                                          ;; tab's `:pastes` BEFORE [:reset-input] clears that
                                                          ;; registry. The new session's `:pastes` is empty, so
                                                          ;; deferring expansion to its [:send-message] ships the
                                                          ;; cosmetic token to the provider instead of the pasted
                                                          ;; payload (the `/new-session` paste-loss bug).
                                                          (state/dispatch [:reset-input])
                                                          (switch-session! {:action    :new
                                                                            :seed-text seed}))
                                                   ;; Workspace ops (`:workspace`,
                                                   ;; `:apply-workspace-to-trunk`,
                                                   ;; `:discard-workspace-{soft,hard}`) live as
                                                   ;; typed slash commands now. No bespoke
                                                   ;; Ctrl+K palette case here.
                                           :fork-session (switch-session! {:action :fork})
                                           :switch-session (show-sessions!)
                                                   ;; :search-in-session removed from
                                                   ;; the command palette — the in-session
                                                   ;; search lives in the upper bar (above
                                                   ;; messages) and is triggered by F3 /
                                                   ;; Shift+F3 / its in-place input field. The
                                                   ;; previous palette entry was a duplicate
                                                   ;; entry point for the same action.
                                           :providers
                                           (when-let [c (with-dialog-lock
                                                          #(provider/show-provider-dialog!
                                                             screen
                                                             (:config @state/app-db)))]
                                             (state/dispatch [:set-config c]))
                                           :model (when-let
                                                    [c (with-dialog-lock
                                                         #(provider/show-provider-dialog!
                                                            screen
                                                            (:config @state/app-db)))]
                                                    (state/dispatch [:set-config c]))
                                           :settings
                                           (when-let [s (with-dialog-lock
                                                          #(dlg/settings-dialog!
                                                             screen
                                                             (:settings @state/app-db)
                                                             {:on-change
                                                              (fn [settings]
                                                                (state/dispatch
                                                                  [:update-settings
                                                                   settings])
                                                                (repaint-chat-frame! screen))}))]
                                             (state/dispatch [:update-settings s]))
                                                   ;; App verbs reachable from the palette (Ctrl+P)
                                                   ;; in addition to their direct keys — the palette
                                                   ;; is the reliable, searchable entry point, so it
                                                   ;; runs the SAME actions the key dispatch does.
                                           :cycle-model     (state/dispatch [:cycle-model])
                                           :pick-model      (show-model-picker!)
                                           :cycle-reasoning (state/dispatch [:cycle-reasoning-level])
                                           :cycle-verbosity (state/dispatch [:cycle-codex-verbosity])
                                           :search-open     (state/dispatch [:search-open])
                                           :toggle-help     (state/dispatch [:toggle-help])
                                           :open-resources  (open-resources!)
                                           :show-sessions   (show-sessions!)
                                           :open-dirs       (pick-dir!)
                                           :recenter        (state/dispatch [:scroll-to-bottom])
                                           :toggle-all-details   (state/dispatch [:toggle-all-details])
                                           :toggle-detail-labels (state/dispatch [:set-detail-labels true])
                                           :close-tab       (do (state/dispatch [:close-tab])
                                                              (persist-tabs!))
                                           :toggle-voice-recording
                                           (if-let [toggle (try (requiring-resolve
                                                                  'com.blockether.vis.ext.foundation-voice.input/toggle-recording!)
                                                             (catch Throwable _ nil))]
                                             (try (toggle {:app-db state/app-db})
                                               (catch Throwable t
                                                 (vis/notify! (str "Voice toggle failed: " (or (ex-message t) (str t)))
                                                   :level :error :ttl-ms status-error-ttl-ms)))
                                             (vis/notify! "Voice extension not loaded (foundation-voice)."
                                               :level :warn :ttl-ms status-error-ttl-ms))
                                           :pick-file
                                           ;; `@` opens the INLINE file picker now (same as the
                                           ;; web); the modal is gone. This palette entry just
                                           ;; seeds an `@` at the caret to start it.
                                           (state/dispatch
                                             [:update-input (input/paste-text state "@")])
                                                   ;; No :quit branch - the palette has no Quit
                                                   ;; entry; Ctrl+C is the only quit path.
                                           nil)))))]
                       (case action
                         :quit
                             ;; Ctrl+C with an empty draft normally exits the
                             ;; TUI. While a turn is in flight that exit path
                             ;; orphans the worker future — and worse, gives
                             ;; the user no way to abort a stuck iteration
                             ;; (e.g. an LLM HTTP response that never starts
                             ;; streaming). Intercept: cancel the in-flight
                             ;; turn instead of quitting. A second Ctrl+C
                             ;; with no turn running falls through to nil and
                             ;; the TUI exits as before.
                         (let [db @state/app-db]
                           (cond
                             (:loading? db)
                             (do (state/dispatch [:cancel-turn]) (recur))

                             (state/any-background-loading? db)
                             (let [n   (count (state/background-loading-tokens db))
                                   ok? (with-dialog-lock
                                         #(dlg/confirm-dialog! screen
                                            "Abort running tasks?"
                                            [(str n " background task" (when (> n 1) "s")
                                               " still running in other tab" (when (> n 1) "s") ".")
                                             "Abort them and quit?"]))]
                               (if ok?
                                 (do (state/dispatch [:cancel-all-turns]) nil)
                                 (recur)))

                             :else nil))
                         :clear-input
                             ;; Priority order while a turn is loading:
                             ;;  1. cancel the turn (Esc/Ctrl+C is the user's
                             ;;     only escape hatch from a stuck iteration)
                             ;;  2. clear the search overlay if active
                             ;;  3. clear the input draft
                             ;; The pre-fix behaviour skipped (1) whenever the
                             ;; user had typed even one character into the
                             ;; draft, which made hung turns unrecoverable
                             ;; short of killing the JVM.
                         (cond (:help-open? @state/app-db) (do (state/dispatch [:toggle-help]) (recur))
                           (:tasks-open? @state/app-db) (do (state/dispatch [:toggle-tasks]) (recur))
                           (:loading? @state/app-db) (do (state/dispatch [:cancel-turn])
                                                       (recur))
                           (get-in @state/app-db [:search :active?])
                           (do (state/dispatch [:search-clear])
                             (vis/notify! "Search cleared"
                               :level :info
                               :ttl-ms copy-success-ttl-ms)
                             (recur))
                           :else (do (state/dispatch [:reset-input]) (recur)))
                         :search-next (do (state/dispatch [:search-next])
                                        (when-let [s (:search @state/app-db)]
                                          (let [n (count (:hits s))]
                                            (when (pos? n)
                                              (vis/notify! (str "Find \""
                                                             (:query s)
                                                             "\" — match "
                                                             (inc (long (:index s)))
                                                             "/"
                                                             n
                                                             "  (C-p prev, Esc clear)")
                                                :level :info
                                                :ttl-ms copy-success-ttl-ms))))
                                        (recur))
                         :search-prev (do (state/dispatch [:search-prev])
                                        (when-let [s (:search @state/app-db)]
                                          (let [n (count (:hits s))]
                                            (when (pos? n)
                                              (vis/notify! (str "Find \""
                                                             (:query s)
                                                             "\" — match "
                                                             (inc (long (:index s)))
                                                             "/"
                                                             n
                                                             "  (C-n next, Esc clear)")
                                                :level :info
                                                :ttl-ms copy-success-ttl-ms))))
                                        (recur))
                         :search-open
                         (do (state/dispatch [:search-open]) (recur))
                         :open-resources (do (open-resources!) (recur))
                         :show-palette (do (when-not (:dialog-open? @state/app-db)
                                               ;; One invocation, one command. Reopening here
                                               ;; caused a modal loop: after choosing any
                                               ;; command the palette immediately appeared
                                               ;; again until Esc. Ctrl+K should close after
                                               ;; execution; users can press Ctrl+K again for
                                               ;; another command. `command-palette!`
                                               ;; re-concats `dlg/palette-commands` internally.
                                               ;; Keep extension slash roots out of. Ctrl+K;
                                               ;; typed `/` suggestion owns those.
                                             (when-let [cmd
                                                        (with-dialog-lock
                                                          #(dlg/command-palette!
                                                             screen
                                                             (command-palette-extra-commands)))]
                                               (run-command! cmd)))
                                         (recur))
                         :select-tab-index
                         (do (let [tabs   (:tabs @state/app-db)
                                   n      (count tabs)
                                   before (:active-tab-id @state/app-db)]
                               ;; A numeric jump (C-x N / M-N) to a workspace that
                               ;; isn't open: surface it as a TUI notice instead of
                               ;; silently swallowing the keystroke. :next/:prev and
                               ;; in-range indexes fall through to the normal switch.
                               (if (and (integer? workspace-index)
                                     (or (neg? workspace-index)
                                       (>= workspace-index n)))
                                 (vis/notify! (str "No workspace " (inc workspace-index)
                                                " \u2014 only " n
                                                (if (= 1 n) " tab open" " tabs open"))
                                   :level :warn :ttl-ms 3000)
                                 (do (state/dispatch [:select-tab-index workspace-index])
                                   (when-not (= before (:active-tab-id @state/app-db))
                                     (refresh-active-tab! false)))))
                           (recur))
                         :close-tab
                         (do (let [before-active (:active-tab-id @state/app-db)
                                   before-n      (count (:tabs @state/app-db))]
                               (state/dispatch [:close-tab])
                               (when (not= before-n (count (:tabs @state/app-db)))
                                 (when (not= before-active (:active-tab-id @state/app-db))
                                   (refresh-active-tab! false))
                                 (persist-tabs!)))
                           (recur))
                         :new-session
                             ;; Ctrl+N — start a fresh session in a new tab (same
                             ;; path as the Ctrl+P palette "New Session" and the
                             ;; header `+` button). Reset the draft first so the
                             ;; new session opens on a clean buffer.
                         (do (when-not (:dialog-open? @state/app-db)
                               (state/dispatch [:reset-input])
                               (switch-session! {:action :new}))
                           (recur))
                         :toggle-help (do (state/dispatch [:toggle-help]) (recur))
                         :toggle-tasks (do (state/dispatch [:toggle-tasks]) (recur))
                         :history-up (do (if (:tasks-open? @state/app-db)
                                           (do (state/dispatch [:ctx-scroll-by -1])
                                             (state/dispatch [:bump-render-version]))
                                           (state/dispatch [:history-up]))
                                       (recur))
                         :history-down (do (if (:tasks-open? @state/app-db)
                                             (do (state/dispatch [:ctx-scroll-by 1])
                                               (state/dispatch [:bump-render-version]))
                                             (state/dispatch [:history-down]))
                                         (recur))
                         :cycle-reasoning (do (state/dispatch [:cycle-reasoning-level]) (recur))
                         :cycle-verbosity (do (state/dispatch [:cycle-codex-verbosity]) (recur))
                         :cycle-model (do (state/dispatch [:cycle-model]) (recur))
                         :pick-model (do (show-model-picker!) (recur))
                         :toggle-voice-recording
                             ;; Voice toggle (Ctrl+P palette → Voice Recording):
                             ;; first run starts a microphone recording, second
                             ;; stops + transcribes +
                             ;; appends the text to the active input box (see
                             ;; foundation-voice/input/toggle-recording!).
                             ;;
                             ;; No `:loading?` gate: recording the NEXT message
                             ;; while the current turn streams is a normal user
                             ;; workflow; the transcription just appends to the
                             ;; editor, it does not interrupt the in-flight turn.
                             ;;
                             ;; Why catches now `notify!` instead of `tel/log!` only
                             ;; ─────────────────────────────────────────
                             ;; A silent log line buried under ~/.vis/vis.log
                             ;; is invisible from the TUI. Mic permission
                             ;; denied / no audio device / ASR native lib
                             ;; missing all surfaced as nothing at all. Each
                             ;; throw now also pushes a user-visible
                             ;; notification through `vis/notify!` so the
                             ;; failure mode is observable from the same
                             ;; surface that took the keystroke.
                         (do
                           (let
                             [toggle
                              (try
                                (requiring-resolve
                                  'com.blockether.vis.ext.foundation-voice.input/toggle-recording!)
                                (catch Throwable t
                                  (tel/log! {:level :error,
                                             :id ::voice-toggle-resolve-failed,
                                             :data {:ex t}})
                                  (vis/notify! (str "Voice toggle unavailable: "
                                                 (or (ex-message t) (str t)))
                                    :level :error
                                    :ttl-ms status-error-ttl-ms)
                                  nil))]
                             (cond (nil? toggle)
                               (vis/notify!
                                 "Voice extension not loaded (foundation-voice)."
                                 :level :warn
                                 :ttl-ms status-error-ttl-ms)
                               :else (try (toggle {:app-db state/app-db})
                                       (catch Throwable t
                                         (tel/log! {:level :error,
                                                    :id ::voice-toggle-failed,
                                                    :data {:ex t}})
                                         (vis/notify! (str "Voice toggle failed: "
                                                        (or (ex-message t) (str t)))
                                           :level :error
                                           :ttl-ms status-error-ttl-ms)))))
                           (recur))
                         :show-sessions (do (show-sessions!) (recur))
                         ;; Ctrl+B: provider / model configuration dialog (also
                         ;; reachable via the Ctrl+P palette → "Configure Providers").
                         :providers
                         (do (when-not (:dialog-open? @state/app-db)
                               (when-let [c (with-dialog-lock
                                              #(provider/show-provider-dialog!
                                                 screen (:config @state/app-db)))]
                                 (state/dispatch [:set-config c])))
                           (recur))
                         ;; Ctrl+G: filesystem / directory picker (also the `/fs`
                         ;; slash's rich-channel realization).
                         :open-dirs (do (pick-dir!) (recur))
                         :pick-file (do (state/dispatch
                                          [:update-input (input/paste-text state "@")])
                                      (recur))
                         :send
                             ;; If the slash overlay is visible, Enter was
                             ;; handled above as completion, same as Tab.
                             ;; Here Enter only runs an exact slash command
                             ;; already present in the input, or submits a
                             ;; normal message.
                         (do (cond
                               ;; A slash that requires an argument typed with
                               ;; none (e.g. `/draft new`): pop a text-input for
                               ;; it, then run the slash with the value. Cancel
                               ;; (Esc) just clears the editor.
                               (prompt-arg-slash-for-input state)
                               (let [{:keys [slash-text prompt]} (prompt-arg-slash-for-input state)]
                                 (when-not (:dialog-open? @state/app-db)
                                   (when-let [val (with-dialog-lock
                                                    #(dlg/text-input-dialog! screen slash-text prompt))]
                                     (when-not (str/blank? val)
                                       (state/dispatch [:send-message
                                                        (str slash-text " " (str/trim val))]))))
                                 (state/dispatch [:reset-input]))

                               ;; UI-intent slash (e.g. /workspace → navigator,
                               ;; /fs → directory picker): realize the intent
                               ;; in the channel instead of dispatching to the
                               ;; engine. Typed nested slashes never match
                               ;; `exact-command`, so this resolves them by full
                               ;; path against the engine registry.
                               (navigator-slash-for-input state)
                               (let [kind (get-in (navigator-slash-for-input state)
                                            [:slash/ui :kind])]
                                 (when-not (:dialog-open? @state/app-db)
                                   (case kind
                                     :dir-picker (pick-dir! (get-in (navigator-slash-for-input state)
                                                              [:slash/ui :purpose]))
                                     :clear-session (clear-session!)
                                     (show-sessions!)))
                                 (state/dispatch [:reset-input]))

                               (slash-command-for-input screen state)
                               (let [cmd (slash-command-for-input screen state)]
                                 (run-command! cmd (:slash/args cmd))
                                 (state/dispatch [:reset-input]))

                               :else
                               (submit-input! @state/app-db state))
                           (recur))
                         :cancel (cond (:help-open? @state/app-db) (do (state/dispatch [:toggle-help]) (recur))
                                   (:tasks-open? @state/app-db) (do (state/dispatch [:toggle-tasks]) (recur))
                                   :else (do (when (:loading? @state/app-db)
                                               (state/dispatch [:cancel-turn]))
                                           (recur)))
                         ;; PageUp / M-v (Emacs `scroll-down-command`): a FULL screen
                         ;; back, keeping 2 lines of context overlap (Emacs's
                         ;; `next-screen-context-lines` default) — not a 5-row nudge.
                         :scroll-up (do (cond (:help-open? @state/app-db)
                                          (do (state/dispatch [:help-scroll-by -10])
                                            (state/dispatch [:bump-render-version]))
                                          (:tasks-open? @state/app-db)
                                          (do (state/dispatch [:ctx-scroll-by -10])
                                            (state/dispatch [:bump-render-version]))
                                          :else
                                          (state/dispatch [:scroll-up (max 1 (- (long inner-h) 2)) total-h
                                                           inner-h]))
                                      (recur))
                         ;; PageDown / C-v (Emacs `scroll-up-command`): a FULL screen forward.
                         :scroll-down (do (cond (:help-open? @state/app-db)
                                            (do (state/dispatch [:help-scroll-by 10])
                                              (state/dispatch [:bump-render-version]))
                                            (:tasks-open? @state/app-db)
                                            (do (state/dispatch [:ctx-scroll-by 10])
                                              (state/dispatch [:bump-render-version]))
                                            :else
                                            (state/dispatch [:scroll-down (max 1 (- (long inner-h) 2)) total-h
                                                             inner-h]))
                                        (recur))
                         ;; C-l / C-x j / C-End / M-> (Emacs `end-of-buffer`): snap to
                         ;; the newest content + repaint.
                         :recenter (do (state/dispatch [:scroll-to-bottom])
                                     (state/dispatch [:bump-render-version])
                                     (recur))
                         ;; C-x TAB / C-x S-TAB — Emacs global fold cycle: toggle
                         ;; every disclosure collapsed↔expanded in one keystroke.
                         :toggle-all-details (do (state/dispatch [:toggle-all-details])
                                               (recur))
                         ;; C-x t — vim-style jump labels: badge every visible
                         ;; disclosure so a letter toggles that one fold.
                         :toggle-detail-labels
                         (do (if (:detail-labels-active? @state/app-db)
                               (state/dispatch [:set-detail-labels false])
                               (if (seq (cr/assign-labels (cr/current)))
                                 (do (state/dispatch [:set-detail-labels true])
                                   (vis/notify! "Jump to fold: press a highlighted letter · Esc / C-g to cancel"
                                     :level :info :ttl-ms 4000))
                                 (vis/notify! "No collapsible elements on screen to label."
                                   :level :info :ttl-ms 2000)))
                           (state/dispatch [:bump-render-version])
                           (recur))
                         ;; M-< (Emacs `beginning-of-buffer`): park at the very top.
                         :scroll-to-top (do (state/dispatch [:scroll-to-top])
                                          (state/dispatch [:bump-render-version])
                                          (recur))
                         :continue (recur))))))))
           (finally
             ;; Restore process-level INT/TSTP handling before teardown. If the
             ;; user hits Ctrl+C during cleanup, let the JVM/default handler win
             ;; instead of re-entering the TUI dispatcher.
             (when-let [cleanup @terminal-signal-cleanup] (try (cleanup) (catch Throwable _ nil)))
             ;; Tell native terminals to stop wrapping pastes and reporting
             ;; SGR mouse events.
             (disable-terminal-escape-modes! opts)
             ;; Drop the notifications watcher so the next TUI session
             ;; doesn't accumulate stale hooks (the screen is short-lived
             ;; relative to the JVM - leaving stale watchers around would
             ;; eventually hold references to dead atoms).
             (try (vis/unwatch-notifications! :tui-screen) (catch Throwable _ nil))
             (try (vis/remove-channel-event-listener! :tui :tui-screen) (catch Throwable _ nil))
             ;; Tell the render thread to exit and wake it so the wait
             ;; finishes immediately. Daemon thread, so the join is
             ;; optional - doing it anyway lets the final paint (or the
             ;; no-op when shutdown? was already true) finish before we
             ;; tear down the screen.
             (state/dispatch [:shutdown])
             ;; Cancel the pre-warm worker BEFORE joining the render
             ;; thread - it might still be holding `cached*` work
             ;; that we'd rather drop than wait on.
             (virtual/stop-rewarm!)
             (when-let [t @render-thread] (try (.join ^Thread t 500) (catch Throwable _ nil)))
             (when-let [t @provider-limits-thread]
               (try (.join ^Thread t 500) (catch Throwable _ nil)))
             (doseq [[_ cleanup] @title-listeners] (try (cleanup) (catch Throwable _ nil)))
             (do
               (chat/discard-prewarmed-session!)
               (doseq [session (workspace-sessions)]
                 (chat/dispose! session)))
             (when-let [cleanup @ssh-passphrase-cleanup] (try (cleanup) (catch Throwable _ nil)))
             (.stopScreen screen))))))))
;;; ── CLI argument parsing for the TUI channel ─────────────────────────
(def ^:private tui-usage "vis channels tui [--session-id ID | --resume | --continue]")
(defn- missing-value? [v] (or (nil? v) (str/starts-with? v "--")))
(defn- flag-value
  [flag more]
  (let [v (first more)]
    (when (missing-value? v)
      (throw (ex-info (str flag " requires a value" "\nUsage: " tui-usage) {:vis/user-error true})))
    v))
(defn- parse-args
  "Parse `vis channels tui` flags.
     --session-id ID   Resume a session (full UUID or short prefix)
     --resume, -r           Open the session picker at startup
     --continue, -c         Reopen the most-recent :tui session

   Unknown flags and missing flag values throw a `:vis/user-error` ex-info
   so the user sees a clean error instead of the TUI silently swallowing a
   typo (e.g. `--sessions-id`)."
  [args]
  (loop [args (seq args)
         opts {}]
    (if-not args
      opts
      (let [arg (first args)
            more (next args)]
        (case arg
          "--session-id" (let [v (flag-value arg more)]
                           (recur (next more) (assoc opts :session-id v)))
          ("--resume" "-r") (recur more (assoc opts :resume true))
          ("--continue" "-c") (recur more (assoc opts :continue true))
          (throw (ex-info (str "unknown flag: " arg "\nUsage: " tui-usage)
                   {:vis/user-error true})))))))
(defn- redirect-stdio-to-log!
  "Lanterna writes to /dev/tty directly. Everything else (Telemere, SLF4J,
   library prints, JVM warnings) MUST be redirected to ~/.vis/vis.log
   before any other code runs - otherwise stray bytes corrupt the screen."
  []
  (try (require 'taoensso.telemere)
    ((resolve 'taoensso.telemere/remove-handler!) :default/console)
    (catch Throwable _ nil))
  (let [log-dir (java.io.File. (str (System/getProperty "user.home") "/.vis"))
        _ (when-not (.exists log-dir) (.mkdirs log-dir))
        log-path (str log-dir "/vis.log")
        log-ps (java.io.PrintStream. (java.io.FileOutputStream. log-path true) true)
        log-w (java.io.OutputStreamWriter. log-ps)]
    (System/setOut log-ps)
    (System/setErr log-ps)
    (alter-var-root #'*out* (constantly log-w))
    (alter-var-root #'*err* (constantly log-w))))
(defn- print-session-id-on-exit!
  "After the fullscreen TUI releases the terminal, print the session id
   back to the shell so users can resume/copy it from scrollback. During the
   TUI session the same id is visible in the header; stdout is intentionally
   quiet until teardown because Lanterna owns the screen."
  []
  (when-let [id (current-session-id)]
    (let [^java.io.PrintStream out vis/original-stdout]
      ;; Lanterna leaves the cursor wherever the alt-screen teardown
      ;; parked it (rarely column 0), so a bare println lands the
      ;; banner mid-row and the leading char looks "eaten". Lead with
      ;; a carriage return to snap back to column 0 first.
      ;; Explicit `\n` (not println) so the banner is byte-identical on every
      ;; OS — Windows' println would emit `\r\n` and desync the resume line.
      (.print out "\rResume with:\n")
      (.print out (str "vis channels tui --session-id " id "\n"))
      (.flush out))))
(defn channel-main
  "Channel entry point: full TUI bootstrap. Performs the stdout/stderr
   redirect, runs `vis/init!`, then hands off to `run-chat!`. Errors
   surface on the original terminal and the log file.

   Invoked by `com.blockether.vis.core` dispatch - not called from
   vis-runtime directly."
  [args]
  (redirect-stdio-to-log!)
  (vis/init!)
  (let [exit-code (atom 0)]
    (try (run-chat! (parse-args args))
      (print-session-id-on-exit!)
      (catch Throwable t
        (if (:vis/user-error (ex-data t))
             ;; Caller-facing error: invalid flag value, missing
             ;; session id, etc. Print the message clean and let the
             ;; process exit non-zero - no Java stack trace, no rethrow
             ;; (which would trigger clojure.main's auto-trace dump).
          (do (.println ^java.io.PrintStream vis/original-stdout (str "vis: " (.getMessage t)))
            (reset! exit-code 2))
             ;; Genuine fatal: dump the trace to the terminal AND the log
             ;; so we can post-mortem it.
          (do (.println ^java.io.PrintStream vis/original-stdout
                (str "vis: fatal error - " (.getMessage t)))
            (.printStackTrace t (java.io.PrintStream. ^java.io.OutputStream @vis/tty-out true))
            (throw t))))
      (finally (vis/shutdown!)
                  ;; Stop the agent thread-pool so the JVM can exit immediately
                  ;; after the TUI tears down. Without this the pool's non-daemon
                  ;; threads keep the process alive for ~60s of idle keep-alive,
                  ;; which from the user's seat looks like "Ctrl+C froze vis":
                  ;; the screen is gone, raw mode is restored, but the shell
                  ;; prompt does not return. CLI / Telegram channel paths already
                  ;; call `shutdown-agents` after their main loops; the TUI did
                  ;; not, so the hang was channel-specific.
        (try (shutdown-agents) (catch Throwable _ nil))))
    (when (pos? @exit-code) (System/exit @exit-code))))
;;; Channel registration lives in com.blockether.vis.ext.channel-tui.core.
;;; Keep this namespace as the heavyweight runtime implementation loaded only
;;; when the TUI channel actually runs.
