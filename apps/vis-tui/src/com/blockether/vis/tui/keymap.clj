(ns com.blockether.vis.tui.keymap
  "Single source of truth for the TUI's keyboard shortcuts.

   Shortcuts are CTRL chords, identical on every platform (macOS / Linux).
   The old Alt/Option chords were removed: stock macOS terminals send
   Option+letter as a special character, never an Alt modifier, so those chords
   silently did nothing. Ctrl always reaches the app.

   ONE registry, ONE exception: every vis-side chord is defined HERE — verbs in
   `bindings`, structural keys (palette / help / quit) as the constants below.
   constants below. The SOLE exception is the Emacs EDITING chords
   (C-a/C-e/C-b/C-f/C-p/C-n/C-k/C-u/C-w/C-d), which live in lanterna's
   `TextEditKeymap` so they work in EVERY input (prompt + dialogs); they are
   never re-declared here, and no vis chord may reuse one of those letters
   (enforced by `keymap-test`).

   Chord labels use ONE format everywhere: Emacs notation, lower-cased —
   `C-c`, `C-g`, `C-l`, and `C-x <key>` (plain second key) for the prefixed vis
   commands (e.g. `C-x m`, `C-x s`). `chord` / `label-for` / `palette-chord` all
   emit this shape, so a hint never drifts between `Ctrl+X` and `C-x`.

   Tiers:
   - `palette-chord` (C-x p) opens the searchable command palette, which
     can run EVERY app verb. It is the discoverable entry point. (A PLAIN
     second key after C-x always reaches the app — unlike Ctrl+S/Ctrl+M — so
     the palette is `C-x p`, not `C-x C-p`.)
   named verbs (model, reasoning, length, search, attach, voice,
     named verbs (model, reasoning, length, search, attach, voice, dirs,
     resources, help) — the C-x prefix keeps them off the editing letters, and a
     PLAIN second key keeps them clear of unusable Ctrl bytes (Ctrl+S=flow
     control, Ctrl+M=Enter).
   - `bindings` (direct Ctrl chords) is EMPTY: every Ctrl letter is an Emacs
     editing key, so vis verbs all live behind the C-x prefix or in the palette.

   Every surface — the input dispatcher, the pickers, footer hints, the help
   overlay, the clickable header chips — reads this namespace, so a shortcut is
   defined once and stays in sync."
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.header-model :as vh])
  (:import [com.googlecode.lanterna.input KeyStroke KeyType]))

(def
  ^{:const true
    :doc
    "Label for the chord that opens the searchable Command Palette. It is
   C-x p — the Emacs C-x prefix followed by a PLAIN `p` (`prefix-palette-key`).
   This is the RELIABLE, no-config trigger: both bytes reach the app on macOS
   AND Linux, nothing OS-grabs them, and it touches no editing key. C-x C-p
   still ALSO opens it (old muscle memory), and M-x (Alt/Option+x) is the
   canonical Emacs alias (needs \"Use Option as Meta\" on macOS), but C-x p is
   the displayed binding."}
  palette-chord
  "C-x p")

(defn chord
  "Human label for a Ctrl + `key` chord in Emacs notation, e.g. `(chord \\f)` →
   `\"C-f\"`. Single letters are LOWER-cased (Emacs style); named keys pass
   through verbatim (`(chord \"Enter\")` → `\"C-Enter\"`). This is the ONE chord
   format across the whole TUI — `label-for`/`palette-chord` use the same
   `C-x C-…` shape, so a hint never drifts between `Ctrl+X` and `C-x`. Identical
   on every platform."
  [key]
  (let [s (str key)]
    (str "C-" (if (= 1 (count s)) (str/lower-case s) s))))

;; ── App-verb bindings ────────────────────────────────────────────────────────
;;
;; Direct single-Ctrl-letter verb chords are GONE: every Ctrl letter is now a
;; FAITHFUL Emacs key — editing (C-a/C-e/C-b/C-f/C-p/C-n/C-k/C-d/C-t/C-u/C-w via
;; lanterna `TextEditKeymap`), abort (C-g = keyboard-quit). vis's own commands —
;; INCLUDING help (C-x h) — live behind the Emacs PREFIX key: C-x, then a PLAIN
;; letter (like Emacs `C-x b` / `C-x o`), or in the C-x C-p palette. So
;; `bindings` (direct chords) is now EMPTY.

(def ^:const prefix-key
  "The Emacs prefix key for vis commands: C-x, then one of `prefix-commands`.
   C-x is Emacs's own command prefix, so this is the faithful home for vis verbs."
  \x)

(def ^:const prefix-delete-key
  "C-x D (Shift+d) — delete the current session after a confirmation. `D` also
   deletes the row in the project sidebar. C-x Backspace and C-x Delete do the same.
   Every lower-case letter after C-x is taken."
  \D)

(def prefix-commands
  "C-x <key> → app verb. `:key` is the SECOND key pressed after the C-x prefix,
   displayed and pressed as a PLAIN letter (`C-x s`, `C-x m`, …). A Ctrl'd second
   key is NOT reliable: `Ctrl+S`/`Ctrl+Q` are tty flow-control (eaten before the
   app) and `Ctrl+M` == Enter (byte 0x0D). The dispatcher still ALSO accepts a
   Ctrl'd second key where it survives (so `C-x C-f` == `C-x f`), but the plain
   form is the one we advertise because it works for EVERY letter.

   `:group` is the heading the verb sits under in the C-x HYDRA (`prefix-spec`)
   and must be one of `prefix-groups`; order inside a group is declaration order.
   An optional `:show-when` tag (`:multi-tab`, `:has-turns`, `:codex`) gates the verb's
   HYDRA row to the context where it can act; `:never` keeps it palette-only. The
   chord, palette, and help still list every verb."
  [{:action :cycle-model :key \m :label "model" :group "Model"}
   {:action :pick-model :key \c :label "choose model" :group "Model"}
   {:action :pick-reasoning :key \r :label "reasoning" :group "Model"}
   ;; Fast mode acts only on OpenAI Codex, so other sessions do not see it (#333).
   {:action :toggle-codex-fast :key \q :label "fast mode" :group "Model" :show-when :codex}
   {:action :cycle-verbosity :key \l :label "length" :group "Model" :show-when :never}
   ;; `x` = show or omit the Claude thinking summary. Palette-only: the Model band
   ;; keeps four rows, and the footer chip carries the chord.
   {:action :toggle-thinking-summary
    :key \x
    :label "thinking summary"
    :group "Model"
    :show-when :never}
   {:action :search-open :key \f :label "search" :group "Buffer" :show-when :never}
   {:action :pick-file :key \a :label "attach file" :group "Tools"}
   {:action :focus-attachments
    :key \i
    :label "inspect attachments"
    :group "Tools"
    :show-when :never}
   ;; C-x I (Shift+i) shows every picture of the session in the image viewer, next to
   ;; `i` for the attachment inspector. Only a graphical terminal can show the viewer, so the
   ;; hydra lists it only there, and only when the session has a picture.
   {:action :view-images :key \I :label "view images" :group "Tools" :show-when :image-viewer}
   ;; C-x O (Shift+o) opens the newest picture of the session in the system viewer. It works
   ;; on every terminal, also where the TUI cannot show pictures.
   {:action :open-newest-image :key \O :label "open picture" :group "Tools" :show-when :has-images}
   {:action :toggle-voice-recording :key \v :label "voice" :group "Tools"}
   ;; `b` = the conversation MODE, next to `v` = one recording. Arming is not a
   ;; setting: it says SPEAK THE ANSWERS OF THIS CONVERSATION, and only this one.
   ;; Palette-only because the Tools pane is FULL: a fifth verb there re-packs the
   ;; hydra and costs six rows of transcript in a 30-row terminal (screen-test pins
   ;; the band's top row). The chord, the palette and help still carry it, and while
   ;; the mode is armed the header banner says so.
   {:action :toggle-voice-conversation
    :key \b
    :label "voice conversation"
    :group "Tools"
    :show-when :never} {:action :toggle-help :key \h :label "help" :group "Buffer"}
   {:action :new-session :key \n :label "new session" :group "Session"}
   {:action :show-sessions :key \s :label "switch session" :group "Session"}
   {:action :delete-session :key prefix-delete-key :label "delete session" :group "Session"}
   ;; C-x A (Shift+a) archives the current session or restores it, like `A` in the sidebar.
   {:action :archive-session :key \A :label "archive session" :group "Session"}
   {:action :session-metrics :key \u :label "session metrics" :group "Session"}
   ;; `k` = kick the queue: mark every queued message for the running turn's next
   ;; step (the header's Send all now button). Palette-only — the Session band is full.
   {:action :queue-send-all-now :key \k :label "send queue now" :group "Session" :show-when :never}
   ;; `e` = edit: the keys act on the Queued block until Esc, so a queued message can be
   ;; edited, sent, removed or merged (`input/queue-focus-event`).
   ;; Palette-only for the same reason; the hint under the queue shows the chord.
   {:action :queue-edit :key \e :label "edit queued" :group "Session" :show-when :never}
   ;; C-x T (Shift+t) shows or hides the agent table under the one-line agent dock. The dock
   ;; line shows the chord, so the hydra does not list it.
   {:action :toggle-agent-dock :key \T :label "agent details" :group "Session" :show-when :never}
   ;; fork: `y` = the Y-shaped split of a branch; `t` = fork AT a chosen turn.
   {:action :fork-session :key \y :label "fork session" :group "Session" :show-when :has-turns}
   {:action :fork-at-turn :key \t :label "fork at turn" :group "Session" :show-when :has-turns}
   {:action :switch-project :key \w :label "project sidebar" :group "Session"}
   ;; A group is the folder a session is filed in, so `d` (Emacs' dired letter)
   ;; carries it: `g` can never be a prefix key, because C-g is the abort.
   ;; Palette-only — the Session band of the hydra is full, and the project rail
   ;; advertises its own `g` menu.
   {:action :session-group :key \d :label "move to group" :group "Session" :show-when :never}
   {:action :recenter :key \j :label "jump to bottom" :group "Buffer"}
   ;; C-x V (Shift+v) opens the side panel, then moves the focus between it and the
   ;; transcript.
   {:action :live-panel :key \V :label "side panel" :group "Buffer" :show-when :has-panel}
   ;; `z` = vim's fold prefix — the jump-label overlay toggles folds.
   {:action :toggle-detail-labels :key \z :label "label folds" :group "Buffer"}
   ;; C-x Z (Shift+z) turns Zen mode on and off: the screen shows only the transcript
   ;; and the prompt, without the header and the footer.
   {:action :toggle-zen-mode :key \Z :label "zen mode" :group "Buffer"}
   {:action :providers :key \o :label "providers" :group "Tools"}])

(def prefix-groups
  "Heading order of the C-x hydra. A band taller than the terminal loses its LAST
   rows (`transient/band-geometry`), so the groups are ordered by how often the
   verb under them is the one being reached for."
  ["Session" "Model" "Buffer" "Tools"])

(def bindings
  "Direct (single-chord) app verbs — EMPTY now. Every verb moved behind the C-x
   prefix or into the M-x palette, freeing the Ctrl letters for Emacs editing."
  [])

(def ^:private action-by-char
  "Lowercase DIRECT-chord char → action (empty now)."
  (into {} (map (juxt :key :action)) bindings))

(def ^:private binding-by-action (into {} (map (juxt :action identity)) bindings))

(defn- index-commands
  "The lookup maps of the merged C-x commands `commands`."
  [commands refused]
  {:commands commands
   :refused refused
   :by-char (into {} (keep #(when (:key %) [(:key %) %])) commands)
   :by-action (into {} (map (juxt :action identity)) commands)})

(defonce ^:private merged-keymap
  ;; The C-x commands after the key bindings of the session's extensions.
  (atom (index-commands prefix-commands [])))

(def reserved-prefix-keys
  "Keys after C-x that no extension can bind: the palette, the abort key and the
   queue digits."
  (into #{\p \g} (map char) (range (int \0) (inc (int \9)))))

(def default-binding-group "The C-x heading of an extension command." "Extensions")

(defn- binding-char
  "The one printable ASCII character of binding key `k`, or nil."
  [k]
  (when (and (string? k) (= 1 (count k)))
    (let [c (.charAt ^String k 0)]
      (when (<= 33 (long (int c)) 126) c))))

(defn- refuse [b reason] {:binding b :reason reason})

(defn- move-verb
  "Phase 1 of the merge: binding `b` moves a built-in verb to its key."
  [{:keys [commands claimed] :as acc} b]
  (let [c
        (binding-char (:key b))

        verb
        (keyword (str (:action b)))

        idx
        (first (keep-indexed #(when (= verb (:action %2)) %1) commands))]

    (cond (nil? c) (update acc :refused conj (refuse b "the key is not one printable character"))
          (reserved-prefix-keys c) (update acc :refused conj (refuse b "the key is reserved"))
          (nil? idx) (update acc :refused conj (refuse b "the TUI has no such verb"))
          (contains? (:moved acc) verb)
          (update acc :refused conj (refuse b "the verb moved already"))
          (contains? claimed c) (update acc :refused conj (refuse b "another binding has the key"))
          :else (let [commands
                      (mapv #(if (and (= c (:key %)) (not= verb (:action %))) (dissoc % :key) %)
                            commands)]
                  (-> acc
                      (assoc :commands (update commands
                                               idx
                                               #(cond-> (assoc % :key c) (:label b) (assoc :label
                                                                                      (:label b))
                                                  (:group b) (assoc :group (:group b)))))
                      (update :claimed conj c)
                      (update :moved conj verb))))))

(defn- add-command
  "Phase 2 of the merge: binding `b` runs a slash command from a free key."
  [{:keys [commands] :as acc} b]
  (let [c
        (binding-char (:key b))

        command
        (str/trim (str (:command b)))]

    (cond (nil? c) (update acc :refused conj (refuse b "the key is not one printable character"))
          (reserved-prefix-keys c) (update acc :refused conj (refuse b "the key is reserved"))
          (not (str/starts-with? command "/"))
          (update acc :refused conj (refuse b "the command does not start with /"))
          (some #(= c (:key %)) commands) (update acc :refused conj (refuse b "the key is taken"))
          :else (update acc
                        :commands
                        conj
                        {:action (keyword "keybinding" (str c))
                         :key c
                         :label (or (not-empty (str (:label b))) command)
                         :group (or (not-empty (str (:group b))) default-binding-group)
                         :command command
                         :extension (:extension b)}))))

(defn merge-keybindings
  "PURE: the C-x commands after extension `bindings` (maps with `:key`, `:command`
   or `:action`, `:label`, `:group` and `:extension`, in gateway order).

   First every `:action` binding moves its built-in verb to its key. A built-in verb
   that had that key keeps only the palette. Then every `:command` binding takes a
   free key. A reserved, taken or malformed binding is refused. Answers
   `{:commands [...] :refused [{:binding b :reason text}]}`."
  [bindings]
  (let [moved
        (reduce move-verb
                {:commands prefix-commands :claimed #{} :moved #{} :refused []}
                (filter :action bindings))

        added
        (reduce add-command moved (remove :action bindings))]

    (select-keys added [:commands :refused])))

(defn set-keybindings!
  "Use extension `bindings` for C-x from now on. Answers the refused bindings."
  [bindings]
  (let [{:keys [commands refused]} (merge-keybindings bindings)]
    (reset! merged-keymap (index-commands commands refused))
    refused))

(defn refused-keybindings
  "The extension bindings that the current keymap refused."
  []
  (:refused @merged-keymap))

(defn free-prefix-letters
  "The letters after C-x that an extension can still bind, as one string: lower case
   first, then upper case."
  []
  (let [taken (set (keep :key (:commands @merged-keymap)))]
    (apply str
      (remove #(or (taken %) (reserved-prefix-keys %))
        (concat (map char (range (int \a) (inc (int \z))))
                (map char (range (int \A) (inc (int \Z)))))))))

(defn current-prefix-commands
  "Every C-x command after the extension bindings, built-in verbs first."
  []
  (:commands @merged-keymap))

(defn action-for
  "The action bound to a DIRECT Ctrl + `ch` chord, or nil — always nil now (verbs
   are C-x-prefixed). Kept so the dispatcher's direct-chord clause stays total."
  [ch]
  (when ch (action-by-char (Character/toLowerCase ^char ch))))

(defn prefix-command-for
  "The C-x command bound to `ch`, or nil. An exact match wins, so C-x D is not
   C-x d; any other key is lower-cased."
  [ch]
  (when ch
    (let [by-char (:by-char @merged-keymap)]
      (or (by-char ch) (by-char (Character/toLowerCase ^char ch))))))

(defn prefix-action-for
  "The verb action bound to the C-x prefix followed by `ch`, or nil. An exact match
   wins, so C-x D is not C-x d; any other key is lower-cased."
  [ch]
  (:action (prefix-command-for ch)))

(defn queue-row-index
  "The 1-based queue row that C-x followed by `ch` toggles for the next step, or nil.
   Only the digits 1-9 bind; row 1 is the next message to send."
  [ch]
  (when (and ch (Character/isDigit ^char ch) (not= \0 ch)) (long (Character/digit ^char ch 10))))

(defn queue-row-chord
  "The chord label that toggles queue row `n` (1-based), or nil past row 9."
  [n]
  (when (<= 1 (long n) 9) (str "C-x " n)))

(defn label-for
  "Display label for `action`'s shortcut, or nil. Direct chord → `C-l`; a C-x
   prefix command → `C-x <key>` with a PLAIN second key (e.g. `C-x s`).

   The second key is PLAIN, never `C-x C-<key>`: a Ctrl'd second key is unusable
   for several letters — `Ctrl+S`/`Ctrl+Q` are tty flow-control (XOFF/XON, eaten
   before the app sees them) and `Ctrl+M` is byte 0x0D = Enter (indistinguishable
   from Return). A plain letter byte always reaches the app, so `C-x s`,
   `C-x m`, … work everywhere. (`resolve-prefix-key` still ALSO accepts a Ctrl'd
   second key for the letters where it survives, so old muscle memory keeps
   working — the displayed hint is just the reliable one.) palette-only → nil."
  [action]
  (or (when (= action :toggle-all-details) "C-x TAB")
      (some-> (binding-by-action action)
              :key
              chord)
      (when-let [k (get-in @merged-keymap [:by-action action :key])]
        (str "C-x " k))))

(defn label-or-palette
  "A WORKING chord hint for `action`: its direct/prefix chord if it has one, else
   `palette-chord` (C-x C-p) — every verb is in the palette, so this never
   advertises a dead key."
  [action]
  (or (label-for action) palette-chord))

;; ── Structural chords (handled directly by the input dispatcher / pickers) ───
;; The vis-side chords that are NOT app verbs. Defined here so the dispatcher and
;; the pickers reference one place instead of hardcoding magic chars.
;; Global help is the C-x h verb in `prefix-commands`. The sidebar also owns
;; shorter help keys while it has focus.
(def ^:const quit-key "C-c — quit on an empty draft, else clear it (terminal reflex)." \c)

(def ^:const focus-key
  "C-o — give the keyboard to the next open area: the project sidebar, the chat or
   the side panel. Emacs moves between windows with C-x o; Vis drops the prefix."
  \o)

(defn focus-key?
  "True when `key` is the `focus-key` chord."
  [key]
  (and (instance? KeyStroke key)
       (= KeyType/Character (.getKeyType ^KeyStroke key))
       (.isCtrlDown ^KeyStroke key)
       (not (.isAltDown ^KeyStroke key))
       (= focus-key (Character/toLowerCase (.charValue (.getCharacter ^KeyStroke key))))))

(def ^:const sidebar-help-key "C-h — open or close help while the sidebar has focus." \h)

(def ^:const sidebar-help-alias-key
  "? — sidebar help outside an inline search or project field."
  \?)

(def sidebar-help-hint
  "The sidebar help keys, shared by dispatch, the key list and the help card."
  (str (chord sidebar-help-key) " / " sidebar-help-alias-key))

(def sidebar-commands
  "The project sidebar's own keys while it has focus, in the order help lists
   them. `:key` is the plain character the sidebar reads; `:keys` labels a key
   that is not one plain character. Row menus show the same keys beside their
   items, and the help keys list this table inside the sidebar."
  [{:id :open :keys "Enter" :label "Open the row"} {:id :menu :key \g :label "Open the row menu"}
   {:id :new-session :key \n :label "Start a session here"}
   {:id :new-group :key \G :label "Create a group"}
   {:id :add-project :key \a :label "Add a project"}
   {:id :settings :key \s :label "Open settings for the row"}
   {:id :select :key \space :label "Select or clear a session"}
   {:id :move :key \m :label "Move sessions to a group"}
   {:id :ungroup :key \u :label "Take sessions out of a group"}
   {:id :details :key \d :label "Show session details"}
   {:id :star :key \f :label "Star or unstar a session"}
   {:id :rename :key \R :label "Rename the row"}
   {:id :recolour :key \c :label "Change the group colour"}
   {:id :archive :key \A :label "Archive or restore the row"}
   {:id :show-archived :key \v :label "Show or hide archived rows"}
   {:id :delete :key \D :label "Delete the row"}
   {:id :add-here :key \+ :label "Add a group, session or project here"}
   {:id :search :key \/ :label "Search sessions"} {:id :refresh :key \r :label "Reload projects"}
   {:id :help :key sidebar-help-alias-key :keys sidebar-help-hint :label "Show these keys"}
   {:id :back :keys "Esc · Tab" :label "Return to the chat"}
   {:id :areas :keys (chord focus-key) :label "Move to the next area"}
   {:id :hide :keys (label-for :switch-project) :label "Hide the sidebar"}
   {:id :rows :keys "↑ ↓ · C-p C-n" :label "Move between rows"}
   {:id :fold :keys "← →" :label "Fold or unfold the row"}
   {:id :jump :keys "PgUp PgDn · Home End" :label "Jump a page or to an end"}])

(defn sidebar-key
  "The plain character of the `sidebar-commands` entry `id`, or nil."
  [id]
  (some #(when (= id (:id %)) (:key %)) sidebar-commands))

(defn sidebar-key-label
  "The printed key of a `sidebar-commands` entry: its `:keys` label, `Space` or
   the character itself."
  [{:keys [key keys]}]
  (cond keys keys
        (= \space key) "Space"
        key (str key)
        :else ""))

(def ^:const abort-key
  "C-g — Emacs `keyboard-quit` (abort): cancel a running turn / close a
   dialog / clear the draft. The ctrl-char the dispatcher matches; Esc is its
   mirror. See `abort-keys` for the full advertised set."
  \g)

(def abort-keys
  "The COMPLETE set of keys that abort (cancel a running turn / close a dialog /
   clear the draft): C-g (Emacs `keyboard-quit`) and its Esc mirror. ONE source
   of truth — the dispatcher recognizes all of them and the footer/echo hint
   advertises all of them, so an abort key never fires without being shown, and
   the two never drift apart. Ordered for a stable hint (`abort-hint`)."
  [(chord abort-key) "Esc"])

(defn abort-hint
  "The advertised abort chord for a footer/echo hint — every `abort-keys` label
   joined, e.g. `C-g / Esc`."
  []
  (str/join " / " abort-keys))

(def ^:const recenter-key
  "C-l — Emacs `recenter`: jump the conversation to the bottom + repaint."
  \l)

(def ^:const prefix-palette-key
  "C-x p — after the C-x prefix, this key opens the Command Palette. A PLAIN `p`
   is the primary, reliable, no-config, Emacs-idiomatic palette trigger; a Ctrl'd
   second key (C-x C-p) is ALSO accepted for old muscle memory."
  \p)

(def ^:const palette-meta-key
  "M-x palette alias: Alt/Option + this key also opens the palette (the canonical
   Emacs command launcher; needs \"Use Option as Meta\" on macOS).
   `x` = `execute-extended-command`."
  \x)

;; ── The C-x hydra ───────────────────────────────────────────────────────────
;; C-x is not a dead prefix waiting in the echo area: it opens a
;; TRANSIENT band listing every verb the next keystroke can run, exactly like an
;; Emacs hydra / doom leader. The spec is PURE data here; `dialogs/prefix-band!`
;; paints it and `input/resolve-prefix-key` still owns the keystroke, so the band
;; can never bind a key the chord itself does not.

(defn verb-available?
  "Does a prefix-command's `:show-when` tag hold for the current `db`? Untagged
   verbs always apply; the tagged ones surface in the hydra ONLY where they can
   act — `:multi-tab` needs a second tab to close, `:has-turns` needs a turn to
   fork at, `:codex` needs a session on OpenAI Codex (the screen marks it as
   `:codex-session?`), `:has-images` needs a picture in the session (the screen
   marks it as `:has-images?`), `:image-viewer` also needs a graphical terminal (the
   screen marks it as `:graphical-terminal?`), `:has-panel` needs a panel view, and `:never` is
   palette-only. Resolving a chord
   does not bypass its handler's feature gate."
  [db {:keys [show-when]}]
  (case show-when
    :multi-tab
    (> (count (vh/project-tabs db)) 1)

    :has-turns
    (boolean (seq (:messages db)))

    :codex
    (true? (:codex-session? db))

    :has-images
    (true? (:has-images? db))

    :image-viewer
    (and (true? (:has-images? db)) (true? (:graphical-terminal? db)))

    :has-panel
    (boolean (some #(= :panel (get-in % [:view :placement])) (:live-views db)))

    :never
    false

    true))

(defn available-prefix-commands
  "`prefix-commands` filtered to the verbs that can act in `db`, in declaration
   order."
  [db]
  (filterv #(and (:key %) (verb-available? db %)) (current-prefix-commands)))

(defn prefix-spec
  "PURE: the `transient/run!` spec for the C-x hydra in `db` — one group per
   `prefix-groups` heading that has a verb to show. The palette is a TOOL like
   any other — it sits at the end of the Tools column instead of buying a
   heading of its own — and it is the way to every verb this band had no room
   or no context for.

   Rows are display-only: the band paints them and hands the raw keystroke back
   to `input/resolve-prefix-key`, so the label and the key can never drift from
   the chord that actually fires."
  [db]
  (let [by-group
        (group-by :group (available-prefix-commands db))

        palette
        {:key (str prefix-palette-key) :type :action :id :show-palette :label "command palette…"}]

    {:title (str (chord prefix-key) " — vis commands")
     :groups (into []
                   (keep (fn [g]
                           (let [items
                                 (cond-> (mapv
                                           (fn [{:keys [action key label]}]
                                             {:key (str key) :type :action :id action :label label})
                                           (get by-group g))
                                   (= g "Tools")
                                   (conj palette))]
                             (when (seq items) {:title g :items items}))))
                   (into prefix-groups
                         (comp (map :group) (distinct) (remove (set prefix-groups)))
                         (current-prefix-commands)))}))
