(ns com.blockether.vis.ext.channel-tui.keymap
  "Single source of truth for the TUI's keyboard shortcuts.

   Shortcuts are CTRL chords, identical on every platform (macOS / Windows /
   Linux). The old Alt/Option chords were removed: stock macOS terminals send
   Option+letter as a special character, never an Alt modifier, so those chords
   silently did nothing. Ctrl always reaches the app.

   ONE registry, ONE exception: every vis-side chord is defined HERE — verbs in
   `bindings`, structural keys (palette / help / quit / picker-reorder) as the
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
   - `prefix-commands` are the `C-x <key>` plain-second-key shortcuts for the
     named verbs (model, reasoning, length, search, attach, voice, dirs,
     resources, help) — the C-x prefix keeps them off the editing letters, and a
     PLAIN second key keeps them clear of unusable Ctrl bytes (Ctrl+S=flow
     control, Ctrl+M=Enter).
   - `bindings` (direct Ctrl chords) is EMPTY: every Ctrl letter is an Emacs
     editing key, so vis verbs all live behind the C-x prefix or in the palette.

   Every surface — the input dispatcher, the pickers, footer hints, the help
   overlay, the clickable header chips — reads this namespace, so a shortcut is
   defined once and stays in sync."
  (:require [clojure.string :as str]))

(def ^{:const true
       :doc "Label for the chord that opens the searchable Command Palette. It is
   C-x p — the Emacs C-x prefix followed by a PLAIN `p` (`prefix-palette-key`).
   This is the RELIABLE, no-config trigger: both bytes reach the app on macOS
   AND Linux, nothing OS-grabs them, and it touches no editing key. C-x C-p
   still ALSO opens it (old muscle memory), and M-x (Alt/Option+x) is the
   canonical Emacs alias (needs \"Use Option as Meta\" on macOS), but C-x p is
   the displayed binding."}
  palette-chord "C-x p")

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

(def prefix-commands
  "C-x <key> → app verb. `:key` is the SECOND key pressed after the C-x prefix,
   displayed and pressed as a PLAIN letter (`C-x s`, `C-x m`, …). A Ctrl'd second
   key is NOT reliable: `Ctrl+S`/`Ctrl+Q` are tty flow-control (eaten before the
   app) and `Ctrl+M` == Enter (byte 0x0D). The dispatcher still ALSO accepts a
   Ctrl'd second key where it survives (so `C-x C-f` == `C-x f`), but the plain
   form is the one we advertise because it works for EVERY letter.
   Order is the which-key / help display order."
  [{:action :cycle-model            :key \m :label "model"}
   {:action :pick-model             :key \o :label "model picker"}
   {:action :cycle-reasoning        :key \r :label "reasoning"}
   {:action :cycle-verbosity        :key \l :label "length"}
   {:action :search-open            :key \f :label "search"}
   {:action :pick-file              :key \a :label "attach file"}
   {:action :toggle-voice-recording :key \v :label "voice"}
   {:action :open-dirs              :key \d :label "filesystem"}
   {:action :open-resources         :key \s :label "resources"}
   {:action :toggle-help            :key \h :label "help"}
   {:action :new-session            :key \n :label "new session"}
   {:action :show-sessions          :key \b :label "switch session"}
   {:action :close-tab              :key \k :label "close tab"}
   {:action :recenter               :key \j :label "jump to bottom"}])

(def bindings
  "Direct (single-chord) app verbs — EMPTY now. Every verb moved behind the C-x
   prefix or into the M-x palette, freeing the Ctrl letters for Emacs editing."
  [])

(def ^:private action-by-char
  "Lowercase DIRECT-chord char → action (empty now)."
  (into {} (map (juxt :key :action)) bindings))
(def ^:private binding-by-action
  (into {} (map (juxt :action identity)) bindings))
(def ^:private prefix-action-by-char
  "Lowercase SECOND-key char → action, for the C-x prefix."
  (into {} (map (juxt :key :action)) prefix-commands))
(def ^:private prefix-binding-by-action
  (into {} (map (juxt :action identity)) prefix-commands))

(defn action-for
  "The action bound to a DIRECT Ctrl + `ch` chord, or nil — always nil now (verbs
   are C-x-prefixed). Kept so the dispatcher's direct-chord clause stays total."
  [ch]
  (when ch (action-by-char (Character/toLowerCase ^char ch))))

(defn prefix-action-for
  "The verb action bound to the C-x prefix followed by `ch`, or nil. Lower-cased."
  [ch]
  (when ch (prefix-action-by-char (Character/toLowerCase ^char ch))))

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
  (or (some-> (binding-by-action action) :key chord)
    (when-let [b (prefix-binding-by-action action)]
      (str "C-x " (str/lower-case (str (:key b)))))))

(defn label-or-palette
  "A WORKING chord hint for `action`: its direct/prefix chord if it has one, else
   `palette-chord` (C-x C-p) — every verb is in the palette, so this never
   advertises a dead key."
  [action]
  (or (label-for action) palette-chord))

;; ── Structural chords (handled directly by the input dispatcher / pickers) ───
;; The vis-side chords that are NOT app verbs. Defined here so the dispatcher and
;; the pickers reference one place instead of hardcoding magic chars.
;; Help (toggle overlay) is NOT a structural const — it's a normal C-x prefix
;; command (C-x h) like every other vis verb, so it lives in `prefix-commands`.
(def ^:const quit-key
  "C-c — quit on an empty draft, else clear it (terminal reflex)." \c)
(def ^:const abort-key
  "C-g — Emacs `keyboard-quit` (abort): cancel a running turn / close a
   dialog / clear the draft. Mirrors Escape." \g)
(def ^:const recenter-key
  "C-l — Emacs `recenter`: jump the conversation to the bottom + repaint." \l)
(def ^:const prefix-palette-key
  "C-x p — after the C-x prefix, this key opens the Command Palette. A PLAIN `p`
   is the primary, reliable, no-config, Emacs-idiomatic palette trigger; a Ctrl'd
   second key (C-x C-p) is ALSO accepted for old muscle memory." \p)
(def ^:const palette-meta-key
  "M-x palette alias: Alt/Option + this key also opens the palette (the canonical
   Emacs command launcher; needs \"Use Option as Meta\" on macOS).
   `x` = `execute-extended-command`." \x)
;; List pickers (providers / models) reorder the SELECTED row with the Emacs
;; prev/next-line keys. This is a MODAL context (no text being edited), so
;; reusing C-p / C-n for up / down is intentional and consistent — NOT a clash
;; with the editor, which never runs at the same time as a picker.
(def ^:const picker-reorder-up
  "C-p — move the selected picker row up." \p)
(def ^:const picker-reorder-down
  "C-n — move the selected picker row down." \n)
