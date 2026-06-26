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

   Tiers:
   - `palette-chord` (M-x) opens the searchable command palette, which
     can run EVERY app verb. It is the discoverable entry point. It is NOT a
     letter, because the Emacs editing keys own the letters.
   - `bindings` are the direct Ctrl chords for the FREQUENT verbs, on the
     collision-free letters (R/L/T/G/X) — never an editing letter, never a
     control-code collision (Ctrl+M=Enter, Ctrl+I=Tab, Ctrl+H=BS, Ctrl+S/Q=flow,
     Ctrl+O=stty DISCARD, Ctrl+Y=DSUSP). Rarer verbs stay palette-only.

   Every surface — the input dispatcher, the pickers, footer hints, the help
   overlay, the clickable header chips — reads this namespace, so a shortcut is
   defined once and stays in sync."
  (:require [clojure.string :as str]))

(def ^{:const true
       :doc "Label for the chord that opens the searchable command palette. It is
   M-x — the authentic Emacs `execute-extended-command`. Two triggers open it
   (see `input/palette-trigger?` + the constants below):
   - M-x = Alt/Option+x (the Emacs idiom). Lives in the META keyspace, so it
     touches NONE of the Ctrl editing keys or verbs. Works on Linux out of the
     box; on macOS enable \"Use Option as Meta key\" (the standard Emacs-on-Mac
     terminal setting).
   - Ctrl+] = a zero-config FALLBACK for terminals where Meta is dead (default
     macOS). Byte 0x1d, decoded by lanterna to `]`+ctrl; never OS-grabbed, not a
     letter (the Emacs keys own those), and in Emacs C-] is the obscure
     abort-recursive-edit, so it steals nothing.
   The label shows M-x because that is the discoverable Emacs name."}
  palette-chord "M-x")

(defn chord
  "Human label for a Ctrl + `key` chord, e.g. `(chord \\f)` → `\"Ctrl+F\"`.
   Single letters are upper-cased; named keys pass through (`(chord \"Enter\")`
   → `\"Ctrl+Enter\"`). Identical on every platform."
  [key]
  (let [s (str key)]
    (str "Ctrl+" (if (= 1 (count s)) (str/upper-case s) s))))

;; ── App-verb bindings ────────────────────────────────────────────────────────
;;
;; Direct single-Ctrl-letter verb chords are GONE: every Ctrl letter is now a
;; FAITHFUL Emacs key — editing (C-a/C-e/C-b/C-f/C-p/C-n/C-k/C-d/C-t/C-u/C-w via
;; lanterna `TextEditKeymap`), abort (C-g = keyboard-quit), help (C-h). vis's own
;; commands live behind the Emacs PREFIX key: C-x, then a letter (exactly like
;; Emacs `C-x C-s` / `C-x C-f`), or in the M-x palette. So `bindings` (direct
;; chords) is now EMPTY.

(def ^:const prefix-key
  "The Emacs prefix key for vis commands: Ctrl+X, then one of `prefix-commands`.
   C-x is Emacs's own command prefix, so this is the faithful home for vis verbs."
  \x)

(def prefix-commands
  "C-x <key> → app verb. `:key` is the SECOND key pressed after the C-x prefix.
   Order is the which-key / help display order."
  [{:action :cycle-model     :key \m :label "model"}
   {:action :cycle-reasoning :key \r :label "reasoning"}
   {:action :cycle-verbosity :key \v :label "length"}
   {:action :open-dirs       :key \d :label "context dirs"}
   {:action :open-resources  :key \s :label "resources"}])

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
  "Display label for `action`'s shortcut, or nil. Direct chord → `Ctrl+X`; a C-x
   prefix command → `Ctrl+X M`; palette-only → nil (hint builders
   `(some-> (label-for …) …)` uniformly)."
  [action]
  (or (some-> (binding-by-action action) :key chord)
    (when-let [b (prefix-binding-by-action action)]
      (str (chord prefix-key) " " (str/upper-case (str (:key b)))))))

(defn label-or-palette
  "A WORKING chord hint for `action`: its direct/prefix chord if it has one, else
   `palette-chord` (M-x) — every verb is in the palette, so this never advertises
   a dead key."
  [action]
  (or (label-for action) palette-chord))

;; ── Structural chords (handled directly by the input dispatcher / pickers) ───
;; The vis-side chords that are NOT app verbs. Defined here so the dispatcher and
;; the pickers reference one place instead of hardcoding magic chars.
(def ^:const help-key
  "Ctrl+H — toggle the help overlay (input dispatcher)." \h)
(def ^:const quit-key
  "Ctrl+C — quit on an empty draft, else clear it (terminal reflex)." \c)
(def ^:const abort-key
  "Ctrl+G — Emacs `keyboard-quit` (abort): cancel a running turn / close a
   dialog / clear the draft. Mirrors Escape." \g)
(def ^:const recenter-key
  "Ctrl+L — Emacs `recenter`: jump the conversation to the bottom + repaint." \l)
(def ^:const palette-meta-key
  "The letter of the Emacs M-x palette trigger: Alt/Option + this key opens the
   command palette. `x` = `execute-extended-command`." \x)
(def palette-trigger-chars
  "Char(s) that, with Ctrl held, open the palette as the FALLBACK to M-x (for
   terminals where Meta is dead). A terminal sends byte 0x1d for Ctrl+], which
   lanterna decodes into the character `]` (CtrlAndCharacterPattern), so that is
   what the dispatcher matches. A set so the trigger can grow in one place."
  #{\]})
;; List pickers (providers / models) reorder the SELECTED row with the Emacs
;; prev/next-line keys. This is a MODAL context (no text being edited), so
;; reusing C-p / C-n for up / down is intentional and consistent — NOT a clash
;; with the editor, which never runs at the same time as a picker.
(def ^:const picker-reorder-up
  "Ctrl+P — move the selected picker row up." \p)
(def ^:const picker-reorder-down
  "Ctrl+N — move the selected picker row down." \n)
