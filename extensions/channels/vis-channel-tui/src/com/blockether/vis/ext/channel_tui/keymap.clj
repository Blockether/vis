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
   - `palette-chord` (Ctrl+Space) opens the searchable command palette, which
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
       :doc "Chord that opens the searchable command palette — the primary entry
   point for every app verb. Ctrl+SPACE, not a letter, because the Emacs editing
   keys (C-a/C-e/C-b/C-f/C-p/C-n/C-k/C-u/C-w/C-d) are first-class in every input
   and own those letters; the palette can't sit on Ctrl+P (prev-line) anymore."}
  palette-chord "Ctrl+Space")

(defn chord
  "Human label for a Ctrl + `key` chord, e.g. `(chord \\f)` → `\"Ctrl+F\"`.
   Single letters are upper-cased; named keys pass through (`(chord \"Enter\")`
   → `\"Ctrl+Enter\"`). Identical on every platform."
  [key]
  (let [s (str key)]
    (str "Ctrl+" (if (= 1 (count s)) (str/upper-case s) s))))

;; ── Bindings ─────────────────────────────────────────────────────────────────
;;
;; Each entry: the engine `:action`, the lowercase chord `:key` (matched against
;; the typed char while Ctrl is down), and a terse `:label`. Order is the help
;; overlay / display order. These are the FREQUENT verbs; rarer ones
;; (sessions, voice, attach file, help, close tab) are reachable
;; through the Ctrl+P palette, which lists them all. Resources also has a direct
;; Ctrl+X chord because it is exposed as a live footer affordance.
;;
;; The Emacs editing keys are FIRST-CLASS in every input and own their letters —
;; a/e/b/f/p/n/k/u/w/d (see `input/emacs-edit` + lanterna `TextEditKeymap`). So a
;; direct verb chord may ONLY use a letter that is NOT one of those. That rules
;; out F (search), B (providers), N (new-session) and P (the old palette): they
;; are PALETTE-ONLY now (reachable via Ctrl+Space, the header buttons, and the
;; `+` tab button). The remaining frequent verbs keep mnemonic chords on the
;; collision-free letters:
;;   R reasoning · L length · T model · G context dirs · X resources.
;;   (Ctrl+Space is the palette; Ctrl+H is help — both handled in `input`.)

(def bindings
  [{:action :cycle-reasoning :key \r :label "reasoning"}
   {:action :cycle-verbosity :key \l :label "length"}
   {:action :cycle-model     :key \t :label "model"}
   {:action :open-dirs       :key \g :label "context dirs"}
   {:action :open-resources  :key \x :label "resources"}])

(def ^:private action-by-char
  "Lowercase chord char → action, for O(1) dispatch."
  (into {} (map (juxt :key :action)) bindings))

(def ^:private binding-by-action
  (into {} (map (juxt :action identity)) bindings))

(defn action-for
  "The engine action bound to Ctrl + `ch` (a char), or nil. `ch` is lower-cased
   so Shift/caps don't matter."
  [ch]
  (when ch (action-by-char (Character/toLowerCase ^char ch))))

(defn label-for
  "The `Ctrl+X` chord label for an `:action`, or nil when it has no direct
   chord (so hint builders can `(some-> (label-for …) …)` uniformly — a
   palette-only verb returns nil)."
  [action]
  (some-> (binding-by-action action) :key chord))

(defn label-or-palette
  "A WORKING chord hint for `action`: its direct `Ctrl+X` chord if it has one,
   else `palette-chord` (Ctrl+Space) — every verb is in the palette, so this
   never advertises a dead key."
  [action]
  (or (label-for action) palette-chord))

;; ── Structural chords (handled directly by the input dispatcher / pickers) ───
;; The vis-side chords that are NOT app verbs. Defined here so the dispatcher and
;; the pickers reference one place instead of hardcoding magic chars.
(def ^:const help-key
  "Ctrl+H — toggle the help overlay (input dispatcher)." \h)
(def ^:const quit-key
  "Ctrl+C — quit on an empty draft, else clear it (input dispatcher)." \c)
(def palette-trigger-chars
  "Chars that, with Ctrl held, open the palette (`palette-chord`). Terminals
   send Ctrl+Space as NUL (Ctrl+@), so space / NUL / @ all count."
  #{\space (char 0) \@})
;; List pickers (providers / models) reorder the SELECTED row with the Emacs
;; prev/next-line keys. This is a MODAL context (no text being edited), so
;; reusing C-p / C-n for up / down is intentional and consistent — NOT a clash
;; with the editor, which never runs at the same time as a picker.
(def ^:const picker-reorder-up
  "Ctrl+P — move the selected picker row up." \p)
(def ^:const picker-reorder-down
  "Ctrl+N — move the selected picker row down." \n)
