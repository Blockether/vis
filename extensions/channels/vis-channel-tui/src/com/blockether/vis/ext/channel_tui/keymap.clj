(ns com.blockether.vis.ext.channel-tui.keymap
  "Single source of truth for the TUI's keyboard shortcuts.

   Shortcuts are CTRL chords, identical on every platform (macOS / Windows /
   Linux). The old Alt/Option chords were removed: stock macOS terminals send
   Option+letter as a special character, never an Alt modifier, so those chords
   silently did nothing. Ctrl always reaches the app.

   Two tiers:
   - `palette-chord` (Ctrl+P) opens the searchable command palette, which can
     run EVERY app verb. It is the discoverable entry point.
   - `bindings` are the direct Ctrl chords for the FREQUENT verbs, chosen to
     avoid the crowded control codes (Ctrl+M=Enter, Ctrl+I=Tab, Ctrl+H=BS,
     Ctrl+S=flow-control, Ctrl+O=stty DISCARD, Ctrl+Y=DSUSP) and the emacs keys kept
     for the input box (Ctrl+A/E/K/U/W/D). Less frequent verbs stay in the
     palette unless they need an always-visible footer affordance.

   Every surface — the input dispatcher, footer hints, the help overlay, the
   clickable header chips — reads `bindings` / `action-for` / `chord` /
   `label-for` here, so a shortcut is defined once and stays in sync."
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
   else `palette-chord` (Ctrl+P) — every verb is in the palette, so this never
   advertises a dead key."
  [action]
  (or (label-for action) palette-chord))
