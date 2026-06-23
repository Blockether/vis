(ns com.blockether.vis.ext.channel-tui.keymap
  "Single source of truth for the TUI's Alt/Option chord shortcuts.

   The whole F-key row was retired in favour of Alt chords (Option on macOS):
   one modifier, one mnemonic letter, reachable without leaving the home row.
   The same modifier is reported by Lanterna's `.isAltDown` on every platform
   — what differs is only how it READS to the user: macOS keyboards print it
   as `⌥` (Option), everywhere else as `Alt`. So detection is uniform and only
   the LABEL is conditional.

   Everything that needs to know about a shortcut — the input dispatcher, the
   footer hints, the F1 help overlay, the clickable header chips — reads from
   `bindings` / `action-for` / `chord` here, so a new shortcut is added in
   exactly one place and every surface stays in sync."
  (:require [clojure.string :as str]))

;; ── Platform ────────────────────────────────────────────────────────────────

(def ^{:doc "True on macOS, where the Alt modifier is the Option (⌥) key."}
  mac?
  (delay (str/includes? (str/lower-case (or (System/getProperty "os.name") ""))
           "mac")))

(defn alt-prefix
  "The modifier label for THIS platform: `\"⌥\"` on macOS (Option), `\"Alt+\"`
   elsewhere. A prefix, so `(str (alt-prefix) \"H\")` reads `⌥H` / `Alt+H`."
  []
  (if @mac? "⌥" "Alt+"))

(defn chord
  "Human label for an Alt/Option + `key` chord, e.g. `(chord \\h)` → `\"⌥H\"`
   on macOS, `\"Alt+H\"` elsewhere. Single letters are upper-cased (⌥H); named
   keys pass through verbatim (`(chord \"Enter\")` → `⌥Enter`)."
  [key]
  (let [s (str key)]
    (str (alt-prefix) (if (= 1 (count s)) (str/upper-case s) s))))

;; ── Bindings ─────────────────────────────────────────────────────────────────
;;
;; Each entry: the engine `:action`, the lowercase chord `:key`, and a terse
;; `:label` for hints/help. Order is the help-overlay display order. These are
;; the SIMPLE action chords (one keystroke → one action); the dispatcher keeps
;; a few structural chords inline (emacs word-motion ⌥B/⌥F, tab jumps ⌥1-9,
;; ⌥Enter newline) because they take arguments or transform input state.

(def bindings
  [{:action :toggle-help           :key \h :label "help"}
   {:action :search-open           :key \g :label "search"}
   {:action :open-resources        :key \j :label "resources"}
   {:action :show-palette          :key \x :label "command palette"}
   {:action :show-sessions         :key \s :label "sessions · workspaces"}
   {:action :cycle-model           :key \m :label "cycle model"}
   {:action :cycle-reasoning       :key \r :label "cycle reasoning"}
   {:action :cycle-verbosity       :key \l :label "cycle length"}
   {:action :toggle-voice-recording :key \v :label "voice recording"}
   {:action :pick-file             :key \o :label "open / pick file"}
   {:action :open-dirs             :key \d :label "context dirs"}
   {:action :close-tab             :key \w :label "close tab"}])

(def ^:private action-by-char
  "Lowercase chord char → action, for O(1) dispatch."
  (into {} (map (juxt :key :action)) bindings))

(def ^:private binding-by-action
  (into {} (map (juxt :action identity)) bindings))

(defn action-for
  "The engine action bound to Alt/Option + `ch` (a char), or nil. `ch` is
   lower-cased so Shift/caps don't matter."
  [ch]
  (when ch (action-by-char (Character/toLowerCase ^char ch))))

(defn label-for
  "The `⌥X` / `Alt+X` chord label for an `:action`, or nil when it has no
   chord (so hint builders can `(some-> (label-for …) …)` uniformly)."
  [action]
  (some-> (binding-by-action action) :key chord))
