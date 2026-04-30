(ns com.blockether.vis.ext.channel-tui.input
  (:require [clojure.string :as str])
  (:import [com.googlecode.lanterna.input
            CharacterPattern CharacterPattern$Matching
            KeyDecodingProfile KeyStroke KeyType]
           [com.googlecode.lanterna.terminal.ansi UnixTerminal]
           [java.awt Toolkit]
           [java.awt.datatransfer DataFlavor StringSelection]))

;;; ── Custom input patterns ──────────────────────────────────────────────────
;; Lanterna's AltAndCharacterPattern rejects ISO control chars (\n, \r, \t).
;; Alt+Enter sends ESC + '\n' which gets rejected. We fix it here.

(def ^:private ESC_CHAR (Character. (char 0x1b)))

(def alt-enter-pattern
  (reify CharacterPattern
    (match [_ seq]
      (let [size (.size seq)]
        (cond
          (and (= size 2)
            (= (.get seq 0) ESC_CHAR)
            (let [c (.get seq 1)]
              (or (= c (Character. \newline))
                (= c (Character. \return)))))
          (CharacterPattern$Matching.
            (KeyStroke. KeyType/Enter false true))

          (and (= size 1)
            (= (.get seq 0) ESC_CHAR))
          CharacterPattern$Matching/NOT_YET

          :else nil)))))

;; ── Bracketed-paste mode ────────────────────────────────────────────────
;;
;; Modern terminals support "bracketed paste": when the user pastes
;; multi-line text, the terminal wraps the payload in two escape
;; sequences
;;
;;     ESC [ 200 ~   ...payload...   ESC [ 201 ~
;;
;; so the host can tell "this is a paste" apart from "the user
;; manually typed Enter". Without it, every newline in a pasted block
;; fires KeyType/Enter — which the input handler treats as "send the
;; message" — and the message ships before the user can react.
;;
;; We surface the brackets to the input loop as two KeyStrokes carrying
;; PUA marker characters (`PASTE_START_CHAR` / `PASTE_END_CHAR`). The
;; screen poll loop sees those markers, flips a buffering mode on/off,
;; accumulates every keystroke between them into one string, and feeds
;; that string to `paste-text`. Same code path as Ctrl+V — we just
;; reused the bracketed-paste sequences as the trigger.
;;
;; Enabling / disabling the mode is a separate concern: the terminal
;; only emits the brackets when we tell it to. `enable-bracketed-paste!`
;; and `disable-bracketed-paste!` write the toggling escapes directly
;; to the controlling TTY, so the screen lifecycle owns the on/off
;; bracket points.

(def ^:const PASTE_START_CHAR
  "PUA marker char used as the KeyStroke payload when the bracketed-
   paste START sequence (`ESC[200~`) arrives. Picked from the same
   PUA range our inline-style sentinels live in (U+E2xx) so a stray
   bracket lookalike in real text could never collide with it."
  \uE200)

(def ^:const PASTE_END_CHAR
  "PUA marker char for the bracketed-paste END sequence (`ESC[201~`)."
  \uE201)

(defn- bracket-pattern-prefix-match?
  "True when the sequence `s` is a prefix of `target` — used by the
   bracket patterns to return NOT_YET while the user-typed `ESC[`
   could still complete into one of our triggers."
  [^java.util.List s ^String target]
  (let [size (.size s)]
    (and (<= size (.length target))
      (loop [i 0]
        (cond
          (>= i size)             true
          (= (.charAt target i)
            (char (.charValue ^Character (.get s i)))) (recur (inc i))
          :else                   false)))))

(defn- bracket-pattern
  "Build a CharacterPattern that matches `target` (e.g. `ESC[200~`)
   exactly and returns a KeyStroke whose character is `marker`. Used
   by the bracketed-paste START / END detectors."
  [^String target marker]
  (reify CharacterPattern
    (match [_ s]
      (let [size (.size s)]
        (cond
          (and (= size (.length target))
            (bracket-pattern-prefix-match? s target))
          (CharacterPattern$Matching.
            (KeyStroke. (Character. (char marker)) false false))

          (and (< size (.length target))
            (bracket-pattern-prefix-match? s target))
          CharacterPattern$Matching/NOT_YET

          :else nil)))))

(def paste-start-pattern (bracket-pattern "\u001B[200~" PASTE_START_CHAR))
(def paste-end-pattern   (bracket-pattern "\u001B[201~" PASTE_END_CHAR))

(defn paste-marker?
  "True when `key` is the bracketed-paste START or END marker. Lets
   the screen polling loop pick the brackets out before normal key
   handling runs."
  [^KeyStroke key marker]
  (and (= KeyType/Character (.getKeyType key))
    (some? (.getCharacter key))
    (= (long marker) (long (.charValue ^Character (.getCharacter key))))))

(defn paste-start? [key] (paste-marker? key PASTE_START_CHAR))
(defn paste-end?   [key] (paste-marker? key PASTE_END_CHAR))

(defn keystroke->paste-char
  "Convert one KeyStroke received WHILE INSIDE a bracketed-paste
   block into the character it represents, so the buffer reflects
   the user's pasted bytes verbatim. Returns nil for keystrokes that
   don't map to a single text character (e.g. function keys); the
   input loop drops those."
  [^KeyStroke key]
  (case (str (.getKeyType key))
    "Character" (str (.getCharacter key))
    "Enter"     "\n"
    "Tab"       "\t"
    nil))

(defn enable-bracketed-paste!
  "Tell the terminal to wrap subsequent pastes in `ESC[200~ … ESC[201~`.
   Bytes go straight to the controlling TTY; safe to call inside the
   screen lifecycle."
  [^java.io.OutputStream out]
  (try
    (.write out (.getBytes "\u001B[?2004h" "UTF-8"))
    (.flush out)
    (catch Throwable _ nil)))

(defn disable-bracketed-paste!
  "Reverse of `enable-bracketed-paste!`. Always called in the screen
   tear-down `finally` so a crashed TUI doesn't leave the user's
   shell stuck in bracketed-paste mode."
  [^java.io.OutputStream out]
  (try
    (.write out (.getBytes "\u001B[?2004l" "UTF-8"))
    (.flush out)
    (catch Throwable _ nil)))

(defn register-custom-patterns!
  "Register Alt+Enter and bracketed-paste patterns on the terminal's
   input decoder. The two paste-bracket patterns turn the
   `ESC[200~` / `ESC[201~` markers into KeyStrokes carrying our PUA
   sentinel chars; the screen poll loop reads those to switch in/out
   of paste-buffering mode."
  [^UnixTerminal terminal]
  (.addProfile (.getInputDecoder terminal)
    (reify KeyDecodingProfile
      (getPatterns [_] [alt-enter-pattern
                        paste-start-pattern
                        paste-end-pattern]))))

;;; ── Clipboard (java.awt.datatransfer) ──────────────────────────────────────

(defn clipboard-paste []
  (try
    (let [clipboard (.getSystemClipboard (Toolkit/getDefaultToolkit))
          data      (.getContents clipboard nil)]
      (when (and data (.isDataFlavorSupported data DataFlavor/stringFlavor))
        (let [text (.getTransferData data DataFlavor/stringFlavor)]
          (when (seq text) text))))
    (catch Exception _ nil)))

(defn clipboard-copy! [^String text]
  (try
    (let [clipboard (.getSystemClipboard (Toolkit/getDefaultToolkit))
          selection (StringSelection. text)]
      (.setContents clipboard selection nil))
    (catch Exception _ nil)))

;;; ── Input buffer state ─────────────────────────────────────────────────────

(defn empty-input []
  {:lines [""] :crow 0 :ccol 0})

(defn input->text [{:keys [lines]}]
  (str/join "\n" lines))

(defn insert-char [{:keys [lines crow ccol] :as st} ch]
  (let [line     (nth lines crow)
        new-line (str (subs line 0 ccol) ch (subs line ccol))]
    (-> st
      (assoc-in [:lines crow] new-line)
      (update :ccol inc))))

(defn insert-newline [{:keys [lines crow ccol] :as st}]
  (let [line   (nth lines crow)
        before (subs line 0 ccol)
        after  (subs line ccol)]
    (-> st
      (assoc :lines (into (conj (subvec lines 0 crow) before)
                      (cons after (subvec lines (inc crow)))))
      (assoc :crow (inc crow))
      (assoc :ccol 0))))

(defn delete-backward [{:keys [lines crow ccol] :as st}]
  (cond
    (pos? ccol)
    (let [line (nth lines crow)]
      (-> st
        (assoc-in [:lines crow] (str (subs line 0 (dec ccol)) (subs line ccol)))
        (update :ccol dec)))

    (pos? crow)
    (let [previous-line (nth lines (dec crow))
          current-line  (nth lines crow)]
      (-> st
        (assoc :lines (into (conj (subvec lines 0 (dec crow)) (str previous-line current-line))
                        (subvec lines (inc crow))))
        (assoc :crow (dec crow))
        (assoc :ccol (count previous-line))))

    :else st))

(defn move-left [{:keys [lines crow ccol] :as st}]
  (cond
    (pos? ccol)  (update st :ccol dec)
    (pos? crow)  (-> st (update :crow dec) (assoc :ccol (count (nth lines (dec crow)))))
    :else        st))

(defn move-right [{:keys [lines crow ccol] :as st}]
  (let [line (nth lines crow)]
    (cond
      (< ccol (count line))         (update st :ccol inc)
      (< crow (dec (count lines)))  (-> st (update :crow inc) (assoc :ccol 0))
      :else                         st)))

(defn move-up [{:keys [lines crow ccol] :as st}]
  (if (pos? crow)
    (-> st (update :crow dec) (assoc :ccol (min ccol (count (nth lines (dec crow))))))
    st))

(defn move-down [{:keys [lines crow ccol] :as st}]
  (if (< crow (dec (count lines)))
    (-> st (update :crow inc) (assoc :ccol (min ccol (count (nth lines (inc crow))))))
    st))

;; ── Paste placeholders (pi-style)  ───────────────────────────────────────
;;
;; A multi-line / large clipboard payload doesn't go inline into the
;; input box — it would scroll the typing area off the user's screen
;; and force them to navigate around hundreds of unwanted rows. Pi /
;; Claude Code's UX (and what we mirror here): the screen loop stashes
;; the payload in `app-db :pastes` keyed by an auto-incrementing id,
;; and inserts a single-line PLACEHOLDER token into the input buffer:
;;
;;     [Pasted #1: 42 lines]
;;
;; The user keeps typing around it, hits Enter, and the send path
;; substitutes every `[Pasted #N: …]` with its content via
;; `expand-paste-placeholders` before the message reaches the agent.
;; Backspace right after the closing `]` deletes the whole token (and
;; the screen loop drops that entry from `:pastes` so memory tracks
;; what's actually still referenced).

(def placeholder-regex
  "Anchored shape `[Pasted #N: …]`. The non-greedy negated-bracket
   body keeps the regex idempotent against nested-bracket text the
   user may have typed adjacent to the placeholder."
  #"\[Pasted #(\d+): [^\]]*?\]")

(defn- format-bytes
  "Human-readable byte count: 1234 -> 1.2KB, 12 -> 12B. Locale-safe
   (US locale so a Polish JVM doesn't render a comma decimal)."
  [^long n]
  (cond
    (< n 1024)         (str n "B")
    (< n (* 1024 1024)) (String/format java.util.Locale/US "%.1fKB"
                          (into-array Object [(double (/ n 1024.0))]))
    :else               (String/format java.util.Locale/US "%.1fMB"
                          (into-array Object [(double (/ n 1024.0 1024.0))]))))

(defn format-paste-placeholder
  "Produce the visible token text for `app-db :pastes` entry `entry`.
   Pure: same input -> same output, no allocation games. Used both
   when the screen loop inserts the token AND when the renderer or
   send path needs to re-derive the canonical shape.

   Always carries BOTH the line count AND the human-readable byte
   weight — the user wants to see both at a glance:

     [Pasted #1: 42 lines, 1.2KB]
     [Pasted #2: 1 line, 73B]

   `lines` is correctly pluralised; size is locale-safe via
   `format-bytes` so a Polish JVM doesn't render `1,2KB`."
  [{:keys [id content]}]
  (let [text       (str content)
        line-count (inc (count (filter #(= % \newline) text)))
        char-count (count text)
        line-word  (if (= 1 line-count) "line" "lines")]
    (str "[Pasted #" id ": " line-count " " line-word ", "
      (format-bytes char-count) "]")))

(def ^:const PASTE_INLINE_MAX_CHARS
  "Threshold below which we DON'T use a placeholder — a short
   single-line paste like `git rev-parse HEAD` reads naturally
   inline and a placeholder would just be noise."
  80)

(defn use-placeholder?
  "True when the pasted text is large enough OR multi-line enough
   that the user benefits from the placeholder UX. Single-line
   ASCII pastes shorter than `PASTE_INLINE_MAX_CHARS` go inline,
   matching the same instinct pi follows."
  [^String text]
  (boolean
    (or (.contains text "\n")
      (> (count text) PASTE_INLINE_MAX_CHARS))))

(defn expand-paste-placeholders
  "Substitute every `[Pasted #N: …]` token in `text` with its content
   from `pastes-map`. Used by the send path so the agent receives
   the user's full payload, not the cosmetic token.

   `pastes-map` shape: `{<id-int> {:id N :content \"…\"}}`.
   Tokens whose id has no entry in the map (e.g. the user typed the
   bracket text manually) pass through unchanged.

   NOTE: `clojure.string/replace` with a fn already runs the fn's
   return through `Matcher/quoteReplacement` internally, so a
   payload carrying `$` or `\\` is handled verbatim — we don't
   double-quote here."
  [^String text pastes-map]
  (str/replace text placeholder-regex
    (fn [[whole id-str]]
      (let [id (try (Integer/parseInt id-str) (catch Throwable _ nil))
            entry (when id (get pastes-map id))]
        (if entry (str (:content entry)) whole)))))

(defn placeholder-id-before-cursor
  "When the cursor of `state` sits IMMEDIATELY AFTER the closing `]`
   of a paste placeholder on the current line, return that
   placeholder's `:id` (Integer). nil otherwise. The screen loop
   uses this to turn one Backspace into a whole-token delete."
  [{:keys [lines crow ccol]}]
  (let [line (nth lines crow nil)]
    (when (and (string? line) (pos? ccol))
      (let [before (subs line 0 ccol)]
        (when (str/ends-with? before "]")
          (let [m (re-find placeholder-regex before)
                ;; Only fire when the match SITS AT THE END of
                ;; `before`. A placeholder somewhere earlier on the
                ;; line shouldn't get nuked just because the cursor
                ;; is past it.
                match-start (when m (str/last-index-of before (first m)))
                match-end   (when match-start (+ match-start (count (first m))))]
            (when (and m match-end (= match-end (count before)))
              (try (Integer/parseInt (second m)) (catch Throwable _ nil)))))))))

(defn delete-placeholder-backward
  "Remove the placeholder token immediately before the cursor in
   `state`. Returns the new state (whole token gone, cursor at the
   start of where the token was). Caller is responsible for dropping
   the matching `:pastes` entry from app-db.

   No-op when no placeholder ends at the cursor — the screen loop
   guards via `placeholder-id-before-cursor` first."
  [{:keys [lines crow ccol] :as state}]
  (let [line   (nth lines crow "")
        before (subs line 0 ccol)
        after  (subs line ccol)
        m      (re-find placeholder-regex before)]
    (if-let [match-start (when m (str/last-index-of before (first m)))]
      (let [new-line (str (subs before 0 match-start) after)
            new-ccol match-start]
        (-> state
          (assoc-in [:lines crow] new-line)
          (assoc :ccol new-ccol)))
      state)))

(defn paste-text [{:keys [lines crow ccol] :as st} text]
  (let [paste-lines  (str/split text #"\r?\n" -1)
        current-line (nth lines crow)
        before       (subs current-line 0 ccol)
        after        (subs current-line ccol)]
    (if (= 1 (count paste-lines))
      (-> st
        (assoc-in [:lines crow] (str before (first paste-lines) after))
        (assoc :ccol (+ ccol (count (first paste-lines)))))
      (let [first-l  (str before (first paste-lines))
            last-l   (str (last paste-lines) after)
            mid      (subvec (vec paste-lines) 1 (dec (count paste-lines)))
            new-crow (+ crow (dec (count paste-lines)))]
        (-> st
          (assoc :lines (into (conj (subvec lines 0 crow) first-l)
                          (concat mid [last-l] (subvec lines (inc crow)))))
          (assoc :crow new-crow)
          (assoc :ccol (count (last paste-lines))))))))

;;; ── Key handling ───────────────────────────────────────────────────────────

(defn handle-key
  "Process keystroke. Returns {:action kw, :state s}."
  [^KeyStroke key state]
  (let [ktype (.getKeyType key)]
    (condp = ktype
      ;; Esc — the screen loop turns this into a cancel when a query is
      ;; in flight; otherwise it's a no-op (no dialog is open here, the
      ;; dialog code intercepts Esc itself).
      KeyType/Escape
      {:action :cancel :state state}

      KeyType/Character
      (let [c    (.getCharacter key)
            ctrl (.isCtrlDown key)]
        (cond
          (and ctrl (= c \c)) {:action :quit :state state}

          (and ctrl (= c \v)) (if-let [t (clipboard-paste)]
                                {:action :continue :state (paste-text state t)}
                                {:action :continue :state state})

          (and ctrl (= c \x)) {:action :send :state state}

          (and ctrl (= c \k)) {:action :show-palette :state state}

          ;; Ctrl+P / Ctrl+N — input history (Emacs-style)
          (and ctrl (= c \p)) {:action :history-up :state state}
          (and ctrl (= c \n)) {:action :history-down :state state}

          :else {:action :continue :state (insert-char state c)}))

      KeyType/Enter
      (if (.isAltDown key)
        {:action :continue :state (insert-newline state)}
        {:action :send :state state})

      KeyType/F2         {:action :continue :state state}
      KeyType/Backspace  {:action :continue :state (delete-backward state)}
      KeyType/ArrowLeft  {:action :continue :state (move-left state)}
      KeyType/ArrowRight {:action :continue :state (move-right state)}
      ;; Arrow Up/Down — scroll messages
      KeyType/ArrowUp    {:action :scroll-up :state state}
      KeyType/ArrowDown  {:action :scroll-down :state state}
      KeyType/PageUp     {:action :scroll-up :state state}
      KeyType/PageDown   {:action :scroll-down :state state}

      ;; Ignore everything else (including mouse events)
      {:action :continue :state state})))

;;; ── Message formatting ─────────────────────────────────────────────────────

(defn format-message [text]
  (let [ls (str/split-lines text)]
    (into [(str "you: " (first ls))]
      (map #(str "     " %) (rest ls)))))
