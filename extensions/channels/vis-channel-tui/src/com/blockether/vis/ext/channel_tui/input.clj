(ns com.blockether.vis.ext.channel-tui.input
  "Keyboard / paste / clipboard surface for the TUI channel.

   Clipboard read/write goes through OS shell helpers only: `pbcopy` /
   `pbpaste` on macOS (always present since 10.0), `wl-copy` /
   `wl-paste` on Wayland, `xclip` / `xsel` on X11."
  (:require [babashka.process :as process]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.keymap :as keymap]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.workspace :as workspace]
            [taoensso.telemere :as tel])
  (:import [com.googlecode.lanterna TerminalPosition]
           [com.googlecode.lanterna.gui2 TextEditBuffer TextEditKeymap]
           [com.googlecode.lanterna.input CharacterPattern CharacterPattern$Matching
            KeyDecodingProfile KeyStroke KeyType MouseAction MouseActionType]
           [com.googlecode.lanterna.terminal.ansi UnixTerminal]))

(set! *unchecked-math* :warn-on-boxed)

;;; ── Custom input patterns ──────────────────────────────────────────────────
;; Lanterna's AltAndCharacterPattern rejects ISO control chars (\n, \r, \t).
;; Alt+Enter sends ESC + '\n' which gets rejected. We fix it here.

(def ^:private ESC_CHAR (Character. (char 0x1b)))

(def escape-pattern
  "Full match for bare Escape. Custom ESC-prefixed patterns below return
   partial matches for Alt+Enter, Alt+Backspace, bracketed paste, and SGR
   mouse sequences. Lanterna keeps a candidate full match only when one
   exists; without this, a lone Esc can stay buffered and the next Enter is
   decoded as Alt+Enter instead of closing/canceling."
  (reify
    CharacterPattern
      (match [_ seq]
        (when (and (= 1 (.size seq)) (= (.get seq 0) ESC_CHAR))
          (CharacterPattern$Matching. (KeyStroke. KeyType/Escape false false))))))

(def alt-enter-pattern
  (reify
    CharacterPattern
      (match [_ seq]
        (let [size (.size seq)]
          (cond (and (= size 2)
                     (= (.get seq 0) ESC_CHAR)
                     (let [c (.get seq 1)]
                       (or (= c (Character. \newline)) (= c (Character. \return)))))
                (CharacterPattern$Matching. (KeyStroke. KeyType/Enter false true))
                (and (= size 1) (= (.get seq 0) ESC_CHAR)) CharacterPattern$Matching/NOT_YET
                :else nil)))))

(def alt-backspace-pattern
  "Lanterna's stock Alt+character decoder rejects ISO control chars.
   Option+Backspace commonly arrives as ESC + DEL (0x7f), with some
   terminals using ESC + Ctrl-H (0x08). Surface both as Alt+Backspace
   so the editor can delete one word backward."
  (reify
    CharacterPattern
      (match [_ seq]
        (let [size (.size seq)]
          (cond (and (= size 2)
                     (= (.get seq 0) ESC_CHAR)
                     (let [c (.get seq 1)]
                       (or (= c (Character. (char 0x7f))) (= c (Character. (char 0x08))))))
                (CharacterPattern$Matching. (KeyStroke. KeyType/Backspace false true))
                (and (= size 1) (= (.get seq 0) ESC_CHAR)) CharacterPattern$Matching/NOT_YET
                :else nil)))))

(def ctrl-h-pattern
  "Make Ctrl+H reach the app as Ctrl+H. The terminal sends byte 0x08 for Ctrl+H,
   which lanterna's DEFAULT profile decodes as Backspace — so help-on-Ctrl+H was
   dead (it just deleted a char). Decode a LONE 0x08 as Ctrl+H instead (char `h`
   + ctrl). The physical Backspace key sends 0x7f on every modern terminal
   (macOS Terminal/iTerm2, Linux), so it stays Backspace and is unaffected.
   Registered AFTER the default profile, and `InputDecoder.getBestMatch` keeps
   the LAST full match, so this wins over the stock 0x08->Backspace mapping.
   (Option+Backspace = ESC+0x08 is a TWO-char sequence handled by
   `alt-backspace-pattern`, so it does not collide with this lone-0x08 match.)"
  (reify
    CharacterPattern
      (match [_ seq]
        (when (and (= 1 (.size seq)) (= (.get seq 0) (Character. (char 0x08))))
          (CharacterPattern$Matching. (KeyStroke. (Character. \h) true false))))))

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
;; fires KeyType/Enter - which the input handler treats as "send the
;; message" - and the message ships before the user can react.
;;
;; We surface the brackets to the input loop as two KeyStrokes carrying
;; PUA marker characters (`PASTE_START_CHAR` / `PASTE_END_CHAR`). The
;; screen poll loop sees those markers, flips a buffering mode on/off,
;; accumulates every keystroke between them into one string, and feeds
;; that string to `paste-text`. Same code path as Ctrl+V - we just
;; reused the bracketed-paste sequences as the trigger.
;;
;; Enabling / disabling the mode is a separate concern: the terminal
;; only emits the brackets when we tell it to. `enable-bracketed-paste!`
;; and `disable-bracketed-paste!` write the toggling escapes directly
;; to the controlling TTY, so the screen lifecycle owns the on/off
;; bracket points.

(defn alt-modifier?
  "True when Lanterna decoded an Alt/Option modifier."
  [^KeyStroke key]
  (.isAltDown key))

(defn shift-modifier?
  "True when Lanterna decoded a Shift modifier."
  [^KeyStroke key]
  (.isShiftDown key))

(defn ctrl-modifier?
  "True when Lanterna decoded a Ctrl modifier."
  [^KeyStroke key]
  (.isCtrlDown key))

(defn alt-char?
  "True for Alt/Option + character `c`, case-insensitive."
  [^KeyStroke key c]
  (and (alt-modifier? key)
       (= (Character/toLowerCase (char (.getCharacter key))) (Character/toLowerCase (char c)))))

(defn ctrl-char?
  "True for Ctrl + character `c`, case-insensitive. The reliable, cross-platform
   modifier for in-modal toggles (Alt/Option is eaten by stock macOS terminals).
   Pick `c` to avoid the terminal control codes (I=Tab, M=Enter, H=BS, S/Q=flow
   control, O=stty DISCARD), which never arrive as a Character keystroke."
  [^KeyStroke key c]
  (and (ctrl-modifier? key)
       (let [ch (.getCharacter key)]
         (and ch (= (Character/toLowerCase (char ch)) (Character/toLowerCase (char c)))))))

(defn reorder-modifier?
  "True when a MODIFIED arrow should reorder a list item instead of moving the
   cursor. Shift/Alt+↑/↓ is a BONUS path (decoded from the xterm modified-arrow
   sequence); the reliable, documented reorder keys are plain `K`/`J`, handled
   directly in each list dialog — stock macOS terminals don't surface
   Alt+arrow, and Option+arrow is reserved for word motion."
  [^KeyStroke key]
  (or (alt-modifier? key) (shift-modifier? key)))

(def ^:const reorder-modifier-label
  "Hint label for the list-reorder keys. Plain `K`/`J` always reach the app
   (Shift/Alt+↑/↓ also work where the terminal sends the xterm sequence)."
  "K/J")

(def ^:private arrow-final->keytype
  {\A KeyType/ArrowUp \B KeyType/ArrowDown \C KeyType/ArrowRight \D KeyType/ArrowLeft})

(def ^:private modified-arrow-targets
  (for
    [modifier
     (range 2 9)

     final
     (keys arrow-final->keytype)]

    (str (char 0x1b) "[1;" modifier final)))

(defn- modified-arrow-keystroke
  [^String s]
  (when-let [final (get s (dec (count s)))]
    (when-let [^KeyType ktype (arrow-final->keytype final)]
      (let
        [modifier (Character/digit (.charAt s 4) 10)
         bits (dec modifier)]

        (when (<= 2 modifier 8)
          (KeyStroke. ktype
                      (pos? (bit-and bits 4))
                      (pos? (bit-and bits 2))
                      (pos? (bit-and bits 1))))))))

(def modified-arrow-pattern
  "Decode xterm-style modified arrows (`ESC[1;<mod>A-D`) into Lanterna
   KeyStrokes with Ctrl/Alt/Shift flags. macOS/iTerm/Terminal commonly
   use these for Option/Shift arrow combos; Lanterna's stock decoder does
   not consistently surface Alt+arrow across terminals."
  (reify
    CharacterPattern
      (match [_ seq]
        (let [s (apply str (map char seq))]
          (cond (some #(= s %) modified-arrow-targets) (CharacterPattern$Matching.
                                                         (modified-arrow-keystroke s))
                (some #(str/starts-with? % s) modified-arrow-targets)
                CharacterPattern$Matching/NOT_YET
                :else nil)))))

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
  "True when the sequence `s` is a prefix of `target` - used by the
   bracket patterns to return NOT_YET while the user-typed `ESC[`
   could still complete into one of our triggers."
  [^java.util.List s ^String target]
  (let [size (.size s)]
    (and (<= size (.length target))
         (loop [i 0]
           (cond (>= i size) true
                 (= (.charAt target i) (char (.charValue ^Character (.get s i)))) (recur (inc i))
                 :else false)))))

(defn- bracket-pattern
  "Build a CharacterPattern that matches `target` (e.g. `ESC[200~`)
   exactly and returns a KeyStroke whose character is `marker`. Used
   by the bracketed-paste START / END detectors."
  [^String target marker]
  (reify
    CharacterPattern
      (match [_ s]
        (let [size (.size s)]
          (cond (and (= size (.length target)) (bracket-pattern-prefix-match? s target))
                (CharacterPattern$Matching. (KeyStroke. (Character. (char marker)) false false))
                (and (< size (.length target)) (bracket-pattern-prefix-match? s target))
                CharacterPattern$Matching/NOT_YET
                :else nil)))))

(def paste-start-pattern (bracket-pattern "\u001B[200~" PASTE_START_CHAR))

(def paste-end-pattern (bracket-pattern "\u001B[201~" PASTE_END_CHAR))

(defn paste-marker?
  "True when `key` is the bracketed-paste START or END marker. Lets
   the screen polling loop pick the brackets out before normal key
   handling runs."
  [^KeyStroke key marker]
  (and (= KeyType/Character (.getKeyType key))
       (some? (.getCharacter key))
       (= (long (int marker)) (long (int (.charValue ^Character (.getCharacter key)))))))

(defn paste-start? [key] (paste-marker? key PASTE_START_CHAR))

(defn paste-end? [key] (paste-marker? key PASTE_END_CHAR))

(defn keystroke->paste-char
  "Convert one KeyStroke received WHILE INSIDE a bracketed-paste
   block into the character it represents, so the buffer reflects
   the user's pasted bytes verbatim. Returns nil for keystrokes that
   don't map to a single text character (e.g. function keys); the
   input loop drops those."
  [^KeyStroke key]
  (case (str (.getKeyType key))
    "Character"
    (str (.getCharacter key))

    "Enter"
    "\n"

    "Tab"
    "\t"

    nil))

;; ── SGR mouse mode (1006) ─────────────────────────────────────────────
;;
;; Lanterna's stock `MouseCharacterPattern` parses the *legacy* X10
;; mouse protocol: `ESC [ M Cb Cx Cy` where Cx/Cy are RAW BYTES with
;; offset 32. The moment a column or row exceeds 95, the byte rises
;; above 0x7F and the JVM's UTF-8 input decoder treats it as an
;; invalid byte, replacing it with U+FFFD (65533). Lanterna then
;; subtracts 33 - producing a phantom column of `65500` that of
;; course doesn't match any click region. The header copy-id
;; affordance lives near the right edge of the screen, where this
;; bug fires every single time on a terminal wider than ~100
;; columns.
;;
;; SGR mode (CSI 1006) sidesteps the UTF-8 issue entirely by
;; encoding the same payload as PURE ASCII text:
;;
;;     ESC [ < Cb ; Cx ; Cy M     (button-down / drag / move / scroll)
;;     ESC [ < Cb ; Cx ; Cy m     (button-release - lowercase `m`)
;;
;; Cx/Cy are 1-based decimal numbers. We register a custom
;; `CharacterPattern` that recognises this shape and emits a proper
;; `MouseAction`, then send `ESC [ ? 1006 h` alongside the legacy
;; mode-1003 enable that Lanterna already does, so the terminal
;; switches over to the ASCII format.

(defn- sgr-mouse-decode
  "Given the parsed SGR fields `[button col row final-char]`, return
   the `MouseActionType` that fits. Mirrors Lanterna's stock
   classification but on SGR semantics: the high bits of the button
   byte distinguish button-press / drag / scroll, the case of the
   final character distinguishes press (`M`) from release (`m`)."
  [^long button ^long _col ^long _row ^long final-ch]
  (let
    [release?
     (= final-ch (long (int \m)))

     ;; bit 5 (0x20) = drag/move, bit 6 (0x40) = wheel.
     drag?
     (not (zero? (bit-and button 0x20)))

     wheel?
     (not (zero? (bit-and button 0x40)))

     button-bits
     (bit-and button 0x03)]

    (cond wheel? (if (zero? button-bits) MouseActionType/SCROLL_UP MouseActionType/SCROLL_DOWN)
          drag?
          ;; button bits 3 (0x03 == 3) means \"no button held\" -> plain
          ;; cursor movement; otherwise a drag with that button held.
          (if (= button-bits 3) MouseActionType/MOVE MouseActionType/DRAG)
          release? MouseActionType/CLICK_RELEASE
          :else MouseActionType/CLICK_DOWN)))

(defn- sgr-button-number
  ^long [^long button]
  ;; Map raw SGR button byte to Lanterna's 1-based button number,
  ;; matching `MouseCharacterPattern` semantics (1=left, 2=middle,
  ;; 3=right, 4=wheel-up, 5=wheel-down). For DRAG/MOVE/RELEASE the
  ;; field still carries the button bits, so the same mapping holds.
  (let
    [wheel?
     (not (zero? (bit-and button 0x40)))

     bits
     (bit-and button 0x03)]

    (cond wheel? (if (zero? bits) 4 5)
          ;; bits == 3 in non-wheel modes means \"no button held\" -
          ;; that's the SGR convention for plain motion. Report 0 to
          ;; match what Lanterna's parser does for legacy MOVE events.
          (= bits 3) 0
          :else (inc bits))))

(def sgr-mouse-pattern
  "CharacterPattern matching `ESC [ < N ; N ; N M/m`. Emits a
   `MouseAction` with proper integer column/row so SGR-capable
   terminals (every modern macOS / Linux emulator) deliver clicks
   that the screen handler can hit-test against click regions.

   Returns NOT_YET while the prefix is still consistent with the
   shape, returns nil (no match) the instant a character violates
   it, returns Matching with the built MouseAction once the final
   `M` / `m` byte arrives."
  (reify
    CharacterPattern
      (match [_ chars]
        (let
          [^java.util.List chars
           chars

           n
           (.size chars)]

          (cond (zero? n) CharacterPattern$Matching/NOT_YET
                ;; chars[0] must be ESC (0x1B); chars[1] '['; chars[2] '<'.
                (or (and (>= n 1) (not= \u001B (.charValue ^Character (.get chars 0))))
                    (and (>= n 2) (not= \[ (.charValue ^Character (.get chars 1))))
                    (and (>= n 3) (not= \< (.charValue ^Character (.get chars 2)))))
                nil
                (< n 4) CharacterPattern$Matching/NOT_YET
                :else
                ;; Walk chars[3..] as: digits ';' digits ';' digits final.
                ;; Keep primitive numeric accumulators. This path runs once
                ;; per received byte while the user scrolls; persistent
                ;; vectors here used to peg a core under SGR wheel floods.
                (loop
                  [i
                   (long 3)

                   field
                   (long 0)

                   acc
                   (long 0)

                   have-digit?
                   false

                   button
                   (long 0)

                   col
                   (long 0)]

                  (if (>= i n)
                    CharacterPattern$Matching/NOT_YET
                    (let
                      [c
                       (.charValue ^Character (.get chars i))

                       ci
                       (long (int c))]

                      (cond
                        ;; ASCII digit: accumulate current numeric field.
                        (and (>= ci 48) (<= ci 57)) (recur (unchecked-inc i)
                                                           field
                                                           (unchecked-add
                                                             (unchecked-multiply acc 10)
                                                             (unchecked-subtract ci 48))
                                                           true
                                                           button
                                                           col)
                        ;; Field separator. Need at least one digit collected,
                        ;; and exactly button/col separators before row.
                        (= c \;) (cond (not have-digit?) nil
                                       (>= field 2) nil
                                       (zero? field)
                                       (recur (unchecked-inc i) (long 1) (long 0) false acc col)
                                       :else
                                       (recur (unchecked-inc i) (long 2) (long 0) false button acc))
                        ;; Terminator. Must be `M` or `m`, must arrive with
                        ;; button + col completed and row in progress.
                        (or (= c \M) (= c \m))
                        (if (and (= field 2) have-digit?)
                          (let
                            [row
                             acc

                             atype
                             (sgr-mouse-decode button col row ci)

                             btn
                             (sgr-button-number button)

                             ;; SGR is 1-indexed, Lanterna's 0-indexed.
                             pos
                             (TerminalPosition. (max 0 (dec col)) (max 0 (dec row)))]

                            (CharacterPattern$Matching. (MouseAction. atype (int btn) pos)))
                          nil)
                        ;; Anything else - stray byte mid-sequence - rejects
                        ;; this pattern. Lanterna will hand the chars to the
                        ;; next pattern in the profile.
                        :else nil)))))))))

(defn bare-escape?
  "True for a plain Escape KeyStroke (not a MouseAction)."
  [k]
  (and k
       (instance? KeyStroke k)
       (not (instance? MouseAction k))
       (= KeyType/Escape (.getKeyType ^KeyStroke k))))

(defn drain-sgr-leak!
  "Swallow a literal SGR mouse tail that leaked past the decoder.

   When the bytes of an SGR mouse report split across two terminal reads
   (wheel flood, GC/paint hitch, tmux/SSH chunking), Lanterna's decoder
   sees only the lone ESC, emits the bare-Escape full match, and the rest
   of the sequence - `[<65;32;43M` or `<65;32;43M` - decodes as ordinary
   Character keystrokes that would be typed into the input box. Call this
   right after a bare Escape keystroke with a NON-BLOCKING `poll-next`:
   it consumes queued Character keystrokes while they spell
   `[? < digits ; digits ; digits M|m` runs (back-to-back runs included).

   Returns {:swallowed? <true when at least one complete run was dropped>
            :replay     [consumed keystrokes that were NOT part of a run]}.
   The caller must replay `:replay` (oldest first) as if never consumed.
   Real typing is unaffected: a human cannot queue a '<' in the same
   millisecond burst as an Escape press, and an incomplete candidate is
   replayed verbatim."
  [poll-next]
  (loop
    [swallowed?
     false

     consumed
     []

     st
     ::start

     fields
     0

     digit?
     false]

    (let [k (poll-next)]
      (if-not (and k
                   (instance? KeyStroke k)
                   (not (instance? MouseAction k))
                   (= KeyType/Character (.getKeyType ^KeyStroke k)))
        {:swallowed? swallowed? :replay (if k (conj consumed k) consumed)}
        (let
          [ch (.charValue ^Character (.getCharacter ^KeyStroke k))
           consumed (conj consumed k)]

          (case st
            ::start
            (cond
              ;; some terminals surface a stray ESC as a Character - the
              ;; prefix of a back-to-back leaked run.
              (= ch \u001B) (recur swallowed? consumed ::start 0 false)
              (= ch \[) (recur swallowed? consumed ::bracket 0 false)
              (= ch \<) (recur swallowed? consumed ::body 0 false)
              :else {:swallowed? swallowed? :replay consumed})

            ::bracket
            (if (= ch \<)
              (recur swallowed? consumed ::body 0 false)
              {:swallowed? swallowed? :replay consumed})

            ::body
            (cond (and (>= (int ch) 48) (<= (int ch) 57))
                  (recur swallowed? consumed ::body fields true)
                  (and (= ch \;) digit? (< (long fields) 2))
                  (recur swallowed? consumed ::body (inc (long fields)) false)
                  (and (or (= ch \M) (= ch \m)) (= (long fields) 2) digit?)
                  ;; complete run consumed - drop it, then look for another
                  ;; leaked run immediately behind it.
                  (recur true [] ::start 0 false)
                  :else {:swallowed? swallowed? :replay consumed})))))))

(defn enable-sgr-mouse!
  "Send `ESC [ ? 1006 h`. Asks the terminal to deliver subsequent
   mouse events in SGR encoding. Independent of the legacy
   mode-1003 enable Lanterna already issues via
   `setMouseCaptureMode`; both can be active at once and the
   terminal will use whichever the remote consumer claims to
   support - every modern emulator (Apple Terminal 2.10+, iTerm2,
   alacritty, kitty, wezterm, foot, gnome-terminal, mintty,
   vscode integrated terminal) honours 1006."
  [^java.io.OutputStream out]
  (try (.write out (.getBytes "\u001B[?1006h" "UTF-8")) (.flush out) (catch Throwable _ nil)))

(defn disable-sgr-mouse!
  "Reverse of `enable-sgr-mouse!`. Sent in the screen tear-down
   `finally` so a crashed TUI doesn't leave the user's shell with
   the mode dangling."
  [^java.io.OutputStream out]
  (try (.write out (.getBytes "\u001B[?1006l" "UTF-8")) (.flush out) (catch Throwable _ nil)))

(defn enable-bracketed-paste!
  "Tell the terminal to wrap subsequent pastes in `ESC[200~ ... ESC[201~`.
   Bytes go straight to the controlling TTY; safe to call inside the
   screen lifecycle."
  [^java.io.OutputStream out]
  (try (.write out (.getBytes "\u001B[?2004h" "UTF-8")) (.flush out) (catch Throwable _ nil)))

(defn disable-bracketed-paste!
  "Reverse of `enable-bracketed-paste!`. Always called in the screen
   tear-down `finally` so a crashed TUI doesn't leave the user's
   shell stuck in bracketed-paste mode."
  [^java.io.OutputStream out]
  (try (.write out (.getBytes "\u001B[?2004l" "UTF-8")) (.flush out) (catch Throwable _ nil)))

(defn set-default-bg!
  "Set the terminal's DEFAULT background color via OSC 11. Modern
   emulators (iTerm2, kitty, ghostty, wezterm, alacritty, foot, vscode)
   extend the default background into the window padding around the
   cell grid - without this the padding stays the user's shell color
   (often white) instead of the theme background. Best-effort: a
   terminal that ignores OSC 11 simply keeps its own background."
  [^java.io.OutputStream out r g b]
  (try (.write out
               (.getBytes (format "\u001B]11;rgb:%02x/%02x/%02x\u0007" (long r) (long g) (long b))
                          "UTF-8"))
       (.flush out)
       (catch Throwable _ nil)))

(defn reset-default-bg!
  "Reverse of `set-default-bg!` via OSC 111 (reset default background).
   Always called in the screen tear-down `finally` so the user's shell
   gets its original background back."
  [^java.io.OutputStream out]
  (try (.write out (.getBytes "\u001B]111\u0007" "UTF-8")) (.flush out) (catch Throwable _ nil)))

(defn- stty!
  "Best-effort `stty <flag> < /dev/tty` — Unix only (Windows has no /dev/tty or
   stty; the Windows console TUI never needs this). Bounded + never throws."
  [flag]
  (when (.exists (io/file "/dev/tty"))
    (try (let [p (process/process {:cmd ["sh" "-c" (str "stty " flag " < /dev/tty")] :err :string})]
           (.waitFor ^Process (:proc p) 400 java.util.concurrent.TimeUnit/MILLISECONDS)
           nil)
         (catch Throwable _ nil))))

(defn disable-literal-next!
  "Disable the tty's IEXTEN so Ctrl+V (VLNEXT — the line discipline's \"literal
   next\" quoting char) and Ctrl+O (VDISCARD) reach the app as real keystrokes
   instead of being swallowed before lanterna ever decodes them. This is the SAME
   raw-mode move Emacs (and every terminal Emacs) makes so `C-v` can scroll. Best
   effort; a host without stty simply keeps Ctrl+V as literal-next. Paired with
   `restore-literal-next!` on teardown."
  []
  (stty! "-iexten"))

(defn restore-literal-next!
  "Re-enable IEXTEN on teardown so the user's shell gets its literal-next /
   discard quoting back, regardless of whether lanterna restores termios itself."
  []
  (stty! "iexten"))

(defn register-custom-patterns!
  "Register Escape, Alt+Enter, Alt+Backspace, modified arrows,
   bracketed-paste, and SGR-mouse patterns on the terminal's input
   decoder. Without `sgr-mouse-pattern` the stock Lanterna parser
   handles only legacy X10 mouse events, whose raw-byte coordinate
   encoding clashes with the JVM's UTF-8 input decoder for any
   column/row beyond 95 - producing a phantom `mx=65500` from a U+FFFD
   replacement byte."
  [^UnixTerminal terminal]
  (.addProfile (.getInputDecoder terminal)
               (reify
                 KeyDecodingProfile
                   (getPatterns [_] [escape-pattern alt-enter-pattern alt-backspace-pattern
                                     ctrl-h-pattern modified-arrow-pattern paste-start-pattern
                                     paste-end-pattern sgr-mouse-pattern]))))

;;; ── Clipboard (shell helpers) ───────────────────────────────────────────────
;;
;; Two shell candidate lists, one per direction. Ordered most-likely
;; first per platform but every entry is tried in turn, so a macOS
;; user running inside a Linux container (or vice versa) still has
;; a working path. Each helper is given 1 second to finish so a
;; hung process can't pin the input thread.

(def ^:private copy-helpers
  ;; argv lists for clipboard-WRITE helpers. Helper reads payload
  ;; from stdin.
  [["pbcopy"] ;; macOS
   ["wl-copy"] ;; Wayland
   ["xclip" "-selection" "clipboard"] ;; X11
   ["xsel" "--clipboard" "--input"]])    ;; X11 alt

(def ^:private paste-helpers
  ;; argv lists for clipboard-READ helpers. Helper writes payload
  ;; to stdout, which we capture verbatim and decode as UTF-8.
  [["pbpaste"] ;; macOS
   ["wl-paste" "--no-newline"] ;; Wayland
   ["xclip" "-selection" "clipboard" "-o"] ;; X11
   ["xsel" "--clipboard" "--output"]])   ;; X11 alt

(defn- run-helper-process!
  "Spawn `cmd` (an argv vector) and drain its stdout under a HARD `timeout-ms`
   deadline. Returns `{:success? boolean :bytes byte-array}` — `:success?`
   only when the process exited 0 within the deadline. Never throws.

   Three hang traps the old per-helper loops fell into are closed here:

   1. The child's stdin is CLOSED right after start (after writing
      `stdin-bytes` when given), so a helper that reads stdin sees EOF
      instead of waiting forever on a pipe nobody writes.
   2. The drain used to `.read` to EOF with NO bound — the `waitFor` cap
      only ran AFTER EOF, so a helper that never exits (osascript stuck on
      a macOS automation prompt, xclip without an X server, a wedged
      pngpaste) blocked the CALLING thread — the TUI input loop — forever:
      one ⌘V froze the whole session. A watchdog now `destroyForcibly`s the
      process at the deadline, which closes its stdout pipe and unblocks
      the drain, so the caller always returns in ~timeout-ms.
   3. EOF itself is not trusted: it only arrives once EVERY write-end of
      the pipe is closed, and a helper that exits after spawning a lingering
      grandchild (xclip-style daemonize, `foo & exit 0`) leaves the pipe held
      open with NOTHING for the watchdog to kill — the helper already
      exited. The drain is therefore deadline-bounded (`.available`
      polling), never a bare blocking `.read`-to-EOF.

   `merge-stderr?` folds stderr into stdout (text helpers); otherwise stderr
   is DISCARDED so a binary payload can't be corrupted by warnings."
  [cmd {:keys [^bytes stdin-bytes merge-stderr? timeout-ms] :or {timeout-ms 2000}}]
  (try
    (let
      [pb
       (ProcessBuilder. ^java.util.List cmd)

       _
       (if merge-stderr?
         (.redirectErrorStream pb true)
         (.redirectError pb java.lang.ProcessBuilder$Redirect/DISCARD))

       p
       (.start pb)

       _
       (try (let [out (.getOutputStream p)]
              (when stdin-bytes (.write out ^bytes stdin-bytes))
              (.close out))
            (catch Throwable _ nil))

       watchdog
       (future (when-not (.waitFor p (long timeout-ms) java.util.concurrent.TimeUnit/MILLISECONDS)
                 (.destroyForcibly p)))

       in
       (.getInputStream p)

       buf
       (java.io.ByteArrayOutputStream.)

       tmp
       (byte-array 8192)]

      ;; Deadline-bounded drain — trap 3 in the docstring. A bare blocking
      ;; `.read`-to-EOF needs every pipe write-end closed; a lingering
      ;; grandchild keeps it open after the helper exits, and then the
      ;; watchdog has nothing left to kill. Poll instead: consume whatever
      ;; is buffered, stop at the deadline or once the helper died with the
      ;; pipe drained.
      (let [deadline (+ (System/nanoTime) (* (long timeout-ms) 1000000))]
        (loop []

          (let [avail (long (try (.available in) (catch Throwable _ -1)))]
            (cond (pos? avail) (let
                                 [n (long (try (.read in tmp 0 (int (min avail 8192)))
                                               (catch Throwable _ -1)))]
                                 (when (pos? n) (.write buf tmp 0 n) (recur)))
                  ;; stream closed / broken — nothing more will arrive
                  (neg? avail) nil
                  (>= (System/nanoTime) deadline) nil
                  ;; helper exited and the pipe is drained
                  (not (.isAlive p)) nil
                  :else (do (Thread/sleep 5) (recur))))))
      (let [exited? (.waitFor p (long timeout-ms) java.util.concurrent.TimeUnit/MILLISECONDS)]
        @watchdog
        {:success? (boolean (and exited? (zero? (.exitValue p)))) :bytes (.toByteArray buf)}))
    (catch Throwable _ {:success? false :bytes nil})))

(defn- run-shell-helper!
  "Spawn `cmd` (an argv vector), feed `stdin-bytes` to its stdin when
   non-nil, drain stdout (stderr merged in), return
   `{:success? boolean :stdout String}`. Bounded by
   `run-helper-process!`'s hard deadline — a wedged helper can no longer
   freeze the input loop."
  [cmd ^bytes stdin-bytes]
  (let
    [{:keys [success? ^bytes bytes]}
     (run-helper-process! cmd {:stdin-bytes stdin-bytes :merge-stderr? true})]
    {:success? success? :stdout (when bytes (String. bytes "UTF-8"))}))

(def ^:private cached-copy-strategy
  "Memoized clipboard-WRITE strategy, resolved once per session by the first
   real copy so we don't re-spawn doomed helper processes on every ⌘C:
     `nil`   — not probed yet
     `:none` — every helper in `copy-helpers` was missing/failed, so callers
               skip the shell entirely and go straight to OSC 52 next time
     argv    — the helper vector that worked; reused directly on later copies.
   A cached helper that later stops working (display torn down, binary removed)
   clears the cache and forces a fresh probe."
  (atom nil))

(defn- shell-clipboard-copy!
  "Copy `text` via the first working helper in `copy-helpers`. Returns the
   keyword name of the winning helper (`:pbcopy`, `:wl-copy`, `:xclip`,
   `:xsel`) or `:none` when every candidate failed.

   The resolved strategy is memoized in `cached-copy-strategy` so we probe the
   helper chain ONCE: a box with no clipboard helper records `:none` and every
   later copy short-circuits straight to `:none` (→ OSC 52) without spawning a
   single doomed process; a box where one helper works reuses that argv. A
   cached helper that unexpectedly fails clears the cache and re-probes."
  [^String text]
  (let
    [bytes
     (.getBytes text "UTF-8")

     cached
     @cached-copy-strategy]

    (cond
      ;; Known: no shell helper works here — tell the caller to use OSC 52.
      (= cached :none) :none
      ;; Known winner — reuse it; on unexpected failure forget and re-probe.
      (vector? cached) (if (:success? (run-shell-helper! cached bytes))
                         (keyword (first cached))
                         (do (reset! cached-copy-strategy nil) (recur text)))
      ;; First copy (or post-invalidation): probe the whole chain once.
      :else (loop [[cmd & rest] copy-helpers]
              (if (nil? cmd)
                (do (reset! cached-copy-strategy :none) :none)
                (if (:success? (run-shell-helper! cmd bytes))
                  (do (reset! cached-copy-strategy cmd) (keyword (first cmd)))
                  (recur rest)))))))

(defn- shell-clipboard-paste!
  "Try every helper in `paste-helpers` until one returns a non-empty
   string. Returns the captured text or `nil`."
  []
  (loop [[cmd & rest] paste-helpers]
    (when cmd
      (let [{:keys [success? stdout]} (run-shell-helper! cmd nil)]
        (cond (and success? (some? stdout) (pos? (count stdout))) stdout
              :else (recur rest))))))

(defn clipboard-paste
  "Read the system clipboard as a UTF-8 string. Returns the captured
   text or `nil` when no helper produced output."
  []
  (try (shell-clipboard-paste!) (catch Throwable _ nil)))

(def ^:private osc52-truncation-warn-bytes
  "Base64 payloads beyond this many bytes are silently dropped or truncated
   by many terminals' OSC 52 parsers (classic xterm caps far lower). We still
   send — modern emulators and tmux handle large writes — but log a warning so a
   silently-truncated big copy is diagnosable instead of a false green."
  100000)

(defn- osc52-sequence
  "Build the `ESC ] 52 ; c ; <base64> BEL` clipboard sequence for `text`, adapting
   to a terminal multiplexer that `env-get` reveals (`$TMUX`, or GNU `screen` via
   `$STY` + a `screen*` `$TERM`). Inside a multiplexer a copy can fail two ways:
   tmux drops raw OSC 52 unless `set-clipboard on`, and drops DCS passthrough unless
   `allow-passthrough on` — neither is a universal default. So under tmux we emit
   BOTH the raw sequence and the DCS passthrough (`ESC P tmux; ... ESC \\` with every
   inner `ESC` doubled), an idempotent double clipboard-set that copies across the
   widest range of configs. GNU screen only forwards via DCS passthrough and
   truncates an over-long DCS string, so its base64 is split into 76-byte chunks
   rejoined by `<end-DCS><start-DCS>`. tmux/screen wrapping matches go-osc52."
  ^String [^String text env-get]
  (let
    [b64
     (.encodeToString (java.util.Base64/getEncoder) (.getBytes text "UTF-8"))

     base
     (str "\u001B]52;c;" b64 "\u0007")

     tmux?
     (some? (env-get "TMUX"))

     term
     (env-get "TERM")

     screen?
     (and (not tmux?) (some? (env-get "STY")) (some? term) (str/starts-with? term "screen"))

     screen-body
     ;; GNU screen truncates a DCS string past its length limit, so chunk the
     ;; base64 into 76-byte pieces rejoined by <end-DCS><start-DCS> (go-osc52).
     (str/join "\u001B\\\u001BP" (map #(apply str %) (partition-all 76 b64)))]

    ;; tmux: emit BOTH the raw OSC 52 (forwarded when `set-clipboard on`) and the
    ;; DCS passthrough (reaches the outer term when `allow-passthrough on`). Neither
    ;; config is universal, so sending both — idempotent clipboard set — copies
    ;; across the widest range of tmux setups. Ref: opencode #19982/#26815 (raw) vs
    ;; claude-code #38944 (DCS unreliable in alt-screen); dual-emit satisfies both.
    (cond tmux? (str base "\u001BPtmux;" (str/replace base "\u001B" "\u001B\u001B") "\u001B\\")
          screen? (str "\u001BP\u001B]52;c;" screen-body "\u0007\u001B\\")
          :else base)))

(defn osc52-copy!
  "Terminal-native clipboard write via the OSC 52 escape
   (`ESC ] 52 ; c ; <base64> BEL`). Needs NO external binary and works
   over SSH: the terminal emulator itself puts `text` on the user's LOCAL
   system clipboard. Bytes go straight to the controlling TTY (`out`) —
   the same channel `enable-bracketed-paste!` writes to. Automatically wraps
   the sequence for tmux / GNU screen so it survives a multiplexer. Returns
   true when the sequence was written and flushed, false on any failure
   (nil/closed stream). Best-effort: a terminal that doesn't implement OSC 52
   (or has it disabled) silently drops the sequence, so `true` means 'sent',
   not 'confirmed on the clipboard'."
  ([out text]
   (osc52-copy! out
                text
                (fn [k]
                  (System/getenv k))))
  ([^java.io.OutputStream out ^String text env-get]
   (try (if (nil? out)
          false
          (let [b64-len (quot (* 4 (long (count (.getBytes text "UTF-8")))) 3)]
            (when (> b64-len (long osc52-truncation-warn-bytes))
              (try (tel/log! {:level :warn
                              :id ::osc52-large
                              :data {:b64-bytes b64-len}
                              :msg (str "osc52-copy! large payload ("
                                        b64-len
                                        " b64 bytes) — some terminals truncate OSC 52")})
                   (catch Throwable _ nil)))
            (.write out (.getBytes (osc52-sequence text env-get) "UTF-8"))
            (.flush out)
            true))
        (catch Throwable _ false))))

(defn clipboard-copy!
  "Best-effort copy `text` onto the system clipboard. Tries the OS shell
   helpers first (`pbcopy` / `wl-copy` / `xclip` / `xsel`); when EVERY
   helper is missing — a bare SSH session or a minimal Linux box with no
   `wl-clipboard`/`xclip`/`xsel` installed — falls back to the terminal's
   own OSC 52 clipboard over the controlling TTY, so copy still works with
   zero binaries. Returns true on success, false when both paths failed.
   Logs the winning mechanism so \"the copy didn't work\" reports can be
   diagnosed against `~/.vis/vis.log`."
  [^String text]
  (let
    [helper
     (shell-clipboard-copy! text)

     winner
     (if (not= helper :none)
       helper
       (when (osc52-copy! (try (force config/tty-out) (catch Throwable _ nil)) text) :osc52))

     ok?
     (some? winner)]

    (try (tel/log! {:level (if ok? :info :warn)
                    :id ::clipboard-copy
                    :data {:winner (or winner :none)
                           :len (count text)
                           :platform (System/getProperty "os.name")}
                    :msg (str "clipboard-copy! winner=" (or winner :none) " len=" (count text))})
         (catch Throwable _ nil))
    ok?))

;;; ── Clipboard IMAGE read ─────────────────────────────────────────────────────
;;
;; Copying an image (a screenshot, a browser "Copy Image", ⌘⇧4-to-clipboard)
;; puts PIXELS on the pasteboard with NO text representation. When the user
;; then hits ⌘V, the terminal fires a bracketed paste whose payload is EMPTY -
;; the text-only paste path drops it and nothing shows in the input. The web
;; channel accepts pasted image blobs directly; here we reach for the same
;; pixels via OS helpers, drop them into a temp PNG, and hand the path back so
;; the normal image-paste flow (placeholder + send-time attach) takes over.

(def ^:private clipboard-image-helpers
  ;; argv lists for clipboard-READ-IMAGE helpers that write PNG bytes to
  ;; stdout. `pngpaste` is a Homebrew tool (macOS); macOS without it falls
  ;; back to the osascript path in `read-clipboard-image!`.
  [["pngpaste" "-"] ;; macOS (brew install pngpaste)
   ["wl-paste" "--type" "image/png"] ;; Wayland
   ["xclip" "-selection" "clipboard" "-t" "image/png" "-o"]]) ;; X11

(defn- run-shell-helper-bytes!
  "Like `run-shell-helper!` but returns the process's stdout as RAW bytes
   (`{:success? boolean :bytes byte-array}`) instead of a UTF-8 string, so a
   binary payload (clipboard image data) survives intact. stderr is DISCARDED
   rather than merged - merging would corrupt the image stream. Hard-capped
   by `run-helper-process!` (default 2s; marshalling a large screenshot off
   the pasteboard can take a moment) — a helper that never exits is killed
   at the deadline instead of freezing the input loop. The `timeout-ms`
   arity exists for tests."
  ([cmd] (run-shell-helper-bytes! cmd 2000))
  ([cmd timeout-ms] (run-helper-process! cmd {:timeout-ms timeout-ms})))

(defn- png-bytes?
  "True when `b` opens with the 8-byte PNG signature (`\\x89PNG`). Guards
   against a helper that 'succeeded' but wrote an empty/text payload."
  [^bytes b]
  (and b
       (>= (alength b) 8)
       (= 137 (bit-and 0xff (aget b 0)))
       (= 80 (bit-and 0xff (aget b 1)))
       (= 78 (bit-and 0xff (aget b 2)))
       (= 71 (bit-and 0xff (aget b 3)))))

(defn- write-temp-png!
  "Write `bytes` to a fresh temp `.png` file (auto-deleted on JVM exit) and
   return its absolute path."
  [^bytes bytes]
  (let [f (java.io.File/createTempFile "vis-clip-" ".png")]
    (.deleteOnExit f)
    (with-open [o (io/output-stream f)]
      (.write o bytes))
    (.getAbsolutePath f)))

(defn- macos? [] (str/starts-with? (str/lower-case (or (System/getProperty "os.name") "")) "mac"))

(defn- macos-osascript-clipboard-png!
  "macOS fallback when `pngpaste` isn't installed: ask AppleScript to marshal
   the clipboard's PNG representation straight to `path`. Returns true when the
   command exited cleanly. No image on the pasteboard raises inside the script,
   so the exit is non-zero and we report false."
  [^String path]
  (let
    [script (str "set outFile to (POSIX file \"" path
                 "\")\n" "set pngData to (the clipboard as \u00abclass PNGf\u00bb)\n"
                 "set fh to open for access outFile with write permission\n" "set eof of fh to 0\n"
                 "write pngData to fh\n" "close access fh")]
    (boolean (:success? (run-shell-helper-bytes! ["osascript" "-e" script])))))

(defn read-clipboard-image!
  "Best-effort read of an IMAGE sitting on the system clipboard. Writes it to a
   temp PNG file and returns `{:path :mime}` (mime always `\"image/png\"`), or
   nil when the clipboard holds no image / no helper is available. Lets ⌘V of a
   screenshot or a copied image attach the pixels.
   Never throws."
  []
  (try (or
         ;; stdout-based helpers: pngpaste / wl-paste / xclip.
         (loop [[cmd & rest] clipboard-image-helpers]
           (when cmd
             (let [{:keys [success? bytes]} (run-shell-helper-bytes! cmd)]
               (if (and success? (png-bytes? bytes))
                 {:path (write-temp-png! bytes) :mime "image/png"}
                 (recur rest)))))
         ;; macOS with no pngpaste: let AppleScript write the PNG for us.
         (when (macos?)
           (let
             [f
              (java.io.File/createTempFile "vis-clip-" ".png")

              path
              (.getAbsolutePath f)]

             (.deleteOnExit f)
             (if (and (macos-osascript-clipboard-png! path)
                      (png-bytes? (java.nio.file.Files/readAllBytes (.toPath f))))
               {:path path :mime "image/png"}
               (do (.delete f) nil)))))
       (catch Throwable _ nil)))

;;; ── Input buffer state ─────────────────────────────────────────────────────

(defn empty-input [] {:lines [""] :crow 0 :ccol 0})

(defn input->text [{:keys [lines]}] (str/join "\n" lines))

(defn input-empty?
  "True when the input buffer is the pristine empty prompt.

   Whitespace, extra lines, or cursor state from edits count as non-empty so
   Esc/Ctrl+C can clear the draft before they regain their quit/cancel meaning."
  [state]
  (= (empty-input) state))

;; ── Buffer editing ───────────────────────────────────────────────────────
;;
;; The cursor-motion and editing arithmetic lives in lanterna's reusable
;; `TextEditBuffer` (plain Java, unit-tested in the fork). These fns are thin
;; adapters: the public input state stays the `{:lines :crow :ccol}` map every
;; other part of the channel renders against, and each op converts to a
;; `TextEditBuffer`, delegates, and converts back. The keymap (`handle-key`)
;; and all vis-specific behaviour (paste placeholders, slash, send…) stay here.

(defn- ->buf
  "Adapt the `{:lines :crow :ccol}` input state to a lanterna `TextEditBuffer`."
  ^TextEditBuffer [{:keys [lines crow ccol]}]
  (TextEditBuffer/of ^java.util.List lines (int crow) (int ccol)))

(defn- buf->
  "Fold an edited `TextEditBuffer` back onto `st`, preserving any extra keys."
  [st ^TextEditBuffer b]
  (assoc st
    :lines (vec (.getLines b))
    :crow (.getRow b)
    :ccol (.getColumn b)))

(defn insert-char [st ch] (buf-> st (.insertCharacter (->buf st) (char ch))))

(defn insert-newline [st] (buf-> st (.newline (->buf st))))

(defn delete-backward [st] (buf-> st (.deleteBackward (->buf st))))

(defn delete-forward [st] (buf-> st (.deleteForward (->buf st))))

(defn delete-word-backward [st] (buf-> st (.deleteWordBackward (->buf st))))

(defn delete-line-backward [st] (buf-> st (.killToLineStart (->buf st))))

(defn kill-line [st] (buf-> st (.killLine (->buf st))))

(defn transpose-chars [st] (buf-> st (.transposeCharacters (->buf st))))

(defn move-left [st] (buf-> st (.moveLeft (->buf st))))

(defn move-right [st] (buf-> st (.moveRight (->buf st))))

(defn move-word-left [st] (buf-> st (.moveWordLeft (->buf st))))

(defn move-word-right [st] (buf-> st (.moveWordRight (->buf st))))

(defn move-line-start [st] (buf-> st (.moveLineStart (->buf st))))

(defn move-line-end [st] (buf-> st (.moveLineEnd (->buf st))))

(defn- emacs-edit
  "Apply a lanterna Emacs editing chord (C-a/C-e/C-b/C-f/C-p/C-n/C-k/C-u/C-w/C-d)
   to input state `st` via the SHARED `TextEditKeymap` — the SAME source of truth
   every lanterna `TextBox` uses, so the prompt and every dialog input behave
   identically. Returns the new state, or nil when `key` is not an editing chord
   (the dispatcher then tries app verbs / inserts the char)."
  [^KeyStroke key st]
  (when (TextEditKeymap/isBinding key)
    (when-let [edited (TextEditKeymap/apply (->buf st) key)]
      (buf-> st edited))))

(defn- palette-trigger?
  "True when `key` opens the command palette via the M-x ALIAS — Alt/Option +
   `keymap/palette-meta-key` (x), the canonical Emacs command launcher. The
   PRIMARY trigger, C-x p, is handled by the prefix dispatcher (see
   `resolve-prefix-key`), not here."
  [^KeyStroke key]
  (boolean (and (= KeyType/Character (.getKeyType key))
                (.isAltDown key)
                (not (.isCtrlDown key))
                (when-let [c (.getCharacter key)]
                  (= (Character/toLowerCase ^char c) keymap/palette-meta-key)))))

(defn move-up [st] (buf-> st (.moveUp (->buf st))))

(defn move-down [st] (buf-> st (.moveDown (->buf st))))

;; ── Paste placeholders  ─────────────────────────────────────────────────
;;
;; A multi-line / large clipboard payload doesn't go inline into the
;; input box - it would scroll the typing area off the user's screen
;; and force them to navigate around hundreds of unwanted rows. The
;; screen loop stashes the payload in `app-db :pastes` keyed by an
;; auto-incrementing id, and inserts a single-line PLACEHOLDER token
;; into the input buffer:
;;
;;     [Pasted #1: 42 lines]
;;
;; The user keeps typing around it, hits Enter, and the send path
;; substitutes every `[Pasted #N: ...]` with its content via
;; `expand-paste-placeholders` before the message reaches the agent.
;; Backspace right after the closing `]` deletes the whole token (and
;; the screen loop drops that entry from `:pastes` so memory tracks
;; what's actually still referenced).

(def placeholder-regex
  "Anchored shape `[Pasted #N: ...]` / `[Image #N: ...]`. The non-greedy
   negated-bracket body keeps the regex idempotent against nested-bracket
   text the user may have typed adjacent to the placeholder. Group 1 is
   always the numeric id — the leading kind word is non-capturing."
  #"\[(?:Pasted|Image) #(\d+): [^\]]*?\]")

(defn- format-bytes
  "Human-readable byte count: 1234 -> 1.2KB, 12 -> 12B. Locale-safe
   (US locale so a Polish JVM doesn't render a comma decimal)."
  [^long n]
  (cond (< n 1024) (str n "B")
        (< n (* 1024 1024))
        (String/format java.util.Locale/US "%.1fKB" (into-array Object [(double (/ n 1024.0))]))
        :else (String/format java.util.Locale/US
                             "%.1fMB"
                             (into-array Object [(double (/ n 1024.0 1024.0))]))))

(defn format-paste-placeholder
  "Produce the visible token text for `app-db :pastes` entry `entry`.
   Pure: same input -> same output, no allocation games. Used both
   when the screen loop inserts the token AND when the renderer or
   send path needs to re-derive the canonical shape.

   Always carries BOTH the line count AND the human-readable byte
   weight - the user wants to see both at a glance:

     [Pasted #1: 42 lines, 1.2KB]
     [Pasted #2: 1 line, 73B]

   `lines` is correctly pluralised; size is locale-safe via
   `format-bytes` so a Polish JVM doesn't render `1,2KB`."
  [{:keys [id content image]}]
  (if image
    ;; Image drop: name the file + its intrinsic size instead of the
    ;; pasted PATH's line/byte weight, which is meaningless to the user.
    (let
      [{:keys [filename width height size-label]}
       image

       dims
       (when (and width height (pos? (long width)) (pos? (long height))) (str width "×" height))]

      (str "[Image #"
           id
           ": "
           (or filename "image")
           (when dims (str " " dims))
           (when size-label (str ", " size-label))
           "]"))
    (let
      [text
       (str content)

       line-count
       (inc (count (filter #(= % \newline) text)))

       char-count
       (count text)

       line-word
       (if (= 1 line-count) "line" "lines")]

      (str "[Pasted #" id ": " line-count " " line-word ", " (format-bytes char-count) "]"))))

(def ^:const PASTE_INLINE_MAX_CHARS
  "Threshold below which we DON'T use a placeholder - a short
   single-line paste like `git rev-parse HEAD` reads naturally
   inline and a placeholder would just be noise."
  80)

(defn use-placeholder?
  "True when the pasted text is large enough OR multi-line enough
   that the user benefits from the placeholder UX. Single-line
   ASCII pastes shorter than `PASTE_INLINE_MAX_CHARS` go inline."
  [^String text]
  (boolean (or (.contains text "\n") (> (count text) PASTE_INLINE_MAX_CHARS))))

(defn expand-paste-placeholders
  "Substitute every `[Pasted #N: ...]` token in `text` with its content
   from `pastes-map`. Used by the send path so the agent receives
   the user's full payload, not the cosmetic token.

   `pastes-map` shape: `{<id-int> {:id N :content \"...\"}}`.
   Tokens whose id has no entry in the map (e.g. the user typed the
   bracket text manually) pass through unchanged.

   NOTE: `clojure.string/replace` with a fn already runs the fn's
   return through `Matcher/quoteReplacement` internally, so a
   payload carrying `$` or `\\` is handled verbatim - we don't
   double-quote here."
  [^String text pastes-map]
  (str/replace text
               placeholder-regex
               (fn [[whole id-str]]
                 (let
                   [id
                    (try (Integer/parseInt id-str) (catch Throwable _ nil))

                    entry
                    (when id (get pastes-map id))]

                   (if entry (str (:content entry)) whole)))))

(def ^:const PASTE_PREVIEW_HEAD_LINES
  "How many leading lines of a pasted payload the collapsed transcript
   preview shows."
  6)

(def ^:const PASTE_PREVIEW_TAIL_LINES
  "How many trailing lines of a pasted payload the collapsed transcript
   preview shows."
  3)

(def ^:const PASTE_PREVIEW_MAX_LINE_CHARS
  "A single previewed line longer than this is itself middle-elided so one
   2KB single-line paste can't blow the bubble open."
  200)

(defn- clamp-preview-line
  "Middle-elide one preview line that runs past `PASTE_PREVIEW_MAX_LINE_CHARS`
   - keep a generous head plus a short tail so the line stays identifiable."
  [^String line]
  (let [n (count line)]
    (if (<= n PASTE_PREVIEW_MAX_LINE_CHARS)
      line
      (str (subs line 0 130) " … " (subs line (- n 60))))))

(defn paste-content-preview
  "Head+tail preview lines for a pasted payload. Short payloads come back
   whole (each line clamped); long ones keep the first
   `PASTE_PREVIEW_HEAD_LINES` and last `PASTE_PREVIEW_TAIL_LINES` lines with
   a `⋯ N more lines ⋯` marker between them. Returns a seq of strings."
  [^String content]
  (let
    [lines
     (str/split (str content) #"\n" -1)

     n
     (count lines)]

    (if (<= n (+ PASTE_PREVIEW_HEAD_LINES PASTE_PREVIEW_TAIL_LINES 1))
      (map clamp-preview-line lines)
      (concat (map clamp-preview-line (take PASTE_PREVIEW_HEAD_LINES lines))
              [(str "⋯ " (- n PASTE_PREVIEW_HEAD_LINES PASTE_PREVIEW_TAIL_LINES) " more lines ⋯")]
              (map clamp-preview-line (take-last PASTE_PREVIEW_TAIL_LINES lines))))))

(defn collapse-paste-placeholders
  "Substitute every `[Pasted #N: ...]` token in `text` with a fenced
   `vis-paste` block the TUI renders as a COLLAPSIBLE disclosure: the
   `[Pasted #N: ...]` token becomes the chevron summary row, the full
   payload the body you expand to read. Used by the send path to build
   the VISIBLE transcript copy — the user keeps a one-line marker they
   can open on demand, while `expand-paste-placeholders` still ships the
   complete payload to the agent.

   The block body is `<token>\\n<full content>`: the renderer peels the
   first line off as the summary and shows the rest verbatim when
   expanded (no head+tail truncation — the whole paste survives a
   session reopen). A four-backtick fence keeps ordinary three-backtick
   code the payload may itself contain from closing it early. Tokens
   with no entry in `pastes-map` pass through unchanged."
  [^String text pastes-map]
  (str/replace text
               placeholder-regex
               (fn [[whole id-str]]
                 (let
                   [id
                    (try (Integer/parseInt id-str) (catch Throwable _ nil))

                    entry
                    (when id (get pastes-map id))]

                   (cond (nil? entry) whole
                         ;; Image drop: a `vis-image` fence carries the file path +
                         ;; metadata the renderer reads to draw the picture inline (it
                         ;; re-reads the file, so no base64 rides in the transcript and
                         ;; the image survives a session reopen). Body lines, in order:
                         ;; summary, absolute path, mime, `WxH`, size-label.
                         (:image entry)
                         (let [{:keys [path mime width height size-label]} (:image entry)]
                           (str "\n````vis-image\n" (format-paste-placeholder entry)
                                "\n" path
                                "\n" (or mime "")
                                "\n" (if (and width height) (str width "x" height) "")
                                "\n" (or size-label "")
                                "\n" "````\n"))
                         :else (str "\n````vis-paste\n"
                                    (format-paste-placeholder entry)
                                    "\n"
                                    (:content entry)
                                    "\n````\n"))))))

(defn placeholder-id-before-cursor
  "When the cursor of `state` sits IMMEDIATELY AFTER the closing `]`
   of a paste placeholder on the current line, return that
   placeholder's `:id` (Integer). nil otherwise. The screen loop
   uses this to turn one Backspace into a whole-token delete."
  [{:keys [lines crow ccol]}]
  (let
    [line
     (nth lines crow nil)

     ccol
     (long ccol)]

    (when (and (string? line) (pos? ccol))
      (let [before (subs line 0 ccol)]
        (when (str/ends-with? before "]")
          (let
            [m (re-find placeholder-regex before)
             ;; Only fire when the match SITS AT THE END of
             ;; `before`. A placeholder somewhere earlier on the
             ;; line shouldn't get nuked just because the cursor
             ;; is past it.
             match-start (when m (str/last-index-of before (first m)))
             match-end (when match-start (+ (long match-start) (count (first m))))]

            (when (and m match-end (= match-end (count before)))
              (try (Integer/parseInt (second m)) (catch Throwable _ nil)))))))))

(defn delete-placeholder-backward
  "Remove the placeholder token immediately before the cursor in
   `state`. Returns the new state (whole token gone, cursor at the
   start of where the token was). Caller is responsible for dropping
   the matching `:pastes` entry from app-db.

   No-op when no placeholder ends at the cursor - the screen loop
   guards via `placeholder-id-before-cursor` first."
  [{:keys [lines crow ccol] :as state}]
  (let
    [line
     (nth lines crow "")

     before
     (subs line 0 ccol)

     after
     (subs line ccol)

     m
     (re-find placeholder-regex before)]

    (if-let [match-start (when m (str/last-index-of before (first m)))]
      (let
        [new-line (str (subs before 0 match-start) after)
         new-ccol match-start]

        (-> state
            (assoc-in [:lines crow] new-line)
            (assoc :ccol new-ccol)))
      state)))

(def ^:private file-mention-regex #"(?<!\S)@(?:\"([^\"]+)\"|([A-Za-z0-9][A-Za-z0-9._/\-]*))")

(defn- file-mention-needs-quotes? [path] (boolean (re-find #"\s" (str path))))

(defn format-file-mention
  "Visible inline file mention token inserted by the `@` picker.
   Paths containing whitespace are quoted so send-time expansion can
   still recover the exact filename."
  [path]
  (let [path* (str path)]
    (if (file-mention-needs-quotes? path*) (str "@\"" path* "\"") (str "@" path*))))

(defn- resolve-local-file
  [path]
  (try (let
         [cwd
          (.getCanonicalFile (workspace/cwd))

          cwd-path
          (.getPath cwd)

          prefix
          (str cwd-path java.io.File/separator)

          candidate
          (.getCanonicalFile (io/file cwd path))

          candidate-p
          (.getPath candidate)]

         (when (and (.isFile candidate)
                    (or (= candidate-p cwd-path) (str/starts-with? candidate-p prefix)))
           candidate))
       (catch Throwable _ nil)))

(defn- file-mention->prompt-block
  "Agent-facing expansion of `@path`. Minimal directive only - tells
   the model the user attached a file and to read it via the normal
   tool surface before answering. Does NOT pre-bind a Python variable, does
   NOT prescribe a specific `(cat ...)` / `(cat ...)` form,
   does NOT inject a line range. The model picks the right tool
   (`cat`, `z/symbols`, `z/locators`, etc.) based on
   the question.

   Unresolved paths fall back to the visible `@path` token verbatim
   so the agent still sees the user's exact mention."
  [path]
  (if (resolve-local-file path)
    (str "[Attached File: " path
         "]\n" "The user attached this file. Read it (via the file/zipper tools) before answering.")
    (format-file-mention path)))

(defn expand-file-mentions
  "Replace inline `@path/to/file` mentions with a short read-this-file
   directive aimed at the agent.

   The visible chat transcript keeps the concise `@path` token (the
   `:send-message` event passes the un-expanded text as the display
   value, so the user bubble and persisted `user_request` stay short).
   The outbound agent prompt - this function's output - carries the
   directive so the model knows the user attached the file and should
   read it before answering. We deliberately do NOT inject any
   `(def ...)` / `(cat ...)` / `(cat ...)` boilerplate; forcing
   a specific first-observation form makes the model dump focused-range
   previews even when the question is structural and wastes iterations.
   Unknown paths pass through unchanged."
  [^String text]
  (str/replace text
               file-mention-regex
               (fn [[_ quoted-path bare-path]]
                 (file-mention->prompt-block (or quoted-path bare-path)))))

(defn paste-text
  [{:keys [lines crow ccol] :as st} text]
  (let
    [paste-lines
     (str/split text #"\r?\n" -1)

     current-line
     (nth lines crow)

     before
     (subs current-line 0 ccol)

     after
     (subs current-line ccol)]

    (if (= 1 (count paste-lines))
      (-> st
          (assoc-in [:lines crow] (str before (first paste-lines) after))
          (assoc :ccol (+ (long ccol) (count (first paste-lines)))))
      (let
        [first-l
         (str before (first paste-lines))

         last-l
         (str (last paste-lines) after)

         mid
         (subvec (vec paste-lines) 1 (dec (count paste-lines)))

         new-crow
         (+ (long crow) (dec (count paste-lines)))]

        (-> st
            (assoc :lines (into (conj (subvec lines 0 crow) first-l)
                                (concat mid [last-l] (subvec lines (inc (long crow))))))
            (assoc :crow new-crow)
            (assoc :ccol (count (last paste-lines))))))))

;;; ── Key handling ───────────────────────────────────────────────────────────

(defn- resolve-prefix-key
  "Resolve the keystroke pressed AFTER the C-x prefix (state carries `:prefix`).
   The prefix is cleared either way:
     • C-x p (or C-x C-p) (`keymap/prefix-palette-key`) → the Command Palette.
     • C-x <letter> in `keymap/prefix-commands` → the vis verb.
     • anything else → abort the prefix (no-op), so a stray C-x never swallows
       the next keystroke."
  [^KeyStroke key state]
  (let
    [state
     (dissoc state :prefix)

     c
     (when (= KeyType/Character (.getKeyType key)) (.getCharacter key))]

    (cond
      ;; C-x p → command palette. A PLAIN `p` is the advertised trigger; a Ctrl'd
      ;; second key (C-x C-p) is also accepted for old muscle memory.
      (and c (= (Character/toLowerCase ^char c) keymap/prefix-palette-key)) {:action :show-palette
                                                                             :state state}
      ;; C-x <letter> → a vis verb (plain second key).
      (and c (keymap/prefix-action-for c)) {:action (keymap/prefix-action-for c) :state state}
      ;; C-x ← / C-x → → previous / next workspace (Emacs previous-buffer /
      ;; next-buffer). The switch-to-buffer LIST is C-x b (a `:show-sessions`
      ;; prefix verb resolved by the `prefix-action-for` clause above).
      (= KeyType/ArrowLeft (.getKeyType key))
      {:action :select-tab-index :workspace-index :prev :state state}
      (= KeyType/ArrowRight (.getKeyType key))
      {:action :select-tab-index :workspace-index :next :state state}
      ;; C-x <digit> → jump straight to workspace N (C-x 1 … C-x 9), the Emacs
      ;; numeric buffer reflex (mirrors the M-1 … M-9 chords). Digits are 1-based
      ;; on screen, the index 0-based; C-x 0 is ignored (no 0th workspace). A
      ;; non-existent N is caught downstream and surfaced as a TUI notice.
      (and c (Character/isDigit c) (not= \0 c))
      {:action :select-tab-index :workspace-index (dec (Character/digit c 10)) :state state}
      ;; C-x TAB / C-x S-TAB → Emacs global fold cycle (org/magit `<backtab>`):
      ;; toggle EVERY disclosure collapsed↔expanded in one keystroke. Living behind
      ;; the C-x prefix keeps it off bare Tab/S-Tab (which switch workspaces).
      (#{KeyType/Tab KeyType/ReverseTab} (.getKeyType key)) {:action :toggle-all-details
                                                             :state state}
      :else {:action :continue :state state})))

(defn handle-key
  "Process keystroke. Returns {:action kw, :state s}."
  [^KeyStroke key state]
  (if (:prefix state)
    ;; In a C-x prefix: the NEXT keystroke picks a vis command (or aborts it).
    (resolve-prefix-key key state)
    (let [ktype (.getKeyType key)]
      (condp = ktype
        ;; Esc clears a non-empty draft first. Press Esc again on an empty input
        ;; to keep the existing cancel-turn behaviour.
        KeyType/Escape (if (input-empty? state)
                         {:action :cancel :state state}
                         {:action :clear-input :state (empty-input)})
        KeyType/Character
        (let
          [c (.getCharacter key)
           ctrl (.isCtrlDown key)
           alt (.isAltDown key)
           ;; Emacs editing chord result (nil unless `key` is one) — computed
           ;; once, applied below. Editing keys take PRECEDENCE over app verbs.
           emacs (emacs-edit key state)]

          (cond
            ;; ── Clipboard is the TERMINAL's job, not ours ───────────────────
            ;; The terminal auto-copies on selection and pastes natively
            ;; (bracketed paste is caught by the screen loop and turned into a
            ;; paste placeholder), so the editor binds NEITHER Ctrl+C nor Ctrl+V
            ;; for clipboard. Ctrl+C keeps its terminal reflex: quit on an empty
            ;; prompt, clear the draft otherwise — and while a turn is running
            ;; the screen loop turns that clear into cancel-turn.
            (and ctrl (= (Character/toLowerCase c) keymap/quit-key)) (if (input-empty? state)
                                                                       {:action :quit :state state}
                                                                       {:action :clear-input
                                                                        :state (empty-input)})
            ;; ── Ctrl+G: Emacs keyboard-quit (abort) — same as Escape. The screen
            ;; loop turns :cancel / :clear-input into "cancel turn / close dialog /
            ;; clear draft" by priority.
            (and ctrl (= (Character/toLowerCase c) keymap/abort-key))
            (if (input-empty? state)
              {:action :cancel :state state}
              {:action :clear-input :state (empty-input)})
            ;; ── Emacs editing keys (FULL set, first-class) ──────────────────
            ;; C-a/C-e begin/end · C-b/C-f back/forward char · C-p/C-n prev/next
            ;; line · C-k kill-to-end · C-u kill-to-start · C-w kill-word · C-d
            ;; delete-char · C-t transpose — ALL routed through lanterna's shared
            ;; `TextEditKeymap`, the SAME source of truth every lanterna `TextBox`
            ;; (dialog inputs) uses, so the chords behave identically in every
            ;; input. They take precedence over app verbs / the prefix.
            emacs {:action :continue :state emacs}
            ;; ── Ctrl+X: the Emacs PREFIX key. Arm it — the NEXT (PLAIN) key runs a
            ;; vis command (C-x m model · r reasoning · l length · f search · a
            ;; attach · v voice · d dirs · s resources · h help) or C-x C-p palette.
            ;; Plain second keys so Ctrl-byte collisions (Ctrl+S=flow, Ctrl+M=Enter)
            ;; can't eat them. Resolved at the top of `handle-key` via :prefix.
            (and ctrl (= (Character/toLowerCase c) keymap/prefix-key))
            {:action :continue :state (assoc state :prefix :cx)}
            ;; ── Command palette ── M-x (Alt+x), the canonical Emacs alias for the
            ;; C-x C-p palette (the search / providers / new-session verbs live there).
            (palette-trigger? key) {:action :show-palette :state state}
            ;; ── Ctrl+L: Emacs recenter — jump the conversation to the bottom and
            ;; force a repaint (the screen loop owns :recenter).
            (and ctrl (= (Character/toLowerCase c) keymap/recenter-key)) {:action :recenter
                                                                          :state state}
            ;; ── Emacs buffer navigation (faithful) — the transcript IS the buffer:
            ;;   M-> end-of-buffer (the REAL Emacs "jump to bottom"; C-l above is
            ;;       recenter, kept as a vis alias), M-< beginning-of-buffer,
            ;;   C-v scroll forward a screen (page down), M-v scroll backward (page up).
            ;; Meta needs "Use Option as Meta" on macOS — same as the M-x palette.
            (and alt (= c \>)) {:action :recenter :state state}
            (and alt (= c \<)) {:action :scroll-to-top :state state}
            (and ctrl (= (Character/toLowerCase c) \v)) {:action :scroll-down :state state}
            (and alt (= (Character/toLowerCase c) \v)) {:action :scroll-up :state state}
            ;; ── Emacs word motion — M-b / M-f (backward-word / forward-word).
            ;; These are ALSO exactly what stock macOS terminals (Ghostty,
            ;; Terminal.app, iTerm2) send for Option+←/→: their default keybinds
            ;; translate the chord into `ESC b` / `ESC f`, NOT the xterm modified
            ;; arrow `ESC[1;3D/C` that `modified-arrow-pattern` decodes. Lanterna
            ;; surfaces those bytes as Alt+b / Alt+f Character keystrokes, so this
            ;; clause is what makes Option+arrow word motion work on macOS.
            (and alt (not ctrl) (= (Character/toLowerCase c) \b)) {:action :continue
                                                                   :state (move-word-left state)}
            (and alt (not ctrl) (= (Character/toLowerCase c) \f)) {:action :continue
                                                                   :state (move-word-right state)}
            ;; ── Alt+<digit>: jump straight to workspace N (M-1 … M-9), the
            ;; terminal-tab / Emacs numeric reflex. Digits are 1-based on screen,
            ;; the index is 0-based; Alt+0 is ignored (there is no 0th workspace).
            (and alt (Character/isDigit c) (not= \0 c))
            {:action :select-tab-index :workspace-index (dec (Character/digit c 10)) :state state}
            ;; No DIRECT app-verb chords remain — every verb (help included, now
            ;; C-x h) is C-x-prefixed or in the palette. Clause kept total in
            ;; case a direct chord is re-added.
            (and ctrl (keymap/action-for c)) {:action (keymap/action-for c) :state state}
            ;; Unbound control / meta chords are ignored instead of inserting their
            ;; letter payload into the prompt.
            (or ctrl alt) {:action :continue :state state}
            :else {:action :continue :state (insert-char state c)}))
        KeyType/Tab (if (.isShiftDown key)
                      {:action :select-tab-index :workspace-index :prev :state state}
                      {:action :select-tab-index :workspace-index :next :state state})
        KeyType/ReverseTab {:action :select-tab-index :workspace-index :prev :state state}
        ;; App verbs live on cross-platform Ctrl chords (Ctrl+F search, Ctrl+R
        ;; reasoning, …) + the Ctrl+P palette, dispatched from `keymap/bindings`
        ;; in the Character branch above. While
        ;; search is ACTIVE the screen key-loop intercepts typing → query and
        ;; Ctrl+N/P → next/prev BEFORE this handler.
        KeyType/Enter (if (.isAltDown key)
                        {:action :continue :state (insert-newline state)}
                        {:action :send :state state})
        ;; Backspace: plain deletes a char; Ctrl/Alt+Backspace deletes the word
        ;; back (the common editor reflex). Help is no longer here — it moved to
        ;; the C-x prefix (C-x h) — so a ctrl-modified Backspace is free to be
        ;; word-delete, matching Alt+Backspace.
        KeyType/Backspace {:action :continue
                           :state (if (or (.isAltDown key) (.isCtrlDown key))
                                    (delete-word-backward state)
                                    (delete-backward state))}
        ;; Workspace switching is the Emacs way: C-x ←/→ (previous/next-buffer)
        ;; and C-x b (switch-to-buffer list) — handled by `resolve-prefix-key`.
        ;; Bare arrows move the cursor; Ctrl/Alt+arrow is word motion.
        KeyType/ArrowLeft {:action :continue
                           :state (if (or (.isCtrlDown key) (.isAltDown key))
                                    (move-word-left state)
                                    (move-left state))}
        KeyType/ArrowRight {:action :continue
                            :state (if (or (.isCtrlDown key) (.isAltDown key))
                                     (move-word-right state)
                                     (move-right state))}
        KeyType/Home {:action :continue :state (move-line-start state)}
        ;; Ctrl+End — "end of buffer": jump the conversation to the newest content
        ;; (the SAME FOLLOW re-arm the C-l recenter and the `↓ latest` button do).
        ;; Plain End stays end-of-line editing inside the input box.
        KeyType/End (if (.isCtrlDown key)
                      {:action :recenter :state state}
                      {:action :continue :state (move-line-end state)})
        ;; Arrow Up/Down - input history, or Alt+Shift+↑/↓ session switcher
        KeyType/ArrowUp (if (and (.isAltDown key) (.isShiftDown key))
                          {:action :show-sessions :state state}
                          {:action :history-up :state state})
        KeyType/ArrowDown (if (and (.isAltDown key) (.isShiftDown key))
                            {:action :show-sessions :state state}
                            {:action :history-down :state state})
        KeyType/PageUp {:action :scroll-up :state state}
        KeyType/PageDown {:action :scroll-down :state state}
        ;; Ignore everything else (including mouse events)
        {:action :continue :state state}))))

;;; ── Message formatting ─────────────────────────────────────────────────────

(defn format-message
  [text]
  (let [ls (str/split-lines text)]
    (into [(str "you: " (first ls))] (map #(str "     " %) (rest ls)))))
