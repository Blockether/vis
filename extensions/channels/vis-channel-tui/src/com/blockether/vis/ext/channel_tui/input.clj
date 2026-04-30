(ns com.blockether.vis.ext.channel-tui.input
  "Keyboard / paste / clipboard surface for the TUI channel.

   Important: this namespace MUST NOT import any `java.awt.*` class.
   Loading AWT on macOS — even just touching `Toolkit` from a JVM
   subprocess — spawns a full Cocoa application, registers a Dock
   icon, and steals focus from the user's terminal. We saw it in
   the wild: a single `verify.sh` run popped the Java rocket onto
   the Dock and switched the front window away from the shell.

   Clipboard read/write therefore goes through OS shell helpers
   only: `pbcopy` / `pbpaste` on macOS (always present since 10.0),
   `wl-copy` / `wl-paste` on Wayland, `xclip` / `xsel` on X11. AWT
   is gone for good — not a fallback, not a comment, not an
   import."
  (:require [clojure.string :as str]
            [taoensso.telemere :as tel])
  (:import [com.googlecode.lanterna TerminalPosition]
           [com.googlecode.lanterna.input
            CharacterPattern CharacterPattern$Matching
            KeyDecodingProfile KeyStroke KeyType
            MouseAction MouseActionType]
           [com.googlecode.lanterna.terminal.ansi UnixTerminal]))

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

;; ── SGR mouse mode (1006) ─────────────────────────────────────────────
;;
;; Lanterna's stock `MouseCharacterPattern` parses the *legacy* X10
;; mouse protocol: `ESC [ M Cb Cx Cy` where Cx/Cy are RAW BYTES with
;; offset 32. The moment a column or row exceeds 95, the byte rises
;; above 0x7F and the JVM's UTF-8 input decoder treats it as an
;; invalid byte, replacing it with U+FFFD (65533). Lanterna then
;; subtracts 33 — producing a phantom column of `65500` that of
;; course doesn't match any click region. The header copy-id
;; affordance lives near the right edge of the screen, where this
;; bug fires every single time on a terminal wider than ~100
;; columns.
;;
;; SGR mode (CSI 1006) sidesteps the UTF-8 issue entirely by
;; encoding the same payload as PURE ASCII text:
;;
;;     ESC [ < Cb ; Cx ; Cy M     (button-down / drag / move / scroll)
;;     ESC [ < Cb ; Cx ; Cy m     (button-release — lowercase `m`)
;;
;; Cx/Cy are 1-based decimal numbers. We register a custom
;; `CharacterPattern` that recognises this shape and emits a proper
;; `MouseAction`, then send `ESC [ ? 1006 h` alongside the legacy
;; mode-1003 enable that Lanterna already does, so the terminal
;; switches over to the ASCII format.

(defn- digits->int ^long [digits]
  ;; Parses a non-empty decimal sequence held in a vector of
  ;; java.lang.Character. Used for the SGR Cb / Cx / Cy fields.
  ;; Allocation-free over the input — no temp String created.
  (loop [i 0 acc 0]
    (if (>= i (count digits))
      acc
      (let [c (long (.charValue ^Character (nth digits i)))]
        (recur (inc i) (+ (* 10 acc) (- c 48)))))))

(defn- sgr-mouse-decode
  "Given the parsed SGR fields `[button col row final-char]`, return
   the `MouseActionType` that fits. Mirrors Lanterna's stock
   classification but on SGR semantics: the high bits of the button
   byte distinguish button-press / drag / scroll, the case of the
   final character distinguishes press (`M`) from release (`m`)."
  [^long button ^long _col ^long _row ^long final-ch]
  (let [release? (= final-ch (long (int \m)))
        ;; bit 5 (0x20) = drag/move, bit 6 (0x40) = wheel.
        drag?    (not (zero? (bit-and button 0x20)))
        wheel?   (not (zero? (bit-and button 0x40)))
        button-bits (bit-and button 0x03)]
    (cond
      wheel?
      (if (zero? button-bits) MouseActionType/SCROLL_UP
        MouseActionType/SCROLL_DOWN)
      drag?
      ;; button bits 3 (0x03 == 3) means \"no button held\" → plain
      ;; cursor movement; otherwise a drag with that button held.
      (if (= button-bits 3) MouseActionType/MOVE
        MouseActionType/DRAG)
      release? MouseActionType/CLICK_RELEASE
      :else    MouseActionType/CLICK_DOWN)))

(defn- sgr-button-number ^long [^long button]
  ;; Map raw SGR button byte to Lanterna's 1-based button number,
  ;; matching `MouseCharacterPattern` semantics (1=left, 2=middle,
  ;; 3=right, 4=wheel-up, 5=wheel-down). For DRAG/MOVE/RELEASE the
  ;; field still carries the button bits, so the same mapping holds.
  (let [wheel? (not (zero? (bit-and button 0x40)))
        bits   (bit-and button 0x03)]
    (cond
      wheel? (if (zero? bits) 4 5)
      ;; bits == 3 in non-wheel modes means \"no button held\" —
      ;; that's the SGR convention for plain motion. Report 0 to
      ;; match what Lanterna's parser does for legacy MOVE events.
      (= bits 3) 0
      :else  (inc bits))))

(def sgr-mouse-pattern
  "CharacterPattern matching `ESC [ < N ; N ; N M/m`. Emits a
   `MouseAction` with proper integer column/row so SGR-capable
   terminals (every modern macOS / Linux emulator) deliver clicks
   that the screen handler can hit-test against click regions.

   Returns NOT_YET while the prefix is still consistent with the
   shape, returns nil (no match) the instant a character violates
   it, returns Matching with the built MouseAction once the final
   `M` / `m` byte arrives."
  (reify CharacterPattern
    (match [_ chars]
      (let [n (.size ^java.util.List chars)]
        (cond
          (zero? n) CharacterPattern$Matching/NOT_YET

          ;; chars[0] must be ESC (0x1B); chars[1] '['; chars[2] '<'.
          (or (and (>= n 1)
                (not= 0x1B (long (.charValue ^Character (.get ^java.util.List chars 0)))))
            (and (>= n 2)
              (not= \[ (.charValue ^Character (.get ^java.util.List chars 1))))
            (and (>= n 3)
              (not= \< (.charValue ^Character (.get ^java.util.List chars 2)))))
          nil

          (< n 4) CharacterPattern$Matching/NOT_YET

          :else
          ;; Walk chars[3..] as: digits ';' digits ';' digits final.
          (let [seen (volatile! [])      ;; collected digits for the
                                         ;; field currently being read
                fields (volatile! [])]   ;; completed fields so far
            (loop [i 3]
              (if (>= i n)
                CharacterPattern$Matching/NOT_YET
                (let [c (.charValue ^Character (.get ^java.util.List chars i))]
                  (cond
                    ;; ASCII digit: accumulate.
                    (and (>= (long c) 48) (<= (long c) 57))
                    (do (vswap! seen conj (Character. c))
                      (recur (inc i)))

                    ;; Field separator. Need at least one digit
                    ;; collected, and at most 2 fields finished
                    ;; before this one.
                    (= c \;)
                    (cond
                      (empty? @seen) nil
                      (>= (count @fields) 2) nil
                      :else
                      (do (vswap! fields conj (digits->int @seen))
                        (vreset! seen [])
                        (recur (inc i))))

                    ;; Terminator. Must be `M` or `m`, must arrive
                    ;; with exactly two completed fields plus one
                    ;; in-progress (button, col, row).
                    (or (= c \M) (= c \m))
                    (if (and (= (count @fields) 2) (seq @seen))
                      (let [button (long (nth @fields 0))
                            col    (long (nth @fields 1))
                            row    (long (digits->int @seen))
                            atype  (sgr-mouse-decode button col row (long (int c)))
                            btn    (sgr-button-number button)
                            ;; SGR is 1-indexed, Lanterna's 0-indexed.
                            pos    (TerminalPosition. (max 0 (dec col))
                                     (max 0 (dec row)))]
                        (CharacterPattern$Matching.
                          (MouseAction. atype (int btn) pos)))
                      nil)

                    ;; Anything else — stray byte mid-sequence —
                    ;; rejects this pattern. Lanterna will hand the
                    ;; chars to the next pattern in the profile.
                    :else nil))))))))))

(defn enable-sgr-mouse!
  "Send `ESC [ ? 1006 h`. Asks the terminal to deliver subsequent
   mouse events in SGR encoding. Independent of the legacy
   mode-1003 enable Lanterna already issues via
   `setMouseCaptureMode`; both can be active at once and the
   terminal will use whichever the remote consumer claims to
   support — every modern emulator (Apple Terminal 2.10+, iTerm2,
   alacritty, kitty, wezterm, foot, gnome-terminal, mintty,
   vscode integrated terminal) honours 1006."
  [^java.io.OutputStream out]
  (try
    (.write out (.getBytes "\u001B[?1006h" "UTF-8"))
    (.flush out)
    (catch Throwable _ nil)))

(defn disable-sgr-mouse!
  "Reverse of `enable-sgr-mouse!`. Sent in the screen tear-down
   `finally` so a crashed TUI doesn't leave the user's shell with
   the mode dangling."
  [^java.io.OutputStream out]
  (try
    (.write out (.getBytes "\u001B[?1006l" "UTF-8"))
    (.flush out)
    (catch Throwable _ nil)))

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
  "Register Alt+Enter, bracketed-paste, and SGR-mouse patterns on
   the terminal's input decoder. Without `sgr-mouse-pattern` the
   stock Lanterna parser handles only legacy X10 mouse events,
   whose raw-byte coordinate encoding clashes with the JVM's
   UTF-8 input decoder for any column/row beyond 95 — producing
   a phantom `mx=65500` from a U+FFFD replacement byte."
  [^UnixTerminal terminal]
  (.addProfile (.getInputDecoder terminal)
    (reify KeyDecodingProfile
      (getPatterns [_] [alt-enter-pattern
                        paste-start-pattern
                        paste-end-pattern
                        sgr-mouse-pattern]))))

;;; ── Clipboard (shell helpers, AWT-free) ─────────────────────────────────────
;;
;; Why no AWT, anywhere: importing `java.awt.Toolkit` on a macOS JVM
;; is enough to register a Dock icon and steal focus from the user's
;; terminal the first time the AWT subsystem initialises. We saw it
;; on a normal `verify.sh` run — a single class-load triggered the
;; Cocoa bridge and yanked the foreground app away. Even guarding
;; the call sites isn't enough; the side effect is at the JVM /
;; class-init layer, well below our control. The only safe choice
;; is to keep `java.awt.*` out of the codebase entirely.
;;
;; Two shell candidate lists, one per direction. Ordered most-likely
;; first per platform but every entry is tried in turn, so a macOS
;; user running inside a Linux container (or vice versa) still has
;; a working path. Each helper is given 1 second to finish so a
;; hung process can't pin the input thread.

(def ^:private copy-helpers
  ;; argv lists for clipboard-WRITE helpers. Helper reads payload
  ;; from stdin.
  [["pbcopy"]                            ;; macOS
   ["wl-copy"]                            ;; Wayland
   ["xclip" "-selection" "clipboard"]    ;; X11
   ["xsel" "--clipboard" "--input"]])    ;; X11 alt

(def ^:private paste-helpers
  ;; argv lists for clipboard-READ helpers. Helper writes payload
  ;; to stdout, which we capture verbatim and decode as UTF-8.
  [["pbpaste"]                            ;; macOS
   ["wl-paste" "--no-newline"]           ;; Wayland
   ["xclip" "-selection" "clipboard" "-o"] ;; X11
   ["xsel" "--clipboard" "--output"]])   ;; X11 alt

(defn- run-shell-helper!
  "Spawn `cmd` (an argv vector), feed `stdin-bytes` to its stdin
   when non-nil, drain stdout, return `{:ok? boolean :stdout String}`.
   Capped at 1 second; a missing helper or unreadable PATH entry
   raises in the `try` block and we report `{:ok? false}` so the
   caller falls through to the next candidate."
  [cmd ^bytes stdin-bytes]
  (try
    (let [pb  (ProcessBuilder. ^java.util.List cmd)
          _   (.redirectErrorStream pb true)
          p   (.start pb)]
      (when stdin-bytes
        (let [out (.getOutputStream p)]
          (.write out stdin-bytes)
          (.close out)))
      (let [in   (.getInputStream p)
            buf  (java.io.ByteArrayOutputStream.)
            tmp  (byte-array 4096)]
        ;; Drain stdout BEFORE waitFor: bigger paste payloads can
        ;; exceed the OS pipe buffer and deadlock the helper if its
        ;; output isn't being read.
        (loop []
          (let [n (.read in tmp)]
            (when (pos? n)
              (.write buf tmp 0 n)
              (recur))))
        (.waitFor p 1 java.util.concurrent.TimeUnit/SECONDS)
        {:ok?    (zero? (.exitValue p))
         :stdout (.toString buf "UTF-8")}))
    (catch Throwable _ {:ok? false :stdout nil})))

(defn- shell-clipboard-copy!
  "Try every helper in `copy-helpers` until one succeeds. Returns
   the keyword name of the helper that won (`:pbcopy`, `:wl-copy`,
   `:xclip`, `:xsel`) or `:none` when every candidate failed."
  [^String text]
  (let [bytes (.getBytes text "UTF-8")]
    (loop [[cmd & rest] copy-helpers]
      (if (nil? cmd)
        :none
        (if (:ok? (run-shell-helper! cmd bytes))
          (keyword (first cmd))
          (recur rest))))))

(defn- shell-clipboard-paste!
  "Try every helper in `paste-helpers` until one returns a non-empty
   string. Returns the captured text or `nil`."
  []
  (loop [[cmd & rest] paste-helpers]
    (when cmd
      (let [{:keys [ok? stdout]} (run-shell-helper! cmd nil)]
        (cond
          (and ok? (some? stdout) (pos? (count stdout))) stdout
          :else (recur rest))))))

(defn clipboard-paste
  "Read the system clipboard as a UTF-8 string. Returns the captured
   text or `nil` when no helper produced output. Pure shell pipeline
   — no AWT, no Cocoa init, no Dock icon."
  []
  (try (shell-clipboard-paste!) (catch Throwable _ nil)))

(defn clipboard-copy!
  "Best-effort copy `text` onto the system clipboard via shell
   helpers (`pbcopy` / `wl-copy` / `xclip` / `xsel`). Returns true
   on success, false when every helper failed. Logs the winning
   helper so \"the copy didn't work\" reports can be diagnosed
   against `~/.vis/vis.log`.

   AWT is intentionally absent: on macOS, touching
   `Toolkit.getDefaultToolkit()` boots a full Cocoa app (Dock icon,
   menu bar, focus theft); on headless Linux it throws; on SSH it
   writes to the wrong machine's clipboard. The shell helpers Just
   Work on every supported platform."
  [^String text]
  (let [winner (shell-clipboard-copy! text)
        ok?    (not= winner :none)]
    (try
      (tel/log!
        {:level (if ok? :info :warn)
         :id    ::clipboard-copy
         :data  {:winner   winner
                 :len      (count text)
                 :platform (System/getProperty "os.name")}
         :msg   (str "clipboard-copy! winner=" winner
                  " len=" (count text))})
      (catch Throwable _ nil))
    ok?))

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
