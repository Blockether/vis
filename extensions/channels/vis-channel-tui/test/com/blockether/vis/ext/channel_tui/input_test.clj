(ns com.blockether.vis.ext.channel-tui.input-test
  "Coverage for `channel-tui.input`.

   Two surfaces under test:

     1. The pure input-buffer helpers (`empty-input`, `insert-char`,
        `insert-newline`, `delete-backward`, `move-*`, `paste-text`)
        - used by every keystroke handled in the screen poll loop.
     2. The bracketed-paste sentinels + helpers
        (`paste-start?`, `paste-end?`, `keystroke->paste-char`).
        These power the multi-line paste flow added so a clipboard
        payload with embedded newlines doesn't fire mid-paste sends.

   The hard rule (AGENTS.md): every namespace ships with a test
   file. This is that file."
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.input :as input]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.googlecode.lanterna.input CharacterPattern InputDecoder KeyDecodingProfile KeyStroke
            KeyType]
           [java.io StringReader]))

(defn- pat-match
  "Typed `.match` so the CharacterPattern (and its Matching result) don't reflect."
  ^com.googlecode.lanterna.input.CharacterPattern$Matching [^CharacterPattern p chars]
  (.match p chars))

(defdescribe input-buffer-test
             (it "empty-input is one empty line, cursor at 0,0"
                 (let [s (input/empty-input)]
                   (expect (= [""] (:lines s)))
                   (expect (= 0 (:crow s)))
                   (expect (= 0 (:ccol s)))))
             (it "insert-char appends + advances cursor"
                 (let [s (-> (input/empty-input)
                             (input/insert-char \h)
                             (input/insert-char \i))]
                   (expect (= ["hi"] (:lines s)))
                   (expect (= 2 (:ccol s)))))
             (it "insert-newline splits the current line at the cursor"
                 (let [s (-> (input/empty-input)
                             (input/insert-char \a)
                             (input/insert-char \b)
                             (input/move-left)
                             (input/insert-newline))]
                   (expect (= ["a" "b"] (:lines s)))
                   (expect (= 1 (:crow s)))
                   (expect (= 0 (:ccol s)))))
             (it "delete-backward removes the char before the cursor"
                 (let [s (-> (input/empty-input)
                             (input/insert-char \x)
                             (input/insert-char \y)
                             (input/delete-backward))]
                   (expect (= ["x"] (:lines s)))))
             (it "input->text re-joins lines with newlines"
                 (let [s (-> (input/empty-input)
                             (input/insert-char \a)
                             (input/insert-newline)
                             (input/insert-char \b))]
                   (expect (= "a\nb" (input/input->text s))))))

(defdescribe paste-text-test
             (it "single-line paste inserts inline"
                 (let [s (-> (input/empty-input)
                             (input/paste-text "hello"))]
                   (expect (= ["hello"] (:lines s)))
                   (expect (= 5 (:ccol s)))))
             (it "multi-line paste lays each line out as its own row"
                 (let [s (-> (input/empty-input)
                             (input/paste-text "first\nsecond\nthird"))]
                   (expect (= ["first" "second" "third"] (:lines s)))
                   (expect (= 2 (:crow s)))
                   (expect (= 5 (:ccol s)))))
             (it "paste at non-zero cursor splices around the existing text"
                 (let [s (-> (input/empty-input)
                             (input/insert-char \[)
                             (input/paste-text "x\ny")
                             (input/insert-char \]))]
                   (expect (= ["[x" "y]"] (:lines s)))
                   (expect (= 1 (:crow s)))
                   (expect (= 2 (:ccol s)))))
             (it "CRLF newlines in the payload are normalised to LF"
                 (let [s (-> (input/empty-input)
                             (input/paste-text "a\r\nb"))]
                   (expect (= ["a" "b"] (:lines s))))))

(defn- char-key [^Character ch] (KeyStroke. ch false false))

(defn- ctrl-key [^Character ch] (KeyStroke. ch true false))

(defn- special-key [^KeyType ktype] (KeyStroke. ktype false false))

(defn- alt-key [^Character ch] (KeyStroke. ch false true))

(defn- alt-special-key [^KeyType ktype] (KeyStroke. ktype false true))

(defn- ctrl-special-key [^KeyType ktype] (KeyStroke. ktype true false))

(defn- alt-shift-special-key [^KeyType ktype] (KeyStroke. ktype false true true))

(defdescribe
  handle-key-test
  (it "Arrow Up/Down cycle input history"
      (let [state (input/empty-input)]
        (expect (= :history-up (:action (input/handle-key (special-key KeyType/ArrowUp) state))))
        (expect (= :history-down
                   (:action (input/handle-key (special-key KeyType/ArrowDown) state))))))
  (it "Page Up/Down still scroll the transcript"
      (let [state (input/empty-input)]
        (expect (= :scroll-up (:action (input/handle-key (special-key KeyType/PageUp) state))))
        (expect (= :scroll-down
                   (:action (input/handle-key (special-key KeyType/PageDown) state))))))
  (it "Alt+Shift+Up/Down open the session switcher"
      ;; Sessions is a palette-only verb now (Ctrl+P → Switch Session); the
      ;; Alt+Shift+↑/↓ arrow shortcut stays (decoded from the xterm sequence,
      ;; which terminals do deliver — unlike a bare Option+letter).
      (let [state (-> (input/empty-input)
                      (input/paste-text "draft"))]
        (expect (= {:action :show-sessions :state state}
                   (input/handle-key (alt-shift-special-key KeyType/ArrowUp) state)))
        (expect (= {:action :show-sessions :state state}
                   (input/handle-key (alt-shift-special-key KeyType/ArrowDown) state)))))
  (it "verbs dispatch from the C-x prefix; the freed Ctrl letters are Emacs keys"
      ;; No DIRECT verb chords anymore: every vis command lives behind the Emacs
      ;; prefix C-x (C-x C-m/r/l/f/a/v/d/s/h), and the Ctrl letters they vacated
      ;; are Emacs editing keys.
      (let [state
            (-> (input/empty-input)
                (input/paste-text "draft"))

            armed
            (:state (input/handle-key (ctrl-key (Character. \x)) state))]

        ;; C-x arms the prefix; the next key runs the verb (with or without Ctrl).
        (expect (= :cx (:prefix armed)))
        (expect (= :cycle-model (:action (input/handle-key (char-key (Character. \m)) armed))))
        (expect (= :cycle-reasoning (:action (input/handle-key (char-key (Character. \r)) armed))))
        (expect (= :cycle-verbosity (:action (input/handle-key (char-key (Character. \l)) armed))))
        (expect (= :search-open (:action (input/handle-key (char-key (Character. \f)) armed))))
        (expect (= :pick-file (:action (input/handle-key (char-key (Character. \a)) armed))))
        (expect (= :toggle-voice-recording
                   (:action (input/handle-key (char-key (Character. \v)) armed))))
        (expect (= :open-dirs (:action (input/handle-key (char-key (Character. \d)) armed))))
        (expect (= :open-resources (:action (input/handle-key (char-key (Character. \s)) armed))))
        (expect (= :toggle-help (:action (input/handle-key (char-key (Character. \h)) armed))))
        ;; WITHOUT the prefix the same Ctrl letters are Emacs keys / abort — NOT verbs:
        (expect (= :continue (:action (input/handle-key (ctrl-key (Character. \h)) state))))      ; C-h inert (help is C-x C-h)
        (expect (= :recenter (:action (input/handle-key (ctrl-key (Character. \l)) state))))      ; C-l recenter
        (expect (= :continue (:action (input/handle-key (ctrl-key (Character. \t)) state))))      ; C-t transpose (editing)
        (expect (= :continue (:action (input/handle-key (ctrl-key (Character. \f)) state))))      ; C-f forward-char
        (expect (= :clear-input (:action (input/handle-key (ctrl-key (Character. \g)) state)))))) ; C-g abort
  (it "old Alt app-chords are dead (Option is eaten by macOS terminals)"
      ;; The retired Alt VERB chords (settings/model/resources) fall through to
      ;; :continue — those verbs live on the C-x prefix + palette now. (The Emacs
      ;; NAVIGATION Meta keys M-> / M-< / M-v ARE live, like M-x — covered above.)
      (let [state (-> (input/empty-input)
                      (input/paste-text "draft"))]
        (expect (= {:action :continue :state state}
                   (input/handle-key (alt-key (Character. \s)) state)))
        (expect (= {:action :continue :state state}
                   (input/handle-key (alt-key (Character. \o)) state)))))
  (it "Ctrl+C and Escape clear non-empty input instead of exiting"
      (let [state (-> (input/empty-input)
                      (input/paste-text "draft"))]
        (expect (= {:action :clear-input :state (input/empty-input)}
                   (input/handle-key (ctrl-key (Character. \c)) state)))
        (expect (= {:action :clear-input :state (input/empty-input)}
                   (input/handle-key (special-key KeyType/Escape) state)))))
  (it "Ctrl+C and Escape keep their existing behavior when input is empty"
      (let [state (input/empty-input)]
        (expect (= {:action :quit :state state}
                   (input/handle-key (ctrl-key (Character. \c)) state)))
        (expect (= {:action :cancel :state state}
                   (input/handle-key (special-key KeyType/Escape) state)))))
  (it "C-x p (and C-x C-p) open the palette; C-x n is new-session; bare Ctrl+P/N are line motion"
      (let [state (-> (input/empty-input)
                      (input/paste-text "keep"))]
        ;; C-x C-p — the primary trigger: C-x arms the prefix, then Ctrl+P fires the
        ;; palette (the second key is consumed by the prefix, not line motion).
        (let [armed (input/handle-key (ctrl-key (Character. \x)) state)]
          (expect (= :continue (:action armed)))
          (expect (= :cx (:prefix (:state armed))))
          (expect (= :show-palette
                     (:action (input/handle-key (ctrl-key (Character. \p)) (:state armed)))))
          ;; C-x p — the ADVERTISED trigger: a PLAIN p after the prefix.
          (expect (= :show-palette
                     (:action (input/handle-key (char-key (Character. \p)) (:state armed)))))
          ;; C-x n — start a fresh session.
          (expect (= :new-session
                     (:action (input/handle-key (char-key (Character. \n)) (:state armed))))))
        ;; M-x — the Emacs command-launcher alias, in the Meta keyspace.
        (expect (= {:action :show-palette :state state}
                   (input/handle-key (alt-key (Character. \x)) state)))
        ;; Plain x types; Alt+x does NOT touch the Ctrl editing keys.
        (expect (= :continue (:action (input/handle-key (char-key (Character. \x)) state))))
        ;; BARE Ctrl+P prev-line, Ctrl+N next-line (no prefix armed) — on a
        ;; single-line draft they are a no-op on the TEXT (action :continue, draft
        ;; intact), NOT an app verb and NOT the palette.
        (let [p (input/handle-key (ctrl-key (Character. \p)) state)
              n (input/handle-key (ctrl-key (Character. \n)) state)]

          (expect (= :continue (:action p)))
          (expect (= :continue (:action n)))
          (expect (= (:lines state) (:lines (:state p))))
          (expect (= (:lines state) (:lines (:state n)))))))
  (it "C-x C-h opens help; a LONE Ctrl+H is inert (help moved to the C-x prefix)"
      ;; ctrl-h-pattern still decodes a lone 0x08 to Ctrl+H (physical Backspace
      ;; sends 0x7f, so it stays Backspace) — that decode is what lets the SECOND
      ;; key of C-x C-h arrive as Ctrl+H. Help is no longer a direct chord, so a
      ;; lone Ctrl+H does nothing; only C-x then Ctrl+H toggles help.
      (let [ctrl-h-pattern
            @#'input/ctrl-h-pattern

            m
            (pat-match ctrl-h-pattern [(Character. (char 0x08))])

            ks
            (.fullMatch m)]

        (expect (= \h (.getCharacter ks)))
        (expect (true? (.isCtrlDown ks))))
      ;; lone Ctrl+H is inert now
      (expect (= :continue
                 (:action (input/handle-key (ctrl-key (Character. \h)) (input/empty-input)))))
      ;; C-x then Ctrl+H fires help
      (let [armed (:state (input/handle-key (ctrl-key (Character. \x)) (input/empty-input)))]
        (expect (= :toggle-help (:action (input/handle-key (ctrl-key (Character. \h)) armed))))))
  (it "@ inserts a literal char; it does not open the file picker"
      ;; The file picker is a palette verb (Ctrl+P → Attach File) now; `@` just
      ;; types itself.
      (let [state (input/empty-input)]
        (expect (= {:action :continue
                    :state (-> (input/empty-input)
                               (input/paste-text "@"))}
                   (input/handle-key (char-key (Character. \@)) state)))))
  (it "the old setting-cycle chords are Emacs keys now, not direct verbs"
      ;; Reasoning/length/model moved to C-x r/v/m. The Ctrl letters they vacated
      ;; are Emacs keys: C-l recenter, C-t transpose (editing), C-r unbound.
      (let [state (-> (input/empty-input)
                      (input/paste-text "keep"))]
        (expect (= :recenter (:action (input/handle-key (ctrl-key (Character. \l)) state))))
        (expect (= :continue (:action (input/handle-key (ctrl-key (Character. \t)) state))))
        (expect (not= :cycle-reasoning
                      (:action (input/handle-key (ctrl-key (Character. \r)) state))))))
  (it
    "Tab/Shift+Tab and C-x arrows switch workspaces; bare Ctrl+arrows are word motion"
    (let [state (-> (input/empty-input)
                    (input/paste-text "keep"))]
      ;; Plain Tab cycles forward, Shift+Tab (both decodings) backward.
      (expect (= {:action :select-tab-index :workspace-index :next :state state}
                 (input/handle-key (special-key KeyType/Tab) state)))
      (expect (= {:action :select-tab-index :workspace-index :prev :state state}
                 (input/handle-key (special-key KeyType/ReverseTab) state)))
      (expect (= {:action :select-tab-index :workspace-index :prev :state state}
                 (input/handle-key (KeyStroke. KeyType/Tab false false true) state)))
      ;; C-x ←/→ are the Emacs prev/next-buffer way to switch workspaces.
      (let [armed (:state (input/handle-key (ctrl-key (Character. \x)) state))]
        (expect (= {:action :select-tab-index :workspace-index :prev :state state}
                   (input/handle-key (special-key KeyType/ArrowLeft) armed)))
        (expect (= {:action :select-tab-index :workspace-index :next :state state}
                   (input/handle-key (special-key KeyType/ArrowRight) armed))))
      ;; C-x <digit> jumps straight to workspace N (C-x 1 … C-x 9) — the SAME
      ;; :select-tab-index the M-N chords fire (1-based on screen, 0-based
      ;; index). C-x 0 has no target → the prefix aborts.
      (let [armed (:state (input/handle-key (ctrl-key (Character. \x)) state))]
        (expect (= {:action :select-tab-index :workspace-index 0 :state state}
                   (input/handle-key (char-key (Character. \1)) armed)))
        (expect (= {:action :select-tab-index :workspace-index 8 :state state}
                   (input/handle-key (char-key (Character. \9)) armed)))
        (expect (= :continue (:action (input/handle-key (char-key (Character. \0)) armed)))))
      ;; BARE Ctrl+arrow is word motion now (NOT a workspace switch).
      (expect (= :continue (:action (input/handle-key (ctrl-special-key KeyType/ArrowLeft) state))))
      (expect (= :continue
                 (:action (input/handle-key (ctrl-special-key KeyType/ArrowRight) state))))
      (expect (= {:action :continue :state state}
                 (input/handle-key (ctrl-key (Character. \1)) state)))
      (expect (= {:action :continue :state state}
                 (input/handle-key (ctrl-key (Character. \9)) state)))
      ;; Alt+<digit> (M-1 … M-9) jumps straight to workspace N (1-based
      ;; on screen, 0-based index). Alt+0 has no target → ignored.
      (expect (= {:action :select-tab-index :workspace-index 0 :state state}
                 (input/handle-key (alt-key (Character. \1)) state)))
      (expect (= {:action :select-tab-index :workspace-index 2 :state state}
                 (input/handle-key (alt-key (Character. \3)) state)))
      (expect (= {:action :select-tab-index :workspace-index 8 :state state}
                 (input/handle-key (alt-key (Character. \9)) state)))
      (expect (= {:action :continue :state state}
                 (input/handle-key (alt-key (Character. \0)) state)))))
  (it "Alt+Left/Right move by whitespace-delimited words"
      ;; Word motion rides Alt+arrow (the xterm modified-arrow sequence, which
      ;; some terminals deliver).
      (let [state (-> (input/empty-input)
                      (input/paste-text "hello   world"))]
        (expect (= {:action :continue :state (assoc state :ccol 8)}
                   (input/handle-key (alt-special-key KeyType/ArrowLeft) state)))
        (expect (= {:action :continue :state (assoc state :ccol 5)}
                   (input/handle-key (alt-special-key KeyType/ArrowRight) (assoc state :ccol 0))))))
  (it "M-b / M-f (Alt+b / Alt+f) are word motion — what macOS terminals send for Option+arrow"
      ;; Ghostty / Terminal.app / iTerm2 default keybinds translate Option+←/→
      ;; into the readline bytes `ESC b` / `ESC f`, NOT the xterm modified-arrow
      ;; sequence. Lanterna decodes those as Alt+b / Alt+f Character keystrokes,
      ;; so these chords ARE Option+arrow on a stock macOS terminal.
      (let [state (-> (input/empty-input)
                      (input/paste-text "hello   world"))]
        (expect (= {:action :continue :state (assoc state :ccol 8)}
                   (input/handle-key (alt-key \b) state)))
        (expect (= {:action :continue :state (assoc state :ccol 5)}
                   (input/handle-key (alt-key \f) (assoc state :ccol 0))))))
  (it "Home/End and Ctrl+A/E move to current line bounds"
      (let [state (-> (input/empty-input)
                      (input/paste-text "first\nsecond"))]
        (expect (= {:action :continue :state (assoc state :ccol 0)}
                   (input/handle-key (special-key KeyType/Home) state)))
        (expect (= {:action :continue :state state}
                   (input/handle-key (special-key KeyType/End) (assoc state :ccol 0))))
        (expect (= {:action :continue :state (assoc state :ccol 0)}
                   (input/handle-key (ctrl-key (Character. \a)) state)))
        (expect (= {:action :continue :state state}
                   (input/handle-key (ctrl-key (Character. \e)) (assoc state :ccol 0))))))
  (it "Alt+Backspace and Ctrl+Backspace delete a word back; Ctrl+U deletes to line start"
      (let [state
            (-> (input/empty-input)
                (input/paste-text "hello world"))

            word-gone
            (-> state
                (assoc-in [:lines 0] "hello ")
                (assoc :ccol 6))

            line-gone
            (-> state
                (assoc-in [:lines 0] "")
                (assoc :ccol 0))]

        (expect (= {:action :continue :state word-gone}
                   (input/handle-key (alt-special-key KeyType/Backspace) state)))
        ;; Ctrl+Backspace now deletes a word back too (same as Alt+Backspace) —
        ;; help moved to the C-x prefix (C-x C-h), freeing this chord.
        (expect (= {:action :continue :state word-gone}
                   (input/handle-key (ctrl-special-key KeyType/Backspace) state)))
        ;; delete-to-line-start stays on Ctrl+U.
        (expect (= {:action :continue :state line-gone}
                   (input/handle-key (ctrl-key (Character. \u)) state))))))

(defn- custom-decoder
  ^InputDecoder [s]
  (let [decoder (InputDecoder. (StringReader. s))]
    (.addProfile decoder
                 (reify
                   KeyDecodingProfile
                     (getPatterns [_] [input/escape-pattern input/alt-enter-pattern
                                       input/alt-backspace-pattern input/modified-arrow-pattern
                                       input/paste-start-pattern input/paste-end-pattern
                                       input/sgr-mouse-pattern])))
    decoder))

(defdescribe escape-pattern-test
             (it "bare ESC decodes immediately instead of poisoning the next key"
                 (let [decoder
                       (custom-decoder (str (char 0x1b)))

                       key
                       (.getNextCharacter decoder false)]

                   (expect (= KeyType/Escape (.getKeyType key)))
                   (expect (not (.isAltDown key)))))
             (it "ESC+Enter still decodes as Alt+Enter when both bytes are queued"
                 (let [decoder
                       (custom-decoder (str (char 0x1b) \newline))

                       key
                       (.getNextCharacter decoder false)]

                   (expect (= KeyType/Enter (.getKeyType key)))
                   (expect (.isAltDown key)))))

(defdescribe modified-arrow-pattern-test
             (it "decodes xterm modified arrows so Alt+arrow works on macOS terminals"
                 (let [alt-up
                       (let [decoder (custom-decoder (str (char 0x1b) "[1;3A"))]
                         (.getNextCharacter decoder false))

                       shift-down
                       (let [decoder (custom-decoder (str (char 0x1b) "[1;2B"))]
                         (.getNextCharacter decoder false))]

                   (expect (= KeyType/ArrowUp (.getKeyType alt-up)))
                   (expect (.isAltDown alt-up))
                   (expect (not (.isShiftDown alt-up)))
                   (expect (= KeyType/ArrowDown (.getKeyType shift-down)))
                   (expect (.isShiftDown shift-down))
                   (expect (not (.isAltDown shift-down))))))

(defdescribe alt-backspace-pattern-test
             (it "ESC+DEL and ESC+Ctrl-H decode as Alt+Backspace"
                 (doseq [ch [(Character. (char 0x7f)) (Character. (char 0x08))]]
                   (let [match (pat-match input/alt-backspace-pattern [(Character. (char 0x1b)) ch])
                         key (.-fullMatch match)]

                     (expect (= KeyType/Backspace (.getKeyType key)))
                     (expect (.isAltDown key))
                     (expect (not (.isCtrlDown key)))))))

(defdescribe bracketed-paste-helpers-test
             (it "paste-start? is true ONLY for a KeyStroke carrying PASTE_START_CHAR"
                 (expect (input/paste-start? (char-key (Character. input/PASTE_START_CHAR))))
                 (expect (not (input/paste-start? (char-key (Character. \a)))))
                 (expect (not (input/paste-start? (KeyStroke. KeyType/Enter false false))))
                 (expect (not (input/paste-start? (char-key (Character. input/PASTE_END_CHAR))))))
             (it "paste-end? is true ONLY for a KeyStroke carrying PASTE_END_CHAR"
                 (expect (input/paste-end? (char-key (Character. input/PASTE_END_CHAR))))
                 (expect (not (input/paste-end? (char-key (Character. input/PASTE_START_CHAR)))))
                 (expect (not (input/paste-end? (KeyStroke. KeyType/Tab false false))))))

(defdescribe
  keystroke->paste-char-test
  (it "Character key returns its char"
      (expect (= "x" (input/keystroke->paste-char (char-key (Character. \x))))))
  (it "Enter becomes a newline char so multi-line pastes survive"
      (expect (= "\n" (input/keystroke->paste-char (KeyStroke. KeyType/Enter false false)))))
  (it "Tab becomes \\t - preserves indentation in pasted code"
      (expect (= "\t" (input/keystroke->paste-char (KeyStroke. KeyType/Tab false false)))))
  (it "non-text keys (function keys, arrows) drop to nil"
      (expect (nil? (input/keystroke->paste-char (KeyStroke. KeyType/F2 false false))))
      (expect (nil? (input/keystroke->paste-char (KeyStroke. KeyType/ArrowLeft false false))))
      (expect (nil? (input/keystroke->paste-char (KeyStroke. KeyType/Escape false false))))))

(defdescribe
  placeholder-format-test
  (it "multi-line paste shows lines AND bytes"
      (let [text
            (apply str (repeat 42 "line of text\n"))

            token
            (input/format-paste-placeholder {:id 1 :content text})]

        (expect (str/includes? token "#1"))
        (expect (str/includes? token "43 lines"))
        (expect (re-find #"\d+(?:\.\d+)?[BKM]" token))))
  (it "single-line paste pluralises correctly + reports byte size"
      (let [token (input/format-paste-placeholder {:id 7 :content "abcdef"})]
        (expect (= "[Pasted #7: 1 line, 6B]" token))))
  (it "sub-1024 chars render as <N>B; 1024+ render as KB"
      (let [small
            (input/format-paste-placeholder {:id 1 :content "x"})

            big
            (input/format-paste-placeholder {:id 2 :content (apply str (repeat 2048 "a"))})]

        (expect (str/includes? small "1B"))
        (expect (str/includes? big "2.0KB"))))
  (it "an image entry is labelled `Image` with filename + dims + size"
      (let [token (input/format-paste-placeholder
                    {:id 3
                     :content "/tmp/shot.png"
                     :image {:filename "shot.png" :width 1200 :height 800 :size-label "245KB"}})]
        (expect (= "[Image #3: shot.png 1200×800, 245KB]" token))))
  (it "an image with unknown dims omits the WxH clause"
      (let [token (input/format-paste-placeholder
                    {:id 4 :content "/tmp/x.gif" :image {:filename "x.gif" :size-label "12KB"}})]
        (expect (= "[Image #4: x.gif, 12KB]" token)))))

(defdescribe placeholder-threshold-test
             (it "short single-line text bypasses the placeholder UX"
                 (expect (false? (input/use-placeholder? "hello world"))))
             (it "any newline triggers a placeholder regardless of length"
                 (expect (true? (input/use-placeholder? "a\nb"))))
             (it "long single-line text triggers a placeholder by length"
                 (expect (true? (input/use-placeholder? (apply str (repeat 200 "x")))))))

(defdescribe placeholder-expand-test
             (it "substitutes every known token; passes unknown ones through"
                 (let [pastes-map
                       {1 {:id 1 :content "FIRST"} 2 {:id 2 :content "SECOND"}}

                       input-text
                       "prefix [Pasted #1: 5 lines, 1.0KB] middle [Pasted #2: 1 line, 6B] suffix"]

                   (expect (= "prefix FIRST middle SECOND suffix"
                              (input/expand-paste-placeholders input-text pastes-map)))))
             (it "unknown id passes through verbatim"
                 (let [unchanged "hello [Pasted #99: 1 line, 1B] world"]
                   (expect (= unchanged (input/expand-paste-placeholders unchanged {})))))
             (it "replacement preserves regex-special chars in the content"
                 (let [pastes
                       {1 {:id 1 :content "$1 \\n raw"}}

                       out
                       (input/expand-paste-placeholders "x [Pasted #1: 1 line, 9B] y" pastes)]

                   (expect (= "x $1 \\n raw y" out)))))

(defdescribe
  paste-collapse-test
  (it "short payload comes back whole (head+tail covers every line)"
      (let [content "line-1\nline-2\nline-3"]
        (expect (= ["line-1" "line-2" "line-3"] (input/paste-content-preview content)))))
  (it "long payload keeps head + tail with a `more lines` marker between"
      (let [content
            (str/join "\n" (map #(str "line-" %) (range 1 30)))

            preview
            (input/paste-content-preview content)]

        (expect (= (take input/PASTE_PREVIEW_HEAD_LINES preview)
                   (map #(str "line-" %) (range 1 (inc input/PASTE_PREVIEW_HEAD_LINES)))))
        (expect (= "⋯ 20 more lines ⋯" (nth preview input/PASTE_PREVIEW_HEAD_LINES)))
        (expect (= (take-last input/PASTE_PREVIEW_TAIL_LINES preview)
                   (map #(str "line-" %) (range 27 30))))))
  (it "a single very long line is itself middle-elided"
      (let [line
            (apply str (repeat 500 "x"))

            [out]
            (input/paste-content-preview line)]

        (expect (< (count out) 500))
        (expect (str/includes? out " … "))))
  (it "collapse folds the token into a `vis-paste` fence carrying token + full payload"
      (let [content
            (str/join "\n" (map #(str "line-" %) (range 1 30)))

            pastes
            {1 {:id 1 :content content}}

            out
            (input/collapse-paste-placeholders "before [Pasted #1: 29 lines, 1KB] after" pastes)]

        ;; Token becomes the summary FIRST body line of a `vis-paste` fence,
        ;; the WHOLE payload verbatim underneath (no head+tail truncation).
        ;; The summary is RECOMPUTED from the payload, so its byte count is
        ;; canonical (222B) regardless of the stale count in the input token.
        (expect (str/includes? out "````vis-paste\n[Pasted #1: 29 lines, 222B]\n"))
        (expect (str/includes? out "line-1\n"))
        (expect (str/includes? out "line-29"))
        (expect (not (str/includes? out "more lines")))
        (expect (str/ends-with? out "````\n after"))))
  (it "collapse passes unknown tokens through unchanged"
      (let [unchanged "hello [Pasted #99: 1 line, 1B] world"]
        (expect (= unchanged (input/collapse-paste-placeholders unchanged {})))))
  (it "an image token collapses into a `vis-image` fence carrying path + metadata"
      (let [pastes
            {1 {:id 1
                :content "/tmp/shot.png"
                :image {:path "/tmp/shot.png"
                        :mime "image/png"
                        :filename "shot.png"
                        :width 1200
                        :height 800
                        :size-label "245KB"}}}

            out
            (input/collapse-paste-placeholders "see [Image #1: shot.png 1200×800, 245KB]" pastes)]

        (expect (str/includes? out "````vis-image\n[Image #1: shot.png 1200×800, 245KB]\n"))
        (expect (str/includes? out "/tmp/shot.png\nimage/png\n1200x800\n245KB\n"))
        (expect (str/ends-with? out "````\n"))))
  (it "an image token expands to just the file PATH for the engine to attach"
      (let [pastes {1 {:id 1
                       :content "/tmp/shot.png"
                       :image {:path "/tmp/shot.png" :filename "shot.png"}}}]
        (expect (= "see /tmp/shot.png"
                   (input/expand-paste-placeholders "see [Image #1: shot.png 1200×800, 245KB]"
                                                    pastes))))))

(defdescribe placeholder-smart-delete-test
             (it "placeholder-id-before-cursor returns the id when cursor sits right after `]`"
                 (let [token
                       (input/format-paste-placeholder {:id 4 :content "line one\nline two"})

                       state
                       (-> (input/empty-input)
                           (input/paste-text token))]

                   (expect (= 4 (input/placeholder-id-before-cursor state)))))
             (it "returns nil when the cursor is somewhere in the middle of a placeholder"
                 (let [token
                       (input/format-paste-placeholder {:id 4 :content "a\nb"})

                       state
                       (-> (input/empty-input)
                           (input/paste-text token)
                           (input/move-left)
                           (input/move-left))]

                   (expect (nil? (input/placeholder-id-before-cursor state)))))
             (it "returns nil when there's no placeholder on the line"
                 (let [state (-> (input/empty-input)
                                 (input/paste-text "plain text"))]
                   (expect (nil? (input/placeholder-id-before-cursor state)))))
             (it "delete-placeholder-backward removes the WHOLE token in one shot"
                 (let [token
                       (input/format-paste-placeholder {:id 4 :content "a\nb"})

                       state
                       (-> (input/empty-input)
                           (input/insert-char \h)
                           (input/insert-char \i)
                           (input/insert-char \space)
                           (input/paste-text token)
                           (input/insert-char \space)
                           (input/insert-char \!)
                           ;; Move cursor back to right after `]`.
                           (input/move-left)
                           (input/move-left))

                       deleted
                       (input/delete-placeholder-backward state)]

                   (expect (= ["hi  !"] (:lines deleted)))
                   ;; Cursor lands at the start of where the token used to be.
                   (expect (= 3 (:ccol deleted))))))

(defdescribe clipboard-copy-test
             (it "clipboard-copy! returns a boolean (smoke)"
                 ;; We can't reliably test that the SYSTEM clipboard now contains
                 ;; the bytes - that would require a `pbcopy`/`xclip` helper on
                 ;; $PATH on the CI box. The contract
                 ;; we DO guarantee is that the function returns truthy on success
                 ;; and falsy on total failure, never throws, and never blocks
                 ;; longer than the per-helper 1s cap.
                 (let [r (input/clipboard-copy! "vis-clip-test")]
                   (expect (or (true? r) (false? r) (nil? r)))))
             (it "clipboard-copy! never throws on weird input"
                 (expect (some? (input/clipboard-copy! "")))
                 (expect (some? (input/clipboard-copy! "line1\nline2\nline3")))
                 (expect (some? (input/clipboard-copy! "tab\there\u00e9 \u4e2d\u6587")))))

(defn- mk-chars
  [^String s]
  ;; Helper: java.util.List<Character> from a String, the shape
  ;; Lanterna feeds CharacterPattern.match. Renamed from `chars`
  ;; to dodge the `clojure.core/chars` shadow warning.
  (let [v (java.util.ArrayList.)]
    (doseq [c s]
      (.add v (Character. ^char c)))
    v))

(defn- sgr [button col row final-ch] (str "\u001B[<" button ";" col ";" row final-ch))

(defdescribe
  sgr-mouse-pattern-test
  (it "NOT_YET on empty / partial prefixes"
      (let [m (pat-match input/sgr-mouse-pattern (mk-chars ""))]
        (expect (= com.googlecode.lanterna.input.CharacterPattern$Matching/NOT_YET m)))
      (let [m (pat-match input/sgr-mouse-pattern (mk-chars "\u001B"))]
        (expect (= com.googlecode.lanterna.input.CharacterPattern$Matching/NOT_YET m)))
      (let [m (pat-match input/sgr-mouse-pattern (mk-chars "\u001B[<0;1"))]
        (expect (= com.googlecode.lanterna.input.CharacterPattern$Matching/NOT_YET m))))
  (it "nil on shape violation (third byte is not '<')"
      ;; Plain CSI sequences (no '<') belong to other patterns -
      ;; sgr-mouse-pattern must reject them so Lanterna keeps trying.
      (expect (nil? (pat-match input/sgr-mouse-pattern (mk-chars "\u001B[A")))))
  (it "left-button press at column 50, row 1 decodes correctly"
      (let [m
            (pat-match input/sgr-mouse-pattern (mk-chars (sgr 0 50 1 \M)))

            ks
            (.-fullMatch ^com.googlecode.lanterna.input.CharacterPattern$Matching m)]

        (expect (instance? com.googlecode.lanterna.input.MouseAction ks))
        (let [^com.googlecode.lanterna.input.MouseAction ma
              ks

              pos
              (.getPosition ma)]

          (expect (= 49 (.getColumn pos))) ; SGR is 1-based, ours is 0-based
          (expect (= 0 (.getRow pos)))
          (expect (= com.googlecode.lanterna.input.MouseActionType/CLICK_DOWN
                     (.getActionType ma))))))
  (it "left-button release uses lowercase 'm' as terminator"
      (let [m
            (pat-match input/sgr-mouse-pattern (mk-chars (sgr 0 112 1 \m)))

            ma
            ^com.googlecode.lanterna.input.MouseAction
            (.-fullMatch ^com.googlecode.lanterna.input.CharacterPattern$Matching m)]

        (expect (= com.googlecode.lanterna.input.MouseActionType/CLICK_RELEASE (.getActionType ma)))
        ;; Critically: column 112 (the copy-id range) decodes to 111,
        ;; NOT to the broken 65500 the legacy parser produced.
        (expect (= 111 (.getColumn (.getPosition ma))))))
  (it "wheel-up button (64) maps to SCROLL_UP"
      (let [m
            (pat-match input/sgr-mouse-pattern (mk-chars (sgr 64 10 5 \M)))

            ma
            ^com.googlecode.lanterna.input.MouseAction
            (.-fullMatch ^com.googlecode.lanterna.input.CharacterPattern$Matching m)]

        (expect (= com.googlecode.lanterna.input.MouseActionType/SCROLL_UP (.getActionType ma)))))
  (it "wheel-down button (65) maps to SCROLL_DOWN without reflection warnings"
      (let [m
            (pat-match input/sgr-mouse-pattern (mk-chars (sgr 65 10 5 \M)))

            ma
            ^com.googlecode.lanterna.input.MouseAction
            (.-fullMatch ^com.googlecode.lanterna.input.CharacterPattern$Matching m)]

        (expect (= com.googlecode.lanterna.input.MouseActionType/SCROLL_DOWN (.getActionType ma))))
      (let [err (java.io.StringWriter.)]
        (binding [*warn-on-reflection* true
                  *err* err]

          (require 'com.blockether.vis.ext.channel-tui.input :reload))
        (expect (not (str/includes? (str err) "input.clj")))))
  (it "drag with button held (32) maps to DRAG"
      (let [m
            (pat-match input/sgr-mouse-pattern (mk-chars (sgr 32 10 5 \M)))

            ma
            ^com.googlecode.lanterna.input.MouseAction
            (.-fullMatch ^com.googlecode.lanterna.input.CharacterPattern$Matching m)]

        (expect (= com.googlecode.lanterna.input.MouseActionType/DRAG (.getActionType ma)))))
  (it "plain motion (35: bits=3 + drag bit) maps to MOVE"
      (let [m
            (pat-match input/sgr-mouse-pattern (mk-chars (sgr 35 10 5 \M)))

            ma
            ^com.googlecode.lanterna.input.MouseAction
            (.-fullMatch ^com.googlecode.lanterna.input.CharacterPattern$Matching m)]

        (expect (= com.googlecode.lanterna.input.MouseActionType/MOVE (.getActionType ma)))))
  (it "wide-column press at column 200 decodes faithfully"
      ;; The whole point of switching to SGR: legacy X10 corrupted
      ;; this case to mx=65500. SGR is ASCII text so col 200 stays 200.
      (let [m
            (pat-match input/sgr-mouse-pattern (mk-chars (sgr 0 200 50 \M)))

            ma
            ^com.googlecode.lanterna.input.MouseAction
            (.-fullMatch ^com.googlecode.lanterna.input.CharacterPattern$Matching m)]

        (expect (= 199 (.getColumn (.getPosition ma))))
        (expect (= 49 (.getRow (.getPosition ma)))))))

(defdescribe
  file-mention-expand-test
  (it "replaces inline @mentions via the file-expander helper"
      (with-redefs [com.blockether.vis.ext.channel-tui.input/file-mention->prompt-block
                    (fn [path]
                      (str "<FILE:" path ">"))]
        (expect (= "see <FILE:src/foo.clj> please"
                   (input/expand-file-mentions "see @src/foo.clj please")))))
  (it "expands local @mentions to a tool-agnostic attached-file directive"
      ;; Per `file-mention->prompt-block` contract: emit a short
      ;; directive only (no `(cat ...)` / `(v/preview ...)`
      ;; boilerplate). The model picks the right tool itself.
      (let [file (java.io.File/createTempFile "vis-input-large" ".clj")]
        (spit file (str/join "\n" (repeat 121 "x")))
        (try (with-redefs [com.blockether.vis.ext.channel-tui.input/resolve-local-file (fn [_path]
                                                                                         file)]
               (let [expanded (input/expand-file-mentions "inspect @src/foo.clj now")]
                 (expect (str/includes? expanded "[Attached File: src/foo.clj]"))
                 (expect (str/includes? expanded "Read it"))
                 (expect (not (str/includes? expanded "v/preview")))))
             (finally (.delete file)))))
  (it "supports quoted @mentions for paths with spaces"
      (with-redefs [com.blockether.vis.ext.channel-tui.input/file-mention->prompt-block
                    (fn [path]
                      (str "<FILE:" path ">"))]
        (expect (= "see <FILE:docs/My File.md> please"
                   (input/expand-file-mentions "see @\"docs/My File.md\" please")))))
  (it "leaves non-matching @text alone"
      (expect (= "email me at a@b.com" (input/expand-file-mentions "email me at a@b.com"))))
  (it "formats simple visible mention tokens with a leading @"
      (expect (= "@src/foo.clj" (input/format-file-mention "src/foo.clj"))))
  (it "quotes visible mention tokens when the path contains whitespace"
      (expect (= "@\"docs/My File.md\"" (input/format-file-mention "docs/My File.md")))))

(defdescribe
  jump-to-bottom-keymap-test
  ;; The `↓ latest` jump-to-bottom chip's keyboard twin: Ctrl+End re-arms FOLLOW
  ;; (the `:recenter` action the screen owns), while a PLAIN End must fall through
  ;; (`:continue`) so the input box keeps its end-of-line editing.
  (let [st (input/empty-input)]
    (it "Ctrl+End jumps the conversation to the newest content (:recenter)"
        (expect (= :recenter (:action (input/handle-key (KeyStroke. KeyType/End true false) st)))))
    (it "plain End stays end-of-line editing (:continue, not :recenter)"
        (expect (= :continue
                   (:action (input/handle-key (KeyStroke. KeyType/End false false) st)))))))

(defdescribe
  emacs-navigation-keymap-test
  ;; Faithful Emacs buffer navigation over the transcript: M-> end-of-buffer,
  ;; M-< beginning-of-buffer, C-v scroll forward a screen, M-v scroll backward.
  ;; KeyStroke(char, ctrlDown, altDown).
  (let [st
        (input/empty-input)

        act
        (fn [ch ctrl? alt?]
          (:action (input/handle-key
                     (KeyStroke. (Character/valueOf ch) (boolean ctrl?) (boolean alt?))
                     st)))]

    (it "M-> jumps to the bottom (end-of-buffer → :recenter)"
        (expect (= :recenter (act \> false true))))
    (it "M-< jumps to the top (beginning-of-buffer → :scroll-to-top)"
        (expect (= :scroll-to-top (act \< false true))))
    (it "C-v scrolls a screen forward (:scroll-down)" (expect (= :scroll-down (act \v true false))))
    (it "M-v scrolls a screen backward (:scroll-up)" (expect (= :scroll-up (act \v false true))))))

(defdescribe
  shell-helper-deadline-test
  ;; Regression: the clipboard-helper drain loop read stdout to EOF with NO
  ;; bound — the `waitFor` "cap" only ran AFTER EOF — so a helper that never
  ;; exits (osascript stuck on a macOS automation prompt, xclip without an X
  ;; server) blocked the TUI input thread FOREVER: one ⌘V of an image froze
  ;; the whole vis session. `run-helper-process!` now kills the child at a
  ;; hard deadline and closes its stdin so stdin-readers see EOF.
  (it "a never-exiting helper is killed at the deadline instead of hanging"
      (let [t0
            (System/currentTimeMillis)

            r
            (deref (future (#'input/run-shell-helper-bytes! ["sleep" "30"] 500)) 5000 ::still-hung)

            elapsed
            (- (System/currentTimeMillis) t0)]

        (expect (map? r) "still blocked after 5s — the deadline never fired")
        (expect (false? (:success? r)) "a killed helper must not report success")
        (expect (< elapsed 4000))))
  (it "a helper that reads stdin sees EOF instead of waiting forever"
      ;; `cat` blocks on its inherited stdin pipe unless the parent closes it.
      (let [r (deref (future (#'input/run-shell-helper-bytes! ["cat"] 2000)) 5000 ::still-hung)]
        (expect (map? r) "cat never saw stdin EOF")
        (expect (true? (:success? r)))))
  (it "a fast helper's stdout bytes ride through intact"
      (let [r (#'input/run-shell-helper-bytes! ["printf" "abc"] 2000)]
        (expect (true? (:success? r)))
        (expect (= "abc" (String. ^bytes (:bytes r) "UTF-8")))))
  (it "run-shell-helper! still feeds stdin and captures stdout"
      (let [r (#'input/run-shell-helper! ["cat"] (.getBytes "hello" "UTF-8"))]
        (expect (true? (:success? r)))
        (expect (= "hello" (:stdout r)))))
  (it "a helper that exits but leaves a grandchild holding stdout open returns at the deadline"
      ;; `printf abc; sleep 600 & exit 0`: the helper exits 0 immediately but
      ;; the orphaned `sleep` inherits the stdout pipe write-end, so EOF never
      ;; arrives — and the watchdog has nothing to kill (the helper is already
      ;; dead). This is the xclip-daemonize shape. The deadline-bounded drain
      ;; must return anyway, with the bytes the helper DID write.
      (let [t0
            (System/currentTimeMillis)

            r
            (deref (future (#'input/run-shell-helper-bytes!
                            ["bash" "-c" "printf abc; sleep 600 & exit 0"]
                            700))
                   5000
                   ::still-hung)

            elapsed
            (- (System/currentTimeMillis) t0)]

        (expect (map? r) "grandchild pipe-hold must not hang the caller")
        (expect (< elapsed 4000))
        (expect (true? (:success? r)) "the helper itself exited 0")
        (expect (= "abc" (String. ^bytes (:bytes r) "UTF-8"))
                "bytes written before exit still ride through"))))
