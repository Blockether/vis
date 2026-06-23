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
  (:require
   [clojure.string :as str]
   [com.blockether.vis.ext.channel-tui.input :as input]
   [lazytest.core :refer [defdescribe expect it]])
  (:import [com.googlecode.lanterna.input InputDecoder KeyDecodingProfile KeyStroke KeyType]
           [java.io StringReader]))

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

(defn- char-key [^Character ch]
  (KeyStroke. ch false false))

(defn- ctrl-key [^Character ch]
  (KeyStroke. ch true false))

(defn- special-key [ktype]
  (KeyStroke. ktype false false))

(defn- alt-key [^Character ch]
  (KeyStroke. ch false true))

(defn- alt-special-key [ktype]
  (KeyStroke. ktype false true))

(defn- ctrl-special-key [ktype]
  (KeyStroke. ktype true false))

(defn- alt-shift-special-key [ktype]
  (KeyStroke. ktype false true true))

(defdescribe handle-key-test
  (it "Arrow Up/Down cycle input history"
    (let [state (input/empty-input)]
      (expect (= :history-up
                (:action (input/handle-key (special-key KeyType/ArrowUp) state))))
      (expect (= :history-down
                (:action (input/handle-key (special-key KeyType/ArrowDown) state))))))

  (it "Page Up/Down still scroll the transcript"
    (let [state (input/empty-input)]
      (expect (= :scroll-up
                (:action (input/handle-key (special-key KeyType/PageUp) state))))
      (expect (= :scroll-down
                (:action (input/handle-key (special-key KeyType/PageDown) state))))))

  (it "Alt+S and Alt+Shift+Up/Down open the session switcher"
    ;; App verbs moved off the Ctrl namespace onto Meta/Alt (aa1f7cba); the
    ;; Ctrl keys are reserved for Emacs text-editing motion. Ctrl+G is now the
    ;; Emacs keyboard-quit (→ :cancel), so the session switcher lives on Alt+S
    ;; (also F6) plus the Alt+Shift+↑/↓ arrows.
    (let [state (-> (input/empty-input)
                  (input/paste-text "draft"))]
      (expect (= {:action :show-sessions :state state}
                (input/handle-key (alt-key (Character. \s)) state)))
      (expect (= {:action :show-sessions :state state}
                (input/handle-key (alt-key (Character. \S)) state)))
      (expect (= {:action :show-sessions :state state}
                (input/handle-key (alt-shift-special-key KeyType/ArrowUp) state)))
      (expect (= {:action :show-sessions :state state}
                (input/handle-key (alt-shift-special-key KeyType/ArrowDown) state)))))

  (it "migrated F-key actions dispatch from their Alt/Option chords"
    ;; The F-key row was retired; help / search / resources now ride Alt
    ;; chords resolved through `keymap/bindings` (Option on macOS).
    (let [state (-> (input/empty-input) (input/paste-text "draft"))]
      (expect (= {:action :toggle-help :state state}
                (input/handle-key (alt-key (Character. \h)) state)))
      (expect (= {:action :search-open :state state}
                (input/handle-key (alt-key (Character. \g)) state)))
      (expect (= {:action :open-resources :state state}
                (input/handle-key (alt-key (Character. \j)) state)))))

  (it "Ctrl+O stays unbound because BSD/macOS terminals reserve it as VDISCARD"
    (let [state (-> (input/empty-input)
                  (input/paste-text "draft"))]
      (expect (= {:action :continue :state state}
                (input/handle-key (ctrl-key (Character. \o)) state)))))

  (it "Alt+V directly toggles voice recording"
    ;; Voice recording moved onto Meta (aa1f7cba); Ctrl+B is now Emacs
    ;; backward-char (move-left), so voice lives on Alt+V.
    (let [state (-> (input/empty-input)
                  (input/paste-text "draft"))]
      (expect (= {:action :toggle-voice-recording :state state}
                (input/handle-key (alt-key (Character. \v)) state)))
      (expect (= {:action :toggle-voice-recording :state state}
                (input/handle-key (alt-key (Character. \V)) state)))))

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

  (it "Ctrl+P opens the command palette; Ctrl+N stays line motion and leaves text intact"
    (let [state (-> (input/empty-input)
                  (input/paste-text "keep"))]
      ;; Ctrl+P is the reliable, cross-platform palette opener (Alt/Option
      ;; chords don't survive stock macOS terminals).
      (expect (= {:action :show-palette :state state}
                (input/handle-key (ctrl-key (Character. \p)) state)))
      ;; Ctrl+N is emacs next-line; on a single line it's a no-op on the text.
      (expect (= {:action :continue :state state}
                (input/handle-key (ctrl-key (Character. \n)) state)))))

  (it "@ inserts a literal char; the fuzzy file picker is bound to Alt+O"
    (let [state (input/empty-input)]
      ;; `@` is an ordinary character — it types itself, it does NOT open the
      ;; picker.
      (expect (= {:action :continue
                  :state  (-> (input/empty-input) (input/paste-text "@"))}
                (input/handle-key (char-key (Character. \@)) state)))
      ;; The picker moved onto Meta (aa1f7cba); Ctrl+F is now Emacs
      ;; forward-char (move-right), so Alt+O is the file-picker trigger.
      (expect (= {:action :pick-file :state state}
                (input/handle-key (alt-key (Character. \o)) state)))))

  (it "Alt+R, Alt+L, and Alt+M cycle settings without editing the prompt"
    ;; Setting-cycle verbs moved onto Meta (aa1f7cba): Alt+R reasoning,
    ;; Alt+L verbosity (length), Alt+M model. The matching Ctrl keys are now
    ;; Emacs editing (Ctrl+T transpose-chars, Ctrl+R/Ctrl+L unbound).
    (let [state (-> (input/empty-input)
                  (input/paste-text "keep"))]
      (expect (= {:action :cycle-reasoning :state state}
                (input/handle-key (alt-key (Character. \r)) state)))
      (expect (= {:action :cycle-reasoning :state state}
                (input/handle-key (alt-key (Character. \R)) state)))
      (expect (= {:action :cycle-verbosity :state state}
                (input/handle-key (alt-key (Character. \l)) state)))
      (expect (= {:action :cycle-verbosity :state state}
                (input/handle-key (alt-key (Character. \L)) state)))
      (expect (= {:action :cycle-model :state state}
                (input/handle-key (alt-key (Character. \m)) state)))
      (expect (= {:action :cycle-model :state state}
                (input/handle-key (alt-key (Character. \M)) state)))))

  (it "Shift+Tab cycles workspaces and Ctrl+numbers are unbound"
    (let [state (-> (input/empty-input)
                  (input/paste-text "keep"))]
      (expect (= {:action :select-tab-index :workspace-index :next :state state}
                (input/handle-key (special-key KeyType/ReverseTab) state)))
      (expect (= {:action :select-tab-index :workspace-index :next :state state}
                (input/handle-key (KeyStroke. KeyType/Tab false false true) state)))
      (expect (= {:action :continue :state state}
                (input/handle-key (ctrl-key (Character. \1)) state)))
      (expect (= {:action :continue :state state}
                (input/handle-key (ctrl-key (Character. \9)) state)))))

  (it "Alt+Left/Right and Meta-b/f move by whitespace-delimited words"
    (let [state (-> (input/empty-input)
                  (input/paste-text "hello   world"))]
      (expect (= {:action :continue
                  :state  (assoc state :ccol 8)}
                (input/handle-key (alt-special-key KeyType/ArrowLeft) state)))
      (expect (= {:action :continue
                  :state  (assoc state :ccol 8)}
                (input/handle-key (alt-key (Character. \b)) state)))
      (expect (= {:action :continue
                  :state  (assoc state :ccol 5)}
                (input/handle-key (alt-special-key KeyType/ArrowRight)
                  (assoc state :ccol 0))))
      (expect (= {:action :continue
                  :state  (assoc state :ccol 13)}
                (input/handle-key (alt-key (Character. \f))
                  (assoc state :ccol 5))))))

  (it "Home/End and Ctrl+A/E move to current line bounds"
    (let [state (-> (input/empty-input)
                  (input/paste-text "first\nsecond"))]
      (expect (= {:action :continue
                  :state  (assoc state :ccol 0)}
                (input/handle-key (special-key KeyType/Home) state)))
      (expect (= {:action :continue
                  :state  state}
                (input/handle-key (special-key KeyType/End)
                  (assoc state :ccol 0))))
      (expect (= {:action :continue
                  :state  (assoc state :ccol 0)}
                (input/handle-key (ctrl-key (Character. \a)) state)))
      (expect (= {:action :continue
                  :state  state}
                (input/handle-key (ctrl-key (Character. \e))
                  (assoc state :ccol 0))))))

  (it "Alt+Backspace deletes a word and Ctrl+Backspace/Ctrl+U delete to line start"
    (let [state      (-> (input/empty-input)
                       (input/paste-text "hello world"))
          word-gone  (-> state
                       (assoc-in [:lines 0] "hello ")
                       (assoc :ccol 6))
          line-gone  (-> state
                       (assoc-in [:lines 0] "")
                       (assoc :ccol 0))]
      (expect (= {:action :continue :state word-gone}
                (input/handle-key (alt-special-key KeyType/Backspace) state)))
      ;; Ctrl+Backspace ≡ Ctrl+H → toggles the help overlay (state untouched);
      ;; delete-to-line-start moved fully to Ctrl+U (below).
      (expect (= {:action :toggle-help :state state}
                (input/handle-key (ctrl-special-key KeyType/Backspace) state)))
      (expect (= {:action :continue :state line-gone}
                (input/handle-key (ctrl-key (Character. \u)) state))))))

(defn- custom-decoder
  [s]
  (let [decoder (InputDecoder. (StringReader. s))]
    (.addProfile decoder
      (reify KeyDecodingProfile
        (getPatterns [_]
          [input/escape-pattern
           input/alt-enter-pattern
           input/alt-backspace-pattern
           input/modified-arrow-pattern
           input/paste-start-pattern
           input/paste-end-pattern
           input/sgr-mouse-pattern])))
    decoder))

(defdescribe escape-pattern-test
  (it "bare ESC decodes immediately instead of poisoning the next key"
    (let [decoder (custom-decoder (str (char 0x1b)))
          key     (.getNextCharacter decoder false)]
      (expect (= KeyType/Escape (.getKeyType key)))
      (expect (not (.isAltDown key)))))

  (it "ESC+Enter still decodes as Alt+Enter when both bytes are queued"
    (let [decoder (custom-decoder (str (char 0x1b) \newline))
          key     (.getNextCharacter decoder false)]
      (expect (= KeyType/Enter (.getKeyType key)))
      (expect (.isAltDown key)))))

(defdescribe modified-arrow-pattern-test
  (it "decodes xterm modified arrows so Alt+arrow works on macOS terminals"
    (let [alt-up (let [decoder (custom-decoder (str (char 0x1b) "[1;3A"))]
                   (.getNextCharacter decoder false))
          shift-down (let [decoder (custom-decoder (str (char 0x1b) "[1;2B"))]
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
      (let [match (.match input/alt-backspace-pattern
                    (java.util.ArrayList. [(Character. (char 0x1b)) ch]))
            key   (.-fullMatch match)]
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

(defdescribe keystroke->paste-char-test
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

(defdescribe placeholder-format-test
  (it "multi-line paste shows lines AND bytes"
    (let [text  (apply str (repeat 42 "line of text\n"))
          token (input/format-paste-placeholder {:id 1 :content text})]
      (expect (str/includes? token "#1"))
      (expect (str/includes? token "43 lines"))
      (expect (re-find #"\d+(?:\.\d+)?[BKM]" token))))

  (it "single-line paste pluralises correctly + reports byte size"
    (let [token (input/format-paste-placeholder
                  {:id 7 :content "abcdef"})]
      (expect (= "[Pasted #7: 1 line, 6B]" token))))

  (it "sub-1024 chars render as <N>B; 1024+ render as KB"
    (let [small (input/format-paste-placeholder {:id 1 :content "x"})
          big   (input/format-paste-placeholder
                  {:id 2 :content (apply str (repeat 2048 "a"))})]
      (expect (str/includes? small "1B"))
      (expect (str/includes? big "2.0KB")))))

(defdescribe placeholder-threshold-test
  (it "short single-line text bypasses the placeholder UX"
    (expect (false? (input/use-placeholder? "hello world"))))

  (it "any newline triggers a placeholder regardless of length"
    (expect (true? (input/use-placeholder? "a\nb"))))

  (it "long single-line text triggers a placeholder by length"
    (expect (true? (input/use-placeholder? (apply str (repeat 200 "x")))))))

(defdescribe placeholder-expand-test
  (it "substitutes every known token; passes unknown ones through"
    (let [pastes-map {1 {:id 1 :content "FIRST"}
                      2 {:id 2 :content "SECOND"}}
          input-text "prefix [Pasted #1: 5 lines, 1.0KB] middle [Pasted #2: 1 line, 6B] suffix"]
      (expect (= "prefix FIRST middle SECOND suffix"
                (input/expand-paste-placeholders input-text pastes-map)))))

  (it "unknown id passes through verbatim"
    (let [unchanged "hello [Pasted #99: 1 line, 1B] world"]
      (expect (= unchanged
                (input/expand-paste-placeholders unchanged {})))))

  (it "replacement preserves regex-special chars in the content"
    (let [pastes {1 {:id 1 :content "$1 \\n raw"}}
          out (input/expand-paste-placeholders "x [Pasted #1: 1 line, 9B] y" pastes)]
      (expect (= "x $1 \\n raw y" out)))))

(defdescribe placeholder-smart-delete-test
  (it "placeholder-id-before-cursor returns the id when cursor sits right after `]`"
    (let [token (input/format-paste-placeholder
                  {:id 4 :content "line one\nline two"})
          state (-> (input/empty-input)
                  (input/paste-text token))]
      (expect (= 4 (input/placeholder-id-before-cursor state)))))

  (it "returns nil when the cursor is somewhere in the middle of a placeholder"
    (let [token (input/format-paste-placeholder
                  {:id 4 :content "a\nb"})
          state (-> (input/empty-input)
                  (input/paste-text token)
                  (input/move-left)
                  (input/move-left))]
      (expect (nil? (input/placeholder-id-before-cursor state)))))

  (it "returns nil when there's no placeholder on the line"
    (let [state (-> (input/empty-input)
                  (input/paste-text "plain text"))]
      (expect (nil? (input/placeholder-id-before-cursor state)))))

  (it "delete-placeholder-backward removes the WHOLE token in one shot"
    (let [token (input/format-paste-placeholder
                  {:id 4 :content "a\nb"})
          state (-> (input/empty-input)
                  (input/insert-char \h)
                  (input/insert-char \i)
                  (input/insert-char \space)
                  (input/paste-text token)
                  (input/insert-char \space)
                  (input/insert-char \!)
                  ;; Move cursor back to right after `]`.
                  (input/move-left)
                  (input/move-left))
          deleted (input/delete-placeholder-backward state)]
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

(defn- mk-chars [^String s]
  ;; Helper: java.util.List<Character> from a String, the shape
  ;; Lanterna feeds CharacterPattern.match. Renamed from `chars`
  ;; to dodge the `clojure.core/chars` shadow warning.
  (let [v (java.util.ArrayList.)]
    (doseq [c s] (.add v (Character. ^char c)))
    v))

(defn- sgr [button col row final-ch]
  (str "\u001B[<" button ";" col ";" row final-ch))

(defdescribe sgr-mouse-pattern-test
  (it "NOT_YET on empty / partial prefixes"
    (let [m (.match input/sgr-mouse-pattern (mk-chars ""))]
      (expect (= com.googlecode.lanterna.input.CharacterPattern$Matching/NOT_YET m)))
    (let [m (.match input/sgr-mouse-pattern (mk-chars "\u001B"))]
      (expect (= com.googlecode.lanterna.input.CharacterPattern$Matching/NOT_YET m)))
    (let [m (.match input/sgr-mouse-pattern (mk-chars "\u001B[<0;1"))]
      (expect (= com.googlecode.lanterna.input.CharacterPattern$Matching/NOT_YET m))))

  (it "nil on shape violation (third byte is not '<')"
    ;; Plain CSI sequences (no '<') belong to other patterns -
    ;; sgr-mouse-pattern must reject them so Lanterna keeps trying.
    (expect (nil? (.match input/sgr-mouse-pattern (mk-chars "\u001B[A")))))

  (it "left-button press at column 50, row 1 decodes correctly"
    (let [m (.match input/sgr-mouse-pattern (mk-chars (sgr 0 50 1 \M)))
          ks (.-fullMatch ^com.googlecode.lanterna.input.CharacterPattern$Matching m)]
      (expect (instance? com.googlecode.lanterna.input.MouseAction ks))
      (let [^com.googlecode.lanterna.input.MouseAction ma ks
            pos (.getPosition ma)]
        (expect (= 49 (.getColumn pos)))      ; SGR is 1-based, ours is 0-based
        (expect (= 0  (.getRow pos)))
        (expect (= com.googlecode.lanterna.input.MouseActionType/CLICK_DOWN
                  (.getActionType ma))))))

  (it "left-button release uses lowercase 'm' as terminator"
    (let [m  (.match input/sgr-mouse-pattern (mk-chars (sgr 0 112 1 \m)))
          ma ^com.googlecode.lanterna.input.MouseAction
          (.-fullMatch ^com.googlecode.lanterna.input.CharacterPattern$Matching m)]
      (expect (= com.googlecode.lanterna.input.MouseActionType/CLICK_RELEASE
                (.getActionType ma)))
      ;; Critically: column 112 (the copy-id range) decodes to 111,
      ;; NOT to the broken 65500 the legacy parser produced.
      (expect (= 111 (.getColumn (.getPosition ma))))))

  (it "wheel-up button (64) maps to SCROLL_UP"
    (let [m  (.match input/sgr-mouse-pattern (mk-chars (sgr 64 10 5 \M)))
          ma ^com.googlecode.lanterna.input.MouseAction
          (.-fullMatch ^com.googlecode.lanterna.input.CharacterPattern$Matching m)]
      (expect (= com.googlecode.lanterna.input.MouseActionType/SCROLL_UP
                (.getActionType ma)))))

  (it "wheel-down button (65) maps to SCROLL_DOWN without reflection warnings"
    (let [m  (.match input/sgr-mouse-pattern (mk-chars (sgr 65 10 5 \M)))
          ma ^com.googlecode.lanterna.input.MouseAction
          (.-fullMatch ^com.googlecode.lanterna.input.CharacterPattern$Matching m)]
      (expect (= com.googlecode.lanterna.input.MouseActionType/SCROLL_DOWN
                (.getActionType ma))))
    (let [err (java.io.StringWriter.)]
      (binding [*warn-on-reflection* true
                *err* err]
        (require 'com.blockether.vis.ext.channel-tui.input :reload))
      (expect (not (str/includes? (str err) "input.clj")))))

  (it "drag with button held (32) maps to DRAG"
    (let [m  (.match input/sgr-mouse-pattern (mk-chars (sgr 32 10 5 \M)))
          ma ^com.googlecode.lanterna.input.MouseAction
          (.-fullMatch ^com.googlecode.lanterna.input.CharacterPattern$Matching m)]
      (expect (= com.googlecode.lanterna.input.MouseActionType/DRAG
                (.getActionType ma)))))

  (it "plain motion (35: bits=3 + drag bit) maps to MOVE"
    (let [m  (.match input/sgr-mouse-pattern (mk-chars (sgr 35 10 5 \M)))
          ma ^com.googlecode.lanterna.input.MouseAction
          (.-fullMatch ^com.googlecode.lanterna.input.CharacterPattern$Matching m)]
      (expect (= com.googlecode.lanterna.input.MouseActionType/MOVE
                (.getActionType ma)))))

  (it "wide-column press at column 200 decodes faithfully"
    ;; The whole point of switching to SGR: legacy X10 corrupted
    ;; this case to mx=65500. SGR is ASCII text so col 200 stays 200.
    (let [m  (.match input/sgr-mouse-pattern (mk-chars (sgr 0 200 50 \M)))
          ma ^com.googlecode.lanterna.input.MouseAction
          (.-fullMatch ^com.googlecode.lanterna.input.CharacterPattern$Matching m)]
      (expect (= 199 (.getColumn (.getPosition ma))))
      (expect (= 49  (.getRow    (.getPosition ma)))))))

(defdescribe file-mention-expand-test
  (it "replaces inline @mentions via the file-expander helper"
    (with-redefs [com.blockether.vis.ext.channel-tui.input/file-mention->prompt-block
                  (fn [path] (str "<FILE:" path ">"))]
      (expect (= "see <FILE:src/foo.clj> please"
                (input/expand-file-mentions "see @src/foo.clj please")))))

  (it "expands local @mentions to a tool-agnostic attached-file directive"
    ;; Per `file-mention->prompt-block` contract: emit a short
    ;; directive only (no `(cat ...)` / `(v/preview ...)`
    ;; boilerplate). The model picks the right tool itself.
    (let [file (java.io.File/createTempFile "vis-input-large" ".clj")]
      (spit file (str/join "\n" (repeat 121 "x")))
      (try
        (with-redefs [com.blockether.vis.ext.channel-tui.input/resolve-local-file
                      (fn [_path] file)]
          (let [expanded (input/expand-file-mentions "inspect @src/foo.clj now")]
            (expect (str/includes? expanded "[Attached File: src/foo.clj]"))
            (expect (str/includes? expanded "Read it"))
            (expect (not (str/includes? expanded "v/preview")))))
        (finally
          (.delete file)))))

  (it "supports quoted @mentions for paths with spaces"
    (with-redefs [com.blockether.vis.ext.channel-tui.input/file-mention->prompt-block
                  (fn [path] (str "<FILE:" path ">"))]
      (expect (= "see <FILE:docs/My File.md> please"
                (input/expand-file-mentions "see @\"docs/My File.md\" please")))))

  (it "leaves non-matching @text alone"
    (expect (= "email me at a@b.com"
              (input/expand-file-mentions "email me at a@b.com"))))

  (it "formats simple visible mention tokens with a leading @"
    (expect (= "@src/foo.clj"
              (input/format-file-mention "src/foo.clj"))))

  (it "quotes visible mention tokens when the path contains whitespace"
    (expect (= "@\"docs/My File.md\""
              (input/format-file-mention "docs/My File.md")))))
