(ns com.blockether.vis.ext.channel-tui.input-test
  "Coverage for `channel-tui.input`.

   Two surfaces under test:

     1. The pure input-buffer helpers (`empty-input`, `insert-char`,
        `insert-newline`, `delete-backward`, `move-*`, `paste-text`)
        — used by every keystroke handled in the screen poll loop.
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
  (:import [com.googlecode.lanterna.input KeyStroke KeyType]))

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

  (it "Tab becomes \\t — preserves indentation in pasted code"
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
