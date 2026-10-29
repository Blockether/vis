(ns com.blockether.vis.tui.primitives-test
  "Coverage for the column-aware width math in primitives.clj.

   Every test here would FAIL on a `(count s)` / `(subs s 0 w)`
   implementation. They are the ratchet against re-introducing
   char-count math for terminal-rendered strings."
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.primitives :as p]
            [lazytest.core :refer [defdescribe describe expect it]]))

;; Reference fixtures. Pick code points that exercise distinct
;; failure modes so a single broken implementation surfaces clearly.

(def ASCII "abc")          ;; 3 cells / 3 chars / 3 columns

(def CJK "日本語")        ;; 3 cells / 3 chars / 6 columns (each glyph double-width)

(def EMOJI_BMP_TEXT "\u2615")  ;; 1 cell / 1 char / TWO cols (☕ hot beverage - Emoji_Presentation=Yes)

(def EMOJI_BMP_NARROW "\u2764")  ;; 1 cell / 1 char / TWO cols (❤ heavy heart - Extended_Pictographic, wide in OpenTUI)

(def EMOJI_BMP_WIDE "\u2705")  ;; 1 cell / 1 char / TWO cols (✅ white check mark button - the bug)

(def EMOJI_BMP_STAR "\u2B50")  ;; 1 cell / 1 char / TWO cols (⭐ star)

(def EMOJI_BMP_BOLT "\u26A1")  ;; 1 cell / 1 char / TWO cols (⚡ high voltage)

(def EMOJI_SMP "\uD83D\uDCC1")  ;; 1 cell / 2 chars / 2 columns (📁 - supplementary plane)

(def FLAG_PL "\uD83C\uDDF5\uD83C\uDDF1") ;; 1 cell / 4 chars / 2 columns (🇵🇱 - regional indicator pair)

(def MIXED (str "x " EMOJI_SMP " y")) ;; "x 📁 y" - 5 cells / 6 chars / 6 columns

(defdescribe
  display-width-test
  (describe "display-width counts terminal columns, not Java chars"
            (it "CJK is two columns per glyph" (expect (= 6 (p/display-width CJK))))
            (it "SMP emoji 📁 is two columns, not two chars"
                (expect (= 2 (p/display-width EMOJI_SMP))))
            (it "Regional-indicator flag 🇵🇱 is two columns, not four chars"
                (expect (= 2 (p/display-width FLAG_PL))))
            (it "Mixed ASCII + emoji line is six columns" (expect (= 6 (p/display-width MIXED)))))
  (describe
    "control characters are sanitized, NEVER throw (regression: session 954bf315)"
    ;; Pre-sanitizer, Lanterna's `TextCharacter.fromString` threw on
    ;; any C0 byte (0x00-0x1F), the render thread's catch-all
    ;; swallowed it, the bubble silently failed to paint, the user
    ;; saw a blank scrollback. Each control byte must now degrade to
    ;; a single visible / column instead of taking the thread down.
    (it
      "NEVER throws on a non-String arg (Character / number / nil) — the
         ClassCastException that froze the whole TUI when the F2 panel mapped
         display-width over a string's characters (regression)"
      (expect (= 1 (p/display-width \a)))
      (expect (= 1 (p/display-width \space)))
      (expect (= 0 (p/display-width nil)))
      (expect (= 1 (p/display-width 7)))))
  (describe "BMP `Emoji_Presentation=Yes` chars are TWO columns (lanterna fork fix)"
            ;; Pre-fix lanterna's TextCharacter.isDoubleWidth() returned false
            ;; for these because they're single-`char` BMP code points, not CJK,
            ;; not multi-`char` graphemes, and pass `isPrintableCharacter`.
            ;; The vis-3.1.5-vis.2 fork adds an explicit Emoji_Presentation=Yes
            ;; range check; this test pins it for the four chars that hit the
            ;; user's table-rendering bug.
            (it "☕ hot beverage is 2 cols" (expect (= 2 (p/display-width EMOJI_BMP_TEXT)))))
  (describe "VS-16/VS-15 graphemes match observed terminal width (lanterna fork 3.1.5-vis.5 fix)"
            ;; VS-16 (U+FE0F) requests EMOJI presentation — the vis target terminals
            ;; paint these as TWO cells, so display-width counts them as 2 (deferring to
            ;; lanterna isDoubleWidth). VS-15 (U+FE0E) requests TEXT presentation = 1.
            ;; OpenTUI measures every Extended_Pictographic code point as 2 columns,
            ;; so a bare heart is wide too.
            (it "❤ heart alone is 2 cols, as in OpenTUI"
                (expect (= 2 (p/display-width EMOJI_BMP_NARROW))))
            (it "❤️ heart + VS-16 is 2 cols (emoji presentation)"
                (expect (= 2 (p/display-width (str EMOJI_BMP_NARROW "️")))))
            (it "☑︎ ballot box + VS-15 is 1 col" (expect (= 1 (p/display-width "\u2611\uFE0E")))))
  (describe "edge cases" (it "nil is zero" (expect (= 0 (p/display-width nil))))))

(defdescribe expand-tabs-test
             (describe "expand-tabs replaces hard TABs with 4-col-stop spaces (matches paint)"
                       ;; Regression: tab-separated tool output (e.g. `gh issue list`)
                       ;; folded as if each TAB were 1 column, then overflowed the bubble's
                       ;; right edge once `putString` re-expanded tabs to 4-col stops.
                       (it "aligns to the next 4-column stop, mirroring putString"
                           (expect (= "38  OPEN    XY" (p/expand-tabs "38\tOPEN\tXY"))))
                       (it "a tab sitting exactly on a stop advances a full width"
                           (expect (= "abcd    e" (p/expand-tabs "abcd\te"))))
                       (it "nil / empty become empty string"
                           (expect (= "" (p/expand-tabs nil)))
                           (expect (= "" (p/expand-tabs ""))))))

(defdescribe put-str-sanitizer-test
             (it "strips inline style sentinels before raw Lanterna putString"
                 (let [captured
                       (atom [])

                       graphics
                       (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
                         (putString ([_col _row text] (swap! captured conj text) this)))

                       line
                       (str "Searched — 0 hit(s), truncated-by "
                            p/INLINE_CODE_ON
                            "end-of-results"
                            p/INLINE_CODE_OFF
                            ".")]

                   (p/put-str! graphics 0 0 line)
                   (let [painted (apply str @captured)]
                     (expect (= "Searched — 0 hit(s), truncated-by end-of-results." painted))
                     (expect (not (str/includes? painted p/INLINE_CODE_ON)))
                     (expect (not (str/includes? painted p/INLINE_CODE_OFF)))))))

(defdescribe col-prefix-end-test
             (describe "col-prefix-end returns the longest grapheme-safe prefix length"
                       (it "CJK stops at the last whole glyph that fits"
                           ;; "日本語" - each glyph 2 cols. max-cols=5 -> fit 2 glyphs (4 cols), drop 3rd.
                           (expect (= 2 (p/col-prefix-end CJK 5))))
                       (it "SMP emoji is atomic"
                           ;; "x📁y" -> max-cols=2 should yield "x" only (the emoji is 2 cols and would overflow).
                           (expect (= 1 (p/col-prefix-end (str "x" EMOJI_SMP "y") 2))))
                       (it "Returns 0 for nil / non-positive max-cols"
                           (expect (= 0 (p/col-prefix-end nil 5)))
                           (expect (= 0 (p/col-prefix-end CJK 0)))
                           (expect (= 0 (p/col-prefix-end CJK -1))))))

(defdescribe truncate-cols-test
             (describe "truncate-cols never splits a grapheme"
                       (it "CJK truncates per double-width glyph"
                           (expect (= "日本" (p/truncate-cols CJK 4))))
                       (it "Cutting an emoji boundary drops the emoji and pads with one space"
                           ;; "x 📁" has display-width 4. truncating to 3 must NOT include
                           ;; half of 📁 - drop it, pad to keep width invariant.
                           (let [out (p/truncate-cols (str "x " EMOJI_SMP) 3)]
                             (expect (= 3 (p/display-width out)))
                             (expect (= "x  " out))))
                       (it "Truncating to a width that exactly fits a wide glyph keeps it"
                           (let [out (p/truncate-cols (str "x" EMOJI_SMP) 3)]
                             (expect (= 3 (p/display-width out)))
                             (expect (= (str "x" EMOJI_SMP) out)))))
             (describe "edge cases"
                       (it "max-cols<0 returns empty" (expect (= "" (p/truncate-cols ASCII -1))))))

(def ^:private ESC (str \u001b))

(defn- strip-ansi [s] (str/replace s (re-pattern (str ESC "\\[[0-9;]*m")) ""))

(defn- sgr [code] (str ESC "[" code "m"))

(defdescribe ansi-fold-cols-test
             (describe "ansi-fold-cols folds SGR-aware, never counting escapes as width"
                       (it "a wide colorized line folds to the budget, visible content preserved"
                           (let [line
                                 (str (sgr 31) (apply str (repeat 100 "x")) (sgr 0))

                                 segs
                                 (p/ansi-fold-cols line 40)]

                             ;; every segment fits once escapes are stripped...
                             (expect (every? #(<= (p/display-width (strip-ansi %)) 40) segs))
                             ;; ...it folded into several rows...
                             (expect (> (count segs) 1))
                             ;; ...and the visible text is preserved byte-for-byte.
                             (expect (= (strip-ansi line) (apply str (map strip-ansi segs))))))
                       (it "re-opens the SGR active at each cut on every continuation row"
                           ;; one green token spanning >1 row: each row must carry the color.
                           (let [line
                                 (str (sgr 32) (apply str (repeat 100 "x")) (sgr 0))

                                 segs
                                 (p/ansi-fold-cols line 40)]

                             (expect (every? #(str/starts-with? % (sgr 32)) segs))))
                       (it "leaves color reset before the cut off the continuation row"
                           ;; color closes before the wide plain run, so folds don't re-open it.
                           (let [line
                                 (str (sgr 31) "pre" (sgr 0) (apply str (repeat 100 "y")))

                                 segs
                                 (p/ansi-fold-cols line 40)]

                             (expect (not (str/starts-with? (second segs) ESC)))))))

(defdescribe ansi-slice-cols-test
             (describe
               "ansi-slice-cols chops a horizontal display-column window (less -S)"
               (it "plain text: CJK window respects double-width columns"
                   ;; "日本語" = 6 columns; window [2,4) is the 2nd glyph.
                   (expect (= "本" (p/ansi-slice-cols CJK 2 2))))
               (it "start past the end yields empty" (expect (= "" (p/ansi-slice-cols "abc" 10 5))))
               (it "non-positive width yields empty"
                   (expect (= "" (p/ansi-slice-cols "abc" 0 0)))
                   (expect (= "" (p/ansi-slice-cols "abc" 1 -2))))
               (it "a window inside a colored run RE-OPENS the color and closes it"
                   (let [line
                         (str (sgr 32) (apply str (repeat 40 "y")) (sgr 0))

                         w
                         (p/ansi-slice-cols line 10 5)]

                     (expect (str/starts-with? w (sgr 32)))
                     (expect (= "yyyyy" (strip-ansi w)))
                     (expect (str/ends-with? w (sgr 0)))))
               (it "the visible width of a window never exceeds the requested width"
                   (let [line (str (sgr 36) "hello " (sgr 0) (sgr 31) "world-is-wide" (sgr 0))]
                     (expect (<= (p/display-width (strip-ansi (p/ansi-slice-cols line 3 8))) 8))))))

(defdescribe pad-right-test
             (describe "pad-right pads to display columns"
                       (it "CJK fills to exact column count, not char count"
                           (expect (= 8 (p/display-width (p/pad-right CJK 8))))
                           (expect (= "日本語  " (p/pad-right CJK 8))))
                       (it "Over-wide input is truncated to width"
                           (expect (= 4 (p/display-width (p/pad-right CJK 4))))
                           (expect (= "日本" (p/pad-right CJK 4))))))

(defdescribe pad-left-test
             (describe "pad-left mirrors pad-right"
                       (it "CJK columns" (expect (= 8 (p/display-width (p/pad-left CJK 8)))))))

(defdescribe center-text-test
             (describe "center-text centers by columns"
                       (it "ASCII odd-padding rounds down on the left"
                           (expect (= " abc  " (p/center-text ASCII 6))))
                       (it "CJK centred in 8 cols"
                           (expect (= 8 (p/display-width (p/center-text CJK 8)))))))

(defdescribe
  space-between-test
  (describe "space-between distributes by columns"
            (it "Two ASCII items"
                (expect (= "left           right" (p/space-between ["left" "right"] 20))))
            (it "Single item is centred" (expect (= "    abc    " (p/space-between [ASCII] 11))))))

(defdescribe space-around-test
             (describe "space-around honours columns"
                       (it "Two ASCII items, exact width"
                           (let [out (p/space-around ["a" "b"] 6)]
                             (expect (= 6 (p/display-width out)))))))

(defdescribe tabs-test
             (it "lays tabs out within the requested terminal width"
                 (let [layout (p/tab-layout [{:id :main :label "Main"}
                                             {:id :work :label "日本" :dirty? true}
                                             {:id :err :label "Broken" :state :error}]
                                            2
                                            18
                                            :work)]
                   (expect (= [2 9 15] (mapv :left layout)))
                   (expect (= [6 5 5] (mapv :width layout)))
                   (expect (= [false true false] (mapv :active? layout)))
                   (expect (every? true?
                                   (map (fn [{:keys [text width]}]
                                          (<= (p/display-width text) width))
                                        layout)))
                   (expect (= :work (:id (p/tab-at layout 9))))))
             (it "keeps geometry safe when there are more tabs than columns"
                 (let [layout (p/tab-layout [{:id :a} {:id :b} {:id :c}] 0 2 :a)]
                   (expect (= 3 (count layout)))
                   (expect (= 2 (reduce + (map :width layout))))
                   (expect (= :a (:id (p/tab-at layout 0)))))))
