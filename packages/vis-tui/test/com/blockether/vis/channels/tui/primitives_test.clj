(ns com.blockether.vis.channels.tui.primitives-test
  "Coverage for the column-aware width math in primitives.clj.

   Every test here would FAIL on a `(count s)` / `(subs s 0 w)`
   implementation. They are the ratchet against re-introducing
   char-count math for terminal-rendered strings."
  (:require [com.blockether.vis.channels.tui.primitives :as p]
            [lazytest.core :refer [defdescribe describe expect it]]))

;; Reference fixtures. Pick code points that exercise distinct
;; failure modes so a single broken implementation surfaces clearly.

(def ASCII       "abc")          ;; 3 cells / 3 chars / 3 columns
(def CJK         "日本語")        ;; 3 cells / 3 chars / 6 columns (each glyph double-width)
(def EMOJI_BMP_TEXT   "\u2615")  ;; 1 cell / 1 char / TWO cols (☕ hot beverage — Emoji_Presentation=Yes)
(def EMOJI_BMP_NARROW "\u2764")  ;; 1 cell / 1 char / 1  col   (❤ heavy heart — Emoji_Presentation=No, narrow without VS-16)
(def EMOJI_BMP_WIDE   "\u2705")  ;; 1 cell / 1 char / TWO cols (✅ white check mark button — the bug)
(def EMOJI_BMP_STAR   "\u2B50")  ;; 1 cell / 1 char / TWO cols (⭐ star)
(def EMOJI_BMP_BOLT   "\u26A1")  ;; 1 cell / 1 char / TWO cols (⚡ high voltage)
(def EMOJI_SMP   "\uD83D\uDCC1")  ;; 1 cell / 2 chars / 2 columns (📁 — supplementary plane)
(def FLAG_PL     "\uD83C\uDDF5\uD83C\uDDF1") ;; 1 cell / 4 chars / 2 columns (🇵🇱 — regional indicator pair)
(def MIXED       (str "x " EMOJI_SMP " y")) ;; "x 📁 y" — 5 cells / 6 chars / 6 columns

(defdescribe display-width-test
  (describe "display-width counts terminal columns, not Java chars"
    (it "ASCII matches char count"            (expect (= 3 (p/display-width ASCII))))
    (it "CJK is two columns per glyph"        (expect (= 6 (p/display-width CJK))))
    (it "SMP emoji 📁 is two columns, not two chars"
      (expect (= 2 (p/display-width EMOJI_SMP))))
    (it "Regional-indicator flag 🇵🇱 is two columns, not four chars"
      (expect (= 2 (p/display-width FLAG_PL))))
    (it "Mixed ASCII + emoji line is six columns"
      (expect (= 6 (p/display-width MIXED)))))

  (describe "BMP `Emoji_Presentation=Yes` chars are TWO columns (lanterna fork fix)"
    ;; Pre-fix lanterna's TextCharacter.isDoubleWidth() returned false
    ;; for these because they're single-`char` BMP code points, not CJK,
    ;; not multi-`char` graphemes, and pass `isPrintableCharacter`.
    ;; The vis-3.1.5-vis.2 fork adds an explicit Emoji_Presentation=Yes
    ;; range check; this test pins it for the four chars that hit the
    ;; user's table-rendering bug.
    (it "☕ hot beverage is 2 cols"     (expect (= 2 (p/display-width EMOJI_BMP_TEXT))))
    (it "✅ white check mark is 2 cols" (expect (= 2 (p/display-width EMOJI_BMP_WIDE))))
    (it "⭐ star is 2 cols"             (expect (= 2 (p/display-width EMOJI_BMP_STAR))))
    (it "⚡ high voltage is 2 cols"     (expect (= 2 (p/display-width EMOJI_BMP_BOLT)))))

  (describe "VS-16 graphemes are 1 col (lanterna fork 3.1.5-vis.3 fix)"
    ;; Unicode says VS-16 forces emoji presentation = 2 cols. Real
    ;; terminals (macOS Terminal, iTerm2 in some font configs) paint
    ;; them at base text-presentation width = 1 col. The lanterna
    ;; fork's TextCharacter.isDoubleWidth was updated to match what
    ;; terminals actually do, so display-width now reports 1 col for
    ;; any grapheme containing VS-16. Pins the contract here.
    (it "❤ heart alone is 1 col"   (expect (= 1 (p/display-width EMOJI_BMP_NARROW))))
    (it "❤️ heart + VS-16 stays 1 col (the user-terminal-matching fix)"
      (expect (= 1 (p/display-width (str EMOJI_BMP_NARROW "\uFE0F")))))
    (it "🏷️ label + VS-16 is 1 col"
      (expect (= 1 (p/display-width "\uD83C\uDFF7\uFE0F")))))

  (describe "edge cases"
    (it "nil is zero"   (expect (= 0 (p/display-width nil))))
    (it "empty is zero" (expect (= 0 (p/display-width ""))))))

(defdescribe col-prefix-end-test
  (describe "col-prefix-end returns the longest grapheme-safe prefix length"
    (it "ASCII matches max-cols verbatim"
      (expect (= 3 (p/col-prefix-end "abcdef" 3))))
    (it "CJK stops at the last whole glyph that fits"
      ;; "日本語" — each glyph 2 cols. max-cols=5 → fit 2 glyphs (4 cols), drop 3rd.
      (expect (= 2 (p/col-prefix-end CJK 5))))
    (it "SMP emoji is atomic"
      ;; "x📁y" → max-cols=2 should yield "x" only (the emoji is 2 cols and would overflow).
      (expect (= 1 (p/col-prefix-end (str "x" EMOJI_SMP "y") 2))))
    (it "Returns full length when string fits"
      (expect (= (.length CJK) (p/col-prefix-end CJK 99))))
    (it "Returns 0 for nil / non-positive max-cols"
      (expect (= 0 (p/col-prefix-end nil 5)))
      (expect (= 0 (p/col-prefix-end CJK 0)))
      (expect (= 0 (p/col-prefix-end CJK -1))))))

(defdescribe truncate-cols-test
  (describe "truncate-cols never splits a grapheme"
    (it "ASCII truncates char-for-char"
      (expect (= "ab" (p/truncate-cols ASCII 2))))
    (it "CJK truncates per double-width glyph"
      (expect (= "日本" (p/truncate-cols CJK 4))))
    (it "Cutting an emoji boundary drops the emoji and pads with one space"
      ;; "x 📁" has display-width 4. truncating to 3 must NOT include
      ;; half of 📁 — drop it, pad to keep width invariant.
      (let [out (p/truncate-cols (str "x " EMOJI_SMP) 3)]
        (expect (= 3 (p/display-width out)))
        (expect (= "x  " out))))
    (it "Truncating to a width that exactly fits a wide glyph keeps it"
      (let [out (p/truncate-cols (str "x" EMOJI_SMP) 3)]
        (expect (= 3 (p/display-width out)))
        (expect (= (str "x" EMOJI_SMP) out)))))

  (describe "edge cases"
    (it "nil returns empty"          (expect (= "" (p/truncate-cols nil 5))))
    (it "max-cols=0 returns empty"   (expect (= "" (p/truncate-cols ASCII 0))))
    (it "max-cols<0 returns empty"   (expect (= "" (p/truncate-cols ASCII -1))))
    (it "max-cols >= width returns input verbatim"
      (expect (identical? CJK (p/truncate-cols CJK 6)))
      (expect (identical? CJK (p/truncate-cols CJK 99))))))

(defdescribe pad-right-test
  (describe "pad-right pads to display columns"
    (it "ASCII shorter than width is right-padded with spaces"
      (expect (= "abc  " (p/pad-right ASCII 5))))
    (it "CJK fills to exact column count, not char count"
      (expect (= 8 (p/display-width (p/pad-right CJK 8))))
      (expect (= "日本語  " (p/pad-right CJK 8))))
    (it "Emoji-bearing line pads correctly"
      (expect (= 8 (p/display-width (p/pad-right MIXED 8)))))
    (it "Over-wide input is truncated to width"
      (expect (= 4 (p/display-width (p/pad-right CJK 4))))
      (expect (= "日本" (p/pad-right CJK 4))))
    (it "nil treated as empty"
      (expect (= "     " (p/pad-right nil 5))))))

(defdescribe pad-left-test
  (describe "pad-left mirrors pad-right"
    (it "ASCII"        (expect (= "  abc" (p/pad-left ASCII 5))))
    (it "CJK columns"  (expect (= 8 (p/display-width (p/pad-left CJK 8)))))
    (it "Emoji line"   (expect (= 10 (p/display-width (p/pad-left MIXED 10)))))
    (it "Truncates"    (expect (= "日" (p/pad-left CJK 2))))))

(defdescribe center-text-test
  (describe "center-text centers by columns"
    (it "ASCII odd-padding rounds down on the left"
      (expect (= " abc  " (p/center-text ASCII 6))))
    (it "Emoji width is honoured"
      (expect (= 10 (p/display-width (p/center-text MIXED 10)))))
    (it "CJK centred in 8 cols"
      (expect (= 8 (p/display-width (p/center-text CJK 8)))))
    (it "Truncates over-wide input"
      (expect (= 4 (p/display-width (p/center-text CJK 4)))))))

(defdescribe space-between-test
  (describe "space-between distributes by columns"
    (it "Two ASCII items"
      (expect (= "left           right" (p/space-between ["left" "right"] 20))))
    (it "Item with emoji keeps row width"
      (let [out (p/space-between [(str "Vbin" EMOJI_SMP) "ok"] 20)]
        (expect (= 20 (p/display-width out)))))
    (it "Single item is centred"
      (expect (= "    abc    " (p/space-between [ASCII] 11))))
    (it "Empty list fills with spaces"
      (expect (= "     " (p/space-between [] 5))))))

(defdescribe space-around-test
  (describe "space-around honours columns"
    (it "Two ASCII items, exact width"
      (let [out (p/space-around ["a" "b"] 6)]
        (expect (= 6 (p/display-width out)))))
    (it "Mixed emoji items keep row width"
      (let [out (p/space-around [(str "📁" "f") "ok"] 12)]
        (expect (= 12 (p/display-width out)))))))
