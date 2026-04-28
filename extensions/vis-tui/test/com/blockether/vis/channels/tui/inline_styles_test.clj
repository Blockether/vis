(ns com.blockether.vis.channels.tui.inline-styles-test
  "Coverage for the inline markdown span pipeline:
     `markdown->inline` (parse `**bold**`, `*italic*`, `~~strike~~`,
                          `` `code` `` into sentinel pairs)
     +
     sentinel handling in `display-width` / `col-prefix-end` /
     `truncate-cols` (must count as zero columns).

   Painter is exercised indirectly via the round-trip width pin."
  (:require [com.blockether.vis.channels.tui.primitives :as p]
            [com.blockether.vis.channels.tui.render :as render]
            [lazytest.core :refer [defdescribe describe expect it]]))

(def ^:private markdown->inline @#'render/markdown->inline)

;; Convenience: build the expected string with sentinels inline so
;; assertions stay readable.
(def ^:private B  p/INLINE_BOLD_ON)
(def ^:private B' p/INLINE_BOLD_OFF)
(def ^:private I  p/INLINE_ITALIC_ON)
(def ^:private I' p/INLINE_ITALIC_OFF)
(def ^:private S  p/INLINE_STRIKE_ON)
(def ^:private S' p/INLINE_STRIKE_OFF)
(def ^:private C  p/INLINE_CODE_ON)
(def ^:private C' p/INLINE_CODE_OFF)

(defdescribe inline-sentinel-test
  (describe "Predicate"
    (it "BOLD_ON sentinel is recognised"  (expect (p/inline-sentinel? p/INLINE_BOLD_ON)))
    (it "CODE_OFF sentinel is recognised" (expect (p/inline-sentinel? p/INLINE_CODE_OFF)))
    (it "ASCII char is not a sentinel"    (expect (not (p/inline-sentinel? "a"))))
    (it "Emoji is not a sentinel"         (expect (not (p/inline-sentinel? "📁"))))
    (it "Empty string is not a sentinel"  (expect (not (p/inline-sentinel? ""))))
    (it "PUA char outside the inline range is not a sentinel"
      (expect (not (p/inline-sentinel? "\uE001"))))))   ;; line marker, not inline

(defdescribe sentinel-zero-width-test
  (describe "display-width treats sentinels as 0 columns"
    (it "Bare sentinel is 0 cols"
      (expect (= 0 (p/display-width B))))
    (it "Decorated 'hello' has the same width as plain 'hello'"
      (expect (= (p/display-width "hello")
                (p/display-width (str B "hello" B')))))
    (it "Multiple sentinels stack to zero"
      (expect (= 3 (p/display-width (str B I S "abc" S' I' B'))))))

  (describe "col-prefix-end skips over sentinels without consuming budget"
    (it "Five-col budget consumes 'hello' regardless of decorations around it"
      (let [decorated (str B "hello" B' " world")]
        ;; The first 5 cols are 'hello'; everything before/after costs 0.
        (expect (= (count (str B "hello" B'))
                  (p/col-prefix-end decorated 5))))))

  (describe "truncate-cols preserves sentinels"
    (it "Truncating after the bold-off keeps the close sentinel"
      (let [decorated (str B "hi" B' "..............................")
            cut       (p/truncate-cols decorated 2)]
        (expect (= 2 (p/display-width cut)))
        (expect (.contains cut B))
        (expect (.contains cut B'))))))

(defdescribe markdown->inline-test
  (describe "Bold"
    (it "**bold** wraps the span"
      (expect (= (str "hi " B "world" B' "!")
                (markdown->inline "hi **world**!"))))
    (it "__bold__ alternative syntax"
      (expect (= (str B "x" B')
                (markdown->inline "__x__"))))
    (it "Multiple bold spans on one line"
      (expect (= (str B "a" B' " " B "b" B')
                (markdown->inline "**a** **b**"))))
    (it "Empty **** falls through as literal asterisks"
      (expect (= "****" (markdown->inline "****")))))

  (describe "Italic"
    (it "*italic* wraps the span"
      (expect (= (str I "yo" I')
                (markdown->inline "*yo*"))))
    (it "_italic_ alternative syntax"
      (expect (= (str I "yo" I')
                (markdown->inline "_yo_"))))
    (it "Bold takes precedence over italic for `**`"
      ;; `**a**` should be BOLD, NOT italic-italic-a-italic-italic.
      (expect (= (str B "a" B')
                (markdown->inline "**a**")))))

  (describe "Strike"
    (it "~~strike~~ wraps the span"
      (expect (= (str S "old" S')
                (markdown->inline "~~old~~")))))

  (describe "Inline code"
    (it "`code` wraps the span"
      (expect (= (str C "f(x)" C')
                (markdown->inline "`f(x)`"))))
    (it "Markdown inside `code` is NOT recursively parsed (literal)"
      ;; Per our design: code spans are atomic, no nested **bold**.
      (expect (= (str C "**not bold**" C')
                (markdown->inline "`**not bold**`")))))

  (describe "Mixed"
    (it "Bold + italic + code on one line, all preserved"
      (let [in  "**a** *b* `c`"
            out (markdown->inline in)]
        (expect (.contains out B))
        (expect (.contains out I))
        (expect (.contains out C))
        ;; Display-width = original visible content = 'a b c' = 5
        (expect (= 5 (p/display-width out)))))

    (it "Unmatched opener falls through as literal"
      (expect (= "**unclosed" (markdown->inline "**unclosed"))))

    (it "Decorated text wraps to display columns, not Java chars"
      ;; `**hello world**` → 11 visible cols, but the sentinel-decorated
      ;; string is 13 chars. wrap-text keyed on display-width must NOT
      ;; insert a wrap at col 11.
      (let [decorated (markdown->inline "**hello world**")]
        (expect (= 11 (p/display-width decorated)))))))

(defdescribe nested-spans-test
  (describe "Bold inside italic"
    (it "*italic with **bold** inside* parses as italic-wrapping-bold"
      (expect (= (str I "italic with " B "bold" B' " inside" I')
                (markdown->inline "*italic with **bold** inside*"))))
    (it "_italic_ + __bold__ alternative syntaxes nest the same way"
      (expect (= (str I "a " B "b" B' " c" I')
                (markdown->inline "_a __b__ c_")))))

  (describe "Italic inside bold"
    (it "**bold *italic* bold-again** parses as bold-wrapping-italic"
      (expect (= (str B "bold " I "italic" I' " bold-again" B')
                (markdown->inline "**bold *italic* bold-again**"))))
    (it "Multiple italics nested in one bold"
      (expect (= (str B "a " I "b" I' " c " I "d" I' " e" B')
                (markdown->inline "**a *b* c *d* e**")))))

  (describe "Strike combinations"
    (it "Bold inside strike: ~~strike **bold** strike~~"
      (expect (= (str S "strike " B "bold" B' " strike" S')
                (markdown->inline "~~strike **bold** strike~~"))))
    (it "Italic inside strike: ~~strike *italic* strike~~"
      (expect (= (str S "strike " I "italic" I' " strike" S')
                (markdown->inline "~~strike *italic* strike~~"))))
    (it "Strike inside bold: **bold ~~strike~~ bold**"
      (expect (= (str B "bold " S "strike" S' " bold" B')
                (markdown->inline "**bold ~~strike~~ bold**"))))
    (it "Strike inside italic: *italic ~~strike~~ italic*"
      (expect (= (str I "italic " S "strike" S' " italic" I')
                (markdown->inline "*italic ~~strike~~ italic*")))))

  (describe "Code is atomic — NEVER recursed"
    (it "Bold inside code stays literal: `**not-bold**`"
      (expect (= (str C "**not-bold**" C')
                (markdown->inline "`**not-bold**`"))))
    (it "Italic inside code stays literal: `*not-italic*`"
      (expect (= (str C "*not-italic*" C')
                (markdown->inline "`*not-italic*`"))))
    (it "Strike inside code stays literal: `~~not-strike~~`"
      (expect (= (str C "~~not-strike~~" C')
                (markdown->inline "`~~not-strike~~`")))))

  (describe "Code INSIDE other spans — the outer span recurses normally"
    (it "Code inside bold: **bold `code` bold**"
      (expect (= (str B "bold " C "code" C' " bold" B')
                (markdown->inline "**bold `code` bold**"))))
    (it "Code inside italic: *italic `code` italic*"
      (expect (= (str I "italic " C "code" C' " italic" I')
                (markdown->inline "*italic `code` italic*"))))
    (it "Code inside strike: ~~strike `code` strike~~"
      (expect (= (str S "strike " C "code" C' " strike" S')
                (markdown->inline "~~strike `code` strike~~")))))

  (describe "Triple nesting"
    (it "Bold > italic > strike: **bold *italic ~~strike~~ italic* bold**"
      (expect (= (str B "bold " I "italic " S "strike" S' " italic" I' " bold" B')
                (markdown->inline "**bold *italic ~~strike~~ italic* bold**"))))
    (it "Italic > bold > code: *italic **bold `code` bold** italic*"
      (expect (= (str I "italic " B "bold " C "code" C' " bold" B' " italic" I')
                (markdown->inline "*italic **bold `code` bold** italic*"))))
    (it "Strike > italic > bold: ~~strike *italic **bold** italic* strike~~"
      (expect (= (str S "strike " I "italic " B "bold" B' " italic" I' " strike" S')
                (markdown->inline "~~strike *italic **bold** italic* strike~~")))))

  (describe "Code inside code-inside-bold — still atomic"
    (it "**`outer code` and **bold-text** more**"
      ;; Outer **…** wraps everything; inside, the FIRST `code` is
      ;; atomic, then ` and ` plain, then `**bold-text**` is the
      ;; closing of outer-bold. Wait — that's actually the case where
      ;; `**` shows up inside our outer bold. Should be 'and **bold-
      ;; text**' literal? No — we recurse into the content. After find-
      ;; close handles code-skip, we look for our outer `**` past the
      ;; code span. Inside content we run markdown->inline, which sees
      ;; the inner `**bold-text**` and parses it as nested bold. Note
      ;; though that outer bold then contains nested bold — BOLD
      ;; toggle ON → OFF → ON → OFF, which the painter handles per
      ;; SGR semantics (treat each toggle independently).
      (let [out (markdown->inline "**`outer code` middle `inner` more**")]
        (expect (= (str B C "outer code" C' " middle " C "inner" C' " more" B')
                  out)))))

  (describe "Edge cases that should fall through to literal"
    (it "Empty bold: **** → literal asterisks"
      (expect (= "****" (markdown->inline "****"))))
    (it "Unmatched opener: **incomplete → literal"
      (expect (= "**incomplete" (markdown->inline "**incomplete"))))
    (it "Lone asterisk in middle: a*b is just literal"
      ;; Single `*` followed by content followed by NO close → literal.
      (expect (= "a*b" (markdown->inline "a*b"))))
    (it "Adjacent spans don't bleed: **a****b** → BOLD a, BOLD b"
      ;; The middle `**` closes the first bold and opens the second.
      (expect (= (str B "a" B' B "b" B')
                (markdown->inline "**a****b**"))))))

(defdescribe display-width-with-nesting-test
  (describe "Sentinel-decorated nested spans count zero columns total"
    (it "*italic with **bold** inside* visible width = 27 cols"
      (let [in  "*italic with **bold** inside*"
            out (markdown->inline in)]
        ;; Visible: 'italic with bold inside' = 23 chars
        (expect (= 23 (p/display-width out)))))
    (it "Triple-nested visible width matches the prose, not the markdown"
      (let [out (markdown->inline "**a *b ~~c~~ d* e**")]
        ;; Visible: 'a b c d e' = 9 chars
        (expect (= 9 (p/display-width out)))))
    (it "Code inside bold counts both contents normally"
      (let [out (markdown->inline "**bold `code` more**")]
        ;; 'bold code more' = 14 chars
        (expect (= 14 (p/display-width out)))))))

(defdescribe paint-styled-line-stacking-test
  ;; The Polish bug report: `> **Lącznie:**` inside a quote rendered
  ;; bold-without-italic because paint-styled-line! cleared the
  ;; wrapping italic at entry. We pin the fix by recording the SGR
  ;; set on every paint call via a stub TextGraphics, then asserting
  ;; bold + italic stack correctly.
  (let [;; Capture every (putString ...) as [text {:fg :bg :sgr}].
        captured (atom [])
        active   (atom #{})
        fg       (atom nil)
        bg       (atom nil)
        ;; Lanterna's TextGraphics is an interface with ~30 methods;
        ;; we proxy the four paint-styled-line! actually calls.
        graphics (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
                   (clearModifiers []
                     (reset! active #{})
                     this)
                   (enableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                     (swap! active into (seq arr))
                     this)
                   (disableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                     (apply swap! active disj (seq arr))
                     this)
                   (getActiveModifiers []
                     ;; Return a defensive EnumSet so paint-styled-line!
                     ;; can `EnumSet/copyOf` it.
                     (if (empty? @active)
                       (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR)
                       (java.util.EnumSet/copyOf ^java.util.Collection @active)))
                   (setForegroundColor [c] (reset! fg c) this)
                   (setBackgroundColor [c] (reset! bg c) this)
                   (putString
                     ([col row text]
                      (swap! captured conj [text {:fg @fg :bg @bg :sgr @active}])
                      this)))]

    (describe "paint-styled-line! inherits the wrapping SGR modifiers"
      (it "BOLD inside a wrapping ITALIC stacks to bold-italic"
        (reset! captured [])
        (reset! active #{com.googlecode.lanterna.SGR/ITALIC})
        (let [line (str "plain " p/INLINE_BOLD_ON "loud" p/INLINE_BOLD_OFF " tail")]
          (p/paint-styled-line! graphics 0 0 line
            (com.googlecode.lanterna.TextColor$RGB. 0 0 0)
            (com.googlecode.lanterna.TextColor$RGB. 255 255 255)
            (com.googlecode.lanterna.TextColor$RGB. 50 50 50)
            (com.googlecode.lanterna.TextColor$RGB. 240 240 240))
          ;; Three segments: "plain ", "loud", " tail"
          (let [segs @captured]
            (expect (= 3 (count segs)))
            (let [[seg0 seg1 seg2] segs]
              ;; Segment 1: 'plain ' — inherits italic only.
              (expect (= "plain " (first seg0)))
              (expect (contains? (:sgr (second seg0)) com.googlecode.lanterna.SGR/ITALIC))
              (expect (not (contains? (:sgr (second seg0)) com.googlecode.lanterna.SGR/BOLD)))
              ;; Segment 2: 'loud' — italic + bold stacked.
              (expect (= "loud" (first seg1)))
              (expect (contains? (:sgr (second seg1)) com.googlecode.lanterna.SGR/ITALIC))
              (expect (contains? (:sgr (second seg1)) com.googlecode.lanterna.SGR/BOLD))
              ;; Segment 3: ' tail' — italic again, bold cleared.
              (expect (= " tail" (first seg2)))
              (expect (contains? (:sgr (second seg2)) com.googlecode.lanterna.SGR/ITALIC))
              (expect (not (contains? (:sgr (second seg2)) com.googlecode.lanterna.SGR/BOLD)))))))

      (it "At exit, the inherited SGR set is restored exactly"
        ;; Caller relies on `(p/styled g [p/ITALIC] (paint-styled-line! ...))`
        ;; ending with the same modifier state it started with, so its
        ;; own cleanup can finalise correctly.
        (reset! captured [])
        (reset! active #{com.googlecode.lanterna.SGR/ITALIC})
        (p/paint-styled-line! graphics 0 0
          (str p/INLINE_BOLD_ON "x" p/INLINE_BOLD_OFF)
          (com.googlecode.lanterna.TextColor$RGB. 0 0 0)
          (com.googlecode.lanterna.TextColor$RGB. 255 255 255)
          (com.googlecode.lanterna.TextColor$RGB. 50 50 50)
          (com.googlecode.lanterna.TextColor$RGB. 240 240 240))
        (expect (= #{com.googlecode.lanterna.SGR/ITALIC} @active)))

      (it "Dangling sentinel (no close) doesn't leak BOLD past the call"
        (reset! captured [])
        (reset! active #{com.googlecode.lanterna.SGR/ITALIC})
        (p/paint-styled-line! graphics 0 0
          (str "open " p/INLINE_BOLD_ON "never closes")
          (com.googlecode.lanterna.TextColor$RGB. 0 0 0)
          (com.googlecode.lanterna.TextColor$RGB. 255 255 255)
          (com.googlecode.lanterna.TextColor$RGB. 50 50 50)
          (com.googlecode.lanterna.TextColor$RGB. 240 240 240))
        ;; Even though the line ended mid-bold, the inherited italic
        ;; (NOT bold) is what the caller sees on exit.
        (expect (= #{com.googlecode.lanterna.SGR/ITALIC} @active))))))

(defdescribe round-trip-pad-cell-test
  (describe "pad-cell tolerates sentinel-bearing text"
    (let [pad-cell @#'render/pad-cell]
      (it "Bold cell pads to the same display-width as plain"
        (let [plain  (pad-cell "hi" 5 :left)
              styled (pad-cell (str B "hi" B') 5 :left)]
          (expect (= (p/display-width plain)
                    (p/display-width styled)))))
      (it "Code cell pads to same width as plain"
        (let [plain  (pad-cell "fn" 5 :left)
              styled (pad-cell (str C "fn" C') 5 :left)]
          (expect (= (p/display-width plain)
                    (p/display-width styled))))))))
