(ns com.blockether.vis.ext.channel-tui.render-test
  (:require
   [com.blockether.vis.ext.channel-tui.links :as links]
   [com.blockether.vis.ext.channel-tui.primitives :as p]
   [com.blockether.vis.ext.channel-tui.render :as render]
   [clojure.string :as str]
   [lazytest.core :refer [defdescribe describe expect it]]))

;; ─── from render_test.clj ───

(def ^:private md->lines @#'render/markdown->lines)

(defn- marker-of
  "First codepoint of `s` as a single-char string, or nil for empty."
  [s]
  (when (and (string? s) (pos? (count s)))
    (subs s 0 1)))

(defn- body-of
  "Drop the leading marker (PUA codepoint) and return the visible text."
  [s]
  (when (string? s) (subs s 1)))

(defdescribe markdown-headings-test
  (describe "ATX headings 1-3 each carry their own marker"
    (it "# Heading 1 → MARKER_MD_H1"
      (let [[line] (md->lines "# Heading 1" 80 :answer)]
        (expect (= p/MARKER_MD_H1 (marker-of line)))
        (expect (= "Heading 1" (body-of line)))))

    (it "## Heading 2 → MARKER_MD_H2"
      (let [[line] (md->lines "## Heading 2" 80 :answer)]
        (expect (= p/MARKER_MD_H2 (marker-of line)))
        (expect (= "Heading 2" (body-of line)))))

    (it "### Heading 3 → MARKER_MD_H3"
      (let [[line] (md->lines "### Heading 3" 80 :answer)]
        (expect (= p/MARKER_MD_H3 (marker-of line)))
        (expect (= "Heading 3" (body-of line))))))

  (describe "H4-H6 fold onto the H3 marker (terminal palettes top out)"
    ;; This is the regression we are pinning. Pre-fix, `#### Heading
    ;; 4` had no matching `cond` branch, so it fell through to
    ;; plain-text rendering and the literal `####` showed up in the
    ;; answer body. The fix collapses H4-H6 onto the H3 marker (same
    ;; convention as glow / mdcat / bat) so deep headings still read
    ;; AS headings instead of leaking hash characters.
    (it "#### Heading 4 → MARKER_MD_H3, no leading hashes in body"
      (let [[line] (md->lines "#### Heading 4" 80 :answer)]
        (expect (= p/MARKER_MD_H3 (marker-of line)))
        (expect (= "Heading 4" (body-of line)))
        (expect (not (str/includes? (body-of line) "#")))))

    (it "##### Heading 5 → MARKER_MD_H3, no leading hashes in body"
      (let [[line] (md->lines "##### Heading 5" 80 :answer)]
        (expect (= p/MARKER_MD_H3 (marker-of line)))
        (expect (= "Heading 5" (body-of line)))
        (expect (not (str/includes? (body-of line) "#")))))

    (it "###### Heading 6 → MARKER_MD_H3, no leading hashes in body"
      (let [[line] (md->lines "###### Heading 6" 80 :answer)]
        (expect (= p/MARKER_MD_H3 (marker-of line)))
        (expect (= "Heading 6" (body-of line)))
        (expect (not (str/includes? (body-of line) "#"))))))

  (describe "Boundary conditions"
    (it "####### (7 hashes) is NOT a heading — too deep, falls through"
      ;; CommonMark caps ATX headings at 6. A 7-hash line is plain
      ;; text. We assert that the marker is NOT one of the heading
      ;; markers.
      (let [[line] (md->lines "####### too deep" 80 :answer)
            mk    (marker-of line)]
        (expect (not= p/MARKER_MD_H1 mk))
        (expect (not= p/MARKER_MD_H2 mk))
        (expect (not= p/MARKER_MD_H3 mk))))

    (it "#NoSpace is NOT a heading — ATX requires a space after #"
      (let [[line] (md->lines "#NoSpace" 80 :answer)
            mk    (marker-of line)]
        (expect (not= p/MARKER_MD_H1 mk))
        (expect (not= p/MARKER_MD_H2 mk))
        (expect (not= p/MARKER_MD_H3 mk))))

    (it "All six heading levels in one document each render with a heading marker"
      (let [doc   (str/join "\n"
                    ["# H1" "## H2" "### H3" "#### H4" "##### H5" "###### H6"])
            lines (md->lines doc 80 :answer)
            heading-markers #{p/MARKER_MD_H1 p/MARKER_MD_H2 p/MARKER_MD_H3}]
        (expect (= 6 (count lines)))
        (expect (every? heading-markers (map marker-of lines)))
        ;; And specifically: H4-H6 all map to H3.
        (expect (= [p/MARKER_MD_H1 p/MARKER_MD_H2 p/MARKER_MD_H3
                    p/MARKER_MD_H3 p/MARKER_MD_H3 p/MARKER_MD_H3]
                  (mapv marker-of lines)))))))

(defdescribe markdown-headings-thinking-mode-test
  (describe "Thinking-mode headings use the thinking marker bundle"
    ;; The thinking-mode marker bundle is a parallel set of PUA
    ;; codepoints; the renderer paints them with the iteration-bg + dim
    ;; italic style. Same H4-H6 → H3 collapse must apply.
    (it "#### Heading 4 (thinking) → MARKER_TH_MD_H3"
      (let [[line] (md->lines "#### Heading 4" 80 :thinking)]
        (expect (= p/MARKER_TH_MD_H3 (marker-of line)))
        (expect (= "Heading 4" (body-of line)))))))

;; ─── from inline_styles_test.clj ───

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
    (it "Adjacent spans stay isolated: **a****b** → BOLD a, BOLD b"
      ;; The middle `**` closes the first bold and opens the second.
      (expect (= (str B "a" B' B "b" B')
                (markdown->inline "**a****b**")))))

  (describe "Intra-word underscore rule (CommonMark / GFM)"
    ;; Bug repro: identifiers with underscores were being chewed up
    ;; into italic spans by an underscore-greedy tokenizer. CommonMark
    ;; explicitly forbids underscore emphasis from opening or closing
    ;; mid-word; asterisk emphasis is unaffected.
    (it "VARIABLES_LIKE_THIS stays literal — no italic, underscores preserved"
      (expect (= "VARIABLES_LIKE_THIS"
                (markdown->inline "VARIABLES_LIKE_THIS"))))
    (it "Trailing prose after an identifier stays literal too"
      (expect (= "VARIABLES_LIKE_THIS in code"
                (markdown->inline "VARIABLES_LIKE_THIS in code"))))
    (it "FOO_BAR_BAZ — every underscore preserved"
      (expect (= "FOO_BAR_BAZ" (markdown->inline "FOO_BAR_BAZ"))))
    (it "snake_case_var here — lowercase identifiers also protected"
      (expect (= "snake_case_var here"
                (markdown->inline "snake_case_var here"))))
    (it "text_with_under — plain word with embedded underscores"
      (expect (= "text_with_under"
                (markdown->inline "text_with_under"))))
    (it "call(VAR_NAME) stays literal even with surrounding punctuation"
      (expect (= "call(VAR_NAME) please"
                (markdown->inline "call(VAR_NAME) please"))))
    (it "Repeated identifiers in one line: VAR_A and VAR_B"
      (expect (= "VAR_A and VAR_B"
                (markdown->inline "VAR_A and VAR_B"))))
    ;; And the regression sentinels for the CommonMark cases the rule
    ;; MUST NOT break: real underscore emphasis still works as long as
    ;; the boundaries are non-word characters (BOL/EOL/space/punct).
    (it "_word_ at boundaries still italicises"
      (expect (= (str I "word" I') (markdown->inline "_word_"))))
    (it "a _word_ b still italicises"
      (expect (= (str "a " I "word" I' " b")
                (markdown->inline "a _word_ b"))))
    (it "__bold__ at boundaries still bolds"
      (expect (= (str B "bold" B') (markdown->inline "__bold__"))))
    (it "_emph_, then text — punctuation after closer is allowed"
      (expect (= (str I "emph" I' ", then text")
                (markdown->inline "_emph_, then text"))))
    (it "Asterisk emphasis is intentionally unaffected: a*b*c stays italic-b"
      ;; CommonMark allows intra-word asterisk emphasis; we mirror it
      ;; on purpose so prose like `re*ally* good` keeps working.
      (expect (= (str "a" I "b" I' "c") (markdown->inline "a*b*c"))))))

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

;; ─── from table_render_test.clj ───

;; render-table is private — reach in via the var to test it directly.
(def ^:private render-table @#'render/render-table)

(def ^:private dummy-markers
  "render-table prepends a marker char per line so the bubble renderer
   knows whether each line is a top border, header, separator, or row.
   For width-math tests the marker identity is irrelevant — strip it
   before measuring."
  {:thead "H" :tsep "S" :trow "R"})

(defn- strip-marker
  "Drop the leading 1-char marker that render-table prepends to every
   line. Returns the bare visual line."
  [^String line]
  (subs line 1))

(defn- visual-widths
  "All distinct display-widths in the rendered table (excluding the
   marker prefix). A correct table is monomorphic — every line is the
   same number of terminal columns wide."
  [lines]
  (->> lines (map strip-marker) (map p/display-width) distinct sort))

(defdescribe pad-cell-width-test
  ;; pad-cell is private; reach in.
  (let [pad-cell @#'render/pad-cell]
    (describe "pad-cell pads to display columns, not Java chars"
      (it "ASCII cell padded to 5 cols"
        (let [out (pad-cell "abc" 5 :left)]
          ;; ` abc   ` → 1 + 3 + 2 = 5 cols of body + 2 outer padding spaces = 7 cols
          (expect (= 7 (p/display-width out)))))
      (it "BMP single-col emoji '☕' padded to 5"
        (let [out (pad-cell "☕" 5 :left)]
          (expect (= 7 (p/display-width out)))))
      (it "SMP emoji '📄' (2 chars / 2 cols) padded to 5 has the same visual width as ASCII"
        (let [out (pad-cell "📄" 5 :left)]
          (expect (= 7 (p/display-width out)))))
      (it "VS-16 emoji '🏷️' (3 chars / 1 col after lanterna fork's VS-16 fix)"
        ;; lanterna 3.1.5-vis.3's TextCharacter.isDoubleWidth returns
        ;; false for VS-16 graphemes (matches what real terminals
        ;; actually paint). display-width therefore reports 1, and
        ;; pad-cell allocates one extra space than a wide-emoji cell
        ;; would — cell winds up the SAME visual width as siblings.
        (let [out (pad-cell "🏷️" 5 :left)]
          (expect (= 7 (p/display-width out)))))
      (it "Regional-indicator flag '🇵🇱' (4 chars / 2 cols) has no VS-16 — 7 cols"
        (let [out (pad-cell "🇵🇱" 5 :left)]
          (expect (= 7 (p/display-width out)))))
      (it "Every emoji class — VS-16 included — pads to identical width when w=5"
        (let [w (fn [s] (p/display-width (pad-cell s 5 :left)))]
          (expect (= (w "abc") (w "☕") (w "📄") (w "🏷️") (w "🇵🇱"))))))

    (describe "pad-cell truncation respects column count"
      (it "Truncates ASCII over-wide content with ellipsis"
        ;; "abcdefghij" → 10 cols. w=5 → " abcd… " = 1+5+1 = 7 cols.
        (let [out (pad-cell "abcdefghij" 5 :left)]
          (expect (= 7 (p/display-width out)))
          (expect (str/includes? out "…"))))
      (it "Never splits an emoji at the truncation boundary"
        (let [out (pad-cell (str "x" "🏷️" "y") 2 :left)]
          ;; w=2 → " <body 2 cols> " = 4 cols
          (expect (= 4 (p/display-width out))))))))

(defdescribe render-table-width-test
  (describe "User's exact pathological table renders with monomorphic row width"
    ;; Reproduces the table from query 71794c5e: 16 file rows, every
    ;; row has [icon, name, size, type] and one row uses 🏷️ (VS-16).
    ;; Pre-fix: that row was 1 col narrower than the others, every `┃`
    ;; on it drifted, the whole grid broke. Post-fix: every line is
    ;; the same width, the grid is monomorphic.
    (let [headers ["Ikona" "Plik" "Rozmiar" "Typ"]
          rows    [["📄" "AGENTS.md"    "31 KB"   "docs"]
                   ["📝" "CHANGELOG.md" "1.3 KB"  "docs"]
                   ["📋" "CRITIQUE.md"  "43 KB"   "docs"]
                   ["🚧" "GATES.md"     "7 KB"    "docs"]
                   ["📜" "LICENSE"      "11 KB"   "legal"]
                   ["📖" "README.md"    "7.5 KB"  "docs"]
                   ["🏷️" "VERSION"     "6 B"     "config"]   ;; <-- the row that broke
                   ["📁" "bin/"         "985 B"   "bin"]
                   ["🔧" "build.clj"    "13 KB"   "build"]
                   ["📦" "deps.edn"     "5.8 KB"  "config"]
                   ["📁" "docs/"        "8.6 MB"  "docs"]
                   ["🔌" "extensions/"  "83 KB"   "code"]
                   ["📁" "packages/"    "1.2 MB"  "code"]
                   ["🧪" "test/"        "5.2 KB"  "tests"]
                   ["✅" "verify.sh"    "13.5 KB" "scripts"]]
          out     (render-table headers rows 200 dummy-markers)]

      (it "renders the right number of lines (top + header + sep + N rows interspersed + bottom)"
        ;; top + header + head-sep + (N rows + (N-1) row-seps) + bottom
        (let [n (count rows)]
          (expect (= (+ 1 1 1 (+ n (dec n)) 1) (count out)))))

      (it "every line has the exact same display width — the grid is monomorphic"
        ;; With lanterna 3.1.5-vis.3's VS-16 width fix, our model and
        ;; the terminal agree on every emoji width. The grid is
        ;; once again exactly one width across all rows.
        (expect (= 1 (count (visual-widths out)))))

      (it "each non-marker line ends with the right corner glyph for its row type"
        (let [bare (mapv strip-marker out)]
          (expect (str/ends-with? (first bare) "┐"))            ;; top — light corner
          (expect (str/ends-with? (last bare) "┘"))             ;; bottom — light corner
          (expect (every? #(str/ends-with? % "│")
                    (remove #(re-find #"^[┌└├]" %) bare))))))) ;; data lines end with light vertical

  (describe "Single-row table with VS-16 emoji in isolation"
    (it "Header column-width is computed from display-width, not char count"
      (let [out (render-table ["Ikona"] [["🏷️"] ["📄"]] 100 dummy-markers)]
        (expect (= 1 (count (visual-widths out))))))

    (it "Flag emoji (regional indicator pair) — 4 chars, 2 cols"
      (let [out (render-table ["X"] [["🇵🇱"] ["abc"]] 100 dummy-markers)]
        (expect (= 1 (count (visual-widths out))))))

    (it "Mix of CJK (1 char / 2 cols) with ASCII"
      (let [out (render-table ["A" "B"]
                  [["日本語" "abc"]
                   ["x" "y"]]
                  100 dummy-markers)]
        (expect (= 1 (count (visual-widths out))))))))

(defdescribe render-table-cell-wrapping-test
  ;; The user complaint: tables don't auto-wrap. Pre-fix render-table
  ;; truncated long cells with `…` once the column was forced narrow,
  ;; so a `vis ls` table on a 60-col bubble shrunk every filename to
  ;; gibberish. Post-fix every overflowing cell word-wraps onto more
  ;; physical lines inside the same row, and the grid stays
  ;; monomorphic.
  (describe "Cells that don't fit are word-wrapped onto multiple lines"
    (it "narrow bubble: long cell content wraps instead of being truncated"
      ;; 30-col viewport, 2 cols. Natural widths would need ~50 cols.
      ;; Pre-fix: each col shrunk to ~6, content truncated with ….
      ;; Post-fix: each col >= the longest token, content wrapped.
      (let [out  (render-table
                   ["Name" "Description"]
                   [["alpha-module" "the very first module of the system"]
                    ["beta"         "second module, smaller scope"]]
                   30 dummy-markers)
            bare (mapv strip-marker out)]
        ;; Grid stays monomorphic.
        (expect (= 1 (count (visual-widths out))))
        ;; The whole table fits inside max-w.
        (expect (every? #(<= (p/display-width %) 30) bare))
        ;; No truncation ellipsis sneaked in — we wrapped, not cut.
        (expect (not-any? #(str/includes? % "…") bare))
        ;; Every original word is somewhere in the rendered output.
        (let [joined (str/join " " bare)]
          (doseq [w ["alpha-module" "first" "system" "smaller" "scope"]]
            (expect (str/includes? joined w))))))

    (it "wrapping a cell expands the row to multiple physical lines"
      ;; One column, narrow viewport, content longer than the column.
      ;; The single body row should emit > 1 inner line so the cell
      ;; word-wraps vertically.
      (let [out  (render-table
                   ["Note"]
                   [["this is a fairly long sentence that has to wrap"]]
                   20 dummy-markers)
            bare (mapv strip-marker out)
            ;; Inner lines = everything that isn't a horizontal rule.
            inner (filterv #(str/starts-with? % "│") bare)]
        (expect (= 1 (count (visual-widths out))))
        ;; Header (1) + at least 2 wrapped body lines.
        (expect (>= (count inner) 3))
        (expect (every? #(<= (p/display-width %) 20) bare))))

    (it "multi-column row equalises height: every column gets the same number of physical lines"
      ;; Cell-level wrapping must keep the grid aligned: if column A
      ;; wraps to 3 lines, column B (1 line) is padded to 3 lines.
      ;; Verified by counting `│` separators on every row line —
      ;; identical separator counts ⇒ grid intact.
      (let [out  (render-table
                   ["A" "B" "C"]
                   [["one two three four five six seven"
                     "x"
                     "y"]]
                   30 dummy-markers)
            bare (mapv strip-marker out)
            inner (filterv #(str/starts-with? % "│") bare)]
        (expect (= 1 (count (visual-widths out))))
        ;; Every inner row has the same number of `│` (= n-cols + 1).
        (expect (= 1 (count (distinct (map #(count (re-seq #"│" %))
                                        inner)))))))

    (it "naturally-fitting tables do NOT wrap (no behaviour change for short content)"
      ;; Regression guard: the user's pathological table (max-w=200)
      ;; fits naturally and must keep emitting exactly 1 physical
      ;; line per logical row — no surprise expansion.
      (let [out (render-table
                  ["A" "B"]
                  [["x" "y"]
                   ["foo" "bar"]]
                  100 dummy-markers)]
        (expect (= 1 (count (visual-widths out))))
        ;; top + header + sep + row + sep + row + bot = 7 lines.
        (expect (= 7 (count out)))))))

(defdescribe scrollbar-thumb-geometry-test
  ;; Pinning the painter/hit-test contract. Both the message-area
  ;; painter and the input-thread mouse handler must agree on which
  ;; rows belong to the thumb; if they drift, the user clicks "on
  ;; the thumb" and nothing happens, or clicks "off the thumb" and
  ;; the viewport jumps. The pure helper IS the contract.
  (let [g render/scrollbar-thumb-geometry]
    (describe "Returns nil when there's no overflow"
      (it "total-h < inner-h: nothing to scroll"
        (expect (nil? (g 10 20 nil))))
      (it "total-h == inner-h: nothing to scroll"
        (expect (nil? (g 20 20 nil))))
      (it "inner-h is zero: no viewport, no thumb"
        (expect (nil? (g 100 0 0)))))

    (describe "Standard 100/20 conversation"
      (it "Auto-bottom (scroll=nil) places thumb at the END of the track"
        (let [{:keys [thumb-top-rel thumb-h max-scroll]} (g 100 20 nil)]
          (expect (= 16 thumb-top-rel))   ;; track-h(20) - thumb-h(4) = 16
          (expect (= 4 thumb-h))           ;; 20 * 20/100
          (expect (= 80 max-scroll))))    ;; 100 - 20

      (it "scroll=0 places thumb at the TOP"
        (expect (= {:thumb-top-rel 0 :thumb-h 4 :max-scroll 80}
                  (g 100 20 0))))

      (it "scroll=40 places thumb in the MIDDLE of the free track"
        (expect (= {:thumb-top-rel 8 :thumb-h 4 :max-scroll 80}
                  (g 100 20 40))))

      (it "scroll=80 places thumb at the BOTTOM (== max-scroll)"
        (expect (= {:thumb-top-rel 16 :thumb-h 4 :max-scroll 80}
                  (g 100 20 80)))))

    (describe "Out-of-range scroll values are clamped"
      (it "Negative scroll clamps to 0 (top)"
        (expect (zero? (:thumb-top-rel (g 100 20 -50)))))
      (it "Excessive scroll clamps to max-scroll (bottom)"
        (let [{:keys [thumb-top-rel max-scroll]} (g 100 20 9999)]
          (expect (= 16 thumb-top-rel))
          (expect (= 80 max-scroll)))))

    (describe "Tiny viewport on huge content keeps thumb-h >= 1"
      ;; If `inner-h * track-h / total-h` rounds to 0, we still draw
      ;; SOMETHING — a 1-row thumb — so the user has a target to grab.
      (it "1000-row content in a 5-row viewport: thumb-h is 1, not 0"
        (let [{:keys [thumb-h]} (g 1000 5 0)]
          (expect (= 1 thumb-h))))
      (it "And the thumb still slides through the full track"
        (let [top (:thumb-top-rel (g 1000 5 0))
              bot (:thumb-top-rel (g 1000 5 995))]
          (expect (= 0 top))
          (expect (= 4 bot)))))    ;; track-h(5) - thumb-h(1) = 4
    ))

;; ─────────────────────────────────────────────────────────────────────────
;; <details> / <summary> rendering
;;
;; Pre-fix the TUI markdown pipeline had no branch for HTML block tags,
;; so the lines emitted by `(md/details (md/summary …) body)` rendered
;; literally as `<details>`, `<summary>Click</summary>`, `</details>`
;; in the chat bubble — three rows of raw HTML the user can't act on.
;; The renderer now drops the framing tags and paints the summary as
;; a bold `▾ label` disclosure heading. True click-to-collapse still
;; needs per-bubble state + click regions; this fix is the visual
;; baseline.
;; ─────────────────────────────────────────────────────────────────────────

(defdescribe details-summary-rendering-test
  (describe "`<details>` / `</details>` framing tags are dropped"
    (it "`<details>` line emits no row"
      ;; If the renderer ever leaks the framing tag back into the
      ;; output we'll see a PLAIN `<details>` row reappear — this
      ;; pins the drop.
      (let [lines (md->lines "<details>\n<summary>L</summary>\n\nbody\n\n</details>" 60)]
        (expect (not-any? #(str/includes? % "<details>") lines))
        (expect (not-any? #(str/includes? % "</details>") lines))))

    (it "`<details open>` (with attribute) is also dropped"
      (let [lines (md->lines "<details open>\nbody\n</details>" 60)]
        (expect (not-any? #(str/includes? % "<details") lines))
        (expect (not-any? #(str/includes? % "</details>") lines))))

    (it "body between framing tags renders as normal markdown"
      ;; The whole point of stripping the wrappers is so the inner
      ;; markdown still flows through the existing pipeline. A bullet
      ;; inside <details> must still come out with the bullet marker.
      (let [lines (md->lines "<details>\n<summary>L</summary>\n\n- item\n\n</details>" 60)
            bullet-line (first (filter #(= p/MARKER_MD_BULLET (marker-of %)) lines))]
        (expect (some? bullet-line))
        (expect (str/includes? bullet-line "item")))))

  (describe "`<summary>label</summary>` renders on its own SUMMARY band"
    ;; The summary line carries `MARKER_MD_SUMMARY` (distinct from
    ;; `MARKER_MD_BOLD`) so the painter can paint it with a tinted
    ;; lavender band — the visual cue that the section is a
    ;; disclosure region. Bolt-on `▾ ` glyph stays so the user reads
    ;; the band as an expanded-disclosure header even before they
    ;; notice the colour.
    (it "emits a SUMMARY-marked row prefixed with `▾ `"
      (let [[line] (filter #(= p/MARKER_MD_SUMMARY (marker-of %))
                     (md->lines "<summary>Click to expand</summary>" 60))]
        (expect (some? line))
        (expect (str/starts-with? (body-of line) "▾ "))
        (expect (str/includes? line "Click to expand"))))

    (it "summary marker is distinct from BOLD (own painter zone)"
      ;; Pinning that a regression that mapped `:summary` back to
      ;; `:bold` (the painter would lose the lavender band) shows up
      ;; as a test failure here, not as a silent visual regression.
      (let [lines (md->lines "<summary>Logs</summary>" 60)]
        (expect (some #(= p/MARKER_MD_SUMMARY (marker-of %)) lines))
        (expect (not-any? #(= p/MARKER_MD_BOLD (marker-of %)) lines))))

    (it "thinking-mode summary uses TH_MD_SUMMARY marker"
      ;; <details> can land inside an iteration's `:thinking` block
      ;; just like in the final answer; the dim variant of the band
      ;; keeps the reasoning zone cohesive.
      (let [[line] (filter #(= p/MARKER_TH_MD_SUMMARY (marker-of %))
                     (md->lines "<summary>Plan</summary>" 60 :thinking))]
        (expect (some? line))
        (expect (str/starts-with? (body-of line) "▾ "))))

    (it "inner inline markdown is honoured (`**bold** inside summary`)"
      ;; `<summary>**Logs**</summary>` should keep the inner bold
      ;; styling, same as a heading body would. The renderer routes
      ;; the inner content through `markdown->inline` so the bold
      ;; sentinels show up alongside the visible text.
      (let [[line] (filter #(= p/MARKER_MD_SUMMARY (marker-of %))
                     (md->lines "<summary>**Logs**</summary>" 60))]
        (expect (str/includes? line p/INLINE_BOLD_ON))
        (expect (str/includes? line "Logs"))
        (expect (str/includes? line p/INLINE_BOLD_OFF)))))

  (describe "full `(md/details (md/summary …) body)` shape round-trips"
    ;; Mirrors the actual emit shape `vis-foundation/markdown.clj`
    ;; produces. Pre-fix the user saw three rows of raw HTML; post-fix
    ;; only the disclosure label + body remain.
    (it "summary label carries SUMMARY marker, body lines are PLAIN, framing tags absent"
      (let [src    (str "<details>\n<summary>Logs</summary>\n\n"
                     "Hidden text.\n\n</details>")
            lines  (md->lines src 60)
            text   (str/join "\n" lines)]
        (expect (not (str/includes? text "<details")))
        (expect (not (str/includes? text "<summary")))
        (expect (some #(and (= p/MARKER_MD_SUMMARY (marker-of %))
                         (str/includes? % "Logs")) lines))
        ;; `:answer` mode uses an empty-string marker for PLAIN lines
        ;; (see `md-marker-sets`). `marker-of` returns the FIRST char
        ;; of the row, so for plain lines it's the first content char
        ;; (`H`), not a known styling marker. We assert via
        ;; "present and not styled by any known marker".
        (let [styled-markers #{p/MARKER_MD_H1 p/MARKER_MD_H2 p/MARKER_MD_H3
                               p/MARKER_MD_BOLD p/MARKER_MD_CODE
                               p/MARKER_MD_BULLET p/MARKER_MD_QUOTE
                               p/MARKER_MD_HR  p/MARKER_MD_SUMMARY}]
          (expect (some #(and (str/includes? % "Hidden text.")
                           (not (contains? styled-markers (marker-of %))))
                    lines)))))))

;; ─────────────────────────────────────────────────────────────────────────
;; Link-chrome strip — strip inline emphasis from anchor text
;;
;; `parse-md-refs` captures whatever sits between `[…]` raw, so
;; `[See **here**](url)` flows into the chrome row with literal `**`
;; baked in: "🔗 See **here** → url". The renderer now runs the
;; anchor text through `markdown->inline` and strips the styling
;; sentinels, leaving plain visible text in the chrome.
;; ─────────────────────────────────────────────────────────────────────────

(def ^:private chrome-display-text @#'render/chrome-display-text)

(defn- chrome-of [src]
  (let [[ref] (links/parse-md-refs src)]
    (chrome-display-text ref 80)))

(defdescribe chrome-display-text-strips-inline-markup-test
  (describe "emphasis inside link text doesn't leak into chrome row"
    (it "`[See **here**](url)` → no `**` in chrome"
      (let [s (chrome-of "[See **here**](https://example.com)")]
        (expect (str/includes? s "See here"))
        (expect (not (str/includes? s "**")))))

    (it "`[Spec *v2*](url)` → no stray `*` in chrome"
      (let [s (chrome-of "[Spec *v2*](https://example.com)")]
        (expect (str/includes? s "Spec v2"))
        (expect (not (str/includes? s "*v2*")))))

    (it "``[Title with `code`](url)`` → backticks stripped from chrome"
      (let [s (chrome-of "[Title with `code`](https://example.com)")]
        (expect (str/includes? s "Title with code"))
        (expect (not (str/includes? s "`code`"))))))

  (describe "emphasis OUTSIDE the brackets is unaffected (regression net)"
    ;; `**[link](url)**` — bold lives outside the bracket, so
    ;; `parse-md-refs` already captures clean text. This test pins
    ;; that the new strip doesn't accidentally mangle the URL or icon.
    (it "`**[link](url)**` → chrome reads `link` cleanly"
      (let [s (chrome-of "**[link](https://example.com)**")]
        (expect (str/includes? s "link"))
        (expect (str/includes? s "https://example.com"))
        (expect (not (str/includes? s "**"))))))

  (describe "chrome icon + url tail still render after the strip"
    ;; Smoke test that the strip didn't drop the leading icon or the
    ;; ` → ` separator. Without these the affordance loses its
    ;; "this is a link" cue.
    (it "icon + arrow + url all present"
      (let [s (chrome-of "[plain](https://example.com)")]
        (expect (str/includes? s " → "))
        (expect (str/includes? s "https://example.com"))))))

;; ─────────────────────────────────────────────────────────────────────────
;; Loose-bullet coalesce — multi-paragraph list items render as one bullet
;;
;; Poorly-formatted markdown (LLM output, hand-edited prose) often
;; emits list items whose body has been fragmented across blank
;; lines:
;;
;;     - `dialogs.clj`
;;
;;      — removed `:system-prompt` palette command entry
;;     - `screen.clj`
;;
;;      — removed `:system-prompt` handler
;;
;; CommonMark spec-compliantly treats every fragment as its own
;; loose paragraph: only `dialogs.clj` lands under the bullet
;; marker, and `— removed …` shows up flush-left as if it weren't
;; part of the bullet at all. The TUI bubble has nowhere to render
;; that hierarchy correctly.
;;
;; `coalesce-loose-list-items` (the pre-pass) folds every fragment
;; back into the bullet's text on a single line. Tests below pin
;; the specific shapes the user reported.
;; ─────────────────────────────────────────────────────────────────────────

(def ^:private coalesce-loose-list-items @#'render/coalesce-loose-list-items)

(defn- bullet-bodies
  "Return the visible body of every bullet line in a rendered output.
   Matches `:answer` mode where the marker is `MARKER_MD_BULLET`."
  [text bubble-w]
  (->> (md->lines text bubble-w :answer)
    (filter #(= p/MARKER_MD_BULLET (marker-of %)))
    (mapv body-of)))

(defn- strip-sentinels
  "Drop inline-style sentinels (PUA U+E110..U+E2FF) so equality
   assertions compare the visible text only."
  [s]
  (->> s
    (remove #(<= 0xE110 (int %) 0xE2FF))
    (apply str)))

(defdescribe loose-bullet-coalesce-test
  (describe "blank-line-fragmented bullets render as a single bullet line"
    (it "joins one bullet's three paragraphs into one bullet body"
      (let [src (str "- `dialogs.clj`\n"
                  "\n"
                  " — removed `:system-prompt` palette command entry")
            bullets (bullet-bodies src 80)]
        ;; Exactly ONE bullet, not three top-level paragraphs.
        (expect (= 1 (count bullets)))
        (let [body (strip-sentinels (first bullets))]
          ;; The whole sentence reads as one line (modulo wrapping).
          (expect (str/includes? body "dialogs.clj"))
          (expect (str/includes? body "— removed"))
          (expect (str/includes? body ":system-prompt"))
          (expect (str/includes? body "palette command entry")))))

    (it "two consecutive bullets each get a single-line body"
      (let [src (str "- `dialogs.clj`\n"
                  "\n"
                  " — removed `:system-prompt` palette command entry\n"
                  "- `screen.clj`\n"
                  "\n"
                  " — removed `:system-prompt` handler (viewer dialog)")
            bullets (bullet-bodies src 80)]
        (expect (= 2 (count bullets)))
        (expect (str/includes? (strip-sentinels (nth bullets 0)) "dialogs.clj"))
        (expect (str/includes? (strip-sentinels (nth bullets 0)) "palette command entry"))
        (expect (str/includes? (strip-sentinels (nth bullets 1)) "screen.clj"))
        (expect (str/includes? (strip-sentinels (nth bullets 1)) "viewer dialog"))))

    (it "regression for the user's exact report — six fragmented bullets coalesce to six"
      (let [src (str "## Pruned: System Prompt Copy / Inspector\n"
                  "\n"
                  "Removed the  `Inspect Latest System Prompt`  feature from TUI + core:\n"
                  "\n"
                  "- `dialogs.clj`\n"
                  "\n"
                  " — removed \n"
                  "\n"
                  "`:system-prompt`\n"
                  "\n"
                  " palette command entry\n"
                  "- `screen.clj`\n"
                  "\n"
                  " — removed \n"
                  "\n"
                  "`:system-prompt`\n"
                  "\n"
                  " handler (viewer dialog)\n"
                  "- `core.clj`\n"
                  "\n"
                  " — removed \n"
                  "\n"
                  "`effective-system-prompt`\n"
                  "\n"
                  " and \n"
                  "\n"
                  "`effective-system-prompt-for-query`\n"
                  "\n"
                  " exports + docstring mention\n"
                  "- `loop.clj`\n"
                  "\n"
                  " — removed both function definitions (\n"
                  "\n"
                  "`effective-system-prompt`\n"
                  "\n"
                  ", \n"
                  "\n"
                  "`effective-system-prompt-for-query`\n"
                  "\n"
                  ")\n"
                  "- `CLAUDE.md`\n"
                  "\n"
                  " — removed two references\n"
                  "- `docs/src/extensions/overview.md`\n"
                  "\n"
                  " — updated paragraph about inspector\n"
                  "\n"
                  "**Loop clean: ** true\n"
                  "\n"
                  "**No remaining source references.**")
            lines  (md->lines src 80 :answer)
            bullets (bullet-bodies src 80)]
        ;; SIX bullets in, SIX bullet items out. Each one carries the
        ;; signature of its first source code-span so we can pin the
        ;; mapping bullet-by-bullet.
        (let [first-bodies (->> bullets
                             (map strip-sentinels)
                             ;; Some items wrap onto multiple lines;
                             ;; pick only those with the ` • ` prefix
                             ;; that opens a fresh item, not the
                             ;; continuation indent.
                             (filter #(str/starts-with? % "  • "))
                             vec)]
          (expect (= 6 (count first-bodies)))
          (expect (str/includes? (nth first-bodies 0) "dialogs.clj"))
          (expect (str/includes? (nth first-bodies 1) "screen.clj"))
          (expect (str/includes? (nth first-bodies 2) "core.clj"))
          (expect (str/includes? (nth first-bodies 3) "loop.clj"))
          (expect (str/includes? (nth first-bodies 4) "CLAUDE.md"))
          (expect (str/includes? (nth first-bodies 5) "overview.md")))
        ;; The closing summary lines must NOT be folded into the
        ;; last bullet — they're top-level bold paragraphs.
        (let [bold-lines (->> lines
                           (filter #(= p/MARKER_MD_BOLD (marker-of %)))
                           (map (comp strip-sentinels body-of)))]
          (expect (some #(str/includes? % "Loop clean")          bold-lines))
          (expect (some #(str/includes? % "No remaining source") bold-lines))))))

  (describe "structural lines close an open list scope"
    (it "heading after a fragmented bullet doesn't get sucked into it"
      (let [src (str "- foo\n"
                  "\n"
                  "continuation\n"
                  "\n"
                  "## A new heading")
            lines (md->lines src 80 :answer)
            bullets (bullet-bodies src 80)]
        (expect (= 1 (count bullets)))
        (expect (str/includes? (strip-sentinels (first bullets)) "foo continuation"))
        (expect (some #(and (= p/MARKER_MD_H2 (marker-of %))
                         (str/includes? (body-of %) "A new heading"))
                  lines))))

    (it "code fence after a fragmented bullet stays a fence"
      (let [src (str "- foo\n"
                  "\n"
                  "still bullet\n"
                  "\n"
                  "```clj\n"
                  "(+ 1 2)\n"
                  "```\n")
            lines (md->lines src 80 :answer)
            bullets (bullet-bodies src 80)]
        (expect (= 1 (count bullets)))
        (expect (str/includes? (strip-sentinels (first bullets)) "foo still bullet"))
        (expect (some #(and (= p/MARKER_MD_CODE (marker-of %))
                         (str/includes? (body-of %) "(+ 1 2)"))
                  lines))))

    (it "bold-only paragraph after a bullet does not get coalesced"
      ;; This is the regression that broke `**Loop clean:**` etc.
      (let [src (str "- foo\n"
                  "\n"
                  "**Status:** done.")
            lines (md->lines src 80 :answer)
            bullets (bullet-bodies src 80)]
        (expect (= 1 (count bullets)))
        (expect (str/includes? (strip-sentinels (first bullets)) "foo"))
        (expect (not (str/includes? (strip-sentinels (first bullets)) "Status")))
        ;; Either MARKER_MD_BOLD (when the line is purely bold) or a
        ;; plain line carrying inline-bold sentinels — both prove it
        ;; is no longer part of the bullet.
        (expect (some #(str/includes? (strip-sentinels %) "Status")
                  lines)))))

  (describe "two consecutive blank lines close the list (CommonMark loose-list end)"
    (it "second paragraph after double-blank doesn't fold into bullet"
      (let [src (str "- foo\n"
                  "\n"
                  "still item\n"
                  "\n"
                  "\n"
                  "fresh paragraph")
            bullets (bullet-bodies src 80)]
        (expect (= 1 (count bullets)))
        (expect (str/includes? (strip-sentinels (first bullets)) "foo still item"))
        (expect (not (str/includes? (strip-sentinels (first bullets)) "fresh paragraph"))))))

  (describe "punctuation reflow on coalesce"
    (it "stray spaces around () , and . are tightened back to prose form"
      (let [src (str "- foo (\n"
                  "\n"
                  "bar\n"
                  "\n"
                  ", baz\n"
                  "\n"
                  ")\n")
            bullets (bullet-bodies src 80)]
        (expect (= 1 (count bullets)))
        ;; "( bar" → "(bar", "bar ," → "bar,", "baz )" → "baz)"
        (expect (str/includes? (strip-sentinels (first bullets)) "foo (bar, baz)")))))

  (describe "well-formed input still passes through cleanly"
    (it "single-line bullets are not altered by the pre-pass"
      ;; Pure pass-through — the coalesce shouldn't ever change the
      ;; shape of a bullet that's already on one line.
      (let [src "- alpha\n- beta\n- gamma"
            bullets (bullet-bodies src 80)]
        (expect (= 3 (count bullets)))
        (expect (str/includes? (strip-sentinels (nth bullets 0)) "alpha"))
        (expect (str/includes? (strip-sentinels (nth bullets 1)) "beta"))
        (expect (str/includes? (strip-sentinels (nth bullets 2)) "gamma"))))

    (it "lines outside any list are not touched"
      ;; `coalesce-loose-list-items` runs on the raw line vec; assert
      ;; non-list input round-trips unchanged.
      (let [in  ["plain prose" "" "more prose" "" "## heading"]
            out (coalesce-loose-list-items in)]
        (expect (= in out))))))
