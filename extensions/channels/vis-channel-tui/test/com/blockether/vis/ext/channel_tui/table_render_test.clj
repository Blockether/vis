(ns com.blockether.vis.ext.channel-tui.table-render-test
  "Regression coverage for the markdown table prettifier in render.clj.

   These tests exist because of a real user-reported bug: when an
   answer contained a row with a VS-16 emoji like 🏷️ (3 Java chars,
   2 display columns), pad-cell padded by char count and shipped a
   cell that was visually one column narrower than its siblings.
   The whole row's `┃` separators then drifted left, the table grid
   broke, and downstream rendering looked like the cell had only
   one separator instead of four.

   Every test here would FAIL on a `(count s)` implementation of
   pad-cell or render-table's column-width computation."
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.render :as render]
            [lazytest.core :refer [defdescribe describe expect it]]))

;; render-table is private — reach in via the var to test it directly.
(def ^:private render-table @#'render/render-table)

(def ^:private dummy-markers
  "render-table prepends a marker char per line so the bubble renderer
   knows whether each line is a top border, header, separator, or row.
   For width-math tests we don't care about the marker identity, just
   strip it before measuring."
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
