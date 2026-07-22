(ns com.blockether.vis.ext.channel-tui.code-block-test
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.render :as render]
            [com.blockether.vis.ext.channel-tui.highlight :as highlight]
            [com.blockether.vis.ext.channel-tui.markdown-layout :as layout]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  tui-code-block-test
  (it "keeps code text visible in markdown formatting"
      (let [out (render/format-answer-markdown [:ast {} [:code {:lang "clojure"} "(+ 1 2)"]] 80)]
        (expect (string? out))
        (expect (re-find #"\(\+ 1 2\)" out)))))

;; --- soft-wrap (`:wrap?`) ---------------------------------------------------
;; Regression for the "wide tool output overflows the bubble" thread (session
;; d5493dc9): a pathologically wide single-line value must fold at the bubble
;; edge when the code block carries `:wrap?`. A colorized (real-grammar)
;; fence now folds SGR-aware too, so a wide highlighted line stops overflowing.

(def ^:private code-block->lines @#'layout/code-block->lines)

(def ^:private folded-highlight-cache @#'layout/folded-highlight-cache)

(defn- content-rows
  "Visible (non-pad) rows from `code-block->lines`, as concatenated text."
  [lines]
  (->> lines
       (map (fn [line]
              (apply str (map :text (:runs line)))))
       (remove #(= "" %))
       vec))

(defdescribe
  tui-code-block-wrap-test
  (it "soft-folds a wide one-line value to the budget when :wrap? is set"
      ;; A 200-col one-liner like a wide clj_eval value map / long git_commit arg.
      (let
        [wide
         (apply str (repeat 100 "ab"))

         width
         40

         lines
         (code-block->lines [:code {:lang "clojure" :wrap? true} wide] width {})

         rows
         (content-rows lines)]

        ;; every produced row fits the bubble...
        (expect (every? #(<= (p/display-width %) width) rows))
        ;; ...it actually folded into several rows...
        (expect (> (count rows) 1))
        ;; ...and the content is preserved byte-for-byte (soft wrap, no reflow).
        (expect (= wide (apply str rows)))))
  (it "ansi-char-folds a wide colorized (real-grammar) value to the budget"
      ;; A ```clojure fence no longer overflows: each over-wide row
      ;; folds at the bubble edge, SGR-aware, so its tail stays
      ;; visible instead of being clipped off the right edge.
      (let
        [strip-ansi
         (fn [s]
           (str/replace s #"\u001b\[[0-9;]*m" ""))

         wide
         (apply str (repeat 100 "ab"))

         width
         40

         lines
         (code-block->lines [:code {:lang "clojure"} wide] width {})

         rows
         (mapv strip-ansi (content-rows lines))]

        ;; every visible row fits the bubble...
        (expect (every? #(<= (p/display-width %) width) rows))
        ;; ...it actually folded into several rows...
        (expect (> (count rows) 1))
        ;; ...and the visible content is preserved byte-for-byte.
        (expect (= wide (apply str rows)))))
  (it "preserves leading indentation when folding"
      (let
        [src
         "    {\"message\": \"Fix live tab title update in the TUI\", \"all\": true}"

         width
         24

         lines
         (code-block->lines [:code {:lang "clojure" :wrap? true} src] width {})

         rows
         (content-rows lines)]

        (expect (every? #(<= (p/display-width %) width) rows))
        (expect (= src (apply str rows)))
        (expect (str/starts-with? (first rows) "    {")))))

(defdescribe tui-cat-code-highlight-test
             (it "highlights CAT source while dimming line-number gutters and dividers"
                 (let
                   [src
                    "   9  (def x 1)\n1200  (inc x)\n   ⋯\n1300  (str x)"

                    lines
                    (code-block->lines [:code {:lang "clojure"} src] 80 {})

                    rows
                    (content-rows lines)

                    strip-ansi
                    #(str/replace % #"\u001b\[[0-9;]*m" "")]

                   (expect (= (str/split-lines src) (mapv strip-ansi rows)))
                   (expect (str/starts-with? (first rows) "\u001b[90m   9  \u001b[0m"))
                   (expect (str/includes? (subs (first rows) 15) "\u001b["))
                   (expect (= "\u001b[90m   ⋯\u001b[0m" (nth rows 2)))))
             (it "falls back to plain CAT rows when the native highlighter is unavailable"
                 (let
                   [src
                    "1  (def x 1)\n2  (inc x)"

                    rows
                    (with-redefs [highlight/highlight (constantly nil)]
                      (content-rows (code-block->lines [:code {:lang "clojure"} src] 80 {})))]

                   (expect (= (str/split-lines src) rows))))
             (it "reuses completed CAT highlighting and ANSI folding at the same width"
                 (let
                   [src
                    "901  (def cache-proof 1)\n902  (inc cache-proof)"

                    fold-calls
                    (atom 0)]

                   (.clear ^java.util.Map folded-highlight-cache)
                   (with-redefs
                     [highlight/highlight
                      (fn [_grammar source]
                        source)

                      p/ansi-fold-cols
                      (fn [line ^long _budget]
                        (swap! fold-calls inc)
                        [line])]

                     (code-block->lines [:code {:lang "clojure"} src] 80 {})
                     (code-block->lines [:code {:lang "clojure"} src] 80 {})
                     (expect (= 2 @fold-calls))
                     (code-block->lines [:code {:lang "clojure"} src] 79 {})
                     (expect (= 4 @fold-calls))))))

;; --- plain-fence char-fold (no `:lang`) -------------------------------------
;; Regression for the "can't see the full bookmarklet" thread: a plain fence
;; (no grammar) with a pathologically wide single line must char-fold to the
;; bubble width so its tail stays visible, instead of overflowing off the right
;; edge with no wrap and no horizontal scroll.

(defdescribe tui-code-block-plain-fold-test
             (it "char-folds a wide plain (no-lang) line to the budget"
                 (let
                   [wide
                    (str "javascript:(function(){" (apply str (repeat 100 "ab")) "})();")

                    width
                    40

                    lines
                    (code-block->lines [:code {} wide] width {})

                    rows
                    (content-rows lines)]

                   ;; every produced row fits the bubble...
                   (expect (every? #(<= (p/display-width %) width) rows))
                   ;; ...it actually folded into several rows...
                   (expect (> (count rows) 1))
                   ;; ...and the content is preserved byte-for-byte.
                   (expect (= wide (apply str rows)))))
             (it "leaves a plain line that already fits untouched (one row)"
                 (let
                   [src
                    "a short plain line"

                    lines
                    (code-block->lines [:code {} src] 40 {})

                    rows
                    (content-rows lines)]

                   (expect (= [src] rows)))))
