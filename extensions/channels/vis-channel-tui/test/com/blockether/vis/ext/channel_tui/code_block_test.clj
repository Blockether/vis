(ns com.blockether.vis.ext.channel-tui.code-block-test
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.render :as render]
            [com.blockether.vis.ext.channel-tui.render-ir :as ir]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  tui-code-block-test
  (it "keeps code text visible in markdown formatting"
      (let [out (render/format-answer-markdown [:ir {} [:code {:lang "clojure"} "(+ 1 2)"]] 80)]
        (expect (string? out))
        (expect (re-find #"\(\+ 1 2\)" out)))))

;; --- soft-wrap (`:wrap?`) ---------------------------------------------------
;; Regression for the "wide tool output overflows the bubble" thread (session
;; d5493dc9): a pathologically wide single-line value must fold at the bubble
;; edge when the code block carries `:wrap?`, while staying verbatim without it.

(def ^:private code-block->lines @#'ir/code-block->lines)

(defn- content-rows
  "Visible (non-pad) rows from `code-block->lines`, as concatenated text."
  [lines]
  (->> lines
       (map (fn [line]
              (apply str (map :text (:runs line)))))
       (remove #(= "" %))
       vec))

(defdescribe tui-code-block-wrap-test
             (it "soft-folds a wide one-line value to the budget when :wrap? is set"
                 ;; A 200-col one-liner like a wide clj_eval value map / long git_commit arg.
                 (let [wide
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
             (it "keeps a wide value verbatim (one overflowing row) without :wrap?"
                 (let [wide
                       (apply str (repeat 100 "ab"))

                       width
                       40

                       lines
                       (code-block->lines [:code {:lang "clojure"} wide] width {})

                       rows
                       (content-rows lines)]

                   (expect (= 1 (count rows)))
                   (expect (= wide (first rows)))
                   (expect (> (p/display-width (first rows)) width))))
             (it "preserves leading indentation when folding"
                 (let [src
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
