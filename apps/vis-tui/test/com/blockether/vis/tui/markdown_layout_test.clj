(ns com.blockether.vis.tui.markdown-layout-test
  "Tests for the canonical-IR → styled-line walker.

   Two layers of coverage:

   1. Unit tests on small hand-crafted IR fragments — soft-break
      collapse already happened in `ir/->ast`, so the walker only
      needs to wrap, indent, and propagate styles correctly.

   2. End-to-end fixture from session `bdc79ae9` — the LLM
      output that produced the 3-space hanging indent in the TUI.
      Asserts the bug is structurally impossible on the new path."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.markdown-layout :as layout]
            [com.blockether.vis.tui.presentation :as ir]
            [lazytest.core :refer [defdescribe expect it]]))

;; small helpers

(defn- texts
  [lines]
  (mapv (fn [l]
          (apply str
            (map (fn [r]
                   (or (:text r) ""))
                 (:runs l))))
        lines))

(defn- styled?
  [lines kw]
  (some (fn [l]
          (some (fn [r]
                  (contains? (or (:style r) #{}) kw))
                (:runs l)))
        lines))

(defn- markers [strings] (mapv #(subs % 0 1) strings))

(defn- bodies [strings] (mapv #(subs % 1) strings))

;; basic blocks

(defdescribe paragraph-test
             (it "paragraph emits one line for short content"
                 (let [lines (layout/ast->lines [:ast [:p "hello world"]] 80)]
                   (expect (= ["hello world"] (texts lines)))))
             (it "paragraph wraps at width on word boundary"
                 (let [lines (layout/ast->lines [:ast [:p "lorem ipsum dolor sit amet"]] 12)]
                   (expect (every? (fn [t]
                                     (<= (count t) 12))
                                   (texts lines))
                           (str "got: " (texts lines)))
                   (expect (= "lorem ipsum dolor sit amet"
                              (str/replace (str/join " " (texts lines)) #"\s+" " ")))))
             (it "inline code keeps :code style on the word"
                 (let [lines (layout/ast->lines [:ast [:p "use " [:c "send!"] " here"]] 80)]
                   (expect (= ["use send! here"] (texts lines)))
                   (expect (styled? lines :code))))
             (it "strong + em propagate style flags"
                 (let [lines (layout/ast->lines [:ast [:p [:strong "bold"] " and " [:em "ital"]]]
                                                80)]
                   (expect (styled? lines :bold))
                   (expect (styled? lines :italic)))))

;; lists

;; heading hierarchy

;; A terminal has ONE font size. Without a rule + gutter ladder every
;; heading level collapses onto the slate colour ramp alone, and `##`
;; renders indistinguishably from `###` (and H4-H6 from either).
(defdescribe
  heading-hierarchy-test
  (it "H1 is underlined by a heavy rule spanning the full width"
      (let [ls (remove str/blank? (texts (layout/ast->lines [:ast [:h {:level 1} "Title"]] 20)))]
        (expect (= ["Title" (apply str (repeat 20 \━))] (vec ls)))))
  (it "H2 is underlined by a light rule sized to the heading text"
      (let [ls (remove str/blank? (texts (layout/ast->lines [:ast [:h {:level 2} "Problem"]] 20)))]
        (expect (= ["Problem" (apply str (repeat 7 \─))] (vec ls)))))
  (it "H3-H6 carry distinct gutter marks and no rule"
      (let [head
            #(first (remove str/blank? (texts (layout/ast->lines [:ast [:h {:level %} "x"]] 40))))

            marks
            (mapv head [3 4 5 6])]

        (expect (= ["▍ x" "▸ x" "· x" "  · x"] marks))
        (expect (= 4 (count (distinct marks))))))
  (it "wrapped heading lines hang under their gutter mark"
      (let [ls (texts (layout/ast->lines [:ast [:h {:level 3} "aaaa bbbb cccc"]] 10))]
        (expect (str/starts-with? (first ls) "▍ "))
        (expect (str/starts-with? (second ls) "  ")))))

(defdescribe list-test
             (it "ul renders '- ' marker per item"
                 (let [lines
                       (layout/ast->lines [:ast [:ul [:li "a"] [:li "b"]]] 80)

                       ts
                       (texts lines)]

                   (expect (some #(= "- a" %) ts))
                   (expect (some #(= "- b" %) ts))))
             (it "ol numbers items starting at 1"
                 (let [lines
                       (layout/ast->lines [:ast [:ol [:li "x"] [:li "y"]]] 80)

                       ts
                       (texts lines)]

                   (expect (some #(= "1. x" %) ts))
                   (expect (some #(= "2. y" %) ts))))
             (it "ol counts from the number the list starts on"
                 (let [lines
                       (layout/ast->lines [:ast [:ol {:start 8} [:li "eight"] [:li "nine"]]] 80)

                       ts
                       (texts lines)]

                   (expect (some #(= "8. eight" %) ts))
                   (expect (some #(= "9. nine" %) ts))))
             ;; User report (screenshot): steps split by fenced code blocks end the list,
             ;; so CommonMark hands the walker `2.` and `3.` as lists of their own — every
             ;; step painted "1." and the reader could not tell the steps apart.
             (it "ol keeps counting through steps split by a code fence"
                 (let [md
                       (str "1. add the allowance:\n\n```\nsqs:ListQueues\n```\n\n"
                            "2. add the action:\n\n```\nsqs:PurgeQueue\n```\n\n"
                            "3. keep the restrictions.")

                       ts
                       (texts (layout/ast->lines (ir/markdown->ast md) 80))]

                   (expect (some #(= "1. add the allowance:" %) ts))
                   (expect (some #(= "2. add the action:" %) ts))
                   (expect (some #(= "3. keep the restrictions." %) ts))))
             ;; The companion hangs every item of one list off the same column
             ;; (`--marker-column` in index.css); "9." and "10." used to start their
             ;; text one column apart here.
             (it "ol hangs every item off the column its widest number needs"
                 (let [md
                       "9. nine\n10. ten\n11. eleven"

                       ts
                       (texts (layout/ast->lines (ir/markdown->ast md) 80))]

                   (expect (some #(= "9.  nine" %) ts))
                   (expect (some #(= "10. ten" %) ts))
                   (expect (some #(= "11. eleven" %) ts))))
             (it "ul renders GFM task-list markers as checklist glyphs"
                 (let [lines
                       (layout/ast->lines [:ast
                                           [:ul [:li "[x] Completed item"] [:li "[ ] Pending item"]
                                            [:li "[X] Also completed"]]]
                                          80)

                       ts
                       (texts lines)]

                   (expect (= ["☑️  Completed item" "⬜ Pending item" "☑️  Also completed"] ts))))
             (it "task-list continuations indent by display width, not char count"
                 (let [lines
                       (layout/ast->lines [:ast [:ul [:li "[ ] Pending item wraps here"]]] 14)

                       ts
                       (texts lines)]

                   (expect (= ["⬜ Pending" "   item wraps" "   here"] ts))
                   (expect (every? #(<= (p/display-width %) 14) ts))))
             (it "does not crash when a list item starts with a non-text inline node"
                 (let [lines
                       (layout/ast->lines [:ast [:ul [:li [:br] "after break"]]] 80)

                       ts
                       (texts lines)]

                   (expect (some #(str/includes? % "after break") ts))))
             (it "wrapped li uses hanging indent equal to marker width (NOT 3 spaces)"
                 ;; This is the regression target: pre-IR code produced "   foo" continuation.
                 (let [lines
                       (layout/ast->lines [:ast
                                           [:ul
                                            [:li "short " [:c "code-token"] " then a long tail "
                                             "that will force wrapping at the chosen width"]]]
                                          30)

                       ts
                       (texts lines)

                       continuations
                       (rest ts)]

                   (expect (every? #(str/starts-with? % "  ") continuations)
                           (str "expected 2-space hanging indent, got: " (vec continuations)))
                   (expect (not-any? #(re-matches #"^   \S.*" %) continuations)
                           (str "no continuation may begin with 3-space indent: "
                                (vec continuations))))))

;; code blocks

(defdescribe code-block-test
             (it "code block preserves whitespace verbatim, never wraps"
                 (let [src
                       "(let [x 1]\n  (println x))"

                       lines
                       (layout/ast->lines [:ast [:code {:lang "clj"} src]] 40)

                       ts
                       (mapv #(str/replace (or % "") #"\u001b\[[0-9;]*m" "") (texts lines))]

                   (expect (some #(= "(let [x 1]" %) ts))
                   (expect (some #(= "  (println x))" %) ts))
                   (expect (every? #(contains? (or (:style %) #{}) :code)
                                   (mapcat :runs (filter #(seq (:runs %)) lines)))))))

(defdescribe mermaid-code-block-test
             (it "a mermaid fence is painted as a box-drawing diagram, not as its source"
                 (let [lines
                       (layout/ast->lines
                         [:ast [:code {:lang "mermaid"} "flowchart TD\n  A[One] --> B[Two]\n"]]
                         60)

                       joined
                       (str/join "\n" (texts lines))]

                   (expect (str/includes? joined "One") joined)
                   (expect (str/includes? joined "▼") joined)
                   (expect (not (str/includes? joined "flowchart TD")) joined)
                   (expect (not (str/includes? joined "-->")) joined)))
             (it "diagram chrome is dim and the labels keep the foreground"
                 (let [lines
                       (layout/ast->lines
                         [:ast [:code {:lang "mermaid"} "flowchart TD\n  A[One] --> B[Two]\n"]]
                         60)

                       runs
                       (mapcat :runs lines)

                       chrome
                       (filter #(str/includes? (or (:text %) "") "─") runs)

                       label
                       (filter #(str/includes? (or (:text %) "") "One") runs)]

                   (expect (seq chrome))
                   (expect (seq label))
                   (expect (every? #(contains? (:style %) :dim) chrome))
                   (expect (every? #(and (contains? (:style %) :code)
                                         (not (contains? (:style %) :dim)))
                                   label))))
             (it "a fence the renderer does not own stays verbatim"
                 (let [lines
                       (layout/ast->lines
                         [:ast [:code {:lang "mermaid"} "sequenceDiagram\n  Alice->>Bob: hi\n"]]
                         60)

                       joined
                       (str/join "\n" (texts lines))]

                   (expect (str/includes? joined "sequenceDiagram") joined))))

;; tables

(defdescribe repeated-table-header-test
             (it "retains bold header tags and a separator for each result group"
                 (let [lines (layout/ast->lines [:ast
                                                 [:table [:tr [:th "Detail"] [:th "Result"]]
                                                  [:tr [:td "Kind"] [:td "ready"]]
                                                  [:tr [:th "Detail"] [:th "Result"]]
                                                  [:tr [:td "Last checked"] [:td "today"]]]]
                                                60)]
                   (expect (= [:table-sep :table-head :table-sep :table-row :table-sep :table-head
                               :table-sep :table-row :table-sep]
                              (mapv :block-tag lines)))
                   (expect (apply =
                             (map #(str/index-of % "Result")
                                  (texts (filter #(= :table-head (:block-tag %)) lines))))))))

(defdescribe table-test
             (it "renders IR tables as boxed rows with semantic table tags"
                 (let [lines
                       (layout/ast->lines [:ast
                                           [:table [:tr [:th "Name"] [:th "Count"]]
                                            [:tr [:td "apples"] [:td "12"]]]]
                                          80)

                       ts
                       (texts lines)

                       tags
                       (mapv :block-tag lines)]

                   (expect (= [:table-sep :table-head :table-sep :table-row :table-sep] tags))
                   (expect (= "┌────────┬───────┐" (first ts)))
                   (expect (= "│ Name   │ Count │" (second ts)))
                   (expect (= "├────────┼───────┤" (nth ts 2)))
                   (expect (= "│ apples │ 12    │" (nth ts 3)))
                   (expect (= "└────────┴───────┘" (nth ts 4)))))
             (it "emits table head/separator/body markers for the painter"
                 (let [out
                       (layout/ast->sentinel-strings
                         [:ast [:table [:tr [:th "A"] [:th "B"]] [:tr [:td "1"] [:td "2"]]]]
                         80)

                       ms
                       (markers out)]

                   (expect (= [p/MARKER_MD_TABLE_SEP p/MARKER_MD_TABLE_HEAD p/MARKER_MD_TABLE_SEP
                               p/MARKER_MD_TABLE_ROW p/MARKER_MD_TABLE_SEP]
                              ms))))
             (it "uses thinking table markers in thinking mode"
                 (let [out
                       (layout/ast->sentinel-strings [:ast [:table [:tr [:th "A"]] [:tr [:td "1"]]]]
                                                     80
                                                     {:mode :thinking})

                       ms
                       (markers out)]

                   (expect (= p/MARKER_TH_MD_TABLE_HEAD (second ms)))
                   (expect (= p/MARKER_TH_MD_TABLE_SEP (first ms)))
                   (expect (= p/MARKER_TH_MD_TABLE_ROW (nth ms 3))))))

(defdescribe
  table-wrap-test
  (it
    "long cell text WRAPS inside its column instead of being truncated"
    (let [long-desc
          (str "This is a very long description that absolutely cannot "
               "fit on one physical terminal row and must wrap inside its cell")

          lines
          (layout/ast->lines [:ast
                              [:table [:tr [:th "Option"] [:th "Description"]]
                               [:tr [:td "alpha"] [:td long-desc]]
                               [:tr [:td "beta"] [:td "short"]]]]
                             40)

          ts
          (texts lines)

          row-lines
          (filterv #(= :table-row (:block-tag %)) lines)]

      ;; every physical line fits the requested width exactly
      (expect (every? #(<= (p/display-width %) 40) ts)
              (str "over-wide lines: " (vec (filter #(> (p/display-width %) 40) ts))))
      ;; the long logical row expanded into multiple physical rows
      (expect (> (count row-lines) 2)
              (str "expected wrapped continuation rows, got: " (count row-lines)))
      ;; NO content was lost to truncation — the whole sentence survives
      (expect (= (str/replace long-desc #"\s+" " ")
                 (-> (str/join " " (texts row-lines))
                     (str/replace #"[│]" "")
                     (str/replace #"\s+" " ")
                     str/trim
                     ;; strip the first-column cells + the short row
                     (str/replace #"alpha ?" "")
                     (str/replace #"beta short" "")
                     str/trim)))
      ;; grid chrome stays intact around the wrapped body
      (expect (str/starts-with? (first ts) "┌"))
      (expect (str/starts-with? (last ts) "└"))
      (expect (every? #(and (str/starts-with? % "│") (str/ends-with? % "│")) (texts row-lines)))))
  (it "continuation rows keep sibling short cells blank-padded"
      (let [lines
            (layout/ast->lines [:ast
                                [:table [:tr [:th "K"] [:th "V"]]
                                 [:tr [:td "k1"]
                                  [:td "a long value that needs several rows to fit"]]]]
                               24)

            row-ts
            (texts (filterv #(= :table-row (:block-tag %)) lines))]

        (expect (> (count row-ts) 1))
        ;; first physical row carries the key, continuations are blank there
        (expect (str/includes? (first row-ts) "k1"))
        (expect (every? #(not (str/includes? % "k1")) (rest row-ts)))))
  (it "header cells wrap too, tagged :table-head on every physical row"
      (let [lines
            (layout/ast->lines
              [:ast
               [:table [:tr [:th "A"] [:th "an extremely verbose header label that wraps"]]
                [:tr [:td "1"] [:td "x"]]]]
              24)

            head-lines
            (filterv #(= :table-head (:block-tag %)) lines)]

        (expect (> (count head-lines) 1))
        (expect (every? #(<= (p/display-width %) 24) (texts lines)))))
  (it "wide graphemes in a squeezed column terminate and stay in-width"
      ;; regression: col-prefix-end returns 0 fitting chars for an
      ;; emoji in a width-1 column — the wrap loop must still advance.
      (let [lines
            (layout/ast->lines [:ast
                                [:table [:tr [:th "😀😀"] [:th "b"]]
                                 [:tr [:td "😀 zażółć gęślą jaźń"] [:td "y"]]]]
                               8)

            ts
            (texts lines)]

        (expect (pos? (count ts)))
        (expect (every? #(<= (p/display-width %) 8) ts) (str "got: " ts)))))

;; table links (issue #91)

(defn- link-spans
  "Body text + link spans of every entry that carries clickable links,
   as `[body [{:col :width :url} ...]]`. `:line` starts with the block
   marker (one zero-width char), and link `:col` is body-relative."
  [entries]
  (into []
        (keep (fn [e]
                (when-let [links (:links (:meta e))]
                  [(subs (:line e) 1) links])))
        entries))

(defdescribe
  table-link-test
  (it "a link inside a table cell becomes a clickable span on its grid row"
      ;; #91: `[label](url)` in a GFM table cell rendered as plain text and
      ;; registered no `:url` click region, while the same link in a paragraph
      ;; did. The grid painter owns the row style and cannot consume inline
      ;; sentinels, so the span rides along as `:meta {:links ...}` instead.
      (let [url
            "https://example.com/browse/CARS-9862"

            entries
            (layout/ast->entries [:ast
                                  [:table [:tr [:th "repo"] [:th "ticket"]]
                                   [:tr [:td "glms-web"] [:td [:a {:href url} "CARS-9862"]]]]]
                                 60)

            spans
            (link-spans entries)]

        (expect (= [["│ glms-web │ CARS-9862 │" [{:col 13 :width 9 :url url}]]] spans))
        ;; the span really covers the visible label
        (let [[body [{:keys [col width]}]] (first spans)]
          (expect (= "CARS-9862" (subs body col (+ (long col) (long width))))))
        ;; the label itself stays plain text — no markdown syntax leaks
        (expect (not-any? #(str/includes? (:line %) "](") entries))))
  (it "a link wrapping inside its cell yields one span per physical row"
      (let [url
            "https://example.com/a-very-long-target"

            entries
            (layout/ast->entries [:ast
                                  [:table [:tr [:th "k"] [:th "v"]]
                                   [:tr [:td "row"]
                                    [:td "see " [:a {:href url} "the long link label"] " and "
                                     [:a {:href "https://example.com/b"} "b"]]]]]
                                 26)

            spans
            (link-spans entries)]

        (expect (= [["│ row │ see the long     │" [{:col 12 :width 8 :url url}]]
                    ["│     │ link label and b │"
                     [{:col 8 :width 10 :url url} {:col 23 :width 1 :url "https://example.com/b"}]]]
                   spans))
        (expect (every? (fn [[body links]]
                          (every? (fn [{:keys [col width]}]
                                    (let [seg (subs body col (+ (long col) (long width)))]
                                      (and (= seg (str/trim seg)) (pos? (count seg)))))
                                  links))
                        spans))))
  (it "table rows without links carry no link meta"
      (let [entries (layout/ast->entries [:ast [:table [:tr [:th "A"]] [:tr [:td "1"]]]] 40)]
        (expect (empty? (link-spans entries))))))

;; table paths (BLO-172)

(defn- path-spans
  "Body text + file spans of every entry whose cells name files, as
   `[body [{:col :width :path} ...]]` — the shape `link-spans` reads for URLs."
  [entries]
  (keep (fn [e]
          (when-let [paths (:paths (:meta e))]
            [(subs (:line e) 1) paths]))
        entries))

(defdescribe
  table-path-test
  (it "a cell that names a file turns the words it spells into a file span"
      ;; BLO-172: a listing printed file names it would not let you open. The
      ;; cell keeps its own words; `{:path …}` makes the span they occupy the press.
      (let [entries
            (layout/ast->entries [:ast
                                  [:table [:tr [:th "Name"] [:th "Kind"]]
                                   [:tr [:td {:path "/w/src/core.clj"} "core.clj"] [:td "File"]]]]
                                 60)

            spans
            (vec (path-spans entries))]

        (expect (= 1 (count spans)))
        (let [[body paths]
              (first spans)

              {:keys [col width path]}
              (first paths)]

          (expect (= "/w/src/core.clj" path))
          (expect (= "core.clj" (subs body col (+ (long col) (long width))))))))
  (it "a wrapped row presses on its FIRST physical line only"
      (let [entries
            (layout/ast->entries [:ast
                                  [:table [:tr [:th "Name"] [:th "Note"]]
                                   [:tr [:td {:path "/w/src/core.clj"} "core.clj"]
                                    [:td
                                     "a note long enough to wrap this row over several lines"]]]]
                                 26)

            spans
            (vec (path-spans entries))]

        (expect (= 1 (count spans)))
        (expect (str/includes? (first (first spans)) "core.clj"))))
  (it "cells without a path carry no file meta"
      (let [entries (layout/ast->entries [:ast [:table [:tr [:th "A"]] [:tr [:td "1"]]]] 40)]
        (expect (empty? (path-spans entries))))))

;; bdc79ae9 fixture — end-to-end regression

(defn- fixture-ir
  []
  ;; Resolve through the classpath (the extension's `test/` dir is a test
  ;; root in both deps.edn files), so the fixture is found whether the suite
  ;; runs from the repo root or from this extension's own directory. The old
  ;; repo-root-relative `io/file` path only resolved from the repo root and
  ;; silently returned nil (unreachable fixture) when run from the extension.
  (when-let [r (io/resource "resources/fixtures/bdc79ae9_markdown_ast.edn")]
    (edn/read-string (slurp r))))

(defdescribe
  bdc79ae9-walker-test
  (it "fixture is reachable" (expect (some? (fixture-ir))))
  (it "no line begins with the 3-space hanging-indent pattern that broke the TUI"
      (when-let [raw (fixture-ir)]
        (let [lines (layout/ast->lines raw 100)
              ts (texts lines)
              offenders (filter #(re-matches #"^   \S.*" %) ts)]

          (expect (empty? offenders) (str "first 3 offenders: " (vec (take 3 offenders)))))))
  (it "every line fits within the requested width"
      (when-let [raw (fixture-ir)]
        (let [lines (layout/ast->lines raw 100)
              ts (texts lines)
              over (filter #(> (count %) 100) ts)]

          (expect (empty? over) (str "lines over 100 cols: " (vec (take 3 (map count over))))))))
  (it "the broken paragraph is now joined onto continuous wrap (no mid-string newline)"
      ;; Source had: \"`z/locators` — główny koń roboczy. Z `:source-contains` i `:symbol` \\n   znajduje nodes wiarygodnie. ...\"
      ;; After ->ast + walker, the sentence flows in one wrap chunk.
      (when-let [raw (fixture-ir)]
        (let [lines (layout/ast->lines raw 100)
              ts (texts lines)
              joined (str/join " " ts)]

          (expect (str/includes?
                    joined
                    "główny koń roboczy. Z :source-contains i :symbol znajduje nodes wiarygodnie")
                  (str "joined snippet not found; sample lines: " (vec (take 5 ts))))))))

;; block-tag enrichment + sentinel-string adapter

(defdescribe block-tag-test
             (it ":p block stamps :block-tag :p on every produced line"
                 (let [lines (layout/ast->lines [:ast [:p "hello"]] 80)]
                   (expect (every? #(= :p (:block-tag %)) lines))))
             (it ":h block stamps :block-tag :h with :block-level"
                 (let [lines
                       (layout/ast->lines [:ast [:h {:level 2} "Title"]] 80)

                       [first-line]
                       lines]

                   (expect (= :h (:block-tag first-line)))
                   (expect (= 2 (:block-level first-line)))))
             (it ":code block has neutral outside margins and code-bg inside padding"
                 (let [lines
                       (layout/ast->lines [:ast [:code "a\n\nb"]] 80)

                       code-lines
                       (filter #(= :code (:block-tag %)) lines)

                       margin-lines
                       (filter #(= :outer-margin (:block-tag %)) lines)

                       tags
                       (mapv :block-tag lines)

                       ts
                       (texts lines)]

                   ;; Shape:
                   ;;   :outer-margin - outside margin before the chip (bubble bg)
                   ;;   :code         - inside top padding (code bg)
                   ;;   :code         - content "a"
                   ;;   :code         - literal blank line inside the source
                   ;;   :code         - content "b"
                   ;;   :code         - inside bottom padding (code bg)
                   ;;   :outer-margin - outside margin after the chip (bubble bg)
                   (expect (= [:outer-margin :code :code :code :code :code :outer-margin] tags))
                   (expect (= 5 (count code-lines)))
                   (expect (= 2 (count margin-lines)))
                   (expect (= ["" "" "a" "" "b" "" ""] ts))))
             (it "adjacent :code blocks keep one neutral margin between padded chips"
                 (let [lines
                       (layout/ast->lines [:ast [:code "a"] [:code "b"]] 80)

                       tags
                       (mapv :block-tag lines)

                       ts
                       (texts lines)]

                   ;; The middle :outer-margin is the one-line margin between code chips;
                   ;; each chip still has its own :code top/bottom padding rows.
                   (expect (= [:outer-margin :code :code :code :outer-margin :code :code :code
                               :outer-margin]
                              tags))
                   (expect (= ["" "" "a" "" "" "" "b" "" ""] ts))))
             (it ":ul list stamps :block-tag :ul on marker + continuation lines"
                 (let [lines (layout/ast->lines [:ast [:ul [:li "x"] [:li "y"]]] 80)]
                   (expect (every? #(= :ul (:block-tag %)) lines)))))

(defdescribe sentinel-adapter-test
             (it "emits H1/H2/H3 markers for headings, picking by :level"
                 (let [out
                       (layout/ast->sentinel-strings [:ast [:h {:level 1} "A"] [:h {:level 2} "B"]
                                                      [:h {:level 3} "C"]]
                                                     80)

                       ms
                       (markers out)]

                   (expect (some #(= p/MARKER_MD_H1 %) ms))
                   (expect (some #(= p/MARKER_MD_H2 %) ms))
                   (expect (some #(= p/MARKER_MD_H3 %) ms))))
             (it "emits MARKER_MD_BULLET for ul / ol items"
                 (let [out (layout/ast->sentinel-strings [:ast [:ul [:li "x"]]] 80)]
                   (expect (some #(= p/MARKER_MD_BULLET (subs % 0 1)) out))))
             (it "emits MARKER_MD_CODE for fenced code"
                 (let [out (layout/ast->sentinel-strings [:ast [:code {:lang "clj"} "(+ 1 1)"]] 80)]
                   (expect (some #(= p/MARKER_MD_CODE (subs % 0 1)) out))))
             (it "emits MARKER_ANSWER_TXT for plain paragraphs"
                 (let [out (layout/ast->sentinel-strings [:ast [:p "hello world"]] 80)]
                   (expect (some #(= p/MARKER_ANSWER_TXT (subs % 0 1)) out))))
             (it "wraps :strong runs in INLINE_BOLD_ON/OFF sentinel pair"
                 (let [out
                       (layout/ast->sentinel-strings [:ast [:p "hi " [:strong "bold"] " rest"]] 80)

                       body
                       (str/join "" (bodies out))]

                   (expect (str/includes? body (str p/INLINE_BOLD_ON "bold" p/INLINE_BOLD_OFF)))))
             (it "wraps :c (inline code) in INLINE_CODE_ON/OFF sentinel pair"
                 (let [out
                       (layout/ast->sentinel-strings [:ast [:p "use " [:c "send!"] " here"]] 80)

                       body
                       (str/join "" (bodies out))]

                   (expect (str/includes? body (str p/INLINE_CODE_ON "send!" p/INLINE_CODE_OFF)))))
             (it "sentinel adapter is a string-only contract (every entry begins with a marker)"
                 (let [out (layout/ast->sentinel-strings [:ast [:h {:level 1} "T"] [:p "x"]
                                                          [:ul [:li "y"]] [:code "z"]]
                                                         80)]
                   (expect (every? string? out))
                   (expect (every? #(>= (count %) 1) out))))
             (it "bdc79ae9 fixture round-trips through the sentinel adapter without throwing"
                 (when-let [raw (fixture-ir)]
                   (let [out (layout/ast->sentinel-strings raw 100)]
                     (expect (vector? out))
                     (expect (every? string? out))
                     ;; the bug-paragraph still flows on one wrap chunk
                     (expect (some #(str/includes? % "znajduje nodes wiarygodnie") out))))))

;; Retired disclosure tags stay out of answer rendering

(defdescribe retired-disclosure-tags-test
             (it ":details/:summary input is flattened without toggle metadata"
                 (let [entries
                       (layout/ast->entries [:ast [:p "intro"]
                                             [:details {:open? true} [:summary "toggle"]
                                              [:p "body"]]]
                                            80)

                       body
                       (str/join "\n" (map :line entries))]

                   (expect (str/includes? body "intro"))
                   (expect (str/includes? body "togglebody"))
                   (expect (not-any? #(= :toggle-details (get-in % [:meta :kind])) entries)))))

(defdescribe wrap-cell-lines-delegation-test
             (it "table-cell wrap IS the shared lanterna word-wrap (one implementation)"
                 ;; `wrap-cell-lines` must produce exactly `p/word-wrap`'s lines
                 ;; (`TerminalTextUtils/wordWrap` in the lanterna fork) so table cells
                 ;; break at the same points as every other wrapped surface — a
                 ;; hand-rolled divergent wrapper is the regression this pins against.
                 (doseq [[s w] [["a quick brown fox jumps over it" 7] ["zażółć gęślą jaźń ✅ done" 6]
                                ["one-unbreakable-supertoken" 5] ["" 5] [nil 4] ["🎉🎉" 1]]]
                   (expect (= (p/word-wrap (str s) (max 1 (long w)))
                              (mapv :text
                                    (#'layout/wrap-cell-lines
                                     (mapv (fn [c]
                                             [c nil])
                                           (str s))
                                     w)))
                           (str "diverged from p/word-wrap for " (pr-str [s w]))))))

;; `:justify?` — flush-both-margins prose (op-card bodies, e.g. a fold receipt)

(defn- plain-lines
  "Entry `:line` strings with every PUA style/block sentinel removed."
  [entries]
  (mapv (fn [e]
          (str/replace (:line e) #"[\uE000-\uF8FF]" ""))
        entries))

(defdescribe
  justified-entries-test
  (it "stretches ONLY the overflow-wrapped lines to the full width"
      (let [ir
            [:ast
             [:p
              (str "folded a long narrative gist that explains what happened "
                   "during the fold and why it mattered enough to keep one "
                   "durable takeaway around for later readers")]]

            w
            46

            ragged
            (plain-lines (layout/ast->entries ir w {:mode :channel :justify? false}))

            flush-both
            (plain-lines (layout/ast->entries ir w {:mode :channel :justify? true}))]

        ;; every wrapped line is edge-to-edge, the paragraph-terminal
        ;; line stays ragged-right (never stretched)
        (expect (every? #(= w (p/display-width %)) (butlast flush-both)))
        (expect (< (p/display-width (last flush-both)) w))
        (expect (= (count ragged) (count flush-both)))
        (expect (= (last ragged) (last flush-both)))
        ;; same words, only inter-word gaps widened
        (expect (= (mapv #(str/split % #"\s+") ragged) (mapv #(str/split % #"\s+") flush-both)))))
  (it "never stretches a list marker or a code block"
      (let [entries
            (layout/ast->entries
              [:ast [:ul [:li (str "recover one stored output each, " "no re-run at all")]]
               [:code {:lang "clojure"} "(a  b  c)"]]
              40
              {:mode :channel :justify? true})

            lines
            (plain-lines entries)]

        ;; `- ` keeps its single space; the content after it still justifies
        (expect (some #(str/starts-with? % "- recover") lines))
        (expect (not-any? #(re-find #"^-\s\s+" %) lines))
        ;; code columns ARE the content — untouched
        (expect (some #(str/includes? % "(a  b  c)") lines))))
  (it "keeps one logical whitespace gap when styling splits it across runs"
      (let [justify
            (deref (ns-resolve 'com.blockether.vis.tui.markdown-layout 'justify-line-runs))

            runs
            [{:text "foo " :style #{}} {:text " " :style #{:bold}} {:text "bar baz" :style #{}}]

            justified
            (justify runs 13)

            text
            (apply str (map :text justified))]

        ;; The two adjacent run-local whitespace spans are ONE gap in the
        ;; concatenated line. Counting them twice used to index past `widened` and
        ;; crash the whole render frame.
        (expect (= 13 (p/display-width text)))
        (expect (= ["foo" "bar" "baz"] (str/split text #"\s+"))))))

(defdescribe quote-paragraph-gap-test
             (it "collapses the margin BETWEEN quoted paragraphs to one bar-only row"
                 ;; REGRESSION: every per-paragraph outer-margin was stripped, so a commit
                 ;; message quoted into a tool card read as one run-on paragraph.
                 (let [lines (layout/ast->lines [:ast [:quote [:p "feat: thing"] [:p "body line"]]]
                                                40)]
                   (expect (= ["│ feat: thing" "│ " "│ body line"] (texts lines)))))
             (it "leaves no bar-only row at the head or tail of the quote"
                 ;; The bar still paints as ONE solid block: only interior gaps survive.
                 (let [lines (layout/ast->lines [:ast [:quote [:p "only"]]] 40)]
                   (expect (= ["│ only"] (texts lines))))))

(def ^:private justice-prose
  (str "A quiet paragraph can become much more comfortable when its lines share "
       "a reasonably even rhythm of spaces instead of alternating between very tight "
       "and very loose arrangements."))

(defdescribe
  paragraph-optimizer-test
  (it "chooses Justice's paragraph-wide cell breaks rather than greedy ones"
      (expect (= ["A quiet paragraph" "can become much more" "comfortable when its"
                  "lines share a reasonably" "even rhythm of spaces" "instead of alternating"
                  "between very tight and" "very loose arrangements."]
                 (texts (layout/ast->lines [:ast [:p justice-prose]] 24)))))
  (it "does not turn a style boundary into a word boundary"
      (expect (= ["one" "abcdef" "end"]
                 (texts (layout/ast->lines [:ast [:p "one " [:strong "abc"] "def end"]] 7)))))
  (it "keeps inline code literal when surrounding prose is justified"
      (let [runs
            [{:text "Use " :style #{}} {:text "foo bar" :style #{:code}}
             {:text " with other words" :style #{}}]

            result
            (layout/justify-line-runs runs 30)]

        ;; Only prose gaps widen; the space inside the code run is never a gap.
        (expect (= ["Use  " "foo bar" "  with other words"] (mapv :text result)))))
  (it "uses restrained justification for ordinary TUI prose by default"
      (let [lines (plain-lines (layout/ast->entries [:ast [:p justice-prose]] 60 {:mode :channel}))]
        (expect (= 60 (p/display-width (first lines))))
        (expect (str/starts-with? (first lines) "A  quiet"))
        (expect (not (str/includes? (last lines) "  ")))))
  (it "stretches optimized soft lines fully when asked, as Justice renders them"
      (let [full
            (plain-lines (layout/ast->entries [:ast [:p justice-prose]]
                                              24
                                              {:mode :channel :full-justify? true}))

            restrained
            (plain-lines (layout/ast->entries [:ast [:p justice-prose]] 24 {:mode :channel}))]

        (expect (every? #(= 24 (p/display-width %)) (butlast full)))
        (expect (some #(< (p/display-width %) 24) (butlast restrained)))
        (expect (= (str/split justice-prose #" ") (mapcat #(str/split % #" +") full)))))
  (it "keeps unoptimized fallback lines on the near-full rule under full justification"
      (let [lines (plain-lines
                    (layout/ast->entries
                      [:ast [:p "one two https://example.com/a/very/long/path/that/cannot/fit end"]]
                      24
                      {:mode :channel :full-justify? true}))]
        (expect (= "one two" (first lines))))))

(def ^:private code-crowded-list
  [:ast
   [:ol
    [:li "Run " [:c "npm run build --workspace apps/vis-companion"]
     " from the repository root, then open " [:c "apps/vis-companion/dist/index.html"]
     " in a browser to check the result."]
    [:li "Check " [:c "git status --short"] " before staging, and never stage " [:c "PLAN.md"]
     " or " [:c "dev/com/blockether/vis/dev/repro.clj"] " with the fix."]]
   [:ul
    [:li "The helper " [:c "com.blockether.vis.tui.markdown-layout/justify-line-runs"]
     " widens gaps between words on every soft line of the paragraph."]
    [:li "Use " [:c "clojure -M:test --namespace com.blockether.vis.tui.markdown-layout-test"]
     " to run the tests, then read the counts it prints."]]])

(defdescribe
  paragraph-optimizer-boundaries-test
  (it
    "preserves Unicode graphemes and source text through narrow and wide resizes"
    (let
      [source
       "Zażółć gęślą jaźń élan 👩‍💻 can become more comfortable when its lines share an even rhythm"]
      (doseq [width (range 18 81)]
        (let [lines (texts (layout/ast->lines [:ast [:p source]] width))]
          (expect (= source (str/join " " lines)))
          (expect (every? #(<= (p/display-width %) width) lines))))))
  (it "keeps hard breaks and paragraph endings ragged while retaining empty break rows"
      (let [ast
            [:ast [:p "alpha beta" [:br] [:br] "one two"]]

            lines
            (layout/ast->lines ast 12)]

        (expect (= ["alpha beta" "" "one two"] (texts lines)))
        (expect (not-any? :wrap? lines))
        (expect (= ["alpha beta" "" "one two"]
                   (plain-lines (layout/ast->entries ast 12 {:mode :channel}))))))
  (it "maps a styled word and its link to the optimized physical row"
      (let [ast
            [:ast [:p "one " [:a {:href "https://example.com"} [:strong "abc"] "def"] " end"]]

            lines
            (layout/ast->lines ast 7)

            entries
            (layout/ast->entries ast 7 {:mode :channel})

            runs
            (:runs (second lines))]

        (expect (= ["one" "abcdef" "end"] (texts lines)))
        (expect (= ["abc" "def"] (mapv :text runs)))
        (expect (contains? (:style (first runs)) :bold))
        (expect (every? #(= "https://example.com" (:href %)) runs))
        (expect (= [{:col 0 :width 6 :url "https://example.com"}]
                   (get-in entries [1 :meta :links])))))
  (it "wraps inline code at its own spaces but never at its hyphens"
      (let [lines
            (layout/ast->lines [:ast [:p "Use " [:c "foo bar-baz"] " with other words now"]] 14)

            code-runs
            (filter #(contains? (:style %) :code) (mapcat :runs lines))]

        (expect (= ["foo" "bar-baz"] (mapv :text code-runs)))
        (expect (every? #(<= (p/display-width %) 14) (texts lines)))))
  (it "breaks long inline code after slashes and before dots"
      (let [ast [:ast
                 [:p "See " [:c "com.blockether.vis.tui.markdown-layout/justify-line-runs"]
                  " for the rule."]]]
        (expect (= [["See " "com.blockether.vis"] [".tui.markdown-layout/"]
                    ["justify-line-runs" " for"] ["the rule."]]
                   (mapv #(mapv :text (:runs %)) (layout/ast->lines ast 24))))))
  (it "keeps lists crowded with inline code inside the width without wide holes"
      ;; REGRESSION: unbreakable inline code pushed justified list items past the
      ;; terminal edge, and the few prose gaps beside it stretched into holes.
      (let [unwrapped (str/replace (str/join (plain-lines (layout/ast->entries code-crowded-list
                                                                               400
                                                                               {:mode :channel})))
                                   #"\s"
                                   "")]
        (doseq [width (range 24 91)
                opts [{:mode :channel} {:mode :channel :full-justify? true}]]

          (let [lines (plain-lines (layout/ast->entries code-crowded-list width opts))]
            (expect (every? #(<= (p/display-width %) width) lines))
            (expect (not-any? #(re-find #"\S {6,}" %) lines))
            (expect (= unwrapped (str/replace (str/join lines) #"\s" "")))))))
  (it "optimizes list and quote prose inside their fixed structural prefixes"
      (doseq [[ast prefix] [[[:ast [:ul [:li justice-prose]]] "- "]
                            [[:ast [:quote [:p justice-prose]]] "│ "]]]
        (let [lines (texts (layout/ast->lines ast 26))]
          (expect (= (str prefix "A quiet paragraph") (first lines)))
          (expect (every? #(<= (p/display-width %) 26) lines))))
      (let [lines (plain-lines
                    (layout/ast->entries [:ast [:quote [:p justice-prose]]] 62 {:mode :channel}))]
        (expect (str/starts-with? (first lines) "│ A  quiet"))
        (expect (= 62 (p/display-width (first lines))))))
  (it "uses different first and continuation widths without stretching the prefix"
      (let [prefix
            {:initial [{:text "-> " :style #{:marker}}] :cont [{:text " " :style #{}}]}

            lines
            (#'layout/prose-wrap-runs [{:text justice-prose :style #{}}] 25 prefix)]

        (expect (str/starts-with? (first (texts lines)) "-> "))
        (expect (every? #(str/starts-with? % " ") (rest (texts lines))))
        (expect (every? #(<= (p/display-width %) 25) (texts lines)))))
  (it "does not justify headings and allows explicit ragged-right prose"
      (doseq [ast [[:ast [:h {:level 3} justice-prose]] [:ast [:table [:tr [:td justice-prose]]]]]]
        (expect (= (layout/ast->entries ast 40 {:justify? false})
                   (layout/ast->entries ast 40 {:justify? true}))))
      (let [lines (plain-lines (layout/ast->entries [:ast [:p justice-prose]]
                                                    60
                                                    {:mode :channel :justify? false}))]
        (expect (str/starts-with? (first lines) "A quiet"))
        (expect (< (p/display-width (first lines)) 60))))
  (it "falls back unchanged for unsupported scripts, literal spacing and oversized paragraphs"
      (doseq [source ["  keep this indent" "keep  these spaces" "keep\tthis tab"
                      "中文文字 日本語 हिन्दी ไทย" "مرحبا بالعالم" (str/join " " (repeat 401 "word"))
                      (apply str (repeat 17000 "a"))]]
        (let [runs [{:text source :style #{}}]]
          (expect (= (#'layout/wrap-runs runs 24 []) (#'layout/prose-wrap-runs runs 24 []))))))
  (it "reuses measured paragraphs and bounds the cache during streaming"
      (let [prepare
            #'layout/prepared-prose

            ^java.util.Map cache
            @#'layout/prepared-prose-cache]

        (expect (identical? (prepare justice-prose []) (prepare justice-prose [])))
        (doseq [i (range 100)]
          (prepare (str "streamed paragraph " i) []))
        (locking cache (expect (<= (.size cache) 64))))))
