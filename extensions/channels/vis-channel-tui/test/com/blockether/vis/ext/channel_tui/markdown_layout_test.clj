(ns com.blockether.vis.ext.channel-tui.markdown-layout-test
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
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.markdown-layout :as layout]
            [lazytest.core :refer [defdescribe expect it]]))

;; ---------------------------------------------------------------------------
;; small helpers
;; ---------------------------------------------------------------------------

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

;; ---------------------------------------------------------------------------
;; basic blocks
;; ---------------------------------------------------------------------------

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
                 (let
                   [lines (layout/ast->lines [:ast [:p [:strong "bold"] " and " [:em "ital"]]] 80)]
                   (expect (styled? lines :bold))
                   (expect (styled? lines :italic)))))

;; ---------------------------------------------------------------------------
;; lists
;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;; heading hierarchy
;; ---------------------------------------------------------------------------

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
      (let
        [head
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
                 (let
                   [lines
                    (layout/ast->lines [:ast [:ul [:li "a"] [:li "b"]]] 80)

                    ts
                    (texts lines)]

                   (expect (some #(= "- a" %) ts))
                   (expect (some #(= "- b" %) ts))))
             (it "ol numbers items starting at 1"
                 (let
                   [lines
                    (layout/ast->lines [:ast [:ol [:li "x"] [:li "y"]]] 80)

                    ts
                    (texts lines)]

                   (expect (some #(= "1. x" %) ts))
                   (expect (some #(= "2. y" %) ts))))
             (it "ul renders GFM task-list markers as checklist glyphs"
                 (let
                   [lines
                    (layout/ast->lines [:ast
                                        [:ul [:li "[x] Completed item"] [:li "[ ] Pending item"]
                                         [:li "[X] Also completed"]]]
                                       80)

                    ts
                    (texts lines)]

                   (expect (= ["☑️  Completed item" "⬜ Pending item" "☑️  Also completed"] ts))))
             (it "task-list continuations indent by display width, not char count"
                 (let
                   [lines
                    (layout/ast->lines [:ast [:ul [:li "[ ] Pending item wraps here"]]] 14)

                    ts
                    (texts lines)]

                   (expect (= ["⬜ Pending" "   item wraps" "   here"] ts))
                   (expect (every? #(<= (p/display-width %) 14) ts))))
             (it "does not crash when a list item starts with a non-text inline node"
                 (let
                   [lines
                    (layout/ast->lines [:ast [:ul [:li [:br] "after break"]]] 80)

                    ts
                    (texts lines)]

                   (expect (some #(str/includes? % "after break") ts))))
             (it "wrapped li uses hanging indent equal to marker width (NOT 3 spaces)"
                 ;; This is the regression target: pre-IR code produced "   foo" continuation.
                 (let
                   [lines
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

;; ---------------------------------------------------------------------------
;; code blocks
;; ---------------------------------------------------------------------------

(defdescribe code-block-test
             (it "code block preserves whitespace verbatim, never wraps"
                 (let
                   [src
                    "(let [x 1]\n  (println x))"

                    lines
                    (layout/ast->lines [:ast [:code {:lang "clj"} src]] 40)

                    ts
                    (mapv #(str/replace (or % "") #"\u001b\[[0-9;]*m" "") (texts lines))]

                   (expect (some #(= "(let [x 1]" %) ts))
                   (expect (some #(= "  (println x))" %) ts))
                   (expect (every? #(contains? (or (:style %) #{}) :code)
                                   (mapcat :runs (filter #(seq (:runs %)) lines)))))))

;; ---------------------------------------------------------------------------
;; tables
;; ---------------------------------------------------------------------------

(defdescribe table-test
             (it "renders IR tables as boxed rows with semantic table tags"
                 (let
                   [lines
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
                 (let
                   [out
                    (layout/ast->sentinel-strings
                      [:ast [:table [:tr [:th "A"] [:th "B"]] [:tr [:td "1"] [:td "2"]]]]
                      80)

                    ms
                    (markers out)]

                   (expect (= [p/MARKER_MD_TABLE_SEP p/MARKER_MD_TABLE_HEAD p/MARKER_MD_TABLE_SEP
                               p/MARKER_MD_TABLE_ROW p/MARKER_MD_TABLE_SEP]
                              ms))))
             (it "uses thinking table markers in thinking mode"
                 (let
                   [out
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
    (let
      [long-desc
       (str "This is a very long description that absolutely cannot "
            "fit on one physical terminal row and must wrap inside its cell")

       lines
       (layout/ast->lines [:ast
                           [:table [:tr [:th "Option"] [:th "Description"]]
                            [:tr [:td "alpha"] [:td long-desc]] [:tr [:td "beta"] [:td "short"]]]]
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
      (let
        [lines
         (layout/ast->lines [:ast
                             [:table [:tr [:th "K"] [:th "V"]]
                              [:tr [:td "k1"] [:td "a long value that needs several rows to fit"]]]]
                            24)

         row-ts
         (texts (filterv #(= :table-row (:block-tag %)) lines))]

        (expect (> (count row-ts) 1))
        ;; first physical row carries the key, continuations are blank there
        (expect (str/includes? (first row-ts) "k1"))
        (expect (every? #(not (str/includes? % "k1")) (rest row-ts)))))
  (it "header cells wrap too, tagged :table-head on every physical row"
      (let
        [lines
         (layout/ast->lines [:ast
                             [:table
                              [:tr [:th "A"] [:th "an extremely verbose header label that wraps"]]
                              [:tr [:td "1"] [:td "x"]]]]
                            24)

         head-lines
         (filterv #(= :table-head (:block-tag %)) lines)]

        (expect (> (count head-lines) 1))
        (expect (every? #(<= (p/display-width %) 24) (texts lines)))))
  (it "wide graphemes in a squeezed column terminate and stay in-width"
      ;; regression: col-prefix-end returns 0 fitting chars for an
      ;; emoji in a width-1 column — the wrap loop must still advance.
      (let
        [lines
         (layout/ast->lines
           [:ast [:table [:tr [:th "😀😀"] [:th "b"]] [:tr [:td "😀 zażółć gęślą jaźń"] [:td "y"]]]]
           8)

         ts
         (texts lines)]

        (expect (pos? (count ts)))
        (expect (every? #(<= (p/display-width %) 8) ts) (str "got: " ts)))))

;; ---------------------------------------------------------------------------
;; table links (issue #91)
;; ---------------------------------------------------------------------------

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
      (let
        [url
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
      (let
        [url
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

;; ---------------------------------------------------------------------------
;; bdc79ae9 fixture — end-to-end regression
;; ---------------------------------------------------------------------------

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
        (let
          [lines (layout/ast->lines raw 100)
           ts (texts lines)
           offenders (filter #(re-matches #"^   \S.*" %) ts)]

          (expect (empty? offenders) (str "first 3 offenders: " (vec (take 3 offenders)))))))
  (it "every line fits within the requested width"
      (when-let [raw (fixture-ir)]
        (let
          [lines (layout/ast->lines raw 100)
           ts (texts lines)
           over (filter #(> (count %) 100) ts)]

          (expect (empty? over) (str "lines over 100 cols: " (vec (take 3 (map count over))))))))
  (it "the broken paragraph is now joined onto continuous wrap (no mid-string newline)"
      ;; Source had: \"`z/locators` — główny koń roboczy. Z `:source-contains` i `:symbol` \\n   znajduje nodes wiarygodnie. ...\"
      ;; After ->ast + walker, the sentence flows in one wrap chunk.
      (when-let [raw (fixture-ir)]
        (let
          [lines (layout/ast->lines raw 100)
           ts (texts lines)
           joined (str/join " " ts)]

          (expect (str/includes?
                    joined
                    "główny koń roboczy. Z :source-contains i :symbol znajduje nodes wiarygodnie")
                  (str "joined snippet not found; sample lines: " (vec (take 5 ts))))))))

;; ---------------------------------------------------------------------------
;; block-tag enrichment + sentinel-string adapter
;; ---------------------------------------------------------------------------

(defdescribe block-tag-test
             (it ":p block stamps :block-tag :p on every produced line"
                 (let [lines (layout/ast->lines [:ast [:p "hello"]] 80)]
                   (expect (every? #(= :p (:block-tag %)) lines))))
             (it ":h block stamps :block-tag :h with :block-level"
                 (let
                   [lines
                    (layout/ast->lines [:ast [:h {:level 2} "Title"]] 80)

                    [first-line]
                    lines]

                   (expect (= :h (:block-tag first-line)))
                   (expect (= 2 (:block-level first-line)))))
             (it ":code block has neutral outside margins and code-bg inside padding"
                 (let
                   [lines
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
                 (let
                   [lines
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
                 (let
                   [out
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
                 (let
                   [out
                    (layout/ast->sentinel-strings [:ast [:p "hi " [:strong "bold"] " rest"]] 80)

                    body
                    (str/join "" (bodies out))]

                   (expect (str/includes? body (str p/INLINE_BOLD_ON "bold" p/INLINE_BOLD_OFF)))))
             (it "wraps :c (inline code) in INLINE_CODE_ON/OFF sentinel pair"
                 (let
                   [out
                    (layout/ast->sentinel-strings [:ast [:p "use " [:c "send!"] " here"]] 80)

                    body
                    (str/join "" (bodies out))]

                   (expect (str/includes? body (str p/INLINE_CODE_ON "send!" p/INLINE_CODE_OFF)))))
             (it "sentinel adapter is a string-only contract (every entry begins with a marker)"
                 (let
                   [out (layout/ast->sentinel-strings [:ast [:h {:level 1} "T"] [:p "x"]
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

;; ---------------------------------------------------------------------------
;; Retired disclosure tags stay out of answer rendering
;; ---------------------------------------------------------------------------

(defdescribe retired-disclosure-tags-test
             (it ":details/:summary input is flattened without toggle metadata"
                 (let
                   [entries
                    (layout/ast->entries [:ast [:p "intro"]
                                          [:details {:open? true} [:summary "toggle"] [:p "body"]]]
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
                 (doseq
                   [[s w] [["a quick brown fox jumps over it" 7] ["zażółć gęślą jaźń ✅ done" 6]
                           ["one-unbreakable-supertoken" 5] ["" 5] [nil 4] ["🎉🎉" 1]]]
                   (expect (= (p/word-wrap (str s) (max 1 (long w)))
                              (mapv :text
                                    (#'layout/wrap-cell-lines (mapv (fn [c] [c nil]) (str s)) w)))
                           (str "diverged from p/word-wrap for " (pr-str [s w]))))))

;; ---------------------------------------------------------------------------
;; `:justify?` — flush-both-margins prose (op-card bodies, e.g. a fold receipt)
;; ---------------------------------------------------------------------------

(defn- plain-lines
  "Entry `:line` strings with every PUA style/block sentinel removed."
  [entries]
  (mapv (fn [e]
          (str/replace (:line e) #"[\uE000-\uF8FF]" ""))
        entries))

(defdescribe
  justified-entries-test
  (it "stretches ONLY the overflow-wrapped lines to the full width"
      (let
        [ir
         [:ast
          [:p
           (str "folded a long narrative gist that explains what happened "
                "during the fold and why it mattered enough to keep one "
                "durable takeaway around for later readers")]]

         w
         46

         ragged
         (plain-lines (layout/ast->entries ir w {:mode :channel}))

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
      (let
        [entries
         (layout/ast->entries
           [:ast [:ul [:li (str "recover one stored native result each, " "no re-run at all")]]
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
      (let
        [justify
         (deref (ns-resolve 'com.blockether.vis.ext.channel-tui.markdown-layout 'justify-line-runs))

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
