(ns com.blockether.vis.ext.channel-tui.render-ir-test
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
            [com.blockether.vis.ext.channel-tui.render-ir :as ir-tui]
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
                 (let [lines (ir-tui/ir->lines [:ir [:p "hello world"]] 80)]
                   (expect (= ["hello world"] (texts lines)))))
             (it "paragraph wraps at width on word boundary"
                 (let [lines (ir-tui/ir->lines [:ir [:p "lorem ipsum dolor sit amet"]] 12)]
                   (expect (every? (fn [t]
                                     (<= (count t) 12))
                                   (texts lines))
                           (str "got: " (texts lines)))
                   (expect (= "lorem ipsum dolor sit amet"
                              (str/replace (str/join " " (texts lines)) #"\s+" " ")))))
             (it "inline code keeps :code style on the word"
                 (let [lines (ir-tui/ir->lines [:ir [:p "use " [:c "send!"] " here"]] 80)]
                   (expect (= ["use send! here"] (texts lines)))
                   (expect (styled? lines :code))))
             (it "strong + em propagate style flags"
                 (let [lines (ir-tui/ir->lines [:ir [:p [:strong "bold"] " and " [:em "ital"]]] 80)]
                   (expect (styled? lines :bold))
                   (expect (styled? lines :italic)))))

;; ---------------------------------------------------------------------------
;; lists
;; ---------------------------------------------------------------------------

(defdescribe list-test
             (it "ul renders '- ' marker per item"
                 (let [lines
                       (ir-tui/ir->lines [:ir [:ul [:li "a"] [:li "b"]]] 80)

                       ts
                       (texts lines)]

                   (expect (some #(= "- a" %) ts))
                   (expect (some #(= "- b" %) ts))))
             (it "ol numbers items starting at 1"
                 (let [lines
                       (ir-tui/ir->lines [:ir [:ol [:li "x"] [:li "y"]]] 80)

                       ts
                       (texts lines)]

                   (expect (some #(= "1. x" %) ts))
                   (expect (some #(= "2. y" %) ts))))
             (it "ul renders GFM task-list markers as checklist glyphs"
                 (let [lines
                       (ir-tui/ir->lines [:ir
                                          [:ul [:li "[x] Completed item"] [:li "[ ] Pending item"]
                                           [:li "[X] Also completed"]]]
                                         80)

                       ts
                       (texts lines)]

                   (expect (= ["☑️  Completed item" "⬜ Pending item" "☑️  Also completed"] ts))))
             (it "task-list continuations indent by display width, not char count"
                 (let [lines
                       (ir-tui/ir->lines [:ir [:ul [:li "[ ] Pending item wraps here"]]] 14)

                       ts
                       (texts lines)]

                   (expect (= ["⬜ Pending" "   item wraps" "   here"] ts))
                   (expect (every? #(<= (p/display-width %) 14) ts))))
             (it "does not crash when a list item starts with a non-text inline node"
                 (let [lines
                       (ir-tui/ir->lines [:ir [:ul [:li [:br] "after break"]]] 80)

                       ts
                       (texts lines)]

                   (expect (some #(str/includes? % "after break") ts))))
             (it "wrapped li uses hanging indent equal to marker width (NOT 3 spaces)"
                 ;; This is the regression target: pre-IR code produced "   foo" continuation.
                 (let [lines
                       (ir-tui/ir->lines [:ir
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
                 (let [src
                       "(let [x 1]\n  (println x))"

                       lines
                       (ir-tui/ir->lines [:ir [:code {:lang "clj"} src]] 10)

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
                 (let [lines
                       (ir-tui/ir->lines [:ir
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
                       (ir-tui/ir->sentinel-strings
                         [:ir [:table [:tr [:th "A"] [:th "B"]] [:tr [:td "1"] [:td "2"]]]]
                         80)

                       ms
                       (markers out)]

                   (expect (= [p/MARKER_MD_TABLE_SEP p/MARKER_MD_TABLE_HEAD p/MARKER_MD_TABLE_SEP
                               p/MARKER_MD_TABLE_ROW p/MARKER_MD_TABLE_SEP]
                              ms))))
             (it "uses thinking table markers in thinking mode"
                 (let [out
                       (ir-tui/ir->sentinel-strings [:ir [:table [:tr [:th "A"]] [:tr [:td "1"]]]]
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
          (ir-tui/ir->lines [:ir
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
      (let [lines
            (ir-tui/ir->lines [:ir
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
            (ir-tui/ir->lines [:ir
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
      (let [lines
            (ir-tui/ir->lines [:ir
                               [:table [:tr [:th "😀😀"] [:th "b"]]
                                [:tr [:td "😀 zażółć gęślą jaźń"] [:td "y"]]]]
                              8)

            ts
            (texts lines)]

        (expect (pos? (count ts)))
        (expect (every? #(<= (p/display-width %) 8) ts) (str "got: " ts)))))

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
  (when-let [r (io/resource "resources/fixtures/bdc79ae9_answer_ir.edn")]
    (edn/read-string (slurp r))))

(defdescribe
  bdc79ae9-walker-test
  (it "fixture is reachable" (expect (some? (fixture-ir))))
  (it "no line begins with the 3-space hanging-indent pattern that broke the TUI"
      (when-let [raw (fixture-ir)]
        (let [lines (ir-tui/ir->lines raw 100)
              ts (texts lines)
              offenders (filter #(re-matches #"^   \S.*" %) ts)]

          (expect (empty? offenders) (str "first 3 offenders: " (vec (take 3 offenders)))))))
  (it "every line fits within the requested width"
      (when-let [raw (fixture-ir)]
        (let [lines (ir-tui/ir->lines raw 100)
              ts (texts lines)
              over (filter #(> (count %) 100) ts)]

          (expect (empty? over) (str "lines over 100 cols: " (vec (take 3 (map count over))))))))
  (it "the broken paragraph is now joined onto continuous wrap (no mid-string newline)"
      ;; Source had: \"`z/locators` — główny koń roboczy. Z `:source-contains` i `:symbol` \\n   znajduje nodes wiarygodnie. ...\"
      ;; After ->ast + walker, the sentence flows in one wrap chunk.
      (when-let [raw (fixture-ir)]
        (let [lines (ir-tui/ir->lines raw 100)
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
                 (let [lines (ir-tui/ir->lines [:ir [:p "hello"]] 80)]
                   (expect (every? #(= :p (:block-tag %)) lines))))
             (it ":h block stamps :block-tag :h with :block-level"
                 (let [lines
                       (ir-tui/ir->lines [:ir [:h {:level 2} "Title"]] 80)

                       [first-line]
                       lines]

                   (expect (= :h (:block-tag first-line)))
                   (expect (= 2 (:block-level first-line)))))
             (it ":code block has neutral outside margins and code-bg inside padding"
                 (let [lines
                       (ir-tui/ir->lines [:ir [:code "a\n\nb"]] 80)

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
                       (ir-tui/ir->lines [:ir [:code "a"] [:code "b"]] 80)

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
                 (let [lines (ir-tui/ir->lines [:ir [:ul [:li "x"] [:li "y"]]] 80)]
                   (expect (every? #(= :ul (:block-tag %)) lines)))))

(defdescribe sentinel-adapter-test
             (it "emits H1/H2/H3 markers for headings, picking by :level"
                 (let [out
                       (ir-tui/ir->sentinel-strings [:ir [:h {:level 1} "A"] [:h {:level 2} "B"]
                                                     [:h {:level 3} "C"]]
                                                    80)

                       ms
                       (markers out)]

                   (expect (some #(= p/MARKER_MD_H1 %) ms))
                   (expect (some #(= p/MARKER_MD_H2 %) ms))
                   (expect (some #(= p/MARKER_MD_H3 %) ms))))
             (it "emits MARKER_MD_BULLET for ul / ol items"
                 (let [out (ir-tui/ir->sentinel-strings [:ir [:ul [:li "x"]]] 80)]
                   (expect (some #(= p/MARKER_MD_BULLET (subs % 0 1)) out))))
             (it "emits MARKER_MD_CODE for fenced code"
                 (let [out (ir-tui/ir->sentinel-strings [:ir [:code {:lang "clj"} "(+ 1 1)"]] 80)]
                   (expect (some #(= p/MARKER_MD_CODE (subs % 0 1)) out))))
             (it "emits MARKER_ANSWER_TXT for plain paragraphs"
                 (let [out (ir-tui/ir->sentinel-strings [:ir [:p "hello world"]] 80)]
                   (expect (some #(= p/MARKER_ANSWER_TXT (subs % 0 1)) out))))
             (it "wraps :strong runs in INLINE_BOLD_ON/OFF sentinel pair"
                 (let [out
                       (ir-tui/ir->sentinel-strings [:ir [:p "hi " [:strong "bold"] " rest"]] 80)

                       body
                       (str/join "" (bodies out))]

                   (expect (str/includes? body (str p/INLINE_BOLD_ON "bold" p/INLINE_BOLD_OFF)))))
             (it "wraps :c (inline code) in INLINE_CODE_ON/OFF sentinel pair"
                 (let [out
                       (ir-tui/ir->sentinel-strings [:ir [:p "use " [:c "send!"] " here"]] 80)

                       body
                       (str/join "" (bodies out))]

                   (expect (str/includes? body (str p/INLINE_CODE_ON "send!" p/INLINE_CODE_OFF)))))
             (it "sentinel adapter is a string-only contract (every entry begins with a marker)"
                 (let [out (ir-tui/ir->sentinel-strings [:ir [:h {:level 1} "T"] [:p "x"]
                                                         [:ul [:li "y"]] [:code "z"]]
                                                        80)]
                   (expect (every? string? out))
                   (expect (every? #(>= (count %) 1) out))))
             (it "bdc79ae9 fixture round-trips through the sentinel adapter without throwing"
                 (when-let [raw (fixture-ir)]
                   (let [out (ir-tui/ir->sentinel-strings raw 100)]
                     (expect (vector? out))
                     (expect (every? string? out))
                     ;; the bug-paragraph still flows on one wrap chunk
                     (expect (some #(str/includes? % "znajduje nodes wiarygodnie") out))))))

;; ---------------------------------------------------------------------------
;; Retired disclosure tags stay out of answer rendering
;; ---------------------------------------------------------------------------

(defdescribe retired-disclosure-tags-test
             (it ":details/:summary input is flattened without toggle metadata"
                 (let [entries
                       (ir-tui/ir->entries
                         [:ir [:p "intro"] [:details {:open? true} [:summary "toggle"] [:p "body"]]]
                         80)

                       body
                       (str/join "\n" (map :line entries))]

                   (expect (str/includes? body "intro"))
                   (expect (str/includes? body "togglebody"))
                   (expect (not-any? #(= :toggle-details (get-in % [:meta :kind])) entries)))))

(defdescribe wrap-cell-cols-delegation-test
             (it "table-cell wrap IS the shared lanterna word-wrap (one implementation)"
                 ;; `wrap-cell-cols` must produce exactly `p/word-wrap`'s lines
                 ;; (`TerminalTextUtils/wordWrap` in the lanterna fork) so table cells
                 ;; break at the same points as every other wrapped surface — a
                 ;; hand-rolled divergent wrapper is the regression this pins against.
                 (doseq [[s w] [["a quick brown fox jumps over it" 7] ["zażółć gęślą jaźń ✅ done" 6]
                                ["one-unbreakable-supertoken" 5] ["" 5] [nil 4] ["🎉🎉" 1]]]
                   (expect (= (p/word-wrap (str s) (max 1 (long w))) (#'ir-tui/wrap-cell-cols s w))
                           (str "diverged from p/word-wrap for " (pr-str [s w]))))))
