(ns com.blockether.vis.ext.channel-tui.render-ir-test
  "Tests for the canonical-IR → styled-line walker.

   Two layers of coverage:

   1. Unit tests on small hand-crafted IR fragments — soft-break
      collapse already happened in `ir/->ast`, so the walker only
      needs to wrap, indent, and propagate styles correctly.

   2. End-to-end fixture from conversation `bdc79ae9` — the LLM
      output that produced the 3-space hanging indent in the TUI.
      Asserts the bug is structurally impossible on the new path."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.ext.channel-tui.primitives :as p]
   [com.blockether.vis.ext.channel-tui.render-ir :as ir-tui]
   [lazytest.core :refer [defdescribe expect it]]))

;; ---------------------------------------------------------------------------
;; small helpers
;; ---------------------------------------------------------------------------

(defn- texts [lines]
  (mapv (fn [l] (apply str (map (fn [r] (or (:text r) "")) (:runs l)))) lines))

(defn- styled? [lines kw]
  (some (fn [l] (some (fn [r] (contains? (or (:style r) #{}) kw)) (:runs l))) lines))

;; ---------------------------------------------------------------------------
;; basic blocks
;; ---------------------------------------------------------------------------

(defdescribe paragraph-test
  (it "paragraph emits one line for short content"
    (let [lines (ir-tui/ir->lines [:ir [:p "hello world"]] 80)]
      (expect (= ["hello world"] (texts lines)))))

  (it "paragraph wraps at width on word boundary"
    (let [lines (ir-tui/ir->lines [:ir [:p "lorem ipsum dolor sit amet"]] 12)]
      (expect (every? (fn [t] (<= (count t) 12)) (texts lines))
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
    (let [lines (ir-tui/ir->lines [:ir [:ul [:li "a"] [:li "b"]]] 80)
          ts    (texts lines)]
      (expect (some #(= "- a" %) ts))
      (expect (some #(= "- b" %) ts))))

  (it "ol numbers items starting at 1"
    (let [lines (ir-tui/ir->lines [:ir [:ol [:li "x"] [:li "y"]]] 80)
          ts    (texts lines)]
      (expect (some #(= "1. x" %) ts))
      (expect (some #(= "2. y" %) ts))))

  (it "wrapped li uses hanging indent equal to marker width (NOT 3 spaces)"
    ;; This is the regression target: pre-IR code produced "   foo" continuation.
    (let [lines (ir-tui/ir->lines
                  [:ir [:ul [:li "short " [:c "code-token"] " then a long tail "
                             "that will force wrapping at the chosen width"]]]
                  30)
          ts (texts lines)
          continuations (rest ts)]
      (expect (every? #(str/starts-with? % "  ") continuations)
        (str "expected 2-space hanging indent, got: " (vec continuations)))
      (expect (not-any? #(re-matches #"^   \S.*" %) continuations)
        (str "no continuation may begin with 3-space indent: " (vec continuations))))))

;; ---------------------------------------------------------------------------
;; code blocks
;; ---------------------------------------------------------------------------

(defdescribe code-block-test
  (it "code block preserves whitespace verbatim, never wraps"
    (let [src   "(let [x 1]\n  (println x))"
          lines (ir-tui/ir->lines [:ir [:code {:lang "clj"} src]] 10)
          ts    (texts lines)]
      (expect (some #(= "(let [x 1]" %) ts))
      (expect (some #(= "  (println x))" %) ts))
      (expect (every? #(contains? (or (:style %) #{}) :code)
                (mapcat :runs (filter #(seq (:runs %)) lines)))))))

;; ---------------------------------------------------------------------------
;; bdc79ae9 fixture — end-to-end regression
;; ---------------------------------------------------------------------------

(defn- fixture-ir []
  (let [f (io/file "extensions/channels/vis-channel-tui/test/resources/fixtures/bdc79ae9_answer_ir.edn")]
    (when (.exists f)
      (edn/read-string (slurp f)))))

(defdescribe bdc79ae9-walker-test
  (it "fixture is reachable"
    (expect (some? (fixture-ir))))

  (it "no line begins with the 3-space hanging-indent pattern that broke the TUI"
    (when-let [raw (fixture-ir)]
      (let [lines (ir-tui/ir->lines raw 100)
            ts    (texts lines)
            offenders (filter #(re-matches #"^   \S.*" %) ts)]
        (expect (empty? offenders)
          (str "first 3 offenders: " (vec (take 3 offenders)))))))

  (it "every line fits within the requested width"
    (when-let [raw (fixture-ir)]
      (let [lines (ir-tui/ir->lines raw 100)
            ts    (texts lines)
            over  (filter #(> (count %) 100) ts)]
        (expect (empty? over)
          (str "lines over 100 cols: " (vec (take 3 (map count over))))))))

  (it "the broken paragraph is now joined onto continuous wrap (no mid-string newline)"
    ;; Source had: \"`z/locators` — główny koń roboczy. Z `:source-contains` i `:symbol` \\n   znajduje nodes wiarygodnie. ...\"
    ;; After ->ast + walker, the sentence flows in one wrap chunk.
    (when-let [raw (fixture-ir)]
      (let [lines (ir-tui/ir->lines raw 100)
            ts    (texts lines)
            joined (str/join " " ts)]
        (expect (str/includes? joined "główny koń roboczy. Z :source-contains i :symbol znajduje nodes wiarygodnie")
          (str "joined snippet not found; sample lines: " (vec (take 5 ts))))))))

;; ---------------------------------------------------------------------------
;; block-tag enrichment + sentinel-string adapter
;; ---------------------------------------------------------------------------

(defdescribe block-tag-test
  (it ":p block stamps :block-tag :p on every produced line"
    (let [lines (ir-tui/ir->lines [:ir [:p "hello"]] 80)]
      (expect (every? #(= :p (:block-tag %)) lines))))

  (it ":h block stamps :block-tag :h with :block-level"
    (let [lines (ir-tui/ir->lines [:ir [:h {:level 2} "Title"]] 80)
          [first-line] lines]
      (expect (= :h (:block-tag first-line)))
      (expect (= 2 (:block-level first-line)))))

  (it ":code block has neutral outside margins and code-bg inside padding"
    (let [lines       (ir-tui/ir->lines [:ir [:code "a\n\nb"]] 80)
          code-lines  (filter #(= :code (:block-tag %)) lines)
          pad-lines   (filter #(= :p (:block-tag %)) lines)
          tags        (mapv :block-tag lines)
          ts          (texts lines)]
      ;; Shape:
      ;;   :p     - outside margin before the chip (bubble bg)
      ;;   :code  - inside top padding (code bg)
      ;;   :code  - content "a"
      ;;   :code  - literal blank line inside the source
      ;;   :code  - content "b"
      ;;   :code  - inside bottom padding (code bg)
      ;;   :p     - outside margin after the chip (bubble bg)
      (expect (= [:p :code :code :code :code :code :p] tags))
      (expect (= 5 (count code-lines)))
      (expect (= 2 (count pad-lines)))
      (expect (= ["" "" "a" "" "b" "" ""] ts))))

  (it "adjacent :code blocks keep one neutral margin between padded chips"
    (let [lines (ir-tui/ir->lines [:ir [:code "a"] [:code "b"]] 80)
          tags  (mapv :block-tag lines)
          ts    (texts lines)]
      ;; The middle :p is the one-line margin between code chips; each
      ;; chip still has its own :code top/bottom padding rows.
      (expect (= [:p :code :code :code :p :code :code :code :p] tags))
      (expect (= ["" "" "a" "" "" "" "b" "" ""] ts))))

  (it ":ul list stamps :block-tag :ul on marker + continuation lines"
    (let [lines (ir-tui/ir->lines [:ir [:ul [:li "x"] [:li "y"]]] 80)]
      (expect (every? #(= :ul (:block-tag %)) lines)))))

(defn- markers [strings]
  (mapv #(subs % 0 1) strings))

(defn- bodies [strings]
  (mapv #(subs % 1) strings))

(defdescribe sentinel-adapter-test
  (it "emits H1/H2/H3 markers for headings, picking by :level"
    (let [out (ir-tui/ir->sentinel-strings
                [:ir [:h {:level 1} "A"] [:h {:level 2} "B"] [:h {:level 3} "C"]]
                80)
          ms  (markers out)]
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
    (let [out (ir-tui/ir->sentinel-strings
                [:ir [:p "hi " [:strong "bold"] " rest"]]
                80)
          body (str/join "" (bodies out))]
      (expect (str/includes? body (str p/INLINE_BOLD_ON "bold" p/INLINE_BOLD_OFF)))))

  (it "wraps :c (inline code) in INLINE_CODE_ON/OFF sentinel pair"
    (let [out (ir-tui/ir->sentinel-strings
                [:ir [:p "use " [:c "send!"] " here"]]
                80)
          body (str/join "" (bodies out))]
      (expect (str/includes? body (str p/INLINE_CODE_ON "send!" p/INLINE_CODE_OFF)))))

  (it "sentinel adapter is a string-only contract (every entry begins with a marker)"
    (let [out (ir-tui/ir->sentinel-strings
                [:ir [:h {:level 1} "T"] [:p "x"] [:ul [:li "y"]] [:code "z"]]
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
;; :details / :summary disclosure widget
;; ---------------------------------------------------------------------------

(defdescribe details-walker-test
  (it ":details emits one summary line tagged :summary with toggle meta"
    (let [lines (ir-tui/ir->lines
                  [:ir [:details {:open? true}
                        [:summary "Click me"]
                        [:p "hidden body"]]]
                  80)
          summary-line (first (filter #(= :summary (:block-tag %)) lines))]
      (expect (some? summary-line))
      (expect (= :toggle-details (get-in summary-line [:meta :kind])))
      (expect (true? (get-in summary-line [:meta :open?])))
      (expect (string? (get-in summary-line [:meta :node-id])))))

  (it ":details {:open? true} body lines tagged :details-body"
    (let [lines (ir-tui/ir->lines
                  [:ir [:details {:open? true} [:summary "X"] [:p "body"]]]
                  80)
          body-lines (filter #(= :details-body (:block-tag %)) lines)]
      (expect (seq body-lines))
      (expect (some (fn [l] (some #(str/includes? (or (:text %) "") "body") (:runs l))) body-lines))))

  (it "`ir->entries` propagates `:meta` per line for the painter's click regions"
    (let [entries (ir-tui/ir->entries
                    [:ir [:p "intro"]
                     [:details {:open? true}
                      [:summary "toggle"]
                      [:p "body"]]]
                    80)
          summary (first (filter #(= :toggle-details (get-in % [:meta :kind])) entries))]
      (expect (some? summary))
      (expect (string? (:line summary)))
      ;; meta carries node-id + open? for state diff
      (expect (true? (get-in summary [:meta :open?])))
      (expect (string? (get-in summary [:meta :node-id]))))))
