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
