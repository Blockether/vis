;; Pins the TUI glyph policy: the client draws only the non-ASCII glyphs that
;; opencode's TUI draws (packages/tui) plus the opentui single and rounded border
;; styles that opencode uses.
(ns com.blockether.vis.tui.glyph-policy-test
  "The TUI source draws only the opencode glyph set."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe describe expect it]]))

(def ^:private opencode-glyphs
  "Non-ASCII glyphs that opencode's TUI draws, with the single and rounded
   opentui border characters."
  (set "·—•…←↑→↓↳⇆⊙⋯─│┃┌┐└┘├┤┬┴┼╭╮╯╰╹▀▄█■▣△▶▸▼▾◆◈◉○●⚙✓✕✗✱⟳⠇⠋⠏⠙⠦⠧⠴⠸⠹⠼⬖⬝⬥⬩⬪"))

(def ^:private input-data
  "Characters that only parse input text and are never drawn, by file name."
  {"attachments.clj" #{\uFEFF}
   "markdown_layout.clj" #{\u2011}
   "state.clj" #{\u3002 \uFF01 \uFF1F \u00BB \u2019 \u201D}})

(def ^:private regex-literal
  "A regex literal only matches input text, so its characters are not drawn."
  #"#\"(?:[^\"\\]|\\.)*\"")

(defn- violations
  "Return `[file line char]` for each non-ASCII character in `text` outside the
   opencode glyph set, the file's input data and regex literals."
  [file text]
  (let [exempt (get input-data file #{})]
    (for [[n line] (map-indexed vector (str/split-lines text))
          ch (str/replace line regex-literal "")
          :when (and (> (int ch) 127) (not (opencode-glyphs ch)) (not (exempt ch)))]

      [file (inc (long n)) ch])))

(defn- source-files
  []
  (->> (file-seq (io/file "src"))
       (filter #(str/ends-with? (.getName ^java.io.File %) ".clj"))))

(defdescribe
  glyph-policy-test
  (describe "violations"
            (it "flags a drawn glyph outside the opencode set"
                (expect (= [["a.clj" 2 \u00D7]]
                           (violations "a.clj" "(str \"ok\")\n(str \"\u00D7\")"))))
            (it "accepts opencode glyphs, regex literals and declared input data"
                (expect (empty? (violations "a.clj" "(str \"\u2713 \u2502 \u256D\")")))
                (expect (empty? (violations "a.clj" "(re-find #\"[\u20AC\u00A3]\" s)")))
                (expect (empty? (violations "state.clj" "#{\\\u3002}")))
                (expect (= [["table.clj" 1 \u3002]] (violations "table.clj" "#{\\\u3002}")))))
  (it "every TUI source file draws only opencode glyphs"
      (let [files (source-files)]
        (expect (< 50 (count files)))
        (expect (= [] (vec (mapcat #(violations (.getName ^java.io.File %) (slurp %)) files)))))))
