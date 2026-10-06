(ns com.blockether.vis.tui.mermaid-test
  "Tests for the mermaid flowchart renderer.

   The renderer owns the `mermaid` fences the TUI can draw and answers nil for
   everything else, so the markdown walker can fall back to the source text.
   Coverage is golden rows for the small canonical shapes plus structural
   assertions for the layouts whose exact geometry may keep improving."
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.mermaid :as mermaid]
            [com.blockether.vis.tui.primitives :as p]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- rows ([source] (rows source 72)) ([source width] (mermaid/diagram source width)))

(defn- picture
  ([source] (picture source 72))
  ([source width]
   (some->> (rows source width)
            (str/join "\n"))))

(defdescribe node-test
             (it "a lone node is a box around its label"
                 (expect (= ["┌──────┐" "│ Only │" "└──────┘"] (rows "flowchart TD\n  A[Only]\n"))))
             (it "round and diamond shapes keep their own chrome"
                 (expect (= ["╭───────╮" "│ Round │" "╰───────╯" "    │" "    │" "    ▼"
                             "╱────────╲" "│ Choice │" "╲────────╱"]
                            (rows "flowchart TD\n  A([Round]) --> B{Choice}\n"))))
             (it "a quoted label keeps the characters mermaid would escape"
                 (expect (str/includes? (picture "flowchart TD\n  A[\"a --> b\"]\n") "a --> b")))
             (it "a <br/> label becomes two rows inside one box"
                 (let [drawn (picture "flowchart TD\n  A[\"one<br/>two\"]\n")]
                   (expect (str/includes? drawn "one") drawn)
                   (expect (str/includes? drawn "two") drawn)))
             (it "an HTML tag inside a label is stripped"
                 (expect (not (str/includes? (picture "flowchart TD\n  A[\"<b>bold</b>\"]\n")
                                             "<b>")))))

(defdescribe
  direction-test
  (it "TD stacks the ranks and points down"
      (expect (= ["┌─────┐" "│ One │" "└─────┘" "   │" "   │" "   ▼" "┌─────┐" "│ Two │" "└─────┘"]
                 (rows "flowchart TD\n  A[One] --> B[Two]\n"))))
  (it "LR lays the ranks out in columns and points right"
      (expect (= ["┌─────┐     ┌─────┐" "│ One │────▶│ Two │" "└─────┘     └─────┘"]
                 (rows "flowchart LR\n  A[One] --> B[Two]\n"))))
  (it "BT points up"
      (expect (some #(str/includes? % "▲") (rows "flowchart BT\n  A[One] --> B[Two]\n"))))
  (it "RL points left"
      (expect (some #(str/includes? % "◀") (rows "flowchart RL\n  A[One] --> B[Two]\n"))))
  (it "`graph` is accepted as the header keyword"
      (expect (= (rows "flowchart LR\n  A[One] --> B[Two]\n")
                 (rows "graph LR\n  A[One] --> B[Two]\n")))))

(defdescribe link-test
             (it "a dotted link is drawn dotted and a thick link heavy"
                 (expect (= ["┌─────┐     ┌─────┐     ┌───────┐" "│ One │╌╌╌╌▶│ Two │━━━━▶│ Three │"
                             "└─────┘     └─────┘     └───────┘"]
                            (rows "flowchart LR\n  A[One] -.-> B[Two]\n  B ==> C[Three]\n"))))
             (it "an edge label rides next to its edge"
                 (expect (some #(str/includes? % "ok")
                               (rows "flowchart TD\n  A[One] -->|ok| B[Two]\n"))))
             (it "`-- text -->` is the same edge as `-->|text|`"
                 (expect (= (rows "flowchart TD\n  A[One] -->|ok| B[Two]\n")
                            (rows "flowchart TD\n  A[One] -- ok --> B[Two]\n"))))
             (it "an open link draws no arrowhead"
                 (expect (not (str/includes? (picture "flowchart LR\n  A[One] --- B[Two]\n") "▶"))))
             (it "a chain declares both edges"
                 (let [drawn (picture "flowchart LR\n  A[One] --> B[Two] --> C[Three]\n")]
                   (expect (str/includes? drawn "One") drawn)
                   (expect (str/includes? drawn "Three") drawn)))
             (it "an `&` list fans every source into the target"
                 (let [drawn (picture "flowchart TD\n  A[One] & B[Two] --> C[Three]\n")]
                   (expect (str/includes? drawn "One") drawn)
                   (expect (str/includes? drawn "Two") drawn)
                   (expect (str/includes? drawn "Three") drawn))))

(defdescribe
  loop-test
  (it "a back edge rejoins the trunk at a junction, leaving the arrowhead its own cell"
      (let [drawn (rows "flowchart TD\n  A[Start] --> B[Run]\n  B --> C[Check]\n  C --> B\n")]
        (expect (some #(str/includes? % "├") drawn) drawn)
        (expect (not-any? #(re-find #"[▼▲][─━╌]" %) drawn) drawn)))
  (it "a sideways back edge keeps its label"
      (expect
        (some #(str/includes? % "again")
              (rows "flowchart LR\n  A[Read] --> B[Parse]\n  B --> C[Emit]\n  C -->|again| B\n"))))
  (it "a self loop still renders" (expect (seq (rows "flowchart TD\n  A[One] --> A\n")))))

(defdescribe decoration-test
             (it "classDef, class, style and click lines are ignored"
                 (expect (= (rows "flowchart TD\n  A[One] --> B[Two]\n")
                            (rows (str "flowchart TD\n"
                                       "  classDef hot fill:#f00\n" "  A[One] --> B[Two]\n"
                                       "  class A hot\n" "  style B fill:#0f0\n"
                                       "  linkStyle 0 stroke:#333\n" "  click A callback\n")))))
             (it "a comment line is ignored"
                 (expect (= (rows "flowchart TD\n  A[One] --> B[Two]\n")
                            (rows "flowchart TD\n  %% a note\n  A[One] --> B[Two]\n")))))

(defdescribe
  width-test
  (it "every row fits the budget"
      (doseq [width [40 56 72 100]]
        (let [drawn (rows (str "flowchart TD\n"
                               "  A[Gateway] --> B[Engine]\n" "  A --> C[Sandbox]\n"
                               "  B --> D[(Store)]\n" "  C --> D\n")
                          width)]
          (expect (seq drawn) (str "no diagram at width " width))
          (expect (every? #(<= (p/display-width %) width) drawn)
                  (str "overflow at width " width ": " (pr-str drawn))))))
  (it "labels shrink before the renderer gives up"
      (expect (seq (rows (str "flowchart LR\n"
                              "  A[A long descriptive label here] --> B[Another long label]\n")
                         44))))
  (it "a diagram that cannot fit answers nil"
      (expect (nil? (rows "flowchart LR\n  A[One] --> B[Two]\n  B --> C[Three]\n" 8))))
  (it "a non-positive width answers nil" (expect (nil? (rows "flowchart TD\n  A[One]\n" 0)))))

(defdescribe unsupported-test
             (it "another diagram type is not ours"
                 (expect (nil? (rows "sequenceDiagram\n  Alice->>Bob: hi\n")))
                 (expect (nil? (rows "classDiagram\n  Animal <|-- Duck\n")))
                 (expect (nil? (rows "stateDiagram-v2\n  [*] --> Idle\n"))))
             (it "an unparsable statement gives the whole fence back"
                 (expect (nil? (rows "flowchart TD\n  A[One] --> \n"))))
             (it "an empty fence answers nil"
                 (expect (nil? (rows "")))
                 (expect (nil? (rows "flowchart TD\n")))))

;; Regression for https://github.com/Blockether/vis/issues/319: a valid
;; flowchart with a subgraph legend, `:::class` suffixes and `~~~` links fell
;; back to its source text.
(def ^:private issue-319-fence
  (str "graph LR\n"
       "  E[\"Epic\"] --> S1[\"Story A\"]\n"
       "  S1 --> T1[\"Task 1\"] --> G1[\"Change 1<br/>V+1\"]\n"
       "  S1 --> T2[\"Task 2\"] --> G2[\"Change 2<br/>V-1\"]\n"
       "  G2 -. \"tests code from\" .-> G1\n"
       "  E --> S2[\"Story B\"] --> D1[\"Done\"]\n" "  subgraph Legend\n"
       "    direction TB\n" "    L1[\"Merged, task not Done\"]:::warn\n"
       "    L2[\"Open\"]:::ok\n" "    L1 ~~~ L2\n"
       "  end\n" "  D1 ~~~~~ Legend\n"
       "  classDef warn fill:#fff176,stroke:#f57f17;\n"
       "  classDef ok fill:#c8f7c5,stroke:#2e7d32;\n"))

(defdescribe
  issue-319-test
  (it "the flowchart from the report renders as a diagram"
      (doseq [width [60 100 118]]
        (let [drawn (picture issue-319-fence width)]
          (expect (some? drawn) (str "no diagram at width " width))
          (expect (str/includes? drawn "Story B") drawn)
          (expect (str/includes? drawn "Merged") drawn)
          (expect (str/includes? drawn "Open") drawn))))
  (it "a subgraph is flattened: its nodes stay, its header and id draw no box"
      (let [drawn (picture "flowchart TD\n  subgraph one [Group]\n  A[One] --> B[Two]\n  end\n")]
        (expect (str/includes? drawn "One") drawn)
        (expect (not (str/includes? drawn "Group")) drawn)
        (expect (not (str/includes? drawn "one")) drawn)))
  (it "a `:::class` suffix is dropped"
      (expect (= (rows "flowchart TD\n  A[One] --> B[Two]\n")
                 (rows "flowchart TD\n  A[One]:::hot --> B[Two]:::cold\n"))))
  (it "a `~~~` link keeps its nodes and draws no line"
      (let [drawn (rows "flowchart LR\n  A[One] ~~~ B[Two]\n")]
        (expect (some #(str/includes? % "Two") drawn) drawn)
        (expect (not-any? #(re-find #"│[─╌━]|▶" %) drawn) drawn)))
  (it "a sideways chart too wide for the bubble is stacked top-down"
      (let [source "flowchart LR\n  A[One] --> B[Two] --> C[Three] --> D[Four] --> E[Five]\n"]
        (expect (some #(str/includes? % "▼") (rows source 30)) (pr-str (rows source 30))))))

(defdescribe reason-test
             (it "a fence that is not drawn answers the reason"
                 (expect (= {:reason "only flowcharts are drawn, not sequenceDiagram"}
                            (mermaid/draw "sequenceDiagram\n  Alice->>Bob: hi\n" 60)))
                 (expect (= {:reason "cannot read: A[One] -->"}
                            (mermaid/draw "flowchart TD\n  A[One] --> \n" 60)))
                 (expect (= {:reason "empty flowchart"} (mermaid/draw "flowchart TD\n" 60))))
             (it "a graph too wide at every label limit names the width it needs"
                 (expect (re-matches #"too wide: \d+ > 8 cols"
                                     (:reason
                                       (mermaid/draw
                                         "flowchart LR\n  A[One] --> B[Two]\n  B --> C[Three]\n"
                                         8))))))
