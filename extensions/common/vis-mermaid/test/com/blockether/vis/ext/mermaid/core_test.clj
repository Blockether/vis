(ns com.blockether.vis.ext.mermaid.core-test
  (:require
   [clojure.string :as str]
   [com.blockether.vis.ext.mermaid.core :as mermaid]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe pure-jvm-mermaid-renderer-test
  (it "parses flowcharts into an AST before layout/rendering"
    (let [ast (mermaid/parse-mermaid
                "flowchart TD\n  A[Start] --> B{Choice}\n  B -->|Yes| C[Do thing]")]
      (expect (= :flowchart (:diagram/type ast)))
      (expect (= "TD" (:direction ast)))
      (expect (= "Choice" (get-in ast [:nodes "B" :label])))
      (expect (= "Yes" (:label (second (:edges ast)))))))

  (it "builds a canvas layout for multi-node flowcharts"
    (let [layout (-> "flowchart TD\n  A[Start] --> B{Choice}\n  B -->|Yes| C[Do thing]\n  B -->|No| D[Skip]"
                   mermaid/parse-mermaid
                   mermaid/layout-diagram)]
      (expect (= :canvas (:layout/type layout)))
      (expect (contains? (:positions layout) "A"))
      (expect (contains? (:positions layout) "D"))))

  (it "renders a simple LR flowchart as boxed terminal text"
    (let [out (:lines (mermaid/render-mermaid
                        {:width 80
                         :source "flowchart LR\n  A[Start] --> B[End]"}))]
      (expect (not (str/includes? (first out) "Mermaid")))
      (expect (some #(str/includes? % "Start") out))
      (expect (some #(str/includes? % "──▶") out))))

  (it "renders labelled flowchart branches as readable edge rows"
    (let [out (:lines (mermaid/render-mermaid
                        {:width 80
                         :source "flowchart LR\n  A[Start] --> B{Choice}\n  B -->|Yes| C[Do thing]\n  B -->|No| D[Skip]"}))]
      (expect (some #(str/includes? % "Choice") out))
      (expect (some #(str/includes? % "Yes") out))
      (expect (some #(str/includes? % "No") out))))

  (it "renders sequence messages without JavaScript"
    (let [out (:lines (mermaid/render-mermaid
                        {:width 80
                         :source "sequenceDiagram\n  participant U as User\n  participant API as API\n  U->>API: Submit\n  API-->>U: OK"}))]
      (expect (not (str/includes? (first out) "Mermaid")))
      (expect (some #(str/includes? % "User -> API: Submit") out))
      (expect (some #(str/includes? % "API ⇢ User: OK") out))))

  (it "clips output to the requested width"
    (let [out (:lines (mermaid/render-mermaid
                        {:width 24
                         :source "flowchart LR\n  A[Start with very long label] --> B[End with very long label]"}))]
      (expect (every? #(<= (count %) 24) out))))

  (it "wraps long flowchart labels and Mermaid br tags instead of showing ellipses or HTML"
    (let [out (:lines (mermaid/render-mermaid
                        {:width 100
                         :source "flowchart TD\n  A[Assistant emits Clojure form<br/>with full readable text] --> B[Journal updated<br/>tool output preserved]"}))]
      (expect (some #(str/includes? % "Assistant emits Clojure") out))
      (expect (some #(str/includes? % "with full readable text") out))
      (expect (some #(str/includes? % "Journal updated") out))
      (expect (some #(str/includes? % "tool output preserved") out))
      (expect (not-any? #(str/includes? % "<br") out))
      (expect (not-any? #(str/includes? % "...") out))))

  (it "renders complex/cyclic flowcharts as readable wrapped adjacency instead of broken box routing"
    (let [source "flowchart TD\n  A[User request] --> B[Assistant emits Clojure forms]\n  B --> C[Journal updated<br/>tool results, errors, previews]\n  B --> D[Bindings<br/>named vars, *1 *2 *3 *e]\n  B --> E[System vars<br/>TURN_*, CONVERSATION_*]\n  B --> F[SQLite persistence]\n  C --> G[Assistant reads journal]\n  G --> H{Need more observation?}\n  H -- No --> I[Emit answer as single form]\n  H --> J[Call tools<br/>v/cat v/rg z/locators z/patch v/bash]\n  J --> K[SCI sandbox evaluates]\n  K --> C\n  I --> L[Rendered to user]"
          out (:lines (mermaid/render-mermaid {:width 100 :source source}))]
      (expect (some #(str/includes? % "Assistant emits Clojure forms") out))
      (expect (some #(str/includes? % "tool results, errors, previews") out))
      (expect (some #(str/includes? % "v/cat v/rg z/locators") out))
      (expect (some #(str/includes? % "No → Emit answer as single form") out))
      (expect (not-any? #(str/includes? % "<br") out))
      (expect (not-any? #(str/includes? % "...") out))
      (expect (not-any? #(re-find #"[┌┐┘╱╲]" %) out))))

  (it "uses the supplied terminal width for flowchart spacing"
    (let [source "flowchart LR\n  A[Start] --> B{Choice}\n  B -->|Yes| C[Do thing]\n  B -->|No| D[Skip]"
          w60 (apply max (map count (:lines (mermaid/render-mermaid {:width 60 :source source}))))
          w120 (apply max (map count (:lines (mermaid/render-mermaid {:width 120 :source source}))))]
      (expect (<= w60 60))
      (expect (<= w120 120))
      (expect (> w120 w60))))

  (it "recognizes every documented Mermaid diagram family with a pure JVM renderer"
    (let [samples [{:type :class :source "classDiagram\n  Animal <|-- Duck\n  class Animal"}
                   {:type :state :source "stateDiagram-v2\n  [*] --> Still\n  Still --> Moving: go"}
                   {:type :er :source "erDiagram\n  CUSTOMER ||--o{ ORDER : places"}
                   {:type :journey :source "journey\n  title My day\n  section Go\n  Wake up: 5: Me"}
                   {:type :gantt :source "gantt\n  title Plan\n  section Build\n  Task A :a1, 2026-01-01, 1d"}
                   {:type :pie :source "pie title Pets\n  \"Dogs\" : 386\n  \"Cats\" : 85"}
                   {:type :gitgraph :source "gitGraph\n  commit\n  branch dev\n  checkout dev\n  commit"}
                   {:type :mindmap :source "mindmap\n  root\n    A\n    B"}
                   {:type :timeline :source "timeline\n  title Releases\n  2024 : Alpha\n  2025 : Beta"}
                   {:type :quadrant :source "quadrantChart\n  title Reach\n  A: [0.3, 0.6]"}
                   {:type :requirement :source "requirementDiagram\n  requirement test_req {\n    id: 1\n  }"}
                   {:type :c4 :source "C4Context\n  Person(user, User)\n  System(app, App)\n  Rel(user, app, Uses)"}
                   {:type :sankey :source "sankey-beta\n  A,B,10\n  B,C,5"}
                   {:type :xychart :source "xyChart-beta\n  title Sales\n  x-axis [jan, feb]\n  line [1, 2]"}
                   {:type :block :source "block-beta\n  columns 2\n  A B"}
                   {:type :architecture :source "architecture-beta\n  group api(cloud)[API]\n  service db(database)[DB]"}
                   {:type :packet :source "packet-beta\n  0-15: Source Port"}
                   {:type :kanban :source "kanban\n  Todo\n    Task"}
                   {:type :treemap :source "treemap-beta\n  Root\n    Child: 10"}
                   {:type :radar :source "radar-beta\n  axis A\n  curve X [1]"}
                   {:type :ishikawa :source "ishikawa\n  Problem\n    Cause"}
                   {:type :tree :source "treeView\n  root\n    child"}]]
      (doseq [{:keys [type source]} samples]
        (let [ast (mermaid/parse-mermaid source)
              out (:lines (mermaid/render-mermaid {:width 100 :source source}))]
          (expect (= type (:diagram/type ast)))
          (expect (seq out))
          (expect (not (str/includes? (first out) "Mermaid"))))))))
