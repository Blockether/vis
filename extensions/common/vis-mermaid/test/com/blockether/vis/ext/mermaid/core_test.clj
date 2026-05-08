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
      (expect (= "Mermaid (flowchart)" (first out)))
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
      (expect (= "Mermaid (sequence)" (first out)))
      (expect (some #(str/includes? % "User -> API: Submit") out))
      (expect (some #(str/includes? % "API ⇢ User: OK") out))))

  (it "clips output to the requested width"
    (let [out (:lines (mermaid/render-mermaid
                        {:width 24
                         :source "flowchart LR\n  A[Start with very long label] --> B[End with very long label]"}))]
      (expect (every? #(<= (count %) 24) out))))

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
          (expect (str/includes? (first out) "Mermaid")))))))
