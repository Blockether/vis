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
      (expect (some #(str/includes? % "User → API: Submit") out))
      (expect (some #(str/includes? % "API ⇢ User: OK") out))))

  (it "clips output to the requested width"
    (let [out (:lines (mermaid/render-mermaid
                        {:width 24
                         :source "flowchart LR\n  A[Start with very long label] --> B[End with very long label]"}))]
      (expect (every? #(<= (count %) 24) out)))))
