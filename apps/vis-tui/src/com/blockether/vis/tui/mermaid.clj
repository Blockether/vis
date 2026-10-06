(ns com.blockether.vis.tui.mermaid
  "Mermaid fences painted as terminal pictures in the colours of the TUI theme.

   `draw` is TOTAL: it answers `{:rows rows}`, or `{:reason text}` when the fence
   is not a diagram this renderer understands or the picture cannot fit the
   bubble. The caller then paints the fence source verbatim with the reason, so
   an unsupported diagram never shows a broken picture. `diagram` answers only
   the rows, or nil.

   A row is a string with ANSI SGR colour codes (see `mermaid.canvas`). This
   namespace reads the front matter, the `%%{init}%%` directives and the header
   keyword, then gives the body to the renderer of that diagram type."
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.mermaid.canvas :as c]
            [com.blockether.vis.tui.mermaid.charts :as charts]
            [com.blockether.vis.tui.mermaid.gitgraph :as gitgraph]
            [com.blockether.vis.tui.mermaid.graphs :as graphs]
            [com.blockether.vis.tui.mermaid.railroad :as railroad]
            [com.blockether.vis.tui.mermaid.sequence :as sequence]
            [com.blockether.vis.tui.mermaid.trees :as trees]))

(defn- front-matter
  "[title lines-after yaml] of `lines` that may start with a `---` YAML block."
  [lines]
  (let [lines (drop-while str/blank? lines)]
    (if (= "---"
           (some-> (first lines)
                   str/trim))
      (let [[yaml after] (split-with #(not= "---" (str/trim %)) (rest lines))
            title (some (fn [line]
                          (when-let [[_ title] (re-matches #"title:\s*(.*)" (str/trim line))]
                            (when (= line (str/triml line)) (c/clean-label title))))
                        yaml)]

        [title (rest after) yaml])
      [nil lines nil])))

(def ^:private renderers
  "Header keyword (lower case) -> renderer of `{:kind :args :lines}` and a width."
  {"flowchart" graphs/flowchart
   "graph" graphs/flowchart
   "swimlane-beta" graphs/flowchart
   "agentflow-beta" graphs/flowchart
   "classdiagram" graphs/class-diagram
   "classdiagram-v2" graphs/class-diagram
   "statediagram" graphs/state-diagram
   "statediagram-v2" graphs/state-diagram
   "erdiagram" graphs/er-diagram
   "requirementdiagram" graphs/requirement-diagram
   "c4context" graphs/c4-diagram
   "c4container" graphs/c4-diagram
   "c4component" graphs/c4-diagram
   "c4dynamic" graphs/c4-diagram
   "c4deployment" graphs/c4-diagram
   "architecture-beta" graphs/architecture-diagram
   "usecase-beta" graphs/usecase-diagram
   "block" graphs/block-diagram
   "block-beta" graphs/block-diagram
   "eventmodeling" charts/event-modeling
   "sequencediagram" sequence/sequence-diagram
   "zenuml" sequence/zenuml
   "gitgraph" gitgraph/git-graph
   "gitgraph:" gitgraph/git-graph
   "pie" charts/pie
   "gantt" charts/gantt
   "xychart" charts/xy-chart
   "xychart-beta" charts/xy-chart
   "quadrantchart" charts/quadrant-chart
   "radar-beta" charts/radar
   "sankey" charts/sankey
   "sankey-beta" charts/sankey
   "journey" charts/journey
   "packet" charts/packet
   "packet-beta" charts/packet
   "wardley-beta" charts/wardley
   "venn-beta" charts/venn
   "cynefin-beta" charts/cynefin
   "mindmap" trees/mindmap
   "treeview-beta" trees/tree-view
   "treemap" trees/treemap
   "treemap-beta" trees/treemap
   "ishikawa-beta" trees/ishikawa
   "ishikawa" trees/ishikawa
   "timeline" trees/timeline
   "kanban" trees/kanban
   "railroad-beta" railroad/railroad
   "railroad-ebnf-beta" railroad/railroad
   "railroad-abnf-beta" railroad/railroad
   "railroad-peg-beta" railroad/railroad})

(defn- read-source
  "`{:kind :args :lines :title :settings}` of a Mermaid source, or nil when it
   is empty. `:settings` joins the front matter and the `%%{init}%%` lines."
  [source]
  (let [[title lines yaml]
        (front-matter (str/split-lines (or source "")))

        directives
        (filter #(re-find #"^\s*%%\{" %) lines)

        lines
        (remove #(re-find #"^\s*%%" %) lines)

        [blank lines]
        (split-with str/blank? lines)

        header
        (first lines)]

    (when header
      (let [[_ kind args] (re-find #"^\s*(\S+)\s*(.*)$" header)]
        {:kind (str/lower-case kind)
         :args (str/trim (str/replace args #"%%.*$" ""))
         :lines (vec (rest lines))
         :skipped (count blank)
         :title title
         :settings (str/join "\n" (concat yaml directives))}))))

(defn- with-title
  "`drawing` with the front matter title, else the title of the diagram, on top."
  [{:keys [rows] :as drawing} title width]
  (if-let [title (when rows (first (remove str/blank? [title (:title drawing)])))]
    (-> drawing
        (assoc :rows (into [(c/seg-row [[(c/clip title width) :text]]) ""] rows))
        (dissoc :title))
    drawing))

(defn draw
  "`{:rows rows}` of `source` painted inside `width` columns, else `{:reason
   text}` that says why this renderer does not draw it."
  [source width]
  (let [width
        (long (or width 0))

        {:keys [kind title] :as diagram}
        (read-source source)

        renderer
        (get renderers kind)]

    (cond (not (pos? width)) {:reason "no width"}
          (nil? diagram) {:reason "empty diagram"}
          (nil? renderer) {:reason (str "unknown diagram type: " (c/clip kind 30))}
          :else
          (let [drawing
                (try (renderer diagram width)
                     (catch Exception e
                       {:reason (str "cannot read: "
                                     (c/clip (or (ex-message e) (str (class e))) 60))}))

                drawing
                (with-title drawing title width)]

            (cond
              (:reason drawing) drawing
              (empty? (:rows drawing)) {:reason "empty diagram"}
              (some #(> (c/row-width %) width) (:rows drawing))
              {:reason
               (str "too wide: " (apply max (map c/row-width (:rows drawing))) " > " width " cols")}
              :else {:rows (vec (:rows drawing))})))))

(defn diagram
  "Rows of `source` painted inside `width` columns, or nil when this renderer
   does not own the fence or cannot fit it."
  [source width]
  (:rows (draw source width)))
