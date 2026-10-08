(ns com.blockether.vis.tui.mermaid.graphs
  "Mermaid diagram types that are boxes and links: flowcharts and their beta
   variants, class, state, entity-relationship, requirement, C4, architecture
   and use case diagrams. Each parser builds the graph shape of
   `mermaid.graph`, which ranks, places and paints it. Block diagrams are a
   grid, so they get their own layout here.

   The parsers of the newer diagram types skip a statement that they do not
   know, so a new Mermaid keyword costs a detail of the picture, not the picture."
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.mermaid.canvas :as c]
            [com.blockether.vis.tui.mermaid.graph :as g]))

(def ^:private flow-of {"TD" :down "TB" :down "BT" :up "LR" :right "RL" :left})

(defn- flow-in
  "Flow of a header argument or of a top-level `direction` statement."
  [args lines]
  (or (get flow-of
           (some-> (re-find #"(?i)\b(TD|TB|BT|LR|RL)\b" (or args ""))
                   second
                   str/upper-case))
      (some (fn [line]
              (when-let [[_ dir] (re-find #"(?i)^direction\s+(TD|TB|BT|LR|RL)\b" (str/trim line))]
                (flow-of (str/upper-case dir))))
            lines)
      :down))

(def ^:private empty-graph {:order [] :nodes {} :edges []})

(defn- edge
  [from to & {:as opts}]
  (merge {:from from :to to :style :solid :head? true :tail? false :label ""} opts))

(defn- add-edge [graph e] (update graph :edges conj e))

(defn- ensure-node
  "Add node `id` unless it is known, with `defaults` for a new one."
  [graph id defaults]
  (if (get-in graph [:nodes id])
    graph
    (g/add-node graph (merge {:id id :label id :shape :rect} defaults))))

(defn- update-node [graph id f & args] (apply update-in graph [:nodes id] f args))

(defn- add-note
  "A note box linked by a dotted line to the node `target`, or a free note."
  [graph target text]
  (let [id (str "\u0000note-" (count (:order graph)))]
    (cond-> (g/add-node graph {:id id :label (c/clean-label text) :shape :round})
      true
      (update-node id assoc :tone :yellow :text-tone :yellow)

      target
      (ensure-node target {})

      target
      (add-edge (edge target id :style :dotted :head? false)))))

(defn- skip-blocks
  "Lines without multi-line `accDescr { ... }` and `json X@{ ... }` blocks. A
   `json` block becomes one plain node line."
  [lines]
  (loop [lines
         (seq lines)

         out
         []]

    (if-let [line (first lines)]
      (let [t (str/trim line)]
        (cond (re-find #"(?i)^accdescr\s*\{" t)
              (recur (rest (drop-while #(not (str/includes? % "}")) lines)) out)
              (re-find #"^json\s+\S+@\{" t) (let [[_ id] (re-find #"^json\s+([^@\s]+)@\{" t)
                                                  after (drop-while #(not (re-find #"^\s*\}" %))
                                                                    (rest lines))]

                                              (recur (rest after) (conj out (str id "[" id "]"))))
              :else (recur (rest lines) (conj out line))))
      out)))

(defn- slug [label] (str/replace label #"[^\p{L}\p{N}_]+" "_"))

(defn- prepare-flow-line
  "Rewrite use case and agent-flow statements into flowchart statements."
  [line]
  (let [t (str/trim line)]
    (cond (re-find #"(?i)^(?:systemboundary|boundary)\b" t)
          (str "subgraph "
               (-> t
                   (str/replace #"(?i)^(?:systemboundary|boundary)\s*" "")
                   (str/replace #"@\{[^}]*\}" "")
                   (str/replace #":::\S+" "")))
          (re-find #"^actor\b" t)
          (let [[_ id label] (re-find #"^actor\s+([^\s(\[@:<]+)(?:\(\"?([^\")]*)\"?\))?" t)]
            (str id "([" (or label id) "])"))
          (re-find #"^note\s+for\s+" t) t
          (re-find #"^flow\s+" t) (str "subgraph " (subs t 5))
          (re-find #"^connector\s+" t) (str/replace (subs t 10) #"@\{[^}]*\}" "")
          :else (-> t
                    (str/replace #"\s[A-Za-z0-9_]+@(?=[-=.~<ox])" " ")
                    (str/replace #"--\|>" "-->")
                    (str/replace #"^\"([^\"]+)\"(?=\s|$)"
                                 (fn [[_ label]]
                                   (str (slug label) "(\"" label "\")")))
                    (str/replace #"(?<=\s)\"([^\"]+)\"$"
                                 (fn [[_ label]]
                                   (str (slug label) "(\"" label "\")")))
                    (str/replace #"\s*<<[^>]*>>" "")
                    (str/replace #"@\{[^}]*\}" "")
                    (str/replace #"^(\S+)\s*(\.\.>|-->|--)\s*:\s*(\S+)\s+(\S+)$"
                                 "$1 $2|<<$3>>| $4")))))

(defn- note-statements
  "[lines notes] where `note for X \"text\"` statements move into `notes`."
  [lines]
  (let [note? #(re-find #"^\s*note\s+for\s+" %)]
    [(remove note? lines)
     (for [line (filter note? lines)
           :let [[_ target text] (re-find #"^\s*note\s+for\s+(\S+)\s+(.*)$" line)]
           :when target]

       [target text])]))

(defn flowchart
  "Flowchart, swimlane, agent flow and the flowchart-like body of other types."
  [{:keys [kind args lines]} width]
  (let [usecase?
        (= "usecase-beta" kind)

        lines
        (cond->> (skip-blocks lines)
          (not= "flowchart" kind)
          (map prepare-flow-line))

        [lines notes]
        (note-statements lines)

        {:keys [graph reason]}
        (g/parse-flowchart lines (flow-in args lines))]

    (if reason
      {:reason reason}
      (g/fit-graph (cond-> (reduce (fn [graph [target text]]
                                     (add-note graph target text))
                                   graph
                                   notes)
                     usecase?
                     (update :nodes
                             update-vals
                             (fn [node]
                               (case (:shape node)
                                 :stadium
                                 (assoc node :tone (or (:tone node) :blue))

                                 :rect
                                 (if (= (:label node) (:id node)) (assoc node :shape :round) node)

                                 node))))
                   width))))

(defn usecase-diagram [diagram width] (flowchart diagram width))

;; Style statements shared by the diagrams below

(defn- read-style
  "Record a `style`, `classDef`, `class` or `cssClass` statement in `graph`, or nil."
  [graph statement]
  (let [[_ word a b]
        (re-find #"(?i)^(style|classdef|class|cssclass)\s+(\"[^\"]*\"|\S+)\s*(.*)$" statement)

        a
        (some-> a
                (str/replace "\"" ""))]

    (case (some-> word
                  str/lower-case)
      "style"
      (update graph :styles assoc a b)

      "classdef"
      (reduce #(assoc-in %1 [:class-styles (str/trim %2)] b) graph (str/split a #","))

      ("class" "cssclass")
      (when (and (seq b) (not (re-find #"[{\[~(]" b)) (not (str/includes? b ":")))
        (reduce #(assoc-in %1 [:node-classes (str/trim %2)] (str/trim b)) graph (str/split a #",")))

      nil)))

(defn- strip-class-suffix
  "[text class-name] of `text` without its `:::name` suffix."
  [text]
  (if-let [[_ head class-name] (re-matches #"(.*?):::([\w\-]+)(.*)" text)]
    [(str head) class-name]
    [text nil]))

(defn- finish
  "Graph with its style tones applied, drawn inside `width`."
  [graph flow width]
  (g/fit-graph (-> graph
                   g/apply-styles
                   (assoc :flow flow))
               width))

;; Class diagrams

(def ^:private class-rel-re
  #"^([\w$]+(?:~[^~]+~)?)\s*(?:\"([^\"]*)\"\s*)?(<\||\*|o|<|\(\))?(--|\.\.)(\|>|\*|o|>|\(\))?\s*(?:\"([^\"]*)\"\s*)?([\w$]+(?:~[^~]+~)?)\s*(?::\s*(.*))?$")

(def ^:private class-end
  {"<|" :triangle "|>" :triangle "*" :diamond "o" :hollow-diamond "()" :circle})

(defn- generic [s] (str/replace (str s) #"~([^~]*)~" "<$1>"))

(defn- class-id [s] (str/replace (str s) #"~[^~]*~" ""))

(defn- add-class
  [graph raw]
  (let [[raw class-name]
        (strip-class-suffix (str/trim raw))

        [_ id-part label]
        (re-matches #"([^\[]+?)\s*(?:\[\"?([^\]\"]*)\"?\])?" raw)

        id
        (class-id (str/trim id-part))]

    (cond-> (ensure-node graph id {:label (generic (str/trim id-part)) :text-tone :cyan})
      label
      (update-node id assoc :label label)

      class-name
      (update-node id assoc :class class-name))))

(defn- add-member
  [graph id member]
  (let [member (generic (str/trim member))]
    (if-let [[_ annotation] (re-matches #"<<(.+)>>" member)]
      (update-node graph id update :label #(str "<<" annotation ">>\n" %))
      (update-node graph id update :members (fnil conj []) member))))

(defn- member-sections
  [node]
  (let [members
        (:members node)

        methods
        (filterv #(str/includes? % "(") members)

        fields
        (filterv #(not (str/includes? % "(")) members)]

    (cond-> (dissoc node :members)
      (seq members)
      (assoc :sections
        (filterv seq
          [fields
           (mapv (fn [m]
                   [m :green])
                 methods)])))))

(defn class-diagram
  [{:keys [args lines]} width]
  (loop [lines
         (seq (g/statements lines))

         graph
         empty-graph

         open
         nil]

    (if-let [line (first lines)]
      (cond open (if (str/starts-with? line "}")
                   (recur (rest lines) graph nil)
                   (recur (rest lines) (add-member graph open line) open))
            (re-find #"^class\s+" line)
            (let [body (str/replace line #"^class\s+" "")
                  opens? (str/ends-with? body "{")
                  head (str/trim (str/replace body #"\{\s*\}?$" ""))
                  graph (add-class graph head)
                  id (class-id (first (str/split (first (strip-class-suffix head)) #"[\s\[]")))]

              (recur (rest lines) graph (when (and opens? (not (str/ends-with? body "}"))) id)))
            (re-find #"^namespace\b|^\}$|^direction\b|^(?:click|link|callback)\b" line)
            (recur (rest lines) graph nil)
            (re-find #"^<<.+>>\s+\S+$" line)
            (let [[_ annotation id] (re-find #"^<<(.+)>>\s+(\S+)$" line)]
              (recur
                (rest lines)
                (add-member (ensure-node graph id {:text-tone :cyan}) id (str "<<" annotation ">>"))
                nil))
            (re-find #"^note\b" line)
            (let [[_ target text] (or (re-find #"^note\s+for\s+(\S+)\s+(.*)$" line)
                                      (re-find #"^note\s+()(.*)$" line))]
              (recur (rest lines) (add-note graph (not-empty target) text) nil))
            :else
            (if-let [[_ a card-a left link right card-b b label] (re-matches class-rel-re line)]
              (let [graph (-> graph
                              (add-class a)
                              (add-class b))
                    text (str/join " "
                                   (remove str/blank?
                                     [card-a
                                      (some-> label
                                              c/clean-label) card-b]))]

                (recur (rest lines)
                       (add-edge graph
                                 (edge (class-id a)
                                       (class-id b)
                                       :style (if (= ".." link) :dotted :solid)
                                       :head? (boolean right)
                                       :tail? (boolean left)
                                       :head-kind (class-end right)
                                       :tail-kind (class-end left)
                                       :label text))
                       nil))
              (if-let [[_ id member] (re-matches #"([\w$]+(?:~[^~]+~)?)\s*:\s*(.+)" line)]
                (recur (rest lines) (add-member (add-class graph id) (class-id id) member) nil)
                (recur (rest lines) (or (read-style graph line) graph) nil))))
      (finish (update graph :nodes update-vals member-sections) (flow-in args lines) width))))

;; State diagrams

(defn- state-start [scope] (str "\u0000start:" scope))

(defn- state-end [scope] (str "\u0000end:" scope))

(defn- state-ref
  "[graph id] of a state reference inside `scope`; `[*]` is the start when
   `start?`, else the end of that scope."
  [graph raw scope start?]
  (let [[raw class-name] (strip-class-suffix (str/trim raw))]
    (if (= "[*]" raw)
      (let [id (if start? (if scope scope (state-start scope)) (state-end scope))]
        [(if (get-in graph [:nodes id])
           graph
           (-> graph
               (g/add-node {:id id :label (if start? "●" "◉") :shape :circle})
               (update-node id assoc :text-tone (if start? :green :red)))) id])
      [(cond-> (ensure-node graph raw {:shape :round})
         class-name
         (update-node raw assoc :class class-name)) raw])))

(defn state-diagram
  [{:keys [args lines]} width]
  (loop [lines
         (seq (g/statements lines))

         graph
         empty-graph

         scopes
         ()

         note
         nil]

    (if-let [line (first lines)]
      (let [scope (first scopes)]
        (cond note (if (re-find #"(?i)^end\s+note$" line)
                     (recur (rest lines)
                            (add-note graph (first note) (str/join "\n" (second note)))
                            scopes
                            nil)
                     (recur (rest lines) graph scopes [(first note) (conj (second note) line)]))
              (re-find #"^\}$" line) (recur (rest lines) graph (rest scopes) nil)
              (re-find #"^--$|^direction\b|^(?:hide|scale|click)\b" line)
              (recur (rest lines) graph scopes nil)
              (re-find #"^note\s+(?:left|right|top|bottom)\s+of\s+" line)
              (let [[_ target text] (re-find #"^note\s+\S+\s+of\s+([^\s:]+)\s*(?::\s*(.*))?$" line)]
                (if text
                  (recur (rest lines) (add-note graph target text) scopes nil)
                  (recur (rest lines) graph scopes [target []])))
              (re-find #"^state\s+" line)
              (let [body (str/trim (str/replace line #"^state\s+" ""))
                    opens? (str/ends-with? body "{")
                    body (str/trim (str/replace body #"\{$" ""))
                    [_ described id-a] (re-matches #"\"([^\"]*)\"\s+as\s+(\S+)" body)
                    [_ id-b kind] (re-matches #"(\S+)\s*<<(\w+)>>" body)
                    [_ id-c described-c] (re-matches #"([^\s:]+)\s*:\s*(.*)" body)
                    id (or id-a id-b id-c (first (str/split body #"\s+")))
                    [id class-name] (strip-class-suffix id)
                    graph (cond-> (ensure-node graph id {:shape :round})
                            described
                            (update-node id assoc :label described)

                            described-c
                            (update-node id assoc :sections [[described-c]])

                            class-name
                            (update-node id assoc :class class-name)

                            (= "choice" kind)
                            (update-node id assoc :shape :diamond :label " ")

                            (#{"fork" "join"} kind)
                            (update-node id assoc :shape :rect :label kind :tone :chrome))]

                (recur (rest lines) graph (if opens? (cons id scopes) scopes) nil))
              :else (if-let [[_ a b label] (re-matches #"(\S+)\s*-->\s*([^\s:]+)\s*(?::\s*(.*))?"
                                                       line)]
                      (let [[graph from] (state-ref graph a scope true)
                            [graph to] (state-ref graph b scope false)]

                        (recur (rest lines)
                               (add-edge graph (edge from to :label (c/clean-label label)))
                               scopes
                               nil))
                      (if-let [[_ id text] (or (re-matches #"([^\s:]+)\s*:\s*(.*)" line)
                                               (re-matches #"([\w\-]+)()" line))]
                        (recur (rest lines)
                               (-> graph
                                   (ensure-node id {:shape :round})
                                   (cond->
                                     (seq text)
                                     (update-node id
                                                  update
                                                  :sections
                                                  (fn [s]
                                                    [(conj (vec (first s)) text)]))))
                               scopes
                               nil)
                        (recur (rest lines) (or (read-style graph line) graph) scopes nil)))))
      (finish graph (flow-in args lines) width))))

;; Entity-relationship diagrams

(def ^:private er-symbols
  {"|o" "0..1" "o|" "0..1" "||" "1" "}o" "0..*" "o{" "0..*" "}|" "1..*" "|{" "1..*"})

(def ^:private er-words
  [[#"(?i)^(?:one or zero|zero or one)$" "0..1"]
   [#"(?i)^(?:one or more|one or many|many\(1\)|1\+)$" "1..*"]
   [#"(?i)^(?:zero or more|zero or many|many\(0\)|many|0\+)$" "0..*"]
   [#"(?i)^(?:only one|1|one)$" "1"]])

(defn- er-word
  [s]
  (some (fn [[re card]]
          (when (re-matches re (str/trim s)) card))
        er-words))

(def ^:private entity-re "(\"[^\"]+\"|[\\p{L}\\p{N}_\\-]+(?:\\[[^\\]]*\\])?)")

(def ^:private er-rel-re
  (re-pattern
    (str "^" entity-re "\\s*([|}][|o]|o\\|)(--|\\.\\.)([|o][|{])\\s*" entity-re "\\s*:\\s*(.*)$")))

(def ^:private er-word-re
  (re-pattern
    (str "^" entity-re "\\s+(.+?)\\s+(to|optionally to)\\s+(.+?)\\s+" entity-re "\\s*:\\s*(.*)$")))

(defn- add-entity
  [graph raw]
  (let [[raw class-name]
        (strip-class-suffix (str/trim raw))

        [_ id alias]
        (re-matches #"\"?([^\[\"]+)\"?(?:\[\"?([^\]\"]*)\"?\])?" raw)

        id
        (str/trim id)]

    (cond-> (ensure-node graph id {:text-tone :cyan})
      alias
      (update-node id assoc :label alias)

      class-name
      (update-node id assoc :class class-name))))

(defn- entity-key
  [raw]
  (str/trim (first (str/split (str/replace (first (strip-class-suffix (str/trim raw))) "\"" "")
                              #"\["))))

(defn er-diagram
  [{:keys [args lines]} width]
  (loop [lines
         (seq (g/statements lines))

         graph
         empty-graph

         open
         nil]

    (if-let [line (first lines)]
      (cond open (if (str/starts-with? line "}")
                   (recur (rest lines) graph nil)
                   (let
                     [[_ type attr keys _comment]
                      (re-matches
                        #"(\S+)\s+(\S+)\s*((?:PK|FK|UK)(?:\s*,\s*(?:PK|FK|UK))*)?\s*(?:\"(.*)\")?"
                        line)
                      row (str/join " " (remove str/blank? [type attr keys]))
                      row (if (str/blank? row) line row)]

                     (recur (rest lines)
                            (update-node graph
                                         open
                                         update
                                         :attrs
                                         (fnil conj [])
                                         (if (str/blank? keys) [row nil] [row :yellow]))
                            open)))
            (str/ends-with? line "{")
            (let [head (str/trim (subs line 0 (dec (count line))))]
              (recur (rest lines) (add-entity graph head) (entity-key head)))
            (re-find #"^direction\b" line) (recur (rest lines) graph nil)
            :else (if-let [[_ a left link right b label] (re-matches er-rel-re line)]
                    (recur (rest lines)
                           (-> graph
                               (add-entity a)
                               (add-entity b)
                               (add-edge (edge (entity-key a)
                                               (entity-key b)
                                               :style (if (= ".." link) :dotted :solid)
                                               :head? false
                                               :label (str (er-symbols left)
                                                           " " (c/clean-label label)
                                                           " " (er-symbols right)))))
                           nil)
                    (if-let [[_ a left to right b label] (re-matches er-word-re line)]
                      (recur (rest lines)
                             (-> graph
                                 (add-entity a)
                                 (add-entity b)
                                 (add-edge (edge (entity-key a)
                                                 (entity-key b)
                                                 :style (if (= "to" to) :solid :dotted)
                                                 :head? false
                                                 :label (str (er-word left)
                                                             " " (c/clean-label label)
                                                             " " (er-word right)))))
                             nil)
                      (if (re-matches (re-pattern (str entity-re "(?::::[\\w\\-]+)?")) line)
                        (recur (rest lines) (add-entity graph line) nil)
                        (recur (rest lines) (or (read-style graph line) graph) nil)))))
      (finish (update graph
                      :nodes
                      update-vals
                      (fn [node]
                        (cond-> (dissoc node :attrs)
                          (seq (:attrs node))
                          (assoc :sections [(:attrs node)]))))
              (flow-in args lines)
              width))))

;; Requirement diagrams

(def ^:private requirement-kinds
  {"requirement" "requirement"
   "functionalrequirement" "functional requirement"
   "interfacerequirement" "interface requirement"
   "performancerequirement" "performance requirement"
   "physicalrequirement" "physical requirement"
   "designconstraint" "design constraint"
   "element" "element"})

(defn- unquote-name [s] (str/trim (str/replace (str s) #"^\"|\"$" "")))

(defn requirement-diagram
  [{:keys [args lines]} width]
  (loop [lines
         (seq (g/statements lines))

         graph
         empty-graph

         open
         nil]

    (if-let [line (first lines)]
      (cond open (if (str/starts-with? line "}")
                   (recur (rest lines) graph nil)
                   (let [[_ k v] (re-matches #"(\w+)\s*:\s*(.*)" line)]
                     (recur (rest lines)
                            (cond-> graph
                              k
                              (update-node open
                                           update
                                           :sections
                                           (fn [s]
                                             [(conj
                                                (vec (first s))
                                                (str (str/capitalize k) ": " (unquote-name v)))])))
                            open)))
            (re-find #"(?i)^(\w+)\s+(\"[^\"]+\"|\S+)\s*\{$" line)
            (let [[_ kind name] (re-find #"^(\w+)\s+(\"[^\"]+\"|\S+)\s*\{$" line)
                  [name class-name] (strip-class-suffix (unquote-name name))
                  label (get requirement-kinds (str/lower-case kind) kind)
                  element? (= "element" label)]

              (recur (rest lines)
                     (cond-> (-> graph
                                 (ensure-node name {})
                                 (update-node name
                                              assoc
                                              :label (str "<<" label ">>\n" name)
                                              :text-tone (if element? :cyan :purple)
                                              :shape (if element? :round :rect)))
                       class-name
                       (update-node name assoc :class class-name))
                     name))
            :else
            (if-let [[_ a kind b]
                     (re-matches #"(\"[^\"]+\"|\S+)\s+-\s*(\w+)\s*->\s+(\"[^\"]+\"|\S+)" line)]
              (let [a (unquote-name a)
                    b (unquote-name b)]

                (recur (rest lines)
                       (-> graph
                           (ensure-node a {})
                           (ensure-node b {})
                           (add-edge (edge a b :label (str "<<" kind ">>") :style :dotted)))
                       nil))
              (if-let [[_ b kind a]
                       (re-matches #"(\"[^\"]+\"|\S+)\s+<-\s*(\w+)\s*-\s+(\"[^\"]+\"|\S+)" line)]
                (let [a (unquote-name a)
                      b (unquote-name b)]

                  (recur (rest lines)
                         (-> graph
                             (ensure-node a {})
                             (ensure-node b {})
                             (add-edge (edge a b :label (str "<<" kind ">>") :style :dotted)))
                         nil))
                (recur (rest lines) (or (read-style graph line) graph) nil))))
      (finish graph (flow-in args lines) width))))

;; C4 diagrams

(defn- call-args
  "Arguments of a C4 macro call, without `$name=value` options."
  [^String body]
  (loop [at
         0

         quote?
         false

         start
         0

         out
         []]

    (if (>= at (.length body))
      (->> (conj out (subs body start))
           (map str/trim)
           (remove #(str/starts-with? % "$"))
           (mapv unquote-name))
      (let [ch (.charAt body at)]
        (cond (= ch \") (recur (inc at) (not quote?) start out)
              (and (= ch \,) (not quote?))
              (recur (inc at) quote? (inc at) (conj out (subs body start at)))
              :else (recur (inc at) quote? start out))))))

(defn c4-diagram
  [{:keys [lines]} width]
  (loop [lines
         (seq (g/statements lines))

         graph
         empty-graph

         title
         nil]

    (if-let [line (first lines)]
      (if-let [[_ macro body] (re-matches #"(\w+)\s*\((.*)\)\s*\{?" line)]
        (let [macro (str/lower-case macro)
              args (call-args body)
              [id label a b] args
              ext? (str/ends-with? macro "_ext")
              base (str/replace macro #"_ext$" "")
              element (fn [kind tech desc shape tone]
                        (-> graph
                            (ensure-node id {})
                            (update-node id
                                         assoc
                                         :label (str "<<" kind ">>\n" (or label id))
                                         :shape shape
                                         :tone (if ext? :chrome tone)
                                         :text-tone (if ext? nil tone)
                                         :sections (filterv seq
                                                     [(cond-> []
                                                        (seq tech)
                                                        (conj [(str "[" tech "]") :chrome])

                                                        (seq desc)
                                                        (conj desc))]))))]

          (recur (rest lines)
                 (cond (= "person" base) (element "person" nil a :stadium :blue)
                       (#{"system" "systemdb" "systemqueue"} base)
                       (element "system" nil a (if (= "systemdb" base) :cylinder :rect) :cyan)
                       (#{"container" "containerdb" "containerqueue"} base)
                       (element "container" a b (if (= "containerdb" base) :cylinder :rect) :green)
                       (#{"component" "componentdb" "componentqueue"} base)
                       (element "component" a b (if (= "componentdb" base) :cylinder :rect) :purple)
                       (re-find #"^(?:bi)?rel(?:_\w+)?$" macro)
                       (let [back? (str/includes? macro "back")
                             [from to text tech] (if back? [label id a b] [id label a b])]

                         (-> graph
                             (ensure-node from {})
                             (ensure-node to {})
                             (add-edge (edge from
                                             to
                                             :tail? (str/starts-with? macro "bi")
                                             :label (str/join " "
                                                              (remove str/blank?
                                                                [text
                                                                 (some->> tech
                                                                          (format "[%s]"))]))))))
                       (= "relindex" macro) (let [[_ from to text] args]
                                              (-> graph
                                                  (ensure-node from {})
                                                  (ensure-node to {})
                                                  (add-edge
                                                    (edge from to :label (str id ". " text)))))
                       :else graph)
                 title))
        (recur (rest lines) graph (or title (second (re-matches #"title\s+(.*)" line)))))
      (assoc (finish graph :down width) :title title))))

;; Architecture diagrams

(def ^:private arch-edge-re
  #"^([\w\-]+)(?:\{group\})?\s*:\s*([LRTB])\s*(<)?-(?:-)?(>)?\s*([LRTB])\s*:\s*([\w\-]+)(?:\{group\})?$")

(defn architecture-diagram
  [{:keys [lines]} width]
  (loop [lines
         (seq (g/statements lines))

         graph
         empty-graph

         sides
         []]

    (if-let [line (first lines)]
      (if-let [[_ kind id icon label]
               (re-find #"^(service|junction)\s+([\w\-]+)(?:\(([^)]*)\))?(?:\[([^\]]*)\])?" line)]
        (recur (rest lines)
               (-> graph
                   (ensure-node id {})
                   (update-node id
                                assoc
                                :label (if (= "junction" kind) "•" (c/clean-label (or label id)))
                                :shape (cond (= "junction" kind) :circle
                                             (#{"database" "disk"} icon) :cylinder
                                             (#{"internet" "cloud"} icon) :round
                                             :else :rect)
                                :text-tone (when (= "service" kind) :cyan)))
               sides)
        (if-let [[_ a side-a left right _side-b b] (re-matches arch-edge-re line)]
          (let [swap? (#{"L" "T"} side-a)
                [from to] (if swap? [b a] [a b])
                [tail? head?]
                (if swap? [(boolean right) (boolean left)] [(boolean left) (boolean right)])]

            (recur (rest lines)
                   (-> graph
                       (ensure-node a {})
                       (ensure-node b {})
                       (add-edge (edge from to :head? head? :tail? tail?)))
                   (conj sides side-a)))
          (recur (rest lines) graph sides)))
      (let [horizontal (count (filter #{"L" "R"} sides))]
        (finish graph (if (> horizontal (- (count sides) horizontal)) :right :down) width)))))

;; Block diagrams

(defn- block-tokens
  "Whitespace-separated tokens of a block statement, keeping quoted and
   bracketed parts together."
  [^String line]
  (loop [at
         0

         depth
         0

         quote?
         false

         start
         0

         out
         []]

    (if (>= at (.length line))
      (filterv seq (conj out (subs line start)))
      (let [ch (.charAt line at)]
        (cond (= ch \") (recur (inc at) depth (not quote?) start out)
              quote? (recur (inc at) depth quote? start out)
              (#{\[ \( \{ \<} ch) (recur (inc at) (inc depth) quote? start out)
              (#{\] \) \} \>} ch) (recur (inc at) (max 0 (dec depth)) quote? start out)
              (and (Character/isWhitespace ch) (zero? depth))
              (recur (inc at) depth quote? (inc at) (conj out (subs line start at)))
              :else (recur (inc at) depth quote? start out))))))

(def ^:private arrow-glyph {"right" "──▶" "left" "←──" "up" "↑" "down" "↓" "x" "←─▶" "y" "↑↓"})

(defn- block-item
  [token]
  (cond (re-matches #"space(?::(\d+))?" token)
        {:kind :space :span (parse-long (or (second (re-matches #"space(?::(\d+))?" token)) "1"))}
        (re-find #"<\[" token) (let [[_ id label dir]
                                     (re-find #"^([^<]+)<\[\"?([^\]\"]*)\"?\]>\((\w+)\)" token)]
                                 {:kind :arrow
                                  :id id
                                  :label (str/trim (or label ""))
                                  :glyph (get arrow-glyph dir "──▶")
                                  :span 1})
        :else (let [[token class-name]
                    (strip-class-suffix token)

                    [_ id span]
                    (re-find #"^([^\[\(\{:>]+)(?::(\d+))?" token)

                    shaped
                    (when id (second (g/parse-flowchart [token] :down)))

                    node
                    (some-> shaped
                            :nodes
                            (get id))]

                {:kind :node
                 :id id
                 :label (or (:label node) id)
                 :shape (or (:shape node) :rect)
                 :class class-name
                 :span (parse-long (or span "1"))})))

(defn- parse-blocks
  "[tree edges graph-styles] of block statements."
  [statements]
  (loop [statements
         (seq statements)

         stack
         [{:kind :group :columns nil :children []}]

         edges
         []

         styles
         empty-graph]

    (if-let [line (first statements)]
      (cond
        (re-matches #"(?i)columns\s+(\d+|auto)" line)
        (let [n (parse-long (second (re-matches #"(?i)columns\s+(\d+|auto)" line)))]
          (recur (rest statements) (assoc-in stack [(dec (count stack)) :columns] n) edges styles))
        (re-find #"^block(?::|\s|$)" line)
        (let [[_ id span] (re-find #"^block(?::([\w\-]+))?(?::(\d+))?" line)]
          (recur (rest statements)
                 (conj
                   stack
                   {:kind :group :id id :columns nil :span (parse-long (or span "1")) :children []})
                 edges
                 styles))
        (re-matches #"(?i)end" line)
        (if (> (count stack) 1)
          (let [done (peek stack)
                stack (pop stack)]

            (recur (rest statements)
                   (update-in stack [(dec (count stack)) :children] conj done)
                   edges
                   styles))
          (recur (rest statements) stack edges styles))
        (re-find #"(?i)^(?:style|classdef|class)\s" line)
        (recur (rest statements) stack edges (or (read-style styles line) styles))
        (re-find #"--|==|-\.|~~~" line)
        (let [{:keys [graph]} (g/parse-flowchart [line] :down)]
          (recur (rest statements) stack (into edges (:edges graph)) styles))
        :else (recur (rest statements)
                     (update-in stack
                                [(dec (count stack)) :children]
                                into
                                (map block-item (block-tokens line)))
                     edges
                     styles))
      [(first stack) edges styles])))

(defn- block-cell
  "Rows of one block item, exactly `w` columns wide. `rows-of` lays out a nested group."
  [item w tones rows-of]
  (let [{:keys [kind label shape id]}
        item

        inner
        (max 1 (- (long w) 4))]

    (case kind
      :space
      [[[(apply str (repeat w \space)) :none]]]

      :arrow
      (let [rows (cond-> []
                   (seq label)
                   (conj (c/clip label w))

                   true
                   (conj (:glyph item)))]
        (mapv (fn [text]
                (let [pad (quot (- (long w) (c/width text)) 2)]
                  [[(apply str (repeat pad \space)) :none] [text :chrome]
                   [(apply str (repeat (- (long w) pad (c/width text)) \space)) :none]]))
              rows))

      :group
      (let [inner-w
            (- (long w) 2)

            tone
            (or (get-in tones [id :border]) :chrome)

            body
            (rows-of item inner-w tones)]

        (vec (concat [[["┌" tone] [(apply str (repeat inner-w \─)) tone] ["┐" tone]]]
                     (map (fn [row]
                            (into [["│" tone]] (conj row ["│" tone])))
                          body)
                     [[["└" tone] [(apply str (repeat inner-w \─)) tone] ["┘" tone]]])))

      (let [{:keys [border text]}
            (get tones id)

            tone
            (or border :chrome)

            [tl tr bl br]
            (case shape
              (:round :stadium :circle :cylinder :flag)
              ["╭" "╮" "╰" "╯"]

              (:diamond :hexagon)
              ["/" "\\" "\\" "/"]

              ["┌" "┐" "└" "┘"])

            lines
            (c/label-lines label inner)

            bar
            (apply str (repeat (- (long w) 2) \─))]

        (vec (concat [[[tl tone] [bar tone] [tr tone]]]
                     (for [line
                           lines

                           :let [pad
                                 (quot (- (- (long w) 2) (c/width line)) 2)]]

                       [["│" tone] [(apply str (repeat pad \space)) :none] [line text]
                        [(apply str (repeat (- (long w) 2 pad (c/width line)) \space)) :none]
                        ["│" tone]])
                     [[[bl tone] [bar tone] [br tone]]]))))))

(defn- block-rows
  "Rows of a block group laid out on its column grid inside `w` columns."
  [group w tones]
  (let [children
        (:children group)

        columns
        (long (or (:columns group) (max 1 (reduce + 0 (map #(long (:span % 1)) children)))))

        columns
        (max 1 columns)

        col-w
        (max 3 (quot (- (long w) (dec columns)) columns))

        cell-w
        (fn [span]
          (+ (* (long span) col-w) (dec (long span))))

        lines
        (loop [children
               children

               line
               []

               used
               0

               out
               []]

          (if-let [child (first children)]
            (let [span (min columns (long (:span child 1)))]
              (if (and (seq line) (> (+ used span) columns))
                (recur children [] 0 (conj out line))
                (recur (rest children) (conj line (assoc child :span span)) (+ used span) out)))
            (cond-> out
              (seq line)
              (conj line))))]

    (vec
      (mapcat
        (fn [line]
          (let [cells
                (mapv #(block-cell % (cell-w (:span %)) tones block-rows) line)

                height
                (apply max 1 (map count cells))

                widths
                (mapv #(cell-w (:span %)) line)]

            (for [r (range height)]
              (let [row (vec (butlast
                               (mapcat (fn [cell cw]
                                         (conj (get cell r [[(apply str (repeat cw \space)) :none]])
                                               [" " :none]))
                                       cells
                                       widths)))
                    fill (- (long w) (c/seg-width row))]

                (cond-> row
                  (pos? fill)
                  (conj [(apply str (repeat fill \space)) :none]))))))
        lines))))

(defn block-diagram
  [{:keys [lines]} width]
  (let [[tree edges styles]
        (parse-blocks (g/statements lines))

        labels
        (into {}
              (for [item
                    (tree-seq :children :children tree)

                    :when (:id item)]

                [(:id item) (:label item (:id item))]))

        tones
        (into {}
              (for [[id node] (:nodes (g/apply-styles
                                        (reduce (fn [graph [id label]]
                                                  (ensure-node graph id {:label label}))
                                                styles
                                                labels)))]
                [id {:border (:tone node) :text (:text-tone node)}]))

        tones
        (merge-with merge
                    tones
                    (into {}
                          (for [item
                                (tree-seq :children :children tree)

                                :when (and (:id item) (:class item))]

                            [(:id item)
                             (c/style-tones (get-in styles [:class-styles (:class item)]))])))

        grid
        (block-rows tree width tones)

        links
        (for [{:keys [from to label]} edges]
          (c/seg-clip [[(get labels from from) :text] [" ──▶ " :chrome] [(get labels to to) :text]
                       [(if (seq label) (str "  " label) "") :none]]
                      width))]

    {:rows (mapv c/seg-row (concat grid (when (seq links) [[]]) links))}))
