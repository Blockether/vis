(ns com.blockether.vis.internal.human-input.live
  "What a live view IS after patch N — and what the MODEL reads when it ends.

   A form is answered once, so nobody has to own its state; a live view is a
   stream of operations against nodes addressed by id, so somebody does.
   [[apply-patch]] is that owner: pure, total, and the ONE place the surfaces
   read from. The terminal pane, the companion screen and the model all paint
   the same materialized map, so none of them can disagree about a row.

   [[->markdown]] is the MODEL's surface. The human watches the stream; the
   model gets the finished picture ONCE, as markdown, rendered from the same
   state the other two painted. Markdown is a RENDERING, never a fourth
   vocabulary: every node type has exactly one markdown form here, so a view
   that reads well in the terminal already reads well in a prompt. Colour is
   the only thing that cannot cross — where a surface paints red, the model
   reads a `[tone]` token.

   Nothing here evicts the RECORD. Bounds on keyed collections are REFUSALS
   (`spec/item-bounds`) naming the bound and the node, a log's `:window-lines`
   is only how much a surface holds hot, and the model's own budget always says
   how many lines it left behind."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.human-input.spec :as spec]))

;; The materializer

(defn- invalid-patch!
  "Refuse a patch the way the parser refuses a request: one line naming what to
   fix, thrown where the extension wrote it."
  [node-id message]
  (throw (ex-info (str "Invalid live-view patch" (when node-id (str " for node " node-id))
                       ": " message)
                  {:type :vis/human-input-invalid-patch :node-id node-id :reason message})))

(def ^:private settable-keys
  "Which keys a `set` may carry, per node type, over the `:label` every node
   answers. A `progress` has no text and a `status` has no value: crossing them
   is refused with both named, because the alternative is a patch that lands
   nowhere while its author believes it painted."
  {:status #{:text :detail :tone}
   :progress #{:value :done :total}
   :stat #{:stats}
   :steps #{:steps}
   :log #{:window-lines}
   :table #{:order :max-rows}
   :link #{:links}})

(def ^:private appendable-key
  "The key an `append` carries, per node type — the one axis a `log` and a
   `table` differ on, and the reason appending rows to a log is a refusal
   rather than a silent no-op."
  {:stat :stats :steps :steps :log :lines :table :rows :link :links})

(def ^:private patch-item-caps
  "How many items ONE patch may carry into a node. Not a cap on the node: it
   keeps a single operation from parking the publishing thread on the journal
   writer, and its refusal says to split."
  {:log {:key :lines :max (:max-patch-lines spec/log-defaults)}
   :table {:key :rows :max (:max-patch-rows spec/table-defaults)}})

(defn- node-ids
  "Every node id in the view, in paint order — what a refusal lists when an op
   named an address that is not there."
  [view]
  (mapv :id (:nodes view)))

(defn- node-position
  "Where `node-id` sits in the view, or nil. Ids are addresses, so this is the
   only lookup an op needs."
  [view node-id]
  (first (keep-indexed (fn [i node]
                         (when (= node-id (:id node)) i))
                       (:nodes view))))

(defn- item-bound
  "How many items this node may HOLD: a table declares its own `:max-rows`
   inside the type's ceiling, every other keyed type takes the type's."
  [node]
  (let [{:keys [max]} (spec/item-bounds (:type node))]
    (long (or (:max-rows node) max 0))))

(defn- checked-count!
  "`items` when the node may hold them all, else a refusal naming the bound, the
   node and the home for unbounded volume."
  [node items]
  (let [bound (long (item-bound node))]
    (when (> (count items) bound)
      (invalid-patch! (:id node)
                      (str "a " (name (:type node))
                           " node holds at most " bound
                           " items and this would hold " (count items)
                           ". Nothing is trimmed: an unbounded stream belongs in a `log` node, "
                           "whose record keeps every line.")))
    items))

(defn- upsert
  "`incoming` merged into `existing` BY ID: an id already present is REPLACED IN
   PLACE — the row keeps the slot the eye left it in — and an unseen id is
   appended to the order. One index pass, so a full table costs the same
   whether one item changed or two hundred did."
  [existing incoming]
  (let
    [start (into {}
                 (map-indexed (fn [i item]
                                [(:id item) i]))
                 existing)]
    (first (reduce (fn [[acc idx] item]
                     (if-let [pos (get idx (:id item))]
                       [(assoc acc pos item) idx]
                       [(conj acc item) (assoc idx (:id item) (count acc))]))
                   [existing start]
                   incoming))))

(defn- without-ids
  "`existing` without the named items. Removing an id that is not there is a
   NO-OP: a patch states the wanted state, and an extension polling a fleet
   must not have to remember what it already said."
  [existing item-ids]
  (let [dropped? (set item-ids)]
    (into [] (remove (comp dropped? :id)) existing)))

(defn- stamped-log
  "A log node carrying `:total-lines` — how many lines its RECORD has, window
   included. Stamped by the engine so `… N earlier lines` is counted, never
   guessed, on every surface."
  [node]
  (cond-> node
    (= :log (:type node))
    (update :total-lines #(long (or % (count (:lines node)))))))

(defn materialize
  "The declared view as the materializer holds it. Only stamps: every log node
   learns the size of its own record, so the first patch already knows what the
   window is a window ONTO."
  [view]
  (update view :nodes #(mapv stamped-log %)))

(defn- checked-node
  "`node` once it still satisfies the declared contract, else the reason it does
   not — an engine bug surfaces here instead of as a torn pane three
   namespaces away."
  [node]
  (if-let [reason (spec/live-node-error node)]
    (invalid-patch! (:id node) reason)
    node))

(defn- apply-set
  "A node with its own state replaced. Keys foreign to the node's type are
   refused BY NAME rather than merged into a shape no surface can paint."
  [node op]
  (let
    [allowed
     (conj (get settable-keys (:type node) #{}) :label)

     given
     (disj (set (keys op)) :op :node-id)

     foreign
     (sort (map name (remove allowed given)))]

    (when (seq foreign)
      (invalid-patch! (:id node)
                      (str "a " (name (:type node))
                           " node has no " (str/join ", " foreign)
                           " to set; it sets " (str/join ", " (sort (map name allowed))))))
    (let [merged (merge node (select-keys op (vec given)))]
      (checked-node (if (contains? given :stats)
                      (assoc merged :stats (checked-count! merged (:stats op)))
                      merged)))))

(defn- apply-append
  "A node with items added. A `log` grows its window and its record; a keyed
   node upserts by id and is bounded by refusal."
  [node op]
  (let
    [k
     (get appendable-key (:type node))

     given
     (disj (set (keys op)) :op :node-id)

     wrong
     (sort (map name (disj given k)))]

    (when (nil? k)
      (invalid-patch! (:id node) (str "a " (name (:type node)) " node has nothing to append to")))
    (when (seq wrong)
      (invalid-patch!
        (:id node)
        (str "a " (name (:type node)) " node appends " (name k) ", not " (str/join ", " wrong))))
    (let [items (get op k)]
      (when-let [{:keys [max]} (get patch-item-caps (:type node))]
        (when (> (count items) (long max))
          (invalid-patch! (:id node)
                          (str "one patch carries at most "
                               max
                               " "
                               (name k)
                               " and this one carries "
                               (count items)
                               "; split it"))))
      (if (= :log (:type node))
        (let
          [window (long (:window-lines node))
           all (into (:lines node) items)
           overflow (max 0 (- (count all) window))]

          (assoc node
            :lines (if (pos? overflow) (subvec all overflow) all)
            :total-lines (+ (long (:total-lines node)) (count items))))
        (update node k #(checked-count! node (upsert % items)))))))

(defn- apply-remove
  "A node without the named items. A node with no keyed collection has nothing
   to remove and says so."
  [node op]
  (let [{:keys [key]} (spec/item-bounds (:type node))]
    (when (nil? key)
      (invalid-patch! (:id node) (str "a " (name (:type node)) " node holds no removable items")))
    (update node key without-ids (:item-ids op))))

(defn- apply-clear
  "A node emptied. Clearing a log empties the WINDOW, never the record: the
   count of what came before is what the model is later told about."
  [node]
  (case (:type node)
    :log
    (assoc node :lines [])

    (let [{:keys [key]} (spec/item-bounds (:type node))]
      (when (nil? key)
        (invalid-patch! (:id node) (str "a " (name (:type node)) " node holds nothing to clear")))
      (assoc node key []))))

(defn- apply-add-node
  "The view with one more node — the shape changing while it runs, because a
   scan that discovers a seventh device should not have to have declared it."
  [view {:keys [node-spec after]}]
  (let
    [node
     (stamped-log (checked-node node-spec))

     max-nodes
     (long (:max-nodes spec/view-defaults))

     nodes
     (:nodes view)]

    (when (node-position view (:id node))
      (invalid-patch! (:id node) "a node with this id is already in the view; an id is an address"))
    (when (>= (count nodes) max-nodes)
      (invalid-patch! (:id node)
                      (str "a view holds at most "
                           max-nodes
                           " nodes; 200 devices are 200 ROWS in one table, not 200 panes")))
    (let
      [pos (when after
             (or (node-position view after)
                 (invalid-patch!
                   (:id node)
                   (str "cannot place it after " after ": the view has no such node"))))]
      (assoc view
        :nodes (if pos
                 (into (conj (subvec nodes 0 (inc (long pos))) node)
                       (subvec nodes (inc (long pos))))
                 (conj nodes node))))))

(defn- apply-remove-node
  "The view without that node, its items with it. Dropping a node that is not
   there is a NO-OP — teardown is idempotent, while a WRITE to a node that is
   gone is a lost patch and refuses."
  [view {:keys [node-id]}]
  (if-let [pos (node-position view node-id)]
    (let [nodes (:nodes view)]
      (assoc view :nodes (into (subvec nodes 0 (long pos)) (subvec nodes (inc (long pos))))))
    view))

(defn- apply-op
  "The view after ONE operation."
  [view op]
  (case (:op op)
    :add-node
    (apply-add-node view op)

    :remove-node
    (apply-remove-node view op)

    (let
      [pos
       (or (node-position view (:node-id op))
           (invalid-patch! (:node-id op)
                           (str "the view has no such node; it has "
                                (str/join ", " (node-ids view)))))

       node
       (get-in view [:nodes (long pos)])

       patched
       (case (:op op)
         :set
         (apply-set node op)

         :append
         (apply-append node op)

         :remove
         (apply-remove node op)

         :clear
         (apply-clear node))]

      (assoc-in view [:nodes (long pos)] patched))))

(defn apply-patch
  "`view` after every operation in `patch`, or a refusal naming the first one
   that could not land.

   ALL OR NOTHING: a patch that refuses half way returns nothing and leaves the
   view it was handed, so no surface ever paints half a patch. `:seq` must
   ADVANCE — a replayed or duplicated patch is refused rather than applied
   twice, which is what lets a surface treat a gap as `re-read the snapshot`."
  [view patch]
  (when-let [reason (spec/live-patch-error patch)]
    (invalid-patch! nil reason))
  (when-not (= (:view-id patch) (:id view))
    (invalid-patch! nil (str "this patch names view " (:view-id patch) ", not " (:id view))))
  (when-not (> (long (:seq patch)) (long (:seq view)))
    (invalid-patch! nil
                    (str "patch seq " (:seq patch) " does not advance the view's " (:seq view))))
  (assoc (reduce apply-op view (:ops patch)) :seq (:seq patch)))

;; The model's surface

(def ^:private model-budget
  "What the MODEL is handed of an unbounded node. A log renders its TAIL and a
   table its head, both saying how much they left behind and that the record
   still holds it — the human scrolls the whole thing, the model reads the end
   of the story."
  {:log-tail-lines 120 :table-rows 50})

(defn- tone-tag
  "How the model reads a colour it cannot see."
  [tone]
  (when tone (str "[" (name tone) "] ")))

(defn- percent "A fraction as whole percent." [value] (long (Math/round (* 100.0 (double value)))))

(defn- cell-text
  "One table cell, made safe for a pipe table: a newline would end the row and a
   pipe would invent a column."
  [text]
  (-> (str text)
      (str/replace #"\s*\n\s*" " ")
      (str/replace "|" "\\|")))

(defn- cell-at
  "The cell this row carries for a declared column id."
  [columns row column-id]
  (let
    [pos (first (keep-indexed (fn [i c]
                                (when (= column-id (:id c)) i))
                              columns))]
    (if pos (get (:cells row) (long pos) "") "")))

(defn- numeric-column?
  "True when every value that is there parses as a number — the same rule the
   companion's table already sorts by (`DataTable.tsx`), so the two surfaces
   cannot order the same rows differently."
  [texts]
  (let [present (remove str/blank? texts)]
    (and (seq present) (every? parse-double present))))

(defn- sorted-rows
  "Rows in a declared `{:by … :dir …}` order: blanks last, ties keeping
   insertion order, so the same script paints identically everywhere."
  [columns rows {:keys [by dir]}]
  (let
    [numeric?
     (numeric-column? (mapv #(cell-at columns % by) rows))

     key-of
     (fn [row]
       (let [text (cell-at columns row by)]
         (when-not (str/blank? text) (if numeric? (parse-double text) (str/lower-case text)))))

     descending?
     (= :desc dir)]

    (vec (sort (fn [a b]
                 (let
                   [ka
                    (key-of a)

                    kb
                    (key-of b)]

                   (cond (and (nil? ka) (nil? kb)) 0
                         (nil? ka) 1
                         (nil? kb) -1
                         :else (let [c (long (compare ka kb))]
                                 (if descending? (- c) c)))))
               rows))))

(defn ordered-rows
  "A table's rows in the order it DECLARED — applied at paint time, never by
   re-sorting the record, so a re-sort never loses the identity a scroll anchor
   is pinned to."
  [{:keys [rows columns order]}]
  (cond (= :newest-first order) (vec (reverse rows))
        (map? order) (sorted-rows columns rows order)
        :else rows))

(defn- fenced
  "`lines` in a code fence long enough to survive whatever backticks they carry."
  [lines]
  (let
    [longest
     (reduce max
             2
             (map (fn [line]
                    (reduce max 0 (map count (re-seq #"`+" (str line)))))
                  lines))

     fence
     (apply str (repeat (inc (long longest)) "`"))]

    (concat [fence] (map str lines) [fence])))

(defmulti ^:private node->markdown
  "One markdown form per node type — the whole vocabulary, once."
  (fn [node _budget]
    (:type node)))

(defmethod node->markdown :status
  [{:keys [text detail tone]} _]
  (cond-> [(str (tone-tag tone) "**" text "**")]
    detail
    (conj (str "_" detail "_"))))

(defmethod node->markdown :progress
  [{:keys [value done total]} _]
  (let
    [head
     (if value (str "**" (percent value) "%**") "_working_")

     counted
     (when done (str done (when total (str "/" total)) " done"))]

    [(str/join " · " (remove nil? [head counted]))]))

(defmethod node->markdown :stat
  [{:keys [stats]} _]
  (if (seq stats)
    [(str/join
       " · "
       (map (fn [{:keys [label value-text tone]}]
              (str "**" label "** " value-text (when tone (str " " (str/trim (tone-tag tone))))))
            stats))]
    ["_nothing counted yet_"]))

(defmethod node->markdown :steps
  [{:keys [steps]} _]
  (if (seq steps)
    (mapv (fn [{:keys [label tone detail value]}]
            (str "- "
                 (tone-tag tone)
                 label
                 (when detail (str " — " detail))
                 (when value (str " · " (percent value) "%"))))
          steps)
    ["_no steps yet_"]))

(defmethod node->markdown :log
  [{:keys [lines total-lines]} {:keys [log-tail-lines]}]
  (if (seq lines)
    (let
      [tail
       (long log-tail-lines)

       shown
       (if (> (count lines) tail) (subvec lines (- (count lines) tail)) lines)

       behind
       (- (long (or total-lines (count lines))) (count shown))]

      (cond-> (vec (fenced shown))
        (pos? behind)
        (conj (str "_… " behind " earlier lines — the view's record keeps them all_"))))
    ["_no output yet_"]))

(defmethod node->markdown :table
  [{:keys [columns] :as node} {:keys [table-rows]}]
  (let [rows (ordered-rows node)]
    (if (seq rows)
      (let
        [limit (long table-rows)
         shown (if (> (count rows) limit) (subvec rows 0 limit) rows)
         toned? (boolean (some :tone rows))
         header (cond->> (mapv :label columns)
                  toned?
                  (into ["!"]))
         rule (cond->> (mapv #(if (= :right (:align %)) "---:" "---") columns)
                toned?
                (into ["---"]))
         row->cells (fn [row]
                      (cond->> (mapv #(cell-text (cell-at columns row (:id %))) columns)
                        toned?
                        (into [(if (:tone row) (name (:tone row)) "")])))
         line (fn [cells]
                (str "| " (str/join " | " cells) " |"))]

        (cond-> (into [(line header) (line rule)] (map (comp line row->cells)) shown)
          (> (count rows) limit)
          (conj
            (str "_… " (- (count rows) limit) " more rows — the view's record keeps them all_"))))
      ["_no rows yet_"])))

(defmethod node->markdown :link
  [{:keys [links]} _]
  (if (seq links)
    (mapv (fn [{:keys [label target-kind target tone]}]
            (str "- "
                 (tone-tag tone)
                 (case target-kind
                   :url
                   (str "[" label "](" target ")")

                   :path
                   (str label " — `" target "`")

                   :attachment
                   (str label " — attachment `" target "`"))))
          links)
    ["_no links_"]))

(defn- verdict-line
  "The first thing the model reads: how the view ended, before anything it
   painted."
  [{:keys [is-completed reason summary error]}]
  (str "**"
       (name reason)
       "**"
       (when-not is-completed " — this view did not finish")
       (when summary (str " · " summary))
       (when error (str " · " error))))

(defn ->markdown
  "The whole view as markdown — what the MODEL gets, once, when the view ends.

   Rendered from the materialized state the human's surfaces painted, so the
   prompt and the pane cannot tell different stories. `opts` may carry the
   `:result` (rendered first, because the verdict is what a reader needs
   before the detail) and may widen the model's budget with `:log-tail-lines`
   and `:table-rows`. Both budgets truncate the RENDER and say so with the
   count they left behind; neither touches the record."
  ([view] (->markdown view nil))
  ([view {:keys [result] :as opts}]
   (let
     [budget
      (merge model-budget (select-keys opts [:log-tail-lines :table-rows]))

      head
      (cond-> [(str "# " (:title view))]
        (:description view)
        (into ["" (str "_" (:description view) "_")])

        result
        (into ["" (verdict-line result)]))

      body
      (mapcat (fn [node]
                (into (if-let [label (:label node)]
                        ["" (str "### " label)]
                        [""])
                      (node->markdown node budget)))
              (:nodes view))]

     (str/join "\n" (into head body)))))
