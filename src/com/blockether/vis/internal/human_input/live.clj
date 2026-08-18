(ns com.blockether.vis.internal.human-input.live
  "What a live view IS after patch N — and what the MODEL reads when it ends.

   A form is answered once, so nobody has to own its state; a live view is a
   stream of operations against nodes addressed by id, so somebody does.
   [[apply-patch]] is that owner: pure, total, and the ONE place the surfaces
   read from. The terminal pane, the companion screen and the model all paint
   the same materialized map, so none of them can disagree about a row.

   [[picture]] is the MODEL's surface: the finished view as DATA. Ids and tones
   come with it, so a node the model read is a node it can patch, and a state is
   never recovered from a sentence — the model acts on the same materialized map
   the surfaces paint, budgeted, never on a description of it.

   [[->markdown]] is the DOCUMENT that state renders into: what a human reopens,
   what an artifact stores, what a transcript embeds. Markdown is a RENDERING,
   never a fourth vocabulary: every node type has exactly one markdown form here,
   so a view that reads well in the terminal already reads well on a page. Colour
   is the only thing that cannot cross — where a surface paints red, the page
   reads a `[tone]` token.

   [[parse-markdown]] is that document read BACK. One markdown form per node type
   in one direction is one form per node type in the other, so a whole view can be
   AUTHORED as markdown and a rendered one re-read. What a budget left behind is
   named, never guessed.

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

(defn- node-tree
  "Every node of the view depth first, the children of a layout group included.
   Ids are unique across the whole tree, so this is what an ADDRESS is looked up
   in."
  [nodes]
  (into []
        (mapcat (fn [node]
                  (cons node (node-tree (:fields node)))))
        nodes))

(defn- leaf-nodes
  "Every node that PAINTS something, depth first. A layout group holds no state of
   its own — it says where the nodes inside it stand — so the model and the
   document read the same content whether a status was stood beside a table or
   under it."
  [nodes]
  (into []
        (mapcat (fn [node]
                  (if (seq (:fields node)) (leaf-nodes (:fields node)) [node])))
        nodes))

(defn- node-ids
  "Every node id in the view, in paint order — what a refusal lists when an op
   named an address that is not there. Groups are in it: `add-node :after` and
   `remove-node` speak to a row by id too."
  [view]
  (mapv :id (node-tree (:nodes view))))

(defn- path-in
  [nodes node-id]
  (first (keep-indexed (fn [i node]
                         (cond (= node-id (:id node)) [i]
                               (seq (:fields node)) (when-let [sub (path-in (:fields node) node-id)]
                                                      (into [i :fields] sub))))
                       nodes)))

(defn- node-path
  "The path from the view down to `node-id` — `[:nodes 2 :fields 0]` for a node
   arranged inside a layout group — or nil. Ids are addresses and unique across
   the tree, so a node standing in a row is patched exactly like one at the top."
  [view node-id]
  (when-let [found (path-in (:nodes view) node-id)]
    (into [:nodes] found)))

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
  (let [start (into {}
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

(defn- stamped
  "`node` stamped through: a layout group hands the stamp to the nodes inside it,
   because a log arranged into a row is still a log."
  [node]
  (cond-> (stamped-log node)
    (seq (:fields node))
    (update :fields #(mapv stamped %))))

(defn materialize
  "The declared view as the materializer holds it. Only stamps: every log node
   learns the size of its own record, so the first patch already knows what the
   window is a window ONTO."
  [view]
  (update view :nodes #(mapv stamped %)))

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
  (let [allowed
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
  (let [k
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
        (let [window (long (:window-lines node))
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

(defn- insert-after
  "`nodes` with `node` in the slot right after `idx` — where the eye expects it,
   next to the node the op named."
  [nodes idx node]
  (into (conj (subvec nodes 0 (inc (long idx))) node) (subvec nodes (inc (long idx)))))

(defn- drop-at
  "`nodes` without the one at `idx`."
  [nodes idx]
  (into (subvec nodes 0 (long idx)) (subvec nodes (inc (long idx)))))

(defn- apply-add-node
  "The view with one more node — the shape changing while it runs, because a
   scan that discovers a seventh device should not have to have declared it.
   `:after` names the node the newcomer stands next to WHEREVER that node is, so
   a node added after one inside a layout group joins that group; a group added
   this way lands with everything inside it."
  [view {:keys [node-spec after]}]
  (let [node
        (stamped (checked-node node-spec))

        max-nodes
        (long (:max-nodes spec/view-defaults))

        taken
        (set (node-ids view))

        minted
        (mapv :id (node-tree [node]))]

    (doseq [id minted]
      (when (taken id)
        (invalid-patch! id "a node with this id is already in the view; an id is an address")))
    (when (> (+ (count (node-ids view)) (count minted)) max-nodes)
      (invalid-patch! (:id node)
                      (str "a view holds at most "
                           max-nodes
                           " nodes; 200 devices are 200 ROWS in one table, not 200 panes")))
    (if-let [path (when after
                    (or (node-path view after)
                        (invalid-patch!
                          (:id node)
                          (str "cannot place it after " after ": the view has no such node"))))]
      (update-in view (pop path) insert-after (peek path) node)
      (update view :nodes conj node))))

(defn- apply-remove-node
  "The view without that node, its items with it — and a layout group without the
   nodes it arranged, because dropping the row drops what stood in it. Dropping a
   node that is not there is a NO-OP — teardown is idempotent, while a WRITE to a
   node that is gone is a lost patch and refuses."
  [view {:keys [node-id]}]
  (if-let [path (node-path view node-id)]
    (update-in view (pop path) drop-at (peek path))
    view))

(defn- apply-op
  "The view after ONE operation."
  [view op]
  (case (:op op)
    :add-node
    (apply-add-node view op)

    :remove-node
    (apply-remove-node view op)

    (let [path
          (or (node-path view (:node-id op))
              (invalid-patch! (:node-id op)
                              (str "the view has no such node; it has "
                                   (str/join ", " (node-ids view)))))

          node
          (get-in view path)

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

      (assoc-in view path patched))))

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
  "What the MODEL is handed of an unbounded node — shared by [[picture]] and
   [[->markdown]], so the data and the document leave the same thing behind. A log
   answers its TAIL and a table its head, both counting what they left and saying
   the record still holds it: the human scrolls the whole thing, the model reads
   the end of the story."
  {:log-tail-lines 120 :table-rows 50})

(def ^:private empty-line
  "The one line each node type paints when it holds nothing. Distinct per type on
   purpose: it is what tells [[parse-markdown]] which node painted an empty
   block, so a view survives the trip through the model's surface with its shape
   intact. A table paints it UNDER its header — the columns are the declaration,
   and a table with no rows still says what it is watching."
  {:stat "_nothing counted yet_"
   :steps "_no steps yet_"
   :log "_no output yet_"
   :table "_no rows yet_"
   :link "_no links_"})

(def ^:private indeterminate-line
  "A progress node with no fraction: started, size unknown. Not an empty state —
   a bar nobody has measured still counts what it finished."
  "_working_")
(defn- invalid-view!
  "Refuse a view the engine may not paint: one line naming what to fix, thrown
   where the view was declared."
  [message]
  (throw (ex-info (str "Invalid live view: " message)
                  {:type :vis/human-input-invalid-live-view :reason message})))

(def ^:private tone?
  "Every colour a surface may paint, as a reader recognizes it. CLOSED in BOTH
   directions: text that merely looks like a `[tone]` marker stays text, and a
   tone no reader knows is never painted."
  (set (vals spec/live-tones)))

(defn- tone-tag
  "How the model reads a colour it cannot see. A tone outside the closed
   vocabulary is REFUSED rather than painted, because the marker would render as
   prose [[parse-markdown]] hands back as text — the round trip would lose the
   very node the colour was on."
  [tone]
  (when tone
    (when-not (tone? tone)
      (invalid-view! (str "unknown tone " (pr-str tone)
                          " — a surface paints one of " (str/join ", " (sort (map name tone?))))))
    (str "[" (name tone) "] ")))

(defn percent
  "A fraction as whole percent — the ONE rounding every surface shows, so the
   document and a pane never disagree by a point."
  [value]
  (long (Math/round (* 100.0 (double value)))))

(defn fraction
  "How far a progress has come, as a fraction of one — its declared `:value`, or
   what `:done` of `:total` works out to. `nil` is INDETERMINATE: started, size
   unknown, which is the honest picture while a job queues.

   ONE definition, because the document, the model's picture and every surface
   that paints a bar have to stand for the same number."
  [{:keys [value done total]}]
  (cond value (double value)
        (and done total (pos? (long total))) (/ (double done) (double total))
        :else nil))
(defn- cell-text
  "One table cell, made safe for a pipe table: a newline would end the row, a
   pipe would invent a column, and padding is the rail's, so it is trimmed off
   the text before the rail puts its own back."
  [text]
  (-> (str text)
      (str/replace #"\s*\n\s*" " ")
      (str/replace "|" "\\|")
      str/trim))

(defn- cell-at
  "The cell this row carries for a declared column id."
  [columns row column-id]
  (let [pos (first (keep-indexed (fn [i c]
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
  (let [numeric?
        (numeric-column? (mapv #(cell-at columns % by) rows))

        key-of
        (fn [row]
          (let [text (cell-at columns row by)]
            (when-not (str/blank? text) (if numeric? (parse-double text) (str/lower-case text)))))

        descending?
        (= :desc dir)]

    (vec (sort (fn [a b]
                 (let [ka
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
  (let [longest
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
  [{:keys [done total] :as node} _]
  (let [head
        (if-let [f (fraction node)]
          (str "**" (percent f) "%**")
          indeterminate-line)

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
    [(empty-line :stat)]))

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
    [(empty-line :steps)]))

(defmethod node->markdown :log
  [{:keys [lines total-lines]} {:keys [log-tail-lines]}]
  (if (seq lines)
    (let [tail
          (long log-tail-lines)

          shown
          (if (> (count lines) tail) (subvec lines (- (count lines) tail)) lines)

          behind
          (- (long (or total-lines (count lines))) (count shown))]

      (cond-> (vec (fenced shown))
        (pos? behind)
        (conj (str "_… " behind " earlier lines — the view's record keeps them all_"))))
    [(empty-line :log)]))

(defmethod node->markdown :table
  [{:keys [columns] :as node} {:keys [table-rows]}]
  (let [rows
        (ordered-rows node)

        limit
        (long table-rows)

        shown
        (if (> (count rows) limit) (subvec rows 0 limit) rows)

        toned?
        (boolean (some :tone rows))

        header
        (cond->> (mapv :label columns)
          toned?
          (into ["!"]))

        rule
        (cond->> (mapv #(if (= :right (:align %)) "---:" "---") columns)
          toned?
          (into ["---"]))

        row->cells
        (fn [row]
          (cond->> (mapv #(cell-text (cell-at columns row (:id %))) columns)
            toned?
            (into [(if (:tone row) (name (:tone row)) "")])))

        line
        (fn [cells]
          (str "| " (str/join " | " cells) " |"))]

    (cond-> (into [(line header) (line rule)] (map (comp line row->cells)) shown)
      (empty? rows)
      (conj (empty-line :table))

      (> (count rows) limit)
      (conj (str "_… " (- (count rows) limit) " more rows — the view's record keeps them all_")))))

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
    [(empty-line :link)]))

(defn picture
  "The finished view as DATA — what the MODEL reads when it ends, and the only
   shape it acts on. Ids come with it, so a node the model read is a node it can
   patch by name; tones stay keywords, so nothing has to be recovered from a
   sentence. The mount bookkeeping does not come: a picture is
   `{:view {:title :description :nodes} :elided […]}`, the same pair
   [[parse-markdown]] answers, so data out and document in describe ONE shape.

   LAYOUT is left behind with the bookkeeping: a group says where the nodes
   inside it STAND, and where a node stands is the surface's business, so the
   picture carries the nodes themselves, flattened in declaration order.

   Budgeted from the same `model-budget` [[->markdown]] renders with, so the two
   surfaces leave the same thing behind: a log answers its TAIL, a table the head
   of the order it declared, and `:elided` COUNTS what neither carried. The record
   keeps all of it. A table's rows come in PAINT order and the picture says
   `:insertion`, because the order is already applied — mounting a picture again
   must not sort it twice.

   `opts` may widen the budget with `:log-tail-lines` and `:table-rows`."
  ([view] (picture view nil))
  ([view opts]
   (let [{:keys [log-tail-lines table-rows]}
         (merge model-budget (select-keys opts [:log-tail-lines :table-rows]))

         budgeted
         (mapv (fn [node]
                 (case (:type node)
                   :log
                   (let [lines
                         (:lines node)

                         tail
                         (long log-tail-lines)

                         shown
                         (if (> (count lines) tail) (subvec lines (- (count lines) tail)) lines)

                         behind
                         (- (long (or (:total-lines node) (count lines))) (count shown))]

                     (with-meta (assoc node :lines shown) {:elided (max 0 behind)}))

                   :table
                   (let [rows
                         (ordered-rows node)

                         limit
                         (long table-rows)

                         shown
                         (if (> (count rows) limit) (subvec rows 0 limit) rows)]

                     (with-meta (assoc node
                                  :rows shown
                                  :order :insertion)
                       {:elided (- (count rows) (count shown))}))

                   node))
               (leaf-nodes (:nodes view)))]

     {:view (cond-> {:title (:title view) :nodes budgeted}
              (:description view)
              (assoc :description (:description view)))
      :elided (into []
                    (keep (fn [node]
                            (let [items (long (or (:elided (meta node)) 0))]
                              (when (pos? items) {:node-id (:id node) :items items}))))
                    budgeted)})))
(defn- verdict-line
  "The first thing the document says: how the view ended, before anything it
   painted. `->markdown` sets it apart in a `>` block, and the error carries a
   marker of its own — a reader (and [[parse-markdown]]) can tell a summary from
   what went wrong."
  [{:keys [is-completed reason summary error]}]
  (str "**"
       (name reason)
       "**"
       (when-not is-completed " — this view did not finish")
       (when summary (str " · " summary))
       (when error (str " · error: " error))))

(defn ->markdown
  "The whole view as markdown — the DOCUMENT the same state renders into, for a
   human to reopen, an artifact to store and a transcript to embed. The model is
   handed [[picture]] instead: data, not prose.

   Rendered from the materialized state the human's surfaces painted, so the page
   and the pane cannot tell different stories. `opts` may carry the `:result`
   (rendered first, because the verdict is what a reader needs before the detail)
   and may widen the same budget with `:log-tail-lines` and `:table-rows`. Both
   budgets truncate the RENDER and say so with the count they left behind; neither
   touches the record. Layout is left behind for the same reason [[picture]]
   leaves it: the page is the CONTENT of a view, so a group's nodes are rendered
   in declaration order and a document read back is a flat view."
  ([view] (->markdown view nil))
  ([view {:keys [result] :as opts}]
   (let [budget
         (merge model-budget (select-keys opts [:log-tail-lines :table-rows]))

         head
         (cond-> [(str "# " (:title view))]
           (:description view)
           (conj (str "_" (:description view) "_"))

           result
           (into ["" (str "> " (verdict-line result))]))

         body
         (mapcat (fn [node]
                   (into (if-let [label (:label node)]
                           ["" (str "### " label)]
                           [""])
                         (node->markdown node budget)))
                 (leaf-nodes (:nodes view)))]

     (str/join "\n" (into head body)))))

;; Reading the picture back
;;
;; A rendering nobody can read back is a one-way door: the model would be handed
;; a picture it could not answer with, and a human could not hand the engine one
;; either. [[parse-markdown]] is [[->markdown]] inverted — the same vocabulary,
;; read the other way — so markdown is a SURFACE of this engine and not its
;; exhaust. Every form below is the mirror of the one that painted it, and the
;; two are written next to each other so neither can drift alone.

(defn- invalid-markdown!
  "Refuse a picture the way the materializer refuses a patch: one line naming
   what to fix, and the line it is on."
  [line-no message]
  (throw (ex-info (str "Invalid live-view markdown" (when line-no (str " at line " line-no))
                       ": " message)
                  {:type :vis/human-input-invalid-markdown :line line-no :reason message})))

(def ^:private reason? "Every ending a verdict may name." (set (vals spec/live-reasons)))

(def ^:private type-painting-nothing
  "[[empty-line]] read backwards: which node painted an empty block."
  (into {}
        (map (fn [[type line]]
               [line type]))
        empty-line))

(defn slug
  "The address a label earns when nobody wrote one: lower case, one dash for every
   run of anything else, nil when nothing legible is left. What [[addressed]] gives
   a picture's items, and what a settled view is filed under."
  [text]
  (let [id (-> (str text)
               str/lower-case
               (str/replace #"[^\p{L}\p{N}]+" "-")
               (str/replace #"^-+|-+$" ""))]
    (when-not (str/blank? id) id)))

(defn- addressed
  "`items` given the `:id` markdown does not paint: the slug of the text the eye
   reads, `prefix-N` where that is blank, and a numbered suffix where two would
   collide. Derived the same way every time, so the same picture yields the same
   addresses and a patch written against a parsed view still lands."
  [prefix text-of items]
  (first (reduce (fn [[acc taken] item]
                   (let [base
                         (or (slug (text-of item)) (str prefix "-" (inc (count acc))))

                         id
                         (loop [n 1]
                           (let [candidate (if (= 1 n) base (str base "-" n))]
                             (if (contains? taken candidate) (recur (inc n)) candidate)))]

                     [(conj acc (assoc item :id id)) (conj taken id)]))
                 [[] #{}]
                 items)))

(defn- untoned
  "`text` without the marker [[tone-tag]] wrote, and the tone it named."
  [text]
  (if-let [[marker named] (re-find #"^\[([a-z-]+)\] " text)]
    (let [tone (keyword named)]
      (if (tone? tone) [(subs text (count marker)) tone] [text nil]))
    [text nil]))

(defn- italicized
  "What a `_…_` line says, or nil when the line is not one."
  [line]
  (when line (second (re-matches #"_(.+)_" line))))

(defn- bullet
  "What one `- ` item says. A list that mixes a bullet with something else is
   refused rather than half-read."
  [at line]
  (or (second (re-matches #"- (.*)" line))
      (invalid-markdown! at "every line of a list is a `- ` item")))

(defn- counted-behind
  "How many items a budget note says the picture left behind, or nil when the
   block carries no note."
  [pattern line]
  (some->> (italicized line)
           (re-matches pattern)
           second
           parse-long))

(defn- blocks
  "The numbered lines below the title, grouped the way [[->markdown]] laid them
   out: one group per blank line, a fence keeping its own blank lines. Each
   group carries the line it starts on, so a refusal can point at it."
  [numbered]
  (let [{:keys [acc at cur]} (reduce
                               (fn [{:keys [acc at cur fence] :as state} [n line]]
                                 (cond (and fence (= line fence)) (assoc state
                                                                    :cur (conj cur line)
                                                                    :fence nil)
                                       fence (assoc state :cur (conj cur line))
                                       (re-matches #"`{3,}" line) (assoc state
                                                                    :cur (conj (or cur []) line)
                                                                    :at (or at n)
                                                                    :fence line)
                                       (str/blank? line) (if (seq cur)
                                                           (assoc state
                                                             :acc (conj acc {:at at :lines cur})
                                                             :at nil
                                                             :cur nil)
                                                           state)
                                       :else (assoc state
                                               :cur (conj (or cur []) line)
                                               :at (or at n))))
                               {:acc [] :at nil :cur nil :fence nil}
                               numbered)]
    (cond-> acc
      (seq cur)
      (conj {:at at :lines cur}))))

(defn- link-item?
  "True when a bullet is one of the three shapes a `link` node paints."
  [text]
  (boolean (or (re-matches #"\[.+\]\(.+\)" text)
               (re-matches #".+ — attachment `.+`" text)
               (re-matches #".+ — `.+`" text))))

(defn- block-type
  "Which node painted this block. Every type paints a shape no other one makes: a
   fence is a log, a rail is a table, a bullet list is steps or links, a bold
   percent is progress, a line that is nothing but bold is a status, bold with a
   value after it is a stat, and each empty state names its own type."
  [{:keys [at lines]}]
  (let [head
        (first lines)

        [text _]
        (untoned head)]

    (cond (contains? type-painting-nothing head) (type-painting-nothing head)
          (re-matches #"`{3,}" head) :log
          (str/starts-with? head "|") :table
          (str/starts-with? head "- ") (if (every? (fn [line]
                                                     (link-item? (first (untoned (bullet at
                                                                                         line)))))
                                                   lines)
                                         :link
                                         :steps)
          (or (str/starts-with? head indeterminate-line) (re-find #"^\*\*\d+%\*\*" head)) :progress
          (re-matches #"\*\*[^*]+\*\*" text) :status
          (re-find #"^\*\*[^*]+\*\* " text) :stat
          :else (invalid-markdown! at (str "no live node paints this: " (pr-str head))))))

(defmulti ^:private markdown->node
  "One node per markdown form — [[node->markdown]] read backwards, method for
   method. The `:id` is not here: it is derived once, for every node at once, by
   [[addressed]]."
  (fn [type _block]
    type))

(defmethod markdown->node :status
  [_ {:keys [at lines]}]
  (when (> (count lines) 2)
    (invalid-markdown! at "a status paints its text and at most one italic detail"))
  (let [[text tone]
        (untoned (first lines))

        detail
        (italicized (second lines))]

    (when (and (second lines) (nil? detail))
      (invalid-markdown! at "a status' second line is its detail, written `_like this_`"))
    (cond-> {:type :status
             :text (or (second (re-matches #"\*\*(.+)\*\*" text))
                       (invalid-markdown! at "a status paints its text in bold"))
             :tone (or tone :idle)}
      detail
      (assoc :detail detail))))

(defmethod markdown->node :progress
  [_ {:keys [at lines]}]
  (when (> (count lines) 1) (invalid-markdown! at "a progress paints one line"))
  (let [[head counted]
        (str/split (first lines) #" · " 2)

        value
        (when-let [percent-text (second (re-matches #"\*\*(\d+)%\*\*" head))]
          (/ (long (parse-long percent-text)) 100.0))

        [_ done total]
        (some->> counted
                 (re-matches #"(\d+)(?:/(\d+))? done"))]

    (when-not (or value (= indeterminate-line head))
      (invalid-markdown! at "a progress paints `**N%**` or `_working_`"))
    (cond-> {:type :progress}
      value
      (assoc :value value)

      done
      (assoc :done (parse-long done))

      total
      (assoc :total (parse-long total)))))

(defmethod markdown->node :stat
  [_ {:keys [at lines]}]
  (when (> (count lines) 1) (invalid-markdown! at "a stat paints one strip"))
  (if (= (empty-line :stat) (first lines))
    {:type :stat :stats []}
    (let [entries (re-seq #"\*\*([^*]+)\*\* ?(.*?)(?= · \*\*|$)" (first lines))]
      (when (empty? entries) (invalid-markdown! at "a stat paints `**label** value`"))
      {:type :stat
       :stats (addressed "stat"
                         :label
                         (mapv (fn [[_ label value]]
                                 (let [[_ text named] (re-matches #"(.*?) ?\[([a-z-]+)\]" value)
                                       tone (when named (keyword named))]

                                   (cond-> {:label label :value-text (if (tone? tone) text value)}
                                     (tone? tone)
                                     (assoc :tone tone))))
                               entries))})))

(defmethod markdown->node :steps
  [_ {:keys [at lines]}]
  (if (= (empty-line :steps) (first lines))
    {:type :steps :steps []}
    {:type :steps
     :steps (addressed "step"
                       :label
                       (mapv (fn [line]
                               (let [[body tone]
                                     (untoned (bullet at line))

                                     [_ measured percent-text]
                                     (re-matches #"(.*) · (\d+)%" body)

                                     text
                                     (or measured body)

                                     [_ label detail]
                                     (re-matches #"(.*?) — (.*)" text)]

                                 (cond-> {:label (or label text) :tone (or tone :idle)}
                                   detail
                                   (assoc :detail detail)

                                   percent-text
                                   (assoc :value (/ (long (parse-long percent-text)) 100.0)))))
                             lines))}))

(defmethod markdown->node :log
  [_ {:keys [at lines]}]
  (if (= (empty-line :log) (first lines))
    {:type :log :lines [] :window-lines (long (:window-lines spec/log-defaults)) :total-lines 0}
    (let [fence
          (first lines)

          body
          (vec (rest lines))

          closing
          (or (first (keep-indexed (fn [i line]
                                     (when (= fence line) i))
                                   body))
              (invalid-markdown! at "a log's code fence is never closed"))

          window
          (subvec body 0 (long closing))

          behind
          (long (or (counted-behind #"… (\d+) earlier lines.*" (get body (inc (long closing)))) 0))]

      (with-meta {:type :log
                  :lines window
                  :window-lines (long (max (long (:window-lines spec/log-defaults)) (count window)))
                  :total-lines (+ (count window) behind)}
        {:elided behind}))))

(defn- table-cells
  "One painted row, back into cells: split on the pipes that were not escaped,
   drop the rail on either side, and give an escaped pipe its meaning back."
  [line]
  (let [parts
        (vec (str/split line #"(?<!\\)\|" -1))

        from
        (if (str/starts-with? (str/triml line) "|") 1 0)

        to
        (if (str/ends-with? (str/trimr line) "|") (dec (count parts)) (count parts))]

    (mapv (fn [cell]
            (str/replace (str/trim cell) "\\|" "|"))
          (subvec parts from (max (long from) (long to))))))

(defmethod markdown->node :table
  [_ {:keys [at lines]}]
  (let [noted
        (italicized (last lines))

        behind
        (long (or (some->> noted
                           (re-matches #"… (\d+) more rows.*")
                           second
                           parse-long)
                  0))

        painted
        (mapv table-cells
              (cond-> lines
                noted
                (subvec 0 (dec (count lines)))))

        [header rule]
        painted

        _
        (when (or (nil? rule) (not= (count header) (count rule)))
          (invalid-markdown! at "a table paints a header row and a rule of the same width"))

        toned?
        (and (= "!" (first header)) (= "---" (first rule)))

        columns
        (addressed "column"
                   :label
                   (mapv (fn [label align]
                           (cond-> {:label label}
                             (str/ends-with? align ":")
                             (assoc :align :right)))
                         (cond-> header
                           toned?
                           (subvec 1))
                         (cond-> rule
                           toned?
                           (subvec 1))))

        rows
        (addressed
          "row"
          (fn [row]
            (first (:cells row)))
          (mapv (fn [cells]
                  (let [tone
                        (when (and toned? (not (str/blank? (first cells)))) (keyword (first cells)))

                        painted-cells
                        (cond-> cells
                          toned?
                          (subvec 1))]

                    (when (> (count painted-cells) (count columns))
                      (invalid-markdown! at
                                         (str "a row paints more cells than the table declares: "
                                              (count painted-cells)
                                              " against " (count columns))))
                    (cond-> {:cells painted-cells}
                      (tone? tone)
                      (assoc :tone tone))))
                (drop 2 painted)))]

    (with-meta {:type :table
                :columns columns
                :rows rows
                :max-rows (long (:max-rows spec/table-defaults))
                :order :insertion}
      {:elided behind})))

(defmethod markdown->node :link
  [_ {:keys [at lines]}]
  (if (= (empty-line :link) (first lines))
    {:type :link :links []}
    {:type :link
     :links (addressed
              "link"
              :label
              (mapv (fn [line]
                      (let [[body tone]
                            (untoned (bullet at line))

                            [kind label target]
                            (or (some->> (re-matches #"\[(.+)\]\((.+)\)" body)
                                         rest
                                         (cons :url))
                                (some->> (re-matches #"(.+) — attachment `(.+)`" body)
                                         rest
                                         (cons :attachment))
                                (some->> (re-matches #"(.+) — `(.+)`" body)
                                         rest
                                         (cons :path))
                                (invalid-markdown!
                                  at
                                  (str "a link is `[label](url)`, a path or an attachment, not "
                                       (pr-str body))))]

                        (cond-> {:label label :target-kind kind :target target}
                          tone
                          (assoc :tone tone))))
                    lines))}))

(defn- verdict
  "The ending a `>` block states, as much of [[spec/live-result]] as a picture can
   carry: no `:view-id`, no `:artifact-id` and no `:markdown`, because those are
   the engine's and a picture holds none of them."
  [{:keys [at lines]}]
  (when (> (count lines) 1) (invalid-markdown! at "a verdict is one line"))
  (let [[_ named tail]
        (or (re-matches #"> \*\*([a-z-]+)\*\*(.*)" (first lines))
            (invalid-markdown! at "a verdict opens with `> **<ending>**`"))

        ending
        (keyword named)

        _
        (when-not (reason? ending)
          (invalid-markdown! at
                             (str "no view ends " (pr-str named)
                                  " — " (str/join ", " (sort (map name reason?))))))

        stated
        (str/replace tail #"^ — this view did not finish" "")

        marker
        " · error: "

        at-error
        (str/index-of stated marker)

        said
        (if at-error (subs stated 0 (long at-error)) stated)]

    (cond-> {:is-completed (= :completed ending) :reason ending}
      (str/starts-with? said " · ")
      (assoc :summary (subs said 3))

      at-error
      (assoc :error (subs stated (+ (long at-error) (count marker)))))))

(defn parse-markdown
  "A picture read back: the markdown [[->markdown]] wrote, as the view and the
   verdict that painted it — `{:view … :result … :elided …}`.

   THE LAW: a picture that elided nothing renders back exactly, so
   `(->markdown view {:result result})` is the markdown it was parsed from. That
   is what makes markdown two-way — a view can be AUTHORED as markdown, a
   rendered view can be re-read, and neither direction invents anything.

   Markdown paints the PICTURE, not the record, so what cannot cross is named
   here rather than discovered later. Ids are never painted: each is derived
   from the label the eye reads ([[addressed]]), deterministically, so a patch
   written against a parsed view still lands. The declaration-only keys
   (`:window-lines`, `:max-rows`, `:order`) are refilled with their defaults —
   the render reads none of them. A percent is painted whole, so the fraction
   that comes back is the one the eye saw. And `:elided` names every node whose
   picture COUNTED items it did not carry: a log still round-trips (the count is
   stamped, so the note is repainted), a table holds the rows it was shown.

   Every node is checked against `spec/live-node` before it is answered, so what
   this returns is a view the engine will run, or a refusal naming the line."
  [markdown]
  (let [numbered
        (map-indexed (fn [i line]
                       [(inc (long i)) line])
                     (str/split-lines (str markdown)))

        title
        (or (second (re-matches #"# (.+)" (str (second (first numbered)))))
            (invalid-markdown! 1 "a view opens with `# <title>`"))

        described
        (italicized (second (second numbered)))

        groups
        (blocks (drop (if described 2 1) numbered))

        ends?
        (str/starts-with? (str (first (:lines (first groups)))) "> ")

        result
        (when ends? (verdict (first groups)))

        painted
        (cond-> groups
          ends?
          rest)

        nodes
        (addressed "node"
                   :label
                   (mapv (fn [{:keys [at lines] :as group}]
                           (let [label
                                 (second (re-matches #"### (.+)" (first lines)))

                                 block
                                 (if label {:at (inc (long at)) :lines (vec (rest lines))} group)]

                             (when (empty? (:lines block))
                               (invalid-markdown! at
                                                  "a heading with nothing under it paints no node"))
                             (cond-> (markdown->node (block-type block) block)
                               label
                               (assoc :label label))))
                         painted))]

    (when (empty? nodes) (invalid-markdown! 1 "a view paints at least one node"))
    (doseq [[node group] (map vector nodes painted)]
      (when-let [problem (spec/live-node-error node)]
        (invalid-markdown! (:at group) problem)))
    {:view (cond-> {:title title :nodes nodes}
             described
             (assoc :description described))
     :result result
     :elided (into []
                   (keep (fn [node]
                           (let [items (long (or (:elided (meta node)) 0))]
                             (when (pos? items) {:node-id (:id node) :items items}))))
                   nodes)}))
