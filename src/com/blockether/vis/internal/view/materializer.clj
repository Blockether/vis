(ns com.blockether.vis.internal.view.materializer
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

   Nothing here evicts the RECORD. Bounds on keyed collections are REFUSALS of the
   shared node check (`vis-contract`) naming the bound and the node, a log's
   `:window-lines` is only how much a surface holds hot, and the model's own budget
   always says how many lines it left behind."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [com.blockether.vis.contract.app :as app]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.view :as spec]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.util :as util]))

(defn log-text
  "Make C0/C1 terminal controls visible in retained output; tab and newline stay literal.
   No escape is interpreted or removed. Run before redaction and publication."
  [text]
  (str/replace (str text)
               #"[\u0000-\u0008\u000b-\u001f\u007f-\u009f]"
               (fn [s]
                 (format "\\u%04x" (int (first s))))))

(def ^:private presentation-text-keys
  #{:title :label :description :placeholder :source :text :detail :summary :error :note :message
    :alt :model-result :target :value :value-text :parent :lines :cells})

(defn redact-presentation
  "Redact human text in normalized View data, never structural IDs, selections,
   ordering or enums. Recurse through containers, propagating text context into
   lines/cells but choosing it afresh for every map key. Pure and idempotent."
  [value]
  (letfn [(visit [value text?]
            (cond (map? value) (into {}
                                     (map (fn [[k v]]
                                            [k (visit v (contains? presentation-text-keys k))]))
                                     value)
                  (vector? value) (mapv #(visit % text?) value)
                  (and text? (string? value)) (util/redact-secret-text value)
                  :else value))]
    (visit value false)))

;; The materializer

(defn- invalid-patch!
  "Refuse a patch the way the parser refuses a request: one line naming what to
   fix, thrown where the extension wrote it."
  [message]
  (throw (ex-info (str spec/patch-refusal-prefix message)
                  {:type :vis/view-invalid-patch :reason message})))

(defn- app-rule
  "Run one app rule of [[com.blockether.vis.contract.app]]. A rule that refuses
   refuses the patch with its own reason."
  [f]
  (try
    (f)
    (catch clojure.lang.ExceptionInfo e
      (if (= :app/invalid (:type (ex-data e))) (invalid-patch! (:reason (ex-data e))) (throw e)))))

(defn- node-tree
  "Every node of the view depth first, including all children.
   Ids are unique across the whole tree, so this is what an ADDRESS is looked up
   in."
  [nodes]
  (into []
        (mapcat (fn [node]
                  (cons node (node-tree (:children node)))))
        nodes))

(defn- leaf-nodes
  "Every node that PAINTS something, depth first. A layout container holds no state of
   its own — it says where the nodes inside it stand — so the model and the
   document read the same content whether a status was stood beside a table or
   under it."
  [nodes]
  (into []
        (mapcat (fn [node]
                  (cond
                    ;; A diff paints itself; its toolbar buttons follow it.
                    (= "diff" (name (:type node ""))) (cons (dissoc node :children)
                                                            (leaf-nodes (:children node)))
                    (seq (:children node)) (leaf-nodes (:children node))
                    :else [node])))
        nodes))

(defn- node-ids
  "Every node id in the view, in paint order — what a refusal lists when an op
   named an address that is not there. Containers are in it: `add-node :after` and
   `remove-node` speak to a row by id too."
  [view]
  (mapv :id (node-tree (:nodes view))))

(defn- path-in
  [nodes node-id]
  (first (keep-indexed (fn [i node]
                         (cond (= node-id (:id node)) [i]
                               (seq (:children node)) (when-let [sub (path-in (:children node)
                                                                              node-id)]
                                                        (into [i :children] sub))))
                       nodes)))

(defn- node-path
  "The path from the view down to `node-id` — `[:nodes 2 :children 0]` for a node
   inside a container — or nil. Ids are addresses and unique across
   the tree, so a node standing in a row is patched exactly like one at the top."
  [view node-id]
  (when-let [found (path-in (:nodes view) node-id)]
    (into [:nodes] found)))

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

(defn- merge-by-id
  "`incoming` merged into `existing` by id like [[upsert]], except that keys the
   new item leaves out are KEPT: re-toning a group mid-run must not drop the
   label it was declared with, because the producer that knows the tone changed
   may not be the call that named the group."
  [existing incoming]
  (let [known (into {} (map (juxt :id identity)) existing)]
    (upsert existing
            (mapv (fn [item]
                    (merge (get known (:id item)) item))
                  incoming))))

(defn- without-ids
  "`existing` without the named items. Removing an id that is not there is a
   NO-OP: a patch states the wanted state, and an extension polling a fleet
   must not have to remember what it already said."
  [existing item-ids]
  (let [dropped? (set item-ids)]
    (into [] (remove (comp dropped? :id)) existing)))

(defn- trim-log-window
  "Copy a log's visible tail so discarded lines and tones are no longer reachable."
  [node]
  (let [overflow (max 0 (- (count (:lines node)) (long (:window-lines node))))]
    (if (pos? overflow)
      (cond-> (update node :lines #(into [] (subvec % overflow)))
        (:line-tones node)
        (update :line-tones #(into [] (subvec % overflow))))
      node)))

(defn- stamped-log
  "A log node carrying `:total-lines` — how many lines its RECORD has, window
   included. Stamped by the engine so `… N earlier lines` is counted, never
   guessed, on every surface."
  [node]
  (cond-> node
    (= :log (:type node))
    (update :total-lines #(long (or % (count (:lines node)))))

    (or (= :button (:type node)) (and (= :card (:type node)) (:is-selectable node)))
    (update :clicks #(long (or % 0)))))

(defn- stamped
  "`node` stamped through: a layout group hands the stamp to the nodes inside it,
   because a log arranged into a row is still a log."
  [node]
  (cond-> (stamped-log node)
    (seq (:children node))
    (update :children #(mapv stamped %))))

(defn record-view
  "Stamp the complete initial declaration before writing it to the journal.
   This transient record includes every initial log line; use `materialize` for
   the bounded state retained by a live view."
  [view]
  (update view :nodes #(mapv stamped %)))

(defn- windowed-node
  [node]
  (cond-> node
    (= :log (:type node))
    trim-log-window

    (seq (:children node))
    (update :children #(mapv windowed-node %))))

(defn materialize
  "Stamp the declared view and keep only each log's visible window.
   The complete declaration must be journaled before retaining this projection."
  [view]
  (update (record-view view) :nodes #(mapv windowed-node %)))

(defn- checked-node
  "`node` once it passes the one node check of every host, else a refusal with its
   fault text, as the recorder of the SDK gives it (`live-view-checks.json`)."
  [node]
  (if-some [text (spec/live-node-fault (spec/wire-data node) false)]
    (invalid-patch! text)
    node))

(defn- checked-op!
  "Refuse `op` when the shared patch target table says that it does not apply to
   `node`, or that `node` does not take one of its keys."
  [node op]
  (when-some [lines (seq (spec/live-op-target-lines (name (:type node)) (:id node) op))]
    (invalid-patch! (spec/node-fault-text (:id node) lines))))

(defn- apply-set
  "A node with its own state replaced. The target table allows each key, and the node
   check after the op holds the result to the schema."
  [node op]
  (let [given (disj (set (keys op)) :op :node-id)]
    (cond-> (merge node (select-keys op (vec given)))
      ;; A blank `:detail` REMOVES it — the same "no detail" a declaration
      ;; states by leaving the key out, so a status can drop the second
      ;; line it was carrying instead of refusing the patch.
      (and (contains? given :detail) (str/blank? (:detail op)))
      (dissoc :detail)

      (and (#{:canvas :board} (:type node)) (false? (:is-selectable op)))
      (dissoc :selected-ids))))

(defn- apply-append
  "A node with items added. A `log` grows its window and its record; a keyed
   node upserts by id, and the node check holds it to its bound. A scene merges
   its objects."
  [node op]
  (let [given (disj (set (keys op)) :op :node-id :tone)]
    (if (= :log (:type node))
      (let [items (:lines op)
            all (into (:lines node) items)
            styled? (or (contains? node :line-tones) (contains? op :tone))
            tones (when styled?
                    (into (or (:line-tones node) (vec (repeat (count (:lines node)) nil)))
                          (repeat (count items) (:tone op))))]

        (trim-log-window (cond-> (assoc node
                                   :lines all
                                   :total-lines (+ (long (:total-lines node)) (count items)))
                           styled?
                           (assoc :line-tones tones))))
      (reduce (fn [node k]
                (case k
                  :groups
                  (assoc node :groups (merge-by-id (or (:groups node) []) (:groups op)))

                  :objects
                  (update node :objects spec/scene-merge (:objects op))

                  (update node k #(upsert (or % []) (get op k)))))
              node
              (sort given)))))

(defn- prune-selected
  "Drop selected ids whose rows or cards no longer exist, without adding selection
   metadata to a node that never declared the control."
  [node]
  (if (contains? node :selected-ids)
    (let [held (set (map :id (get node (:key (spec/item-bounds (:type node))))))]
      (update node :selected-ids #(filterv held %)))
    node))

(defn- apply-remove
  "Remove the named items and threads."
  [node op]
  (let [ids (:item-ids op)]
    (if (= :diff (:type node))
      (app/remove-threads node ids)
      (cond-> (update node (:key (spec/item-bounds (:type node))) without-ids ids)
        (= :canvas (:type node))
        (app/remove-threads ids)

        (#{:table :canvas :board} (:type node))
        prune-selected))))

(defn- apply-clear
  "A node emptied. Clearing a log empties the WINDOW and starts its RECORD over: the
   record reader (`live-sink/log-range`) cannot serve what came before a clear either,
   so a kept count would offer earlier lines nothing can answer."
  [node]
  (case (:type node)
    :log
    (-> node
        (assoc :lines []
               :total-lines 0)
        (dissoc :line-tones))

    (cond-> (assoc node (:key (spec/item-bounds (:type node))) [])
      (and (= :canvas (:type node)) (contains? node :threads))
      (assoc :threads [])

      (#{:table :canvas :board} (:type node))
      prune-selected)))

(defn- apply-replace
  "A table whose rows are exactly the op's `:rows`, or a board whose issues are
   exactly the op's `:issues`, in their order. Selection is pruned in the same step,
   so an item that comes back stays selected and an item that is gone does not."
  [node op]
  (let [k (if (contains? op :issues) :issues :rows)]
    (prune-selected (assoc node k (upsert [] (get op k))))))

(defn- insert-after
  "`nodes` with `node` in the slot right after `idx` — where the eye expects it,
   next to the node the op named."
  [nodes idx node]
  (into (conj (into [] (subvec nodes 0 (inc (long idx)))) node) (subvec nodes (inc (long idx)))))

(defn- drop-at
  "`nodes` without the one at `idx`."
  [nodes idx]
  (into (into [] (subvec nodes 0 (long idx))) (subvec nodes (inc (long idx)))))

(defn- checked-parent
  "`view` once the container that holds the node at `path` passes the node check
   of every host. A node at the top has no parent."
  [view path]
  (when (< 2 (count path)) (checked-node (get-in view (pop (pop path)))))
  view)

(defn- apply-add-node
  "The view with one more node — the shape changing while it runs, because a
   scan that discovers a seventh device should not have to have declared it.
   `:after` names the node the newcomer stands next to WHEREVER that node is, so
   a node added after one a container joins that container; a container added
   this way lands with everything inside it. The view check of [[apply-patch]]
   refuses a repeated id and too many nodes."
  [view {:keys [node-spec after]}]
  (let [node (windowed-node (stamped (checked-node node-spec)))]
    (if-let [path (when after
                    (or (node-path view after)
                        (invalid-patch!
                          (spec/node-fault-text
                            (:id node)
                            [(document/named-line "/after" "missing_after" {"after" after})]))))]
      (-> (update-in view (pop path) insert-after (peek path) node)
          (checked-parent path))
      (update view :nodes conj node))))

(defn- apply-remove-node
  "The view without that node, its items with it — and a container without the
   nodes it arranged, because dropping the row drops what stood in it. Dropping a
   node that is not there is a NO-OP — teardown is idempotent, while a WRITE to a
   node that is gone is a lost patch and refuses."
  [view {:keys [node-id]}]
  (if-let [path (node-path view node-id)]
    (-> (update-in view (pop path) drop-at (peek path))
        (checked-parent path))
    view))

(defn- apply-set-value
  "An input with its new `:value`, or without one when the value is nil. The node
   check after the op holds the value to the input domain and refuses a value on a
   secret input."
  [node {:keys [value]}]
  (if (nil? value) (dissoc node :value) (assoc node :value value)))

(def ^:private view-appliers
  "The operations that change the view itself, not one of its nodes."
  {:add-node apply-add-node
   :remove-node apply-remove-node
   :submission (fn [view op]
                 (app-rule #(app/upsert-submission view (:submission op))))
   :shortcuts (fn [view op]
                (app-rule #(app/set-shortcuts view (:shortcuts op))))
   :agent-note (fn [view op]
                 (app-rule #(app/set-agent-note view (:text op))))
   :log (fn [view op]
          (app-rule #(app/append-log view op)))})

(def ^:private node-appliers
  "The operations on one node, each answering the node after it."
  {:set apply-set
   :append apply-append
   :remove apply-remove
   :clear (fn [node _]
            (apply-clear node))
   :replace apply-replace
   :thread (fn [node op]
             (app/upsert-thread node (:thread op)))
   :set-value apply-set-value})

(defn- checked-node-path
  [view node-id]
  (or (node-path view node-id)
      (invalid-patch! (spec/node-fault-text node-id
                                            [(document/named-line
                                               "/node_id"
                                               "missing_node"
                                               {"ids" (str/join ", " (node-ids view))})]))))

(defn- canvas-camera-op?
  "True when `op` only changes how a canvas is shown, not its data."
  [op]
  (and (= :set (:op op)) (every? #{:op :node-id :zoom :focus :is-locked :flow :pattern} (keys op))))

(defn- apply-node-op
  "The view after ONE operation on the node that `op` names; an unknown node is
   refused with the ids the view has."
  [view op]
  (let [path
        (checked-node-path view (:node-id op))

        apply-node
        (or (get node-appliers (:op op))
            (throw (IllegalArgumentException. (str "No matching clause: " (:op op)))))

        node
        (get-in view path)]

    ;; The shared target table first, then one node check after each op, so a
    ;; patch is held to the same rules as a declaration.
    (checked-op! node op)
    (let [after (apply-node node op)]
      (assoc-in view
        path
        (cond (and (= :canvas (:type node)) (canvas-camera-op? op))
              (do (checked-node (assoc (dissoc after :threads :selected-ids) :cards [])) after)
              (and (= :canvas (:type node)) (spec/canvas-change-checked? after op)) after
              :else (checked-node after))))))

(defn- apply-op
  "The view after ONE operation."
  [view op]
  (if-let [apply-view (get view-appliers (:op op))]
    (apply-view view op)
    (apply-node-op view op)))

(defn- item-references? [op] (or (contains? op :selected-ids) (contains? op :item-ids)))

(defn- land-node-op
  [view op]
  (let [resolve?
        (app/resolution-only? (:thread op))

        node
        (when (or resolve? (item-references? op))
          (get-in
            view
            (if resolve? (checked-node-path view (:node-id op)) (node-path view (:node-id op)))))]

    (if resolve?
      (let [{:keys [id is-resolved]} (:thread op)]
        (checked-op! node op)
        (if-let [thread (app/find-thread node id)]
          (assoc op :thread (assoc thread :is-resolved is-resolved))
          (invalid-patch! (spec/node-fault-text (:node-id op)
                                                [(document/named-line "/thread/id"
                                                                      "thread_not_found")]))))
      (cond-> op
        (and (= :canvas (:type node)) (contains? op :selected-ids))
        (update :selected-ids #(mapv str/trim %))

        (and (= :canvas (:type node)) (contains? op :item-ids))
        (update :item-ids #(mapv str/trim %))))))

(defn land-node-refs
  "Normalize canvas references and expand thread resolution requests before storage.
   `prepare-op` applies owner rules to the state before each operation, when set."
  [view patch prepare-op]
  (let [ops
        (:ops patch)

        land
        (fn [current op]
          (land-node-op current (if prepare-op (prepare-op current op) op)))]

    (assoc patch
      :ops (if (or (and prepare-op (some #(= :thread (:op %)) (rest ops)))
                   (some (comp app/resolution-only? :thread) ops)
                   (and (some item-references? ops)
                        (some (comp #{:add-node :remove-node} :op) ops)))
             (second (reduce (fn [[current landed] op]
                               (let [op (land current op)]
                                 [(apply-op current op) (conj landed op)]))
                             [view []]
                             ops))
             (mapv #(land view %) ops)))))

(defn apply-patch
  "`view` after every operation in `patch`, or a refusal naming the first one
   that could not land.

   ALL OR NOTHING: a patch that refuses half way returns nothing and leaves the
   view it was handed, so no surface ever paints half a patch. `:seq` must
   ADVANCE — a replayed or duplicated patch is refused rather than applied
   twice, which is what lets a surface treat a gap as `re-read the snapshot`."
  [view patch]
  (when-let [reason (spec/live-patch-error patch)]
    (invalid-patch! reason))
  (when-not (= (:view-id patch) (:id view))
    (invalid-patch! (document/rule-text "patch_view"
                                        {"patch_view" (:view-id patch) "view" (:id view)})))
  (when-not (> (long (:seq patch)) (long (:seq view)))
    (invalid-patch! (document/rule-text "patch_seq" {"seq" (:seq patch) "view_seq" (:seq view)})))
  (let [ops
        (:ops patch)

        patched
        (reduce apply-op view ops)]

    ;; Check the whole view after all ops. Camera values passed their node
    ;; check and cannot change shortcuts, node ids or counts, or picture bytes.
    (when-not (every? #(and (canvas-camera-op? %)
                            (= :canvas (:type (get-in view (node-path view (:node-id %))))))
                      ops)
      (when-some [lines (seq (spec/live-view-lines (spec/wire-data patched) false))]
        (invalid-patch! (spec/view-fault-text lines))))
    (assoc patched :seq (:seq patch))))

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
   :timeline "_no lanes yet_"
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
  (throw (ex-info (str spec/view-refusal-prefix message)
                  {:type :vis/view-invalid-live-view :reason message})))

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

(defn- marked-lines
  "Keep presentation types and literal blank lines unambiguous when reading markdown back."
  [type lines]
  (let [painted (vec (mapcat #(str/split % #"\n" -1) lines))]
    (into [(str "<!-- vis:" (name type) " " (count painted) " -->")] painted)))

(defmethod node->markdown :paragraph [{:keys [text]} _] (marked-lines :paragraph [text]))

(defmethod node->markdown :divider [_ _] (marked-lines :divider ["---"]))

(defmethod node->markdown :heading
  [{:keys [text level]} _]
  (marked-lines :heading [(str (apply str (repeat (long level) "#")) " " text)]))

(defmethod node->markdown :code
  [{:keys [text language]} _]
  (let [fenced (vec (fenced (str/split text #"\n" -1)))]
    (marked-lines :code (update fenced 0 str (or language "")))))

(defn- tagged-json
  "One line: `tag`, then `value` as wire JSON. JSON keeps every text exact."
  [tag value]
  (str tag
       " "
       (wire/json-str (walk/postwalk #(if (map? %) (into (sorted-map) %) %) (wire/->wire value)))))

(declare preview-facts)

(defmethod node->markdown :diff
  [{:keys [patch old-revision new-revision threads previews]} _]
  (let [fenced (vec (fenced (str/split patch #"\n" -1)))]
    (marked-lines :diff
                  (concat [(tagged-json "revisions"
                                        (cond-> {}
                                          old-revision
                                          (assoc :old-revision old-revision)

                                          new-revision
                                          (assoc :new-revision new-revision)))]
                          (update fenced 0 str "diff")
                          (map #(tagged-json "thread" %) threads)
                          (map #(tagged-json "preview" (preview-facts %)) previews)))))

(defn- picture-facts
  "What a reader learns of picture `p` without its bytes: media type, size in
   bytes and the declared width and height."
  [{:keys [media-type data width height bytes]}]
  (let [text
        (str data)

        padding
        (count (take-while #{\=} (reverse text)))]

    (cond-> {:media-type media-type :bytes (if data (- (quot (* 3 (count text)) 4) padding) bytes)}
      width
      (assoc :width width)

      height
      (assoc :height height))))

(defn- image-facts
  "Image node `node` as the model reads it: the picture facts, the alt text and
   the facts of the `:before` picture. The base64 bytes never reach the model."
  [{:keys [alt before] :as node}]
  (cond-> (picture-facts node)
    alt
    (assoc :alt alt)

    before
    (assoc :before (picture-facts before))))

(defmethod node->markdown :image
  [node _]
  (marked-lines :image [(tagged-json "image" (image-facts node))]))

(defn- media-source-facts
  "What a reader learns of media source `m` without its bytes: media type, web
   address and size in bytes."
  [{:keys [media-type data bytes url]}]
  (cond-> {}
    media-type
    (assoc :media-type media-type)

    url
    (assoc :url url)

    (or data bytes)
    (assoc :bytes (if data (spec/picture-byte-count data) bytes))))

(defn- media-facts
  "Media node `node` as the model reads it: kind, alt text and the facts of each
   version. The base64 bytes never reach the model."
  [{:keys [kind alt before] :as node}]
  (cond-> (assoc (media-source-facts node) :kind kind :alt alt)
    before
    (assoc :before (media-source-facts before))))

(defn- media-elided
  "How many versions of media node `node` lose their bytes in the model copy."
  [node]
  (count (filter :data [node (:before node)])))

(defmethod node->markdown :media
  [node _]
  (marked-lines :media [(tagged-json "media" (media-facts node))]))

(defn- preview-facts
  "Diff preview `preview` as the model reads it: the path and the facts of its
   image or media node. The base64 bytes never reach the model."
  [{:keys [path node]}]
  {:path path
   :node (merge (select-keys node [:id :type :label])
                (if (= "image" (name (:type node))) (image-facts node) (media-facts node)))})

(defn- previews-elided
  "How many versions of the diff previews `previews` lose their bytes in the model copy."
  [previews]
  (transduce (map (fn [{:keys [node]}]
                    (if (= "image" (name (:type node)))
                      (if (:before node) 2 1)
                      (count (filter #(or (:data %) (:bytes %)) [node (:before node)])))))
             +
             0
             previews))

(defmethod node->markdown :thread
  [node _]
  (marked-lines :thread
                [(tagged-json "thread"
                              (select-keys node [:anchor :messages :draft :is-resolved]))]))

(defmethod node->markdown :badge
  [node _]
  (marked-lines :badge [(tagged-json "badge" (select-keys node [:badges]))]))

(defmethod node->markdown :list
  [node _]
  (marked-lines :list [(tagged-json "list" (select-keys node [:is-ordered :items]))]))

(defmethod node->markdown :chart
  [node _]
  (marked-lines :chart [(tagged-json "chart" (select-keys node [:kind :categories :series]))]))

(defn- plain-number
  "A number as a person writes it: 48 rather than 48.0."
  [n]
  (let [d (double n)]
    (if (== d (Math/rint d)) (str (long d)) (str d))))

(defmethod node->markdown :scene
  [{:keys [width height objects] :as node} _]
  (marked-lines :scene
                [(str "Scene "
                      (plain-number width)
                      " x "
                      (plain-number height)
                      " · "
                      (count objects)
                      " objects")
                 (tagged-json "scene"
                              (select-keys node
                                           [:width :height :background :fov :is-wrapping :tick-ms
                                            :meshes :objects]))]))

(defmethod node->markdown :canvas
  [{:keys [cards] :as node} _]
  (marked-lines :canvas
                [(str "Canvas · " (count cards) " cards")
                 (tagged-json "canvas"
                              (select-keys (update node :cards #(mapv spec/canvas-card-facts %))
                                           [:flow :pattern :zoom :focus :is-locked :is-selectable
                                            :selected-ids :cards :threads]))]))

(defmethod node->markdown :board
  [{:keys [columns issues] :as node} _]
  (let [held (frequencies (map :column issues))]
    (marked-lines :board
                  [(str "Board · " (count issues)
                        " issues" (apply str
                                    (map #(str " · " (:title %) " " (get held (:id %) 0)) columns)))
                   (tagged-json "board"
                                (select-keys node
                                             [:is-editable :is-selectable :selected-ids :columns
                                              :people :epics :issues]))])))

(defmethod node->markdown :spinner
  [{:keys [text is-active]} _]
  (marked-lines :spinner [(str text (when is-active " (working)"))]))

(defmethod node->markdown :button
  [{:keys [label clicks]} _]
  (marked-lines :button [(str label " (button; " (long (or clicks 0)) " activations)")]))

(defmethod node->markdown :card
  [{:keys [title text tone meta]} _]
  (marked-lines :card
                (cond-> [(str (tone-tag tone) title)]
                  meta
                  (conj (str "_" meta "_"))

                  (seq text)
                  (conj text))))

(defn- input-text
  "An input value as one line of text. A secret input shows only that it is set."
  [{:keys [type value is-secret]}]
  (cond (nil? value) ""
        is-secret "(set)"
        (= :checkbox type) (if value "yes" "no")
        (sequential? value) (str/join ", " value)
        :else (str value)))

(def ^:private input-domain-keys
  "The keys that bound the value of an input, as the wire spells them."
  {:is-multiple "is_multiple"
   :options "options"
   :min "min"
   :max "max"
   :step "step"
   :mode "mode"
   :min-length "min_length"
   :max-length "max_length"})

(defn- input-domain-line
  "The domain of input `node` as one `Domain:` line of JSON, or nil when it has none."
  [node]
  (let [domain (into {}
                     (keep (fn [[k wire-key]]
                             (when (contains? node k)
                               [wire-key
                                (case k
                                  :options
                                  (mapv #(select-keys % [:value :label :icon]) (k node))

                                  :mode
                                  (name (k node))

                                  (k node))])))
                     input-domain-keys)]
    (when (seq domain) (str "Domain: " (wire/json-str domain)))))

(doseq [input-type (vals spec/field-types)]
  (defmethod node->markdown input-type
    [{:keys [label] :as node} _]
    (marked-lines input-type
                  (into (vec (keep identity [(input-domain-line node)]))
                        [(str label ": " (input-text node))]))))

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

(def ^:private lane-time-format
  "UTC with exactly three millisecond digits, also when they are zero."
  (.withZone (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
             java.time.ZoneOffset/UTC))

(defn- iso-ms
  "Epoch milliseconds as UTC ISO-8601 with milliseconds, the one time form a
   timeline lane prints and reads back."
  [ms]
  (.format ^java.time.format.DateTimeFormatter lane-time-format
           (java.time.Instant/ofEpochMilli (long ms))))

(defmethod node->markdown :timeline
  [{:keys [lanes]} _]
  (if (seq lanes)
    (mapv (fn [{:keys [label tone detail start end]}]
            (str "- "
                 (tone-tag tone)
                 label
                 (when detail (str " — " detail))
                 " · " (iso-ms start)
                 " → " (if end (iso-ms end) "now")))
          lanes)
    [(empty-line :timeline)]))

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

(def ^:private parent-marker
  "The column a grouped table paints each row's parent in — what `!` is to tone.
   A MARKER, never a label: a declared column carries its own name, and this one
   is written and read back by the two table forms alone."
  "/")

(defn- group-note
  "One DECLARED group, as the note a table trails its rows with: the marker, the
   id rows point at, then the state no row carries — tone, place in the order and
   whether it opens by itself. The label comes LAST, so free text can never be
   read back as one of those flags."
  [{:keys [id label tone order is-open]}]
  (str "_"
       parent-marker
       " "
       id
       (when tone (str " !" (name tone)))
       (when order (str " #" order))
       (when is-open " +open")
       (when label (str " = " label))
       "_"))

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

        ;; A row's PARENT is the group it was declared into, so the document carries
        ;; it the way it carries a tone: one marker column, written when some row has
        ;; one and read back by [[markdown->node]]. Without it the picture the model
        ;; reads — and every document a view is reopened from — lost the grouping the
        ;; surfaces paint.
        parented?
        (boolean (some :parent rows))

        header
        (cond->> (mapv :label columns)
          parented?
          (into [parent-marker])

          toned?
          (into ["!"]))

        rule
        (cond->> (mapv #(if (= :right (:align %)) "---:" "---") columns)
          parented?
          (into ["---"])

          toned?
          (into ["---"]))

        row->cells
        (fn [row]
          (cond->> (mapv #(cell-text (cell-at columns row (:id %))) columns)
            parented?
            (into [(or (:parent row) "")])

            toned?
            (into [(if (:tone row) (name (:tone row)) "")])))

        line
        (fn [cells]
          (str "| " (str/join " | " cells) " |"))]

    (cond-> (into [(line header) (line rule)] (map (comp line row->cells)) shown)
      (seq (:groups node))
      (into (map group-note) (:groups node))

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

(defn- budget-log
  "A log node with only its TAIL: the last `log-tail-lines` lines and their tones."
  [node {:keys [log-tail-lines]}]
  (let [lines
        (:lines node)

        tail
        (long log-tail-lines)

        shown
        (if (> (count lines) tail) (into [] (subvec lines (- (count lines) tail))) lines)

        behind
        (- (long (or (:total-lines node) (count lines))) (count shown))]

    (with-meta (cond-> (assoc node :lines shown)
                 (:line-tones node)
                 (update :line-tones #(vec (take-last (count shown) %))))
      {:elided (max 0 behind)})))

(defn- budget-table
  "A table node with the head of its rows in PAINT order, and no selection."
  [node {:keys [table-rows]}]
  (let [rows
        (ordered-rows node)

        limit
        (long table-rows)

        shown
        (if (> (count rows) limit) (into [] (subvec rows 0 limit)) rows)]

    (with-meta (-> node
                   (assoc :rows shown
                          :order :insertion)
                   (dissoc :is-selectable :selected-ids))
      {:elided (- (count rows) (count shown))})))

(defn- budget-image
  "An image node as facts: its pictures never reach the model, and they count as elided."
  [node _]
  (with-meta (merge (select-keys node [:id :type :label]) (image-facts node))
    {:elided (if (:before node) 2 1)}))

(defn- budget-media
  "A media node as facts: its bytes never reach the model, and they count as elided."
  [node _]
  (with-meta (merge (select-keys node [:id :type :label]) (media-facts node))
    {:elided (media-elided node)}))

(defn- budget-scene
  "A scene node with the first `table-rows` objects."
  [node {:keys [table-rows]}]
  (let [objects
        (:objects node)

        limit
        (long table-rows)

        shown
        (if (> (count objects) limit) (subvec objects 0 limit) objects)]

    (with-meta (assoc node :objects shown) {:elided (- (count objects) (count shown))})))

(defn- budget-canvas
  "A canvas node with the first `table-rows` cards, and no selection."
  [node {:keys [table-rows]}]
  (let [cards
        (:cards node)

        limit
        (long table-rows)

        shown
        (if (> (count cards) limit) (subvec cards 0 limit) cards)]

    (with-meta (-> node
                   (assoc :cards (mapv spec/canvas-card-facts shown))
                   (dissoc :is-selectable :selected-ids))
      {:elided (- (count cards) (count shown))})))

(defn- budget-board
  "A board node with the first `table-rows` issues, and no selection."
  [node {:keys [table-rows]}]
  (let [issues
        (:issues node)

        limit
        (long table-rows)

        shown
        (if (> (count issues) limit) (subvec issues 0 limit) issues)]

    (with-meta (-> node
                   (assoc :issues shown)
                   (dissoc :is-selectable :selected-ids))
      {:elided (- (count issues) (count shown))})))

(defn- budget-diff
  "A diff node with each preview as facts: their bytes never reach the model, and they
   count as elided."
  [node _]
  (if (seq (:previews node))
    (with-meta (update node :previews #(mapv preview-facts %))
      {:elided (previews-elided (:previews node))})
    node))

(defn- budget-node
  "One node as the model is handed it under `budget`; `:elided` meta counts what it left."
  [node budget]
  (case (:type node)
    :diff
    (budget-diff node budget)

    :log
    (budget-log node budget)

    :table
    (budget-table node budget)

    :image
    (budget-image node budget)

    :media
    (budget-media node budget)

    :scene
    (budget-scene node budget)

    :canvas
    (budget-canvas node budget)

    :board
    (budget-board node budget)

    node))

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
   (let [budget
         (merge model-budget (select-keys opts [:log-tail-lines :table-rows]))

         budgeted
         (mapv #(budget-node % budget) (leaf-nodes (:nodes view)))]

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
                  {:type :vis/view-invalid-markdown :line line-no :reason message})))

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

(def ^:private input-types "The node types that hold one answer." (set (vals spec/field-types)))

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

                     [(conj acc
                            (cond-> (assoc item :id id)
                              (contains? input-types (:type item))
                              (assoc :name id))) (conj taken id)]))
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

(defn- trailing-notes
  "A block's PAINTED lines and the `_…_` notes trailing them. A table writes its
   group declarations and its budget note there, so both are read back without
   either being mistaken for a row."
  [lines]
  (let [painted
        (vec lines)

        cut
        (- (count painted) (count (take-while (comp some? italicized) (rseq painted))))]

    [(subvec painted 0 cut) (subvec painted cut)]))

(defn- note->group
  "A `_/ …_` note back into the group it declares, or nil when the note says
   something else — the budget line lives in the same place."
  [line]
  (when-let [[_ id flags label] (some->> (italicized line)
                                         (re-matches
                                           #"/ (\S+)((?: ![a-z-]+| #-?\d+| \+open)*)(?: = (.*))?"))]
    (let [tone (some-> (re-find #" !([a-z-]+)" flags)
                       second
                       keyword)
          order (some-> (re-find #" #(-?\d+)" flags)
                        second
                        parse-long)]

      (cond-> {:id id}
        (tone? tone)
        (assoc :tone tone)

        order
        (assoc :order order)

        (str/includes? flags "+open")
        (assoc :is-open true)

        (not (str/blank? label))
        (assoc :label label)))))

(def ^:private marked-types
  "The node types whose markdown form starts with a counted `vis:` marker."
  (into #{:paragraph :heading :divider :code :spinner :button :card :diff :thread :scene :image
          :media :badge :list :chart :canvas :board}
        (vals spec/field-types)))

(def ^:private presentation-marker
  (re-pattern (str "<!-- vis:(" (str/join "|" (sort (map name marked-types))) ") ([0-9]+) -->")))

(defn- blocks
  "Group numbered lines without splitting fences or counted presentation content."
  [numbered]
  (let [{:keys [acc at cur remaining]}
        (reduce (fn [{:keys [acc at cur fence remaining] :as state} [n line]]
                  (cond (pos? (long (or remaining 0))) (assoc state
                                                         :cur (conj cur line)
                                                         :remaining (dec (long remaining)))
                        (and fence (= line fence)) (assoc state
                                                     :cur (conj cur line)
                                                     :fence nil)
                        fence (assoc state :cur (conj cur line))
                        (re-matches presentation-marker line)
                        (assoc state
                          :cur (conj (or cur []) line)
                          :at (or at n)
                          :remaining (parse-long (nth (re-matches presentation-marker line) 2)))
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
                {:acc [] :at nil :cur nil :fence nil :remaining 0}
                numbered)]
    (when (pos? (long remaining)) (invalid-markdown! at "presentation content is incomplete"))
    (cond-> acc
      (seq cur)
      (conj {:at at :lines cur}))))

(defn- link-item?
  "True when a bullet is one of the three shapes a `link` node paints."
  [text]
  (boolean (or (re-matches #"\[.+\]\(.+\)" text)
               (re-matches #".+ — attachment `.+`" text)
               (re-matches #".+ — `.+`" text))))

(def ^:private lane-line
  "One painted timeline lane: what it says, then its start and its end or `now`."
  #"(.*) · (\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z) → (\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z|now)")

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

    (cond (re-matches presentation-marker head) (keyword (second (re-matches presentation-marker
                                                                             head)))
          (contains? type-painting-nothing head) (type-painting-nothing head)
          (re-matches #"`{3,}" head) :log
          (str/starts-with? head "|") :table
          (and (str/starts-with? head "- ")
               (every? #(re-matches lane-line (first (untoned (bullet at %)))) lines))
          :timeline
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

(defmethod markdown->node :paragraph
  [_ {:keys [lines]}]
  {:type :paragraph :text (str/join "\n" (rest lines))})

(defmethod markdown->node :divider
  [_ {:keys [at lines]}]
  (when-not (= ["---"] (vec (rest lines)))
    (invalid-markdown! at "a divider is one horizontal rule"))
  {:type :divider})

(defmethod markdown->node :heading
  [_ {:keys [at lines]}]
  (let [[_ marks text] (or (re-matches #"(#{1,6}) (.*)" (second lines))
                           (invalid-markdown! at "a heading needs a level from 1 to 6"))]
    {:type :heading :level (count marks) :text (str/join "\n" (cons text (drop 2 lines)))}))

(defmethod markdown->node :code
  [_ {:keys [at lines]}]
  (let [[_ fence language] (or (re-matches #"(`{3,})(.*)" (second lines))
                               (invalid-markdown! at "code needs a fence"))]
    (when-not (= fence (last lines)) (invalid-markdown! at "code needs its closing fence"))
    (cond-> {:type :code :text (str/join "\n" (butlast (drop 2 lines)))}
      (seq language)
      (assoc :language language))))

(defn- tagged-value
  "The value of one [[tagged-json]] line, or a refusal at line `at`."
  [at tag line]
  (let [prefix (str tag " ")]
    (when-not (and (string? line) (str/starts-with? line prefix))
      (invalid-markdown! at (str "expected a `" tag "` line")))
    (wire/->engine (wire/parse-json (subs line (count prefix))))))

(defmethod markdown->node :diff
  [_ {:keys [at lines]}]
  (let [[head opening & more]
        (rest lines)

        revisions
        (tagged-value at "revisions" head)

        [_ fence]
        (or (when (string? opening) (re-matches #"(`{3,})diff" opening))
            (invalid-markdown! at "a diff needs a fenced patch"))

        closing
        (or (first (keep-indexed (fn [i line]
                                   (when (= fence line) i))
                                 more))
            (invalid-markdown! at "a diff patch needs its closing fence"))

        [thread-lines preview-lines]
        (split-with #(not (str/starts-with? % "preview ")) (drop (inc (long closing)) more))

        previews
        (mapv #(tagged-value at "preview" %) preview-lines)]

    (with-meta (cond-> (merge {:type :diff :patch (str/join "\n" (take closing more))}
                              (select-keys revisions [:old-revision :new-revision]))
                 (seq thread-lines)
                 (assoc :threads (mapv #(tagged-value at "thread" %) thread-lines))

                 (seq previews)
                 (assoc :previews previews))
      {:elided (previews-elided previews)})))

(defmethod markdown->node :thread
  [_ {:keys [at lines]}]
  (merge {:type :thread :messages []} (tagged-value at "thread" (second lines))))

(defmethod markdown->node :image
  [_ {:keys [at lines]}]
  (let [facts (tagged-value at "image" (second lines))]
    (with-meta (merge {:type :image} facts) {:elided (if (:before facts) 2 1)})))

(defmethod markdown->node :media
  [_ {:keys [at lines]}]
  (let [facts (tagged-value at "media" (second lines))]
    (with-meta (merge {:type :media} facts) {:elided (count (filter :bytes [facts (:before facts)]))})))

(defmethod markdown->node :scene
  [_ {:keys [at lines]}]
  (merge {:type :scene} (tagged-value at "scene" (nth lines 2 nil))))

(defn- toned
  "The items of a tagged line, each `:tone` a keyword again."
  [items]
  (mapv #(cond-> % (:tone %) (update :tone keyword)) items))

(defmethod markdown->node :badge
  [_ {:keys [at lines]}]
  (update (merge {:type :badge :badges []} (tagged-value at "badge" (second lines))) :badges toned))

(defmethod markdown->node :list
  [_ {:keys [at lines]}]
  (update (merge {:type :list :is-ordered false :items []} (tagged-value at "list" (second lines)))
          :items
          toned))

(defmethod markdown->node :chart
  [_ {:keys [at lines]}]
  (-> (merge {:type :chart :series []} (tagged-value at "chart" (second lines)))
      (update :kind keyword)
      (update :series toned)))

(defmethod markdown->node :canvas
  [_ {:keys [at lines]}]
  (update (merge {:type :canvas :cards []} (tagged-value at "canvas" (nth lines 2 nil)))
          :cards
          (fn [cards]
            (mapv #(cond-> % (:edges %) (update :edges toned)) (toned cards)))))

(defmethod markdown->node :board
  [_ {:keys [at lines]}]
  (update (merge {:type :board :columns [] :people [] :epics [] :issues []}
                 (tagged-value at "board" (nth lines 2 nil)))
          :columns
          toned))

(defmethod markdown->node :spinner
  [_ {:keys [lines]}]
  (let [text
        (str/join "\n" (rest lines))

        working
        (re-matches #"(?s)(.*) \(working\)" text)]

    {:type :spinner
     :text (if working (second working) text)
     :is-active (boolean working)
     :variant :braille}))

(defmethod markdown->node :card
  [_ {:keys [at lines]}]
  (let [[head & more]
        (rest lines)

        _
        (when (str/blank? head) (invalid-markdown! at "a card needs a title"))

        [title tone]
        (untoned head)

        [meta-line & text-lines]
        more

        meta
        (some->> meta-line
                 (re-matches #"_(.+)_")
                 second)

        text-lines
        (if meta text-lines more)]

    (cond-> {:type :card :title title}
      tone
      (assoc :tone tone)

      meta
      (assoc :meta meta)

      (seq text-lines)
      (assoc :text (str/join "\n" text-lines)))))

(defn- markdown-domain
  "The domain keys of a `Domain:` line, or nil when `line` is not one."
  [line]
  (when-let [[_ json] (re-matches #"Domain: (\{.*\})" (str line))]
    (let [domain (try (wire/parse-json json) (catch Exception _ nil))]
      (when (map? domain)
        (into {}
              (keep
                (fn [[k wire-key]]
                  (when (contains? domain wire-key)
                    [k
                     (case k
                       :options
                       (mapv (fn [option]
                               (cond-> {:value (get option "value") :label (get option "label")}
                                  (contains? option "icon") (assoc :icon (get option "icon"))))
                             (get domain wire-key))

                       :mode
                       (get spec/datetime-mode-terms (get domain wire-key) (get domain wire-key))

                       (get domain wire-key))])))
              input-domain-keys)))))

(defn- markdown-input-value
  "The value that `text` paints for an input of `type`, or nil when it shows none."
  [{:keys [type] :as node} text]
  (cond (str/blank? text) nil
        (contains? spec/secret-types type) nil
        (= :checkbox type) (= "yes" text)
        (spec/multiple-choice? node) (str/split text #", ")
        (= :range type) (or (parse-long text) (parse-double text))
        :else text))

(doseq [input-type (vals spec/field-types)]
  (defmethod markdown->node input-type
    [_ {:keys [at lines]}]
    (let [[first-line & more] (rest lines)
          domain (when (seq more) (markdown-domain first-line))
          body (if domain more (rest lines))
          [_ label value] (or (re-matches #"(?s)(.*?): (.*)" (str/join "\n" body))
                              (re-matches #"(?s)(.*?):()" (str/join "\n" body))
                              (invalid-markdown! at "an input needs a label and a value"))
          value (markdown-input-value (assoc domain :type input-type) value)]

      (cond-> (merge {:type input-type
                      :label label
                      :is-required false
                      :is-secret (contains? spec/secret-types input-type)}
                     domain)
        (some? value)
        (assoc :value value)))))

(defmethod markdown->node :button
  [_ {:keys [at lines]}]
  (let [[_ label clicks] (or (re-matches #"(?s)(.*) \(button; ([0-9]+) activations\)"
                                         (str/join "\n" (rest lines)))
                             (invalid-markdown! at "a button needs a label and activation count"))]
    {:type :button :label label :clicks (parse-long clicks) :is-disabled false}))

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

(defmethod markdown->node :timeline
  [_ {:keys [at lines]}]
  (if (= (empty-line :timeline) (first lines))
    {:type :timeline :lanes []}
    {:type :timeline
     :lanes (addressed
              "lane"
              :label
              (mapv (fn [line]
                      (let [[body tone]
                            (untoned (bullet at line))

                            [_ text start end]
                            (or (re-matches lane-line body)
                                (invalid-markdown! at "a lane paints `label · start → end`"))

                            [_ label detail]
                            (re-matches #"(.*?) — (.*)" text)

                            epoch
                            #(.toEpochMilli (java.time.Instant/parse %))]

                        (cond-> {:label (or label text) :tone (or tone :idle) :start (epoch start)}
                          detail
                          (assoc :detail detail)

                          (not= "now" end)
                          (assoc :end (epoch end)))))
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
          (into [] (subvec body 0 (long closing)))

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
  (let [[body notes]
        (trailing-notes lines)

        behind
        (long (or (some #(counted-behind #"… (\d+) more rows.*" %) notes) 0))

        groups
        (into [] (keep note->group) notes)

        painted
        (mapv table-cells body)

        [header rule]
        painted

        _
        (when (or (nil? rule) (not= (count header) (count rule)))
          (invalid-markdown! at "a table paints a header row and a rule of the same width"))

        toned?
        (and (= "!" (first header)) (= "---" (first rule)))

        after-tone
        (fn [cells]
          (if toned? (vec (rest cells)) cells))

        parented?
        (and (= parent-marker (first (after-tone header))) (= "---" (first (after-tone rule))))

        declared
        (fn [cells]
          (let [rest-cells (after-tone cells)]
            (if parented? (vec (rest rest-cells)) rest-cells)))

        columns
        (addressed "column"
                   :label
                   (mapv (fn [label align]
                           (cond-> {:label label}
                             (str/ends-with? align ":")
                             (assoc :align :right)))
                         (declared header)
                         (declared rule)))

        rows
        (addressed
          "row"
          (fn [row]
            (first (:cells row)))
          (mapv (fn [cells]
                  (let [tone
                        (when (and toned? (not (str/blank? (first cells)))) (keyword (first cells)))

                        parent
                        (let [cell (first (after-tone cells))]
                          (when (and parented? (not (str/blank? cell))) cell))

                        painted-cells
                        (declared cells)]

                    (when (> (count painted-cells) (count columns))
                      (invalid-markdown! at
                                         (str "a row paints more cells than the table declares: "
                                              (count painted-cells)
                                              " against " (count columns))))
                    (cond-> {:cells painted-cells}
                      (tone? tone)
                      (assoc :tone tone)

                      parent
                      (assoc :parent parent))))
                (drop 2 painted)))]

    (with-meta (cond-> {:type :table
                        :columns columns
                        :rows rows
                        :max-rows (long (:max-rows spec/table-defaults))
                        :order :insertion}
                 (seq groups)
                 (assoc :groups groups))
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
                     (str/split (str markdown) #"\n" -1))

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
