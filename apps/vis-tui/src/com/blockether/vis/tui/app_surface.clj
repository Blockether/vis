(ns com.blockether.vis.tui.app-surface
  "The full surface of one app attachment: its diff with threads under their
   lines, thread nodes, the other components painted by the live view, the Save
   and Send states and the Send receipts.

   The rows and the keys are pure. Gateway calls run on workers through the
   effect functions of [[component]], so tests record them instead. The view
   comes from the session stream (`:app-views` of the tab) or from a full read
   of `GET .../views/live/:view_id`; the newer copy wins."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.app :as app]
            [com.blockether.vis.tui.app-log :as app-log]
            [com.blockether.vis.tui.app-model :as model]
            [com.blockether.vis.tui.app-surfaces :as app-surfaces]
            [com.blockether.vis.tui.canvas-map :as cm]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.columns :as columns]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.frame :as frame]
            [com.blockether.vis.tui.input :as input]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.live-view :as lv]
             [com.blockether.vis.tui.media :as media]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.state :as state]
            [com.blockether.vis.tui.theme :as t]
            [com.blockether.vis.tui.view-model :as view-model]
            [taoensso.telemere :as tel])
  (:import [com.googlecode.lanterna TerminalPosition TerminalSize]
           [com.googlecode.lanterna.graphics TextGraphics]
           [com.googlecode.lanterna.gui2 ScrollBar]
           [com.googlecode.lanterna.input KeyStroke KeyType MouseAction MouseActionType]
           [com.googlecode.lanterna.screen TerminalScreen]))

(set! *warn-on-reflection* true)

(set! *unchecked-math* :warn-on-boxed)

;;; ── Rows ───────────────────────────────────────────────────────────────────

(defn- holds-threads?
  [node]
  (some (comp app/thread-types :type) (tree-seq #(seq (:children %)) :children node)))

(defn- pressable-entry?
  "True when Enter on plan `entry` does something: an enabled button, a selectable
   card or table row, or the first row of an enabled field."
  [entry first-field-row?]
  (case (:kind entry)
    :button
    (not (:is-disabled entry))

    (:card :trow)
    (boolean (:is-selectable entry))

    :board-issue
    (boolean (or (:is-selectable entry) (:is-editable entry)))

    (:picture-open :media-open :disclosure)
    true

    :field
    (boolean
      (and first-field-row? (not (:is-disabled entry)) (not= :blank (get-in entry [:field :kind]))))

    false))

(defn- cell-entries
  "The cells of a plan `entry` that lays nodes side by side, or nil."
  [entry]
  (when (= :columns (:kind entry)) (:cells entry)))

(defn- pressable-parts
  "The indexes of the pressable parts of one plan row and the field ids seen
   after it. A row of side-by-side cells has one part for each cell."
  [parts seen]
  (reduce (fn [[found seen] [i part]]
            (let [field?
                  (and (= :field (:kind part)) (not= :blank (get-in part [:field :kind])))

                  first-field-row?
                  (and field? (not (contains? seen (:node-id part))))]

              [(cond-> found
                 (and part (pressable-entry? part first-field-row?))
                 (conj i))
               (cond-> seen
                 field?
                 (conj (:node-id part)))]))
          [[] seen]
          (map-indexed vector parts)))

(defn- plan-rows
  "Rows of a node that the live view paints: one `:plan` row for each plan entry.
   The cursor stops on each control, and on one row of each field. A row of
   side-by-side cells has one stop for each control in it, in `:cells`."
  [view node width disclosures]
  (first
    (reduce (fn [[rows seen] entry]
              (let [cells
                    (cell-entries entry)

                    [found seen]
                    (pressable-parts (or cells [entry]) seen)]

                [(conj rows
                       (cond-> {:kind :plan
                                :node-id (:node-id entry)
                                :item-id (:item-id entry)
                                 :thread-id (:canvas-thread-id entry)
                                 :entry entry
                                :text (:text entry)
                                :is-selectable (boolean (seq found))}
                         cells
                         (assoc :cells found))) seen]))
            [[] #{}]
            (lv/layout-plan (cond-> (assoc (lv/opened (assoc view :nodes [node] :description nil))
                                    :disclosures disclosures)
                       (= :canvas (:type node))
                       (lv/expanded (:id node)))
                     width))))

(defn- canvas-rows
  [view node width disclosures]
  (into []
        (mapcat (fn [[head & more :as rows]]
                  (if-let [item-id (:item-id head)]
                    (let [selectable? (:is-selectable node)
                          head (assoc head :kind (if selectable? :canvas-select :canvas-comment)
                                           :is-selectable true)
                          comment {:kind :canvas-comment
                                   :node-id (:id node) :item-id item-id
                                   :text "  New thread" :is-selectable true}]
                      (concat (if selectable? [head comment] [head])
                              (map #(cond-> % (:thread-id %) (assoc :is-selectable true)) more)))
                    rows)))
        (partition-by :item-id (plan-rows view node width disclosures))))

(defn- preview-key
  [{:keys [diff-id path preview]}]
  [:picture diff-id path (:id preview)])

(defn- diff-row-plans
  [view width folded disclosures rows]
  (mapcat (fn [{:keys [kind node-id path node] :as row}]
            (if (= :diff-preview kind)
              (let [owner {:diff-id node-id :path path :preview node}
                    planned (cond-> node
                              (= :image (:type node))
                              (assoc :is-flipped (contains? folded (preview-key owner))))]
                (into []
                      (comp (remove #(= :node (get-in % [:entry :kind])))
                            (map #(merge % owner)))
                      (plan-rows view planned width disclosures)))
              [row]))
          rows))

(defn- node-rows
  [view node width folded opts]
  (cond (:is-dialog node) []
        (= :diff (:type node))
        ;; Toolbar buttons that no key covers stand above the diff as rows.
        (into (into [] (mapcat #(plan-rows view % width (:disclosures opts))) (:children node))
              (diff-row-plans view width folded (:disclosures opts)
                              (model/diff-rows (dissoc node :children) width folded opts)))
        (= :thread (:type node))
        (into [{:kind :thread-node
                :node-id (:id node)
                :thread-id (:id node)
                :text (or (:label node) "Discussion")
                :is-selectable true}]
              (model/thread-rows node width {:node-id (:id node) :thread-id (:id node)}))
        (and (seq (:children node)) (holds-threads? node))
        (let [disclosures (:disclosures opts)
              header (plan-rows view (dissoc node :children) width disclosures)
              open? (or (not (:is-collapsible node))
                        (get disclosures (:id node) (:default-expanded node)))]
          (cond-> header
            open? (into (mapcat #(node-rows view % width folded opts)) (:children node))))
        (= :canvas (:type node)) (canvas-rows view node width (:disclosures opts))
        (= :image (:type node))
        (plan-rows view (assoc node :is-flipped (contains? folded [:picture (:id node)]))
                   width (:disclosures opts))
        :else (plan-rows view node width (:disclosures opts))))

(def ^:private surface-letters
  "The letters that the surface gives to its own verbs. An automatic key takes none
   of them, so no verb moves behind C-x without a decision of the app."
  (into #{} (map str) "uwftiacvsrl"))

(defn- choice-strip?
  "True when group `node` holds only choice buttons: a strip of tabs."
  [node]
  (let [children (:children node)]
    (and (= :group (:type node))
         (< 1 (count children))
         (every? #(and (= :button (:type %)) (some? (:is-selected %))) children))))

(defn- buttons-in
  "The buttons of `nodes` in reading order. A button of a strip of tabs has `::tab?`."
  [nodes]
  (mapcat (fn [node]
            (cond (or (:is-dialog node) (lv/toolbar? node)) []
                  (= :button (:type node)) [node]
                  (choice-strip? node) (map #(assoc % ::tab? true) (:children node))
                  :else (buttons-in (:children node))))
          nodes))

(defn- letter-key
  "The key of an action `label` that `taken` does not hold: its first letter in lower
   case, then in upper case, then each next letter. Nil when no letter is free."
  [label taken]
  (let [letters
        (filter #(and (< (int %) 128) (Character/isLetterOrDigit (char %))) (str label))

        candidates
        (when-let [c (first letters)]
          (distinct (concat [(str/lower-case c) (str/upper-case c)]
                            (map #(str/lower-case %) (rest letters)))))]

    (some #(when-not (contains? taken %) %) candidates)))

(defn- dialog-nodes [nodes]
  (mapcat (fn [node]
            (if (:is-dialog node)
              [node]
              (dialog-nodes (:children node))))
          nodes))

(defn- without-dialog-shortcuts [view]
  (if (seq (:shortcuts view))
    (let [dialogs (dialog-nodes (:nodes view))
          ids (into #{} (comp (mapcat #(tree-seq (comp seq :children) :children %)) (map :id)) dialogs)]
      (update view :shortcuts #(into [] (remove (comp ids :node-id)) %)))
    view))

(defn auto-keyed
  "`view` with a key for each button, when the app declares no shortcut of its own:
   in a terminal a key is better than a button row. A strip of tabs takes the digits
   from 1, and an action takes a letter of its label. An app that declares shortcuts
   keeps full control of its keys. A disabled button gets no shortcut, so it keeps
   its row and still shows that it exists."
  [view]
  (if (or (seq (:shortcuts view)) (model/read-only? view))
    view
    (let [buttons
          (buttons-in (:nodes view))

          tab-keys
          (into {}
                (map-indexed (fn [i button]
                               [(:id button) (str (inc (long i)))]))
                (take 9 (filter ::tab? buttons)))

          ;; Each button takes part, also a disabled one: a key stays with its button
          ;; when another button turns on or off.
          [_ keys]
          (reduce (fn [[taken keys] button]
                    (if-let [k (letter-key (:label button) taken)]
                      [(conj taken k) (assoc keys (:id button) k)]
                      [taken keys]))
                  [(into surface-letters (vals tab-keys)) tab-keys]
                  (remove ::tab? buttons))

          shortcuts
          (into []
                (keep (fn [button]
                        (when-let [k (and (not (:is-disabled button)) (get keys (:id button)))]
                          {:key k :node-id (:id button)})))
                buttons)]

      (cond-> view
        (seq shortcuts)
        (assoc :shortcuts
          shortcuts :auto-keys?
          true)))))

(defn- keyed-buttons
  "Remove buttons named by shortcuts before layout, including nested controls."
  [nodes covered]
  (into []
        (comp (remove #(and (= :button (:type %)) (contains? covered (:id %))))
              (keep (fn [node]
                      (if (and (seq (:children node)) (not (lv/toolbar? node)))
                        (let [children (keyed-buttons (:children node) covered)]
                          (cond (seq children) (assoc node :children children)
                                ;; A diff toolbar is optional; the diff stays without it.
                                (= :diff (:type node)) (dissoc node :children)
                                :else nil))
                        node))))
        nodes))

(defn surface-rows
  "Every row of the app surface for `view`, `width` columns wide. `folded` holds
   the diff file paths whose lines are hidden. `opts` holds the diff `:unified?`
   and `:wrap?` choices."
  ([view width folded] (surface-rows view width folded nil))
  ([view width folded opts]
   (let [receipts
         (model/receipt-rows view)

         view
         (update view :nodes keyed-buttons (set (keep :node-id (:shortcuts view))))]

     (cond-> (into []
                   (comp (map #(seq (node-rows view % width folded opts)))
                         (filter some?)
                         (interpose [{:kind :section-gap :text ""}])
                         cat)
                   ;; The first top-level toolbar stands at the top, as in the app band.
                   (let [nodes (:nodes view)
                         band (first (filter #(and (lv/toolbar? %) (not (:is-dialog %))) nodes))]
                     (if band (cons band (remove #(identical? band %) nodes)) nodes)))
       (seq receipts)
       (into (cons {:kind :receipts-head :text "Send receipts"} receipts))))))

(defn selectable-indexes
  "The row indexes that the cursor stops on."
  [rows]
  (into []
        (keep-indexed (fn [i row]
                        (when (:is-selectable row) i)))
        rows))

(defn- file-row-index
  "The index of the diff file row that row `i` belongs to, or nil."
  [rows i]
  (when-let [path (:path (nth rows i nil))]
    (some (fn [j]
            (let [row (nth rows j)]
              (when (and (= :file (:kind row)) (= path (:path row))) j)))
          (range i -1 -1))))

(defn- thread-of-anchor
  [node anchor]
  (some #(when (= (model/anchor-key anchor)
                  (some-> (:anchor %)
                          model/anchor-key))
           %)
        (app/threads node)))

(defn editor-target
  "The thread that Enter on `row` edits: `{:node-id :thread-id :anchor :label}`,
   or nil. A diff line with a thread edits that thread; else it starts one."
  [view row]
  (let [node (model/find-node view (:node-id row))]
    (cond (and (= :canvas (:type node)) (not (:is-transient view)) (:is-selectable row) (:item-id row)
               (not= :canvas-select (:kind row)))
          (cond-> {:node-id (:id node) :item-id (:item-id row)}
            (:thread-id row)
            (assoc :thread-id (:thread-id row) :draft (:draft (app/find-thread node (:thread-id row)))))
          (#{:thread-head :thread-node} (:kind row))
          {:node-id (:node-id row)
           :thread-id (:thread-id row)
           :anchor (:anchor row)
           :draft (:draft (app/find-thread node (:thread-id row)))}
          (and (:anchor row) (= :diff (:type node)))
          (let [thread (thread-of-anchor node (:anchor row))]
            (cond-> {:node-id (:node-id row) :anchor (:anchor row)}
              thread
              (assoc :thread-id
                (:id thread) :draft
                (:draft thread)))))))

(defn target-label
  "What the editor title says about its target."
  [{:keys [anchor]}]
  (if anchor
    (str "Comment on " (:path anchor) ":" (:line anchor) " (" (:side anchor) ")")
    "Comment"))

;;; ── Keys ───────────────────────────────────────────────────────────────────

(defn initial-state
  "The surface state before the first paint."
  []
  {:selected 0
   :scroll 0
   :hscroll 0
   :folded #{}
   :unified? true
   :wrap? true
   :files? false
   :hide-tests? false
   :hide-whitespace? false
   :editor nil
   :save nil
   :send nil})

(defn stops
  "The cursor stops of `rows` in order: `[row-index cell-index]`. The cell index
   is nil for a row that is one control."
  [rows]
  (into []
        (mapcat (fn [i]
                  (if-let [cells (seq (:cells (nth rows i)))]
                    (map #(vector i %) cells)
                    [[i nil]])))
        (selectable-indexes rows)))

(defn focused-entry
  "The plan entry that the cursor is on in `row`: the cell `cell` of a row of
   side-by-side cells, else the entry of the row."
  [row cell]
  (if (and (some? cell) (seq (:cells row)))
    (nth (get-in row [:entry :cells]) cell nil)
    (:entry row)))

(defn- move
  [{:keys [rows]} state delta]
  (let [all
        (stops rows)

        at
        (or (first (keep-indexed (fn [k [i c]]
                                   (when (and (= i (:selected state)) (= c (:cell state))) k))
                                 all))
            (first (keep-indexed (fn [k [i _]]
                                   (when (= i (:selected state)) k))
                                 all))
            0)

        k
        (p/clamp (+ (long at) (long delta)) 0 (max 0 (dec (count all))))

        [i c]
        (nth all k [(:selected state) (:cell state)])]

    (assoc state
      :selected i
      :cell c)))

(defn- fold
  "← folds the file row that the cursor is on, or the file of the row it is in."
  [{:keys [rows]} state]
  (if-let [i (file-row-index rows (:selected state))]
    (-> state
        (update :folded conj (:path (nth rows i)))
        (assoc :selected i))
    state))

(defn- unfold
  [{:keys [rows]} state]
  (let [row (nth rows (:selected state) nil)]
    (if (= :file (:kind row)) (update state :folded disj (:path row)) state)))

(defn open-editor
  "State with the editor open on `target`, its saved draft in the field."
  [state target]
  (assoc state
    :editor {:target target
             :input (if (:draft target)
                      (input/paste-text (input/empty-input) (:draft target))
                      (input/empty-input))}))

(def ^:private canvas-item-gone-notice "The canvas item is gone. The comment stays here.")

(defn- canvas-item-gone?
  [view {:keys [node-id item-id]}]
  (and view item-id
       (not-any? #(= item-id (:id %)) (:cards (model/find-node view node-id)))))

(defn- target-thread
  [view {:keys [node-id thread-id]}]
  (when thread-id
    (app/find-thread (model/find-node view node-id) thread-id)))

(defn save-request
  "What Save does with the editor: `{:action a}` for the gateway, `{:skip reason}`
   when there is no draft to save, `{:blocked reason}` when the draft
   must stay here, or nil without an editor."
  [view {:keys [target input remove? base-seq]}]
  (when target
    (let [view
          (cond-> view
            (some? base-seq)
            (assoc :seq base-seq))

          ^String body
          (input/input->text input)

          blank?
          (app/blank-text? body)

          messages?
          (seq (:messages (target-thread view target)))

          limit
          (long (:canvas-draft app/bounds))]

      (cond (canvas-item-gone? view target) {:blocked canvas-item-gone-notice}
            remove? (when (string? (:draft target)) {:action (model/draft-action view target nil)})
            (and (not blank?) (:item-id target) (> (.codePointCount body 0 (.length body)) limit))
            {:blocked (str "Use at most " limit " characters in a canvas draft")}
            (and blank? (not messages?) (nil? (:draft target)))
            {:skip "Nothing saved: the comment is empty"}
            :else {:action
                   (model/draft-action view target (if blank? (when messages? "") body))}))))

(defn send-key
  "The idempotency key of the next Send: the key of a Send that did not get a
   receipt, else a new one from `fresh`."
  [send fresh]
  (if (and (:key send) (not= :sent (:state send))) (:key send) (fresh)))

(defn- save-editor
  [state view {:keys [save! reload!]} remove?]
  (if (= :saving (get-in state [:save :state]))
    state
    (let [retry-seq
          (get-in state [:editor :retry-seq])

          editor
          (cond-> (:editor state)
            retry-seq
            (assoc :base-seq (:seq view)))

          {:keys [action skip blocked]}
          (if (and retry-seq (< (long (or (:seq view) 0)) (long retry-seq)))
            (do (when reload! (reload!)) {:blocked "Wait for the app to reload, then save again"})
            (save-request view (assoc editor :remove? remove?)))]

      (cond blocked (assoc state :notice blocked)
            skip (assoc state
                   :editor nil
                   :save {:state :skipped :message skip})
            action (do (save! action editor)
                       (assoc (dissoc state :notice)
                         :editor editor
                         :save {:state :saving :target (:target editor)}))
            :else state))))

(defn- save-shortcut?
  [^KeyStroke key]
  (and (.isCtrlDown key)
       (or (= KeyType/Enter (.getKeyType key))
           (and (= KeyType/Character (.getKeyType key)) (#{\s \S} (.getCharacter key))))))

(defn- edit-input-key
  [in ^KeyStroke key]
  (if (= KeyType/Character (.getKeyType key))
    (if (or (.isCtrlDown key) (.isAltDown key)) in (input/insert-char in (.getCharacter key)))
    ((get {KeyType/Enter input/insert-newline
           KeyType/Backspace input/delete-backward
           KeyType/Delete input/delete-forward
           KeyType/ArrowLeft input/move-left
           KeyType/ArrowRight input/move-right
           KeyType/ArrowUp input/move-up
           KeyType/ArrowDown input/move-down
           KeyType/Home input/move-line-start
           KeyType/End input/move-line-end}
          (.getKeyType key)
          identity)
      in)))

(defn- editor-key
  [state ^KeyStroke key view effects]
  (cond (= KeyType/Escape (.getKeyType key)) (assoc state :editor nil)
        (and (.isCtrlDown key) (#{\d \D} (.getCharacter key))) (save-editor state view effects true)
        (save-shortcut? key) (let [{:keys [target input]} (:editor state)]
                               (if (and (= KeyType/Enter (.getKeyType key))
                                        (app/blank-text? (input/input->text input))
                                        (not (seq (:messages (target-thread view target)))))
                                 state
                                 (save-editor state view effects false)))
        :else (update-in state [:editor :input] edit-input-key key)))

(defn- input-values
  "The value of every input of `view`: what a Send carries."
  [view]
  (lv/input-values {:view view}))

(defn- send-app
  "Send the app with idempotency key reuse; `node-id` names the submit button."
  [state view node-id {:keys [send!]}]
  (let [k (send-key (:send state) model/idempotency-key)]
    (send! (model/submit-action view k (input-values view)) node-id)
    (assoc state :send {:state :sending :key k})))

(def disconnected-notice
  "What the status line says when a control of an app without its owner is used."
  "Disconnected: the app waits for its owner")

(def connecting-notice
  "What the status line says when a control is used while the owner of the app starts."
  "Connecting: the app waits for its owner")

(defn- waiting-notice [view] (if (model/away? view) connecting-notice disconnected-notice))

(defn- owner-control
  [state view node item-id {:keys [activate! select! edit! reveal!]}]
  (let [node-id (:id node)]
    (cond (#{:button :card} (:type node)) (do (activate! {:action :activate :node-id node-id})
                                              (when reveal! (reveal! view node))
                                              state)
          (and item-id (= :board (:type node)) (:is-selectable node))
          (do (select! {:action :select :node-id node-id
                        :item-ids (if (= item-id (first (:selected-ids node))) [] [item-id])}) state)
          (and item-id (#{:table :canvas} (:type node)))
          (do (select! {:action :select :node-id node-id :item-ids [item-id]}) state)
          (lv/input-node {:view view} node-id) (do (edit! node) state)
          :else state)))

(defn- button-refusal [node] (and (= :button (:type node)) (:is-disabled node)))

(defn- submit-button? [node] (and (= :button (:type node)) (= :submit (keyword (:action node)))))

(defn- press
  "Press a control, preserving disabled, submit, and disconnected precedence."
  [state view node-id item-id {:keys [inspect!] :as effects}]
  (let [node (model/find-node view node-id)]
    (cond (nil? node) (assoc state :notice (str "The app has no control " node-id))
          (= :image (:type node)) (do (inspect! node) state)
          (button-refusal node) (assoc state :notice "That button is disabled")
          (submit-button? node) (send-app state view node-id effects)
          (model/stale? view) (assoc state :notice (waiting-notice view))
          :else (owner-control state view node item-id effects))))

(def ^:private named-shortcut-keys
  "The named keys that an app shortcut can bind, by Lanterna key type."
  {KeyType/Enter "Enter"
   KeyType/Backspace "Backspace"
   KeyType/Delete "Delete"
   KeyType/ArrowUp "ArrowUp"
   KeyType/ArrowDown "ArrowDown"
   KeyType/ArrowLeft "ArrowLeft"
   KeyType/ArrowRight "ArrowRight"
   KeyType/Home "Home"
   KeyType/End "End"
   KeyType/PageUp "PageUp"
   KeyType/PageDown "PageDown"})

(defn shortcut-key
  "The shortcut key name of `key`: one printable ASCII character, `Space` or a
   named key. Nil for Escape, Tab and every key with Ctrl or Alt."
  [^KeyStroke key]
  (when-not (or (.isCtrlDown key) (.isAltDown key))
    (let [kt (.getKeyType key)]
      (if (= KeyType/Character kt)
        (let [c (.getCharacter key)
              n (long (int (.charValue c)))]

          (cond (= \space c) "Space"
                (<= 33 n 126) (str c)))
        (get named-shortcut-keys kt)))))

(defn shortcut-for
  "The shortcut of `view` bound to key name `k`, or nil. A closed app has none."
  [view k]
  (when (and k (not (model/read-only? view))) (some #(when (= k (:key %)) %) (:shortcuts view))))

(def ^:private key-glyphs
  {"Space" "Space"
   "Enter" "Enter"
   "Backspace" "Bksp"
   "Delete" "Del"
   "ArrowUp" "↑"
   "ArrowDown" "↓"
   "ArrowLeft" "←"
   "ArrowRight" "→"
   "Home" "Home"
   "End" "End"
   "PageUp" "PgUp"
   "PageDown" "PgDn"})

(defn shortcut-label
  "What the legend says that shortcut `s` does."
  ([s] (shortcut-label nil s))
  ([view s]
   (or (not-empty (str (:label s)))
       (some-> (model/find-node view (:node-id s))
               :label
               not-empty)
       (:action s)
       (:node-id s))))

(defn- merged-pairs
  "`[key label]` pairs with one pair for each label, in first-seen order. Keys of
   the same label join with `/`, so `← Left` and `h Left` read `←/h Left`."
  [pairs]
  (let [order
        (distinct (map second pairs))

        keys-of
        (reduce (fn [m [k label]]
                  (update m label (fnil conj []) k))
                {}
                pairs)]

    (mapv (fn [label]
            [(str/join "/" (distinct (keys-of label))) label])
          order)))

(defn shortcut-legend
  "The legend of the shortcuts of `view`, or nil: `[key label]` pairs, or one dim
   line that says the app waits for its owner."
  [view]
  (when-not (model/read-only? view)
    (let [enabled
          (remove #(some-> (model/find-node view (:node-id %))
                           button-refusal)
            (:shortcuts view))

          ;; The choice that holds now (`is_selected`) keeps its key, marked like a
          ;; checked menu row.
          pairs
          (merged-pairs (mapv (fn [s]
                                [(get key-glyphs (:key s) (:key s))
                                 (cond->> (shortcut-label view s)
                                   (:is-selected (model/find-node view (:node-id s)))
                                   (str "• "))])
                              enabled))]

      (if (model/stale? view)
        (str (if (model/away? view) "Connecting" "Disconnected")
             ": the controls wait for the owner of the app"
             (when (seq pairs) (str "  ·  " (str/join "  ·  " (map #(str/join " " %) pairs)))))
        (not-empty pairs)))))

(defn- press-shortcut
  [state view s {:keys [shortcut!] :as effects}]
  (cond (model/stale? view) (assoc state :notice (waiting-notice view))
        (:node-id s) (press state view (:node-id s) nil effects)
        :else (do (shortcut! {:action :shortcut :key (:key s)} s)
                  (assoc state :notice (str (shortcut-label view s) " …")))))

(defn compare-image
  "The image node with an old picture that `row` shows, or nil."
  [view row]
  (when-let [node (or (:preview row) (some->> (:node-id row) (model/find-node view)))]
    (when (and (= :image (:type node)) (:before node)) node)))

(defn diff-view?
  "True when `rows` show diff code, so the Split and Wrap keys apply."
  [rows]
  (boolean (some #(#{:add :del :ctx :split} (:kind %)) rows)))

(defn- file-row-of
  "The index of the file header for path, or nil."
  [rows path]
  (first (keep-indexed (fn [i row]
                         (when (= [:file path] ((juxt :kind :path) row)) i))
                       rows)))

(defn- toggle-fold [state k] (update state :folded #(if (contains? % k) (disj % k) (conj % k))))

(defn- selected-row
  [state rows]
  (let [row (nth rows (:selected state) nil)]
    (if (and (= :split (:kind row)) (:diff-side state))
      (merge (dissoc row :anchor) (get row (:diff-side state)))
      row)))

(defn- enter-row
  [state {:keys [view rows]} {:keys [inspect! media!] :as effects}]
  (let [row
        (selected-row state rows)

        entry
        (focused-entry row (:cell state))]

    (case (:kind row)
      :file-item
      (if-let [i (file-row-of rows (:path row))]
        (-> state
            (update :folded disj (:path row))
            (assoc :selected i
                   :cell nil))
        state)

      :file
      (toggle-fold state (:path row))

      (cond (= :disclosure (:kind entry))
            (let [id
                  (:node-id entry)

                  node
                  (model/find-node view id)]

              (assoc-in state
                [:disclosures id]
                (not (get (:disclosures state) id (:default-expanded node)))))
            (= :media-open (:kind entry))
            (do ((or media! media/open!)
                 (or (:preview row) (model/find-node view (:node-id entry))) (:item-id entry))
                state)

            (and (= :picture-open (:kind entry)) (:preview row))
            (do (inspect! (:preview row)) state)

            (model/read-only? view) (assoc state :save {:state :closed})
            :else (if-let [target (editor-target view row)]
                    (assoc-in (open-editor state target) [:editor :base-seq] (:seq view))
                    (if (and entry (:is-selectable row))
                      (press state view (:node-id entry) (:item-id entry) effects)
                      state))))))

(defn- horizontal-key
  [state delta]
  (if (:wrap? state) state (update state :hscroll #(max 0 (+ (long %) (long delta))))))

(defn- diff-mode-key
  [state {:keys [rows]} mode]
  (if (diff-view? rows)
    (cond-> (update state mode not)
      (= :wrap? mode)
      (assoc :hscroll 0))
    state))

(defn- compare-key
  [state {:keys [view rows]}]
  (let [row (nth rows (:selected state) nil)]
    (if (compare-image view row)
      (toggle-fold state (if (:preview row) (preview-key row) [:picture (:node-id row)]))
      state)))

(defn- send-key-action
  "Send the saved comments with a new turn of the session, or else Send the app."
  [state {:keys [view]} effects]
  (cond (model/read-only? view) (assoc state :save {:state :closed})
        (and (:comment! effects) (seq (app/drafts (:nodes view))))
        (do ((:comment! effects)) (assoc state :notice "Sending the comments to the session"))
        :else (send-app state view nil effects)))

(defn- fold-all-files
  [state rows]
  (let [paths (set (keep #(when (= :file (:kind %)) (:path %)) rows))]
    (assoc state :folded (if (every? (:folded state) paths) #{} paths))))

(defn- copy-path
  [state rows copy!]
  (when-let [path (:path (nth rows (:selected state) nil))] (copy! path))
  state)

(defn- find-code
  [state rows text after & [direction]]
  (if-let [i (model/find-code rows text after direction)]
    (assoc state
      :selected i
      :notice (str "Find: " text))
    (assoc state :notice (str "Find: " text (when-not (str/blank? text) " · No matches")))))

(defn- search-key
  [state ^KeyStroke key rows]
  (let [kt
        (.getKeyType key)

        text
        (get-in state [:search :text] "")]

    (cond (= KeyType/Escape kt) (dissoc state :search :notice)
          (= KeyType/Enter kt)
          (find-code state rows text (:selected state) (if (.isShiftDown key) :previous :next))
          :else (let [text (cond (and (= KeyType/Character kt)
                                      (not (.isCtrlDown key))
                                      (not (.isAltDown key)))
                                 (str text (.getCharacter key))
                                 (= KeyType/Backspace kt) (subs text 0 (max 0 (dec (count text))))
                                 :else text)]
                  (find-code (assoc-in state [:search :text] text) rows text -1)))))

(declare open-key-menu menu-key)

(defn- character-key
  [state ch geom effects]
  (let [actions {\? #(open-key-menu state geom)
                 \< #(horizontal-key state -8)
                 \> #(horizontal-key state 8)
                 \u #(diff-mode-key state geom :unified?)
                 \w #(diff-mode-key state geom :wrap?)
                 \/ #(assoc state
                       :search {:text ""}
                       :notice "Find: ")
                 \f #(update state :files? not)
                 \t #(update state :hide-tests? not)
                 \i #(update state :hide-whitespace? not)
                 \a #(fold-all-files state (:rows geom))
                 \c #(copy-path state (:rows geom) (:copy! effects))
                 \v #(compare-key state geom)
                 \s #(send-key-action state geom effects)
                 \r #(do ((:reload! effects)) state)
                 \l #(do (when (seq (:log (:view geom))) ((:log! effects))) state)}]
    ((get actions ch (constantly state)))))

(defn- tab-row
  [state rows]
  (if (= :split (:kind (nth rows (:selected state) nil)))
    (update state :diff-side #(if (= :left %) :right :left))
    state))

(defn- move-board-issue
  [state ^KeyStroke key {:keys [view rows]} {:keys [move!]}]
  (when (and (.isShiftDown key) (not (.isCtrlDown key)) (not (.isAltDown key)))
    (when-let [direction (get {KeyType/ArrowLeft -1 KeyType/ArrowRight 1} (.getKeyType key))]
      (let [entry (focused-entry (selected-row state rows) (:cell state))]
        (when (= :board-issue (:kind entry))
          (let [node (model/find-node view (:node-id entry))]
            (cond (model/read-only? view) state
                  (model/stale? view) (assoc state :notice (waiting-notice view))
                  :else (do (when-let [action (lv/board-move-action node (:item-id entry) direction)]
                              (move! action))
                            state))))))))

(defn- row-key
  [state kt key geom effects]
  (let [actions {KeyType/Escape (constantly {::dlg/done nil})
                 KeyType/ArrowLeft #(fold geom state)
                 KeyType/ArrowRight #(unfold geom state)
                 KeyType/Tab #(tab-row state (:rows geom))
                 KeyType/Enter #(enter-row state geom effects)
                 KeyType/Character
                 #(character-key state (.getCharacter ^KeyStroke key) geom effects)}]
    ((get actions kt (constantly state)))))

(defn- read-rows
  "Scroll the list without moving its control cursor."
  [state {:keys [rows list-h]} delta]
  (assoc state
    :reading? true
    :scroll (p/clamp (+ (long (:scroll state)) (long delta)) 0
                     (max 0 (- (count rows) (long list-h))))))

(defn- cursor-in-view?
  [state geom]
  (let [scroll (long (:scroll state))]
    (<= scroll (long (:selected state)) (dec (+ scroll (long (:list-h geom)))))))

(defn- surface-key
  "The surface's own meaning of key, without app shortcuts."
  [state ^KeyStroke key {:keys [rows list-h canvas?] :as geom} effects]
  (let [kt
        (.getKeyType key)

        steps
        {KeyType/ArrowUp -1
         KeyType/ArrowDown 1
         KeyType/PageUp (- (max 1 (long list-h)))
         KeyType/PageDown (max 1 (long list-h))
         KeyType/Home (- (count rows))
         KeyType/End (count rows)}]

    (cond (.isCtrlDown key) state
          (and canvas? (#{KeyType/PageUp KeyType/PageDown KeyType/Home KeyType/End} kt))
          (read-rows state geom (get steps kt))
          (and (:reading? state) (= KeyType/Enter kt) (not (cursor-in-view? state geom))) state
          (contains? steps kt) (dissoc (move geom state (get steps kt)) :reading?)
          :else (row-key state kt key geom effects))))

(defn log-chip
  "Where the log chip of `view` sits on the top border of box `bounds`, or nil for an
   empty log: left of the close control of a dialog or of a side pane."
  [{:keys [left top right]} view]
  (when-let [label (app-log/chip-label (:log view))]
    (let [w (long (p/display-width label))
          col (- (long right) 4 w)]

      (when (> col (+ (long left) 8)) {:row (long top) :col col :width w :label label}))))

(defn log-chip-click?
  "True when mouse `key` releases a click on `chip`."
  [key {:keys [row col width] :as chip}]
  (boolean (when (and chip
                      (instance? MouseAction key)
                      (= MouseActionType/CLICK_RELEASE (.getActionType ^MouseAction key)))
             (let [^TerminalPosition at
                   (.getPosition ^MouseAction key)

                   x
                   (.getColumn at)

                   y
                   (.getRow at)]

               (and (= y (long row)) (<= (long col) x) (< x (+ (long col) (long width))))))))

(defn- map-node [state geom]
  (when-let [id (get-in state [:canvas-map :id])]
    (some #(when (= id (:id %)) %) (:canvases geom))))

(defn- map-viewport [{:keys [bounds list-h]}]
  [(max 1 (long (:inner-w bounds))) (max 1 (dec (long list-h)))])

(defn- map-choice [state {:keys [canvases rows]}]
  (let [id (:node-id (nth rows (:selected state) nil))]
    (or (some #(when (= id (:id %)) %) canvases) (first canvases))))

(defn- toggle-map [state geom]
  (cond (:canvas-map state) (dissoc state :canvas-map)
        :else (if-let [node (map-choice state geom)]
                (let [item (:item-id (nth (:rows geom) (:selected state) nil))
                      focus (when (and (nil? (:focus node)) item) item)
                      viewport (map-viewport geom)
                      now (System/currentTimeMillis)]
                  (assoc state :canvas-map (cm/reconcile {:focus focus} node viewport now)))
                state)))

(defn- map-enter [state geom node effects comment?]
  (if-let [id (get-in state [:canvas-map :focus])]
    (let [{:keys [view items]} geom
          [index] (get items [(:id node) id])
          comment? (or comment? (not (:is-selectable node)))
          thread (when comment? (first (filter #(= id (:item-id %)) (:threads node))))]
      (cond (and (:is-locked node)
                 (let [camera (:canvas-map state)
                       viewport (map-viewport geom)
                       plan (cm/plan-for node (:level camera) viewport)]
                   (not (cm/card-visible? plan camera viewport id (System/currentTimeMillis))))) state
            (and (:is-transient view) comment?) state
            (model/read-only? view) (assoc state :save {:state :closed})
            comment? (let [target (editor-target view (cond-> {:node-id (:id node) :item-id id
                                                               :kind :canvas-comment :is-selectable true}
                                                        thread (assoc :thread-id (:id thread))))]
                       (assoc-in (open-editor state target) [:editor :base-seq] (:seq view)))
            index (enter-row (assoc state :selected index :cell nil) geom effects)
            :else (press state view (:id node) id effects)))
    state))

(defn- map-key [state ^KeyStroke key geom effects]
  (when-let [node (map-node state geom)]
    (let [kt (.getKeyType key)
          shift? (.isShiftDown key)
          ch (when (and (= kt KeyType/Character) (not (.isCtrlDown key)) (not (.isAltDown key)))
               (.getCharacter key))
          [w h :as viewport] (map-viewport geom)
          now (System/currentTimeMillis)
          dx (max 1 (quot (long w) (if shift? 1 4)))
          dy (max 1 (quot (long h) (if shift? 1 4)))
          camera (:canvas-map state)
          next (cond (or (= KeyType/ArrowLeft kt) (#{\h \H} ch)) (cm/pan camera node viewport (- dx) 0 now)
                     (or (= KeyType/ArrowRight kt) (#{\l \L} ch)) (cm/pan camera node viewport dx 0 now)
                     (or (= KeyType/ArrowUp kt) (#{\k \K} ch)) (cm/pan camera node viewport 0 (- dy) now)
                     (or (= KeyType/ArrowDown kt) (#{\j \J} ch)) (cm/pan camera node viewport 0 dy now)
                     (#{\+ \=} ch) (cm/zoom camera node viewport 1 nil now)
                     (= \- ch) (cm/zoom camera node viewport -1 nil now)
                     (or (= \0 ch) (= KeyType/Home kt)) (cm/fit camera node viewport now)
                     (or (= \r ch) (= KeyType/End kt)) (cm/reset-camera camera node viewport now)
                     (= KeyType/Tab kt) (cm/tab-card camera node viewport (if shift? -1 1) now)
                     (= KeyType/ReverseTab kt) (cm/tab-card camera node viewport -1 now)
                     :else nil)]
      (cond next (assoc state :canvas-map next)
            (= KeyType/Enter kt) (map-enter state geom node effects shift?)
            (#{KeyType/F4 KeyType/Escape} kt) (toggle-map state geom)
            :else nil))))

(defn map-mouse-target?
  "True if the pointer is on the map or the map holds its drag."
  [state {:keys [bounds content-top list-h]} x y]
  (and (:canvas-map state) (not (or (:editor state) (:key-menu state) (:search state)))
       (or (get-in state [:canvas-map :drag])
           (and bounds (< (long (:left bounds)) (long x) (long (:right bounds)))
                (<= (long content-top) (long y)) (< (long y) (+ (long content-top) (dec (long list-h))))))))

(defn- map-mouse [state ^MouseAction key geom]
  (let [pos (.getPosition key)
        x (.getColumn pos) y (.getRow pos)
        action (.getActionType key)
        amount (.getScrollDelta key)
         horizontal (.getHorizontalScrollDelta key)]
    (when-let [node (and (map-mouse-target? state geom x y) (map-node state geom))]
      (let [camera (:canvas-map state)
            [w h :as viewport] (map-viewport geom)
            point [(- x (inc (long (get-in geom [:bounds :left])))) (- y (long (:content-top geom)))]
            now (System/currentTimeMillis)
            drag (:drag camera)
            next (cond
                   (and (= action MouseActionType/CLICK_DOWN) (= 1 (.getButton key)))
                   (assoc camera :drag {:point point :position (cm/camera-at camera now)})
                   (and (= action MouseActionType/DRAG) drag (not (:is-locked node)))
                   (let [[[sx sy] [px py] [cx cy]] [(:point drag) point (:position drag)]
                         plan (cm/plan-for node (:level camera) viewport)
                         next (cm/clamp-camera plan viewport [(+ (long cx) (- (long sx) (long px)))
                                                               (+ (long cy) (- (long sy) (long py)))])]
                     (-> camera (assoc :position next :manual? true) (assoc-in [:drag :moved?] true)
                         (dissoc :started :from)))
                   (= action MouseActionType/CLICK_RELEASE)
                   (if (and drag (not (:moved? drag)))
                     (let [plan (cm/plan-for node (:level camera) viewport)
                           [left top] (cm/clamp-camera plan viewport (cm/camera-at camera now))
                           [px py] point]
                       (cm/focus-card (dissoc camera :drag) node viewport
                                      (cm/hit-card plan [(+ (long left) (long px)) (+ (long top) (long py))]) now))
                     (dissoc camera :drag))
                   (and (.isCtrlDown key) (not (zero? amount)))
                    (cm/zoom camera node viewport (- (Long/signum amount)) point now)
                    (or (not (zero? horizontal)) (and (.isShiftDown key) (not (zero? amount))))
                    (cm/pan camera node viewport (* (if (zero? horizontal) amount horizontal)
                                                     (max 1 (quot (long w) 8))) 0 now)
                    (not (zero? amount))
                    (cm/pan camera node viewport 0 (* amount (max 1 (quot (long h) 6))) now)
                    :else camera)]
        (assoc state :canvas-map next)))))

(defn- map-letter?
  [state {:keys [view rows]}]
  (and (not (:is-transient view))
       (not-any? #(= "m" (:key %)) (:shortcuts view))
       (or (:canvas-map state)
           (not= :field (:kind (focused-entry (nth rows (:selected state) nil) (:cell state)))))))

(defn- command-key
  [state ^KeyStroke key {:keys [view] :as geom} effects]
  (let [^KeyStroke key
        key

        kt
        (.getKeyType key)

        ctrl-ch
        (when (and (= KeyType/Character kt) (.isCtrlDown key))
          (Character/toLowerCase (.charValue (.getCharacter key))))

        state
        (dissoc state :notice)]

    (cond (:prefix state) (if (or (= KeyType/Escape kt) (= \g ctrl-ch))
                            (dissoc state :prefix)
                            (or (map-key (dissoc state :prefix) key geom effects)
                                 (surface-key (dissoc state :prefix) key geom effects)))
          (= \x ctrl-ch) (assoc state :prefix true)
          (= \h ctrl-ch) (open-key-menu state geom)
          (= \f ctrl-ch) (assoc state
                           :search {:text ""}
                           :notice "Find: ")
          :else (or (move-board-issue state key geom effects)
                    (if-let [s (shortcut-for view (shortcut-key key))]
                      (press-shortcut state view s effects)
                      (or (map-key state key geom effects)
                          (when (or (= KeyType/F4 kt)
                                    (and (= "m" (shortcut-key key)) (map-letter? state geom)))
                            (toggle-map state geom))
                          (surface-key state key geom effects)))))))

(defn- pasted-key? [key] (and (map? key) (:paste key)))

(defn- paste-key
  [state text paste]
  (if (:editor state) (update-in state [:editor :input] input/paste-text (paste text)) state))

(defn mouse-target
  "The selectable row under a click inside the content area, or nil on chrome."
  [{:keys [bounds content-top list-h rows]} scroll col row]
  (when (and bounds
             content-top
             list-h
             (< (long (:left bounds))
                (long col)
                (+ (long (:left bounds)) (long (:inner-w bounds)) 1)))
    (let [dy
          (- (long row) (long content-top))

          i
          (+ (long scroll) dy)]

      (when (and (<= 0 dy) (< dy (long list-h)) (:is-selectable (nth rows i nil))) i))))

(defn mouse-cell
  "The selectable cell at terminal column `col`, or nil for a full row."
  [row {:keys [left inner-w]} col]
  (when (seq (:cells row))
    (let [entry (:entry row)
          indent (long (or (:indent entry) 0))
          w (- (long inner-w) indent)
          x (- (long col) (long left) indent)
          cells (if-let [ws (:cell-widths entry)]
                  (columns/width-slots w ws)
                  (columns/slots w (count (:cells entry))))]
      (some (fn [i]
              (let [[at width] (nth cells i)]
                (when (<= (inc (long at)) x (+ (long at) (long width))) i)))
            (:cells row)))))

(defn- mouse-key
  [state ^MouseAction key geom effects]
  (let [action (.getActionType key)
        pos (.getPosition key)
        wheel (when (and (:canvas? geom) (not (or (:editor state) (:key-menu state) (:search state))))
                (ScrollBar/wheelStep key))]
    (cond wheel (read-rows state geom wheel)

          (= MouseActionType/CLICK_RELEASE action)
          (if-let [i (mouse-target geom (:scroll state) (.getColumn pos) (.getRow pos))]
            (let [row (nth (:rows geom) i)
                  midpoint (+ (long (get-in geom [:bounds :left]))
                              (quot (long (get-in geom [:bounds :inner-w])) 2))
                  selected (assoc (dissoc state :reading?)
                             :selected i
                             :cell (mouse-cell row (:bounds geom) (.getColumn pos))
                             :diff-side (if (< (.getColumn pos) midpoint) :left :right))]
              (if (or (= :file (:kind row))
                       (= :media-open (:kind (focused-entry row (:cell selected)))))
                 (enter-row selected geom effects)
                 selected))
            state)

          :else state)))

(defn- canvas-focus
  [row cell]
  (let [entry (focused-entry row cell)]
    (cond (= :board-issue (:kind entry))
          [(:node-id entry) (:item-id entry) nil :board-issue]
          (and (:item-id row) (or (:thread-id row) (#{:canvas-select :canvas-comment} (:kind row))))
          ((juxt :node-id :item-id :thread-id :kind) row))))

(defn- dialog-key
  [state ^KeyStroke key {:keys [view dialog-id] :as geom} effects]
  (cond (instance? MouseAction key) (mouse-key state key geom effects)
        (= KeyType/Escape (.getKeyType key))
        (if (model/stale? view)
          (assoc state :notice (waiting-notice view))
          (do ((:activate! effects) {:action :activate :node-id dialog-id}) state))
        (#{KeyType/Tab KeyType/ReverseTab} (.getKeyType key))
        (move geom state (if (or (.isShiftDown key) (= KeyType/ReverseTab (.getKeyType key))) -1 1))
        (= KeyType/Character (.getKeyType key)) state
        :else (surface-key (dissoc state :notice) key geom effects)))

(defn handle-key
  "The next surface state for `key`. `geom` holds `:rows`, `:view` and `:list-h`;
   `effects` holds `:save!`, `:send!`, `:comment!`, `:activate!`, `:select!`, `:edit!`,
   `:shortcut!` and `:reload!`. Answers `{::dlg/done nil}` to close.

   App shortcuts come before the surface keys. Escape, Tab and every Ctrl key keep
   their surface meaning. After C-x, the next key has its surface meaning, so a
   verb that a shortcut hides stays reachable: C-x s sends and C-x r reloads."
  [state key {:keys [view] :as geom} {:keys [paste] :as effects}]
  (let [next-state
        (cond (pasted-key? key) (paste-key state (:paste key) paste)
              (not (instance? KeyStroke key)) state
              (:dialog-id geom) (dialog-key state key geom effects)
              (log-chip-click? key (:log-chip geom)) (do ((:log! effects)) state)
              (instance? MouseAction key) (or (map-mouse state key geom) (mouse-key state key geom effects))
              (:key-menu state) (menu-key state key geom effects)
              (:editor state) (editor-key state key view effects)
              (:search state) (search-key state key (:rows geom))
              :else (command-key state key geom effects))]
    (if (and (contains? next-state :selected)
             (not= ((juxt :selected :cell) state) ((juxt :selected :cell) next-state)))
      (assoc next-state :canvas-focus (canvas-focus (nth (:rows geom) (:selected next-state) nil)
                                                  (:cell next-state)))
      next-state)))

;;; ── Outcomes ───────────────────────────────────────────────────────────────

(defn saved
  "The Save state after the gateway answered `outcome`. Saved only with an ack seq."
  [save outcome]
  (let [st (model/outcome-state outcome)
        detail (when (= 400 (:http-status outcome))
                 (or (not-empty (:message outcome))
                     (when-not (= "invalid" (:reason outcome)) (not-empty (:reason outcome)))))]
    (if (and (= :saved st) (some? (:seq outcome)))
      (assoc save
        :state :saved
        :seq (:seq outcome)
        :thread-id (:thread-id outcome))
      (assoc save
        :state (if (= :saved st) :failed st)
        :message detail))))

(defn sent
  "The Send state after the gateway answered `outcome`. The key stays until a
   receipt exists, so a retry posts the same key."
  [send outcome]
  (let [st (model/outcome-state outcome)]
    (if (and (= :saved st) (get-in outcome [:submission :id]))
      (assoc send
        :state :sent
        :submission-id (get-in outcome [:submission :id])
        :seq (:seq outcome))
      (assoc send
        :state (if (= :saved st) :failed st)
        :message (:message outcome)))))

(defn status-line
  "The one line under the rows that says what Save and Send did last."
  [{:keys [save send notice]}]
  (str/join "  ·  "
            (remove nil?
              [notice
               (case (:state save)
                 nil
                 nil

                 :skipped
                 (:message save)

                 :saved
                 (str "Saved · state " (:seq save))

                 (or (:message save) (get model/save-label (:state save) "Save failed")))
               (case (:state send)
                 nil
                 nil

                 :sent
                 (str "Sent · " (:submission-id send))

                 (get model/send-label (:state send) "Send failed"))])))

;;; ── Painting ───────────────────────────────────────────────────────────────

(def ^:private scrolling-kinds #{:add :del :ctx :hunk :meta})

(defn- single-row-text
  "The text a row paints, `w` columns wide, with code rows shifted by `hscroll`."
  [row hscroll w]
  (let [{:keys [gutter body]}
        (model/row-parts row)

        body
        (if (and (scrolling-kinds (:kind row)) (pos? (long hscroll)))
          (let [n (count body)]
            (subs body (min n (long hscroll))))
          body)]

    (if-let [counts (when (#{:file :file-item} (:kind row)) (model/file-counts row))]
      (let [w (max 1 (long w))
            room (- w (long (p/display-width gutter)) (long (p/display-width counts)) 2)]

        (if (pos? room)
          (str gutter (p/pad-right (model/file-title row room) (+ room 2)) counts)
          (p/ellipsize (str gutter body) w)))
      (p/ellipsize (str gutter body) (max 1 (long w))))))

(defn row-text
  "The text of `row` within `w` cells. Both split sides keep their gutters while scrolling."
  [row hscroll w]
  (if (= :split (:kind row))
    (let [half
          (:half row)

          side
          #(p/pad-right (single-row-text % hscroll half) half)]

      (p/ellipsize (str (side (:left row)) " │ " (side (:right row))) (max 1 (long w))))
    (single-row-text row hscroll w)))

(defn- diff-palette
  "The theme colors of a diff: the code tokens that the transcript also uses for a patch."
  []
  {:paper t/dialog-bg
   :add-paper t/code-ok-bg
   :del-paper t/code-err-bg
   :hunk-paper (t/mix-color t/dialog-bg t/dialog-fg 0.06)
   :body t/dialog-fg
   :gutter t/dialog-hint
   :add-ink t/code-success-fg
   :del-ink t/code-error-fg})

(defn row-paper
  "The paper of a diff row: the theme's green for an added line, its red for a removed
   one, else the dialog paper."
  [row]
  (get (diff-palette) (get {:add :add-paper :del :del-paper} (:kind row))))

(defn row-inks
  "The colors of one diff code row: `:paper`, the change `:bar` (nil for an unchanged
   line), the `:gutter` ink of the line numbers and the `:body` ink of the code. A
   selected row moves its paper toward the text ink; each ink stays legible on it."
  [row]
  (let [palette
        (diff-palette)

        change
        (get palette (get {:add :add-ink :del :del-ink} (:kind row)))

        tinted
        (or (row-paper row) (:paper palette))

        paper
        (if (:selected? row) (t/mix-color tinted t/dialog-fg 0.18) tinted)]

    {:paper paper
     :bar (some-> change
                  (t/legible-ink paper))
     :gutter (t/legible-ink (or change (:gutter palette)) paper)
     :body (t/legible-ink (:body palette) paper)}))

(defn word-paper
  "The paper of a changed word: the row paper moved toward the change ink."
  [row paper]
  (t/mix-color paper (get (diff-palette) (if (= :add (:kind row)) :add-ink :del-ink)) 0.2))

(defn mark-span
  "The `[from to]` of word mark `mark` inside a painted body of `n` characters that
   starts `hscroll` characters into the line, or nil when no marked character shows."
  [mark hscroll n]
  (when-let [[from to] mark]
    (let [hs (long hscroll)
          n (long n)
          from (min n (max 0 (- (long from) hs)))
          to (min n (max 0 (- (long to) hs)))]

      (when (< from to) [from to]))))

(defn- paint-code-row!
  "One diff code row: a change bar at the edge, the line numbers in the change ink and
   the code in the text ink, on the tinted paper of the row."
  [g left row-y inner-w row hscroll]
  (let [{paper :paper bar :bar gutter-ink :gutter body-ink :body}
        (row-inks row)

        w
        (max 1 (- (long inner-w) 3))

        {:keys [gutter]}
        (model/row-parts row)

        gw
        (min w (long (p/display-width gutter)))

        text
        (row-text row hscroll w)

        body
        (p/ellipsize (subs text (min (count text) (count gutter))) (max 0 (- w gw)))]

    (p/set-colors! g t/dialog-fg paper)
    (p/fill-rect! g (inc (long left)) row-y inner-w 1)
    (when bar (p/set-colors! g bar paper) (p/put-str! g (inc (long left)) row-y "┃"))
    (p/set-colors! g gutter-ink paper)
    (p/put-str! g (+ (long left) 2) row-y (p/ellipsize gutter gw))
    (p/set-colors! g body-ink paper)
    (p/put-str! g (+ (long left) 2 gw) row-y body)
    (when-let [[from to] (mark-span (:mark row) hscroll (count body))]
      (let [mark-paper (word-paper row paper)]
        (p/set-colors! g (t/legible-ink body-ink mark-paper) mark-paper))
      (p/put-str! g
                  (+ (long left) 2 (long gw) (long (p/display-width (subs body 0 from))))
                  row-y
                  (subs body from to)))))

(defn- paint-present-side!
  [g x row-y w hscroll side]
  (when side (paint-code-row! g x row-y w side hscroll)))

(defn- paint-split-row!
  "Paint the present sides of a split diff row."
  [g left row-y inner-w row hscroll]
  (let [hl (quot (long inner-w) 2)]
    (p/set-colors! g t/dialog-fg t/dialog-bg)
    (p/fill-rect! g (inc (long left)) row-y inner-w 1)
    (paint-present-side! g left row-y hl hscroll (:left row))
    (paint-present-side! g (+ (long left) hl) row-y (- (long inner-w) hl) hscroll (:right row))))

(def ^:private thread-kinds #{:thread-head :message-head :message :draft-head :draft :thread-empty})

(defn- paint-thread-row!
  "One thread row as a comment card: an accent edge on the rail and the text on the
   code paper of the theme, inside a row a step off the dialog paper."
  [g left row-y inner-w row text]
  (let [rail
        (count "    ")

        x
        (+ (long left) 2 rail)

        body
        (subs text (min (count text) (+ rail 2)))

        thread-paper
        (t/mix-color t/dialog-bg t/dialog-fg 0.04)

        card-paper
        t/code-block-bg

        head?
        (#{:thread-head :message-head :draft-head} (:kind row))]

    (p/set-colors! g t/dialog-fg thread-paper)
    (p/fill-rect! g (inc (long left)) row-y inner-w 1)
    (p/set-colors! g t/dialog-fg card-paper)
    (p/fill-rect! g x row-y (max 0 (- (long inner-w) rail 2)) 1)
    (p/set-colors! g (t/legible-ink t/header-active-tab-accent card-paper) card-paper)
    (p/put-str! g x row-y "┃")
    (p/set-colors! g (t/legible-ink (if head? t/dialog-hint t/code-block-fg) card-paper) card-paper)
    (p/put-str! g (+ x 2) row-y body)))

(defn- row-ink
  [row]
  (t/legible-ink (case (model/row-tone row)
                   :ok
                   t/status-ok

                   :error
                   t/footer-error-fg

                   :running
                   t/header-active-tab-accent

                   :warn
                   t/footer-warning-fg

                   :idle
                   t/dialog-hint

                   (case (:kind row)
                     (:receipts-head :thread-node)
                     t/dialog-hint-key

                     :receipt
                     (if (= "failed" (:status row)) t/footer-error-fg t/dialog-fg)

                     t/dialog-fg))
                 t/dialog-bg))

(defn- paint-focus!
  "Mark the control `entry` that the cursor is on: its text takes the accent
   fill, the colour of a focused button. A control without text gets a mark."
  [g left row-y inner-w view-id entry]
  (if (#{:trow :tparent} (:kind entry))
    (lv/paint-plan-entry! g left row-y inner-w view-id (assoc entry :is-focused true))
    (let [indent
          (min (long (or (:indent entry) 0)) (max 0 (long inner-w)))

          left
          (+ (long left) indent)

          text
          (str (:text entry))]

      (p/set-colors! g t/header-active-tab-fg t/header-active-tab-bg)
      (if (str/blank? text)
        (p/put-str! g (inc left) row-y "▸")
        (p/styled g
                  [p/BOLD]
                  (p/put-str! g
                              (+ left 2)
                              row-y
                              (p/ellipsize text (max 1 (- (long inner-w) indent 3)))))))))

(defn- paint-row-focus!
  "Mark the focused control of plan `row`: the whole entry, or cell `cell` at
   the place where the live view paints that cell."
  [g left row-y inner-w view-id row cell]
  (let [entry (:entry row)]
    (if (and (some? cell) (seq (:cells row)))
      (let [indent (min (long (or (:indent entry) 0)) (max 0 (long inner-w)))
            inner-w (- (long inner-w) indent)
            places (if-let [ws (:cell-widths entry)]
                     (columns/width-slots inner-w ws)
                     (columns/slots inner-w (count (:cells entry))))
            [x w] (nth places cell nil)]

        (when (and x (pos? (long w)))
          (paint-focus! g
                        (+ (long left) indent (long x))
                        row-y
                        w
                        view-id
                        (nth (:cells entry) cell))))
      (paint-focus! g left row-y inner-w view-id entry))))

(defn- paint-plain-row!
  [g left row-y inner-w row text]
  (p/set-colors! g (row-ink row) t/dialog-bg)
  (p/fill-rect! g (inc (long left)) row-y inner-w 1)
  (p/put-str! g (+ (long left) 2) row-y text))

(defn- file-ink
  [row]
  (t/legible-ink
    (get {:added t/status-ok
          :deleted t/footer-error-fg
          :renamed (get {:file-item t/footer-warning-fg} (:kind row) t/header-active-tab-accent)}
         (:status row)
         t/header-active-tab-accent)
    t/dialog-bg))

(defn- file-count-runs
  [text row]
  (let [counts (str (model/file-counts row))]
    (when-let [start (str/last-index-of text counts)]
      (let [parts (str/split counts #" ")
            offsets (reductions +
                                (p/display-width (subs text 0 start))
                                (map #(inc (long (p/display-width %))) parts))]

        (map vector offsets parts)))))

(defn- paint-file-counts!
  [g x y text row]
  (doseq [[col part] (file-count-runs text row)]
    (p/set-colors! g
                   (t/legible-ink (get {\- t/footer-error-fg} (first part) t/status-ok) t/dialog-bg)
                   t/dialog-bg)
    (p/put-str! g (+ (long x) (long col)) y part)))

(defn- paint-file-label!
  [g x y row text]
  (let [end
        (count text)

        from
        (min 2 end)

        to
        (if (= :file-item (:kind row)) end (min 3 end))]

    (p/set-colors! g (file-ink row) t/dialog-bg)
    (p/put-str! g (+ (long x) from) y (subs text from to))))

(defn- paint-old-path!
  [g x y text]
  (when-let [old (second (re-find #"^..R (.*?) → " text))]
    (p/set-colors! g t/dialog-hint t/dialog-bg)
    (p/put-str! g (+ (long x) 4) y old)))

(defn- paint-file-row!
  [g left row-y inner-w row text]
  (paint-plain-row! g left row-y inner-w row text)
  (let [x (+ (long left) 2)]
    (paint-file-label! g x row-y row text)
    (paint-old-path! g x row-y text)
    (paint-file-counts! g x row-y text row)))

(defn- paint-empty-row!
  [g left row-y inner-w row text]
  (paint-plain-row! g left row-y inner-w row "")
  (p/set-colors! g t/dialog-hint t/dialog-bg)
  (p/put-str! g
              (+ (long left) 2 (quot (max 0 (- (long inner-w) 3 (long (p/display-width text)))) 2))
              row-y
              text))

(defn- paint-hunk-row!
  [g left row-y inner-w text]
  (let [{:keys [gutter paper hunk-paper]} (diff-palette)]
    (p/set-colors! g (t/legible-ink gutter paper) paper)
    (p/fill-rect! g (inc (long left)) row-y inner-w 1)
    (p/set-colors! g (t/legible-ink gutter hunk-paper) hunk-paper)
    (p/put-str! g (+ (long left) 2) row-y (p/pad-right text (max 1 (- (long inner-w) 3))))))

(defn- paint-text-row!
  [g left row-y inner-w hscroll row]
  (let [text
        (row-text row hscroll (- (long inner-w) 3))

        paint-code
        #(paint-code-row! g left row-y inner-w row hscroll)

        paint-thread
        #(paint-thread-row! g left row-y inner-w row text)

        paints
        (merge (zipmap [:add :del :ctx] (repeat paint-code))
               (zipmap thread-kinds (repeat paint-thread))
               {:split #(paint-split-row! g left row-y inner-w row hscroll)
                :file #(paint-file-row! g left row-y inner-w row text)
                :file-item #(paint-file-row! g left row-y inner-w row text)
                :diff-empty #(paint-empty-row! g left row-y inner-w row text)
                :hunk #(paint-hunk-row! g left row-y inner-w text)})]

    ((get paints (:kind row) #(paint-plain-row! g left row-y inner-w row text)))))

(defn- paint-selected-plan!
  [g left row-y inner-w view-id row cell]
  (when (:is-selectable row) (paint-row-focus! g left row-y inner-w view-id row cell)))

(defn- paint-plan-row!
  [g left row-y inner-w view-id selected? cell row]
  (let [table?
        (#{:trow :tparent} (get-in row [:entry :kind]))

        entry
        (cond-> (:entry row)
          table?
          (assoc :is-focused selected?))]

    (lv/paint-plan-entry! g left row-y inner-w view-id entry)
    (when (and selected? (not table?))
      (paint-selected-plan! g left row-y inner-w view-id row cell))))

(defn- selected-source-row
  [row]
  (if (= :split (:kind row))
    (update row (get row :selected-side :right) #(when % (assoc % :selected? true)))
    (assoc row :selected? true)))

(defn- paint-source-row!
  [g left row-y inner-w selected? hscroll row]
  (let [selected-code
        #(paint-text-row! g left row-y inner-w hscroll (selected-source-row row))

        selected-plain
        #(dlg/draw-selectable-row! g
                                   left
                                   row-y
                                   inner-w
                                   true
                                   (str " " (row-text row hscroll (- (long inner-w) 3))))

        selected-paint
        (get (zipmap [:add :del :ctx :split] (repeat selected-code)) (:kind row) selected-plain)]

    (if selected? (selected-paint) (paint-text-row! g left row-y inner-w hscroll row))))

(defn- paint-row!
  [g left row-y inner-w view-id selected? cell hscroll row]
  (if (:entry row)
    (paint-plan-row! g left row-y inner-w view-id selected? cell row)
    (paint-source-row! g left row-y inner-w selected? hscroll row)))

(defn- hidden-key
  "The footer key of surface verb `k` with key name `named`: `C-x k` when an app
   shortcut takes that key, else `k`."
  [view k named]
  (if (shortcut-for view named) (str "C-x " k) k))

(defn- enter-label
  [state view row]
  (case (:kind row)
    :file
    "fold"

    :file-item
    "go to"

    :canvas-select
    "select"

    :canvas-comment
    "new thread"

    :plan
    (case (:kind (focused-entry row (:cell state)))
      :disclosure
      "fold or expand"
      :field
      "edit"

      :media-open
      (some-> (focused-entry row (:cell state)) :action-label str/lower-case)

      :picture-open
      "open"

      :board-issue
      (let [entry (focused-entry row (:cell state))]
        (when (:is-selectable entry) (if (seq (:item-ids entry)) "open issue" "close issue")))

      (if (:thread-id row) "reply" "press"))

    (when (editor-target view row) "comment")))

(defn- named-stroke
  [key]
  (cond (= key "Space") (KeyStroke. \space false false)
        (= 1 (count key)) (KeyStroke. (Character/valueOf (char (first key))) false false false)
        :else (KeyStroke. (KeyType/valueOf key))))

(defn- menu-label
  [label]
  (let [label (str label)]
    (if (seq label) (str (str/upper-case (subs label 0 1)) (subs label 1)) label)))

(defn- menu-action
  [view key label]
  {:label (menu-label label)
   :hint (str (hidden-key view (get key-glyphs key key) key) (when (= key "/") ", C-f"))
   :stroke (named-stroke key)
   :surface? true
   :prefixed? (boolean (shortcut-for view key))})

(defn key-menu-items
  "App actions first, then surface actions with their current local state."
  [state {:keys [view rows pane?]}]
  (let [app-items
        (when-not (or (model/read-only? view) (model/stale? view))
          (for [s
                (:shortcuts view)

                :when (not (some-> (model/find-node view (:node-id s))
                                   button-refusal))]

            {:label (menu-label (shortcut-label view s))
             :hint (get key-glyphs (:key s) (:key s))
             :stroke (named-stroke (:key s))
             :shortcut s}))

        pane-items
        (mapv (fn [[key label]]
                (menu-action view key label))
              [["ArrowUp" "Previous row"] ["ArrowDown" "Next row"] ["PageUp" "Previous page"]
               ["PageDown" "Next page"] ["Home" "First row"] ["End" "Last row"]
               ["ArrowLeft" "Fold file"] ["ArrowRight" "Expand file"]
               ["Enter"
                (or (enter-label state view (nth rows (:selected state) nil))
                    "Press selected control")] ["/" "Find code"] ["r" "Reload"]])

        diff-items
        (when (diff-view? rows)
          (mapv (fn [[key label]]
                  (menu-action view key label))
                [["u" (str "Diff: " (if (:unified? state) "stacked" "split"))]
                 ["w" (str "Wrap: " (if (:wrap? state) "on" "off"))]
                 ["f" (str "Files: " (if (:files? state) "on" "off"))]
                 ["t" (str "Tests: " (if (:hide-tests? state) "hidden" "shown"))]
                 ["i"
                  (str "Whitespace-only files: " (if (:hide-whitespace? state) "hidden" "shown"))]
                 ["a" "Fold / expand all files"] ["c" "Copy file path"]
                 ["Tab" (str "Comment side: " (name (:diff-side state :right)))]]))]

    (vec (concat app-items
                 [{:section? true :label "Pane"}]
                 pane-items
                 (when-not (:wrap? state)
                   [(menu-action view "<" "Scroll code left")
                    (menu-action view ">" "Scroll code right")])
                 (when-not (model/read-only? view) [(menu-action view "s" "Send review")])
                 (when (seq (:log view)) [(menu-action view "l" "Open app log")])
                 (when (compare-image view (nth rows (:selected state) nil))
                   [(menu-action view "v" "Compare before / after")])
                 [(menu-action view "Escape" (:close-label state "Close app"))]
                 (when pane? [{:label "Hide pane" :hint "C-x b p" :hide? true}])
                 (when (seq diff-items) (cons {:section? true :label "Diff"} diff-items))))))

(defn- menu-component
  [items]
  (dlg/select-modal-component "App keys" items {:enter-label "run" :min-width 44}))

(defn- menu-layout
  [state {:keys [cols rows-count bounds pane?]}]
  (let [{:keys [items menu-state]}
        (:key-menu state)

        {:keys [left top right bottom]}
        bounds

        [x y width height]
        (if pane?
          [left top (inc (- (long right) (long left))) (inc (- (long bottom) (long top)))]
          [0 0 cols rows-count])

        region
        (when pane?
          {:width (fn [cols _rows]
                    cols)})

        component
        (menu-component items)

        geom
        (binding [dlg/*dialog-region* region]
          ((:measure component) menu-state width height))]

    {:component component
     :geom geom
     :region region
     :origin [x y]
     :width width
     :height height
     :state ((:reconcile component) menu-state geom)}))

(defn- paint-key-menu!
  [^TextGraphics graphics state geom]
  (let [{:keys [component state geom region origin width height]}
        (menu-layout state geom)

        [x y]
        origin

        g
        (.newTextGraphics graphics
                          (TerminalPosition. (int x) (int y))
                          (TerminalSize. (int width) (int height)))]

    (p/clear-styles! g)
    (p/set-colors! g t/dialog-fg t/dialog-bg)
    (binding [dlg/*dialog-region* region]
      ((:paint component) g state geom))))

(defn- open-key-menu
  [state geom]
  (let [items
        (key-menu-items state geom)

        component
        (menu-component items)]

    (assoc state :key-menu {:items items :menu-state (:init component)})))

(defn- menu-choice
  [state item geom effects]
  (let [state (dissoc state :key-menu)]
    (cond (:shortcut item) (press-shortcut state (:view geom) (:shortcut item) effects)
          (:hide? item) (do ((:hide! effects)) state)
          (:stroke item) (surface-key state (:stroke item) geom effects)
          :else state)))

(defn- menu-accelerator
  [items key prefixed?]
  (let [name (shortcut-key key)]
    (some #(when (and (:stroke %)
                      (not= KeyType/Enter (.getKeyType ^KeyStroke key))
                      (or (= key (:stroke %)) (and name (= name (shortcut-key (:stroke %)))))
                      (or (:shortcut %) (= KeyType/Character (.getKeyType ^KeyStroke key)))
                      (= prefixed? (boolean (:prefixed? %))))
             %)
          items)))

(defn- menu-key
  [state ^KeyStroke key geom effects]
  (let [ch
        (when (= KeyType/Character (.getKeyType key)) (.getCharacter key))

        ctrl?
        (.isCtrlDown key)

        prefixed?
        (boolean (get-in state [:key-menu :prefix]))

        choice
        (menu-accelerator (get-in state [:key-menu :items]) key prefixed?)]

    (cond (or (= KeyType/Escape (.getKeyType key)) (= \? ch) (and ctrl? (= \h ch))) (dissoc state
                                                                                      :key-menu)
          (and ctrl? (= \x ch)) (assoc-in state [:key-menu :prefix] true)
          (and ctrl? (= \f ch)) (command-key (dissoc state :key-menu) key geom effects)
          (and prefixed? (:pane? geom) (= \b ch)) (-> state
                                                      (assoc-in [:key-menu :pane-prefix] true)
                                                      (update :key-menu dissoc :prefix))
          (and (get-in state [:key-menu :pane-prefix]) (= \p ch))
          (menu-choice state {:hide? true} geom effects)
          choice (menu-choice state choice geom effects)
          :else (let [layout
                      (menu-layout state geom)

                      next
                      ((:on-key (:component layout)) (:state layout) key (:geom layout))]

                  (if (contains? next ::dlg/done)
                    (menu-choice state (::dlg/done next) geom effects)
                    (-> state
                        (assoc-in [:key-menu :menu-state] next)
                        (update :key-menu dissoc :prefix :pane-prefix)))))))

(defn- map-footer [state geom]
  (let [node (map-node state geom)
        wide? (> (long (get-in geom [:bounds :inner-w])) 66)
        camera (:canvas-map state)
        toggle (if (map-letter? state geom) "F4/m" "F4")]
    (cond (:is-locked node) [[toggle "list"] ["Tab" "cards"] ["" "Locked"]]
          wide? (cond-> [[toggle "list"] ["↑↓←→" "pan"] ["+/-" "zoom"] ["0" "fit"] ["Tab" "cards"] ["S-wheel" "sideways"] ["C-wheel" "zoom"]]
                  (:manual? camera) (conj ["End" "owner view"]))
          :else [[(str toggle " list   ↑↓←→ +/- 0 Tab") ""]])))

(defn- footer
  "The keys of the hint bar. The automatic keys of an app stand in the bar: their
   buttons have no row, so no other place names them or shows the tab that holds.
   `? keys` is then the last pair, the one that a narrow bar keeps."
  [state {:keys [view rows legend] :as geom}]
  (let [enter
        (when (or (not (:reading? state)) (cursor-in-view? state geom))
          (when-let [label (enter-label state view (nth rows (:selected state) nil))]
            [(hidden-key view "Enter" "Enter") label]))

        keys
        [(hidden-key view "?" "?") "keys"]

        map-hint
        (when (seq (:canvases geom)) [(if (map-letter? state geom) "F4/m" "F4") "map"])

        board-hint
        (when (and (not (model/read-only? view))
                   (:is-editable (focused-entry (selected-row state rows) (:cell state))))
          ["S-←/→" "move issue"])]

    (cond (:editor state) (into [["C-Enter/C-s" "save"]]
                                (if (string? (get-in state [:editor :target :draft]))
                                  [["C-d" "remove draft"] ["Esc" "cancel"]]
                                  [["Esc" "cancel"]]))
          (:search state) [["Enter/S-Enter" "find"] ["Esc" "back"]]
          (:prefix state) [["C-x" "surface key"] ["?" "keys"]]
          (map-node state geom) (map-footer state geom)
          (and (:auto-keys? view) (vector? legend)) (-> legend
                                                        (cond->
                                                          enter (conj enter)
                                                          board-hint (conj board-hint))
                                                        (conj (or map-hint keys)))
          :else (cond-> [keys]
                  map-hint (conj map-hint)
                  board-hint (conj board-hint)
                  enter
                  (conj enter)))))

(def ^:private editor-h "Rows of the editor field." 5)

(defn- title
  "The app's name, then its ONE lifecycle word when it is not live (#430). The state
   counter is the gateway's, not the human's; Save reports its own state in the status line."
  [view]
  (str (app/icon-mark view)
       "  "
       (or (not-empty (str (:title view))) "App")
       (when-let [word
                  (if (model/away? view) "Connecting" (get model/status-label (model/status view)))]
         (str "  ·  " word))))

(def ^:private app-region
  "The dialog region of the app surface: the whole terminal width without an anchor.
   An app uses the height of the terminal, so a small terminal still shows its
   controls and not only a few rows."
  {:width (fn [cols _rows]
            cols)})

(defn- in-app-region
  "Call `f` with the app region bound, unless a caller bound a region already."
  [f]
  (binding [dlg/*dialog-region* (or dlg/*dialog-region* app-region)]
    (f)))

(defn- app-content-h
  "The content rows that the app surface asks for in a `rows-count` terminal."
  ^long [^long rows-count]
  (max 8 (- rows-count (if (< rows-count 36) 8 12))))

(defonce ^:private surface-plan-cache (atom nil))

(defn- surface-plan
  [view width state]
  (let [opts (select-keys state [:unified? :wrap? :files? :hide-tests? :hide-whitespace? :disclosures])
        inputs [width (:folded state) opts @t/theme-revision]
        cached @surface-plan-cache]
    (if (and (identical? view (:source cached)) (= inputs (:inputs cached)))
      (:plan cached)
      (let [view* (without-dialog-shortcuts (auto-keyed view))
            rows (surface-rows view* width (:folded state) opts)
            selectable (selectable-indexes rows)
            anchors (into {} (keep (fn [[idx cell :as pos]]
                                     (when-let [anchor (canvas-focus (nth rows idx) cell)]
                                       [anchor pos]))) (stops rows))
            items (reduce-kv (fn [acc anchor [idx :as pos]]
                               (let [k (subvec anchor 0 2)]
                                 (if (< (long idx) (long (get-in acc [k 0] Long/MAX_VALUE)))
                                   (assoc acc k pos)
                                   acc))) {} anchors)
            nodes (tree-seq #(seq (:children %)) :children {:children (:nodes view*)})
            ;; Timelines measure their live bars on each pass.
            static? (not-any? #(or (#{:timeline :scene} (:type %))
                                  (and (= :spinner (:type %)) (:is-active %))) nodes)
            plan {:view view* :rows rows :stops selectable :anchors anchors :items items
                  :canvas? (boolean (some #(not= :board-issue (peek %)) (keys anchors))) :canvases (cm/cards-in view*)
                   :static? static? :legend (shortcut-legend view*)
                  :paint-version (Object.)}]
        (when static?
          (reset! surface-plan-cache {:source view :inputs inputs :plan plan}))
        plan))))

(defn- geom-in
  "The surface geometry for `state` in the box `bounds`."
  [state cols rows-count content-w bounds view]
  (let [{:keys [content-top content-h hint-row]}
        (dlg/dialog-layout bounds)

        width
        (max 1 (- (long (:inner-w bounds)) 3))

        {:keys [view] :as plan}
        (surface-plan view width state)

        content-h
        (long content-h)

        status?
        (and (> content-h 1)
             (or (not (:editor state)) (> content-h 2) (get-in state [:save :message])))

        editor-title?
        (and (:editor state) (> content-h 3))

        field-h
        (if (:editor state)
          (min (long editor-h) (- content-h (if editor-title? 1 0) (if status? 1 0)
                                             (if (> content-h (if status? 2 1)) 1 0)))
          0)]

    (assoc plan
      :cols cols
      :rows-count rows-count
      :content-w content-w
      :bounds bounds
      :content-top content-top
      :content-h content-h
      :hint-row hint-row
      :status? status?
      :editor-title? editor-title?
      :field-h field-h
      :log-chip (log-chip bounds view)
      :list-h (- content-h (if status? 1 0) (if editor-title? 1 0) field-h))))

(defn- inset-dialog
  "Measure the first open dialog inside the app box. Its controls use the app's owner."
  [state {:keys [bounds view] :as geom}]
  (if-let [node (and (not (model/read-only? view))
                    (first (filter :is-open (dialog-nodes (:nodes view)))))]
    (let [{:keys [left right top bottom]} bounds
          inset (if (>= (- (long right) (long left)) 20) 2 0)
          inner-w (- (long right) (long left) 1 (* 2 inset))
          content (-> view
                      (dissoc :shortcuts :submissions)
                      (assoc :nodes [(dissoc node :is-dialog :is-open :label)]))
          rows (surface-rows content (max 1 (- inner-w 3)) #{} (select-keys state [:disclosures]))
          height (min (inc (- (long bottom) (long top))) (+ (count rows) 8))
          y (+ (long top) (quot (- (inc (- (long bottom) (long top))) height) 2))
          bounds {:left (+ (long left) inset) :right (- (long right) inset)
                  :top y :bottom (+ y height -1) :inner-w inner-w :inner-h (- height 2)}
          layout (dlg/dialog-layout bounds)
          status? (> (long (:content-h layout)) 1)]
      (merge geom layout
             {:background geom :dialog-id (:id node) :dialog-title (:label node)
              :bounds bounds :rows rows :stops (selectable-indexes rows)
              :list-h (- (long (:content-h layout)) (if status? 1 0))
              :status? status? :canvases [] :canvas? false :anchors {} :items {}
              :legend [] :log-chip nil}))
    geom))

(defn measure
  "The surface geometry for `state` in a `cols`x`rows` terminal."
  [state cols rows-count view]
  (let [content-w (dlg/default-content-width cols)]
    (inset-dialog state
                  (geom-in state cols rows-count content-w
                           (in-app-region
                             #(dlg/dialog-bounds cols rows-count content-w (app-content-h rows-count)))
                           view))))

(defn pane-measure
  "The surface geometry for `state` in a side pane: the box at `left` and `top`,
   `width` columns wide and `height` rows high."
  [state left top width height view]
  (let [left
        (long left)

        top
        (long top)

        width
        (long width)

        height
        (long height)

        bounds
        {:left left
         :top top
         :right (+ left width -1)
         :bottom (+ top height -1)
         :inner-w (- width 2)
         :inner-h (- height 2)}]

    ;; The pane title has its own row, so the log chip sits on that row.
    (inset-dialog state
                  (assoc (geom-in state (+ left width) (+ top height) width bounds view)
                    :pane? true
                    :log-chip (log-chip (update bounds :top inc) view)))))

(defn- reconcile-dialog-focus
  [state dialog-id]
  (if (= (:dialog-id state) dialog-id)
    state
    (let [state (dissoc state :key-menu :editor :search :prefix :canvas-map :reading? :notice)]
      (if dialog-id
        (assoc state :dialog-id dialog-id
                     :background-focus (or (:background-focus state)
                                           (select-keys state [:selected :cell :scroll :canvas-focus]))
                     :selected 0 :cell nil :scroll 0 :canvas-focus nil)
        (merge (dissoc state :dialog-id :background-focus :canvas-focus) (:background-focus state))))))

(defn reconcile
  "Keep the cursor on a stop and in view."
  [state {:keys [rows list-h view canvas? anchors items stops dialog-id] :as geom}]
  (let [state (reconcile-dialog-focus state dialog-id)
        stops
        (or stops (selectable-indexes rows))

        [anchor-row anchor-cell]
        (when-let [anchor (:canvas-focus state)]
          (or (get anchors anchor) (get items (subvec anchor 0 2))))

        selected
        (long (or anchor-row (:selected state)))

        selected
        (long (cond (empty? stops) 0
                    (:is-selectable (nth rows selected nil)) selected
                    :else (or (last (take-while #(<= (long %) selected) stops)) (first stops))))

        cells
        (:cells (nth rows selected nil))

        cell
        (when (seq cells)
          (let [cell (if (some? anchor-row) anchor-cell (:cell state))]
            (if (some #{cell} cells) cell (first cells))))

        list-h
        (long list-h)

        scroll
        (let [scroll (long (:scroll state))]
          (p/clamp (if (and canvas? (:reading? state))
                     scroll
                     (max (min scroll selected) (inc (- selected list-h))))
                   0 (max 0 (- (count rows) list-h))))]

    (cond-> (assoc state
              :selected selected
              :cell cell
              :scroll scroll
              :canvas-focus (canvas-focus (nth rows selected nil) cell))
      (map-node state geom)
      (update :canvas-map cm/reconcile (map-node state geom) (map-viewport geom) (System/currentTimeMillis))

      (and (:canvas-map state) (not (map-node state geom))) (dissoc :canvas-map)

      (canvas-item-gone? view (get-in state [:editor :target]))
      (assoc :notice canvas-item-gone-notice)

      (and (= canvas-item-gone-notice (:notice state))
           (not (canvas-item-gone? view (get-in state [:editor :target]))))
      (dissoc :notice))))

(defn- paint-pane-chrome!
  "Paint the side pane like the project sidebar: a box, the title on its own row
   and rules under the title and over the footer. The pane key chord hides it."
  [g {:keys [left top right bottom inner-w]} title chip]
  (let [left
        (long left)

        top
        (long top)

        bottom
        (long bottom)

        width
        (inc (- (long right) left))

        height
        (inc (- bottom top))]

    (p/set-bg! g t/dialog-bg)
    (p/fill-rect! g left top width height)
    (p/set-colors! g t/dialog-hint t/dialog-bg)
    (p/draw-box! g left top width height)
    (doseq [row (distinct (filter #(< top (long %) bottom) [(+ top 2) (- bottom 2)]))]
      (p/put-str! g left row (str "├" (p/horiz-line (- width 2)) "┤")))
    (when (> height 3)
      (p/set-colors! g t/header-fg t/dialog-bg)
      (p/styled g
                [p/BOLD]
                (p/put-str!
                  g
                  (+ left 2)
                  (inc top)
                  (p/ellipsize
                    title
                    (max 0 (if chip (- (long (:col chip)) left 4) (- (long inner-w) 2)))))))))

(defn- paint-log-hit!
  [{:keys [row col width]}]
  (.register interactions/hit-map
             {:bounds {:row row :col col :width width}
              :kind :app-log
              :label "Open the app log"
              :enabled? true}))

(defn- log-chip-ink [view] (if (pos? (app-log/error-count (:log view))) t/status-bad t/dialog-hint))

(defn- paint-pane-log-hit! [pane? chip] (when pane? (paint-log-hit! chip)))

(defn- paint-log-chip!
  [g {:keys [log-chip view pane?]}]
  (when-let [{:keys [col row label]} log-chip]
    (p/set-colors! g (log-chip-ink view) t/dialog-bg)
    (p/put-str! g col row label)
    (paint-pane-log-hit! pane? log-chip)))

(defn- paint-frame-chrome!
  [g {:keys [bounds view pane? cols rows-count content-w log-chip]}]
  (if pane?
    (paint-pane-chrome! g bounds (title view) log-chip)
    (in-app-region #(dlg/draw-dialog-chrome! g
                                             cols
                                             rows-count
                                             (title view)
                                             content-w
                                             (app-content-h rows-count)))))

(defn- paint-chrome! [g geom] (paint-frame-chrome! g geom) (paint-log-chip! g geom))

(defn canvas-moving?
  "True when the canvas needs another frame."
  [state {:keys [view rows list-h canvas?] :as geom}]
  (when-not (:key-menu state)
    (if-let [node (map-node state geom)]
      (or (cm/gliding? (:canvas-map state) (System/currentTimeMillis))
          (and (not (:is-closed view))
               (let [viewport (map-viewport geom)
                     camera (:canvas-map state)]
                 (cm/moving? (cm/plan-for node (:level camera) viewport) camera viewport
                             (System/currentTimeMillis)))))
      (when (and canvas? (not (:is-closed view)))
        (let [start (min (count rows) (long (:scroll state)))
              end (min (count rows) (+ start (long list-h)))]
          (some #(lv/canvas-motion-row? (:entry %)) (subvec rows start end)))))))

(defn- surface-paint-key [st geom]
  (when (and (seq (:canvases geom)) (:static? geom))
    (let [now (System/currentTimeMillis)
          camera (:canvas-map st)]
      [st (:bounds geom) (:paint-version geom)
       (when (canvas-moving? st geom) (lv/canvas-step now))
       (when camera (cm/camera-at camera now))])))

(defn- paint-map! [g state geom node]
  (let [camera (:canvas-map state)
        [w h :as viewport] (map-viewport geom)
        plan (cm/plan-for node (:level camera) viewport)
        left (inc (long (get-in geom [:bounds :left])))
        top (long (:content-top geom))
        now (System/currentTimeMillis)
        [x y] (cm/clamp-camera plan viewport (cm/camera-at camera now))
        card (get-in plan [:boxes (:focus camera)])
        level (case (:level plan) :overview "Overview" :far "Far" :near "Near" "Normal")
        status (str level (when (:is-locked node) " · Locked")
                    (when card (str " · " (p/ellipsize (:title card) (max 2 (- (long w) 24)))))
                    (when (and (:manual? camera) (not (:is-locked node))) " · End: reset"))]
    (cm/paint! g plan camera [left top] viewport now
               (when-not (get-in geom [:view :is-closed]) (lv/canvas-step now))
               {:paper t/dialog-bg :card t/dialog-fg :edge t/dialog-hint :selected t/dialog-hint-key}
               (set (:selected-ids node)))
    (when (> (long (:list-h geom)) 1)
      (p/set-colors! g t/dialog-hint t/dialog-bg)
      (p/put-str! g left (+ top (long h)) (p/ellipsize (str status
                             (when (> (long w) 66)
                               (str " · " (max 0 (long x)) "," (max 0 (long y)) " / " (:w plan) "×" (:h plan))))
                           (long w))))))

(defn- paint-body-rows!
  [g state {:keys [bounds content-top rows list-h view]}]
  (let [{:keys [left inner-w]} bounds
        step (when-not (:is-closed view) (lv/canvas-step (System/currentTimeMillis)))]
    (doseq [idx (range (long (:scroll state))
                       (min (count rows) (+ (long (:scroll state)) (long list-h))))]
      (paint-row! g
                  (long left)
                  (+ (long content-top) (- (long idx) (long (:scroll state))))
                  inner-w
                  (:id view)
                  (= idx (:selected state))
                  (:cell state)
                  (:hscroll state)
                  (let [row (assoc (nth rows idx) :selected-side (get state :diff-side :right))]
                     (if (and step (lv/canvas-motion-row? (:entry row)))
                       (update row :entry lv/animate-canvas-row step)
                       row))))))

(defn- paint-body!
  [g state {:keys [bounds content-top content-h rows] :as geom}]
  (let [{:keys [left inner-w]}
        bounds

        left
        (long left)

        top
        (long content-top)]

    (p/set-colors! g t/dialog-fg t/dialog-bg)
    (p/fill-rect! g (inc left) top inner-w content-h)
    (when (pos? (long (:list-h geom)))
      (if-let [node (map-node state geom)]
        (paint-map! g state geom node)
        (if (empty? rows)
          (do (p/set-colors! g t/dialog-hint t/dialog-bg)
              (p/put-str! g (+ left 2) top (p/ellipsize "This app shows nothing yet" (max 1 (- (long inner-w) 3)))))
          (paint-body-rows! g state geom))))))

(defn- status-state [state] (or (get-in state [:save :state]) (get-in state [:send :state])))

(defn- status-ink
  [state]
  (if (#{:offline :conflict :failed :closed :invalid} (status-state state))
    t/footer-warning-fg
    t/dialog-hint))

(defn- paint-status!
  [g state {:keys [bounds content-top list-h]}]
  (let [{:keys [left inner-w]}
        bounds

        left
        (long left)

        row
        (+ (long content-top) (long list-h))]

    (p/set-colors! g (status-ink state) t/dialog-bg)
    (p/put-str! g
                (+ left 2)
                row
                (p/ellipsize (status-line state) (max 1 (- (long inner-w) 3))))))

(defn- editor-geometry
  "The editor window keeps its cursor line visible without leaving the app content."
  [{:keys [input]} {:keys [bounds content-top list-h status? editor-title? field-h]}]
  (let [left
        (+ (long (:left bounds)) 2)

        w
        (max 1 (- (long (:inner-w bounds)) 3))

        top
        (+ (long content-top) (long list-h) (if status? 1 0) (if editor-title? 1 0))

        field-h
        (long field-h)

        first-line
        (max 0 (inc (- (long (:crow input)) field-h)))]

    {:left left
     :top top
     :width w
     :lines (take field-h (drop first-line (:lines input)))
     :cursor (p/cursor-pos (+ left (min (long (:ccol input)) (dec w)))
                           (+ top (- (long (:crow input)) first-line)))}))

(defn- paint-editor-lines!
  [g {:keys [left top width lines]}]
  (doseq [[i line] (map-indexed vector lines)]
    (p/put-str! g left (+ (long top) (long i)) (p/ellipsize line (long width)))))

(defn- paint-editor!
  [g {:keys [target] :as editor} {:keys [field-h editor-title?] :as geom}]
  (let [{:keys [left top width cursor] :as box} (editor-geometry editor geom)]
    (when editor-title?
      (p/set-colors! g t/dialog-hint-key t/dialog-bg)
      (p/put-str! g left (dec (long top)) (p/ellipsize (target-label target) width)))
    (p/set-colors! g t/code-block-fg t/code-block-bg)
    (p/fill-rect! g left top width field-h)
    (paint-editor-lines! g box)
    cursor))

(defn- fitted-footer
  [state geom width]
  (let [pairs
        (footer state geom)

        fitted
        (dlg/fit-hint-keeping-last pairs width)]

    (if (and (:editor state) (= 1 (count fitted))) [["C-s" "save"]] fitted)))

(defn- paint-surface!
  [g state {:keys [bounds hint-row] :as geom}]
  (paint-chrome! g geom)
  (paint-body! g state geom)
  (when (:status? geom) (paint-status! g state geom))
  (let [cursor
        (when-let [editor (:editor state)]
          (paint-editor! g editor geom))

        {:keys [left inner-w]}
        bounds]

    (dlg/draw-hint-bar! g
                        left
                        hint-row
                        inner-w
                        (fitted-footer state geom (max 0 (- (long inner-w) 2))))
    (if (:key-menu state) (paint-key-menu! g state geom) cursor)))

(defn- paint-dialog!
  [g state {:keys [bounds dialog-title hint-row list-h rows view] :as geom}]
  (let [{:keys [left inner-w]} bounds
        enter (when-let [label (enter-label state view (nth rows (:selected state) nil))]
                ["Enter" label])]
    (paint-pane-chrome! g bounds (or dialog-title "") nil)
    (when (pos? (long list-h)) (paint-body-rows! g state geom))
    (when (:status? geom) (paint-status! g state geom))
    (dlg/draw-hint-bar! g left hint-row inner-w (cond-> [["Esc" "close form"]] enter (conj enter)))
    nil))

(defn- paint!
  [g state {:keys [background] :as geom}]
  (if background
    (do (binding [interactions/hit-map (interactions/create-hit-map)]
          (paint-surface! g (merge (initial-state) (:background-focus state)) background))
        (paint-dialog! g state geom))
    (paint-surface! g state geom)))

;;; ── Driver ─────────────────────────────────────────────────────────────────

(defn- newer
  "The newer copy of two copies of one app view. At the same seq, `b` wins
   unless only `a` is closed: a later copy carries the newer owner state. A copy
   with a body wins over a summary, which has none to show."
  [a b]
  (let [seq-a
        (long (or (:seq a) 0))

        seq-b
        (long (or (:seq b) 0))]

    (cond (nil? a) b
          (nil? b) a
          (not= (boolean (:is-summary a)) (boolean (:is-summary b))) (if (:is-summary a) b a)
          (> seq-b seq-a) b
          (> seq-a seq-b) a
          (and (:is-closed a) (not (:is-closed b))) a
          :else b)))

(defn- held-view
  "The held copy of app `view-id`: a session app, or a global app without `session-id`."
  [session-id view-id]
  (get-in @state/app-db [(if session-id :app-views :global-apps) view-id]))

(defn- read-app
  "The newest copy of the app from the gateway. With copy `held`, the read asks only
   for the frames after its seq and folds them into it. The whole view comes when
   `held` is a summary, when the gateway does not hold those frames or when a frame
   does not fit `held`."
  [session-id view-id held]
  (let [whole
        #(view-model/live-view<-wire (get (vis/gateway-app-read session-id view-id nil) "view"))

        since
        (long (or (:seq held) 0))]

    ;; A summary has no body that the frames can change.
    (if (and held (not (:is-summary held)))
      (let [answer (vis/gateway-app-read session-id view-id since)]
        (if (contains? answer "patches")
          (or (lv/caught-up held (get answer "patches")) (whole))
          (view-model/live-view<-wire (get answer "view"))))
      (whole))))

(defonce ^:private reads*
  ;; `[session-id view-id]` -> `:running`, or the time in ms of a failed read.
  (atom {}))

(def ^:private read-retry-ms "The wait before a failed read of a gap runs again." 2000)

(defn- gap-read-due?
  "True when no read of the app runs and the last one did not fail a moment ago."
  [session-id view-id]
  (let [state (get @reads* [session-id view-id])]
    (not (or (= :running state)
             (and state (< (- (System/currentTimeMillis) (long state)) (long read-retry-ms)))))))

(defn- read-view!
  "Read the current state of the app and give it to the session state, and to `box`
   when a surface shows the app. One read of an app runs at a time. With `whole?`,
   the read ignores the held copy and takes the whole view."
  [box session-id view-id & [whole? pane]]
  (let [app [session-id view-id]]
    (when (not= :running (get (first (swap-vals! reads* assoc app :running)) app))
      (vis/worker-future
        "vis-app-read"
        (fn []
          (try (when-let [view (read-app session-id
                                         view-id
                                         (when-not whole?
                                           (:view (or pane (held-view session-id view-id)))))]
                 (some-> (:view box)
                         (swap! newer view))
                 (state/dispatch [(cond pane :live-pane-loaded
                                        session-id :app-view-loaded
                                        :else :global-app-open) view]))
               (swap! reads* dissoc app)
               (catch Throwable e
                 (swap! reads* assoc app (System/currentTimeMillis))
                 (tel/log!
                   {:level :warn :id ::app-read-failed :error e :data {:view-id view-id}}))))))))

(defn- run-action!
  "Post `action` on a worker and hand the outcome to `on-outcome`."
  [box session-id view-id action on-outcome]
  (vis/worker-future
    "vis-app-action"
    (fn []
      (let [outcome (if session-id
                      (vis/gateway-app-action! session-id view-id action)
                      (vis/gateway-global-app-action! view-id action))]
        (swap! (:outcomes box) conj [on-outcome outcome])
        ((:on-change box))
        (if (and (= :draft (:action action)) (= :conflict (model/outcome-state outcome)))
          (read-view! box session-id view-id true)
          (when (and (:is-accepted outcome)
                     (> (long (or (:seq outcome) 0)) (long (or (:seq @(:view box)) 0))))
            (read-view! box session-id view-id)))))))

(defn refusal-notice
  "What the status line says when the gateway refuses an app action, or nil."
  [outcome]
  (when-not (:is-accepted outcome)
    (case (model/outcome-state outcome)
      :offline
      "Offline: the gateway is out of reach"

      :closed
      "The app is closed"

      (case (:reason outcome)
        "disconnected"
        disconnected-notice

        "unbound"
        "That key has no app action"

        "disabled"
        "That control is disabled"

        (str "Refused"
             (when-let [m (not-empty (str (or (:message outcome) (:reason outcome))))]
               (str ": " m)))))))

(defn- noticed
  "The state after a plain action: a refusal shows in the status line."
  [st outcome]
  (if-let [notice (refusal-notice outcome)]
    (assoc st :notice notice)
    st))

(defn- saved-editor
  "Keep a saved draft attached to the thread that the gateway accepted."
  [body st outcome]
  (let [save (saved (:save st) outcome)]
    (cond-> (assoc st :save save)
      (and (= :conflict (:state save)) (:editor st))
      (assoc-in [:editor :retry-seq] (:seq outcome))

      (and (= :saved (:state save)) (:editor st))
      (update :editor
              #(-> %
                   (dissoc :retry-seq)
                   (assoc :base-seq (:seq save))))

      (and (= :saved (:state save)) (:editor st))
      (update-in [:editor :target]
                 #(cond-> (assoc % :draft body) (:thread-id save) (assoc :thread-id
                                                                    (:thread-id save)))))))

(defn- save-effect!
  [box session-id view-id action _editor]
  (run-action! box session-id view-id action (partial saved-editor (:body action))))

(defn- send-outcome [st outcome] (assoc st :send (sent (:send st) outcome)))

(defn- send-effect!
  [box session-id view-id action node-id]
  (run-action! box
               session-id
               view-id
               (cond-> action
                 node-id
                 (assoc :node-id node-id))
               send-outcome))

(defn- comment-refs
  "The refs of the saved drafts of `view`: what a new turn of the session adds."
  [view-id view]
  (mapv #(assoc (select-keys % [:node-id :thread-id]) :view-id view-id) (app/drafts (:nodes view))))

(defn- comment-effect!
  "Send the saved drafts of the app with a new turn of the session. The gateway
   removes the drafts when it accepts the turn."
  [box session-id view-id]
  (vis/worker-future
    "vis-app-comments"
    (fn []
      (let [notice (try (if (:turn (vis/gateway-submit-turn!
                                     session-id
                                     {:request "" :comments (comment-refs view-id @(:view box))}))
                          "Comments sent to the session"
                          "Comments not sent")
                        (catch Throwable t (str "Comments not sent: " (ex-message t))))]
        (swap! (:outcomes box) conj
          [(fn [st _]
             (assoc st :notice notice)) nil])
        ((:on-change box))
        (read-view! box session-id view-id)))))

(defn- notice-effect!
  [box notice]
  (swap! (:outcomes box) conj
    [(fn [st _]
       (assoc st :notice notice)) nil]))

(defn- edit-value!
  [box session-id view-id screen modal! node]
  (when-let [{:keys [value]} (modal! #(dlg/live-input-value! screen node))]
    (run-action! box
                 session-id
                 view-id
                 {:action :change :node-id (:id node) :value value}
                 noticed)))

(defn- edit-effect!
  [box session-id view-id screen modal! node]
  (if screen
    (edit-value! box session-id view-id screen modal! node)
    (notice-effect! box "This input is read-only here")))

(defn- inspect-effect!
  [box open-picture! node]
  (if open-picture! (open-picture! node) (notice-effect! box "Pictures open from the chat only")))

(defn- shortcut-outcome
  [shortcut st outcome]
  (assoc st :notice (or (refusal-notice outcome) (str (shortcut-label shortcut) " ✓"))))

(defn- shortcut-effect!
  [box session-id view-id action shortcut]
  (run-action! box session-id view-id action (partial shortcut-outcome shortcut)))

(defn- log-effect! [screen modal! current] (when screen (modal! #(app-log/show! screen current))))

(defn- app-effects
  [box session-id view-id screen modal! open-picture! current]
  {:save! (partial save-effect! box session-id view-id)
   :send! (partial send-effect! box session-id view-id)
   :comment! (when session-id (partial comment-effect! box session-id view-id))
   :activate! (fn [action]
                (run-action! box session-id view-id action noticed))
   :select! (fn [action]
              (run-action! box session-id view-id action noticed))
   :move! (fn [action]
             (run-action! box session-id view-id action noticed))
   :edit! (partial edit-effect! box session-id view-id screen modal!)
   :shortcut! (partial shortcut-effect! box session-id view-id)
   :reload! (fn []
              (read-view! box session-id view-id true))
   :inspect! (partial inspect-effect! box open-picture!)
   :log! (partial log-effect! screen modal! current)
   :copy! input/clipboard-copy!
   :hide! #(state/dispatch [:live-panel-toggle-open])
   :reveal!
   (fn [view node]
     (when-let [reveal
                (app-surfaces/reveal-request session-id view node (System/currentTimeMillis))]
       (state/dispatch [:app-reveal reveal])))
   :paste identity})

(defn- paste-start?
  [^KeyStroke key]
  (= KeyType/PasteStart
     (some-> key
             .getKeyType)))

(defn- modal-key!
  [^TerminalScreen screen]
  (let [key (dlg/read-modal-key! screen)]
    (if (paste-start? key) {:paste (dlg/drain-modal-paste! screen)} key)))

(defn- refresh-gap!
  [box session-id view-id]
  (when (and (:is-gap (held-view session-id view-id)) (gap-read-due? session-id view-id))
    (read-view! box session-id view-id)))

(defn refresh-gaps!
  "Read each view of the tab in front that is behind the gateway."
  [session-id]
  (let [{:keys [app-views live-views]} @state/app-db]
    (doseq [[view-id held] app-views
            :let [sid (or (:session-id (:view held)) session-id)]
            :when (and sid (:is-gap held))]

      (refresh-gap! nil sid view-id))
    (doseq [pane live-views
            :let [view-id (lv/view-id pane)
                  sid (or (get-in pane [:view :session-id]) session-id)]
            :when (and sid (:is-gap pane) (not (lv/settled? pane)) (gap-read-due? sid view-id))]

      (read-view! nil sid view-id false pane))))

(defn- connect-away!
  "Bring the owner of the app back for the person who has the app open: one `connect`
   action for each time that the owner is away."
  [box session-id view-id]
  (let [connect (:connect box)]
    (cond (not (model/away? (newer @(:view box) (:view (held-view session-id view-id)))))
          (reset! connect nil)
          (compare-and-set! connect nil :sent)
          (vis/worker-future
            "vis-app-connect"
            (fn []
              (if (:is-accepted (if session-id
                                  (vis/gateway-app-action! session-id view-id {:action :connect})
                                  (vis/gateway-global-app-action! view-id {:action :connect})))
                (read-view! box session-id view-id true)
                (reset! connect :refused))
              ((:on-change box)))))))

(defn- idle-paint! [] (Thread/sleep (if (lv/scene-moving?) 12 40)) nil)

(defn- reveal-ready?
  "True when a press asked for another app of `session-id` and that app is open."
  [session-id]
  (let [db @state/app-db]
    (map? (app-surfaces/reveal-step (:app-reveal db)
                                    session-id
                                    (:app-views db)
                                    (System/currentTimeMillis)))))

(defn- read-component-key!
  "The next key of the app dialog. A ready reveal answers `::reveal`: the dialog
   closes, so the chat shows the revealed app."
  [box session-id view-id ^TerminalScreen screen]
  (refresh-gap! box session-id view-id)
  (connect-away! box session-id view-id)
  (cond (dlg/modal-input-pending? screen) (modal-key! screen)
        (reveal-ready? session-id) ::reveal
        :else (idle-paint!)))

(defn- reconcile-outcomes!
  [box st geom]
  (let [outcomes (first (reset-vals! (:outcomes box) []))]
    (reconcile (reduce (fn [acc [f outcome]]
                         (f acc outcome))
                       st
                       outcomes)
               geom)))

(defn- component-box
  [first-view on-change]
  {:view (atom first-view)
   :outcomes (atom [])
   ;; nil, `:sent` after the `connect` of this outage, or `:refused`.
   :connect (atom nil)
   :on-change (or on-change (constantly nil))})

(defn- current-app-view
  "The copy of the app that the surface shows. After a refused `connect`, the app
   shows as disconnected: nothing brings its owner back."
  [box session-id view-id]
  (cond-> (or (newer @(:view box) (:view (held-view session-id view-id))) {:id view-id :nodes []})
    (= :refused @(:connect box))
    (dissoc :is-reconnectable)))

(defn- component-initial-state
  [close-label]
  (cond-> (initial-state)
    close-label
    (assoc :close-label close-label)))

(defn component
  "The app dialog, with one shared state and a queue of worker outcomes."
  [session-id view-id first-view & [screen close-label {:keys [on-change modal! open-picture!]}]]
  (let [box
        (component-box first-view on-change)

        modal!
        (or modal!
            (fn [f]
              (f)))

        current
        (partial current-app-view box session-id view-id)

        effects
        (app-effects box session-id view-id screen modal! open-picture! current)]

    ;; The stream holds no app frame, so read the newest copy.
    (read-view! box session-id view-id)
    (connect-away! box session-id view-id)
    {:init (component-initial-state close-label)
     :measure (fn [st cols rows]
                (measure st cols rows (current)))
     :measure-pane (fn [st left top width height]
                     (refresh-gap! box session-id view-id)
                     (connect-away! box session-id view-id)
                     (pane-measure st left top width height (current)))
     :reconcile (partial reconcile-outcomes! box)
     :paint-key surface-paint-key
     :paint paint!
     :on-key (fn [st key geom]
               (if (= ::reveal key) {::dlg/done nil} (handle-key st key geom effects)))
     :read-key (partial read-component-key! box session-id view-id)}))

(defn show-transcript-map!
  "Open a mounted transcript view in map mode."
  [^TerminalScreen screen current node-id effects]
  (let [init (assoc (initial-state) :canvas-map {:id node-id})]
    (in-app-region
      #(dlg/run-modal! screen
                       {:init init
                        :measure (fn [st cols rows] (measure st cols rows (current)))
                        :reconcile reconcile
                        :paint-key surface-paint-key
                        :paint paint!
                        :on-key (fn [st key geom] (handle-key st key geom effects))
                        :read-key (fn [screen]
                                    (if (dlg/modal-input-pending? screen) (modal-key! screen)
                                      (idle-paint!)))}))))

(defn show-app!
  "Open app `view-id` of `session-id`, or global app `view-id` without `session-id`,
   until the human closes it. `band` `{:left :width}` keeps the app in those screen
   columns; without it the app uses the whole terminal width. `close-label` names
   what Esc does. An app that cannot connect does not open."
  [^TerminalScreen screen session-id view-id & [{:keys [band close-label open-picture!]}]]
  (let [run! #(dlg/run-modal! screen
                              (component session-id
                                         view-id
                                         (:view (held-view session-id view-id))
                                         screen
                                         close-label
                                         {:open-picture! open-picture!}))]
    (cond (not (model/openable? (:view (held-view session-id view-id)))) nil
          band (binding [frame/*column-offset* (long (:left band))
                         dlg/*dialog-region* {:width (fn [cols _rows]
                                                       (app-surfaces/band-width band cols))}]

                 (run!))
          :else (run!))))

(defn show!
  "Open the app attachment `row` of `session-id` until the human closes it."
  [^TerminalScreen screen session-id row & [opts]]
  (if-let [view-id (model/row-view-id row)]
    (show-app! screen session-id view-id opts)
    (vis/notify! "This attachment names no app" :level :warn)))
