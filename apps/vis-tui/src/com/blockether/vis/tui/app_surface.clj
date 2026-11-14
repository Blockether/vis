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
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.columns :as columns]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.frame :as frame]
            [com.blockether.vis.tui.input :as input]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.live-view :as lv]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.state :as state]
            [com.blockether.vis.tui.theme :as t]
            [com.blockether.vis.tui.view-model :as view-model]
            [taoensso.telemere :as tel])
  (:import [com.googlecode.lanterna TerminalPosition TerminalSize]
           [com.googlecode.lanterna.graphics TextGraphics]
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

    :picture-open
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
  [view node width]
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
                                :entry entry
                                :text (:text entry)
                                :is-selectable (boolean (seq found))}
                         cells
                         (assoc :cells found))) seen]))
            [[] #{}]
            (lv/plan (lv/opened (assoc view
                                  :nodes [node]
                                  :description nil))
                     width))))

(defn- node-rows
  [view node width folded opts]
  (cond (= :diff (:type node))
        ;; Toolbar buttons that no key covers stand above the diff as rows.
        (into (into [] (mapcat #(plan-rows view % width)) (:children node))
              (model/diff-rows (dissoc node :children) width folded opts))
        (= :thread (:type node))
        (into [{:kind :thread-node
                :node-id (:id node)
                :thread-id (:id node)
                :text (or (:label node) "Discussion")
                :is-selectable true}]
              (model/thread-rows node width {:node-id (:id node) :thread-id (:id node)}))
        (and (seq (:children node)) (holds-threads? node))
        (into [] (mapcat #(node-rows view % width folded opts)) (:children node))
        (= :image (:type node))
        (plan-rows view (assoc node :is-flipped (contains? folded [:picture (:id node)])) width)
        :else (plan-rows view node width)))

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
            (cond (= :button (:type node)) [node]
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
                      (if (seq (:children node))
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
                   (:nodes view))
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
    (cond (#{:thread-head :thread-node} (:kind row))
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

(defn save-request
  "What Save does with the editor: `{:action a}` for the gateway, `{:skip reason}`
   when a blank text has nothing to remove, or nil without an editor."
  [view {:keys [target input]}]
  (when target
    (let [body (input/input->text input)]
      (if (and (app/blank-text? body) (not (:draft target)))
        {:skip "Nothing saved: the comment is empty"}
        {:action (model/draft-action view target body)}))))

(defn send-key
  "The idempotency key of the next Send: the key of a Send that did not get a
   receipt, else a new one from `fresh`."
  [send fresh]
  (if (and (:key send) (not= :sent (:state send))) (:key send) (fresh)))

(defn- save-editor
  [state view save!]
  (if (= :saving (get-in state [:save :state]))
    state
    (let [{:keys [action skip]} (save-request view (:editor state))]
      (cond skip (assoc state
                   :editor nil
                   :save {:state :skipped :message skip})
            action (do (save! action (:editor state))
                       (assoc state
                         :save {:state :saving :target (get-in state [:editor :target])}))
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
  [state ^KeyStroke key view {:keys [save!]}]
  (cond (= KeyType/Escape (.getKeyType key)) (assoc state :editor nil)
        (save-shortcut? key) (if (and (= KeyType/Enter (.getKeyType key))
                                      (app/blank-text? (input/input->text
                                                         (get-in state [:editor :input]))))
                               state
                               (save-editor state view save!))
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

(defn- owner-control
  [state view node item-id {:keys [activate! select! edit! reveal!]}]
  (let [node-id (:id node)]
    (cond (#{:button :card} (:type node)) (do (activate! {:action :activate :node-id node-id})
                                              (when reveal! (reveal! view node))
                                              state)
          (and item-id (= :table (:type node)))
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
          (model/stale? view) (assoc state :notice disconnected-notice)
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
        (str "Disconnected: the controls wait for the owner of the app"
             (when (seq pairs) (str "  ·  " (str/join "  ·  " (map #(str/join " " %) pairs)))))
        (not-empty pairs)))))

(defn- press-shortcut
  [state view s {:keys [shortcut!] :as effects}]
  (cond (model/stale? view) (assoc state :notice disconnected-notice)
        (:node-id s) (press state view (:node-id s) nil effects)
        :else (do (shortcut! {:action :shortcut :key (:key s)} s)
                  (assoc state :notice (str (shortcut-label view s) " …")))))

(defn compare-image
  "The image node with an old picture that `row` shows, or nil."
  [view row]
  (when-let [node (some->> (:node-id row)
                           (model/find-node view))]
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
  [state {:keys [view rows]} effects]
  (let [row (selected-row state rows)]
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

      (if (model/read-only? view)
        (assoc state :save {:state :closed})
        (if (= :plan (:kind row))
          (let [entry (focused-entry row (:cell state))]
            (press state view (:node-id entry) (:item-id entry) effects))
          (if-let [target (editor-target view row)]
            (open-editor state target)
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
  (if (compare-image view (nth rows (:selected state) nil))
    (toggle-fold state [:picture (:node-id (nth rows (:selected state)))])
    state))

(defn- send-key-action
  [state {:keys [view]} effects]
  (if (model/read-only? view)
    (assoc state :save {:state :closed})
    (send-app state view nil effects)))

(defn- fold-all-files
  [state rows]
  (let [paths (set (keep #(when (= :file (:kind %)) (:path %)) rows))]
    (assoc state :folded (if (every? (:folded state) paths) #{} paths))))

(defn- copy-path
  [state rows copy!]
  (if-let [path (:path (nth rows (:selected state) nil))]
    (do (copy! path) (assoc state :notice (str "Copied " path)))
    state))

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

(defn- surface-key
  "The surface's own meaning of key, without app shortcuts."
  [state ^KeyStroke key {:keys [rows list-h] :as geom} effects]
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
          (contains? steps kt) (move geom state (get steps kt))
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
                            (surface-key (dissoc state :prefix) key geom effects))
          (= \x ctrl-ch) (assoc state :prefix true)
          (= \h ctrl-ch) (open-key-menu state geom)
          (= \f ctrl-ch) (assoc state
                           :search {:text ""}
                           :notice "Find: ")
          :else (if-let [s (shortcut-for view (shortcut-key key))]
                  (press-shortcut state view s effects)
                  (surface-key state key geom effects)))))

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

(defn- mouse-key
  [state ^MouseAction key geom effects]
  (if (= MouseActionType/CLICK_RELEASE (.getActionType key))
    (let [pos (.getPosition key)]
      (if-let [i (mouse-target geom (:scroll state) (.getColumn pos) (.getRow pos))]
        (let [row (nth (:rows geom) i)
              midpoint (+ (long (get-in geom [:bounds :left]))
                          (quot (long (get-in geom [:bounds :inner-w])) 2))
              selected (assoc state
                         :selected i
                         :cell nil
                         :diff-side (if (< (.getColumn pos) midpoint) :left :right))]

          (if (= :file (:kind row)) (enter-row selected geom effects) selected))
        state))
    state))

(defn handle-key
  "The next surface state for `key`. `geom` holds `:rows`, `:view` and `:list-h`;
   `effects` holds `:save!`, `:send!`, `:activate!`, `:select!`, `:edit!`,
   `:shortcut!` and `:reload!`. Answers `{::dlg/done nil}` to close.

   App shortcuts come before the surface keys. Escape, Tab and every Ctrl key keep
   their surface meaning. After C-x, the next key has its surface meaning, so a
   verb that a shortcut hides stays reachable: C-x s sends and C-x r reloads."
  [state key {:keys [view] :as geom} {:keys [paste] :as effects}]
  (cond (pasted-key? key) (paste-key state (:paste key) paste)
        (not (instance? KeyStroke key)) state
        (log-chip-click? key (:log-chip geom)) (do ((:log! effects)) state)
        (instance? MouseAction key) (mouse-key state key geom effects)
        (:key-menu state) (menu-key state key geom effects)
        (:editor state) (editor-key state key view effects)
        (:search state) (search-key state key (:rows geom))
        :else (command-key state key geom effects)))

;;; ── Outcomes ───────────────────────────────────────────────────────────────

(defn saved
  "The Save state after the gateway answered `outcome`. Saved only with an ack seq."
  [save outcome]
  (let [st (model/outcome-state outcome)]
    (if (and (= :saved st) (some? (:seq outcome)))
      (assoc save
        :state :saved
        :seq (:seq outcome)
        :thread-id (:thread-id outcome))
      (assoc save
        :state (if (= :saved st) :failed st)
        :message (:message outcome)))))

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

                 (get model/save-label (:state save) "Save failed"))
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
  (if (= :plan (:kind row))
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

    :plan
    (case (:kind (focused-entry row (:cell state)))
      :field
      "edit"

      :picture-open
      "open"

      "press")

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

(defn- footer
  "The keys of the hint bar. The automatic keys of an app stand in the bar: their
   buttons have no row, so no other place names them or shows the tab that holds.
   `? keys` is then the last pair, the one that a narrow bar keeps."
  [state view rows legend]
  (let [enter
        (when-let [label (enter-label state view (nth rows (:selected state) nil))]
          [(hidden-key view "Enter" "Enter") label])

        keys
        [(hidden-key view "?" "?") "keys"]]

    (cond (:editor state) [["C-Enter/C-s" "save"] ["Esc" "cancel"]]
          (:search state) [["Enter/S-Enter" "find"] ["Esc" "back"]]
          (:prefix state) [["C-x" "surface key"] ["?" "keys"]]
          (and (:auto-keys? view) (vector? legend)) (-> legend
                                                        (cond->
                                                          enter
                                                          (conj enter))
                                                        (conj keys))
          :else (cond-> [keys]
                  enter
                  (conj enter)))))

(def ^:private editor-h "Rows of the editor field." 5)

(defn- title
  "The app's name and its ONE lifecycle word (#430). The state counter is the
   gateway's, not the human's; Save reports its own state in the status line."
  [view]
  (str (or (not-empty (str (:title view))) "App")
       "  ·  "
       (get model/status-label (model/status view))))

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

(defn- geom-in
  "The surface geometry for `state` in the box `bounds`."
  [state cols rows-count content-w bounds view]
  (let [{:keys [content-top content-h hint-row]}
        (dlg/dialog-layout bounds)

        view
        (auto-keyed view)

        width
        (max 20 (- (long (:inner-w bounds)) 3))

        rows
        (surface-rows view
                      width
                      (:folded state)
                      (select-keys state [:unified? :wrap? :files? :hide-tests? :hide-whitespace?]))

        editor-rows
        (if (:editor state) (+ 2 (long editor-h)) 0)

        legend
        (shortcut-legend view)

        lines
        []]

    {:cols cols
     :rows-count rows-count
     :content-w content-w
     :bounds bounds
     :content-top content-top
     :content-h content-h
     :hint-row hint-row
     :view view
     :rows rows
     :editor-rows editor-rows
     :legend legend
     :legend-lines lines
     :log-chip (log-chip bounds view)
     :list-h (max 1 (- (long content-h) 1 (count lines) editor-rows))}))

(defn measure
  "The surface geometry for `state` in a `cols`x`rows` terminal."
  [state cols rows-count view]
  (let [content-w (dlg/default-content-width cols)]
    (geom-in state
             cols
             rows-count
             content-w
             (in-app-region
               #(dlg/dialog-bounds cols rows-count content-w (app-content-h rows-count)))
             view)))

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
    (assoc (geom-in state (+ left width) (+ top height) width bounds view)
      :pane? true
      :log-chip (log-chip (update bounds :top inc) view))))

(defn reconcile
  "Keep the cursor on a stop and in view."
  [state {:keys [rows list-h]}]
  (let [stops
        (selectable-indexes rows)

        selected
        (long (:selected state))

        selected
        (long (cond (empty? stops) 0
                    (some #{selected} stops) selected
                    :else (or (last (filter #(<= (long %) selected) stops)) (first stops))))

        cells
        (:cells (nth rows selected nil))

        cell
        (when (seq cells) (if (some #{(:cell state)} cells) (:cell state) (first cells)))

        list-h
        (long list-h)

        scroll
        (-> (long (:scroll state))
            (min selected)
            (max (inc (- selected list-h)))
            (p/clamp 0 (max 0 (- (count rows) list-h))))]

    (assoc state
      :selected selected
      :cell cell
      :scroll scroll)))

(defn- paint-pane-chrome!
  "Paint the side pane like the project sidebar: a box, the title on its own row
   and rules under the title and over the footer. The pane key chord hides it."
  [g {:keys [left top right bottom inner-w]} view chip]
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
                    (title view)
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
    (paint-pane-chrome! g bounds view log-chip)
    (in-app-region #(dlg/draw-dialog-chrome! g
                                             cols
                                             rows-count
                                             (title view)
                                             content-w
                                             (app-content-h rows-count)))))

(defn- paint-chrome! [g geom] (paint-frame-chrome! g geom) (paint-log-chip! g geom))

(defn- paint-body-rows!
  [g state {:keys [bounds content-top rows list-h view]}]
  (let [{:keys [left inner-w]} bounds]
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
                  (assoc (nth rows idx) :selected-side (get state :diff-side :right))))))

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
    (if (empty? rows)
      (do (p/set-colors! g t/dialog-hint t/dialog-bg)
          (p/put-str! g (+ left 2) top "This app shows nothing yet"))
      (paint-body-rows! g state geom))))

(defn- legend-height [lines] (count lines))

(defn- status-state [state] (or (get-in state [:save :state]) (get-in state [:send :state])))

(defn- status-ink
  [state]
  (if (#{:offline :conflict :failed :closed} (status-state state))
    t/footer-warning-fg
    t/dialog-hint))

(defn- paint-status!
  [g state {:keys [bounds content-top list-h legend-lines]}]
  (let [{:keys [left inner-w]}
        bounds

        left
        (long left)

        row
        (+ (long content-top) (long list-h))]

    (p/set-colors! g (status-ink state) t/dialog-bg)
    (p/put-str! g
                (+ left 2)
                (+ row (long (legend-height legend-lines)))
                (p/ellipsize (status-line state) (max 1 (- (long inner-w) 3))))))

(defn- editor-geometry
  "The editor window keeps its cursor line visible without leaving the app content."
  [{:keys [input]} {:keys [bounds content-top list-h legend-lines editor-rows content-h]}]
  (let [left
        (long (:left bounds))

        w
        (max 1 (- (long (:inner-w bounds)) 3))

        top
        (+ (long content-top) (long list-h) 1 (long (legend-height legend-lines)))

        first-line
        (max 0 (- (long (:crow input)) (dec (long editor-h))))]

    {:left (+ left 2)
     :top top
     :width w
     :first-line first-line
     :lines (take editor-h (drop first-line (:lines input)))
     :cursor (when (< (long editor-rows) (long content-h))
               (p/cursor-pos (+ left 2 (min (long (:ccol input)) (dec w)))
                             (+ top 1 (- (long (:crow input)) first-line))))}))

(defn- paint-editor-lines!
  [g {:keys [left top width lines]}]
  (doseq [[i line] (map-indexed vector lines)]
    (p/put-str! g left (+ (long top) 1 (long i)) (p/ellipsize line (long width)))))

(defn- paint-editor!
  [g {:keys [target] :as editor} geom]
  (let [{:keys [left top width cursor] :as box} (editor-geometry editor geom)]
    (p/set-colors! g t/dialog-hint-key t/dialog-bg)
    (p/put-str! g left top (p/ellipsize (target-label target) width))
    (p/set-colors! g t/code-block-fg t/code-block-bg)
    (p/fill-rect! g left (inc (long top)) width editor-h)
    (paint-editor-lines! g box)
    cursor))

(defn- paint!
  [g state {:keys [bounds hint-row view rows] :as geom}]
  (paint-chrome! g geom)
  (paint-body! g state geom)
  (paint-status! g state geom)
  (let [cursor
        (when-let [editor (:editor state)]
          (paint-editor! g editor geom))

        {:keys [left inner-w]}
        bounds]

    (dlg/draw-hint-bar! g
                        left
                        hint-row
                        inner-w
                        (dlg/fit-hint-keeping-last (footer state view rows (:legend geom))
                                                   (max 0 (- (long inner-w) 2))))
    (if (:key-menu state) (paint-key-menu! g state geom) cursor)))

;;; ── Driver ─────────────────────────────────────────────────────────────────

(defn- newer
  "The newer copy of two copies of one app view. At the same seq, `b` wins
   unless only `a` is closed: a later copy carries the newer owner state."
  [a b]
  (let [seq-a
        (long (or (:seq a) 0))

        seq-b
        (long (or (:seq b) 0))]

    (cond (nil? a) b
          (nil? b) a
          (> seq-b seq-a) b
          (> seq-a seq-b) a
          (and (:is-closed a) (not (:is-closed b))) a
          :else b)))

(defn- held-view
  "The held copy of app `view-id`: a session app, or a global app without `session-id`."
  [session-id view-id]
  (get-in @state/app-db [(if session-id :app-views :global-apps) view-id]))

(defn- read-view!
  "Read the current state of the app and give it to the session state."
  [box session-id view-id]
  (when (compare-and-set! (:reading box) false true)
    (vis/worker-future
      "vis-app-read"
      (fn []
        (try (when-let [view (view-model/live-view<-wire (if session-id
                                                           (vis/gateway-live-view-state session-id
                                                                                        view-id)
                                                           (vis/gateway-global-app view-id)))]
               (swap! (:view box) newer view)
               (state/dispatch [(if session-id :app-view-loaded :global-app-open) view]))
             (catch Throwable e
               (tel/log! {:level :warn :id ::app-read-failed :error e :data {:view-id view-id}}))
             (finally (reset! (:reading box) false)))))))

(defn- run-action!
  "Post `action` on a worker and hand the outcome to `on-outcome`."
  [box session-id view-id action on-outcome]
  (vis/worker-future "vis-app-action"
                     (fn []
                       (let [outcome (if session-id
                                       (vis/gateway-app-action! session-id view-id action)
                                       (vis/gateway-global-app-action! view-id action))]
                         (swap! (:outcomes box) conj [on-outcome outcome])
                         ((:on-change box))
                         (when (and (:is-accepted outcome)
                                    (> (long (or (:seq outcome) 0))
                                       (long (or (:seq @(:view box)) 0))))
                           (read-view! box session-id view-id))))))

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
      (and (= :saved (:state save)) (:editor st))
      (update-in [:editor :target]
                 #(cond-> (assoc % :draft body) (:thread-id save) (assoc :thread-id
                                                                    (:thread-id save)))))))

(defn- save-effect!
  [box session-id view-id action editor]
  (let [body (not-empty (str/trim (input/input->text (:input editor))))]
    (run-action! box session-id view-id action (partial saved-editor body))))

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
   :activate! (fn [action]
                (run-action! box session-id view-id action noticed))
   :select! (fn [action]
              (run-action! box session-id view-id action noticed))
   :edit! (partial edit-effect! box session-id view-id screen modal!)
   :shortcut! (partial shortcut-effect! box session-id view-id)
   :reload! (fn []
              (read-view! box session-id view-id))
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
  (when (:is-gap (held-view session-id view-id)) (read-view! box session-id view-id)))

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
   :reading (atom false)
   :on-change (or on-change (constantly nil))})

(defn- current-app-view
  [box session-id view-id]
  (or (newer @(:view box) (:view (held-view session-id view-id))) {:id view-id :nodes []}))

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
    {:init (component-initial-state close-label)
     :measure (fn [st cols rows]
                (measure st cols rows (current)))
     :measure-pane (fn [st left top width height]
                     (refresh-gap! box session-id view-id)
                     (pane-measure st left top width height (current)))
     :reconcile (partial reconcile-outcomes! box)
     :paint paint!
     :on-key (fn [st key geom]
               (if (= ::reveal key) {::dlg/done nil} (handle-key st key geom effects)))
     :read-key (partial read-component-key! box session-id view-id)}))

(defn show-app!
  "Open app `view-id` of `session-id`, or global app `view-id` without `session-id`,
   until the human closes it. `band` `{:left :width}` keeps the app in those screen
   columns; without it the app uses the whole terminal width. `close-label` names
   what Esc does."
  [^TerminalScreen screen session-id view-id & [{:keys [band close-label open-picture!]}]]
  (let [run! #(dlg/run-modal! screen
                              (component session-id
                                         view-id
                                         (:view (held-view session-id view-id))
                                         screen
                                         close-label
                                         {:open-picture! open-picture!}))]
    (if band
      (binding [frame/*column-offset* (long (:left band))
                dlg/*dialog-region* {:width (fn [cols _rows]
                                              (app-surfaces/band-width band cols))}]

        (run!))
      (run!))))

(defn show!
  "Open the app attachment `row` of `session-id` until the human closes it."
  [^TerminalScreen screen session-id row & [opts]]
  (if-let [view-id (model/row-view-id row)]
    (show-app! screen session-id view-id opts)
    (vis/notify! "This attachment names no app" :level :warn)))
