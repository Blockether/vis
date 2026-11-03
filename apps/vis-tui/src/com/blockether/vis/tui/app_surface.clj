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
  (:import [com.googlecode.lanterna TerminalPosition TextColor$RGB]
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
  (cond (= :diff (:type node)) (model/diff-rows node width folded opts)
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

(defn surface-rows
  "Every row of the app surface for `view`, `width` columns wide. `folded` holds
   the diff file paths whose lines are hidden. `opts` holds the diff `:unified?`
   and `:wrap?` choices."
  ([view width folded] (surface-rows view width folded nil))
  ([view width folded opts]
   (let [receipts (model/receipt-rows view)]
     (cond-> (into [] (mapcat #(node-rows view % width folded opts)) (:nodes view))
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
   :unified? false
   :wrap? false
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

(defn- editor-key
  [state ^KeyStroke key view {:keys [save!]}]
  (let [kt
        (.getKeyType key)

        ed
        (fn [f]
          (update-in state [:editor :input] f))]

    (cond (= KeyType/Escape kt) (assoc state :editor nil)
          (and (= KeyType/Character kt) (.isCtrlDown key) (#{\s \S} (.getCharacter key)))
          (let [{:keys [action skip]} (save-request view (:editor state))]
            (cond skip (assoc state
                         :editor nil
                         :save {:state :skipped :message skip})
                  action (do (save! action (:editor state))
                             (assoc state
                               :save {:state :saving :target (get-in state [:editor :target])}))
                  :else state))
          (= KeyType/Character kt) (if (or (.isCtrlDown key) (.isAltDown key))
                                     state
                                     (ed #(input/insert-char % (.getCharacter key))))
          (= KeyType/Enter kt) (ed input/insert-newline)
          (= KeyType/Backspace kt) (ed input/delete-backward)
          (= KeyType/Delete kt) (ed input/delete-forward)
          (= KeyType/ArrowLeft kt) (ed input/move-left)
          (= KeyType/ArrowRight kt) (ed input/move-right)
          (= KeyType/ArrowUp kt) (ed input/move-up)
          (= KeyType/ArrowDown kt) (ed input/move-down)
          (= KeyType/Home kt) (ed input/move-line-start)
          (= KeyType/End kt) (ed input/move-line-end)
          :else state)))

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
  [state view node item-id {:keys [activate! select! edit!]}]
  (let [node-id (:id node)]
    (cond (#{:button :card} (:type node)) (do (activate! {:action :activate :node-id node-id})
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
  [s]
  (or (not-empty (str (:label s))) (:action s) (:node-id s)))

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
    (let [pairs (merged-pairs (mapv (fn [s]
                                      [(get key-glyphs (:key s) (:key s)) (shortcut-label s)])
                                    (:shortcuts view)))]
      (if (model/stale? view)
        (str "Disconnected: the controls wait for the owner of the app"
             (when (seq pairs) (str "  ·  " (str/join "  ·  " (map #(str/join " " %) pairs)))))
        (not-empty pairs)))))

(defn- press-shortcut
  [state view s {:keys [shortcut!] :as effects}]
  (cond (model/stale? view) (assoc state :notice disconnected-notice)
        (:node-id s) (press state view (:node-id s) nil effects)
        :else (do (shortcut! {:action :shortcut :key (:key s)} s)
                  (assoc state :notice (str (shortcut-label s) " …")))))

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

(defn- enter-row
  [state {:keys [view rows]} effects]
  (let [row (nth rows (:selected state) nil)]
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

(defn- character-key
  [state ch geom effects]
  (case ch
    (\< \>)
    (horizontal-key state (if (= \< ch) -8 8))

    (\u \w)
    (diff-mode-key state geom (if (= \u ch) :unified? :wrap?))

    \v
    (compare-key state geom)

    \s
    (send-key-action state geom effects)

    \r
    (do ((:reload! effects)) state)

    \l
    (do (when (seq (:log (:view geom))) ((:log! effects))) state)

    state))

(defn- surface-key
  "The surface's own meaning of `key`, without app shortcuts."
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
          :else (condp = kt
                  KeyType/Escape {::dlg/done nil}
                  KeyType/ArrowLeft (fold geom state)
                  KeyType/ArrowRight (unfold geom state)
                  KeyType/Enter (enter-row state geom effects)
                  KeyType/Character (character-key state (.getCharacter key) geom effects)
                  state))))

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
          :else (if-let [s (shortcut-for view (shortcut-key key))]
                  (press-shortcut state view s effects)
                  (surface-key state key geom effects)))))

(defn- pasted-key? [key] (and (map? key) (:paste key)))

(defn- paste-key
  [state text paste]
  (if (:editor state) (update-in state [:editor :input] input/paste-text (paste text)) state))

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
        (instance? MouseAction key) state
        (:editor state) (editor-key state key view effects)
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
          (str gutter (p/pad-right (p/ellipsize body room) (+ room 2)) counts)
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

(def ^:private add-paper (TextColor$RGB. 2 17 11))

(def ^:private del-paper (TextColor$RGB. 23 7 5))

(def ^:private add-ink (TextColor$RGB. 7 196 128))

(def ^:private del-ink (TextColor$RGB. 255 46 63))

(def ^:private add-word
  "T3 word highlight: 20% of rgb(5,195,130) over the added paper."
  (TextColor$RGB. 3 53 35))

(def ^:private del-word
  "T3 word highlight: 20% of rgb(255,45,65) over the removed paper."
  (TextColor$RGB. 69 15 17))

(def ^:private code-ink "T3 code text on a dark tinted paper." (TextColor$RGB. 250 250 250))

(def ^:private muted-ink "T3 secondary text on a dark paper." (TextColor$RGB. 155 155 155))

(defn row-paper
  "The paper of a diff row, as T3 tints it: green for an added line, red for a removed
   one, else the dialog paper."
  [row]
  (case (:kind row)
    :add
    add-paper

    :del
    del-paper

    nil))

(defn- code-fg
  "The code ink of a diff row: light on the dark tinted paper, else the dialog ink."
  [row]
  (if (row-paper row) code-ink t/dialog-fg))

(defn row-inks
  "The colors of one diff code row: `:paper`, the change `:bar` (nil for an unchanged
   line), the `:gutter` ink of the line numbers and the `:body` ink of the code."
  [row]
  (let [change (case (:kind row)
                 :add add-ink
                 :del del-ink
                 nil)]
    {:paper (or (row-paper row) t/dialog-bg)
     :bar change
     :gutter (or change t/dialog-hint)
     :body (code-fg row)}))

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
      (p/set-colors! g code-ink (if (= :add (:kind row)) add-word del-word))
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

(def ^:private thread-paper (TextColor$RGB. 23 23 23))

(def ^:private card-paper (TextColor$RGB. 24 27 32))

(def ^:private card-edge (TextColor$RGB. 40 69 142))

(defn- paint-thread-row!
  "One thread row as a T3 comment card: a blue edge on the rail and the text on the
   card paper, inside a darker row."
  [g left row-y inner-w row text]
  (let [rail
        (count "    ")

        x
        (+ (long left) 2 rail)

        body
        (subs text (min (count text) (+ rail 2)))]

    (p/set-colors! g t/dialog-fg thread-paper)
    (p/fill-rect! g (inc (long left)) row-y inner-w 1)
    (p/set-colors! g t/dialog-fg card-paper)
    (p/fill-rect! g x row-y (max 0 (- (long inner-w) rail 2)) 1)
    (p/set-colors! g card-edge card-paper)
    (p/put-str! g x row-y "┃")
    (p/set-colors! g
                   (if (#{:thread-head :message-head :draft-head} (:kind row)) muted-ink code-ink)
                   card-paper)
    (p/put-str! g (+ x 2) row-y body)))

(defn- row-ink
  [row]
  (case (model/row-tone row)
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

      t/dialog-fg)))

(defn- paint-focus!
  "Mark the control `entry` that the cursor is on: its text takes the accent
   fill, the colour of a focused button. A control without text gets a mark."
  [g left row-y inner-w entry]
  (let [indent
        (min (long (or (:indent entry) 0)) (max 0 (long inner-w)))

        left
        (+ (long left) indent)

        text
        (str (:text entry))]

    (p/set-colors! g t/header-active-tab-fg t/header-active-tab-bg)
    (if (str/blank? text)
      (p/put-str! g (inc left) row-y "▸")
      (p/styled
        g
        [p/BOLD]
        (p/put-str! g (+ left 2) row-y (p/ellipsize text (max 1 (- (long inner-w) indent 3))))))))

(defn- paint-row-focus!
  "Mark the focused control of plan `row`: the whole entry, or cell `cell` at
   the place where the live view paints that cell."
  [g left row-y inner-w row cell]
  (let [entry (:entry row)]
    (if (and (some? cell) (seq (:cells row)))
      (let [indent (min (long (or (:indent entry) 0)) (max 0 (long inner-w)))
            inner-w (- (long inner-w) indent)
            places (if-let [ws (:cell-widths entry)]
                     (columns/width-slots inner-w ws)
                     (columns/slots inner-w (count (:cells entry))))
            [x w] (nth places cell nil)]

        (when (and x (pos? (long w)))
          (paint-focus! g (+ (long left) indent (long x)) row-y w (nth (:cells entry) cell))))
      (paint-focus! g left row-y inner-w entry))))

(defn- paint-plain-row!
  [g left row-y inner-w row text]
  (p/set-colors! g (row-ink row) t/dialog-bg)
  (p/fill-rect! g (inc (long left)) row-y inner-w 1)
  (p/put-str! g (+ (long left) 2) row-y text))

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
               {:split #(paint-split-row! g left row-y inner-w row hscroll)})]

    ((get paints (:kind row) #(paint-plain-row! g left row-y inner-w row text)))))

(defn- paint-selected-plan!
  [g left row-y inner-w row cell]
  (when (:is-selectable row) (paint-row-focus! g left row-y inner-w row cell)))

(defn- paint-plan-row!
  [g left row-y inner-w view-id selected? cell row]
  (lv/paint-plan-entry! g left row-y inner-w view-id (:entry row))
  (when selected? (paint-selected-plan! g left row-y inner-w row cell)))

(defn- paint-source-row!
  [g left row-y inner-w selected? hscroll row]
  (if selected?
    (dlg/draw-selectable-row! g
                              left
                              row-y
                              inner-w
                              true
                              (str " " (row-text row hscroll (- (long inner-w) 3))))
    (paint-text-row! g left row-y inner-w hscroll row)))

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

(defn- review-footer
  [state view rows row]
  (cond-> []
    (seq (:log view))
    (conj [(hidden-key view "l" "l") "log"])

    (compare-image view row)
    (conj [(hidden-key view "v" "v") "Before/After"])

    (some #(= :split (:kind %)) rows)
    (conj [(hidden-key view "u" "u") "unified"])

    (and (diff-view? rows) (:unified? state))
    (conj [(hidden-key view "u" "u") "split"])

    (diff-view? rows)
    (conj [(hidden-key view "w" "w") (if (:wrap? state) "cut" "wrap")])))

(defn- footer
  [state view rows]
  (let [row
        (nth rows (:selected state) nil)

        enter
        (enter-label state view row)

        verbs
        (cond-> [[(hidden-key view "↑/↓" "ArrowUp") "move"]
                 [(hidden-key view "←/→" "ArrowLeft") "fold"]]
          (not (:wrap? state))
          (conj [(hidden-key view "</>" "<") "scroll"])

          (and enter (not (model/read-only? view)))
          (conj [(hidden-key view "Enter" "Enter") enter])

          (not (model/read-only? view))
          (conj [(hidden-key view "s" "s") "send"]))]

    (cond (:editor state) [["Ctrl+S" "save"] ["Esc" "cancel"]]
          (:prefix state) [["C-x" "next key is a surface key"] ["s" "send"] ["r" "reload"]
                           ["Esc" "cancel"]]
          :else (into (into verbs (review-footer state view rows row))
                      [[(hidden-key view "r" "r") "reload"]
                       ["Esc" (:close-label state "close")]]))))

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

        width
        (max 20 (- (long (:inner-w bounds)) 3))

        rows
        (surface-rows view width (:folded state) (select-keys state [:unified? :wrap?]))

        editor-rows
        (if (:editor state) (+ 2 (long editor-h)) 0)

        legend
        (shortcut-legend view)]

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
     :log-chip (log-chip bounds view)
     :list-h (max 1 (- (long content-h) 1 (if legend 1 0) editor-rows))}))

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
        (long height)]

    (assoc (geom-in state
                    (+ left width)
                    (+ top height)
                    width
                    {:left left
                     :top top
                     :right (+ left width -1)
                     :bottom (+ top height -1)
                     :inner-w (- width 2)
                     :inner-h (- height 2)}
                    view)
      :pane? true)))

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

(def ^:private pane-close-label " ✕ ")

(defn- paint-pane-chrome!
  "Paint the box of a side pane in `bounds`, its title and its close control."
  [g {:keys [left top right bottom inner-w]} view]
  (let [left
        (long left)

        top
        (long top)

        right
        (long right)

        close-w
        (long (p/display-width pane-close-label))

        close-col
        (- right close-w)]

    (p/set-bg! g t/dialog-bg)
    (p/fill-rect! g left top (inc (- right left)) (inc (- (long bottom) top)))
    (p/set-colors! g t/dialog-border t/dialog-bg)
    (p/draw-box! g left top (inc (- right left)) (inc (- (long bottom) top)))
    (p/set-colors! g t/dialog-title-bg t/dialog-bg)
    (p/styled g
              [p/BOLD]
              (p/put-str!
                g
                (+ left 2)
                top
                (str " " (p/ellipsize (title view) (max 0 (- (long inner-w) 6 close-w))) " ")))
    (when (< (+ left 2) close-col)
      (p/set-colors! g t/dialog-title-bg t/dialog-title-fg)
      (p/put-str! g close-col top pane-close-label)
      (.register interactions/hit-map
                 {:bounds {:row top :col close-col :width close-w}
                  :kind :app-pane-close
                  :label "Close the app"
                  :enabled? true}))))

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
  [g {:keys [bounds view pane? cols rows-count content-w]}]
  (if pane?
    (paint-pane-chrome! g bounds view)
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
                  (nth rows idx)))))

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

(defn- legend-height [legend] (if legend 1 0))

(defn- status-state [state] (or (get-in state [:save :state]) (get-in state [:send :state])))

(defn- status-ink
  [state]
  (if (#{:offline :conflict :failed :closed} (status-state state))
    t/footer-warning-fg
    t/dialog-hint))

(defn- paint-legend!
  [g left row inner-w legend]
  (when legend (dlg/draw-hint-bar! g left row inner-w legend)))

(defn- paint-status!
  [g state {:keys [bounds content-top list-h legend]}]
  (let [{:keys [left inner-w]}
        bounds

        left
        (long left)

        row
        (+ (long content-top) (long list-h))]

    (paint-legend! g left row inner-w legend)
    (p/set-colors! g (status-ink state) t/dialog-bg)
    (p/put-str! g
                (+ left 2)
                (+ row (long (legend-height legend)))
                (p/ellipsize (status-line state) (max 1 (- (long inner-w) 3))))))

(defn- editor-geometry
  "The editor window keeps its cursor line visible without leaving the app content."
  [{:keys [input]} {:keys [bounds content-top list-h legend editor-rows content-h]}]
  (let [left
        (long (:left bounds))

        w
        (max 1 (- (long (:inner-w bounds)) 3))

        top
        (+ (long content-top) (long list-h) 1 (long (legend-height legend)))

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
                        (dlg/fit-hint-keeping-last (footer state view rows)
                                                   (max 0 (- (long inner-w) 2))))
    cursor))

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

(defn- read-component-key!
  [box session-id view-id ^TerminalScreen screen]
  (refresh-gap! box session-id view-id)
  (if (dlg/modal-input-pending? screen) (modal-key! screen) (idle-paint!)))

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
               (handle-key st key geom effects))
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
