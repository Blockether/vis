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
            [com.blockether.vis.tui.app-model :as model]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.columns :as columns]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.input :as input]
            [com.blockether.vis.tui.live-view :as lv]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.state :as state]
            [com.blockether.vis.tui.theme :as t]
            [com.blockether.vis.tui.view-model :as view-model]
            [taoensso.telemere :as tel])
  (:import [com.googlecode.lanterna.input KeyStroke KeyType]
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
  [view node width folded]
  (cond (= :diff (:type node)) (model/diff-rows node width folded)
        (= :thread (:type node))
        (into [{:kind :thread-node
                :node-id (:id node)
                :thread-id (:id node)
                :text (or (:label node) "Discussion")
                :is-selectable true}]
              (model/thread-rows node width {:node-id (:id node) :thread-id (:id node)}))
        (and (seq (:children node)) (holds-threads? node))
        (into [] (mapcat #(node-rows view % width folded)) (:children node))
        :else (plan-rows view node width)))

(defn surface-rows
  "Every row of the app surface for `view`, `width` columns wide. `folded` holds
   the diff file paths whose lines are hidden."
  [view width folded]
  (let [receipts (model/receipt-rows view)]
    (cond-> (into [] (mapcat #(node-rows view % width folded)) (:nodes view))
      (seq receipts)
      (into (cons {:kind :receipts-head :text "Send receipts"} receipts)))))

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
  {:selected 0 :scroll 0 :hscroll 0 :folded #{} :editor nil :save nil :send nil})

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

(defn- press
  "Press the control `node-id` of `view` like Enter or a click: Send for a submit
   button, `activate` for a button or a card, `select` for table row `item-id`,
   and a value dialog for an input. An app without its owner takes only a Send."
  [state view node-id item-id {:keys [activate! select! edit!] :as effects}]
  (let [node (model/find-node view node-id)]
    (cond (nil? node) (assoc state :notice (str "The app has no control " node-id))
          (and (= :button (:type node)) (:is-disabled node)) (assoc state
                                                               :notice "That button is disabled")
          (and (= :button (:type node)) (= :submit (keyword (:action node))))
          (send-app state view node-id effects)
          (model/stale? view) (assoc state :notice disconnected-notice)
          (#{:button :card} (:type node)) (do (activate! {:action :activate :node-id node-id})
                                              state)
          (and item-id (= :table (:type node)))
          (do (select! {:action :select :node-id node-id :item-ids [item-id]}) state)
          (lv/input-node {:view view} node-id) (do (edit! node) state)
          :else state)))

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

(defn- surface-key
  "The surface's own meaning of `key`, without app shortcuts."
  [state ^KeyStroke key {:keys [view rows list-h] :as geom} {:keys [reload!] :as effects}]
  (let [read-only?
        (model/read-only? view)

        kt
        (.getKeyType key)

        ch
        (when (= KeyType/Character kt) (.getCharacter key))

        row
        (nth rows (:selected state) nil)]

    (cond (.isCtrlDown key) state
          (= KeyType/Escape kt) {::dlg/done nil}
          (= KeyType/ArrowUp kt) (move geom state -1)
          (= KeyType/ArrowDown kt) (move geom state 1)
          (= KeyType/PageUp kt) (move geom state (- (max 1 (long list-h))))
          (= KeyType/PageDown kt) (move geom state (max 1 (long list-h)))
          (= KeyType/Home kt) (move geom state (- (count rows)))
          (= KeyType/End kt) (move geom state (count rows))
          (= KeyType/ArrowLeft kt) (fold geom state)
          (= KeyType/ArrowRight kt) (unfold geom state)
          (= \< ch) (update state :hscroll #(max 0 (- (long %) 8)))
          (= \> ch) (update state :hscroll #(+ (long %) 8))
          (= \r ch) (do (reload!) state)
          (and (= KeyType/Enter kt) (= :file (:kind row)))
          (update state
                  :folded
                  #(if (contains? % (:path row)) (disj % (:path row)) (conj % (:path row))))
          (and (or (= KeyType/Enter kt) (= \s ch)) read-only?) (assoc state :save {:state :closed})
          (and (= KeyType/Enter kt) (= :plan (:kind row)))
          (let [entry (focused-entry row (:cell state))]
            (press state view (:node-id entry) (:item-id entry) effects))
          (= KeyType/Enter kt) (if-let [target (editor-target view row)]
                                 (open-editor state target)
                                 state)
          (= \s ch) (send-app state view nil effects)
          :else state)))

(defn handle-key
  "The next surface state for `key`. `geom` holds `:rows`, `:view` and `:list-h`;
   `effects` holds `:save!`, `:send!`, `:activate!`, `:select!`, `:edit!`,
   `:shortcut!` and `:reload!`. Answers `{::dlg/done nil}` to close.

   App shortcuts come before the surface keys. Escape, Tab and every Ctrl key keep
   their surface meaning. After C-x, the next key has its surface meaning, so a
   verb that a shortcut hides stays reachable: C-x s sends and C-x r reloads."
  [state key {:keys [view] :as geom} {:keys [paste] :as effects}]
  (cond (and (map? key) (:paste key))
        (if (:editor state)
          (update-in state [:editor :input] input/paste-text (paste (:paste key)))
          state)
        (not (instance? KeyStroke key)) state
        (:editor state) (editor-key state key view effects)
        :else (let [^KeyStroke key
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
                              (surface-key state key geom effects))))))

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

(defn row-text
  "The text a row paints, `w` columns wide, with code rows shifted by `hscroll`."
  [row hscroll w]
  (let [{:keys [gutter body]}
        (model/row-parts row)

        body
        (if (and (scrolling-kinds (:kind row)) (pos? (long hscroll)))
          (let [n (count body)]
            (subs body (min n (long hscroll))))
          body)]

    (p/ellipsize (str gutter body) (max 1 (long w)))))

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

(defn- paint-row!
  [g left row-y inner-w view-id selected? cell hscroll row]
  (if (= :plan (:kind row))
    (do (lv/paint-plan-entry! g left row-y inner-w view-id (:entry row))
        (when (and selected? (:is-selectable row))
          (paint-row-focus! g left row-y inner-w row cell)))
    (let [text (row-text row hscroll (- (long inner-w) 3))]
      (if selected?
        (dlg/draw-selectable-row! g left row-y inner-w true text)
        (do (p/set-colors! g (row-ink row) t/dialog-bg)
            (p/fill-rect! g (inc (long left)) row-y inner-w 1)
            (p/put-str! g (+ (long left) 2) row-y text))))))

(defn- hidden-key
  "The footer key of surface verb `k` with key name `named`: `C-x k` when an app
   shortcut takes that key, else `k`."
  [view k named]
  (if (shortcut-for view named) (str "C-x " k) k))

(defn- footer
  [state view rows]
  (let [row
        (nth rows (:selected state) nil)

        enter
        (cond (= :file (:kind row)) "fold"
              (= :plan (:kind row))
              (if (= :field (:kind (focused-entry row (:cell state)))) "edit" "press")
              (editor-target view row) "comment")

        verbs
        (cond-> [[(hidden-key view "↑/↓" "ArrowUp") "move"]
                 [(hidden-key view "←/→" "ArrowLeft") "fold"]
                 [(hidden-key view "</>" "<") "scroll"]]
          (and enter (not (model/read-only? view)))
          (conj [(hidden-key view "Enter" "Enter") enter])

          (not (model/read-only? view))
          (conj [(hidden-key view "s" "s") "send"])

          true
          (conj [(hidden-key view "r" "r") "reload"] ["Esc" "close"]))]

    (cond (:editor state) [["Ctrl+S" "save"] ["Esc" "cancel"]]
          (:prefix state) [["C-x" "next key is a surface key"] ["s" "send"] ["r" "reload"]
                           ["Esc" "cancel"]]
          :else verbs)))

(def ^:private editor-h "Rows of the editor field." 5)

(defn- title
  [view]
  (str (or (not-empty (str (:title view))) "App")
       "  ·  "
       (get model/status-label (model/status view))
       (when-let [n (:seq view)]
         (str "  ·  state " n))))

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

(defn measure
  "The surface geometry for `state` in a `cols`x`rows` terminal."
  [state cols rows-count view]
  (let [content-w
        (dlg/default-content-width cols)

        bounds
        (in-app-region #(dlg/dialog-bounds cols rows-count content-w (app-content-h rows-count)))

        {:keys [content-top content-h hint-row]}
        (dlg/dialog-layout bounds)

        width
        (max 20 (- (long (:inner-w bounds)) 3))

        rows
        (surface-rows view width (:folded state))

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
     :list-h (max 1 (- (long content-h) 1 (if legend 1 0) editor-rows))}))

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

(defn- paint!
  [g state
   {:keys [cols rows-count content-w bounds content-top content-h hint-row view rows list-h
           editor-rows legend]}]
  (let [{:keys [left inner-w]}
        bounds

        left
        (long left)

        list-h
        (long list-h)

        top
        (long content-top)]

    (in-app-region #(dlg/draw-dialog-chrome! g
                                             cols
                                             rows-count
                                             (title view)
                                             content-w
                                             (app-content-h rows-count)))
    (p/set-colors! g t/dialog-fg t/dialog-bg)
    (p/fill-rect! g (inc left) top inner-w content-h)
    (if (empty? rows)
      (do (p/set-colors! g t/dialog-hint t/dialog-bg)
          (p/put-str! g (+ left 2) top "This app shows nothing yet"))
      (dotimes [i (min list-h (- (count rows) (long (:scroll state))))]
        (let [idx (+ (long (:scroll state)) i)]
          (paint-row! g
                      left
                      (+ top i)
                      inner-w
                      (:id view)
                      (= idx (:selected state))
                      (:cell state)
                      (:hscroll state)
                      (nth rows idx)))))
    (when legend (dlg/draw-hint-bar! g left (+ top list-h) inner-w legend))
    (let [status-y (+ top list-h (if legend 1 0))]
      (p/set-colors! g
                     (if (#{:offline :conflict :failed :closed}
                          (or (get-in state [:save :state]) (get-in state [:send :state])))
                       t/footer-warning-fg
                       t/dialog-hint)
                     t/dialog-bg)
      (p/put-str! g
                  (+ left 2)
                  status-y
                  (p/ellipsize (status-line state) (max 1 (- (long inner-w) 3)))))
    (let [cursor (when-let [{:keys [target input]} (:editor state)]
                   (let [ed-top (+ top list-h 1 (if legend 1 0))
                         w (max 1 (- (long inner-w) 3))
                         lines (:lines input)
                         first-line (max 0 (- (long (:crow input)) (dec (long editor-h))))]

                     (p/set-colors! g t/dialog-hint-key t/dialog-bg)
                     (p/put-str! g (+ left 2) ed-top (p/ellipsize (target-label target) w))
                     (p/set-colors! g t/code-block-fg t/code-block-bg)
                     (p/fill-rect! g (+ left 2) (inc ed-top) w editor-h)
                     (dotimes [i editor-h]
                       (when-let [line (nth lines (+ first-line (long i)) nil)]
                         (p/put-str! g (+ left 2) (+ ed-top 1 (long i)) (p/ellipsize line w))))
                     (when (< (long editor-rows) (long content-h))
                       (p/cursor-pos (+ left 2 (min (long (:ccol input)) (dec w)))
                                     (+ ed-top 1 (- (long (:crow input)) first-line))))))]
      (dlg/draw-hint-bar! g left hint-row inner-w (footer state view rows))
      cursor)))

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

(defn- held-view [view-id] (get-in @state/app-db [:app-views view-id]))

(defn- read-view!
  "Read the current state of the app and give it to the session state."
  [box session-id view-id]
  (when (compare-and-set! (:reading box) false true)
    (vis/worker-future
      "vis-app-read"
      (fn []
        (try (when-let [view (view-model/live-view<-wire (vis/gateway-live-view-state session-id
                                                                                      view-id))]
               (swap! (:view box) newer view)
               (state/dispatch [:app-view-loaded view]))
             (catch Throwable e
               (tel/log! {:level :warn :id ::app-read-failed :error e :data {:view-id view-id}}))
             (finally (reset! (:reading box) false)))))))

(defn- run-action!
  "Post `action` on a worker and hand the outcome to `on-outcome`."
  [box session-id view-id action on-outcome]
  (vis/worker-future "vis-app-action"
                     (fn []
                       (let [outcome (vis/gateway-app-action! session-id view-id action)]
                         (swap! (:outcomes box) conj [on-outcome outcome])
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

(defn component
  "The `dlg/run-modal!` component of the app `view-id` of `session-id`. `screen`
   runs the value dialog of an input; without it an input is read-only."
  [session-id view-id first-view & [screen]]
  (let [box
        {:view (atom first-view) :outcomes (atom []) :reading (atom false)}

        current
        (fn []
          (newer @(:view box) (:view (held-view view-id))))

        effects
        {:save! (fn [action editor]
                  (let [body (not-empty (str/trim (input/input->text (:input editor))))]
                    (run-action! box
                                 session-id
                                 view-id
                                 action
                                 (fn [st outcome]
                                   (let [save (saved (:save st) outcome)]
                                     (cond-> (assoc st :save save)
                                       (and (= :saved (:state save)) (:editor st))
                                       (update-in [:editor :target]
                                                  #(cond-> (assoc % :draft body) (:thread-id save)
                                                     (assoc :thread-id (:thread-id save))))))))))
         :send! (fn [action node-id]
                  (run-action! box
                               session-id
                               view-id
                               (cond-> action
                                 node-id
                                 (assoc :node-id node-id))
                               (fn [st outcome]
                                 (assoc st :send (sent (:send st) outcome)))))
         :activate! (fn [action]
                      (run-action! box session-id view-id action noticed))
         :select! (fn [action]
                    (run-action! box session-id view-id action noticed))
         :edit! (fn [node]
                  (if-let [{:keys [value]} (some-> screen
                                                   (dlg/live-input-value! node))]
                    (run-action! box
                                 session-id
                                 view-id
                                 {:action :change :node-id (:id node) :value value}
                                 noticed)
                    (when-not screen
                      (swap! (:outcomes box) conj
                        [(fn [st _]
                           (assoc st :notice "This input is read-only here")) nil]))))
         :shortcut! (fn [action s]
                      (run-action! box
                                   session-id
                                   view-id
                                   action
                                   (fn [st outcome]
                                     (assoc st
                                       :notice (or (refusal-notice outcome)
                                                   (str (shortcut-label s) " ✓"))))))
         :reload! (fn []
                    (read-view! box session-id view-id))
         :paste identity}]

    (when-not first-view (read-view! box session-id view-id))
    {:init (initial-state)
     :measure (fn [st cols rows]
                (measure st cols rows (or (current) {:id view-id :nodes []})))
     :reconcile (fn [st geom]
                  (let [outcomes (first (reset-vals! (:outcomes box) []))]
                    (reconcile (reduce (fn [acc [f outcome]]
                                         (f acc outcome))
                                       st
                                       outcomes)
                               geom)))
     :paint paint!
     :on-key (fn [st key geom]
               (handle-key st key geom effects))
     :read-key (fn [^TerminalScreen screen]
                 (when (:is-gap (held-view view-id)) (read-view! box session-id view-id))
                 (if (dlg/modal-input-pending? screen)
                   (let [key (dlg/read-modal-key! screen)]
                     (if (and key (= KeyType/PasteStart (.getKeyType ^KeyStroke key)))
                       {:paste (dlg/drain-modal-paste! screen)}
                       key))
                   ;; A moving scene paints about 60 times a second, and a key waits less.
                   (do (Thread/sleep (if (lv/scene-moving?) 12 40)) nil)))}))

(defn show!
  "Open the app attachment `row` of `session-id` until the human closes it."
  [^TerminalScreen screen session-id row]
  (if-let [view-id (model/row-view-id row)]
    (dlg/run-modal! screen (component session-id view-id (:view (held-view view-id)) screen))
    (vis/notify! "This attachment names no app" :level :warn)))
