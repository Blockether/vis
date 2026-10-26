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

(defn- plan-rows
  "Rows of a node that the live view paints: one `:plan` row for each plan entry."
  [view node width]
  (mapv (fn [entry]
          {:kind :plan
           :node-id (:node-id entry)
           :entry entry
           :text (:text entry)
           :is-selectable (= :button (:kind entry))})
        (lv/plan (lv/opened (assoc view
                              :nodes [node]
                              :description nil))
                 width)))

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

(defn- move
  [{:keys [rows]} state delta]
  (let [stops
        (selectable-indexes rows)

        at
        (or (some (fn [[k i]]
                    (when (= i (:selected state)) k))
                  (map-indexed vector stops))
            0)

        k
        (p/clamp (+ (long at) (long delta)) 0 (max 0 (dec (count stops))))]

    (assoc state :selected (nth stops k (:selected state)))))

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

(defn- activate
  [state view row {:keys [send! activate!]}]
  (let [node (model/find-node view (:node-id row))]
    (if (and (= :button (:type node)) (= :submit (keyword (:action node))))
      (let [k (send-key (:send state) model/idempotency-key)]
        (send! (model/submit-action view k nil) (:id node))
        (assoc state :send {:state :sending :key k}))
      (do (activate! {:action :activate :node-id (:node-id row)}) state))))

(defn handle-key
  "The next surface state for `key`. `geom` holds `:rows`, `:view` and `:list-h`;
   `effects` holds `:save!`, `:send!`, `:activate!` and `:reload!`. Answers
   `{::dlg/done nil}` to close."
  [state key {:keys [view rows list-h] :as geom} {:keys [send! reload! paste] :as effects}]
  (let [read-only? (model/read-only? view)]
    (cond (and (map? key) (:paste key))
          (if (:editor state)
            (update-in state [:editor :input] input/paste-text (paste (:paste key)))
            state)
          (not (instance? KeyStroke key)) state
          (:editor state) (editor-key state key view effects)
          :else
          (let [^KeyStroke key key
                kt (.getKeyType key)
                ch (when (= KeyType/Character kt) (.getCharacter key))
                row (nth rows (:selected state) nil)]

            (cond (= KeyType/Escape kt) {::dlg/done nil}
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
                  (and (or (= KeyType/Enter kt) (= \s ch)) read-only?) (assoc state
                                                                         :save {:state :closed})
                  (and (= KeyType/Enter kt) (= :plan (:kind row))) (activate state view row effects)
                  (= KeyType/Enter kt) (if-let [target (editor-target view row)]
                                         (open-editor state target)
                                         state)
                  (= \s ch) (let [k (send-key (:send state) model/idempotency-key)]
                              (send! (model/submit-action view k nil) nil)
                              (assoc state :send {:state :sending :key k}))
                  :else state)))))

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
  [{:keys [save send]}]
  (str/join "  ·  "
            (remove nil?
              [(case (:state save)
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

(defn- paint-row!
  [g left row-y inner-w view-id selected? hscroll row]
  (if (= :plan (:kind row))
    (lv/paint-plan-entry! g left row-y inner-w view-id (:entry row))
    (let [text (row-text row hscroll (- (long inner-w) 3))]
      (if selected?
        (dlg/draw-selectable-row! g left row-y inner-w true text)
        (do (p/set-colors! g (row-ink row) t/dialog-bg)
            (p/fill-rect! g (inc (long left)) row-y inner-w 1)
            (p/put-str! g (+ (long left) 2) row-y text))))))

(defn- footer
  [state view]
  (cond (:editor state) [["Ctrl+S" "save"] ["Esc" "cancel"]]
        (model/read-only? view) [["↑/↓" "move"] ["←/→" "fold"] ["</>" "scroll"] ["r" "reload"]
                                 ["Esc" "close"]]
        :else [["↑/↓" "move"] ["←/→" "fold"] ["</>" "scroll"] ["Enter" "comment"] ["s" "send"]
               ["r" "reload"] ["Esc" "close"]]))

(def ^:private editor-h "Rows of the editor field." 5)

(defn- title
  [view]
  (str (or (not-empty (str (:title view))) "App")
       "  ·  "
       (get model/status-label (model/status view))
       (when-let [n (:seq view)]
         (str "  ·  state " n))))

(defn measure
  "The surface geometry for `state` in a `cols`x`rows` terminal."
  [state cols rows-count view]
  (let [content-w
        (dlg/default-content-width cols)

        bounds
        (dlg/dialog-bounds cols rows-count content-w (max 8 (- (long rows-count) 6)))

        {:keys [content-top content-h hint-row]}
        (dlg/dialog-layout bounds)

        width
        (max 20 (- (long (:inner-w bounds)) 3))

        rows
        (surface-rows view width (:folded state))

        editor-rows
        (if (:editor state) (+ 2 (long editor-h)) 0)]

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
     :list-h (max 1 (- (long content-h) 1 editor-rows))}))

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

        list-h
        (long list-h)

        scroll
        (-> (long (:scroll state))
            (min selected)
            (max (inc (- selected list-h)))
            (p/clamp 0 (max 0 (- (count rows) list-h))))]

    (assoc state
      :selected selected
      :scroll scroll)))

(defn- paint!
  [g state
   {:keys [cols rows-count content-w bounds content-top content-h hint-row view rows list-h
           editor-rows]}]
  (let [{:keys [left inner-w]}
        bounds

        left
        (long left)

        list-h
        (long list-h)

        top
        (long content-top)]

    (dlg/draw-dialog-chrome! g cols rows-count (title view) content-w (:inner-h bounds))
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
                      (:hscroll state)
                      (nth rows idx)))))
    (let [status-y (+ top list-h)]
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
                   (let [ed-top (+ top list-h 1)
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
      (dlg/draw-hint-bar! g left hint-row inner-w (footer state view))
      cursor)))

;;; ── Driver ─────────────────────────────────────────────────────────────────

(defn- newer
  [a b]
  (cond (nil? a) b
        (nil? b) a
        (> (long (or (:seq b) 0)) (long (or (:seq a) 0))) b
        (and (:is-closed b) (not (:is-closed a))) b
        :else a))

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

(defn component
  "The `dlg/run-modal!` component of the app `view-id` of `session-id`."
  [session-id view-id first-view]
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
                      (run-action! box
                                   session-id
                                   view-id
                                   action
                                   (fn [st _]
                                     st)))
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
                   (do (Thread/sleep 40) nil)))}))

(defn show!
  "Open the app attachment `row` of `session-id` until the human closes it."
  [^TerminalScreen screen session-id row]
  (if-let [view-id (model/row-view-id row)]
    (dlg/run-modal! screen (component session-id view-id (:view (held-view view-id))))
    (vis/notify! "This attachment names no app" :level :warn)))
