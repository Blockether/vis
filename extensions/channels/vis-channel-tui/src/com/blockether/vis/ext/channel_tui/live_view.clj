(ns com.blockether.vis.ext.channel-tui.live-view
  "TUI painting for a LIVE VIEW — the picture an extension streams WHILE it works
   (`com.blockether.vis.internal.human-input`, materialized by
   `com.blockether.vis.internal.human-input.live`).

   A form is a QUESTION and owns the keyboard until it is answered; a view is a
   PICTURE and owns nothing. The composer keeps focus while a view paints, the
   wheel over the band scrolls it, a click opens a link or expands a node, and
   the only key it takes is Escape — which ARMS a stop on the newest open view,
   before it interrupts the turn: the band then takes one line for the comment the
   human types, Enter interrupts with it, Escape keeps watching. A view is ALWAYS
   stoppable; the note is what says why.

   Everything except [[paint!]] is PURE: [[opened]] builds the pane from the
   engine's own materialized view, [[patched]] is the reducer over one patch,
   [[plan]] is the paint plan, [[offset]] says where the viewport sits and
   [[painted]] takes back what the paint measured. Lanterna shows up in [[paint!]]
   alone, so the whole interaction is testable without a terminal.

   ONE SCROLL SURFACE. A view is a STACK of labelled nodes in declaration order,
   so three tables and two logs read as sections of one document instead of as
   competing panes. What the eye is on is an ANCHOR — the node id, and inside a
   keyed node the item id, of the row at the TOP of the viewport — never a line
   offset: rows arriving above it move the scrollbar, not the reading position.
   A viewport parked at the end FOLLOWS new rows; scrolling up releases the
   follow and landing back on the last row re-arms it.

   EVERY NODE PAINTS A WINDOW, never its record — [[node-window]] item rows and
   one line saying how many more, which a click expands. The record is what the
   sink keeps and what the model reads; a pane that painted five thousand rows
   would bury the four nodes under it.

   SEVERAL VIEWS AT ONCE: the newest paints in full and every older one keeps ONE
   collapsed line above it. A band shares its rows with the transcript and the
   prompt, so three full panes would leave none of them readable — and the
   collapsed line still carries the title, the state, and the fact that Escape
   hits the newest one first."
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.click-regions :as cr]
            [com.blockether.vis.ext.channel-tui.dialogs :as dialogs]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.scrollbar :as scrollbar]
            [com.blockether.vis.ext.channel-tui.theme :as t]
            [com.blockether.vis.ext.channel-tui.transient :as tr]
            [com.blockether.vis.internal.human-input.live :as live]
            [com.blockether.vis.internal.human-input.spec :as hi-spec]))

(set! *warn-on-reflection* true)

(def node-window
  "Item rows ONE node paints before it says how many more it holds. A view is a
   picture, not a spreadsheet: the log AND the table under it have to reach the
   same screen, and the record keeps every row either way. A click on the
   `+ N more` line expands the node it sits on."
  12)

(def ^:private bar-w
  "Cells the progress track gets — the same bar the tasks overlay paints, so one
   product has one progress bar."
  14)

(def ^:private empty-text
  "What each node type says when it holds nothing — the same words the model's
   document prints (`live/->markdown`), without the markdown that carries them
   there."
  {:stat "nothing counted yet"
   :steps "no steps yet"
   :log "no output yet"
   :table "no rows yet"
   :link "no links"})

(def ^:private tone-glyph
  "One cell that says the tone WITHOUT colour, for a terminal that has none, a
   screenshot in review, and a human who does not see red."
  {:ok "✓" :error "✗" :warn "!" :running "▸" :idle "·"})

(defn- tone-fg
  "The ink one tone wears. The vocabulary is the engine's own closed `live-tones`,
   so a colour the phone paints is a colour the terminal paints."
  ([tone] (tone-fg tone t/dialog-fg))
  ([tone default]
   (case tone
     :ok
     t/status-ok

     :warn
     t/footer-warning-fg

     :error
     t/footer-error-fg

     :running
     t/header-active-tab-accent

     :idle
     t/dialog-hint

     default)))

(defn- clamp ^long [^long v ^long lo ^long hi] (max lo (min hi v)))

(defn- flat-text
  "One cell's worth of text: a newline would eat the row under it and padding is
   the rail's job."
  [x]
  (-> (str x)
      (str/replace #"\s*\n\s*" " ")
      str/trim))

(defn elapsed-text
  "How long the view has been open, the way a person reads a duration — `13s`,
   `1m 12s`, `2h 4m`. It rides the band's own rule beside the title, because the
   first question about a running thing is how long it has been running."
  [ms]
  (let
    [secs
     (quot (max 0 (long ms)) 1000)

     mins
     (quot secs 60)

     hours
     (quot mins 60)]

    (cond (< secs 60) (str secs "s")
          (< mins 60) (str mins "m " (rem secs 60) "s")
          :else (str hours "h " (rem mins 60) "m"))))

;;; ── The pane ────────────────────────────────────────────────────────────────

(def ^:private item-keys
  "Every key an `append` op carries its items under — the engine's own
   `item-bounds` plus the log's lines, read off that table so a new node type
   cannot grow a second spelling here."
  (conj (into #{} (map :key) (vals hi-spec/item-bounds)) :lines))

(defn- touched
  "What ONE patch upserted — `{:items {node-id #{item-id …}} :nodes #{node-id}}`.

   Emphasis marks what the LAST patch touched and clears when the next one lands,
   so a changing table reads as movement instead of as flicker and the mark is
   pinned by the patch that caused it rather than by a timer no test can hold
   still. A log is left out on purpose: its tail is already where the eye is."
  [patch]
  (reduce (fn [acc {:keys [op node-id node-spec] :as o}]
            (case op
              :append
              (let [k (some item-keys (keys o))]
                (if (or (nil? k) (= :lines k))
                  acc
                  (update-in acc [:items node-id] (fnil into #{}) (keep :id (get o k)))))

              :add-node
              (update acc :nodes (fnil conj #{}) (:id node-spec))

              acc))
          {:items {} :nodes #{}}
          (:ops patch)))

(defn opened
  "The pane a freshly mounted `view` opens: the engine's own materialized view,
   a viewport parked at the end, and nothing measured yet.

   That materialized map IS the model — the terminal keeps no second copy of a
   row, so a scroll can never paint state the phone does not have."
  [view]
  {:view view
   :offset 0
   :is-following true
   :widths {}
   :fresh {:items {} :nodes #{}}
   :expanded #{}
   :total 0
   :visible 0})

(defn view-id "The id every patch and the close name." [pane] (get-in pane [:view :id]))

(defn patched
  "The pane one patch leaves behind. The view is advanced by the ENGINE's own
   `live/apply-patch`, never by a second interpreter here: the terminal and the
   phone disagreeing about a row is the one bug this primitive cannot afford."
  [pane patch]
  (-> pane
      (update :view live/apply-patch patch)
      (assoc :fresh (touched patch))))

(defn max-offset
  "The last row the viewport may start on, from what the last paint measured."
  ^long [pane]
  (max 0 (- (long (or (:total pane) 0)) (long (or (:visible pane) 0)))))

(defn scrolled
  "The pane after the human moved the wheel `delta` rows over it (positive is
   DOWN). Scrolling up RELEASES follow-tail — reading back is always a deliberate
   intent — and landing on the last row re-arms it. The anchor is dropped because
   this gesture, not the previous paint, is now what says where the eye is; the
   next paint derives a fresh one from the row it lands on."
  [pane delta]
  (let [next-offset (clamp (+ (long (or (:offset pane) 0)) (long delta)) 0 (max-offset pane))]
    (assoc pane
      :offset next-offset
      :anchor nil
      :is-following (>= next-offset (max-offset pane)))))

(defn expanded
  "The pane with node `node-id` toggled between its window and everything it
   holds. The `+ N more` line is the control, so expanding is a click and needs
   no key the composer would have to give up."
  [pane node-id]
  (update pane
          :expanded
          (fn [ids]
            (let [ids (set ids)]
              (if (contains? ids node-id) (disj ids node-id) (conj ids node-id))))))

(defn painted
  "The pane taught what the last paint measured — where the viewport landed, the
   anchor under it, how tall the plan was, and the column widths the tables
   reached. Widths live HERE and not in the plan because they may only ever GROW
   while a view is open: a wider value in row 900 must not shuffle every column
   the human already read."
  [pane {:keys [offset anchor total visible widths]}]
  (assoc pane
    :offset (long (or offset 0))
    :anchor anchor
    :total (long (or total 0))
    :visible (long (or visible 0))
    :widths (merge (:widths pane) widths)))

;;; ── The paint plan ──────────────────────────────────────────────────────────

(defn- cell-of [row idx] (flat-text (get (:cells row) (long idx) "")))

(defn- windowed
  "`items` cut to the node's window plus how many stayed behind. A log answers its
   TAIL — the newest lines are the reason anybody is watching — and every keyed
   node the HEAD of the order it declared."
  [items is-expanded is-tail]
  (let
    [n
     (count items)

     limit
     (long node-window)]

    (if (or is-expanded (<= n limit))
      {:shown (vec items) :behind 0}
      (if is-tail
        {:shown (subvec (vec items) (- n limit)) :behind (- n limit)}
        {:shown (subvec (vec items) 0 limit) :behind (- n limit)}))))

(defn- more-row
  [node-id behind noun]
  {:kind :more
   :node-id node-id
   :count behind
   :text (str "+ " behind " more " noun (when (not= 1 (long behind)) "s"))})

(defn- desired-widths
  "Column widths measured from the window that is actually PAINTED — its header
   and the rows on screen, never the rows the record holds behind them."
  [columns rows]
  (into []
        (map-indexed (fn [idx col]
                       (reduce (fn [w row]
                                 (max (long w) (p/display-width (cell-of row idx))))
                               (p/display-width (str (:label col)))
                               rows)))
        columns))

(defn- grown-widths
  "What a table is measured at now: never narrower than it already was while this
   view is open, so a column the human has been reading does not jump."
  [previous desired]
  (into []
        (map-indexed (fn [idx w]
                       (max (long w) (long (get previous idx 0)))))
        desired))

(defn- fitted-widths
  "`widths` squeezed into `text-w` by taking cells off the WIDEST column first, so
   a narrow terminal costs the one long free-text column and not the four short
   ones beside it. Only the PAINT shrinks — the pane keeps what it measured, so
   widening the terminal restores the columns instead of re-measuring them from
   whatever rows happen to be on screen."
  [widths text-w]
  (let
    [n
     (count widths)

     chrome
     (+ 2 (* 3 (max 0 (dec n))))

     room
     (max n (- (long text-w) chrome))]

    (loop [ws (mapv #(max 1 (long %)) widths)]
      (if (or (<= (long (reduce + 0 ws)) room) (every? #(<= (long %) 1) ws))
        ws
        (let
          [idx (first (apply max-key
                        (fn [[_ w]]
                          (long w))
                        (map-indexed vector ws)))]
          (recur (update ws idx #(max 1 (dec (long %))))))))))

(defn- table-line
  "One table line — cells padded to their column and separated by the same `│`
   the rest of the TUI's tables use."
  [widths cells aligns]
  (str " "
       (str/join " │ "
                 (map (fn [cell w align]
                        (let
                          [w
                           (long w)

                           text
                           (p/ellipsize (str cell) w)]

                          (if (= :right align) (p/pad-left text w) (p/pad-right text w))))
                      cells
                      widths
                      aligns))
       " "))

(defn- rule-line
  [widths]
  (str " "
       (str/join "─┼─"
                 (map (fn [w]
                        (apply str (repeat (long w) "─")))
                      widths))
       " "))

(defmulti ^:private node-rows
  "The rows ONE node contributes to the pane's single scroll surface — the whole
   live vocabulary, once, in the same order `live/->markdown` prints it."
  (fn [node _ctx]
    (:type node)))

(defmethod node-rows :status
  [{:keys [id text detail tone]} _]
  (cond-> [{:kind :status :node-id id :text (flat-text text) :tone tone}]
    (seq (flat-text detail))
    (conj {:kind :prose :node-id id :text (flat-text detail)})))

(defmethod node-rows :progress
  [{:keys [id value done total]} _]
  [{:kind :progress :node-id id :value value :done done :total total}])

(defmethod node-rows :stat
  [{:keys [id stats]} {:keys [text-w fresh]}]
  (if (seq stats)
    ;; A strip, not a column: the counters pack onto as few rows as the terminal
    ;; allows, because a score reads at a glance or not at all.
    (let
      [cells
       (mapv (fn [{:keys [label value-text tone] :as stat}]
               {:label (flat-text label)
                :value (flat-text value-text)
                :tone tone
                :is-fresh (contains? fresh (:id stat))})
             stats)

       width
       (fn [{:keys [label value]}]
         ;; What [[paint-entry!]] really puts down — `label value` and the gap
         ;; after it — so a row is measured by exactly what it costs.
         (+ 4 (p/display-width label) (p/display-width value)))]

      (->> cells
           (reduce (fn [rows cell]
                     (let [row (peek rows)]
                       (if (and row
                                (<= (+ (long (reduce + 0 (map width row))) (long (width cell)))
                                    (long text-w)))
                         (conj (pop rows) (conj row cell))
                         (conj rows [cell]))))
                   [])
           (mapv (fn [row]
                   {:kind :stats :node-id id :items row}))))
    [{:kind :empty :node-id id :text (empty-text :stat)}]))

(defmethod node-rows :steps
  [{:keys [id steps]} {:keys [fresh is-expanded]}]
  (if (seq steps)
    (let [{:keys [shown behind]} (windowed steps is-expanded false)]
      (cond->
        (mapv (fn [step]
                {:kind :step
                 :node-id id
                 :item-id (:id step)
                 :text (flat-text (:label step))
                 :detail (flat-text (:detail step))
                 :value (:value step)
                 :tone (:tone step)
                 :is-fresh (contains? fresh (:id step))})
              shown)
        (pos? (long behind))
        (conj (more-row id behind "step"))))
    [{:kind :empty :node-id id :text (empty-text :steps)}]))

(defmethod node-rows :log
  [{:keys [id lines total-lines]} {:keys [is-expanded]}]
  (if (seq lines)
    (let
      [{:keys [shown]}
       (windowed lines is-expanded true)

       ;; What is behind the log is what the RECORD holds, not what the window
       ;; cut: the sink keeps every line that was ever accepted.
       behind
       (- (long (or total-lines (count lines))) (count shown))]

      (into (if (pos? behind)
              [{:kind :note
                :node-id id
                :text (str "… " behind " earlier lines — the view's record keeps them all")}]
              [])
            (map (fn [line]
                   {:kind :log :node-id id :text (str line)}))
            shown))
    [{:kind :empty :node-id id :text (empty-text :log)}]))

(defmethod node-rows :table
  [{:keys [id columns] :as node} {:keys [text-w widths fresh is-expanded]}]
  (let
    [ordered
     (live/ordered-rows node)

     {:keys [shown behind]}
     (windowed ordered is-expanded false)

     ;; Measured from the painted window and never narrower than last time; the
     ;; FIT is what a cramped terminal does to the paint, and it is deliberately
     ;; not what the pane remembers.
     measured
     (grown-widths (get widths id) (desired-widths columns shown))

     ws
     (fitted-widths measured text-w)

     aligns
     (mapv #(or (:align %) :left) columns)]

    (with-meta (cond->
                 (into [{:kind :thead
                         :node-id id
                         :text (table-line ws (mapv #(str (:label %)) columns) aligns)}
                        {:kind :trule :node-id id :text (rule-line ws)}]
                       (map (fn [row]
                              {:kind :trow
                               :node-id id
                               :item-id (:id row)
                               :tone (:tone row)
                               :is-fresh (contains? fresh (:id row))
                               :text (table-line ws
                                                 (map-indexed (fn [idx _]
                                                                (cell-of row idx))
                                                              columns)
                                                 aligns)}))
                       shown)
                 (empty? ordered)
                 (conj {:kind :empty :node-id id :text (empty-text :table)})

                 (pos? (long behind))
                 (conj (more-row id behind "row")))
      {:widths {id measured}})))

(defmethod node-rows :link
  [{:keys [id links]} {:keys [fresh is-expanded]}]
  (if (seq links)
    (let [{:keys [shown behind]} (windowed links is-expanded false)]
      (cond->
        (mapv (fn [{:keys [label target-kind target tone] :as link}]
                {:kind :link
                 :node-id id
                 :item-id (:id link)
                 :text (flat-text label)
                 :target (str target)
                 :target-kind target-kind
                 :tone tone
                 :is-fresh (contains? fresh (:id link))})
              shown)
        (pos? (long behind))
        (conj (more-row id behind "link"))))
    [{:kind :empty :node-id id :text (empty-text :link)}]))

(defn plan
  "The pane's whole paint plan, `text-w` columns wide: one entry per painted row,
   nodes in DECLARATION order, each under its own label with a row of air above
   it. Carries `{:widths …}` as metadata — what the tables measured this pass, on
   its way back into the pane through [[painted]]."
  [pane text-w]
  (let
    [{:keys [view widths fresh expanded]}
     pane

     ctx
     (fn [node]
       {:text-w text-w
        :widths widths
        :fresh (get-in fresh [:items (:id node)] #{})
        :is-expanded (contains? (set expanded) (:id node))})

     head
     (if (seq (flat-text (:description view)))
       [{:kind :prose :text (flat-text (:description view))}]
       [])

     sections
     (map-indexed (fn [idx node]
                    (let
                      [rows
                       (node-rows node (ctx node))

                       label
                       (flat-text (:label node))]

                      (with-meta (cond-> []
                                   (pos? (long idx))
                                   (conj {:kind :blank})

                                   (seq label)
                                   (conj {:kind :node
                                          :node-id (:id node)
                                          :text label
                                          :is-fresh (contains? (:nodes fresh) (:id node))})

                                   :always
                                   (into rows))
                        (meta rows))))
                  (:nodes view))]

    (with-meta (into head cat sections)
      {:widths (reduce merge {} (map (comp :widths meta) sections))})))

;;; ── Where the viewport sits ─────────────────────────────────────────────────

(defn anchor-at
  "The anchor of the row at `idx` — `[node-id item-id]`, the identity the eye is
   on. Rows that belong to no node (the view's own prose) anchor on nothing, so
   the viewport falls back to its offset for them."
  [rows idx]
  (when-let [row (get (vec rows) (long idx))]
    (when (:node-id row) [(:node-id row) (:item-id row)])))

(defn offset
  "Where the viewport starts in `rows`, `visible` rows tall.

   A FOLLOWING pane sits at the end. A parked one is pinned by its ANCHOR: the
   row carrying the same `[node-id item-id]` it was reading, wherever the patches
   moved it to — rows arriving above it change the scrollbar, not the reading
   position. An anchor whose row is gone (the item was removed, the node
   collapsed) falls back to the node it belonged to, and then to the raw offset,
   so the eye lands near what it was on instead of at the top."
  ^long [pane rows visible]
  (let
    [rows
     (vec rows)

     limit
     (max 0 (- (count rows) (long visible)))

     [node-id item-id :as anchor]
     (:anchor pane)

     found
     (when anchor
       (or (first (keep-indexed (fn [idx row]
                                  (when (and (= node-id (:node-id row)) (= item-id (:item-id row)))
                                    idx))
                                rows))
           (first (keep-indexed (fn [idx row]
                                  (when (= node-id (:node-id row)) idx))
                                rows))))]

    (cond (:is-following pane) limit
          found (clamp (long found) 0 limit)
          :else (clamp (long (or (:offset pane) 0)) 0 limit))))

;;; ── Chrome ──────────────────────────────────────────────────────────────────

(defn title-line
  "What rides the band's opening rule: the view's title and how long it has been
   open. `source` — the extension that opened it — comes after the title, because
   the first thing the human asks a picture that appeared on its own is who put
   it there."
  [pane now-ms]
  (let [{:keys [title source created-at]} (:view pane)]
    (str/join " · "
              (remove str/blank?
                [(flat-text title) (flat-text source)
                 (elapsed-text (- (long now-ms) (long (or created-at now-ms))))]))))

(defn- status-summary
  "The one line a view is worth when it is not the pane in front: its newest
   status, else its progress, else what it is called."
  [pane]
  (let
    [nodes
     (get-in pane [:view :nodes])

     status
     (first (filter #(= :status (:type %)) nodes))

     progress
     (first (filter #(= :progress (:type %)) nodes))]

    (cond (and status (seq (flat-text (:text status)))) {:text (flat-text (:text status))
                                                         :tone (:tone status)}
          (and progress (:value progress))
          {:text (str (long (Math/round (* 100.0 (double (:value progress))))) "%")
           :tone (:tone progress)}
          :else {:text (flat-text (get-in pane [:view :title])) :tone nil})))

(defn footer-text
  "What an open view contributes to the footer, so a view scrolled away — or
   behind a form — is still legible: its title and the state it is in."
  [pane]
  (when pane
    (let [{:keys [text]} (status-summary pane)]
      (str/join " · " (remove str/blank? [(flat-text (get-in pane [:view :title])) text])))))

(defn interruptible
  "The pane a stop would hit: the NEWEST open view, or nil when the band is empty.

   EVERY view answers. A form may refuse to be cancelled because the run cannot
   continue without an answer; a view asks nothing, so refusing to stop it would
   only trap the human in front of work they already told to stop.

   ONE place decides WHICH one. The footer advertises the abort key by asking here
   and the terminal's abort branch acts by asking here, so the row the human reads
   and the key they press can never name different views."
  [panes]
  (last panes))

(defn stopping
  "The note the human is typing into an ARMED stop on `pane` — `\"\"` the moment
   Escape arms it — or nil while the view is only being watched."
  [pane]
  (:stop pane))

(defn armed
  "The pane with its stop ARMED: Escape asked to interrupt and the band takes the
   keyboard for one line. NOTHING is stopped yet — the engine hears about it when
   the human presses Enter, so the comment travels with the stop instead of
   arriving after it."
  [pane]
  (cond-> pane
    (nil? (stopping pane))
    (assoc :stop "")))

(defn disarmed
  "The pane back to being watched: whatever was typed is dropped with the stop."
  [pane]
  (dissoc pane :stop))

(defn typed
  "Apply ONE keystroke to an armed stop — the normalized vocabulary
   `human-input/key->event` speaks, so the note line and a form's fields read the
   same keyboard. Returns `{:pane pane' :action action :note note}`, where `action`
   is `:stop` (interrupt it, carrying `note`), `:keep` (keep watching) or nil
   (still typing).

   The note stops growing at `hi-spec/note-chars`: the engine cuts a longer one
   anyway, and a field that swallowed the words past the bound would lie about
   what the model is going to read."
  [pane {:keys [kind char]}]
  (let [note (or (stopping pane) "")]
    (case kind
      :cancel
      {:pane (disarmed pane) :action :keep}

      (:enter :submit)
      {:pane (disarmed pane) :action :stop :note (not-empty (str/trim note))}

      :backspace
      {:pane (assoc pane :stop (subs note 0 (max 0 (dec (count note))))) :action nil}

      :char
      (let [full (>= (count note) (long hi-spec/note-chars))]
        {:pane (cond-> pane
                 (not full)
                 (assoc :stop (str note char)))
         :action nil})

      {:pane pane :action nil})))

(defn stop-prompt
  "What an armed stop asks, as `{:label … :note …}`: the line above the hint bar
   while the human types. It names the view, because the stop hits the newest one
   and several may be open."
  [pane]
  (when-let [note (stopping pane)]
    {:label (str "interrupt " (flat-text (get-in pane [:view :title])) " — why? ") :note note}))

(defn hint
  "The hint bar under the band. Escape is the ONE key a view takes, and while
   several are open it says WHICH one it will hit — the newest, the one the band
   is painting. Once the stop is armed the bar says the two keys that end the
   typing: Enter interrupts with the note, Escape keeps watching."
  [pane others]
  (if (stopping pane)
    [["⏎" "interrupt"] ["Esc" "keep watching"]]
    (cond-> []
      (some? pane)
      (conj ["Esc" (str "interrupt " (flat-text (get-in pane [:view :title])))])

      (seq others)
      (conj [(str (inc (count others))) "views open"]))))

;;; ── Painting ────────────────────────────────────────────────────────────────

(defn- fill!
  [g left row inner-w fg]
  (p/set-colors! g fg t/dialog-bg)
  (p/fill-rect! g (inc (long left)) row inner-w 1))

(defn- put!
  [g left row inner-w text]
  (p/put-str! g (+ (long left) 2) row (p/ellipsize (str text) (max 0 (- (long inner-w) 3)))))

(defn- paint-plain!
  [g left row inner-w fg text]
  (fill! g left row inner-w fg)
  (put! g left row inner-w text))

(defn- paint-styled!
  [g left row inner-w fg styles text]
  (fill! g left row inner-w fg)
  (p/styled g styles (put! g left row inner-w text)))

(defn- progress-text
  "`▰▰▰▰▰▰▱▱▱▱▱▱▱▱  62%  ·  18/29 done` — the bar, the number it stands for, and
   what the number counts. A bar alone never says how much is left in units the
   human cares about; an indeterminate node has no bar at all and says the one
   true thing instead.

   The fraction is the engine's own [[live/fraction]], so a node that declared
   `:done` of `:total` and one that declared a `:value` paint the same bar."
  [{:keys [done total] :as entry}]
  (let
    [counted
     (when done (str done (when total (str "/" total)) " done"))

     value
     (live/fraction entry)]

    (if (nil? value)
      (str/join "  ·  " (remove str/blank? ["working" counted]))
      (let
        [pct
         (live/percent value)

         filled
         (clamp (long (Math/round (* (double bar-w) (double value)))) 0 (long bar-w))

         bar
         (str (apply str (repeat filled "▰")) (apply str (repeat (- (long bar-w) filled) "▱")))]

        (str/join "  ·  " (remove str/blank? [(str bar "  " pct "%") counted]))))))

(defn- paint-segments!
  "One row painted in SEVERAL inks, left to right inside the body's own margin.
   The strip of counters is the only row where the ink changes mid-line, and it
   has to: a counter's tone belongs on the counter. A segment that reaches the
   band's edge is cut there, so a strip too wide for the terminal ends in an
   ellipsis instead of in the chrome."
  [g left row inner-w segments]
  (fill! g left row inner-w t/dialog-fg)
  (reduce (fn [^long used {:keys [text fg styles]}]
            (let [shown (p/ellipsize (str text) (max 0 (- (long inner-w) 3 used)))]
              (when (seq shown)
                (p/set-colors! g (or fg t/dialog-fg) t/dialog-bg)
                (p/styled g (or styles []) (p/put-str! g (+ (long left) 2 used) row shown)))
              (+ used (long (p/display-width shown)))))
          0
          segments))

(defn- paint-entry!
  "Paint ONE plan row and register whatever on it can be clicked. `left` is the
   band's rail and the body opens two columns inside it, exactly like the form's
   rows, so a view and a form painted in the same band line up."
  [g left row inner-w view-id entry]
  (case (:kind entry)
    :blank
    (fill! g left row inner-w t/dialog-fg)

    ;; A node's own label is the section heading the stack is read by.
    :node
    (paint-styled! g
                   left
                   row
                   inner-w
                   (if (:is-fresh entry) t/dialog-fg t/dialog-hint)
                   [p/BOLD]
                   (:text entry))

    :status
    (do (fill! g left row inner-w (tone-fg (:tone entry)))
        (p/styled
          g
          [p/BOLD]
          (put! g left row inner-w (str (get tone-glyph (:tone entry) "·") " " (:text entry)))))

    :progress
    (paint-plain! g
                  left
                  row
                  inner-w
                  (if (live/fraction entry) t/dialog-fg t/dialog-hint)
                  (progress-text entry))

    ;; The label recedes and the value takes the ink: a strip is read by its
    ;; numbers, and the words beside them are only there to name which number —
    ;; which is why the tone a counter carries lands on the counter itself.
    :stats
    (paint-segments! g
                     left
                     row
                     inner-w
                     (into []
                           (mapcat (fn [{:keys [label value tone is-fresh]}]
                                     [{:text label :fg t/dialog-hint} {:text " "}
                                      {:text value
                                       :fg (tone-fg tone)
                                       :styles (when is-fresh [p/BOLD])} {:text "   "}])
                                   (:items entry))))

    :step
    (paint-styled! g
                   left
                   row
                   inner-w
                   (tone-fg (:tone entry))
                   (if (:is-fresh entry) [p/BOLD] [])
                   (str (get tone-glyph (:tone entry) "·")
                        " "
                        (:text entry)
                        (when (seq (str (:detail entry))) (str " — " (:detail entry)))))

    :log
    (paint-plain! g left row inner-w t/dialog-fg (:text entry))

    :thead
    (paint-styled! g left row inner-w t/dialog-hint [p/BOLD] (:text entry))

    :trule
    (paint-plain! g left row inner-w t/dialog-hint (:text entry))

    :trow
    (paint-styled! g
                   left
                   row
                   inner-w
                   (tone-fg (:tone entry))
                   (if (:is-fresh entry) [p/BOLD] [])
                   (:text entry))

    :link
    (do (paint-plain! g left row inner-w t/link-chrome-fg (str "→ " (:text entry)))
        ;; The terminal already knows how to open a URL and a path — a live link
        ;; registers as the SAME click region the transcript's own links do, so
        ;; there is no second opener to keep in step.
        (when-let
          [kind (case (:target-kind entry)
                  :url
                  :url

                  :path
                  :file

                  nil)]
          (cr/register! {:bounds
                         {:row row :col (+ (long left) 2) :width (max 0 (- (long inner-w) 3))}
                         :kind kind
                         :url (:target entry)
                         :enabled? true})))

    ;; `+ N more` is a CONTROL, so it wears the accent the rest of the TUI gives
    ;; a thing you can press, and it registers the region that expands its node.
    :more
    (do (paint-styled! g left row inner-w t/dialog-hint-key [p/BOLD] (:text entry))
        (cr/register! {:bounds {:row row :col (+ (long left) 2) :width (max 0 (- (long inner-w) 3))}
                       :kind :live-expand
                       :view-id view-id
                       :node-id (:node-id entry)
                       :enabled? true}))

    :collapsed
    (paint-plain! g left row inner-w (tone-fg (:tone entry) t/dialog-hint) (:text entry))

    ;; Everything that is prose — the view's description, a status detail, a node
    ;; that holds nothing, the count of what the record kept — speaks in the dim
    ;; italic voice the rest of the TUI reserves for explanation.
    (paint-styled! g left row inner-w t/dialog-hint [p/ITALIC] (:text entry))))

(defn collapsed-row
  "The ONE line an older open view keeps above the band: its title and where it
   got to. Newest last, so the pane in front is the one the human is watching."
  [pane]
  (let [{:keys [text tone]} (status-summary pane)]
    {:kind :collapsed
     :node-id nil
     :tone tone
     :text (str "▸ "
                (str/join " · "
                          (remove str/blank? [(flat-text (get-in pane [:view :title])) text])))}))

(defn band-rows
  "The rows the band covers on a `cols`×`rows` terminal, as `[from to]`
   inclusive — what the wheel needs to know to tell 'over the pane' from 'over
   the transcript'."
  [cols rows panes content-top prompt-h]
  (when (seq panes)
    (let
      [region
       (tr/band-region (long cols) (long rows) (long content-top) (long prompt-h))

       {:keys [sep-row foot-row]}
       (tr/band-geometry region 1 false)]

      [(long sep-row) (long foot-row)])))

(defn paint!
  "Draw the live-view band for `panes` — oldest first, the newest in front —
   INSIDE the session's own frame, and return the geometry the front pane was
   painted with: `{:view-id :offset :anchor :total :visible :widths}`.

   The caller hands that back through [[painted]], which is what makes the next
   wheel tick, the next anchor and the column widths agree with what is on
   screen. Nothing is dispatched from here: the render thread paints, the state
   thread decides.

   The SAME band the human-input form paints in (`transient/band-region`) — a
   closed box on the terminal's own paper, anchored above the prompt and growing
   upward over the transcript, never past `content-top`. A view takes at most
   half the rows between the transcript and the prompt, because the run it is
   reporting on is still being read above it.

   Returns nil when there is nothing to paint."
  ([g cols rows panes content-top prompt-h]
   (paint! g cols rows panes content-top prompt-h (System/currentTimeMillis)))
  ([g cols rows panes content-top prompt-h now-ms]
   (when (seq panes)
     (let
       [{:keys [left inner-w] :as region}
        (tr/band-region (long cols) (long rows) (long content-top) (long prompt-h))]
       (binding [t/dialog-bg (if (:is-sideless region) t/terminal-bg t/dialog-bg)]
         (let
           [left (long left)
            inner-w (long inner-w)
            body-w (dec inner-w)
            ;; The body opens one column inside the rails and the row painters
            ;; take the next one, so nothing a view paints ever touches a rail —
            ;; and the right lane stays clear for the scrollbar.
            text-w (max 8 (- body-w 4))
            pane (last panes)
            others (vec (butlast panes))
            collapsed (mapv collapsed-row others)
            rows-plan (plan pane text-w)
            ;; Half the rows between the top of the transcript and the prompt, at
            ;; the most: the run this view reports on is still being read above it.
            room (max 4 (quot (- (long (:hint-row region)) 1 (long (:min-row region))) 2))
            ;; An ARMED stop takes one body row for the line the human types into,
            ;; right above the fence: the band asks WHY it is being stopped where
            ;; it is being stopped, and the answer rides along with the stop.
            stop (stop-prompt pane)
            ;; What the band WANTS: every collapsed line, the plan, the row the
            ;; fenced hint rule costs, and that note line when it is armed.
            wanted (+ (count collapsed) (count rows-plan) 1 (if stop 1 0))
            {:keys [sep-row body-top foot-rule-row foot-row visible top-limit]}
            (tr/band-geometry region (min wanted room) false)
            ;; The band CLOSES below its hint bar, exactly like the form's: the
            ;; bar takes the row the closing rule used to own, the rule drops onto
            ;; the host's hint row, and the fence above the bar costs the body one
            ;; row.
            hint-at (long foot-rule-row)
            rule-at (long foot-row)
            hint-rule-at (dec hint-at)
            visible (max 1 (dec (long visible)))
            body-visible (max 1 (- visible (count collapsed) (if stop 1 0)))
            total (count rows-plan)
            start (offset pane rows-plan body-visible)
            shown (subvec (vec rows-plan) (min start total) (min total (+ start body-visible)))
            view-id (view-id pane)]

           (tr/clear-rows! g region (max 0 (long sep-row)) rule-at)
           ;; The title is the rule's own label — `── CI · 1m 12s ──` — so the
           ;; first row is chrome and every row under it is the view.
           (when (>= (long sep-row) (long top-limit))
             (tr/draw-rule! g region sep-row (title-line pane now-ms)))
           (when (> rule-at (max (long sep-row) (long top-limit))) (tr/draw-rule! g region rule-at))
           (when (> (long hint-rule-at) (max (long sep-row) (long top-limit)))
             (tr/draw-rule! g region hint-rule-at))
           (doseq [[idx entry] (map-indexed vector collapsed)]
             (paint-entry! g left (+ (long body-top) (long idx)) body-w view-id entry))
           (doseq [[idx entry] (map-indexed vector shown)]
             (paint-entry! g
                           left
                           (+ (long body-top) (count collapsed) (long idx))
                           body-w
                           view-id
                           entry))
           (doseq [idx (range (count shown) body-visible)]
             (paint-entry! g
                           left
                           (+ (long body-top) (count collapsed) (long idx))
                           body-w
                           view-id
                           {:kind :blank}))
           (when stop
             (paint-segments! g
                              left
                              (dec (long hint-rule-at))
                              body-w
                              [{:text (:label stop) :fg t/dialog-hint}
                               {:text (:note stop) :fg t/dialog-fg :styles [p/BOLD]}
                               {:text "▏" :fg t/dialog-hint-key}]))
           (dialogs/draw-hint-bar! g left hint-at inner-w (hint pane others))
           ;; The gutter lane every scrollable dialog draws its bar in: the last
           ;; column inside the right rail, which the body's own lead keeps clear.
           (when (> total body-visible)
             (scrollbar/draw! g
                              {:col (+ left inner-w)
                               :top (+ (long body-top) (count collapsed))
                               :track-h body-visible
                               :total-h total
                               :inner-h body-visible
                               :scroll start
                               :track-fg t/border-fg}))
           (tr/draw-band-border! g region sep-row rule-at top-limit)
           (p/clear-styles! g)
           {:view-id view-id
            :offset start
            :anchor (anchor-at rows-plan start)
            :total total
            :visible body-visible
            :widths (:widths (meta rows-plan))}))))))
