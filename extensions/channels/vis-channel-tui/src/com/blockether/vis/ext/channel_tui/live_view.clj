(ns com.blockether.vis.ext.channel-tui.live-view
  "TUI painting for a LIVE VIEW — the picture an extension streams WHILE it works
   (`com.blockether.vis.internal.human-input`, materialized by
   `com.blockether.vis.internal.human-input.live`).

   A form is a QUESTION and owns the keyboard until it is answered; a view is a
   PICTURE and owns nothing. The composer keeps focus while a view paints, the
   wheel over the band scrolls it, a click opens a link or expands a node, and
   the only key it takes is Escape — which ARMS a stop on the newest open view,
   before it interrupts the turn: the band then takes one FENCED line for the
   comment the human types, Escape or Enter interrupts with it, Backspace on an
   empty line keeps watching. A view is ALWAYS stoppable; the note is what says
   why.

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
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.click-regions :as cr]
            [com.blockether.vis.ext.channel-tui.dialogs :as dialogs]
            [com.blockether.vis.ext.channel-tui.markdown-layout :as layout]
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

(def ^:private markdown-mark
  "What could START inline markdown. Text holding none of these cannot hold any
   and skips the walker entirely: a table repaints its whole window on every
   patch, so the ordinary cell — a host name, a count — must not pay for a parse
   it cannot need."
  #"[*_`\[]")

(defn- md-runs
  "ONE line of styled runs: `text` read as INLINE markdown through the TUI's own
   markdown walker — the same one the transcript above the band paints through —
   so a `code span`, a **bold** word or a link inside a live node reads here
   exactly as it reads there.

   This is why the node vocabulary needs no `markdown` type and refuses one: the
   markup lives INSIDE the strings a node already carries, where every surface
   and the model's own document render it, instead of in a node whose shape only
   one surface could paint."
  [text]
  (let [flat (flat-text text)]
    (if (re-find markdown-mark flat)
      (into [] (comp (map :runs) cat) (layout/ast->lines (vis/markdown->ast flat) 4096))
      [{:text flat}])))

(defn- md-lines
  "`text` laid out into `width` columns as styled lines — [[md-runs]]'s walker
   with its wrapping. A line the wrapper BROKE is justified edge to edge and the
   last line of a paragraph stays ragged, which is the rule the transcript's own
   prose follows and the reason a paragraph beside a table has a straight right
   edge instead of four words and a hole."
  [text width]
  (let
    [w
     (max 1 (long width))

     flat
     (flat-text text)]

    (if (str/blank? flat)
      []
      (->> (layout/ast->lines (vis/markdown->ast flat) w)
           (remove (fn [{:keys [runs]}]
                     (every? #(str/blank? (:text %)) runs)))
           (mapv (fn [{:keys [runs wrap?]}]
                   (if wrap? (layout/justify-line-runs runs w) runs)))))))

(defn- runs-width
  "How many columns `runs` (or segments — both are `:text` carriers) put down."
  ^long [runs]
  (reduce (fn [^long w r]
            (+ w (long (p/display-width (str (:text r))))))
          0
          runs))

(defn- runs-cut
  "`runs` cut to `w` display columns, the last run that still fits ellipsized —
   the styled twin of `primitives/ellipsize`, so a cell too long for its column
   says so with the same `…` the rest of the TUI uses."
  [runs w]
  (loop
    [out
     []

     used
     0

     rs
     (seq runs)]

    (if (or (nil? rs) (>= (long used) (long w)))
      out
      (let
        [{:keys [text style]}
         (first rs)

         shown
         (p/ellipsize (str text) (- (long w) (long used)))]

        (recur (cond-> out
                 (seq shown)
                 (conj {:text shown :style style}))
               (+ (long used) (long (p/display-width shown)))
               (next rs))))))

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

(defn- filled-widths
  "`widths` GROWN into the room the band gives them, the slack going to the WIDEST
   column — the same column [[fitted-widths]] takes cells from first, so a table
   that shrank and grew back is the table it was.

   A table fills the band because a rule that stops halfway across reads as a
   table still loading, and because the column that holds free text is the one
   that can use the room."
  [widths text-w]
  (let
    [n
     (count widths)

     chrome
     (+ 2 (* 3 (max 0 (dec n))))

     slack
     (- (long text-w) chrome (long (reduce + 0 (map long widths))))]

    (if (or (zero? n) (<= slack 0))
      (vec widths)
      (let
        [idx (first (apply max-key
                      (fn [[_ w]]
                        (long w))
                      (map-indexed vector widths)))]
        (update (vec widths) idx #(+ (long %) slack))))))

(defn- run-segments
  "Styled markdown runs as [[paint-segments!]] segments over one base ink: a code
   span or a link takes the accent the transcript gives it, bold adds BOLD and
   italic adds ITALIC, and every other run wears the ink of the row it is on."
  [runs fg styles]
  (mapv (fn [{:keys [text style]}]
          {:text text
           :fg
           (if (or (contains? style :code) (contains? style :link)) t/header-active-tab-accent fg)
           :styles (cond-> (vec styles)
                     (contains? style :bold)
                     (conj p/BOLD)

                     (contains? style :italic)
                     (conj p/ITALIC))})
        runs))

(defn- segment-line
  "The plain line a row of segments reads as — what a test, a screenshot and a
   copied screen all see, taken FROM the segments instead of measured twice."
  [segments]
  (apply str (map :text segments)))

(defn- cell-segments
  "One table cell: its inline markdown cut to the column and padded to it, on the
   side its declared alignment asks for."
  [runs w align fg styles]
  (let
    [cut
     (runs-cut runs w)

     pad
     (apply str (repeat (max 0 (- (long w) (runs-width cut))) \space))]

    (if (= :right align)
      (into [{:text pad}] (run-segments cut fg styles))
      (conj (vec (run-segments cut fg styles)) {:text pad}))))

(defn- table-segments
  "One table line — cells padded to their columns and separated by the same `│`
   the rest of the TUI's tables use, kept in the chrome's own dim ink so a toned
   row does not drag the rules along with it."
  [widths cells aligns fg styles]
  (conj (into [{:text " "}]
              (comp (map (fn [[runs w align]]
                           (cell-segments runs w align fg styles)))
                    (interpose [{:text " │ " :fg t/dialog-hint}])
                    cat)
              (map vector cells widths aligns))
        {:text " "}))

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
  [{:keys [id text detail tone]} {:keys [text-w]}]
  (let
    [glyph
     (str (get tone-glyph tone "·") " ")

     ;; The glyph's two columns become a hanging indent under it, so a statement
     ;; that wraps reads as ONE paragraph and not as two unrelated rows.
     body
     (max 4 (- (long text-w) 2))]

    (into (vec (map-indexed
                 (fn [idx runs]
                   (let [runs (into [{:text (if (zero? (long idx)) glyph "  ")}] runs)]
                     {:kind :status :node-id id :tone tone :runs runs :text (segment-line runs)}))
                 (md-lines text body)))
          (map (fn [runs]
                 (let [runs (into [{:text "  "}] runs)]
                   {:kind :prose :node-id id :runs runs :text (segment-line runs)})))
          (md-lines detail body))))

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
               {:label (md-runs label)
                :value (md-runs value-text)
                :tone tone
                :is-fresh (contains? fresh (:id stat))})
             stats)

       width
       (fn [{:keys [label value]}]
         ;; What [[paint-entry!]] really puts down — `label value` and the gap
         ;; after it — so a row is measured by exactly what it costs.
         (+ 4 (runs-width label) (runs-width value)))]

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
  [{:keys [id steps]} {:keys [text-w fresh is-expanded]}]
  (if (seq steps)
    (let [{:keys [shown behind]} (windowed steps is-expanded false)]
      (cond->
        (mapv
          (fn [{:keys [label detail value tone] :as step}]
            (let
              [is-fresh (contains? fresh (:id step))
               ink (tone-fg tone)
               styles (if is-fresh [p/BOLD] [])
               said (into [{:text (str (get tone-glyph tone "·") " ") :fg ink :styles styles}]
                          cat
                          [(run-segments (md-runs label) ink styles)
                           (when (seq (flat-text detail))
                             (into [{:text " — " :fg t/dialog-hint}]
                                   (run-segments (md-runs detail) t/dialog-hint [])))])
               ;; What the step REPORTS rides the right edge, where a table
               ;; keeps its numbers, so a checklist and a table beside it read
               ;; down the same column.
               reported (when (seq (flat-text value))
                          (run-segments (md-runs value) t/dialog-hint []))
               segments
               (cond-> said
                 reported
                 (conj {:text (apply str
                                (repeat
                                  (max 1 (- (long text-w) (runs-width said) (runs-width reported)))
                                  \space))})

                 reported
                 (into reported))]

              {:kind :step
               :node-id id
               :item-id (:id step)
               :tone tone
               :is-fresh is-fresh
               :segments segments
               :text (segment-line segments)}))
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

     ;; Squeezed if it must be, then GROWN into whatever the band has left: a
     ;; table is a block, and a block that ends in mid-air reads as unfinished.
     ws
     (filled-widths (fitted-widths measured text-w) text-w)

     aligns
     (mapv #(or (:align %) :left) columns)

     line
     (fn [cells fg styles]
       (let [segments (table-segments ws cells aligns fg styles)]
         {:segments segments :text (segment-line segments)}))]

    (with-meta (cond->
                 (into [(merge {:kind :thead :node-id id}
                               (line (mapv #(md-runs (:label %)) columns) t/dialog-hint [p/BOLD]))
                        {:kind :trule :node-id id :text (rule-line ws)}]
                       (map (fn [row]
                              (let [is-fresh (contains? fresh (:id row))]
                                (merge {:kind :trow
                                        :node-id id
                                        :item-id (:id row)
                                        :tone (:tone row)
                                        :is-fresh is-fresh}
                                       (line (map-indexed (fn [idx _]
                                                            (md-runs (cell-of row idx)))
                                                          columns)
                                             (tone-fg (:tone row))
                                             (if is-fresh [p/BOLD] []))))))
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
                 :runs (md-runs label)
                 :target (str target)
                 :target-kind target-kind
                 :tone tone
                 :is-fresh (contains? fresh (:id link))})
              shown)
        (pos? (long behind))
        (conj (more-row id behind "link"))))
    [{:kind :empty :node-id id :text (empty-text :link)}]))

(def ^:private aside-gutter "Columns of air between two nodes standing side by side." 2)

(def ^:private aside-min-w
  "The narrowest column a node may be laid into. Under it the terminal has no room
   for two of anything and the group STACKS: `:is-aside` says where a node stands
   when there is room, never that a surface must cram."
  24)

(defn- node-section
  "One node's own rows: its label, then what the node paints, `text-w` columns
   wide. The row of air between sections belongs to the GROUP, not to the node, so
   two nodes standing side by side start on the same line."
  [node ctx fresh text-w]
  (let
    [rows
     (node-rows node (ctx node text-w))

     label
     (flat-text (:label node))]

    (with-meta (cond-> []
                 (seq label)
                 (conj {:kind :node
                        :node-id (:id node)
                        :text label
                        :is-fresh (contains? (:nodes fresh) (:id node))})

                 :always
                 (into rows))
      (meta rows))))

(defn- grouped
  "`nodes` gathered into the groups a surface lays out TOGETHER: a node that
   declared `:is-aside` joins the group of the node before it, every other node
   opens one. The first node cannot be an aside — nothing stands to the left of
   it — so it opens a group whatever it declared."
  [nodes]
  (reduce (fn [groups node]
            (if (and (seq groups) (:is-aside node))
              (conj (pop groups) (conj (peek groups) node))
              (conj groups [node])))
          []
          nodes))

(defn- zipped
  "Planned columns as ONE row list: row for row, each column's row in its own
   cell, a column that ran out of rows leaving air. The row anchors on the first
   cell that names a node, so the reading position still belongs to a node."
  [columns widths]
  (let [height (reduce max 0 (map count columns))]
    (mapv (fn [idx]
            (let
              [cells (mapv #(get % idx) columns)
               lead (first (filter :node-id cells))]

              {:kind :columns
               :node-id (:node-id lead)
               :item-id (:item-id lead)
               :cells cells
               :widths widths}))
          (range height))))

(defn- stacked
  "A group's nodes one under the next, a row of air between them — what a view
   with no aside in it paints, and what a band too narrow to split falls back to."
  [group ctx fresh text-w]
  (let
    [sections (map-indexed (fn [idx node]
                             (let [rows (node-section node ctx fresh text-w)]
                               (with-meta (cond-> []
                                            (pos? (long idx))
                                            (conj {:kind :blank})

                                            :always
                                            (into rows))
                                 (meta rows))))
                           group)]
    (with-meta (into [] cat sections)
      {:widths (reduce merge {} (map (comp :widths meta) sections))})))

(defn- group-rows
  "One group's rows: SIDE BY SIDE when it holds an aside and every column would
   still be at least [[aside-min-w]] wide, stacked otherwise. This is how a table
   gets prose down its right-hand side — and how the same view stays readable on a
   terminal half as wide."
  [group ctx fresh text-w]
  (let
    [k
     (count group)

     room
     (- (long text-w) (* (long aside-gutter) (dec k)))

     each
     (quot room (max 1 k))]

    (if (or (= 1 k) (< each (long aside-min-w)))
      (stacked group ctx fresh text-w)
      (let
        [;; The remainder goes to the FIRST column: it is the node the aside was
         ;; declared beside, and the annotated node is the wider one.
         widths
         (into [(+ each (rem room k))] (repeat (dec k) each))

         columns
         (mapv (fn [node w]
                 (node-section node ctx fresh w))
               group
               widths)]

        (with-meta (zipped columns widths)
          {:widths (reduce merge {} (map (comp :widths meta) columns))})))))

(defn plan
  "The pane's whole paint plan, `text-w` columns wide: one entry per painted row,
   nodes in DECLARATION order, each under its own label with a row of air above
   it. A node that declared `:is-aside` stands BESIDE the one before it wherever
   the band is wide enough for both. Carries `{:widths …}` as metadata — what the
   tables measured this pass, on its way back into the pane through [[painted]]."
  [pane text-w]
  (let
    [{:keys [view widths fresh expanded]}
     pane

     ctx
     (fn [node w]
       {:text-w w
        :widths widths
        :fresh (get-in fresh [:items (:id node)] #{})
        :is-expanded (contains? (set expanded) (:id node))})

     head
     (mapv (fn [runs]
             {:kind :prose :runs runs :text (segment-line runs)})
           (md-lines (:description view) text-w))

     sections
     (map-indexed (fn [idx group]
                    (let [rows (group-rows group ctx fresh (long text-w))]
                      (with-meta (cond-> []
                                   (pos? (long idx))
                                   (conj {:kind :blank})

                                   :always
                                   (into rows))
                        (meta rows))))
                  (grouped (:nodes view)))]

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
   keyboard for one line. NOTHING is stopped yet — Escape again (or Enter) sends it,
   so the comment travels WITH the stop instead of arriving after it, and one
   mistaken Escape never kills work the human still wanted."
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

   Escape is the key that STOPS, at both ends: it arms the note line and it sends
   the stop, so the human who reached for Escape to kill the run gets exactly that
   by pressing it twice. Enter sends too, for the human who came to write a reason.
   Backspace on an empty line is the way BACK — the one key that can only ever
   undo, so keeping watching is never one keystroke away from stopping.

   The note stops growing at `hi-spec/note-chars`: the engine cuts a longer one
   anyway, and a field that swallowed the words past the bound would lie about
   what the model is going to read."
  [pane {:keys [kind char]}]
  (let [note (or (stopping pane) "")]
    (case kind
      (:cancel :enter :submit)
      {:pane (disarmed pane) :action :stop :note (not-empty (str/trim note))}

      :backspace
      (if (str/blank? note)
        {:pane (disarmed pane) :action :keep}
        {:pane (assoc pane :stop (subs note 0 (max 0 (dec (count note))))) :action nil})

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
   typing: Escape or Enter interrupt with whatever was written, Backspace on an
   empty line keeps watching."
  [pane others]
  (if-let [note (stopping pane)]
    (if (str/blank? note)
      [["Esc / ⏎" "interrupt"] ["⌫" "keep watching"]]
      [["Esc / ⏎" "interrupt with the note"] ["⌫" "erase"]])
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

(defn- paint-runs!
  "One row of styled markdown runs over a base ink. [[run-segments]] decides which
   run leaves that ink — a code span and a link take the accent the transcript
   gives them — so `code` inside a statement reads as code on every surface."
  [g left row inner-w fg styles runs]
  (paint-segments! g left row inner-w (run-segments runs fg styles)))

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
    (paint-runs! g left row inner-w (tone-fg (:tone entry)) [p/BOLD] (:runs entry))

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
                                     (concat
                                       (run-segments label t/dialog-hint [])
                                       [{:text " "}]
                                       (run-segments value (tone-fg tone) (if is-fresh [p/BOLD] []))
                                       [{:text "   "}]))
                                   (:items entry))))

    :step
    (paint-segments! g left row inner-w (:segments entry))

    :log
    (paint-plain! g left row inner-w t/dialog-fg (:text entry))

    :thead
    (paint-segments! g left row inner-w (:segments entry))

    :trule
    (paint-plain! g left row inner-w t/dialog-hint (:text entry))

    :trow
    (paint-segments! g left row inner-w (:segments entry))

    :link
    (do (paint-segments! g
                         left
                         row
                         inner-w
                         (into [{:text "→ " :fg t/link-chrome-fg}]
                               (run-segments (:runs entry) t/link-chrome-fg [])))
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

    ;; Nodes standing SIDE BY SIDE: one plan row carries one cell per column, and
    ;; each cell is painted into its own slice of the band by THIS function — a
    ;; table beside a paragraph is painted by the code that paints them alone.
    :columns
    (do (fill! g left row inner-w t/dialog-fg)
        (reduce (fn [^long x [cell w]]
                  (when cell (paint-entry! g (+ (long left) x) row (+ (long w) 3) view-id cell))
                  (+ x (long w) (long aside-gutter)))
                0
                (map vector (:cells entry) (:widths entry))))

    :collapsed
    (paint-plain! g left row inner-w (tone-fg (:tone entry) t/dialog-hint) (:text entry))

    ;; Everything that is prose — the view's description, a status detail, a node
    ;; that holds nothing, the count of what the record kept — speaks in the dim
    ;; italic voice the rest of the TUI reserves for explanation.
    (if-let [runs (:runs entry)]
      (paint-runs! g left row inner-w t/dialog-hint [p/ITALIC] runs)
      (paint-styled! g left row inner-w t/dialog-hint [p/ITALIC] (:text entry)))))

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
            ;; An ARMED stop takes two body rows: the line the human types into and
            ;; the rule that fences it off from the view above. The band asks WHY it
            ;; is being stopped where it is being stopped, and the answer rides
            ;; along with the stop — but the question is the BAND speaking, not one
            ;; more row of the run's own report, so it is ruled off on both sides.
            stop (stop-prompt pane)
            ;; What the band WANTS: every collapsed line, the plan, the row the
            ;; fenced hint rule costs, and that note line when it is armed.
            wanted (+ (count collapsed) (count rows-plan) 1 (if stop 2 0))
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
            body-visible (max 1 (- visible (count collapsed) (if stop 2 0)))
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
             (let
               [note-row (dec (long hint-rule-at))
                note-rule-at (dec note-row)]

               (when (> note-rule-at (max (long sep-row) (long top-limit)))
                 (tr/draw-rule! g region note-rule-at))
               (paint-segments! g
                                left
                                note-row
                                body-w
                                [{:text (:label stop) :fg t/dialog-hint}
                                 {:text (:note stop) :fg t/dialog-fg :styles [p/BOLD]}
                                 {:text "▏" :fg t/dialog-hint-key}])))
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
