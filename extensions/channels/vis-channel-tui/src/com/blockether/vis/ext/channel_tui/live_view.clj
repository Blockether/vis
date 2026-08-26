(ns com.blockether.vis.ext.channel-tui.live-view
  "TUI painting for a LIVE VIEW — the picture an extension streams WHILE it works
   (`com.blockether.vis.internal.human-input`, materialized by
   `com.blockether.vis.internal.human-input.live`).

   A form is a QUESTION and owns the keyboard until it is answered; a view is a
   PICTURE and leaves the composer focused. The wheel over the band scrolls it, and
   clicks open links, expand nodes, focus rows in a focusable table, or fold the
   live surface down to a compact status line without stopping it. The only key it
   takes is Escape — which ARMS a stop on the newest open view before it interrupts
   the turn: the band then takes one FENCED line for the comment the human types,
   Escape or Enter interrupts with it, Backspace on an empty line keeps watching.
   A view is ALWAYS stoppable; the note is what says why.

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
            [com.blockether.vis.ext.channel-tui.columns :as columns]
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
  (let [w
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
  (loop [out
         []

         used
         0

         rs
         (seq runs)]

    (if (or (nil? rs) (>= (long used) (long w)))
      out
      (let [{:keys [text style]}
            (first rs)

            shown
            (p/ellipsize (str text) (- (long w) (long used)))]

        (recur (cond-> out
                 (seq shown)
                 (conj {:text shown :style style}))
               (+ (long used) (long (p/display-width shown)))
               (next rs))))))

(defn- elapsed-text
  "How long the view has been open, the way a person reads a duration — `13s`,
   `1m 12s`, `2h 4m`. It rides the band's own rule beside the title, because the
   first question about a running thing is how long it has been running."
  [ms]
  (let [secs
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
   phone disagreeing about a row is the one bug this primitive cannot afford.

   A patch that does not ADVANCE the view is dropped rather than refused. A view
   opened in THIS process reaches the tab on two routes — the in-process channel
   bus and the session event the gateway journals — and the journalled frame
   coalesces ops the bus already applied one at a time."
  [pane patch]
  (if (<= (long (or (:seq patch) 0)) (long (or (get-in pane [:view :seq]) 0)))
    pane
    (-> pane
        (update :view live/apply-patch patch)
        (assoc :fresh (touched patch)))))

(defn settled
  "The pane a close leaves behind — the run's FINAL picture and the verdict that
   ended it, stamped `ended-at`.

   A settled pane is not dropped: it collapses to ONE line the human can press.
   Dismissing the pane the instant the work finishes is exactly what made a
   watched log unreachable — the artifact the close files exists so it never is,
   and this line is the door to it."
  ([pane result] (settled pane result (System/currentTimeMillis)))
  ([pane result ended-at]
   (let [activity (contains? #{:activity "activity"} (get-in pane [:view :classification]))]
     (-> pane
         (dissoc :stop :is-minimized)
         (update :view merge (:view result))
         (assoc :settled (-> (select-keys result [:reason :artifact-id :is-from-human])
                             (assoc :ended-at ended-at))
                :is-reopened (and activity (boolean (:is-reopened pane))))))))

(defn settled?
  "True when this view has ENDED and the pane is its record."
  [pane]
  (contains? pane :settled))

(defn activity?
  "True only for the host-owned Activity specialization."
  [pane]
  (contains? #{:activity "activity"} (get-in pane [:view :classification])))

(defn dormant?
  "True when a pane belongs in its transcript receipt instead of the live band.

   Activity is always transcript-native, including its independently expanded detail.
   Generic views move to the transcript after settlement."
  [pane]
  (or (activity? pane) (and (settled? pane) (not (:is-reopened pane)))))

(defn minimized?
  "True when a still-running pane was folded to its compact status line. This is
   terminal-local presentation state: patches keep landing and the run keeps going."
  [pane]
  (and (not (settled? pane)) (true? (:is-minimized pane))))

(defn minimized
  "Fold a running pane without stopping it. An armed stop is abandoned because its
   note field cannot remain hidden behind the compact line. Settled records use their
   transcript row instead and are left alone."
  [pane]
  (if (settled? pane)
    pane
    (-> pane
        (dissoc :stop)
        (assoc :is-minimized true))))

(defn restored
  "Return a minimized pane to the full live surface, preserving its viewport."
  [pane]
  (dissoc pane :is-minimized))

(defn reopened
  "Toggle a transcript disclosure into the read-only band.

   Activity is transcript-native from its first event, so it uses this same toggle
   while running without gaining the generic Live View stop control."
  [pane]
  (cond-> (restored pane)
    (or (settled? pane) (activity? pane))
    (-> (update :is-reopened not)
        (assoc :is-following true
               :offset 0))))

(defn- activity-row-tone
  [state]
  (case (some-> state
                name)
    "failed"
    :error

    "cancelled"
    :warn

    "succeeded"
    :ok

    "running"
    :running

    :idle))

(defn- activity-row-label
  [{:keys [operation summary]}]
  (str (flat-text operation)
       (when-not (str/blank? (flat-text summary)) (str " · " (flat-text summary)))))

(defn- activity-row-detail
  [{:keys [error-summary result-summary duration-ms]}]
  (or (not-empty (flat-text error-summary))
      (not-empty (flat-text result-summary))
      (when duration-ms (vis/format-duration duration-ms))
      ""))

(defn- activity-count [activity state] (max 0 (long (get-in activity [:counts state] 0))))

(defn- activity-state
  "The concise user-facing state for one Activity receipt. Internal settlement vocabulary
   remains untouched; only the terminal says finished/succeeded/failed/cancelled."
  [pane]
  (case (get-in pane [:settled :reason])
    nil
    "running"

    :completed
    "succeeded"

    :failed
    "failed"

    :timeout
    "failed"

    :interrupted
    "cancelled"

    :cancelled
    "cancelled"

    "finished"))

(defn- activity-status-text
  "One honest Activity sentence: the active invocation while live, the actual total after settlement."
  [pane]
  (let [activity
        (get-in pane [:view :activity])

        finished
        (long (+ (long (activity-count activity :succeeded))
                 (long (activity-count activity :failed))
                 (long (activity-count activity :cancelled))))

        running
        (long (activity-count activity :running))

        total
        (long (+ (long finished) (long running)))

        active
        (some (fn [row]
                (when (= "running"
                         (some-> (:state row)
                                 name))
                  (activity-row-label row)))
              (:rows activity))]

    (if (nil? (get-in pane [:settled :reason]))
      (str "running · "
           (or active "waiting for first activity")
           (when (> (long total) 1) " · and others"))
      (str (activity-state pane)
           " · "
           finished
           " "
           (if (= 1 finished) "activity" "activities")
           " run"))))

(defn run-row
  "The transcript receipt for a run or host Activity, anchored at its form.

   Activity rows exist while running and are replaced in place by patches. Generic
   extension runs are filed only after settlement."
  [pane]
  (let [{:keys [reason ended-at]}
        (:settled pane)

        view
        (:view pane)

        activity
        (activity? pane)

        activity-data
        (when activity (:activity view))

        lines
        (reduce + 0 (keep #(when (= :log (:type %)) (:total-lines %)) (:nodes view)))

        end
        (long (or ended-at (System/currentTimeMillis)))]

    (cond-> {:view-id (:id view)
             :title (flat-text (:title view))
             :reason reason
             :lines (long lines)
             :elapsed-ms (max 0 (- end (long (or (:created-at view) end))))
             :is-reopened (boolean (:is-reopened pane))}
      activity
      (assoc :is-activity
        true :status-text
        (activity-status-text pane) :status-tone
        (activity-row-tone (:state activity-data)) :activity-view
        view :activity-rows
        (:rows activity-data) :activity-focused
        (:activity-focused pane) :activity-evidence
        (set (:activity-evidence pane)))

      (:trace-anchor pane)
      (assoc :anchor (:trace-anchor pane)))))

(defn watching-title
  "The title of the view the band is PAINTING right now, or nil when the band is
   empty. A settled pane has left the band for the transcript, so it never names
   the ticker: what this returns is a run the human can still stop."
  [panes]
  (when-let [pane (last (remove dormant? panes))]
    (let [title (flat-text (get-in pane [:view :title]))]
      (when-not (str/blank? title) title))))

(defn- max-offset
  "The last row the viewport may start on, from what the last paint measured."
  ^long [pane]
  (max 0 (- (long (or (:total pane) 0)) (long (or (:visible pane) 0)))))

(defn scrolled
  "The pane after the human moved the wheel `delta` rows over it (positive is
   DOWN). Scrolling up RELEASES follow-tail — reading back is always a deliberate
   intent — and a DOWNWARD gesture that reaches the last row re-arms it. The
   anchor is dropped because this gesture, not the previous paint, is now what
   says where the eye is; the next paint derives a fresh one from the row it
   lands on.

   Only a downward gesture ARMS the tail: an upward one that merely CLAMPS at the
   end (the plan shrank under it, or there is nothing to scroll at all) can keep
   a follow already in force but must never start one, or reading back would snap
   the pane to the live edge the moment its rows ran out."
  [pane delta]
  (let [delta
        (long delta)

        next-offset
        (clamp (+ (long (or (:offset pane) 0)) delta) 0 (max-offset pane))]

    (assoc pane
      :offset next-offset
      :anchor nil
      :is-following (and (>= next-offset (max-offset pane))
                         (or (pos? delta) (true? (:is-following pane)))))))

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
  (let [n
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

(def ^:private table-frame
  "The glyphs a table is drawn with. A live table wears the same box the rest of
   the TUI's tables wear, so the band reads it as ONE block instead of as a
   header and some lines that happen to line up."
  {:top ["┌" "┬" "┐"] :mid ["├" "┼" "┤"] :bottom ["└" "┴" "┘"]})

(defn- table-chrome
  "Columns a boxed table spends on its own frame: a rail at every column edge and
   a space of air on each side of every cell. Measuring and painting read this
   ONE number, so a table fitted to the band lands exactly on its right rail."
  ^long [n]
  (+ 1 (* 3 (max 1 (long n)))))

(defn- fitted-widths
  "`widths` squeezed into `text-w` by taking cells off the WIDEST column first, so
   a narrow terminal costs the one long free-text column and not the four short
   ones beside it. Only the PAINT shrinks — the pane keeps what it measured, so
   widening the terminal restores the columns instead of re-measuring them from
   whatever rows happen to be on screen."
  [widths text-w]
  (let [n
        (count widths)

        chrome
        (table-chrome n)

        room
        (max n (- (long text-w) chrome))]

    (loop [ws (mapv #(max 1 (long %)) widths)]
      (if (or (<= (long (reduce + 0 ws)) room) (every? #(<= (long %) 1) ws))
        ws
        (let [idx (first (apply max-key
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
  (let [n
        (count widths)

        chrome
        (table-chrome n)

        slack
        (- (long text-w) chrome (long (reduce + 0 (map long widths))))]

    (if (or (zero? n) (<= slack 0))
      (vec widths)
      (let [idx (first (apply max-key
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
  (let [cut
        (runs-cut runs w)

        pad
        (apply str (repeat (max 0 (- (long w) (runs-width cut))) \space))]

    (if (= :right align)
      (into [{:text pad}] (run-segments cut fg styles))
      (conj (vec (run-segments cut fg styles)) {:text pad}))))

(defn- table-segments
  "One table line INSIDE its frame — cells padded to their columns, a rail at
   every edge, all of it in the chrome's own dim ink so a toned row does not drag
   the box along with it."
  [widths cells aligns fg styles]
  (conj (into [{:text "│ " :fg t/dialog-hint}]
              (comp (map (fn [[runs w align]]
                           (cell-segments runs w align fg styles)))
                    (interpose [{:text " │ " :fg t/dialog-hint}])
                    cat)
              (map vector cells widths aligns))
        {:text " │" :fg t/dialog-hint}))

(defn- span-segments
  "A line that spans the whole table inside its frame: what a table says when it
   holds nothing yet."
  [widths text fg styles]
  (let [inner (+ (long (reduce + 0 (map long widths))) (* 3 (max 0 (dec (count widths)))))]
    [{:text "│ " :fg t/dialog-hint}
     {:text (p/pad-right (p/ellipsize (str text) inner) inner) :fg fg :styles (vec styles)}
     {:text " │" :fg t/dialog-hint}]))

(defn- rule-line
  "One frame line of a table: the corner glyphs `edge` names and a dash for every
   column the cells occupy, the pads included, so the rails of two lines stand in
   the same columns."
  [widths edge]
  (let [[l m r] (get table-frame edge)]
    (str l
         (str/join m
                   (map (fn [w]
                          (apply str (repeat (+ 2 (long w)) "─")))
                        widths))
         r)))

(defmulti ^:private node-rows
  "The rows ONE node contributes to the pane's single scroll surface — the whole
   live vocabulary, once, in the same order `live/->markdown` prints it."
  (fn [node _ctx]
    (:type node)))

(defmethod node-rows :status
  [{:keys [id text detail tone]} {:keys [text-w]}]
  (let [glyph
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
    (let [cells
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
      (cond-> (mapv
                (fn [{:keys [label detail value tone] :as step}]
                  (let [is-fresh (contains? fresh (:id step))
                        ink (tone-fg tone)
                        styles (if is-fresh [p/BOLD] [])
                        said (into
                               [{:text (str (get tone-glyph tone "·") " ") :fg ink :styles styles}]
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
                        segments (cond-> said
                                   reported
                                   (conj {:text (apply str
                                                  (repeat (max 1
                                                               (- (long text-w)
                                                                  (runs-width said)
                                                                  (runs-width reported)))
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
    (let [{:keys [shown]}
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
  [{:keys [id columns is-focusable focused-ids] :as node}
   {:keys [text-w widths fresh is-expanded is-interactive]}]
  (let [ordered
        (live/ordered-rows node)

        {:keys [shown behind]}
        (windowed ordered is-expanded false)

        ;; Measured from the painted window and never narrower than last time; the
        ;; FIT is what a cramped terminal does to the paint, and it is deliberately
        ;; not what the pane remembers. A focusable first cell also carries the same
        ;; two-column ●/○ state mark as the Companion.
        desired
        (cond-> (desired-widths columns shown)
          (and is-focusable (seq columns))
          (update 0 + 2))

        measured
        (grown-widths (get widths id) desired)

        ;; Squeezed if it must be, then GROWN into whatever the band has left: a
        ;; table is a block, and a block that ends in mid-air reads as unfinished.
        ws
        (filled-widths (fitted-widths measured text-w) text-w)

        aligns
        (mapv #(or (:align %) :left) columns)

        focused
        (set focused-ids)

        line
        (fn [cells fg styles]
          (let [segments (table-segments ws cells aligns fg styles)]
            {:segments segments :text (segment-line segments)}))

        rule
        (fn [edge]
          {:kind :trule :node-id id :text (rule-line ws edge)})

        ;; A rail between EVERY pair of rows: a live table is read while it fills,
        ;; and the eye needs the line that says where one row's answer ends and the
        ;; next one begins — especially when a cell wears a tone of its own.
        body
        (if (seq shown)
          (into []
                (comp (map-indexed
                        (fn [idx row]
                          (let [is-fresh
                                (contains? fresh (:id row))

                                is-focused
                                (contains? focused (:id row))

                                cells
                                (cond-> (mapv (fn [col-idx]
                                                (md-runs (cell-of row col-idx)))
                                              (range (count columns)))
                                  (and is-focusable (seq columns))
                                  (update 0 #(into [{:text (if is-focused "● " "○ ")}] %)))]

                            [(when (pos? (long idx)) (rule :mid))
                             (merge {:kind :trow
                                     :node-id id
                                     :item-id (:id row)
                                     :tone (:tone row)
                                     :is-fresh is-fresh
                                     :is-focusable (boolean (and is-focusable is-interactive))
                                     :is-focused is-focused}
                                    (line cells
                                          (if is-focused
                                            t/header-active-tab-accent
                                            (tone-fg (:tone row)))
                                          (if (or is-fresh is-focused) [p/BOLD] [])))])))
                      cat
                      (remove nil?))
                shown)
          (let [segments (span-segments ws (empty-text :table) t/dialog-hint [p/ITALIC])]
            [{:kind :empty :node-id id :segments segments :text (segment-line segments)}]))]

    (with-meta (cond-> (-> [(rule :top)
                            (merge
                              {:kind :thead :node-id id}
                              (line (mapv #(md-runs (:label %)) columns) t/dialog-hint [p/BOLD]))
                            (rule :mid)]
                           (into body)
                           (conj (rule :bottom)))
                 (pos? (long behind))
                 (conj (more-row id behind "row")))
      {:widths {id measured}})))

(defmethod node-rows :link
  [{:keys [id links]} {:keys [fresh is-expanded]}]
  (if (seq links)
    (let [{:keys [shown behind]} (windowed links is-expanded false)]
      (cond-> (mapv (fn [{:keys [label target-kind target tone] :as link}]
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

(def ^:private min-column-w
  "The narrowest column a node may be laid into. Under it the terminal has no room
   for two of anything and a `row` group STACKS: a row says where its nodes stand
   when there IS room, never that a surface must cram."
  24)

(defn- stacked-rows
  "Sections one under the next, a row of air between them — what a `column` group
   paints, what a view's own nodes do, and what a band too narrow to split falls
   back to."
  [sections]
  (into []
        (comp (map-indexed (fn [idx rows]
                             (if (pos? (long idx)) (into [{:kind :blank}] rows) rows)))
              cat)
        sections))

(defn- split-rows
  "Sections side by side: [[columns/zip-columns]] lines them up line for line, and
   the composite row takes its anchor from the first cell that names a node, so
   the reading position still belongs to a node."
  [sections]
  (mapv (fn [row]
          (let [lead (first (filter :node-id (:cells row)))]
            (assoc row
              :node-id (:node-id lead)
              :item-id (:item-id lead))))
        (columns/zip-columns sections)))

(defn- node-section
  "One node's rows, `text-w` columns wide: its label, then what the node paints.

   A LAYOUT GROUP paints its CHILDREN instead — side by side when it is a `:row`
   and every column would still be at least [[min-column-w]] wide, one under the
   next otherwise. That is how a table gets prose down its right-hand side, and
   how the same view stays readable on a terminal half as wide. Groups nest, so
   the two directions compose without another rule, and they are the FORM's own
   groups: one layout vocabulary, one painter, [[columns/cell-width]] deciding
   how wide a column is for both.

   The row of air between two sections belongs to whoever stacks them, never to
   the node, so two nodes standing side by side start on the same line."
  [node ctx fresh text-w]
  (let [children
        (not-empty (:fields node))

        cell-w
        (when children (columns/cell-width text-w (count children)))

        is-split
        (boolean (and children (= :row (:direction node)) (>= (long cell-w) (long min-column-w))))

        parts
        (when children (mapv #(node-section % ctx fresh (if is-split cell-w text-w)) children))

        body
        (cond (nil? children) (node-rows node (ctx node text-w))
              is-split (split-rows parts)
              :else (stacked-rows parts))

        label
        (flat-text (:label node))]

    (with-meta (cond-> []
                 (seq label)
                 (conj {:kind :node
                        :node-id (:id node)
                        :text label
                        :is-fresh (contains? (:nodes fresh) (:id node))})

                 :always
                 (into body))
      {:widths (reduce merge {} (map (comp :widths meta) (or parts [body])))})))

(defn plan
  "The pane's whole paint plan, `text-w` columns wide: one entry per painted row,
   nodes in DECLARATION order, each under its own label with a row of air above
   it. A node the run wrapped in a `row` group stands BESIDE its siblings wherever
   the band is wide enough for all of them. Carries `{:widths …}` as metadata —
   what the tables measured this pass, on its way back into the pane through
   [[painted]]."
  [pane text-w]
  (let [{:keys [view widths fresh expanded]}
        pane

        ctx
        (fn [node w]
          {:text-w w
           :widths widths
           :fresh (get-in fresh [:items (:id node)] #{})
           :is-expanded (contains? (set expanded) (:id node))
           :is-interactive (not (settled? pane))})

        head
        (mapv (fn [runs]
                {:kind :prose :runs runs :text (segment-line runs)})
              (md-lines (:description view) text-w))

        sections
        (mapv #(node-section % ctx fresh (long text-w)) (:nodes view))]

    (with-meta (into head (stacked-rows sections))
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
  (let [rows
        (vec rows)

        limit
        (max 0 (- (count rows) (long visible)))

        [node-id item-id :as anchor]
        (:anchor pane)

        found
        (when anchor
          (or (first (keep-indexed
                       (fn [idx row]
                         (when (and (= node-id (:node-id row)) (= item-id (:item-id row))) idx))
                       rows))
              (first (keep-indexed (fn [idx row]
                                     (when (= node-id (:node-id row)) idx))
                                   rows))))]

    (cond (:is-following pane) limit
          found (clamp (long found) 0 limit)
          :else (clamp (long (or (:offset pane) 0)) 0 limit))))

;;; ── Chrome ──────────────────────────────────────────────────────────────────

(defn- title-line
  "What rides the band's opening rule: the view's title and how long it has been
   open. `source` — the extension that opened it — comes after the title, because
   the first thing the human asks a picture that appeared on its own is who put
   it there."
  [pane now-ms]
  (let [{:keys [title source created-at]}
        (:view pane)

        ;; A settled view stops counting: what a finished run wears is how long it
        ;; TOOK, not how long ago it ended.
        end
        (long (or (:ended-at (:settled pane)) now-ms))]

    (str/join " · "
              (remove str/blank?
                [(flat-text title) (flat-text source)
                 (elapsed-text (- end (long (or created-at end))))]))))

(defn- status-summary
  "The one line a view is worth when it is not the pane in front: its newest
   status, else its progress, else what it is called."
  [pane]
  (let [nodes
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
  "The pane a stop would hit: the newest open extension view.

   Host Activity reports the enclosing Python evaluation and cannot be stopped
   independently; the ordinary turn cancel remains authoritative. The footer and
   abort branch both ask here, so neither can advertise or invoke a false action."
  [panes]
  (last (remove #(or (settled? %) (activity? %)) panes)))

(defn stopping
  "The note the human is typing into an ARMED stop on `pane` — `\"\"` the moment
   Escape arms it — or nil while the view is only being watched."
  [pane]
  (:stop pane))

(defn armed
  "The pane with its stop ARMED: Escape asked to interrupt and the band takes the
   keyboard for one line. NOTHING is stopped yet — Escape again (or Enter) sends it,
   so the comment travels WITH the stop instead of arriving after it, and one
   mistaken Escape never kills work the human still wanted. A compact pane restores
   first, because the note field must never be hidden."
  [pane]
  (let [pane (restored pane)]
    (cond-> pane
      (nil? (stopping pane))
      (assoc :stop ""))))

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

(defn- has-focusable-table?
  "True when `pane` carries a live table whose rows change the detail below it."
  [pane]
  (boolean (some #(and (= :table (:type %)) (:is-focusable %)) (get-in pane [:view :nodes]))))

(defn hint
  "The hint bar under the band. Escape is the ONE key an interruptible view takes,
   and while several are open it says WHICH one it will hit — the newest, the one
   the band is painting. System Activity is read-only: its expanded rail offers only
   fold/restore controls. A focusable table advertises its click. Once a stop is armed,
   the bar says the two keys that end typing: Escape or Enter interrupt with whatever
   was written, Backspace on an empty line keeps watching."
  [pane others]
  (let [open (remove settled? others)]
    (if-let [note (stopping pane)]
      (if (str/blank? note)
        [["Esc / ⏎" "interrupt"] ["⌫" "keep watching"]]
        [["Esc / ⏎" "interrupt with the note"] ["⌫" "erase"]])
      (if (activity? pane)
        [["click ▴" "close Activity"]]
        (if (minimized? pane)
          [["click ▴" "restore live view"]
           ["Esc" (str "interrupt " (flat-text (get-in pane [:view :title])))]]
          (cond-> []
            (and (some? pane) (not (settled? pane)))
            (conj ["click ▾" "minimize"])

            (and (some? pane) (not (settled? pane)) (has-focusable-table? pane))
            (conj ["click" "focus a row"])

            (and (some? pane) (not (settled? pane)))
            (conj ["Esc" (str "interrupt " (flat-text (get-in pane [:view :title])))])

            (and (some? pane) (settled? pane))
            (conj ["click" "close the record"])

            (seq open)
            (conj [(str (+ (if pane 1 0) (count open))) "views open"])))))))

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
  (let [counted
        (when done (str done (when total (str "/" total)) " done"))

        value
        (live/fraction entry)]

    (if (nil? value)
      (str/join "  ·  " (remove str/blank? ["working" counted]))
      (let [pct
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
    (do (paint-segments! g left row inner-w (:segments entry))
        ;; The full visible table row is the control, not a tiny glyph. Its item id
        ;; is what the shared focus patch names; the next live patch repaints every
        ;; attached TUI and Companion from that one engine state.
        (when (:is-focusable entry)
          (cr/register! {:bounds
                         {:row row :col (+ (long left) 2) :width (max 0 (- (long inner-w) 3))}
                         :kind :live-focus
                         :view-id view-id
                         :node-id (:node-id entry)
                         :item-id (:item-id entry)
                         :enabled? true})))

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
        (when-let [kind (case (:target-kind entry)
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
        (reduce (fn [pos [[x width] cell]]
                  (let [here (when cell
                               (paint-entry! g (+ (long left) (long x)) row width view-id cell))]
                    (or pos here)))
                nil
                (map vector (columns/slots inner-w (count (:cells entry))) (:cells entry))))

    ;; A minimized LIVE pane is still a control: the whole status row restores its
    ;; full surface while patches continue to land behind it.
    :minimized
    (do (paint-styled! g left row inner-w t/dialog-hint-key [p/BOLD] (:text entry))
        (cr/register! {:bounds {:row row :col (inc (long left)) :width (long inner-w)}
                       :kind :live-restore
                       :view-id view-id
                       :enabled? true}))

    ;; A view standing BEHIND the one in front: one line saying where it got to.
    ;; Nothing here is pressable, because a run that has ENDED is not on the band
    ;; at all — its row is in the transcript, and that row is the door to the
    ;; record.
    :collapsed
    (paint-plain! g left row inner-w (tone-fg (:tone entry) t/dialog-hint) (:text entry))

    ;; Everything that is prose — the view's description, a status detail, a node
    ;; that holds nothing, the count of what the record kept — speaks in the dim
    ;; italic voice the rest of the TUI reserves for explanation.
    (cond (:segments entry) (paint-segments! g left row inner-w (:segments entry))
          (:runs entry) (paint-runs! g left row inner-w t/dialog-hint [p/ITALIC] (:runs entry))
          :else (paint-styled! g left row inner-w t/dialog-hint [p/ITALIC] (:text entry)))))

(defn- collapsed-row
  "The ONE line an open view keeps above the band while another is in front: its
   title and where it got to. Newest last, so the pane in front is the one the
   human is watching.

   A SETTLED view has no line here — it left the band for the transcript."
  [pane]
  (let [{:keys [tone text]} (status-summary pane)]
    {:kind :collapsed
     :node-id nil
     :view-id (get-in pane [:view :id])
     :tone tone
     :text (str (or (tone-glyph tone) "▸")
                " "
                (str/join " · "
                          (remove str/blank? [(flat-text (get-in pane [:view :title])) text])))}))

(defn- minimized-row
  "The compact row a folded active view leaves: current status plus an explicit
   minimized label, so it cannot be mistaken for a finished transcript record."
  [pane pane-count]
  (let [{:keys [tone text]} (status-summary pane)]
    {:kind :minimized
     :node-id nil
     :view-id (view-id pane)
     :tone tone
     :text (str (or (tone-glyph tone) "▸")
                " "
                (str/join " · "
                          (remove str/blank?
                            [text "minimized"
                             (when (> (long pane-count) 1) (str pane-count " views open"))])))}))

(defn- paint-fold-control!
  "Paint the title control. Activity closes its transcript disclosure; generic live
   views retain minimize/restore semantics."
  [g {:keys [left inner-w]} row pane]
  (when pane
    (let [activity
          (activity? pane)

          generic-running
          (not (settled? pane))]

      (when (or activity generic-running)
        (let [label
              (if activity " ▴ " (if (minimized? pane) " ▴ " " ▾ "))

              width
              (long (p/display-width label))

              col
              (max (inc (long left)) (- (+ (long left) (long inner-w) 1) width))]

          (p/set-colors! g t/dialog-hint-key t/dialog-bg)
          (p/styled g [p/BOLD] (p/put-str! g col row label))
          (cr/register!
            {:bounds {:row row :col col :width width}
             :kind (if activity :live-reopen (if (minimized? pane) :live-restore :live-minimize))
             :view-id (view-id pane)
             :enabled? true}))))))

(def ^:private activity-detail-rows
  "Expanded Activity is a transcript disclosure, not a live dashboard. Its ten-row
   reservation is stable while patches land, bounded on tall terminals, and yields to
   the composer on short ones."
  10)

(defn- anchor-floated-region
  "REGION lifted so the expanded Activity surface's TOP row sits directly under its
   collapsed transcript receipt at `anchor-row` (a screen row; nil docks the band
   above the prompt as before). The surface is the receipt's disclosure, so it must
   move WITH the receipt as the transcript scrolls, not hang at the bottom of the
   terminal while its anchor walks away."
  [region anchor-row]
  (if (nil? anchor-row)
    region
    (assoc region
      :hint-row (min (long (:hint-row region))
                     (+ (long anchor-row) 1 (long activity-detail-rows))))))

(defn activity-focused
  "Move the terminal-local Activity operation focus to `item-id`. This never publishes a
   shared Live View focus patch: Activity focus only controls its own evidence disclosure."
  [pane item-id]
  (assoc pane :activity-focused item-id))

(defn activity-evidence-toggled
  "Focus one Activity operation and toggle its bounded evidence row."
  [pane item-id]
  (-> pane
      (activity-focused item-id)
      (update :activity-evidence
              (fn [ids]
                (let [ids (set ids)]
                  (if (contains? ids item-id) (disj ids item-id) (conj ids item-id)))))))

(defn- activity-plan
  "PURE: the compact Activity presenter plan. It consumes the one semantic Activity
   projection directly; no generic Live View nodes mirror its status, counts, or rows."
  [pane _text-w]
  (let [activity
        (get-in pane [:view :activity])

        rows
        (:rows activity)

        focused
        (or (:activity-focused pane) (:id (first rows)))

        evidence
        (set (:activity-evidence pane))

        counts
        (:counts activity)]

    (into [{:kind :activity-status
            :tone (activity-row-tone (:state activity))
            :text (activity-status-text pane)}
           {:kind :activity-counts
            :text (str/join "   "
                            (map (fn [[state label]]
                                   (str label " " (max 0 (long (get counts state 0)))))
                                 [[:running "Running"] [:succeeded "Succeeded"] [:failed "Failed"]
                                  [:cancelled "Cancelled"]]))}]
          (mapcat (fn [{:keys [id state] :as activity-row}]
                    (let [detail
                          (activity-row-detail activity-row)

                          row
                          {:kind :activity-step
                           :item-id id
                           :tone (activity-row-tone state)
                           :text (activity-row-label activity-row)
                           :detail detail
                           :is-focused (= focused id)}]

                      (cond-> [row]
                        (and (contains? evidence id) (seq detail))
                        (conj {:kind :activity-evidence
                               :item-id id
                               :tone (activity-row-tone state)
                               :text detail}))))
                  rows))))

(defn- activity-shape
  "PURE: bounded transcript-native Activity geometry and rows. This intentionally does
   not call [[band-shape]]: Activity owns neither its four-fifths reservation nor any
   generic collapsed, stop, rail, or hint-bar furniture."
  [pane region]
  (let [available
        (max 0 (- (long (:hint-row region)) (long (:min-row region))))

        n
        (min (long activity-detail-rows) available)

        text-w
        (max 8 (- (long (:inner-w region)) 3))]

    {:pane pane :rows-plan (activity-plan pane text-w) :n n}))

(defn- band-title
  "The generic Live View title. Activity never reaches this shell."
  [pane now-ms]
  (title-line pane now-ms))

(defn- band-shape
  "PURE: what the band is made of on `region` — the live panes oldest first, the
   collapsed lines behind the pane in FRONT, that pane's plan rows, its armed
   stop, and `:n`, the display rows the band geometry is asked for.

   The HEIGHT is decided once, HERE, because two callers have to agree on it:
   [[paint!]] draws that many rows and [[band-rows]] tells the wheel which rows
   those are. A band drawn taller than the wheel believes hands the top of the
   pane's own scroll back to the transcript underneath it, which reads as a live
   view that only scrolls near its footer."
  [panes region]
  (let [;; A settled view is not band furniture: it has already given its rows
        ;; back to the transcript, where its own row now waits.
        panes
        (vec (remove dormant? panes))

        ;; The body opens one column inside the rails and the row painters take
        ;; the next one, so nothing a view paints ever touches a rail — and the
        ;; right lane stays clear for the scrollbar.
        text-w
        (max 8 (- (dec (long (:inner-w region))) 4))

        ;; The pane IN FRONT is the newest view still on the band.
        front
        (last panes)

        minimized-front?
        (minimized? front)

        others
        (if front (vec (butlast panes)) (vec panes))

        ;; Folding the transient returns ALL of its body rows to the transcript;
        ;; older open views remain represented by the compact row's count.
        collapsed
        (if minimized-front? [] (mapv collapsed-row others))

        rows-plan
        (cond minimized-front? [(minimized-row front (count panes))]
              front (plan front text-w)
              :else [])

        ;; Four fifths of the rows between the transcript top and the prompt, at
        ;; most: a watched run is the live surface, while one fifth still keeps the
        ;; conversation that launched it in sight. Four rows is the floor — a band
        ;; shorter than that says nothing at all. A minimized surface keeps only one
        ;; status row plus the chrome needed to make its restore action explicit.
        available
        (max 0 (- (long (:hint-row region)) 1 (long (:min-row region))))

        room
        ;; `band-geometry` spends one enclosing row outside this body budget. Add
        ;; it here so the complete visible band, not merely its body, reaches 4/5.
        (if minimized-front? 2 (max 4 (min available (inc (quot (* 4 available) 5)))))

        ;; An ARMED stop takes two body rows: the line the human types into and
        ;; the rule that fences it off from the view above. The band asks WHY it
        ;; is being stopped where it is being stopped, and the answer rides along
        ;; with the stop — but the question is the BAND speaking, not one more row
        ;; of the run's own report, so it is ruled off on both sides.
        stop
        (stop-prompt front)

        ;; `room` is the surface contract, not merely a content cap. A sparse live
        ;; view still owns four fifths of the terminal so incoming rows do not make
        ;; the whole conversation jump downward as work progresses.
        n
        room]

    {:panes panes
     :front front
     :others others
     :collapsed collapsed
     :rows-plan rows-plan
     :stop stop
     :is-minimized minimized-front?
     :n n}))

(defn band-rows
  "The rows an expanded surface covers, inclusive. Activity uses its dedicated fixed
   transcript-detail bound; ordinary Live Views retain [[band-shape]] geometry. When
   `anchor-row` names the screen row of the front Activity's collapsed receipt, the
   surface floats directly under it (see [[anchor-floated-region]])."
  ([cols rows panes content-top prompt-h] (band-rows cols rows panes content-top prompt-h nil))
  ([cols rows panes content-top prompt-h anchor-row]
   (when-let [front (last (remove dormant? panes))]
     (let [region (tr/band-region (long cols) (long rows) (long content-top) (long prompt-h))]
       (if (activity? front)
         (let [region (anchor-floated-region region anchor-row)
               n (:n (activity-shape front region))
               to (dec (long (:hint-row region)))]

           (when (pos? (long n)) [(- (inc to) (long n)) to]))
         (let [{:keys [sep-row foot-row]}
               (tr/band-geometry region (:n (band-shape panes region)) false)]
           [(long sep-row) (long foot-row)]))))))

(defn- paint-generic!
  "Draw the ordinary Live View band. Activity is dispatched by [[paint!]] before this
   generic four-fifths-height shell, so its geometry can never drift back here."
  ([g cols rows panes content-top prompt-h]
   (paint-generic! g cols rows panes content-top prompt-h (System/currentTimeMillis)))
  ([g cols rows panes content-top prompt-h now-ms]
   (when (some (complement dormant?) panes)
     (let [;; A settled view is not band furniture: it has already given its rows
           ;; back to the transcript, where its own row now waits. The band exists
           ;; while something is still happening — or while a record is being read
           ;; again.
           panes
           (vec (remove dormant? panes))

           {:keys [left inner-w] :as region}
           (tr/band-region (long cols) (long rows) (long content-top) (long prompt-h))]

       (binding [t/dialog-bg (if (:is-sideless region) t/terminal-bg t/dialog-bg)]
         (let [left (long left)
               inner-w (long inner-w)
               body-w (dec inner-w)
               {:keys [front others collapsed rows-plan stop is-minimized n]} (band-shape panes
                                                                                          region)
               {:keys [sep-row body-top foot-rule-row foot-row visible top-limit]}
               (tr/band-geometry region n false)
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
               start (if (and front (not is-minimized)) (offset front rows-plan body-visible) 0)
               shown (subvec (vec rows-plan) (min start total) (min total (+ start body-visible)))
               view-id (view-id front)]

           (tr/clear-rows! g region (max 0 (long sep-row)) rule-at)
           ;; The title is the rule's own label — `── CI · 1m 12s ──` — so the
           ;; first row is chrome and every row under it is the view.
           (when (>= (long sep-row) (long top-limit))
             (tr/draw-rule! g
                            region
                            sep-row
                            (p/ellipsize (band-title (or front (last panes)) now-ms)
                                         (max 1 (- inner-w 10))))
             (paint-fold-control! g region sep-row front))
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
             (let [note-row (dec (long hint-rule-at))
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
           (dialogs/draw-hint-bar! g left hint-at inner-w (hint front others))
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
           (if is-minimized
             {:view-id view-id
              :offset (:offset front)
              :anchor (:anchor front)
              :total (:total front)
              :visible (:visible front)
              :widths (:widths front)}
             {:view-id view-id
              :offset start
              :anchor (anchor-at rows-plan start)
              :total total
              :visible body-visible
              :widths (:widths (meta rows-plan))})))))))

(defn- paint-activity-entry!
  "Paint one Activity presenter row without borrowing generic Live View body chrome."
  [g left row width view-id entry]
  (let [col
        (+ (long left) 2)

        width
        (max 1 (- (long width) 2))

        put
        (fn [fg styles text]
          (p/set-colors! g fg t/terminal-bg)
          (p/styled g styles (p/put-str! g col row (p/ellipsize (str text) width))))]

    (case (:kind entry)
      :activity-status
      (put (tone-fg (:tone entry))
           [p/BOLD]
           (str (or (tone-glyph (:tone entry)) "·") " " (:text entry)))

      :activity-counts
      (put t/dialog-hint [] (:text entry))

      :activity-step
      (let [focused?
            (:is-focused entry)

            marker
            (if focused? "› " "  ")

            glyph
            (str (or (tone-glyph (:tone entry)) "·") " ")]

        (put (if focused? t/header-active-tab-accent (tone-fg (:tone entry)))
             (if focused? [p/BOLD] [])
             (str marker glyph (:text entry)))
        (cr/register! {:bounds {:row row :col col :width 2}
                       :kind :activity-focus
                       :view-id view-id
                       :item-id (:item-id entry)
                       :enabled? true})
        (when (seq (:detail entry))
          (cr/register! {:bounds {:row row :col (+ col 2) :width (max 1 (- width 2))}
                         :kind :activity-evidence
                         :view-id view-id
                         :item-id (:item-id entry)
                         :enabled? true})))

      :activity-evidence
      (put t/dialog-hint [p/ITALIC] (str "    ↳ " (:text entry)))

      :activity-fallback
      (put t/dialog-hint [] (str "  · " (:text entry)))

      nil)))

(defn- paint-activity!
  "Paint one expanded Activity as a bounded transcript disclosure. The semantic rail
   and concise headline are the only chrome; there are no dialog rails, title rule,
   stop footer, generic hint bar, or reserved blank dashboard. Body entries retain the
   existing evidence and focus click contracts."
  [g cols rows pane content-top prompt-h now-ms anchor-row]
  (let [{:keys [left inner-w hint-row] :as region}
        (anchor-floated-region
          (tr/band-region (long cols) (long rows) (long content-top) (long prompt-h))
          anchor-row)

        {:keys [rows-plan n]}
        (activity-shape pane region)]

    (when (pos? (long n))
      (binding [t/dialog-bg t/terminal-bg]
        (let [left (long left)
              inner-w (long inner-w)
              top (- (long hint-row) (long n))
              bottom (dec (long hint-row))
              body-top (inc top)
              visible (max 0 (dec (long n)))
              total (count rows-plan)
              start (offset pane rows-plan visible)
              shown (subvec (vec rows-plan) (min start total) (min total (+ start visible)))
              view-id (view-id pane)
              {:keys [text tone]} (status-summary pane)
              state (if (settled? pane) "SETTLED" "LIVE")
              ended-at (long (or (get-in pane [:settled :ended-at]) now-ms))
              created-at (long (or (get-in pane [:view :created-at]) ended-at))
              headline (str "▾ ACTIVITY · "
                            state
                            (when-not (str/blank? text) (str " · " text))
                            " · "
                            (elapsed-text (- ended-at created-at))
                            (when (> total visible)
                              (str " · " (inc start) "–" (min total (+ start visible)) "/" total)))]

          (tr/clear-rows! g region top bottom)
          (p/set-colors! g (tone-fg tone t/header-active-tab-accent) t/terminal-bg)
          (doseq [row (range top (inc bottom))]
            (p/set-char! g left row \▎))
          (p/set-colors! g t/dialog-fg t/terminal-bg)
          (p/styled g
                    [p/BOLD]
                    (p/put-str! g (+ left 2) top (p/ellipsize headline (max 1 (- inner-w 3)))))
          (cr/register! {:bounds {:row top :col left :width (+ inner-w 1)}
                         :kind :live-reopen
                         :view-id view-id
                         :enabled? true})
          (doseq [[idx entry] (map-indexed vector shown)]
            (paint-activity-entry! g left (+ body-top (long idx)) inner-w view-id entry))
          (doseq [idx (range (count shown) visible)]
            (paint-activity-entry! g left (+ body-top (long idx)) inner-w view-id nil))
          (p/clear-styles! g)
          {:view-id view-id
           :offset start
           :anchor (anchor-at rows-plan start)
           :total total
           :visible visible
           :widths (:widths (meta rows-plan))})))))

(defn paint!
  "Paint the newest expanded surface. Host Activity always takes the dedicated bounded
   transcript painter; ordinary Live Views keep their established generic painter.
   `anchor-row` is the screen row of the front Activity's collapsed transcript
   receipt: the expanded surface floats directly under it instead of docking above
   the prompt, so scrolling the transcript never separates the pair."
  ([g cols rows panes content-top prompt-h]
   (paint! g cols rows panes content-top prompt-h (System/currentTimeMillis)))
  ([g cols rows panes content-top prompt-h now-ms]
   (paint! g cols rows panes content-top prompt-h now-ms nil))
  ([g cols rows panes content-top prompt-h now-ms anchor-row]
   (when-let [front (last (remove dormant? panes))]
     (if (activity? front)
       (paint-activity! g cols rows front content-top prompt-h now-ms anchor-row)
       (paint-generic! g cols rows panes content-top prompt-h now-ms)))))
