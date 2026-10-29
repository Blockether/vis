(ns com.blockether.vis.tui.live-view
  "TUI painting for a LIVE VIEW — the picture an extension streams WHILE it works
   (`com.blockether.vis.tui.view-model`, materialized by
   `com.blockether.vis.tui.view-materializer`).

   A form is a QUESTION and owns the keyboard until it is answered; a view is a
   PICTURE and leaves the composer focused. The wheel over the band scrolls it, and
   clicks open links, expand nodes, select rows in a selectable table, or fold the
   live surface down to a compact status line without stopping it. F3 opens its
   keyboard controls. Log searches keep their query and wrapped results in this same
   band; Escape returns from search to the view. Otherwise, Escape ARMS a stop on
   the newest open view before it interrupts
   the turn: the band then takes one FENCED line for the comment the human types,
   Escape or Enter interrupts with it, Backspace on an empty line keeps watching.
   A view is ALWAYS stoppable; the note is what says why.

   Everything except [[paint!]] is PURE: [[opened]] builds the pane from the
   engine's own materialized view, [[patched]] is the reducer over one patch,
   [[plan]] is the paint plan, [[offset]] says where the viewport sits and
   [[painted]] takes back what the paint measured. Key decoding and painting are
   the Lanterna edges; the reducers can be tested without a terminal.

   ONE SCROLL SURFACE. A view is a STACK of labelled nodes in declaration order,
   so three tables and two logs read as sections of one document instead of as
   competing panes. What the eye is on is an ANCHOR — the node id, and inside a
   keyed node the item id, of the row at the TOP of the viewport — never a line
   offset: rows arriving above it move the scrollbar, not the reading position.
   A viewport parked at the end FOLLOWS new rows; scrolling up releases the
   follow and landing back on the last row re-arms it.

   A KEYED NODE PAINTS A WINDOW — [[node-window]] item rows and one line saying
   how many more, which a click expands: a pane that painted five thousand table
   rows would bury the four nodes under it. A LOG NEVER DOES. Output is what the
   human came to read, so an open log paints every line it holds, and
   [[log-fill-request]] reads the record back to its first line for whatever this
   terminal attached too late to be sent.

   SEVERAL VIEWS AT ONCE: the newest paints in full and every older one keeps ONE
   collapsed line above it. A band shares its rows with the transcript and the
   prompt, so three full panes would leave none of them readable — and the
   collapsed line still carries the title, the state, and the fact that Escape
   hits the newest one first."
  (:require [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis.tui.app-model :as app-model]
            [com.blockether.vis.tui.view-model :as view-model]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.components :as components]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.columns :as columns]
            [com.blockether.vis.tui.dialogs :as dialogs]
            [com.blockether.vis.tui.human-input :as hi]
            [com.blockether.vis.tui.input :as input]
            [com.blockether.vis.tui.markdown-layout :as layout]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.theme :as t]
            [com.blockether.vis.tui.transient :as tr]
            [com.blockether.vis.tui.scene-motion :as scene-motion]
            [com.blockether.vis.tui.scene-raster :as scene-raster]
            [com.blockether.vis.tui.view-materializer :as live]
            [com.blockether.vis.contract.view :as hi-spec])
  (:import [com.googlecode.lanterna TerminalPosition TextColor$RGB]
           [com.googlecode.lanterna.graphics TextGraphics]
           [com.googlecode.lanterna.gui2 Direction ScrollBar]
           [com.googlecode.lanterna.input KeyStroke KeyType]))

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

(defn- log-ink
  "Use readable theme ink when an accent cannot serve as small log text."
  [tone]
  (letfn [(channel ^double [^long c]
            (let [v (/ (double c) 255.0)]
              (if (<= v 0.04045) (/ v 12.92) (Math/pow (/ (+ v 0.055) 1.055) 2.4))))
          (luminance ^double [^com.googlecode.lanterna.TextColor color]
            (+ (* 0.2126 (channel (.getRed color)))
               (* 0.7152 (channel (.getGreen color)))
               (* 0.0722 (channel (.getBlue color)))))]
    (let [ink
          (tone-fg tone)

          foreground
          (luminance ink)

          background
          (luminance t/dialog-bg)

          contrast
          (/ (+ (max foreground background) 0.05) (+ (min foreground background) 0.05))]

      (if (>= contrast 4.5) ink t/dialog-fg))))

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
   says so with the same `…` the rest of the TUI uses. Every other key a run
   carries rides through the cut, so this trims painted SEGMENTS — ink, styles
   and all — as readily as markdown runs."
  [runs w]
  (loop [out
         []

         used
         0

         rs
         (seq runs)]

    (if (or (nil? rs) (>= (long used) (long w)))
      out
      (let [run
            (first rs)

            shown
            (p/ellipsize (str (:text run)) (- (long w) (long used)))]

        (recur (cond-> out
                 (seq shown)
                 (conj (assoc run :text shown)))
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
   :disclosures {}
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
   coalesces ops the bus already applied one at a time.

   New output also clears a FAILED record read, so the next line that lands
   retries the earlier ones this pane has not seen."
  [pane patch]
  (if (<= (long (or (:seq patch) 0)) (long (or (get-in pane [:view :seq]) 0)))
    pane
    (-> pane
        (update :view live/apply-patch patch)
        (dissoc :log-fill-error)
        (assoc :fresh (touched patch)))))

(def log-fill-page-size
  "Record lines ONE unasked read takes. The gateway serves at most the default log
   window in a page (`gateway.view/live-log-page`), so this is the largest read
   that comes back whole."
  2000)

(defn log-fill-request
  "The record page an open log is still MISSING — `{:node-id … :from … :limit …}`
   for `client/live-view-log` — or nil when this pane holds every line the record
   has.

   A terminal that attached mid-run was handed the producer's window and nothing
   before it. That is a hole in what a human is reading, so the pane READS IT BACK
   instead of printing a note about it: the newest missing page first, walking to
   line 0. One read is in flight at a time, and a read that FAILED waits for the
   next line of output rather than hammering the gateway."
  [pane]
  (when-not (:log-fill pane)
    (let [unread
          (or (:log-fill-error pane) #{})

          missing
          (fn [node]
            (max 0 (- (long (or (:total-lines node) 0)) (count (:lines node)))))

          node
          (first (filter #(and (= :log (:type %))
                               (not (contains? unread (:id %)))
                               (pos? (long (missing %))))
                         (mapcat #(tree-seq :children :children %) (get-in pane [:view :nodes]))))]

      (when node
        (let [behind
              (long (missing node))

              from
              (max 0 (- behind (long log-fill-page-size)))]

          {:node-id (:id node) :from from :limit (- behind from)})))))

(defn log-fill-requested
  "The pane with one record read in flight. Its identity is what makes a page that
   comes back after a newer read, a `clear` or a tab switch harmless."
  [pane node-id request-id]
  (assoc pane :log-fill {:node-id node-id :request-id request-id}))

(defn log-filled
  "The pane one answered read leaves behind: the page's lines standing in front of
   the log they belong to. A refusal — or a page the record could not serve —
   remembers the node as unread, which is what the pane SAYS instead of promising
   lines nothing is fetching. Only the current read is accepted."
  [pane request-id {:keys [page error]}]
  (if (not= request-id (:request-id (:log-fill pane)))
    pane
    (let [node-id
          (:node-id (:log-fill pane))

          {:keys [lines line-tones]}
          (when-not error (view-model/live-log-page<-wire page))]

      (if (seq lines)
        (-> (dissoc pane :log-fill)
            (update :view live/log-head-filled node-id (vec lines) (vec line-tones)))
        (-> (dissoc pane :log-fill)
            (update :log-fill-error (fnil conj #{}) node-id))))))

(defn settled
  "The pane a close leaves behind — the run's FINAL picture and the verdict that
   ended it, stamped `ended-at`.

   A settled pane is not dropped: it collapses to ONE line the human can press.
   Dismissing the pane the instant the work finishes is exactly what made a
   watched log unreachable — the artifact the close files exists so it never is,
   and this line is the door to it."
  ([pane result] (settled pane result (System/currentTimeMillis)))
  ([pane result ended-at]
   (-> pane
       (dissoc :stop :is-minimized)
       (update :view merge (:view result))
       (assoc :disclosures {})
       (assoc :settled (-> (select-keys result [:reason :artifact-id :is-from-human])
                           (assoc :ended-at ended-at))))))

(defn settled?
  "True when this view has ENDED and the pane is its record."
  [pane]
  (contains? pane :settled))

(defn dormant?
  "True when a pane belongs in its transcript receipt instead of the live band.
   A view moves to the transcript after it settles."
  [pane]
  (and (settled? pane) (not (:is-reopened pane))))

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
  "Toggle a settled run's transcript disclosure into the read-only band."
  [pane]
  (cond-> (restored pane)
    (settled? pane)
    (-> (update :is-reopened not)
        (assoc :is-following true
               :offset 0))))

(defn recorded-pane
  "Restore the sealed picture from a durable NDJSON record. The closing snapshot
   owns mutable presentation; the declaration owns identity. Incomplete records
   are refused rather than displayed as a finished run. No live action is replayed."
  [source session-id]
  (let [{:keys [view result ended-at]}
        (reduce (fn [record line]
                  (if (str/blank? line)
                    record
                    (let [frame (json/read-json line)]
                      (case (get frame "kind")
                        "open"
                        (assoc record :view (view-model/live-view<-wire (get frame "view")))

                        "close"
                        (assoc record
                          :result (view-model/live-result<-wire (get frame "result"))
                          :ended-at (get frame "at"))

                        record))))
                {}
                (str/split-lines source))]
    (when-not (and (:id view) (map? (:view result)) (sequential? (get-in result [:view :nodes])))
      (throw (ex-info "Live view record is incomplete. Reopen it after the run finishes." {})))
    (-> (opened (merge view (:view result) {:id (:id view) :session-id session-id}))
        (settled (update (dissoc result :view)
                         :reason
                         #(some-> %
                                  keyword))
                 (or ended-at 0))
        reopened
        (assoc :is-following false))))

(defn run-row
  "The transcript receipt for a settled extension run, anchored at its form.

   Activity is NOT one of these: it belongs to the form that produced it and travels
   on that form's own envelope, so no run row has to carry it."
  [pane]
  (let [{:keys [reason ended-at]}
        (:settled pane)

        view
        (:view pane)

        lines
        (reduce +
                0
                (keep #(when (= :log (:type %)) (or (:total-lines %) (count (:lines %))))
                      (:nodes view)))

        end
        (long (or ended-at (System/currentTimeMillis)))]

    (cond-> {:view-id (:id view)
             :title (flat-text (:title view))
             :reason reason
             :lines (long lines)
             :elapsed-ms (max 0 (- end (long (or (:created-at view) end))))
             :is-reopened (boolean (:is-reopened pane))}
      (:owner view)
      (assoc :owner (:owner view)))))

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
  "Toggle a disclosure locally, or expand a windowed collection. Updates retain choices."
  [pane node-id]
  (let [node (first (filter #(= node-id (:id %))
                            (mapcat #(tree-seq :children :children %)
                                    (get-in pane [:view :nodes]))))]
    (if (or (= :log (:type node)) (:is-collapsible node))
      (let [initial (and (not (settled? pane)) (true? (:default-expanded node)))
            is-open (get (:disclosures pane) node-id initial)]

        (assoc-in pane [:disclosures node-id] (not is-open)))
      (update pane
              :expanded
              (fn [ids]
                (let [ids (set ids)]
                  (if (contains? ids node-id) (disj ids node-id) (conj ids node-id))))))))

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
   header and some lines that happen to line up. A frame line's `:corners`
   follow its edge; where it crosses a column edge, its `:junctions` follow
   whether the lines above and below keep a rail there."
  {:corners {:top ["┌" "┐"] :mid ["├" "┤"] :bottom ["└" "┘"]}
   :junctions {[true true] "┼" [true false] "┴" [false true] "┬" [false false] "─"}})

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

(def ^:private free-text-width "The content width above which a table column holds free text." 24)

(defn- filled-widths
  "`widths` GROWN into the room the band gives them, the slack going to the WIDEST
   column — the same column [[fitted-widths]] takes cells from first, so a table
   that shrank and grew back is the table it was.

   A table fills the band because a rule that stops halfway across reads as a
   table still loading, and because the column that holds free text is the one
   that can use the room. With `content`, the slack goes to the column with the
   widest content instead: a row mark or an indent makes no column wider there.
   When no content is longer than [[free-text-width]], no column holds free
   text: the slack is spread evenly, so a grid or a short table keeps its shape."
  ([widths text-w] (filled-widths widths text-w widths))
  ([widths text-w content]
   (let [n
         (count widths)

         chrome
         (table-chrome n)

         slack
         (- (long text-w) chrome (long (reduce + 0 (map long widths))))

         content
         (if (= n (count content)) content widths)]

     (cond (or (zero? n) (<= slack 0)) (vec widths)
           (and (not (identical? content widths))
                (<= (long (reduce max 0 (map long content))) (long free-text-width)))
           (let [share
                 (quot slack n)

                 extra
                 (rem slack n)]

             (vec (map-indexed (fn [i w]
                                 (+ (long w) share (if (< (long i) extra) 1 0)))
                               widths)))
           :else (let [idx (first (apply max-key
                                    (fn [[_ w]]
                                      (long w))
                                    (map-indexed vector content)))]
                   (update (vec widths) idx #(+ (long %) slack)))))))

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

(defn- span-inner
  "The columns a line spanning the whole table gets INSIDE its frame: every
   column it covers, plus the rail and the pads standing between them."
  ^long [widths]
  (+ (long (reduce + 0 (map long widths))) (* 3 (max 0 (dec (count widths))))))

(defn- span-segments
  "A line that spans the whole table inside its frame: what a table says when it
   holds nothing yet."
  [widths text fg styles]
  (let [inner (span-inner widths)]
    [{:text "│ " :fg t/dialog-hint}
     {:text (p/pad-right (p/ellipsize (str text) inner) inner) :fg fg :styles (vec styles)}
     {:text " │" :fg t/dialog-hint}]))

(defn- rule-line
  "One frame line of a table: the corner glyphs `edge` names and a dash for every
   column the cells occupy, the pads included, so the rails of two lines stand in
   the same columns. At a column edge it joins only the rails that meet it there:
   `above` and `below` say whether the line on that side keeps its inner rails."
  [widths edge above below]
  (let [[l r] (get-in table-frame [:corners edge])]
    (str l
         (str/join (get-in table-frame [:junctions [(boolean above) (boolean below)]])
                   (map (fn [w]
                          (apply str (repeat (+ 2 (long w)) "─")))
                        widths))
         r)))

(defmulti ^:private node-rows
  "The rows ONE node contributes to the pane's single scroll surface — the whole
   live vocabulary, once, in the same order `live/->markdown` prints it."
  (fn [node _ctx]
    (:type node)))

(defmethod node-rows :divider
  [{:keys [id]} {:keys [text-w]}]
  [{:kind :trule :node-id id :text (apply str (repeat (max 0 (long text-w)) "─"))}])

(defmethod node-rows :paragraph
  [{:keys [id text]} {:keys [text-w]}]
  (mapv (fn [runs]
          {:kind :paragraph :node-id id :runs runs :text (segment-line runs)})
        (md-lines text text-w)))

(defmethod node-rows :heading
  [{:keys [id text level]} {:keys [text-w]}]
  (mapv (fn [runs]
          {:kind :heading :node-id id :runs runs :text (segment-line runs)})
        (md-lines (str (apply str (repeat (long level) "#")) " " text) text-w)))

(defmethod node-rows :code
  [{:keys [id text]} {:keys [text-w]}]
  (into []
        (mapcat (fn [line]
                  (map (fn [part]
                         {:kind :code :node-id id :text part})
                       (p/fold-cols line text-w))))
        (str/split text #"\n" -1)))

(def ^:private scene-max-rows
  "Cell rows a scene may take: 40 pixels high, so the nodes under it stay on screen."
  20)

;; The band paints many times between two ticks; a scene rasters once per frame.
(def ^:private scene-frame (atom nil))

;; The motion of each scene by node id: objects ease between two patches.
(def ^:private scene-motion (atom {}))

(defn- now-ms ^double [] (/ (double (System/nanoTime)) 1.0e6))

(defn scene-moving?
  "Whether a painted scene still eases, so its surface paints again soon."
  []
  (let [now (now-ms)]
    (boolean (some #(< now (double (:until %))) (vals @scene-motion)))))

(defn- scene-now
  "Scene `node` as it shows now: each object partway to its last pose."
  [node]
  (let [now
        (now-ms)

        {:keys [held node]}
        (scene-motion/step (get @scene-motion (:id node)) node now)

        until
        (reduce (fn [^double m {:keys [start duration]}]
                  (max m (+ (double start) (double duration))))
                0.0
                (vals (:eases held)))]

    (swap! scene-motion assoc (:id node) (assoc held :until until))
    node))

(defn- scene-raster
  [node pw ph]
  (let [k
        [node pw ph]

        [held frame]
        @scene-frame]

    (if (= k held)
      frame
      (let [frame (scene-raster/raster node pw ph)]
        (reset! scene-frame [k frame])
        frame))))

;; A scene paints as half blocks: one cell row holds two pixel rows.
(defmethod node-rows :scene
  [{:keys [id] :as node} {:keys [text-w]}]
  (let [[pw ph]
        (scene-raster/size node text-w scene-max-rows)

        frame
        (scene-raster (scene-now node) pw ph)

        pad
        (quot (- (long text-w) (long pw)) 2)]

    (mapv (fn [r]
            {:kind :scene :node-id id :scene-row r :pad pad :frame frame})
          (range (quot (long ph) 2)))))

(defn- app-entries
  [rows text-w]
  (mapv (fn [row]
          (let [{:keys [gutter body]} (app-model/row-parts row)]
            {:kind :app-row
             :node-id (:node-id row)
             :tone (app-model/row-tone row)
             :text (p/ellipsize (str gutter body) (max 1 (long text-w)))}))
        rows))

;; An app's diff and thread paint read only here; the app surface owns their keys.
(defmethod node-rows :diff
  [node {:keys [text-w]}]
  (app-entries (app-model/diff-rows node text-w #{}) text-w))

(defmethod node-rows :thread
  [node {:keys [text-w]}]
  (app-entries (app-model/thread-rows node text-w {:node-id (:id node)}) text-w))

(defmethod node-rows :spinner
  [{:keys [id text variant is-active]} {:keys [is-interactive]}]
  [{:kind :spinner
    :node-id id
    :text text
    :variant variant
    :is-active (and is-interactive is-active)}])

(defmethod node-rows :button
  [{:keys [id label is-disabled]} {:keys [is-interactive]}]
  [{:kind :button
    :node-id id
    :text (str "[ " (flat-text label) " ]")
    :is-disabled (or is-disabled (not is-interactive))}])

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
                        ;; …and it KEEPS that slot: a step too long for the band
                        ;; trims its own words with the same `…`, instead of
                        ;; pushing what it reports off the row.
                        fitted (cond-> said
                                 reported
                                 (runs-cut (max 1 (- (long text-w) (runs-width reported) 1))))
                        segments (cond-> fitted
                                   reported
                                   (conj {:text (apply str
                                                  (repeat (max 1
                                                               (- (long text-w)
                                                                  (runs-width fitted)
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

(defn- log-line-rows
  "One log line wrapped to `text-w` columns.

   - Expand tabs first, because the painter widens a tab to its stop.
   - Break at a space, and drop only the spaces that pass the row's end.
   - Keep indentation and the spaces between columns.
   - Fold a word wider than the row at the row's width."
  [line text-w]
  (let [w (max 1 (long text-w))]
    (loop [[token & more :as tokens] (re-seq #"\s+|\S+" (p/expand-tabs (str line)))
           row ""
           used 0
           rows []]

      (if (empty? tokens)
        (if (and (= "" row) (seq rows)) rows (conj rows row))
        (let [token-w (long (p/display-width token))
              room (max 0 (- w used))]

          (cond (<= token-w room) (recur more (str row token) (+ used token-w) rows)
                (str/blank? token) (recur more "" 0 (conj rows (str row (subs token 0 room))))
                (and (pos? used) (<= token-w w)) (recur more token token-w (conj rows row))
                :else
                (let [parts (p/fold-cols token w)
                      rows (cond-> rows
                             (pos? used)
                             (conj row))
                      tail (peek parts)]

                  (recur more tail (long (p/display-width tail)) (into rows (pop parts))))))))))

(defmethod node-rows :log
  [{:keys [id lines line-tones total-lines]} {:keys [text-w is-expanded is-record-unread]}]
  (if (seq lines)
    (let [{:keys [shown]}
          (windowed lines is-expanded true)

          behind
          (- (long (or total-lines (count lines))) (count shown))]

      (into (if (pos? behind)
              [{:kind :note
                :node-id id
                :item-id :history
                :text (if is-record-unread
                        (str "… " behind " earlier lines are in the record — reading them failed")
                        (str "… reading " behind " earlier lines from the record"))}]
              [])
            (apply concat
              (map-indexed (fn [index line]
                             (let [tone (get line-tones (+ (- (count lines) (count shown)) index))]
                               (map-indexed (fn [part text]
                                              {:kind :log
                                               :node-id id
                                               ;; An absolute line number survives new output and
                                               ;; earlier-page fills; each wrapped row keeps its own.
                                               :item-id (cond-> [:line
                                                                 (+ (long behind) (long index))]
                                                          (pos? (long part))
                                                          (conj part))
                                               :text text
                                               :tone tone})
                                            (log-line-rows line text-w))))
                           shown))))
    [{:kind :empty :node-id id :text (empty-text :log)}]))

(defn- painted-groups
  "Every group ONE table paints, in the order it paints them: the groups the
   producer DECLARED first — by `order`, ties broken by the order they were
   declared — and then the ones a row invented by naming a parent nobody
   declared, in the order those rows first appeared.

   A declaration only ever adds: an undeclared `parent` is still a group, and it
   wears its own id as its label."
  [groups rows]
  (let [declared (into {}
                       (map-indexed (fn [index group]
                                      [(:id group)
                                       (assoc group :rank [(long (or (:order group) 0)) index])]))
                       groups)]
    (-> (vec (sort-by :rank (vals declared)))
        (into (comp (keep :parent)
                    (distinct)
                    (remove declared)
                    (map (fn [id]
                           {:id id})))
              rows))))

(defn- parent-items
  "The rows ONE table paints, in order. A row that names a `parent` stands under
   that group's HEAD, with every leg of the group gathered at the head's own
   place — a producer lists a matrix interleaved with the rest of its work, and
   legs stranded under whichever head came next read as somebody else's. The
   heads stand together where the FIRST grouped row was listed, in the order the
   table declared them; a row that names no parent is simply itself.

   A group stays SHUT unless it was declared open, and `toggled?` — the reader's
   own hand on the fold — always flips whatever the producer asked for.

   The grouping belongs to the LIVE INTERFACE and not to one surface's reading of
   one producer: the Companion folds the same field into the same shape."
  [groups rows toggled?]
  (let [legs
        (reduce (fn [acc row]
                  (if-let [parent (:parent row)]
                    (update acc parent (fnil conj []) row)
                    acc))
                {}
                rows)

        heads
        (into []
              (comp (filter #(seq (get legs (:id %))))
                    (mapcat (fn [{:keys [id label tone is-open]}]
                              (let [held
                                    (get legs id)

                                    is-open
                                    (if (toggled? id) (not (true? is-open)) (true? is-open))]

                                (cond-> [{:parent id
                                          :label (or label id)
                                          :tone tone
                                          :held (count held)
                                          :is-open is-open}]
                                  is-open
                                  (into (map (fn [leg]
                                               {:row leg :is-leg true}))
                                        held))))))
              (painted-groups groups rows))]

    (:items (reduce (fn [acc row]
                      (cond (nil? (:parent row)) (update acc :items conj {:row row})
                            (:is-spliced acc) acc
                            :else (-> acc
                                      (assoc :is-spliced true)
                                      (update :items into heads))))
                    {:items [] :is-spliced false}
                    rows))))

(defmethod node-rows :table
  [{:keys [id columns groups is-selectable selected-ids] :as node}
   {:keys [text-w widths fresh is-expanded is-interactive expanded]}]
  (let [items
        (parent-items groups (live/ordered-rows node) #(contains? (set expanded) [id %]))

        {:keys [shown behind]}
        (windowed items is-expanded false)

        painted
        (into [] (keep :row) shown)

        ;; Measured from the painted window and never narrower than last time; the
        ;; FIT is what a cramped terminal does to the paint, and it is deliberately
        ;; not what the pane remembers. A selectable first cell also carries the same
        ;; two-column ●/○ state mark as the Companion, and a parent's leg its indent.
        content
        (desired-widths columns painted)

        desired
        (cond-> content
          (and (seq columns) (some :is-leg shown))
          (update 0 + 2)

          (and is-selectable (seq columns))
          (update 0 + 2))

        measured
        (grown-widths (get widths id) desired)

        ;; Squeezed if it must be, then GROWN into whatever the band has left: a
        ;; table is a block, and a block that ends in mid-air reads as unfinished.
        ws
        (filled-widths (fitted-widths measured text-w) text-w content)

        aligns
        (mapv #(or (:align %) :left) columns)

        selected
        (set selected-ids)

        line
        (fn [cells fg styles]
          (let [segments (table-segments ws cells aligns fg styles)]
            {:segments segments :text (segment-line segments)}))

        ;; Only a line of CELLS keeps a rail at every column edge. A head and an
        ;; empty line span the whole table, so a rule beside one bends away (`┴`,
        ;; `┬`) or runs straight instead of poking a stub into its words.
        rule
        (fn [edge above below]
          (let [railed #{:thead :trow}]
            {:kind :trule
             :node-id id
             :text (rule-line ws edge (railed (:kind above)) (railed (:kind below)))}))

        ;; A parent head wears the group's LABEL and then says how much its fold
        ;; holds, and the whole line is the control that opens it. The count is
        ;; counted HERE, from the rows themselves, so no producer has to smuggle
        ;; it into the name.
        head
        (fn [{:keys [parent label tone held is-open]}]
          (let [mark
                (if is-open "▾ " "▸ ")

                ;; How much the fold holds is the head's own FURNITURE: a band too
                ;; narrow for the whole name trims the NAME with the same `…` and
                ;; keeps the count, instead of ellipsizing away the one thing a
                ;; shut fold says about itself.
                tail
                (str " · " held " row" (when (not= 1 (long held)) "s"))

                text
                (str mark
                     (p/ellipsize (str label)
                                  (max 1
                                       (- (span-inner ws)
                                          (long (p/display-width mark))
                                          (long (p/display-width tail)))))
                     tail)

                segments
                (span-segments ws text (tone-fg tone t/dialog-hint-key) [p/BOLD])]

            {:kind :tparent
             :node-id [id parent]
             :table-id id
             :parent parent
             :tone tone
             :is-open is-open
             :segments segments
             :text (segment-line segments)}))

        body
        (if (seq shown)
          (mapv (fn [item]
                  (let [row (:row item)]
                    (if (nil? row)
                      (head item)
                      (let [is-fresh (contains? fresh (:id row))
                            is-selected (contains? selected (:id row))
                            cells (cond-> (mapv (fn [col-idx]
                                                  (md-runs (cell-of row col-idx)))
                                                (range (count columns)))
                                    (and (:is-leg item) (seq columns))
                                    (update 0 #(into [{:text "  "}] %))

                                    (and is-selectable (seq columns))
                                    (update 0 #(into [{:text (if is-selected "● " "○ ")}] %)))]

                        (merge {:kind :trow
                                :node-id id
                                :item-id (:id row)
                                :tone (:tone row)
                                :is-fresh is-fresh
                                :is-selectable (boolean (and is-selectable is-interactive))
                                :is-selected is-selected}
                               (line
                                 cells
                                 (if is-selected t/header-active-tab-accent (tone-fg (:tone row)))
                                 (if (or is-fresh is-selected) [p/BOLD] [])))))))
                shown)
          (let [segments (span-segments ws (empty-text :table) t/dialog-hint [p/ITALIC])]
            [{:kind :empty :node-id id :segments segments :text (segment-line segments)}]))

        lines
        (into [(merge {:kind :thead :node-id id}
                      (line (mapv #(md-runs (:label %)) columns) t/dialog-hint [p/BOLD]))]
              body)

        ;; A rail between EVERY pair of rows: a live table is read while it fills,
        ;; and the eye needs the line that says where one row's answer ends and the
        ;; next one begins — especially when a cell wears a tone of its own.
        framed
        (conj (reduce (fn [out entry]
                        (conj out (rule (if (seq out) :mid :top) (peek out) entry) entry))
                      []
                      lines)
              (rule :bottom (peek lines) nil))]

    (with-meta (cond-> framed
                 (pos? (long behind))
                 (conj (more-row id behind "row")))
      {:widths {id measured}})))

(defn- card-segments
  "One card line: the rail that marks the card, then `segments`. A card that a
   human can pick wears the accent rail, the one a button wears."
  [is-selectable segments]
  (into [{:text "│ " :fg (if is-selectable t/dialog-hint-key t/dialog-hint)}] segments))

(defmethod node-rows :card
  [{:keys [id title text meta tone is-selectable]} {:keys [text-w is-interactive]}]
  (let [pickable
        (boolean (and is-selectable is-interactive))

        body-w
        (max 4 (- (long text-w) 2))

        head
        (cond-> []
          tone
          (conj {:text (str (get tone-glyph tone "·") " ") :fg (tone-fg tone)})

          true
          (conj {:text (flat-text title) :fg t/dialog-fg :styles [p/BOLD]}))

        ;; The meta line sits at the right edge, so the title and its figure read as
        ;; one row the way a table row does, and never wraps under the title.
        meta-text
        (some-> meta
                flat-text
                not-empty)

        gap
        (when meta-text
          (- body-w
             (long (p/display-width (segment-line head)))
             (long (p/display-width meta-text))))

        head
        (cond-> head
          (and meta-text (pos? (long gap)))
          (into [{:text (apply str (repeat gap " "))} {:text meta-text :fg t/dialog-hint}])

          (and meta-text (not (pos? (long gap))))
          (conj {:text (str "  " meta-text) :fg t/dialog-hint}))

        title-row
        (let [segments (card-segments pickable head)]
          {:kind :card
           :node-id id
           :segments segments
           :text (segment-line segments)
           :label (flat-text title)
           :is-selectable pickable})]

    (into [title-row]
          (map (fn [runs]
                 (let [segments (card-segments pickable (run-segments runs t/dialog-fg []))]
                   {:kind :card :node-id id :segments segments :text (segment-line segments)})))
          (when (seq (flat-text text)) (md-lines text body-w)))))

(defn- input-rows
  "An input node is a FIELD: the form's own renderer draws it, so a field in a
   view and the same field in a form read alike. Each row keeps the field row it
   came from for that painter, and its text for the plan."
  [{:keys [id] :as node} {:keys [text-w is-interactive]}]
  (mapv (fn [row]
          {:kind :field
           :node-id id
           :field row
           :text (str (:text row))
           :is-disabled (not is-interactive)})
        (hi/view-field-rows node text-w)))

(def ^:private input-node-types
  "The node types a view edits: every field type a form knows."
  (set (vals hi-spec/field-types)))

(doseq [type input-node-types]
  (defmethod node-rows type [node ctx] (input-rows node ctx)))

(defmethod node-rows :link
  [{:keys [id links]} {:keys [fresh is-expanded text-w]}]
  (if (seq links)
    (let [{:keys [shown behind]}
          (windowed links is-expanded false)

          entries
          (mapv (fn [{:keys [label target-kind target tone] :as link}]
                  {:kind :link
                   :node-id id
                   :item-id (:id link)
                   :label label
                   :text (flat-text label)
                   :runs (md-runs label)
                   :target (str target)
                   :target-kind target-kind
                   :tone tone
                   :is-fresh (contains? fresh (:id link))})
                shown)

          body
          (if (= 1 (count entries))
            entries
            ;; One frame for the result group, with row-major cells. The longest
            ;; label chooses the column count; cramped labels wrap instead of
            ;; disappearing behind a newly introduced column boundary.
            (let [desired
                  (+ 2 (long (reduce max 0 (map #(runs-width (:runs %)) entries))))

                  n
                  (max 1 (min (count entries) (quot (dec (long text-w)) (+ desired 3))))

                  widths
                  (filled-widths (vec (repeat n
                                              (max 1 (quot (- (long text-w) (table-chrome n)) n))))
                                 text-w)

                  wrap
                  (fn [link w]
                    (when link
                      (mapv (fn [idx runs]
                              (assoc link
                                :runs (into [{:text (if (zero? (long idx)) "→ " "  ")}] runs)))
                            (range)
                            (md-lines (:label link) (max 1 (- (long w) 2))))))

                  ;; Every line of the grid is a line of cells, so its top rule
                  ;; opens a rail at every column edge and its bottom one closes it.
                  rule
                  (fn [edge]
                    {:kind :trule
                     :node-id id
                     :text (rule-line widths edge (= :bottom edge) (= :top edge))})]

              (into [(rule :top)]
                    (concat (mapcat (fn [batch]
                                      (mapv (fn [{:keys [cells]}]
                                              (let [segments (table-segments
                                                               widths
                                                               (mapv #(or (:runs %) []) cells)
                                                               (repeat :left)
                                                               t/link-chrome-fg
                                                               [])]
                                                {:kind :link-grid
                                                 :node-id id
                                                 :item-id (:item-id (first (remove nil? cells)))
                                                 :links cells
                                                 :widths widths
                                                 :segments segments
                                                 :text (segment-line segments)}))
                                            (columns/zip-columns (mapv (fn [idx w]
                                                                         (wrap (nth batch idx nil)
                                                                               w))
                                                                       (range n)
                                                                       widths))))
                                    (partition-all n entries))
                            [(rule :bottom)]))))]

      (cond-> body
        (pos? (long behind))
        (conj (more-row id behind "link"))))
    [{:kind :empty :node-id id :text (empty-text :link)}]))

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

(defn- row-text-width
  "Columns ONE plan row puts down inside `text-w`: what a `wrap` group measures
   its children by. A field and a progress bar fill whatever cell they get, so
   they ask for a readable cell; a nested split row asks for the whole width."
  ^long [text-w row]
  (+ (long (or (:indent row) 0))
     (long (case (:kind row)
             :columns
             (long text-w)

             (:field :progress)
             (max (long columns/min-cell-width) (+ 3 (long (p/display-width (str (:text row))))))

             :stats
             (reduce + 0 (map #(+ 4 (runs-width (:label %)) (runs-width (:value %))) (:items row)))

             (cond (= :scene (:kind row)) (long (:width (:frame row)))
                   (:segments row) (runs-width (:segments row))
                   (:runs row) (runs-width (:runs row))
                   :else (p/display-width (str (:text row))))))
     ;; A log's Search control stands beside its title: ` Search `, its four
     ;; columns of air to the scrollbar lane and the three before the title.
     (if (:search-label row) 15 0)))

(defn- wrap-sections
  "A `wrap` group's rows: every child keeps its natural width and the children
   pack left to right, a new line of cells where the next one does not fit.

   `sections` are the children planned at the full `text-w`. A line that holds
   ONE child paints that full-width plan; a line of several plans each child
   again at its own width through `replan` (`(fn [idx w] rows)`) and carries the
   widths as `:cell-widths` for the painter. Lines stack like sections do."
  [text-w sections replan]
  (let [widths (mapv (fn [rows]
                       (columns/natural-width text-w (map #(row-text-width text-w %) rows)))
                     sections)]
    (stacked-rows (mapv (fn [line]
                          (if (= 1 (count line))
                            (nth sections (first line))
                            (let [ws (mapv widths line)]
                              (mapv #(assoc % :cell-widths ws)
                                    (split-rows (mapv replan line ws))))))
                        (columns/wrap-lines text-w widths)))))

(defn- node-section
  "One node's rows, `text-w` columns wide: its label, then what the node paints.

   A layout group places its children by its direction. Ask applies the same
   rules, including inside nested groups:

   - `:row` stands them side by side when [[columns/row-fits?]] allows it.
   - `:grid` cuts them into rows of [[columns/grid-span]] equal columns, one row
     of air between, the last row padded so its columns stay aligned.
   - `:wrap` packs them at their natural width through [[wrap-sections]].
   - Everything else, and a row or grid too narrow to split, stacks them in
     source order.

   [[columns/cell-width]] supplies the text budget for each equal column.

   Disclosed children gain two columns; wrapping and hit targets use that same inset.
   The row of air between two sections belongs to whoever stacks them, never to
   the node, so two nodes standing side by side start on the same line."
  [node ctx fresh text-w]
  (let [disclosure?
        (or (= :log (:type node)) (:is-collapsible node))

        indent
        (if disclosure? (min 2 (max 0 (dec (long text-w)))) 0)

        body-w
        (max 1 (- (long text-w) indent))

        node-ctx
        (ctx node body-w)

        is-open
        (or (not disclosure?) (:is-open node-ctx))

        children
        (not-empty (:children node))

        n
        (count children)

        span
        (if (and children (= :grid (:direction node)))
          (columns/grid-span body-w n (:max-columns node))
          0)

        layout
        (cond (nil? children) :leaf
              (and (= :row (:direction node)) (columns/row-fits? body-w n)) :row
              (< 1 span) :grid
              (and (= :wrap (:direction node)) (< 1 n)) :wrap
              :else :stack)

        cell-w
        (case layout
          :row
          (columns/cell-width body-w n)

          :grid
          (columns/cell-width body-w span)

          body-w)

        parts
        (when (and is-open children) (mapv #(node-section % ctx fresh cell-w) children))

        body
        (cond (not is-open) []
              (= :leaf layout) (node-rows node node-ctx)
              (= :row layout) (split-rows parts)
              (= :grid layout) (stacked-rows (mapv split-rows (columns/grid-chunks parts span)))
              (= :wrap layout) (wrap-sections body-w
                                              parts
                                              (fn [idx w]
                                                (node-section (nth children idx) ctx fresh w)))
              :else (stacked-rows parts))

        label
        (if disclosure?
          (str (if is-open "▾ " "▸ ")
               (or (:label node) "Output")
               (when (= :log (:type node))
                 (str " · " (long (or (:total-lines node) (count (:lines node)))) " lines")))
          ;; A button names itself, and the field renderer draws a field's label.
          (when-not (or (= :button (:type node)) (contains? input-node-types (:type node)))
            (flat-text (:label node))))]

    (with-meta (cond-> []
                 (seq label)
                 (conj (cond-> {:kind (if disclosure? :disclosure :node)
                                :node-id (:id node)
                                :text label
                                :is-fresh (contains? (:nodes fresh) (:id node))}
                         (and is-open (= :log (:type node)))
                         (assoc :search-label (str "Search " (or (:label node) "Output")))))

                 :always
                 (into (if (pos? indent) (map #(update % :indent (fnil + 0) indent) body) body)))
      {:widths (reduce merge {} (map (comp :widths meta) (or parts [body])))})))

(defn plan
  "The pane's whole paint plan, `text-w` columns wide: one entry per painted row,
   nodes in DECLARATION order, each under its own label with a row of air above
   it. A node the run wrapped in a `row` group stands BESIDE its siblings wherever
   the band is wide enough for all of them; a `grid` or `wrap` group lays its
   nodes out as [[node-section]] describes. Carries `{:widths …}` as metadata —
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
           :is-expanded (or (= :log (:type node)) (contains? (set expanded) (:id node)))
           :expanded (set expanded)
           :is-open (get (:disclosures pane)
                         (:id node)
                         (and (not (settled? pane)) (true? (:default-expanded node))))
           :is-interactive (not (settled? pane))
           :is-record-unread (contains? (:log-fill-error pane) (:id node))})

        head
        (mapv (fn [runs]
                {:kind :prose :runs runs :text (segment-line runs)})
              (md-lines (:description view) text-w))

        sections
        (mapv #(node-section % ctx fresh (long text-w)) (:nodes view))]

    (with-meta (stacked-rows (cond-> []
                               (seq head)
                               (conj head)

                               true
                               (into sections)))
      {:widths (reduce merge {} (map (comp :widths meta) sections))})))

(defn- view-nodes
  "Every node of `pane`'s view, groups opened, in declaration order."
  [pane]
  (tree-seq #(seq (:children %)) :children {:children (get-in pane [:view :nodes])}))

(defn input-node
  "The input node `node-id` of `pane`'s view, or nil when it is no input."
  [pane node-id]
  (some #(when (and (= node-id (:id %)) (contains? input-node-types (:type %))) %)
        (view-nodes pane)))

(defn submit-button?
  "True when `node-id` is a button that submits the view's inputs."
  [pane node-id]
  (boolean (some #(and (= node-id (:id %)) (= :button (:type %)) (= :submit (keyword (:action %))))
                 (view-nodes pane))))

(defn input-values
  "The value of every input that holds one, keyed by node id: what a submit button
   sends. A secret never comes back to a surface, so a set secret is left out."
  [pane]
  (into {}
        (keep (fn [{:keys [id type value]}]
                (when (and (contains? input-node-types type) (some? value)) [id value])))
        (view-nodes pane)))

(defn controls
  "Keyboard-accessible controls for the visible pane, in reading order."
  [panes]
  (when-let [pane (last (remove dormant? panes))]
    (if (minimized? pane)
      [{:id :restore :label "Restore live view" :kind :live-restore :view-id (view-id pane)}]
      (into []
            (mapcat
              (fn [entry]
                (let [kind (case (:kind entry)
                             (:disclosure :more :tparent)
                             :live-expand

                             :button
                             (when-not (:is-disabled entry) :live-activate)

                             :trow
                             (when (:is-selectable entry) :live-select)

                             :card
                             (when (:is-selectable entry) :live-activate)

                             :field
                             (when (and (not (:is-disabled entry))
                                        (#{:label :checkbox} (get-in entry [:field :kind])))
                               :live-edit)

                             nil)
                      control (assoc (select-keys entry [:node-id :item-id])
                                :id [(:node-id entry) (:item-id entry)]
                                :label (or (:label entry) (:text entry))
                                :kind kind
                                :view-id (view-id pane))]

                  (cond-> []
                    kind
                    (conj control)

                    (:search-label entry)
                    (conj (assoc control
                            :id [(:node-id entry) :search]
                            :item-id :search
                            :label (:search-label entry)
                            :kind :live-log-search))))))
            (tree-seq #(seq (:cells %)) :cells {:cells (plan pane 80)})))))

(def log-search-page-size "Maximum matching lines read from the retained log in one request." 200)

(defn log-search-opened
  "Local search and viewport state for the existing Live View transient."
  [node-id]
  {:node-id node-id
   :input (input/empty-input)
   :from 0
   :offset 0
   :is-following false
   :total 0
   :visible 0})

(defn- log-search-input
  [search editor]
  (cond-> (assoc search :input editor)
    (not= (input/input->text (:input search)) (input/input->text editor))
    (-> (dissoc :page :error :request-id :loading? :anchor)
        (assoc :from 0
               :offset 0
               :total 0
               :is-following false))))

(defn log-search-typed
  "Edit only the search field, or scroll its results; never touch the composer.
   Bracketed paste is inserted as one literal line, without submitting a request."
  [search ^KeyStroke key]
  (let [kt
        (.getKeyType key)

        editor
        (:input search)]

    (cond (= kt KeyType/PasteStart) (assoc search :paste "")
          (= kt KeyType/PasteEnd)
          (-> search
              (dissoc :paste)
              (log-search-input
                (input/paste-text editor (str/replace (or (:paste search) "") #"[\r\n\t]+" " "))))
          (some? (:paste search)) (update search :paste str (.getText key))
          (= kt KeyType/ArrowUp) (scrolled search -1)
          (= kt KeyType/ArrowDown) (scrolled search 1)
          :else (log-search-input search
                                  (or (input/emacs-edit key editor)
                                      (condp = kt
                                        KeyType/ArrowLeft (input/move-left editor)
                                        KeyType/ArrowRight (input/move-right editor)
                                        KeyType/Home (input/move-line-start editor)
                                        KeyType/End (input/move-line-end editor)
                                        KeyType/Backspace (input/delete-backward editor)
                                        KeyType/Delete (input/delete-forward editor)
                                        KeyType/Character (if (and (not (.isCtrlDown key))
                                                                   (some? (.getText key)))
                                                            (input/paste-text editor (.getText key))
                                                            editor)
                                        editor))))))

(defn log-search-page-from
  "The adjacent page's match offset, or nil when that page does not exist."
  [search direction]
  (let [from (+ (long (:from search)) (* (long direction) (long log-search-page-size)))]
    (when (and (>= from 0)
               (or (neg? (long direction)) (< from (long (get (:page search) "matched" 0)))))
      from)))

(defn log-search-requested
  "Start a bounded read. Its identity fences out cancelled or superseded results."
  [search from request-id]
  (-> search
      (dissoc :page :error :anchor)
      (assoc :from from
             :request-id request-id
             :loading? true
             :offset 0
             :total 0
             :is-following false)))

(defn log-search-loaded
  "Accept only the current read, including after a tab switch."
  [search request-id result]
  (if (and search (= request-id (:request-id search)))
    (merge (dissoc search :loading? :request-id) result)
    search))

(defn log-search-plan
  "Wrapped, numbered matches in the viewer's own scroll surface, never a dialog."
  [{:keys [node-id page loading? error] :as search} text-w]
  (cond loading? [{:kind :note :text "Searching…"}]
        error [{:kind :note :text "Could not read log. Enter to retry."}]
        (nil? page) [{:kind :note :text "Enter to search · Case-insensitive · entire retained log"}]
        (empty? (get page "lines")) [{:kind :empty :text "No matching lines"}]
        :else (into []
                    (concat
                      (for [[direction label]
                            [[-1 "Previous matches"] [1 "Next matches"]]

                            :when (some? (log-search-page-from search direction))]

                        {:kind :log-search-page :direction direction :text label})
                      (mapcat (fn [number text]
                                (map-indexed
                                  (fn [idx line]
                                    {:kind :log :node-id node-id :item-id [number idx] :text line})
                                  (log-line-rows (str number ": " text) text-w)))
                              (get page "line_numbers")
                              (get page "lines"))))))

(defn animating?
  "Whether the visible, expanded pane contains an active spinner or a moving scene."
  [panes]
  (when-let [pane (last (remove dormant? panes))]
    (when-not (or (settled? pane) (minimized? pane))
      (letfn [(active? [node]
                (or (and (= :spinner (:type node)) (:is-active node))
                    (and (= :scene (:type node)) (scene-moving?))
                    (and (= :group (:type node))
                         (or (not (:is-collapsible node))
                             (get (:disclosures pane) (:id node) (:default-expanded node)))
                         (some active? (:children node)))))]
        (boolean (some active? (get-in pane [:view :nodes])))))))

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

(defn log-span
  "The rows the first open log owns in `rows`, as `[from to)`: its own row — the
   one carrying Search — through the last line under it. Nil without an open log.

   The band's scrollbar belongs to that log. It starts where the log starts, never
   over the prose and status rows above it, and measures the log alone: those
   rows are what a watched run keeps adding to, and what the eye scrolls."
  [rows]
  (let [rows
        (vec rows)

        from
        (first (keep-indexed (fn [idx row]
                               (when (:search-label row) idx))
                             rows))]

    (when from
      (let [node-id (:node-id (rows from))]
        [from
         (loop [idx (inc (long from))]
           (if (and (< idx (count rows)) (= node-id (:node-id (rows idx))))
             (recur (inc idx))
             idx))]))))

(defn- bar-shape
  "Where the band's scrollbar goes and what it measures, or nil when nothing
   overflows: `:row` and `:track` place it beside the rows painted from `start`,
   `body-visible` of them from `body-row`.

   With an open log the bar is the log's: the track runs from the log's own row
   down the lines painted under it, and the thumb says how much of the log is on
   screen. Without one the whole body is the scrolled thing, and the bar spans it."
  [rows-plan ^long start ^long body-visible ^long body-row]
  (if-let [[from to] (log-span rows-plan)]
    (let [from (long from)
          to (long to)
          first-row (max from start)
          last-row (min to (+ start body-visible))]

      (when (and (< first-row last-row) (> (- to from) (- last-row first-row)))
        {:row (+ body-row (- first-row start))
         :track (- last-row first-row)
         :total (- to from)
         :start (- first-row from)}))
    (when (> (count rows-plan) body-visible)
      {:row body-row :track body-visible :total (count rows-plan) :start start})))

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

   The footer and the abort branch both ask here, so neither can advertise or
   invoke a stop the run cannot honor."
  [panes]
  (last (remove settled? panes)))

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

(defn- has-selectable-table?
  "True when `pane` carries a live table whose rows change the detail below it."
  [pane]
  (boolean (some #(and (= :table (:type %)) (:is-selectable %)) (get-in pane [:view :nodes]))))

(defn hint
  "The hint bar advertises F3 controls and the keys for interrupting the view,
   and while several are open it says WHICH one it will hit — the newest, the one
   the band is painting. A selectable table advertises its click. Once a stop is armed,
   the bar says the two keys that end typing: Escape or Enter interrupt with whatever
   was written, Backspace on an empty line keeps watching."
  [pane others]
  (cond (:log-search pane) [["Enter" "search / refresh"] ["↑/↓" "scroll"] ["PgUp/PgDn" "matches"]
                            ["Esc" "back"]]
        (:is-viewer pane) [["F3" "controls"] ["Esc" "close view"]]
        :else (let [open (remove settled? others)]
                (if-let [note (stopping pane)]
                  (if (str/blank? note)
                    [["Esc / Enter" "interrupt"] ["Backspace" "keep watching"]]
                    [["Esc / Enter" "interrupt with the note"] ["Backspace" "erase"]])
                  (if (minimized? pane)
                    [["click ↑" "restore live view"]
                     ["Esc" (str "interrupt " (flat-text (get-in pane [:view :title])))]]
                    (cond-> [["F3" "controls"]]
                      (and (some? pane) (not (settled? pane)))
                      (conj ["click ▾" "minimize"])

                      (and (some? pane) (not (settled? pane)) (has-selectable-table? pane))
                      (conj ["click" "select a row"])

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
  (p/put-str! g
              (+ (long left) 2)
              row
              (p/ellipsize (p/expand-tabs text) (max 0 (- (long inner-w) 3)))))

(defn- paint-plain!
  [g left row inner-w fg text]
  (fill! g left row inner-w fg)
  (put! g left row inner-w text))

(defn- paint-styled!
  [g left row inner-w fg styles text]
  (fill! g left row inner-w fg)
  (p/styled g styles (put! g left row inner-w text)))

(defn- progress-text
  "`■■■■■■⬝⬝⬝⬝⬝⬝⬝⬝  62%  ·  18/29 done` — the bar, the number it stands for, and
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
            (str (apply str (repeat filled "■")) (apply str (repeat (- (long bar-w) filled) "⬝")))]

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
            (let [shown (p/ellipsize (p/expand-tabs text) (max 0 (- (long inner-w) 3 used)))]
              (when (seq shown)
                (p/set-colors! g (or fg t/dialog-fg) t/dialog-bg)
                (p/styled g (or styles []) (p/put-str! g (+ (long left) 2 used) row shown)))
              (+ used (long (p/display-width shown)))))
          0
          segments))

(defn- rgb-ink
  ^TextColor$RGB [^long rgb]
  (TextColor$RGB. (int (bit-and 0xff (bit-shift-right rgb 16)))
                  (int (bit-and 0xff (bit-shift-right rgb 8)))
                  (int (bit-and 0xff rgb))))

(defn- paint-scene!
  "One cell row of a scene: `▀` in the ink of the upper pixel on the paper of
   the lower one, so each cell shows two pixels in truecolor. A cell is set
   directly: the glyph is fixed, so the string path's sanitizing and width
   measuring would run once per pixel pair for nothing."
  [^TextGraphics g left row inner-w {:keys [frame scene-row pad]}]
  (fill! g left row inner-w t/dialog-fg)
  (let [{:keys [width pixels]}
        frame

        ^ints pixels
        pixels

        w
        (long width)

        pad
        (long pad)

        top
        (* 2 w (long scene-row))]

    (dotimes [c (min w (max 0 (- (long inner-w) 3 pad)))]
      (p/set-colors! g (rgb-ink (aget pixels (+ top c))) (rgb-ink (aget pixels (+ top w c))))
      (.setCharacter g (int (+ (long left) 2 pad c)) (int row) \▀))))

(defn- paint-runs!
  "One row of styled markdown runs over a base ink. [[run-segments]] decides which
   run leaves that ink — a code span and a link take the accent the transcript
   gives them — so `code` inside a statement reads as code on every surface."
  [g left row inner-w fg styles runs]
  (paint-segments! g left row inner-w (run-segments runs fg styles)))

(def ^:private ^:dynamic *hit-row-offset* 0)

(def ^:private bar-right-margin
  "Space beyond the live scrollbar: two clear columns before the close control
   and its rail, so the thumb never appears attached to the X above it."
  5)

(defn- bar-col
  "The band's scrollbar lane, inset from the right rail and close control."
  ^long [^long left ^long inner-w]
  (- (+ left inner-w) (inc (long bar-right-margin))))

(def ^:private search-air
  "Columns a Search control keeps clear of the scrollbar lane. The open log's bar
   starts on the very row that carries Search, so the control stands off the
   thumb instead of leaning on it."
  4)

(defn- search-col
  "Where a Search control starts, given the band's [[bar-col]]: the same column on
   the log's own row and on the heading that stands in for it."
  ^long [^long bar-col]
  (- bar-col (long search-air) (long (p/display-width " Search "))))

(defn- register-link!
  "Register a link cell with the transcript's existing URL/path opener."
  [left row width {:keys [target-kind target]}]
  (when-let [kind (case target-kind
                    :url
                    :url

                    :path
                    :file

                    nil)]
    (when (pos? (long width))
      (.register interactions/hit-map
                 {:bounds {:row (+ (long row) (long *hit-row-offset*)) :col left :width width}
                  :kind kind
                  :url target
                  :enabled? true}))))

(defn- paint-entry!
  "Paint ONE plan row and register whatever on it can be clicked. `left` is the
   band's rail and the body opens two columns inside it, exactly like the form's
   rows, so a view and a form painted in the same band line up. A disclosure's
   `:indent` shifts both its content and controls without changing row identities."
  [g left row inner-w view-id entry]
  (case (if (:indent entry) :indented (:kind entry))
    :indented
    (let [indent (min (long (:indent entry)) (max 0 (long inner-w)))]
      (fill! g left row inner-w t/dialog-fg)
      (paint-entry! g
                    (+ (long left) indent)
                    row
                    (- (long inner-w) indent)
                    view-id
                    (dissoc entry :indent)))

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

    :disclosure
    (let [width
          (max 0 (- (long inner-w) 3))

          ;; The body's right gutter leaves the scrollbar lane and its margin clear.
          col
          (search-col (bar-col (long left) (+ (long inner-w) 2 (long bar-right-margin))))

          ;; Three columns of air between the title and Search — and Search only
          ;; where a title still reads beside it once ellipsized.
          beside-w
          (- col (long left) 3)

          search?
          (and (:search-label entry) (>= beside-w 3))

          title-w
          (if search? beside-w width)

          hit-row
          (+ (long row) (long *hit-row-offset*))]

      (paint-styled! g
                     left
                     row
                     inner-w
                     t/dialog-hint-key
                     [p/BOLD]
                     (p/ellipsize (:text entry) title-w))
      (.register interactions/hit-map
                 {:bounds {:row hit-row :col (+ (long left) 2) :width title-w}
                  :kind :live-expand
                  :view-id view-id
                  :node-id (:node-id entry)
                  :enabled? true})
      (when search?
        (components/button! g
                            col
                            row
                            " Search "
                            :live-log-search
                            {:extra {:bounds {:row hit-row :col col :width 8}
                                     :view-id view-id
                                     :node-id (:node-id entry)
                                     :item-id :search
                                     :label (:search-label entry)}})))

    :log-search-page
    (do (paint-styled! g left row inner-w t/dialog-hint-key [p/BOLD] (:text entry))
        (.register interactions/hit-map
                   {:bounds {:row (+ (long row) (long *hit-row-offset*))
                             :col (+ (long left) 2)
                             :width (max 0 (- (long inner-w) 3))}
                    :kind :live-log-page
                    :view-id view-id
                    :direction (:direction entry)
                    :enabled? true}))

    :button
    (do (paint-styled! g
                       left
                       row
                       inner-w
                       (if (:is-disabled entry) t/dialog-hint t/dialog-hint-key)
                       [p/BOLD]
                       (:text entry))
        (when-not (:is-disabled entry)
          (.register interactions/hit-map
                     {:bounds {:row (+ (long row) (long *hit-row-offset*))
                               :col (+ (long left) 2)
                               :width (max 0 (- (long inner-w) 3))}
                      :kind :live-activate
                      :view-id view-id
                      :node-id (:node-id entry)
                      :enabled? true})))

    :card
    (do (paint-segments! g left row inner-w (:segments entry))
        (when (:is-selectable entry)
          (.register interactions/hit-map
                     {:bounds {:row (+ (long row) (long *hit-row-offset*))
                               :col (+ (long left) 2)
                               :width (max 0 (- (long inner-w) 3))}
                      :kind :live-activate
                      :view-id view-id
                      :node-id (:node-id entry)
                      :enabled? true})))

    :field
    (do (fill! g left row inner-w t/dialog-fg)
        (hi/paint-field-row! g left row inner-w (:field entry))
        (when-not (or (:is-disabled entry) (= :blank (get-in entry [:field :kind])))
          (.register interactions/hit-map
                     {:bounds {:row (+ (long row) (long *hit-row-offset*))
                               :col (+ (long left) 2)
                               :width (max 0 (- (long inner-w) 3))}
                      :kind :live-edit
                      :view-id view-id
                      :node-id (:node-id entry)
                      :enabled? true})))

    :spinner
    (let [frames
          (get hi-spec/spinner-frames (:variant entry))

          frame
          (if (:is-active entry)
            (nth frames (mod (quot (System/currentTimeMillis) 100) (count frames)))
            "·")]

      (paint-plain! g left row inner-w t/dialog-fg (str (p/pad-right frame 5) " " (:text entry))))

    :heading
    (paint-runs! g left row inner-w t/dialog-fg [p/BOLD] (:runs entry))

    :paragraph
    (paint-runs! g left row inner-w t/dialog-fg [] (:runs entry))

    :code
    (paint-plain! g left row inner-w t/dialog-fg (:text entry))

    :app-row
    (paint-plain! g left row inner-w (tone-fg (:tone entry)) (:text entry))

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
    (paint-plain! g left row inner-w (log-ink (:tone entry)) (:text entry))

    :thead
    (paint-segments! g left row inner-w (:segments entry))

    :scene
    (paint-scene! g left row inner-w entry)

    :trule
    (paint-plain! g left row inner-w t/dialog-hint (:text entry))

    ;; A parent is a FOLD, so its head is a control: the whole line opens and shuts
    ;; the rows gathered under it, the way the Companion's disclosure does.
    :tparent
    (do (paint-segments! g left row inner-w (:segments entry))
        (.register interactions/hit-map
                   {:bounds {:row (+ (long row) (long *hit-row-offset*))
                             :col (+ (long left) 2)
                             :width (max 0 (- (long inner-w) 3))}
                    :kind :live-expand
                    :view-id view-id
                    :node-id (:node-id entry)
                    :enabled? true}))

    :trow
    (do (paint-segments! g left row inner-w (:segments entry))
        ;; The full visible table row is the control, not a tiny glyph. Its item id
        ;; is what the shared selection patch names; the next live patch repaints every
        ;; attached TUI and Companion from that one engine state.
        (when (:is-selectable entry)
          (.register interactions/hit-map
                     {:bounds {:row (+ (long row) (long *hit-row-offset*))
                               :col (+ (long left) 2)
                               :width (max 0 (- (long inner-w) 3))}
                      :kind :live-select
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
        (register-link! (+ (long left) 2) row (max 0 (- (long inner-w) 3)) entry))

    :link-grid
    (do (paint-segments! g left row inner-w (:segments entry))
        (reduce (fn [^long offset [link width]]
                  (when link (register-link! (+ (long left) 4 offset) row width link))
                  (+ offset (long width) 3))
                0
                (map vector (:links entry) (:widths entry))))

    ;; `+ N more` is a CONTROL, so it wears the accent the rest of the TUI gives
    ;; a thing you can press, and it registers the region that expands its node.
    :more
    (do (paint-styled! g left row inner-w t/dialog-hint-key [p/BOLD] (:text entry))
        (.register interactions/hit-map
                   {:bounds {:row (+ (long row) (long *hit-row-offset*))
                             :col (+ (long left) 2)
                             :width (max 0 (- (long inner-w) 3))}
                    :kind :live-expand
                    :view-id view-id
                    :node-id (:node-id entry)
                    :enabled? true}))

    ;; Nodes standing SIDE BY SIDE: one plan row carries one cell per column, and
    ;; each cell is painted into its own slice of the band by THIS function — a
    ;; table beside a paragraph is painted by the code that paints them alone.
    ;; A `wrap` line names each cell's text width; other rows divide evenly.
    :columns
    (let [cells
          (:cells entry)

          places
          (if-let [ws (:cell-widths entry)]
            (columns/width-slots inner-w ws)
            (columns/slots inner-w (count cells)))]

      (fill! g left row inner-w t/dialog-fg)
      (reduce (fn [pos [[x width] cell]]
                (let [here (when (and cell (pos? (long width)))
                             (paint-entry! g (+ (long left) (long x)) row width view-id cell))]
                  (or pos here)))
              nil
              (map vector places cells)))

    ;; A minimized LIVE pane is still a control: the whole status row restores its
    ;; full surface while patches continue to land behind it.
    :minimized
    (do (paint-styled! g left row inner-w t/dialog-hint-key [p/BOLD] (:text entry))
        (.register interactions/hit-map
                   {:bounds {:row (+ (long row) (long *hit-row-offset*))
                             :col (inc (long left))
                             :width (long inner-w)}
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
  "Paint minimize/restore for a running view and a close icon for a read-only record."
  [g {:keys [left inner-w]} row pane]
  (when pane
    (let [close?
          (or (:is-viewer pane) (settled? pane))

          label
          (cond close? " ✕ "
                (minimized? pane) " ↑ "
                :else " ▾ ")

          width
          (long (p/display-width label))

          ;; Inset ONE column from the interior's right edge: the control keeps a
          ;; margin from the band's corner instead of sitting on the scrollbar lane.
          col
          (max (inc (long left)) (- (+ (long left) (long inner-w)) width))

          target
          {:bounds {:row (+ (long row) (long *hit-row-offset*)) :col col :width width}
           :kind (cond (:is-viewer pane) :live-viewer-close
                       (settled? pane) :live-reopen
                       (minimized? pane) :live-restore
                       :else :live-minimize)
           :label (cond close? "Close live view"
                        (minimized? pane) "Restore live view"
                        :else "Minimize live view")
           :view-id (view-id pane)
           :enabled? true}]

      (if close?
        (components/button! g col row label (:kind target) {:danger? true :extra target})
        (do (p/set-colors! g t/dialog-hint-key t/dialog-bg)
            (p/styled g [p/BOLD] (p/put-str! g col row label))
            (.register interactions/hit-map target))))))

(defn- paint-heading-search!
  "Paint the heading's Search control for an open log, left of the fold control.

   A log's own Search sits on that log's row, which the body scrolls out of the
   window as soon as the view follows a long tail — a watched run then had no
   reachable way into its retained output at all. The heading never scrolls, so the
   whole record stays one click away for the life of the run."
  [g {:keys [left inner-w]} row view-id entry]
  (let [label
        " Search "

        width
        (long (p/display-width label))

        ;; The same column as the Search on the log's own row, clear of the bar lane
        ;; and of the fold control, itself inset one column from the interior's
        ;; right edge.
        col
        (search-col (bar-col (long left) (long inner-w)))]

    (when (> col (inc (long left)))
      (components/button!
        g
        col
        row
        label
        :live-log-search
        {:extra {:bounds {:row (+ (long row) (long *hit-row-offset*)) :col col :width width}
                 :view-id view-id
                 :node-id (:node-id entry)
                 :item-id :search
                 :label (:search-label entry)}}))))

(defn- band-title
  "The Live View title, including the recorded outcome when the run has ended."
  [pane now-ms]
  (str/join " · "
            (remove str/blank?
              ["LIVE"
               (when (settled? pane)
                 (if-let [reason (get-in pane [:settled :reason])]
                   (str/capitalize (name reason))
                   "Recorded")) (title-line pane now-ms)])))

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

        ;; Keep wrapping inside the body, before the inset scrollbar lane.
        text-w
        (max 8 (- (long (:inner-w region)) 6 (long bar-right-margin)))

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
              (:log-search front) (log-search-plan (:log-search front) text-w)
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
  "The rows an expanded Live View covers, inclusive."
  [cols rows panes content-top prompt-h]
  (when (last (remove dormant? panes))
    (let [region
          (tr/band-region (long cols) (long rows) (long content-top) (long prompt-h))

          {:keys [sep-row foot-row]}
          (tr/band-geometry region (:n (band-shape panes region)) false)]

      [(long sep-row) (long foot-row)])))

(defn- paint-generic!
  "Draw the Live View band."
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
               body-left left
               body-w (- inner-w 2 (long bar-right-margin))
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
               ;; Keep the description directly below the expanded title (#220).
               ;; Without a description, retain the title's blank separator row.
               ;; The heading's LEADING blank row is its top margin: the title and
               ;; the close control stand off the band's own top rule.
               search (:log-search front)
               heading-h (if (and (not is-minimized)
                                  (>= (- visible (count collapsed) (if stop 2 0)) (if search 6 4)))
                           (if (str/blank? (get-in front [:view :description])) 3 2)
                           0)
               title-row (if (pos? heading-h) (inc (long body-top)) (long sep-row))
               body-top (+ (long body-top) heading-h)
               visible (- visible heading-h)
               search-top body-top
               search-h (if search (min 2 (max 0 (dec visible))) 0)
               body-top (+ body-top search-h)
               body-visible (max 1 (- visible search-h (count collapsed) (if stop 2 0)))
               ;; Leave one blank row before the footer (or stop prompt) when space allows.
               body-visible
               (if (and (not is-minimized) (> body-visible 1)) (dec body-visible) body-visible)
               total (count rows-plan)
               start (if (and front (not is-minimized)) (offset front rows-plan body-visible) 0)
               shown (subvec (vec rows-plan) (min start total) (min total (+ start body-visible)))
               view-id (view-id front)
               ;; Every open log's Search sits on that log's own row — which the body
               ;; carries out of the window the moment it follows a long tail, leaving
               ;; a watched run with no entrance to its retained output. The heading
               ;; stands in for the first log whose row is not painted, so the whole
               ;; record stays one click away.
               heading-search (when (and (pos? heading-h) (not search) (>= body-w 24))
                                (let [painted
                                      (into #{} (comp (filter :search-label) (map :node-id)) shown)]
                                  (first (remove #(contains? painted (:node-id %))
                                           (filter :search-label rows-plan)))))]

           (tr/clear-rows! g region (max 0 (long sep-row)) rule-at)
           (when (>= (long sep-row) (long top-limit))
             (let [title-w (if heading-search body-w (+ body-w (long bar-right-margin)))
                   title (p/ellipsize (band-title (or front (last panes)) now-ms)
                                      (max 1 (- title-w 6 (if heading-search 9 0))))]

               (tr/draw-rule! g region sep-row (when (zero? heading-h) title))
               (when (pos? heading-h)
                 (paint-styled! g body-left title-row title-w t/dialog-fg [p/BOLD] title))
               (paint-fold-control! g region title-row front)
               (when heading-search
                 (paint-heading-search! g region title-row view-id heading-search))))
           (when (= 2 search-h)
             (let [node (first (filter #(= (:node-id search) (:id %))
                                       (mapcat #(tree-seq :children :children %)
                                               (get-in front [:view :nodes]))))
                   page (:page search)]

               (paint-styled!
                 g
                 body-left
                 search-top
                 body-w
                 t/dialog-fg
                 [p/BOLD]
                 (str
                   "Search "
                   (or (:label node) "Output")
                   (when page
                     (str " · " (get page "matched") " matches / " (get page "total") " lines"))))))
           (when (> rule-at (max (long sep-row) (long top-limit))) (tr/draw-rule! g region rule-at))
           (when (> (long hint-rule-at) (max (long sep-row) (long top-limit)))
             (tr/draw-rule! g region hint-rule-at))
           (doseq [[idx entry] (map-indexed vector collapsed)]
             (paint-entry! g body-left (+ (long body-top) (long idx)) body-w view-id entry))
           (doseq [[idx entry] (map-indexed vector shown)]
             (paint-entry! g
                           body-left
                           (+ (long body-top) (count collapsed) (long idx))
                           body-w
                           view-id
                           entry))
           (doseq [idx (range (count shown) body-visible)]
             (paint-entry! g
                           body-left
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
                                body-left
                                note-row
                                body-w
                                [{:text (:label stop) :fg t/dialog-hint}
                                 {:text (:note stop) :fg t/dialog-fg :styles [p/BOLD]}
                                 {:text "│" :fg t/dialog-hint-key}])))
           (dialogs/draw-hint-bar! g left hint-at inner-w (hint front others))
           ;; With an open log the bar is the log's own: it starts on the log's row,
           ;; beside Search, and measures the log alone.
           (when-let [{:keys [row track total start]} (bar-shape rows-plan
                                                                 start
                                                                 body-visible
                                                                 (+ (long body-top)
                                                                    (count collapsed)))]
             (ScrollBar/draw g
                             Direction/VERTICAL
                             (TerminalPosition. (int (bar-col left inner-w)) (int row))
                             (int track)
                             (int total)
                             (int track)
                             (Integer/valueOf (int start))
                             t/border-fg
                             t/dialog-bg
                             t/dialog-hint-key
                             t/dialog-bg))
           (tr/draw-band-border! g region sep-row rule-at top-limit)
           (p/clear-styles! g)
           (if is-minimized
             {:view-id view-id
              :offset (:offset front)
              :anchor (:anchor front)
              :total (:total front)
              :visible (:visible front)
              :widths (:widths front)}
             (cond-> {:view-id view-id
                      :offset start
                      :anchor (anchor-at rows-plan start)
                      :total total
                      :visible body-visible
                      :widths (:widths (meta rows-plan))}
               search
               (assoc :is-log-search
                 true :cursor
                 (when (pos? search-h)
                   (dialogs/draw-text-input-field! g
                                                   (inc body-left)
                                                   (+ search-top (dec search-h))
                                                   (dec body-w)
                                                   (input/input->text (:input search))
                                                   (get-in search [:input :ccol])
                                                   "Literal text (empty shows all)")))))))))))

(defn paint-plan-entry!
  "Paint ONE entry of [[plan]] at `row`, `inner-w` columns from the rail `left`.
   The app surface paints the nodes it does not own through this."
  [g left row inner-w view-id entry]
  (paint-entry! g left row inner-w view-id entry))

(defn paint!
  "Paint the newest expanded Live View with one empty row above its heading.
   Minimized views and short terminals keep a compact titled rule."
  ([g cols rows panes content-top prompt-h]
   (paint! g cols rows panes content-top prompt-h (System/currentTimeMillis)))
  ([g cols rows panes content-top prompt-h now-ms]
   (when (last (remove dormant? panes))
     (paint-generic! g cols rows panes content-top prompt-h now-ms))))

(defn placement
  "Where a view shows: `:inline` (the default) or `:panel`."
  [pane]
  (or (get-in pane [:view :placement]) :inline))

(defn- owner
  "The extension that owns a panel view, or an empty string."
  [pane]
  (flat-text (get-in pane [:view :extension])))

(defn panel-panes
  "The open panel views among `panes`, oldest first. A closed panel view leaves at once."
  [panes]
  (filterv #(and (= :panel (placement %)) (not (settled? %))) panes))

(defn panel-tabs
  "The owners of `panes` in first-open order. The panel shows tabs only for two or more."
  [panes]
  (vec (distinct (map owner panes))))

(defn panel-shown
  "The panel views on `tab`. With one owner, every view shows."
  [panes tab]
  (let [tabs
        (panel-tabs panes)

        tab
        (if (some #{tab} tabs) tab (first tabs))]

    (if (< (count tabs) 2) (vec panes) (filterv #(= tab (owner %)) panes))))

(defn panel-selected
  "The selected view id among `shown`: the stored one when it still shows, else the first."
  [shown selected]
  (let [ids (mapv view-id shown)]
    (if (some #{selected} ids) selected (first ids))))

(defn panel-geometry
  "The side panel on `cols` chat columns: 40% of them, at least 40. It docks only
   when 60 chat columns remain; otherwise it covers the right side of the chat."
  [cols]
  (let [cols
        (long cols)

        width
        (min cols (max 40 (quot (* 2 cols) 5)))

        docked?
        (>= (- cols width) 60)]

    {:left (- cols width)
     :width width
     :is-docked docked?
     :chat-cols (if docked? (- cols width) cols)}))

(defn- panel-rows
  "The panel body: one title row for each shown view, then the plan rows of an open view."
  [shown {:keys [selected folded]} text-w now-ms]
  (vec
    (mapcat (fn [idx pane]
              (let [id
                    (view-id pane)

                    open?
                    (not (contains? folded id))]

                (concat (when (pos? (long idx)) [{:view-id id :entry {:kind :blank}}])
                        [{:view-id id
                          :is-title true
                          :is-open open?
                          :is-selected (= id selected)
                          :pane pane
                          :elapsed (title-line pane now-ms)}]
                        (when open?
                          (map (fn [entry]
                                 {:view-id id :entry entry})
                               (plan pane text-w))))))
            (range)
            shown)))

(defn- paint-panel-title!
  [g left row width {:keys [view-id is-open is-selected pane]} focused?]
  (let [{:keys [text tone]}
        (status-summary pane)

        title
        (flat-text (get-in pane [:view :title]))

        bg
        (if (and is-selected focused?) t/header-active-tab-bg t/dialog-bg)

        meta
        (when (not= text title) text)

        meta-w
        (if meta (min (quot (long width) 2) (long (p/display-width meta))) 0)

        title-w
        (max 1 (- (long width) 5 meta-w (if meta 2 0)))]

    (p/set-colors! g t/dialog-fg bg)
    (p/fill-rect! g (inc (long left)) row (dec (long width)) 1)
    ;; The selected row of a focused panel inverts, so its marks take the row's ink.
    (p/set-colors! g
                   (cond (and is-selected focused?) t/header-active-tab-fg
                         is-selected t/header-active-tab-accent
                         :else t/dialog-hint-key)
                   bg)
    (p/put-str! g (+ (long left) 2) row (if is-open "▾" "▸"))
    (p/set-colors! g (if (and is-selected focused?) t/header-active-tab-fg t/dialog-fg) bg)
    (p/styled g [p/BOLD] (p/put-str! g (+ (long left) 4) row (p/ellipsize title title-w)))
    (when meta
      (p/set-colors!
        g
        (if (and is-selected focused?) t/header-active-tab-fg (tone-fg tone t/dialog-hint))
        bg)
      (p/put-str! g (- (+ (long left) (long width)) meta-w 1) row (p/ellipsize meta meta-w)))
    (.register interactions/hit-map
               {:bounds {:row row :col (inc (long left)) :width (dec (long width))}
                :kind :live-panel-toggle
                :label (if is-open "Fold view" "Unfold view")
                :view-id view-id
                :enabled? true})))

(defn- paint-panel-tabs!
  [g left row width tabs tab]
  (p/set-colors! g t/dialog-fg t/dialog-bg)
  (p/fill-rect! g (inc (long left)) row (dec (long width)) 1)
  (reduce (fn [^long col name]
            (let [label
                  (str " " (if (str/blank? name) "Views" name) " ")

                  label
                  (p/ellipsize label (max 0 (- (+ (long left) (long width)) col 1)))

                  w
                  (long (p/display-width label))

                  on?
                  (= name tab)]

              (when (pos? w)
                (p/set-colors! g
                               (if on? t/header-active-tab-fg t/dialog-hint)
                               (if on? t/header-active-tab-bg t/dialog-bg))
                (p/styled g (if on? [p/BOLD] []) (p/put-str! g col row label))
                (.register interactions/hit-map
                           {:bounds {:row row :col col :width w}
                            :kind :live-panel-tab
                            :label "Show views"
                            :tab name
                            :enabled? true}))
              (+ col w 1)))
          (+ (long left) 2)
          tabs))

(defn paint-panel!
  "Paint the side panel at `left` from row `top`, `height` rows tall. `state` is the
   TUI panel state: `:tab`, `:selected`, `:folded` and `:focused?`. A `│` rule at
   `left` divides the panel from the chat; it takes the accent while the panel has
   the keyboard. Returns `{:selected view-id :shown [view-id …]}`."
  [g left top width height panes state now-ms]
  (binding [t/dialog-bg t/terminal-bg]
    (let [left (long left)
          top (long top)
          width (long width)
          height (long height)
          tabs (panel-tabs panes)
          tab (if (some #{(:tab state)} tabs) (:tab state) (first tabs))
          shown (panel-shown panes tab)
          selected (panel-selected shown (:selected state))
          focused? (boolean (:focused? state))
          tabs? (>= (count tabs) 2)
          body-top (if tabs? (+ top 2) top)
          visible (max 0 (- (+ top height) body-top))
          inner-w (- width 2)
          text-w (max 8 (- inner-w 4))
          rows (panel-rows shown {:selected selected :folded (set (:folded state))} text-w now-ms)
          sel-idx (long (or (first (keep-indexed #(when (and (:is-title %2) (:is-selected %2)) %1)
                                                 rows))
                            0))
          start (if (< sel-idx visible)
                  0
                  (min (- sel-idx (quot visible 3)) (max 0 (- (count rows) visible))))]

      (p/set-colors! g t/dialog-fg t/dialog-bg)
      (p/fill-rect! g left top width height)
      (p/set-colors! g (if focused? t/header-active-tab-accent t/border-fg) t/dialog-bg)
      (doseq [row (range top (+ top height))]
        (p/put-str! g left row "│"))
      (when tabs? (paint-panel-tabs! g left top width tabs tab))
      (doseq [[idx row-entry] (map-indexed vector (take visible (drop start rows)))]
        (let [row (+ body-top (long idx))]
          (if (:is-title row-entry)
            (paint-panel-title! g left row width row-entry focused?)
            (paint-entry! g (+ left 2) row (dec inner-w) (:view-id row-entry) (:entry row-entry)))))
      (p/clear-styles! g)
      {:selected selected :shown (mapv view-id shown) :tab tab})))

(defn inline-entries
  "A compact LIVE receipt. Pane details belong only to the transient viewer."
  [pane _width]
  (let [title (str "LIVE " (flat-text (get-in pane [:view :title])))]
    [{:line title
      :meta {:kind :activity-live-entry
             :run-header? true
             :view-id (view-id pane)
             :live-pane pane
             :live-entry {:kind :inline-title :text title}}}]))

(defn paint-inline-entry!
  "Paint a compact LIVE receipt; only its button opens the transient viewer."
  [g left row width viewport-top {:keys [live-pane live-entry]}]
  (binding [t/dialog-bg t/code-block-bg]
    (let [status (p/ellipsize (if (settled? live-pane) " Recorded " " LIVE ")
                              (max 0 (dec (long width))))
          status-w (p/display-width status)
          ;; Match COPY's two-cell inset; the inline body already reserves one cell.
          status-col (+ (long left) (max 0 (- (long width) status-w 1)))
          title-w (max 0 (- (long width) status-w 2))]

      (p/set-colors! g t/dialog-fg t/dialog-bg)
      (p/styled g [p/BOLD] (p/put-str! g left row (p/ellipsize (:text live-entry) title-w)))
      (components/button! g
                          status-col
                          row
                          status
                          :live-reopen
                          {:extra {:view-id (view-id live-pane)
                                   :bounds {:row (+ (long row) (long viewport-top))
                                            :col status-col
                                            :width status-w}}}))))

(defn transcript-run
  "A compact receipt with its painter attached for transcript projection.
   Keeping these functions here avoids a render → dialogs → render dependency."
  [pane]
  (assoc (run-row pane)
    :live-pane pane
    :inline-entries (fn [width]
                      (mapv #(assoc-in % [:meta :live-paint] paint-inline-entry!)
                            (inline-entries pane width)))))
