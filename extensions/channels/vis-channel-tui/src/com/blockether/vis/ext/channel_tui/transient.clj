(ns com.blockether.vis.ext.channel-tui.transient
  "Magit-style TRANSIENT as a REUSABLE, EMBEDDABLE component.

   A transient is a VALUE — a `spec` — and this namespace knows how to lay it
   out, paint it into a rectangle, and (optionally) run its key loop. It owns NO
   screen, NO dialog chrome and NO Lanterna types, so ANY surface that can hand
   over a `TextGraphics` and answer keystrokes can embed one: the magit status
   buffer, the provider dialog, a modal of its own (`dialogs/transient-dialog!`),
   or a panel that does not exist yet.

   THREE values, three concerns:

     `spec`   WHAT the transient is — a pure, reusable description:
              `{:title \"Commit\"
                :groups [{:title \"Arguments\" :items [item …]} …]
                :read-option (fn [item current] str|nil)}`
              where an `item` is
              `{:key \"h\" :type :switch|:option|:action :id :no-verify
                :label \"Disable hooks\" :arg \"--no-verify\" :secret? false}`.
              FLAGS (`:switch` / `:option`) render with magit's leading `-` and
              TOGGLE — press once to arm, again to disarm — while COMMANDS
              (`:action`) fire once and close. Keys are CASE-SENSITIVE, exactly
              like magit (`-f` is not `-F`). `:read-option` (impure, optional)
              fetches an OPTION's value; nil (Esc) leaves it unchanged.

     `region`  WHERE it sits — the host's rectangle:
              `{:left :inner-w :hint-row :text-w :min-row :restore!}`.
              `:left`/`:inner-w` are the host frame's border column and inner
              width, `:hint-row` the row the host's hint bar owns (the popup
              REPLACES it), `:text-w` the text budget inside the padding.
              `:min-row` is the first row the popup may touch — a tall transient
              stops there instead of climbing over whatever the host keeps
              visible. A band wipes ITS OWN rows and nothing above them; a host
              that pages bands of DIFFERENT heights passes `:restore!`
              (`(fn [from to])`, from `dialogs/frame-restorer`), and the rows a
              taller band covered are given back to the HOST instead of blanked.

     `host`   HOW to talk to the terminal — the ONLY impure dependency:
              `{:g          TextGraphics to paint into
                :hint-bar!  (fn [g left row inner-w pairs])
                :refresh!   (fn [])         flush the frame
                :read-key!  (fn [])         `:esc` | Character | nil}`
              nil from `:read-key!` means \"nothing actionable\" — the loop simply
              repaints. `dialogs/transient-host` builds the standard modal one.

   The popup is bottom-anchored INSIDE the host's frame and reads
   `───` / bold title / `───` / groups / `───` / hint bar: the same chrome the
   host gives any other titled section, never a second window pasted on top."
  (:refer-clojure :exclude [run!])
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.theme :as t]
            [com.blockether.vis.ext.channel-tui.transient.spec :as sp]))

(set! *unchecked-math* :warn-on-boxed)

;;; ── Pure model ──────────────────────────────────────────────────────────────

(defn check
  "nil when `spec` is a legal transient, else ONE line saying why. The same
   judge [[run!]] uses, answering instead of throwing: a producer's own test —
   or a caller assembling a spec from live data — asks BEFORE a terminal is
   involved, and gets prose a human can read."
  [spec]
  (sp/spec-error spec))

(defn item-by-key
  "PURE: the spec item bound to single character `ch` (a Character or string),
   scanning every group in order. nil when nothing is bound."
  [spec ch]
  (let [k (str ch)]
    (some (fn [{:keys [items]}]
            (some #(when (= k (:key %)) %) items))
          (:groups spec))))

(defn item-by-id
  "PURE: the spec item carrying `id`, scanning every group in order. nil when
   the band does not offer that command right now — which is how a caller that
   fires a command WITHOUT a keystroke (a slash that names it) finds out the
   band never showed it."
  [spec id]
  (when (some? id)
    (some (fn [{:keys [items]}]
            (some #(when (= id (:id %)) %) items))
          (:groups spec))))

(defn- index-by-key
  "PURE: `{keystroke item}` over every row of `spec`, in group order. Two rows
   sharing a keystroke make one of them unreachable, which is why the spec
   refuses it — so this index loses nothing and the key loop stops scanning."
  [spec]
  (into {} (map (juxt :key identity)) (mapcat :items (:groups spec))))

(defn- item-step
  "PURE: what pressing `it`'s key does to `state`, decided by the TRAITS its
   type carries in `sp/item-types` and never by the type keyword itself — a
   COMMAND ends the run, a VALUED row sends the caller off to read a value, a
   FLAG toggles in place. A new kind of row is a row in that table, not a new
   branch here."
  [state {:keys [type id] :as it}]
  (let [{:keys [is-flag is-valued is-command]} (get sp/item-types type)]
    (cond is-command {:kind :action :item it}
          is-valued {:kind :option :item it}
          is-flag {:kind :continue
                   :state
                   (update state :switches #(if (contains? % id) (disj % id) (conj (or % #{}) id)))}
          :else {:kind :continue :state state})))

(defn- step-key
  "PURE: [[item-step]] for whatever `index` binds `ch` to. An unbound key is a
   no-op that keeps the popup open."
  [index state ch]
  (if-let [it (get index (str ch))]
    (item-step state it)
    {:kind :continue :state state}))

(defn toggle
  "PURE reducer for ONE keystroke against a transient `state`
   (`{:switches #{ids} :options {id val}}`). Returns a map whose `:kind` tells
   the impure caller what to do next:
     {:kind :continue :state s'}  a SWITCH flipped (or an unbound key — no-op)
     {:kind :option   :item it}   an OPTION was hit; caller reads a value then
                                  re-enters with it stored under [:options id]
     {:kind :action   :item it}   an ACTION fires; caller runs it and closes.
   Switches are the only kind this fn mutates; options/actions leave `state`
   untouched (the impure loop finishes their job)."
  [spec state ch]
  (step-key (index-by-key spec) state ch))

(defn key-glyph
  "PURE: the key column glyph magit paints for one item. FLAGS (`:switch` /
   `:option`) carry magit's leading `-` — `-h`, `-t` — so a toggle can never be
   mistaken for a fire-once verb; COMMANDS (`sp/command-types`) show the bare
   key. Tolerant of a partial item: an unknown type is not a command."
  [{:keys [type key]}]
  (if (contains? sp/command-types type) (str key) (str "-" key)))

(defn item-arg
  "PURE: the trailing git-argument cell magit shows for a FLAG: `(--no-verify)` for
   a switch, `(%topic=fix)` for an option carrying `value`. nil for commands (a
   command contributes no argument) and for flags that name none.

   A `:secret? true` option NEVER renders what it holds: an armed API key shows
   as `(••••••)`, so a credential can be carried by a transient without being
   echoed onto the screen or into a screenshot."
  [{:keys [type arg secret?]} value]
  (let
    [raw
     (when (and value (not (str/blank? (str value)))) (str value))

     v
     (when raw (if secret? "••••••" raw))]

    (cond (contains? sp/command-types type) nil
          (and arg (contains? sp/valued-types type)) (str "(" arg v ")")
          arg (str "(" arg ")")
          v (str "(" v ")")
          :else nil)))

(def pane-lead
  "Columns between a pane's left edge and the first ink in it. The heading starts
   here, and so does the pane's own grid."
  2)

(def item-indent
  "Columns an item row steps RIGHT of the heading above it. A heading NAMES a
   column and the verbs under it are its content, so they are indented under it
   exactly like magit's; flush left, the key letter collided with the first
   letter of the heading and the two read as one ragged column."
  1)

(def key-gap
  "Columns between the key column and the description column. One space beside a
   one-letter key read as a typo; two make the keys a COLUMN the eye can run
   down."
  2)

(def pane-trail
  "The one column a description keeps CLEAR at its pane's right edge — `draw-item!`
   ellipsizes one cell short of it, so a full-width label never runs into the
   column beside it. It is part of what a pane NEEDS ([[pane-natural]]): left out
   of the measurement, a pane sized to its own widest verb still painted that verb
   with an ellipsis."
  1)

(defn pane-columns
  "PURE: `{:key-w :label-w}` for ONE pane's display rows — that pane's OWN grid.

   A pane measures ITSELF: a category of long verbs never forces its label
   column onto the narrow category beside it, which is what left a wide band
   packed into its left half with every pane the width of the worst label on
   the screen."
  [pane]
  (let [items (keep :item pane)]
    {:key-w (reduce max 0 (map #(long (p/display-width (key-glyph %))) items))
     :label-w (reduce max 0 (map #(long (p/display-width (str (:label %)))) items))}))

(defn columns
  "PURE: `{:key-w :label-w}` for the whole spec — the single column's grid, and
   what a one-pane band is measured with."
  [spec]
  (pane-columns (map (fn [it] {:item it}) (mapcat :items (:groups spec)))))

(def band-body-pad
  "Blank display rows of BREATHING SPACE at the TOP and BOTTOM of every pane: the
   band's body is not glued to the rule that carries its title, nor to the closing
   rule above the hint bar. It is part of the BODY, not of the chrome, so a pane
   the terminal is too short for loses a padding row before it loses a verb."
  1)

(defn- pad-block
  "PURE: `block` with [[band-body-pad]] blank rows above and below it."
  [block]
  (let [blanks (repeat (long band-body-pad) {:kind :blank})]
    (-> (vec blanks)
        (into block)
        (into blanks))))

(defn rows
  "PURE: the popup's display rows, top to bottom — `{:kind :header :text}`,
   `{:kind :item :item}` and `{:kind :blank}` spacers between groups, wrapped in
   the [[band-body-pad]] blanks the band breathes with.

   The title carries its OWN rule underneath (see `geometry`), so the first
   group header needs no blank margin of its own; groups after it still get one
   from the trailing blank of the group before."
  [spec]
  (pad-block (vec (butlast (into []
                                 (mapcat (fn [{:keys [title items]}]
                                           (concat [{:kind :header :text title}]
                                                   (map (fn [it]
                                                          {:kind :item :item it})
                                                        items)
                                                   [{:kind :blank}])))
                                 (:groups spec))))))

(def ^:private pane-gap
  "Columns between two side-by-side panes: pure BREATHING SPACE. Panes are told
   apart by their own bold headings and by the gap, not by a rule — a `│` down
   the body made a two-column hydra read as a table."
  3)

(defn- group-block
  "PURE: one group's display rows — its header, then its items."
  [{:keys [title items]}]
  (into [{:kind :header :text title}]
        (map (fn [it]
               {:kind :item :item it}))
        items))

(defn panes
  "PURE: `spec`'s groups dealt into at most `n` side-by-side panes, in order,
   ONE GROUP PER PANE while there is a pane for it — a category IS a column,
   heading and verbs together, so the grid reads like which-key's own. Only when
   there are more groups than panes are neighbours packed together, balanced by
   row count and still NEVER splitting a group.

   Every pane is padded with blanks to the tallest, so pane `j` row `i` is always
   the cell at that grid position and a painter walks a rectangle instead of a
   ragged list.

   `n` = 1 is exactly [[rows]]: one pane IS the single column."
  [spec ^long n]
  (let
    [blocks
     (mapv group-block (:groups spec))

     total
     (+ (long (reduce + 0 (map count blocks))) (max 0 (dec (count blocks))))

     target
     (max 1 (long (Math/ceil (/ (double total) (double (max 1 n))))))

     packed
     (if (>= n (count blocks))
       (mapv vec blocks)
       (reduce (fn [acc block]
                 (let [cur (peek acc)]
                   (if (and (seq cur) (< (count acc) n) (> (+ (count cur) 1 (count block)) target))
                     (conj acc (vec block))
                     (conj (pop acc)
                           (into (cond-> cur
                                   (seq cur)
                                   (conj {:kind :blank}))
                                 block)))))
               [[]]
               blocks))

     padded
     ;; Every pane breathes the same: the grid is one rectangle, so a blank top
     ;; row on one column and none on the next would tilt the whole band.
     (mapv pad-block packed)

     h
     (long (reduce max 0 (map count padded)))]

    (mapv (fn [pane]
            (into pane (repeat (- h (count pane)) {:kind :blank})))
          padded)))

(defn pane-natural
  "PURE: the columns ONE pane needs for its own content — the lead, its widest
   heading, and its own key/label grid with the indent, the gutter and the
   [[pane-trail]] it is painted with. Measuring the PANE (not the spec) is what
   lets a band stand a narrow category beside a wide one."
  ^long [pane]
  (let
    [{:keys [key-w label-w]}
     (pane-columns pane)

     head-w
     (reduce max
             0
             (map #(long (p/display-width (str (:text %))))
                  (filter #(= :header (:kind %)) pane)))]

    (max (+ (long pane-lead) (long item-indent) (long key-w) (long key-gap)
            (long label-w) (long pane-trail))
         (+ (long pane-lead) (long head-w)))))

(defn- panes-fit?
  "PURE: do `ps` stand side by side inside `inner-w`, gaps included?"
  [ps ^long inner-w]
  (<= (+ (long (reduce + 0 (map pane-natural ps)))
         (* (long pane-gap) (dec (count ps))))
      inner-w))

(defn pane-count
  "PURE: how many side-by-side panes `spec` is dealt into inside `region`.

   EVERY CATEGORY GETS ITS OWN COLUMN: as many panes as the spec has groups, so
   a heading is never stacked under another heading while the terminal is wide
   enough to stand them side by side. The bound is what those panes ACTUALLY
   measure ([[pane-natural]]) rather than the widest label anywhere in the spec
   — one long verb in one category used to shrink the whole grid. When the
   groups outnumber the room the leftovers are packed by [[panes]]. No region ⇒
   one column."
  ^long [spec region]
  (if (or (nil? region) (not (:is-sideless region)))
    1
    (let [inner-w (long (or (:inner-w region) 0))]
      (loop [n (max 1 (count (:groups spec)))]
        (if (or (= n 1) (panes-fit? (panes spec n) inner-w)) n (recur (dec n)))))))

(defn pane-widths
  "PURE: the columns EACH of `ps` gets inside `region`'s inner width, the gaps
   between them already paid for.

   The band is ONE GRID: every pane is an EQUAL share of the width, widened only
   where a pane's own [[pane-natural]] needs more, and the cells that do not
   divide are handed out left to right. So heading `j` and key column `j` stand at
   the same stride the whole way across, and no category falls below what its own
   verbs need. Sharing the leftover in PROPORTION to what each pane happened to
   measure is what put four headings at four unrelated offsets (37/35/37/38 on a
   160-column band) — full width, and still not a grid.

   When even the naturals do not fit side by side (a column holding a very long
   label) the width is split evenly and the labels ellipsize — which for one pane
   IS the whole band."
  [region ps]
  (let
    [inner-w
     (long (or (:inner-w region) 0))

     n
     (max 1 (count ps))

     nats
     (mapv pane-natural ps)

     avail
     (- inner-w (* (long pane-gap) (dec n)))

     share
     (quot avail n)

     ;; The equal grid, and the panes a long category pushes wider than it.
     floors
     (mapv (fn [^long nat] (max share nat)) nats)

     slack
     (- avail (long (reduce + 0 floors)))]

    (if (neg? slack)
      (vec (repeat n (max 1 share)))
      (let [each (quot slack n)
            extra (rem slack n)]
        (into []
              (map-indexed (fn [^long i ^long w] (+ w each (if (< i extra) 1 0))))
              floors)))))

(def ^:private chrome-rows
  "Rows of the popup's OWN chrome above its body: the opening separator, and
   nothing else. A transient band carries NO title row — its first row is the
   `───` rule and everything under it is the column grid. The closing rule and
   the hint bar belong to the host's bottom chrome, so they are not counted
   here."
  1)

(defn height
  "PURE: rows the popup needs INSIDE the host's frame — its opening separator
   and every display row (the padding blanks included). The closing rule and the
   hint bar are the host's OWN bottom chrome, so they are not counted; a host
   sizes its box with this before it paints one."
  ^long [spec]
  (+ (long chrome-rows) (count (rows spec))))

(defn band-geometry
  "PURE: which row of `region` every part of a band `n` display rows tall
   lands on.

     `:sep-row`         the band's opening separator — the FIRST row it paints
     `:title-row`       the bold title, alone on its row (title-less band: the
                        separator's own row, so nothing extra is painted)
     `:title-rule-row`  the title's OWN rule, closing the title band
     `:body-top`        first display row
     `:visible`         display rows that actually fit (overflow is dropped)
     `:foot-rule-row`   the rule directly above the hint bar
     `:foot-row`        the hint bar (the host's own hint row)
     `:wipe-top`        first row the popup wipes before painting

   Everything is clamped to `:min-row`, so a tall band — or a short
   terminal — stops at the host's content top instead of climbing over it.

   The row count is the parameter, not the spec, because a band is not always a
   transient: the human-input form paints its own plan rows into exactly this
   geometry, and both must land on the same chrome.

   A band is TITLE-LESS by default — the rule, then the body — because the C-x
   hydra's own heading said nothing its columns did not. A band that asks ONE
   question (`dialogs/band-question-frame!`, the human-input form) passes
   `is-title` true, because there the title IS the question."
  ([region ^long n] (band-geometry region n false))
  ([{:keys [hint-row min-row]} ^long n is-title]
   (let
     [top-limit
      (long (or min-row 0))

      ;; The popup is GLUED to the frame's BOTTOM CHROME: the host paints
      ;; `├───┤` directly above its hint row, and a band that simply overwrote
      ;; that rule left its last command running into the footer text — which
      ;; reads as the popup eating the bottom border. The popup repaints it.
      foot-row
      (long hint-row)

      foot-rule-row
      (dec foot-row)

      ;; Anchor to the bottom of the band: the last body row sits DIRECTLY on
      ;; the closing rule, with the chrome stacked above it.
      body-top
      (max (+ top-limit (if is-title 2 1)) (- foot-rule-row n))

      title-rule-row
      (max top-limit (dec body-top))

      title-row
      (if is-title (max top-limit (dec title-rule-row)) title-rule-row)

      sep-row
      (if is-title (max 0 (dec title-row)) title-row)]

     {:top-limit top-limit
      :sep-row sep-row
      :title-row title-row
      :title-rule-row title-rule-row
      :body-top body-top
      :foot-rule-row foot-rule-row
      :foot-row foot-row
      ;; The band wipes exactly the rows it paints — the buffer above its opening
      ;; separator is what magit keeps visible behind a transient. A host that
      ;; repaints bands of different heights owns those rows and clears them
      ;; itself (`clear-rows!`); the component never reaches over them.
      :wipe-top (max sep-row top-limit)
      ;; The closing rule is the floor: a page taller than the band it was given
      ;; loses its overflow rows rather than painting over the host's frame.
      :visible (min n (max 1 (- foot-rule-row body-top)))})))

(defn geometry
  "PURE: [[band-geometry]] for `spec`'s OWN display rows — where every part of
   the popup lands."
  [region spec]
  (band-geometry region (count (rows spec))))

(def ^:private band-pad
  "Columns of empty space on each end of a SIDELESS band's rules — the same
   inset `render/draw-input-box!` gives the prompt's own top and bottom rules,
   so a band lines up with the chrome it takes over instead of floating beside
   it."
  2)

(def prompt-rows
  "Rows the session's PROMPT box occupies at its resting single-line size: its two
   rules and one text row (`render/input-pad-y` is 0). A caller that knows the
   LIVE height — `screen`'s `input-box-h`, which grows with what is typed — passes
   that instead, so the band stays glued to the prompt however tall it got."
  3)

(defn band-region
  "PURE: the rectangle an in-session BAND paints into on a `cols`×`rows`
   terminal whose content starts at `content-top` and whose prompt box is
   `prompt-h` rows tall.

   The session frame is SIDELESS — the prompt is two horizontal rules with no
   `│` rails — so a band that takes it over borrows exactly that: rules inset
   [[band-pad]] columns, text one column further in, and `:is-sideless true`
   so the chrome paints rules instead of `├───┤` junctions and wipes the FULL
   terminal width (a band sits on the live transcript, not on a modal's own
   paper).

   The band sits ABOVE THE PROMPT, never over it. `:hint-row` is the echo-area
   row directly above the input box (`rows - prompt-h - 3`, mirroring `screen`'s
   own `input-top`/`echo-row` math), so the thing the human is typing into — and
   the footer under it — stay visible and in place while a transient, the C-x
   hydra or a human-input form is up. `:min-row` is the floor, so however tall
   the band the header and the top of the transcript stay on screen."
  ([^long cols ^long rows ^long content-top] (band-region cols rows content-top prompt-rows))
  ([^long cols ^long rows ^long content-top ^long prompt-h]
   (let
     [pad
      (long band-pad)

      min-row
      (max 0 content-top)

      inner-w
      (max 4 (- cols (* 2 pad)))]

     {:left (dec pad)
      :inner-w inner-w
      :text-w (max 1 (- inner-w 2))
      :hint-row (max (+ min-row 3) (- rows prompt-h 3))
      :min-row min-row
      :cols cols
      :is-sideless true})))

;;; ── Paint ───────────────────────────────────────────────────────────────────

(defn hint-pairs
  "The footer THIS transient shows: what a command key does, the way out, and
   what a flag key does ONLY when the spec actually has a flag. A band whose
   keys are all commands (the draft band spends `c`/`d`/`s`/`k` on verbs) must
   not advertise a `-key` nothing responds to."
  [spec]
  (cond-> []
    (some (comp sp/flag-types :type) (mapcat :items (:groups spec)))
    (conj ["-key" "toggle flag"])

    :always
    (into [["key" "run command"] ["Esc" "cancel"]])))

(defn layout
  "PURE: everything ONE frame needs from `spec`, computed ONCE — the display
   [[rows]], the [[panes]] they are dealt into for `region`, the grid [[columns]]
   every row aligns to, the [[hint-pairs]] its footer shows, the [[height]] a
   host sizes its box with, and the keystroke index its key loop dispatches on.

   Painting used to walk the whole spec twice per frame (once for geometry,
   once for the body) and scan every group again for each keystroke; a run
   builds this once and the loop reads it.

   Without a `region` the layout is the single column: nothing has told it how
   wide or how tall the host is, so nothing may wrap."
  ([spec] (layout spec nil))
  ([spec region]
   (let
     [n
      (pane-count spec region)

      ps
      (panes spec n)

      pane-h
      (count (first ps))]

     {:rows (rows spec)
      :panes ps
      :pane-count (count ps)
      :pane-ws (when region (pane-widths region ps))
      :pane-cols (mapv pane-columns ps)
      :row-count pane-h
      :columns (columns spec)
      :hint-pairs (hint-pairs spec)
      :title (:title spec)
      :height (+ (long chrome-rows) pane-h)
      :by-key (index-by-key spec)})))

(defn- draw-item!
  "One transient row as a GRID cell: key glyph column, description column, and the
   git-argument column.

   A FLAG (`:switch` / `:option`) reads dim while OFF and turns BOLD `dialog-fg`
   with its argument accented while ON, so pressing its key visibly toggles it.
   A COMMAND (`:action`) always shows a BOLD accented key with its description in
   full `dialog-fg` — a command is never `off`."
  [g left row inner-w {:keys [key-w label-w]} {:keys [type label] :as it} active? value]
  (let
    [action?
     (= :action type)

     keytxt
     (key-glyph it)

     argtxt
     (item-arg it value)

     ;; The key column steps in past the heading above it, and the description
     ;; column clears the key by [[key-gap]] — the two are COLUMNS, not a
     ;; sentence with a letter in front of it.
     x
     (+ (long left) (long pane-lead) (long item-indent))

     lx
     (+ (long x) (long key-w) (long key-gap))

     right
     (+ (long left) (long inner-w))

     label-txt
     (str label)

     ;; Argument column: aligned past the widest description when it fits, else
     ;; trailing the description inline, else dropped (a very narrow buffer).
     arg-x
     (when argtxt
       (let
         [w
          (long (p/display-width argtxt))

          col
          (+ (long lx) (long label-w) 2)

          inline
          (+ (long lx) (long (p/display-width label-txt)) 2)]

         (cond (<= (+ col w) (- (long right) (long pane-trail))) col
               (<= (+ inline w) (- (long right) (long pane-trail))) inline)))

     shown
     (p/ellipsize label-txt
                  (long (max 0 (- (long (or arg-x right)) (long lx) (long pane-trail)))))

     fg
     (if (or action? active?) t/dialog-fg t/dialog-hint)]

    (p/set-colors! g t/dialog-fg t/dialog-bg)
    (p/fill-rect! g (inc (long left)) row inner-w 1)
    (p/set-colors! g t/dialog-hint-key t/dialog-bg)
    (if (or action? active?)
      (p/styled g [p/BOLD] (p/put-str! g x row keytxt))
      (p/put-str! g x row keytxt))
    (p/set-colors! g fg t/dialog-bg)
    (if active? (p/styled g [p/BOLD] (p/put-str! g lx row shown)) (p/put-str! g lx row shown))
    (when arg-x
      (p/set-colors! g (if active? t/dialog-hint-key t/dialog-hint) t/dialog-bg)
      (if active?
        (p/styled g [p/BOLD] (p/put-str! g arg-x row argtxt))
        (p/put-str! g arg-x row argtxt)))
    (p/set-colors! g t/dialog-fg t/dialog-bg)))

(defn draw-rule!
  "One horizontal rule of the popup's chrome, on whatever frame `region` names.

   A framed popup draws a capped `├───┤` separator that joins the host's rails;
   a SIDELESS band has no rails for a junction to join, so it draws the prompt's
   own inset line instead. One function, so a band and a modal wear the same
   chrome and neither grows a second copy of it."
  ([g region row] (draw-rule! g region row nil))
  ([g {:keys [left inner-w is-sideless]} row label]
   (let
     [left
      (long left)

      inner-w
      (long inner-w)]

     (if is-sideless
       (do (p/set-colors! g t/border-fg t/dialog-bg)
           (p/put-str! g (inc left) row (p/horiz-line inner-w)))
       (do (p/set-colors! g t/dialog-border t/dialog-bg)
           (p/draw-separator! g left (+ left inner-w 1) row)))
     ;; The band has no title ROW, so a titled band says its name ON the rule —
     ;; magit's own `── Commit ──`. The first row stays chrome and every row
     ;; below it stays column grid.
     (when-not (str/blank? (str label))
       (let [txt (str " " (p/ellipsize (str label) (long (max 0 (- inner-w 6)))) " ")]
         (p/set-colors! g t/dialog-fg t/dialog-bg)
         (p/styled g [p/BOLD] (p/put-str! g (+ left 3) row txt))))
     (p/set-colors! g t/dialog-fg t/dialog-bg))))

(defn clear-rows!
  "Blank rows `from`..`to` (INCLUSIVE) inside the host's frame: dialog paper
   across the inner columns and the frame's plain edge back in both border
   columns.

   Wiping only the band's INNER columns leaves whatever the host painted in the
   two border columns: a status buffer's own section separator survived as stray
   `├`/`┤` junctions beside the popup. Capping separators put their own junctions
   back afterwards.

   A SIDELESS region (`band-region`) has no border columns, but its paper is
   still its OWN rectangle: it fills the band's inner columns with the band's
   bg and repaints the [[band-pad]] margins on each side with the TERMINAL's
   own background, so the band is a slab inset to its rules instead of a colour
   wash from screen edge to screen edge, and no stale transcript survives beside
   it either.

   A transient erases exactly the rows it paints. A host that repaints BANDS OF
   DIFFERENT HEIGHTS into one rectangle — the model picker paging a catalog —
   owns the rows between its content and the band, and erases them with this
   before running the next page."
  [g {:keys [left inner-w is-sideless cols]} from to]
  (let
    [left
     (long left)

     inner-w
     (long inner-w)

     right
     (+ left inner-w 1)]

    (dotimes [i (max 0 (inc (- (long to) (long from))))]
      (let [row (+ (long from) i)]
        (p/set-colors! g t/dialog-fg t/dialog-bg)
        (if is-sideless
          (do (p/set-colors! g t/dialog-fg t/terminal-bg)
              (p/fill-rect! g 0 row (inc left) 1)
              (p/fill-rect! g (+ left inner-w 1) row (max 0 (- (long cols) (+ left inner-w 1))) 1)
              (p/set-colors! g t/dialog-fg t/dialog-bg)
              (p/fill-rect! g (inc left) row inner-w 1))
          (do (p/fill-rect! g (inc left) row inner-w 1)
              (p/set-colors! g t/dialog-border t/dialog-bg)
              (p/set-char! g left row p/BOX_V)
              (p/set-char! g right row p/BOX_V)))))))

(defn- paint-layout!
  "Paint ONE frame of an already-computed [[layout]] at `state`. Everything the
   spec decides is decided in [[layout]]; this fn only puts cells on a screen.

   The body is a GRID: pane `j` is [[pane-widths]] wide and starts one
   [[pane-gap]] after the pane before it ended
   after pane `j-1`, and the gap is EMPTY — a sibling column is told by its own
   bold heading and by the space, never by a rule down the body.

   The band has NO title row: its FIRST row is the opening `───` rule and
   everything under it is the column grid, because a heading over columns that
   already name themselves is a row of chrome bought with a row of content.

   A SIDELESS band is not a dialog: it lies on the LIVE transcript and wears the
   TERMINAL's own paper — no tint, in the body and in the footer alike, so it is
   the border that says where the band is. That is bound ONCE here, so every
   painter below — rules, rows, hint bar — follows without carrying a colour
   argument of its own.

   The band is BORDERED: its opening and closing rules are corner-capped and
   every row between them carries a `│` in the same two columns. The hint bar
   rides INSIDE that box, on the row directly above the closing rule, under its
   OWN rule, so the footer is fenced off from the commands it explains."
  [{:keys [g hint-bar!]} {:keys [left inner-w restore!] :as region}
   {panes :panes n :row-count pane-ws :pane-ws pane-cols :pane-cols hints :hint-pairs title :title}
   state]
  (binding [t/dialog-bg (if (:is-sideless region) t/terminal-bg t/dialog-bg)]
    (let
      [{:keys [sep-row body-top foot-rule-row foot-row wipe-top visible top-limit]}
       (band-geometry region n)
       sideless? (boolean (:is-sideless region))
       ;; A sideless band CLOSES below its footer: the hint bar takes the row the
       ;; rule used to own and the rule drops onto the host's hint row, so the
       ;; footer is inside the box. A framed popup keeps the host's own order.
       hint-at (long (if sideless? foot-rule-row foot-row))
       rule-at (long (if sideless? foot-row foot-rule-row))
       ;; The footer is FENCED OFF by its own rule above it. The row it costs is
       ;; the body's bottom [[band-body-pad]] blank, not a verb.
       hint-rule-at (long (if sideless? (dec hint-at) hint-at))
       visible (long (if sideless? (max 1 (dec (long visible))) visible))
       left (long left)
       inner-w (long inner-w)
       ;; Each pane owns its own width, so pane `j` starts where every pane
       ;; before it ended plus one gap — never at a fixed stride.
       widths (vec (if (seq pane-ws) pane-ws (repeat (count panes) inner-w)))
       lefts (vec (reductions (fn [^long x ^long w] (+ x w (long pane-gap))) left widths))
       clear-row! (fn [row]
                    (clear-rows! g region row row))]

      ;; A taller band before this one covered rows this one does not. They belong
      ;; to the HOST again: blanking them left a hole in the list behind the popup,
      ;; so the host's snapshot is painted back onto them instead.
      (when restore! (restore! top-limit (dec (long wipe-top))))
      (dotimes [i (max 1 (- (long body-top) (long wipe-top)))]
        (clear-row! (+ (long wipe-top) i)))
      (when (>= (long sep-row) (max (long wipe-top) (long top-limit)))
        (draw-rule! g region sep-row title))
      ;; The closing rule and the hint bar are the band's own rows too: they are
      ;; papered first, so the slab's edges are the same colour all the way down.
      (when (>= (long foot-row) (long top-limit)) (clear-row! foot-row))
      (when (> (long foot-rule-row) (max (long sep-row) (long top-limit)))
        (clear-row! foot-rule-row))
      (when (> rule-at (max (long sep-row) (long top-limit))) (draw-rule! g region rule-at))
      (when (and sideless? (> hint-rule-at (max (long sep-row) (long top-limit))))
        (clear-row! hint-rule-at)
        (draw-rule! g region hint-rule-at))
      (p/set-colors! g t/dialog-hint-key t/dialog-bg)
      (dotimes [i visible]
        (let [row (+ (long body-top) (long i))]
          (clear-row! row)
          (dotimes [j (count panes)]
            (let
              [pane-left (long (nth lefts j))
               pane-w (long (nth widths j))
               r (nth (nth panes j) i)]

              (case (:kind r)
                :header
                (do (p/set-colors! g t/dialog-fg t/dialog-bg)
                    (p/styled g
                              [p/BOLD]
                              (p/put-str! g
                                          (+ pane-left (long pane-lead))
                                          row
                                          (p/ellipsize (str (:text r))
                                                       (long (max 1 (- pane-w (long pane-lead))))))))

                :blank
                nil

                :item
                (let
                  [{:keys [type id] :as it} (:item r)
                   {:keys [is-flag is-valued]} (get sp/item-types type)
                   active? (boolean (or (and is-valued (contains? (:options state) id))
                                        (and is-flag (contains? (:switches state) id))))
                   value (when is-valued (get (:options state) id))]

                  (draw-item! g pane-left row pane-w (nth pane-cols j) it active? value)))))))
      (when (and sideless? (>= hint-at (long top-limit))) (clear-row! hint-at))
      (hint-bar! g left hint-at inner-w hints)
      ;; The band's own BORDER: corner-capped rules with a `│` down both edge
      ;; columns, so a sideless transient is a closed box over the transcript —
      ;; the hint bar included.
      (when sideless?
        (let [right (+ left inner-w 1)]
          (p/set-colors! g t/border-fg t/dialog-bg)
          (when (>= (long sep-row) (long top-limit))
            (p/set-char! g left sep-row p/BOX_TL)
            (p/set-char! g right sep-row p/BOX_TR))
          (doseq [^long r (range (inc (long sep-row)) rule-at)]
            (when (>= r (long top-limit))
              (p/set-char! g left r p/BOX_V)
              (p/set-char! g right r p/BOX_V)))
          (when (> rule-at (max (long sep-row) (long top-limit)))
            (p/set-char! g left rule-at p/BOX_BL)
            (p/set-char! g right rule-at p/BOX_BR))
          (p/set-colors! g t/dialog-fg t/dialog-bg))))))

(defn paint!
  "Paint ONE frame of `spec` at `state` into `region` on `host`. Pure geometry,
   one pass, no key handling — a host that owns its own event loop embeds a
   transient with this plus `toggle`; `run!` is the batteries-included loop.
   A host that paints many frames computes [[layout]] once instead."
  [host region spec state]
  (paint-layout! host region (layout spec region) state))

;;; ── Run ─────────────────────────────────────────────────────────────────────

(defn- invalid!
  "Refuse at the ONE seam where a producer's data meets a terminal, with the
   envelope the rest of the product uses: a `:type` a caller can match and the
   spec ns' own one-line `:reason`."
  [type message data]
  (throw (ex-info (str "Invalid transient " (name type) ": " message)
                  (assoc data
                    :type type
                    :reason message))))

(defn run!
  "Paint `spec` into `region` on `host` and run its key loop until an ACTION
   fires or the user cancels. Returns `{:action id :switches #{…} :options {…}}`,
   or nil on Esc. Flags toggle in place and keep the popup open; an OPTION calls
   the spec's `:read-option` for a value.

   The contract is checked ONCE per run, here: an illegal spec, an unpaintable
   region or a host that cannot answer keystrokes throws
   `:vis/transient-invalid-spec` / `-invalid-region` / `-invalid-host` before a
   single cell is painted, and a `:read-option` that hands back something no row
   can carry throws `:vis/transient-invalid-option` instead of painting it. Not
   once per keystroke: the loop below runs on data already known legal."
  [{:keys [read-key! refresh!] :as host} region spec]
  (when-let [why (sp/spec-error spec)]
    (invalid! :vis/transient-invalid-spec why {:spec spec}))
  (when-let [why (sp/region-error region)]
    (invalid! :vis/transient-invalid-region why {:region region}))
  (when-let [why (sp/host-error host)]
    (invalid! :vis/transient-invalid-host why {}))
  (let
    [lay
     (layout spec region)

     read-option
     (or (:read-option spec) (constantly nil))]

    (loop [state {:switches #{} :options {}}]
      (paint-layout! host region lay state)
      (refresh!)
      (let [k (read-key!)]
        (cond (= :esc k) nil
              (nil? k) (recur state)
              :else (let [r (step-key (:by-key lay) state k)]
                      (case (:kind r)
                        :continue
                        (recur (:state r))

                        :option
                        (let
                          [{:keys [id] :as it} (:item r)
                           v (read-option it (get (:options state) id))]

                          (when (some? v)
                            (when-let [why (sp/option-value-error v)]
                              (invalid! :vis/transient-invalid-option why {:item it})))
                          (recur (if (nil? v) state (assoc-in state [:options id] v))))

                        :action
                        {:action (:id (:item r))
                         :switches (:switches state)
                         :options (:options state)})))))))
