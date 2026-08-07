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

(defn columns
  "PURE: `{:key-w :label-w}` column widths for the transient grid. Every group's
   items share one key column and one description column, so flags and commands
   line up as a grid exactly like magit's popup."
  [spec]
  (let [items (mapcat :items (:groups spec))]
    {:key-w (reduce max 0 (map #(long (p/display-width (key-glyph %))) items))
     :label-w (reduce max 0 (map #(long (p/display-width (str (:label %)))) items))}))

(defn rows
  "PURE: the popup's display rows, top to bottom — `{:kind :header :text}`,
   `{:kind :item :item}` and `{:kind :blank}` spacers between groups.

   The title carries its OWN rule underneath (see `geometry`), so the first
   group header needs no blank margin of its own; groups after it still get one
   from the trailing blank of the group before."
  [spec]
  (vec (butlast (into []
                      (mapcat (fn [{:keys [title items]}]
                                (concat [{:kind :header :text title}]
                                        (map (fn [it]
                                               {:kind :item :item it})
                                             items)
                                        [{:kind :blank}])))
                      (:groups spec)))))

(def ^:private pane-gap
  "Columns between two side-by-side panes: one of padding, the `│` rule itself,
   one more of padding. The rule lives in the MIDDLE column, so a pane's text
   never touches it."
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

     h
     (long (reduce max 0 (map count packed)))]

    (mapv (fn [pane]
            (into pane (repeat (- h (count pane)) {:kind :blank})))
          packed)))

(defn pane-count
  "PURE: how many side-by-side panes `spec` is dealt into inside `region`.

   EVERY CATEGORY GETS ITS OWN COLUMN: as many panes as the spec has groups, so
   a heading is never stacked under another heading while the terminal is wide
   enough to stand them side by side. The only bound is the width one pane needs
   for its own key and label columns; when the groups outnumber that capacity the
   leftovers are packed by [[panes]]. No region ⇒ one column."
  ^long [spec region]
  (if (or (nil? region) (not (:is-sideless region)))
    1
    (let
      [{:keys [key-w label-w]}
       (columns spec)

       natural
       (+ 3 (long key-w) (long label-w))

       inner-w
       (long (or (:inner-w region) 0))

       capacity
       (max 1 (quot (+ inner-w (long pane-gap)) (+ natural (long pane-gap))))]

      (max 1 (min (count (:groups spec)) capacity)))))

(defn pane-width
  "PURE: the columns ONE of `n` panes gets inside `region`'s inner width, the
   gaps between them already paid for.

   A pane is only as wide as its own grid needs — gutter, key column, label — so
   the columns sit BESIDE each other like which-key's, instead of one heading
   floating alone at the far edge of a wide terminal. The even split is the
   ceiling, never the target."
  ^long [region spec ^long n]
  (let
    [{:keys [key-w label-w]}
     (columns spec)

     inner-w
     (long (or (:inner-w region) 0))

     share
     (max 1 (quot (- inner-w (* (long pane-gap) (dec n))) n))]

    (if (= 1 n) share (max 1 (min share (+ 4 (long key-w) (long label-w)))))))

(def ^:private chrome-rows
  "Rows of the popup's OWN chrome above its body: the opening separator, the
   title, and the title's rule. The closing rule and the hint bar belong to the
   host's bottom chrome, so they are not counted here."
  3)

(defn height
  "PURE: rows the popup needs INSIDE the host's frame — its opening separator,
   the title, the title's rule, and every display row. The closing rule and the
   hint bar are the host's OWN bottom chrome, so they are not counted; a host
   sizes its box with this before it paints one."
  ^long [spec]
  (+ (long chrome-rows) (count (rows spec))))

(defn band-geometry
  "PURE: which row of `region` every part of a band `n` display rows tall
   lands on.

     `:sep-row`         the band's opening separator
     `:title-row`       the bold title, alone on its row
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
   geometry, and both must land on the same chrome."
  [{:keys [hint-row min-row]} ^long n]
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
     ;; the closing rule, with the title's rule, the title and the opening
     ;; separator stacked above it.
     body-top
     (max (+ top-limit 2) (- foot-rule-row n))

     title-rule-row
     (max top-limit (dec body-top))

     title-row
     (max top-limit (dec title-rule-row))

     sep-row
     (max 0 (dec title-row))]

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
     :visible (min n (max 1 (- foot-rule-row body-top)))}))

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

     {:title (:title spec)
      :rows (rows spec)
      :panes ps
      :pane-count (count ps)
      :pane-w (if region (pane-width region spec (count ps)) 0)
      :row-count pane-h
      :columns (columns spec)
      :hint-pairs (hint-pairs spec)
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

     x
     (+ (long left) 2)

     lx
     (+ (long x) (long key-w) 1)

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

         (cond (<= (+ col w) (dec (long right))) col
               (<= (+ inline w) (dec (long right))) inline)))

     shown
     (p/ellipsize label-txt (max 0 (- (long (or arg-x right)) (long lx) 1)))

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
  [g {:keys [left inner-w is-sideless]} row]
  (let
    [left
     (long left)

     inner-w
     (long inner-w)]

    (if is-sideless
      (do (p/set-colors! g t/border-fg t/dialog-bg)
          (p/put-str! g (inc left) row (p/horiz-line inner-w)))
      (do (p/set-colors! g t/dialog-border t/dialog-bg)
          (p/draw-separator! g left (+ left inner-w 1) row)))))

(defn clear-rows!
  "Blank rows `from`..`to` (INCLUSIVE) inside the host's frame: dialog paper
   across the inner columns and the frame's plain edge back in both border
   columns.

   Wiping only the band's INNER columns leaves whatever the host painted in the
   two border columns: a status buffer's own section separator survived as stray
   `├`/`┤` junctions beside the popup. Capping separators put their own junctions
   back afterwards.

   A SIDELESS region (`band-region`) has no border columns and no paper of its
   own: it wipes the FULL terminal width, because anything it does not repaint
   is the live transcript showing through between its rules.

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
          (p/fill-rect! g 0 row (long cols) 1)
          (do (p/fill-rect! g (inc left) row inner-w 1)
              (p/set-colors! g t/dialog-border t/dialog-bg)
              (p/set-char! g left row p/BOX_V)
              (p/set-char! g right row p/BOX_V)))))))

(defn- paint-layout!
  "Paint ONE frame of an already-computed [[layout]] at `state`. Everything the
   spec decides is decided in [[layout]]; this fn only puts cells on a screen.

   The body is a GRID: pane `j` starts [[pane-width]] + [[pane-gap]] columns
   after pane `j-1`, and the middle column of every gap carries a `│` down the
   whole body — which is what tells a reader that the next heading is a sibling
   column and not the continuation of this one.

   A SIDELESS band is not a dialog: it lies on the LIVE transcript, so it wears
   `theme/band-bg` instead of the dialog's paper. That is bound ONCE here, so
   every painter below — rules, title, rows, hint bar — follows without carrying
   a colour argument of its own."
  [{:keys [g hint-bar!]} {:keys [left inner-w text-w restore!] :as region}
   {title :title panes :panes n :row-count pane-w :pane-w grid :columns hints :hint-pairs} state]
  (binding [t/dialog-bg (if (:is-sideless region) (t/band-bg) t/dialog-bg)]
    (let
      [{:keys [sep-row title-row title-rule-row body-top foot-rule-row foot-row wipe-top visible
               top-limit]}
       (band-geometry region n)
       left (long left)
       inner-w (long inner-w)
       pane-w (long (if (pos? (long (or pane-w 0))) pane-w inner-w))
       clear-row! (fn [row]
                    (clear-rows! g region row row))]

      ;; A taller band before this one covered rows this one does not. They belong
      ;; to the HOST again: blanking them left a hole in the list behind the popup,
      ;; so the host's snapshot is painted back onto them instead.
      (when restore! (restore! top-limit (dec (long wipe-top))))
      (dotimes [i (max 1 (- (long body-top) (long wipe-top)))]
        (clear-row! (+ (long wipe-top) i)))
      (when (>= (long sep-row) (max (long wipe-top) (long top-limit)))
        (draw-rule! g region sep-row))
      (when (> (long title-rule-row) (long title-row)) (draw-rule! g region title-rule-row))
      (when (> (long foot-rule-row) (max (long sep-row) (long top-limit)))
        (draw-rule! g region foot-rule-row))
      (p/set-colors! g t/dialog-hint-key t/dialog-bg)
      (p/styled g [p/BOLD] (p/put-str! g (+ left 2) title-row (p/ellipsize (str title) text-w)))
      (dotimes [i visible]
        (let [row (+ (long body-top) (long i))]
          (clear-row! row)
          (dotimes [j (count panes)]
            (let
              [pane-left (+ left (* (long j) (+ pane-w (long pane-gap))))
               r (nth (nth panes j) i)]

              (when (pos? (long j))
                (p/set-colors! g t/border-fg t/dialog-bg)
                (p/set-char! g (- pane-left 2) row p/BOX_V))
              (case (:kind r)
                :header
                (do (p/set-colors! g t/dialog-hint t/dialog-bg)
                    (p/styled g
                              [p/BOLD]
                              (p/put-str! g
                                          (+ pane-left 2)
                                          row
                                          (p/ellipsize (str (:text r)) (max 1 (- pane-w 2))))))

                :blank
                nil

                :item
                (let
                  [{:keys [type id] :as it} (:item r)
                   {:keys [is-flag is-valued]} (get sp/item-types type)
                   active? (boolean (or (and is-valued (contains? (:options state) id))
                                        (and is-flag (contains? (:switches state) id))))
                   value (when is-valued (get (:options state) id))]

                  (draw-item! g pane-left row pane-w grid it active? value)))))))
      (hint-bar! g left foot-row inner-w hints))))

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
