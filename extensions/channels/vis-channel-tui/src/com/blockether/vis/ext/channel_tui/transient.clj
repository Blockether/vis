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
              `{:left :inner-w :hint-row :text-w :min-row :band}`.
              `:left`/`:inner-w` are the host frame's border column and inner
              width, `:hint-row` the row the host's hint bar owns (the popup
              REPLACES it), `:text-w` the text budget inside the padding.
              `:min-row` is the first row the popup may touch — a tall transient
              stops there instead of climbing over whatever the host keeps
              visible. A band wipes ITS OWN rows and nothing above them; a host
              that pages bands of DIFFERENT heights passes a `:band` atom, and
              each band then also erases what the PREVIOUS one left above it.

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
            [com.blockether.vis.ext.channel-tui.theme :as t]))

(set! *unchecked-math* :warn-on-boxed)

;;; ── Pure model ──────────────────────────────────────────────────────────────

(defn item-by-key
  "PURE: the spec item bound to single character `ch` (a Character or string),
   scanning every group in order. nil when nothing is bound."
  [spec ch]
  (let [k (str ch)]
    (some (fn [{:keys [items]}]
            (some #(when (= k (:key %)) %) items))
          (:groups spec))))

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
  (if-let [{:keys [type id] :as it} (item-by-key spec ch)]
    (case type
      :switch
      {:kind :continue
       :state (update state :switches #(if (contains? % id) (disj % id) (conj (or % #{}) id)))}

      :option
      {:kind :option :item it}

      :action
      {:kind :action :item it}

      {:kind :continue :state state})
    {:kind :continue :state state}))

(defn key-glyph
  "PURE: the key column glyph magit paints for one item. FLAGS (`:switch` /
   `:option`) carry magit's leading `-` — `-h`, `-t` — so a toggle can never be
   mistaken for a fire-once verb; COMMANDS (`:action`) show the bare key."
  [{:keys [type key]}]
  (if (= :action type) (str key) (str "-" key)))

(defn item-arg
  "PURE: the trailing git-argument cell magit shows for a FLAG: `(--no-verify)` for
   a switch, `(%topic=fix)` for an option carrying `value`. nil for actions (a
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

    (cond (= :action type) nil
          (and arg (= :option type)) (str "(" arg v ")")
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

(defn height
  "PURE: rows the popup needs INSIDE the host's frame — its opening separator,
   the title, the title's rule, and every display row. The closing rule and the
   hint bar are the host's OWN bottom chrome, so they are not counted; a host
   sizes its box with this before it paints one."
  ^long [spec]
  (+ 3 (count (rows spec))))

(defn geometry
  "PURE: which row of `region` every part of the popup lands on, for `spec`.

     `:sep-row`         the band's opening separator
     `:title-row`       the bold title, alone on its row
     `:title-rule-row`  the title's OWN rule, closing the title band
     `:body-top`        first group header
     `:visible`         display rows that actually fit (overflow is dropped)
     `:foot-rule-row`   the rule directly above the hint bar
     `:foot-row`        the hint bar (the host's own hint row)
     `:wipe-top`        first row the popup wipes before painting

   Everything is clamped to `:min-row`, so a tall transient — or a short
   terminal — stops at the host's content top instead of climbing over it."
  [{:keys [hint-row min-row]} spec]
  (let
    [n
     (count (rows spec))

     top-limit
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

;;; ── Paint ───────────────────────────────────────────────────────────────────

(def hint-pairs
  "The footer every transient shows: what a flag key does, what a command key
   does, and the way out."
  [["-key" "toggle flag"] ["key" "run command"] ["Esc" "cancel"]])

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

(defn clear-rows!
  "Blank rows `from`..`to` (INCLUSIVE) inside the host's frame: dialog paper
   across the inner columns and the frame's plain edge back in both border
   columns.

   Wiping only the band's INNER columns leaves whatever the host painted in the
   two border columns: a status buffer's own section separator survived as stray
   `├`/`┤` junctions beside the popup. Capping separators put their own junctions
   back afterwards.

   A transient erases exactly the rows it paints. A host that repaints BANDS OF
   DIFFERENT HEIGHTS into one rectangle — the model picker paging a catalog —
   owns the rows between its content and the band, and erases them with this
   before running the next page."
  [g {:keys [left inner-w]} from to]
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
        (p/fill-rect! g (inc left) row inner-w 1)
        (p/set-colors! g t/dialog-border t/dialog-bg)
        (p/set-char! g left row p/BOX_V)
        (p/set-char! g right row p/BOX_V)))))

(defn paint!
  "Paint ONE frame of `spec` at `state` into `region` on `host`. Pure geometry,
   one pass, no key handling — a host that owns its own event loop embeds a
   transient with this plus `toggle`; `run!` is the batteries-included loop."
  [{:keys [g hint-bar!]} {:keys [left inner-w text-w band] :as region} spec state]
  (let
    [{:keys [sep-row title-row title-rule-row body-top foot-rule-row foot-row wipe-top visible
             top-limit]}
     (geometry region spec)

     left
     (long left)

     inner-w
     (long inner-w)

     right
     (+ left inner-w 1)

     display-rows
     (rows spec)

     grid
     (columns spec)

     clear-row!
     (fn [row]
       (clear-rows! g region row row))

     ;; A band erases the rows it PAINTS, and above them ONLY what a PREVIOUS
     ;; band through this same region left behind — the model picker paging a
     ;; tall catalog into a short one. `:band` remembers that top; without it a
     ;; band reaches over nothing at all, so the host's own content — the
     ;; provider card, the settings list — stays visible behind the popup.
     wipe-from
     (max (long top-limit)
          (min (long wipe-top)
               (long (or (some-> band
                                 deref)
                         wipe-top))))]

    (dotimes [i (max 1 (- (long body-top) (long wipe-from)))]
      (clear-row! (+ (long wipe-from) i)))
    (p/set-colors! g t/dialog-border t/dialog-bg)
    (when (>= (long sep-row) (max (long wipe-top) (long top-limit)))
      (p/draw-separator! g left right sep-row))
    (when (> (long title-rule-row) (long title-row))
      (p/draw-separator! g left right title-rule-row))
    (when (> (long foot-rule-row) (max (long sep-row) (long top-limit)))
      (p/draw-separator! g left right foot-rule-row))
    (p/set-colors! g t/dialog-hint-key t/dialog-bg)
    (p/styled g
              [p/BOLD]
              (p/put-str! g (+ left 2) title-row (p/ellipsize (str (:title spec)) text-w)))
    (dotimes [i visible]
      (let
        [r (nth display-rows i)
         row (+ (long body-top) (long i))]

        (clear-row! row)
        (case (:kind r)
          :header
          (do (p/set-colors! g t/dialog-hint t/dialog-bg)
              (p/styled g
                        [p/BOLD]
                        (p/put-str! g (+ left 2) row (p/ellipsize (str (:text r)) text-w))))

          :blank
          nil

          :item
          (let
            [{:keys [type id] :as it} (:item r)
             active? (case type
                       :switch
                       (contains? (:switches state) id)

                       :option
                       (contains? (:options state) id)

                       false)
             value (when (= type :option) (get (:options state) id))]

            (draw-item! g left row inner-w grid it active? value)))))
    (hint-bar! g left foot-row inner-w hint-pairs)
    (some-> band
            (reset! wipe-top))))

;;; ── Run ─────────────────────────────────────────────────────────────────────

(defn run!
  "Paint `spec` into `region` on `host` and run its key loop until an ACTION
   fires or the user cancels. Returns `{:action id :switches #{…} :options {…}}`,
   or nil on Esc. Flags toggle in place and keep the popup open; an OPTION calls
   the spec's `:read-option` for a value."
  [{:keys [read-key! refresh!] :as host} region spec]
  (let [read-option (or (:read-option spec) (constantly nil))]
    (loop [state {:switches #{} :options {}}]
      (paint! host region spec state)
      (refresh!)
      (let [k (read-key!)]
        (cond (= :esc k) nil
              (nil? k) (recur state)
              :else (let [r (toggle spec state k)]
                      (case (:kind r)
                        :continue
                        (recur (:state r))

                        :option
                        (let
                          [{:keys [id] :as it} (:item r)
                           v (read-option it (get (:options state) id))]

                          (recur (if (nil? v) state (assoc-in state [:options id] v))))

                        :action
                        {:action (:id (:item r))
                         :switches (:switches state)
                         :options (:options state)})))))))
