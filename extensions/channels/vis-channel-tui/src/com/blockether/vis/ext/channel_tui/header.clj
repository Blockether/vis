(ns com.blockether.vis.ext.channel-tui.header
  "Dedicated header band painted above the messages area.

   Three-region layout:

       [LEFT]                    [CENTER]                    [RIGHT]
       ✓ Copied!                 Session title          d8d6a0a1
       (notification/status)     (or fallback placeholder)   (id target)

   - LEFT: latest active host notification (`com.blockether.vis.core/notify!`),
     otherwise live channel status. The session title does NOT live here.
   - CENTER: session title from app-db (`:title`). When the
     session has no title yet, falls back to a placeholder so
     the row never looks broken on a fresh run.
   - RIGHT: short session id (first 8 chars of the UUID) as the clickable
     affordance that drops the FULL UUID onto the system clipboard. No
     notifications or channel statuses render here.

   Pure draw: reads `:title` and `:session` from app-db, the
   active notifications list from `vis.core/notifications`, writes
   cells, registers ONE click region for the copy affordance.

   Repaint: the banner updates as notifications come and go.
   `screen.clj` registers a watcher on screen mount that bumps the
   render version for any change, so a `(notify! ...)` from anywhere
   nudges this band to repaint immediately."
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.components :as components]
            [com.blockether.vis.ext.channel-tui.keymap :as keymap]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.render-ir :as ir-tui]
            [com.blockether.vis.ext.channel-tui.theme :as t]
            ;; Channel-agnostic header policy (slot ratios, workspace
            ;; switcher padding/cap, glyphs, default labels). Lives in
            ;; `internal/header.cljc` so a future web/Telegram channel
            ;; reuses the same values without touching TUI code.
            [com.blockether.vis.internal.header :as vh]))

(set! *unchecked-math* :warn-on-boxed)

;; -- Channel-contribution conventions consumed by this namespace --------------
;;
;; Extensions contribute to the header band by adding entries to their
;; `:ext/channel-contributions` map. The TUI consumes the following slot:
;;
;;   {:tui.slot/header-row
;;    [{:id :my.extension/header-row
;;      :fn (fn [db cols] -> ir | nil)}]}
;;
;; The fn returns CANONICAL IR (`[:ir {?:align ?:fg-role ...} &
;; blocks]`) — channel-agnostic data, NOT a paint thunk. The TUI
;; walks the IR via `ir-tui/ir->lines` and paints the resulting
;; styled lines into rows; other channels (Telegram, web) translate
;; the same IR to their own surface (markdown, HTML, etc.).
;;
;; Optional render hints carried on the IR root attrs map:
;;   :align    one of #{:left :center :right} — default :center
;;   :fg-role  one of #{:default :muted :warn :error :success}
;;             — maps to the channel's palette; default :default
;;
;; Channels are free to ignore unknown hint keys.
;;
;; Same declarative pattern as `:goal/slash` (slash commands) and
;; `:voice/foo` (voice indicator). No `requiring-resolve` into
;; channel-tui from extension code; channel-tui never imports specific
;; extension namespaces. Symmetric with how other channels would
;; expose their own hook conventions (e.g. `:telegram/preamble`).
;;
;; See `com.blockether.vis.internal.extension/channel-contributions-for`.

(def ^:const header-rows-base
  "Minimum rows reserved by the header band: top rule + content row +
   bottom rule. `header-rows` adds the height contributed by registered
   `:tui.slot/header-row` extensions on top of this base."
  3)

(defn- title-or-placeholder
  "Visible title for the active session. Delegates to the shared
   helper so every channel reuses the same placeholder text."
  [db]
  (vh/title-or-placeholder (:title db)))

(def ^:private active-workspace-states
  "Header strip shows live workspaces only. Merged + discarded rows
   stay in DB for transcript references but never appear in any list,
   panel, or overlay."
  #{:active :merging})

(defn- tab-strip-visible?
  "True for entries that should appear in the header strip. Entries
   without an attached :workspace record (synthetic fallback) are
   always visible. Entries with a workspace record are visible only
   when its state is :active or :merging."
  [entry]
  (let [state (some-> entry
                      :workspace
                      :state)]
    (or (nil? state) (contains? active-workspace-states state))))

(defn- tab-entries
  "Return entries to render in the centre strip, ALWAYS non-empty.

   Each entry represents a workspace (1:1 with its session); the
   active entry's label tracks the session title (the state layer
   updates it on `:set-title`). Finished (merged + discarded)
   workspaces never reach the strip. When the app-db has not yet
   initialised its workspace list — fresh boot, first paint, or
   stand-alone draw in tests — we synthesise a single active entry
   labelled with the session title (or the `Untitled session`
   placeholder) so the centre slot is never empty."
  [db]
  (let [entries
        (filterv tab-strip-visible? (:tabs db))

        active-id
        (or (:active-tab-id db) (:id (some #(when (:active? %) %) entries)) (:id (first entries)))

        ;; A tab is "running" when its session has a turn in flight. The
        ;; active tab's run-state lives at the db root; every other tab's
        ;; lives frozen in `:tab-locals` (its streaming worker keeps
        ;; updating it there). This is what surfaces concurrent turns.
        running?
        (fn [id]
          (boolean (if (= id active-id) (:loading? db) (get-in db [:tab-locals id :loading?]))))

        ;; Auto-title generation runs for the ACTIVE session (the host
        ;; fires it at the start of that session's turn), so the spinner
        ;; only ever attaches to the active tab.
        title-loading?
        (fn [id]
          (and (= id active-id) (boolean (:title-loading? db))))]

    (if (seq entries)
      (mapv #(assoc %
               :running? (running? (:id %))
               :title-loading? (title-loading? (:id %)))
            entries)
      [{:id (or (:active-tab-id db) :main)
        :label (title-or-placeholder db)
        :active? true
        :running? (boolean (:loading? db))
        :title-loading? (boolean (:title-loading? db))}])))

(defn- active-tab-entry-id
  [db entries]
  (or (:active-tab-id db) (:id (some #(when (:active? %) %) entries)) (:id (first entries))))

(defn- contributor-disabled?
  [db contribution-id]
  (let [disabled (get-in db [:settings :contributors-disabled])]
    (and (set? disabled) (contains? disabled contribution-id))))

(defn- ir-root?
  "Lightweight check: shape returned by contribution fn is canonical IR.
   Does NOT validate inner blocks; just confirms the envelope so
   we know to walk it. Bad inner shape will be caught by ir-tui."
  [x]
  (and (vector? x) (= :ir (first x)) (>= (count x) 1)))

(defn- fg-role->color
  "Map an extension-declared `:fg-role` keyword to a TUI theme color.
   Channels are free to interpret the same role differently; the role
   is the channel-agnostic intent (`:warn` = warning, `:muted` = de-
   emphasised). Unknown roles fall back to `:default` (`footer-fg`)."
  [role]
  (case role
    :muted
    t/footer-fg-muted

    :warn
    t/footer-warning-fg

    :error
    t/footer-error-fg

    :success
    t/footer-fg-strong

    t/footer-fg))

(defn- align-x
  "Compute the starting column for a line of `line-w` cells inside a
   `cols`-wide band, given the alignment hint. `:center` is the
   default; `:left` pins to `edge-pad`; `:right` pins to the right."
  ^long [align ^long line-w ^long cols]
  (let [edge-pad 1]
    (case align
      :left
      (long edge-pad)

      :right
      (long (max edge-pad (- cols edge-pad line-w)))

      ;; default = :center
      (long (max edge-pad (quot (- cols line-w) 2))))))

(defn- ir->header-row-spec
  "Convert canonical IR (returned by a header-row hook) into the
   internal row spec `{:height :draw!}` used by `draw-header!`.

   - Walks the IR via `ir-tui/ir->lines` to get styled lines.
   - Reads optional `:align` and `:fg-role` from the IR root attrs.
   - Returns nil when the IR walks to zero lines.

   The :draw! closure paints each line via `paint-styled-line!`,
   honouring inline sentinels (bold/italic/code/strike) emitted by
   the IR walker."
  [ir ^long cols]
  (let [attrs
        (when (and (>= (count ir) 2) (map? (nth ir 1))) (nth ir 1))

        align
        (or (:align attrs) :center)

        fg-role
        (or (:fg-role attrs) :default)

        ;; Subtract a 1-col edge pad on each side; tighter wrap than
        ;; the cols arg the painter receives.
        wrap-w
        (max 1 (- cols 2))

        ;; ir-tui returns vec of `{:runs [{:text :style}...]}`. We
        ;; convert to sentinel-prefixed plain strings via the
        ;; existing adapter so paint-styled-line! can render bold /
        ;; italic / code spans the same way every other styled line
        ;; in the TUI does.
        lines
        (ir-tui/ir->lines ir wrap-w)

        line-strs
        (when (seq lines) (ir-tui/lines->sentinel-strings lines))]

    (when (seq line-strs)
      (let [fg (fg-role->color fg-role)]
        {:height (count line-strs)
         :draw!
         (fn [^com.googlecode.lanterna.graphics.TextGraphics g ^long row-top]
           (loop [i 0
                  lines (seq line-strs)]

             (when lines
               (let [^String line (first lines)
                     visible-w (p/display-width line)
                     x (align-x align visible-w cols)
                     y (+ row-top (long i))]

                 (p/clear-styles! g)
                 (p/set-colors! g fg t/terminal-bg)
                 ;; italic by default for header subtitle rows -
                 ;; that's the established visual style for this
                 ;; band; extensions can override by emitting their
                 ;; own ITALIC sentinels via [:em ...] inside the IR.
                 (p/enable! g p/ITALIC)
                 (p/paint-styled-line! g x y line fg t/terminal-bg t/code-block-fg t/code-block-bg)
                 (p/clear-styles! g)
                 (recur (inc i) (next lines))))))}))))

(defn- header-row-specs
  "Query every extension's `:tui.slot/header-row` contribution, run its
   `:fn`, collect the resulting row specs.

   Slot contract: `:fn` returns canonical IR (or nil to skip this frame).
   The IR is walked + converted to a row spec here.

   Settings can disable a contribution by id via `:contributors-disabled`.
   Contribution crashes never propagate — misbehaving extensions just lose
   their row that frame."
  [db cols]
  (vec (for [{:keys [id] f :fn}
             (vis/channel-contributions-for :tui :tui.slot/header-row)

             :when (and (ifn? f) (not (contributor-disabled? db id)))
             :let [ir
                   (try (f db cols) (catch Throwable _ nil))

                   spec
                   (when (ir-root? ir) (try (ir->header-row-spec ir cols) (catch Throwable _ nil)))]
             :when (and spec (pos? (long (or (:height spec) 0))))]

         {:id id :spec spec})))

(defn header-rows
  "Rows needed by the header for this app-db. Workspace switcher lives inside
   the base content row; each enabled extension that declares a
   `:tui.slot/header-row` channel contribution adds its fn's reported height.

   Note: contribution fns are invoked here AND in `draw-header!` (the
   render path needs both the total height and the draw-fn).
   Extensions should keep their fn cheap or memoise
   per-frame internally — the goal extension uses a 100ms TTL atom
   for its SQLite lookup."
  ([db]
   ;; Arity without cols: pass a sentinel. Most hooks key their
   ;; height on db state only.
   (header-rows db 0))
  ([db cols]
   (+ (long header-rows-base)
      (long (reduce + 0 (map #(long (:height (:spec %))) (header-row-specs db cols)))))))

(defn- short-id
  "Project a session's UUID onto the shared short-form length."
  [session]
  (vh/short-id (:id session)))

(defn- full-id
  [session]
  (some-> session
          :id
          str))

(defn- ellipsize [text max-cols] (p/ellipsize text max-cols))

(defn- latest-notification
  "Most-recently-pushed active notification, or nil. We display ONE
   at a time in the header - the LEFT slot is a single row. If
   multiple are active simultaneously, the freshest wins; older ones
   stay in the queue and surface as the freshest one expires."
  []
  (last (vis/notifications)))

(defn- status-expired?
  [status now-ms]
  (when-let [until (:until status)]
    (<= (long until) (long now-ms))))

(defn- latest-channel-status
  [{:keys [channel-status]}]
  (let [now-ms (System/currentTimeMillis)]
    (->> (vals channel-status)
         (filter #(seq (:text %)))
         (remove #(= :ready (:phase %)))
         (remove #(status-expired? % now-ms))
         (sort-by #(long (or (:updated-at-ms %) 0)))
         last)))

;; `level->fg` (notification color) + the band rule, left notification slot,
;; and id-copy badge now live in `components` (band-rule!, notification-slot!,
;; id-badge!).

(defn- id-copy-block-text
  "Space-padded chip label for the id copy button, same shape as the F1/F2
   buttons so `id-badge!` can paint it through the shared `button!` and it
   reads as a real button: the `#id` is the affordance text."
  [id-short]
  (if id-short (str " #" id-short " ") ""))

(def ^:dynamic *register-click-regions?*
  "Bind false for header-only hover repaints. Geometry did not change,
   so the previous full frame's published click regions remain valid and
   the repaint must not mutate the staged click-region buffer."
  true)

(defn- right-block-text
  "Compose the right-side text: \" #4b1ed602 \" when a session id exists,
   otherwise empty. Single place that knows the layout so `draw-header!`
   can stay focused on placement math."
  [id-short]
  (id-copy-block-text id-short))

(defn- clamp-long [n lo hi] (max (long lo) (min (long hi) (long n))))

(defn- active-strip-index
  [entries active-id]
  (or (first (keep-indexed #(when (= (:id %2) active-id) %1) entries)) 0))

(defn- visible-tab-window
  [entries active-id width]
  (let [entries
        (vec entries)

        n
        (count entries)

        width
        (max 0 (long width))

        max-visible
        (long (vh/max-visible-workspace-count n width))

        overflow?
        (> n max-visible)

        active-idx
        (long (active-strip-index entries active-id))

        half
        (quot max-visible 2)

        start
        (long (if overflow? (clamp-long (- active-idx half) 0 (max 0 (- n max-visible))) 0))

        end
        (min n (+ start max-visible))]

    {:overflow? overflow?
     :start start
     :entries (mapv (fn [idx entry]
                      (assoc entry :header/original-index idx))
                    (range start end)
                    (subvec entries start end))}))

;; `truncate-with-ellipsis` + `center-padded` now live in `components` (the
;; tab cell that consumes them does too).

;; The overflow nav arrow + inert center title are now `components/nav-arrow!`
;; and `components/title!`.

(defn- draw-center-workspaces!
  "Paint the visible workspace switcher window inside the center 60% slot.

   Workspaces are painted directly here because the header needs a fixed
   `vh/tab-entry-padding`-cell inner margin and an ellipsis on overflow.
   Each cell still occupies its full width on screen — fill-rect paints the
   active/inactive background — but the label itself is centred within the
   inner area `(cell-w - 2*padding)`."
  [g entries active-id row left width]
  (let [plus-label
        " + "

        ;; minimal accent ＋ chip — the footer owns the `C-x n` hint
        plus-w
        (p/display-width plus-label)

        plus-gap
        1

        ;; `+` at the FAR LEFT of the tab strip — opens a new session (same as
        ;; Ctrl+N). Draw it first, then reserve its width + a 1-col gap and shift
        ;; the tabs right so it reads as part of the centre tab group, sitting
        ;; just ahead of tab 1.
        _
        (components/button! g
                            left
                            row
                            plus-label
                            :header-new-session
                            {:accent? true :register? *register-click-regions?*})

        left
        (+ (long left) plus-w plus-gap)

        width
        (max 0 (- (long width) plus-w plus-gap))

        multi?
        (> (count entries) 1)

        {:keys [overflow? entries]}
        (visible-tab-window entries active-id width)

        arrow-w
        3

        arrow-gap
        1

        entries-left
        (if overflow? (+ left arrow-w arrow-gap) left)

        entries-width
        (max 0 (- width (if overflow? (* 2 (+ arrow-w arrow-gap)) 0)))

        n
        (count entries)]

    (when overflow?
      (components/nav-arrow! g row left vh/workspace-arrow-left :prev *register-click-regions?*)
      (components/nav-arrow! g
                             row
                             (+ left width (- arrow-w))
                             vh/workspace-arrow-right
                             :next
                             *register-click-regions?*))
    (when (and (pos? n) (pos? entries-width))
      ;; Reserve a 1-col `│` divider between each adjacent pair of tabs, then
      ;; share the rest of the width across the tabs.
      (let [divider-w
            (max 0 (dec n))

            tab-total
            (max 0 (- entries-width divider-w))

            base
            (quot tab-total n)

            extra
            (rem tab-total n)

            ;; Lay out each tab cell (the loop advances an extra col past
            ;; each tab for its trailing divider), then hand the drawing to
            ;; `components/tab-cell!` (slab + centered label + hover-✕ close
            ;; button + click regions). `:status` drives each cell's underline
            ;; border: `:running` while a turn is in-flight or this active tab's
            ;; title is generating (the border blinks), `:ready` for a finished,
            ;; unread background tab (steady green glow), nil otherwise.
            cells
            (loop [idx
                   0

                   x
                   entries-left

                   out
                   []]

              (if (= idx n)
                out
                (let [cell-w
                      (+ base (if (< idx extra) 1 0))

                      entry
                      (nth entries idx)

                      active?
                      (= (:id entry) active-id)

                      tab-no
                      (inc (long (:header/original-index entry)))

                      status
                      (cond
                        ;; The tab you're already looking at gets
                        ;; NO cue — the live work is right there in
                        ;; the view, so the dots would be noise.
                        active? nil
                        (and (:running? entry) (not (:title-loading? entry))) :running
                        (and (not (:running? entry)) (:unread? entry) (not (:title-loading? entry)))
                        :ready
                        (:title-loading? entry) :running
                        :else nil)

                      label
                      (p/tab-display-label entry)

                      ;; TRUE when the NEXT visible tab belongs to a different
                      ;; PROJECT (`vh/tab-group-root`) — the divider after this
                      ;; cell then paints as the solid group separator instead
                      ;; of the soft dotted in-group one.
                      group-end?
                      (and (< idx (dec n))
                           (not= (vh/tab-group-root entry)
                                 (vh/tab-group-root (nth entries (inc idx)))))]

                  (recur (inc idx)
                         (+ x cell-w (if (< idx (dec n)) 1 0))
                         (conj out
                               (assoc entry
                                 :left x
                                 :width cell-w
                                 :label label
                                 :status status
                                 :tab-no tab-no
                                 :active? active?
                                 :last? (= idx (dec n))
                                 :group-end? group-end?))))))]

        (doseq [{:keys [left width active? label status id last? tab-no group-end?]
                 idx :header/original-index}
                cells

                :when (pos? (long width))]

          (components/tab-cell! g
                                {:left left
                                 :row row
                                 :width width
                                 :label label
                                 :status status
                                 :tab-no tab-no
                                 :active? active?
                                 :workspace-id id
                                 :index idx
                                 :register? *register-click-regions?*
                                 :closable? multi?})
          ;; Divider after every tab but the last: dotted `┊` inside a project
          ;; group, solid `│` where the NEXT tab starts a different project.
          (when-not last?
            (let [divider-col (+ (long left) (long width))]
              (if group-end?
                (components/tab-group-divider! g row divider-col)
                (components/tab-divider! g row divider-col)))))
        cells))))

(defn draw-header!
  "Paint the header band starting at `header-top`, full width `cols`.
   Main content row is a fixed-width 3-slot flex (see `vh/slot-layout`):

   - LEFT (static `vh/left-slot-cols`): latest notification, otherwise live channel status.
   - CENTER (rest, minus a `vh/slot-gap-cols` gap each side): workspace title or switcher. With one workspace,
     paint inert title text. With multiple workspaces, paint switchable
     workspace entries. When app-db has not yet materialised a workspace list,
     `tab-entries` synthesises one placeholder workspace so a fresh
     session reads as `Untitled session` in the centre.
   - RIGHT (static `vh/right-slot-cols`): stable session-id copy affordance only.

   Workspaces are part of the header row (no separate band). Overflow shows
   clickable left/right arrows that cycle through workspaces."
  [g db header-top cols]
  (let [workspaces
        (tab-entries db)

        header-top
        (long header-top)

        cols
        (long cols)

        top-rule-row
        header-top

        content-row
        (inc header-top)

        contrib-specs
        (header-row-specs db cols)

        bottom-row
        (dec (+ header-top (long (header-rows db cols))))

        edge-pad
        1

        {:keys [left-x left-w center-x center-w right-x]}
        (vh/slot-layout cols)

        left-x
        (long left-x)

        left-w
        (long left-w)

        center-x
        (long center-x)

        center-w
        (long center-w)

        right-x
        (long right-x)

        id-short
        (short-id (:session db))

        full-uuid
        (full-id (:session db))

        id-copy-text
        (id-copy-block-text id-short)

        action-text
        (right-block-text id-short)

        banner
        (latest-notification)

        status
        (latest-channel-status db)

        left-message
        (or banner status)

        left-raw
        (some-> left-message
                :text)

        left-level
        (some-> left-message
                :level)

        left-cap
        (max 0 (- left-w edge-pad 1))

        left-text
        (when (seq left-raw) (ellipsize left-raw left-cap))

        action-w
        (p/display-width action-text)

        right-w
        action-w

        right-col
        (long (max right-x (- cols edge-pad right-w)))

        action-col
        right-col

        active-id
        (active-tab-entry-id db workspaces)

        ;; RIGHT cluster geometry (help/search chips + id badge) is
        ;; right-aligned by ABSOLUTE `cols` math, independent of the centre
        ;; slot. On narrow/medium terminals its real width exceeds the
        ;; layout's `right-w` estimate, so `slot-layout` alone would let the
        ;; chips paint ON TOP of the tab strip. Compute the cluster's true
        ;; left edge here and clamp the centre width to stop short of it.
        chips
        [[:header-help (str " help (" (keymap/label-for :toggle-help) ") ")]
         ;; search chip hidden for now — the C-x f binding still works, just no button.
         #_[:header-search (str " search (" (keymap/label-for :search-open) ") ")]]

        chip-gap
        1

        cluster-w
        (long (+ (long (reduce + (map (comp long p/display-width second) chips)))
                 (* chip-gap (count chips))))

        cluster-start
        (long (max edge-pad (- action-col cluster-w)))

        center-limit
        (- cluster-start (long vh/slot-gap-cols))

        center-w
        (max 0 (min center-w (- center-limit center-x)))]

    (components/band-rule! g top-rule-row cols)
    (p/clear-styles! g)
    (p/set-colors! g t/footer-fg t/terminal-bg)
    (p/fill-rect! g 0 content-row cols 1)
    ;; LEFT 20%: latest notification, otherwise channel status. No title here.
    ;; (Draft status lives in the footer — one indicator, not two.)
    (components/notification-slot! g (+ left-x edge-pad) content-row left-text left-level)
    ;; CENTER 60%: the workspace tab strip. Even a single session renders as a
    ;; real tab, so there's one consistent affordance — no special inert-title
    ;; path. The ✕ close button is suppressed when it's the ONLY session, since
    ;; the last tab can't be closed.
    (draw-center-workspaces! g workspaces active-id content-row center-x center-w)
    ;; RIGHT 20%: session-id copy button.
    (components/id-badge! g action-col content-row id-copy-text full-uuid *register-click-regions?*)
    ;; RIGHT slot: help/search as real BUTTONS — filled chips via the shared
    ;; `button!` (visible inverted-chip bg, accent on hover), right-aligned as a
    ;; cluster just left of the id badge. No `|` separators; the bg IS the
    ;; affordance. Each chip shows its Emacs chord inline (`C-x h` / `C-x f`)
    ;; so the binding is discoverable right on the button; C-x C-p opens the full
    ;; searchable palette.
    (reduce (fn [x [kind label]]
              (let [x (long x)]
                (+ x
                   chip-gap
                   #_{:clj-kondo/ignore [:redundant-primitive-coercion]}
                   (long (components/button! g
                                             x
                                             content-row
                                             label
                                             kind
                                             {:register? *register-click-regions?*})))))
            cluster-start
            chips)
    ;; Extension-contributed rows.
    (loop [row
           (inc content-row)

           specs
           (seq contrib-specs)]

      (when specs
        (let [{:keys [spec]}
              (first specs)

              h
              (long (:height spec))

              draw!
              (:draw! spec)]

          (when (pos? h)
            (p/clear-styles! g)
            (p/set-colors! g t/footer-fg t/terminal-bg)
            (p/fill-rect! g 0 row cols h)
            (when (ifn? draw!) (try (draw! g (long row)) (catch Throwable _ nil))))
          (recur (+ row h) (next specs)))))
    (components/band-rule! g bottom-row cols)
    (p/clear-styles! g)
    (p/set-colors! g t/footer-fg t/terminal-bg)))
