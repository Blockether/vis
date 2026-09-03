(ns com.blockether.vis.tui.header
  "Dedicated header band painted above the messages area.

   Three-region layout:

       [LEFT]                    [CENTER]                    [RIGHT]
       ✓ Copied!                 Session title          d8d6a0a1
       (notification/status)     (or fallback placeholder)   (id target)

   - LEFT: latest active host notification (`com.blockether.vis.tui.client/notify!`),
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
  (:require [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.components :as components]
            [com.blockether.vis.tui.keymap :as keymap]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.theme :as t]
            [com.blockether.vis.tui.header-model :as vh])
  (:import [com.googlecode.lanterna TerminalPosition TerminalSize]
           [com.googlecode.lanterna.graphics TextGraphics]
           [com.googlecode.lanterna.gui2 Button Button$ButtonRenderer GridLayout Panel
            TextGUIGraphics]))

(set! *unchecked-math* :warn-on-boxed)

(def ^:const header-rows-base
  "Rows reserved by the header: top rule, content row, and bottom rule."
  3)

(defn- header-action-chips
  []
  [[:header-help (str " help (" (keymap/label-for :toggle-help) ") ")]
   ;; Search stays available through C-x f but does not occupy the header yet.
   #_[:header-search (str " search (" (keymap/label-for :search-open) ") ")]])

(defn header-actions-component
  "Build the real interactive GUI2 grid used by the header action cluster. With
   no arguments it is a portable component for `HtmlTerminalView`; `on-action`
   receives the action kind when its button is activated. The full-screen form
   also bridges absolute Vis click regions without changing the component tree."
  ([] (header-actions-component nil false nil))
  ([on-action] (header-actions-component nil false on-action))
  ([root-graphics register?] (header-actions-component root-graphics register? nil))
  ([root-graphics register? on-action]
   (let [chips
         (header-action-chips)

         gap
         1

         layout
         (doto (GridLayout. (max 1 (count chips)))
           (.setLeftMarginSize 0)
           (.setRightMarginSize gap)
           (.setHorizontalSpacing gap))

         panel
         (Panel. layout)]

     (.setFillColorOverride panel t/terminal-bg)
     (doseq [[kind label] chips]
       (let [button (Button. label
                             ^Runnable
                             (reify
                               Runnable
                                 (run [_] (when on-action (on-action kind)))))]
         (.setRenderer
           button
           (reify
             Button$ButtonRenderer
               (getCursorLocation [_ _] nil)
               (getPreferredSize [_ _] (TerminalSize. (int (p/display-width label)) 1))
               (drawComponent [_ local-graphics component]
                 (if root-graphics
                   (let [child-position (.getPosition ^Button component)
                         panel-position (.getPosition panel)]

                     (components/button! root-graphics
                                         (+ (.getColumn panel-position) (.getColumn child-position))
                                         (+ (.getRow panel-position) (.getRow child-position))
                                         label
                                         kind
                                         {:register? register?}))
                   (components/button! local-graphics 0 0 label kind {:register? false})))))
         (.addComponent panel button)))
     panel)))

(defn- draw-header-actions!
  [^TextGraphics g ^Panel panel col row]
  (let [size
        (.getPreferredSize panel)

        position
        (TerminalPosition. (int col) (int row))

        children
        (.getChildrenList panel)

        graphics
        (TextGUIGraphics/from g)]

    (.setPosition panel position)
    (.setSize panel size)
    (.doLayout (.getLayoutManager panel) size children)
    (doseq [^Button child children]
      (.drawComponent ^Button$ButtonRenderer (.getRenderer child) graphics child))
    (.getColumns size)))

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

        ;; A tab is PARKED when its session raised a human-input request nobody has
        ;; answered yet. Same split as `running?`: the active tab's open form lives
        ;; at the db root, every other tab's in its `:tab-locals` half (the close
        ;; handler settles it in whichever tab holds it), so a background run
        ;; blocked on the operator is visible without leaving the tab you are on.
        awaiting-input?
        (fn [id]
          (boolean
            (if (= id active-id) (:human-input db) (get-in db [:tab-locals id :human-input]))))

        ;; Auto-title generation runs for the ACTIVE session (the host
        ;; fires it at the start of that session's turn), so the spinner
        ;; only ever attaches to the active tab.
        title-loading?
        (fn [id]
          (and (= id active-id) (boolean (:title-loading? db))))]

    (if (seq entries)
      (mapv #(assoc %
               :running? (running? (:id %))
               :awaiting-input? (awaiting-input? (:id %))
               :title-loading? (title-loading? (:id %)))
            entries)
      [{:id (or (:active-tab-id db) :main)
        :label (title-or-placeholder db)
        :active? true
        :running? (boolean (:loading? db))
        :awaiting-input? (boolean (:human-input db))
        :title-loading? (boolean (:title-loading? db))}])))

(defn- active-tab-entry-id
  [db entries]
  (or (:active-tab-id db) (:id (some #(when (:active? %) %) entries)) (:id (first entries))))


(defn header-rows
  "Rows needed by the fixed standalone-app header."
  ([_db] header-rows-base)
  ([_db _cols] header-rows-base))

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
        (if overflow? (p/clamp (- active-idx half) 0 (long (max 0 (- n max-visible)))) 0)

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

(def ^:private ^:const min-tab-cell-width
  "Smallest cell that keeps a title column beside the three-cell close button."
  (+ (long components/close-button-width) 3))

(defn- even-widths
  "Split `total` cells across `n` entries, assigning remainder left-to-right."
  [n total]
  (let [n
        (long n)

        total
        (max 0 (long total))]

    (if (zero? n)
      []
      (let [base
            (quot total n)

            extra
            (rem total n)]

        (mapv #(long (+ base (if (< (long %) extra) 1 0))) (range n))))))

(defn- tab-natural-width
  "Columns one tab needs for its number, full label, padding, and close button."
  [entry multi?]
  (let [tab-no
        (inc (long (:header/original-index entry)))

        display
        (str tab-no " | " (p/tab-display-label entry))]

    (+ (long (p/display-width display))
       (* 2 (long vh/tab-entry-padding))
       (if multi? (long components/close-button-width) 0))))

(defn- adaptive-tab-widths
  "Allocate all `total` cells without making short labels starve long neighbours.

   Narrow strips split evenly until every visible tab reaches the operable minimum.
   From there, cells grow round-robin only toward each tab's natural content width;
   once every label fits, any remaining room is shared evenly. The result depends on
   labels and strip width only, never selection state."
  [entries total multi?]
  (let [entries
        (vec entries)

        n
        (count entries)

        total
        (max 0 (long total))

        minimum-total
        (* (long n) (long min-tab-cell-width))]

    (cond (zero? n) []
          (< total minimum-total) (even-widths n total)
          :else
          (let [targets (mapv #(max (long min-tab-cell-width) (long (tab-natural-width % multi?)))
                              entries)]
            (loop [widths (vec (repeat n (long min-tab-cell-width)))
                   remaining (- total minimum-total)]

              (if (zero? remaining)
                widths
                (let [growable (keep-indexed (fn [idx width]
                                               (when (< (long width) (long (nth targets idx))) idx))
                                             widths)]
                  (if (seq growable)
                    (let [granted (take remaining growable)
                          grant-n (count granted)]

                      (recur (reduce #(update %1 %2 inc) widths granted) (- remaining grant-n)))
                    (mapv + widths (even-widths n remaining))))))))))

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
      ;; allocate the rest by each label's natural width. Short labels stop
      ;; absorbing cells while a longer neighbour is still truncated.
      (let [divider-w
            (max 0 (dec n))

            tab-total
            (max 0 (- entries-width divider-w))

            cell-widths
            (adaptive-tab-widths entries tab-total multi?)

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
                      (long (nth cell-widths idx))

                      entry
                      (nth entries idx)

                      active?
                      (= (:id entry) active-id)

                      tab-no
                      (inc (long (:header/original-index entry)))

                      status
                      (cond
                        ;; A run BLOCKED on the operator outranks every other cue,
                        ;; including the "you are already looking at it" rule: it is
                        ;; the one state that cannot end without the user, so the
                        ;; strip keeps demanding until the form is answered.
                        (:awaiting-input? entry) :input
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

        ;; RIGHT cluster geometry is measured by the same real GUI2 GridLayout
        ;; that positions and paints the action components. Its right margin is
        ;; the one-cell separation from the id badge.
        actions-component
        (header-actions-component g *register-click-regions?*)

        cluster-w
        (long (.getColumns (.getPreferredSize ^Panel actions-component)))

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
    ;; The action chips are real GUI2 components in a GridLayout. The same tree
    ;; renders as a standalone HtmlTerminalView and inside this full application;
    ;; only the absolute hit-region bridge is specific to the immediate-mode host.
    (draw-header-actions! g actions-component cluster-start content-row)
    (components/band-rule! g bottom-row cols)
    (p/clear-styles! g)
    (p/set-colors! g t/footer-fg t/terminal-bg)))
