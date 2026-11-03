(ns com.blockether.vis.tui.header
  "Dedicated header band painted above the messages area.

   Two-region layout:

       [LEFT]                                              [RIGHT]
       Session title   ● Recording                         d8d6a0a1
       (or fallback placeholder, then channel status)      (id target)

   - LEFT: session title from app-db (`:title`) at the left edge. When the
     session has no title yet, it falls back to a placeholder so the row never
     looks broken on a fresh run. The live channel status (voice,
     transcription) follows the title. Notifications show as toasts in the
     session view (`toasts`), never here.
   - RIGHT: short session id (first 8 chars of the UUID) as the clickable
     affordance that drops the FULL UUID onto the system clipboard. No
     notifications or channel statuses render here.

   Pure draw: reads `:title`, `:session` and `:channel-status` from app-db,
   writes cells, registers ONE click region for the copy affordance."
  (:require [com.blockether.vis.tui.agent-team :as agent-team]
            [com.blockether.vis.tui.components :as components]
            [com.blockether.vis.tui.interactions :as interactions]
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
  ([compact?] (header-action-chips compact? nil))
  ([compact? _apps]
   (if compact? [] [[:header-help (str " help (" (keymap/label-for :toggle-help) ") ")]])))

(defn header-actions-component
  "Build the real interactive GUI2 grid used by the header action cluster. With
   no arguments it is a portable component for `HtmlTerminalView`; `on-action`
   receives the action kind when its button is activated. The full-screen form
   also bridges absolute Vis click regions without changing the component tree."
  ([] (header-actions-component nil false nil))
  ([on-action] (header-actions-component nil false on-action))
  ([root-graphics register?] (header-actions-component root-graphics register? nil))
  ([root-graphics register? on-action]
   (header-actions-component root-graphics register? on-action false))
  ([root-graphics register? on-action compact?]
   (header-actions-component root-graphics register? on-action compact? nil))
  ([root-graphics register? on-action compact? apps]
   (let [chips
         (header-action-chips compact? apps)

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

(defn header-rows
  "Rows reserved by the header: none in Zen mode. Goal controls live in the footer."
  ([db] (if (get-in db [:settings :zen-mode]) 0 header-rows-base))
  ([db _cols] (header-rows db)))

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

(defn- draw-header-band!
  "Paint the header band: the active session title at the left edge with the
   channel status after it, and help and ID controls on the right."
  [g db header-top cols]
  (let [header-top
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

        {:keys [left-x left-w right-x]}
        (vh/slot-layout cols)

        left-x
        (long left-x)

        left-w
        (long left-w)

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

        left-message
        (latest-channel-status db)

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

        ;; RIGHT cluster geometry is measured by the same real GUI2 GridLayout
        ;; that positions and paints the action components. Its right margin is
        ;; the one-cell separation from the id badge.
        actions-component
        (header-actions-component g *register-click-regions?* nil (<= (long cols) 120) nil)

        cluster-w
        (long (.getColumns (.getPreferredSize ^Panel actions-component)))

        cluster-start
        (long (max edge-pad (- action-col cluster-w)))

        center-limit
        (- cluster-start (long vh/slot-gap-cols))

        title-col
        (+ left-x edge-pad)

        ;; The channel status follows the title; the title gives it room first.
        status-room
        (if left-text (+ (long vh/slot-gap-cols) (long (p/display-width left-text))) 0)

        title-cap
        (max 0 (- center-limit title-col status-room))

        ;; A subagent session shows its path from the leader instead of its title. The `<`
        ;; marks the path as the way back to the parent session.
        crumb
        (when (get-in db [:session :agent :parent-id])
          (agent-team/breadcrumb (get-in db [:session :id])))

        title-text
        (ellipsize (if crumb (str "< " (:text crumb)) (title-or-placeholder db)) title-cap)

        title-width
        (p/display-width title-text)

        status-col
        (+ title-col title-width (if (pos? title-width) (long vh/slot-gap-cols) 0))]

    (components/band-rule! g top-rule-row cols)
    (p/clear-styles! g)
    (p/set-colors! g t/footer-fg t/terminal-bg)
    (p/fill-rect! g 0 content-row cols 1)
    ;; Live channel status after the title. Notifications never show here.
    (when (<= (+ status-col (long (p/display-width (or left-text "")))) center-limit)
      (components/notification-slot! g status-col content-row left-text left-level))
    ;; The left of the header shows only the active session title, never tabs.
    (when (pos? title-width)
      (p/clear-styles! g)
      (p/set-colors! g t/header-fg t/terminal-bg)
      (p/enable! g p/BOLD)
      (p/put-str! g title-col content-row title-text)
      ;; The breadcrumb opens the parent session.
      (when (and crumb *register-click-regions?*)
        (.register interactions/hit-map
                   {:bounds {:row content-row :col title-col :width title-width}
                    :kind :switch-session
                    :text (:parent-id crumb)})))
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

(defn draw-header!
  "Paint the header band at `header-top`. Zen mode reserves no header rows, so it paints nothing."
  [g db header-top cols]
  (when (pos? (long (header-rows db cols))) (draw-header-band! g db header-top cols)))
