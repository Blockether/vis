(ns com.blockether.vis.tui.projects
  "Left-hand project navigation. Geometry, hit targets and keyboard share one model."
  (:require [com.blockether.vis.tui.components :as components]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.header-model :as model]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.theme :as t])
  (:import [com.googlecode.lanterna TerminalPosition]
           [com.googlecode.lanterna.gui2 HitRegionMap]
           [com.googlecode.lanterna.input KeyStroke KeyType MouseAction MouseActionType]))

(defonce ^HitRegionMap hit-map (interactions/create-hit-map))

(defn- tab-state
  [db tab]
  (if (= (:id tab) (:active-tab-id db)) db (get-in db [:tab-locals (:id tab)])))

(defn geometry
  "Use 40–56 cells on the left, reserving 56 for session alerts; keep 60 for chat."
  [db cols rows]
  (when (get-in db [:project-sidebar :open?])
    (let [cols
          (long cols)

          width
          (min cols
               (max (if (some #(or (:human-input (tab-state db %)) (:unread? %)) (:tabs db)) 56 40)
                    (min 56 (quot cols 3))))

          remaining
          (- cols width)

          docked?
          (>= remaining 60)]

      {:left 0
       :width width
       :rows (long rows)
       :chat-left (if docked? width 0)
       :chat-cols (if docked? remaining cols)})))

(defn chat-cols [db cols] (or (:chat-cols (geometry db cols 0)) cols))

(defn chat-key
  "Translate a physical pointer into chat-local coordinates, retaining wheel counts."
  [key offset]
  (if (and (instance? MouseAction key) (pos? (long offset)))
    (let [^MouseAction mouse
          key

          position
          (.getPosition mouse)]

      (MouseAction. (.getActionType mouse)
                    (.getButton mouse)
                    (TerminalPosition. (int (- (.getColumn position) (long offset)))
                                       (.getRow position))
                    (.getCount mouse)))
    key))

(defn sidebar-entries
  "Project counters and actionable sessions, in shared paint/keyboard order.
   Waiting tabs are not running. Unread replies persist until their tab is opened."
  [db]
  (->> (get-in db [:project-sidebar :items])
       (mapcat
         (fn [project]
           (let [pid
                 (str (get project "id"))

                 tabs
                 (filterv #(= pid (:project-id %)) (:tabs db))

                 waiting
                 (filterv #(:human-input (tab-state db %)) tabs)

                 unread
                 (set (filter #(and (:unread? %) (not= (:id %) (:active-tab-id db))) tabs))

                 running
                 (count (filter #(let [local (tab-state db %)] (and (:loading? local)
                                                                    (not (:human-input local))))
                                tabs))]

             (into [{:kind :project-select
                     :project project
                     :label (get project "name" "Untitled project")
                     :tab-count (if (seq tabs) (count tabs) (get project "session_count" 0))
                     :running running
                     :needs-input (count waiting)
                     :unread (count unread)
                     :action [:select project]}]
                   (map (fn [tab]
                          (let [local
                                (tab-state db tab)

                                session-id
                                (or (get-in local [:session :id])
                                    (get-in local [:human-input :request :session-id]))]

                            {:kind (if (:human-input local) :project-input :project-unread)
                             :project project
                             :tab-id (:id tab)
                             :unread? (contains? unread tab)
                             :label (model/title-or-placeholder (:label tab))
                             :action [:session (str session-id)]}))
                        (filter #(or (:human-input (tab-state db %)) (contains? unread %))
                                tabs))))))
       (map-indexed #(assoc %2 :index (inc (long %1))))
       vec))

(defn visible-entries
  "Scroll projects and alerts together, retaining the parent of a clipped alert group."
  [db rows]
  (let [entries
        (sidebar-entries db)

        capacity
        (max 0 (- (long rows) (if (get-in db [:project-sidebar :error]) 8 7)))

        index
        (max 0 (long (or (get-in db [:project-sidebar :index]) 0)))

        end
        (min (count entries) (max capacity index))

        start
        (max 0 (- end capacity))

        visible
        (subvec entries start end)]

    (if (and (> capacity 1) (#{:project-input :project-unread} (:kind (first visible))))
      (into [(first (filter #(and (= :project-select (:kind %))
                                  (= (:project (first visible)) (:project %)))
                            entries))]
            (take-last (dec capacity) visible))
      visible)))

(defn- row-status
  [entry opening width]
  (case (:kind entry)
    :project-input
    (if (:unread? entry) " NEW · needs input " " needs input ")

    :project-unread
    " NEW "

    (let [{:keys [tab-count running needs-input unread project]}
          entry

          compact?
          (or (< (long width) 48) (pos? (long unread)))

          separator
          (if (and (< (long width) 48) (pos? (long unread))) " " " · ")]

      (str
        tab-count
        (if (= 1 tab-count) " tab" " tabs")
        (when (pos? (long running))
          (str separator running (if (and (pos? (long needs-input)) compact?) " run" " running")))
        (when (pos? (long needs-input))
          (str separator needs-input (if (pos? (long unread)) " input" " needs input")))
        (when (pos? (long unread)) (str separator unread " NEW"))
        (when (= (get project "id") opening) " · Loading…")))))

(defn paint!
  "Use the main view's three-row header, bordered container and inset footer.
   Waiting and unread sessions have filled buttons and independently focusable rows."
  [g db cols rows]
  (binding [interactions/hit-map
            hit-map

            t/dialog-bg
            t/terminal-bg]

    (.beginFrame hit-map)
    (when-let [{:keys [left width]} (geometry db cols rows)]
      (let [left (long left)
            width (long width)
            rows (long rows)
            sidebar (:project-sidebar db)]

        (p/clear-styles! g)
        (p/set-colors! g t/dialog-fg t/dialog-bg)
        (p/fill-rect! g left 0 width rows)
        (.register interactions/hit-map
                   {:kind :project-rail :bounds {:col left :row 0 :width width :height rows}})
        (when (and (> width 1) (> rows 1))
          (p/set-fg! g t/dialog-hint)
          (p/draw-box! g left 0 width rows)
          (doseq [row (distinct (filter #(< 0 (long %) (dec rows)) [2 (- rows 3)]))]
            (p/put-str! g left row (str "├" (p/horiz-line (- width 2)) "┤"))))
        (when (and (>= width 18) (> rows 3))
          (p/set-fg! g t/header-fg)
          (p/styled g [p/BOLD] (p/put-str! g (+ left 2) 1 "Projects"))
          (components/button! g
                              (- (+ left width) 9)
                              1 " + "
                              :project-add {:accent? (and (:focused? sidebar)
                                                          (zero? (long (or (:index sidebar) 0))))})
          (components/button! g (- (+ left width) 5) 1 " ✕ " :project-hide))
        (doseq [[offset {:keys [index project kind label] :as entry}]
                (map-indexed vector (visible-entries db rows))
                :let [row (+ 4 (long offset))
                      child? (not= :project-select kind)
                      active? (if child?
                                (= (:tab-id entry) (:active-tab-id db))
                                (= (str (get project "id")) (:active-project-id db)))
                      status (p/truncate-cols (row-status entry (:opening sidebar) width)
                                              (max 0 (- width 9)))
                      status-col (- (+ left width) 2 (long (p/display-width status)))
                      row-left (+ left (if child? 2 0))
                      name-width (max 0 (- status-col row-left 1))]]

          (binding [t/dialog-bg (if active? t/input-field-bg t/terminal-bg)]
            (p/set-colors! g t/dialog-fg t/dialog-bg)
            (p/fill-rect! g (inc left) row (max 0 (- width 2)) 1)
            (p/styled g
                      (if active? [p/BOLD] [])
                      (dlg/draw-selectable-row! g
                                                row-left
                                                row
                                                name-width
                                                (and (:focused? sidebar) (= index (:index sidebar)))
                                                (str (if child?
                                                       (if (= :project-input kind) "! " "● ")
                                                       (dlg/choice-mark true active?))
                                                     label)))
            (if child?
              (components/button!
                g
                status-col
                row
                status
                kind
                {:tint :warning :register? false :extra {:tab-id (:tab-id entry)}})
              (do (p/set-colors! g
                                 (if (or (pos? (long (:needs-input entry)))
                                         (pos? (long (:unread entry))))
                                   t/warning-fg
                                   t/dialog-hint-key)
                                 t/dialog-bg)
                  (p/put-str! g status-col row status))))
          (.register interactions/hit-map
                     (assoc entry
                       :bounds {:col (inc left) :row row :width (max 0 (- width 2)) :height 1})))
        (when (and (empty? (:items sidebar)) (> rows 8))
          (p/set-colors! g t/dialog-hint t/dialog-bg)
          (p/put-str! g
                      (+ left 2)
                      4
                      (p/truncate-cols
                        (if (:loading? sidebar) "Loading projects…" "Add a project with +")
                        (max 0 (- width 4)))))
        (when (> rows 6)
          (when-let [error (:error sidebar)]
            (p/set-colors! g t/warning-fg t/dialog-bg)
            (p/put-str! g (+ left 2) (- rows 4) (p/truncate-cols error (max 0 (- width 4))))))
        (when (> rows 5)
          (let [available (max 0 (- width 4))
                hints ["↑↓ select · ↵ open · C-x w hide · Esc chat"
                       "↑↓ · ↵ open · C-x w hide · Esc chat" "↑↓ · ↵ · C-x w · Esc"]
                hint (or (first (filter #(<= (p/display-width %) available) hints)) (last hints))]

            (p/set-colors! g t/dialog-hint t/dialog-bg)
            (p/put-str! g (+ left 2) (- rows 2) (p/truncate-cols hint available))))))
    (.commitFrame hit-map)))

(defn key-action
  "Return a sidebar action or nil to leave the event to the normal TUI dispatcher."
  [db ^KeyStroke key]
  (when (and key (get-in db [:project-sidebar :open?]))
    (let [sidebar
          (:project-sidebar db)

          index
          (long (or (:index sidebar) 0))]

      (cond
        (instance? MouseAction key)
        (let [^MouseAction mouse
              key

              pos
              (.getPosition mouse)

              hit
              (.lookup hit-map (.getColumn pos) (.getRow pos))]

          (if (#{:project-rail :project-select :project-input :project-unread :project-add
                 :project-hide}
               (:kind hit))
            (cond (#{MouseActionType/SCROLL_UP MouseActionType/SCROLL_DOWN} (.getActionType mouse))
                  [:move (if (= MouseActionType/SCROLL_UP (.getActionType mouse)) -1 1)]
                  (and (= MouseActionType/CLICK_DOWN (.getActionType mouse))
                       (= 1 (.getButton mouse)))
                  (case (:kind hit)
                    (:project-select :project-input :project-unread)
                    (:action hit)

                    :project-add
                    [:add]

                    :project-hide
                    [:hide]

                    [:focus])
                  :else [:noop])
            (when (and (:focused? sidebar) (= MouseActionType/CLICK_DOWN (.getActionType mouse)))
              [:blur-pass])))
        (not (:focused? sidebar)) nil
        (or (.isCtrlDown key) (.isAltDown key)) nil
        (= KeyType/Escape (.getKeyType key)) [:blur]
        (= KeyType/Tab (.getKeyType key)) [:blur]
        (= KeyType/ArrowUp (.getKeyType key)) [:move -1]
        (= KeyType/ArrowDown (.getKeyType key)) [:move 1]
        (= KeyType/Enter (.getKeyType key))
        (if (zero? index) [:add] (or (:action (nth (sidebar-entries db) (dec index) nil)) [:noop]))
        (= \+ (.getCharacter key)) [:add]
        (= \r (.getCharacter key)) [:refresh]
        :else [:noop]))))
