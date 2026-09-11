(ns com.blockether.vis.tui.projects
  "Right-hand project navigation. Geometry, hit targets and keyboard share one model."
  (:require [com.blockether.vis.tui.components :as components]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.theme :as t])
  (:import [com.googlecode.lanterna.input KeyStroke KeyType MouseAction MouseActionType]))

(defn geometry
  "Reserve 30 cells on wide terminals; use a dismissible overlay below 72 columns."
  [db cols rows]
  (when (get-in db [:project-sidebar :open?])
    (let [width (min 30 (long cols))]
      {:left (- (long cols) width)
       :width width
       :rows (long rows)
       :chat-cols (if (>= (long cols) 72) (- (long cols) width) (long cols))})))

(defn chat-cols [db cols] (or (:chat-cols (geometry db cols 0)) cols))

(defn visible-projects
  "Keep the keyboard row in the scrolling window; never paint below the hint row."
  [sidebar rows]
  (let [items
        (vec (:items sidebar))

        capacity
        (max 1 (quot (- (long rows) 6) 2))

        index
        (max 0 (dec (long (or (:index sidebar) 0))))

        start
        (min (max 0 (- (count items) capacity)) (max 0 (- index (dec capacity))))]

    (map-indexed (fn [i project]
                   [(+ (long start) (long i) 1) project])
                 (take capacity (drop start items)))))

(defn paint!
  "Paint production row and button controls, registering the whole rail above chat hits."
  [g db cols rows]
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
      (doseq [row (range rows)]
        (p/put-str! g left row "│"))
      (when (>= width 18)
        (p/styled g [p/BOLD] (p/put-str! g (+ left 2) 0 "Projects"))
        (components/button! g
                            (- (+ left width) 8)
                            0 " + "
                            :project-add {:accent? (and (:focused? sidebar)
                                                        (zero? (long (or (:index sidebar) 0))))})
        (components/button! g (- (+ left width) 4) 0 " ✕ " :project-hide))
      (doseq [[index project] (visible-projects sidebar rows)
              :let [row (+ 2 (* 2 (- (long index) (long (ffirst (visible-projects sidebar rows))))))
                    pid (str (get project "id"))
                    tabs (filter #(= pid (:project-id %)) (:tabs db))
                    tab-count (if (seq tabs) (count tabs) (get project "session_count" 0))
                    running (count (filter #(if (= (:id %) (:active-tab-id db))
                                              (:loading? db)
                                              (get-in db [:tab-locals (:id %) :loading?]))
                                           tabs))]
              :when (< (inc row) (- rows 2))]

        (dlg/draw-selectable-row! g
                                  left
                                  row
                                  (dec width)
                                  (and (:focused? sidebar) (= index (:index sidebar)))
                                  (str (dlg/choice-mark true (= pid (:active-project-id db)))
                                       (get project "name" "Untitled project")))
        (p/set-colors! g t/dialog-fg t/dialog-bg)
        (p/put-str! g
                    (+ left 4)
                    (inc row)
                    (p/truncate-cols (str tab-count
                                          (if (= 1 tab-count) " tab" " tabs")
                                          (when (pos? running) (str " · " running " running"))
                                          (when (= pid (:opening sidebar)) " · Loading…"))
                                     (max 0 (- width 5))))
        (.register interactions/hit-map
                   {:kind :project-select
                    :project project
                    :bounds {:col (inc left) :row row :width (dec width) :height 2}}))
      (when (and (empty? (:items sidebar)) (> rows 4))
        (p/set-colors! g t/dialog-fg t/dialog-bg)
        (p/put-str! g
                    (+ left 2)
                    2
                    (p/truncate-cols
                      (if (:loading? sidebar) "Loading projects…" "Add a project with +")
                      (- width 3))))
      (when (> rows 5)
        (p/set-colors! g t/dialog-fg t/dialog-bg)
        (p/put-str! g
                    (+ left 2)
                    (- rows 2)
                    (p/truncate-cols (or (:error sidebar) "↑↓ select · Enter open")
                                     (max 0 (- width 3)))))
      (when (> rows 2)
        (p/put-str! g
                    (+ left 2)
                    (dec rows)
                    (p/truncate-cols "C-x w hide · Esc chat" (max 0 (- width 3))))))))

(defn key-action
  "Return a sidebar action or nil to leave the event to the normal TUI dispatcher."
  [db ^KeyStroke key]
  (when (and key (get-in db [:project-sidebar :open?]))
    (let [sidebar
          (:project-sidebar db)

          index
          (long (or (:index sidebar) 0))

          items
          (:items sidebar)]

      (cond (instance? MouseAction key)
            (let [^MouseAction mouse
                  key

                  pos
                  (.getPosition mouse)

                  hit
                  (.lookup interactions/hit-map (.getColumn pos) (.getRow pos))]

              (if (#{:project-rail :project-select :project-add :project-hide} (:kind hit))
                (cond (#{MouseActionType/SCROLL_UP MouseActionType/SCROLL_DOWN}
                       (.getActionType mouse))
                      [:move (if (= MouseActionType/SCROLL_UP (.getActionType mouse)) -1 1)]
                      (and (= MouseActionType/CLICK_DOWN (.getActionType mouse))
                           (= 1 (.getButton mouse)))
                      (case (:kind hit)
                        :project-select
                        [:select (:project hit)]

                        :project-add
                        [:add]

                        :project-hide
                        [:hide]

                        [:focus])
                      :else [:noop])
                (when (and (:focused? sidebar)
                           (= MouseActionType/CLICK_DOWN (.getActionType mouse)))
                  [:blur-pass])))
            (not (:focused? sidebar)) nil
            (or (.isCtrlDown key) (.isAltDown key)) nil
            (= KeyType/Escape (.getKeyType key)) [:blur]
            (= KeyType/Tab (.getKeyType key)) [:blur]
            (= KeyType/ArrowUp (.getKeyType key)) [:move -1]
            (= KeyType/ArrowDown (.getKeyType key)) [:move 1]
            (= KeyType/Enter (.getKeyType key))
            (if (zero? index) [:add] [:select (nth items (dec index) nil)])
            (= \+ (.getCharacter key)) [:add]
            (= \r (.getCharacter key)) [:refresh]
            :else [:noop]))))
