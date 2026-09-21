(ns com.blockether.vis.tui.projects
  "Left-hand project navigation. Geometry, hit targets and keyboard share one model."
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.components :as components]
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

             (into
               [{:kind :project-select
                 :project project
                 :label (get project "name" "Untitled project")
                 :tab-count (if (seq tabs) (count tabs) (get project "session_count" 0))
                 :running running
                 :needs-input (count waiting)
                 :unread (count unread)
                 :action [:select project]}]
               (concat
                 ;; Groups nest under their project: a group row is the project's
                 ;; own structure, so it sits above the attention rows.
                 (map (fn [group]
                        {:kind :project-group
                         :project project
                         :group group
                         :label (get group "name" "Untitled group")
                         :color (get group "color")
                         :group-count (long (or (get group "session_count") 0))
                         :action [:select project]})
                      (get-in db [:project-sidebar :groups pid]))
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
                      (filter #(or (:human-input (tab-state db %)) (contains? unread %)) tabs)))))))
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

    (if (and (> capacity 1)
             (#{:project-group :project-input :project-unread} (:kind (first visible))))
      (into [(first (filter #(and (= :project-select (:kind %))
                                  (= (:project (first visible)) (:project %)))
                            entries))]
            (take-last (dec capacity) visible))
      visible)))

(defn- row-status
  [entry opening width]
  (case (:kind entry)
    :project-group
    (let [n (long (:group-count entry))]
      (str n (if (= 1 n) " session" " sessions")))

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

(defn add-field
  "The inline add field's empty state — what `+` opens in place of a modal."
  []
  {:text "" :cursor 0})

(defn add-field-insert
  "Insert `s` at the caret and return the field's next state. Control characters
   and bracketed-paste markers are dropped, so a pasted path arrives as one line.
   Typing releases a highlighted completion: what Enter adds is the text again."
  [field s]
  (let [text
        (str (:text field))

        cursor
        (max 0 (min (long (or (:cursor field) 0)) (count text)))

        s
        (str/replace (str s) #"[\p{Cntrl}\uE200\uE201]" "")]

    (-> field
        (assoc :text (str (subs text 0 cursor) s (subs text cursor))
               :cursor (+ cursor (count s)))
        (dissoc :index))))

(defn- add-field-erase
  "Delete the character before (`-1`) or under (`1`) the caret."
  [field delta]
  (let [text
        (str (:text field))

        cursor
        (max 0 (min (long (or (:cursor field) 0)) (count text)))

        [erased caret]
        (cond (neg? (long delta)) (if (zero? cursor)
                                    [text 0]
                                    [(str (subs text 0 (dec cursor)) (subs text cursor))
                                     (dec cursor)])
              (>= cursor (count text)) [text cursor]
              :else [(str (subs text 0 cursor) (subs text (inc cursor))) cursor])]

    (-> field
        (assoc :text erased
               :cursor caret)
        (dissoc :index))))

(defn- add-field-caret
  "Move the caret by a delta, or to `:home`/`:end`."
  [field where]
  (let [text
        (str (:text field))

        cursor
        (max 0 (min (long (or (:cursor field) 0)) (count text)))]

    (-> field
        (assoc :text text
               :cursor (case where
                         :home
                         0

                         :end
                         (count text)

                         (max 0 (min (count text) (+ cursor (long where))))))
        (dissoc :index))))

(defn add-field-dir
  "The directory part of a typed path - everything through its last `/`. Text
   with no separator yet completes inside the gateway host's own home, which is
   what a blank query asks the daemon for."
  [text]
  (let [text (str text)]
    (if-let [slash (str/last-index-of text "/")]
      (subs text 0 (inc (long slash)))
      "")))

(defn add-field-listing
  "Record the directory listing the field completes against: `dir` is the query
   it answers and `rows` its wire entries, or nil while the read is still in
   flight. The typed text and caret are left alone."
  [field dir rows]
  (-> field
      (assoc :dir dir
             :rows (vec rows)
             :loading? (nil? rows))
      (dissoc :index)))

(defn- add-field-matches
  "The directories the typed text completes to: every listed child of its
   directory whose name matches the last segment, prefix matches first so the
   obvious answer is the one `Tab` fills in."
  [field]
  (let [rows
        (vec (:rows field))

        text
        (str (:text field))

        leaf
        (str/lower-case (subs text (count (add-field-dir text))))

        row-name
        (fn [row]
          (str/lower-case (str (get row "name"))))]

    (if (str/blank? leaf)
      rows
      (into (filterv (fn [row]
                       (str/starts-with? (row-name row) leaf))
              rows)
            (filterv (fn [row]
                       (and (not (str/starts-with? (row-name row) leaf))
                            (str/includes? (row-name row) leaf)))
              rows)))))

(defn- add-field-selected
  "The completion row the human has highlighted, or nil while the typed text
   itself is what Enter would add."
  [field]
  (when-let [index (:index field)]
    (nth (add-field-matches field) (long index) nil)))

(defn- add-field-move
  "Walk the completions. Stepping above the first row releases the highlight, so
   the typed text is reachable again; stepping up from the text wraps to the last
   row."
  [field delta]
  (let [total
        (count (add-field-matches field))

        index
        (some-> (:index field)
                long)

        moved
        (cond (zero? total) nil
              (nil? index) (if (neg? (long delta)) (dec total) 0)
              :else (+ (long index) (long delta)))]

    (if (and moved (<= 0 (long moved) (dec total)))
      (assoc field :index (long moved))
      (dissoc field :index))))

(defn- add-field-fill
  "Fill a completion into the field and keep going deeper: the row's own path,
   with the separator that starts the next segment."
  [field row]
  (let [text (str (get row "path") "/")]
    (-> field
        (assoc :text text
               :cursor (count text))
        (dissoc :index))))

(defn- add-field-action
  "Keys while the inline add field is open. Enter adds the highlighted directory
   or else the trimmed text, `Tab` fills the highlighted completion in, `up`/`down`
   walk the completions, Esc closes the field without leaving the rail, and
   everything else is typing."
  [field ^KeyStroke key]
  (let [kind
        (.getKeyType key)

        ch
        (.getCharacter key)]

    (cond (= KeyType/Escape kind) [:adding nil]
          (= KeyType/Enter kind) [:add-commit
                                  (str/trim (str (or (some-> (add-field-selected field)
                                                             (get "path"))
                                                     (:text field))))]
          (= KeyType/Tab kind) (if-let [row (or (add-field-selected field)
                                                (first (add-field-matches field)))]
                                 [:adding (add-field-fill field row)]
                                 [:noop])
          (= KeyType/ArrowUp kind) [:adding (add-field-move field -1)]
          (= KeyType/ArrowDown kind) [:adding (add-field-move field 1)]
          (= KeyType/Backspace kind) [:adding (add-field-erase field -1)]
          (= KeyType/Delete kind) [:adding (add-field-erase field 1)]
          (= KeyType/ArrowLeft kind) [:adding (add-field-caret field -1)]
          (= KeyType/ArrowRight kind) [:adding (add-field-caret field 1)]
          (= KeyType/Home kind) [:adding (add-field-caret field :home)]
          (= KeyType/End kind) [:adding (add-field-caret field :end)]
          (and (= KeyType/Character kind) ch) [:adding (add-field-insert field ch)]
          :else [:noop])))

(defn- paint-suggestions!
  "Draw the directories the open field completes to on the rows the project list
   normally holds: the folder's name, and the branch of one that is already a git
   working tree. `capacity` is how many rows the rail can spare - the highlight
   scrolls inside it."
  [g field left width capacity]
  (let [left
        (long left)

        width
        (long width)

        capacity
        (long capacity)

        matches
        (add-field-matches field)

        index
        (some-> (:index field)
                long)

        start
        (if (and index (>= (long index) capacity)) (inc (- (long index) capacity)) 0)

        shown
        (vec (take capacity (drop start matches)))]

    (if (seq shown)
      (doseq [[offset row]
              (map-indexed vector shown)

              :let [line
                    (+ 4 (long offset))

                    branch
                    (let [branch (str (get row "branch"))]
                      (when-not (str/blank? branch)
                        (p/truncate-cols branch (max 0 (quot width 3)))))

                    hint-col
                    (- (+ left width) 2 (long (p/display-width (str branch))))]]

        (dlg/draw-selectable-row! g
                                  left
                                  line
                                  (max 0 (- hint-col left 1))
                                  (= (+ (long start) (long offset)) index)
                                  (str (get row "name") "/"))
        (when branch
          (p/set-colors! g t/dialog-hint-key t/dialog-bg)
          (p/put-str! g hint-col line branch)
          (p/set-colors! g t/dialog-fg t/dialog-bg))
        (.register interactions/hit-map
                   {:kind :project-suggest
                    :path (str (get row "path"))
                    :bounds {:col (inc left) :row line :width (max 0 (- width 2)) :height 1}}))
      (when (pos? capacity)
        (p/set-colors! g t/dialog-hint t/dialog-bg)
        (p/put-str! g
                    (+ left 2)
                    4
                    (p/truncate-cols
                      (if (:loading? field) "Reading directories…" "No matching directory")
                      (max 0 (- width 4))))))))

(defn- paint-add-field!
  "Paint the inline add field on the rail's spare header row, and the directories
   it completes to below, returning the position its caret sits at - or nil when
   no field is open."
  [g db cols rows]
  (when-let [field (get-in db [:project-sidebar :adding])]
    (when-let [{:keys [left width]} (geometry db cols rows)]
      (when (and (> (long width) 8) (> (long rows) 6))
        (let [cursor (dlg/draw-text-input-field! g
                                                 (long left)
                                                 3
                                                 (long width)
                                                 (str (:text field))
                                                 (long (or (:cursor field) 0))
                                                 "/absolute/directory")]
          (paint-suggestions! g
                              field
                              left
                              width
                              (max 0
                                   (- (long rows) (if (get-in db [:project-sidebar :error]) 8 7))))
          cursor)))))

(defn paint!
  "Use the main view's three-row header, bordered container and inset footer.
   Waiting and unread sessions have filled buttons and independently focusable rows.
   Returns the caret position of the inline add field, or nil when none is open."
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
                ;; An open add field owns the rows area: its completions are drawn
                ;; there in place of the project list.
                (map-indexed vector (if (:adding sidebar) [] (visible-entries db rows)))
                :let [row (+ 4 (long offset))
                      child? (not= :project-select kind)
                      alert? (contains? #{:project-input :project-unread} kind)
                      active? (if child?
                                (= (:tab-id entry) (:active-tab-id db))
                                (= (str (get project "id")) (:active-project-id db)))
                      status (p/truncate-cols (row-status entry (:opening sidebar) width)
                                              (max 0 (- width 9)))
                      status-col (- (+ left width) 2 (long (p/display-width status)))
                      row-left (+ left (if child? 2 0))
                      name-width (max 0 (- status-col row-left 1))]]

          (binding [t/dialog-bg (if active? t/input-field-bg t/terminal-bg)]
            (p/set-colors! g
                           (if (= :project-group kind) (t/group-ink (:color entry)) t/dialog-fg)
                           t/dialog-bg)
            (p/fill-rect! g (inc left) row (max 0 (- width 2)) 1)
            (p/styled g
                      (if active? [p/BOLD] [])
                      (dlg/draw-selectable-row! g
                                                row-left
                                                row
                                                name-width
                                                (and (:focused? sidebar) (= index (:index sidebar)))
                                                (str (case kind
                                                       :project-select
                                                       (dlg/choice-mark true active?)

                                                       :project-input
                                                       "! "

                                                       :project-group
                                                       "◆ "

                                                       "● ")
                                                     label)))
            (if alert?
              (components/button!
                g
                status-col
                row
                status
                kind
                {:tint :warning :register? false :extra {:tab-id (:tab-id entry)}})
              (do (p/set-colors! g
                                 (cond (= :project-group kind) (t/group-ink (:color entry))
                                       (or (pos? (long (or (:needs-input entry) 0)))
                                           (pos? (long (or (:unread entry) 0))))
                                       t/warning-fg
                                       :else t/dialog-hint-key)
                                 t/dialog-bg)
                  (p/put-str! g status-col row status))))
          (.register interactions/hit-map
                     (assoc entry
                       :bounds {:col (inc left) :row row :width (max 0 (- width 2)) :height 1})))
        (when (and (empty? (:items sidebar)) (not (:adding sidebar)) (> rows 8))
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
                hints (if (:adding sidebar)
                        ;; The field owns the keyboard while it is open, so the rail
                        ;; spells what fills it in and what ends it.
                        ["↵ add · ⇥ complete · ↑↓ pick · Esc cancel"
                         "↵ add · ⇥ fill · ↑↓ pick · Esc cancel" "↵ add · ⇥ fill · Esc cancel"
                         "↵ add project · Esc cancel" "↵ add · Esc cancel" "↵ add · Esc"]
                        ["↑↓ select · ↵ open · g menu · C-x w hide · Esc chat"
                         "↑↓ · ↵ open · g menu · C-x w hide · Esc chat"
                         ;; The narrowest rail still spells every verb: tighter
                         ;; separators buy the room the words need.
                         "↑↓·↵ open·g menu·C-x w hide·Esc chat" "↑↓ · ↵ · g menu · C-x w hide · Esc"
                         "↑↓ · ↵ · g menu · C-x w · Esc" "↑↓ · ↵ · g · C-x w · Esc"
                         "↑↓ ↵ g C-x w Esc"])
                hint (or (first (filter #(<= (p/display-width %) available) hints)) (last hints))]

            (p/set-colors! g t/dialog-hint t/dialog-bg)
            (p/put-str! g (+ left 2) (- rows 2) (p/truncate-cols hint available))))))
    (let [cursor (paint-add-field! g db cols rows)]
      (.commitFrame hit-map)
      cursor)))

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

          (if (#{:project-rail :project-select :project-group :project-input :project-unread
                 :project-add :project-hide :project-suggest}
               (:kind hit))
            (cond (#{MouseActionType/SCROLL_UP MouseActionType/SCROLL_DOWN} (.getActionType mouse))
                  (let [delta (if (= MouseActionType/SCROLL_UP (.getActionType mouse)) -1 1)]
                    ;; While the add field is open the wheel walks its completions:
                    ;; the project list is not what the rail is showing.
                    (if-let [field (:adding sidebar)]
                      [:adding (add-field-move field delta)]
                      [:move delta]))
                  (and (= MouseActionType/CLICK_DOWN (.getActionType mouse))
                       (= 1 (.getButton mouse)))
                  (case (:kind hit)
                    (:project-select :project-group :project-input :project-unread)
                    (:action hit)

                    :project-suggest
                    [:add-commit (:path hit)]

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
        ;; The inline `+` field owns every ordinary key while it is open: a path
        ;; is typed, not navigated.
        (:adding sidebar) (add-field-action (:adding sidebar) key)
        (= KeyType/Escape (.getKeyType key)) [:blur]
        (= KeyType/Tab (.getKeyType key)) [:blur]
        (= KeyType/ArrowUp (.getKeyType key)) [:move -1]
        (= KeyType/ArrowDown (.getKeyType key)) [:move 1]
        (= KeyType/Enter (.getKeyType key))
        (if (zero? index) [:add] (or (:action (nth (sidebar-entries db) (dec index) nil)) [:noop]))
        (= \+ (.getCharacter key)) [:add]
        (= \g (.getCharacter key))
        ;; `g` opens the row's own menu: group actions on a group row, project
        ;; actions on a project row. Nothing to act on above the first row.
        (let [entry (when (pos? index) (nth (sidebar-entries db) (dec index) nil))]
          (if (#{:project-select :project-group} (:kind entry)) [:menu entry] [:noop]))
        (= \r (.getCharacter key)) [:refresh]
        :else [:noop]))))
