(ns com.blockether.vis.tui.projects
  "Left-hand project navigation. Geometry, hit targets and keyboard share one model."
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.components :as components]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.header-model :as model]
            [com.blockether.vis.tui.input :as input]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.keymap :as keymap]
            [com.blockether.vis.tui.paths :as paths]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.theme :as t])
  (:import [com.googlecode.lanterna TerminalPosition]
           [com.googlecode.lanterna.gui2 HitRegionMap]
           [com.googlecode.lanterna.input KeyStroke KeyType MouseAction MouseActionType]
           [java.time Instant ZoneId]
           [java.time.format DateTimeFormatter]
           [java.util Locale]))

(defonce ^HitRegionMap hit-map (interactions/create-hit-map))

(defn- tab-state
  [db tab]
  (if (= (:id tab) (:active-tab-id db)) db (get-in db [:tab-locals (:id tab)])))

(defn geometry
  "Give the rail 40% of the terminal, keeping room for alerts and 60 chat cells."
  [db cols rows]
  (when (get-in db [:project-sidebar :open?])
    (let [cols
          (long cols)

          width
          (min cols
               (max (if (some #(or (:human-input (tab-state db %)) (:unread? %)) (:tabs db)) 56 40)
                    (quot (* cols 2) 5)))

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

(defn with-gateway-counts
  "Fold the gateway's own per-project counts (`GET /v1/projects/overview`) into
   the rail's project rows.

   Matching is by project id first and by workspace root second, because a root
   nobody named has no id to match on. A project the overview does not mention is
   left exactly as it came."
  [projects overview]
  (let [rows
        (get overview "projects")

        index
        (fn [k]
          (into {}
                (keep (fn [row]
                        (when-let [v (not-empty (str (get row k)))]
                          [v row])))
                rows))

        by-id
        (index "project_id")

        by-root
        (index "root")]

    (mapv (fn [project]
            (if-let [row (or (get by-id (str (get project "id")))
                             (get by-root (str (get project "workspace_root"))))]
              (assoc project
                "live_count" (long (or (get row "live_count") 0))
                "awaiting_count" (long (or (get row "awaiting_count") 0))
                "unread_count" (long (or (get row "unread_count") 0)))
              project))
          projects)))

(defn- project-counts
  "What a project header paints: the GATEWAY's tally for that project, with the
   tabs open in THIS terminal as the instant overlay on top of it.

   The rail counted its own tabs alone, so a run in another process - or any
   session nobody opened here - counted zero, which is the same bug the local
   unread marks had. The gateway tallies every session in the project
   (`with-gateway-counts`), and a session parked on a human is a LIVE one, so
   running is what is live beside the demand. A local flag is a strict subset of
   the gateway's answer and moves first, so each count is the larger of the two."
  [project running waiting unread]
  (let [demand
        (long (or (get project "awaiting_count") 0))

        live
        (long (or (get project "live_count") 0))]

    {:running (max (long running) (- live (min live demand)))
     :needs-input (max (long waiting) demand)
     :unread (max (long unread) (long (or (get project "unread_count") 0)))}))

(defn project-label
  "What a project row paints. A project nobody renamed is named by its own root
   path, so the rail showed `/Users/me/CryptoSyf` where every other path in the
   TUI reads `~/CryptoSyf`. Home is shortened for DISPLAY only — selection,
   matching and the gateway keep the stored name — and a name a human typed, or
   a root outside home, passes through unchanged."
  [project]
  (paths/abbreviate-home (get project "name" "Untitled project")))

(defn- session-local
  [db sid]
  (or (when-let [tab (some (fn [tab]
                             (when (= sid
                                      (some-> (or (get-in (tab-state db tab) [:session :id])
                                                  (:session-id tab))
                                              str))
                               tab))
                           (:tabs db))]
        (tab-state db tab))
      (get-in db [:session-drafts sid])))

(defn session-status
  "The app's gateway-backed status precedence, with the TUI's local unsent draft."
  [session current? dirty? group-archived?]
  (let [live?
        (true? (get session "live"))

        unread
        (if (or live? current? (not= true (get session "is_unread")))
          0
          (max 0 (long (or (get session "unread_answers") 0))))]

    (cond (or (get session "archived_at") group-archived?) "Archived"
          (true? (get session "is_awaiting_input"))
          (let [n (long (or (get session "awaiting_input_count") 1))]
            (if (> n 1) (str "HITL ×" n) "HITL"))
          live? "Live"
          (and (or (true? (get session "was_interrupted")) (true? (get session "was_failed")))
               (pos? unread))
          "Stopped"
          (pos? unread) (if (> unread 1) (str "New ×" unread) "New")
          (= "suspended" (get session "status")) "Waiting"
          dirty? "Dirty"
          :else "Idle")))

(defn- saved-entries
  "Rows from gateway windows. Open TUI views are never the source of the inventory."
  [db]
  (let [sidebar (:project-sidebar db)]
    (->>
      (:items sidebar)
      (mapcat
        (fn [project]
          (let [pid (str (get project "id"))
                expanded? (contains? (:expanded sidebar) pid)
                page (get-in sidebar [:pages pid])
                archived? (true? (get-in sidebar [:session-archived? pid]))
                groups-archived? (true? (get-in sidebar [:group-archived? pid]))
                groups (get-in sidebar [:groups pid])
                by-group (group-by #(str (get % "group_id")) (:grouped page))
                loaded-ids (set (map #(str (get % "id")) (concat (:sessions page) (:grouped page))))
                pinned (->> (concat (:awaiting page)
                                    (when (= (str (get-in db [:session :id]))
                                             (str (get-in page [:current "id"])))
                                      [(:current page)]))
                            (filter #(and % (not (contains? loaded-ids (str (get % "id"))))))
                            (reduce (fn [rows session]
                                      (if (some #(= (get % "id") (get session "id")) rows)
                                        rows
                                        (conj rows session)))
                                    []))
                session-row
                (fn [session]
                  (let [sid (str (get session "id"))
                        local (session-local db sid)
                        draft (input/input->text (:input local))
                        dirty? (or (not (str/blank? draft))
                                   (seq (:attachments local))
                                   (seq (:pending-sends local)))
                        title (or (not-empty (some-> (get session "title")
                                                     str/trim))
                                  (when dirty?
                                    (not-empty (some-> draft
                                                       str/split-lines
                                                       first
                                                       str/trim)))
                                  (when (seq (:attachments local))
                                    (str (count (:attachments local))
                                         " unsent attachment"
                                         (when (not= 1 (count (:attachments local))) "s")))
                                  "Untitled session")
                        group (some #(when (= (str (get % "id")) (str (get session "group_id"))) %)
                                    groups)]

                    {:kind :project-session
                     :project project
                     :session session
                     :label title
                     :selected? (contains? (get-in sidebar [:selected pid] #{}) sid)
                     :favorite? (some? (get session "favorite_rank"))
                     :status (session-status session
                                             (= sid
                                                (some-> (get-in db [:session :id])
                                                        str))
                                             dirty?
                                             (some? (get group "archived_at")))
                     :turns (long (or (get session "turn_count") 0))
                     :modified-at (or (get session "modified_at") (get session "created_at"))
                     :action [:session sid]}))]

            (into
              [{:kind :project-select
                :project project
                :label (project-label project)
                :tab-count (long (or (get project "session_count") 0))
                :running (long (or (get project "live_count") 0))
                :needs-input (long (or (get project "awaiting_count") 0))
                :unread (long (or (get project "unread_count") 0))
                :expanded? expanded?
                :action [:toggle-project pid]}]
              (when expanded?
                (concat
                  (when (seq pinned)
                    (cons {:kind :project-set :label "Attention" :project project :action [:noop]}
                          (map session-row pinned)))
                  [{:kind :project-set
                    :label (if groups-archived? "Groups · Archived" "Groups")
                    :project project
                    :archived? groups-archived?
                    :folded? (true? (get-in sidebar [:groups-folded? pid]))
                    :set :groups
                    :action [:toggle-groups pid]}]
                  (when-not (get-in sidebar [:groups-folded? pid])
                    (concat
                      (mapcat (fn [group]
                                (let [gid (str (get group "id"))
                                      folded? (contains? (get-in sidebar [:group-folds pid]) gid)]

                                  (cons {:kind :project-group
                                         :project project
                                         :group group
                                         :label (get group "name" "Untitled group")
                                         :color (get group "color")
                                         :group-count (long (or (get group "session_count") 0))
                                         :folded? folded?
                                         :action [:toggle-group pid gid]}
                                        (when-not folded?
                                          (map #(assoc (session-row %) :nested? true)
                                               (get by-group gid))))))
                              groups)
                      ;; Only the archive is paged: live groups always show whole.
                      (when (and groups-archived? (pos? (long (or (:group-offset page) 0))))
                        [{:kind :project-group-page
                          :project project
                          :label "← Previous groups"
                          :action [:group-page pid :previous]}])
                      (when (and groups-archived?
                                 (< (+ (long (or (:group-offset page) 0)) (count groups))
                                    (long (or (get-in sidebar [:group-total pid]) 0))))
                        [{:kind :project-group-page
                          :project project
                          :label "More groups →"
                          :action [:group-page pid :next]}])
                      (when (and groups-archived?
                                 (not (:loading? page))
                                 (not (:error page))
                                 (zero? (long (or (get-in sidebar [:group-total pid]) 0))))
                        [{:kind :project-state :project project :label "No archived groups"}])))
                  [{:kind :project-set
                    :label (if archived? "Sessions · Archived" "Sessions")
                    :project project
                    :archived? archived?
                    :set :sessions
                    :folded? (true? (get-in sidebar [:sessions-folded? pid]))
                    :action [:toggle-sessions pid]}]
                  (when-not (get-in sidebar [:sessions-folded? pid])
                    (map session-row (:sessions page)))
                  (when (and (not (get-in sidebar [:sessions-folded? pid])) (seq (:history page)))
                    [{:kind :project-page
                      :project project
                      :label "← Previous sessions"
                      :action [:page pid :previous]}])
                  (when (and (not (get-in sidebar [:sessions-folded? pid])) (:has-more page))
                    [{:kind :project-page
                      :project project
                      :label "More sessions →"
                      :action [:page pid :next]}])
                  (when (:loading? page)
                    [{:kind :project-state :project project :label "Loading sessions…"}])
                  (when-let [error (:error page)]
                    [{:kind :project-state :project project :label (str error " · r retry")}])
                  (when (and (not (:loading? page))
                             (not (:error page))
                             (empty? (:sessions page))
                             (or archived? (empty? groups)))
                    [{:kind :project-state
                      :project project
                      :label (if archived? "No archived sessions" "No saved sessions")}])))))))
      (map-indexed #(assoc %2 :index (inc (long %1))))
      vec)))

(defn- search-entries
  "One gateway-search window; normal folds and cursors stay parked underneath."
  [db]
  (let [sidebar
        (:project-sidebar db)

        {:keys [text loading? error matches rows offset has-more?]}
        (:search sidebar)

        projects
        (into {}
              (map (fn [project]
                     [(str (get project "id")) project])
                   (:items sidebar)))

        title
        (fn [session]
          (or (not-empty (some-> (get session "title")
                                 str/trim))
              "Untitled session"))

        hits
        (mapcat
          (fn [{:keys [session match]}]
            (let [sid
                  (str (get session "id"))

                  pid
                  (str (get session "project_id"))

                  project
                  (get projects pid {"id" pid "name" "Other sessions"})

                  snippet
                  (or (:request-snippet match) (:reply-snippet match))

                  place
                  (cond (:in-title? match) "title"
                        (:in-request? match) "request"
                        (:in-reply? match) "reply"
                        (:in-thinking? match) "thinking"
                        :else "match")]

              (cond-> [{:kind :project-session
                        :project project
                        :session session
                        :label (str (project-label project) " / " (title session))
                        :favorite? (some? (get session "favorite_rank"))
                        :status (session-status session
                                                (= sid (str (get-in db [:session :id])))
                                                false
                                                false)
                        :turns (long (or (get session "turn_count") 0))
                        :modified-at (or (get session "modified_at") (get session "created_at"))
                        :action [:session sid]}]
                (seq snippet)
                (conj {:kind :project-state
                       :project project
                       :label (str "↳ " place ": " (str/replace snippet #"\s+" " "))
                       :action [:noop]}))))
          rows)

        total
        (count matches)

        offset
        (long (or offset 0))]

    (->> (concat
           (when (pos? offset)
             [{:kind :project-page
               :label "← Previous search results"
               :action [:search-page :previous]}])
           hits
           (when has-more?
             [{:kind :project-page :label "More search results →" :action [:search-page :next]}])
           (when loading? [{:kind :project-state :label "Searching sessions…"}])
           (when error [{:kind :project-state :label (str "Search failed · " error)}])
           (when (and (not loading?) (not error) (zero? total))
             [{:kind :project-state
               :label
               (if (str/blank? text) "Type to search saved sessions" "No saved sessions match")}]))
         (map-indexed #(assoc %2 :index (inc (long %1))))
         vec)))

(defn sidebar-entries
  "Project and saved-session rows in shared paint/keyboard order."
  [db]
  (cond (get-in db [:project-sidebar :search]) (search-entries db)
        (contains? (:project-sidebar db) :pages) (saved-entries db)
        :else (->>
                (get-in db [:project-sidebar :items])
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
                                                                             (not (:human-input
                                                                                    local))))
                                         tabs))

                          counts
                          (project-counts project running (count waiting) (count unread))]

                      (into
                        [{:kind :project-select
                          :project project
                          :label (project-label project)
                          :tab-count (if (seq tabs) (count tabs) (get project "session_count" 0))
                          :running (:running counts)
                          :needs-input (:needs-input counts)
                          :unread (:unread counts)
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
                               (filter #(or (:human-input (tab-state db %)) (contains? unread %))
                                       tabs)))))))
                (map-indexed #(assoc %2 :index (inc (long %1))))
                vec)))

(defn- entry-id
  "Stable row identity across gateway refreshes; display labels and counts can change."
  [entry]
  (let [pid (some-> (get-in entry [:project "id"])
                    str)]
    [(:kind entry) pid
     (case (:kind entry)
       :project-session
       (some-> (get-in entry [:session "id"])
               str)

       :project-group
       (some-> (get-in entry [:group "id"])
               str)

       :project-set
       (:set entry)

       :project-select
       nil

       (:action entry))]))

(defn focused-index
  "Keep the same saved row under the cursor when a project/page refresh reorders it.
   If it vanished, focus its project; if that vanished, clamp to a surviving row."
  [db changes]
  (let [before
        (long (or (get-in db [:project-sidebar :index]) 0))

        entries
        (sidebar-entries db)

        row
        (nth entries (dec before) nil)

        updated
        (sidebar-entries (update db :project-sidebar merge changes))

        key
        (when row (entry-id row))

        pid
        (second key)]

    (if (or (zero? before) (get-in db [:project-sidebar :adding]))
      before
      (or (some (fn [entry]
                  (when (= key (entry-id entry)) (:index entry)))
                updated)
          (some (fn [entry]
                  (when (and (= :project-select (:kind entry))
                             (= pid
                                (some-> (get-in entry [:project "id"])
                                        str)))
                    (:index entry)))
                updated)
          (min before (count updated))))))

(defn- row-height
  "Saved sessions get three lines; project and section headings use one."
  [entry]
  (if (= :project-session (:kind entry)) 3 1))

(defn- fit-back
  "Fill the visible area backwards from `end` with whole rows only."
  [entries end capacity]
  (loop [start
         (long end)

         remaining
         (long capacity)]

    (if (and (pos? start) (<= (long (row-height (nth entries (dec start)))) remaining))
      (recur (dec start) (- remaining (long (row-height (nth entries (dec start))))))
      (subvec entries start end))))

(defn visible-entries
  "Scroll whole cards into view, retaining the project above a clipped child."
  [db rows]
  (let [entries
        (sidebar-entries db)

        capacity
        (max 0 (- (long rows) (if (get-in db [:project-sidebar :error]) 8 7)))

        index
        (max 0 (long (or (get-in db [:project-sidebar :index]) 0)))

        first-end
        (loop [end
               0

               remaining
               capacity]

          (if (and (< end (count entries)) (<= (long (row-height (nth entries end))) remaining))
            (recur (inc end) (- remaining (long (row-height (nth entries end)))))
            end))

        end
        (min (count entries) (max first-end index))

        visible
        (fit-back entries end capacity)]

    (if (and (seq visible)
             (not (get-in db [:project-sidebar :search]))
             (> capacity 1)
             (not= :project-select (:kind (first visible))))
      (let [tail
            (fit-back entries end (dec capacity))

            parent
            (first (filter #(and (= :project-select (:kind %))
                                 (= (:project (first tail)) (:project %)))
                           entries))]

        (if (and (seq tail) parent) (if (= parent (first tail)) tail (into [parent] tail)) visible))
      visible)))

(defn entry-row
  "The last screen row of the visible entry at sidebar `index`, or nil when that
   entry is scrolled away or the add field hides the list. Mirrors `paint!`: the
   first entry starts at row 4, and a session card is three rows tall."
  [db rows index]
  (when-not (get-in db [:project-sidebar :adding])
    (let [visible (visible-entries db rows)]
      (some (fn [[entry row]]
              (when (= index (:index entry)) (+ (long row) (dec (long (row-height entry))))))
            (map vector visible (reductions + 4 (map row-height visible)))))))

(defn- dark-selection?
  "Use a solid accent for focused and selected rows on dark surfaces."
  [entry active? focused?]
  (and (= :dark (:mode t/default-theme))
       (or focused?
           (:selected? entry)
           (and active? (#{:project-session :project-input :project-unread} (:kind entry))))))

(defn- row-bg
  "Keep resting rows subtle; give dark selections the full theme accent."
  [{:keys [kind set selected?] :as entry} active? focused?]
  (cond (dark-selection? entry active? focused?) t/header-active-tab-accent
        focused? (t/mix-color t/terminal-bg t/header-active-tab-bg 0.14)
        selected? (t/mix-color t/terminal-bg t/header-active-tab-bg 0.16)
        (and active? (= :project-session kind))
        (t/mix-color t/terminal-bg t/header-active-tab-bg 0.10)
        (= :project-set kind) (case set
                                :groups
                                (t/mix-color t/terminal-bg t/header-active-tab-bg 0.08)

                                :sessions
                                (t/mix-color t/terminal-bg t/text-fg 0.06)

                                t/terminal-bg)
        (= :project-select kind) (if active?
                                   (t/mix-color t/terminal-bg t/header-active-tab-bg 0.10)
                                   (t/mix-color t/terminal-bg t/text-fg 0.04))
        active? t/input-field-bg
        :else t/terminal-bg))

(defn session-time-label
  "When a session last changed, worded like the companion app: relative inside
   the last day, then the date and time, with the year only when it differs."
  ([value] (session-time-label value (System/currentTimeMillis) (ZoneId/systemDefault)))
  ([value now-ms ^ZoneId zone]
   (when-let [ms (dlg/date->millis value)]
     (let [now-ms (long now-ms)
           minutes (quot (max 0 (- now-ms (long ms))) 60000)]

       (cond (< minutes 1) "now"
             (< minutes 60) (str minutes " min ago")
             (< minutes 120) "1 hour ago"
             (< minutes 1440) (str (quot minutes 60) " hours ago")
             :else (let [then (.atZone (Instant/ofEpochMilli (long ms)) zone)
                         now (.atZone (Instant/ofEpochMilli now-ms) zone)]

                     (.format (DateTimeFormatter/ofPattern
                                (if (= (.getYear then) (.getYear now)) "MMM d HH:mm" "MMM d, yyyy")
                                Locale/ENGLISH)
                              then)))))))

(defn- session-status-label
  "The status that a session row shows, after a star for a favorite."
  [entry]
  (str (when (:favorite? entry) "* ") (:status entry)))

(defn- paint-session-status!
  "Paint the session status in bold, in the companion app's colour for it."
  [g entry col row available selected-ink]
  (p/set-colors! g (or selected-ink (dlg/session-status-ink (:status entry))) t/dialog-bg)
  (p/styled
    g
    [p/BOLD]
    (p/put-str! g col row (p/truncate-cols (session-status-label entry) (max 0 (long available))))))

(defn- paint-session-meta!
  "Paint the second row of a session from `col`: its status when the rail is
   narrow, then when it last changed and how many turns it has. The turns, then
   the date, give way before the row passes `right`."
  [g entry col row right selected-ink status?]
  (let [col
        (long col)

        right
        (long right)

        start
        (if status? (+ col (long (p/display-width (session-status-label entry)))) col)

        turns
        (long (or (:turns entry) 0))

        changed
        (session-time-label (:modified-at entry))

        details
        (->> [(remove nil?
                [changed (when (pos? turns) (str turns (if (= 1 turns) " turn" " turns")))])
              (remove nil? [changed])]
             (map #(cond->> (str/join " · " %) (and status? (seq %)) (str " · ")))
             (filter #(<= (+ start (long (p/display-width %))) (inc right)))
             first)]

    (when status? (paint-session-status! g entry col row (max 0 (- (inc right) col)) selected-ink))
    (when (seq details)
      (p/set-colors! g (or selected-ink (t/legible-ink t/dialog-hint t/dialog-bg)) t/dialog-bg)
      (p/put-str! g start row details))))

(defn- row-status
  [entry sidebar width]
  (case (:kind entry)
    :project-group
    (let [n (long (:group-count entry))]
      (str n (if (= 1 n) " session" " sessions")))

    :project-input
    "HITL"

    :project-unread
    "New"

    :project-session
    ""

    (:project-set :project-page :project-group-page :project-state)
    ""

    (let [{:keys [tab-count running needs-input unread project]}
          entry

          compact?
          (or (< (long width) 48) (pos? (long unread)))

          separator
          (if (and (< (long width) 48) (pos? (long unread))) " " " · ")]

      (if (= (get project "id") (:removing sidebar))
        (or (:progress sidebar) "Removing…")
        (str
          tab-count
          (if (= 1 tab-count)
            (if (= :toggle-project (first (:action entry))) " session" " tab")
            (if (= :toggle-project (first (:action entry))) " sessions" " tabs"))
          (when (pos? (long running))
            (str separator running (if (and (pos? (long needs-input)) compact?) " run" " running")))
          (when (pos? (long needs-input))
            (str separator needs-input (if (pos? (long unread)) " input" " needs input")))
          (when (pos? (long unread)) (str separator unread " new"))
          (when (= (get project "id") (:opening sidebar)) " · Loading…"))))))

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
             :listing-path nil
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

(defn- search-field-action
  "Edit the inline query without changing the saved project folds or page cursors."
  [field ^KeyStroke key]
  (let [kind
        (.getKeyType key)

        ch
        (.getCharacter key)]

    (cond (= KeyType/Escape kind) [:search-close]
          (= KeyType/ArrowUp kind) [:move -1]
          (= KeyType/ArrowDown kind) [:move 1]
          (= KeyType/Backspace kind) [:search-change (add-field-erase field -1)]
          (= KeyType/Delete kind) [:search-change (add-field-erase field 1)]
          (= KeyType/ArrowLeft kind) [:search-change (add-field-caret field -1)]
          (= KeyType/ArrowRight kind) [:search-change (add-field-caret field 1)]
          (= KeyType/Home kind) [:search-change (add-field-caret field :home)]
          (= KeyType/End kind) [:search-change (add-field-caret field :end)]
          (and (= KeyType/Character kind) ch) [:search-change (add-field-insert field ch)]
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
                                   (- (long rows) (if (get-in db [:project-sidebar :error]) 9 8))))
          (components/button!
            g
            (+ (long left) 2)
            (- (long rows) (if (get-in db [:project-sidebar :error]) 5 4))
            (if (get-in db [:project-sidebar :saving?]) "Creating folder…" "＋ New folder  Ctrl+N")
            :project-new-folder)
          cursor)))))

(defn- paint-search-field!
  [g db cols rows]
  (when-let [field (get-in db [:project-sidebar :search])]
    (when-let [{:keys [left width]} (geometry db cols rows)]
      (when (and (> (long width) 8) (> (long rows) 6))
        (.register interactions/hit-map
                   {:kind :project-search-field
                    :bounds {:col (long left) :row 3 :width (long width) :height 1}})
        (dlg/draw-text-input-field! g
                                    (long left)
                                    3
                                    (long width)
                                    (str (:text field))
                                    (long (or (:cursor field) 0))
                                    "Search saved sessions")))))

(def ^:private footer-commands
  "The footer's sidebar commands in priority order; a narrow rail drops them from
   the end. Each pair is a button that runs the same action as its key."
  [[:help "keys"] [:menu "menu"] [:settings "settings"] [:add-project "add project"]
   [:back "chat"]])

(defn- footer-pairs
  "The footer's `[key label command]` triples for the rail's mode, most important
   first: a narrow rail keeps the leading pairs. Search and the add field describe
   their own keys; their pairs have no command."
  [sidebar]
  (cond (:search sidebar) [["Enter" "open"] ["Esc" "clear search"] ["↑↓" "results"]]
        (:adding sidebar) [["Enter" "add"] ["Esc" "cancel"] ["Tab" "complete"] ["C-n" "folder"]]
        :else (mapv (fn [[id label]]
                      [(or (some-> (keymap/sidebar-key id)
                                   str)
                           "Esc") label id])
                    footer-commands)))

(defn- paint-footer!
  "Paint the footer row in the dialog hint style: bold keys and dim labels,
   centered, with whole pairs only. List pairs register as `:project-footer`
   buttons and light up under the pointer."
  [g sidebar left width row]
  (let [text-w
        (max 0 (- (long width) 4))

        sep
        "  ·  "

        sep-w
        (long (p/display-width sep))

        all
        (footer-pairs sidebar)

        pairs
        (subvec all 0 (count (dlg/fit-hint-pairs (mapv #(subvec % 0 2) all) text-w)))

        seg-w
        (fn [[k a]]
          (+ (long (p/display-width k)) 1 (long (p/display-width a))))

        total
        (+ (long (reduce + 0 (map seg-w pairs))) (* sep-w (max 0 (dec (count pairs)))))

        hovered
        (.hovered interactions/hit-map)]

    (loop [col
           (+ (long left) 2 (max 0 (quot (- text-w total) 2)))

           [[k a command :as pair] & more]
           (seq pairs)]

      (when pair
        (let [w
              (long (seg-w pair))

              hot?
              (and command (= :project-footer (:kind hovered)) (= command (:command hovered)))]

          (when command
            (.register interactions/hit-map
                       {:kind :project-footer
                        :command command
                        :bounds {:col col :row row :width w :height 1}}))
          (p/clear-styles! g)
          (if hot?
            (p/set-colors! g t/header-active-tab-fg t/header-active-tab-accent)
            (p/set-colors! g t/dialog-hint-key t/dialog-bg))
          (p/styled g [p/BOLD] (p/put-str! g col row k))
          (when-not hot? (p/set-fg! g t/dialog-hint))
          (p/styled g [p/ITALIC] (p/put-str! g (+ col (long (p/display-width k))) row (str " " a)))
          (p/set-colors! g t/dialog-hint t/dialog-bg)
          (when (seq more) (p/put-str! g (+ col w) row sep))
          (recur (+ col w sep-w) more))))))

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
            sidebar (:project-sidebar db)
            visible (if (:adding sidebar) [] (visible-entries db rows))]

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
        (when (and (>= width 19) (> rows 3))
          (p/set-fg! g t/header-fg)
          (p/styled g [p/BOLD] (p/put-str! g (+ left 2) 1 "Projects"))
          (components/button! g
                              (+ left 11)
                              1 " + "
                              :project-add {:accent? (and (:focused? sidebar)
                                                          (zero? (long (or (:index sidebar) 0))))})
          (components/button! g (- (+ left width) 5) 1 " ✕ " :project-hide))
        (doseq [[{:keys [index project kind label] :as entry} row]
                (map vector visible (reductions + 4 (map row-height visible)))
                :let [child? (not= :project-select kind)
                      alert? (contains? #{:project-input :project-unread} kind)
                      active? (case kind
                                :project-session
                                (= (str (get-in entry [:session "id"]))
                                   (str (get-in db [:session :id])))

                                :project-select
                                (= (str (get project "id")) (:active-project-id db))

                                (and (:tab-id entry) (= (:tab-id entry) (:active-tab-id db))))
                      focused? (and (:focused? sidebar) (= index (:index sidebar)))
                      selected-ink (when (dark-selection? entry active? focused?) t/terminal-bg)
                      status (p/truncate-cols (row-status entry sidebar width) (max 0 (- width 9)))
                      status-col
                      (- (+ left width)
                         (cond (= :project-session kind)
                               (if (>= width 48)
                                 (+ 6 (long (p/display-width (session-status-label entry))))
                                 7)
                               (and (= :project-set kind) (#{:groups :sessions} (:set entry))) 10
                               :else 2)
                         (long (p/display-width status)))
                      row-left (+ left
                                  (if child? 1 0)
                                  (if (#{:project-group :project-session} kind) 1 0)
                                  (if (:nested? entry) 1 0))
                      name-width (max 0 (- status-col row-left 1))]]

          (binding [t/dialog-bg (row-bg entry active? focused?)]
            (p/set-colors! g
                           (or selected-ink
                               (if (and (= :project-group kind) (not focused?))
                                 (t/group-ink (:color entry))
                                 t/dialog-fg))
                           t/dialog-bg)
            (p/fill-rect! g (inc left) row (max 0 (- width 2)) (row-height entry))
            (p/styled g
                      (if (or (#{:project-select :project-set} kind) active? focused? selected-ink)
                        [p/BOLD]
                        [])
                      (p/put-str! g
                                  (inc row-left)
                                  row
                                  (dlg/ellipsize (str " "
                                                      (case kind
                                                        :project-select
                                                        (if (contains? sidebar :pages)
                                                          (if (:expanded? entry) "▾ " "▸ ")
                                                          "  ")

                                                        :project-group
                                                        (if (:folded? entry) "▸ " "▾ ")

                                                        :project-session
                                                        "  "

                                                        :project-set
                                                        (if (#{:groups :sessions} (:set entry))
                                                          (if (:folded? entry) "▸ " "▾ ")
                                                          "  ")

                                                        (:project-page :project-group-page)
                                                        "  "

                                                        :project-state
                                                        "  "

                                                        "  ")
                                                      label)
                                                 (max 0 (- name-width 2)))))
            (if alert?
              (do (p/set-colors! g (or selected-ink (dlg/session-status-ink status)) t/dialog-bg)
                  (p/styled g [p/BOLD] (p/put-str! g status-col row status)))
              (do (p/set-colors! g
                                 (or selected-ink
                                     (cond (and (= :project-group kind) (not focused?))
                                           (t/group-ink (:color entry))
                                           (or (pos? (long (or (:needs-input entry) 0)))
                                               (pos? (long (or (:unread entry) 0))))
                                           t/warning-fg
                                           :else t/dialog-hint-key))
                                 t/dialog-bg)
                  (p/put-str! g status-col row status)))
            (when (= :project-session kind)
              (when (>= width 48) (paint-session-status! g entry status-col row 14 selected-ink))
              (paint-session-meta! g
                                   entry
                                   (+ row-left 4)
                                   (inc row)
                                   (- (+ left width) 3)
                                   selected-ink
                                   (< width 48)))
            (when (= :project-group kind)
              (p/set-colors! g (or selected-ink (t/group-ink (:color entry))) t/dialog-bg)
              (p/put-str! g (inc left) row "▏")))
          (.register
            interactions/hit-map
            (assoc entry
              :bounds
              {:col (inc left) :row row :width (max 0 (- width 2)) :height (row-height entry)}))
          (when (and (= :project-set kind) (#{:groups :sessions} (:set entry)))
            (let [pid (str (get project "id"))]
              (components/button!
                g
                (- (+ left width) 9)
                row
                " + "
                (if (= :groups (:set entry)) :project-group-add :project-session-add)
                {:extra {:project-id pid}})
              (components/button! g
                                  (- (+ left width) 5)
                                  row
                                  " ⋮ "
                                  :project-set-menu
                                  {:extra {:project-id pid :set (:set entry)}})))
          (when (= :project-session kind)
            (components/button! g
                                (- (+ left width) 5)
                                row
                                " ⋮ "
                                :project-details
                                {:extra {:session (:session entry)
                                         :action [:details
                                                  (str (get-in entry [:session "id"]))]}})))
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
        (when (> rows 5) (paint-footer! g sidebar left width (- rows 2)))))
    (let [cursor (or (paint-add-field! g db cols rows) (paint-search-field! g db cols rows))]
      (.commitFrame hit-map)
      cursor)))

(defn help-key?
  "Recognize sidebar help keys without taking question marks from inline fields."
  [db ^KeyStroke key]
  (let [sidebar (:project-sidebar db)]
    (boolean (and key
                  (:open? sidebar)
                  (:focused? sidebar)
                  (= KeyType/Character (.getKeyType key))
                  (not (.isAltDown key))
                  (or (and (.isCtrlDown key) (= keymap/sidebar-help-key (.getCharacter key)))
                      (and (not (.isCtrlDown key))
                           (= keymap/sidebar-help-alias-key (.getCharacter key))
                           (or (:help-open? db)
                               (not (or (:adding sidebar) (:search sidebar))))))))))

(defn- menu-item
  "A row-menu item that the sidebar command `command` runs, with its key chip."
  [id label command]
  (let [k (keymap/sidebar-key command)]
    (cond-> {:id id :label label}
      k
      (assoc :key
        k :hint
        (keymap/sidebar-key-label {:key k})))))

(defn row-menu-items
  "The ⋮ menu of a sidebar `entry`, each verb with the key that runs it. The keys
   work on the row without the menu: a key acts exactly where the menu lists it."
  [db entry]
  (let [project
        (:project entry)

        pid
        (some-> (get project "id")
                str)

        group
        (:group entry)

        session
        (:session entry)

        sid
        (some-> (get session "id")
                str)

        selected
        (get-in db [:project-sidebar :selected pid] #{})

        moving
        (long (if (contains? selected sid) (count selected) 1))

        archived-group?
        (some? (get group "archived_at"))]

    (cond
      sid
      (cond-> [(menu-item :details "Show session details" :details)
               (menu-item :toggle-session
                          (if (contains? selected sid) "Deselect session" "Select session")
                          :select)
               (menu-item
                 :move-session
                 (if (> moving 1) (str "Move " moving " selected sessions…") "Move to group…")
                 :move)]
        (or (> moving 1) (some? (get session "group_id")))
        (conj (menu-item
                :ungroup-session
                (if (> moving 1) (str "Ungroup " moving " selected sessions") "Remove from group")
                :ungroup))

        true
        (into [(menu-item :favorite (if (:favorite? entry) "Unstar session" "Star session") :star)
               (menu-item :rename-session "Rename session…" :rename)
               (menu-item :settings "Session settings…" :settings)])

        (not archived-group?)
        (conj (if (get session "archived_at")
                (menu-item :unarchive-session "Unarchive session" :archive)
                (menu-item :archive-session "Archive session" :archive)))

        true
        (conj (menu-item :delete-session "Delete session…" :delete)))
      group (cond-> []
              (not archived-group?)
              (conj (menu-item :new-session "＋ New session here" :new-session))

              (and (not archived-group?) (seq selected))
              (conj
                (menu-item :move-selected (str "Move " (count selected) " selected here") :move))

              true
              (into [(menu-item :new "＋ New group…" :new-group)
                     (menu-item :rename "Rename group…" :rename)
                     (menu-item :recolour "Change group colour…" :recolour)
                     (menu-item :settings "Group settings…" :settings)
                     (if archived-group?
                       (menu-item :unarchive-group "Unarchive group" :archive)
                       (menu-item :archive-group "Archive group" :archive))
                     (menu-item :delete "✗ Delete group" :delete)]))
      (= :sessions (:set entry))
      (cond-> [(menu-item :toggle-session-archive
                          (if (:archived? entry) "Hide archived sessions" "Show archived sessions")
                          :show-archived) (menu-item :new-session "＋ New session here" :new-session)
               (menu-item :new "＋ New group…" :new-group)]
        (seq selected)
        (conj (menu-item :ungroup-selected
                         (str "Ungroup " (count selected) " selected sessions")
                         :ungroup))

        true
        (conj (menu-item :settings "Project settings…" :settings)))
      (= :groups (:set entry))
      [(menu-item :new "＋ New group…" :new-group)
       (menu-item :toggle-group-archive
                  (if (:archived? entry) "Hide archived groups" "Show archived groups")
                  :show-archived) (menu-item :settings "Project settings…" :settings)]
      :else [(menu-item :use-project "Use project" nil)
             (menu-item :new-session "＋ New session" :new-session)
             (menu-item :new "＋ New group…" :new-group)
             (menu-item :settings "Project settings…" :settings)
             (menu-item :refresh "Refresh projects" :refresh)
             (menu-item :delete-project "✗ Remove project and sessions…" :delete)])))

(def ^:private menu-kinds
  "Row kinds that own a ⋮ menu."
  #{:project-select :project-group :project-set :project-session})

(defn- focused-entry
  "The entry under the sidebar cursor, or nil on the header row."
  [db]
  (let [index (long (or (get-in db [:project-sidebar :index]) 0))]
    (when (pos? index) (nth (sidebar-entries db) (dec index) nil))))

(defn- page-size
  "Entries that PgUp and PgDn move: one screen of visible rows."
  [db]
  (max 1 (dec (count (visible-entries db (long (or (get-in db [:layout :rows]) 24)))))))

(defn char-action
  "The action of the plain character `c` on the row under the sidebar cursor.
   A key that the row's menu lists runs that item without opening the menu."
  [db c]
  (let [entry
        (focused-entry db)

        is?
        #(= c (keymap/sidebar-key %))

        session?
        (= :project-session (:kind entry))

        sid
        (some-> (get-in entry [:session "id"])
                str)

        item
        (when (menu-kinds (:kind entry)) (some #(when (= c (:key %)) %) (row-menu-items db entry)))]

    (cond (is? :search) [:search]
          (is? :select)
          (if session? [:toggle-session (str (get-in entry [:project "id"])) sid] [:noop])
          (is? :details) (if session? [:details sid] [:noop])
          (is? :add-here)
          (if (and (= :project-set (:kind entry)) (#{:groups :sessions} (:set entry)))
            [:menu (assoc entry :initial-action (if (= :groups (:set entry)) :new :new-session))]
            [:add])
          ;; `g` opens the row's own menu. Nothing to act on above the first row.
          (is? :menu) (if (menu-kinds (:kind entry)) [:menu entry] [:noop])
          (is? :add-project) [:add]
          (is? :refresh) [:refresh]
          item [:menu (assoc entry :initial-action (:id item))]
          ;; Without a row of its own, settings opens the main settings.
          (is? :settings) [:menu (assoc (or entry {}) :initial-action :settings)]
          :else [:noop])))

(defn- footer-action
  "The action of a footer button: the same as its key."
  [db command]
  (case command
    :help
    [:help]

    :back
    [:blur]

    (char-action db (keymap/sidebar-key command))))

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
                 :project-session :project-details :project-set :project-page :project-group-page
                 :project-state :project-add :project-hide :project-suggest :project-new-folder
                 :project-search-field :project-group-add :project-session-add :project-set-menu
                 :project-footer}
               (:kind hit))
            (cond
              (#{MouseActionType/SCROLL_UP MouseActionType/SCROLL_DOWN} (.getActionType mouse))
              (let [delta (if (= MouseActionType/SCROLL_UP (.getActionType mouse)) -1 1)]
                ;; While the add field is open the wheel walks its completions:
                ;; the project list is not what the rail is showing.
                (if-let [field (:adding sidebar)]
                  [:adding (add-field-move field delta)]
                  [:move delta]))
              (and (= MouseActionType/CLICK_DOWN (.getActionType mouse))
                   (= 3 (.getButton mouse))
                   (#{:project-select :project-group :project-set :project-session} (:kind hit)))
              [:menu hit]
              (and (= MouseActionType/CLICK_DOWN (.getActionType mouse)) (= 1 (.getButton mouse)))
              (case (:kind hit)
                (:project-select :project-group
                                 :project-input :project-unread
                                 :project-session :project-details
                                 :project-set :project-page
                                 :project-group-page :project-state)
                (:action hit)

                (:project-group-add :project-session-add)
                (when-let [entry (some #(when (and (= :project-set (:kind %))
                                                   (= (:project-id hit)
                                                      (str (get-in % [:project "id"])))
                                                   (= (:set %)
                                                      (if (= :project-group-add (:kind hit))
                                                        :groups
                                                        :sessions)))
                                          %)
                                       (sidebar-entries db))]
                  [:menu
                   (assoc entry
                     :initial-action (if (= :project-group-add (:kind hit)) :new :new-session))])

                :project-set-menu
                (when-let [entry (some #(when (and (= :project-set (:kind %))
                                                   (= (:project-id hit)
                                                      (str (get-in % [:project "id"])))
                                                   (= (:set hit) (:set %)))
                                          %)
                                       (sidebar-entries db))]
                  [:menu entry])

                :project-suggest
                [:add-commit (:path hit)]

                :project-new-folder
                [:add-folder]

                :project-search-field
                [:focus]

                :project-add
                [:add]

                :project-hide
                [:hide]

                :project-footer
                (footer-action db (:command hit))

                [:focus])
              :else [:noop])
            (when (and (:focused? sidebar) (= MouseActionType/CLICK_DOWN (.getActionType mouse)))
              [:blur-pass])))
        (not (:focused? sidebar)) nil
        (help-key? db key) [:help]
        (and (:adding sidebar) (.isCtrlDown key) (= \n (.getCharacter key)))
        (when-not (:saving? sidebar) [:add-folder])
        ;; C-n and C-p are Emacs next-line and previous-line: they walk the rows.
        (and (.isCtrlDown key) (not (.isAltDown key)) (#{\n \p} (.getCharacter key)))
        (key-action db
                    (KeyStroke. (if (= \n (.getCharacter key)) KeyType/ArrowDown KeyType/ArrowUp)))
        (or (.isCtrlDown key) (.isAltDown key)) nil
        (:adding sidebar) (add-field-action (:adding sidebar) key)
        (and (:search sidebar) (= KeyType/Enter (.getKeyType key)))
        (if (pos? index) (or (:action (nth (sidebar-entries db) (dec index) nil)) [:noop]) [:noop])
        (:search sidebar) (search-field-action (:search sidebar) key)
        (= KeyType/Escape (.getKeyType key)) [:blur]
        (= KeyType/Tab (.getKeyType key)) [:blur]
        (= KeyType/ArrowUp (.getKeyType key)) [:move -1]
        (= KeyType/ArrowDown (.getKeyType key)) [:move 1]
        (= KeyType/PageUp (.getKeyType key)) [:move (- (long (page-size db)))]
        (= KeyType/PageDown (.getKeyType key)) [:move (page-size db)]
        (= KeyType/Home (.getKeyType key)) [:move (- 1 index)]
        (= KeyType/End (.getKeyType key)) [:move (- (count (sidebar-entries db)) index)]
        (= KeyType/Enter (.getKeyType key))
        (if (zero? index) [:add] (or (:action (nth (sidebar-entries db) (dec index) nil)) [:noop]))
        (= KeyType/Character (.getKeyType key)) (char-action db (.getCharacter key))
        :else [:noop]))))
