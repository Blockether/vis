(ns com.blockether.vis.tui.projects
  "Left-hand project navigation. Geometry, hit targets and keyboard share one model."
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.components :as components]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.header-model :as model]
            [com.blockether.vis.tui.input :as input]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.keymap :as keymap]
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
                "unread_count" (long (or (get row "unread_count") 0))
                "stopped_count" (long (or (get row "stopped_count") 0)))
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
   the gateway's answer and moves first, so each count is the larger of the two.

   STOPPED is the part of the unread news whose newest turn stopped. Only the
   gateway knows it, so NEW paints the rest and no conversation counts twice."
  [project running waiting unread]
  (let [demand
        (long (or (get project "awaiting_count") 0))

        live
        (long (or (get project "live_count") 0))

        news
        (max (long unread) (long (or (get project "unread_count") 0)))

        stopped
        (min news (long (or (get project "stopped_count") 0)))]

    {:running (max (long running) (- live (min live demand)))
     :needs-input (max (long waiting) demand)
     :stopped stopped
     :unread (- news stopped)}))

(defn project-label
  "What a project row paints: the project's name, never its folder. An older gateway
   stored the folder path as the name, so a name that is a path paints only its last
   folder name. A blank name falls back to the folder name of the project's root."
  [project]
  (let [folder
        (fn [path]
          (let [path (str/replace (str/trim (str path)) #"[\\/]+$" "")]
            (or (not-empty (last (str/split path #"[\\/]"))) (not-empty path))))

        named
        (str/trim (str (get project "name")))]

    (cond (str/blank? named) (or (folder (get project "workspace_root")) "Untitled project")
          (re-find #"^(~|/|[A-Za-z]:[\\/])" named) (or (folder named) named)
          :else named)))

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
            (if (> n 1) (str "HITL x" n) "HITL"))
          live? "Live"
          (and (or (true? (get session "was_interrupted")) (true? (get session "was_failed")))
               (pos? unread))
          "Stopped"
          (pos? unread) (if (> unread 1) (str "New x" unread) "New")
          (= "suspended" (get session "status")) "Waiting"
          dirty? "Dirty"
          :else "Idle")))

(defn- group-counts
  "Count each active session once, without counting HITL sessions as LIVE or
   STOPPED sessions as NEW."
  [sessions current-id]
  (reduce (fn [counts session]
            (let [live?
                  (true? (get session "live"))

                  waiting?
                  (true? (get session "is_awaiting_input"))

                  unread?
                  (and (true? (get session "is_unread"))
                       (pos? (long (or (get session "unread_answers") 0)))
                       (not live?)
                       (not= current-id (str (get session "id"))))

                  ;; The row's STOPPED rule: unread news whose newest turn stopped.
                  stopped?
                  (and unread?
                       (or (true? (get session "was_interrupted"))
                           (true? (get session "was_failed"))))]

              (if (get session "archived_at")
                counts
                (cond-> counts
                  waiting?
                  (update :needs-input inc)

                  (and live? (not waiting?))
                  (update :running inc)

                  stopped?
                  (update :stopped inc)

                  (and unread? (not stopped?))
                  (update :unread inc)))))
          {:running 0 :needs-input 0 :stopped 0 :unread 0}
          (vals (into {} (map (juxt #(str (get % "id")) identity)) sessions))))

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
                status-by-group (group-by #(str (get % "group_id"))
                                          (concat (:sessions page) (:grouped page) pinned))
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
                     :in-archived-group? (some? (get group "archived_at"))
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
              [(merge {:kind :project-select
                       :project project
                       :label (project-label project)
                       :tab-count (long (or (get project "session_count") 0))
                       :expanded? expanded?
                       :action [:toggle-project pid]}
                      (project-counts project 0 0 0))]
              (when expanded?
                (concat
                  (when (seq pinned)
                    (cons {:kind :project-set :label "Attention" :project project :action [:noop]}
                          (map session-row pinned)))
                  [{:kind :project-set
                    :label (if groups-archived? "Archived groups" "Groups")
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

                                  (cons (merge {:kind :project-group
                                                :project project
                                                :group group
                                                :label (get group "name" "Untitled group")
                                                :color (get group "color")
                                                :group-count (long (or (get group "session_count")
                                                                       0))
                                                :folded? folded?
                                                :action [:toggle-group pid gid]}
                                               (group-counts (when-not (get group "archived_at")
                                                               (get status-by-group gid))
                                                             (some-> (get-in db [:session :id])
                                                                     str)))
                                        (when-not folded?
                                          (map #(assoc (session-row %) :nested? true)
                                               (get by-group gid))))))
                              groups)
                      ;; Only the archive is paged: live groups always show whole.
                      (when (and groups-archived? (pos? (long (or (:group-offset page) 0))))
                        [{:kind :project-group-page
                          :project project
                          :label "Previous groups"
                          :action [:group-page pid :previous]}])
                      (when (and groups-archived?
                                 (< (+ (long (or (:group-offset page) 0)) (count groups))
                                    (long (or (get-in sidebar [:group-total pid]) 0))))
                        [{:kind :project-group-page
                          :project project
                          :label "More groups"
                          :action [:group-page pid :next]}])
                      (when (and groups-archived?
                                 (not (:loading? page))
                                 (not (:error page))
                                 (zero? (long (or (get-in sidebar [:group-total pid]) 0))))
                        [{:kind :project-state :project project :label "No archived groups"}])))
                  [{:kind :project-set
                    :label (if archived? "Archived sessions" "Sessions")
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
                      :label "Previous sessions"
                      :action [:page pid :previous]}])
                  (when (and (not (get-in sidebar [:sessions-folded? pid])) (:has-more page))
                    [{:kind :project-page
                      :project project
                      :label "More sessions"
                      :action [:page pid :next]}])
                  (when (:loading? page)
                    [{:kind :project-state :project project :label "Loading sessions…"}])
                  (when-let [error (:error page)]
                    [{:kind :project-state :project project :label (str error "  r retry")}])
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

    (->>
      (concat
        (when (pos? offset)
          [{:kind :project-page :label "Previous search results" :action [:search-page :previous]}])
        hits
        (when has-more?
          [{:kind :project-page :label "More search results" :action [:search-page :next]}])
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
                          :stopped (:stopped counts)
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

(defn- list-top
  "The screen row of the first sidebar row: under the search field while one is
   open, else right under the header divider."
  ^long [db]
  (if (get-in db [:project-sidebar :search]) 4 3))

(defn- list-capacity
  "How many one-line rows fit between the list top and the footer divider."
  ^long [db rows]
  (max 0 (- (long rows) (list-top db) (if (get-in db [:project-sidebar :error]) 4 3))))

(defn visible-entries
  "Scroll the cursor's row into view, retaining the project above a clipped child."
  [db rows]
  (let [entries
        (sidebar-entries db)

        capacity
        (list-capacity db rows)

        index
        (max 0 (long (or (get-in db [:project-sidebar :index]) 0)))

        end
        (min (count entries) (max capacity index))

        visible
        (subvec entries (max 0 (- end capacity)) end)]

    (if (and (seq visible)
             (not (get-in db [:project-sidebar :search]))
             (> capacity 1)
             (not= :project-select (:kind (first visible))))
      (let [tail
            (subvec entries (max 0 (- end (dec capacity))) end)

            parent
            (first (filter #(and (= :project-select (:kind %))
                                 (= (:project (first tail)) (:project %)))
                           entries))]

        (if (and (seq tail) parent (not= parent (first tail))) (into [parent] tail) visible))
      visible)))

(defn entry-row
  "The screen row of the visible entry at sidebar `index`, or nil when that entry
   is scrolled away or the add field hides the list. Mirrors `paint!`: every
   entry takes one row, from `list-top` down."
  [db rows index]
  (when-not (get-in db [:project-sidebar :adding])
    (some (fn [[entry row]]
            (when (= index (:index entry)) row))
          (map vector (visible-entries db rows) (iterate inc (list-top db))))))

(defn- row-bg
  "Tint the focused, selected and current rows a little. Every other row keeps
   the rail's paper, so its text keeps the theme's own contrast."
  [entry focused? active?]
  (cond focused? (t/mix-color t/terminal-bg t/header-active-tab-bg 0.14)
        (:selected? entry) (t/mix-color t/terminal-bg t/header-active-tab-bg 0.16)
        (and active? (= :project-session (:kind entry)))
        (t/mix-color t/terminal-bg t/header-active-tab-bg 0.10)
        :else t/terminal-bg))

(defn- paint-marker!
  "Paint the focus bar in the row's first cell. A dark theme fills the cell, as
   a thin glyph on a dark tint is hard to find."
  [g col row ink]
  (if (= :dark (:mode t/default-theme))
    (do (p/set-colors! g ink ink) (p/put-str! g col row " "))
    (do (p/set-colors! g (t/legible-ink ink t/dialog-bg) t/dialog-bg) (p/put-str! g col row "┃"))))

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

(defn- status-word
  "The sidebar's lowercase word for a session status, or nil for an idle session."
  [status]
  (case (first (str/split (str/upper-case (str/trim (str status))) #"\s+"))
    ("HITL" "WAITING")
    "waiting"

    "LIVE"
    "running"

    "STOPPED"
    "stopped"

    "NEW"
    (if-let [[_ n] (re-find #"(?i)\sx(\d+)$" (str status))]
      (str n " new")
      "new")

    "DIRTY"
    "draft"

    "ARCHIVED"
    "archived"

    nil))

(def ^:private counted-statuses
  "Each counted state with its word and status ink, most urgent first. A narrow
   rail drops whole parts from the end."
  [[:needs-input "waiting" "HITL"] [:stopped "stopped" "STOPPED"] [:running "running" "LIVE"]
   [:unread "new" "NEW"]])

(defn- count-parts
  "The `[text ink]` parts of a project or group's counted states, most urgent first."
  [entry]
  (keep (fn [[key word status]]
          (let [n (long (or (get entry key) 0))]
            (when (pos? n) [(str n " " word) (dlg/session-status-ink status)])))
        counted-statuses))

(defn- row-status-parts
  "The `[text ink]` parts at the right of a row, most important first. A nil ink
   is the quiet hint ink."
  [entry sidebar]
  (case (:kind entry)
    :project-group
    (count-parts entry)

    :project-input
    [["waiting" (dlg/session-status-ink "HITL")]]

    :project-unread
    [["new" (dlg/session-status-ink "NEW")]]

    :project-session
    (when-let [word (status-word (:status entry))]
      [[word (dlg/session-status-ink (:status entry))]])

    (:project-set :project-page :project-group-page :project-state)
    []

    (let [{:keys [tab-count project]}
          entry

          sessions?
          (= :toggle-project (first (:action entry)))

          counts
          (count-parts entry)]

      (cond (= (get project "id") (:removing sidebar)) [[(or (:progress sidebar) "Removing…") nil]]
            :else (concat (when (= (get project "id") (:opening sidebar)) [["Loading…" nil]])
                          (if (seq counts)
                            counts
                            [[(str tab-count
                                   (if (= 1 tab-count)
                                     (if sessions? " session" " tab")
                                     (if sessions? " sessions" " tabs"))) nil]]))))))

(defn- parts-width
  "The columns that `parts` take, two spaces apart."
  ^long [parts]
  (+ (long (reduce + 0 (map #(long (p/display-width (first %))) parts)))
     (* 2 (max 0 (dec (count parts))))))

(defn- fit-parts
  "The leading parts that fit `available` columns whole. A part never loses words."
  [parts available]
  (loop [parts
         (seq parts)

         kept
         []]

    (let [more (conj kept (first parts))]
      (if (and parts (<= (parts-width more) (long available))) (recur (next parts) more) kept))))

(defn- paint-parts!
  "Paint `[text ink]` parts from `col`, two spaces apart, on the row's paper."
  [g parts col row]
  (reduce (fn [col [text ink]]
            (p/set-colors! g (t/legible-ink (or ink t/dialog-hint) t/dialog-bg) t/dialog-bg)
            (p/put-str! g col row text)
            (+ (long col) (long (p/display-width text)) 2))
          (long col)
          parts))

(defn- session-meta
  "What the focused session row adds before its status: when it last changed
   and how many turns it has, longest first."
  [entry]
  (let [turns
        (long (or (:turns entry) 0))

        turns
        (when (pos? turns) (str turns (if (= 1 turns) " turn" " turns")))

        changed
        (session-time-label (:modified-at entry))]

    (remove str/blank? [(str/join "  " (remove nil? [changed turns])) turns changed])))

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
                                                 (if-let [project (:moving field)]
                                                   (str "New folder for " (project-label project))
                                                   "/absolute/directory"))]
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
            (if (get-in db [:project-sidebar :saving?]) "Creating folder…" "New folder  Ctrl+N")
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
        (:adding sidebar) [["Enter" (if (get-in sidebar [:adding :moving]) "move" "add")]
                           ["Esc" "cancel"] ["Tab" "complete"] ["C-n" "folder"]]
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
        "   "

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
          (p/put-str! g (+ col (long (p/display-width k))) row (str " " a))
          (p/set-colors! g t/dialog-hint t/dialog-bg)
          (when (seq more) (p/put-str! g (+ col w) row sep))
          (recur (+ col w sep-w) more))))))

(defn- row-indent
  "The columns before a row's text: projects at the edge, sections one step in,
   groups under their section and sessions as list items under them."
  ^long [entry]
  (case (:kind entry)
    :project-select
    0

    :project-set
    2

    (:project-group :project-input :project-unread)
    4

    :project-session
    (if (:nested? entry) 8 6)

    4))

(defn- list-row?
  "True for a row that names one session. It starts with a list bullet: `●` for
   the current session, `*` for a favourite, else `-`."
  [entry]
  (contains? #{:project-session :project-input :project-unread} (:kind entry)))

(defn- row-lead
  "The `[text ink bold?]` parts left of a row's status. The last part is the
   name, which gives way first."
  [entry sidebar]
  (let [fold
        (if (:folded? entry) "▸ " "▾ ")

        hint
        (t/legible-ink t/dialog-hint t/dialog-bg)

        label
        (str (:label entry))]

    (case (:kind entry)
      :project-select
      [[(str (if (contains? sidebar :pages) (if (:expanded? entry) "▾ " "▸ ") "  ") label)
        t/dialog-fg true]]

      :project-set
      [[(str (if (#{:groups :sessions} (:set entry)) fold "  ") label) hint false]]

      :project-group
      [[fold hint false] ["■ " (t/group-ink (:color entry)) false] [label t/dialog-fg false]]

      (:project-page :project-group-page :project-state)
      [[label hint false]]

      [[label t/dialog-fg false]])))

(defn- add-choice
  "The menu item that `+` runs on `entry`: `:new` on the Groups row, `:new-session`
   on a row where a session can start, else nil. Every such row offers the same `+`."
  [entry]
  (case (:kind entry)
    :project-set
    (case (:set entry)
      :groups
      :new

      :sessions
      :new-session

      nil)

    :project-group
    (when-not (get-in entry [:group "archived_at"]) :new-session)

    :project-session
    (when-not (:in-archived-group? entry) :new-session)

    nil))

(defn- row-tools
  "Paint the plain `+` button of a focused or hovered Groups or Sessions row. Return
   the columns that it takes at the row's right edge. Session and group rows show no
   button: `g` opens their menu."
  [g entry left width row]
  (if-let [choice (when (= :project-set (:kind entry)) (add-choice entry))]
    (do (components/button! g
                            (- (+ (long left) (long width)) 4)
                            row
                            " + "
                            (if (= :new choice) :project-group-add :project-session-add)
                            {:plain? true :extra {:index (:index entry)}})
        4)
    2))

(defn- paint-row!
  "Paint one sidebar row: a focus bar, the indented name and the statuses that
   fit whole at the right. Only a focused or hovered row shows its buttons and
   a session's turns."
  [g entry sidebar {:keys [left width row focused? hot? active?]}]
  (let [left
        (long left)

        width
        (long width)

        row
        (long row)

        start
        (+ left 2 (row-indent entry))

        session?
        (= :project-session (:kind entry))]

    (binding [t/dialog-bg (row-bg entry focused? active?)]
      (p/clear-styles! g)
      (p/set-colors! g t/dialog-fg t/dialog-bg)
      (p/fill-rect! g (inc left) row (max 0 (- width 2)) 1)
      (cond (or focused? (and session? active? (not (:focused? sidebar))))
            (paint-marker! g (inc left) row t/header-active-tab-accent)
            (:selected? entry) (paint-marker! g (inc left) row t/dialog-hint))
      (when (and (list-row? entry) (> start (+ left 3)))
        (p/set-colors! g
                       (t/legible-ink (if active? t/header-active-tab-accent t/dialog-hint)
                                      t/dialog-bg)
                       t/dialog-bg)
        (p/put-str! g
                    (- start 2)
                    row
                    (cond active? "●"
                          (:favorite? entry) "*"
                          :else "-")))
      (let [end (- (+ left width) (if hot? (long (row-tools g entry left width row)) 2))
            lead (row-lead entry sidebar)
            name-col
            (+ start (long (reduce + 0 (map #(long (p/display-width (first %))) (butlast lead)))))
            label (first (last lead))
            room (- end name-col (min 10 (long (p/display-width label))) 1)
            status (fit-parts (row-status-parts entry sidebar) room)
            meta (when (and session? hot?)
                   (first (filter #(<= (+ (long (p/display-width %))
                                          (if (seq status) 2 0)
                                          (parts-width status))
                                       room)
                                  (session-meta entry))))
            parts (cond->> status
                    meta
                    (into [[meta nil]]))
            parts-col (- end (parts-width parts))]

        (reduce (fn [col [text ink bold?]]
                  (let [text (if (= text label)
                               (dlg/ellipsize text (max 0 (- parts-col (long col) 1)))
                               text)]
                    (p/set-colors! g ink t/dialog-bg)
                    (p/styled g (if bold? [p/BOLD] []) (p/put-str! g col row text))
                    (+ (long col) (long (p/display-width text)))))
                start
                lead)
        (paint-parts! g parts parts-col row)))))

(defn paint!
  "Use the main view's three-row header, bordered container and inset footer.
   Each project, section, group and session takes one row.
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
            visible (if (:adding sidebar) [] (visible-entries db rows))
            hovered (.hovered interactions/hit-map)
            top (list-top db)]

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
                              :project-add {:plain? true
                                            :accent? (and (:focused? sidebar)
                                                          (zero? (long (or (:index sidebar) 0))))})
          (components/button! g (- (+ left width) 5) 1 " ✕ " :project-hide {:plain? true}))
        (doseq [[{:keys [index project kind] :as entry} row] (map vector visible (iterate inc top))
                :let [row (long row)
                      active? (case kind
                                :project-session
                                (= (str (get-in entry [:session "id"]))
                                   (str (get-in db [:session :id])))

                                :project-select
                                (= (str (get project "id")) (:active-project-id db))

                                (and (:tab-id entry) (= (:tab-id entry) (:active-tab-id db))))
                      focused? (and (:focused? sidebar) (= index (:index sidebar)))
                      hot? (or focused?
                               (and (= row (get-in hovered [:bounds :row]))
                                    (not= :project-rail (:kind hovered))))]]

          (.register interactions/hit-map
                     (assoc entry
                       :bounds {:col (inc left) :row row :width (max 0 (- width 2)) :height 1}))
          (paint-row!
            g
            entry
            sidebar
            {:left left :width width :row row :focused? focused? :hot? hot? :active? active?}))
        (when (and (empty? (:items sidebar)) (not (:adding sidebar)) (> rows 8))
          (p/set-colors! g t/dialog-hint t/dialog-bg)
          (p/put-str! g
                      (+ left 2)
                      top
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
  "The row menu of a sidebar `entry`, each verb with the key that runs it. The keys
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
      (cond-> (if (:in-archived-group? entry)
                [(menu-item :new "New group…" :new-group)]
                [(menu-item :new-session "New session here" :new-session)
                 (menu-item :new "New group…" :new-group)])
        true
        (into [(menu-item :details "Show session details" :details)
               (menu-item :toggle-session
                          (if (contains? selected sid) "Deselect session" "Select session")
                          :select)
               (menu-item
                 :move-session
                 (if (> moving 1) (str "Move " moving " selected sessions…") "Move to group…")
                 :move)])

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
      group
      (cond-> []
        (not archived-group?)
        (conj (menu-item :new-session "New session here" :new-session))

        (and (not archived-group?) (seq selected))
        (conj (menu-item :move-selected (str "Move " (count selected) " selected here") :move))

        true
        (into [(menu-item :new "New group…" :new-group) (menu-item :rename "Rename group…" :rename)
               (menu-item :recolour "Change group colour…" :recolour)
               (menu-item :settings "Group settings…" :settings)
               (if archived-group?
                 (menu-item :unarchive-group "Unarchive group" :archive)
                 (menu-item :archive-group "Archive group" :archive))
               (menu-item :delete "Delete group" :delete)]))
      (= :sessions (:set entry))
      (cond-> [(menu-item :toggle-session-archive
                          (if (:archived? entry) "Hide archived sessions" "Show archived sessions")
                          :show-archived) (menu-item :new-session "New session here" :new-session)
               (menu-item :new "New group…" :new-group)]
        (seq selected)
        (conj (menu-item :ungroup-selected
                         (str "Ungroup " (count selected) " selected sessions")
                         :ungroup))

        true
        (conj (menu-item :settings "Project settings…" :settings)))
      (= :groups (:set entry))
      [(menu-item :new "New group…" :new-group)
       (menu-item :toggle-group-archive
                  (if (:archived? entry) "Hide archived groups" "Show archived groups")
                  :show-archived) (menu-item :settings "Project settings…" :settings)]
      :else [(menu-item :use-project "Use project" nil)
             (menu-item :new-session "New session" :new-session)
             (menu-item :new "New group…" :new-group)
             (menu-item :rename-project "Rename project…" :rename)
             (menu-item :move-project "Change folder…" nil)
             (menu-item :settings "Project settings…" :settings)
             (menu-item :refresh "Refresh projects" :refresh)
             (menu-item :delete-project "Remove project and sessions…" :delete)])))

(def ^:private menu-kinds
  "Row kinds that own a row menu."
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
          (is? :add-here) (if-let [choice (add-choice entry)]
                            [:menu (assoc entry :initial-action choice)]
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

(defn- fold-state
  "`:open` or `:closed` for a sidebar row that folds, else nil."
  [entry]
  (cond (and (= :project-select (:kind entry)) (contains? entry :expanded?))
        (if (:expanded? entry) :open :closed)
        (and (#{:project-set :project-group} (:kind entry)) (contains? entry :folded?))
        (if (:folded? entry) :closed :open)))

(defn- fold-depth
  "How deep a sidebar row sits under its project row."
  ^long [entry]
  (case (:kind entry)
    :project-select
    0

    :project-set
    1

    :project-session
    (if (:nested? entry) 3 2)

    2))

(defn- fold-key-action
  "Right opens a folded row. Left folds an open row, or moves to the fold row
   that holds the selection and folds it, the same as Settings sections."
  [db index direction]
  (let [entries
        (sidebar-entries db)

        entry
        (nth entries (dec (long index)) nil)

        state
        (fold-state entry)]

    (cond (nil? entry) [:noop]
          (= :right direction) (if (= :closed state) (:action entry) [:noop])
          (= :open state) (:action entry)
          :else (if-let [at (some (fn [at]
                                    (let [row (nth entries at)]
                                      (when (and (< (fold-depth row) (fold-depth entry))
                                                 (= :open (fold-state row)))
                                        at)))
                                  (range (- (long index) 2) -1 -1))]
                  [:fold-parent (- (inc (long at)) (long index))]
                  [:noop]))))

(def ^:private rail-kinds
  "Hit kinds that belong to the rail, so a mouse event on them is a sidebar event."
  #{:project-rail :project-select :project-group :project-input :project-unread :project-session
    :project-set :project-page :project-group-page :project-state :project-add :project-hide
    :project-suggest :project-new-folder :project-search-field :project-group-add
    :project-session-add :project-footer})

(defn click-focus
  "Return the sidebar change for a mouse press in the rail, or nil for other events.
   The press gives the rail keyboard focus and selects the row under the pointer."
  [db key]
  (when (and (instance? MouseAction key)
             (get-in db [:project-sidebar :open?])
             (= MouseActionType/CLICK_DOWN (.getActionType ^MouseAction key))
             (#{1 3} (.getButton ^MouseAction key)))
    (let [pos
          (.getPosition ^MouseAction key)

          hit
          (.lookup hit-map (.getColumn pos) (.getRow pos))]

      (when (rail-kinds (:kind hit))
        (cond-> {:focused? true}
          (integer? (:index hit))
          (assoc :index (:index hit)))))))

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

          (if (rail-kinds (:kind hit))
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
                (:project-select :project-group :project-input
                                 :project-unread :project-session
                                 :project-set :project-page
                                 :project-group-page :project-state)
                (:action hit)

                (:project-group-add :project-session-add)
                (when-let [entry (some #(when (= (:index hit) (:index %)) %) (sidebar-entries db))]
                  [:menu
                   (assoc entry
                     :initial-action (if (= :project-group-add (:kind hit)) :new :new-session))])

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
        (= KeyType/ArrowLeft (.getKeyType key)) (fold-key-action db index :left)
        (= KeyType/ArrowRight (.getKeyType key)) (fold-key-action db index :right)
        (= KeyType/Enter (.getKeyType key))
        (if (zero? index) [:add] (or (:action (nth (sidebar-entries db) (dec index) nil)) [:noop]))
        (= KeyType/Character (.getKeyType key)) (char-action db (.getCharacter key))
        :else [:noop]))))
