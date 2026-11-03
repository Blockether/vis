(ns com.blockether.vis.tui.app-surfaces
  "Where an app of the session shows. The engine sends the EFFECTIVE `surfaces` of
   each app view: the user settings are already applied. An absent field means the
   schema defaults.

   - `inline`: the turn that opened the app shows its frame. Else the turn shows one
     compact row.
   - `session`: the session apps picker opens the app in the side panel (`sidebar`)
     or over the chat area (`chat`). `off` keeps the app out of the picker."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.app :as app]))

(set! *warn-on-reflection* true)

(set! *unchecked-math* :warn-on-boxed)

(defn surfaces
  "The surfaces of app `view`: its own value over the schema defaults."
  [view]
  (merge app/surface-defaults (select-keys (:surfaces view) [:inline :session])))

(defn inline?
  "True when the turn that opened app `view` shows the app frame."
  [view]
  (true? (:inline (surfaces view))))

(defn session-target
  "Where the session apps picker opens app `view`: `:sidebar`, `:chat`, or nil for `off`.
   An unknown value takes the schema default, as the engine does."
  [view]
  (let [session
        (str (:session (surfaces view)))

        session
        (if (some #{session} app/session-surfaces) session (:session app/surface-defaults))]

    (case session
      "sidebar"
      :sidebar

      "chat"
      :chat

      nil)))

(defn- by-title
  [views]
  (vec (sort-by (juxt #(str/lower-case (str (:title %))) #(str (:id %))) views)))

(defn- open-view?
  "True when `view` names an app that did not close."
  [view]
  (and (not (str/blank? (str (:id view)))) (not (:is-closed view))))

(defn session-apps
  "The app views of `app-views` (the tab's `{view-id {:view v}}`) that the session apps
   picker lists, by title. A closed app and an app with `session` `off` are not listed."
  [app-views]
  (->> (vals app-views)
       (keep :view)
       (filter #(and (open-view? %) (session-target %)))
       by-title))

(defn global-apps
  "The global app views of `global-apps` (`{view-id {:view v}}`) that the global apps
   picker lists, by title. A closed app is not listed."
  [global-apps]
  (->> (vals global-apps)
       (keep :view)
       (filter open-view?)
       by-title))

(defn open-label
  "The button of attachment row `row` in the turn: an app opens in full screen unless its
   effective `inline` surface is false, and every other row opens."
  [row]
  (if (and (= app/media-type (:media-type row)) (not (false? (:inline row))))
    " Full screen "
    " Open "))

(defn fleet-app-frame
  "What fleet frame `event` does to the global apps. A frame of a global app has no
   `session_id`. The stream keeps no replay, so a new connection reads the list again.
   Returns `[:refetch]`, `[:open wire-view]`, `[:patch wire-patch first-seq]`,
   `[:close view-id]` or nil."
  [event]
  (let [type (get event "type")]
    (cond (= "gateway.connected" type) [:refetch]
          (not (str/blank? (str (get event "session_id")))) nil
          (not= "live" (get event "kind")) nil
          (= "view.open" type) (when-let [view (get event "view")]
                                 [:open view])
          (= "view.patch" type) (when-let [patch (get event "patch")]
                                  [:patch patch (get event "first_seq")])
          (= "view.close" type) (let [id (str (get event "view_id"))]
                                  (when-not (str/blank? id) [:close id])))))

(def ^:private target-label {:sidebar "side panel" :chat "chat area"})

(defn picker-items
  "The rows of the session apps picker for `views`: the title, where the app opens and
   its state."
  [views status-label]
  (mapv (fn [view]
          {:label (or (not-empty (str (:title view))) "App")
           :description (str "Opens in the " (target-label (session-target view)))
           :hint (status-label view)
           :view-id (str (:id view))})
        views))

(defn chat-band
  "The screen columns `{:left :width}` of the chat pane `{:left :cols}`: a `chat`
   session app fills them."
  [chat]
  {:left (long (:left chat)) :width (long (:cols chat))})

(defn panel-content
  "What the side panel shows: `:app` while its app pane names an app of `app-views`,
   `:views` while live views use the panel, else nil."
  [live-panel app-views panes]
  (when (:open? live-panel)
    (cond (contains? app-views (get-in live-panel [:app :view-id])) :app
          (seq panes) :views
          :else nil)))

(defn pane-pick
  "What a pick of `sidebar` app `view` does: `:close` while the side pane shows it, else `:open`."
  [live-panel view]
  (if (and (:open? live-panel) (= (str (:id view)) (get-in live-panel [:app :view-id])))
    :close
    :open))

(defn pane-click
  "Where a click at `col` and `row` lands in the side pane `box` `{:left :top :width
   :height}`: nil outside it, else `{:row i}` with the surface row `i` under the click,
   or `{:row nil}` off the rows. `geom` holds `:content-top` and `:list-h`."
  [box geom scroll row-count col row]
  (when box
    (let [{:keys [left top width height]}
          box

          col
          (long col)

          row
          (long row)

          content-top
          (long (:content-top geom))

          i
          (+ (long scroll) (- row content-top))]

      (when (and (<= (long left) col)
                 (< col (+ (long left) (long width)))
                 (<= (long top) row)
                 (< row (+ (long top) (long height))))
        {:row (when (and (<= content-top row)
                         (< row (+ content-top (long (:list-h geom))))
                         (< i (long row-count)))
                i)}))))

(defn pane-focus-after
  "The area that holds the keyboard after the app in the side pane answered a key with
   `result`: the chat when the app closes on that key (Esc), else the pane."
  [result]
  (if (and (map? result) (contains? result :com.blockether.vis.tui.dialogs/done)) :chat :panel))

(defn band-width
  "The columns of `band` that fit a `cols`-wide terminal: a resize after the app opened
   can make the terminal narrower than the band."
  ^long [band cols]
  (max 1 (min (long (:width band)) (- (long cols) (long (:left band))))))

(defn preview-lines
  "Up to `n` plain lines of what app `view` shows, for its frame in the turn. Each line
   is the text of one node, in tree order."
  [view n]
  (->> (tree-seq #(seq (:children %)) :children {:children (:nodes view)})
       rest
       (keep (fn [node]
               (some->> [:label :title :text :value :body]
                        (keep #(let [v (get node %)] (when (string? v) v)))
                        first
                        str/split-lines
                        first
                        str/trim
                        not-empty)))
       (take (long n))
       vec))

(defn picker-state
  "Whether an app picker is blocked, empty, or ready. Session pickers need a session."
  [dialog-open? views session-required? session-id]
  (cond dialog-open? :blocked
        (or (empty? views) (and session-required? (nil? session-id))) :empty
        :else :ready))

(defn picked-target
  "The effect of a session app pick: close its pane, open its pane, or cover the chat."
  [live-panel view]
  (if (= :sidebar (session-target view))
    (if (= :close (pane-pick live-panel view)) :close :pane)
    :chat))
