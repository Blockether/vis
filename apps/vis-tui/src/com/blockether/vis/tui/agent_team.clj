(ns com.blockether.vis.tui.agent-team
  "Managed team inspection through the same gateway as the conversation.

   The cached team snapshot also feeds two surfaces: the team card under the turn that
   started a team and the breadcrumb of a child session. None of them fetches during
   painting."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.theme :as t])
  (:import [com.googlecode.lanterna.graphics TextGraphics]
           [com.googlecode.lanterna.input KeyStroke KeyType]
           [java.net URLEncoder]
           [java.nio.charset StandardCharsets]))

(defn- path
  [sid suffix]
  (str "/v1/sessions/" (URLEncoder/encode (str sid) StandardCharsets/UTF_8) "/agents" suffix))

(defonce ^:private snapshots (atom {}))

(defn- roles
  "The subagent roles of the agents contract, in its order."
  []
  (get-in (document/schema-document "agents") ["$defs" "role" "oneOf"]))

(defn role-name
  "The contract role of `agent`, such as `implement`, or nil for no role or an unknown role. The
   TUI writes the role as a word: role glyphs come from a fallback font in many terminal fonts
   and do not line up with the text."
  [agent]
  (some #(when (= (get % "const") (get agent "role")) (get % "const")) (roles)))

(defn waiting?
  "Whether `agent` waits for a person: a form in its own session, or an open question, budget
   request or file request to its parent."
  [agent]
  (boolean (or (get agent "pending_input")
               (get agent "question")
               (get agent "budget_request")
               (get agent "files_request"))))

(defn- active? [agent] (or (waiting? agent) (contains? #{"queued" "running"} (get agent "status"))))

(defn- role-rank
  [agent]
  (or (first (keep-indexed #(when (= (get %2 "const") (get agent "role")) %1) (roles)))
      (count (roles))))

(defn numbered
  "`[n agent]` pairs. `n` is the start order from 1 inside the agent's team (`team_id`), so it
   never changes. Agents that wait for a person come first. Then rows of one role stand
   together in contract order, and a row without a role comes last."
  [agents]
  (let [numbers (second (reduce (fn [[seen out] agent]
                                  (let [team (get agent "team_id")
                                        n (inc (long (get seen team 0)))]

                                    [(assoc seen team n) (conj out n)]))
                                [{} []]
                                agents))]
    (sort-by (fn [[_ agent]]
               [(if (waiting? agent) 0 1) (role-rank agent)])
             (map vector numbers agents))))

(defn summary
  "Cached team snapshot; never performs network work during painting."
  [sid]
  (get @snapshots sid))

(defn enabled?
  "Whether the last gateway check enabled Subagents; never fetch during painting."
  [sid]
  (true? (:enabled? (summary sid))))

(defn fetch!
  [sid]
  (let [enabled?
        ;; Subagents accepts project, group and session overrides, and the session's
        ;; turns use its resolved value (issue #311). The global value can differ.
        (try (true? (get (vis/setting "subagents" {:scope "session" :target-id (str sid)})
                         "enabled"))
             (catch Exception _ false))

        result
        (if enabled?
          (try (let [response (vis/request! :get (path sid "") {:timeout-ms 2000})]
                 (if (= 200 (:status response))
                   (let [rows (wire/parse-json (:body response))]
                     (if (and (vector? rows)
                              (every? #(and (map? %)
                                            (string? (get % "session_id"))
                                            (string? (get % "task"))
                                            (string? (get % "status")))
                                      rows))
                       {:agents rows}
                       {:error "Invalid team response. Refresh to retry."}))
                   {:error "Could not load team. Refresh to retry."}))
               (catch Exception _ {:error "Could not load team. Refresh to retry."}))
          {:agents []})

        result
        (assoc result :enabled? enabled?)]

    (swap! snapshots assoc sid (assoc result :checked-at (System/currentTimeMillis)))
    result))

(defn header-label
  "Make unknown, failed and cached observations explicit in the header."
  [sid child?]
  (let [{:keys [agents error checked-at]}
        (summary sid)

        label
        (if child? "Subagent" "Agents")]

    (cond error (str label " !")
          (nil? checked-at) (str label " ?")
          (> (- (System/currentTimeMillis) (long checked-at)) 10000) (str label " stale")
          :else (str label
                     " " (count (filter #(contains? #{"queued" "running"} (get % "status")) agents))
                     " active/" (count agents)
                     " cached" (when (some #(get % "pending_input") agents) " !")))))

;; ── Team rows ───────────────────────────────────────────────────────────────

(def ^:private ^:const title-width
  "The most cells that the title column takes. A longer title ends with an ellipsis."
  40)

(def ^:private ^:const time-width "Cells of the time column, enough for `59m 59s`." 7)

(def ^:private ^:const narrow-width
  "Below this width a row with a now or result text takes two lines."
  64)

(def ^:private ^:const dock-max-lines
  "The most dock lines above the prompt. One more line counts the hidden rows."
  8)

(defn- one-line [text] (str/trim (str/replace (str text) #"\s+" " ")))

(defn agent-label
  "The title of `agent`, or `#n` when it has no title. `n` is its start order in its team."
  [n agent]
  (or (not-empty (one-line (get agent "title"))) (str "#" n)))

(defn status-word
  "The status of `agent` in words: `queued`, `running`, `asking`, `needs you`, `done`,
   `passed`, `failed`, `cancelled` or `out of budget`."
  [agent]
  (cond (get agent "pending_input") "needs you"
        (waiting? agent) "asking"
        :else (case (get agent "status")
                "queued"
                "queued"

                "running"
                "running"

                "completed"
                (case (get agent "outcome")
                  "passed"
                  "passed"

                  "failed"
                  "failed"

                  "done")

                "failed"
                "failed"

                "cancelled"
                "cancelled"

                "budget_limited"
                "out of budget"

                (str/replace (str (get agent "status")) "_" " "))))

(defn- tone
  [word]
  (case word
    ("asking" "needs you" "out of budget")
    :attention

    ("done" "passed")
    :ok

    "failed"
    :bad

    "cancelled"
    :cancelled

    "running"
    :running

    :muted))

(defn- request-line
  [agent]
  (cond (get agent "question") (one-line (get agent "question"))
        (get agent "budget_request")
        (str "Asks for " (get-in agent ["budget_request" "iterations"]) " more iterations")
        (get agent "files_request") "Asks for file access"
        (get agent "pending_input") "Waits for your input in its own session"))

(defn- now-text
  "What the agent does now while it works, its request while it waits, else its result."
  [agent]
  (one-line
    (or (request-line agent) (when (active? agent) (get agent "activity")) (get agent "result"))))

(defn- start-ms
  [agent]
  (let [created
        (get agent "created_at")

        resumed
        (get agent "resumed_at")]

    (cond (and (number? created) (number? resumed)) (max (long created) (long resumed))
          (number? resumed) (long resumed)
          (number? created) (long created))))

(defn team-row
  "The row data of agent `agent` with start order `n`. Plain data, so an unchanged agent
   compares equal across polls. The painter computes the running time."
  [n agent]
  (let [word (status-word agent)]
    {:n n
     :team-id (get agent "team_id")
     :session-id (get agent "session_id")
     :role (or (role-name agent) "")
     :label (agent-label n agent)
     :status word
     :tone (tone word)
     :now (now-text agent)
     :active? (active? agent)
     :pending-input? (boolean (get agent "pending_input"))
     :start (start-ms agent)
     :end (when (and (not (active? agent)) (number? (get agent "finished_at")))
            (long (get agent "finished_at")))}))

(defn team-rows
  "The rows of team `team-id` from `agents`, in start order, so a row never moves."
  [team-id agents]
  (->> (numbered agents)
       (filter #(= team-id (get (second %) "team_id")))
       (sort-by first)
       (mapv (fn [[n agent]]
               (team-row n agent)))))

(defn duration-text
  "`12s`, `3m 04s` or `1h 02m`, or nil for a negative or unknown span."
  [ms]
  (when (and (number? ms) (not (neg? (long ms))))
    (let [s (quot (long ms) 1000)]
      (cond (< s 60) (str s "s")
            (< s 3600) (format "%dm %02ds" (quot s 60) (rem s 60))
            :else (format "%dh %02dm" (quot s 3600) (quot (rem s 3600) 60))))))

(defn row-time
  "The time of a row: the elapsed time while the agent works or waits, the duration of its
   task when it ended, nil while queued or without times."
  [{:keys [start end active? status]} now-ms]
  (cond (nil? start) nil
        end (duration-text (- (long end) (long start)))
        (and active? (not= "queued" status)) (duration-text (- (long now-ms) (long start)))))

(defn- widest [texts] (reduce max 0 (map #(long (p/display-width (str %))) texts)))

(defn row-columns
  "The cell widths of the role, title and status columns, so every row lines up."
  [rows]
  {:role (widest (map :role rows))
   :label (min (long title-width) (long (widest (map :label rows))))
   :status (widest (map :status rows))})

(defn- pad
  "`text` with spaces to `width` cells."
  [text width]
  (let [text (str text)]
    (str text (apply str (repeat (max 0 (- (long width) (long (p/display-width text)))) \space)))))

(defn narrow?
  "Whether rows at `width` cells take two lines when they have a now or result text."
  [width]
  (< (long width) (long narrow-width)))

(defn row-line-count
  "Lines of `row` at `width` cells: two on a narrow screen with a role or a now or result text."
  ^long [row width]
  (if (and (narrow? width) (or (seq (:role row)) (seq (:now row)))) 2 1))

(defn row-segments
  "The painted parts of `row` at `width` cells: one vector of `[col text part]` for each line.
   `part` is `:role`, `:label`, `:status`, `:now` or `:time`."
  [row columns width now-ms]
  (let [width
        (long width)

        columns
        (or columns (row-columns [row]))

        time
        (or (row-time row now-ms) "")

        time-col
        (- width (long time-width))

        two-lines?
        (= 2 (row-line-count row width))

        ;; On a narrow screen the role moves to the second line, so the title gets the room.
        role-w
        (if (narrow? width) 0 (long (:role columns)))

        label-col
        (if (pos? role-w) (+ role-w 2) 0)

        ;; A narrow screen gives the title only the room that the status and time leave.
        label-w
        (max 4 (min (long (:label columns)) (- time-col 1 label-col 2 (long (:status columns)))))

        status-col
        (+ label-col label-w 2)

        now-col
        (+ status-col (long (:status columns)) 2)

        head
        (cond-> []
          (pos? role-w)
          (conj [0 (:role row) :role])

          true
          (conj [label-col (p/ellipsize (:label row) label-w) :label]
                [status-col (:status row) :status]))

        time-part
        [(+ time-col (- (long time-width) (long (p/display-width time)))) time :time]]

    (if two-lines?
      (let [role
            (:role row)

            now-at
            (if (seq role) (+ 2 (long (p/display-width role)) 2) 2)]

        [(conj head time-part)
         (cond-> []
           (seq role)
           (conj [2 role :role])

           (and (seq (:now row)) (< now-at width))
           (conj [now-at (p/ellipsize (:now row) (max 1 (- width now-at))) :now]))])
      [(cond-> head
         (and (seq (:now row)) (> (- time-col now-col 2) 3))
         (conj [now-col (p/ellipsize (:now row) (- time-col now-col 2)) :now])

         (> time-col (+ status-col (long (:status columns))))
         (conj time-part))])))

(defn segments-text
  "Plain text of one painted line."
  [segments]
  (str/trimr (reduce (fn [line [col text]]
                       (str (pad line col) text))
                     ""
                     (sort-by first segments))))

(defn row-lines
  "Plain lines of `row` at `width` cells, as copy shows them."
  [row columns width now-ms]
  (mapv segments-text (row-segments row columns width now-ms)))

(defn- part-fg
  [part tone-key]
  (case part
    :role
    t/dialog-hint

    :label
    t/code-block-fg

    :now
    t/dialog-hint

    :time
    t/dialog-hint

    :status
    (case tone-key
      :attention
      t/warning-fg

      :ok
      t/status-ok

      :bad
      t/status-bad

      :cancelled
      t/cancelled-fg

      :running
      t/code-block-fg

      t/dialog-hint)))

(defn- register-row!
  "A click on a row opens the timeline, or the child session when it needs a person."
  [row screen-row left width]
  (when-let [id (:session-id row)]
    (.register interactions/hit-map
               {:bounds {:row screen-row :col left :width width}
                :kind (if (:pending-input? row) :switch-session :agent-timeline)
                :text id})))

(defn- paint-segments!
  [^TextGraphics g left row width segments tone-key]
  (let [width (long width)]
    (doseq [[col text part] segments
            :let [room (- width (long col))]
            :when (and (pos? room) (seq text))]

      (p/set-colors! g (part-fg part tone-key) t/code-block-bg)
      (let [text (p/ellipsize (str text) room)
            x (+ (long left) (long col))]

        (if (#{:label :status} part)
          (p/styled g [p/BOLD] (p/put-str! g x row text))
          (p/put-str! g x row text))))))

(defn- paint-team-line!
  "Paint one transcript line of a team block on its Activity band."
  [^TextGraphics g left row width viewport-top {:keys [team-line]}]
  (let [{team-row :row :keys [columns line]}
        team-line

        width
        (long width)]

    (p/set-colors! g t/code-block-fg t/code-block-bg)
    (p/fill-rect! g left row width 1)
    (when team-row
      (paint-segments!
        g
        left
        row
        width
        (nth (row-segments team-row columns width (System/currentTimeMillis)) line [])
        (:tone team-row))
      (register-row! team-row (+ (long viewport-top) (long row)) left width))))

(defn card
  "The team block data of team `team-id` from `agents`, or nil when the team has no agent."
  [team-id agents]
  (let [rows (team-rows team-id agents)]
    (when (seq rows) {:team-id team-id :rows rows :columns (row-columns rows)})))

(defn card-entries
  "Transcript rows of a team block on the Activity band at `width` cells: `{:line :meta}` maps."
  [{:keys [rows columns]} width]
  (let [marker
        p/MARKER_ACTIVITY

        now
        (System/currentTimeMillis)]

    (into [{:line marker :meta nil}]
          (concat (mapcat (fn [row]
                            (map-indexed (fn [i text]
                                           {:line (str marker text)
                                            :meta {:kind :activity-live-entry
                                                   :live-paint paint-team-line!
                                                   :team-line {:row row :columns columns :line i}}})
                                         (row-lines row columns width now)))
                          rows)
                  [{:line marker :meta nil}]))))

(defn with-card-lines
  "Append the team block of `projected` (an assistant message with `:agent-team-card`) to its
   painted lines at bubble width `bubble-w`. A windowed slice keeps its lines."
  [projected bubble-w]
  (if-let [team-card (and (not (:lines-window projected))
                          (:prewrapped-lines projected)
                          (:agent-team-card projected))]
    (let [lines (vec (:prewrapped-lines projected))
          line-meta (or (:line-meta projected) (vec (repeat (count lines) nil)))
          entries (card-entries team-card (max 1 (- (long bubble-w) 4)))]

      (assoc projected
        :prewrapped-lines (into (conj lines "") (map :line) entries)
        :line-meta (into (conj (vec line-meta) nil) (map :meta) entries)))
    projected))

(defn dock-team-ids
  "Teams that the dock shows: the team of the running turn `running-turn`, and each team
   with an agent that still works or waits."
  [agents running-turn]
  (into
    (if (and running-turn (some #(= running-turn (get % "team_id")) agents)) #{running-turn} #{})
    (comp (filter active?) (keep #(get % "team_id")))
    agents))

(defonce ^:private ^java.util.IdentityHashMap decorated-memo (java.util.IdentityHashMap. 64))

(defn- decorate
  [message team-card]
  (locking decorated-memo
    (let [[cached-card decorated] (.get ^java.util.IdentityHashMap decorated-memo message)]
      (if (and decorated (= cached-card team-card))
        decorated
        (let [decorated (assoc message :agent-team-card team-card)]
          (when (> (.size ^java.util.IdentityHashMap decorated-memo) 512)
            (.clear ^java.util.IdentityHashMap decorated-memo))
          (.put ^java.util.IdentityHashMap decorated-memo message [team-card decorated])
          decorated)))))

(defn decorate-messages
  "Give the last assistant message of each turn that started a team its `:agent-team-card`
   from the cached snapshot of `sid`. A team in the dock gets no block: the turn
   `running-turn` still runs, or an agent still works. Returns `messages` unchanged when no
   team is known. An unchanged team keeps the decorated message identity, so height caches
   still hit."
  ([messages sid] (decorate-messages messages sid nil))
  ([messages sid running-turn]
   (let [agents (:agents (summary sid))]
     (if (or (empty? agents) (empty? messages))
       messages
       (let [docked (dock-team-ids agents running-turn)
             teams (into #{} (comp (keep #(get % "team_id")) (remove docked)) agents)
             last-index (reduce (fn [acc [i m]]
                                  (let [turn (:session-turn-id m)]
                                    (if (and (= :assistant (:role m)) (contains? teams turn))
                                      (assoc acc turn i)
                                      acc)))
                                {}
                                (map-indexed vector messages))]

         (if (empty? last-index)
           messages
           (reduce-kv (fn [out team-id i]
                        (if-let [team-card (card team-id agents)]
                          (assoc out i (decorate (nth out i) team-card))
                          out))
                      (vec messages)
                      last-index)))))))

(defn footer-label
  "Team state for the footer chip from the cached snapshot, such as `Agents: 2 running · 1
   asking`. Nil while no agent works or waits for a person, so a finished team leaves the
   footer. Never fetches during painting."
  [sid]
  (let [agents (filter #(or (waiting? %) (contains? #{"queued" "running"} (get % "status")))
                       (:agents (summary sid)))]
    (when (seq agents)
      (str "Agents: "
           (str/join " · "
                     (for [word ["needs you" "asking" "running" "queued"]
                           :let [n (count (filter #(= word (status-word %)) agents))]
                           :when (pos? n)]

                       (str n " " word)))))))

;; ── Dock ────────────────────────────────────────────────────────────────────

(defn- dock-width [cols] (max 1 (- (long cols) 4)))

(defn dock-lines
  "The dock lines of `sid` above the prompt at `cols` columns: `{:row :columns :line}` maps,
   or `{:more n}` for the last line when rows do not fit. Empty without a docked team."
  [sid running-turn cols]
  (let [agents
        (:agents (summary sid))

        docked
        (dock-team-ids agents running-turn)]

    (if (empty? docked)
      []
      (let [rows
            (->> (numbered agents)
                 (filter #(contains? docked (get (second %) "team_id")))
                 (sort-by (fn [[n agent]]
                            [(or (get agent "created_at") 0) (str (get agent "team_id")) n]))
                 (mapv (fn [[n agent]]
                         (team-row n agent))))

            columns
            (row-columns rows)

            width
            (dock-width cols)

            lines
            (vec (mapcat (fn [row]
                           (map (fn [i]
                                  {:row row :columns columns :line i})
                                (range (row-line-count row width))))
                         rows))]

        (if (<= (count lines) (long dock-max-lines))
          lines
          (let [kept
                (vec (take (dec (long dock-max-lines)) lines))

                shown
                (count (distinct (map :row kept)))]

            (conj kept {:more (- (count rows) shown)})))))))

(defn dock-height
  "Rows that the dock of `sid` takes above the prompt, 0 without a docked team."
  ^long [sid running-turn cols]
  (count (dock-lines sid running-turn cols)))

(defn dock-text
  "Plain dock lines of `sid`, as the painter shows them."
  [sid running-turn cols now-ms]
  (let [width (dock-width cols)]
    (mapv (fn [{:keys [row columns line more]}]
            (if more
              (str "+" more " more")
              (segments-text (nth (row-segments row columns width now-ms) line))))
          (dock-lines sid running-turn cols))))

(defn draw-dock!
  "Paint the dock of `sid` at screen row `top` across `cols` columns."
  [^TextGraphics g sid running-turn top cols]
  (let [lines
        (dock-lines sid running-turn cols)

        width
        (dock-width cols)

        now
        (System/currentTimeMillis)]

    (doseq [[i {:keys [row columns line more]}]
            (map-indexed vector lines)

            :let [y
                  (+ (long top) (long i))]]

      (p/set-colors! g t/code-block-fg t/code-block-bg)
      (p/fill-rect! g 0 y (max 1 (long cols)) 1)
      (if more
        (do (p/set-colors! g t/dialog-hint t/code-block-bg)
            (p/put-str! g 2 y (str "+" more " more")))
        (do
          (paint-segments! g 2 y width (nth (row-segments row columns width now) line) (:tone row))
          (when (zero? (long line)) (register-row! row y 0 (max 1 (long cols)))))))))

;; ── Child traffic ───────────────────────────────────────────────────────────

(defn child-session-ids
  "The session ids of the cached agents of `sid`."
  [sid]
  (into #{} (keep #(get % "session_id")) (:agents (summary sid))))

(defn child-traffic?
  "Whether `message` is a Council request that an agent of the session sent, such as a
   child's answer or question. The leader transcript hides it."
  [message child-ids]
  (boolean (and (seq child-ids)
                (= :user (:role message))
                (= :council (:request-kind message))
                (contains? child-ids (get-in message [:council "author_session_id"])))))

(defn- key-type [key] (when (instance? KeyStroke key) (.getKeyType ^KeyStroke key)))

;; ── Breadcrumb ──────────────────────────────────────────────────────────────

(defn breadcrumb
  "Header path of child session `sid` from its cached team, such as `Leader → implement Fix
   parser`: `{:text :parent-id}`, or nil when the TUI does not know the parent."
  [sid]
  (let [pairs
        (numbered (:agents (summary sid)))

        name-of
        (fn [[n agent]]
          (str (some-> (role-name agent)
                       (str " "))
               (agent-label n agent)))]

    (when-let [pair (some #(when (= (str sid) (get (second %) "session_id")) %) pairs)]
      (let [agent (second pair)]
        (when-let [parent (get agent "parent_id")]
          {:parent-id parent
           :text (str (if-let [parent-pair (and (not= parent (get agent "leader_id"))
                                                (some
                                                  #(when (= parent (get (second %) "session_id")) %)
                                                  pairs))]
                        (name-of parent-pair)
                        "Leader")
                      " → "
                      (name-of pair))})))))

(defn start-refresh!
  "One screen-owned worker refreshes only the active session, at most every 5s.
   Call the returned cleanup on screen teardown. No network work runs in paint."
  [session-id changed!]
  (let [running?
        (atom true)

        worker
        (Thread. ^Runnable
                 (fn []
                   (try (loop [previous-id
                               nil

                               next-at
                               0]

                          (when @running?
                            (let [sid
                                  (session-id)

                                  now
                                  (System/currentTimeMillis)

                                  refresh?
                                  (and sid (or (not= sid previous-id) (>= now (long next-at))))]

                              (when refresh?
                                (fetch! sid)
                                ;; A slow response for an old tab must not repaint the new tab.
                                (when (and @running? (= sid (session-id))) (changed!)))
                              (Thread/sleep 250)
                              (recur sid
                                     (if refresh? (+ (System/currentTimeMillis) 5000) next-at)))))
                        (catch InterruptedException _ nil)))
                 "vis-tui-agent-team-refresh")]

    (.setDaemon worker true)
    (.start worker)
    (fn []
      (reset! running? false)
      (.interrupt worker)
      (.join worker 2500))))

(defn timeline!
  "The timeline of child `child-id` of `sid` from the gateway: `{:items [...]}` oldest first,
   or `{:error text}`."
  [sid child-id]
  (try
    (let [response
          (vis/request!
            :get
            (path sid
                  (str "/" (URLEncoder/encode (str child-id) StandardCharsets/UTF_8) "/timeline"))
            {:timeout-ms 3000})]
      (if (= 200 (:status response))
        (let [body (wire/parse-json (:body response))
              items (get body "items")]

          (if (and (map? body)
                   (vector? items)
                   (every? #(and (map? %) (number? (get % "at")) (string? (get % "kind"))) items))
            {:items items}
            {:error "Invalid timeline response. Try again."}))
        {:error "Could not load the timeline. Try again."}))
    (catch Exception _ {:error "Could not load the timeline. Try again."})))

(def ^:private ^java.time.format.DateTimeFormatter clock
  (java.time.format.DateTimeFormatter/ofPattern "HH:mm:ss"))

(defn timeline-line
  "One plain timeline row, such as `14:02:11  note  Read the parser tests`."
  ([item] (timeline-line item (java.time.ZoneId/systemDefault)))
  ([item ^java.time.ZoneId zone]
   (let [kind
         (get item "kind")

         word
         (if (= "finished" kind)
           (status-word {"status" (get item "status" "completed") "outcome" (get item "outcome")})
           kind)]

     (str (.format ^java.time.format.DateTimeFormatter clock
                   (.atZone (java.time.Instant/ofEpochMilli (long (get item "at"))) zone))
          "  "
          word
          (when-let [text (not-empty (one-line (get item "text")))]
            (str "  " text))))))

(defn timeline-items
  "The dialog items of the timeline of `agent`: its actions first, then one row per step."
  [agent {:keys [items error]}]
  (into (cond-> [{:label "Open agent session" :action :inspect}]
          (contains? #{"queued" "running"} (get agent "status"))
          (conj {:label "Stop agent" :action :stop}))
        (cond error [{:section? true :label "Timeline"} {:label error}]
              (empty? items) [{:section? true :label "Timeline"} {:label "No steps yet"}]
              :else (into [{:section? true :label "Timeline"}]
                          (map (fn [item]
                                 {:label (timeline-line item)}))
                          items))))

(defn- agent-of
  [sid child-id]
  (some #(when (= (str child-id) (get (second %) "session_id")) %)
        (numbered (:agents (summary sid)))))

(defn- stop!
  [sid id]
  (try (when-not (= 200
                    (:status (vis/request! :post (path sid "/cancel") {:body {:session_id id}})))
         (vis/notify! "Could not stop agent; refresh its state" :level :error))
       (catch Exception _ (vis/notify! "Could not stop agent; refresh its state" :level :error))))

(defn show-timeline!
  "Show the timeline of child `child-id` of `sid`. Returns the session id to open, or nil."
  [screen sid child-id]
  (loop []

    (let [[n agent]
          (or (agent-of sid child-id) [nil {"session_id" child-id}])

          title
          (str (some-> (role-name agent)
                       (str " "))
               (if n (agent-label n agent) "Agent")
               " · "
               (status-word agent))

          choice
          (dlg/select-dialog! screen title (timeline-items agent (timeline! sid child-id)))]

      (case (:action choice)
        :inspect
        (str child-id)

        :stop
        (do (when (= :stop
                     (:action (dlg/select-dialog! screen
                                                  "Stop this agent and its descendants?"
                                                  [{:label "Keep working" :action :keep}
                                                   {:label "Stop agent" :action :stop}])))
              (stop! sid child-id)
              (fetch! sid))
            (recur))

        nil
        (when choice (recur))))))

(defn items
  "The team modal items of `rows`: one section for each turn team, newest first, with one
   row for each agent."
  [rows parent-id]
  (let [now
        (System/currentTimeMillis)

        teams
        (->> (numbered rows)
             (group-by #(get (second %) "team_id"))
             (sort-by (fn [[_ pairs]]
                        (- (long (reduce max 0 (keep #(get (second %) "created_at") pairs)))))))]

    (into (cond-> [{:label "Refresh team" :action :refresh}]
            parent-id
            (conj {:label "Open parent" :action :parent :session-id parent-id}))
          (mapcat (fn [[team-id _]]
                    (let [team
                          (team-rows team-id rows)

                          columns
                          (row-columns team)]

                      (into [{:section? true
                              :label (str "Turn · "
                                          (count team)
                                          (if (= 1 (count team)) " agent" " agents"))}]
                            (map (fn [row]
                                   {:label (first (row-lines row columns 200 now))
                                    :search-text (str (:role row) " " (:label row))
                                    :agent-id (:session-id row)}))
                            team))))
          teams)))

(defn component
  "Production list component. Snapshot is read off-thread; the modal remains dismissible."
  [snapshot parent-id]
  (let [base (dlg/select-modal-component "Agents" [] {:filter? true :height :content})]
    (assoc base
      :measure
      (fn [state cols rows]
        (let [{:keys [agents error loading? enabled?]} (snapshot)
              title (cond (false? enabled?) "Agents · Disabled"
                          loading? "Agents · Loading…"
                          error "Agents · Unavailable"
                          :else "Agents")
              entries (cond (false? enabled?) [{:label
                                                "Enable Subagents in Settings → Experimental."}]
                            loading? [{:label "Loading team…"}]
                            error (into [{:label error}] (items [] parent-id))
                            (empty? agents)
                            (into [{:label "No subagents yet. Delegate a task to create one."}]
                                  (items [] parent-id))
                            :else (let [all (items agents parent-id)]
                                    (vec (concat (remove :action all) (filter :action all)))))]

          ((:measure (dlg/select-modal-component title entries {:filter? true :height :content}))
            state
            cols
            rows)))
      :on-key
      (fn [state key geom]
        (let [enter? (= KeyType/Enter (key-type key))
              result ((:on-key base) state key geom)
              ;; Enter on a turn header opens the first agent row below it.
              result
              (if (and enter? (= result state))
                ((:on-key base) ((:on-key base) state (KeyStroke. KeyType/ArrowDown) geom) key geom)
                result)
              choice (::dlg/done result)]

          (if (and choice (not (:action choice)) (not (:agent-id choice))) state result))))))

(defn- choose-team!
  [screen sid parent-id]
  (let [task
        (future (fetch! sid))

        snapshot
        #(deref task 0 {:loading? true})]

    (try (dlg/run-modal! screen
                         (assoc (component snapshot parent-id)
                           :read-key (fn [screen]
                                       (if (or (realized? task) (dlg/modal-input-pending? screen))
                                         (dlg/read-modal-key! screen)
                                         (do (Thread/sleep 16) nil)))))
         (finally (future-cancel task)))))

(defn show!
  "The team modal of `sid`. Returns the session id to open, or nil."
  [screen sid parent-id]
  (loop []

    (let [choice (choose-team! screen sid parent-id)]
      (cond (= :refresh (:action choice)) (recur)
            (= :parent (:action choice)) (:session-id choice)
            (:agent-id choice) (or (show-timeline! screen sid (:agent-id choice)) (recur))
            :else nil))))
