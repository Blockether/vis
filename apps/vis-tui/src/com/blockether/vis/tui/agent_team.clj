(ns com.blockether.vis.tui.agent-team
  "Managed team inspection through the same gateway as the conversation.

   The cached team snapshot also feeds two surfaces: the team card in the turn that
   started a team, before its answer, and the breadcrumb of a child session. None of them
   fetches during painting."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.keymap :as keymap]
            [com.blockether.vis.tui.presentation :as presentation]
            [com.blockether.vis.tui.markers :as m]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.theme :as t]
            [com.blockether.vis.tui.time-axis :as time-axis])
  (:import [com.googlecode.lanterna.graphics TextGraphics]
           [com.googlecode.lanterna.input KeyStroke KeyType]))

(defonce ^:private snapshots (atom {}))

(defonce ^:private pending
  ;; Work for the refresh worker, by session id. Only the worker takes it.
  ;; `:rows` holds the `agents` of the newest `subscription.ready` and `:clocks` its
  ;; `team_clocks`: they replace the team.
  ;; `:updates` holds the `session.team_updated` events after it as
  ;; `{:agents :changes :clocks :removed}`, in arrival order: a change needs the row before it. `:setting?` reads the Subagents setting
  ;; again, and `:lost?` marks a dropped event stream.
  (atom {}))

(defn- want!
  "Add work `f` for the refresh worker of `sid`."
  [sid f]
  (when sid (swap! pending update (str sid) f)))

(defn- agent-row?
  "Whether `row` has the fields that a team row needs."
  [row]
  (and (map? row)
       (string? (get row "session_id"))
       (string? (get row "task"))
       (string? (get row "status"))))

(defn- agent-rows
  "The usable team rows of the wire value `agents`. A value that is not a list gives no rows."
  [agents]
  (if (sequential? agents) (filterv agent-row? agents) []))

(defn- clock-rows
  "The usable team clocks of the wire value `clocks`, keyed by the id of their leader turn. A
   value that is not a list gives none."
  [clocks]
  (if (sequential? clocks)
    (into {}
          (keep (fn [clock]
                  (when (and (map? clock)
                             (string? (get clock "turn_id"))
                             (number? (get clock "worked_ms")))
                    [(get clock "turn_id") clock])))
          clocks)
    {}))

(defn merge-agents
  "Apply the changed `rows` of one `session.team_updated` to the team rows `agents`. A row
   replaces the row with the same `session_id` in place. A row with an unknown `session_id`
   goes at the end, so the rows keep their start order. A repeated event changes nothing."
  [agents rows]
  (reduce (fn [out row]
            (let [id (get row "session_id")]
              (if-let [i (first (keep-indexed #(when (= id (get %2 "session_id")) %1) out))]
                (assoc out i row)
                (conj out row))))
          (vec agents)
          rows))

(defn- change-rows
  "The usable row changes of the wire value `changes`. A value that is not a list gives none."
  [changes]
  (if (sequential? changes) (filterv #(and (map? %) (string? (get % "session_id"))) changes) []))

(defn apply-changes
  "Apply the row `changes` of one `session.team_updated` to the team rows `agents`. Each key of a
   change goes into the row with the same `session_id`, and a null value removes the key: null and a
   missing key mean the same. A change of an unknown row changes nothing, and a repeated change
   gives the same rows."
  [agents changes]
  (reduce (fn [out change]
            (let [id (get change "session_id")]
              (if-let [i (first (keep-indexed #(when (= id (get %2 "session_id")) %1) out))]
                (update out
                        i
                        (fn [row]
                          (reduce-kv (fn [row k v]
                                       (if (nil? v) (dissoc row k) (assoc row k v)))
                                     row
                                     change)))
                out)))
          (vec agents)
          changes))

(defn- without-removed
  "The team rows `agents` without the rows of the deleted children `removed`, a set of session ids."
  [agents removed]
  (if (seq removed) (into [] (remove #(contains? removed (get % "session_id"))) agents) agents))

(defn apply-update
  "Apply one `session.team_updated` update, `{:agents :changes :removed}`, to the team rows
   `agents`: the whole rows first, then the changes. The rows of the deleted children go away."
  [team {:keys [agents changes removed]}]
  (-> (merge-agents team agents)
      (apply-changes changes)
      (without-removed removed)))

(defn team-updated!
  "Take the `session.team_updated` chunk of `sid`. Its `:agents` rows and then its `:changes`
   apply to the team after the rows of the last `subscription.ready`, and each clock of its
   `:team-clocks` replaces the clock of the same leader turn. The row of each deleted child of its
   `:removed` goes away. An event without usable rows, changes, clocks or deleted children changes
   nothing."
  [sid {:keys [agents changes team-clocks removed]}]
  (let [rows
        (agent-rows agents)

        removed
        (if (sequential? removed) (into #{} (filter string?) removed) #{})

        changes
        (change-rows changes)

        clocks
        (clock-rows team-clocks)]

    (when (or (seq rows) (seq changes) (seq clocks) (seq removed))
      (want! sid
             #(update %
                      :updates
                      (fnil conj [])
                      {:agents rows :changes changes :clocks clocks :removed removed})))))

(defn stream-lost!
  "Mark the team of `sid` stale: its event stream dropped, so events can be missing."
  [sid]
  (want! sid #(assoc % :lost? true)))

(defn stream-ready!
  "Take the `agents` and the `team_clocks` of `subscription.ready` of `sid`. The gateway sends
   this frame at each connect and reconnect, so its rows and clocks replace the team of `sid`: no
   `agents` gives no rows. Older `session.team_updated` rows do not apply after it."
  ([sid agents] (stream-ready! sid agents nil))
  ([sid agents clocks]
   (want! sid
          #(-> %
               (assoc :rows (agent-rows agents)
                      :clocks (clock-rows clocks)
                      :updates [])
               (dissoc :lost?)))))

(defn settings-changed!
  "Ask the refresh worker to read the Subagents setting of `sid` again, after Settings closes."
  [sid]
  (want! sid #(assoc % :setting? true)))

(defonce ^:private expanded-docks
  ;; Sessions whose dock shows the whole agent table. It lasts for this TUI run only.
  (atom #{}))

(defn dock-expanded?
  "Whether the dock of `sid` shows the whole agent table, not only its summary line."
  [sid]
  (contains? @expanded-docks (str sid)))

(defn toggle-dock!
  "Show or hide the agent table of the dock of `sid`. Returns the new state."
  [sid]
  (let [sid (str sid)]
    (contains? (swap! expanded-docks #(if (contains? % sid) (disj % sid) (conj % sid))) sid)))

(defonce ^:private timeline-docks
  ;; Sessions whose open dock shows the timeline, not the agent table. It lasts for this TUI run only.
  (atom #{}))

(defn dock-timeline?
  "Whether the open dock of `sid` shows the team timeline, not the agent table."
  [sid]
  (contains? @timeline-docks (str sid)))

(defn toggle-timeline!
  "Switch the dock of `sid` between the agent table and the timeline. A folded dock opens on the
   timeline. Returns true when the timeline shows."
  [sid]
  (let [sid (str sid)]
    (if (dock-expanded? sid)
      (contains? (swap! timeline-docks #(if (contains? % sid) (disj % sid) (conj % sid))) sid)
      (do (swap! expanded-docks conj sid) (swap! timeline-docks conj sid) true))))

(defonce ^:private dock-offsets
  ;; The first agent row that the dock table of each session shows. It lasts for this TUI run only.
  (atom {}))

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

(defn role-word
  "The role of `agent` as the views write it, in capitals, such as `IMPLEMENT`, or nil."
  [agent]
  (some-> (role-name agent)
          str/upper-case))

(defn- active? [agent] (contains? #{"queued" "running"} (get agent "status")))

(defn- pending?
  "Whether `agent` did not start: a task in its after did not complete yet, or never will. It has
   no session to open yet."
  [agent]
  (contains? #{"waiting" "blocked"} (get agent "status")))

(defn- waits-for
  "The task ids in the after of `agent` that did not complete, from the rows `agents`. An id
   without a row stays, because nothing shows that it completed."
  [agent agents]
  (remove (fn [id]
            (some #(and (= (get agent "parent_id") (get % "parent_id"))
                        (= id (get % "id"))
                        (= "completed" (get % "status")))
                  agents))
    (get agent "after")))

(defn- role-rank
  [agent]
  (or (first (keep-indexed #(when (= (get %2 "const") (get agent "role")) %1) (roles)))
      (count (roles))))

(defn numbered
  "`[n agent]` pairs. `n` is the start order from 1 inside the agent's team (`team_id`), so it
   never changes. Rows of one role stand together in contract order, and a row without a role
   comes last."
  [agents]
  (let [numbers (second (reduce (fn [[seen out] agent]
                                  (let [team (get agent "team_id")
                                        n (inc (long (get seen team 0)))]

                                    [(assoc seen team n) (conj out n)]))
                                [{} []]
                                agents))]
    (sort-by (fn [[_ agent]]
               (role-rank agent))
             (map vector numbers agents))))

(defn summary
  "Cached team snapshot; never performs network work during painting."
  [sid]
  (get @snapshots
       (some-> sid
               str)))

(defn enabled?
  "Whether the last gateway check enabled Subagents; never fetch during painting."
  [sid]
  (true? (:enabled? (summary sid))))

(defn- read-enabled
  "The Subagents state of `sid`. A failed read keeps the last state of `previous`."
  [sid previous]
  ;; Subagents accepts project, group and session overrides, and the session's
  ;; turns use its resolved value (issue #311). The global value can differ.
  (try (true? (get (vis/setting "subagents" {:scope "session" :target-id (str sid)}) "enabled"))
       (catch Exception _ (true? (:enabled? previous)))))

(defn- sync!
  "Do the queued `work` of `sid` and store its new snapshot. Returns the snapshot.

   The rows of `subscription.ready` replace the team, and the `session.team_updated` updates
   apply after them in arrival order. Nothing reads the team from the gateway. A failed setting
   read keeps the last Subagents state, so one error does not hide the dock."
  [sid {:keys [rows clocks updates setting? lost?] :as work}]
  (let [previous
        (summary sid)

        enabled?
        (if (or setting? (nil? (:enabled? previous)))
          (read-enabled sid previous)
          (true? (:enabled? previous)))

        ready?
        (contains? work :rows)

        team
        (reduce apply-update (if ready? rows (:rows previous)) updates)

        snapshot
        (cond-> {:rows team
                 :clocks (reduce #(merge %1 (:clocks %2))
                                 (or (if ready? clocks (:clocks previous)) {})
                                 updates)
                 :agents (if enabled? team [])
                 ;; The deleted children: a transcript turn can still hold their rows.
                 :gone (into (or (:gone previous) #{}) (mapcat :removed) updates)
                 :enabled? enabled?
                 :checked-at (System/currentTimeMillis)}
          (or lost? (and (not ready?) (:stale? previous)))
          (assoc :stale? true))]

    (swap! snapshots assoc (str sid) snapshot)
    snapshot))

(defn header-label
  "Make unknown and stale observations explicit in the header. A snapshot is stale only after
   its event stream dropped, until the next `subscription.ready`."
  [sid child?]
  (let [{:keys [agents checked-at stale?]}
        (summary sid)

        label
        (if child? "Subagent" "Agents")]

    (cond (nil? checked-at) (str label " ?")
          stale? (str label " stale")
          :else (str label
                     " "
                     (count (filter #(contains? #{"queued" "running"} (get % "status")) agents))
                     " active/"
                     (count agents)
                     " cached"))))

;; ── Team rows ───────────────────────────────────────────────────────────────

(defn- title-width
  "The most cells that the title column takes: the title bound of the agents contract. A longer
   title ends with an ellipsis."
  ^long []
  (get-in (document/schema-document "agents") ["$defs" "spawn" "properties" "title" "maxLength"]))

(def ^:private ^:const time-width "Cells of the time column, enough for `59m 59s`." 7)

(def ^:private ^:const narrow-width
  "Below this width a row with a now or result text takes two lines."
  64)

(def ^:private ^:const dock-max-rows
  "The most agent rows in the dock table. A larger team shows one row fewer and counts the rest."
  6)

(defn- one-line [text] (str/trim (str/replace (str text) #"\s+" " ")))

(defn- markdown-line
  "Agent Markdown `text` on one line without its markers, as a row shows it. Inline code
   keeps its backticks, so a quoted `\\n` does not read as part of the sentence."
  [text]
  (one-line (presentation/render (walk/postwalk #(if (and (vector? %) (= :c (first %)))
                                                   (str "`" (last %) "`")
                                                   %)
                                                (presentation/council->ast (str text)))
                                 :plain)))

(defn agent-label
  "The title of `agent`, or `#n` when it has no title. `n` is its start order in its team."
  [n agent]
  (or (not-empty (one-line (get agent "title"))) (str "#" n)))

(defn status-word
  "The status of `agent` in one word, in capitals: `WAITING`, `QUEUED`, `RUNNING`, `DONE`,
   `FAILED` or `BLOCKED`. An agent that waits for the reply of its parent still runs. A failed
   verdict, a cancel and a spent budget are all `FAILED`: the row text tells which. `WAITING` and
   `BLOCKED` tasks did not start: a task in their after did not complete yet, or never will."
  [agent]
  (case (get agent "status")
    "waiting"
    "WAITING"

    "blocked"
    "BLOCKED"

    "queued"
    "QUEUED"

    "running"
    "RUNNING"

    "completed"
    (if (= "failed" (get agent "outcome")) "FAILED" "DONE")

    ("failed" "cancelled" "budget_limited")
    "FAILED"

    (str/upper-case (str/replace (str (get agent "status")) "_" " "))))

(defn- end-reason
  "The end that `FAILED` hides, for an agent without a result: `cancelled` or `out of budget`. A
   task that did not start names the tasks of its after that did not complete, from `agents`:
   `waiting for a2` or `a2 did not complete`."
  [agent agents]
  (case (get agent "status")
    ("waiting" "blocked")
    (let [ids
          (str/join ", " (waits-for agent agents))

          waiting?
          (= "waiting" (get agent "status"))]

      (cond (str/blank? ids)
            (if waiting? "waits for earlier tasks" "an earlier task did not complete")
            waiting? (str "waiting for " ids)
            :else (str ids " did not complete")))

    "cancelled"
    "cancelled"

    "budget_limited"
    "out of budget"

    nil))

(defn- tone
  [word]
  (case word
    "DONE"
    :ok

    ("FAILED" "BLOCKED")
    :bad

    "RUNNING"
    :running

    :muted))

(defn- now-text
  "The newest progress note or Activity line of a working agent, else its result, else the
   reason of its end. The questions and grant requests of an agent go to its leader, never to
   the person, so no row shows them."
  [agent agents]
  (let [live? (active? agent)]
    (cond (and live? (get agent "note")) (markdown-line (get agent "note"))
          (and live? (get agent "activity")) (one-line (get agent "activity"))
          (get agent "result") (markdown-line (get agent "result"))
          :else (one-line (end-reason agent agents)))))

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
   compares equal across polls. The painter computes the running time. `agents` are the rows of
   the team, which tell which tasks of an after completed."
  ([n agent] (team-row n agent []))
  ([n agent agents]
   (let [word (status-word agent)]
     {:n n
      :team-id (get agent "team_id")
      :session-id (get agent "session_id")
      :role (or (role-word agent) "")
      :role-key (role-name agent)
      :label (agent-label n agent)
      :status word
      :tone (tone word)
      :now (now-text agent agents)
      :active? (active? agent)
      :pending? (pending? agent)
      :start (start-ms agent)
      :created-at (get agent "created_at")
      ;; The earlier tasks, which the timeline draws in the tone of their status.
      :runs (mapv (fn [run]
                    {:start (get run "start")
                     :end (get run "end")
                     :word (status-word {"status" (get run "status") "outcome" (get run "outcome")})})
                  (filter #(and (number? (get % "start")) (number? (get % "end")))
                          (let [runs (get agent "runs")] (when (sequential? runs) runs))))
      :worked-ms (get agent "worked_ms")
      :busy-since (get agent "busy_since")
      :end (when (and (not (active? agent)) (number? (get agent "finished_at")))
             (long (get agent "finished_at")))})))

(defn turns-of
  "The leader turns in which `agent` worked, oldest first: the turn that started it and each turn
   that pinged it with a follow-up task. A row without `turn_ids` worked only in the turn that
   started it."
  [agent]
  (let [turns (get agent "turn_ids")]
    (if (seq turns) (vec turns) [(get agent "team_id")])))

(defn current-turn
  "The leader turn of the current task of `agent`: the last turn that started or pinged it."
  [agent]
  (peek (turns-of agent)))

(defn- in-turn? [turn-id agent] (boolean (some #{turn-id} (turns-of agent))))

(defn turn-rows
  "The rows of the agents that worked in turn `turn-id` (see `turns-of`), in start order."
  [turn-id agents]
  (->> (numbered agents)
       (filter #(in-turn? turn-id (second %)))
       (sort-by first)
       (mapv (fn [[n agent]]
               (team-row n agent agents)))))

(defn team-rows
  "The rows of team `team-id` from `agents`, in start order, so a row never moves."
  [team-id agents]
  (->> (numbered agents)
       (filter #(= team-id (get (second %) "team_id")))
       (sort-by first)
       (mapv (fn [[n agent]]
               (team-row n agent agents)))))

(defn- leader? [row] (= "leader" (:role-key row)))

(def ^:private leader-time-sample
  "The widest time cell of a leader row, so the time column does not change while it counts."
  "59m 59s (waited 59m 59s)")

(defn- time-sample [row] (if (leader? row) leader-time-sample ""))

(defn- agent-wait-forms
  "Waits with an operation recorded by the host, not guesses from Python source."
  [steps]
  (for [form (mapcat :forms steps)
        row (get-in form [:activity :rows])
        :when (and (= "council.wait" (:operation row))
                   (or (number? (:duration-ms row)) (= "running" (:state row))))]
    (assoc (select-keys form [:started-at-ms]) :duration-ms (:duration-ms row))))

(defn- first-line [text] (some #(when-not (str/blank? %) %) (str/split-lines (str text))))

(defn- leader-note
  "The newest line of a leader: the first line of its answer once its turn ended, else the prose
   of its newest step, without Markdown markers."
  [steps answer ended?]
  (let [text (if ended?
               (first-line answer)
               (some #(not-empty (str/trim (str (:assistant-prose %)))) (rseq (vec steps))))]
    (when (seq text) (not-empty (markdown-line text)))))

(defn leader-row
  "The first row of a team table: the leader turn `leader`, `{:start :end :failed? :steps
   :answer}` in epoch ms. `:end` is nil while the turn runs. `:steps` are trace entries, and a
   wait needs a recorded host operation. Nil without a start. The row opens nothing."
  [{:keys [start end failed? steps answer]}]
  (when (number? start)
    (let [ended?
          (number? end)

          forms
          (agent-wait-forms steps)

          word
          (cond (not ended?) "RUNNING"
                failed? "FAILED"
                :else "DONE")]

      {:n 0
       :role "LEADER"
       :role-key "leader"
       :label ""
       :status word
       :tone (tone word)
       :now (leader-note steps answer ended?)
       :active? (not ended?)
       :start (long start)
       :end (when ended? (long end))
       :waited-ms (reduce +
                          0
                          (keep #(some-> (:duration-ms %)
                                         long)
                                forms))
       ;; The waits as `[start end]` in epoch ms for the timeline; a wait that runs has no end.
       :waits (vec (keep #(when (number? (:started-at-ms %))
                            [(long (:started-at-ms %))
                             (some-> (:duration-ms %)
                                     (+ (long (:started-at-ms %))))])
                         forms))
       ;; A wait that still runs counts to now.
       :wait-starts (when-not ended?
                      (vec (keep #(when (and (nil? (:duration-ms %)) (number? (:started-at-ms %)))
                                    (long (:started-at-ms %)))
                                 forms)))})))

(defn message-leader
  "The leader value of the ended turn of assistant message `message`, or nil without a start
   time or a duration. An error turn or a stopped turn failed."
  [{:keys [timestamp duration-ms traces text status content]}]
  (let [start (cond (instance? java.util.Date timestamp) (.getTime ^java.util.Date timestamp)
                    (number? timestamp) (long timestamp))]
    (when (and start (number? duration-ms))
      {:start start
       :end (+ (long start) (long duration-ms))
       :failed? (boolean (or (contains? #{:failed :cancelled :error} status)
                             (some #(= "error" (get % "type")) content)))
       :steps traces
       :answer text})))

(defn duration-text
  "`12s`, `3m 04s` or `1h 02m`, or nil for a negative or unknown span."
  [ms]
  (when (and (number? ms) (not (neg? (long ms))))
    (let [s (quot (long ms) 1000)]
      (cond (< s 60) (str s "s")
            (< s 3600) (format "%dm %02ds" (quot s 60) (rem s 60))
            :else (format "%dh %02dm" (quot s 3600) (quot (rem s 3600) 60))))))

(defn- leader-time
  "The time cell of a leader row at `now-ms`: its own time, then the part of it that it waited
   for its agents, such as `3m 20s (waited 1m 10s)`."
  [{:keys [waited-ms wait-starts start end]} now-ms]
  (let [total
        (max 0 (- (long (or end now-ms)) (long start)))

        waited
        (reduce + (long waited-ms) (map #(max 0 (- (long now-ms) (long %))) wait-starts))]

    (str (duration-text total)
         (when (pos? waited) (str " (waited " (duration-text (min total waited)) ")")))))

(defn row-time
  "The time of a row. The store keeps the time of the tasks that ended (`worked_ms`) and the
   start of the work that runs now (`busy_since`), so this adds only the time from that start to
   now, as for a team clock. An agent that does not work shows the stored time only, so the time
   stops. A gateway from before `worked_ms` gives the time of the current task only. Nil while
   queued without earlier work, or without times. A leader row adds its wait."
  [{:keys [start end active? status worked-ms busy-since] :as row} now-ms]
  (let [worked (long (or worked-ms 0))]
    (cond (leader? row) (when start (leader-time row now-ms))
          busy-since (duration-text (+ worked (max 0 (- (long now-ms) (long busy-since)))))
          (pos? worked) (duration-text worked)
          (or worked-ms (nil? start)) nil
          (and active? (not= "QUEUED" status)) (duration-text (max 0
                                                                   (- (long now-ms) (long start))))
          end (duration-text (- (long end) (long start))))))

(defn- widest [texts] (reduce max 0 (map #(long (p/display-width (str %))) texts)))

(defn- role-width
  "Cells of the role column: the longest role word of the contract, so all teams line up."
  ^long []
  (widest (map #(str/upper-case (str (get % "const"))) (roles))))

(defn row-columns
  "The cell widths of the role, title, status and time columns, so every row lines up. The role
   width is the same for every team."
  [rows]
  {:role (role-width)
   :label (min (title-width) (long (widest (map :label rows))))
   :status (widest (map :status rows))
   :time (max (long time-width) (long (widest (map time-sample rows))))})

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

        time-w
        (long (or (:time columns) time-width))

        time-col
        (- width time-w)

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

        now-col
        (+ label-col label-w 2)

        ;; The status stands right before the time, after the now or result text.
        status-col
        (max now-col (- time-col 2 (long (:status columns))))

        head
        (cond-> []
          (pos? role-w)
          (conj [0 (:role row) :role])

          true
          (conj [label-col (p/ellipsize (:label row) label-w) :label]
                [status-col (:status row) :status]))

        time-part
        [(+ time-col (- time-w (long (p/display-width time)))) time :time]]

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
         (and (seq (:now row)) (> (- status-col now-col 2) 3))
         (conj [now-col (p/ellipsize (:now row) (- status-col now-col 2)) :now])

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

(defn- role-ink
  "The ink of contract role `role`, the same syntax ink as the Companion role ink."
  [role]
  (case role
    "research"
    t/code-syntax-number-fg

    "implement"
    t/code-syntax-string-fg

    "verify"
    t/code-syntax-keyword-fg

    ;; The leader is the Vis agent of the session, so it takes the ink of Vis, as in the Companion.
    "leader"
    t/ai-role-fg

    t/dialog-hint))

(defn- tone-ink
  "The ink of status tone `tone-key`. A running agent takes the warning ink, as in the Companion."
  [tone-key]
  (case tone-key
    :ok
    t/status-ok

    :bad
    t/status-bad

    :running
    t/warning-fg

    t/dialog-hint))

(defn- part-fg
  "The ink of part `part` of `row` on `bg`. A role or status ink keeps 4.5:1 on `bg`."
  [part row bg]
  (case part
    :role
    (t/legible-ink (role-ink (:role-key row)) bg)

    :label
    t/code-block-fg

    :key
    t/dialog-hint-key

    :status
    (t/legible-ink (tone-ink (:tone row)) bg)

    t/dialog-hint))

(defn- register-row!
  "A click on a row opens the session of its agent. A task that did not start has no session to
   open yet, so its row opens nothing. A row of the dock of `dock` also takes the wheel of that
   dock."
  ([row screen-row left width] (register-row! row screen-row left width nil))
  ([row screen-row left width dock]
   (when-let [id (when-not (:pending? row) (:session-id row))]
     (.register
       interactions/hit-map
       (cond-> {:bounds {:row screen-row :col left :width width} :kind :agent-open :text id}
         dock
         (assoc :dock dock))))))

;; ── Table ───────────────────────────────────────────────────────────────────

(def ^:private table-heads
  "The column titles of the agent table."
  {:key "#" :role "Role" :label "Agent" :status "Status" :now "Detail" :time "Time"})

(def ^:private ^:const detail-min-width
  "The fewest cells of the detail column. A narrower table drops the role column, then
   the detail column."
  12)

(defn- table-width
  "Cells of a table with `columns`: one rail, then a padded cell and a rail for each column."
  ^long [columns]
  (+ 1 (long (reduce + 0 (map #(+ 3 (long (:w %))) columns)))))

(defn table-columns
  "The columns of the agent table for `rows` at `width` cells: `[{:key :w}]` in paint order.
   The order is role, title, detail, status and time. The detail column takes the free room,
   so the table fills `width`. A narrow table drops the role, detail and time columns in that
   order, and then the title takes the room. Rows with a `:key` get a first key column."
  [rows width]
  (let [width
        (long width)

        key-col
        (if (some :key rows) [{:key :key :w (widest (cons (table-heads :key) (map :key rows)))}] [])

        fixed
        (fn [k texts]
          {:key k :w (widest (cons (table-heads k) texts))})

        role
        {:key :role :w (max (role-width) (count (:role table-heads)))}

        label
        (update (fixed :label (map :label rows)) :w min (title-width))

        status
        (fixed :status (map :status rows))

        time
        {:key :time
         :w
         (max (long time-width) (count (:time table-heads)) (long (widest (map time-sample rows))))}

        with-detail
        (fn [columns]
          (let [room (- width (table-width (conj columns {:w 0})))]
            (when (>= room (long detail-min-width))
              ;; The detail goes before the status, so the status stands right before the time.
              (into (conj (vec (drop-last 2 columns)) {:key :now :w room})
                    (take-last 2 columns)))))]

    (or (with-detail (into key-col [role label status time]))
        (with-detail (into key-col [label status time]))
        ;; A phone keeps the title readable: the time column goes before the title gets short.
        (let [fit (fn [columns]
                    (let [room (- width (table-width (into key-col columns)) 3)]
                      (when (>= room (min 10 (long (:w label))))
                        (-> key-col
                            (conj (assoc label :w room))
                            (into columns)))))]
          (or (fit [status time])
              (fit [status])
              (conj key-col
                    (assoc label :w (max 4 (- width (table-width (conj key-col status)) 3)))
                    status))))))

(defn- window-start
  "The first shown row of `n` rows in a window of `size` rows that asks to start at `offset`."
  ^long [n size offset]
  (max 0 (min (long (or offset 0)) (- (long n) (long size)))))

(defn- leader-lines
  "The lines of leader row `row` in a table: the rule of the head, the row, and a rule that
   divides the leader from its agents. `:leader` tells a rule on which side the leader row is."
  [row]
  [{:kind :mid :leader :below} {:kind :row :row row} {:kind :mid :leader :above}])

(defn table-lines
  "The lines of the agent table for `rows`, from the top border to the bottom border:
   `{:kind}` maps. A first leader row has a rule under it (see `leader-lines`). With `max-rows`,
   a longer team shows `max-rows - 1` rows from `offset` and a last row that counts the rows
   above (`:above`) and below (`:n`) them."
  ([rows] (table-lines rows nil 0))
  ([rows max-rows] (table-lines rows max-rows 0))
  ([rows max-rows offset]
   (let [rows
         (vec rows)

         n
         (count rows)

         overflow?
         (and max-rows (> n (long max-rows)))

         size
         (if overflow? (dec (long max-rows)) n)

         start
         (window-start n size offset)

         shown
         (subvec rows start (+ start (long size)))

         leader
         (when (leader? (first shown)) (first shown))]

     (-> [{:kind :top} {:kind :head}]
         (into (if leader (leader-lines leader) [{:kind :mid}]))
         (into (map (fn [row]
                      {:kind :row :row row}))
               (cond->> shown
                 leader
                 rest))
         (cond->
           overflow?
           (conj {:kind :more :above start :n (- n start (long size))}))
         (conj {:kind :bottom})))))

(defn- cell-text
  [{:keys [kind row n above]} column-key now-ms]
  (case kind
    :head
    (table-heads column-key)

    :more
    (if (= :label column-key)
      (str/join "  "
                (cond-> []
                  (pos? (long (or above 0)))
                  (conj (str "↑" above " above"))

                  (pos? (long n))
                  (conj (str "+" n " more"))))
      "")

    (case column-key
      :time
      (or (row-time row now-ms) "")

      (str (get row column-key)))))

(defn- line-columns
  "The cells of table line `line`. A leader row has no title, so its newest line takes the title
   cell and the detail cell, with no rail between them."
  [{:keys [kind row]} columns]
  (let [columns
        (vec columns)

        i
        (first (keep-indexed #(when (= :label (:key %2)) %1) columns))]

    (cond (not (and i (= :row kind) (leader? row))) columns
          (= :now (:key (get columns (inc (long i)))))
          (-> (subvec columns 0 i)
              (conj {:key :now
                     :w (+ (long (:w (columns i))) 3 (long (:w (columns (inc (long i))))))})
              (into (subvec columns (+ (long i) 2))))
          :else (assoc-in columns [i :key] :now))))

(defn- rule-text
  "The rule between two table rows across `columns`. `leader` is `:below` or `:above` when the
   row on that side is a leader row. That row has no rail between the title and the detail (see
   `line-columns`), so the junction there has no arm to that side."
  [columns leader]
  (let [columns
        (vec columns)

        text
        (p/boxed-horiz-line (map #(+ 2 (long (:w %))) columns) \├ \┼ \┤)

        i
        (first (keep-indexed #(when (= :label (:key %2)) %1) columns))]

    (if (and leader i (= :now (:key (get columns (inc (long i))))))
      (let [rail (dec (table-width (subvec columns 0 (inc (long i)))))]
        (str (subs text 0 rail) (if (= :below leader) \┴ \┬) (subs text (inc rail))))
      text)))

(defn table-segments
  "The painted parts of one table line: `[col text part]` vectors. `part` is `:chrome` for
   the grid, `:head` for a title, `:more` for the hidden count, else the column key."
  [{:keys [kind] :as line} columns now-ms]
  (let [columns
        (line-columns line columns)

        widths
        (map #(+ 2 (long (:w %))) columns)]

    (case kind
      :top
      [[0 (p/boxed-horiz-line widths \┌ \┬ \┐) :chrome]]

      :mid
      [[0 (rule-text columns (:leader line)) :chrome]]

      :bottom
      [[0 (p/boxed-horiz-line widths \└ \┴ \┘) :chrome]]

      (first
        (reduce (fn [[out col] {column-key :key w :w}]
                  (let [w
                        (long w)

                        text
                        (p/ellipsize (cell-text line column-key now-ms) w)

                        text-w
                        (long (p/display-width text))

                        at
                        (if (and (= :time column-key) (not= :head kind))
                          (+ (long col) 2 (- w text-w))
                          (+ (long col) 2))

                        part
                        (case kind
                          :head
                          :head

                          :more
                          :more

                          column-key)]

                    [(cond-> (conj out [(+ (long col) w 3) "│" :chrome])
                       (seq text)
                       (conj [at text part])) (+ (long col) w 3)]))
                [[[0 "│" :chrome]] 0]
                columns)))))

(defn table-text
  "Plain text of the agent table for `rows` at `width` cells, as the painter shows it."
  [rows width now-ms]
  (let [columns (table-columns rows width)]
    (mapv #(segments-text (table-segments % columns now-ms)) (table-lines rows))))

(defn- paint-table-line!
  "Paint one agent table line at screen row `y` from column `left` across `width` cells, on
   background `bg`."
  [^TextGraphics g left y width line columns now-ms bg]
  (p/clear-styles! g)
  (p/set-colors! g t/code-block-fg bg)
  (p/fill-rect! g left y (min (long width) (table-width columns)) 1)
  (doseq [[col text part]
          (table-segments line columns now-ms)

          :let [room
                (- (long width) (long col))]
          :when (pos? room)]

    (let [text
          (p/ellipsize (str text) room)

          x
          (+ (long left) (long col))]

      (case part
        :chrome
        (do (p/set-colors! g t/code-border-fg bg) (p/put-str! g x y text))

        :head
        (do (p/set-colors! g t/code-block-fg bg) (p/styled g [p/BOLD] (p/put-str! g x y text)))

        :more
        (do (p/set-colors! g t/dialog-hint bg) (p/put-str! g x y text))

        (do (p/set-colors! g (part-fg part (:row line) bg) bg)
            (if (#{:key :label :status} part)
              (p/styled g [p/BOLD] (p/put-str! g x y text))
              (p/put-str! g x y text))))))
  (p/clear-styles! g))

(defn- paint-team-line!
  "Paint one transcript line of a team table."
  [^TextGraphics g left row width viewport-top {:keys [team-line bg]}]
  (let [{:keys [line columns]} team-line]
    (paint-table-line! g
                       left
                       row
                       width
                       line
                       columns
                       (System/currentTimeMillis)
                       (or bg t/terminal-bg))
    (when (= :row (:kind line))
      (register-row! (:row line) (+ (long viewport-top) (long row)) left width))))

(defn clock-ms
  "The time of the stored team clock `clock` at `now-ms`, in ms: the work that ended, plus the
   work that runs now from `busy_since`. The store joins the work of the agents and the gateway
   gives the clock, so this reads no agent row."
  ^long [clock now-ms]
  (let [busy (get clock "busy_since")]
    (+ (long (get clock "worked_ms" 0))
       (if (number? busy) (max 0 (- (long now-ms) (long busy))) 0))))

(defn card-node-id
  "The fold node of the transcript block of team `team-id`. The block starts folded to its head
   line (user decision), so only a click or a bulk unfold shows its table."
  [team-id]
  (str "agent-team:" team-id))

(def ^:private ^:const card-max-dots
  "The most status dots on the head line of a team block. `+N more` counts the other agents."
  3)

(defn- card-dots
  "The status dots of the agent rows of `rows` on the head line of a team block, from column
   `col`: three at most in start order, then `+N more` (user decision, the same as the Companion
   team line). Returns `[segments end]`: `[col text :dot tone]` and `[col text :more]` vectors,
   and the column after them."
  [rows col]
  (let [agents
        (vec (remove leader? rows))

        shown
        (subvec agents 0 (min (count agents) (long card-max-dots)))

        dots
        (vec (map-indexed (fn [i row]
                            [(+ (long col) (long i)) "●" :dot (:tone row)])
                          shown))

        dots-end
        (+ (long col) (count shown))

        more
        (- (count agents) (count shown))]

    (if (pos? more)
      (let [text (str "+" more " more")]
        [(conj dots [(inc dots-end) text :more]) (+ (inc dots-end) (count text))])
      [dots dots-end])))

(defn- head-segments
  "The fold mark, the team, a status dot for each agent of `rows` and the `time` text, as
   `[col text part]` vectors. The transcript block and the live dock share this line."
  [expanded? rows time]
  (let [[dots end]
        (card-dots rows 8)]

    (cond-> (into [[0 (if expanded? "▾" "▸") :mark] [2 "TEAM" :head]] dots)
      time
      (conj [(if (seq dots) (+ (long end) 2) 8) time :time]))))

(defn card-head-segments
  "The painted parts of the head line of a team block: `[col text part]` vectors. `part` is
   `:mark`, `:head`, `:dot`, `:more` or `:time`, and a `:dot` vector ends with the status tone of
   its agent. The fold mark, the team, a status dot for each agent and the time, like the
   Companion team line: `▸ TEAM  ●●● +2 more  3m 20s`."
  [{:keys [expanded? clock rows]}]
  (head-segments expanded?
                 rows
                 (when (and clock (not (get clock "busy_since"))) (duration-text (clock-ms clock 0)))))

(defn- paint-head-segments!
  "Paint the head `segments` of a team block or of the dock on row `y` on `bg`, from column `left`
   across `width` cells."
  [^TextGraphics g left y width segments bg]
  (doseq [[col text part tone]
          segments

          :let [room
                (- (long width) (long col))]
          :when (pos? room)]

    (let [x
          (+ (long left) (long col))

          text
          (p/ellipsize (str text) room)]

      (case part
        :head
        (do (p/set-colors! g t/code-block-fg bg) (p/styled g [p/BOLD] (p/put-str! g x y text)))

        :dot
        (do (p/set-colors! g (t/legible-ink (tone-ink tone) bg) bg) (p/put-str! g x y text))

        (do (p/set-colors! g t/dialog-hint bg) (p/put-str! g x y text)))))
  (p/clear-styles! g))

(defn- paint-card-head!
  "Paint the head line of a team block. A click on it folds or unfolds the table."
  [^TextGraphics g left row width viewport-top {:keys [team-head session-id node-id collapsed? bg]}]
  (let [bg
        (or bg t/terminal-bg)

        y
        (long row)]

    ;; One background across the line, as a table line has, so no word wears its own.
    (p/clear-styles! g)
    (p/set-colors! g t/code-block-fg bg)
    (p/fill-rect! g left y (max 1 (long width)) 1)
    (paint-head-segments! g left y width (card-head-segments team-head) bg)
    (.register interactions/hit-map
               {:bounds {:row (+ (long viewport-top) y) :col left :width (max 1 (long width))}
                :kind :toggle-details
                :session-id session-id
                :node-id node-id
                :collapsed? collapsed?})))

(defn card
  "The team block data of team `team-id` from `agents`, or nil when the team has no agent. The
   row of the leader turn `leader` (see `leader-row`) comes first. `decorate-messages` adds the
   fold state: `:session-id`, `:node-id`, `:expanded?` and the stored `:clock` of the team."
  ([team-id agents] (card team-id agents nil))
  ([team-id agents leader]
   (when-let [rows (seq (turn-rows team-id agents))]
     (let [rows (into (if-let [row (leader-row leader)]
                        [row]
                        [])
                      rows)]
       {:team-id team-id :rows rows :columns (row-columns rows)}))))

(defn card-entries
  "Transcript rows of a team block at `width` cells: `{:line :meta}` maps. The head line folds
   and unfolds the block; an unfolded block shows the table under it. A blank row stands before
   and after the block. Each row stands on the surface of its message (`:plain-surface?`), not on
   the Code band: a team is no code (user decision)."
  [{:keys [rows expanded? session-id node-id] :as team-card} width]
  (let [marker
        m/MARKER_ACTIVITY

        columns
        (table-columns rows width)

        now
        (System/currentTimeMillis)

        head
        {:line (str marker (segments-text (card-head-segments team-card)))
         :meta {:kind :activity-live-entry
                :plain-surface? true
                :live-paint paint-card-head!
                :team-head (select-keys team-card [:expanded? :clock :rows])
                :session-id (str session-id)
                :node-id node-id
                :collapsed? (not expanded?)}}]

    (into [{:line marker :meta {:plain-surface? true}} head]
          (concat (when expanded?
                    (map (fn [line]
                           {:line (str marker (segments-text (table-segments line columns now)))
                            :meta {:kind :activity-live-entry
                                   :plain-surface? true
                                   :live-paint paint-team-line!
                                   :team-line {:line line :columns columns}}})
                         (table-lines rows)))
                  [{:line marker :meta {:plain-surface? true}}]))))

(defn- card-width ^long [bubble-w] (max 1 (- (long bubble-w) 4)))

(defn card-height
  "Rows that `with-card-lines` appends for `team-card` at bubble width `bubble-w`: 0 without a
   card. Layout adds it to the cached height of the message without its card."
  ^long [team-card _bubble-w]
  (if-let [rows (seq (:rows team-card))]
    (+ 4 (long (if (:expanded? team-card) (count (table-lines rows)) 0)))
    0))

(defn- gap-line?
  "True for a terminal-bg blank row: empty, or the iteration pad marker alone."
  [line]
  (let [line (str line)]
    (or (= "" line)
        (and (str/starts-with? line m/MARKER_ITERATION_PAD) (str/blank? (subs line 1))))))

(defn with-card-lines
  "Put the team block of `projected` (an assistant message with `:agent-team-card`) into its
   painted lines at bubble width `bubble-w`: before the answer at `:answer-line`, else at the
   end. One blank row parts the block from the side that has no gap yet. A windowed slice
   keeps its lines."
  [projected bubble-w]
  (if-let [team-card (and (not (:lines-window projected))
                          (:prewrapped-lines projected)
                          (:agent-team-card projected))]
    (let [lines (vec (:prewrapped-lines projected))
          line-meta (vec (or (:line-meta projected) (repeat (count lines) nil)))
          at (min (long (or (:answer-line projected) (count lines))) (count lines))
          entries (card-entries team-card (card-width bubble-w))
          ;; The block always adds one blank row, so `card-height` stays exact.
          gap-after? (and (< at (count lines))
                          (not (gap-line? (nth lines at)))
                          (or (zero? at) (gap-line? (nth lines (dec at)))))
          block (if gap-after?
                  (conj (vec entries) {:line "" :meta nil})
                  (into [{:line "" :meta nil}] entries))]

      (assoc projected
        :prewrapped-lines (-> (subvec lines 0 at)
                              (into (map :line) block)
                              (into (subvec lines at)))
        :line-meta (-> (subvec line-meta 0 at)
                       (into (map :meta) block)
                       (into (subvec line-meta at)))))
    projected))

(defn dock-team-ids
  "Teams that the dock shows, by the turn of their current task (`current-turn`): the team of
   the running turn `running-turn`, also the agents that it pinged, and each team with an agent
   that still works or waits."
  [agents running-turn]
  (into (if (and running-turn (some #(= running-turn (current-turn %)) agents)) #{running-turn} #{})
        (comp (filter active?) (keep current-turn))
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
  "Give the last assistant message of each turn in which a team worked its `:agent-team-card`.
   The rows and the clock of a team come from the transcript turns (`:session-agents`,
   `:session-team-clock`), and the cached snapshot of `sid` replaces each row and clock that it
   also has, so a card paints with its turn. A team in the dock gets
   no block: the turn `running-turn` still runs, or an agent still works. `open?` tells from the
   fold node of a card (`card-node-id`) whether the card shows its table; without it, each card
   is folded. Returns `messages` unchanged when no team is known. An unchanged team keeps the
   decorated message identity, so height caches still hit."
  ([messages sid] (decorate-messages messages sid nil))
  ([messages sid running-turn] (decorate-messages messages sid running-turn (constantly false)))
  ([messages sid running-turn open?]
   (let [agents
         (merge-agents (merge-agents []
                                     (without-removed (mapcat :session-agents messages)
                                                      (:gone (summary sid))))
                       (:agents (summary sid)))

         clocks
         (merge (clock-rows (keep :session-team-clock messages)) (:clocks (summary sid)))]

     (if (or (empty? agents) (empty? messages))
       messages
       (let [docked
             (dock-team-ids agents running-turn)

             teams
             (into #{} (comp (mapcat turns-of) (remove nil?) (remove docked)) agents)

             last-index
             (reduce (fn [acc [i m]]
                       (let [turn (:session-turn-id m)]
                         (if (and (= :assistant (:role m)) (contains? teams turn))
                           (assoc acc turn i)
                           acc)))
                     {}
                     (map-indexed vector messages))]

         (if (empty? last-index)
           messages
           (reduce-kv (fn [out team-id i]
                        (if-let [team-card (card team-id agents (message-leader (nth out i)))]
                          (let [node-id (card-node-id team-id)
                                team-card (assoc team-card
                                            :session-id (str sid)
                                            :node-id node-id
                                            :expanded? (boolean (open? node-id))
                                            :clock (get clocks team-id))]

                            (assoc out i (decorate (nth out i) team-card)))
                          out))
                      (vec messages)
                      last-index)))))))

;; ── Dock ────────────────────────────────────────────────────────────────────

(def ^:private ^:const dock-keys
  "Dock rows that C-x t and a digit open: rows 1 to 9 show their digit in the key column."
  9)

(defn- dock-rows
  "The agent rows of the docked teams of `sid`, in start order. Rows 1 to 9 have their digit as
   `:key`. Empty without a docked team."
  [sid running-turn]
  (let [agents
        (:agents (summary sid))

        docked
        (dock-team-ids agents running-turn)]

    (->> (numbered agents)
         (filter #(contains? docked (current-turn (second %))))
         (sort-by (fn [[n agent]]
                    [(or (get agent "created_at") 0) (str (get agent "team_id")) n]))
         (map-indexed (fn [i [n agent]]
                        (cond-> (team-row n agent agents)
                          (< (long i) (long dock-keys))
                          (assoc :key (str (inc (long i)))))))
         vec)))

(defn dock-row
  "Dock row `n` of `sid`, the row that shows digit `n`, or nil."
  [sid running-turn n]
  (when (<= 1 (long n) (long dock-keys)) (get (dock-rows sid running-turn) (dec (long n)))))

(defn dock-agent-id
  "The session id of dock row `n` of `sid`, or nil. A task that did not start has no session to
   open yet, so its row gives nil."
  [sid running-turn n]
  (let [row (dock-row sid running-turn n)]
    (when-not (:pending? row) (:session-id row))))

(defn scroll-dock!
  "Move the agent table of the dock of `sid` by `delta` rows. Returns true when the shown rows
   changed. A collapsed dock or a team that fits does not move."
  [sid running-turn delta]
  (let [sid
        (str sid)

        n
        (count (dock-rows sid running-turn))

        size
        (dec (long dock-max-rows))]

    (when (and (dock-expanded? sid) (> n (long dock-max-rows)))
      (let [old
            (window-start n size (get @dock-offsets sid))

            new
            (window-start n size (+ old (long delta)))]

        (swap! dock-offsets assoc sid new)
        (not= old new)))))

(defn- dock-width
  "Content cells of the dock frame at `cols` columns. The frame spans columns 1 to `cols - 2`,
   and a border and one space stand at each side of the content."
  ^long [cols]
  (max 1 (- (long cols) 6)))

(def ^:private ^:const phone-width
  "At or below these content cells the team line writes `+N`, not `+N more`, and a short rule."
  70)

(def ^:private ^:const title-cells
  "Cells of the title column of the dock: the title, then the chord that opens its agent."
  30)

(def ^:private ^:const status-cells "Cells of the status column: `RUNNING`." 7)

(def ^:private ^:const time-cells
  "The fewest cells of the time column: a leader time with its wait, `3m 42s (waited 1m 10s)`."
  22)

(def ^:private ^:const note-min-cells
  "The fewest cells of the note column. A narrower dock stacks each agent on its own lines."
  12)

(def ^:private ^:const phone-note-lines "The most lines of a note in a stacked dock." 2)

(def ^:private cell-rule
  "The rule between two cells of a dock row, as in the Companion team table."
  " │ ")

(def ^:private leader-key "The key of the leader lane of the timeline." "L")

(defn- open-chord
  "The chord that opens the agent of dock row `row`, such as `(C-x t 2)`. A task that did not
   start has no session to open, so its row has no chord."
  [row]
  (when (and (:key row) (:session-id row) (not (:pending? row)))
    (str "(" (keymap/group-chord :agents) " " (:key row) ")")))

(defn- seg-width ^long [segs] (reduce + 0 (map #(long (p/display-width (str (second %)))) segs)))

(defn- run-segs
  "`parts`, `[text ink bold?]` vectors, as segments from column `col`. Returns `[segments end]`."
  [col parts]
  (reduce (fn [[segs col] [text ink bold?]]
            [(conj segs [col text ink bold?]) (+ (long col) (long (p/display-width (str text))))])
          [[] col]
          parts))

(defn- cell-segs
  "One dock row from `cells`, `[width parts align]` vectors, from column `col`. A `cell-rule`
   stands between two cells. `parts` are `[text ink bold?]` vectors, cut to the cell."
  [col cells]
  (first
    (reduce (fn [[segs col] [i [width parts align]]]
              (let [width
                    (long width)

                    parts
                    (loop [[[text ink bold?] & more] parts room width out []]
                      (if (or (nil? text) (<= room 0))
                        out
                        (let [text (p/ellipsize (str text) room)]
                          (recur more
                                 (- room (long (p/display-width text)))
                                 (conj out [text ink bold?])))))

                    used
                    (reduce + 0 (map #(long (p/display-width (first %))) parts))

                    [cell]
                    (run-segs (if (= :right align) (+ (long col) (- width used)) col) parts)

                    end
                    (+ (long col) width)]

                [(cond-> (into segs cell)
                   (< (long i) (dec (count cells)))
                   (conj [end cell-rule :edge]))
                 (+ end 3)]))
            [[] col]
            (map-indexed vector cells))))

(defn- framed
  "Content `segs` of a dock line inside the side borders of a frame `iw` content cells wide."
  [iw segs]
  (-> [[0 "│" :edge]]
      (into (map (fn [[col & more]] (into [(+ 2 (long col))] more))) segs)
      (conj [(+ (long iw) 3) "│" :edge])))

(defn- edge-line
  "A border line of a frame `iw` content cells wide: `left`, `mid` cells and `right`. `cuts` are
   content columns where `cut` replaces `mid`, so a rule meets the column rules."
  ([iw left mid right] (edge-line iw left mid right #{} nil))
  ([iw left mid right cuts cut]
   [[0
     (apply str
            (for [i (range (+ (long iw) 4))]
              (cond (zero? (long i)) left
                    (= (long i) (+ (long iw) 3)) right
                    (contains? cuts (- (long i) 2)) cut
                    :else mid)))
     :edge]]))

(defn- role-part
  "The role word of `row` in the ink of its role. The leader word is bold."
  [row]
  [(:role row) [:role (:role-key row)] (leader? row)])

(defn- status-part [row] [(:status row) [:tone (:tone row)]])

(defn- title-parts
  "The title of `row` in bold, padded to `width` cells less its open chord, then the chord."
  [row width]
  (let [chord
        (open-chord row)

        room
        (max 1 (- (long width) (if chord (inc (count chord)) 0)))]

    (cond-> [[(pad (p/ellipsize (str (:label row)) room) room) :fg true]]
      chord
      (conj [(str " " chord) :dim]))))

(defn- table-widths
  "The cells of the dock table columns at `iw` content cells, with a time column of `time-w`, or
   nil when the note column would be narrower than `note-min-cells`."
  [iw time-w]
  (let [role
        (role-width)

        note
        (- (long iw) role (long title-cells) (long time-w) (long status-cells) 12)]

    (when (>= note (long note-min-cells))
      {:role role :title title-cells :time time-w :status status-cells :note note})))

(defn- dock-time-width
  "Cells of the time column for the time texts `times`. A short timeline shows no leader wait,
   so its column only fits the texts."
  (^long [times] (dock-time-width times false))
  (^long [times short?] (max (if short? 6 (long time-cells)) (long (widest times)))))

(defn- wrap-lines
  "`text` in lines of at most `width` cells, at most `n` lines. A cut last line ends in `…`."
  [text width n]
  (let [width
        (max 1 (long width))

        words
        (str/split (str/trim (str text)) #"\s+")

        lines
        (loop [[word & more :as words] words line "" out []]
          (cond (nil? word) (cond-> out (seq line) (conj line))
                (str/blank? word) (recur more line out)
                (str/blank? line) (recur more word out)
                (<= (+ (count line) 1 (long (p/display-width word))) width)
                (recur more (str line " " word) out)
                :else (recur words "" (conj out line))))]

    (if (> (count lines) (long n))
      (conj (vec (take (dec (long n)) lines))
            (p/ellipsize (str (nth lines (dec (long n))) " " (nth lines n)) width))
      (mapv #(p/ellipsize % width) lines))))

(defn- dock-table-lines
  "The lines between the team line and the bottom border of an open dock that shows the table."
  [leader shown more iw]
  (if (table-widths iw time-cells)
    (-> [{:kind :mid}]
        (cond->
          leader
          (into [{:kind :leader :row leader} {:kind :rule}]))
        (into (map (fn [row]
                     {:kind :agent :row row}))
              shown)
        (cond-> more (conj more)))
    (-> [{:kind :mid}]
        (cond->
          leader
          (into (cons {:kind :phone-head :row leader}
                      (map (fn [text]
                             {:kind :phone-note :text text})
                           (wrap-lines (:now leader) iw phone-note-lines))))
          (and leader (seq shown))
          (conj {:kind :mid}))
        (into (comp (map (fn [row]
                           (into [{:kind :phone-head :row row} {:kind :phone-title :row row}]
                                 (map (fn [text]
                                        {:kind :phone-note :text text}))
                                 (wrap-lines (:now row) iw phone-note-lines))))
                    (interpose [{:kind :faint}])
                    cat)
              shown)
        (cond-> more (conj more)))))

(defn- dock-timeline-lines
  "The lines between the team line and the bottom border of an open dock that shows the timeline:
   the axis, a lane for the leader and for each agent, a rule and the legend."
  [leader shown more]
  (-> [{:kind :mid} {:kind :axis}]
      (cond->
        leader
        (conj {:kind :lane :row leader}))
      (into (map (fn [row]
                   {:kind :lane :row row}))
            shown)
      (cond-> more (conj more))
      (conj {:kind :mid})
      (cond->
        leader
        (conj {:kind :legend :row leader}))
      (into (map (fn [row]
                   {:kind :legend :row row}))
            shown)))

(defn dock-lines
  "The dock of `sid` above the prompt at `cols` columns: `{:line}` maps, one for each screen row.
   One blank line stands above the frame. A folded dock shows the team line in a frame. An open
   dock adds the agent table or, after `toggle-timeline!`, the timeline. The row of the running
   turn `leader` (see `leader-row`) stands first and does not scroll. A dock narrower than the
   table stacks each agent on its own lines. With `max-lines`, an open dock shows fewer agent rows
   so that it takes at most that many lines, and folds when no agent row fits. Empty without a
   docked team."
  ([sid running-turn cols] (dock-lines sid running-turn cols nil nil))
  ([sid running-turn cols leader] (dock-lines sid running-turn cols leader nil))
  ([sid running-turn cols leader max-lines]
   (let [agents
         (:agents (summary sid))

         docked
         (dock-team-ids agents running-turn)]

     (if (empty? docked)
       []
       (let [rows
             (dock-rows sid running-turn)

             ;; Only the team of the running turn is the team of this leader turn.
             leader
             (when (contains? docked running-turn) (leader-row leader))

             mode
             (when (dock-expanded? sid) (if (dock-timeline? sid) :timeline :table))

             n
             (count rows)

             head
             {:kind :header
              :mode mode
              :rows rows
              :clocks (vec (keep #(get (:clocks (summary sid)) %) (sort docked)))}

             iw
             (dock-width cols)

             ;; The lines with `size` agent rows from the scroll offset, and a last line that
             ;; counts the others when they do not all show.
             build
             (fn [mode size]
               (let [overflow? (< (long size) n)
                     start (window-start n size (get @dock-offsets (str sid)))
                     shown (subvec rows start (+ start (long size)))
                     more (when overflow? {:kind :more :above start :n (- n start (long size))})]
                 (mapv (fn [line]
                         {:line line})
                       (-> [{:kind :gap} {:kind :top} (assoc head :mode mode)]
                           (into (case mode
                                   :table (dock-table-lines leader shown more iw)
                                   :timeline (dock-timeline-lines leader shown more)
                                   nil))
                           (conj {:kind :bottom})))))

             size
             (if (> n (long dock-max-rows)) (dec (long dock-max-rows)) n)]

         ;; With `max-lines`, an open dock shows fewer agents until it fits, then folds.
         (or (some (fn [size]
                     (let [lines (build mode size)]
                       (when (or (nil? max-lines) (<= (count lines) (long max-lines))) lines)))
                   (if mode (range size 0 -1) [size]))
             (build nil size)))))))

(defn dock-height
  "Rows that the dock of `sid` takes above the prompt, 0 without a docked team."
  (^long [sid running-turn cols] (dock-height sid running-turn cols nil nil))
  (^long [sid running-turn cols leader] (dock-height sid running-turn cols leader nil))
  ([sid running-turn cols leader max-lines]
   (count (dock-lines sid running-turn cols leader max-lines))))

(defn- team-time
  "The time of the docked teams at `now-ms`: the stored clock of the team, or the longest one."
  [clocks now-ms]
  (when (seq clocks) (duration-text (reduce max (map #(clock-ms % now-ms) clocks)))))

(defn- header-controls
  "The right part of the team line in `mode`: the fold control and the switch between the table
   and the timeline, each with its chord. `[parts switch]`: `[text ink bold?]` parts and the index
   of the first part of the switch. `short?` keeps only the marks."
  [mode short?]
  (let [dock
        (str " (" (keymap/label-for :toggle-agent-dock) ")")

        timeline
        (str " (" (keymap/label-for :toggle-agent-timeline) ")")

        switch
        (if (= :timeline mode) "▦ team" "▤ timeline")

        fold
        (if mode "fold" "details")

        mark
        #(subs % 0 1)

        rule
        [(if short? " │ " "  │  ") :edge]

        fold-parts
        [[(if short? (if mode "▴" "▸") fold) :fg true] [dock :dim]]

        switch-parts
        [[(if short? (mark switch) switch) :fg true] [timeline :dim]]]

    (if mode
      [(-> switch-parts
           (conj rule)
           (into fold-parts))
       0]
      [(-> fold-parts
           (conj rule)
           (into switch-parts))
       3])))

(defn header-segments
  "The painted parts of the team line of the dock at `iw` content cells: `[segments switch]`.
   Segments are `[col text ink bold?]` vectors from the left border. The fold mark, the team, a
   status dot for the first three agents and `+N more`, a rule and the team time; at the right
   the fold control and the switch between the table and the timeline with their chords. `switch`
   is `[col width]` of the switch, or nil. A short line keeps only the marks of the controls, then
   drops them."
  [{:keys [mode clocks rows]} iw now-ms]
  (let [iw
        (long iw)

        wide?
        (> iw (long phone-width))

        agents
        (vec (remove leader? rows))

        dots
        (take card-max-dots agents)

        extra
        (- (count agents) (count dots))

        time
        (team-time clocks now-ms)

        ;; A narrow line drops the time, then the count, then the dots.
        [left end]
        (some (fn [[dots? extra? time?]]
                (let [[_ end :as run]
                      (run-segs 2
                                (cond-> [[(if mode "▾ " "▸ ") :dim] ["TEAM" :fg true]]
                                  dots?
                                  (into (cons ["  " :dim]
                                              (map (fn [row]
                                                     ["●" [:tone (:tone row)]])
                                                   dots)))
                                  (and extra? (pos? extra))
                                  (conj [(str " +" extra (when wide? " more")) :dim])
                                  (and time? time)
                                  (into [[(if wide? "  │  " " │ ") :edge] [time :dim]])))]
                  (when (or (<= (long end) (+ iw 2)) (not dots?)) run)))
              [[true true true] [true true false] [true false false] [false false false]])

        room
        (- (+ iw 2) (long end) 2)

        [parts switch-at]
        (some (fn [short?]
                (let [[parts at] (header-controls mode short?)]
                  (when (<= (seg-width (map (fn [[text]] [0 text]) parts)) room) [parts at])))
              [false true])

        start
        (when parts (- (+ iw 2) (seg-width (map (fn [[text]] [0 text]) parts))))

        [right]
        (if parts (run-segs start parts) [[]])

        switch
        (when parts
          (let [[col] (nth right switch-at)]
            [col (seg-width (map (fn [[_ text]] [0 text]) (subvec right switch-at (+ (long switch-at) 2))))]))]

    [(framed iw (concat (map (fn [[col & more]] (into [(- (long col) 2)] more)) left)
                        (map (fn [[col & more]] (into [(- (long col) 2)] more)) right)))
     (when switch [(+ 2 (long (first switch)) -2) (second switch)])]))

(defn- time-text
  "The time cell of `row` at `now-ms`. A short dock shows only the time of a leader, not its wait."
  [row now-ms short?]
  (if (and short? (leader? row) (:start row))
    (duration-text (max 0 (- (long (or (:end row) now-ms)) (long (:start row)))))
    (str (row-time row now-ms))))

(defn- chart-span
  "The time of the chart of `rows` and `leader` in epoch ms, `[start end]`: from the first task
   or the start of the leader, to `now-ms` while anything works, else to the last end. Nil when
   the chart has no length. The rule of the Companion `chartSpan`."
  [rows leader now-ms]
  (let [starts
        (keep (fn [row]
                (or (:start (first (:runs row)))
                    (if (= "QUEUED" (:status row)) (:created-at row) (:start row))))
              rows)

        starts
        (cond-> (filter number? starts) (:start leader) (conj (:start leader)))]

    (when (seq starts)
      (let [start
            (long (reduce min starts))

            ends
            (filter number? (cond-> (map :end rows) leader (conj (:end leader))))

            working?
            (or (some :active? rows) (and leader (nil? (:end leader))))

            end
            (cond working? (reduce max (long now-ms) ends)
                  (seq ends) (reduce max ends))]

        (when (and end (> (long end) start)) [start (long end)])))))

(defn- leader-parts
  "The parts of the lane of `leader` in epoch ms, `[start end wait?]`, in time order: its work and
   its waits for its agents. A wait is cut to the time of the lane, and an overlap counts once."
  [leader now-ms]
  (let [end
        (long (or (:end leader) now-ms))

        [parts at]
        (reduce (fn [[parts at] [ws we]]
                  (let [from (max (long at) (long ws))
                        to (min end (long (or we now-ms)))]
                    (if (<= to from)
                      [parts at]
                      [(cond-> parts
                         (> from (long at)) (conj [at from false])
                         true (conj [from to true]))
                       to])))
                [[] (long (:start leader))]
                (sort-by first (:waits leader)))]

    (cond-> parts (> end (long at)) (conj [at end false]))))

(defn- agent-parts
  "The parts of the lane of agent row `row` in epoch ms, `[start end wait? tone]`: each earlier
   run in the tone of its status, a failed wait after a failed run, then the current task. A
   queued task waits from its spawn to `chart-end`. The rule of the Companion `agentBars`."
  [row now-ms chart-end]
  (let [queued?
        (= "QUEUED" (:status row))

        start
        (if queued? (:created-at row) (:start row))

        end
        (cond queued? chart-end
              (= "RUNNING" (:status row)) now-ms
              :else (:end row))

        runs
        (vec (:runs row))]

    (cond-> (vec (mapcat (fn [i {run-start :start run-end :end word :word}]
                           (let [next (or (:start (get runs (inc (long i)))) start)
                                 tone (tone word)]
                             (cond-> [[run-start run-end false tone]]
                               (and (= :bad tone) (number? next) (> (long next) (long run-end)))
                               (conj [run-end next true :bad]))))
                         (range)
                         runs))
      (and (number? start) (number? end))
      (conj [start end queued? (:tone row)]))))

(defn- lane-cells
  "The bar of a lane, `bar-w` cells, as `[text ink]` parts: `━` for work in the ink of its tone,
   `╌` for a wait, and `┊` at each mark of the axis where the lane is empty."
  [parts [span-start span-end] bar-w marks]
  (let [bar-w
        (long bar-w)

        total
        (double (- (long span-end) (long span-start)))

        cell
        (fn [at]
          (min bar-w (max 0 (Math/round (* bar-w (/ (- (double at) (long span-start)) total))))))

        filled
        (reduce (fn [bar [a b wait? tone]]
                  (let [from (min (dec bar-w) (long (cell a)))
                        to (max (inc from) (long (cell b)))]
                    (reduce (fn [bar i]
                              (assoc bar i (if wait? ["╌" (if (= :bad tone) [:tone :bad] :dim)] ["━" [:tone tone]])))
                            bar
                            (range from (min bar-w to)))))
                (vec (repeat bar-w [" " :dim]))
                parts)

        grid
        (set (map #(cell (+ (long span-start) (long %))) (rest marks)))]

    (map-indexed (fn [i [text :as part]]
                   (if (and (= " " text) (contains? grid i)) ["┊" :edge] part))
                 filled)))

(defn- axis-label
  "A mark of the dock timeline axis: `0s`, `45s`, `2m` or `2m 30s`."
  [ms]
  (let [s (quot (long ms) 1000)]
    (cond (< s 60) (str s "s")
          (zero? (rem s 60)) (str (quot s 60) "m")
          :else (format "%dm %02ds" (quot s 60) (rem s 60)))))

(defn- legend-widths
  "Cells of the title and the note of a legend row at `iw` content cells: `[title note]`. A
   narrow dock gives the title the rest of the row and has no note."
  [iw]
  (let [base (+ 1 (role-width) 6)
        note (- (long iw) base (long title-cells) 3)]
    (if (>= note (long note-min-cells))
      [title-cells note]
      [(- (long iw) base) nil])))

(defn- line-segments
  "The painted parts of dock line `line` at `iw` content cells and `now-ms`: `[col text ink
   bold?]` vectors from the left border. `ctx` holds the shown rows and the time span of the
   chart, which every lane shares."
  [{:keys [kind row text above n] :as line} iw now-ms {:keys [rows leader span]}]
  (let [iw
        (long iw)

        short?
        (<= iw (long phone-width))]

    (case kind
      :gap
      []

      :top
      (edge-line iw "┌" "─" "┐")

      :mid
      (edge-line iw "├" "─" "┤")

      :bottom
      (edge-line iw "└" "─" "┘")

      :header
      (first (header-segments line iw now-ms))

      (:leader :rule :agent)
      (let [times
            (map #(time-text % now-ms false) (cond->> rows leader (cons leader)))

            ;; The layout chose the table at the narrowest time column, so that one always fits.
            {:keys [role title note time status]}
            (or (table-widths iw (dock-time-width times)) (table-widths iw time-cells))]

        (case kind
          :leader
          (framed iw
                  (cell-segs 0
                             [[role [(role-part row)]]
                              [(+ (long title) 3 (long note)) [[(str (:now row)) :dim]]]
                              [time [[(time-text row now-ms false) :dim]] :right]
                              [status [(status-part row)]]]))

          :rule
          (edge-line iw
                     "├"
                     "─"
                     "┤"
                     #{(inc (long role))
                       (+ (long role) 3 (long title) 3 (long note) 1)
                       (+ (long role) 3 (long title) 3 (long note) 3 (long time) 1)}
                     "┼")

          :agent
          (framed iw
                  (cell-segs 0
                             [[role [(role-part row)]]
                              [title (title-parts row title)]
                              [note [[(str (:now row)) :dim]]]
                              [time [[(time-text row now-ms false) :dim]] :right]
                              [status [(status-part row)]]]))))

      :more
      (framed iw [[0 (p/ellipsize (str/join "  "
                                            (cond-> []
                                              (pos? (long above)) (conj (str "↑" above " above"))
                                              (pos? (long n)) (conj (str "+" n " more"))))
                                  iw)
                   :dim]])

      :phone-head
      (let [time (time-text row now-ms false)
            right (cond-> [] (seq time) (into [[time :dim] ["  " :dim]]) true (conj (status-part row)))
            [segs] (run-segs (- iw (seg-width (map (fn [[t]] [0 t]) right))) right)]
        (framed iw (into [(into [0] (role-part row))] segs)))

      :phone-title
      (let [chord (open-chord row)
            room (max 1 (- iw (if chord (inc (count chord)) 0)))]
        (framed iw (cond-> [[0 (p/ellipsize (str (:label row)) room) :fg true]]
                     chord (conj [(- iw (count chord)) chord :dim]))))

      :phone-note
      (framed iw [[0 text :dim]])

      :faint
      (framed iw [[0 (apply str (repeat iw "┈")) :edge]])

      :axis
      (let [times (map #(time-text % now-ms short?) (cond->> rows leader (cons leader)))
            time-w (dock-time-width times short?)
            bar-w (max 1 (- iw 1 (long time-w) (long status-cells) 9))
            [start end] (or span [0 1])
            total (max 1 (- (long end) (long start)))
            labels (keep (fn [at]
                           (let [label (axis-label at)
                                 col (min (- bar-w (count label))
                                          (long (Math/floor (* bar-w (/ (double at) total)))))]
                             (when (>= col 0) [(+ 4 col) label :dim])))
                         (time-axis/axis-marks total))]
        (framed iw (concat (cell-segs 0 [[1 []] [bar-w []] [time-w []] [status-cells []]]) labels)))

      :lane
      (let [times (map #(time-text % now-ms short?) (cond->> rows leader (cons leader)))
            time-w (dock-time-width times short?)
            bar-w (max 1 (- iw 1 (long time-w) (long status-cells) 9))
            [start end] span
            parts (if (leader? row)
                    (map (fn [[a b wait?]] [a b wait? (:tone row)]) (leader-parts row now-ms))
                    (agent-parts row now-ms end))]
        (framed iw
                (cell-segs 0
                           [[1 [(if (leader? row)
                                  [leader-key [:role "leader"] true]
                                  [(or (:key row) "·") :fg true])]]
                            [bar-w (if span (lane-cells parts span bar-w (time-axis/axis-marks (- (long end) (long start)))) [])]
                            [time-w [[(time-text row now-ms short?) :dim]] :right]
                            [status-cells [(status-part row)]]])))

      :legend
      (let [[title-w note-w] (legend-widths iw)
            role-w (role-width)]
        (framed iw
                (cell-segs 0
                           (if (leader? row)
                             [[1 [[leader-key [:role "leader"] true]]]
                              [role-w [(role-part row)]]
                              [(+ (long title-w) (if note-w (+ 3 (long note-w)) 0)) [[(str (:now row)) :dim]]]]
                             (cond-> [[1 [[(or (:key row) "·") :fg true]]]
                                      [role-w [(role-part row)]]
                                      [title-w (title-parts row title-w)]]
                               note-w (conj [note-w [[(str (:now row)) :dim]]])))))))))

(defn- dock-context
  "What every line of the dock of `sid` shares at `now-ms`: the shown rows, the leader row and
   the time span of the chart."
  [lines now-ms]
  (let [rows (keep #(when (#{:agent :lane :phone-head} (:kind %)) (:row %)) (map :line lines))
        leader (first (filter leader? rows))
        rows (vec (remove leader? rows))]
    {:rows rows :leader leader :span (chart-span rows leader now-ms)}))

(defn dock-text
  "Plain dock lines of `sid`, as the painter shows them, from the left border of the frame."
  ([sid running-turn cols now-ms] (dock-text sid running-turn cols now-ms nil))
  ([sid running-turn cols now-ms leader] (dock-text sid running-turn cols now-ms leader nil))
  ([sid running-turn cols now-ms leader max-lines]
   (let [lines (dock-lines sid running-turn cols leader max-lines)
         ctx (dock-context lines now-ms)
         iw (dock-width cols)]
     (mapv (fn [{:keys [line]}] (segments-text (line-segments line iw now-ms ctx))) lines))))

(defn- segment-ink
  "The ink of the segment ink key `ink` on `bg`."
  [ink bg]
  (if (vector? ink)
    (let [[kind value] ink]
      (t/legible-ink (if (= :role kind) (role-ink value) (tone-ink value)) bg))
    (case ink
      :edge t/dialog-border
      :fg t/code-block-fg
      t/dialog-hint)))

(defn- paint-segments!
  "Paint `[col text ink bold?]` segments on row `y` from column `left` on `bg`."
  [^TextGraphics g left y segments bg]
  (doseq [[col text ink bold?] segments]
    (p/set-colors! g (segment-ink ink bg) bg)
    (if bold?
      (p/styled g [p/BOLD] (p/put-str! g (+ (long left) (long col)) y (str text)))
      (p/put-str! g (+ (long left) (long col)) y (str text))))
  (p/clear-styles! g))

(defn draw-dock!
  "Paint the dock of `sid` at screen row `top` across `cols` columns. `leader` is the running
   turn, as `dock-lines` takes it. A click on the team line folds or opens the dock, and a click on
   its switch shows the timeline or the table. A click on an agent row opens its agent."
  ([g sid running-turn top cols] (draw-dock! g sid running-turn top cols nil nil))
  ([g sid running-turn top cols leader] (draw-dock! g sid running-turn top cols leader nil))
  ([^TextGraphics g sid running-turn top cols leader max-lines]
   (let [iw
         (dock-width cols)

         now
         (System/currentTimeMillis)

         lines
         (dock-lines sid running-turn cols leader max-lines)

         ctx
         (dock-context lines now)

         all
         (max 1 (long cols))]

     (doseq [[i {:keys [line]}] (map-indexed vector lines)
             :let [y (+ (long top) (long i))]]
       (p/set-colors! g t/code-block-fg t/terminal-bg)
       (p/fill-rect! g 0 y all 1)
       (paint-segments! g 1 y (line-segments line iw now ctx) t/terminal-bg)
       ;; Each dock line takes the wheel, so the wheel scrolls the agent table, not the transcript.
       (case (:kind line)
         :gap
         nil

         :header
         (let [[_ [col width]] (header-segments line iw now)
               region (fn [kind from to]
                        (when (> (long to) (long from))
                          (.register interactions/hit-map
                                     {:bounds {:row y :col from :width (- (long to) (long from))}
                                      :kind kind
                                      :text (str sid)
                                      :dock (str sid)})))]
           (if col
             (let [from (inc (long col))
                   to (+ from (long width))]
               (region :agent-dock-toggle 0 from)
               (region :agent-timeline-toggle from to)
               (region :agent-dock-toggle to all))
             (region :agent-dock-toggle 0 all)))

         (if (:session-id (:row line))
           (register-row! (:row line) y 0 all (str sid))
           (.register interactions/hit-map
                      {:bounds {:row y :col 0 :width all}
                       :kind :agent-dock
                       :dock (str sid)})))))))

(defn- key-type [key] (when (instance? KeyStroke key) (.getKeyType ^KeyStroke key)))

;; ── Breadcrumb ──────────────────────────────────────────────────────────────

(defn breadcrumb
  "Header path of child session `sid` from its cached team, such as `LEADER → IMPLEMENT | Fix
   parser`: `{:text :parent-id}`, or nil when the TUI does not know the parent."
  [sid]
  (let [pairs
        (numbered (:agents (summary sid)))

        name-of
        (fn [[n agent]]
          (str (some-> (role-word agent)
                       (str " | "))
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
                        "LEADER")
                      " → "
                      (name-of pair))})))))

(defn wait-line
  "Why child session `sid` did not start, from its cached team, such as `Waiting for a7`, or nil
   when it started or the TUI does not know it."
  [sid]
  (let [agents (:agents (summary sid))]
    (when-let [agent (some #(when (= (str sid) (get % "session_id")) %) agents)]
      (when (pending? agent)
        (let [line (end-reason agent agents)]
          (str (str/upper-case (subs line 0 1)) (subs line 1)))))))

(defn- shown-change?
  "Whether a refresh from `before`, the last snapshot, to `after` can change the screen. An
   active agent shows an elapsed time."
  [before after]
  (or (nil? before)
      (not= (dissoc before :checked-at) (dissoc after :checked-at))
      (boolean (some active? (:agents after)))))

(def ^:private ^:const tick-ms
  "How often an active team repaints its elapsed times. A repaint reads nothing."
  5000)

(defn- take-work!
  "Take the queued work of `sid`."
  [sid]
  (let [k (str sid)]
    (get (first (swap-vals! pending dissoc k)) k)))

(defn start-refresh!
  "One screen-owned worker keeps the team of the active session. It reads the Subagents
   setting when the active session changes, and then does only queued work:
   `subscription.ready`, `session.team_updated`, a dropped stream or a settings change. The
   team rows come only from the event stream, so the worker never reads the team. Call the
   returned cleanup on screen teardown. No network work runs in paint."
  [session-id changed!]
  (let [running?
        (atom true)

        worker
        (Thread.
          ^Runnable
          (fn []
            (try
              (loop [previous-id
                     nil

                     tick-at
                     0]

                (when @running?
                  (let [sid
                        (session-id)

                        now
                        (System/currentTimeMillis)

                        switched?
                        (and sid (not= sid previous-id))

                        work
                        (cond-> (when sid (take-work! sid))
                          switched?
                          (assoc :setting? true))

                        tick?
                        (and sid
                             (not switched?)
                             (>= now (long tick-at))
                             (boolean (some active? (:agents (summary sid)))))]

                    (when (seq work)
                      (let [before
                            (summary sid)

                            after
                            (sync! sid work)]

                        ;; A slow setting read for an old tab must not repaint the new tab.
                        ;; An unchanged idle team must not force a full frame.
                        (when (and @running? (= sid (session-id)) (shown-change? before after))
                          (changed!))))
                    (when (and tick? @running? (= sid (session-id))) (changed!))
                    (Thread/sleep 250)
                    (recur sid (if (or switched? tick?) (+ now (long tick-ms)) tick-at)))))
              (catch InterruptedException _ nil)))
          "vis-tui-agent-team-refresh")]

    (.setDaemon worker true)
    (.start worker)
    (fn []
      (reset! running? false)
      (.interrupt worker)
      (.join worker 2500))))

(def ^:private ^:const modal-row-width "The most cells of one agent row in the team modal." 200)

(defn- modal-row-cells
  "Cells of one agent row in the team modal at `cols` terminal columns. The list content is
   at most `cols - 8` cells, and a label takes 5 of them for the gutter, so the status and
   time at the end of a row stay in view."
  ^long [cols]
  (max 24 (min (long modal-row-width) (- (long cols) 13))))

(defn items
  "The team modal items of `rows`: one section for each turn team, newest first, with one
   row for each agent. A row is `width` cells wide."
  ([rows parent-id] (items rows parent-id modal-row-width))
  ([rows parent-id width]
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
                                    {:label (first (row-lines row columns width now))
                                     :search-text (str (:role row) " " (:label row))
                                     ;; A task that did not start has no session to open yet.
                                     :agent-id (when-not (:pending? row) (:session-id row))}))
                             team))))
           teams))))

(defn component
  "Production list component. Snapshot is read off-thread; the modal remains dismissible."
  [snapshot parent-id]
  (let [base (dlg/select-modal-component "Agents" [] {:filter? true :height :content})]
    (assoc base
      :measure
      (fn [state cols rows]
        (let [{:keys [agents loading? enabled?]} (snapshot)
              title (cond (false? enabled?) "Agents · Disabled"
                          loading? "Agents · Loading…"
                          :else "Agents")
              entries (cond (false? enabled?) [{:label
                                                "Enable Subagents in Settings → Experimental."}]
                            loading? [{:label "Loading team…"}]
                            (empty? agents)
                            (into [{:label "No subagents yet. Delegate a task to create one."}]
                                  (items [] parent-id))
                            :else (let [all (items agents parent-id (modal-row-cells cols))]
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
  "Show the team modal of `sid` with its cached snapshot. Each paint takes the newest one."
  [screen sid parent-id]
  (dlg/run-modal! screen (component #(or (summary sid) {:loading? true}) parent-id)))

(defn show!
  "The team modal of `sid`. Returns the session id to open, or nil."
  [screen sid parent-id]
  (loop []

    (let [choice (choose-team! screen sid parent-id)]
      (cond (= :refresh (:action choice)) (recur)
            (= :parent (:action choice)) (:session-id choice)
            :else (:agent-id choice)))))
