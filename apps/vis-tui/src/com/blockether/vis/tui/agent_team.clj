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
                     " "
                     (count (filter #(contains? #{"queued" "running"} (get % "status")) agents))
                     " active/"
                     (count agents)
                     " cached"))))

;; ── Team rows ───────────────────────────────────────────────────────────────

(def ^:private ^:const title-width
  "The most cells that the title column takes. A longer title ends with an ellipsis."
  40)

(def ^:private ^:const time-width "Cells of the time column, enough for `59m 59s`." 7)

(def ^:private ^:const narrow-width
  "Below this width a row with a now or result text takes two lines."
  64)

(def ^:private ^:const dock-max-rows
  "The most agent rows in the dock table. A larger team shows one row fewer and counts the rest."
  6)

(defn- one-line [text] (str/trim (str/replace (str text) #"\s+" " ")))

(defn agent-label
  "The title of `agent`, or `#n` when it has no title. `n` is its start order in its team."
  [n agent]
  (or (not-empty (one-line (get agent "title"))) (str "#" n)))

(defn status-word
  "The status of `agent` in one of four words, in capitals: `QUEUED`, `RUNNING`, `DONE` or
   `FAILED`. An agent that waits for the reply of its parent still runs. A failed verdict, a
   cancel and a spent budget are all `FAILED`: the row text tells which."
  [agent]
  (case (get agent "status")
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
  "The end that `FAILED` hides, for an agent without a result: `cancelled` or `out of budget`."
  [agent]
  (case (get agent "status")
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

    "FAILED"
    :bad

    "RUNNING"
    :running

    :muted))

(defn- now-text
  "The newest progress note or Activity line of a working agent, else its result, else the
   reason of its end. The questions and grant requests of an agent go to its leader, never to
   the person, so no row shows them."
  [agent]
  (one-line (or (when (active? agent) (or (get agent "note") (get agent "activity")))
                (get agent "result")
                (end-reason agent))))

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
     :role (or (role-word agent) "")
     :label (agent-label n agent)
     :status word
     :tone (tone word)
     :now (now-text agent)
     :active? (active? agent)
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
        (and active? (not= "QUEUED" status)) (duration-text (- (long now-ms) (long start)))))

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
      :ok
      t/status-ok

      :bad
      t/status-bad

      :running
      t/code-block-fg

      t/dialog-hint)))

(defn- register-row!
  "A click on a row opens the timeline of its agent."
  [row screen-row left width]
  (when-let [id (:session-id row)]
    (.register interactions/hit-map
               {:bounds {:row screen-row :col left :width width} :kind :agent-timeline :text id})))

;; ── Table ───────────────────────────────────────────────────────────────────

(def ^:private table-heads
  "The column titles of the agent table."
  {:role "Role" :label "Agent" :status "Status" :now "Detail" :time "Time"})

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
   order, and then the title takes the room."
  [rows width]
  (let [width
        (long width)

        fixed
        (fn [k texts]
          {:key k :w (widest (cons (table-heads k) texts))})

        role
        (fixed :role (map :role rows))

        label
        (update (fixed :label (map :label rows)) :w min (long title-width))

        status
        (fixed :status (map :status rows))

        time
        {:key :time :w (max (long time-width) (count (:time table-heads)))}

        with-detail
        (fn [columns]
          (let [room (- width (table-width (conj columns {:w 0})))]
            (when (>= room (long detail-min-width))
              ;; The detail goes before the status, so the status stands right before the time.
              (into (conj (vec (drop-last 2 columns)) {:key :now :w room})
                    (take-last 2 columns)))))]

    (or (with-detail [role label status time])
        (with-detail [label status time])
        ;; A phone keeps the title readable: the time column goes before the title gets short.
        (let [fit (fn [columns]
                    (let [room (- width (table-width columns) 3)]
                      (when (>= room (min 10 (long (:w label))))
                        (into [(assoc label :w room)] columns))))]
          (or (fit [status time])
              (fit [status])
              [(assoc label :w (max 4 (- width (table-width [status]) 3))) status])))))

(defn table-lines
  "The lines of the agent table for `rows`, from the top border to the bottom border:
   `{:kind}` maps. With `max-rows`, a longer team shows `max-rows - 1` rows and a last row
   that counts the rest."
  ([rows] (table-lines rows nil))
  ([rows max-rows]
   (let [shown
         (if (and max-rows (> (count rows) (long max-rows)))
           (subvec (vec rows) 0 (dec (long max-rows)))
           rows)

         hidden
         (- (count rows) (count shown))]

     (-> [{:kind :top} {:kind :head} {:kind :mid}]
         (into (map (fn [row]
                      {:kind :row :row row}))
               shown)
         (cond->
           (pos? hidden)
           (conj {:kind :more :n hidden}))
         (conj {:kind :bottom})))))

(defn- cell-text
  [{:keys [kind row n]} column-key now-ms]
  (case kind
    :head
    (table-heads column-key)

    :more
    (if (= :label column-key) (str "+" n " more") "")

    (case column-key
      :time
      (or (row-time row now-ms) "")

      (str (get row column-key)))))

(defn table-segments
  "The painted parts of one table line: `[col text part]` vectors. `part` is `:chrome` for
   the grid, `:head` for a title, `:more` for the hidden count, else the column key."
  [{:keys [kind] :as line} columns now-ms]
  (let [widths (map #(+ 2 (long (:w %))) columns)]
    (case kind
      :top
      [[0 (p/boxed-horiz-line widths \┌ \┬ \┐) :chrome]]

      :mid
      [[0 (p/boxed-horiz-line widths \├ \┼ \┤) :chrome]]

      :bottom
      [[0 (p/boxed-horiz-line widths \└ \┴ \┘) :chrome]]

      (first
        (reduce (fn [[out col] {column-key :key w :w}]
                  (let [w (long w)
                        text (p/ellipsize (cell-text line column-key now-ms) w)
                        text-w (long (p/display-width text))
                        at (if (and (= :time column-key) (not= :head kind))
                             (+ (long col) 2 (- w text-w))
                             (+ (long col) 2))
                        part (case kind
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
  "Paint one agent table line at screen row `y` from column `left` across `width` cells."
  [^TextGraphics g left y width line columns now-ms]
  (p/clear-styles! g)
  (p/set-colors! g t/code-block-fg t/code-block-bg)
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
        (do (p/set-colors! g t/code-border-fg t/code-block-bg) (p/put-str! g x y text))

        :head
        (do (p/set-colors! g t/code-block-fg t/code-block-bg)
            (p/styled g [p/BOLD] (p/put-str! g x y text)))

        :more
        (do (p/set-colors! g t/dialog-hint t/code-block-bg) (p/put-str! g x y text))

        (do (p/set-colors! g (part-fg part (:tone (:row line))) t/code-block-bg)
            (if (#{:label :status} part)
              (p/styled g [p/BOLD] (p/put-str! g x y text))
              (p/put-str! g x y text))))))
  (p/clear-styles! g))

(defn- paint-team-line!
  "Paint one transcript line of a team table."
  [^TextGraphics g left row width viewport-top {:keys [team-line]}]
  (let [{:keys [line columns]} team-line]
    (paint-table-line! g left row width line columns (System/currentTimeMillis))
    (when (= :row (:kind line))
      (register-row! (:row line) (+ (long viewport-top) (long row)) left width))))

(defn card
  "The team block data of team `team-id` from `agents`, or nil when the team has no agent."
  [team-id agents]
  (let [rows (team-rows team-id agents)]
    (when (seq rows) {:team-id team-id :rows rows :columns (row-columns rows)})))

(defn card-entries
  "Transcript rows of a team table at `width` cells: `{:line :meta}` maps. A blank row stands
   before and after the table."
  [{:keys [rows]} width]
  (let [marker
        p/MARKER_ACTIVITY

        columns
        (table-columns rows width)

        now
        (System/currentTimeMillis)]

    (into [{:line marker :meta nil}]
          (concat (map (fn [line]
                         {:line (str marker (segments-text (table-segments line columns now)))
                          :meta {:kind :activity-live-entry
                                 :live-paint paint-team-line!
                                 :team-line {:line line :columns columns}}})
                       (table-lines rows))
                  [{:line marker :meta nil}]))))

(defn- card-width ^long [bubble-w] (max 1 (- (long bubble-w) 4)))

(defn card-height
  "Rows that `with-card-lines` appends for `team-card` at bubble width `bubble-w`: 0 without a
   card. Layout adds it to the cached height of the message without its card."
  ^long [team-card _bubble-w]
  (if-let [rows (seq (:rows team-card))]
    (+ 3 (count (table-lines rows)))
    0))

(defn with-card-lines
  "Append the team block of `projected` (an assistant message with `:agent-team-card`) to its
   painted lines at bubble width `bubble-w`. A windowed slice keeps its lines."
  [projected bubble-w]
  (if-let [team-card (and (not (:lines-window projected))
                          (:prewrapped-lines projected)
                          (:agent-team-card projected))]
    (let [lines (vec (:prewrapped-lines projected))
          line-meta (or (:line-meta projected) (vec (repeat (count lines) nil)))
          entries (card-entries team-card (card-width bubble-w))]

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

;; ── Dock ────────────────────────────────────────────────────────────────────

(defn- dock-width
  "The dock spans the transcript bubble: `cols` less `render/MESSAGE_SIDE_PAD` (5), which
   this namespace cannot require."
  [cols]
  (max 1 (- (long cols) 5)))

(def ^:private summary-words
  "The status words of the dock summary line, in their order."
  ["RUNNING" "QUEUED" "DONE" "FAILED"])

(defn summary-counts
  "The status counts of `rows`, such as `2 RUNNING · 1 QUEUED · 1 DONE`."
  [rows]
  (let [counts (frequencies (map :status rows))]
    (str/join " · "
              (for [word summary-words
                    :let [n (get counts word 0)]
                    :when (pos? (long n))]

                (str n " " word)))))

(defn- summary-hint [expanded?] (if expanded? "C-x T hide details" "C-x T details"))

(defn summary-segments
  "The painted parts of the dock summary line at `width` cells: `[col text part]` vectors.
   `part` is `:head`, `:counts` or `:hint`. Long counts shorten first, so the hint stays. A
   line too narrow for both drops the hint, then shortens the counts."
  [{:keys [counts expanded?]} width]
  (let [width
        (long width)

        head
        "Agents"

        counts-col
        (+ (count head) 3)

        hint
        (summary-hint expanded?)

        hint-col
        (- width (long (p/display-width hint)))

        hint?
        (>= hint-col (+ counts-col (min 12 (long (p/display-width counts))) 3))

        room
        (- (if hint? (- hint-col 3) width) counts-col)]

    (cond-> [[0 head :head]]
      (and (seq counts) (pos? room))
      (conj [counts-col (p/ellipsize counts room) :counts])

      hint?
      (conj [hint-col hint :hint]))))

(defn dock-lines
  "The dock of `sid` above the prompt at `cols` columns: `{:line :columns}` maps, one for each
   screen row. A docked team shows one summary line. After `toggle-dock!`, the agent table
   follows it. Empty without a docked team."
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

            expanded?
            (dock-expanded? sid)

            head
            {:line {:kind :summary :counts (summary-counts rows) :expanded? expanded?}}]

        (if expanded?
          (let [columns (table-columns rows (dock-width cols))]
            (into [head]
                  (map (fn [line]
                         {:line line :columns columns}))
                  (table-lines rows dock-max-rows)))
          [head])))))

(defn dock-height
  "Rows that the dock of `sid` takes above the prompt, 0 without a docked team."
  ^long [sid running-turn cols]
  (count (dock-lines sid running-turn cols)))

(defn dock-text
  "Plain dock lines of `sid`, as the painter shows them."
  [sid running-turn cols now-ms]
  (mapv (fn [{:keys [line columns]}]
          (segments-text (if (= :summary (:kind line))
                           (summary-segments line (dock-width cols))
                           (table-segments line columns now-ms))))
        (dock-lines sid running-turn cols)))

(defn- paint-summary-line!
  "Paint the dock summary line at screen row `y` from column `left` across `width` cells."
  [^TextGraphics g left y width line]
  (doseq [[col text part] (summary-segments line width)]
    (let [x (+ (long left) (long col))]
      (p/set-colors! g (if (= :head part) t/code-block-fg t/dialog-hint) t/terminal-bg)
      (if (= :head part) (p/styled g [p/BOLD] (p/put-str! g x y text)) (p/put-str! g x y text))))
  (p/clear-styles! g))

(defn draw-dock!
  "Paint the dock table of `sid` at screen row `top` across `cols` columns."
  [^TextGraphics g sid running-turn top cols]
  (let [width
        (dock-width cols)

        now
        (System/currentTimeMillis)]

    (doseq [[i {:keys [line columns]}]
            (map-indexed vector (dock-lines sid running-turn cols))

            :let [y
                  (+ (long top) (long i))]]

      (p/set-colors! g t/code-block-fg t/terminal-bg)
      (p/fill-rect! g 0 y (max 1 (long cols)) 1)
      (if (= :summary (:kind line))
        (do (paint-summary-line! g 2 y width line)
            ;; A click on the summary line shows or hides the agent table, like C-x T.
            (.register interactions/hit-map
                       {:bounds {:row y :col 0 :width (max 1 (long cols))}
                        :kind :agent-dock-toggle
                        :text (str sid)}))
        (paint-table-line! g 2 y width line columns now))
      (when (= :row (:kind line)) (register-row! (:row line) y 0 (max 1 (long cols)))))))

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
  "Header path of child session `sid` from its cached team, such as `Leader → IMPLEMENT Fix
   parser`: `{:text :parent-id}`, or nil when the TUI does not know the parent."
  [sid]
  (let [pairs
        (numbered (:agents (summary sid)))

        name-of
        (fn [[n agent]]
          (str (some-> (role-word agent)
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

         end
         (when (= "finished" kind)
           {"status" (get item "status" "completed") "outcome" (get item "outcome")})

         word
         (if end (status-word end) kind)]

     (str (.format ^java.time.format.DateTimeFormatter clock
                   (.atZone (java.time.Instant/ofEpochMilli (long (get item "at"))) zone))
          "  "
          word
          (when-let [text (or (not-empty (one-line (get item "text")))
                              (some-> end
                                      end-reason))]
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
          (str (some-> (role-word agent)
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
                                     :agent-id (:session-id row)}))
                             team))))
           teams))))

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
