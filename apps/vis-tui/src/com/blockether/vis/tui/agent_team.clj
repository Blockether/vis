(ns com.blockether.vis.tui.agent-team
  "Managed team inspection through the same gateway as the conversation.

   The cached team snapshot also feeds three surfaces: the team card under the turn that
   started a team, the Questions inbox above the prompt and the breadcrumb of a child
   session. None of them fetches during painting."
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

(defn- agent-state
  "Short state words for one agent in the footer chip."
  [agent]
  (let [open (long (or (get agent "pending_input_count") 1))]
    (if (waiting? agent)
      (if (and (get agent "pending_input") (> open 1)) (str "asks you (" open ")") "asks you")
      (case (get agent "status")
        "running"
        (str (get agent "iterations_used" 0) "/" (get agent "iteration_budget" "?"))

        "completed"
        "done"

        "failed"
        "failed"

        "queued"
        "queued"

        "cancelled"
        "stopped"

        "budget reached"))))

(defn- active? [agent] (or (waiting? agent) (contains? #{"queued" "running"} (get agent "status"))))

(defn footer-label
  "Team state for the footer chip from the cached snapshot. Nil while no agent works or waits
   for a person, so a finished team leaves the footer. Only teams with such an agent show.
   Never fetches during painting."
  [sid]
  (let [agents
        (:agents (summary sid))

        live-teams
        (set (map #(get % "team_id") (filter active? agents)))]

    (when (seq (filter active? agents))
      (str "Agents: "
           (str/join " · "
                     (map (fn [[n agent]]
                            (str "A" n " " (agent-state agent)))
                          (filter #(contains? live-teams (get (second %) "team_id"))
                                  (numbered agents))))))))

;; ── Team card ───────────────────────────────────────────────────────────────

(def ^:private ^:const bar-width "The cells of the iteration bar of a card row." 10)

(defn- bar-done
  "How many of the `bar-width` cells the used iterations fill."
  [used budget]
  (let [used
        (long (or used 0))

        budget
        (long (or budget 0))]

    (if (pos? budget)
      (min (long bar-width) (long (Math/round (/ (* 1.0 used (long bar-width)) budget))))
      0)))

(defn- card-state
  [agent]
  (cond (waiting? agent) "asks you"
        :else (case (get agent "status")
                "running"
                "running"

                "queued"
                "queued"

                "completed"
                "done"

                "failed"
                "failed"

                "cancelled"
                "stopped"

                "budget reached")))

(defn- card-progress
  "The iteration bar and count of a running row, such as `────────── 4/10`. The bar is one box
   glyph that the terminal draws itself, and the used part has its own colour, so it stays on the
   text line in every font."
  [{:keys [status used budget]}]
  (when (contains? #{"running" "budget_limited"} status)
    (str (apply str (repeat bar-width \─)) " " (or used 0) "/" (or budget "?"))))

(defn- widest [texts] (reduce max 0 (map #(long (p/display-width (str %))) texts)))

(defn- card-columns
  "The cell widths of the card columns, so A-numbers, roles, states, bars and tasks of every row
   start in the same column. A column that no row uses has width 0."
  [rows]
  {:n (widest (map #(str "A" (:n %)) rows))
   :role (widest (keep :role rows))
   :state (widest (map :state rows))
   :progress (widest (keep card-progress rows))})

(defn card
  "The team card data of team `team-id` from `agents`, or nil when the team has no agent.
   Plain data, so an unchanged team compares equal across polls."
  [team-id agents]
  (let [rows (filter #(= team-id (get (second %) "team_id")) (numbered agents))]
    (when (seq rows)
      (let [rows (mapv (fn [[n agent]]
                         {:n n
                          :role (role-name agent)
                          :state (card-state agent)
                          :waiting? (waiting? agent)
                          :status (get agent "status")
                          :used (get agent "iterations_used")
                          :budget (get agent "iteration_budget")
                          :task (str/replace (str (get agent "task")) #"\s+" " ")
                          :activity (get agent "activity")
                          :session-id (get agent "session_id")})
                       rows)
            columns (card-columns rows)]

        {:team-id team-id :rows (mapv #(assoc % :columns columns) rows)}))))

(defn card-title
  "The first row of a team card, such as `Team · 3 agents · 1 running · 1 asks you`."
  [{:keys [rows]}]
  (let [counted
        (fn [pred]
          (count (filter pred rows)))

        running
        (counted #(and (not (:waiting? %)) (= "running" (:status %))))

        asking
        (counted :waiting?)

        done
        (counted #(= "completed" (:status %)))]

    (str "Team · "
         (count rows)
         (if (= 1 (count rows)) " agent" " agents")
         (when (pos? running) (str " · " running " running"))
         (when (pos? asking) (str " · " asking " asks you"))
         (when (pos? done) (str " · " done " done")))))

(defn- pad
  "`text` with spaces to `width` cells."
  [text width]
  (let [text (str text)]
    (str text (apply str (repeat (max 0 (- (long width) (long (p/display-width text)))) \space)))))

(defn- card-offsets
  "The cell offsets of the columns of one card row from its left edge."
  [{:keys [n role state progress]}]
  (let [role-col
        (+ (long n) 2)

        state-col
        (if (pos? (long role)) (+ role-col (long role) 2) role-col)

        progress-col
        (+ state-col (long state) 2)

        task-col
        (if (pos? (long progress)) (+ progress-col (long progress) 2) progress-col)]

    {:n 0 :role role-col :state state-col :progress progress-col :task task-col}))

(defn card-row-text
  "Plain text of one agent row of the card, as copy shows it, in the card columns."
  [{:keys [n role state task columns] :as row}]
  (let [{n-width :n role-width :role state-width :state progress-width :progress}
        (or columns (card-columns [row]))]
    (str (pad (str "A" n) n-width)
         "  "
         (when (pos? (long role-width)) (str (pad role role-width) "  "))
         (pad state state-width)
         "  "
         (when (pos? (long progress-width)) (str (pad (card-progress row) progress-width) "  "))
         task)))

(defn card-detail-text
  "The dim second row of a waiting or running agent, or nil. It starts with a box glyph, so the
   terminal draws it in the cell grid."
  [{:keys [waiting? status activity]}]
  (cond waiting? "└ waiting for you · see Questions below"
        (and (= "running" status) (seq activity)) (str "└ " activity)))

(defn- detail-indent
  "The cells before the detail row of `row`: it starts under the state column."
  [row]
  (:state (card-offsets (or (:columns row) (card-columns [row])))))

(defn- paint-card-line!
  [^TextGraphics g left row width viewport-top {:keys [team-line]}]
  (let [width
        (long width)

        bg
        t/code-block-bg

        put!
        (fn [col fg text bold?]
          (let [room (- (+ (long left) width) (long col))]
            (when (pos? room)
              (let [text (p/ellipsize (str text) room)]
                (p/set-colors! g fg bg)
                (if bold?
                  (p/styled g [p/BOLD] (p/put-str! g col row text))
                  (p/put-str! g col row text))
                (+ (long col) (long (p/display-width text)))))))]

    (case (:type team-line)
      :title
      (put! left t/code-block-fg (:text team-line) true)

      :detail
      (put! (+ (long left) (long (:indent team-line 2))) t/dialog-hint (:text team-line) false)

      :agent
      (let [{:keys [n role state waiting? status used budget task session-id] :as agent-row}
            (:row team-line)

            state-fg
            (cond waiting? t/warning-fg
                  (= "running" status) t/warning-fg
                  (= "failed" status) t/status-bad
                  (= "completed" status) t/status-ok
                  :else t/dialog-hint)

            ;; Each part starts at its column, so every row of the card lines up.
            offsets
            (card-offsets (or (:columns agent-row) (card-columns [agent-row])))

            at
            (fn [k]
              (+ (long left) (long (get offsets k))))

            progress
            (card-progress agent-row)]

        (put! (at :n) t/code-block-fg (str "A" n) true)
        (when role (put! (at :role) t/dialog-hint role false))
        (put! (at :state) state-fg state (boolean waiting?))
        (when progress
          (let [done (bar-done used budget)]
            (when (pos? (long done)) (put! (at :progress) t/status-ok (subs progress 0 done) false))
            (put! (+ (long (at :progress)) (long done))
                  t/dialog-hint
                  (subs progress done bar-width)
                  false)
            (put! (+ (long (at :progress)) (long bar-width))
                  t/code-block-fg
                  (subs progress bar-width)
                  false)))
        (put! (at :task) t/code-block-fg task false)
        (when session-id
          (.register interactions/hit-map
                     {:bounds {:row (+ (long viewport-top) (long row)) :col left :width width}
                      :kind :switch-session
                      :text session-id})))

      nil)))

(defn card-entries
  "Transcript rows of a team card on the Activity band: `{:line :meta}` maps."
  [team-card]
  (let [marker
        p/MARKER_ACTIVITY

        entry
        (fn [text team-line]
          {:line (str marker text)
           :meta {:kind :activity-live-entry :live-paint paint-card-line! :team-line team-line}})]

    (into [{:line marker :meta nil}
           (entry (card-title team-card) {:type :title :text (card-title team-card)})]
          (concat (mapcat (fn [row]
                            (cons (entry (card-row-text row) {:type :agent :row row})
                                  (when-let [detail (card-detail-text row)]
                                    (let [indent (detail-indent row)]
                                      [(entry (str (apply str (repeat indent \space)) detail)
                                              {:type :detail :text detail :indent indent})]))))
                          (:rows team-card))
                  [{:line marker :meta nil}]))))

(defn with-card-lines
  "Append the team card rows of `projected` (an assistant message with `:agent-team-card`) to its
   painted lines. A windowed slice keeps its lines."
  [projected]
  (if-let [team-card (and (not (:lines-window projected))
                          (:prewrapped-lines projected)
                          (:agent-team-card projected))]
    (let [lines (vec (:prewrapped-lines projected))
          line-meta (or (:line-meta projected) (vec (repeat (count lines) nil)))
          entries (card-entries team-card)]

      (assoc projected
        :prewrapped-lines (into (conj lines "") (map :line) entries)
        :line-meta (into (conj (vec line-meta) nil) (map :meta) entries)))
    projected))

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
   from the cached snapshot of `sid`. Returns `messages` unchanged when no team is known.
   An unchanged team keeps the decorated message identity, so height caches still hit."
  [messages sid]
  (let [agents (:agents (summary sid))]
    (if (or (empty? agents) (empty? messages))
      messages
      (let [teams (set (keep #(get % "team_id") agents))
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
                     last-index))))))

;; ── Questions inbox ─────────────────────────────────────────────────────────

(defonce ^:private inbox (atom {}))

(defn- request-kind
  [agent]
  (cond (get agent "question") :question
        (get agent "budget_request") :budget
        (get agent "files_request") :files
        (get agent "pending_input") :input))

(defn requests
  "Open requests of the agents of `sid` to a person, oldest first by Council entry. A request
   that the person moved to Later stands at the end."
  [sid]
  (let [later
        (get-in @inbox [sid :later] [])

        items
        (->> (numbered (:agents (summary sid)))
             (keep (fn [[n agent]]
                     (when-let [kind (request-kind agent)]
                       (let [entry-id (get agent "question_entry_id")]
                         {:key [(get agent "session_id") kind entry-id]
                          :n n
                          :kind kind
                          :entry-id entry-id
                          :agent agent}))))
             (sort-by (fn [{:keys [entry-id n]}]
                        [(if entry-id 0 1) (or entry-id 0) n])))

        rank
        (zipmap later (range))]

    (vec (sort-by #(get rank (:key %) -1) items))))

(defn- inbox-state [sid] (get @inbox sid))

(defn- current-request
  [sid items]
  (when (seq items) (nth items (mod (long (or (:index (inbox-state sid)) 0)) (count items)))))

(defn- request-text
  [{:keys [kind agent]}]
  (case kind
    :question
    (str (get agent "question"))

    :budget
    (let [{:strs [iterations reason]} (get agent "budget_request")]
      (str "Asks for " iterations " more iterations" (when (seq reason) (str ": " reason))))

    :files
    (let [{:strs [files reason]}
          (get agent "files_request")

          paths
          (mapcat #(get files %) ["write" "read_only" "deny"])]

      (str "Asks for file access: " (str/join ", " paths) (when (seq reason) (str ". " reason))))

    :input
    (let [open (long (or (get agent "pending_input_count") 1))]
      (str "Waits for your input in its own session" (when (> open 1) (str " (" open " forms)"))))))

(defn request-actions
  "The numbered actions of a request: `[[key label action] ...]`."
  [{:keys [kind agent]}]
  (case kind
    :question
    [["1" "Answer" :answer]]

    :budget
    [["1" (str "Grant +" (get-in agent ["budget_request" "iterations"])) :extend]
     ["2" "Deny" :deny]]

    :files
    [["1" "Grant" :grant] ["2" "Deny" :deny]]

    :input
    [["1" "Open agent session" :open]]))

(defn- hint-text
  [sid item]
  (let [{:keys [focused? answering]} (inbox-state sid)]
    (cond (= answering (:key item)) "Type the answer below · Enter send · Esc cancel"
          focused? (str/join " · "
                             (concat (map (fn [[k label]]
                                            (str k " " label))
                                          (request-actions item))
                                     (when (#{:budget :files} (:kind item)) ["C-d deny"])
                                     ["Tab next" "Esc later"]))
          :else "C-x Q answer")))

(defn- wrap-words
  "Up to `max-lines` lines of `text` at `width` columns; the last one ends with … when cut."
  [text width max-lines]
  (let [width
        (max 1 (long width))

        words
        (str/split (str/trim (str/replace (str text) #"\s+" " ")) #" ")

        lines
        (reduce (fn [lines word]
                  (let [line
                        (peek lines)

                        joined
                        (if (str/blank? line) word (str line " " word))]

                    (if (or (str/blank? line) (<= (long (p/display-width joined)) width))
                      (conj (pop lines) joined)
                      (conj lines word))))
                [""]
                words)]

    (if (> (count lines) (long max-lines))
      (let [kept (vec (take max-lines lines))]
        (conj (pop kept) (p/ellipsize (str (peek kept) " " (nth lines max-lines)) width)))
      (mapv #(p/ellipsize % width) lines))))

(defn- body-width [cols] (max 1 (- (long cols) 4)))

(defn inbox-height
  "Rows that the Questions inbox of `sid` takes above the prompt, 0 without open requests."
  ^long [sid cols]
  (let [items (requests sid)]
    (if-let [item (current-request sid items)]
      (+ 3 1 (count (wrap-words (request-text item) (body-width cols) 2)))
      0)))

(defn inbox-view
  "Plain rows of the Questions inbox of `sid`: `{:title :head :text [lines] :hint}`, or nil."
  [sid cols]
  (let [items (requests sid)]
    (when-let [item (current-request sid items)]
      (let [index (inc (long (.indexOf ^java.util.List items item)))
            agent (:agent item)
            unseen? (not (contains? (:seen (inbox-state sid) #{}) (:key item)))]

        {:title (str " Questions · " index " of " (count items) " ")
         :head (str "A"
                    (:n item)
                    (when-let [role (get agent "role")]
                      (str " " role))
                    (when unseen? " · NEW")
                    " · "
                    (str/replace (str (get agent "task")) #"\s+" " "))
         :text (wrap-words (request-text item) (body-width cols) 2)
         :hint (hint-text sid item)
         :focused? (boolean (:focused? (inbox-state sid)))}))))

(defn draw-inbox!
  "Paint the Questions inbox of `sid` at `top` across `cols` columns."
  [^TextGraphics g sid top cols]
  (when-let [{:keys [title head text hint focused?]} (inbox-view sid cols)]
    (let [width (max 1 (long cols))
          height (+ 3 1 (count text))
          inner (body-width cols)]

      (p/set-colors! g t/dialog-border t/dialog-bg)
      (p/fill-rect! g 0 top width height)
      (when (>= width 4)
        (p/draw-box! g 0 top width height)
        (p/set-colors! g (if focused? t/header-active-tab-accent t/dialog-border) t/dialog-bg)
        (p/styled g [p/BOLD] (p/put-str! g 2 top (p/truncate-cols title (max 0 (- width 4))))))
      (p/set-colors! g t/dialog-fg t/dialog-bg)
      (p/styled g [p/BOLD] (p/put-str! g 2 (inc (long top)) (p/ellipsize head inner)))
      (doseq [[i line] (map-indexed vector text)]
        (p/put-str! g 2 (+ (long top) 2 (long i)) line))
      (p/set-colors! g (if focused? t/header-active-tab-accent t/dialog-hint) t/dialog-bg)
      (p/put-str! g 2 (+ (long top) 2 (count text)) (p/ellipsize hint inner)))))

(defn focus-inbox!
  "Give the Questions inbox of `sid` the keyboard. Returns false when nothing is open."
  [sid]
  (let [items (requests sid)]
    (if (seq items)
      (do (swap! inbox update
            sid
            (fn [s]
              (-> s
                  (assoc :focused? true
                         :answering nil)
                  (update :seen (fnil conj #{}) (:key (current-request sid items))))))
          true)
      false)))

(defn- character
  [^KeyStroke key]
  (when (and (instance? KeyStroke key)
             (= KeyType/Character (.getKeyType key))
             (not (.isAltDown key)))
    (.getCharacter key)))

(defn- key-type [key] (when (instance? KeyStroke key) (.getKeyType ^KeyStroke key)))

(defn inbox-owns-key?
  "Whether the Questions inbox of `sid` takes `key`. A focused inbox takes its action keys,
   Tab, C-d and Esc. While the person types an answer, it takes only Enter and Esc."
  [sid key]
  (let [{:keys [focused? answering]} (inbox-state sid)]
    (boolean (and (or focused? answering)
                  (seq (requests sid))
                  (if answering
                    (contains? #{KeyType/Enter KeyType/Escape} (key-type key))
                    (or (contains? #{KeyType/Tab KeyType/Escape} (key-type key))
                        (let [ch (character key)]
                          (and ch
                               (or (and (.isCtrlDown ^KeyStroke key) (= \d ch))
                                   (and (not (.isCtrlDown ^KeyStroke key))
                                        (contains? #{\1 \2 \3} ch)))))))))))

(defn- control!
  "POST one team control for `item` off the input thread, then refresh the team."
  [sid suffix body failure changed! after-success]
  (vis/worker-future "tui-agent-inbox"
                     (fn []
                       (try (if (= 200
                                   (:status (vis/request! :post (path sid suffix) {:body body})))
                              (after-success)
                              (vis/notify! failure :level :error))
                            (catch Exception _ (vis/notify! failure :level :error)))
                       (fetch! sid)
                       (changed!))))

(defn- run-action!
  [sid item action {:keys [changed! open-session!]}]
  (let [id
        (get-in item [:agent "session_id"])

        after
        (fn []
          (swap! inbox update sid assoc :focused? false :answering nil))]

    (case action
      :answer
      (swap! inbox update sid assoc :focused? false :answering (:key item))

      :extend
      (control! sid
                "/extend"
                {:session_id id :iterations (get-in item [:agent "budget_request" "iterations"])}
                "Could not grant iterations; refresh the team"
                changed!
                after)

      :grant
      (control! sid
                "/grant"
                {:session_id id :files (get-in item [:agent "files_request" "files"])}
                "Could not grant file access; refresh the team"
                changed!
                after)

      :deny
      (control! sid
                "/deny"
                {:session_id id :reason "A person denied the request."}
                "Could not deny the request; refresh the team"
                changed!
                after)

      :open
      (do (after) (open-session! id))

      nil)))

(defn inbox-key!
  "Act on `key` for the Questions inbox of `sid`. Call only when `inbox-owns-key?` is true.
   `opts`: `:changed!` repaints, `:open-session!` opens a session id, `:input-text` returns the
   prompt text and `:reset-input!` clears it."
  [sid key {:keys [changed! input-text reset-input!] :as opts}]
  (let [items
        (requests sid)

        item
        (current-request sid items)

        {:keys [answering]}
        (inbox-state sid)

        kind
        (key-type key)

        ch
        (character key)

        action-for
        (fn [n]
          (some (fn [[k _ action]]
                  (when (= k (str n)) action))
                (request-actions item)))]

    (cond answering (if (= KeyType/Escape kind)
                      (swap! inbox update sid assoc :answering nil)
                      (let [target
                            (some #(when (= answering (:key %)) %) items)

                            text
                            (str/trim (str (input-text)))]

                        (cond (nil? target) (swap! inbox update sid assoc :answering nil)
                              (str/blank? text) (vis/notify! "Type the answer first" :level :warn)
                              :else (do (reset-input!)
                                        (control!
                                          sid
                                          "/answer"
                                          {:session_id (get-in target [:agent "session_id"])
                                           :text text}
                                          "Could not send the answer; refresh the team"
                                          changed!
                                          #(swap! inbox update sid assoc :answering nil))))))
          (= KeyType/Tab kind) (let [next-index
                                     (inc (long (or (:index (inbox-state sid)) 0)))

                                     next-item
                                     (nth items (mod next-index (count items)))]

                                 (swap! inbox update
                                   sid
                                   #(-> %
                                        (assoc :index next-index)
                                        (update :seen (fnil conj #{}) (:key next-item)))))
          (= KeyType/Escape kind)
          (swap! inbox update
            sid
            (fn [s]
              (-> s
                  (update :later #(conj (vec (remove #{(:key item)} %)) (:key item)))
                  (assoc :focused? false))))
          (and ch (.isCtrlDown ^KeyStroke key)) (when (#{:budget :files} (:kind item))
                                                  (run-action! sid item :deny opts))
          ch (when-let [action (action-for ch)]
               (run-action! sid item action opts)))
    (changed!)
    true))

;; ── Breadcrumb ──────────────────────────────────────────────────────────────

(defn breadcrumb
  "Header path of child session `sid` from its cached team, such as `Leader → A2 implement · task`:
   `{:text :parent-id}`, or nil when the TUI does not know the parent."
  [sid]
  (let [pairs
        (numbered (:agents (summary sid)))

        number-of
        (into {}
              (map (fn [[n agent]]
                     [(get agent "session_id") n]))
              pairs)]

    (when-let [[n agent] (some #(when (= (str sid) (get (second %) "session_id")) %) pairs)]
      (when-let [parent (get agent "parent_id")]
        {:parent-id parent
         :text (str (if-let [k (and (not= parent (get agent "leader_id")) (number-of parent))]
                      (str "A" k)
                      "Leader")
                    " → A"
                    n
                    (some->> (role-name agent)
                             (str " "))
                    " · "
                    (str/replace (str (get agent "task")) #"\s+" " "))}))))

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

(defn- row-label
  [row]
  (str (some-> (role-name row)
               (str " · "))
       (get row "task")
       " · "
       (if (get row "pending_input") "Needs input" (str/replace (get row "status") "_" " "))
       " · "
       (or (get row "model") "Router default")
       (when (get row "routing_locked") " (locked)")
       " · " (get row "iterations_used")
       "/" (get row "iteration_budget")
       " iterations" (when-let [cost (get-in row ["usage" "cost_usd"])]
                       (when (number? cost) (str " · $" cost)))))

(defn items
  [rows parent-id]
  (into (cond-> [{:label "Refresh team" :action :refresh}]
          parent-id
          (conj {:label "Open parent" :action :inspect :session-id parent-id}))
        (map (fn [row]
               {:label (str/replace (row-label row) #"\s+" " ")
                :search-text (get row "task")
                :agent row})
             (sort-by #(get % "parent_id" "") (map second (numbered rows))))))

(defn component
  "Production list component. Snapshot is read off-thread; the modal remains dismissible."
  [snapshot parent-id]
  (let [base (dlg/select-modal-component "Agent team" [] {:filter? true :height :content})]
    (assoc base
      :measure
      (fn [state cols rows]
        (let [{:keys [agents error loading? enabled?]} (snapshot)
              title (cond (false? enabled?) "Agent team · Disabled"
                          loading? "Agent team · Loading…"
                          error "Agent team · Unavailable"
                          :else (str "Agent team · "
                                     (count agents)
                                     (if (= 1 (count agents)) " subagent" " subagents")))
              entries (cond (false? enabled?) [{:label
                                                "Enable Subagents in Settings → Experimental."}]
                            loading? [{:label "Loading team…"}]
                            error (into [{:label error}] (items [] parent-id))
                            (empty? agents)
                            (into [{:label "No subagents yet. Delegate a task to create one."}]
                                  (items [] parent-id))
                            :else (let [all (items agents parent-id)]
                                    (vec (concat (filter :agent all) (remove :agent all)))))]

          ((:measure (dlg/select-modal-component title entries {:filter? true :height :content}))
            state
            cols
            rows)))
      :on-key (fn [state key geom]
                (let [result ((:on-key base) state key geom)
                      choice (::dlg/done result)]

                  (if (and choice (not (:action choice)) (not (:agent choice))) state result))))))

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

(defn- stop!
  [sid id]
  (try (when-not (= 200
                    (:status (vis/request! :post (path sid "/cancel") {:body {:session_id id}})))
         (vis/notify! "Could not stop agent; refresh its state" :level :error))
       (catch Exception _ (vis/notify! "Could not stop agent; refresh its state" :level :error))))

(defn show!
  [screen sid parent-id]
  (loop []

    (let [choice (choose-team! screen sid parent-id)]
      (cond (= :refresh (:action choice)) (recur)
            (= :inspect (:action choice)) (:session-id choice)
            (:agent choice)
            (let [row (:agent choice)
                  id (get row "session_id")
                  action
                  (dlg/select-dialog!
                    screen
                    (get row "task")
                    (cond-> [{:label "Inspect agent" :action :inspect}
                             {:label (str "Status: " (str/replace (get row "status") "_" " "))}
                             {:label (str
                                       "Input: "
                                       (if (get row "pending_input") "Needs input" "None pending"))}
                             {:label (str "Model: "
                                          (or (get row "model") "Router default")
                                          (when (get row "routing_locked") " (locked)"))}
                             {:label (str "Iterations: " (get row "iterations_used")
                                          "/" (get row "iteration_budget"))}
                             {:label (str "Cost: "
                                          (if-let [cost (get-in row ["usage" "cost_usd"])]
                                            (str "$" cost)
                                            "Not reported"))}]
                      (get row "parent_id")
                      (conj {:label "Open parent" :action :parent})

                      (contains? #{"queued" "running"} (get row "status"))
                      (conj {:label "Stop agent and descendants" :action :stop})))]

              (case (:action action)
                :inspect
                id

                :parent
                (get row "parent_id")

                :stop
                (do (when (= :stop
                             (:action (dlg/select-dialog!
                                        screen
                                        (str "Stop '" (get row "task") "' and descendants?")
                                        [{:label "Keep working" :action :keep}
                                         {:label "Stop agent and descendants" :action :stop}])))
                      (stop! sid id))
                    (recur))

                (recur)))
            :else nil))))
