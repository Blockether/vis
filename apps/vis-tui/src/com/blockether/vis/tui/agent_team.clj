(ns com.blockether.vis.tui.agent-team
  "Managed team inspection through the same gateway as the conversation."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.dialogs :as dlg])
  (:import [java.net URLEncoder]
           [java.nio.charset StandardCharsets]))

(defn- path
  [sid suffix]
  (str "/v1/sessions/" (URLEncoder/encode (str sid) StandardCharsets/UTF_8) "/agents" suffix))

(defonce ^:private snapshots (atom {}))

(defn- roles
  "The subagent roles of the agents contract, in its order. Each `x-vis-mark` is a glyph that the
   Companion draws as its Lucide twin."
  []
  (get-in (document/schema-document "agents") ["$defs" "role" "oneOf"]))

(defn role-mark
  "The contract glyph of the role of `agent`, or nil for no role or an unknown role."
  [agent]
  (some #(when (= (get % "const") (get agent "role")) (get % "x-vis-mark")) (roles)))

(defn numbered
  "`[n agent]` pairs. `n` is the start order from 1. Rows of one role stand together in contract
   order, and a row without a role comes last."
  [agents]
  (let [rank (fn [[_ agent]]
               (or (first (keep-indexed #(when (= (get %2 "const") (get agent "role")) %1) (roles)))
                   (count (roles))))]
    (sort-by rank
             (map-indexed (fn [index agent]
                            [(inc (long index)) agent])
                          agents))))

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
    (if (get agent "pending_input")
      (if (> open 1) (str "needs you (" open ")") "needs you")
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

(defn footer-label
  "Team state for the footer chip from the cached snapshot. Nil while no agent works or waits
   for a person, so a finished team leaves the footer. Never fetches during painting."
  [sid]
  (let [agents (:agents (summary sid))]
    (when (some #(or (get % "pending_input") (contains? #{"queued" "running"} (get % "status")))
                agents)
      (str "Agents: "
           (str/join " · "
                     (map (fn [[n agent]]
                            (str (some-> (role-mark agent)
                                         (str " "))
                                 "A" n
                                 " " (agent-state agent)))
                          (numbered agents)))))))

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
  (str (some-> (role-mark row)
               (str " " (get row "role") " · "))
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
