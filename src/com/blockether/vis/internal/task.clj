(ns com.blockether.vis.internal.task
  "Session-owned work. The registry owns state; work and interrupt functions run outside its lock."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.session.cancellation :as cancellation]
            [taoensso.telemere :as tel])
  (:import [java.util UUID]))

(defonce ^:private sessions (atom {}))

;; Append jobs under the registry lock, but run them on a separate worker.
(defonce ^:private outbox (agent nil))
(defonce ^:private publisher (atom nil))

(defn set-publisher!
  "Set the gateway callback `(publish session event-type row)`, or nil to remove it."
  [publish]
  (locking sessions (reset! publisher publish))
  nil)

(defn- enqueue! [f]
  (send-off outbox
            (fn [_]
              (try (f)
                   (catch Throwable _
                     (tel/log! {:level :warn :id ::publication-failed})))
              nil)))

(def ^:dynamic *task*
  "The `[session id]` of the Task that owns this host call."
  nil)

(def ^:dynamic ^:private *ticket* nil)

(defn call-id [] (str (UUID/randomUUID)))

(defn- key-for [session id] [(str session) :entries id])

(defn- public-row [entry] (:row entry))

(defn state [session id]
  (public-row (get-in @sessions (key-for session id))))

(defn tasks
  "Running Tasks and completed Tasks whose result the guest has not read."
  [session]
  (->> (get-in @sessions [(str session) :entries])
       vals
       (remove :acknowledged?)
       (sort-by :number)
       (mapv public-row)))

(defn- wire-row [entry]
  (when entry
    (let [{:keys [id kind] :as row} (:row entry)]
      (cond-> (assoc row :name (str id " " (or (:public-name entry) (name kind))))
        (:cancel-reason row) (update :cancel-reason #(str/replace (name %) "-" "_"))
        (:error row) (assoc :error "Task failed")))))

(defn wire-state [session id]
  (wire-row (get-in @sessions (key-for session id))))

(defn wire-tasks [session]
  (into [] (comp (remove :acknowledged?) (keep wire-row))
        (sort-by :number (vals (get-in @sessions [(str session) :entries])))))

(defn at-publication-cursor
  "Capture Task rows after prior publications and before later ones. `cursor` reads the event seq."
  [session cursor]
  (let [snapshot (promise)]
    (locking sessions
      (let [rows (wire-tasks session)]
        (enqueue! #(try (deliver snapshot {:tasks rows :tasks-seq (cursor)})
                        (catch Throwable e (deliver snapshot e))))))
    (let [result @snapshot]
      (if (instance? Throwable result) (throw result) result))))

(defn- publish-row! [session event entry]
  (when-let [publish @publisher]
    (when-let [row (wire-row entry)]
      (enqueue! #(publish (str session) event row)))))

(defn- update-entry!
  "Commit one entry update under the registry lock and queue its public snapshot."
  [path event f]
  (let [entry (get-in (swap! sessions update-in path f) path)]
    (publish-row! (first path) event entry)
    entry))

(defn live? [session]
  (boolean (some #(= :running (:state %)) (tasks session))))

(defn detached?
  "Whether this Task has let its caller continue without it."
  []
  (boolean (and *task* (:detached (apply state *task*)))))

(defn track-view!
  "Register a view and its close callback if its Task still accepts views."
  [[session id] view-id close!]
  (let [path (key-for session id)]
    (locking sessions
      (let [{:keys [row closing?]} (get-in @sessions path)]
        (when (and (= :running (:state row)) (not closing?))
          (update-entry! path "task.updated"
                         #(-> %
                              (assoc-in [:views view-id] close!)
                              (update-in [:row :view-ids] (fn [ids] (if (some #{view-id} ids) ids (conj ids view-id))))))
          true)))))

(defn forget-view!
  "Release a view already closed by the tool or person."
  [[session id] view-id]
  (let [path (key-for session id)]
    (locking sessions
      (when (get-in @sessions (conj path :views view-id))
        (update-entry! path "task.updated"
                       #(-> %
                            (update :views dissoc view-id)
                    (update-in [:row :view-ids] (fn [ids] (filterv (partial not= view-id) ids)))))
        nil))))

(defn- close-views! [views ending]
  (doseq [[view-id close!] views]
    (try (close! ending)
         (catch Throwable e
           (tel/log! {:level :warn :id ::view-close-failed :error e :data {:view-id view-id}})))))

(defn- settlement
  [entry produce]
  (when-not (:external-settlement? entry)
    (when-let [interrupted (:interrupt-complete entry)] @interrupted))
  (let [outcome (try (produce)
                     (catch Throwable e
                       {:state :failed :error (or (ex-message e) (str e))}))]
    (if (or (:interrupt-complete entry)
            (and (:cancel-on-request? entry) (get-in entry [:row :cancel-reason])))
      (do (doseq [rollback (:handoff-rollbacks entry)] (rollback))
          (assoc (select-keys outcome [:record :snapshot]) :state :cancelled))
      outcome)))

(defn- settle!
  [session id ticket produce]
  (let [path (key-for session id)
        entry (locking sessions
                (let [{:keys [row closing?] :as entry} (get-in @sessions path)]
                  (when (and (identical? ticket (:ticket entry))
                             (= :running (:state row)) (not closing?))
                    (swap! sessions update-in path #(-> % (assoc :closing? true)
                                                         (dissoc :views :release-activity :handoff-rollbacks)))
                    entry)))]
    (when entry
      (let [{:keys [state error] :as outcome} (settlement entry produce)]
        (close-views! (:views entry) (case state
                                      :cancelled {:reason :cancelled}
                                      :failed {:reason :failed :error error}
                                      {:reason :completed}))
        (when-let [release (:release-activity entry)] (release outcome))
        (locking sessions
          (when (identical? ticket (get-in @sessions (conj path :ticket)))
            (let [{:keys [signal waiters]} (get-in @sessions path)]
              (doseq [{:keys [release]} (vals waiters)] (when release (release)))
              (deliver signal true))
            (update-entry! path "task.ended"
                           #(-> %
                                (assoc :outcome (cond-> outcome
                                           (:cancelling %) (assoc :cancelling (:cancelling %))
                                           (and (= :cancelled state) (:cancel-message %))
                                           (assoc :message (:cancel-message %))))
                        (dissoc :interrupt :interrupt-complete :waiters :snapshot :shell-snapshot :admit :detach-reason)
                        (update :row merge (cond-> {:state state :view-ids []
                                                   :ended-at (System/currentTimeMillis)}
                                             error (assoc :error error)))))
            true))))))

(declare cancel!)

(defn- register!
  [session {:keys [kind name origin detachable? guest-data guest? public-name]
            :or {kind :tool detachable? true guest? true}} extra]
  (let [sid (str session)
        ticket (Object.)
        parent-path (when (and *ticket* (= sid (first *task*))) (apply key-for *task*))]
    (locking sessions
      (let [parent (when parent-path (get-in @sessions parent-path))]
        (when (and parent-path (not (identical? *ticket* (:ticket parent))))
          (throw (ex-info "The parent Task no longer belongs to this session." {})))
        (when (and (:manual? extra) (:cancel-on-request? parent) (get-in parent [:row :cancel-reason]))
          (throw (ex-info "The shell starter was cancelled." {}))))
      (let [n (inc (long (get-in @sessions [sid :sequence] 0)))
            id (str "t" n)
            row {:id id
                 :session-id sid
                 :kind kind
                 :name (str id " " name)
                 :origin (or origin {})
                 :started-at (System/currentTimeMillis)
                 :state :running
                 :view-ids []
                 :detached false}]
        (swap! sessions update sid assoc :sequence n
               :entries (assoc (get-in @sessions [sid :entries]) id
                               (merge {:row row :number n :signal (promise) :detachable? detachable?
                                       :ticket ticket :guest? guest? :guest-data guest-data
                                       :public-name (when (and (string? public-name) (not (str/blank? public-name)))
                                                      (str/replace (subs public-name 0 (min 160 (count public-name)))
                                                                   #"[^\p{L}\p{N}_.-]" "_"))}
                                      extra)))
        (when (and parent-path (:manual? extra) (get-in @sessions (conj parent-path :cancel-on-request?)))
          (swap! sessions update-in (conj parent-path :handoff-rollbacks) (fnil conj [])
                 #(when (cancel! sid id :agent)
                    (when-not (:external-settlement? extra) ((:interrupt extra) :cancel)))))
        (publish-row! sid "task.started" (get-in @sessions (key-for sid id)))
        [row ticket]))))

(defn guest-task
  "The guest descriptor, or nil for a Task it does not own."
  [session id]
  (let [{:keys [row guest? guest-data]} (get-in @sessions (key-for session id))]
    (when guest?
      (merge (select-keys row [:id :name :kind]) guest-data))))

(defn owns-generation?
  "Whether this Task still names the given process generation."
  [session id generation]
  (and (some? generation)
       (= generation (get-in @sessions (conj (key-for session id) :guest-data :generation)))))

(defn shell-snapshot
  "Read only the shell generation named by this capability."
  [session task-id generation origin id]
  (let [{:keys [guest-data outcome shell-snapshot]} (get-in @sessions (key-for session task-id))]
    (when (and generation (= generation (:generation guest-data))
               (= origin (:shell-origin guest-data)) (= id (get-in guest-data [:snapshot "id"])))
      (if outcome
        (or (:snapshot outcome) (:value outcome))
        (when shell-snapshot (shell-snapshot))))))

(defn start-manual!
  "Register work whose owner will call `complete!` with an outcome function once
   it ends. Its interrupt is installed before the row becomes visible."
  [session opts interrupt]
  (let [extra (merge (select-keys opts [:resource :snapshot :shell-snapshot :external-settlement? :admit :detach-reason])
                     {:manual? true :interrupt interrupt})
        [{:keys [id] :as row} ticket] (register! session opts extra)]
    {:row row :complete! (partial settle! session id ticket)}))

(defn resource-task
  "The newest Task row for a resource owned by this session, if any."
  [session resource]
  (->> (get-in @sessions [(str session) :entries])
       vals
       (filter #(= resource (:resource %)))
       (sort-by :number >)
       first
       public-row))

(defn discard!
  "Remove a reservation that failed before admission."
  [session id]
  (let [path (key-for session id)]
    (locking sessions
      (when-let [entry (get-in @sessions path)]
        (swap! sessions update-in (pop path) dissoc id)
        (when (= :running (get-in entry [:row :state]))
          (publish-row! session "task.ended"
                        (update entry :row assoc :state :cancelled :ended-at (System/currentTimeMillis)
                                :view-ids []))))))
  nil)

(defn admission
  "The launch callback of a pending resource Task."
  [session id]
  (get-in @sessions (conj (key-for session id) :admit)))

(defn complete-resource!
  "Settle the current running Task for this resource once."
  [session resource produce]
  (when-let [id (:id (resource-task session resource))]
    (when-let [ticket (get-in @sessions (conj (key-for session id) :ticket))]
      (settle! session id ticket produce))))

(defn status
  "Read a Task without acknowledging its result. A live snapshot runs outside the lock."
  ([session id] (status session id false))
  ([session id wire?]
   (let [path (key-for session id)
         {:keys [row snapshot ticket]} (get-in @sessions path)]
     (when row
       (let [record (when snapshot (snapshot id))
            entry (get-in @sessions path)]
        (when (identical? ticket (:ticket entry))
          (let [{:keys [value] :as outcome} (:outcome entry)]
            (cond-> {:task (if wire? (wire-row entry) (:row entry))}
              (or (:record outcome) record) (assoc :record (or (:record outcome) record))
              (and (contains? outcome :value)
                    (= :council (get-in entry [:guest-data :readiness]))) (assoc :value value)))))))))

(defn start!
  "Start `run` once. It returns `{:state :done|:failed|:cancelled, ...}`."
  [session {:keys [hold-activity cancel-on-request?] :as opts} run]
  (let [[{:keys [id] :as row} ticket] (register! session opts {:cancel-on-request? cancel-on-request?})
        sid (str session)
        path (key-for sid id)]
    (try (let [release (when hold-activity (hold-activity [sid id]))
               admitted? (locking sessions
                           (when (identical? ticket (get-in @sessions (conj path :ticket)))
                             (swap! sessions assoc-in (conj path :release-activity) release)
                             true))]
           (if admitted?
             (cancellation/worker-future
               (str "vis-task-" id)
               (bound-fn []
                 (when (locking sessions
                         (identical? ticket (get-in @sessions (conj path :ticket))))
                   (binding [*task* [sid id]
                             *ticket* ticket]
                     (let [outcome (try (run)
                                        (catch Throwable e
                                          {:state :failed :error (or (ex-message e) (str e))}))]
                       (settle! sid id ticket (constantly outcome)))))))
             (when release (release {:state :cancelled}))))
         (catch Throwable e
           (settle! sid id ticket (constantly {:state :failed :error (or (ex-message e) (str e))}))))
    row))

(defn install-interrupt!
  "Set the interrupt for the executing Task. Honor a request made before it started."
  [interrupt]
  (when-let [[session id] *task*]
    (let [path (key-for session id)
          kind (locking sessions
                 (if (= :running (get-in @sessions (conj path :row :state)))
                   (get-in (swap! sessions assoc-in (conj path :interrupt) interrupt)
                           (conj path :interrupt-kind))
                   :interrupt))]
      (when kind (interrupt kind)))))

(defn- cancel-result*
  [session id reason kind message]
  (let [path (key-for session id)
        result (locking sessions
                 (when-let [{:keys [row interrupt manual? cancel-on-request? closing? interrupt-complete]}
                            (get-in @sessions path)]
                   (let [previous (:cancel-reason row)]
                     (cond
                       (not= :running (:state row)) {:requested false :reason "already_ended"}
                       (or closing? (and manual? interrupt-complete)
                           (and cancel-on-request? previous)
                           (and (= reason :person) (= :person previous)))
                       {:requested false :reason (if previous "already_requested" "already_ended")}
                       :else
                       (let [complete (when manual? (promise))]
                         (update-entry! path "task.updated"
                                        #(cond-> (-> %
                                                     (assoc :interrupt-kind kind :cancel-message message)
                                                     (update :cancelling (fnil inc 0))
                                                     (assoc-in [:row :cancel-reason] reason))
                                           complete (assoc :interrupt-complete complete)))
                         {:requested true :interrupt interrupt :complete complete})))))]
    (when-let [interrupt (:interrupt result)]
      (if-let [complete (:complete result)]
        (cancellation/worker-future "vis-task-cancel"
          (fn [] (try (interrupt kind) (finally (deliver complete true)))))
        (interrupt kind)))
    (some-> result (select-keys [:requested :reason]))))

(defn cancel-result!
  "Request cancellation and return the decision, or nil if the Task does not exist."
  [session id reason]
  (cancel-result* session id reason :cancel nil))

(defn cancel!
  "Request an interrupt and report whether the Task accepted it."
  ([session id reason] (cancel! session id reason :cancel))
  ([session id reason kind] (cancel! session id reason kind nil))
  ([session id reason kind message]
   (boolean (:requested (cancel-result* session id reason kind message)))))

(defn cancelling [session id]
  (get-in @sessions (conj (key-for session id) :cancelling) 0))

(defn uncancel! [session id]
  (let [path (key-for session id)]
    (locking sessions
      (when-let [entry (get-in @sessions path)]
        (let [n (max 0 (dec (long (:cancelling entry 0))))]
          (swap! sessions update-in path
                 #(cond-> (assoc % :cancelling n) (zero? n) (dissoc :interrupt-kind)))
          n)))))

(defn rename! [session id name]
  (let [path (key-for session id)]
    (locking sessions
      (when-let [entry (get-in @sessions path)]
        (swap! sessions assoc-in (conj path :row :name) name)
        (when (= :running (get-in entry [:row :state]))
          (publish-row! session "task.updated" (get-in @sessions path)))
        nil))))

(defn await!
  "Read an outcome, or wait for it. `token` registers a wait by this block."
  [session id {:keys [block token timeout-ms leave? hold observation?]}]
  (let [path (key-for session id)
        signal (locking sessions
                 (when-let [entry (get-in @sessions path)]
                   (when token
                     (if leave?
                       (do (when-let [release (get-in entry [:waiters token :release])] (release))
                           (swap! sessions update-in (conj path :waiters) dissoc token))
                       (when (and (= :running (get-in entry [:row :state]))
                                  (not (contains? (:waiters entry) token)))
                         ;; A requested cancel ends the Task: wait for that end, not for a question.
                         (if-let [reason (when (and (not observation?) (:detachable? entry) (:detach-reason entry)
                                                    (zero? (long (:cancelling entry 0))))
                                           ((:detach-reason entry)))]
                           (do (swap! sessions update (first path)
                                      (fn [owner]
                                    (-> owner
                                        (update-in [:detach-notices block] (fnil conj [])
                                                   {:task (:row entry) :reason reason})
                                        (assoc-in [:entries id :row :detached] true)
                                        (assoc-in [:entries id :waiters token] {:block block :detached? true}))))
                               (publish-row! session "task.updated" (get-in @sessions path)))
                           (swap! sessions assoc-in (conj path :waiters token)
                                  {:block block :observation? observation? :release (when hold (hold))})))))
                   (when-not (and (get-in @sessions (into path [:waiters token :detached?]))
                                  (zero? (long (:cancelling (get-in @sessions path) 0))))
                     (:signal (get-in @sessions path)))))]
    (if leave?
      {:state :released}
      (do (when (and signal (pos? (long (or timeout-ms 0))))
            (deref signal (long timeout-ms) nil))
          (locking sessions
            (when-let [entry (get-in @sessions path)]
              (or (:outcome entry)
                  (when (and (get-in entry [:waiters token :detached?])
                             (zero? (long (:cancelling entry 0))))
                    {:state :detached})
                  {:state :running})))))))

(def ^:private receipt-cap 128)

(defn acknowledge! [session id]
  (let [path (key-for session id)]
    (locking sessions
      (when (get-in @sessions (conj path :outcome))
        ;; Keep a bounded receipt for other observers, but remove it from discovery.
        (swap! sessions update-in (pop path)
               (fn [entries]
                 (let [entries (assoc-in entries [id :acknowledged?] true)
                       expired (->> entries vals (filter :acknowledged?) (sort-by :number >)
                                    (drop receipt-cap) (map #(get-in % [:row :id])))]
                   (apply dissoc entries expired))))
        true))))

(defn- foreground-wait? [{:keys [detached? observation?]}]
  (not (or detached? observation?)))

(defn- mark-detached
  [session [id {:keys [row waiters]}] reason]
  (let [blocks (into #{} (comp (filter foreground-wait?) (map :block)) (vals waiters))]
    (-> (reduce (fn [held block]
                  (update-in held [:detach-notices block] (fnil conj []) {:task row :reason reason}))
                session blocks)
        (assoc-in [:entries id :signal] (promise))
        (assoc-in [:entries id :row :detached] true)
        (update-in [:entries id :waiters] update-vals
                    #(cond-> % (foreground-wait? %) (assoc :detached? true))))))

(defn detach-awaits!
  "Release current waits without interrupting their Tasks."
  [session reason]
  (let [sid (str session)]
    (locking sessions
      (when-let [entries (get-in @sessions [sid :entries])]
        ;; A Task with a cancel request keeps its waits until it ends.
        (let [detaching (filter (fn [[_ {:keys [row waiters detachable? cancelling]}]]
                                  (and detachable? (= :running (:state row))
                                       (zero? (long (or cancelling 0)))
                                       (some foreground-wait? (vals waiters))))
                                entries)]
          (swap! sessions update sid
                 (fn [session]
                   (reduce #(mark-detached %1 %2 reason)
                           session (sort-by (comp :number val) detaching))))
          (doseq [[id {:keys [signal]}] detaching]
            (publish-row! sid "task.updated" (get-in @sessions (key-for sid id)))
            (deliver signal true))
          (mapv (comp :row val) detaching))))))

(defn take-detach-notices!
  "Take the notifications for this block, not other blocks in the session."
  [session block]
  (let [sid (str session)]
    (locking sessions
      (when-let [notices (get-in @sessions [sid :detach-notices block])]
        (swap! sessions update-in [sid :detach-notices] dissoc block)
        notices))))

(defn take-ended-notices!
  "Take notices for detached Tasks that ended and whose result is unread."
  [session]
  (let [sid (str session)]
    (locking sessions
      (let [entries (get-in @sessions [sid :entries])
            ended (->> entries vals
                       (filter #(and (get-in % [:row :detached]) (:outcome %)
                                     (not (:end-notified? %))))
                       (sort-by :number))]
        (when (seq ended)
          (swap! sessions update-in [sid :entries]
                 (fn [held]
                   (reduce #(assoc-in %1 [(get-in %2 [:row :id]) :end-notified?] true) held ended)))
          (mapv :row ended))))))

(defn leave-block!
  "Remove waits the block could not release as it unwound."
  [session block]
  (let [sid (str session)]
    (locking sessions
      (doseq [[id {:keys [waiters]}] (get-in @sessions [sid :entries])
              [token {:keys [release] :as waiter}] waiters
              :when (= block (:block waiter))]
        (when release (release))
        (swap! sessions update-in [sid :entries id :waiters] dissoc token)))))

(defn awaited-ids [session block]
  (into []
        (keep (fn [[id {:keys [row waiters]}]]
                (when (and (= :running (:state row))
                           (some #(and (= block (:block %)) (not (:detached? %))
                                        (not (:observation? %)))
                                  (vals waiters)))
                  id)))
        (get-in @sessions [(str session) :entries])))

(defn sweep!
  "Remove the session's Tasks and request stop. A late outcome cannot recreate them."
  [session reason]
  (let [sid (str session)
        entries (locking sessions
                  (let [entries (sort-by :number (vals (get-in @sessions [sid :entries])))]
                    (if (= reason :session-close)
                      (swap! sessions dissoc sid)
                      (when (contains? @sessions sid)
                        (swap! sessions update sid dissoc :entries :detach-notices)))
                    (doseq [entry entries :when (= :running (get-in entry [:row :state]))]
                      (publish-row! sid "task.ended"
                                    (update entry :row assoc :state :cancelled :ended-at (System/currentTimeMillis)
                                            :cancel-reason reason :view-ids [])))
                    entries))]
    (doseq [{:keys [interrupt signal waiters views row release-activity]} entries]
      (doseq [{:keys [release]} (vals waiters)] (when release (release)))
      (deliver signal true)
      (close-views! views {:reason :interrupted})
      (when release-activity (release-activity {:state :cancelled}))
      (when (and interrupt (= :running (:state row)))
        (cancellation/worker-future "vis-task-sweep"
          (fn [] (interrupt :interrupt)))))
    (mapv #(assoc (:row %) :cancel-reason reason) entries)))
