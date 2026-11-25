(ns com.blockether.vis.internal.task
  "Session-owned work. The registry owns state; work and interrupt functions run outside its lock."
  (:require [com.blockether.vis.internal.session.cancellation :as cancellation]
            [taoensso.telemere :as tel])
  (:import [java.util UUID]))

(defonce ^:private sessions (atom {}))

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
       (sort-by :number)
       (mapv public-row)))

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
          (swap! sessions update-in path
                 #(-> %
                      (assoc-in [:views view-id] close!)
                      (update-in [:row :view-ids] conj view-id)))
          true)))))

(defn forget-view!
  "Release a view already closed by the tool or person."
  [[session id] view-id]
  (let [path (key-for session id)]
    (locking sessions
      (when (get-in @sessions (conj path :views view-id))
        (swap! sessions update-in path
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
  (when-let [interrupted (:interrupt-complete entry)] @interrupted)
  (let [outcome (try (produce)
                     (catch Throwable e
                       {:state :failed :error (or (ex-message e) (str e))}))]
    (if (or (:interrupt-complete entry)
            (and (:cancel-on-request? entry) (get-in entry [:row :cancel-reason])))
      (do (doseq [rollback (:handoff-rollbacks entry)] (rollback))
          {:state :cancelled})
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
            (swap! sessions update-in path
                   #(-> %
                        (assoc :outcome (cond-> outcome
                                           (:cancelling %) (assoc :cancelling (:cancelling %))
                                           (and (= :cancelled state) (:cancel-message %))
                                           (assoc :message (:cancel-message %))))
                        (dissoc :interrupt :interrupt-complete :waiters)
                        (update :row merge (cond-> {:state state :view-ids []
                                                   :ended-at (System/currentTimeMillis)}
                                             error (assoc :error error)))))
            true))))))

(declare cancel!)

(defn- register!
  [session {:keys [kind name origin detachable? guest-data guest?]
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
                                       :ticket ticket :guest? guest? :guest-data guest-data}
                                      extra)))
        (when (and parent-path (:manual? extra) (get-in @sessions (conj parent-path :cancel-on-request?)))
          (let [interrupt (:interrupt extra)]
            (swap! sessions update-in (conj parent-path :handoff-rollbacks) (fnil conj [])
                   (fn [] (when (cancel! sid id :agent) (interrupt :cancel))))))
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

(defn start-manual!
  "Register work whose owner will call `complete!` with an outcome function once
   it ends. Its interrupt is installed before the row becomes visible."
  [session opts interrupt]
  (let [[{:keys [id] :as row} ticket] (register! session opts {:manual? true :interrupt interrupt})]
    {:row row :complete! (partial settle! session id ticket)}))

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

(defn cancel!
  "Request an interrupt and report whether the Task was still running."
  ([session id reason] (cancel! session id reason :cancel))
  ([session id reason kind] (cancel! session id reason kind nil))
  ([session id reason kind message]
   (let [path (key-for session id)
         request (locking sessions
                   (when-let [{:keys [row interrupt manual? cancel-on-request? closing? interrupt-complete]}
                              (get-in @sessions path)]
                     (when (and (= :running (:state row))
                                (or (not manual?) (not (or closing? interrupt-complete)))
                                 (or (not cancel-on-request?) (not (or closing? (:cancel-reason row)))))
                       (let [complete (when manual? (promise))]
                         (swap! sessions update-in path
                                #(cond-> (-> %
                                             (assoc :interrupt-kind kind :cancel-message message)
                                             (update :cancelling (fnil inc 0))
                                             (assoc-in [:row :cancel-reason] reason))
                                   complete (assoc :interrupt-complete complete)))
                         {:interrupt interrupt :complete complete}))))]
     (when-let [interrupt (:interrupt request)]
       (if-let [complete (:complete request)]
         (cancellation/worker-future "vis-task-cancel"
           (fn [] (try (interrupt kind) (finally (deliver complete true)))))
         (interrupt kind)))
     (boolean request))))

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
      (when (get-in @sessions path)
        (swap! sessions assoc-in (conj path :row :name) name)
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
                         (swap! sessions assoc-in (conj path :waiters token)
                                {:block block :observation? observation? :release (when hold (hold))}))))
                   (:signal entry)))]
    (if leave?
      {:state :released}
      (do (when (and signal (pos? (long (or timeout-ms 0))))
            (deref signal (long timeout-ms) nil))
          (locking sessions
            (when-let [entry (get-in @sessions path)]
              (or (when (get-in entry [:waiters token :detached?]) {:state :detached})
                  (:outcome entry)
                  {:state :running})))))))

(defn acknowledge! [session id]
  (let [path (key-for session id)]
    (locking sessions
      (when (get-in @sessions (conj path :outcome))
        (swap! sessions update-in (pop path) dissoc id)))
    nil))

(defn- mark-detached
  [session [id {:keys [row waiters]}] reason]
  (let [blocks (into #{} (comp (remove :detached?) (map :block)) (vals waiters))]
    (-> (reduce (fn [held block]
                  (update-in held [:detach-notices block] (fnil conj []) {:task row :reason reason}))
                session blocks)
        (assoc-in [:entries id :signal] (promise))
        (assoc-in [:entries id :row :detached] true)
        (update-in [:entries id :waiters] update-vals #(assoc % :detached? true)))))

(defn detach-awaits!
  "Release current waits without interrupting their Tasks."
  [session reason]
  (let [sid (str session)]
    (locking sessions
      (when-let [entries (get-in @sessions [sid :entries])]
        (let [detaching (filter (fn [[_ {:keys [row waiters detachable?]}]]
                                  (and detachable? (= :running (:state row))
                                       (some (complement :detached?) (vals waiters))))
                                entries)]
          (swap! sessions update sid
                 (fn [session]
                   (reduce #(mark-detached %1 %2 reason)
                           session (sort-by (comp :number val) detaching))))
          (doseq [[_ {:keys [signal]}] detaching] (deliver signal true))
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
                  (let [entries (vals (get-in @sessions [sid :entries]))]
                    (if (= reason :session-close)
                      (swap! sessions dissoc sid)
                      (when (contains? @sessions sid)
                        (swap! sessions update sid dissoc :entries :detach-notices)))
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
