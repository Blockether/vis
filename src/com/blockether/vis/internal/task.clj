(ns com.blockether.vis.internal.task
  "Session-owned work. The registry owns state; work and interrupt functions run outside its lock."
  (:require [com.blockether.vis.internal.session.cancellation :as cancellation]
            [taoensso.telemere :as tel])
  (:import [java.util UUID]))

(defonce ^:private sessions (atom {}))

(def ^:dynamic *task*
  "The `[session id]` of the Task that owns this host call."
  nil)

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

(defn- settle!
  [session id ticket {:keys [state error] :as outcome}]
  (let [path (key-for session id)
        entry (locking sessions
                (let [{:keys [row closing?] :as entry} (get-in @sessions path)]
                  (when (and (identical? ticket (:ticket entry))
                             (= :running (:state row)) (not closing?))
                    (swap! sessions update-in path #(-> % (assoc :closing? true)
                                                         (dissoc :views :release-activity)))
                    entry)))]
    (when entry
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
                       (dissoc :interrupt :waiters)
                       (update :row merge (cond-> {:state state :view-ids []
                                                 :ended-at (System/currentTimeMillis)}
                                           error (assoc :error error)))))
          true)))))

(defn start!
  "Start `run` once. It returns `{:state :done|:failed|:cancelled, ...}`."
  [session {:keys [kind name origin detachable? hold-activity] :or {kind :tool detachable? true}} run]
  (let [sid (str session)
        ticket (Object.)
        row (locking sessions
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
                                       {:row row :number n :signal (promise) :detachable? detachable?
                                        :ticket ticket}))
                row))
        id (:id row)
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
                   (binding [*task* [sid id]]
                     (settle! sid id ticket
                              (try (run)
                                   (catch Throwable e
                                     {:state :failed :error (or (ex-message e) (str e))})))))))
             (when release (release {:state :cancelled}))))
         (catch Throwable e
           (settle! sid id ticket {:state :failed :error (or (ex-message e) (str e))})))
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
                   (when-let [{:keys [row interrupt]} (get-in @sessions path)]
                     (when (= :running (:state row))
                       (swap! sessions update-in path
                              #(-> %
                                   (assoc :interrupt-kind kind :cancel-message message)
                                   (update :cancelling (fnil inc 0))
                                   (assoc-in [:row :cancel-reason] reason)))
                       {:interrupt interrupt})))]
     (when-let [interrupt (:interrupt request)] (interrupt kind))
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
  [session id {:keys [block token timeout-ms leave? hold]}]
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
                                {:block block :release (when hold (hold))}))))
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
                           (some #(and (= block (:block %)) (not (:detached? %))) (vals waiters)))
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
