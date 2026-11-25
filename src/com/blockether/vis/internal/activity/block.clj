(ns com.blockether.vis.internal.activity.block
  "The Activity of one executed code block.

   Host tool events are recorded FIFO off the tool-callback threads, running snapshots
   are coalesced before they reach a listener. A block ends its own calls, while an
   admitted Task can keep writing its Activity. Durable history is an injected store."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.activity.core :as activity]
            [com.blockether.vis.internal.activity.event :as event]
            [com.blockether.vis.internal.util :as util]
            [taoensso.telemere :as tel])
  (:import [java.util.concurrent ExecutionException ExecutorService Future]))

(def ^:private coalesce-ms
  "Floor between two live Activity publications from one block.

   A running snapshot is a convenience, not the record: it rides the bus as a
   TRANSIENT frame a full queue may drop, and every publication also costs a line
   of the turn's journal, which is truncated whole once it passes its file cap. The
   settled replacement is durable and also persists on the form, so a tool storm
   coalesces here instead of spending the turn's replay budget on pictures nobody reads."
  120)

(defn- serial-dispatcher
  "Run Activity lifecycle transitions FIFO off the tool-callback threads.

   Returns `[dispatch! shutdown!]`. `dispatch!` captures the caller's dynamic
   bindings and answers a Future; optional delay-ms schedules a trailing flush
   on the same serial worker. At most 64 immediate transitions can be pending;
   producers wait without dropping admitted events. Shutdown discards delayed flushes."
  []
  (let [executor
        (doto (java.util.concurrent.ScheduledThreadPoolExecutor. 1
                                                                 (util/daemon-thread-factory
                                                                   "vis-activity-dispatch"))
          (.setExecuteExistingDelayedTasksAfterShutdownPolicy false))

        pending
        (java.util.concurrent.Semaphore. 64)

        dispatch!
        (fn [f & [delay-ms]]
          (let [^java.util.concurrent.Callable task
                (bound-fn [] (try (f) (finally (when-not delay-ms (.release pending)))))]
            (if delay-ms
              (.schedule executor task (long delay-ms) java.util.concurrent.TimeUnit/MILLISECONDS)
              (do (.acquireUninterruptibly pending)
                  (try (.submit ^ExecutorService executor task)
                       (catch java.util.concurrent.RejectedExecutionException error
                         (.release pending)
                         (throw error)))))))]

    [dispatch! #(.shutdown ^ExecutorService executor)]))

(defn start!
  "Open the Activity of one block and its serial worker.

   `store`, when present, keeps the Activity durable:
   `{:history-id id :apply! (fn [event] state) :settle! (fn [outcome summary selection]) :page (fn [] page)}`.
   Without one the Activity lives in memory for the block. `on-snapshot` receives
   coalesced presentations; `on-late-snapshot` receives the Task's last revision
   after block exit. `on-event` sees the events recorded before block exit.
   Answers the block that [[record!]] and [[settle!]] take."
  [{:keys [store on-snapshot on-late-snapshot on-event]}]
  (let [[dispatch! shutdown!] (serial-dispatcher)]
    {:history-id (:history-id store)
     :context (event/context)
     :store store
     :collector (when-not store (event/collector))
     :on-snapshot on-snapshot
     :on-late-snapshot (or on-late-snapshot on-snapshot)
     :on-event on-event
     :dispatch! dispatch!
     :shutdown! shutdown!
     ;; Durable invocations are separate from the bounded page carried by the form.
     :state (atom activity/empty-state)
     :error (atom nil)
     ;; The dispatcher owns these transitions. At block end only admitted Tasks
     ;; can add events. Their invocation IDs remain private to the dispatcher.
     :phase (atom :open)
     :tasks (atom {})
     :published-at (atom 0)
     :flush-pending? (atom false)}))

(defn- presentation
  [{:keys [store]} state]
  (if store ((:page store)) (activity/presentation state)))

(defn- publish!
  [{:keys [on-snapshot dispatch! state phase published-at flush-pending?] :as block} snapshot]
  (when (and on-snapshot (activity/detected? snapshot))
    (let [now
          (long (util/now-ms))

          delay-ms
          (- (long coalesce-ms) (- now (long @published-at)))]

      (if (pos? delay-ms)
        (when (compare-and-set! flush-pending? false true)
          (dispatch! (fn []
                       (reset! flush-pending? false)
                       (when (= :open @phase) (publish! block @state)))
                     delay-ms))
        (do (reset! published-at now)
            (try (on-snapshot (presentation block snapshot)) (catch Throwable _ nil)))))))

(def ^:private council-conversation
  "Council calls that read the conversation between sessions. Activity keeps their rows. A
   publication has no row, because the team notes already show each message."
  #{"council.read" "council.get" "council.threads" "council.members" "vis.council.wake"})

(defn- team-call?
  "Whether `event` belongs to a subagent call or a Council publication, built in or through
   the host door. The agent table and the team notes show these, so such a call reaches
   `on-event` but leaves no Activity row."
  [event]
  (let [operation (some-> (:operation event)
                          name)]
    (boolean (and operation
                  (or (str/starts-with? operation "council.")
                      (str/starts-with? operation "vis.council."))
                  (not (contains? council-conversation operation))))))

(defn- dispatch-event!
  [{:keys [dispatch!]} f]
  (try (dispatch! f) (catch java.util.concurrent.RejectedExecutionException _ nil)))

(defn- close-if-idle!
  [{:keys [phase tasks shutdown!]}]
  (when (and (= :draining @phase) (empty? @tasks))
    (reset! phase :closed)
    (shutdown!)))

(defn- settle-rows!
  [{:keys [store state]} outcome summary selection]
  (if store
    (when-let [snapshot ((:settle! store) outcome summary selection)]
      (reset! state snapshot))
    (swap! state activity/settle-running outcome summary selection))
  @state)

(defn hold!
  "Keep the dispatcher open for a Task admitted before it runs. Return its release function."
  [{:keys [tasks phase on-late-snapshot] block-error :error :as block} task-id]
  (dispatch-event! block #(when (not= :closed @phase) (swap! tasks assoc task-id #{})))
  (fn [{:keys [state]}]
    (dispatch-event!
      block
      (fn []
        (when-let [ids (get @tasks task-id)]
          (try
            (when (seq ids)
              (let [[outcome summary] (case state
                                        :done [:succeeded "Task completed"]
                                        :failed [:failed "Task failed"]
                                        [:cancelled "Task cancelled"])]
                (settle-rows! block outcome summary {:only ids})))
            (catch Throwable failure
              (compare-and-set! block-error nil failure)
              (tel/log! {:level :warn :id ::task-settlement-failed :error failure
                         :msg "Task Activity settlement failed"}))
            (finally
              (swap! tasks dissoc task-id)
              ;; Deliver the last picture now; shutdown discards a coalesced flush.
              (when (and (= :draining @phase) on-late-snapshot (activity/detected? @(:state block)))
                (try (on-late-snapshot (presentation block @(:state block)))
                     (catch Throwable _ nil)))
              (close-if-idle! block))))))))

(defn record!
  "Record a lifecycle event. After the block ends, only its admitted Tasks can record."
  ([block tool-event] (record! block nil tool-event))
  ([{:keys [store collector on-event state error phase tasks] :as block} task-id tool-event]
    (dispatch-event!
      block
      (fn []
        (let [owned? (contains? @tasks task-id)
              open? (= :open @phase)
              admitted? (or owned? (and open? (nil? task-id)))]
          (when (and admitted? (not (team-call? tool-event)))
            (let [id (:invocation-id tool-event)
                  edge (:phase tool-event)
                  _ (when (and owned? (= :start edge)) (swap! tasks update task-id conj id))
                  snapshot (if store
                             (try (reset! state ((:apply! store) tool-event))
                                  (catch Throwable failure
                                    (compare-and-set! error nil failure)
                                    nil))
                             (do (event/accept! collector tool-event)
                                 (swap! state activity/reduce-event tool-event)))]
              (when snapshot
                (when (and owned? (= :terminal edge)) (swap! tasks update task-id disj id))
                (when (and open? (event/visible-event? tool-event))
                  (publish! block snapshot)))))
          (when (and open? admitted? on-event) (on-event tool-event)))))))

(defn- settled-outcome
  [envelope]
  (cond (or (:timeout? envelope) (= :vis/interrupted (get-in envelope [:error :type]))) :cancelled
        (:error envelope) :failed
        :else :cancelled))

(defn- settled-summary
  [envelope]
  (cond (:timeout? envelope) "Evaluation timed out"
        (:error envelope) (or (:message (:error envelope)) "Evaluation failed")
        :else "Evaluation activity"))

(defn settle!
  "End the block's part of Activity and return its current projection in the envelope."
  [{:keys [dispatch! state error phase tasks] :as block} envelope]
  (let [projection
        (try
          (.get ^Future
                (dispatch!
                  (fn []
                    (reset! phase :draining)
                    (try
                      (let [selection {:except (into #{} cat (vals @tasks))}
                            snapshot (settle-rows! block
                                                   (settled-outcome envelope)
                                                   (settled-summary envelope)
                                                   selection)]
                        (when (activity/detected? snapshot) (presentation block snapshot)))
                      (finally (close-if-idle! block))))))
          (catch ExecutionException e
            (compare-and-set! error nil (.getCause e))
            (tel/log! {:level :warn :id ::settlement-failed :error (.getCause e)
                       :msg "Activity settlement failed"})
            (when (activity/detected? @state)
              (try (presentation block @state) (catch Throwable _ nil)))))]
    (cond-> envelope
      (and @error (nil? (:error envelope)))
      (assoc :error {:type :activity/persistence
                     :message "Activity history could not be saved or read. Check storage before retrying."})
      projection (assoc :activity projection))))
