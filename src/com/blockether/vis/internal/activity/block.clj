(ns com.blockether.vis.internal.activity.block
  "The Activity of one executed code block.

   Host tool events are recorded FIFO off the tool-callback threads, running snapshots
   are coalesced before they reach a listener, and the block settles its Activity once
   when it ends. Durable history is an injected store, so this domain never reaches
   into persistence."
  (:require [com.blockether.vis.internal.activity.core :as activity]
            [com.blockether.vis.internal.activity.event :as event]
            [com.blockether.vis.internal.util :as util]
            [taoensso.telemere :as tel])
  (:import [java.util.concurrent ExecutionException ExecutorService Future ThreadFactory]))

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
  (let [factory
        (reify
          ThreadFactory
            (newThread [_ runnable]
              (doto (Thread. ^Runnable runnable "vis-activity-dispatch") (.setDaemon true))))

        executor
        (doto (java.util.concurrent.ScheduledThreadPoolExecutor. 1 ^ThreadFactory factory)
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
   `{:history-id id :apply! (fn [event] state) :settle! (fn [outcome summary]) :page (fn [] page)}`.
   Without one the Activity lives in memory for the block. `on-snapshot` receives
   coalesced running presentations; `on-event` sees every recorded event in order.
   Answers the block that [[record!]] and [[settle!]] take."
  [{:keys [store on-snapshot on-event]}]
  (let [[dispatch! shutdown!] (serial-dispatcher)]
    {:history-id (:history-id store)
     :context (event/context)
     :store store
     :collector (when-not store (event/collector))
     :on-snapshot on-snapshot
     :on-event on-event
     :dispatch! dispatch!
     :shutdown! shutdown!
     ;; Durable invocations are separate from the bounded page carried by the form.
     :state (atom activity/empty-state)
     :error (atom nil)
     ;; `:open` until the settler freezes the picture. A tool callback that lands
     ;; after that is dropped rather than allowed to edit a snapshot already
     ;; handed to the wire and the database.
     :phase (atom :open)
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

(defn record!
  "Record one host tool event on the block's serial worker. A failed durable write is
   kept for [[settle!]]; an event that arrives after the block settled is dropped."
  [{:keys [store collector on-event dispatch! state error phase] :as block} tool-event]
  (try (dispatch! (fn []
                    (when (= :open @phase)
                      (let [snapshot (if store
                                       (try (reset! state ((:apply! store) tool-event))
                                            (catch Throwable failure
                                              (compare-and-set! error nil failure)
                                              @state))
                                       (do (event/accept! collector tool-event)
                                           (swap! state activity/reduce-event tool-event)))]
                        (when (event/visible-event? tool-event) (publish! block snapshot))))
                    (when on-event (on-event tool-event))))
       (catch java.util.concurrent.RejectedExecutionException _ nil)))

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
  "Settle the block's Activity once from its evaluation `envelope` and stop its worker.

   Answers the envelope with the settled `:activity` projection. When the history could
   not be saved or read, a block without an error of its own reports that failure."
  [{:keys [store dispatch! shutdown! state error phase] :as block} envelope]
  (let [outcome
        (settled-outcome envelope)

        summary
        (settled-summary envelope)

        ;; FIFO behind every transition already submitted: the settler sees
        ;; the last state the reducer reached, then closes the gate.
        final
        (try (.get ^Future
                   (dispatch! (fn []
                                (reset! phase :settled)
                                (when store ((:settle! store) outcome summary))
                                (swap! state activity/settle-running outcome summary))))
             (catch ExecutionException e
               (compare-and-set! error nil (.getCause e))
               (tel/log! {:level :warn
                          :id ::settlement-failed
                          :error (.getCause e)
                          :msg "Activity settlement failed"})
               @state))

        projection
        (when (activity/detected? final)
          (try
            (presentation block final)
            (catch Throwable e
              (compare-and-set! error nil e)
              (tel/log!
                {:level :warn :id ::read-failed :error e :msg "Activity history could not be read"})
              nil)))]

    (shutdown!)
    ;; The first page rides the form; further pages stay durable and load on demand.
    ;; Activity never enters stdout or model context.
    (cond-> envelope
      (and @error (nil? (:error envelope)))
      (assoc :error
        {:type :activity/persistence
         :message "Activity history could not be saved or read. Check storage before retrying."})

      projection
      (assoc :activity projection))))
