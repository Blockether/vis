(ns com.blockether.vis.internal.janitor
  "The one background worker for maintenance: work that keeps a store small and
   that no user waits for.

   One daemon thread at the lowest priority takes the tasks one after another. Two
   chores never run at the same time, and an idle worker waits without a poll. A
   task is data, `{:id :run :delay-ms :every-ms :on}`, so each task has its own times:
   [[submit!]] runs it after `:delay-ms` and then, with `:every-ms`, again that
   long after each run ends. A task can also run on a signal that matches `:on`.

   The worker writes nothing about itself. A run that did work leaves one line with
   the task, the time and the result of the run, and a run that failed leaves one
   warning. A run that answers nil found no work and leaves no line.

   The thread starts at the first [[submit!]], never at namespace load, so the
   native image holds no thread from its build. The owner of each store can give
   its own chore to the worker."
  (:require [com.blockether.vis.internal.bus :as bus]
            [com.blockether.vis.internal.util :as util]
            [taoensso.telemere :as tel])
  (:import
    (java.util.concurrent ScheduledFuture ScheduledThreadPoolExecutor ThreadFactory TimeUnit)))

(defn- thread-factory
  "Daemon threads at the lowest priority, named `vis-janitor-N`."
  ^ThreadFactory []
  (let [daemons (util/daemon-thread-factory "vis-janitor")]
    (reify
      ThreadFactory
        (newThread [_ runnable]
          (doto (.newThread daemons runnable) (.setPriority Thread/MIN_PRIORITY))))))

(defonce ^{:private true :doc "The one worker thread. A cancelled task leaves the queue at once."}
         executor
  (delay (doto (ScheduledThreadPoolExecutor. 1 (thread-factory)) (.setRemoveOnCancelPolicy true))))

(defonce ^{:private true
           :doc
           "Task id -> registration. Each change holds the queue monitor."}
         queue
  (atom {}))

(defn- run-once!
  "Run the task `id` one time and write its line. A throw stops here, because a
   repeating task that throws to its executor never runs again."
  [id run]
  (let [started (util/now-ms)]
    (try (when-some [result (run)]
           (let [ms (- (util/now-ms) started)]
             (tel/log! {:level :info
                        :id ::ran
                        :data {:task id :ms ms :result result}
                        :msg (str "Task " id " ran in " ms " ms")})))
         (catch Throwable t
           (let [ms (- (util/now-ms) started)]
             (tel/log! {:level :warn
                        :id ::failed
                        :data {:task id :ms ms}
                        :error t
                        :msg (str "Task " id " failed after " ms " ms")}))))))

(defn- forget!
  "Remove the registration and its work if `token` still owns the task."
  [id token]
  (locking queue
    (let [{:keys [timer trigger subscription] :as task} (get @queue id)]
      (when (identical? token (:token task))
        (swap! queue dissoc id)
        (.cancel ^ScheduledFuture timer false)
        (when trigger (.cancel ^ScheduledFuture trigger false))
        (when subscription (bus/unsubscribe! :vis subscription))))))

(defn- trigger!
  "Queue one extra run. The timer keeps its own place on the worker."
  [id token run]
  (locking queue
    (let [task (get @queue id)]
      (when (and (identical? token (:token task)) (nil? (:trigger task)))
        (let [^ScheduledThreadPoolExecutor worker @executor
              ^Runnable body
              (fn []
                (when (locking queue
                        (when (identical? token (get-in @queue [id :token]))
                          (swap! queue update id dissoc :trigger)))
                  (run-once! id run)))
              handle (.schedule worker body 0 TimeUnit/MILLISECONDS)]
          (swap! queue assoc-in [id :trigger] handle))))))

(defn submit!
  "Give `task` to the worker and answer its `:id`.

   `:run` is a function of no arguments. It runs on the worker thread after
   `:delay-ms` (default 0). With `:every-ms` it runs again that long after each run
   ends, until [[cancel!]]. A task with the id of a waiting or repeating task
   replaces that task. A run in progress finishes first, so two tasks never run at
   the same time. An `:every-ms` that is not more than 0 throws, and the waiting
   task stays.

   If `:on` is a bus pattern, a matching signal queues an extra run on the same
   thread. Only one extra run can wait at a time. A signal does not change the
   timer. A task without `:every-ms` stops listening when its timer run ends.

   `:run` answers nil when it found no work. Each other answer goes to the log
   with the time of the run. A throw goes to the log as a warning, and a repeating
   task keeps its times. The worker thread has no dynamic bindings of the caller:
   a task that needs them wraps `:run` in `bound-fn*`."
  [{:keys [id run delay-ms every-ms on]}]
  (when (and every-ms (not (pos? (long every-ms))))
    (throw (IllegalArgumentException.
             (str "The :every-ms of the task " id " must be more than 0"))))
  (let [token (Object.)
        delay-ms (long (or delay-ms 0))
        ^ScheduledThreadPoolExecutor worker @executor
        ^Runnable body
        (fn []
          (when (locking queue (identical? token (get-in @queue [id :token])))
            (run-once! id run)
            (when-not every-ms (forget! id token))))]
    (locking queue
      (let [subscription (when (some? on) (bus/subscribe! on (fn [_] (trigger! id token run))))
            timer (try (if every-ms
                         (.scheduleWithFixedDelay worker body delay-ms (long every-ms) TimeUnit/MILLISECONDS)
                         (.schedule worker body delay-ms TimeUnit/MILLISECONDS))
                       (catch Throwable t
                         (when subscription (bus/unsubscribe! :vis subscription))
                         (throw t)))]
        (when-some [old-token (get-in @queue [id :token])] (forget! id old-token))
        (swap! queue assoc id {:token token :timer timer :subscription subscription})))
    id))

(defn cancel!
  "Take the task `id` off the worker. Answer true when it waited or repeated. A run
   in progress finishes."
  [id]
  (locking queue
    (if-some [token (get-in @queue [id :token])]
      (do (forget! id token) true)
      false)))

(defn queued "The ids of the tasks that wait or repeat." [] (set (keys @queue)))
