;; User request 2026-10-31: measure what each part of a turn costs in time and memory.
(ns com.blockether.vis.internal.loop.perf
  "Time and memory of each iteration and turn, logged at info level.

   `iteration-loop*` binds `*ledger*` for one iteration. Each `log-stage!` with a
   `:duration-ms` adds to it. One `::iteration` line then gives the wall time of each
   loop phase, the model and tool time, the framework overhead and the heap."
  (:require [clojure.string :as str]
            [taoensso.telemere :as tel])
  (:import (java.io File)))

(def ^:dynamic *ledger*
  "The atom of stage times of the running iteration: stage → `{:ms :n}`, or nil."
  nil)

(defn new-ledger [] (atom {}))

(defn add-stage
  "Ledger `m` with `ms` added to `stage`. A nil or negative `ms` changes nothing."
  [m stage ms]
  (if (and (number? ms) (not (neg? (double ms))))
    (-> m
        (update-in [stage :ms] (fnil + 0.0) (double ms))
        (update-in [stage :n] (fnil inc 0)))
    m))

(defn record-stage!
  "Add `ms` of `stage` to the bound ledger. Never throws."
  [stage ms]
  (when-let [a *ledger*]
    (swap! a add-stage stage ms)))

(defn now-ns ^long [] (System/nanoTime))

(defn ms-since ^double [^long start-ns] (/ (double (- (System/nanoTime) start-ns)) 1000000.0))

(defmacro timed
  "Run `body` and add its wall time to `stage` in the bound ledger, also on a throw."
  [stage & body]
  `(let [start# (now-ns)]
     (try ~@body (finally (record-stage! ~stage (ms-since start#))))))

(defn- round1 [x] (when (number? x) (/ (Math/round (* 10.0 (double x))) 10.0)))

(defn summary
  "One iteration or turn as a log map. `phases` is phase → ms, `stages` the ledger,
   `wall-ms` the measured total. `:overhead-ms` is the wall time without the model
   call and the tool code, never below zero."
  [wall-ms phases stages]
  (let [stage-ms
        #(double (get-in stages [% :ms] 0.0))

        model
        (stage-ms :provider-call/stop)

        code
        (stage-ms :code-result)]

    {:wall-ms (round1 wall-ms)
     :model-ms (round1 model)
     :code-ms (round1 code)
     :overhead-ms (round1 (max 0.0 (- (double wall-ms) model code)))
     :phases (into (sorted-map)
                   (map (fn [[k v]]
                          [k (round1 v)]))
                   phases)
     :stages (into (sorted-map)
                   (map (fn [[k {:keys [ms n]}]]
                          [k {:ms (round1 ms) :n n}]))
                   stages)}))

(defn- linux-rss-mb
  "Resident memory of this process from /proc, in MiB, or nil off Linux."
  []
  (let [f (File. "/proc/self/status")]
    (when (.exists f)
      (some (fn [^String line]
              (when (str/starts-with? line "VmRSS:")
                (let [kb (parse-long (re-find #"\d+" line))]
                  (when kb (round1 (/ (double kb) 1024.0))))))
            (str/split-lines (slurp f))))))

(defn memory
  "Heap in use and, on Linux, resident memory, in MiB. A native image reports its
   reserved address space as total memory, so committed heap is not logged."
  []
  (let [rt
        (Runtime/getRuntime)

        rss
        (linux-rss-mb)]

    (cond-> {:heap-used-mb (round1 (/ (double (- (.totalMemory rt) (.freeMemory rt))) 1048576.0))}
      rss
      (assoc :rss-mb rss))))

(defn log-iteration!
  "Log one iteration line at info level. Never throws."
  [iteration wall-ms phases stages]
  (try (tel/log! {:level :info
                  :id ::iteration
                  :data (merge {:iteration iteration} (summary wall-ms phases stages) (memory))})
       (catch Throwable _ nil)))

(defn log-turn!
  "Log one turn line at info level. Never throws."
  [iterations wall-ms phases stages]
  (try (tel/log! {:level :info
                  :id ::turn
                  :data (merge {:iterations iterations} (summary wall-ms phases stages) (memory))})
       (catch Throwable _ nil)))
