(ns com.blockether.vis.internal.decisions.cache
  "A bounded, reference-counted cache of native decision model sessions."
  (:require [com.blockether.vis.internal.config.core :as config]))

(set! *warn-on-reflection* true)

(def ^:dynamic *limits* nil)

(def ^:private lock (Object.))

(defonce ^:private entries (atom {}))

(defonce ^:private stopping? (atom false))

(defonce ^:private training? (atom false))

(defn- setting
  [name default lower upper]
  (let [text (config/extension-env-value name)]
    (if (some? text)
      (try (let [value (Long/parseLong (str text))]
             (if (<= lower value upper) value default))
           (catch NumberFormatException _ default))
      default)))

(defn- limits
  []
  (or *limits*
      {:max-models (setting "VIS_DECISION_MAX_RESIDENT" 2 1 8)
       :budget-mb (setting "VIS_DECISION_MEMORY_BUDGET_MB" 8192 4096 32768)
       :reserve-mb (setting "VIS_DECISION_RESERVE_MB" 4096 2048 16384)
       :max-loads (setting "VIS_DECISION_MAX_LOADS" 1 1 4)
       :max-infer (setting "VIS_DECISION_MAX_INFER" 1 1 8)
       :max-waiters (setting "VIS_DECISION_MAX_WAITERS" 8 0 64)
       :wait-ms (setting "VIS_DECISION_WAIT_MS" 60000 1000 180000)}))

(defn- busy!
  []
  (throw (ex-info "Decision model capacity is temporarily exhausted"
                  {:type :decisions/capacity-exceeded})))

(defn- unavailable [] (ex-info "Decision model cache is stopping" {:type :decisions/unavailable}))

(defn- unavailable! [] (throw (unavailable)))

(defn- close!
  [entry]
  (when-let [close (:close (:value entry))]
    (close)))

(defn status
  "Resident state for an immutable model key; installed files are checked separately."
  [key]
  (locking lock (or (:status (get @entries key)) :cold)))

(defn- allocate!
  [key {:keys [max-models budget-mb reserve-mb max-loads max-infer max-waiters]}]
  (when @stopping? (unavailable!))
  (when @training? (busy!))
  (let [all
        (vals @entries)

        active
        (reduce + (map :users all))

        loading
        (count (filter #(= :loading (:status %)) all))]

    (if-let [entry (get @entries key)]
      (do (when (:retire? entry) (unavailable!))
          (case (:status entry)
            :loading
            (if (>= (:waiters entry) max-waiters)
              (busy!)
              (do (swap! entries update-in [key :waiters] inc) {:wait (:ready entry)}))

            :ready
            (if (>= active max-infer)
              (busy!)
              (do (swap! entries update
                    key
                    #(-> %
                         (update :users inc)
                         (assoc :used (System/nanoTime))))
                  {:value (:value entry)}))))
      (let [idle (sort-by (comp :used second)
                          (for [[k v] @entries
                                :when (and (= :ready (:status v)) (zero? (:users v)))]

                            [k v]))
            required (fn [remaining]
                       (or (>= (count remaining) max-models)
                           (> (+ reserve-mb (reduce + (map :reserved-mb (vals remaining))))
                              budget-mb)))
            [remaining evicted]
            (loop [remaining @entries
                   evicted []
                   candidates idle]

              (if (and (required remaining) (seq candidates))
                (let [[victim entry] (first candidates)]
                  (recur (dissoc remaining victim) (conj evicted entry) (rest candidates)))
                [remaining evicted]))]

        (if (or (>= loading max-loads) (>= active max-infer) (required remaining))
          (busy!)
          (let [ready (promise)]
            (reset! entries (assoc remaining
                              key {:status :loading
                                   :users 1
                                   :waiters 0
                                   :ready ready
                                   :reserved-mb reserve-mb
                                   :used (System/nanoTime)}))
            {:load ready :evicted evicted}))))))

(defn- release!
  [key]
  (let [retired (locking lock
                  (let [entry (-> (get @entries key)
                                  (update :users dec)
                                  (assoc :used (System/nanoTime)))]
                    (if (and (:retire? entry) (zero? (long (:users entry))))
                      (do (swap! entries dissoc key) entry)
                      (do (swap! entries assoc key entry) nil))))]
    (when retired (close! retired))))

(defn with-resident!
  "Use one immutable version. Cold requests share one load; idle LRU closes before a
   new load. All waits and reservations are bounded; no active native session closes."
  [key loader operation]
  (let [limits
        (limits)

        selection
        (locking lock (allocate! key limits))]

    (if-let [loaded (:load selection)]
      (try (doseq [entry (:evicted selection)]
             (close! entry))
           (let [value (loader)
                 retired? (locking lock
                            (let [entry (get @entries key)]
                              (if (:retire? entry)
                                (do (swap! entries dissoc key) true)
                                (do (swap! entries assoc
                                      key
                                      (assoc entry
                                        :status :ready
                                        :value value))
                                    (deliver loaded {:ok true})
                                    false))))]

             (when retired? (close! {:value value}) (unavailable!))
             (try (operation value) (finally (release! key))))
           (catch Throwable e
             (locking lock
               (when (= :loading (:status (get @entries key)))
                 (swap! entries dissoc key)
                 (deliver loaded {:error e})))
             (throw e)))
      (if-let [waiting (:wait selection)]
        (try (let [outcome (deref waiting (:wait-ms limits) ::timeout)]
               (when (= ::timeout outcome) (busy!))
               (when-let [error (:error outcome)]
                 (throw error))
               (with-resident! key loader operation))
             (finally (locking lock
                        (when (= :loading (:status (get @entries key)))
                          (swap! entries update-in [key :waiters] dec)))))
        (let [value (:value selection)]
          (try (operation value) (finally (release! key))))))))

(defn enable!
  "Allow new allocations after the gateway starts. Old retiring leases are not reused."
  []
  (locking lock (reset! stopping? false)))

(defn shutdown!
  "Retire every session. Idle sessions close now, active leases on final release.
   A load that returns after shutdown is closed without entering the cache."
  []
  (let [idle (locking lock
               (reset! stopping? true)
               (let [old @entries
                     idle (into {}
                                (filter (fn [[_ entry]]
                                          (and (= :ready (:status entry))
                                               (zero? (long (:users entry)))))
                                        old))]

                 (reset! entries (into {}
                                       (for [[key entry] old
                                             :when (not (contains? idle key))]

                                         [key (assoc entry :retire? true)])))
                 (doseq [entry (vals @entries)
                         :when (= :loading (:status entry))]

                   (deliver (:ready entry) {:error (unavailable)}))
                 (vals idle)))]
    (doseq [entry idle]
      (close! entry))
    (count idle)))

(defn release-idle!
  "Evict idle sessions without preventing new allocations."
  []
  (let [old (locking lock
              (let [idle (into {}
                               (filter (fn [[_ entry]]
                                         (and (= :ready (:status entry))
                                              (zero? (long (:users entry)))))
                                       @entries))]
                (swap! entries #(apply dissoc % (keys idle)))
                (vals idle)))]
    (doseq [entry old]
      (close! entry))
    (count old)))

(defn begin-training!
  "Exclusively reserve the decision model memory budget for a CPU trainer.
   Reject active inference or loads; release only idle sessions before starting."
  []
  (let [idle (locking lock
               (when @stopping? (unavailable!))
               (when (or @training?
                         (some (fn [entry]
                                 (or (pos? (long (:users entry)))
                                     (= :loading (:status entry))
                                     (pos? (long (:waiters entry)))))
                               (vals @entries)))
                 (busy!))
               (reset! training? true)
               (let [old (vals @entries)]
                 (reset! entries {})
                 old))]
    (try (doseq [entry idle]
           (close! entry))
         true
         (catch Throwable t (locking lock (reset! training? false)) (throw t)))))

(defn end-training!
  "Release the exclusive trainer reservation, including after cancellation."
  []
  (locking lock (reset! training? false)))
