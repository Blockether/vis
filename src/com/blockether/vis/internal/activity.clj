(ns com.blockether.vis.internal.activity
  "Pure bounded reducer from immutable lifecycle events to channel-neutral Activity.

   Wrapper-entry sequence owns row placement and terminal events update rows in place."
  (:require [com.blockether.vis.internal.activity.event :as event]
            [com.blockether.vis.internal.activity.presenter :as presenter]
            [com.blockether.vis.internal.gateway.wire :as wire]))

(def max-rows 128)
(def max-receipt-bytes (* 64 1024))
(defn empty-state
  "Initial rendered Activity state."
  [_anchor]
  {:state :idle :rows [] :counts {:running 0 :succeeded 0 :failed 0 :cancelled 0}})

(defn- terminal-state
  [event]
  (cond (:succeeded event) :succeeded
        (:failed event) :failed
        (:cancelled event) :cancelled
        :else nil))

(defn- priority
  ^long [row]
  (case (:state row)
    :failed
    0

    :cancelled
    1

    (case (:classification row)
      :mutation
      2

      :verification
      3

      (if (= :running (:state row)) 4 5))))

(defn- start-row
  [event]
  {:id (:invocation-id event)
   :sequence (:invocation-sequence event)
   :operation (:operation event)
   :presenter (presenter/presenter-for (:operation event) (:presenter event))
   :classification (presenter/classification event)
   :state :running
   :summary (presenter/row-summary event)
   :group-token (:group-token event)
   :resources (vec (take event/max-resources (:resources event)))})

(defn- update-counts
  [counts before after]
  (cond-> counts
    before
    (update before (fnil dec 0))

    after
    (update after (fnil inc 0))))

(defn- replaceable-index
  [rows]
  (->> rows
       (keep-indexed (fn [i row]
                       (when (and (not= :failed (:state row)) (not= :cancelled (:state row)))
                         [i (priority row) (:sequence row)])))
       (sort-by (fn [[_ p sequence]]
                  [(unchecked-negate (long p)) (unchecked-negate (long sequence))]))
       ffirst))

(defn- retain-start
  [state event]
  (let [row (start-row event)]
    (if (< (long (count (:rows state))) (long max-rows))
      (-> state
          (update :rows conj row)
          (update :counts update-counts nil :running))
      (update state :counts update :running (fnil inc 0)))))

(defn- terminal-row
  [row event]
  (let [state
        (terminal-state event)

        refs
        (vec (take event/max-resources (distinct (concat (:resources row) (:resources event)))))]

    (cond-> (assoc row
              :state state
              :duration-ms (:duration-ms event)
              :resources refs)
      (:group-token event)
      (assoc :group-token (:group-token event))

      (:result-summary event)
      (assoc :result-summary (:result-summary event))

      (:error-summary event)
      (assoc :error-summary
        (:error-summary event) :summary
        (:error-summary event)))))

(defn- retain-late-failure
  [state event]
  (if-let [idx (replaceable-index (:rows state))]
    (assoc-in state [:rows idx] (terminal-row (start-row event) event))
    state))

(defn- settle
  [state event]
  (let [id
        (:invocation-id event)

        idx
        (first (keep-indexed #(when (= id (:id %2)) %1) (:rows state)))

        outcome
        (terminal-state event)]

    (cond-> (update state :counts update-counts :running outcome)
      (some? idx)
      (assoc-in [:rows idx] (terminal-row (get (:rows state) idx) event))

      (and (nil? idx) (= :failed outcome))
      (retain-late-failure event))))

(defn reduce-event
  "Apply one validated event. The same ordered input always yields the same state."
  [state raw-event]
  (let [event raw-event]
    (case (:phase event)
      :start
      (-> (retain-start state event)
          (assoc :state :running))

      :terminal
      (let [next (settle state event)]
        (if (pos? (long (get-in next [:counts :running] 0)))
          (assoc next :state :running)
          (assoc next
            :state (cond (pos? (long (get-in next [:counts :failed] 0))) :failed
                         (pos? (long (get-in next [:counts :cancelled] 0))) :cancelled
                         :else :succeeded)))))))

(defn settle-running
  "Finalize every invocation still running when its enclosing evaluation ends.

   `outcome` is `:failed` or `:cancelled`; no synthetic lifecycle event is
   invented. The receipt records that the evaluation, not the tool wrapper,
   supplied the terminal boundary."
  [state outcome summary]
  (let [running (long (get-in state [:counts :running] 0))]
    (if (zero? running)
      state
      (-> state
          (update :rows
                  (fn [rows]
                    (mapv (fn [row]
                            (cond-> row
                              (= :running (:state row))
                              (assoc :state
                                outcome :summary
                                summary)))
                          rows)))
          (assoc :state outcome)
          (assoc-in [:counts :running] 0)
          (update-in [:counts outcome] (fnil + 0) running)))))

(defn replay
  "Reduce a lifecycle event stream into one deterministic snapshot."
  [anchor events]
  (reduce reduce-event (empty-state anchor) events))

(defn byte-size ^long [snapshot] (long (event/utf8-bytes (wire/json-str snapshot))))

(defn- resource-key-of-type
  [row resource-type]
  (some (fn [{:keys [type id]}]
          (when (and (= resource-type type) id) [type id]))
        (:resources row)))

(defn- grouped-state
  [children]
  (cond (some #(= :failed (:state %)) children) :failed
        (some #(= :cancelled (:state %)) children) :cancelled
        (some #(= :running (:state %)) children) :running
        :else :succeeded))

(defn- grouped-row
  [kind children]
  (let [first-row
        (first children)

        state
        (grouped-state children)]

    (cond-> {:id (:id first-row)
             :sequence (:sequence first-row)
             :operation (if (= kind :shell)
                          :shell
                          :observations)
             :presenter (if (= kind :shell)
                          :shell
                          :observation)
             :classification (:classification first-row)
             :state state
             :summary (str
                        (if (= kind :shell) "shell" "observations")
                        " · "

                        (count children)
                        " operations")
             :duration-ms (reduce (fn [total duration]
                                    (Math/addExact (long total) (long duration)))
                            0
                            (keep :duration-ms children))})))

(defn- coalesce-shell-rows
  [rows]
  (let [key-for
        #(when (= :shell (:presenter %)) (resource-key-of-type % :shell-handle))

        frequencies
        (frequencies (keep key-for rows))]

    (loop [remaining
           rows

           emitted
           #{}

           result
           []]

      (if-let [row (first remaining)]
        (let [key (key-for row)]
          (cond (and key (contains? emitted key)) (recur (rest remaining) emitted result)
                (and key (> (long (get frequencies key 0)) 1))
                (let [children (vec (filter #(= key (key-for %)) rows))]
                  (recur (rest remaining)
                         (conj emitted key)
                         (conj result (grouped-row :shell children))))
                :else (recur (rest remaining) emitted (conj result row))))
        result))))

(defn- coalesce-adjacent-observations
  [rows]
  (loop [remaining
         rows

         result
         []]

    (if-let [row (first remaining)]
      (let [token (:group-token row)
            groupable? (and token (= :observation (:presenter row)))
            [siblings tail] (if groupable?
                              (split-with #(and (= :observation (:presenter %))
                                                (= token (:group-token %)))
                                          remaining)
                              [[row] (rest remaining)])]

        (recur tail
               (conj
                 result
                 (if (> (long (count siblings)) 1) (grouped-row :observation (vec siblings)) row))))
      result)))

(defn- projected-rows
  [rows]
  (->> rows
       (sort-by :sequence)
       vec
       coalesce-shell-rows
       coalesce-adjacent-observations
       vec))

(defn- drop-row
  [state idx]
  (update state :rows #(into (subvec % 0 (long idx)) (subvec % (inc (long idx))))))

(defn bounded
  "Receipt no larger than 64 KiB. Low-priority routine rows leave first."
  [state]
  (loop [current (update state :rows projected-rows)]
    (if (or (<= (long (byte-size current)) (long max-receipt-bytes)) (empty? (:rows current)))
      current
      (let [idx (or (replaceable-index (:rows current)) (dec (count (:rows current))))]
        (recur (drop-row current idx))))))

(defn snapshot
  "Bounded running/final projection consumed by both channels."
  [state]
  (bounded state))

(defn- row-tone
  [{:keys [state]}]
  (case state
    :failed
    :error

    :cancelled
    :warn

    :succeeded
    :ok

    :running
    :running

    :idle))

(defn- row-label
  [{:keys [operation summary]}]
  (event/bounded-text (str (name operation) (when summary (str " · " summary)))
                      event/max-summary-bytes))

(defn live-nodes
  "Project a running Activity without paying the receipt's JSON byte check.

   Row count and every field are already bounded during reduction. The whole
   64 KiB receipt is serialized once, by [[settled-live-nodes]], at settlement."
  [state]
  (let [state
        (update state :rows projected-rows)

        counts
        (:counts state)

        status
        (:state state)]

    [{:id "activity-status"
      :type :status
      :text (str (name status)
                 " · "
                 (+ (long (get counts :succeeded 0))
                    (long (get counts :failed 0))
                    (long (get counts :cancelled 0)))
                 " settled · "
                 (get counts :running 0)
                 " running")
      :tone (row-tone {:state status})}
     {:id "activity-counts"
      :type :stat
      :stats (mapv (fn [[id label]]
                     {:id (name id)
                      :label label
                      :value-text (str (get counts id 0))
                      :tone (if (and (= id :failed) (pos? (long (get counts id 0)))) :error :idle)})
                   [[:running "Running"] [:succeeded "Succeeded"] [:failed "Failed"]
                    [:cancelled "Cancelled"]])}
     {:id "activity-rows"
      :type :steps
      :steps (mapv (fn [{:keys [id duration-ms result-summary error-summary] :as row}]
                     (let [detail (or error-summary
                                      result-summary
                                      (when duration-ms (str duration-ms " ms")))]
                       (cond-> {:id id :label (row-label row) :tone (row-tone row)}
                         detail
                         (assoc :detail detail))))
                   (:rows state))}]))

(defn settled-live-nodes
  "Project the final Activity after enforcing its 64 KiB receipt budget once."
  [state]
  (live-nodes (snapshot state)))
