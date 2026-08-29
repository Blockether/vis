(ns com.blockether.vis.internal.activity
  "Pure bounded reducer from immutable lifecycle events to channel-neutral Activity.

   Wrapper-entry sequence owns row placement and terminal events update rows in place."
  (:require [com.blockether.vis.internal.activity.event :as event]
            [com.blockether.vis.internal.activity.presenter :as presenter]
            [com.blockether.vis.internal.gateway.wire :as wire]))

(def max-rows 128)
(def max-receipt-bytes (* 64 1024))
(def empty-state
  "Initial rendered Activity state.

   Ownerless: the form that contains this snapshot is its only identity, so no
   interaction, iteration, form, tool-call or view coordinate appears inside it."
  {:state :idle
   :rows []
   :counts {:running 0 :succeeded 0 :failed 0 :cancelled 0}
   :omitted {:rows 0 :by-classification {}}})

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
  (cond-> {:id (:invocation-id event)
           :sequence (:invocation-sequence event)
           :operation (:operation event)
           :presenter (presenter/presenter-for (:operation event) (:presenter event))
           :classification (presenter/classification event)
           :state :running
           :summary (presenter/row-summary event)
           :group-token (:group-token event)
           :resources (vec (take event/max-resources (:resources event)))
           :evidence [{:kind :arguments :text (:argument-summary event)}]}
    (:argument-truncated event)
    (assoc :is-truncated true)))

(defn- note-omitted
  [state classification]
  (-> state
      (update-in [:omitted :rows] (fnil inc 0))
      (update-in [:omitted :by-classification classification] (fnil inc 0))))

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
      (-> state
          (update :counts update :running (fnil inc 0))
          (note-omitted (:classification row))))))

(defn- terminal-row
  [row event]
  (let [state
        (terminal-state event)

        refs
        (vec (take event/max-resources (distinct (concat (:resources row) (:resources event)))))]

    (cond-> (assoc row
              :state state
              :duration-ms (:duration-ms event)
              :resources refs
              :evidence (conj (vec (:evidence row))
                              {:kind (if (:error-summary event) :error :result)
                               :text (or (:error-summary event) (:result-summary event) "")}))
      (:diff-evidence event)
      (update :evidence conj (:diff-evidence event))

      (:group-token event)
      (assoc :group-token (:group-token event))

      (:result-summary event)
      (assoc :result-summary (:result-summary event))

      (:error-summary event)
      (assoc :error-summary
        (:error-summary event) :summary
        (:error-summary event))

      (:result-truncated event)
      (assoc :is-truncated true))))

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

(defn detected?
  "True once at least one invocation was observed.

   A form that ran no tool carries no `:activity` at all — an empty panel is not
   the same statement as a form that did nothing."
  [state]
  (boolean (or (seq (:rows state))
               (pos? (long (get-in state [:omitted :rows] 0)))
               (some #(pos? (long %)) (vals (:counts state))))))

(defn replay
  "Reduce a lifecycle event stream into one deterministic snapshot."
  [events]
  (reduce reduce-event empty-state events))

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
             :children (vec children)
             :resources (vec (take
                               event/max-resources
                               (distinct (mapcat :resources children))))
             :evidence []
             :summary (if (= kind :shell)
                        (:summary first-row)
                        (str "observations · " (count children) " operations"))
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
  (let [row (get (:rows state) idx)]
    (-> state
        (update :rows #(into (subvec % 0 (long idx)) (subvec % (inc (long idx)))))
        (note-omitted (:classification row)))))

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

(defn- enum-name [value] (if (keyword? value) (name value) (str value)))

(defn- presentation-resource [{:keys [type id]}] {:type (enum-name type) :id (str id)})

(defn- presentation-evidence
  [{:keys [kind text lines additions deletions modifications omitted-lines is-truncated
           is-redacted]}]
  (cond-> {:kind (enum-name kind) :text (str text)}
    (seq lines)
    (assoc :lines
      (mapv (fn [{:keys [kind text is-redacted]}]
              (cond-> {:kind (enum-name kind) :text (str text)}
                is-redacted
                (assoc :is-redacted true)))
            lines))

    (= :diff kind)
    (assoc :additions
      (long (or additions 0)) :deletions
      (long (or deletions 0)) :modifications
      (long (or modifications 0)) :omitted-lines
      (long (or omitted-lines 0)) :is-truncated
      (boolean is-truncated) :is-redacted
      (boolean is-redacted))))

(defn- presentation-row
  [{:keys [id sequence operation presenter classification state summary group-token resources
           duration-ms result-summary error-summary evidence children is-truncated]}]
  (cond-> {:id (str id)
           :sequence (long sequence)
           :operation (enum-name operation)
           :presenter (enum-name presenter)
           :signal (enum-name classification)
           :state (enum-name state)
           :summary (str (or summary ""))
           :resources (mapv presentation-resource resources)
           :evidence (mapv presentation-evidence evidence)}
    group-token
    (assoc :group-token (str group-token))

    duration-ms
    (assoc :duration-ms (long duration-ms))

    result-summary
    (assoc :result-summary (str result-summary))

    error-summary
    (assoc :error-summary (str error-summary))

    (seq children)
    (assoc :children (mapv presentation-row children))

    is-truncated
    (assoc :is-truncated true)))

(defn presentation
  "Bounded Activity data shared by TUI, Companion, and settled replay.

   It contains semantic values only, and no key naming its owner: the form that
   carries it supplies that. Channel markup stays in each painter, while
   presenter and signal names arrive as strings so cross-process readers never
   infer them from operation names. Every collection is realized here, because
   this value is persisted inside the form and a lazy seq would settle as a
   placeholder instead of the picture the human watched."
  [state]
  (let [state (snapshot state)]
    {:state (enum-name (:state state))
     :counts (:counts state)
     :rows (mapv presentation-row (:rows state))
     :omitted (:omitted state)}))
