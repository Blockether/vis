(ns com.blockether.vis.internal.activity.core
  "Pure bounded reducer from immutable lifecycle events to channel-neutral Activity.

   Wrapper-entry sequence owns row placement and terminal events update rows in place."
  (:require [com.blockether.vis.contract.activity :as contract]
            [com.blockether.vis.internal.activity.event :as event]
            [com.blockether.vis.internal.activity.presenter :as presenter]
            [com.blockether.vis.contract.wire :as wire]))

(def max-rows (get contract/limits "max_rows"))

(def max-receipt-bytes (get contract/limits "max_receipt_bytes"))

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
  "Who leaves first when the receipt is full, highest number first. A failure or a
   cancellation is the one row a reader must never lose, and a step still running is
   the picture being watched right now: settled routine rows go first, then
   verification, then what changed files, and only then the running step."
  ^long [row]
  (case (:state row)
    :failed
    0

    :cancelled
    1

    :running
    2

    (case (:classification row)
      :mutation
      3

      :verification
      4

      5)))

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
           :group-head (:group-head event)
           :resources (vec (take event/max-resources (:resources event)))
           :evidence (if-let [argument (:argument-summary event)]
                       [{:kind :arguments :text argument}]
                       [])}
    (:summary-format event)
    (assoc :summary-format (:summary-format event))

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
        (vec (take event/max-resources (distinct (concat (:resources row) (:resources event)))))

        summary-evidence
        (when-let [text (not-empty (or (:error-summary event) (:result-summary event)))]
          {:kind (if (:error-summary event) :error :result) :text text})]

    (cond-> (assoc row
              :state state
              :duration-ms (:duration-ms event)
              :resources refs
              :evidence (cond-> (vec (:evidence row))
                          summary-evidence
                          (conj summary-evidence)))
      (seq (:diff-evidence event))
      (update :evidence into (:diff-evidence event))

      (:group-token event)
      (assoc :group-token (:group-token event))

      (:result-summary event)
      (assoc :result-summary (:result-summary event))

      (:result-format event)
      (assoc :result-format (:result-format event))

      ;; an error head is literal machine text: whatever the row's own words were to be read
      ;; as, the failure that replaced them is never markdown.
      (:error-summary event)
      (-> (assoc :error-summary (:error-summary event)
                 :summary (:error-summary event))
          (dissoc :summary-format))

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

      :content
      (update state
              :rows
              (fn [rows]
                (mapv (fn [row]
                        (if (and (= (:id row) (:invocation-id event)) (= :running (:state row)))
                          (assoc row :presentation (:presentation event))
                          row))
                      rows)))

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

        head
        (:group-head first-row)

        state
        (grouped-state children)]

    (cond-> {;; A head is a ROW, so it needs an id of its own: borrowing its first child's
             ;; id put the same id twice in one tree, and a tree with a duplicate id
             ;; cannot be keyed, expanded or addressed by either surface.
             :id (str "group-" (or (:group-token first-row) (:id first-row)))
             :sequence (:sequence first-row)
             :operation (cond (= kind :shell) :shell
                              ;; A head that names its own act keeps it: "changed 12 files"
                              ;; is a verb the reader knows, where "observations" is a bucket.
                              (:operation head) (:operation head)
                              :else :observations)
             :presenter (if (= kind :shell) :shell :observation)
             :classification (:classification first-row)
             :state state
             :children (vec children)
             :resources (vec (take event/max-resources (distinct (mapcat :resources children))))
             :evidence []
             :summary (cond (= kind :shell) (:summary first-row)
                            (:summary head) (:summary head)
                            :else (str "observations · " (count children) " operations"))
             :duration-ms (reduce (fn [total duration]
                                    (Math/addExact (long total) (long duration)))
                                  0
                                  (keep :duration-ms children))}
      (:summary-format head)
      (assoc :summary-format (:summary-format head))

      (:result-summary head)
      (assoc :result-summary (:result-summary head))

      (:result-format head)
      (assoc :result-format (:result-format head)))))

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
  "The state without row `idx`. A grouped row is several invocations, so dropping one
   omits every child it held: the counts describe invocations, and a `+1` for a group
   of fifteen would let the reader add the picture up to the wrong total."
  [state idx]
  (let [row
        (get (:rows state) idx)

        gone
        (or (seq (:children row)) [row])]

    (reduce (fn [state {:keys [classification]}]
              (note-omitted state classification))
            (update state :rows #(into (subvec % 0 (long idx)) (subvec % (inc (long idx)))))
            gone)))

(defn- deduplicate-results
  "Drop only result evidence already present verbatim in the row's result-summary.
   This saves wire bytes without losing any text or changing the displayed result."
  [row]
  (cond-> (update row
                  :evidence
                  (fn [evidence]
                    (into []
                          (remove #(and (= :result (:kind %)) (= (:text %) (:result-summary row))))
                          evidence)))
    (seq (:children row))
    (update :children #(mapv deduplicate-results %))))

(def ^:private trimmed-text-bytes
  "The words a step keeps once its receipt must shed weight: the head of a result or
   an argument list, enough to recognize the call, never the page it printed."
  160)

(defn- trim-text [text] (event/bounded-text (str text) trimmed-text-bytes))

(defn- marked
  "`after`, flagged `:is-truncated` when it lost something `before` carried."
  [before after]
  (cond-> after
    (not= before after)
    (assoc :is-truncated true)))

(defn- without-presentation-bodies
  "Keep the closed row useful even when the receipt budget must omit its open content."
  [row]
  (cond-> row
    (:presentation row)
    (update :presentation
            (fn [view]
              (let [view (wire/->wire view)]
                (cond-> (assoc view "content" [])
                  (contains? view "sections")
                  (update "sections"
                          #(mapv (fn [section]
                                   (assoc section "content" []))
                                 %))))))))

(defn- shed-bodies
  "The row without its published content and its diff bodies. A patch keeps its file,
   its counts and `:is-truncated`, the way `event/fit-event` cuts one: a hunk that
   stops mid-file reads as the change, so the whole body goes or none of it."
  [row]
  (marked row
          (cond-> (-> row
                      without-presentation-bodies
                      (update :evidence
                              (fn [evidence]
                                (mapv (fn [{:keys [kind lines] :as item}]
                                        (if (and (= :diff kind) (seq lines))
                                          (assoc item
                                            :lines []
                                            :is-truncated true)
                                          item))
                                      evidence))))
            (seq (:children row))
            (update :children #(mapv shed-bodies %)))))

(defn- shed-words
  "The row with its texts cut to `trimmed-text-bytes`. Result evidence leaves whole,
   because `:result-summary` already carries the same words; arguments and errors
   keep their head."
  [row]
  (marked row
          (cond-> (assoc row
                    :evidence (into []
                                    (comp (remove #(= :result (:kind %)))
                                          (map (fn [{:keys [kind text] :as item}]
                                                 (if (= :diff kind)
                                                   item
                                                   (assoc item :text (trim-text text))))))
                                    (:evidence row)))
            (:result-summary row)
            (update :result-summary trim-text)

            (seq (:children row))
            (update :children #(mapv shed-words %)))))

(defn- shed-detail
  "The skeleton: the step's name, state, resources and the head of its outcome, with
   nothing under them."
  [row]
  (marked row
          (cond-> (-> row
                      without-presentation-bodies
                      (assoc :evidence []))
            (:result-summary row)
            (update :result-summary trim-text)

            (:error-summary row)
            (update :error-summary trim-text)

            (seq (:children row))
            (update :children #(mapv shed-detail %)))))

(defn- row-bytes ^long [row] (long (event/utf8-bytes (wire/json-str row))))

(defn- shedding-order
  "Indexes of `rows`, the first to lose detail first: the same ranks `priority` drops
   rows in, but the OLDEST step first within a rank, because the eye is on the newest
   steps and a stale result is the detail a reader misses least."
  [rows]
  (->> rows
       (map-indexed (fn [i row]
                      [i (priority row) (:sequence row)]))
       (sort-by (fn [[_ p sequence]]
                  [(unchecked-negate (long p)) (long sequence)]))
       (mapv first)))

(defn- shed
  "`[rows total]` after `tier` was applied to rows in `order`, one at a time, until
   `total` fits the receipt or every row has taken it. Sizes move by the difference a
   row's own encoding makes, so no pass re-encodes the whole receipt."
  [rows ^long total order tier]
  (loop [rows
         rows

         total
         total

         order
         (seq order)]

    (if (or (<= total (long max-receipt-bytes)) (nil? order))
      [rows total]
      (let [idx
            (first order)

            before
            (get rows idx)

            after
            (tier before)]

        (recur (assoc rows idx after)
               (+ total (- (row-bytes after) (row-bytes before)))
               (next order))))))

(defn bounded
  "Receipt no larger than 64 KiB, paid in DETAIL before steps.

   Twelve settled `grep` calls carry sixty kilobytes of results, and a `shell` loop
   grows one grouped row past the ceiling on its own. Dropping whole rows at that
   point took the fifteen-child shell group AND the running step out of a live
   picture and left `2 more steps` in their place, with nothing a reader could open.
   Remove duplicate result evidence first, without losing any content. Only if that
   still exceeds the limit do we shorten text, shed bodies, then keep skeletons.
   Each pass takes the oldest low-priority rows first; whole steps leave last."
  [state]
  (let [rows
        (projected-rows (:rows state))

        state
        (assoc state :rows rows)

        [rows]
        (reduce (fn [[rows total] tier]
                  (if (<= (long total) (long max-receipt-bytes))
                    (reduced [rows total])
                    (shed rows total (shedding-order rows) tier)))
                [rows (byte-size state)]
                [deduplicate-results shed-words shed-bodies shed-detail])]

    (loop [current (assoc state :rows rows)]
      (if (or (<= (long (byte-size current)) (long max-receipt-bytes)) (empty? (:rows current)))
        current
        (let [idx (or (replaceable-index (:rows current)) (dec (count (:rows current))))]
          (recur (drop-row current idx)))))))

(defn snapshot
  "Bounded running/final projection consumed by both channels."
  [state]
  (bounded state))

(defn- enum-name [value] (if (keyword? value) (name value) (str value)))

(defn- presentation-resource [{:keys [type id]}] {:type (enum-name type) :id (str id)})

(defn- presentation-evidence
  [{:keys [kind text lines additions deletions modifications is-truncated is-redacted]}]
  (cond-> {:kind (enum-name kind) :text (str text)}
    (= :diff kind)
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
      (long (or modifications 0)) :is-truncated
      (boolean is-truncated) :is-redacted
      (boolean is-redacted))))

(defn- presentation-row
  [{:keys [id sequence operation presenter classification state summary group-token resources
           duration-ms result-summary error-summary evidence children is-truncated summary-format
           result-format presentation]}]
  (cond-> {:id (str id)
           :sequence (long sequence)
           :operation (enum-name operation)
           :presenter (enum-name presenter)
           :signal (enum-name classification)
           :state (enum-name state)
           :summary (str (or summary ""))
           :resources (mapv presentation-resource resources)
           :evidence (mapv presentation-evidence evidence)}
    (some? presentation)
    (assoc :presentation presentation)

    group-token
    (assoc :group-token (str group-token))

    summary-format
    (assoc :summary-format (enum-name summary-format))

    duration-ms
    (assoc :duration-ms (long duration-ms))

    result-summary
    (assoc :result-summary (str result-summary))

    result-format
    (assoc :result-format (enum-name result-format))

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
