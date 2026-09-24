(ns com.blockether.vis.internal.activity.core
  "Lossless reducer from immutable lifecycle events to channel-neutral Activity.

   Wrapper-entry sequence owns row placement and terminal events update rows in place."
  (:require [com.blockether.vis.internal.activity.event :as event]
            [com.blockether.vis.internal.activity.presenter :as presenter]
            [com.blockether.vis.contract.wire :as wire]))

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

(defn- start-row
  [event]
  (cond-> {:id (:invocation-id event)
           :sequence (:invocation-sequence event)
           :operation (:operation event)
           :presenter (presenter/presenter-for (:operation event) (:presenter event))
           :classification (presenter/classification event)
           :state :running
           :show-start (not (false? (:show-start event)))
           :extension (:extension event)
           :summary (presenter/row-summary event)
           :group-token (:group-token event)
           :group-head (:group-head event)
           :resources (vec (take event/max-resources (:resources event)))
           :evidence (if-let [argument (:argument-summary event)]
                       [{:kind :arguments :text argument}]
                       [])}
    (:handle-id event)
    (assoc :handle-id (:handle-id event))

    (:presentation event)
    (assoc :presentation (:presentation event))

    (:argument-key event)
    (assoc :argument-key (:argument-key event))

    (:read-key event)
    (assoc :read-key (:read-key event))

    (:summary-format event)
    (assoc :summary-format (:summary-format event))

    (:argument-truncated event)
    (assoc :is-truncated true)))

(defn- update-counts
  [counts before after]
  (cond-> counts
    before
    (update before (fnil dec 0))

    after
    (update after (fnil inc 0))))

(defn- retain-start
  [state event]
  (-> state
      (update :rows conj (start-row event))
      (update :counts update-counts nil :running)))

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

      (:handle-id event)
      (assoc :handle-id (:handle-id event))

      (:presentation event)
      (assoc :presentation (:presentation event))

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
      (assoc-in [:rows idx] (terminal-row (get (:rows state) idx) event)))))

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
                          (cond-> (assoc row :presentation (:presentation event))
                            (:handle-id event)
                            (assoc :handle-id (:handle-id event)))
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

(defn- receipt-presentation
  "Latest authored outcome, with earlier distinct detail and every distinct failure."
  [children]
  (let [current
        (last children)

        current-view
        (:presentation current)

        earlier-views
        (keep :presentation (butlast children))

        latest
        (or current-view
            {"headline" (or (get (last earlier-views) "headline") "Activity")
             "summary" (or (:result-summary current) (:summary current) "")
             "content" []})

        visible
        (set (conj (vec (get latest "content"))
                   {"type" "text" "text" (get latest "summary")}
                   {"type" "text" "text" (:result-summary current)}))

        earlier
        (->> (butlast children)
             (mapcat (fn [{:keys [presentation result-summary]}]
                       (let [content (get presentation "content")]
                         (if (seq content)
                           content
                           (keep (fn [text]
                                   (when (seq text) {"type" "text" "text" text}))
                                 [(get presentation "summary") result-summary])))))
             (remove visible)
             distinct
             vec)

        existing-sections
        (set (get latest "sections"))

        older-sections
        (vec (distinct (remove existing-sections (mapcat #(get % "sections") earlier-views))))

        errors
        (vec (distinct (keep :error-summary children)))

        failed?
        (= :failed (:state current))]

    (cond-> latest
      failed?
      (assoc "headline"
        "Activity failed" "summary"
        (or (:error-summary current) (:summary current)))

      (or (seq earlier) (seq older-sections) (seq errors))
      (update "sections"
              (fnil into [])
              (concat (when (seq earlier)
                        [{"headline" "Earlier details"
                          "summary" (str (count earlier) " distinct items")
                          "content" earlier}])
                      older-sections
                      (when (seq errors)
                        [{"headline" "Errors"
                          "summary" (str (count errors) " distinct errors")
                          "content" (mapv (fn [error]
                                            {"type" "text" "text" error})
                                          errors)}]))))))

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
        (if (= kind :observation) (grouped-state children) (:state (last children)))

        view
        (case kind
          :shell
          (presenter/shell-receipt-presentation children)

          :receipt
          (receipt-presentation children)

          nil)]

    (cond-> {;; A head is a ROW, so it needs an id of its own: borrowing its first child's
             ;; id put the same id twice in one tree, and a tree with a duplicate id
             ;; cannot be keyed, expanded or addressed by either surface.
             :id (str "group-" (or (:group-token first-row) (:id first-row)))
             :sequence (:sequence first-row)
             :operation (cond (= kind :shell) :shell
                              (= kind :receipt) (:operation first-row)
                              ;; Observation heads name their act when one was declared.
                              (:operation head) (:operation head)
                              :else :observations)
             :presenter (if (= kind :observation) :observation (:presenter first-row))
             :classification (:classification first-row)
             :state state
             :children (vec children)
             :extension (:extension first-row)
             :resources (vec (take event/max-resources (distinct (mapcat :resources children))))
             :evidence []
             :summary (cond view (or (get view "summary") (:summary first-row))
                            (:summary head) (:summary head)
                            :else (str "observations · " (count children) " operations"))
             :duration-ms (reduce (fn [total duration]
                                    (Math/addExact (long total) (long duration)))
                                  0
                                  (keep :duration-ms children))}
      view
      (assoc :presentation view)

      (not= kind :observation)
      (assoc :handle-id (:handle-id first-row))

      (and (= kind :shell) (= :shell (:operation first-row)) (:argument-key first-row))
      (assoc :argument-key (:argument-key first-row))

      (:summary-format head)
      (assoc :summary-format (:summary-format head))

      (:result-summary head)
      (assoc :result-summary (:result-summary head))

      (:result-format head)
      (assoc :result-format (:result-format head)))))

(defn- coalesce-handle-rows
  "Group explicit handles once in first-entry order, never by label or argument."
  [rows]
  (let [key-for
        #(when-let [id (:handle-id %)] [(:extension %) id])

        by-handle
        (reduce (fn [groups row]
                  (if-let [key (key-for row)]
                    (update groups key (fnil conj []) row)
                    groups))
                {}
                rows)]

    (loop [remaining
           rows

           emitted
           #{}

           result
           []]

      (if-let [row (first remaining)]
        (let [key (key-for row)
              children (get by-handle key)]

          (cond (and key (contains? emitted key)) (recur (rest remaining) emitted result)
                (and key (next children))
                (recur (rest remaining)
                       (conj emitted key)
                       (conj result
                             (grouped-row
                               (if (every? #(= :shell (:presenter %)) children) :shell :receipt)
                               children)))
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
       (remove #(and (= :running (:state %)) (false? (:show-start %))))
       (sort-by :sequence)
       vec
       coalesce-handle-rows
       coalesce-adjacent-observations
       vec))

(defn snapshot
  "Project every admitted row without shedding history. Transport pages live in storage."
  [state]
  (assoc state :rows (projected-rows (:rows state))))

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
  [{:keys [id sequence operation presenter classification state summary group-token argument-key
           read-key handle-id resources duration-ms result-summary error-summary evidence children
           is-truncated summary-format result-format presentation]}]
  (cond-> {:id (str id)
           :sequence (long sequence)
           :operation (enum-name operation)
           :presenter (enum-name presenter)
           :signal (enum-name classification)
           :state (enum-name state)
           :summary (str (or summary ""))
           :resources (mapv presentation-resource resources)
           :evidence (mapv presentation-evidence evidence)}
    handle-id
    (assoc :handle-id handle-id)

    (some? presentation)
    (assoc :presentation presentation)

    argument-key
    (assoc :argument-key argument-key)

    read-key
    (assoc :read-key read-key)

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
  "Lossless Activity data shared by TUI, Companion, and settled replay.

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
