(ns com.blockether.vis.internal.activity.digest
  "Settled-turn Activity digest: one engine-composed receipt for a finished turn.

   Pure. Reads the turn's complete projections in engine spelling and invocation order
   and answers the contract `digest`: what changed, what was checked, what reached
   outside the workspace and which outcomes still need the reader. A later invocation
   that settles the same outcome supersedes an earlier one, so a failure the agent
   resolved counts as a retry instead of an open problem. Clients render `:summary`
   verbatim."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.activity :as activity-contract]
            [com.blockether.vis.internal.activity.event :as event]))

(def ^:private attention-signals
  "Signals whose unresolved outcome stays in view: changes, checks and effects outside
   the workspace. A failed read is ordinary exploration."
  #{"mutation" "verification" "external"})

(def ^:private unsettled-states #{"failed" "cancelled" "running"})

(defn- handle-group?
  [row]
  (boolean (and (seq (:children row)) (or (:handle-id row) (= "shell" (:operation row))))))

(defn- projection-units
  "Invocations of one projection in order. An observation group expands to its members;
   a live handle with its follow-up calls stays one operation."
  [projection]
  (into []
        (mapcat #(if (and (seq (:children %)) (not (handle-group? %))) (:children %) [%]))
        (sort-by :sequence (:rows projection))))

(defn- unit-verdict [unit] (get-in unit [:presentation :verdict]))

(defn- failing? [unit] (or (= "failed" (:state unit)) (= "failed" (unit-verdict unit))))

(defn- passing? [unit] (and (= "succeeded" (:state unit)) (not (failing? unit))))

(defn- supersedes?
  "Whether `later` settles the outcome `unit` reported: the latest run of a check, a later
   call covering the same resources, or any later call of the operation when `unit` named
   no resource. A failed call usually names none, so its retry settles it."
  [unit later]
  (and (= (:operation unit) (:operation later))
       (or (= "verification" (:signal unit))
           (empty? (:resources unit))
           (every? (set (:resources later)) (:resources unit)))))

(defn- settlements
  "Index -> `{:final? :recovered?}` for every unit. A unit is final when no later unit
   supersedes it and recovered when a superseding unit passed."
  [units]
  (into {}
        (mapcat (fn [indexed]
                  (map-indexed (fn [position [index unit]]
                                 (let [later (keep (fn [[_ candidate]]
                                                     (when (supersedes? unit candidate) candidate))
                                                   (subvec indexed (inc position)))]
                                   [index
                                    {:final? (empty? later)
                                     :recovered? (boolean (some passing? later))}]))
                               indexed)))
        (vals (group-by (comp :operation second) (map-indexed vector units)))))

(defn- text [value] (when-not (str/blank? value) value))

(defn- brief
  "First nonblank line of `value`, bounded like a presentation summary."
  [value]
  (if-let [line (first (keep (comp text str/trim) (str/split-lines (str value))))]
    (event/bounded-text line activity-contract/summary-byte-limit)
    ""))

(defn- unit-summary
  [unit]
  (brief (or (when (failing? unit) (text (:error-summary unit)))
             (text (get-in unit [:presentation :summary]))
             (text (:result-summary unit))
             (:summary unit))))

(defn- operation-group
  [operation members]
  (let [latest
        (peek members)

        verdict
        (unit-verdict latest)]

    (cond-> {:operation operation
             :label (activity-contract/operation-label operation members)
             :signal (:signal (first members))
             :count (count members)
             :failed (count (filter failing? members))
             :state (:state latest)
             :summary (unit-summary latest)}
      verdict
      (assoc :verdict verdict))))

(defn- changes
  "File changes the turn's diffs report, or nil when it changed no file."
  [units]
  (let [diffs (filter #(= "diff" (:kind %)) (mapcat :evidence units))]
    (when (seq diffs)
      {:files (count (distinct (map :text diffs)))
       :additions (reduce + 0 (map #(long (or (:additions %) 0)) diffs))
       :deletions (reduce + 0 (map #(long (or (:deletions %) 0)) diffs))})))

(defn- counted [n singular plural] (str n " " (if (= 1 n) singular plural)))

(defn- summary-line
  [{:keys [mutations changes observations checks failing-checks checks-passing? external retries]}]
  (str/join " · "
            (cond-> [(counted mutations "mutation" "mutations")]
              changes
              (conj (str (counted (:files changes) "file" "files")
                         " +" (:additions changes)
                         " \u2212" (:deletions changes)))

              (pos? (long observations))
              (conj (counted observations "observation" "observations"))

              (pos? (long checks))
              (conj (str (counted checks "check" "checks")
                         (cond (pos? (long failing-checks)) (str ", " failing-checks " failing")
                               checks-passing? ", passing"
                               :else "")))

              (pos? (long external))
              (conj (counted external "external action" "external actions"))

              (pos? (long retries))
              (conj (counted retries "retry" "retries")))))

(defn digest
  "Digest of one settled turn from its `projections` in engine spelling and invocation
   order, or nil when the turn ran no operation."
  [projections]
  (let [units
        (into [] (mapcat projection-units) projections)

        omitted
        (keep :omitted projections)

        omitted-rows
        (long (reduce + 0 (keep :rows omitted)))

        operations
        (+ (count units) omitted-rows)]

    (when (pos? operations)
      (let [settled
            (settlements units)

            final-units
            (keep-indexed (fn [index unit]
                            (when (:final? (settled index)) unit))
                          units)

            signal-count
            (fn [signal]
              (+ (count (filter #(= signal (:signal %)) units))
                 (reduce + 0 (keep #(get-in % [:by-classification (keyword signal)]) omitted))))

            final-checks
            (filter #(= "verification" (:signal %)) final-units)

            attention
            (filterv #(and (attention-signals (:signal %))
                           (or (failing? %) (unsettled-states (:state %))))
              final-units)

            retries
            (count (keep-indexed (fn [index unit]
                                   (when (and (attention-signals (:signal unit))
                                              (failing? unit)
                                              (:recovered? (settled index)))
                                     unit))
                                 units))

            grouped
            (group-by :operation units)

            change-totals
            (changes units)]

        (cond-> {:summary (summary-line {:mutations (signal-count "mutation")
                                         :changes change-totals
                                         :observations (signal-count "observation")
                                         :checks (signal-count "verification")
                                         :failing-checks (count (filter failing? final-checks))
                                         :checks-passing? (and (seq final-checks)
                                                               (every? passing? final-checks))
                                         :external (signal-count "external")
                                         :retries retries})
                 :operations operations
                 :retries retries
                 :groups (into []
                               (comp (map #(operation-group % (get grouped %)))
                                     (take activity-contract/digest-group-limit))
                               (distinct (map :operation units)))
                 :attention (vec (take-last activity-contract/digest-attention-limit attention))
                 :attention-total (count attention)
                 :omitted omitted-rows}
          change-totals
          (assoc :changes change-totals))))))
