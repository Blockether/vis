(ns com.blockether.vis.internal.observation-projection
  "Deterministic observation/evidence projection over persisted form envelopes.

   This namespace is pure. Raw payloads stay in `session_turn_iteration.forms`;
   these functions produce compact index rows and model-facing summaries."
  (:require
   [clojure.string :as str]))

(def ^:private call-head-re #"^\s*([A-Za-z_][A-Za-z0-9_]*)\s*\(")

(defn- normalize-src [src]
  (some-> src str str/trim (str/replace #"\s+" " ")))

(defn- head-name [src]
  (some->> (re-find call-head-re (str src)) second))

(defn- request-meta [form]
  (let [request (:request form)]
    (when (map? request)
      {:request-id (or (:request-id form)
                     (:request_id request)
                     (:request-id request)
                     (:id request))
       :request-mode (or (:mode request) "read")
       :request-purpose (:purpose request)})))

(defn- result-lines [result]
  (cond
    (vector? (:lines result)) (:lines result)
    (seq (:ranges result))   (mapcat :lines (:ranges result))
    :else                    []))

(defn- line-nos [result]
  (keep (fn [line]
          (when (and (sequential? line) (number? (first line)))
            (long (first line))))
    (result-lines result)))

(defn- observed-range [result]
  (or (:range result)
    (when-let [xs (seq (line-nos result))]
      [(apply min xs) (apply max xs)])))

(defn- covers-range?
  [prior current]
  (let [ps (:range-start prior)
        pe (:range-end prior)
        cs (:range-start current)
        ce (:range-end current)]
    (and ps pe cs ce
      (<= (long ps) (long cs))
      (>= (long pe) (long ce)))))

(defn- same-file-version?
  [a b]
  (and (= (:path a) (:path b))
    (= (:mtime a) (:mtime b))
    (= (:size a) (:size b))
    (not (:stale? a))))

(defn- useful-cat-result? [result]
  (and (map? result)
    (string? (:path result))
    (seq (result-lines result))))

(defn- cat-event [form idx]
  (let [result (:result form)
        [rs re] (observed-range result)
        nlines (count (result-lines result))]
    (cond-> {:form-scope     (:scope form)
             :form-index     (inc (long idx))
             :op             "cat"
             :path           (:path result)
             :range-start    rs
             :range-end      re
             :mtime          (:mtime result)
             :size           (:size result)
             :line-count     nlines
             :payload-scope  (:scope form)
             :result-summary (str (:path result)
                               (when (and rs re) (str " lines " rs ".." re))
                               " (" nlines " line" (when (not= 1 nlines) "s") ")")}
      (:hashes result) (assoc :has-hashes? true)
      (:request form) (merge (request-meta form)))))

(defn- attach-cat-repeat [prior event]
  (if-let [cover (some (fn [p]
                         (when (and (= "cat" (:op p))
                                 (same-file-version? p event)
                                 (covers-range? p event))
                           p))
                   prior)]
    (assoc event
      :repeat? true
      :covered-by-scope (:payload-scope cover)
      :repeat-of-scope (:payload-scope cover)
      :result-summary (str (:result-summary event)
                        " - already covered by " (:payload-scope cover)))
    event))

(defn- rg-result? [result]
  (and (map? result)
    (or (contains? result :hits)
      (contains? result :matches)
      (contains? result :total_matches)
      (contains? result :hit_count))))

(defn- rg-hit-count [result]
  (or (:hit_count result)
    (:total_matches result)
    (count (:hits result))
    (count (:matches result))
    0))

(defn- rg-file-count [result]
  (or (:file_count result)
    (count (set (keep :path (or (:hits result) (:matches result)))))
    0))

(defn- first-hit [result]
  (when-let [h (first (or (:hits result) (:matches result)))]
    (cond-> {}
      (:path h) (assoc :path (:path h))
      (:line h) (assoc :line (:line h))
      (:text h) (assoc :text (let [s (str (:text h))]
                               (if (> (count s) 120) (str (subs s 0 120) "...") s))))))

(defn- rg-event [form idx]
  (let [src (normalize-src (:src form))
        result (:result form)
        fingerprint (str "rg:" src)
        hit-count (long (rg-hit-count result))
        file-count (long (rg-file-count result))]
    (cond-> {:form-scope     (:scope form)
             :form-index     (inc (long idx))
             :op             "rg"
             :query          src
             :fingerprint    fingerprint
             :payload-scope  (:scope form)
             :result-summary (str "rg " hit-count " hit"
                               (when (not= 1 hit-count) "s")
                               " in " file-count " file"
                               (when (not= 1 file-count) "s"))}
      (first-hit result) (assoc :first-hit (first-hit result))
      (:request form) (merge (request-meta form)))))

(defn- attach-rg-repeat [prior event]
  (if-let [prev (some (fn [p]
                        (when (and (= "rg" (:op p))
                                (= (:fingerprint p) (:fingerprint event)))
                          p))
                  prior)]
    (assoc event
      :repeat? true
      :repeat-of-scope (:payload-scope prev)
      :covered-by-scope (:payload-scope prev)
      :result-summary (str (:result-summary event)
                        " - repeat of " (:payload-scope prev)))
    event))

(defn- path-values [value]
  (letfn [(walk [v]
            (cond
              (map? v) (concat
                         (when (string? (:path v)) [(:path v)])
                         (when (string? (get v "path")) [(get v "path")])
                         (mapcat walk (vals v)))
              (and (sequential? v) (not (string? v))) (mapcat walk v)
              :else []))]
    (->> (walk value) distinct vec)))

(defn- mutation-event [form idx]
  (let [paths (path-values (:result form))]
    (when (seq paths)
      {:form-scope     (:scope form)
       :form-index     (inc (long idx))
       :op             (or (some-> (:src form) head-name) "mutation")
       :fingerprint    (str "mutation:" (normalize-src (:src form)))
       :path           (first paths)
       :affected-paths paths
       :payload-scope  (:scope form)
       :result-summary (str "mutated " (str/join ", " paths))
       :request-id     (:request-id (request-meta form))
       :request-mode   (:request-mode (request-meta form))
       :request-purpose (:request-purpose (request-meta form))})))

(defn- request-event [form idx]
  (when (:request form)
    (let [{:keys [request-id request-mode request-purpose]} (request-meta form)
          op (or (head-name (:src form)) "request")
          failed? (some? (:error form))
          summary (if failed?
                    (str op " request failed: "
                      (or (get-in form [:error :message]) (:error form)))
                    (str op " request observed"))]
      {:form-scope      (:scope form)
       :form-index      (inc (long idx))
       :op              op
       :fingerprint     (str "request:" request-id ":" op ":" (normalize-src (:src form)))
       :payload-scope   (:scope form)
       :result-summary  summary
       :request-id      request-id
       :request-mode    request-mode
       :request-purpose request-purpose
       :failed?         failed?})))

(defn- fingerprint [event]
  (or (:fingerprint event)
    (str (:op event) ":" (:path event) ":" (:range-start event) ":" (:range-end event)
      ":" (:mtime event) ":" (:size event))))

(defn observation-events
  "Project deterministic observation rows from form envelopes.

   `prior-events` are prior persisted observation maps using the facade shape.
   The returned maps are ready for persistence except for ids/iteration FKs."
  [forms prior-events]
  (loop [idx 0
         remaining (vec (or forms []))
         visible (vec (or prior-events []))
         out []]
    (if-let [form (first remaining)]
      (let [head (head-name (:src form))
            event (cond
                    (and (= "cat" head) (useful-cat-result? (:result form)))
                    (attach-cat-repeat visible (cat-event form idx))

                    (and (= "rg" head) (rg-result? (:result form)))
                    (attach-rg-repeat visible (rg-event form idx))

                    (= :mutation (:tag form))
                    (mutation-event form idx)

                    (:request form)
                    (request-event form idx)

                    :else nil)
            event (some-> event (assoc :fingerprint (fingerprint event)))]
        (recur (inc idx)
          (subvec remaining 1)
          (cond-> visible event (conj event))
          (cond-> out event (conj event))))
      out)))

(defn- compact-value [v]
  (let [s (if (string? v) v (pr-str v))]
    (if (> (count s) 240)
      (str (subs s 0 240) "...<truncated>")
      s)))

(defn evidence-events
  "Project compact evidence rows from DAG public receipts in form results."
  [forms observation-events]
  (let [obs-by-path (group-by :path observation-events)]
    (vec
      (mapcat
        (fn [form]
          (for [e (:resolved_evidence (:result form))
                :let [value (:value e)
                      paths (path-values value)
                      linked (->> paths
                               (mapcat #(get obs-by-path %))
                               (keep :id)
                               distinct
                               vec)
                      explicit (or (:observation_ids e) (:observation-ids e))]]
            {:task-key        (:task e)
             :evidence-id     (:id e)
             :evidence-kind   (:kind e)
             :status          (:status e)
             :payload-scope   (:scope form)
             :summary         (compact-value value)
             :observation-ids (vec (distinct (concat explicit linked)))}))
        (or forms [])))))

(defn affected-paths [events]
  (->> events (mapcat :affected-paths) distinct vec))

(defn observation-index
  "Compact model-facing observation index."
  [events]
  (let [events (vec (or events []))
        files  (->> events
                 (filter #(= "cat" (:op %)))
                 (mapv #(cond-> {:path (:path %)
                                 :scope (:payload-scope %)
                                 :range [(:range-start %) (:range-end %)]
                                 :summary (:result-summary %)}
                          (:stale? %) (assoc :stale true)
                          (:covered-by-scope %) (assoc :covered_by (:covered-by-scope %)))))
        searches (->> events
                   (filter #(= "rg" (:op %)))
                   (mapv #(cond-> {:scope (:payload-scope %)
                                   :fingerprint (:fingerprint %)
                                   :query (:query %)
                                   :summary (:result-summary %)}
                            (:covered-by-scope %) (assoc :repeat_of (:covered-by-scope %)))))
        repeats (->> events
                  (filter :repeat?)
                  (mapv #(cond-> {:scope (:payload-scope %)
                                  :already_covered_by (:covered-by-scope %)
                                  :summary (:result-summary %)}
                           (:path %) (assoc :path (:path %)))))
        stale (->> events
                (filter :stale?)
                (mapv #(select-keys % [:path :payload-scope :result-summary])))]
    (cond-> {}
      (seq files) (assoc :files files)
      (seq searches) (assoc :searches searches)
      (seq (filter :request-id events))
      (assoc :requests
        (mapv #(select-keys % [:request-id :request-mode :request-purpose
                               :op :payload-scope :result-summary :failed?])
          (filter :request-id events)))
      (seq repeats) (assoc :repeats repeats)
      (seq stale) (assoc :stale stale))))

(defn evidence-index [events]
  (->> (or events [])
    (group-by :task-key)
    (into {}
      (map (fn [[task rows]]
             [task (mapv (fn [r]
                           (cond-> {:id (:evidence-id r)
                                    :kind (:evidence-kind r)
                                    :status (:status r)
                                    :scope (:payload-scope r)
                                    :summary (:summary r)}
                             (seq (:observation-ids r))
                             (assoc :observations (:observation-ids r))))
                     rows)])))))
