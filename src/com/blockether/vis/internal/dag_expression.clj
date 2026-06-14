(ns com.blockether.vis.internal.dag-expression
  "Single-expression transaction protocol for the experimental DAG loop.

   Svar transports one raw Python reply. This namespace owns the stronger Vis
   contract: the reply is one `settle({...})` expression whose nested sandbox
   calls have already resolved before the settlement value reaches the host."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.ctx-engine :as ctx-engine]
   [com.blockether.vis.internal.env-python :as env]))

(def ^:private max-entity-updates 128)
(def ^:private max-evidence-chars 2000)
(def ^:private hard-warning-codes
  #{:depends_on_cycle :malformed-scope})
(def ^:private forbidden-nested-calls
  #{"done" "fact_set" "plan_step" "summarize" "update_plan"})

(def ^:private observation-rejection-calls
  ;; Calls that signal a non-observation intent inside a non-settle reply.
  ;; Bare sandbox calls (cat, ls, rg, …) without these are accepted as a
  ;; read-only observation turn — no settlement, no checkpoint, no graph change.
  (conj forbidden-nested-calls "settle" "done"))

(defn source-error
  "Return a model-facing error unless `source` is (a) exactly one top-level
   Python call expression headed by `settle`, or (b) a sequence of bare
   sandbox calls without settle/done/graph-mutations/prose (observation turn).
   Syntax errors return nil here and flow through the normal Python error
   mapper with line/column detail."
  [source]
  (try
    (let [form-count (env/count-top-level-forms source)
          head       (env/single-call-expression-head source)
          calls      (env/direct-call-names source)
          forbidden  (seq (sort (filter forbidden-nested-calls calls)))
          has-settle (contains? calls "settle")
          has-graph  (some observation-rejection-calls calls)
          observation? (and (env/observation-calls-only? source)
                            (not has-settle)
                            (not has-graph))]
      (cond
        ;; --- Observation-only: bare sandbox calls, no settle/graph ops ---
        observation?
        nil

        ;; --- Settlement validation ---
        (not= 1 form-count)
        (str "DAG mode requires exactly one top-level Python expression; got "
          form-count ". Return one settle({...}) call.")

        (not= "settle" head)
        "DAG mode requires the single expression to be settle({...}). Inline sandbox calls inside its evidence values."

        (not= 1 (count (filter #{"settle"} calls)))
        "DAG mode allows exactly one settle call, at the root of the expression."

        forbidden
        (str "DAG mode forbids graph/control calls inside settle: "
          (str/join ", " forbidden)
          ". Put graph changes in the settle payload and only sandbox evidence calls inside values.")

        :else nil))
    (catch Throwable _ nil)))

(defn- invalid!
  [message data]
  (throw (ex-info message (assoc data :type :vis/invalid-settlement))))

(defn- entity-map
  [kind value]
  (cond
    (nil? value) {}
    (map? value) value
    :else (invalid! (str "settle " (name kind) " must be a map keyed by stable id")
            {:field kind :got (type value)})))

(defn- bounded-entities!
  [kind entities]
  (when (> (count entities) max-entity-updates)
    (invalid! "settle transaction is too large"
      {:field kind :count (count entities) :max max-entity-updates})))

(defn- stable-id?
  [value]
  (or (string? value) (keyword? value) (symbol? value)))

(defn- stable-id-str
  [value]
  (if (or (keyword? value) (symbol? value)) (name value) (str value)))

(defn settlement
  "Validate and tag a Python `settle({...})` payload.

   Shape:
     {:tasks  {stable-id partial-task-map}
      :facts  {stable-id partial-fact-map}
      :answer <optional conversational Markdown — does NOT terminate the turn>
      :done   <optional boolean — the sole explicit terminal signal>}

   Authority separation (Goal DAG and Rewrite Authority, status algebra):
   `answer` is prose that accompanies the checkpoint; it does not close the
   turn. Only `done: true` invokes the terminal path, and the host gates it on
   the plan graph (open-plan-steps-block): a :done task needs :evidence.

   Nested sandbox calls belong directly in task/fact values, usually under
   `evidence`; Python evaluates those calls before invoking this function."
  [value]
  (when-not (map? value)
    (invalid! "settle expects one map argument" {:got (type value)}))
  (let [unknown (seq (remove #{:tasks :facts :answer :prose :done} (keys value)))
        tasks   (entity-map :tasks (:tasks value))
        facts   (entity-map :facts (:facts value))
        answer  (or (:answer value) (:prose value))
        done    (:done value)]
    (when unknown
      (invalid! "settle contains unknown top-level keys" {:keys (vec unknown)}))
    (when (and (contains? value :answer) (contains? value :prose))
      (invalid! "settle accepts :answer or :prose, not both" {}))
    (when (and (some? answer) (not (string? answer)))
      (invalid! "settle answer must be a string" {:got (type answer)}))
    (when (and (contains? value :done) (not (boolean? done)))
      (invalid! "settle :done must be a boolean (true to finalize)"
        {:got (type done)}))
    (when (and (empty? tasks) (empty? facts) (str/blank? (or answer "")))
      (invalid! "settle must change a task/fact or provide an answer" {}))
    (bounded-entities! :tasks tasks)
    (bounded-entities! :facts facts)
    (doseq [[kind entities] [[:tasks tasks] [:facts facts]]
            [id partial] entities]
      (when-not (stable-id? id)
        (invalid! "settle entity id must be a string, keyword, or symbol"
          {:field kind :id id :got (type id)}))
      (when (str/blank? (name id))
        (invalid! "settle entity id must be non-blank" {:field kind :id id}))
      (when-not (map? partial)
        (invalid! "settle entity update must be a map"
          {:field kind :id id :got (type partial)})))
    {:vis_settlement true
     :tasks          tasks
     :facts          facts
     :answer         answer
     :done           (boolean done)}))

(defn settlement?
  [value]
  (and (map? value) (true? (:vis_settlement value))))

(defn- compact-evidence
  [value]
  (if (or (nil? value) (string? value))
    value
    (let [rendered (pr-str value)]
      (if (> (count rendered) max-evidence-chars)
        (str (subs rendered 0 max-evidence-chars) "...<truncated>")
        rendered))))

(defn- normalize-partial
  [partial]
  (cond-> partial
    (contains? partial :evidence) (update :evidence compact-evidence)))

(defn- validate-task-tree!
  [ctx]
  (let [tasks (or (:session/tasks ctx) {})]
    (doseq [start (keys tasks)]
      (loop [node start
             path []
             seen #{}]
        (when-let [parent (some-> (get-in tasks [node :parent]) str not-empty)]
          (when-not (contains? tasks parent)
            (invalid! "settle task parent does not exist"
              {:task start :parent parent}))
          (when (contains? seen parent)
            (invalid! "settle task parent relation would introduce a cycle"
              {:task start :cycle (conj path parent)}))
          (recur parent (conj path parent) (conj seen node)))))))

(defn apply-settlement
  "Purely apply all task/fact updates to `ctx`, or throw without exposing a
   partial graph. Workspace effects are committed separately by the caller."
  [ctx form-scope value]
  (when-not (settlement? value)
    (invalid! "expression did not return a settlement" {:value value}))
  (let [ops (concat
              (for [[id partial] (sort-by (comp str key) (:tasks value))]
                [:plan-step! id (normalize-partial partial)])
              (for [[id partial] (sort-by (comp str key) (:facts value))]
                [:fact-set! id (normalize-partial partial)]))]
    (loop [current ctx
           warnings []
           remaining ops]
      (if-let [[mutator id partial] (first remaining)]
        (let [{next-ctx :ctx ws :warnings}
              (ctx-engine/apply-mutator current form-scope mutator [id partial])
              hard (seq (filter #(contains? hard-warning-codes (:code %)) ws))]
          (when hard
            (invalid! "settle graph transaction was rejected"
              {:mutator mutator :id id :warnings (vec hard)}))
          (recur next-ctx (into warnings ws) (next remaining)))
        (do
          (validate-task-tree! current)
          {:ctx current
           :warnings warnings
           :receipt {:tasks (mapv (comp stable-id-str key)
                              (sort-by (comp str key) (:tasks value)))
                     :facts (mapv (comp stable-id-str key)
                              (sort-by (comp str key) (:facts value)))
                     :answered? (not (str/blank? (or (:answer value) "")))}})))))
