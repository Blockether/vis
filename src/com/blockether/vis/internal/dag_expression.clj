(ns com.blockether.vis.internal.dag-expression
  "Single-expression transaction protocol for the experimental DAG loop.

   Svar transports one raw Python reply. This namespace owns the stronger Vis
   contract: the reply is one `advance({...})` expression whose nested sandbox
   calls have already resolved before the advance value reaches the host."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.ctx-engine :as ctx-engine]
   [com.blockether.vis.internal.env-python :as env]))

(def ^:private max-entity-updates 128)
(def ^:private max-evidence-chars 2000)
(def ^:private hard-warning-codes
  #{:depends_on_cycle :malformed-scope})
(def ^:private forbidden-nested-calls
  #{"done" "fact_set" "plan_step" "settle" "summarize" "update_plan"})

(def ^:private observation-rejection-calls
  ;; Calls that signal a non-observation intent inside a non-advance reply.
  ;; Bare sandbox calls (cat, ls, rg, …) without these are accepted as a
  ;; read-only observation turn — no advance, no checkpoint, no graph change.
  ;; `advance` is root-only and not part of nested graph/control validation, but
  ;; it must still block the observation-only fast path.
  (conj forbidden-nested-calls "advance"))

(def ^:private isolation-required-calls
  #{"sub_loop" "parallel" "sequence" "selector" "retry"})

(def ^:private answer-literal-keys
  ["answer" "prose" "answer_template"])

(def ^:private advance-root-keys
  #{"tasks" "facts" "answer" "prose" "answer_template" "done"})

(def ^:private answer-template-pattern
  #"\{\{\s*([^}|]+?)(?:\s*\|\s*([A-Za-z_][A-Za-z0-9_]*))?\s*\}\}")

(def ^:private answer-template-transforms
  #{"evidence_summary" "git_diff_summary" "git_status_summary" "stdout_summary"})

(def ^:private max-template-value-chars 4000)

(defn- answer-template-error
  [template]
  (let [refs (re-seq answer-template-pattern (or template ""))
        missing-transform (seq (keep (fn [[_ path transform]]
                                       (when (str/blank? (str transform))
                                         (str/trim path)))
                                 refs))
        unknown-transform (seq (keep (fn [[_ _ transform]]
                                       (when (and (not (str/blank? (str transform)))
                                               (not (contains? answer-template-transforms transform)))
                                         transform))
                                 refs))]
    (cond
      missing-transform
      (str "answer_template references require an explicit user-facing summary transform: "
        (str/join ", " missing-transform)
        ". Use one of: "
        (str/join ", " (sort answer-template-transforms))
        ".")

      unknown-transform
      (str "answer_template uses unsupported user-facing transform(s): "
        (str/join ", " (sort (set unknown-transform)))
        ". Use one of: "
        (str/join ", " (sort answer-template-transforms))
        "."))))

(defn logical-source-error
  "Return an error when `source` can escape a logical-only checkpoint through
   a child-agent call. Registered mutation tools are denied dynamically by the
   extension wrapper; these engine-owned coordinators need an explicit check."
  [source]
  (let [calls (set (env/direct-call-names source))
        blocked (seq (sort (filter isolation-required-calls calls)))]
    (when blocked
      (str "This DAG expression requires an isolated workspace checkpoint: "
        (str/join ", " blocked)
        ". Install/enable a backend with :isolated-fork and :rollback capabilities."))))

(defn source-error
  "Return a model-facing error unless `source` is (a) exactly one top-level
   Python call expression headed by `advance`, or (b) a sequence of bare
   sandbox calls without advance/done/graph-mutations/prose (observation turn).
   Syntax errors return nil here and flow through the normal Python error
   mapper with line/column detail."
  [source]
  (try
    (let [form-count (env/count-top-level-forms source)
          head       (env/single-call-expression-head source)
          calls      (env/direct-call-names source)
          forbidden  (seq (sort (filter forbidden-nested-calls calls)))
          root-keys   (set (env/advance-root-string-keys source))
          root-strings (env/advance-root-literal-string-values
                         source answer-literal-keys)
          true-keys (set (env/advance-root-literal-true-keys source))
          unknown-root-keys (seq (sort (remove advance-root-keys root-keys)))
          non-literal-answer-keys (seq (env/advance-literal-string-key-errors
                                         source answer-literal-keys))
          answer-template-error (some-> (get root-strings :answer_template)
                                  answer-template-error)
          answer-field-conflict? (and (contains? root-keys "answer_template")
                                   (or (contains? root-keys "answer")
                                     (contains? root-keys "prose")))
          literal-terminal-answer-with-calls? (and (contains? true-keys "done")
                                                (or (contains? root-keys "answer")
                                                  (contains? root-keys "prose"))
                                                (seq (remove #{"advance"} calls)))
          has-advance (contains? calls "advance")
          has-graph  (some observation-rejection-calls calls)
          observation? (and (env/observation-calls-only? source)
                         (not has-advance)
                         (not has-graph))]
      (cond
        ;; --- Observation-only: bare sandbox calls, no advance/graph ops ---
        observation?
        nil

        ;; --- Advance validation ---
        (not= 1 form-count)
        (str "DAG mode requires exactly one top-level Python expression; got "
          form-count ". Return one advance({...}) call.")

        (not= "advance" head)
        "DAG mode requires the single expression to be advance({...}). Inline sandbox calls inside its evidence values."

        (not= 1 (count (filter #{"advance"} calls)))
        "DAG mode allows exactly one advance call, at the root of the expression."

        forbidden
        (str "DAG mode forbids graph/control calls inside advance: "
          (str/join ", " forbidden)
          ". Put graph changes in the advance payload and only sandbox evidence calls inside values.")

        unknown-root-keys
        (str "DAG mode advance contains unknown top-level keys: "
          (str/join ", " (map #(str "`" % "`") unknown-root-keys))
          ". `no_goal` was removed: every turn must advance the root goal with a task/fact update.")

        non-literal-answer-keys
        (str "DAG mode requires literal strings for "
          (str/join ", " (map #(str "`" % "`") non-literal-answer-keys))
          ". Put tool calls in task/fact evidence, then refer to resolved values with `answer_template`.")

        answer-field-conflict?
        "DAG mode accepts either `answer`/`prose` or `answer_template`, not both."

        answer-template-error
        answer-template-error

        literal-terminal-answer-with-calls?
        "DAG mode cannot terminally answer in literal prose from tool calls produced in the same advance. First advance the evidence without `done`, then answer from accepted slots in the next iteration, or use an approved deterministic `answer_template` summary transform."

        :else nil))
    (catch Throwable _ nil)))

(defn- invalid!
  [message data]
  (throw (ex-info message (assoc data :type :vis/invalid-advance))))

(defn- entity-map
  [kind value]
  (cond
    (nil? value) {}
    (map? value) value
    :else (invalid! (str "advance " (name kind) " must be a map keyed by stable id")
            {:field kind :got (type value)})))

(defn- bounded-entities!
  [kind entities]
  (when (> (count entities) max-entity-updates)
    (invalid! "advance transaction is too large"
      {:field kind :count (count entities) :max max-entity-updates})))

(defn- stable-id?
  [value]
  (or (string? value) (keyword? value) (symbol? value)))

(defn- stable-id-str
  [value]
  (if (or (keyword? value) (symbol? value)) (name value) (str value)))

(defn- lookup-key
  [m k]
  (when (map? m)
    (let [ks (str k)]
      (cond
        (contains? m ks) (get m ks)
        (contains? m (keyword ks)) (get m (keyword ks))
        (contains? m (symbol ks)) (get m (symbol ks))
        :else nil))))

(defn- compact-template-value
  [value]
  (let [rendered (if (string? value) value (pr-str value))]
    (if (> (count rendered) max-template-value-chars)
      (str (subs rendered 0 max-template-value-chars) "...<truncated>")
      rendered)))

(defn- evidence-slot-value
  [value]
  (if (and (map? value) (contains? value :value))
    (:value value)
    value))

(defn- git-diff-summary
  [value]
  (let [value (evidence-slot-value value)
        value (or (lookup-key value "diff") value)]
    (if-not (map? value)
      (compact-template-value value)
      (let [stat (lookup-key value "stat")
            files (or (lookup-key value "files") [])
            file-count (or (lookup-key stat "files") (count files))
            add-count (or (lookup-key stat "add") 0)
            del-count (or (lookup-key stat "del") 0)
            names (->> files
                    (keep #(or (lookup-key % "file") (lookup-key % "path")))
                    (take 8)
                    (str/join ", "))
            more (let [n (- (count files) 8)]
                   (when (pos? n) (str " +" n " more")))]
        (str file-count " file" (when (not= 1 file-count) "s")
          " changed (+" add-count "/-" del-count ")"
          (when (not (str/blank? names))
            (str ": " names more)))))))

(defn- git-status-summary
  [value]
  (let [value (evidence-slot-value value)
        value (or (lookup-key value "status") value)]
    (if-not (map? value)
      (compact-template-value value)
      (let [changes (or (lookup-key value "changes") {})
            branch (lookup-key value "branch")
            modified (count (or (lookup-key changes "modified") []))
            deleted (count (or (lookup-key changes "deleted") []))
            untracked (count (or (lookup-key changes "untracked") []))
            total (+ modified deleted untracked)]
        (if (zero? total)
          (str "Working tree" (when branch (str " on " branch)) " is clean.")
          (str "Working tree" (when branch (str " on " branch)) " has "
            modified " modified, " deleted " deleted, and "
            untracked " untracked path" (when (not= 1 untracked) "s") "."))))))

(defn- stdout-summary
  [value]
  (let [value (evidence-slot-value value)
        stdout (if (map? value) (lookup-key value "stdout") value)
        lines (->> (str/split-lines (str (or stdout "")))
                (remove str/blank?)
                (take 8))]
    (if (seq lines)
      (str/join "\n" lines)
      "Command produced no stdout.")))

(defn- evidence-summary
  [value]
  (let [value (if (and (map? value) (contains? value :value))
                (:value value)
                value)]
    (cond
      (and (map? value)
        (or (lookup-key value "status") (lookup-key value "diff")))
      (str (when (lookup-key value "status")
             (git-status-summary (lookup-key value "status")))
        (when (and (lookup-key value "status") (lookup-key value "diff"))
          " ")
        (when (lookup-key value "diff")
          (str "Diff: " (git-diff-summary (lookup-key value "diff")) ".")))

      (and (map? value) (lookup-key value "stdout"))
      (stdout-summary value)

      :else
      (str "Observed " (cond
                         (map? value) "structured"
                         (sequential? value) "list"
                         :else "text")
        " evidence."))))

(defn- transform-template-value
  [name value]
  (case (or name "text")
    "evidence_summary" (evidence-summary value)
    "git_diff_summary" (git-diff-summary value)
    "git_status_summary" (git-status-summary value)
    "stdout_summary" (stdout-summary value)
    (invalid! "answer_template uses an unknown transform"
      {:transform name :allowed (sort answer-template-transforms)})))

(defn- template-root
  [value receipt]
  {"tasks" (:tasks value)
   "facts" (:facts value)
   "receipt" receipt
   "graph_diff" (:graph_diff receipt)
   "evidence" (into {}
                (map (fn [item]
                       [(str (:task item) "." (last (str/split (:id item) #"/")))
                        item]))
                (:resolved_evidence receipt))})

(defn- template-path-value
  [root path]
  (let [parts (map str/trim (str/split (str path) #"\."))]
    (when (some str/blank? parts)
      (invalid! "answer_template contains an empty path segment" {:path path}))
    (loop [current root
           [part & more] parts]
      (if (nil? part)
        current
        (let [next-value (lookup-key current part)]
          (when (nil? next-value)
            (invalid! "answer_template references an unknown slot"
              {:path path :missing part}))
          (recur next-value more))))))

(defn- render-answer-template
  [template value receipt]
  (str/replace template answer-template-pattern
    (fn [[_ path transform]]
      (transform-template-value transform
        (template-path-value (template-root value receipt) path)))))

(defn advance
  "Validate and tag a Python `advance({...})` payload.

   Shape:
     {:tasks  {stable-id partial-task-map}
      :facts  {stable-id partial-fact-map}
      :answer <optional literal conversational Markdown — no calls>
      :answer_template <optional host-rendered prose using accepted slots>
      :done   <optional boolean — the sole explicit terminal signal>}

   Authority separation (Goal DAG and Rewrite Authority, status algebra):
   `answer` is prose that accompanies the checkpoint; it does not close the
   turn. Only `done: true` invokes the terminal path, and the host gates it on
   the plan graph (open-plan-steps-block): a :done task needs :evidence.

   Nested sandbox calls belong directly in task/fact values, usually under
   `evidence`; Python evaluates those calls before invoking this function.
   Use `answer_template` to render prose from resolved task/fact/evidence
   slots after the graph transaction is accepted."
  [value]
  (when-not (map? value)
    (invalid! "advance expects one map argument" {:got (type value)}))
  (let [unknown (seq (remove #{:tasks :facts :answer :prose :answer_template :done}
                       (keys value)))
        tasks   (entity-map :tasks (:tasks value))
        facts   (entity-map :facts (:facts value))
        answer  (or (:answer value) (:prose value))
        answer-template (:answer_template value)
        done    (:done value)]
    (when unknown
      (invalid! "advance contains unknown top-level keys" {:keys (vec unknown)}))
    (when (and (contains? value :answer) (contains? value :prose))
      (invalid! "advance accepts :answer or :prose, not both" {}))
    (when (and (some? answer) (some? answer-template))
      (invalid! "advance accepts :answer/:prose or :answer_template, not both" {}))
    (when (and (some? answer) (not (string? answer)))
      (invalid! "advance answer must be a string" {:got (type answer)}))
    (when (and (some? answer-template) (not (string? answer-template)))
      (invalid! "advance answer_template must be a string"
        {:got (type answer-template)}))
    (when-let [message (and answer-template (answer-template-error answer-template))]
      (invalid! message {:field :answer_template}))
    (when (and (contains? value :done) (not (boolean? done)))
      (invalid! "advance :done must be a boolean (true to finalize)"
        {:got (type done)}))
    (when (and (empty? tasks)
            (empty? facts)
            (str/blank? (or answer answer-template "")))
      (invalid! "advance must change a task/fact or provide an answer" {}))
    (bounded-entities! :tasks tasks)
    (bounded-entities! :facts facts)
    (doseq [[kind entities] [[:tasks tasks] [:facts facts]]
            [id partial] entities]
      (when-not (stable-id? id)
        (invalid! "advance entity id must be a string, keyword, or symbol"
          {:field kind :id id :got (type id)}))
      (when (str/blank? (name id))
        (invalid! "advance entity id must be non-blank" {:field kind :id id}))
      (when-not (map? partial)
        (invalid! "advance entity update must be a map"
          {:field kind :id id :got (type partial)})))
    {:vis_advance true
     :tasks       tasks
     :facts       facts
     :answer      answer
     :answer_template answer-template
     :done        (boolean done)}))

(defn advance?
  [value]
  (and (map? value) (true? (:vis_advance value))))

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
            (invalid! "advance task parent does not exist"
              {:task start :parent parent}))
          (when (contains? seen parent)
            (invalid! "advance task parent relation would introduce a cycle"
              {:task start :cycle (conj path parent)}))
          (recur parent (conj path parent) (conj seen node)))))))

(defn- evidence-items
  [evidence]
  (cond
    (nil? evidence) []
    (and (sequential? evidence) (not (string? evidence))) evidence
    :else [evidence]))

(defn- evidence-kind
  [value]
  (let [kind (when (map? value)
               (or (:kind value) (:type value)
                 (get value "kind") (get value "type")))]
    (cond
      (keyword? kind) (name kind)
      (some? kind) (str kind)
      :else "observed")))

(defn- evidence-value
  [value]
  (if (map? value)
    (cond
      (contains? value :value) (:value value)
      (contains? value "value") (get value "value")
      :else value)
    value))

(defn- resolved-evidence
  [value]
  (mapv
    (fn [[task-id idx evidence]]
      {:id     (str "evidence/" (stable-id-str task-id) "/" idx)
       :task   (stable-id-str task-id)
       :kind   (evidence-kind evidence)
       :status "observed"
       :value  (compact-evidence (evidence-value evidence))})
    (mapcat
      (fn [[task-id partial]]
        (map-indexed (fn [idx evidence] [task-id idx evidence])
          (evidence-items (:evidence partial))))
      (sort-by (comp str key) (:tasks value)))))

(defn- graph-diff
  [before after value]
  {:tasks
   (into {}
     (for [[id partial] (sort-by (comp str key) (:tasks value))
           :let [sid (stable-id-str id)
                 old-task (get-in before [:session/tasks sid])
                 new-task (get-in after [:session/tasks sid])]]
       [sid (cond-> {}
              (not= (:status old-task) (:status new-task))
              (assoc :status [(:status old-task) (:status new-task)])
              (contains? partial :evidence)
              (assoc :evidence_added true)
              (contains? partial :depends_on)
              (assoc :depends_on (:depends_on new-task))
              (contains? partial :parent)
              (assoc :parent (:parent new-task)))]))
   :facts
   (into {}
     (for [[id partial] (sort-by (comp str key) (:facts value))
           :let [sid (stable-id-str id)
                 old-fact (get-in before [:session/facts sid])
                 new-fact (get-in after [:session/facts sid])]]
       [sid (cond-> {}
              (not= (:status old-fact) (:status new-fact))
              (assoc :status [(:status old-fact) (:status new-fact)])
              (contains? partial :content)
              (assoc :content_changed true)
              (contains? partial :depends_on)
              (assoc :depends_on (:depends_on new-fact))
              (contains? partial :contradicts)
              (assoc :contradicts (:contradicts new-fact)))]))})

(defn apply-advance
  "Purely apply all task/fact updates to `ctx`, or throw without exposing a
   partial graph. Workspace effects are committed separately by the caller."
  [ctx form-scope value]
  (when-not (advance? value)
    (invalid! "expression did not return an advance" {:value value}))
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
            (invalid! "advance graph transaction was rejected"
              {:mutator mutator :id id :warnings (vec hard)}))
          (recur next-ctx (into warnings ws) (next remaining)))
        (do
          (validate-task-tree! current)
          (let [receipt {:tasks (mapv (comp stable-id-str key)
                                  (sort-by (comp str key) (:tasks value)))
                         :facts (mapv (comp stable-id-str key)
                                  (sort-by (comp str key) (:facts value)))
                         :resolved_evidence (resolved-evidence value)
                         :graph_diff (graph-diff ctx current value)}
                answer (if-let [template (:answer_template value)]
                         (render-answer-template template value receipt)
                         (:answer value))]
            {:ctx current
             :warnings warnings
             :receipt (assoc receipt
                        :answer answer
                        :answered? (not (str/blank? (or answer ""))))}))))))
