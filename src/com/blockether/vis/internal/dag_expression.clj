(ns com.blockether.vis.internal.dag-expression
  "Single-expression transaction protocol for the experimental DAG loop.

   Svar transports one raw Python reply. This namespace owns the stronger Vis
   contract: the reply is one `advance({...})` expression whose payload is a
   literal transaction. Tool work is requested through literal descriptors and
   executed by the host before the graph transaction is accepted."
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [com.blockether.vis.internal.ctx-engine :as ctx-engine]
   [com.blockether.vis.internal.env-python :as env]))

(def ^:private max-entity-updates 128)
(def ^:private max-evidence-chars 2000)
(def ^:private hard-warning-codes
  #{:depends_on_cycle :malformed-scope})
(def ^:private forbidden-nested-calls
  #{"done" "fact_set" "plan_step" "settle" "summarize" "update_plan"})

(def ^:private answer-literal-keys
  ["answer" "prose" "answer_template"])

(def ^:private advance-root-keys
  #{"base" "intent" "graph" "requests" "citations" "evidence_proposals"
    "answer" "prose" "answer_template" "finalization"})

(def ^:private graph-root-keys
  #{:tasks :facts})

(def ^:private request-modes
  #{"read" "verify" "write"})

(def ^:private tool-name-pattern
  #"[A-Za-z_][A-Za-z0-9_]*")

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

(defn source-error
  "Return a model-facing error unless `source` is exactly one top-level
   Python call expression headed by `advance` with a literal transaction.
   Syntax errors return nil here and flow through the normal Python error
   mapper with line/column detail."
  [source]
  (try
    (let [form-count (env/count-top-level-forms source)
          head       (env/single-call-expression-head source)
          calls      (env/direct-call-names source)
          nested-calls (seq (sort (remove #{"advance"} calls)))
          forbidden  (seq (sort (filter forbidden-nested-calls nested-calls)))
          root-keys   (set (env/advance-root-string-keys source))
          root-strings (env/advance-root-literal-string-values
                         source answer-literal-keys)
          unknown-root-keys (seq (sort (remove advance-root-keys root-keys)))
          non-literal-answer-keys (seq (env/advance-literal-string-key-errors
                                         source answer-literal-keys))
          answer-template-error (some-> (get root-strings :answer_template)
                                  answer-template-error)
          answer-field-conflict? (and (contains? root-keys "answer_template")
                                   (or (contains? root-keys "answer")
                                     (contains? root-keys "prose")))]
      (cond
        ;; --- Advance validation ---
        (not= 1 form-count)
        (str "Advance protocol requires exactly one top-level Python expression; got "
          form-count ". Return one advance({...}) call.")

        (not= "advance" head)
        "Advance protocol requires the single expression to be advance({...}). Put tool work in literal `requests`, not bare sandbox calls."

        (not= 1 (count (filter #{"advance"} calls)))
        "Advance protocol allows exactly one advance call, at the root of the expression."

        nested-calls
        (str "Advance protocol forbids executable calls inside advance: "
          (str/join ", " nested-calls)
          ". Put tool work in literal `requests` descriptors.")

        forbidden
        (str "Advance protocol forbids graph/control calls inside advance: "
          (str/join ", " forbidden)
          ". Put graph changes in `graph.tasks` / `graph.facts` and tool work in `requests`.")

        unknown-root-keys
        (str "Advance protocol payload contains unknown top-level keys: "
          (str/join ", " (map #(str "`" % "`") unknown-root-keys))
          ". Accepted keys are: "
          (str/join ", " (map #(str "`" % "`") (sort advance-root-keys)))
          ".")

        non-literal-answer-keys
        (str "Advance protocol requires literal strings for "
          (str/join ", " (map #(str "`" % "`") non-literal-answer-keys))
          ". Put tool work in `requests`, then refer to observations with `answer_template`.")

        answer-field-conflict?
        "Advance protocol accepts either `answer`/`prose` or `answer_template`, not both."

        answer-template-error
        answer-template-error

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

(defn- graph-map
  [value]
  (cond
    (nil? value) {}
    (map? value) value
    :else (invalid! "advance graph must be a map" {:field :graph :got (type value)})))

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

(defn- request-id
  [request]
  (or (:request_id request) (:request-id request) (:id request)))

(defn- request-tool
  [request]
  (or (:tool request) (get request "tool")))

(defn- request-mode
  [request]
  (some-> (or (:mode request) (get request "mode")) name))

(defn- request-args
  [request]
  (or (:args request) (get request "args")))

(defn- finalization-done?
  [finalization]
  (cond
    (true? finalization) true
    (false? finalization) false
    (nil? finalization) false
    (map? finalization) (or (true? (:done finalization))
                          (true? (:close finalization))
                          (= "done" (some-> (:status finalization) name))
                          (= "final" (some-> (:status finalization) name)))
    :else (invalid! "advance finalization must be a boolean or map"
            {:field :finalization :got (type finalization)})))

(defn- validate-request!
  [idx request]
  (when-not (map? request)
    (invalid! "advance request must be a map"
      {:field :requests :index idx :got (type request)}))
  (let [rid (request-id request)
        tool (request-tool request)
        mode (request-mode request)]
    (when-not (stable-id? rid)
      (invalid! "advance request_id must be a string, keyword, or symbol"
        {:field :requests :index idx :got (type rid)}))
    (when (str/blank? (stable-id-str rid))
      (invalid! "advance request_id must be non-blank"
        {:field :requests :index idx}))
    (when-not (and (string? tool) (re-matches tool-name-pattern tool))
      (invalid! "advance request tool must be a sandbox function name"
        {:field :requests :index idx :tool tool}))
    (when-not (contains? request-modes mode)
      (invalid! "advance request mode must be read, verify, or write"
        {:field :requests :index idx :mode mode}))))

(defn- accepted-evidence-proposal?
  [proposal]
  (and (map? proposal)
    (or (true? (:accepted proposal))
      (true? (:accepted? proposal))
      (= "accepted" (some-> (:status proposal) name)))))

(defn- validate-evidence-proposals!
  [proposals]
  (doseq [[idx proposal] (map-indexed vector proposals)]
    (when (accepted-evidence-proposal? proposal)
      (invalid! "model-authored accepted evidence is rejected"
        {:field :evidence_proposals :index idx}))))

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
   "observations" (into {}
                    (map (fn [obs] [(:request_id obs) obs]))
                    (:observations receipt))
   "citations" (:citations receipt)
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
     {:graph {:tasks {stable-id partial-task-map}
              :facts {stable-id partial-fact-map}}
      :requests [{:request_id stable-id :tool \"cat\" :mode \"read\" :args [...] :purpose \"...\"}]
      :citations [...]
      :evidence_proposals [...]
      :answer <optional literal conversational Markdown — no calls>
      :answer_template <optional host-rendered prose using accepted slots>
      :finalization <optional boolean-or-map — the explicit terminal signal>}

   Authority separation (Goal DAG and Rewrite Authority, status algebra):
   `answer` is prose that accompanies the checkpoint; it does not close the
   turn. Only `finalization` invokes the terminal path, and the host gates it on
   the plan graph and host-owned evidence policy.

   Nested sandbox calls are rejected before this function is invoked.
   `requests` are literal descriptors; Vis executes them synchronously and
   records receipt-backed observations.
   Use `answer_template` to render prose from resolved task/fact/evidence
   slots after the graph transaction is accepted."
  [value]
  (when-not (map? value)
    (invalid! "advance expects one map argument" {:got (type value)}))
  (let [unknown (seq (remove #{:base :intent :graph :requests :citations
                               :evidence_proposals :answer :prose
                               :answer_template :finalization}
                       (keys value)))
        graph   (graph-map (:graph value))
        graph-unknown (seq (remove graph-root-keys (keys graph)))
        tasks   (entity-map :tasks (:tasks graph))
        facts   (entity-map :facts (:facts graph))
        requests (vec (or (:requests value) []))
        citations (vec (or (:citations value) []))
        evidence-proposals (vec (or (:evidence_proposals value) []))
        answer  (or (:answer value) (:prose value))
        answer-template (:answer_template value)
        done    (finalization-done? (:finalization value))]
    (when unknown
      (invalid! "advance contains unknown top-level keys" {:keys (vec unknown)}))
    (when graph-unknown
      (invalid! "advance graph contains unknown keys" {:keys (vec graph-unknown)}))
    (when-not (sequential? (or (:requests value) []))
      (invalid! "advance requests must be a vector/list" {:field :requests}))
    (when-not (sequential? (or (:citations value) []))
      (invalid! "advance citations must be a vector/list" {:field :citations}))
    (when-not (sequential? (or (:evidence_proposals value) []))
      (invalid! "advance evidence_proposals must be a vector/list"
        {:field :evidence_proposals}))
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
    (doseq [[idx request] (map-indexed vector requests)]
      (validate-request! idx request))
    (validate-evidence-proposals! evidence-proposals)
    (when (and (empty? tasks)
            (empty? facts)
            (empty? requests)
            (str/blank? (or answer answer-template "")))
      (invalid! "advance must request work, change graph.tasks/graph.facts, or provide an answer" {}))
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
     :base        (:base value)
     :intent      (:intent value)
     :graph       {:tasks tasks :facts facts}
     :tasks       tasks
     :facts       facts
     :requests    requests
     :citations   citations
     :evidence_proposals evidence-proposals
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
  (vec
    (concat
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
          (sort-by (comp str key) (:tasks value))))
      (map-indexed
        (fn [idx proposal]
          {:id     (str "evidence/proposed/" idx)
           :task   (stable-id-str (or (:task proposal) (:task_id proposal) "unassigned"))
           :kind   (evidence-kind proposal)
           :status "proposed"
           :value  (compact-evidence proposal)
           :observation_ids (vec (or (:observation_ids proposal) []))})
        (:evidence_proposals value)))))

(defn- context-observation-ids
  [ctx]
  (set
    (keep (fn [request]
            (some-> (or (:request-id request) (:request_id request)
                      (get request "request-id") (get request "request_id"))
              str))
      (get-in ctx [:session/observations :requests]))))

(defn- current-observation-ids
  [observations]
  (set (keep #(some-> (:request_id %) str) observations)))

(defn- citation-observation-id
  [citation]
  (when (map? citation)
    (or (:observation citation)
      (:observation_id citation)
      (:observation-id citation)
      (get citation "observation")
      (get citation "observation_id")
      (get citation "observation-id"))))

(defn- proposal-observation-ids
  [proposal]
  (when (map? proposal)
    (or (:observation_ids proposal)
      (:observation-ids proposal)
      (get proposal "observation_ids")
      (get proposal "observation-ids"))))

(defn- validate-observation-references!
  [ctx value observations]
  (let [allowed (set/union (context-observation-ids ctx)
                  (current-observation-ids observations))
        cited (concat
                (keep citation-observation-id (:citations value))
                (mapcat proposal-observation-ids (:evidence_proposals value)))
        unknown (seq (remove #(contains? allowed (str %)) cited))]
    (when unknown
      (invalid!
        (str "advance references unknown observation ids: "
          (str/join ", " (map str unknown))
          ". Only cite observations present in context[\"observations\"] or "
          "the accepted advance receipt; never infer observations from an "
          "errored advance.")
        {:field :citations
         :unknown-observation-ids (vec (map str unknown))
         :allowed-observation-ids (vec (sort allowed))}))))

(defn request-source
  "Render a validated request descriptor as a single sandbox tool call."
  [request]
  (let [tool (request-tool request)
        args (request-args request)
        render-arg env/ctx->python-str
        arg-text (cond
                   (nil? args) ""
                   (and (sequential? args) (not (string? args)))
                   (str/join ", " (map render-arg args))
                   :else (render-arg args))]
    (when-not (and (string? tool) (re-matches tool-name-pattern tool))
      (invalid! "advance request tool must be a sandbox function name"
        {:tool tool}))
    (str tool "(" arg-text ")")))

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
  ([ctx form-scope value]
   (apply-advance ctx form-scope value []))
  ([ctx form-scope value observations]
   (when-not (advance? value)
     (invalid! "expression did not return an advance" {:value value}))
   (validate-observation-references! ctx value observations)
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
                          :requests (:requests value)
                          :observations (vec observations)
                          :citations (:citations value)
                          :evidence_proposals (:evidence_proposals value)
                          :resolved_evidence (resolved-evidence value)
                          :graph_diff (graph-diff ctx current value)}
                 answer (if-let [template (:answer_template value)]
                          (render-answer-template template value receipt)
                          (:answer value))]
             {:ctx current
              :warnings warnings
              :receipt (assoc receipt
                         :answer answer
                         :answered? (not (str/blank? (or answer ""))))})))))))
