(ns com.blockether.vis.internal.ctx-engine
  "Engine surface over CTX — a typed, dependency-checked working memory of
   tasks + facts. Entirely pure. Persistence, IO, and the provider live
   elsewhere and call into these fns.

   The engine keeps the graph consistent (cycle-free `:depends_on`, status
   FSM, fact contradictions, GC/TTL) and surfaces STRUCTURAL warnings — but
   it does NOT verify the model's claims. Done is self-asserted:
   `(task-set! :K {:status :done})` is accepted as-is, the engine stamps
   `:done-born`, and there is no proof, validator, or reversion.

   Public surface (all pure):

     (build-indexes ctx)
       → {:dep-graph :rev-deps :task-status :fact-status}

     (classify-scope scope-form cursor form-results)
       → one of :ok :unknown :errored :future-form :future-iter :future-turn :malformed

     (derive-warnings ctx indexes)
       → {:warnings [short-string …]}   sorted, deduped structural warnings

     (apply-mutator ctx form-scope mutator args)
       → {:ctx :warnings :stamped?}            engine never throws on soft;
                                               hard rejects return :ctx unchanged +
                                               :warnings populated + :stamped? false

     (advance-iter ctx form-results-vec)
       → ctx with trailer pin appended and :session/scope advanced

     (enter-turn ctx turn-pos)
       → ctx with bumped :session/turn, reset :session/scope, gc-pass run

     (gc-pass ctx)
       → ctx with terminal-status entries past TTL removed from live tree

   Mutator keywords accepted by `apply-mutator`:
     :task-set! :fact-set!
     :task-depends! :fact-depends!
     :fact-contradicts! :fact-contradicts-remove!

   Hard rejects (engine writes nothing, warnings carry the reason):
     - malformed scope string anywhere
     - depends-on cycle introduced by task-set! / fact-set! / *-depends!
     - partial-overlap trailer-summarize at done-time

   Everything else is a soft hint surfaced via `derive-warnings` as a
   simple `:session/hints` vec of short strings. The engine NEVER refuses
   a write outside the three hard rules above."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [com.blockether.vis.internal.tokens :as tokens]))
;; =============================================================================
;; Scope parsing — deterministic, regex-driven
;; =============================================================================
(def ^:private scope-form-re #"^t([1-9][0-9]*)/i([1-9][0-9]*)/f([1-9][0-9]*)$")
(defn parse-scope-form
  "Parse a `tN/iM/fK` form-scope into `{:turn :iter :form}` or nil if malformed.
   No exceptions — pure value-or-nil."
  [s]
  (when (string? s)
    (when-let [[_ t i f] (re-matches scope-form-re s)]
      {:turn (parse-long t), :iter (parse-long i), :form (parse-long f)})))
(defn malformed-scope?
  "True if `s` is a string but does not parse as `::cs/scope-form`."
  [s]
  (and (string? s) (nil? (parse-scope-form s))))
(defn scope-compare
  "Total order on form-scope strings by (turn, iter, form). Returns int.
   Compares parsed segments; malformed scopes sort before all valid ones to
   make their presence obvious in render."
  [a b]
  (let [pa (parse-scope-form a)
        pb (parse-scope-form b)]
    (cond (and (nil? pa) (nil? pb)) (compare (str a) (str b))
      (nil? pa) -1
      (nil? pb) 1
      :else (let [c1 (compare (:turn pa) (:turn pb))]
              (if (zero? c1)
                (let [c2 (compare (:iter pa) (:iter pb))]
                  (if (zero? c2) (compare (:form pa) (:form pb)) c2))
                c1)))))
;; =============================================================================
;; classify-scope — the central pure fn for proof scope orientation
;; =============================================================================
(defn classify-scope
  "Classify a `::cs/scope-form` string relative to the engine cursor and the
   set of executed forms. Pure, total, no IO.

   Inputs:
     scope        — the scope-form string the model wrote (e.g. on a proof)
     cursor       — `{:turn N :iter M :next-form K}` from `:session/scope`
     form-results — set or map keyed by scope-form strings of executed forms;
                    map values may carry `:error` to detect errored forms

   Output: one keyword. Engine never throws here.

     :malformed    — does not match the regex; engine will hard-reject the write
     :future-turn  — scope.turn > cursor.turn
     :future-iter  — same turn, scope.iter > cursor.iter
     :future-form  — same iter, scope.form >= cursor.next-form
     :unknown      — past-or-current but not in form-results
     :errored      — present in form-results with non-nil :error
     :ok           — present, has a :result (or no :error map entry)"
  [scope cursor form-results]
  (let [p (parse-scope-form scope)]
    (cond (nil? p) :malformed
      (> (:turn p) (:turn cursor)) :future-turn
      (and (= (:turn p) (:turn cursor)) (> (:iter p) (:iter cursor))) :future-iter
      (and (= (:turn p) (:turn cursor))
        (= (:iter p) (:iter cursor))
        (>= (:form p) (:next-form cursor)))
      :future-form
      :else (let [entry (cond (map? form-results) (get form-results scope)
                          (set? form-results) (when (contains? form-results scope) {})
                          :else nil)]
              (cond (nil? entry) :unknown
                (some? (:error entry)) :errored
                :else :ok)))))
;; =============================================================================
;; build-indexes — single source of derived state, used by every other pass
;; =============================================================================
(defn build-indexes
  "Compute the reverse / projection indexes used by the cycle check and
   warning passes. Pure, idempotent, generator-property-testable.

   Returned shape (every value bounded by ctx size — no unbounded recursion):

     {:dep-graph    {[kind id] → #{[kind id] …}}          ; typed depends-on edges
      :rev-deps     {[kind id] → #{[kind id] …}}          ; reverse depends-on
      :task-status  {task-id → status-keyword}
      :fact-status  {fact-id → status-keyword}}            ; :active when omitted

   Engine NEVER mutates ctx — indexes are throwaway computed fresh on demand.
   Cost is O(|tasks|+|facts|) plus the depends-on traversal. Cheap; rebuilt
   each render."
  [ctx]
  (let [tasks (or (:session/tasks ctx) {})
        facts (or (:session/facts ctx) {})
        ;; ---------- Universal :depends_on ---------- dep-graph nodes are typed refs `[:kind
        ;; :K]` where :kind is one of #{:task :fact}. Edges are sets of typed refs. Bare-key
        ;; entries on a task / fact `:depends_on` are treated as same-kind shorthand (`:K` on a
        ;; task → `[:task :K]`). Engine internals + introspection always work in typed shape.
        normalize-ref (fn [default-kind ref]
                        (cond (vector? ref) (let [[kind k] ref]
                                              (when (and (#{:task :fact} kind) (some? k)) [kind k]))
                          (keyword? ref) [default-kind ref]
                          :else nil))
        edges-from (fn [kind partial-deps]
                     (into #{} (keep (partial normalize-ref kind)) (or partial-deps [])))
        dep-graph (as-> {} g
                    (reduce-kv (fn [acc task-id task]
                                 (assoc acc [:task task-id] (edges-from :task (:depends_on task))))
                      g
                      tasks)
                    (reduce-kv (fn [acc fact-id fact]
                                 (assoc acc [:fact fact-id] (edges-from :fact (:depends_on fact))))
                      g
                      facts))
        rev-deps (reduce-kv (fn [acc node deps]
                              (reduce (fn [a d] (update a d (fnil conj #{}) node)) acc deps))
                   {}
                   dep-graph)
        task-status (into {} (map (fn [[k v]] [k (:status v)])) tasks)
        fact-status (into {} (map (fn [[k v]] [k (or (:status v) :active)])) facts)]
    {:dep-graph dep-graph, :rev-deps rev-deps, :task-status task-status, :fact-status fact-status}))
;; =============================================================================
;; depends-on cycle detection — pure DFS, no recursion-on-recursion
;; =============================================================================
(defn depends-on-cycle?
  "True iff dep-graph contains a directed cycle. Pure DFS with white/grey/black
   coloring. Returns the cycle path as a vec when found (or nil)."
  [dep-graph]
  (let [color (atom (zipmap (keys dep-graph) (repeat :white)))]
    (letfn [(visit [node path]
              (cond (= :grey (@color node)) (vec (drop-while #(not= % node) (conj path node)))
                (= :black (@color node)) nil
                :else (do (swap! color assoc node :grey)
                        (or (some (fn [n] (visit n (conj path node)))
                              (sort (or (get dep-graph node) #{})))
                          (do (swap! color assoc node :black) nil)))))]
      (some #(visit % []) (sort (keys dep-graph))))))
;; =============================================================================
;; Status terminal predicates + done-born stamping
;; =============================================================================
(def ^:private task-terminal? #{:done :cancelled :rejected :archived})
(def ^:private fact-terminal? #{:superseded :archived})
(defn- stamp-or-clear-done-born
  "Pure helper: if status is terminal and :done-born absent, stamp it; if
   status is non-terminal and :done-born present, clear it. Idempotent."
  [entry form-scope terminal?]
  (let [terminal-now? (terminal? (:status entry))
        has-stamp? (contains? entry :done-born)]
    (cond (and terminal-now? (not has-stamp?)) (assoc entry :done-born form-scope)
      (and (not terminal-now?) has-stamp?) (dissoc entry :done-born)
      :else entry)))
;; =============================================================================
;; apply-mutator — dispatch + per-mutator handlers
;; =============================================================================
(defn- warn
  "Construct a single warning map. :code is a stable keyword; :anchor is the
   tuple of keys (and indices) addressing the offending entry; :message is a
   human-readable hint surfaced inline by the renderer."
  [code anchor message]
  {:code code, :anchor anchor, :message message})
(defn- normalize-dep-ref
  "Accept a `:depends_on` element and return a typed `[:kind :K]` ref or
   nil when the input is malformed. Bare keys are resolved to
   `default-kind`, matching the same-kind shorthand for
   `(task-set! :T {:depends_on [:other-task]})`."
  [default-kind ref]
  (cond (vector? ref) (let [[kind k] ref] (when (and (#{:task :fact} kind) (some? k)) [kind k]))
    (keyword? ref) [default-kind ref]
    :else nil))
(defn- new-cycle-on-node?
  "Would assigning `:depends_on` `new-deps` to the typed node `[kind k]`
   introduce a cycle in the unified dep-graph?"
  [ctx kind k new-deps]
  (let [normalized (into #{} (keep (partial normalize-dep-ref kind)) (or new-deps []))
        dg (-> (build-indexes ctx)
             :dep-graph
             (assoc [kind k] normalized))]
    (some? (depends-on-cycle? dg))))
(defn- new-cycle?
  "Task cycle check. Delegates to the universal `new-cycle-on-node?` so a
   task `:depends_on [:other-task]` normalizes via `[:task :other-task]`."
  [ctx task-k deps]
  (new-cycle-on-node? ctx :task task-k deps))
(defn entity-id
  "Stable, turn-qualified entity id derived from the birth form-scope.
   `(entity-id \"t3/i2/f1\" :auth)` => `:t3/auth`. The birth turn makes
   the id unique across turns, so reusing a key in a later turn yields a
   DISTINCT id (`:t5/auth`) that can't clobber the earlier one — and
   `recall` always resolves to the exact version. Falls back to the bare
   key when the scope can't be parsed."
  [form-scope k]
  (if-let [{:keys [turn]} (parse-scope-form form-scope)]
    (keyword (str "t" turn "/" (name k)))
    k))
(defn- apply-task-set!
  [ctx form-scope [task-k partial-map]]
  (let [path [:session/tasks task-k]
        existing (get-in ctx path)
        ;; Hook-task idempotent re-emission (D12). Foundation extension
        ;; `:turn.iteration/start` hooks fire every iter; engine treats
        ;; repeat `(task-set! :hook-id {:source :hook …})` against an
        ;; existing hook-task as a silent no-op regardless of the
        ;; existing task's :status (:todo, :doing, :done, :cancelled).
        ;; Resurrection happens NATURALLY: gc-pass archives terminal
        ;; entries past TTL; once archived the entry is gone from live
        ;; ctx and the next hook fire creates a fresh task.
        hook-repeat? (and (= :hook (:source partial-map))
                       (= :hook (:source existing))
                       (or (= (:hook-id partial-map) (:hook-id existing))
                         (and (nil? (:hook-id partial-map)) (= task-k (:hook-id existing)))))]
    (cond
      ;; Hard reject cycle BEFORE writing :depends_on
      (and (contains? partial-map :depends_on) (new-cycle? ctx task-k (:depends_on partial-map)))
      {:ctx ctx,
       :warnings [(warn :depends_on_cycle
                    [task-k]
                    (str "task "
                      task-k
                      " :depends_on "
                      (:depends_on partial-map)
                      " would introduce a cycle; write refused"))],
       :stamped? false}
      hook-repeat? {:ctx ctx, :warnings [], :stamped? false}
      :else (let [merged (cond-> (merge existing partial-map)
                           (nil? existing) (assoc :born form-scope
                                             :id (entity-id form-scope task-k)))
                  stamped (stamp-or-clear-done-born merged form-scope task-terminal?)]
              {:ctx (assoc-in ctx path stamped), :warnings [], :stamped? true}))))
;; =============================================================================
;; update-plan! — the sole model-facing task surface (ordered plan, Codex-style)
;; =============================================================================
(def ^:private plan-status-aliases
  "Normalize a step's status name (Codex + vis spellings) to an internal
   keyword. `:candidate` is a PROPOSAL awaiting user approval — it never runs.
   `:rejected` is a candidate the USER declined (categorical \"user said no\",
   distinct from `:cancelled` = you abandoned a step you no longer need)."
  {"candidate" :candidate "proposed" :candidate
   "pending" :todo "todo" :todo
   "in_progress" :doing "in-progress" :doing "doing" :doing "active" :doing
   "completed" :done "complete" :done "done" :done
   "cancelled" :cancelled "canceled" :cancelled "skip" :cancelled
   "rejected" :rejected "declined" :rejected "no" :rejected})
(defn- normalize-plan-status [s]
  (let [k (cond (keyword? s) (name s) (nil? s) "" :else (str s))]
    (get plan-status-aliases (str/lower-case k) :todo)))
(defn- plan-canonical-key
  "Canonical snake_case STRING key for a plan step — matches what the model sees
   in `context[\"session_tasks\"]` and types back into `plan_step`. nil for blank."
  [s]
  (let [c (-> (str s) str/lower-case (str/replace #"[^a-z0-9]+" "_") (str/replace #"(^_+)|(_+$)" ""))]
    (when-not (str/blank? c) c)))
(defn- plan-step-key [seen idx title]
  (let [base (or (plan-canonical-key title) (str "step_" (inc idx)))]
    (if (contains? seen base) (str base "_" (inc idx)) base)))
(defn- enforce-one-doing
  "Codex invariant on a plan vec `[[k entry]…]`: among ACCEPTED (non-`:candidate`)
   steps keep at most one `:doing` (extras → `:todo`, preferring `prefer-k`), and
   promote the first accepted `:todo` to `:doing` when none is in progress.
   `:candidate` steps never run. Returns `[plan warnings]`."
  [plan prefer-k]
  (let [warns    (volatile! [])
        accepted (fn [[_ v]] (not= :candidate (:status v)))
        doing-ks (->> plan (filter accepted) (filter (fn [[_ v]] (= :doing (:status v)))) (mapv first))
        keep-k   (cond (some #{prefer-k} doing-ks) prefer-k
                   (seq doing-ks) (first doing-ks)
                   :else nil)
        plan     (if (> (count doing-ks) 1)
                   (let [drop-set (disj (set doing-ks) keep-k)]
                     (vswap! warns conj (warn :plan_multiple_in_progress drop-set
                                          (str "multiple steps in_progress; kept " keep-k
                                            ", demoted the rest to todo")))
                     (mapv (fn [[k v]] [k (cond-> v (contains? drop-set k) (assoc :status :todo))]) plan))
                   plan)
        any?     (some (fn [[_ v]] (and (not= :candidate (:status v)) (= :doing (:status v)))) plan)
        first-td (first (filter (fn [[_ v]] (= :todo (:status v))) plan))
        plan     (if (and (not any?) first-td)
                   (do (vswap! warns conj (warn :plan_promoted_in_progress [(first first-td)]
                                            (str "no step in_progress; promoted " (first first-td) " to doing")))
                     (mapv (fn [[k v]] [k (cond-> v (= k (first first-td)) (assoc :status :doing))]) plan))
                   plan)]
    [plan @warns]))
(defn- apply-update-plan!
  "Whole-plan replace — the ONE task verb. `steps` is a vec of step maps:
   `{:step|:title <str> :status <name> :acceptance <str>? :verified <bool>?}`.

   Rebuilds the plan subtree of `:session/tasks` (tasks tagged `:plan? true`
   with a 1-based `:order`), preserving non-plan tasks (e.g. hook-tasks).
   Identity carries across re-emits by title slug, so flipping a step's status
   keeps its `:born`/`:id`/`:done-born`. Enforces the Codex invariant on
   ACCEPTED (non-`:candidate`) steps: at most one `:doing` (extras demoted to
   `:todo`), and exactly one when any accepted step is still open (first open
   `:todo` promoted). `:candidate` steps never run — they are proposals the
   user approves via chat before the agent flips them to `:todo`/`:doing`."
  [ctx form-scope [steps]]
  (let [steps      (if (sequential? steps) (vec steps) [])
        tasks      (or (:session/tasks ctx) {})
        non-plan   (into {} (remove (fn [[_ v]] (:plan? v)) tasks))
        prev-plan  (into {} (filter (fn [[_ v]] (:plan? v)) tasks))
        built      (loop [todo steps, idx 0, seen #{}, out []]
                     (if (empty? todo)
                       out
                       (let [step   (first todo)
                             title  (str (or (:title step) (:step step) ""))
                             k      (plan-step-key seen idx title)
                             status (normalize-plan-status (:status step))
                             prior  (get prev-plan k)
                             entry  (cond-> (merge (select-keys prior [:born :id :done-born])
                                              {:title title :status status :order (inc idx) :plan? true})
                                      (some? (:acceptance step)) (assoc :acceptance (:acceptance step))
                                      (contains? step :facts)     (assoc :facts (vec (:facts step)))
                                      (contains? step :verified)  (assoc :verified? (boolean (:verified step)))
                                      (contains? step :verified?) (assoc :verified? (boolean (:verified? step))))
                             entry  (if (:born entry) entry
                                      (assoc entry :born form-scope :id (entity-id form-scope k)))
                             entry  (stamp-or-clear-done-born entry form-scope task-terminal?)]
                         (recur (rest todo) (inc idx) (conj seen k) (conj out [k entry])))))
        [built warns] (enforce-one-doing built nil)]
    {:ctx (assoc ctx :session/tasks (into non-plan built)) :warnings warns :stamped? true}))
(defn- apply-plan-step!
  "Targeted single-step merge — change ONE plan step without re-sending the whole
   plan. `k` is the step key (canonicalized to match `update_plan`); `partial` may
   carry :status :title :acceptance :verified/:verified? :facts. Unknown key →
   the step is APPENDED (lets the model add one step surgically). Re-runs the
   one-`:doing` invariant, preferring this step when it was set to `:doing`."
  [ctx form-scope [k partial]]
  (let [partial  (if (map? partial) partial {})
        ck       (or (plan-canonical-key k) (str k))
        tasks    (or (:session/tasks ctx) {})
        non-plan (into {} (remove (fn [[_ v]] (:plan? v)) tasks))
        plan     (vec (sort-by (fn [[_ v]] (or (:order v) 0))
                        (filter (fn [[_ v]] (:plan? v)) tasks)))
        existing (get tasks ck)
        next-ord (inc (reduce max 0 (map (fn [[_ v]] (or (:order v) 0)) plan)))
        base     (or existing {:order next-ord :plan? true :status :todo
                               :born form-scope :id (entity-id form-scope ck)})
        merged   (cond-> base
                   (some? (:title partial))      (assoc :title (str (:title partial)))
                   (some? (:status partial))     (assoc :status (normalize-plan-status (:status partial)))
                   (some? (:acceptance partial)) (assoc :acceptance (:acceptance partial))
                   (contains? partial :facts)    (assoc :facts (vec (:facts partial)))
                   (contains? partial :verified) (assoc :verified? (boolean (:verified partial)))
                   (contains? partial :verified?) (assoc :verified? (boolean (:verified? partial))))
        merged   (cond-> merged (not (:title merged)) (assoc :title ck))
        merged   (stamp-or-clear-done-born merged form-scope task-terminal?)
        prefer   (when (= :doing (:status merged)) ck)
        plan2    (if existing
                   (mapv (fn [[kk v]] (if (= kk ck) [ck merged] [kk v])) plan)
                   (conj plan [ck merged]))
        [plan3 warns] (enforce-one-doing plan2 prefer)]
    {:ctx (assoc ctx :session/tasks (into non-plan plan3)) :warnings warns :stamped? true}))
(def ^:private FACT_CONTENT_SOFT_LIMIT
  "Per-fact `:content` size cap (chars of `pr-str`) above which a soft
   warning fires. Facts ride into every prompt; large blobs belong in
   the trailer or behind `(introspect-form …)`. 2 KB is roughly 500
   tokens — enough headroom for a stable observation map, small enough
   to keep a 20-fact session under ~10k tokens total."
  2048)
(defn- apply-fact-set!
  "Canonical fact upsert. Relations are DECLARATIVE keys on the map — the\n   map IS the desired state. `:depends_on` and `:contradicts`, when present,\n   REPLACE the entity's full edge set (absent key = leave untouched). For\n   `:contradicts` the symmetric back-links are reconciled: links the new\n   vector drops are removed from BOTH facts; links it adds are written to\n   both. This makes `(fact-set! :K {:contradicts [...]})` the single verb\n   for declaring AND retracting contradictions — no standalone mutator."
  [ctx form-scope [fact-k partial-map]]
  (cond
    (and (contains? partial-map :depends_on)
      (new-cycle-on-node? ctx :fact fact-k (:depends_on partial-map)))
    {:ctx ctx,
     :warnings [(warn :depends_on_cycle
                  [:fact fact-k]
                  (str "fact "
                    fact-k
                    " :depends_on "
                    (:depends_on partial-map)
                    " would introduce a cycle; write refused"))],
     :stamped? false}
    :else
    (let [has-contras? (contains? partial-map :contradicts)
          desired-raw (let [c (:contradicts partial-map)]
                        (if (coll? c) (vec c) (when (some? c) [c])))
          partial-map (dissoc partial-map :contradicts)
          path [:session/facts fact-k]
          existing (get-in ctx path)
          merged (cond-> (merge existing partial-map)
                   (nil? existing) (assoc :born form-scope
                                     :id (entity-id form-scope fact-k)))
          stamped (stamp-or-clear-done-born merged form-scope fact-terminal?)
          content (:content stamped)
          size (when (some? content) (try (count (pr-str content)) (catch Throwable _ 0)))
          ctx* (assoc-in ctx path stamped)
          wanted (remove #{fact-k} (or desired-raw []))
          present (set (filter (fn* [p1__52400#] (contains? (:session/facts ctx*) p1__52400#))
                         wanted))
          missing (when has-contras?
                    (remove (fn* [p1__52401#] (contains? (:session/facts ctx*) p1__52401#))
                      wanted))
          old-links (if has-contras? (get-in ctx* [:session/facts fact-k :contradicts] #{}) #{})
          to-add (clojure.set/difference present old-links)
          to-remove (if has-contras? (clojure.set/difference old-links present) #{})
          ctx** (as-> ctx* c
                  (reduce
                    (fn [c other]
                      (-> c
                        (update-in [:session/facts fact-k :contradicts] (fnil disj #{}) other)
                        (update-in [:session/facts other :contradicts] (fnil disj #{}) fact-k)))
                    c
                    to-remove)
                  (reduce
                    (fn [c other]
                      (-> c
                        (update-in [:session/facts fact-k :contradicts] (fnil conj #{}) other)
                        (update-in [:session/facts other :contradicts] (fnil conj #{}) fact-k)))
                    c
                    to-add)
                  (if (and has-contras? (empty? (get-in c [:session/facts fact-k :contradicts])))
                    (update-in c [:session/facts fact-k] dissoc :contradicts)
                    c))]
      {:ctx ctx**,
       :warnings (cond-> []
                   (and size (> size FACT_CONTENT_SOFT_LIMIT))
                   (conj (warn :fact-content-too-large
                           [fact-k]
                           (str "fact "
                             fact-k
                             " :content is "
                             size
                             " chars ("
                             "> "
                             FACT_CONTENT_SOFT_LIMIT
                             "); facts ride into every "
                             "prompt — keep them small, or summarize and reference "
                             "the original form via introspect-form.")))
                   (seq missing) (conj (warn :fact-contradicts-missing
                                         [fact-k]
                                         (str "fact-set! " fact-k
                                           " :contradicts references missing fact(s) "
                                           (vec missing))))),
       :stamped? true})))
(defn- apply-fact-contradicts!
  "Phase C mutator. Declare a SYMMETRIC contradiction between two
   facts: setting `:K1 ↔ :K2` writes `:contradicts #{:K2}` on K1 AND
   `:contradicts #{:K1}` on K2. Engine never auto-resolves which fact
   is correct — the soft warning fires when both stay `:active`, and
   the model decides which to flip `:superseded`.

   Not transitive. A ↔ B and B ↔ C does NOT imply A ↔ C; each pair
   must be declared.

   Self-contradiction (`:K1 :K1`) and unknown-fact references emit a
   warning and skip the write."
  [ctx _form-scope [a b]]
  (let [facts (or (:session/facts ctx) {})]
    (cond (= a b) {:ctx ctx,
                   :warnings [(warn
                                :fact-contradicts-self
                                [a]
                                (str "fact-contradicts! " a " " b " rejects self-contradiction"))],
                   :stamped? false}
      (not (contains? facts a))
      {:ctx ctx,
       :warnings [(warn :fact-contradicts-missing
                    [a]
                    (str "fact-contradicts! references missing fact " a))],
       :stamped? false}
      (not (contains? facts b))
      {:ctx ctx,
       :warnings [(warn :fact-contradicts-missing
                    [b]
                    (str "fact-contradicts! references missing fact " b))],
       :stamped? false}
      :else {:ctx (-> ctx
                    (update-in [:session/facts a :contradicts] (fnil conj #{}) b)
                    (update-in [:session/facts b :contradicts] (fnil conj #{}) a)),
             :warnings [],
             :stamped? true})))
(defn- apply-fact-contradicts-remove!
  "Phase C mutator. Symmetric remove of a contradiction declaration.
   No-op when either side is missing the link — idempotent."
  [ctx _form-scope [a b]]
  (letfn [(drop-link [c k other]
            (let [existing (get-in c [:session/facts k :contradicts])
                  pruned (disj (or existing #{}) other)]
              (cond-> c
                (and existing (empty? pruned)) (update-in [:session/facts k] dissoc :contradicts)
                (and existing (seq pruned)) (assoc-in [:session/facts k :contradicts] pruned))))]
    {:ctx (-> ctx
            (drop-link a b)
            (drop-link b a)),
     :warnings [],
     :stamped? true}))
(defn- apply-depends!
  "Convenience mutator. `kind` is one of #{:task :fact}, `k` is the
   entity id, `deps` is the new vec of dep refs (bare keys shorthand for
   `kind`, or `[:kind :K]` cross-kind). Engine REPLACES the existing
   `:depends_on` rather than merging — the model owns the full vec each
   call.

   Cycle check fires before write. Missing entity emits a soft warning
   and skips the write."
  [ctx _form-scope [kind k deps]]
  (let [subtree (case kind
                  :task :session/tasks
                  :fact :session/facts)
        path [subtree k]
        exists? (some? (get-in ctx path))]
    (cond (not exists?)
      {:ctx ctx,
       :warnings
       [(warn :depends_on-missing-entity
          [kind k]
          (str (name kind) " " k " does not exist; " (name kind) "-depends! ignored"))],
       :stamped? false}
      (new-cycle-on-node? ctx kind k deps)
      {:ctx ctx,
       :warnings [(warn :depends_on_cycle
                    [kind k]
                    (str (name kind)
                      " "
                      k
                      " :depends_on "
                      deps
                      " would introduce a cycle; write refused"))],
       :stamped? false}
      :else {:ctx (assoc-in ctx (conj path :depends_on) (vec deps)),
             :warnings [],
             :stamped? true})))
(defn apply-mutator
  "Apply a single mutator call to the CTX. Returns
   `{:ctx new-ctx :warnings vec :stamped? bool}`.
   On hard reject (cycle, malformed) :ctx is unchanged and :stamped? is false.
   On soft warn (collision, dangling ref) :ctx may still update; :warnings
   carry the diagnostic for the renderer.

"
  [ctx form-scope mutator args]
  (cond (malformed-scope? form-scope)
    {:ctx ctx,
     :warnings [(warn :malformed-scope
                  [form-scope]
                  (str "form-scope " form-scope " is malformed; write refused"))],
     :stamped? false}
    :else (case mutator
            :update-plan! (apply-update-plan! ctx form-scope args)
            :plan-step!   (apply-plan-step! ctx form-scope args)
            ;; :task-set! stays as an ENGINE-INTERNAL primitive (foundation
            ;; hook-tasks); it is no longer bound as a model-facing verb.
            :task-set! (apply-task-set! ctx form-scope args)
            :fact-set! (apply-fact-set! ctx form-scope args)
            :fact-depends! (apply-depends! ctx form-scope (cons :fact args))
            :fact-contradicts! (apply-fact-contradicts! ctx form-scope args)
            :fact-contradicts-remove! (apply-fact-contradicts-remove! ctx form-scope args)
                ;; unknown mutator: soft warn, no write
            {:ctx ctx,
             :warnings [(warn :unknown-mutator [mutator] (str "unknown mutator " mutator))],
             :stamped? false})))
;; =============================================================================
;; derive-warnings — render-time STRUCTURAL passes over indexes
;; =============================================================================
(defn- pass-task-depends-on-refs
  "Structural pass: task/fact `:depends_on` refs must point to a live
   task or fact. Bare-key refs resolve same-kind; typed `[:kind :K]`
   refs are checked against their target subtree. Each dangling ref is
   anchored under its owning entity so the renderer can surface it."
  [ctx _indexes]
  (let [tasks (or (:session/tasks ctx) {})
        facts (or (:session/facts ctx) {})
        subtree {:task tasks, :fact facts}
        ref-exists? (fn [[k kw]] (some? (get (get subtree k) kw)))]
    (vec (for [[default-kind subtree-map] [[:task tasks] [:fact facts]]
               [entity-id entity] subtree-map
               raw-dep (or (:depends_on entity) [])
               :let [ref (normalize-dep-ref default-kind raw-dep)]
               :when (and ref (not (ref-exists? ref)))]
           (warn :depends_on-dangling
             [default-kind entity-id ref]
             (str (name default-kind)
               " " entity-id
               " :depends_on refs nonexistent " (name (first ref))
               " " (second ref)))))))
(defn- pass-task-done-deps
  "Structural pass: task :status :done while a :depends_on target is
   non-terminal. Soft only — done is self-asserted and never reverted."
  [ctx _indexes]
  (let [tasks (or (:session/tasks ctx) {})]
    (vec (for [[task-id task] tasks
               :when (= :done (:status task))
               d (or (:depends_on task) [])
               :let [dep (get tasks d)]
               :when (and (some? dep) (not (task-terminal? (:status dep))))]
           (warn :task-done-pending-dep
             [task-id d]
             (str "task " task-id " :done but dep " d " is " (:status dep)))))))
(defn- pass-task-done-unverified
  "Structural pass (W3): task :status :done with a stated :acceptance but
   :verified? not true. Soft only — done is self-asserted; this nudges the
   model to actually check the acceptance criterion before claiming done."
  [ctx _indexes]
  (let [tasks (or (:session/tasks ctx) {})]
    (vec (for [[task-id task] tasks
               :when (and (= :done (:status task))
                       (some? (:acceptance task))
                       (not (true? (:verified? task))))]
           (warn :task-done-unverified
             [task-id]
             (str "task " task-id " :done but :verified? not true — "
               "check its :acceptance and set :verified? true"))))))
(def ^:private rebind-loop-threshold
  "Number of consecutive trailer pins binding the same def-name that
   trigger a `:trailer-rebind-loop` advisory. 3 catches a real loop
   (model repeating the same `(def x …)` over and over) without
   flagging healthy refinement (typical 1-2 retries)."
  3)
(def ^:private rebind-loop-name-re
  #"^\(\s*def(?:n|n-|macro|multi)?\s+([A-Za-z_*+!?<>=/.-][A-Za-z0-9_*+!?<>=/.\-]*)")
(defn- form-def-name
  [form]
  (when-let [src (some-> (:src form)
                   str
                   str/triml)]
    (when-let [[_ n] (re-find rebind-loop-name-re src)] n)))
(defn- pin-def-names [entry] (into #{} (keep form-def-name) (:forms entry)))
(defn- pass-contradicting-facts
  "Phase C invariant. Two facts that declared symmetric `:contradicts`
   AND remain `:active` simultaneously emit `:contradicting-facts`.
   Predicate is explicit (not transitive). The lexicographically
   smaller fact id is reported first so the warning is stable across
   re-renders — emitting only once per unordered pair."
  [ctx _indexes]
  (let [facts (or (:session/facts ctx) {})
        active? (fn [k] (= :active (or (:status (get facts k)) :active)))]
    (vec (for [[k entry] facts
               other (or (:contradicts entry) [])
               :when (and (contains? facts other)
                       (active? k)
                       (active? other)
                       (neg? (compare (str k) (str other))))]
           (warn
             :contradicting-facts
             [k other]
             (str "fact " k " ↔ " other " both :active — declare :superseded on one of them"))))))
(defn- pass-rebind-loop
  "Surface advisory when the tail of `:session/trailer` repeats the same
   `(def NAME …)` binding `rebind-loop-threshold` times in a row. Rebinding
   is intentional model behaviour and stays allowed; the warning gives the
   model a clear signal that the current tactic is not converging so it can
   switch to a different probe instead of looping silently. Forms pins only;
   summary entries do not count."
  [ctx _indexes]
  (let [trailer (vec (or (:session/trailer ctx) []))
        forms-pins (filterv :forms trailer)]
    (when (>= (count forms-pins) rebind-loop-threshold)
      (let [tail (subvec forms-pins (max 0 (- (count forms-pins) rebind-loop-threshold)))
            name-sets (mapv pin-def-names tail)
            common (apply set/intersection name-sets)]
        (for [n (sort common)]
          (warn :trailer-rebind-loop
            [n]
            (str "`(def " n
              " …)` has been rebound " rebind-loop-threshold
              "+ iters in a row." " The current tactic is not converging—switch to a"
              " different probe or call `(summarize {:trailer …})`"
              " before another rebind.")))))))
(defn derive-warnings
  "Run the STRUCTURAL invariant passes and return a sorted, deduped vec of
   short hint STRINGS — the `:session/hints` shape surfaced verbatim
   in the rendered ctx (no stages, no `;; ⚠` inside EDN). Each pass yields
   `{:code :anchor :message}` maps internally for stable ordering; only the
   `:message` strings are projected out.

   Passes:
     - dep-target-exists      (`pass-task-depends-on-refs`)
     - contradicting-facts    (`pass-contradicting-facts`)
     - rebind-loop            (`pass-rebind-loop`)
     - task-done-with-non-terminal-dep (`pass-task-done-deps`)
     - task-done-unverified   (`pass-task-done-unverified`)

   `form-results` arg is accepted for call-site compatibility but the
   structural passes do not consult it."
  ([ctx indexes] (derive-warnings ctx indexes nil))
  ([ctx indexes _form-results]
   (->> (concat (pass-contradicting-facts ctx indexes)
          (pass-task-depends-on-refs ctx indexes)
          (pass-task-done-deps ctx indexes)
          (pass-task-done-unverified ctx indexes)
          (pass-rebind-loop ctx indexes))
     distinct
     (sort-by (juxt :code (comp str :anchor)))
     (mapv :message))))
;; =============================================================================
;; advance-iter / enter-turn / gc-pass
;;
;; NOTE: the old `prune-stale-observation-pins` (drop ALL observation
;; pins the instant any later iter mutated state) was removed. It was
;; too blunt — reading file A then patching unrelated file B silently
;; dropped A's view, forcing a needless re-read. Trailer size is now
;; governed by ONE engine mechanism: the deterministic, size-triggered
;; auto-archive in `safe-guards/ensure-prompt-under-budget!` (folds the
;; OLDEST pins into a recoverable summary stub, never deletes — the
;; per-form BLOB stays in the DB, reachable via `introspect-iter`). The
;; model can additionally archive/summarize explicitly at `(done …)`.
;; =============================================================================
(defn- iteration-lifetime-hook-task?
  "True for hook-tasks declared `:lifetime :iteration`. These are
   hyper-transient signals that evaporate at the next iter boundary,
   not just the next turn boundary. Use for hooks whose firing
   condition is recomputed every iter from per-iter state (e.g. a
   one-iter retry-shape warning that should not haunt the model after
   the next provider call). If the condition still holds on the next
   iter, the hook re-creates the task; if not, it stays gone."
  [entry]
  (and (= :hook (:source entry)) (= :iteration (:lifetime entry))))
(defn advance-iter
  "Append a trailer pin for the just-finished iter (if it had any non-done
   form-results) and advance the cursor so the next iter starts at
   :iter (current+1) :next-form 1. `form-results-vec` is the ordered vec of
   `{:scope :tag :src :result :error}` envelopes captured during the iter.
   Forms whose src begins with `(done` are excluded from the pin. Observation-
   only pins carry forward until a later mutation makes them stale.

   Also drops every `:lifetime :iteration` hook-task from
   `:session/tasks` so hyper-transient signals do not survive into the
   next iter. The next iter's hook fire is the single source of truth
   for whether the task should exist.

   Rebinding the same def is intentional model behaviour (each iter
   refines arguments / inspects shapes). Pins from earlier rebinds stay
   in the trailer so the model can see prior attempts and avoid looping;
   if a run truly bloats, `(summarize {:trailer …})` is the contract
   to collapse the history."
  [ctx form-results-vec]
  (let [cursor (:session/scope ctx)
        iter-scope (str "t" (:turn cursor) "/i" (:iter cursor))
        ;; drop both `done(…)` AND forms whose result is the silent
        ;; sentinel. Engine mutators (task-set!, fact-set!, etc.) return
        ;; "vis_silent" (Python-native; a keyword snakes to it via `->py`);
        ;; their effect lives in ctx subtree mutations, not the trailer pin
        ;; log. `:src` is Python source, so the answer form reads `done(`.
        keepable (vec (remove (fn [r]
                                (or (str/starts-with? (str/triml (str (:src r))) "done(")
                                  (= "vis_silent" (:result r))
                                  (:vis/silent r)))
                        form-results-vec))
        ;; Observation pins are NO LONGER auto-pruned on mutation
        ;; (that caused needless re-reads). The trailer carries
        ;; forward verbatim; size is bounded by the engine's
        ;; size-triggered auto-archive (`ensure-prompt-under-budget!`)
        ;; and the model's explicit `(summarize {:trailer …})`.
        trailer' (vec (or (:session/trailer ctx) []))
        tasks' (into {}
                 (for [[k v] (or (:session/tasks ctx) {})
                       :when (not (iteration-lifetime-hook-task? v))]
                   [k v]))
        ctx* (-> ctx
               (assoc :session/trailer trailer')
               (assoc :session/tasks tasks'))
        ctx' (if (seq keepable)
               (update ctx* :session/trailer (fnil conj []) {:scope iter-scope, :forms keepable})
               ctx*)]
    (assoc ctx'
      :session/scope (-> cursor
                       (update :iter inc)
                       (assoc :next-form 1)))))
;; --- GC TTL constants ----------------------------------------------------
(def ^:private TTL-TASK-DONE 6)
(def ^:private TTL-TASK-CANCELLED 10)
(def ^:private TTL-TASK-REJECTED 10)
(def ^:private TTL-FACT-SUPERSEDED 6)
(def ^:private TTL-ARCHIVED
  "Turns an `:archived` entity (summarize'd) lingers in live ctx before
   gc-pass moves it to `:session/archived`. Short — the summary fact is
   its visible recap; the original just needs to stay recall-able."
  1)
(defn- ttl-for
  [entity-type status]
  (case [entity-type status]
    [:task :done] TTL-TASK-DONE
    [:task :cancelled] TTL-TASK-CANCELLED
    [:task :rejected] TTL-TASK-REJECTED
    [:task :archived] TTL-ARCHIVED
    [:fact :superseded] TTL-FACT-SUPERSEDED
    [:fact :archived] TTL-ARCHIVED
    nil))
(defn- entry-due-for-archive?
  [current-turn entity-type entry]
  (when-let [ttl (ttl-for entity-type (:status entry))]
    (when-let [{:keys [turn]} (parse-scope-form (:done-born entry))]
      (>= (- current-turn turn) ttl))))
(defn- turn-lifetime-hook-task?
  "True for hook-tasks declared `:lifetime :turn` at hook-registration
   time. These are ephemeral signals (e.g.
   `:vis.foundation/context-pressure`) that should not survive across
   turns: if the originating hint condition still holds, the next iter
   recreates them; if not, they stay gone. Without this prune, a
   transient warning lingered as a `:done` task for 6 turns and the
   model kept emitting `(task-set! … :done)` every turn to silence
   stale CTX chrome it could never actually resolve
   (Vis conv 11d4f817 / t14–t16)."
  [entry]
  (and (= :hook (:source entry)) (= :turn (:lifetime entry))))
(defn gc-pass
  "Drop terminal-status entries past TTL from live CTX. Uses the current
   :session/turn as the reference clock. Returns ctx with affected subtrees
   pared down. Pure: archived entries vanish from ctx but the caller is
   responsible for snapshotting before calling (so history is reachable).

   On top of the TTL pass, `:session/tasks` also drops every entry
   registered with hook `:lifetime :turn`. The turn-boundary prune
   runs regardless of `:status` because turn-lifetime tasks are
   recreated on demand by the next iter's hook fire."
  [ctx]
  (let [t        (:turn (:session/scope ctx))
        archived (volatile! (or (:session/archived ctx) {}))
        ;; entity leaving live keeps its FINAL state in :session/archived
        ;; keyed by stable :id (+ :vis/kind/:vis/key so `recall` puts it
        ;; back in the right subtree). NOT rendered to the prompt; O(1)
        ;; in-memory lookup — no snapshot scan.
        stash!   (fn [v kind k]
                   (when-let [id (:id v)]
                     (vswap! archived assoc id (assoc v :vis/kind kind :vis/key k))))
        gc-facts (reduce (fn [acc [k v]]
                           (if (entry-due-for-archive? t :fact v)
                             (do (stash! v :fact k) acc)
                             (assoc acc k v)))
                   {} (or (:session/facts ctx) {}))
        gc-tasks (reduce (fn [acc [k v]]
                           (cond
                             ;; turn-lifetime hook tasks are ephemeral —
                             ;; dropped, never archived.
                             (turn-lifetime-hook-task? v) acc
                             (entry-due-for-archive? t :task v) (do (stash! v :task k) acc)
                             :else (assoc acc k v)))
                   {} (or (:session/tasks ctx) {}))]
    (-> ctx
      (assoc :session/tasks gc-tasks)
      (assoc :session/facts gc-facts)
      (assoc :session/archived @archived))))
(defn enter-turn
  "Idempotent turn-start sync. Sets `:session/turn` to `turn-pos`,
   resets `:session/scope` to `{:turn turn-pos :iter 1 :next-form 1}`,
   clears `:engine/blockers`, then runs `gc-pass`.
   Safe to call repeatedly with the same `turn-pos` (no-op
   semantically); safe to call when ctx was loaded fresh from DB at
   turn-pos > 1.

   THIS is what the integration layer (vis loop) calls at the start
   of every turn. Single chokepoint for engine turn-state advance —
   no `advance-turn` alias, no auto-incrementing variant. The integration
   layer always knows the target turn-pos (DB tracks
   `session_turn_soul.position`), so the engine takes it as an explicit
   arg."
  [ctx turn-pos]
  (let [next-turn (long (or turn-pos 1))]
    (-> ctx
      (assoc :session/turn next-turn)
      (assoc :session/scope {:turn next-turn, :iter 1, :next-form 1})
      (assoc :engine/blockers [])
      gc-pass)))
;; =============================================================================
;; Empty-ctx constructor — used by tests + scenario replayer
;; =============================================================================
(defn empty-ctx
  "A minimal CTX scaffold that satisfies `::cs/ctx` with all required keys
   filled by empty / default values. Useful as the starting point for
   scenario replays.

   Includes the engine-ephemeral key `:engine/warnings` so the rest of
   the system can swap! it without nil-puncturing. Stripped at
   persistence boundaries via `strip-ephemeral`.

   No `:session/hints` — hook-emitted soft work items live as
   hook-sourced tasks under `:session/tasks` (`:source :hook`,
   `:hook-id`, `:importance`)."
  ([] (empty-ctx "test-session"))
  ([session-id]
   {:session/id session-id,
    :session/turn 1,
    :session/scope {:turn 1, :iter 1, :next-form 1},
    ;; Empty scaffold only. Prompt render replaces this through
    ;; foundation-core with a real workspace identity:
    ;;   {:workspace/root ... :workspace/sandbox? ... :vcs/kind ...}
    ;; `:vcs/kind :none` is reserved for an actual root with no supported VCS.
    :session/workspace {},
    :session/symbols {},
    :session/tasks {},
    :session/facts {},
    :session/trailer [],
    :engine/warnings []}))
(defn strip-ephemeral
  "Remove every `:engine/*` key from a ctx. Call before Nippy-snapshotting
   to persistence so transient mutator state (warnings, pending satisfy
   requests) does not leak into the durable record."
  [ctx]
  (when ctx (into {} (remove (fn [[k _]] (and (keyword? k) (= "engine" (namespace k)))) ctx))))
;; =============================================================================
;; Iter-scope parsing + comparator — used by trailer comparator + overlap check
;; =============================================================================
(def ^:private scope-iter-re #"^t([1-9][0-9]*)/i([1-9][0-9]*)$")
(defn parse-scope-iter
  "Parse `tN/iM` into `{:turn :iter}` or nil."
  [s]
  (when (string? s)
    (when-let [[_ t i] (re-matches scope-iter-re s)] {:turn (parse-long t), :iter (parse-long i)})))
(defn- iter-compare
  "Total order on iter-scope strings by (turn, iter). Malformed sorts first."
  [a b]
  (let [pa (parse-scope-iter a)
        pb (parse-scope-iter b)]
    (cond (and (nil? pa) (nil? pb)) (compare (str a) (str b))
      (nil? pa) -1
      (nil? pb) 1
      :else (let [c (compare (:turn pa) (:turn pb))]
              (if (zero? c) (compare (:iter pa) (:iter pb)) c)))))
(defn iter-in-range?
  "True iff iter-scope `s` falls inside [start, end] inclusive (per iter-compare)."
  [s start end]
  (and (<= (iter-compare start s) 0) (<= (iter-compare s end) 0)))
(defn- iter-ranges-partial-overlap?
  "True iff [a1, a2] and [b1, b2] share points but neither contains the other."
  [a1 a2 b1 b2]
  (let [overlap? (and (<= (iter-compare a1 b2) 0) (<= (iter-compare b1 a2) 0))
        a-in-b? (and (<= (iter-compare b1 a1) 0) (<= (iter-compare a2 b2) 0))
        b-in-a? (and (<= (iter-compare a1 b1) 0) (<= (iter-compare b2 a2) 0))]
    (and overlap? (not a-in-b?) (not b-in-a?))))
;; =============================================================================
;; summarize handler — collapse {:trailer :facts :tasks}
;; =============================================================================
(defn- entry-iter-range
  "Return [start end] iter-scope strings for any trailer entry (pin or summary)."
  [e]
  (cond (contains? e :scope) [(:scope e) (:scope e)]
    (contains? e :scope-start) [(:scope-start e) (:scope-end e)]
    :else nil))
(defn- entry-contained-by?
  [e [start end]]
  (when-let [[a b] (entry-iter-range e)]
    (and (<= (iter-compare start a) 0) (<= (iter-compare b end) 0))))
(defn- find-overlap-conflict
  "Return the existing summary that PARTIALLY overlaps with [start, end], or nil."
  [trailer start end]
  (some (fn [e]
          (when (contains? e :scope-start)
            (when (iter-ranges-partial-overlap? (:scope-start e) (:scope-end e) start end) e)))
    trailer))
(declare sort-trailer)
(defn apply-trailer-summarize
  "Apply one or more summary directives. For each directive `{:scope-start
   :scope-end :summary}`:

     1. Validate :scope-start ≤ :scope-end (else soft-warn, skip)
     2. Reject if any existing summary PARTIALLY overlaps (HARD; warn, skip)
     3. Absorb fully-contained entries (drop them from trailer)
     4. Insert new summary with :born stamped to `form-scope`

   The result is `sort-trailer`'d so the new stub lands at its
   chronological position (by :scope-start), never the bottom — every
   caller gets correct ordering without re-sorting.

   Returns `{:trailer new-trailer :warnings vec}`."
  [trailer summaries form-scope]
  (update
    (reduce (fn [{:keys [trailer warnings]} e]
              ;; The sandbox is full-snake end to end (env_python/py-key->clj),
              ;; so a spec authored as `{"scope_start" …}` arrives here as
              ;; `:scope_start`, NOT `:scope-start`. Storage + renderer are
              ;; kebab (see ctx-renderer snake_case test), so honor BOTH on the
              ;; way in and keep storing kebab — same dual-key tolerance as
              ;; apply-update-plan!'s :verified/:verified?.
              (let [scope-start (or (:scope-start e) (:scope_start e))
                    scope-end   (or (:scope-end e) (:scope_end e))
                    summary     (:summary e)
                    files       (:files e)]
              (cond
                (or (not (parse-scope-iter scope-start)) (not (parse-scope-iter scope-end)))
                {:trailer trailer,
                 :warnings
                 (conj
                   warnings
                   (warn
                     :trailer-summarize-bad-scope
                     [scope-start scope-end]
                     "trailer-summarize :scope-start / :scope-end must be valid iter-scopes"))}
                (pos? (iter-compare scope-start scope-end))
                {:trailer trailer,
                 :warnings
                 (conj
                   warnings
                   (warn
                     :trailer-summarize-inverted
                     [scope-start scope-end]
                     (str "trailer-summarize range " scope-start "->" scope-end " is inverted")))}
                :else (if-let [conflict (find-overlap-conflict trailer scope-start scope-end)]
                        {:trailer trailer,
                         :warnings (conj warnings
                                     (warn :trailer-summarize-partial-overlap
                                       [scope-start scope-end (:scope-start conflict)
                                        (:scope-end conflict)]
                                       (str "trailer-summarize "
                                         scope-start
                                         "->"
                                         scope-end
                                         " partially overlaps existing summary "
                                         (:scope-start conflict)
                                         "->"
                                         (:scope-end conflict)
                                         "; write refused")))}
                        {:trailer (-> trailer
                                    (->> (remove #(entry-contained-by? % [scope-start scope-end])))
                                    vec
                                    (conj (cond-> {:scope-start scope-start,
                                                   :scope-end scope-end,
                                                   :summary summary,
                                                   :born form-scope}
                                            (seq files) (assoc :files (vec files))))),
                         :warnings warnings}))))
      {:trailer trailer, :warnings []}
      (or summaries []))
    :trailer sort-trailer))
(defn sort-trailer
  "Sort by composite key: pin :scope, summary :scope-start. Stable order."
  [trailer]
  (vec (sort-by (fn [e]
                  (cond (contains? e :scope) [(:scope e) 0]
                    (contains? e :scope-start) [(:scope-start e) 1]
                    :else ["" 2]))
         (fn [[a a-tag] [b b-tag]]
           (let [c (iter-compare a b)] (if (zero? c) (compare a-tag b-tag) c)))
         trailer)))
;; =============================================================================
;; Auto-summarization — PURE helpers (call-site supplies summarizer side-effect)
;; =============================================================================
;;
;; Engine measures the trailer in tokens; when the rendered total
;; crosses a budget the integration layer (`loop.clj`) compacts the
;; oldest pins into one summary entry. The summary TEXT can come from
;; either a real companion LLM call (semantic) or a deterministic
;; fallback (`dummy-summary-text`). Either way the resulting stub
;; shape is identical — same `:scope-start :scope-end :summary :born`
;; the model already knows from `(summarize {:trailer …})`, plus
;; `:vis/auto?` / `:vis/summary-source` flags so the model can tell who
;; wrote it.
;;
;; The engine NEVER calls a companion LLM directly — staying out of the
;; side-effect business keeps these helpers pure-test-friendly. The
;; integration layer threads the summarizer in via
;; `summarize-trailer-with-companion`.
(defn pin-tokens
  "Token weight of one trailer entry as it would render into the
   prompt. Uses `pr-str` over the whole pin then `tokens/count-tokens`
   for a fast O(n) approximation. Summary stubs are typically tiny
   (a few hundred tokens); forms pins scale with their `:result`
   payload (post `bound-form-result` in the renderer, but this helper
   runs BEFORE that view-clip so it sees the full envelope — which is
   precisely the budget we are trying to free)."
  ^long [pin]
  (tokens/count-pr-tokens pin))
(defn trailer-total-tokens
  "Sum `pin-tokens` over a trailer vec. 0 for nil/empty."
  ^long [trailer]
  (reduce + 0 (map pin-tokens (or trailer []))))
(defn pick-oldest-batch-for-summarization
  "Walk `trailer` oldest-first, accruing pins until dropping that prefix
   would bring `trailer-total-tokens` under `target-tokens`. Returns
   `{:batch <vec-of-oldest-pins> :kept <vec-of-survivors> :tokens-freed N}`
   where `kept = (subvec trailer (count batch))`.

   Refuses to fold into the TWO most recent trailer entries (W5 safety):
   auto-fold is a blunt budget guard, so it must leave the model enough
   verbatim recent context to reason against — not just one pin. Folding is
   oldest-first and recoverable (the stub carries `(recall …)` pointers; the
   per-form blob stays in the DB), and durable knowledge lives in facts (which
   are never trailer pins), so this protects the active working window without
   losing anything.

   Returns `nil` when no batch can hit the target while leaving ≥2 tail pins
   (i.e. total is already under target, or ≤2 pins exist)."
  [trailer target-tokens]
  (let [trailer (vec (or trailer []))
        n (count trailer)]
    (when (>= n 2)
      (let [sizes (mapv pin-tokens trailer)
            total (reduce + 0 sizes)]
        (when (> total target-tokens)
          ;; sweep k = 1.. keeping at least the TWO most recent pins
          (loop [k 1
                 batch-tokens (long (nth sizes 0))]
            (let [remaining (- total batch-tokens)]
              (cond (<= remaining target-tokens) {:batch (subvec trailer 0 k),
                                                  :kept (subvec trailer k),
                                                  :tokens-freed batch-tokens}
                (>= (+ k 2) n) nil ;; would leave <2 tail pins; refuse
                :else (recur (inc k) (+ batch-tokens (long (nth sizes k))))))))))))
(defn- batch-scope-range
  "Return `[scope-start scope-end]` covering a batch of pins. Works for
   forms pins (`:scope`) and existing summary stubs (`:scope-start` /
   `:scope-end`) so recursive summarization stays consistent."
  [batch]
  (let [scopes (mapcat (fn [p]
                         (cond (:scope p) [(:scope p)]
                           (:scope-start p) [(:scope-start p) (:scope-end p)]
                           :else []))
                 batch)]
    [(first scopes) (last scopes)]))
(defn recall-call
  "Render the Python `recall(...)` call shown to the agent as a recovery
   pointer/hint. The sandbox is Python (env-python), so pointers MUST be
   Python, not Clojure: a single string arg, FULL-SNAKE. `addr` is a form
   scope (\"t3/i1/f2\") or an entity key/id; a namespaced id keyword
   (`:t1/calc-add`) renders by its bare key (\"calc_add\"). With `offset`,
   appends the scroll continuation `{\"offset\": N}`."
  ([addr] (recall-call addr nil))
  ([addr offset]
   (let [a (-> (if (keyword? addr) (name addr) (str addr))
             (str/replace "-" "_"))]
     (if offset
       (str "recall(\"" a "\", {\"offset\": " offset "})")
       (str "recall(\"" a "\")")))))
(defn- compact-src
  "One-line, length-capped form source for the auto-summary listing."
  [src]
  (let [s (-> (or src "") str str/trim (str/replace #"\s+" " "))]
    (if (> (count s) 90) (str (subs s 0 90) "…") s)))
(defn dummy-summary-text
  "Deterministic auto-summary (no companion LLM). Lists WHAT ran — the
   form SOURCES, no results — so the stub is meaningful on its own; the
   results stay recoverable via the (recall …) pointers it carries."
  [batch]
  (let [forms (mapcat #(:forms % []) batch)
        srcs  (->> forms (keep :src) (map compact-src) (remove str/blank?) vec)
        shown (take 25 srcs)
        more  (max 0 (- (count srcs) (count shown)))
        [s e] (batch-scope-range batch)]
    (str "auto-summarized " (count batch) " iter(s), " (count forms) " form(s). ran: "
      (str/join " | " shown)
      (when (pos? more) (str " (+" more " more)"))
      ". results via " (recall-call s)
      (when (not= s e) (str " … " (recall-call e)))
      ".")))
(defn make-summary-stub
  "Build the summary stub for an oldest-batch summarization.
   `summary-source` is `:companion-llm` (semantic) or `:engine-dummy`
   (deterministic). `summary-text` MUST be a non-blank string — callers
   that lose the companion call fall back to `dummy-summary-text`
   before reaching here."
  [batch summary-text summary-source born-scope]
  (let [[s e] (batch-scope-range batch)]
    {:scope-start s,
     :scope-end e,
     :summary summary-text,
     :born born-scope,
     :vis/auto? true,
     :vis/summary-source summary-source}))
(defn summarize-trailer-with-companion
  "Pure summarization step parameterised over a `summarizer-fn` callback.

   `ctx` is the engine ctx; `:session/trailer` is the only thing
   touched. `opts`:
     :target-tokens      desired post-summarization trailer total
     :born-scope         scope string stamped onto the new stub
     :summarizer-fn      `(fn [batch]) -> {:summary str :source kw}`
                          where :source is :companion-llm or :engine-dummy.
                          Must return synchronously; loop side handles
                          timeouts + cascade BEFORE invoking this fn.
                          When nil the engine uses `dummy-summary-text`.

   Returns `{:ctx new-ctx :compacted? bool :tokens-freed N :batch-size K
             :scope-range [start end] :warnings vec}`."
  [ctx {:keys [target-tokens born-scope summarizer-fn]}]
  (let [trailer (or (:session/trailer ctx) [])
        pick (pick-oldest-batch-for-summarization trailer target-tokens)]
    (if-not pick
      {:ctx ctx, :compacted? false}
      (let [{:keys [batch kept tokens-freed]} pick
            [scope-start scope-end] (batch-scope-range batch)
            summarized (when summarizer-fn (try (summarizer-fn batch) (catch Throwable _ nil)))
            summary (or (:summary summarized) (dummy-summary-text batch))
            source (or (:source summarized) :engine-dummy)
            stub (make-summary-stub batch summary source born-scope)
            ;; preserve sort order: stub takes the oldest slot, kept pins
            ;; stay in their original order (already sorted oldest-first)
            new-trail (vec (cons stub kept))]
        {:ctx (assoc ctx :session/trailer new-trail),
         :compacted? true,
         :tokens-freed tokens-freed,
         :batch-size (count batch),
         :scope-range [scope-start scope-end],
         :warnings [(warn :trailer-auto-summarized
                      [scope-start scope-end]
                      (str "Trailer auto-summarized: "
                        (count batch)
                        " oldest iter(s) ("
                        scope-start
                        (when (not= scope-start scope-end) (str " .. " scope-end))
                        ") collapsed into one "
                        (case source
                          :companion-llm "companion-LLM summary"
                          "engine fallback summary")
                        ". Full data via "
                        (recall-call scope-start)
                        "."))]}))))
;; =============================================================================
;; Archive rollup — "archive of archive"
;;
;; `:session/archived` is ALWAYS FULL: GC + summarize stash entries there and
;; they are NEVER evicted, so `(recall :K)` is always byte-exact. What we add
;; is a SECOND, rendered surface — `:session/archive-digest` — a compact
;; rolling gist of the archive, REFRESHED ON A TURN CADENCE (every
;; ARCHIVE-DIGEST-CADENCE turns). Each refresh folds the entries archived
;; since the last digest, plus the previous gist, into one fresh gist — that
;; fold IS the archive-of-archive, and it keeps the RENDERED cost bounded
;; while the raw archive stays complete behind `recall`.
;; Like `summarize-trailer-with-companion`, this is pure and parameterised
;; over a `summarizer-fn`; the loop supplies the cheapest-model call OFF the
;; critical path (async). nil summarizer-fn → deterministic gist.
;; =============================================================================

(def ARCHIVE-DIGEST-CADENCE
  "Turns between `:session/archive-digest` refreshes. The raw
   `:session/archived` is untouched (full, exactly recallable); only the
   rendered gist refreshes on this cadence."
  5)

(defn- archived-entry-turn
  "Birth turn of an archived entry, parsed from its stable `:t<N>/<key>`
   id (namespace `t<N>`). Used for stable oldest-first ordering. 0 when
   unparseable."
  [[id _]]
  (or (some-> id namespace (subs 1) parse-long) 0))

(defn- dummy-archive-digest
  "Deterministic archive digest when no companion model is supplied: the
   previous entry maps plus one normalized `{:source :content :importance
   :recall}` map per freshly folded entry — SAME shape as the companion-LLM
   path and the same vocabulary as `:session/hints` entries, so consumers
   never branch on source or field name."
  [entries prev-summary]
  (let [label (fn [{:keys [title content id]}]
                (let [s (str (or title content id))]
                  (if (> (count s) 80) (str (subs s 0 80) "…") s)))
        new-entries (mapv (fn [{:keys [id] :as e}]
                            {:source     :archive
                             :content    (label e)
                             :importance :low
                             :recall     (recall-call id)})
                      entries)]
    (into (vec prev-summary) new-entries)))

(defn- archived-uncovered
  "Archived entries whose id is NOT yet folded into the digest's
   `:covered-ids` — the delta a refresh needs to absorb."
  [ctx]
  (let [covered (set (:covered-ids (:session/archive-digest ctx)))]
    (into {} (remove (fn [[id _]] (covered id)) (or (:session/archived ctx) {})))))

(defn- ctx-turn [ctx]
  (long (or (:turn (:session/scope ctx)) (:session/turn ctx) 0)))

(defn archive-digest-due?
  "True when the rolling digest is stale: there ARE archived entries not yet
   folded in AND at least `cadence` turns have passed since the last refresh
   (or no digest exists). `:session/archived` is never evicted — this only
   gates the rendered gist refresh. The async rollup trigger polls this."
  ([ctx] (archive-digest-due? ctx ARCHIVE-DIGEST-CADENCE))
  ([ctx cadence]
   (and (seq (archived-uncovered ctx))
     (>= (- (ctx-turn ctx) (long (or (:updated-turn (:session/archive-digest ctx)) 0)))
       (long (or cadence ARCHIVE-DIGEST-CADENCE))))))

(defn roll-archive
  "Refresh `:session/archive-digest` from the archive WITHOUT evicting
   anything — `:session/archived` stays full, so `(recall :K)` is always
   exact. Folds the not-yet-covered archived entries + the previous gist
   into a fresh digest via `summarizer-fn`.

   Pure; parameterised over `summarizer-fn :: (fn [{:keys [entries
   prev-digest]}]) -> {:summary str :source kw}` (the loop wires the
   cheapest-model async call; nil → deterministic `dummy-archive-digest`).

   Returns `{:ctx new-ctx :rolled N :source kw}`, or `{:ctx ctx :rolled 0}`
   when nothing new to fold. Never throws on a well-formed ctx."
  [ctx {:keys [summarizer-fn]}]
  (let [fresh (archived-uncovered ctx)]
    (if (empty? fresh)
      {:ctx ctx :rolled 0}
      (let [prev       (:session/archive-digest ctx)
            entries    (mapv (fn [[id v]]
                               (assoc (select-keys v [:content :title :status :vis/kind :vis/key])
                                 :id id))
                         (sort-by archived-entry-turn fresh))
            summarized (when summarizer-fn
                         (try (summarizer-fn {:entries entries :prev-digest (:summary prev)})
                           (catch Throwable _ nil)))
            gist       (or (:summary summarized) (dummy-archive-digest entries (:summary prev)))
            source     (or (:source summarized) :engine-dummy)
            covered    (into (set (:covered-ids prev)) (map :id) entries)
            digest     {:summary      gist
                        :covered-ids  covered
                        :count        (count covered)
                        :source       source
                        :updated-turn (ctx-turn ctx)}]
        {:ctx    (assoc ctx :session/archive-digest digest)
         :rolled (count entries)
         :source source}))))

(defn- apply-entity-archive
  "Flip the listed `:facts` / `:tasks` entity keys to the `:archived`
   terminal status (distinct from `:done` / `:cancelled` /
   `:superseded`). Missing entities emit a soft warning and skip.

   There is NO `:specs` subtree in the engine — only `:session/facts`
   and `:session/tasks` exist, so this primitive handles those two.

   Snapshots keep the raw entity; `(recall :K)` windows it. Live ctx GC removes archived entries at the next turn boundary
   without waiting for TTL. Returns `{:ctx :warnings}`."
  [ctx form-scope archive-map]
  (let [{:keys [facts tasks]} (or archive-map {})
        kind->subtree {:fact :session/facts, :task :session/tasks}
        attempts (concat (for [k (or facts [])] [:fact k]) (for [k (or tasks [])] [:task k]))
        outcomes (for [[kind k] attempts]
                   (let [subtree (kind->subtree kind)
                         exists? (some? (get-in ctx [subtree k]))]
                     {:kind kind, :k k, :subtree subtree, :exists? exists?}))
        ;; stamp :done-born (the GC clock) so gc-pass can move the
        ;; archived entity into :session/archived after TTL-ARCHIVED.
        ctx-with-status (reduce (fn [acc {:keys [k subtree exists?]}]
                                  (cond-> acc
                                    exists?
                                    (update-in [subtree k] assoc
                                      :status :archived :done-born form-scope)))
                          ctx
                          outcomes)
        ctx' ctx-with-status
        warns (vec (for [{:keys [kind k exists?]} outcomes
                         :when (not exists?)]
                     (warn
                       :done-archive-missing-entity
                       [kind k]
                       (str (name kind) " " k " listed in (summarize …) does not exist"))))]
    {:ctx ctx', :warnings warns}))
(defn- summarize-entities
  "Collapse listed entity keys of one `kind` (:fact|:task) into a single
   new summary FACT, then flip the originals to `:archived`. A settled
   set of facts/tasks IS knowledge, so the recap always lands as a fact.

   `directives` is a vec of `{:keys [k…] :into key? :summary str}`.
   `:into` names the summary fact (auto `:summary-tN-<kind>-<n>` when
   omitted). `start-idx` seeds the auto-key counter. Returns
   `{:ctx :warnings :next-idx N}`."
  [ctx form-scope kind directives start-idx]
  (let [turn (or (:session/turn ctx) 0)]
    (reduce
      (fn [{:keys [ctx warnings next-idx]} {ks :keys :keys [into summary]}]
        (let [summary-text (some-> summary str not-empty)
              new-key (or into
                        (keyword (str "summary-t" turn "-" (name kind) "-" next-idx)))
              {ctx1 :ctx w :warnings}
              (apply-entity-archive ctx form-scope
                (case kind :fact {:facts ks} :task {:tasks ks}))]
          (if-not summary-text
            {:ctx      ctx1
             :warnings (conj (vec (concat warnings w))
                         (warn :done-summarize-missing-text [kind (vec ks)]
                           (str "summarize entry for " (name kind)
                             " " (vec ks) " has no :summary text; originals"
                             " archived but no summary fact written")))
             :next-idx (inc next-idx)}
            {:ctx      (assoc-in ctx1 [:session/facts new-key]
                         {:content         summary-text
                          :born            form-scope
                          :status          :active
                          :source          :done-summarize
                          :scope           (str "t" turn)
                          :summarized-from (vec ks)})
             :warnings (vec (concat warnings w))
             :next-idx (inc next-idx)})))
      {:ctx ctx :warnings [] :next-idx start-idx}
      (or directives []))))
(defn apply-summarize
  "`(summarize {:trailer [{:scope-start :scope-end :summary} …]
   :facts [{:keys :into :summary} …] :tasks […]})` — compress N→1, never
   lose data:
     :trailer  range  → collapsed into one recap stub pin (DB keeps the
                        raw pins; introspect-iter recovers)
     :facts    N keys → one new summary FACT, originals → :archived
     :tasks    N keys → one new summary FACT, originals → :archived

   Returns `{:ctx :warnings}`."
  [ctx form-scope {:keys [trailer facts tasks]}]
  (let [{t :trailer w-trailer :warnings}
        ;; apply-trailer-summarize already returns a sorted trailer, so
        ;; the stub is at its chronological spot for every caller.
        (apply-trailer-summarize (or (:session/trailer ctx) []) (or trailer []) form-scope)
        ctx-t (assoc ctx :session/trailer t)
        {ctx-f :ctx w-f :warnings n1 :next-idx}
        (summarize-entities ctx-t form-scope :fact facts 1)
        {ctx-k :ctx w-k :warnings}
        (summarize-entities ctx-f form-scope :task tasks n1)]
    {:ctx ctx-k, :warnings (vec (concat w-trailer w-f w-k))}))
(declare apply-done-impl)
(defn apply-done
  "Process a `(done {…})` form against the ctx trailer + entity archive.
   The :answer field IS handled here in Phase F: when present + non-blank,
   the engine auto-writes a `:turn-N-answer` fact under `:session/facts`
   whose `:content` is the FULL answer markdown, verbatim — no lossy
   synopsis at write-time. The fact rides into next turn's cached `;; ctx`
   prefix; the renderer head+tail-clips oversized `:content` to a stub with
   a `(recall :turn-N-answer)` hint, so cross-turn answer reference is a
   normal fact lookup and the full body is one `recall` away.

   `args` map keys:
     :answer     markdown payload — stored verbatim as the
                 `:turn-N-answer` fact `:content`; the loop also ships it
                 to the channel.

   done does NOT compact. Compaction is the standalone `(summarize …)` verb
   (batch one right before `(done …)` in the same fence to compact at close).
   The engine ALSO auto-summarizes oldest trailer pins under size pressure
   (`ensure-prompt-under-budget!`). Nothing is hard-deleted; raw data stays
   in the DB (trailer pins + entities recoverable via recall)."
  [ctx form-scope
   {:keys [answer user-request turn-summary]}]
  (apply-done-impl ctx
    form-scope
    {:answer answer,
     :user-request user-request,
     :turn-summary turn-summary}))
(defn- auto-fact-for-turn-answer
  "Phase F: build the `:turn-N-answer` fact — the FULL answer for the
   just-closed turn, stored as a first-class fact so cross-turn reference
   is a normal fact lookup and compression is the normal `summarize` verb
   (N answer-facts → 1 recap, originals :archived, recoverable via
   `recall`). NO lossy synopsis is baked in at write-time; the renderer
   head+tail-clips oversized `:content` in-prompt with a `(recall …)` hint,
   while the stored value stays verbatim.

   Content fields:

     :question   user request that started the turn
     :content    the FULL answer markdown, verbatim

   Model can reconstruct what changed across the turn by walking
   :session/{facts,tasks} entries whose :born or :done-born starts with
   the matching scope prefix — no separate event log needed.

   Plus engine-owned framing: `:status :active`, `:scope \"tN\"`,
   `:source :done-auto`, `:born <form-scope>`. Reserved keys cannot
   be overridden by the model.

   Returns `[fact-id fact-value]` or nil when nothing happened
   (no answer AND no entity-level transitions — a fully silent
   close-of-turn, e.g. cancelled iter that touched nothing)."
  [ctx form-scope {:keys [user-request answer model-summary]}]
  (let [turn-pos (or (:session/turn ctx) 0)
        answer-str (some-> answer
                     str
                     clojure.string/trim
                     not-empty)
        question (some-> user-request
                   str
                   clojure.string/trim
                   not-empty)
        ;; A turn is worth recording when EITHER the user got an answer
        ;; OR at least one entity transitioned in this turn (born scope
        ;; or done-born scope starts with t<turn-pos>/).
        turn-prefix (str "t" turn-pos "/")
        any-born? (some (fn [m]
                          (some (fn [[_ entity]]
                                  (or (clojure.string/starts-with? (str (:born entity) "")
                                        turn-prefix)
                                    (clojure.string/starts-with? (str (:done-born entity) "")
                                      turn-prefix)))
                            (or m {})))
                    [(:session/facts ctx) (:session/tasks ctx)])
        meaningful? (or (some? answer-str) any-born?)
        reserved #{:status :source :scope :born :question :content}
        model-extras (when (map? model-summary)
                       (into {} (remove (fn [[k _]] (contains? reserved k)) model-summary)))
        fact-id (keyword (str "turn-" turn-pos "-answer"))
        base (cond->
               {:status :active, :scope (str "t" turn-pos), :source :done-auto, :born form-scope}
               question   (assoc :question question)
               answer-str (assoc :content answer-str))]
    (when meaningful? [fact-id (merge model-extras base)])))
(defn- apply-done-impl
  "– the un-gated core of `apply-done`. Pulled out so the
   gate can short-circuit cleanly; callers must use `apply-done`."
  [ctx form-scope
   {:keys [answer user-request turn-summary]}]
  (let [;; Phase F (redesigned): `:turn-N-answer` fact carries the FULL
        ;; answer markdown verbatim under :content (head+tail-clipped
        ;; in-prompt, recallable in full) plus the question + entity ids
        ;; born/done this turn. Compression is deferred to the standalone
        ;; `summarize` verb — answers are summarized AFTER the turn, never
        ;; truncated at birth. Optional `:turn-summary` arg from the model
        ;; adds free-form fields merged onto the auto skeleton.
        auto-fact-entry (auto-fact-for-turn-answer ctx
                          form-scope
                          {:user-request user-request,
                           :answer answer,
                           :model-summary turn-summary})
        sorted (sort-trailer (:session/trailer ctx))
        ctx-final (cond-> (assoc ctx :session/trailer sorted)
                    auto-fact-entry (assoc-in [:session/facts (first auto-fact-entry)]
                                      (second auto-fact-entry)))]
    {:ctx ctx-final, :warnings []}))
;; =============================================================================
;; recall — the single recovery verb (replaces the introspect-* sprawl)
;;
;;   recall by ADDRESS  → char-window a stored value (clipped MIDDLE reachable)
;;   recall by CONTENT  → search the live (non-summarized) trace for a scope
;; Pure halves live here; the effectful DB/ctx-atom halves live in
;; `ctx-loop/build-introspect-bindings`.
;; =============================================================================
(def ^:private RECALL_DEFAULT_LIMIT_TOKENS
  "Default recall window in TOKENS, matched to the renderer's
   FORM_RESULT_TOKEN_LIMIT (10 000 tok) so a default `recall` returns the
   SAME evidence budget a `cat`/form-result gets before it is head+tail
   clipped — recall and cat now share one window size. Held just under that
   ceiling (envelope headroom) so the recall result itself renders verbatim
   and is never re-clipped."
  9000)
(defn- token-fit-end
  "Largest char index `end` ≥ `off` such that the slice `(subs s off end)`,
   as it renders into the recall envelope (pr-str-escaped), weighs ≤ `budget`
   tokens under cl100k. Over-allocates chars (~4/tok) then shrinks to fit;
   always advances ≥1 char so the cursor makes forward progress."
  ^long [^String s ^long off ^long budget]
  (let [total (count s)]
    (if (>= off total)
      total
      (loop [end (min total (+ off (* (max 1 budget) 4)))]
        (cond
          (<= end (inc off))
          (min total (inc off))
          (<= (tokens/count-pr-tokens (subs s off end)) budget)
          end
          :else
          (recur (max (inc off) (long (+ off (* (- end off) 0.85))))))))))
(defn recall-window
  "Pure token-bounded window over `(pr-str v)` with a stateless char cursor.
   Returns a self-describing map: the slice plus the exact Python
   `recall(…, {\"offset\": N})` continue-call, so the model scrubs a big value
   the trailer clip would otherwise hide the middle of. The default window
   carries `RECALL_DEFAULT_LIMIT_TOKENS` — the same evidence budget `cat`/any
   form-result gets — so recall and cat read the same amount by default.

   `addr-str` is the PLAIN Python address string (form scope `\"t3/i1/f2\"`
   or entity key `\"calc_add\"`) echoed into `vis_recall` and the `vis_next`
   continue-call — NOT pr-str'd, so it renders as a real Python call. `budget`
   is a TOKEN count (not chars); `offset` stays a char cursor for stateless
   scrolling."
  ([addr-str v] (recall-window addr-str v 0 RECALL_DEFAULT_LIMIT_TOKENS))
  ([addr-str v offset budget]
   (let [s     (try (pr-str v) (catch Throwable _ (str v)))
         total (count s)
         off   (max 0 (min (long (or offset 0)) total))
         bud   (max 1 (long (or budget RECALL_DEFAULT_LIMIT_TOKENS)))
         end   (token-fit-end s off bud)]
     (cond-> {:vis/recall addr-str
              :vis/window [off end]      ; char range of this slice
              :vis/size   total          ; total chars (offset cursor bound)
              :view       (subs s off end)}
       ;; Python scroll-continuation — the agent evals `vis_next` verbatim.
       (< end total) (assoc :vis/next (recall-call addr-str end))))))
(defn find-entity-by-id
  "Locate the entity whose stable `:id` = `id`. Searches LIVE facts then
   tasks, then `:session/archived` (entities GC'd out of live). Returns
   `{:source :live|:archived :kind :key :entry}` or nil. Pure."
  [ctx id]
  (or (some (fn [[k v]] (when (= id (:id v))
                          {:source :live :kind :fact :key k :entry v}))
        (:session/facts ctx))
    (some (fn [[k v]] (when (= id (:id v))
                        {:source :live :kind :task :key k :entry v}))
      (:session/tasks ctx))
    (when-let [v (get-in ctx [:session/archived id])]
      {:source :archived :kind (:vis/kind v) :key (:vis/key v)
       :entry  (dissoc v :vis/kind :vis/key)})))
(defn restored-entry
  "The entity `entry` flipped back to live (`:active` fact / `:todo`
   task) and stamped `:recalled {:scope :why}` so the render shows WHY
   it came back."
  [entry kind scope why]
  (merge entry {:status (case kind :fact :active :task :todo)
                :recalled {:scope scope :why why}}))
(defn recall-entity
  "Restore the entity whose stable `:id` = `id` back to LIVE — from the
   live subtree (same-turn :archived) OR from `:session/archived`
   (GC'd in a past turn, final state captured at GC). Re-inserts into
   `:session/facts`/`:session/tasks`, removes the `:session/archived`
   copy, and stamps `:recalled {:scope :why}`. All in-memory, O(1) — no
   snapshot scan. Returns `{:ctx :found? :kind :key}`."
  [ctx id scope why]
  (if-let [{:keys [source kind key entry]} (find-entity-by-id ctx id)]
    (let [target (case kind :fact :session/facts :task :session/tasks)]
      {:ctx    (cond-> (assoc-in ctx [target key] (restored-entry entry kind scope why))
                 (= source :archived) (update :session/archived dissoc id))
       :found? true, :kind kind, :key key})
    {:ctx ctx, :found? false, :kind nil, :key nil}))
;; ---------------------------------------------------------------------------
;; recall SEARCH — pure helpers (the DB I/O is injected by the caller)
;; ---------------------------------------------------------------------------
(defn iter-scope-after?
  "PURE. True when the iter scope `tN/iM` (turn `tp`, iter `ip`) is strictly
   AFTER `cursor` (`{:turn :iter}`), or always true when `cursor` is nil.
   Lets recall SEARCH page past a point the agent already inspected
   (`recall({\"match\": …, \"scope_after\": \"t2/i1\"})`)."
  [tp ip cursor]
  (or (nil? cursor)
    (> tp (:turn cursor))
    (and (= tp (:turn cursor)) (> ip (:iter cursor)))))
(defn search-hits->scopes
  "PURE. Resolve raw FTS5 `hits` (each `{:owner-id :snippet :rank}`) to recovery
   pointers `[{:scope \"tN/iM\" :preview :rank}]`, preserving FTS5 rank order and
   dropping any hit that (a) resolves to no iteration row, or (b) is not strictly
   after `cursor`. `turns` is a seq of `{:id :position}`; `iters-of` is a fn
   `soul-id -> seq of {:id :position}` so the DB cursor/cache stays in the
   impure caller and this fn is trivially unit-testable.

   Summarized ranges stay findable on purpose: SEARCH runs over the raw
   iteration rows, not the trailer view, so a hit inside a fold is exactly what
   the agent is looking for. This fn never filters on summary state."
  [hits turns iters-of cursor]
  (let [turn-by-soul (into {} (map (juxt :id :position)) turns)]
    (->> hits
      (keep (fn [{:keys [owner-id snippet rank]}]
              (let [[trow it] (some (fn [t]
                                      (some #(when (= owner-id (:id %)) [t %])
                                        (iters-of (:id t))))
                                turns)
                    tp (turn-by-soul (:id trow))
                    ip (:position it)]
                (when (and tp ip (iter-scope-after? tp ip cursor))
                  {:scope (str "t" tp "/i" ip) :preview snippet :rank rank}))))
      vec)))
(defn fts-or-literal
  "Run `search-fn` (a fn of query-mode → hits) as raw FTS5 first; if FTS5 can't
   parse the query (a stray quote / dangling operator from code text), retry as
   a literal phrase so a content search never hard-fails on punctuation.
   Higher-order: the DB call is injected, so the fts→literal policy is tested
   without a database."
  [search-fn]
  (try (search-fn :fts)
    (catch Exception _ (search-fn :literal-text))))
(defn utilization
  "Pure: the `:session/utilization` map the model reads to see how much
   of the context window the LAST request consumed. Keys are spelled out
   so they can't be misread:
     :last-request-tokens  input size of the most recent model call
     :model-input-limit    HARD per-call ceiling (provider rejects above)
     :pct-of-limit         last-request / model-input-limit, rounded
     :auto-compress-above  engine folds the oldest trailer when a call
                           would exceed this (soft guardrail, < limit)
     :turn-total-tokens    cumulative input this turn (billing, NOT a
                           per-call limit — may exceed the limit safely)
   Returns nil until a request has actually been measured (req <= 0), so
   the first iter of a turn shows nothing rather than a bogus 0%."
  [request-tokens window-tokens turn-tokens fold-cap]
  (let [req (long (or request-tokens 0))
        win (long (or window-tokens 0))]
    (when (pos? req)
      (cond-> {:last-request-tokens req
               :turn-total-tokens   (long (or turn-tokens 0))
               :auto-compress-above (long (or fold-cap 0))}
        (pos? win) (assoc :model-input-limit win
                     :pct-of-limit (long (Math/round (* 100.0 (/ (double req) (double win))))))))))

(defn normalize-importance
  "Collapse the various importance vocabularies (severity-style
   :critical/:warn/:info, salience-style :high/:medium/:low, strings) onto
   the ONE scale shared by `:session/hints` entries AND
   `:session/archive-digest` entries: `:high | :medium | :low`. Unknown/nil
   → :medium."
  [v]
  (case (some-> v name clojure.string/lower-case)
    ("critical" "high" "blocker" "error") :high
    ("warn" "warning" "medium" "med")     :medium
    ("info" "low" "trivial" "debug")      :low
    :medium))

(def model-facing-keys
  "EXACT set of `:session/*` keys the model is meant to see — the same
   keys `ctx-renderer/render-ctx` serializes into the `;; ctx` EDN. This
   is the SINGLE definition of 'model-facing'; `session-view` selects on
   it so nothing else can leak into the bound `ctx`.

   Deliberately EXCLUDED:
     :session/archived  the `(summarize …)` store — compressed OUT of the
                        prompt to free tokens, reachable ONLY via
                        `(recall …)`. Inlining it would undo compaction.
     :session/hints     internal; never rendered.
     :engine/*          bookkeeping (`:engine/utilization` is projected to
                        `:session/utilization` below; the rest stays hidden).
   `:session/utilization` is derived (from `:engine/utilization`) and
   `:session/hints` is render-derived, so neither is listed here —
   both are folded in by `session-view`."
  [:session/id :session/turn :session/scope :session/workspace
   :session/env :session/resources :session/symbols :session/tasks :session/facts
   :session/trailer :session/archive-digest])

(defn session-view
  "THE single projection from engine-internal ctx to the model-facing
   `:session/*` view — the ONE way the ctx is shaped for the model.

   Both consumers derive from this, so the EDN the model reads and the
   value bound to the bare `ctx` symbol are the same map by construction:
     - `ctx-renderer/render-ctx`  serializes this view to the `;; ctx` text
     - `ctx-loop/session-snapshot` binds this view as read-only `ctx`

   Rules (mirroring exactly what the renderer shows):
     - keep ONLY `model-facing-keys` (so archive / engine bookkeeping can
       NEVER leak)
     - project `:engine/utilization` → `:session/utilization`
     - CONJOIN hints into ONE `:session/hints` feed of HINT MAPS:
         {:source <:engine | hook-id>  ; WHO raised it
          :content <string>            ; the advisory text
          :importance <:info|:warn|:critical>
          :depends_on [refs]}          ; optional — related entities
       Engine structural advisories (`warnings`) become `:source :engine`
       hints; extension hook hints (active `:source :hook` tasks) become
       `:source <hook-id>` hints and are MOVED OUT of `:session/tasks` so
       the unified advisory surface owns them and the task list stays the
       model's own work. The hook tasks still live in the ctx-atom
       untouched (satisfaction / lifetime / GC all operate there) — this
       is a render-layer projection only. Sorted critical → warn → info.
   Pure; never throws on a well-formed ctx map."
  ([ctx] (session-view ctx nil))
  ([ctx warnings]
   (let [tasks        (:session/tasks ctx)
         hook?        (fn [[_ v]] (= :hook (:source v)))
         own-tasks    (into {} (remove hook?) tasks)
         engine-hints (->> warnings
                        (filter (fn [s] (and (string? s) (not (str/blank? s)))))
                        (map (fn [s] {:source :engine :content s :importance :medium})))
         hook-hints   (->> tasks
                        (filter hook?)
                        (keep (fn [[k v]]
                                (let [t (:title v)]
                                  (when (and (string? t) (not (str/blank? t)))
                                    (cond-> {:source     (or (:hook-id v) k)
                                             :content    t
                                             :importance (normalize-importance (:importance v))}
                                      (seq (:depends_on v)) (assoc :depends_on (vec (:depends_on v)))))))))
         rank         {:critical 0 :warn 1 :info 2}
         hints        (->> (concat engine-hints hook-hints)
                        (sort-by (fn [h] (rank (:importance h) 3)))
                        vec)]
     (cond-> (select-keys ctx model-facing-keys)
       (contains? ctx :session/tasks) (assoc :session/tasks own-tasks)
       (:engine/utilization ctx)      (assoc :session/utilization (:engine/utilization ctx))
       (seq hints)                    (assoc :session/hints hints)))))
;; =============================================================================
;; Form tag classification — derive :tag from the form source string
;; =============================================================================
(def ^:private py-head-name-re
  "Matches the head call NAME of a Python top-level form: any number of
   leading blank or `#`-comment lines, then a bare `identifier(`. Captures
   the identifier. A form that is not a `name(...)` call (a bare value, an
   assignment, a comment-only block) does not match."
  #"\A(?:[ \t]*(?:#[^\n]*)?\n)*[ \t]*([A-Za-z_][A-Za-z0-9_]*)\s*\(")
(defn form-head-name
  "Return the head call NAME (a string) of `src` — a Python source string —
   or nil when `src` is not a `name(...)` call form. Leading comments and
   blank lines are skipped. Reading the head name (rather than scanning the
   raw source) avoids false positives — a `\"done(x)\"` inside a string can't
   match. Used by `classify-form-tag` and `engine-form-src?`; both agree on
   the head, so there is one implementation."
  [src]
  (some-> (re-find py-head-name-re (str src)) second))
(def ^:private core-mutation-heads
  "Engine-owned call NAMES (Python, snake_case) that classify a form as
   `:mutation`: the CTX memory mutators (task/fact surface) plus control
   flow (`done`, `set_session_title`).

   Extension tools (`patch`, `write`, `git_commit`, anything an extension
   ships) are NOT here. Extensions declare their own observation / mutation
   tag at registration time; the integration layer reaches that tag through
   `extension/op-tag` and passes it to `classify-form-tag` as an optional
   resolver. Keeping the core set pure of tool names stops the engine from
   owning extension policy."
  #{"update_plan" "plan_step" "fact_set" "fact_depends" "fact_contradicts"
    "fact_contradicts_remove" "done" "set_session_title"})
(def ^:private engine-form-heads
  "Bare-symbol heads whose RAW source row is engine-only chrome (no
   observable side effect, no answer payload). The UI hides these forms
   from user-facing traces; the engine still evaluates them and their
   return values still ride on the per-form envelope so the live ctx
   surfaces what the model saw.

   Strict subset of `core-mutation-heads`: every member is also a
   mutation. `introspect-*` is treated separately by
   `engine-form-src?` since it is an observation — silent UI but not a
   mutation.

   Single source of truth shared by `progress.clj` (live trace) and
   `channel-tui/chat.clj` (restored bubble) via `engine-form-src?`."
  #{"set_session_title" "done" "update_plan" "plan_step" "fact_set" "fact_depends"
    "fact_contradicts" "fact_contradicts_remove"})
(defn engine-form-src?
  "True when `src` is a top-level call whose head names an engine-only
   form: every member of `engine-form-heads`, plus the entire
   `introspect-*` family (resolved by name prefix — every introspect
   verb is engine-internal). False for plain sandbox code, tool calls,
   defs, observations.

   This is the canonical predicate UI layers should use to decide
   \"is this form silent chrome?\". It parses the head symbol rather than
   scanning the raw source string, which avoids false positives (a
   `\"(done x)\"` inside a string would otherwise have matched)."
  [src]
  (when-let [nm (form-head-name src)]
    (or (contains? engine-form-heads nm) (str/starts-with? nm "introspect_"))))
(defn classify-form-tag
  "Classify a form-source string as `:observation` or `:mutation`.

   1-arity: pure, engine-only. Returns `:mutation` when the head is a
   member of `core-mutation-heads`; everything else is `:observation`.
   Use this from contexts that have no access to the extension
   registry (tests, pure tools).

   2-arity: takes `head-tag-resolver`, an optional fn
   `(fn [^Symbol head]) -> :mutation | :observation | nil`. The
   resolver wins when it returns a non-nil tag; on nil the engine
   falls back to `core-mutation-heads`. The integration layer in
   `loop.clj` builds the resolver from `extension/op-tag` so every
   extension-declared tool (`patch`, `git/commit!`, anything new an
   extension ships) classifies correctly without the engine hard-
   coding its symbol."
  ([src] (classify-form-tag src nil))
  ([src head-tag-resolver]
   (let [nm (form-head-name src)]
     (or (when (and nm head-tag-resolver) (try (head-tag-resolver nm) (catch Throwable _ nil)))
       (if (and nm (contains? core-mutation-heads nm)) :mutation :observation)))))
;; =============================================================================
;; blocks→forms — project per-form data captured by the loop's eval pipeline
;; into the canonical engine envelope shape
;; =============================================================================
(defn- realize-trailer-value
  "Force-realize lazy seqs / nested IDeref refs in `v` so the trailer
   carries DATA, not computations. Without this the freeze-safe path on
   turn-end persists lazy seqs as `{:vis/ref :expr}`, and the next
   iteration / resume sees `#:vis{:ref :expr}` instead of the actual
   collection. Model then misreads the form as `empty` and reruns the
   probe.

   Walks the same shapes `loop/realize-value` covers: maps, vectors,
   sets, sequentials (LazySeq, lists). Plain scalars pass through. Depth
   bounded so a self-referential structure cannot loop."
  ([v] (realize-trailer-value v 8))
  ([v depth]
   (cond (or (nil? v) (zero? depth)) v
     (instance? clojure.lang.IDeref v) (try (realize-trailer-value (deref v) (dec depth))
                                         (catch Throwable _ v))
     (map? v) (into {} (map (fn [[k val]] [k (realize-trailer-value val (dec depth))])) v)
     (vector? v) (mapv #(realize-trailer-value % (dec depth)) v)
     (set? v) (into #{} (map #(realize-trailer-value % (dec depth))) v)
     (sequential? v) (doall (map #(realize-trailer-value % (dec depth)) v))
     :else v)))
(defn block->envelope
  "Project one loop-side block `{:code :result :error :channel}` plus its
   1-based position and the engine cursor into the per-form envelope
   shape:

     {:scope :tag :src :duration-ms :result :error :channel}

   `:src` carries the form's source text. `:tag` is derived from the source via
   `classify-form-tag`. `:result` is included only when the block has
   one (engine convention: drop on default/nil). `:error` is included
   only when the block errored. `:channel` is included only when the
   form actually called one or more extension tools. `:duration-ms` is
   derived from the loop's block envelope so persisted TUI replays keep
   the same per-form footer timing as live progress bubbles.

   When a form's raw return is a Var, deref it once so the trailer
   carries the bound value directly. Every result is also walked through
   `realize-trailer-value` so lazy seqs land as data, never as
   `#:vis{:ref :expr}` placeholders left over from persistence flattening
   unrealized seqs.

   Why `:channel` is carried through (regression: conversation
   11d4f817-fbd1-43ab-a6b4-052c8557af0a turn 2 \"show me ls\"): the
   model wraps tool calls in `(def r (ls \".\"))` per the engine
   contract (\"bind values to defs\"). Binding unwraps the tool
   envelope to its inner `:result` value before binding `r`, so the
   block's `:result` is a plain map without `:success?` and the TUI's
   `render-tool-result` cannot dispatch to the ls renderer — no
   widget/badge. The pre-rendered IR for every call already lives in
   the per-form channel-sink under `:channel`; carrying it onto the
   envelope lets the TUI replay paint the badge from the sink entry
   even after persistence + restore."
  ([block position cursor] (block->envelope block position cursor nil))
  ([block position cursor head-tag-resolver]
   (let [src (or (:code block) (:src block) "")
         scope (str "t" (:turn cursor) "/i" (:iter cursor) "/f" position)
         raw-result (:result block)
         ;; `(def NAME …)` returns a Var. `realize-trailer-value`
         ;; already derefs any `IDeref` it encounters, so explicit
         ;; def-shape detection is redundant: every form's result — Var,
         ;; atom, lazy seq, plain data — lands as fully realised data
         ;; in the trailer envelope, ready for prompt rendering and
         ;; introspection.
         result (realize-trailer-value raw-result)
         channel (seq (:channel block))
         duration-ms
         (when-let [envelope (:envelope block)]
           (when (and (nat-int? (:started-at-ms envelope)) (nat-int? (:finished-at-ms envelope)))
             (max 0 (- (long (:finished-at-ms envelope)) (long (:started-at-ms envelope))))))]
     (cond-> {:scope scope, :tag (classify-form-tag src head-tag-resolver), :src src}
       (some? duration-ms) (assoc :duration-ms duration-ms)
       (contains? block :result) (assoc :result result)
       (some? (:error block)) (assoc :error (:error block))
       channel (assoc :channel (vec channel))))))
(defn blocks->forms
  "Map a loop-side blocks vec into a vec of engine envelopes. `:cursor`
   is `{:turn :iter}` of THIS iter; each block gets a 1-based form
   position by its index in the vec.

   3-arity passes `head-tag-resolver` (see `classify-form-tag`) through
   to every `block->envelope` call so extension-declared mutation tools
   (`patch`, `git/commit!`, any symbol with inline `:tag` on its
   `vis/symbol` entry) classify correctly without the engine
   hard-coding their symbol set."
  ([blocks cursor] (blocks->forms blocks cursor nil))
  ([blocks {:keys [turn iter]} head-tag-resolver]
   (vec (map-indexed (fn [idx block]
                       (block->envelope block (inc idx) {:turn turn, :iter iter} head-tag-resolver))
          (or blocks [])))))
