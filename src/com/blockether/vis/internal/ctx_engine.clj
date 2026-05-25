(ns com.blockether.vis.internal.ctx-engine
  "Engine surface over CTX. Almost entirely pure — only validator-fn compile +
   run depend on SCI (and a per-source compile cache). Persistence, IO, and
   the provider live elsewhere and call into these fns.

   The pure subset:
     parse-scope-form, scope-compare, classify-scope, build-indexes,
     depends-on-cycle?, derive-progression, derive-warnings,
     derive-next-actions, apply-mutator, apply-done,
     reconcile-done-hook-tasks, advance-iter, advance-turn, gc-pass,
     introspect-*.

   The impure subset (clearly fenced; uses SCI + a cache atom):
     compile-validator-fn, run-validator-fn.

   Public surface (all pure):

     (build-indexes ctx)
       → {:req-index :proof-index :task-by-spec :fact-refs :dep-graph :rev-deps}

     (classify-scope scope-form cursor form-results)
       → one of :ok :unknown :errored :future-form :future-iter :future-turn :malformed

     (derive-progression ctx indexes)
       → {spec-id {:total :proven :ratio :state :missing}}

     (derive-warnings ctx indexes)
       → [{:level :anchor :code :message} …]   sorted, deduped

     (derive-next-actions ctx indexes progression last-mutation-map)
       → [{:type :target :priority :hint} …]   top-N ranked

     (apply-mutator ctx form-scope mutator args)
       → {:ctx :warnings :stamped?}            engine never throws on soft;
                                               hard rejects return :ctx unchanged +
                                               :warnings populated + :stamped? false

     (advance-iter ctx form-results-vec)
       → ctx with trailer pin appended and :session/scope advanced

     (reconcile-done-hook-tasks ctx form-results-map)
       → {:ctx :warnings} with hook-tasks whose :status :done failed
         validation reverted to :todo

     (advance-turn ctx)
       → ctx with bumped :session/turn, reset :session/scope, gc-pass run

     (gc-pass ctx)
       → ctx with terminal-status entries past TTL removed from live tree

   Mutator keywords accepted by `apply-mutator`:
     :spec-set! :task-set! :fact-set!
     :req-add! :req-update! :req-remove!
     :proof-add! :proof-remove!

   Hard rejects (engine writes nothing, warnings carry the reason):
     - malformed scope string anywhere
     - depends-on cycle introduced by task-set!
     - partial-overlap trailer-summarize at done-time

   Everything else is a soft warning surfaced via `derive-warnings` and inlined
   by the renderer as `;; ⚠ …` next to the offending entry. The engine NEVER
   refuses a write outside the three hard rules above.

   This file ships only declarations + helpers + the deterministic skeletons
   needed to start scenario testing. Each fn is implemented incrementally; the
   accompanying `ctx-engine-test` namespace drives the implementation order
   via REPL-replayable scenarios."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [edamame.core :as edamame]
            [com.blockether.vis.internal.tokens :as tokens]
            [sci.core :as sci]))

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
      {:turn (parse-long t) :iter (parse-long i) :form (parse-long f)})))

(defn malformed-scope?
  "True if `s` is a string but does not parse as `::cs/scope-form`."
  [s]
  (and (string? s) (nil? (parse-scope-form s))))

(defn scope-compare
  "Total order on form-scope strings by (turn, iter, form). Returns int.
   Compares parsed segments; malformed scopes sort before all valid ones to
   make their presence obvious in render."
  [a b]
  (let [pa (parse-scope-form a) pb (parse-scope-form b)]
    (cond
      (and (nil? pa) (nil? pb)) (compare (str a) (str b))
      (nil? pa) -1
      (nil? pb) 1
      :else
      (let [c1 (compare (:turn pa) (:turn pb))]
        (if (zero? c1)
          (let [c2 (compare (:iter pa) (:iter pb))]
            (if (zero? c2)
              (compare (:form pa) (:form pb))
              c2))
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
    (cond
      (nil? p)
      :malformed

      (> (:turn p) (:turn cursor))
      :future-turn

      (and (= (:turn p) (:turn cursor))
        (> (:iter p) (:iter cursor)))
      :future-iter

      (and (= (:turn p) (:turn cursor))
        (= (:iter p) (:iter cursor))
        (>= (:form p) (:next-form cursor)))
      :future-form

      :else
      (let [entry (cond
                    (map? form-results) (get form-results scope)
                    (set? form-results) (when (contains? form-results scope) {})
                    :else nil)]
        (cond
          (nil? entry)              :unknown
          (some? (:error entry))    :errored
          :else                     :ok)))))

;; =============================================================================
;; build-indexes — single source of derived state, used by every other pass
;; =============================================================================

(defn build-indexes
  "Compute every reverse / projection index used by warnings / progression /
   next-actions. Pure, idempotent, generator-property-testable.

   Returned shape (every value bounded by ctx size — no unbounded recursion):

     {:req-index    {requirement-id → {:spec spec-id :req requirement-map}}
      :proof-index  {[spec-id requirement-id] → [{:task task-id :scope scope-form} …]}
      :task-by-spec {spec-id → #{task-id …}}
      :fact-refs    {fact-id → #{requirement-id …}}      ; which reqs cite this fact
      :spec-by-task {task-id → #{spec-id …}}              ; mirror of :session.task/specs keys
      :dep-graph    {task-id → #{task-id …}}              ; depends-on edges
      :rev-deps     {task-id → #{task-id …}}              ; reverse depends-on
      :spec-status  {spec-id → status-keyword}
      :task-status  {task-id → status-keyword}
      :fact-status  {fact-id → status-keyword}}            ; :active when omitted

   Engine NEVER mutates ctx — indexes are throwaway computed fresh on demand.
   Cost is O(|specs|+|tasks|+|facts|) plus the proof and depends-on traversals,
   both bounded by the same totals. Cheap; rebuilt each render."
  [ctx]
  (let [specs (or (:session/specs ctx) {})
        tasks (or (:session/tasks ctx) {})
        facts (or (:session/facts ctx) {})

        req-index
        (into {}
          (for [[spec-id spec] specs
                req (or (:requirements spec) [])]
            [(:id req) {:spec spec-id :req req}]))

        proof-index
        (reduce-kv
          (fn [acc task-id task]
            (reduce-kv
              (fn [a spec-id proofs]
                (reduce
                  (fn [a' {:keys [requirement proof]}]
                    (update a' [spec-id requirement] (fnil conj [])
                      {:task task-id :scope proof}))
                  a
                  (or proofs [])))
              acc
              (or (:specs task) {})))
          {}
          tasks)

        task-by-spec
        (reduce-kv
          (fn [acc task-id task]
            (reduce (fn [a spec-id] (update a spec-id (fnil conj #{}) task-id))
              acc
              (keys (or (:specs task) {}))))
          {}
          tasks)

        spec-by-task
        (into {}
          (for [[task-id task] tasks]
            [task-id (set (keys (or (:specs task) {})))]))

        fact-refs
        (reduce-kv
          (fn [acc _ {req :req}]
            (reduce (fn [a fid] (update a fid (fnil conj #{}) (:id req)))
              acc
              (or (:facts req) [])))
          {}
          req-index)

        dep-graph
        (into {}
          (for [[task-id task] tasks]
            [task-id (set (or (:depends-on task) []))]))

        rev-deps
        (reduce-kv
          (fn [acc task-id deps]
            (reduce (fn [a d] (update a d (fnil conj #{}) task-id))
              acc deps))
          {}
          dep-graph)

        spec-status (into {} (map (fn [[k v]] [k (:status v)])) specs)
        task-status (into {} (map (fn [[k v]] [k (:status v)])) tasks)
        fact-status (into {} (map (fn [[k v]] [k (or (:status v) :active)])) facts)]
    {:req-index    req-index
     :proof-index  proof-index
     :task-by-spec task-by-spec
     :spec-by-task spec-by-task
     :fact-refs    fact-refs
     :dep-graph    dep-graph
     :rev-deps     rev-deps
     :spec-status  spec-status
     :task-status  task-status
     :fact-status  fact-status}))

;; =============================================================================
;; depends-on cycle detection — pure DFS, no recursion-on-recursion
;; =============================================================================

(defn depends-on-cycle?
  "True iff dep-graph contains a directed cycle. Pure DFS with white/grey/black
   coloring. Returns the cycle path as a vec when found (or nil)."
  [dep-graph]
  (let [color (atom (zipmap (keys dep-graph) (repeat :white)))]
    (letfn [(visit [node path]
              (cond
                (= :grey (@color node))
                (vec (drop-while #(not= % node) (conj path node)))

                (= :black (@color node))
                nil

                :else
                (do
                  (swap! color assoc node :grey)
                  (or (some (fn [n] (visit n (conj path node)))
                        (sort (or (get dep-graph node) #{})))
                    (do (swap! color assoc node :black) nil)))))]
      (some #(visit % []) (sort (keys dep-graph))))))

;; =============================================================================
;; derive-progression — per-spec proof coverage, deterministic
;; =============================================================================

(defn- proof-valid?
  "Stub: a proof is 'valid' for progression accounting iff the proof scope
   exists in form-results and the referenced requirement is present on the
   spec. Validator-fn evaluation lives in a separate pass (T2) because it
   needs SCI and bounded eval — those are non-pure and handled outside this
   namespace. Progression here counts presence only; T2 narrows it later."
  [proof req-index form-results]
  (and (parse-scope-form (:scope proof))
    (contains? req-index (:requirement-id proof))
    (or (nil? form-results)                 ;; no form-results known yet → optimistic
      (contains? form-results (:scope proof)))))

(defn derive-progression
  "For each spec, count how many requirements have at least one proof entry
   that points to an executed form. Returns:

     {spec-id {:total N :proven N :ratio Double|nil :state kw :missing #{req-id}}}

   :state ∈ #{:empty :open :partial :ready}
   :empty when total = 0
   :open when proven = 0 and total > 0
   :partial when 0 < proven < total
   :ready when proven = total > 0

   `form-results` is optional. When nil, every proof scope is assumed
   executed (used for tests that don't simulate form_results). When passed,
   only proofs whose scope is in form-results count."
  ([ctx indexes] (derive-progression ctx indexes nil))
  ([ctx indexes form-results]
   (let [{:keys [req-index proof-index]} indexes
         specs (or (:session/specs ctx) {})]
     (into {}
       (for [[spec-id spec] specs
             :let [reqs   (or (:requirements spec) [])
                   total  (count reqs)
                   proven (set
                            (for [req reqs
                                  :let [proofs (get proof-index [spec-id (:id req)])]
                                  :when (some (fn [p]
                                                (proof-valid?
                                                  {:scope (:scope p)
                                                   :requirement-id (:id req)}
                                                  req-index form-results))
                                          (or proofs []))]
                              (:id req)))
                   hit (count proven)]]
         [spec-id
          {:total   total
           :proven  hit
           :ratio   (when (pos? total) (double (/ hit total)))
           :state   (cond
                      (zero? total) :empty
                      (= hit total) :ready
                      (pos? hit)    :partial
                      :else         :open)
           :missing (set (remove proven (map :id reqs)))}])))))

;; =============================================================================
;; Status terminal predicates + done-born stamping
;; =============================================================================

(def ^:private spec-terminal? #{:done :cancelled})
(def ^:private task-terminal? #{:done :cancelled})
(def ^:private fact-terminal? #{:superseded})

(defn- stamp-or-clear-done-born
  "Pure helper: if status is terminal and :done-born absent, stamp it; if
   status is non-terminal and :done-born present, clear it. Idempotent."
  [entry form-scope terminal?]
  (let [terminal-now? (terminal? (:status entry))
        has-stamp?    (contains? entry :done-born)]
    (cond
      (and terminal-now? (not has-stamp?))         (assoc entry :done-born form-scope)
      (and (not terminal-now?) has-stamp?)         (dissoc entry :done-born)
      :else                                        entry)))

;; =============================================================================
;; apply-mutator — dispatch + per-mutator handlers
;; =============================================================================

(defn- warn
  "Construct a single warning map. :code is a stable keyword; :anchor is the
   tuple of keys (and indices) addressing the offending entry; :message is a
   human-readable hint surfaced inline by the renderer."
  [code anchor message]
  {:code code :anchor anchor :message message})

(defn- apply-spec-set! [ctx form-scope [spec-k partial-map]]
  (let [path     [:session/specs spec-k]
        existing (get-in ctx path)
        merged   (cond-> (merge existing partial-map)
                   (nil? existing) (assoc :born form-scope))
        stamped  (stamp-or-clear-done-born merged form-scope spec-terminal?)]
    {:ctx (assoc-in ctx path stamped) :warnings [] :stamped? true}))

(defn- new-cycle?
  "Quick check: would adding deps to task introduce a cycle?"
  [ctx task-k deps]
  (let [dg (-> (build-indexes ctx)
             :dep-graph
             (assoc task-k (set deps)))]
    (some? (depends-on-cycle? dg))))

(defn- apply-task-set! [ctx form-scope [task-k partial-map]]
  (let [path        [:session/tasks task-k]
        existing    (get-in ctx path)
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
                         (and (nil? (:hook-id partial-map))
                           (= task-k (:hook-id existing)))))]
    (cond
      ;; Hard reject cycle BEFORE writing :depends-on
      (and (contains? partial-map :depends-on)
        (new-cycle? ctx task-k (:depends-on partial-map)))
      {:ctx ctx
       :warnings [(warn :depends-on-cycle [task-k]
                    (str "task " task-k " :depends-on " (:depends-on partial-map)
                      " would introduce a cycle; write refused"))]
       :stamped? false}

      hook-repeat?
      {:ctx ctx :warnings [] :stamped? false}

      :else
      (let [merged   (cond-> (merge existing partial-map)
                       (nil? existing) (assoc :born form-scope))
            stamped  (stamp-or-clear-done-born merged form-scope task-terminal?)
            ;; FSM rule: any non-:done status transition clears the
            ;; `:validated?` flag so a later return to :done forces a
            ;; fresh validator pass. Engine never sets `:validated?`
            ;; here — only `reconcile-done-hook-tasks` does.
            cleared  (if (and (contains? partial-map :status)
                           (not= :done (:status stamped)))
                       (dissoc stamped :validated?)
                       stamped)]
        {:ctx (assoc-in ctx path cleared) :warnings [] :stamped? true}))))

(def ^:private FACT_CONTENT_SOFT_LIMIT
  "Per-fact `:content` size cap (chars of `pr-str`) above which a soft
   warning fires. Facts ride into every prompt; large blobs belong in
   the trailer or behind `(introspect-form …)`. 2 KB is roughly 500
   tokens — enough headroom for a stable observation map, small enough
   to keep a 20-fact session under ~10k tokens total."
  2048)

(defn- apply-fact-set! [ctx form-scope [fact-k partial-map]]
  (let [path     [:session/facts fact-k]
        existing (get-in ctx path)
        merged   (cond-> (merge existing partial-map)
                   (nil? existing) (assoc :born form-scope))
        stamped  (stamp-or-clear-done-born merged form-scope fact-terminal?)
        content  (:content stamped)
        size     (when (some? content)
                   (try (count (pr-str content))
                     (catch Throwable _ 0)))]
    {:ctx       (assoc-in ctx path stamped)
     :warnings  (if (and size (> size FACT_CONTENT_SOFT_LIMIT))
                  [(warn :fact-content-too-large [fact-k]
                     (str "fact " fact-k " :content is " size " chars ("
                       "> " FACT_CONTENT_SOFT_LIMIT "); facts ride into every "
                       "prompt — keep them small, or summarize and reference "
                       "the original form via introspect-form."))]
                  [])
     :stamped?  true}))

(defn- apply-req-add! [ctx _form-scope [spec-k req]]
  (let [path     [:session/specs spec-k :requirements]
        existing (or (get-in ctx path) [])
        rid      (:id req)
        collide? (some #(= (:id %) rid) existing)]
    (cond
      (not (contains? ctx :session/specs))
      {:ctx ctx
       :warnings [(warn :req-add-no-spec [spec-k]
                    (str "req-add! target spec " spec-k " does not exist"))]
       :stamped? false}

      (nil? (get-in ctx [:session/specs spec-k]))
      {:ctx ctx
       :warnings [(warn :req-add-no-spec [spec-k]
                    (str "req-add! target spec " spec-k " does not exist"))]
       :stamped? false}

      collide?
      {:ctx ctx
       :warnings [(warn :req-add-collision [spec-k rid]
                    (str "requirement " rid " already exists on spec " spec-k
                      "; use req-update! to merge"))]
       :stamped? false}

      :else
      {:ctx (update-in ctx path (fnil conj []) req) :warnings [] :stamped? true})))

(defn- apply-req-update! [ctx _form-scope [spec-k rid partial-req]]
  (let [path     [:session/specs spec-k :requirements]
        existing (or (get-in ctx path) [])
        idx      (first (keep-indexed (fn [i r] (when (= (:id r) rid) i)) existing))
        id-warn  (when (contains? partial-req :id)
                   (warn :req-update-id-immutable [spec-k rid]
                     "req-update! cannot change :id; field ignored"))
        clean    (dissoc partial-req :id)]
    (cond
      (nil? idx)
      {:ctx ctx
       :warnings [(warn :req-update-missing [spec-k rid]
                    (str "requirement " rid " not found on spec " spec-k))]
       :stamped? false}

      :else
      {:ctx (update-in ctx path
              (fn [v] (vec (map-indexed (fn [i r] (if (= i idx) (merge r clean) r)) v))))
       :warnings (vec (remove nil? [id-warn]))
       :stamped? true})))

(defn- orphan-warnings-for-removed-req [ctx spec-k rid]
  (vec
    (for [[task-id task] (or (:session/tasks ctx) {})
          :let [proofs (get-in task [:specs spec-k])]
          :when (some? proofs)
          proof proofs
          :when (= (:requirement proof) rid)]
      (warn :req-removed-orphaned-proof
        [task-id spec-k rid]
        (str "task " task-id " proof for " rid "/" spec-k
          " orphaned (req removed); scope " (:proof proof))))))

(defn- apply-req-remove! [ctx _form-scope [spec-k rid]]
  (let [path     [:session/specs spec-k :requirements]
        existing (or (get-in ctx path) [])
        new-vec  (vec (remove #(= (:id %) rid) existing))
        warns    (orphan-warnings-for-removed-req ctx spec-k rid)]
    {:ctx (assoc-in ctx path new-vec) :warnings warns :stamped? true}))

(defn- apply-proof-add! [ctx _form-scope [task-k spec-k proof]]
  (let [task-path  [:session/tasks task-k :specs spec-k]
        existing   (or (get-in ctx task-path) [])
        spec-known (some? (get-in ctx [:session/specs spec-k]))
        req-known  (when spec-known
                     (some #(= (:id %) (:requirement proof))
                       (get-in ctx [:session/specs spec-k :requirements])))
        warns      (cond-> []
                     (not spec-known)
                     (conj (warn :proof-unknown-spec [task-k spec-k]
                             (str "proof-add! refs unknown spec " spec-k)))
                     (and spec-known (not req-known))
                     (conj (warn :proof-unknown-req [task-k spec-k (:requirement proof)]
                             (str "proof-add! refs unknown req " (:requirement proof)
                               " on spec " spec-k))))]
    {:ctx (assoc-in ctx task-path (conj existing proof))
     :warnings warns
     :stamped? true}))

(defn- apply-proof-remove! [ctx _form-scope [task-k spec-k rid]]
  (let [path     [:session/tasks task-k :specs spec-k]
        existing (or (get-in ctx path) [])
        new-vec  (vec (remove #(= (:requirement %) rid) existing))]
    {:ctx (assoc-in ctx path new-vec) :warnings [] :stamped? true}))

(defn apply-mutator
  "Apply a single mutator call to the CTX. Returns
   `{:ctx new-ctx :warnings vec :stamped? bool}`.
   On hard reject (cycle, malformed) :ctx is unchanged and :stamped? is false.
   On soft warn (collision, dangling ref) :ctx may still update; :warnings
   carry the diagnostic for the renderer."
  [ctx form-scope mutator args]
  (cond
    (malformed-scope? form-scope)
    {:ctx ctx
     :warnings [(warn :malformed-scope [form-scope]
                  (str "form-scope " form-scope " is malformed; write refused"))]
     :stamped? false}

    :else
    (case mutator
      :spec-set!     (apply-spec-set!     ctx form-scope args)
      :task-set!     (apply-task-set!     ctx form-scope args)
      :fact-set!     (apply-fact-set!     ctx form-scope args)
      :req-add!      (apply-req-add!      ctx form-scope args)
      :req-update!   (apply-req-update!   ctx form-scope args)
      :req-remove!   (apply-req-remove!   ctx form-scope args)
      :proof-add!    (apply-proof-add!    ctx form-scope args)
      :proof-remove! (apply-proof-remove! ctx form-scope args)
      ;; unknown mutator: soft warn, no write
      {:ctx ctx
       :warnings [(warn :unknown-mutator [mutator]
                    (str "unknown mutator " mutator))]
       :stamped? false})))

;; =============================================================================
;; Validator-fn eval — SCI-bridged, bounded
;; =============================================================================
;;
;; Defined BEFORE derive-warnings because the T2 validator pass consumes it.
;; We don't want a hard dep on the full Vis sandbox here. validator-fn source
;; strings are tiny pure expressions: `(fn [{:keys [result error scope src]}]
;; (…))`. A minimal SCI ctx with core arithmetic / set / coll predicates is
;; enough. The compile result is cached by source hash; eval has a coarse
;; timeout via future + future-cancel to guard against infinite loops.

(def ^:private validator-cache
  ;; {source-string → {:fn compiled-fn} or {:error msg}}
  ;; Compile cache. Pure in spirit: same source → same compiled fn.
  ;; Cached in an atom so the JIT cost is paid once per validator source.
  (atom {}))

(defn- compile-validator-fn*
  "Compile `src` as a single SCI expression that should evaluate to a fn.
   Returns `{:fn compiled-fn}` on success, `{:error msg}` on failure. The SCI
   ctx is intentionally minimal — validator-fns are tiny predicates over the
   form-result map; they don't need the full Vis sandbox."
  [src]
  (try
    (let [v (sci/eval-string src)]
      (if (fn? v)
        {:fn v}
        {:error (str "validator-fn source did not evaluate to a function (got "
                  (pr-str (type v)) ")")}))
    (catch Throwable t
      {:error (str "validator-fn compile error: " (ex-message t))})))

(defn compile-validator-fn
  "Cached front for `compile-validator-fn*`. Returns the cached entry."
  [src]
  (or (get @validator-cache src)
    (let [entry (compile-validator-fn* src)]
      (swap! validator-cache assoc src entry)
      entry)))

(defn run-validator-fn
  "Run a compiled validator with a 50ms timeout. Returns
     {:ok? bool :reason :timed-out|:threw|:falsy|:ok :detail …}
   When src failed to compile, returns {:ok? false :reason :compile-error :detail …}."
  ([src form-result-map] (run-validator-fn src form-result-map 50))
  ([src form-result-map timeout-ms]
   (let [entry (compile-validator-fn src)]
     (cond
       (:error entry)
       {:ok? false :reason :compile-error :detail (:error entry)}

       :else
       (let [fut (future
                   (try
                     (let [r ((:fn entry) form-result-map)]
                       {:result r})
                     (catch Throwable t {:threw (ex-message t)})))
             res (deref fut timeout-ms ::timeout)]
         (cond
           (= res ::timeout)
           (do (future-cancel fut)
             {:ok? false :reason :timed-out})

           (:threw res)
           {:ok? false :reason :threw :detail (:threw res)}

           (not (:result res))
           {:ok? false :reason :falsy :detail (:result res)}

           :else
           {:ok? true :reason :ok :detail (:result res)}))))))

;; =============================================================================
;; derive-warnings — render-time invariant passes over indexes
;; =============================================================================

(defn- pass-req-facts-refs
  "Invariant 2: requirement :facts entries must point to live facts."
  [ctx _indexes]
  (let [facts (or (:session/facts ctx) {})]
    (vec
      (for [[spec-id spec] (or (:session/specs ctx) {})
            req (or (:requirements spec) [])
            f   (or (:facts req) [])
            :when (nil? (get facts f))]
        (warn :req-fact-dangling [spec-id (:id req) f]
          (str "req " (:id req) " on spec " spec-id
            " refs nonexistent fact " f))))))

(defn- pass-task-specs-refs
  "Invariant 4: task :specs keys must point to live specs."
  [ctx _indexes]
  (let [specs (or (:session/specs ctx) {})]
    (vec
      (for [[task-id task] (or (:session/tasks ctx) {})
            sk (keys (or (:specs task) {}))
            :when (nil? (get specs sk))]
        (warn :task-spec-dangling [task-id sk]
          (str "task " task-id " :specs refs nonexistent spec " sk))))))

(defn- pass-task-depends-on-refs
  "Invariant 5: task :depends-on keys must point to live tasks."
  [ctx _indexes]
  (let [tasks (or (:session/tasks ctx) {})]
    (vec
      (for [[task-id task] tasks
            d (or (:depends-on task) [])
            :when (nil? (get tasks d))]
        (warn :task-dep-dangling [task-id d]
          (str "task " task-id " :depends-on refs nonexistent task " d))))))

(defn- pass-proof-req-refs
  "Invariant 7: every task proof :requirement must exist on the referenced
   spec. Tasks that hold proofs for requirements removed from the spec
   surface here even when no req-remove! was emitted (e.g. after spec-set!
   :requirements wholesale replace)."
  [ctx _indexes]
  (vec
    (for [[task-id task] (or (:session/tasks ctx) {})
          [sk proofs]    (or (:specs task) {})
          proof          proofs
          :let [req-ids (set (map :id (get-in ctx [:session/specs sk :requirements])))]
          :when (and (some? (get-in ctx [:session/specs sk]))
                  (not (contains? req-ids (:requirement proof))))]
      (warn :proof-unknown-req [task-id sk (:requirement proof)]
        (str "task " task-id " proof refs unknown req "
          (:requirement proof) " on spec " sk)))))

(defn- pass-spec-done-coverage
  "Invariant 10: spec :status :done ⇒ every requirement has ≥1 proof. Uses
   derive-progression for the count."
  [ctx indexes]
  (let [prog (derive-progression ctx indexes)]
    (vec
      (for [[spec-id spec] (or (:session/specs ctx) {})
            :when (= :done (:status spec))
            :let [p (get prog spec-id)]
            :when (and p (not= (:state p) :ready))
            rid (:missing p)]
        (warn :spec-done-unproven [spec-id rid]
          (str "spec " spec-id " :done but req " rid " has no valid proof"))))))

(defn- pass-task-done-deps
  "Invariant 11: task :status :done ⇒ every :depends-on target is terminal."
  [ctx _indexes]
  (let [tasks (or (:session/tasks ctx) {})]
    (vec
      (for [[task-id task] tasks
            :when (= :done (:status task))
            d (or (:depends-on task) [])
            :let [dep (get tasks d)]
            :when (and (some? dep) (not (task-terminal? (:status dep))))]
        (warn :task-done-pending-dep [task-id d]
          (str "task " task-id " :done but dep " d " is " (:status dep)))))))

(defn- pass-scope-classification
  "Invariant 8: proof scopes classify :ok against the cursor + form-results.
   form-results is optional — when nil the pass only flags :malformed /
   :future-* classes (which don't need executed-form knowledge)."
  ([ctx indexes] (pass-scope-classification ctx indexes nil))
  ([ctx _indexes form-results]
   (let [cursor (:session/scope ctx)]
     (vec
       (for [[task-id task] (or (:session/tasks ctx) {})
             [sk proofs]    (or (:specs task) {})
             proof          proofs
             :let [klass (classify-scope (:proof proof) cursor form-results)]
             :when (and (not= klass :ok)
                     ;; without form-results we can't tell :unknown / :errored apart
                     ;; from :ok; only complain about ones we can prove
                     (or form-results (not (#{:unknown :errored} klass))))]
         (warn :proof-scope-bad-class [task-id sk (:requirement proof) klass]
           (str "task " task-id " proof " (:proof proof)
             " classified " klass " relative to cursor")))))))

(def ^:private ARCHIVED_PROOFS_CAP_PER_TASK
  "Cap on `:archived-proofs` vec length per task. Older entries are dropped
   first when the cap is exceeded. 10 keeps recent rejection history
   readable without unbounded ctx growth across long sessions."
  10)

(defn- find-failed-task-proofs
  "Pure helper: returns a vec of `{:task-id :spec-id :req-id :proof
   :reason :detail}` for every task-level :specs proof whose req has a
   :validator-fn that REJECTS the form result at the proof scope.

   Skipped when `form-results` is nil (cannot judge without :result) or
   when the proof scope is not classified `:ok` (the scope-class pass
   complains separately). Used by both `pass-validators` (warnings) and
   `archive-failed-proofs` (mutation)."
  [ctx form-results]
  (if-not (map? form-results)
    []
    (let [cursor (:session/scope ctx)]
      (vec
        (for [[task-id task] (or (:session/tasks ctx) {})
              [sk proofs]    (or (:specs task) {})
              proof          proofs
              :let [spec (get-in ctx [:session/specs sk])
                    req  (some #(when (= (:id %) (:requirement proof)) %)
                           (:requirements spec))
                    src  (:validator-fn req)
                    klass (classify-scope (:proof proof) cursor form-results)]
              :when (and req src (= klass :ok))
              :let [envelope (get form-results (:proof proof))
                    res (run-validator-fn src envelope)]
              :when (not (:ok? res))]
          {:task-id task-id
           :spec-id sk
           :req-id  (:requirement proof)
           :proof   (:proof proof)
           :reason  (:reason res)
           :detail  (:detail res)})))))

(defn- pass-validators
  "Invariant 9: every task proof whose referenced requirement carries a
   `:validator-fn` source string must pass that validator against the form
   result at the proof scope. The engine evals the source in a bounded SCI
   sandbox (50ms timeout). Skipped when `form-results` is nil (we cannot
   judge without seeing the form's :result), or when the proof scope is
   not classified `:ok` (the scope-class pass already complained)."
  [ctx _indexes form-results]
  (mapv (fn [{:keys [task-id spec-id req-id proof reason detail]}]
          (warn :proof-validator-fail
            [task-id spec-id req-id reason]
            (str "proof " proof " fails validator on req "
              req-id " / spec " spec-id
              " (" reason
              (when detail (str ": " (pr-str detail))) ")")))
    (find-failed-task-proofs ctx form-results)))

(defn- archive-entry
  "Build the entry shape stored under `:archived-proofs`."
  [{:keys [spec-id req-id proof reason detail]} rejected-at rejected-by]
  (cond-> {:proof       proof
           :rejected-by rejected-by
           :rejected-at rejected-at}
    spec-id    (assoc :spec spec-id)
    req-id     (assoc :requirement req-id)
    reason     (assoc :reason (if (keyword? reason) reason (str reason)))
    detail     (assoc :detail (pr-str detail))))

(defn- append-archived-proof
  "Append `entry` to `existing` (vec of archive entries) capped at
   ARCHIVED_PROOFS_CAP_PER_TASK. Dedupes against an entry with the same
   `:proof` + `:rejected-at` + `:reason` so a single iter cannot stamp
   the same rejection twice. Drops oldest entries first on overflow."
  [existing entry]
  (let [existing (vec (or existing []))
        dup?     (some #(and (= (:proof %)       (:proof entry))
                          (= (:rejected-at %) (:rejected-at entry))
                          (= (:reason %)      (:reason entry)))
                   existing)]
    (if dup?
      existing
      (let [appended (conj existing entry)
            overflow (- (count appended) ARCHIVED_PROOFS_CAP_PER_TASK)]
        (if (pos? overflow)
          (vec (drop overflow appended))
          appended)))))

(defn archive-failed-task-proofs
  "End-of-iter mutator. Scans task-level proofs; for every one whose
   :validator-fn rejects the form-result, appends a rejection entry to
   the task's `:archived-proofs` vec. The proof itself is NOT removed
   from `:specs` — model owns the regular-task lifecycle. The model
   sees the archive next iter and decides whether to swap the proof or
   change strategy.

   Returns updated ctx. Idempotent within an iter (dedupe by
   :proof + :rejected-at + :reason). Pure."
  [ctx form-results]
  (let [failures   (find-failed-task-proofs ctx form-results)
        s          (:session/scope ctx)
        rejected-at (str "t" (:turn s) "/i" (:iter s))]
    (reduce (fn [c failure]
              (let [entry (archive-entry failure rejected-at :proof-validator-fail)]
                (update-in c
                  [:session/tasks (:task-id failure) :archived-proofs]
                  append-archived-proof entry)))
      ctx failures)))

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
  (when-let [src (some-> (:src form) str str/triml)]
    (when-let [[_ n] (re-find rebind-loop-name-re src)]
      n)))

(defn- pin-def-names
  [entry]
  (into #{}
    (keep form-def-name)
    (:forms entry)))

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
      (let [tail (subvec forms-pins
                   (max 0 (- (count forms-pins) rebind-loop-threshold)))
            name-sets (mapv pin-def-names tail)
            common (apply set/intersection name-sets)]
        (for [n (sort common)]
          (warn :trailer-rebind-loop [n]
            (str "`(def " n " …)` has been rebound "
              rebind-loop-threshold "+ iters in a row."
              " The current tactic is not converging—switch to a"
              " different probe or call `(done {:trailer-summarize …})`"
              " before another rebind.")))))))

(defn derive-warnings
  "Run every invariant pass and return a sorted, deduped vec of warning maps.
   `form-results` may be nil for off-line / write-time use; pass it from the
   loop's per-iter capture for render-time precision."
  ([ctx indexes] (derive-warnings ctx indexes nil))
  ([ctx indexes form-results]
   (->> (concat
          (pass-req-facts-refs        ctx indexes)
          (pass-task-specs-refs       ctx indexes)
          (pass-task-depends-on-refs  ctx indexes)
          (pass-proof-req-refs        ctx indexes)
          (pass-spec-done-coverage    ctx indexes)
          (pass-task-done-deps        ctx indexes)
          (pass-scope-classification  ctx indexes form-results)
          (pass-validators            ctx indexes form-results)
          (pass-rebind-loop           ctx indexes))
     distinct
     (sort-by (juxt :code (comp str :anchor)))
     vec)))

;; =============================================================================
;; derive-next-actions — top-N ranked, deterministic priority
;; =============================================================================

(defn derive-next-actions
  "Ranked suggestions. Pure fn. Priority categories (lower = higher):
     1 :fix-consistency  — spec :done with unproven; task :done with pending dep
                          (rendered as `:blocking? true` — the symbolic
                          critic says: fix these before generating new work)
     2 :work-unblocked-todo  — task :todo with all deps terminal
     3 :prove-requirement    — spec :partial / :open req without proof
     4 :revisit-stale        — task :doing (stale heuristic deferred to loop)

   Returns the full sorted vec. The renderer caps at NEXT_ACTIONS_BUDGET
   and surfaces the suppressed count so the model sees the backlog
   pressure instead of a silently-truncated list."
  ([ctx indexes progression] (derive-next-actions ctx indexes progression {}))
  ([ctx indexes progression _last-mutation-map]
   (let [{:keys [task-status dep-graph]} indexes
         specs (or (:session/specs ctx) {})
         tasks (or (:session/tasks ctx) {})
         consistency-actions
         (concat
           (for [[spec-id _] specs
                 :let [p (get progression spec-id)]
                 :when (and p (= (:state p) :partial) (= :done (get-in specs [spec-id :status])))]
             {:type :review-spec :target spec-id :priority 1 :blocking? true
              :hint (str "spec " spec-id " :done but "
                      (count (:missing p)) " req(s) unproven: "
                      (vec (sort (:missing p))))})
           (for [[task-id task] tasks
                 :when (= :done (:status task))
                 d (or (:depends-on task) [])
                 :let [dep (get tasks d)]
                 :when (and (some? dep) (not (task-terminal? (:status dep))))]
             {:type :review-task :target task-id :priority 1 :blocking? true
              :hint (str "task " task-id " :done but dep " d " is " (:status dep))}))
         todo-actions
         (for [[task-id task] tasks
               :when (= :todo (:status task))
               :let [deps     (get dep-graph task-id #{})
                     deps-ok? (every? #(let [s (get task-status %)]
                                         (or (nil? s) (task-terminal? s)))
                                deps)]
               :when deps-ok?]
           {:type :work-unblocked-todo :target task-id :priority 2
            :hint (str "task " task-id " is :todo with all deps :done/:cancelled")})
         prove-actions
         (for [[spec-id _] specs
               :let [p (get progression spec-id)]
               :when (and p (#{:partial :open} (:state p)))
               rid (sort (:missing p))]
           {:type :prove-requirement :target [spec-id rid] :priority 3
            :hint (str "spec " spec-id " req " rid " needs a task proof")})]
     (->> (concat consistency-actions todo-actions prove-actions)
       (sort-by (juxt :priority (comp str :target)))
       vec))))

;; =============================================================================
;; advance-iter / advance-turn / gc-pass
;; =============================================================================

(defn- mutation-form?
  [form]
  (= :mutation (:tag form)))

(defn- observation-only-trailer-entry?
  [entry]
  (let [forms (:forms entry)]
    (and (seq forms) (every? #(= :observation (:tag %)) forms))))

(defn- prune-stale-observation-pins
  "When current iter mutates state, previous observation-only pins become
   stale. Keep them while no mutation has happened; drop them at first later
   mutation so the trailer carries tool observations only as long as their
   underlying world is unchanged. Mixed pins and summaries stay model-owned."
  [trailer iter-scope current-forms]
  (if-not (some mutation-form? current-forms)
    (vec (or trailer []))
    (let [current-form-scope (str iter-scope "/f1")]
      (vec
        (remove (fn [entry]
                  (and (observation-only-trailer-entry? entry)
                    (:scope entry)
                    (neg? (scope-compare (str (:scope entry) "/f1")
                            current-form-scope))))
          (or trailer []))))))

(defn- iteration-lifetime-hook-task?
  "True for hook-tasks declared `:lifetime :iteration`. These are
   hyper-transient signals that evaporate at the next iter boundary,
   not just the next turn boundary. Use for hooks whose firing
   condition is recomputed every iter from per-iter state (e.g. a
   one-iter retry-shape warning that should not haunt the model after
   the next provider call). If the condition still holds on the next
   iter, the hook re-creates the task; if not, it stays gone."
  [entry]
  (and (= :hook (:source entry))
    (= :iteration (:lifetime entry))))

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
   if a run truly bloats, `(done {:trailer-summarize …})` is the contract
   to collapse the history."
  [ctx form-results-vec]
  (let [cursor    (:session/scope ctx)
        iter-scope (str "t" (:turn cursor) "/i" (:iter cursor))
        keepable  (vec (remove #(str/starts-with? (str (:src %)) "(done")
                         form-results-vec))
        trailer'  (prune-stale-observation-pins
                    (:session/trailer ctx) iter-scope keepable)
        tasks'    (into {}
                    (for [[k v] (or (:session/tasks ctx) {})
                          :when (not (iteration-lifetime-hook-task? v))]
                      [k v]))
        ctx*      (-> ctx
                    (assoc :session/trailer trailer')
                    (assoc :session/tasks tasks'))
        ctx'      (if (seq keepable)
                    (update ctx* :session/trailer (fnil conj [])
                      {:scope iter-scope :forms keepable})
                    ctx*)]
    (assoc ctx' :session/scope
      (-> cursor (update :iter inc) (assoc :next-form 1)))))

;; --- GC TTL constants ----------------------------------------------------

(def ^:private TTL-TASK-DONE 6)
(def ^:private TTL-TASK-CANCELLED 10)
(def ^:private TTL-SPEC-DONE 6)
(def ^:private TTL-SPEC-CANCELLED 10)
(def ^:private TTL-FACT-SUPERSEDED 6)

(defn- ttl-for [entity-type status]
  (case [entity-type status]
    [:task :done]       TTL-TASK-DONE
    [:task :cancelled]  TTL-TASK-CANCELLED
    [:spec :done]       TTL-SPEC-DONE
    [:spec :cancelled]  TTL-SPEC-CANCELLED
    [:fact :superseded] TTL-FACT-SUPERSEDED
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
   transient warning lingered as a `:done :validated? false` task for
   6 turns and the model kept emitting `(task-set! … :done)` every
   turn to silence stale CTX chrome it could never actually resolve
   (Vis conv 11d4f817 / t14–t16)."
  [entry]
  (and (= :hook (:source entry))
    (= :turn (:lifetime entry))))

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
  (let [t (:turn (:session/scope ctx))
        gc (fn [subtree etype]
             (into {}
               (for [[k v] (or (get ctx subtree) {})
                     :when (not (entry-due-for-archive? t etype v))]
                 [k v])))
        gc-tasks (fn []
                   (into {}
                     (for [[k v] (or (:session/tasks ctx) {})
                           :when (not (or (entry-due-for-archive? t :task v)
                                        (turn-lifetime-hook-task? v)))]
                       [k v])))]
    (-> ctx
      (assoc :session/specs (gc :session/specs :spec))
      (assoc :session/tasks (gc-tasks))
      (assoc :session/facts (gc :session/facts :fact)))))

(defn advance-turn
  "Bump :session/turn, reset :session/scope to {:turn next :iter 1 :next-form 1},
   then run gc-pass. Caller (engine integration layer) is responsible for
   Nippy-snapshotting CTX to session_turn_state.ctx BEFORE calling this."
  [ctx]
  (let [next-turn (inc (or (:session/turn ctx) 0))]
    (-> ctx
      (assoc :session/turn next-turn)
      (assoc :session/scope {:turn next-turn :iter 1 :next-form 1})
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

   D12: no `:session/hints` — hook-emitted soft work items live as
   hook-sourced tasks under `:session/tasks` (`:source :hook`,
   `:hook-id`, `:importance`, `:validator-fn`, `:proof`)."
  ([] (empty-ctx "test-session"))
  ([session-id]
   {:session/id        session-id
    :session/turn      1
    :session/scope     {:turn 1 :iter 1 :next-form 1}
    ;; VCS-agnostic empty workspace. Detectors (foundation hook /
    ;; workspace module) stamp real `:vcs/kind` + branch/head/stats
    ;; once the session resolves to a real git/hg/jj repo. Until then
    ;; the engine treats absence as `:vcs/kind :none` — no branch name
    ;; baked in, no "main" sentinel that would lie about non-git
    ;; workspaces.
    :session/workspace {:vcs/kind :none}
    :session/symbols   {}
    :session/specs     {}
    :session/tasks     {}
    :session/facts     {}
    :session/trailer   []
    :engine/warnings   []}))

(defn strip-ephemeral
  "Remove every `:engine/*` key from a ctx. Call before Nippy-snapshotting
   to persistence so transient mutator state (warnings, pending satisfy
   requests) does not leak into the durable record."
  [ctx]
  (when ctx
    (into {} (remove (fn [[k _]] (and (keyword? k) (= "engine" (namespace k)))) ctx))))

;; =============================================================================
;; Iter-scope parsing + comparator — used by trailer comparator + overlap check
;; =============================================================================

(def ^:private scope-iter-re #"^t([1-9][0-9]*)/i([1-9][0-9]*)$")

(defn parse-scope-iter
  "Parse `tN/iM` into `{:turn :iter}` or nil."
  [s]
  (when (string? s)
    (when-let [[_ t i] (re-matches scope-iter-re s)]
      {:turn (parse-long t) :iter (parse-long i)})))

(defn- iter-compare
  "Total order on iter-scope strings by (turn, iter). Malformed sorts first."
  [a b]
  (let [pa (parse-scope-iter a) pb (parse-scope-iter b)]
    (cond
      (and (nil? pa) (nil? pb)) (compare (str a) (str b))
      (nil? pa) -1
      (nil? pb) 1
      :else
      (let [c (compare (:turn pa) (:turn pb))]
        (if (zero? c) (compare (:iter pa) (:iter pb)) c)))))

(defn iter-in-range?
  "True iff iter-scope `s` falls inside [start, end] inclusive (per iter-compare)."
  [s start end]
  (and (<= (iter-compare start s) 0)
    (<= (iter-compare s end) 0)))

(defn- iter-ranges-partial-overlap?
  "True iff [a1, a2] and [b1, b2] share points but neither contains the other."
  [a1 a2 b1 b2]
  (let [overlap?   (and (<= (iter-compare a1 b2) 0)
                     (<= (iter-compare b1 a2) 0))
        a-in-b?    (and (<= (iter-compare b1 a1) 0)
                     (<= (iter-compare a2 b2) 0))
        b-in-a?    (and (<= (iter-compare a1 b1) 0)
                     (<= (iter-compare b2 a2) 0))]
    (and overlap? (not a-in-b?) (not b-in-a?))))

;; =============================================================================
;; done handler — apply :trailer-drop + :trailer-summarize
;; =============================================================================

(defn- entry-iter-range
  "Return [start end] iter-scope strings for any trailer entry (pin or summary)."
  [e]
  (cond
    (contains? e :scope)        [(:scope e) (:scope e)]
    (contains? e :scope-start)  [(:scope-start e) (:scope-end e)]
    :else                       nil))

(defn- entry-contained-by?
  [e [start end]]
  (when-let [[a b] (entry-iter-range e)]
    (and (<= (iter-compare start a) 0)
      (<= (iter-compare b end) 0))))

(defn- find-overlap-conflict
  "Return the existing summary that PARTIALLY overlaps with [start, end], or nil."
  [trailer start end]
  (some (fn [e]
          (when (contains? e :scope-start)
            (when (iter-ranges-partial-overlap? (:scope-start e) (:scope-end e)
                    start end)
              e)))
    trailer))

(defn apply-trailer-drop
  "Remove trailer entries that match any of the supplied scope keys.
   Pin scope keys are `tN/iM`. Summary scope keys are `tA/iX->tB/iY`."
  [trailer drops]
  (let [drop-set (set drops)
        keep? (fn [e]
                (cond
                  (contains? e :scope)
                  (not (contains? drop-set (:scope e)))
                  (contains? e :scope-start)
                  (let [key (str (:scope-start e) "->" (:scope-end e))]
                    (not (contains? drop-set key)))
                  :else true))]
    (vec (filter keep? trailer))))

(defn apply-trailer-summarize
  "Apply one or more summary directives. For each directive `{:scope-start
   :scope-end :summary}`:

     1. Validate :scope-start ≤ :scope-end (else soft-warn, skip)
     2. Reject if any existing summary PARTIALLY overlaps (HARD; warn, skip)
     3. Absorb fully-contained entries (drop them from trailer)
     4. Insert new summary with :born stamped to `form-scope`

   Returns `{:trailer new-trailer :warnings vec}`."
  [trailer summaries form-scope]
  (reduce
    (fn [{:keys [trailer warnings]} {:keys [scope-start scope-end summary]}]
      (cond
        (or (not (parse-scope-iter scope-start)) (not (parse-scope-iter scope-end)))
        {:trailer trailer
         :warnings (conj warnings
                     (warn :trailer-summarize-bad-scope [scope-start scope-end]
                       "trailer-summarize :scope-start / :scope-end must be valid iter-scopes"))}

        (pos? (iter-compare scope-start scope-end))
        {:trailer trailer
         :warnings (conj warnings
                     (warn :trailer-summarize-inverted [scope-start scope-end]
                       (str "trailer-summarize range " scope-start "->" scope-end
                         " is inverted")))}

        :else
        (if-let [conflict (find-overlap-conflict trailer scope-start scope-end)]
          {:trailer trailer
           :warnings (conj warnings
                       (warn :trailer-summarize-partial-overlap
                         [scope-start scope-end
                          (:scope-start conflict) (:scope-end conflict)]
                         (str "trailer-summarize " scope-start "->" scope-end
                           " partially overlaps existing summary "
                           (:scope-start conflict) "->" (:scope-end conflict)
                           "; write refused")))}
          {:trailer
           (-> trailer
             (->> (remove #(entry-contained-by? % [scope-start scope-end])))
             vec
             (conj {:scope-start scope-start
                    :scope-end   scope-end
                    :summary     summary
                    :born        form-scope}))
           :warnings warnings})))
    {:trailer trailer :warnings []}
    (or summaries [])))

(defn- sort-trailer
  "Sort by composite key: pin :scope, summary :scope-start. Stable order."
  [trailer]
  (vec (sort-by (fn [e]
                  (cond
                    (contains? e :scope)        [(:scope e) 0]
                    (contains? e :scope-start)  [(:scope-start e) 1]
                    :else                       ["" 2]))
         (fn [[a a-tag] [b b-tag]]
           (let [c (iter-compare a b)]
             (if (zero? c) (compare a-tag b-tag) c)))
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
;; the model already knows from `(done {:trailer-summarize …})`, plus
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

   Refuses to collapse the LAST trailer entry — there is no point
   compacting if the result is one giant summary; the model needs at
   least one verbatim recent pin to reason against.

   Returns `nil` when no batch can hit the target while leaving a tail
   (i.e. total is already under target, or only one pin exists)."
  [trailer target-tokens]
  (let [trailer  (vec (or trailer []))
        n        (count trailer)]
    (when (>= n 2)
      (let [sizes (mapv pin-tokens trailer)
            total (reduce + 0 sizes)]
        (when (> total target-tokens)
          ;; sweep k = 1..n-1 keeping at least one tail entry
          (loop [k 1
                 batch-tokens (long (nth sizes 0))]
            (let [remaining (- total batch-tokens)]
              (cond
                (<= remaining target-tokens)
                {:batch        (subvec trailer 0 k)
                 :kept         (subvec trailer k)
                 :tokens-freed batch-tokens}

                (>= (inc k) n) nil   ;; would have to absorb the tail; refuse

                :else
                (recur (inc k)
                  (+ batch-tokens (long (nth sizes k))))))))))))

(defn- batch-scope-range
  "Return `[scope-start scope-end]` covering a batch of pins. Works for
   forms pins (`:scope`) and existing summary stubs (`:scope-start` /
   `:scope-end`) so recursive summarization stays consistent."
  [batch]
  (let [scopes (mapcat (fn [p]
                         (cond
                           (:scope p)        [(:scope p)]
                           (:scope-start p)  [(:scope-start p) (:scope-end p)]
                           :else             []))
                 batch)]
    [(first scopes) (last scopes)]))

(defn dummy-summary-text
  "Deterministic fallback summary string when no companion LLM is
   available (unconfigured, timed out, errored). Carries enough
   anchors that the model can recover details via introspection."
  [batch]
  (let [n-iters (count batch)
        n-forms (reduce + 0 (map (fn [p] (count (:forms p []))) batch))
        [s e]   (batch-scope-range batch)]
    (str "auto-summarized; " n-iters " iter(s), " n-forms
      " form(s); reach details via (introspect-iter \"" s "\")"
      (when (not= s e) (str " … (introspect-iter \"" e "\")")) ".")))

(defn make-summary-stub
  "Build the summary stub for an oldest-batch summarization.
   `summary-source` is `:companion-llm` (semantic) or `:engine-dummy`
   (deterministic). `summary-text` MUST be a non-blank string — callers
   that lose the companion call fall back to `dummy-summary-text`
   before reaching here."
  [batch summary-text summary-source born-scope]
  (let [[s e] (batch-scope-range batch)]
    {:scope-start         s
     :scope-end           e
     :summary             summary-text
     :born                born-scope
     :vis/auto?           true
     :vis/summary-source  summary-source}))

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
        pick    (pick-oldest-batch-for-summarization trailer target-tokens)]
    (if-not pick
      {:ctx ctx :compacted? false}
      (let [{:keys [batch kept tokens-freed]} pick
            [scope-start scope-end] (batch-scope-range batch)
            summarized (when summarizer-fn
                         (try (summarizer-fn batch) (catch Throwable _ nil)))
            summary    (or (:summary summarized)
                         (dummy-summary-text batch))
            source     (or (:source summarized) :engine-dummy)
            stub       (make-summary-stub batch summary source born-scope)
            ;; preserve sort order: stub takes the oldest slot, kept pins
            ;; stay in their original order (already sorted oldest-first)
            new-trail  (vec (cons stub kept))]
        {:ctx          (assoc ctx :session/trailer new-trail)
         :compacted?   true
         :tokens-freed tokens-freed
         :batch-size   (count batch)
         :scope-range  [scope-start scope-end]
         :warnings     [(warn :trailer-auto-summarized
                          [scope-start scope-end]
                          (str "Trailer auto-summarized: " (count batch)
                            " oldest iter(s) (" scope-start
                            (when (not= scope-start scope-end)
                              (str " .. " scope-end))
                            ") collapsed into one "
                            (case source
                              :companion-llm "companion-LLM summary"
                              "engine fallback summary")
                            ". Full data via (introspect-iter \""
                            scope-start "\")."))]}))))

;; =============================================================================
;; reconcile-done-hook-tasks — validate hook-task :done transitions
;; =============================================================================
;;
;; Hook-sourced tasks (D12) carry a task-level `:validator-fn` source
;; string plus a `:proof` scope-form the model writes when flipping the
;; task to `:status :done`. The validator must see the form envelope at
;; that proof scope, which is only fully available after the iter's
;; trailer pin lands. Hence this pass runs end-of-iter.
;;
;; Behaviour for every task with :source :hook, :status :done, and
;; :validator-fn present:
;;
;;   - :proof missing or not a string             → revert to :todo, warn
;;                                                  `:task-done-no-proof`
;;   - :proof malformed (not tN/iM/fK)            → revert + warn
;;                                                  `:task-done-proof-malformed`
;;   - :proof classifies :future-* / :errored     → revert + warn
;;                                                  `:task-done-proof-future` /
;;                                                  `:task-done-proof-errored`
;;   - :proof scope unknown in form-results       → revert + warn
;;                                                  `:task-done-proof-unknown`
;;   - validator-fn returns falsy / throws / etc. → revert + warn
;;                                                  `:task-done-validator-fail`
;;   - validator-fn passes                        → :done sticks (already validated
;;                                                  on a previous iter? —
;;                                                  idempotent, just re-verifies)
;;
;; Tasks without :source :hook are NOT touched by this pass. Tasks with
;; :source :hook but NO :validator-fn (legacy / user-created hook tasks)
;; are NOT touched either — only the contract of
;; \"hook ships validator-fn\" is enforced. Pure: takes ctx + form-results
;; map, returns `{:ctx ctx' :warnings […]}`.

(defn- revert-done-hook-task
  "Flip a hook-task back to :todo, archive the rejected `:proof` into
   `:archived-proofs`, then drop the un-validated `:proof` and the
   engine-stamped `:done-born`. `archive-meta` carries
   `{:rejected-by :reason :detail :rejected-at}` so the archive entry
   captures WHY the proof was rejected, not just THAT it was. Returns
   the rebuilt task map."
  [task archive-meta]
  (let [proof    (:proof task)
        entry    (when (and proof archive-meta)
                   (archive-entry
                     {:proof   proof
                      :reason  (:reason archive-meta)
                      :detail  (:detail archive-meta)}
                     (:rejected-at archive-meta)
                     (:rejected-by archive-meta)))
        archived (cond-> (vec (or (:archived-proofs task) []))
                   entry (append-archived-proof entry))]
    (cond-> task
      true       (assoc :status :todo)
      (seq archived) (assoc :archived-proofs archived)
      true       (dissoc :done-born :proof))))

(defn reconcile-done-hook-tasks
  "Validate every hook-task that landed at `:status :done` this turn.
   On any failure (missing/bad proof, validator fail), engine reverts
   the task to `:todo`, drops `:proof` + `:done-born`, and emits a
   warning. Pure. Called end-of-iter by the loop after advance-iter.

   FSM safety: tasks already carrying `:validated? true` (i.e. a prior
   reconcile passed) are SKIPPED. Without this guard a later
   `(done {:trailer-drop […]})` that wipes the proof envelope would
   retro-actively revert the satisfaction. `:validated?` is cleared by
   `apply-task-set!` on any non-:done transition so re-entry to :done
   triggers a fresh validation pass."
  [ctx form-results-map]
  (let [cursor      (:session/scope ctx)
        rejected-at (str "t" (:turn cursor) "/i" (:iter cursor))
        tasks       (or (:session/tasks ctx) {})
        per-task
        (for [[tk task] tasks
              :when (and (= :hook (:source task))
                      (= :done (:status task))
                      (string? (:validator-fn task))
                      (not (:validated? task)))]
          (let [proof (:proof task)
                outcome
                (cond
                  (or (nil? proof) (not (string? proof)))
                  {:revert? true
                   :archive {:rejected-by :task-done-no-proof :rejected-at rejected-at
                             :reason :missing-proof}
                   :warn (warn :task-done-no-proof [tk]
                           (str "task " tk " :status :done lacks :proof scope; reverting to :todo"))}

                  (malformed-scope? proof)
                  {:revert? true
                   :archive {:rejected-by :task-done-proof-malformed :rejected-at rejected-at
                             :reason :malformed-scope}
                   :warn (warn :task-done-proof-malformed [tk proof]
                           (str "task " tk " :proof " proof
                             " is not a valid tN/iM/fK form-scope; reverting to :todo"))}

                  :else
                  (let [klass (classify-scope proof cursor form-results-map)]
                    (cond
                      (#{:future-form :future-iter :future-turn} klass)
                      {:revert? true
                       :archive {:rejected-by :task-done-proof-future :rejected-at rejected-at
                                 :reason klass}
                       :warn (warn :task-done-proof-future [tk proof klass]
                               (str "task " tk " :proof " proof
                                 " is " (name klass) " relative to cursor; reverting to :todo"))}

                      (= klass :errored)
                      {:revert? true
                       :archive {:rejected-by :task-done-proof-errored :rejected-at rejected-at
                                 :reason :form-errored}
                       :warn (warn :task-done-proof-errored [tk proof]
                               (str "task " tk " :proof " proof
                                 " form errored; reverting to :todo"))}

                      (or (= klass :unknown) (nil? (get form-results-map proof)))
                      {:revert? true
                       :archive {:rejected-by :task-done-proof-unknown :rejected-at rejected-at
                                 :reason :no-form-result}
                       :warn (warn :task-done-proof-unknown [tk proof]
                               (str "task " tk " :proof " proof
                                 " has no captured form result; reverting to :todo"))}

                      :else
                      (let [envelope (get form-results-map proof)
                            res (run-validator-fn (:validator-fn task) envelope)]
                        (if (:ok? res)
                          {:revert? false}
                          {:revert? true
                           :archive {:rejected-by :task-done-validator-fail :rejected-at rejected-at
                                     :reason (:reason res) :detail (:detail res)}
                           :warn (warn :task-done-validator-fail
                                   [tk proof (:reason res)]
                                   (str "task " tk " :status :done at " proof
                                     " failed validator (" (name (:reason res))
                                     (when-let [d (:detail res)] (str ": " (pr-str d)))
                                     "); reverting to :todo"))})))))]
            [tk outcome]))
        reverts  (filter (fn [[_ {:keys [revert?]}]] revert?) per-task)
        passes   (filter (fn [[_ {:keys [revert?]}]] (not revert?)) per-task)
        ctx'     (-> ctx
                   (as-> c
                     (reduce (fn [acc [tk {:keys [archive]}]]
                               (update-in acc [:session/tasks tk]
                                 revert-done-hook-task archive))
                       c reverts))
                   (as-> c
                     (reduce (fn [acc [tk _]]
                               (assoc-in acc [:session/tasks tk :validated?] true))
                       c passes)))
        warns    (vec (keep (fn [[_ {:keys [warn]}]] warn) reverts))]
    {:ctx ctx' :warnings warns}))

(defn apply-done
  "Process a `(done {…})` form against the ctx trailer. Returns
   `{:ctx :warnings}`. The :answer field is NOT handled here — the loop owns
   sending it back to the channel. Engine only touches trailer + persistence.

   `args` is the map `{:answer? :trailer-drop? :trailer-summarize?}`."
  [ctx form-scope {:keys [trailer-drop trailer-summarize]}]
  (let [start  (or (:session/trailer ctx) [])
        after-drop (apply-trailer-drop start (or trailer-drop []))
        {:keys [trailer warnings]} (apply-trailer-summarize
                                     after-drop
                                     (or trailer-summarize [])
                                     form-scope)
        sorted (sort-trailer trailer)
        ;; flag scopes appearing in both drop AND summarize as a soft warn
        drop-set (set (or trailer-drop []))
        summarize-keys (set (for [s (or trailer-summarize [])]
                              (str (:scope-start s) "->" (:scope-end s))))
        conflict-keys (clojure.set/intersection drop-set summarize-keys)
        conflict-warns (for [k conflict-keys]
                         (warn :done-drop-summarize-conflict [k]
                           (str "scope " k " appears in both :trailer-drop and"
                             " :trailer-summarize; drop ignored, summary applied")))]
    {:ctx (assoc ctx :session/trailer sorted)
     :warnings (vec (concat conflict-warns warnings))}))

;; =============================================================================
;; History introspection — pure fns over a {turn-n → ctx-snapshot} map
;; =============================================================================
;;
;; `history` is a sorted map (or vec of [turn ctx] pairs) representing the
;; per-turn snapshots loaded from session_turn_state.ctx by the
;; integration layer (Nippy-decoded). Engine code never persists
;; here; it's the call site's job to load + pass the history map. These fns
;; project that data back to the model via the introspect-* surface.

(defn- snapshots-asc
  "Return [[turn ctx] …] sorted by turn ascending. Accepts a map or a vec."
  [history]
  (cond
    (map? history) (sort-by key history)
    (sequential? history) (sort-by first history)
    :else nil))

(defn introspect-spec
  "Latest snapshot in which spec `k` existed. Returns the entry plus
   `:as-of-turn N`, or nil when never present."
  [history k]
  (let [snaps (snapshots-asc history)]
    (some (fn [[turn ctx]]
            (when-let [entry (get-in ctx [:session/specs k])]
              (assoc entry :as-of-turn turn)))
      (reverse snaps))))

(defn introspect-task
  [history k]
  (let [snaps (snapshots-asc history)]
    (some (fn [[turn ctx]]
            (when-let [entry (get-in ctx [:session/tasks k])]
              (assoc entry :as-of-turn turn)))
      (reverse snaps))))

(defn introspect-failed-proofs
  "Return the `:archived-proofs` vec for task `k` from the latest snapshot
   in which it exists. Each entry: `{:proof :rejected-by :rejected-at
   :reason? :detail? :spec? :requirement?}`. Empty vec when the task
   exists but has no archived rejections. Nil when the task was never
   present in any snapshot."
  [history k]
  (let [snaps (snapshots-asc history)]
    (some (fn [[_ ctx]]
            (when-let [entry (get-in ctx [:session/tasks k])]
              (vec (or (:archived-proofs entry) []))))
      (reverse snaps))))

(defn introspect-fact
  [history k]
  (let [snaps (snapshots-asc history)]
    (some (fn [[turn ctx]]
            (when-let [entry (get-in ctx [:session/facts k])]
              (assoc entry :as-of-turn turn)))
      (reverse snaps))))

(defn- subtree-key-for-kind
  [kind]
  (case kind
    :specs :session/specs
    :tasks :session/tasks
    :facts :session/facts))

(defn introspect-archived
  "Return summaries of entries archived from the LATEST snapshot but present
   in some earlier one. `kind` is `:specs`/`:tasks`/`:facts`. Each entry:
     {:key k :as-of-turn N :status …}"
  [history kind]
  (let [snaps     (snapshots-asc history)
        subtree   (subtree-key-for-kind kind)
        latest    (some-> snaps last second (get subtree) keys set)
        latest    (or latest #{})]
    (vec
      (for [[turn ctx] (reverse snaps)
            :let [entries (or (get ctx subtree) {})
                  k+turn (for [[k v] entries
                               :when (not (contains? latest k))]
                           {:key k :as-of-turn turn :status (:status v)})]
            entry k+turn
            ;; dedupe: only emit a key the first turn we see it (latest
            ;; descending), so each archived key shows up once with its
            ;; latest-known turn.
            ]
        entry))))

(defn introspect-ctx-at
  "Full CTX snapshot at end of turn N. Nil when turn was never persisted."
  [history turn-n]
  (cond
    (map? history)        (get history turn-n)
    (sequential? history) (some (fn [[t ctx]] (when (= t turn-n) ctx)) history)
    :else                 nil))

;; =============================================================================
;; Form tag classification — derive :tag from the form source string
;; =============================================================================

(def ^:private head-edamame-opts
  "Tag-tolerant edamame parse opts. Mirrors `env/validation-edamame-opts`
   so a tagged-literal form (`#inst …`, `#some/tag …`) does not derail
   head extraction. Reader macros expand to `(do val)` which is exactly
   what we want — the head of an iter-top-level form is still detectable."
  {:all true
   :fn true
   :regex true
   :readers (fn [_tag] (fn [val] (list 'do val)))})

(defn form-head-symbol
  "Return the top-level head symbol of `src` (a Clojure source string)
   or nil when `src` is not a call form. Uses edamame so reader meta,
   discard forms (`#_`), comments, and tagged literals all behave the
   same way they do during real evaluation. Used by `classify-form-tag`
   and `engine-form-src?`; both must agree on what counts as the head,
   so there is exactly one implementation.

   Parse failure / non-list head / empty source → nil."
  [src]
  (try
    (let [forms (edamame/parse-string-all (or src "") head-edamame-opts)
          form  (first forms)]
      (when (and (seq? form) (symbol? (first form)))
        (first form)))
    (catch Throwable _ nil)))

(def ^:private core-mutation-heads
  "Engine-owned bare-symbol heads that classify a form as `:mutation`.
   These are the symbols the engine itself defines and recognises:

     • SCI def-shape forms (sandbox state writes)
     • CTX memory mutators (spec/task/fact + req/proof surface)
     • Control flow (done, set-session-title!)
     • Clojure native mutators that survive the sandbox
       (reset!, swap!, alter-var-root)

   Extension tools (`v/patch`, `v/write`, `git/commit!`, anything an
   extension ships) are NOT here. Extensions declare their own
   observation / mutation tag at registration time; the integration
   layer reaches that tag through `extension/op-tag` and passes it to
   `classify-form-tag` as an optional resolver. Keeping the core set
   pure of `v/*` heads stops the engine from owning extension policy."
  '#{def defn defmacro defmulti defmethod
     spec-set! task-set! fact-set!
     req-add! req-update! req-remove!
     proof-add! proof-remove!
     done set-session-title!
     reset! swap! alter-var-root})

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
  '#{set-session-title!
     done
     spec-set! task-set! fact-set!
     req-add! req-update! req-remove!
     proof-add! proof-remove!})

(defn engine-form-src?
  "True when `src` is a top-level call whose head names an engine-only
   form: every member of `engine-form-heads`, plus the entire
   `introspect-*` family (resolved by name prefix — every introspect
   verb is engine-internal). False for plain SCI code, tool calls,
   defs, observations.

   This is the canonical predicate UI layers should use to decide
   \"is this form silent chrome?\"; the legacy string-prefix scan over
   the raw source was a false-positive magnet (a `\"(done x)\"` inside
   a string would have matched)."
  [src]
  (when-let [sym (form-head-symbol src)]
    (let [nm (name sym)]
      (or (contains? engine-form-heads sym)
        (str/starts-with? nm "introspect-")))))

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
   extension-declared tool (`v/patch`, `git/commit!`, anything new an
   extension ships) classifies correctly without the engine hard-
   coding its symbol."
  ([src] (classify-form-tag src nil))
  ([src head-tag-resolver]
   (let [sym (form-head-symbol src)]
     (or (when (and sym head-tag-resolver)
           (try (head-tag-resolver sym) (catch Throwable _ nil)))
       (if (and sym (contains? core-mutation-heads sym))
         :mutation
         :observation)))))

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
   (cond
     (or (nil? v) (zero? depth)) v
     (instance? clojure.lang.IDeref v)
     (try (realize-trailer-value (deref v) (dec depth))
       (catch Throwable _ v))
     (map? v)        (into {} (map (fn [[k val]] [k (realize-trailer-value val (dec depth))])) v)
     (vector? v)     (mapv #(realize-trailer-value % (dec depth)) v)
     (set? v)        (into #{} (map #(realize-trailer-value % (dec depth))) v)
     (sequential? v) (doall (map #(realize-trailer-value % (dec depth)) v))
     :else           v)))

(defn block->envelope
  "Project one loop-side block `{:code :result :error :channel}` plus its
   1-based position and the engine cursor into the per-form envelope
   shape:

     {:scope :tag :src :result :error :channel}

   `:src` carries the form's source text; `:tag` is derived from the
   source via `classify-form-tag`. `:result` is included only when the
   block has one (engine convention: drop on default/nil). `:error` is
   included only when the block errored. `:channel` is included only
   when the form actually called one or more extension tools.

   For `(def NAME …)` forms the raw SCI return is the Var; deref it once
   so the trailer carries the bound value directly. Every result is also
   walked through `realize-trailer-value` so lazy seqs land as data—the
   model used to see `#:vis{:ref :expr}` after persistence flattened
   unrealized seqs.

   Why `:channel` is carried through (regression: conversation
   11d4f817-fbd1-43ab-a6b4-052c8557af0a turn 2 \"show me ls\"): the
   model wraps tool calls in `(def r (v/ls \".\"))` per the engine
   contract (\"bind values to defs\"). SCI's `def` unwraps the tool
   envelope to its inner `:result` value before binding `r`, so the
   block's `:result` is a plain map without `:success?` and the TUI's
   `render-tool-result` cannot dispatch to the v/ls renderer — no
   widget/badge. The pre-rendered IR for every call already lives in
   the per-form channel-sink under `:channel`; carrying it onto the
   envelope lets the TUI replay paint the badge from the sink entry
   even after persistence + restore."
  ([block position cursor]
   (block->envelope block position cursor nil))
  ([block position cursor head-tag-resolver]
   (let [src (or (:code block) (:src block) "")
         scope (str "t" (:turn cursor) "/i" (:iter cursor) "/f" position)
         raw-result (:result block)
         ;; `(def NAME …)` returns the SCI Var. `realize-trailer-value`
         ;; already derefs any `IDeref` it encounters, so explicit
         ;; def-shape detection is redundant: every form's result — Var,
         ;; atom, lazy seq, plain data — lands as fully realised data
         ;; in the trailer envelope, ready for prompt rendering and
         ;; introspection.
         result (realize-trailer-value raw-result)
         channel (seq (:channel block))]
     (cond-> {:scope scope
              :tag   (classify-form-tag src head-tag-resolver)
              :src   src}
       (contains? block :result) (assoc :result result)
       (some? (:error block))    (assoc :error  (:error block))
       channel                   (assoc :channel (vec channel))))))

(defn blocks->forms
  "Map a loop-side blocks vec into a vec of engine envelopes. `:cursor`
   is `{:turn :iter}` of THIS iter; each block gets a 1-based form
   position by its index in the vec.

   3-arity passes `head-tag-resolver` (see `classify-form-tag`) through
   to every `block->envelope` call so extension-declared mutation tools
   (`v/patch`, `git/commit!`, anything registered via `register-op!`)
   classify correctly without the engine hard-coding their symbol set."
  ([blocks cursor] (blocks->forms blocks cursor nil))
  ([blocks {:keys [turn iter]} head-tag-resolver]
   (vec
     (map-indexed
       (fn [idx block]
         (block->envelope block (inc idx) {:turn turn :iter iter} head-tag-resolver))
       (or blocks [])))))
