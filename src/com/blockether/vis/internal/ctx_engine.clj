(ns com.blockether.vis.internal.ctx-engine
  "Engine surface over CTX — a typed, dependency-checked working memory of
   tasks + facts. Entirely pure. Persistence, IO, and the provider live
   elsewhere and call into these fns.

   The engine keeps the graph consistent (cycle-free `:depends-on`, status
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

   Everything else is a soft warning surfaced via `derive-warnings` as a
   simple `:session/warnings` vec of short strings. The engine NEVER refuses
   a write outside the three hard rules above."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [edamame.core :as edamame]
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
        ;; ---------- Universal :depends-on ---------- dep-graph nodes are typed refs `[:kind
        ;; :K]` where :kind is one of #{:task :fact}. Edges are sets of typed refs. Bare-key
        ;; entries on a task / fact `:depends-on` are treated as same-kind shorthand (`:K` on a
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
                                 (assoc acc [:task task-id] (edges-from :task (:depends-on task))))
                               g
                               tasks)
                    (reduce-kv (fn [acc fact-id fact]
                                 (assoc acc [:fact fact-id] (edges-from :fact (:depends-on fact))))
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
(def ^:private task-terminal? #{:done :cancelled :archived})
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
  "Accept a `:depends-on` element and return a typed `[:kind :K]` ref or
   nil when the input is malformed. Bare keys are resolved to
   `default-kind`, matching the same-kind shorthand for
   `(task-set! :T {:depends-on [:other-task]})`."
  [default-kind ref]
  (cond (vector? ref) (let [[kind k] ref] (when (and (#{:task :fact} kind) (some? k)) [kind k]))
        (keyword? ref) [default-kind ref]
        :else nil))
(defn- new-cycle-on-node?
  "Would assigning `:depends-on` `new-deps` to the typed node `[kind k]`
   introduce a cycle in the unified dep-graph?"
  [ctx kind k new-deps]
  (let [normalized (into #{} (keep (partial normalize-dep-ref kind)) (or new-deps []))
        dg (-> (build-indexes ctx)
               :dep-graph
               (assoc [kind k] normalized))]
    (some? (depends-on-cycle? dg))))
(defn- new-cycle?
  "Task cycle check. Delegates to the universal `new-cycle-on-node?` so a
   task `:depends-on [:other-task]` normalizes via `[:task :other-task]`."
  [ctx task-k deps]
  (new-cycle-on-node? ctx :task task-k deps))
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
      ;; Hard reject cycle BEFORE writing :depends-on
      (and (contains? partial-map :depends-on) (new-cycle? ctx task-k (:depends-on partial-map)))
        {:ctx ctx,
         :warnings [(warn :depends-on-cycle
                          [task-k]
                          (str "task "
                               task-k
                               " :depends-on "
                               (:depends-on partial-map)
                               " would introduce a cycle; write refused"))],
         :stamped? false}
      hook-repeat? {:ctx ctx, :warnings [], :stamped? false}
      :else (let [merged (cond-> (merge existing partial-map)
                           (nil? existing) (assoc :born form-scope))
                  stamped (stamp-or-clear-done-born merged form-scope task-terminal?)]
              {:ctx (assoc-in ctx path stamped), :warnings [], :stamped? true}))))
(def ^:private FACT_CONTENT_SOFT_LIMIT
  "Per-fact `:content` size cap (chars of `pr-str`) above which a soft
   warning fires. Facts ride into every prompt; large blobs belong in
   the trailer or behind `(introspect-form …)`. 2 KB is roughly 500
   tokens — enough headroom for a stable observation map, small enough
   to keep a 20-fact session under ~10k tokens total."
  2048)
(defn- apply-fact-set!
  "Canonical fact upsert. Relations are DECLARATIVE keys on the map — the\n   map IS the desired state. `:depends-on` and `:contradicts`, when present,\n   REPLACE the entity's full edge set (absent key = leave untouched). For\n   `:contradicts` the symmetric back-links are reconciled: links the new\n   vector drops are removed from BOTH facts; links it adds are written to\n   both. This makes `(fact-set! :K {:contradicts [...]})` the single verb\n   for declaring AND retracting contradictions — no standalone mutator."
  [ctx form-scope [fact-k partial-map]]
  (cond
    (and (contains? partial-map :depends-on)
         (new-cycle-on-node? ctx :fact fact-k (:depends-on partial-map)))
      {:ctx ctx,
       :warnings [(warn :depends-on-cycle
                        [:fact fact-k]
                        (str "fact "
                             fact-k
                             " :depends-on "
                             (:depends-on partial-map)
                             " would introduce a cycle; write refused"))],
       :stamped? false}
    :else
      (let [has-contras? (contains? partial-map :contradicts)
            desired-raw (let [c (:contradicts partial-map)]
                          (if (coll? c) (vec c) (when (some? c) [c])))
            partial-map (dissoc partial-map :contradicts)
            path [:session/facts fact-k]
            existing (get-in ctx path)
            merged (cond-> (merge existing partial-map) (nil? existing) (assoc :born form-scope))
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
   `:depends-on` rather than merging — the model owns the full vec each
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
               [(warn :depends-on-missing-entity
                      [kind k]
                      (str (name kind) " " k " does not exist; " (name kind) "-depends! ignored"))],
             :stamped? false}
          (new-cycle-on-node? ctx kind k deps)
            {:ctx ctx,
             :warnings [(warn :depends-on-cycle
                              [kind k]
                              (str (name kind)
                                   " "
                                   k
                                   " :depends-on "
                                   deps
                                   " would introduce a cycle; write refused"))],
             :stamped? false}
          :else {:ctx (assoc-in ctx (conj path :depends-on) (vec deps)),
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
                :task-set! (apply-task-set! ctx form-scope args)
                :fact-set! (apply-fact-set! ctx form-scope args)
                :task-depends! (apply-depends! ctx form-scope (cons :task args))
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
  "Structural pass: task/fact `:depends-on` refs must point to a live
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
               raw-dep (or (:depends-on entity) [])
               :let [ref (normalize-dep-ref default-kind raw-dep)]
               :when (and ref (not (ref-exists? ref)))]
           (warn :depends-on-dangling
                 [default-kind entity-id ref]
                 (str (name default-kind)
                      " " entity-id
                      " :depends-on refs nonexistent " (name (first ref))
                      " " (second ref)))))))
(defn- pass-task-done-deps
  "Structural pass: task :status :done while a :depends-on target is
   non-terminal. Soft only — done is self-asserted and never reverted."
  [ctx _indexes]
  (let [tasks (or (:session/tasks ctx) {})]
    (vec (for [[task-id task] tasks
               :when (= :done (:status task))
               d (or (:depends-on task) [])
               :let [dep (get tasks d)]
               :when (and (some? dep) (not (task-terminal? (:status dep))))]
           (warn :task-done-pending-dep
                 [task-id d]
                 (str "task " task-id " :done but dep " d " is " (:status dep)))))))
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
                     " different probe or call `(done {:trailer-summarize …})`"
                       " before another rebind.")))))))
(defn derive-warnings
  "Run the STRUCTURAL invariant passes and return a sorted, deduped vec of
   short warning STRINGS — the `:session/warnings` shape surfaced verbatim
   in the rendered ctx (no stages, no `;; ⚠` inside EDN). Each pass yields
   `{:code :anchor :message}` maps internally for stable ordering; only the
   `:message` strings are projected out.

   Passes:
     - dep-target-exists      (`pass-task-depends-on-refs`)
     - contradicting-facts    (`pass-contradicting-facts`)
     - rebind-loop            (`pass-rebind-loop`)
     - task-done-with-non-terminal-dep (`pass-task-done-deps`)

   `form-results` arg is accepted for call-site compatibility but the
   structural passes do not consult it."
  ([ctx indexes] (derive-warnings ctx indexes nil))
  ([ctx indexes _form-results]
   (->> (concat (pass-contradicting-facts ctx indexes)
                (pass-task-depends-on-refs ctx indexes)
                (pass-task-done-deps ctx indexes)
                (pass-rebind-loop ctx indexes))
        distinct
        (sort-by (juxt :code (comp str :anchor)))
        (mapv :message))))
;; =============================================================================
;; advance-iter / enter-turn / gc-pass
;; =============================================================================
(defn- mutation-form? [form] (= :mutation (:tag form)))
(defn- observation-only-trailer-entry?
  [entry]
  (let [forms (:forms entry)] (and (seq forms) (every? #(= :observation (:tag %)) forms))))
(defn- prune-stale-observation-pins
  "When current iter mutates state, previous observation-only pins become
   stale. Keep them while no mutation has happened; drop them at first later
   mutation so the trailer carries tool observations only as long as their
   underlying world is unchanged. Mixed pins and summaries stay model-owned."
  [trailer iter-scope current-forms]
  (if-not (some mutation-form? current-forms)
    (vec (or trailer []))
    (let [current-form-scope (str iter-scope "/f1")]
      (vec (remove (fn [entry]
                     (and (observation-only-trailer-entry? entry)
                          (:scope entry)
                          (neg? (scope-compare (str (:scope entry) "/f1") current-form-scope))))
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
   if a run truly bloats, `(done {:trailer-summarize …})` is the contract
   to collapse the history."
  [ctx form-results-vec]
  (let [cursor (:session/scope ctx)
        iter-scope (str "t" (:turn cursor) "/i" (:iter cursor))
        ;; drop both `(done …)` AND forms whose result is
        ;; `:vis/silent`. Engine mutators (task-set!, fact-set!, etc.)
        ;; return `:vis/silent`; their effect lives in ctx subtree
        ;; mutations, not in the trailer pin log.
        keepable (vec (remove (fn [r]
                                (or (str/starts-with? (str (:src r)) "(done")
                                    (= :vis/silent (:result r))
                                    (:vis/silent r)))
                        form-results-vec))
        trailer' (prune-stale-observation-pins (:session/trailer ctx) iter-scope keepable)
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
(def ^:private TTL-FACT-SUPERSEDED 6)
(defn- ttl-for
  [entity-type status]
  (case [entity-type status]
    [:task :done] TTL-TASK-DONE
    [:task :cancelled] TTL-TASK-CANCELLED
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
        (assoc :session/tasks (gc-tasks))
        (assoc :session/facts (gc :session/facts :fact)))))
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
;; done handler — apply :trailer-drop + :trailer-summarize
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
(defn apply-trailer-drop
  "Remove trailer entries that match any of the supplied scope keys.
   Pin scope keys are `tN/iM`. Summary scope keys are `tA/iX->tB/iY`."
  [trailer drops]
  (let [drop-set (set drops)
        keep? (fn [e]
                (cond (contains? e :scope) (not (contains? drop-set (:scope e)))
                      (contains? e :scope-start) (let [key
                                                         (str (:scope-start e) "->" (:scope-end e))]
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
  (reduce (fn [{:keys [trailer warnings]} {:keys [scope-start scope-end summary]}]
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
                                    (conj {:scope-start scope-start,
                                           :scope-end scope-end,
                                           :summary summary,
                                           :born form-scope})),
                       :warnings warnings})))
    {:trailer trailer, :warnings []}
    (or summaries [])))
(defn- sort-trailer
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
  (let [trailer (vec (or trailer []))
        n (count trailer)]
    (when (>= n 2)
      (let [sizes (mapv pin-tokens trailer)
            total (reduce + 0 sizes)]
        (when (> total target-tokens)
          ;; sweep k = 1..n-1 keeping at least one tail entry
          (loop [k 1
                 batch-tokens (long (nth sizes 0))]
            (let [remaining (- total batch-tokens)]
              (cond (<= remaining target-tokens) {:batch (subvec trailer 0 k),
                                                  :kept (subvec trailer k),
                                                  :tokens-freed batch-tokens}
                    (>= (inc k) n) nil ;; would have to absorb the tail; refuse
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
(defn dummy-summary-text
  "Deterministic fallback summary string when no companion LLM is
   available (unconfigured, timed out, errored). Carries enough
   anchors that the model can recover details via introspection."
  [batch]
  (let [n-iters (count batch)
        n-forms (reduce + 0 (map (fn [p] (count (:forms p []))) batch))
        [s e] (batch-scope-range batch)]
    (str "auto-summarized; "
         n-iters
         " iter(s), "
         n-forms
         " form(s); reach details via (introspect-iter \""
         s
         "\")"
         (when (not= s e) (str " … (introspect-iter \"" e "\")"))
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
                               ". Full data via (introspect-iter \""
                               scope-start
                               "\")."))]}))))
(defn- apply-bulk-archive
  "bulk-archive entities listed in `(done {:archive {...}})`.
   `archive-map` keys: `:facts :tasks` → vec of entity keys. Each
   listed entity gets its `:status` flipped to `:archived` (new terminal
   status; cleanly distinct from `:done` / `:cancelled` / `:superseded`).
   Missing entities emit a soft warning and skip.

   Snapshots keep the raw entity; `introspect-archived :kind` returns
   them. Live ctx GC removes archived entries at the next turn boundary
   without waiting for TTL.

   Returns `{:ctx :warnings}`."
  [ctx form-scope archive-map]
  (let [{:keys [facts tasks]} (or archive-map {})
        kind->subtree {:fact :session/facts, :task :session/tasks}
        attempts (concat (for [k (or facts [])] [:fact k]) (for [k (or tasks [])] [:task k]))
        outcomes (for [[kind k] attempts]
                   (let [subtree (kind->subtree kind)
                         exists? (some? (get-in ctx [subtree k]))]
                     {:kind kind, :k k, :subtree subtree, :exists? exists?}))
        ctx-with-status (reduce (fn [acc {:keys [k subtree exists?]}]
                                  (cond-> acc exists? (assoc-in [subtree k :status] :archived)))
                          ctx
                          outcomes)
        ctx' ctx-with-status
        warns (vec (for [{:keys [kind k exists?]} outcomes
                         :when (not exists?)]
                     (warn
                       :done-archive-missing-entity
                       [kind k]
                       (str (name kind) " " k " listed in (done {:archive …}) does not exist"))))]
    {:ctx ctx', :warnings warns}))
(declare apply-done-impl)
(defn apply-done
  "Process a `(done {…})` form against the ctx trailer + entity archive.
   The :answer field IS handled here in Phase F: when present + non-blank,
   the engine auto-writes a `:turn-N-answer` fact under `:session/facts`
   carrying a one-line `:summary` and the full `:body`. Next turn's `;; ctx`
   EDN block surfaces that fact inside the cached prefix, so cross-turn answer
   reference is free — the model reads `(introspect-fact :turn-N-answer)`
   instead of vis re-sending `previous-turn-context-block` (deprecated).

   `args` map keys:
     :answer             markdown payload (Phase F: auto-fact + loop ships to channel)
     :answer-summary     optional one-line summary the model emits; falls
                         back to the first non-blank paragraph of :answer
                         when omitted.
     :trailer-drop       vec of `tN/iM` scopes to drop verbatim
     :trailer-summarize  vec of `{:scope-start :scope-end :summary}` pins
     :archive            `{:facts […] :specs […] :tasks […]}` bulk-flip
                         to `:archived` terminal; snapshots keep raw,
                         introspect-archived returns them.

   Granular fact/spec/task-archive! mutators do NOT exist — bulk archive only
   via (done {:archive …}). Close-of-turn is the single authoritative place to
   retire commitments."
  [ctx form-scope
   {:keys [answer answer-summary user-request turn-summary trailer-drop trailer-summarize archive]}]
  (apply-done-impl ctx
                   form-scope
                   {:answer answer,
                    :answer-summary answer-summary,
                    :user-request user-request,
                    :turn-summary turn-summary,
                    :trailer-drop trailer-drop,
                    :trailer-summarize trailer-summarize,
                    :archive archive}))
(defn- first-paragraph-summary
  "Phase F fallback: when the model didn't emit `:answer-summary`,
   take the first non-blank paragraph (up to ~280 chars) as the
   auto-fact summary. Better than nothing; nudges the model to emit
   explicit summaries since the auto extraction is necessarily lossy."
  [^String answer]
  (when (and answer (not (clojure.string/blank? answer)))
    (let [trimmed (clojure.string/trim answer)
          first-blank (or (clojure.string/index-of trimmed "\n\n")
                          (clojure.string/index-of trimmed "\r\n\r\n")
                          (count trimmed))
          para (subs trimmed 0 first-blank)]
      (if (> (count para) 280) (str (subs para 0 277) "…") para))))
(defn- auto-fact-for-turn-summary
  "Phase F: build the `:turn-N-summary` fact — a compact recap of the
   just-closed turn for cross-turn reference inside the cached `;; ctx`
   EDN prefix. The full :answer markdown is NOT stored here (ships via
   the answer channel + `session_turn_state.answer_markdown` column);
   the fact is the model's lookup index, not the artefact.

   Auto-derived skeleton (TWO content fields):

     :question         user request that started the turn
     :answer-summary   one-paragraph synopsis (model `:answer-summary`
                       arg or first paragraph of :answer, 280-char cap)

   Model can reconstruct what changed across the turn by walking
   :session/{facts,specs,tasks} entries whose :born or :done-born
   starts with the matching scope prefix — no separate event log
   needed.

   Plus engine-owned framing: `:status :active`, `:scope \"tN\"`,
   `:source :done-auto`, `:born <form-scope>`. Reserved keys cannot
   be overridden by the model.

   Returns `[fact-id fact-value]` or nil when nothing happened
   (no answer AND no entity-level transitions — a fully silent
   close-of-turn, e.g. cancelled iter that touched nothing).

"
  [ctx form-scope {:keys [user-request answer answer-summary model-summary]}]
  (let [turn-pos (or (:session/turn ctx) 0)
        answer-str (some-> answer
                           str
                           clojure.string/trim
                           not-empty)
        question (some-> user-request
                         str
                         clojure.string/trim
                         not-empty)
        synopsis (or (some-> answer-summary
                             str
                             clojure.string/trim
                             not-empty)
                     (some-> answer-str
                             first-paragraph-summary))
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
        reserved #{:status :source :scope :born}
        model-extras (when (map? model-summary)
                       (into {} (remove (fn [[k _]] (contains? reserved k)) model-summary)))
        fact-id (keyword (str "turn-" turn-pos "-summary"))
        base (cond->
               {:status :active, :scope (str "t" turn-pos), :source :done-auto, :born form-scope}
               question (assoc :question question)
               synopsis (assoc :answer-summary synopsis))]
    (when meaningful? [fact-id (merge model-extras base)])))
(defn- apply-done-impl
  "– the un-gated core of `apply-done`. Pulled out so the
   gate can short-circuit cleanly; callers must use `apply-done`."
  [ctx form-scope
   {:keys [answer answer-summary user-request turn-summary trailer-drop trailer-summarize archive]}]
  (let [start (or (:session/trailer ctx) [])
        after-drop (apply-trailer-drop start (or trailer-drop []))
        {:keys [trailer warnings]}
          (apply-trailer-summarize after-drop (or trailer-summarize []) form-scope)
        ;; Phase F (redesigned): compact `:turn-N-summary` fact. Carries
        ;; question + 1-paragraph answer synopsis + entity ids born/done
        ;; this turn. The full :answer markdown is NOT stored here
        ;; (ships via answer channel + DB answer_markdown column);
        ;; this fact is the cross-turn lookup index that rides inside
        ;; the cached ;; ctx EDN prefix. Optional `:turn-summary` arg
        ;; from the model adds free-form fields (action list,
        ;; what-we-did narrative) merged onto the auto skeleton.
        auto-fact-entry (auto-fact-for-turn-summary ctx
                                                    form-scope
                                                    {:user-request user-request,
                                                     :answer answer,
                                                     :answer-summary answer-summary,
                                                     :model-summary turn-summary})
        sorted (sort-trailer trailer)
        ;; flag scopes appearing in both drop AND summarize as a soft warn
        drop-set (set (or trailer-drop []))
        summarize-keys (set (for [s (or trailer-summarize [])]
                              (str (:scope-start s) "->" (:scope-end s))))
        conflict-keys (clojure.set/intersection drop-set summarize-keys)
        conflict-warns (for [k conflict-keys]
                         (warn :done-drop-summarize-conflict
                               [k]
                               (str "scope " k
                                    " appears in both :trailer-drop and"
                                      " :trailer-summarize; drop ignored, summary applied")))
        ctx-with-trailer (assoc ctx :session/trailer sorted)
        ;; Apply bulk-archive before the auto-fact lands so archive
        ;; entities are visible to the meaningfulness check.
        {ctx-archived :ctx, archive-warns :warnings}
          (if (map? archive)
            (apply-bulk-archive ctx-with-trailer form-scope archive)
            {:ctx ctx-with-trailer, :warnings []})
        ctx-final (cond-> ctx-archived
                    auto-fact-entry (assoc-in [:session/facts (first auto-fact-entry)]
                                      (second auto-fact-entry)))]
    {:ctx ctx-final, :warnings (vec (concat conflict-warns warnings archive-warns))}))
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
  (cond (map? history) (sort-by key history)
        (sequential? history) (sort-by first history)
        :else nil))
(defn- entity-change-summary
  "Pure: given a `before` and `after` snapshot of a single entity
   (`{:status :born :depends-on :title :content …}`), return a vec of
   `[field before-val after-val]` rows for fields that actually changed."
  [before after]
  (let [tracked [:status :title :content :born :done-born :depends-on :hook-id :importance :source
                 :contradicts]]
    (vec (for [f tracked :let [b (get before f) a (get after f)] :when (not= b a)] [f b a]))))
(defn- diff-subtree
  "Compare two snapshots' entries under `subtree-key` (e.g.
   `:session/tasks`). Returns a vec of change records:
     {:kind :task|:fact :K :K
      :change :added | :removed | [[field b a] …]}"
  [before after subtree-key kind]
  (let [bm (or (get before subtree-key) {})
        am (or (get after subtree-key) {})
        all-keys (sort (into #{} (concat (keys bm) (keys am))))]
    (vec (for [k all-keys
               :let [b (get bm k)
                     a (get am k)]
               :when (not= b a)]
           (cond (nil? b) {:kind kind, :K k, :change :added, :after a}
                 (nil? a) {:kind kind, :K k, :change :removed, :before b}
                 :else (let [fields (entity-change-summary b a)]
                         (when (seq fields) {:kind kind, :K k, :change fields})))))))
(defn diff-ctx
  "Pure: return a vec of change records between two ctx snapshots.
   Each record: `{:kind ∈ #{:task :fact} :K :change …}`.
   `:change` is one of `:added` | `:removed` | vec of `[field before
   after]` tuples. Trailer diffs are intentionally NOT included —
   trailer is ephemeral; model already reads it inline."
  [before after]
  (vec (concat (remove nil? (diff-subtree before after :session/tasks :task))
               (remove nil? (diff-subtree before after :session/facts :fact)))))
(defn introspect-changes
  "Lazy turn-delta introspection. `scope` is a turn key (`\"tN\"`); the
   engine fetches snapshots N-1 and N from history and returns the
   diff. Returns nil when N is missing or N-1 doesn't exist (e.g. on
   turn 1).

   Each entry: `{:kind :task|:fact :K k :change …}`
   where `:change` is `:added`, `:removed`, or a vec of `[field b a]`
   field-level transitions (status flip, born stamp, deps reshape, …)."
  [history turn-key]
  (let [snaps (snapshots-asc history)
        n (cond (string? turn-key) (when-let [[_ s] (re-matches #"^t([1-9][0-9]*)$" turn-key)]
                                     (parse-long s))
                (number? turn-key) (long turn-key)
                :else nil)
        snap-at (fn [t] (some (fn [[k v]] (when (= k t) v)) snaps))]
    (when n
      (let [after (snap-at n)
            before (snap-at (dec n))]
        (when (and after before) (diff-ctx before after))))))
(defn introspect-task
  [history k]
  (let [snaps (snapshots-asc history)]
    (some (fn [[turn ctx]]
            (when-let [entry (get-in ctx [:session/tasks k])] (assoc entry :as-of-turn turn)))
          (reverse snaps))))
(defn introspect-fact
  [history k]
  (let [snaps (snapshots-asc history)]
    (some (fn [[turn ctx]]
            (when-let [entry (get-in ctx [:session/facts k])] (assoc entry :as-of-turn turn)))
          (reverse snaps))))
(defn- subtree-key-for-kind
  [kind]
  (case kind
    :tasks :session/tasks
    :facts :session/facts))
(defn introspect-archived
  "Return summaries of entries archived from the LATEST snapshot but present
   in some earlier one. `kind` is `:tasks`/`:facts`. Each entry:
     {:key k :as-of-turn N :status …}"
  [history kind]
  (let [snaps (snapshots-asc history)
        subtree (subtree-key-for-kind kind)
        latest (some-> snaps
                       last
                       second
                       (get subtree)
                       keys
                       set)
        latest (or latest #{})]
    (vec (for [[turn ctx] (reverse snaps)
               :let [entries (or (get ctx subtree) {})
                     k+turn (for [[k v] entries
                                  :when (not (contains? latest k))]
                              {:key k, :as-of-turn turn, :status (:status v)})]
               entry k+turn
               ;; dedupe: only emit a key the first turn we see it (latest
               ;; descending), so each archived key shows up once with its
               ;; latest-known turn.
              ]
           entry))))
(defn introspect-ctx-at
  "Full CTX snapshot at end of turn N. Nil when turn was never persisted."
  [history turn-n]
  (cond (map? history) (get history turn-n)
        (sequential? history) (some (fn [[t ctx]] (when (= t turn-n) ctx)) history)
        :else nil))
;; =============================================================================
;; Form tag classification — derive :tag from the form source string
;; =============================================================================
(def ^:private head-edamame-opts
  "Tag-tolerant edamame parse opts. Mirrors `env/validation-edamame-opts`
   so a tagged-literal form (`#inst …`, `#some/tag …`) does not derail
   head extraction. Reader macros expand to `(do val)` which is exactly
   what we want — the head of an iter-top-level form is still detectable."
  {:all true, :fn true, :regex true, :readers (fn [_tag] (fn [val] (list 'do val)))})
(defn parse-form
  "Parse `src` into a top-level Clojure form via edamame, using the
   same tag-tolerant opts as `form-head-symbol`. Returns the FIRST
   parsed form (sexp / scalar) or nil on parse failure / empty source.

   Single source of truth for any \"give me the actual form structure,
   not a string\" consumer (validator-fns, structural matchers,
   audit logs). String-matching the raw `:src` is fragile —
   comments / strings / nested calls can carry the target symbol
   without the form actually invoking it; pattern-matching on the
   parsed sexp eliminates that whole class of false positives."
  [src]
  (try (first (edamame/parse-string-all (or src "") head-edamame-opts)) (catch Throwable _ nil)))
(defn form-head-symbol
  "Return the top-level head symbol of `src` (a Clojure source string)
   or nil when `src` is not a call form. Uses edamame so reader meta,
   discard forms (`#_`), comments, and tagged literals all behave the
   same way they do during real evaluation. Used by `classify-form-tag`
   and `engine-form-src?`; both must agree on what counts as the head,
   so there is exactly one implementation.

   Parse failure / non-list head / empty source → nil."
  [src]
  (let [form (parse-form src)] (when (and (seq? form) (symbol? (first form))) (first form))))
(def ^:private core-mutation-heads
  "Engine-owned bare-symbol heads that classify a form as `:mutation`.
   These are the symbols the engine itself defines and recognises:

     • SCI def-shape forms (sandbox state writes)
     • CTX memory mutators (task/fact surface)
     • Control flow (done, set-session-title!)
     • Clojure native mutators that survive the sandbox
       (reset!, swap!, alter-var-root)

   Extension tools (`v/patch`, `v/write`, `git/commit!`, anything an
   extension ships) are NOT here. Extensions declare their own
   observation / mutation tag at registration time; the integration
   layer reaches that tag through `extension/op-tag` and passes it to
   `classify-form-tag` as an optional resolver. Keeping the core set
   pure of `v/*` heads stops the engine from owning extension policy."
  '#{def defn defmacro defmulti defmethod task-set! fact-set! task-depends! fact-depends!
     fact-contradicts! fact-contradicts-remove! done set-session-title! reset! swap!
     alter-var-root})
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
  '#{set-session-title! done task-set! fact-set! task-depends! fact-depends! fact-contradicts!
     fact-contradicts-remove!})
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
      (or (contains? engine-form-heads sym) (str/starts-with? nm "introspect-")))))
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
     (or (when (and sym head-tag-resolver) (try (head-tag-resolver sym) (catch Throwable _ nil)))
         (if (and sym (contains? core-mutation-heads sym)) :mutation :observation)))))
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

     {:scope :tag :src :form :duration-ms :result :error :channel}

   `:src` carries the form's source text; `:form` carries the same
   text PARSED via edamame so validator-fns / structural matchers can
   pattern-match on the actual sexp instead of `(includes? src \"...\")`-
   shaped string regexes (a comment or string literal carrying the
   target symbol would have produced false positives). Parse failure
   leaves `:form` absent. `:tag` is derived from the source via
   `classify-form-tag`. `:result` is included only when the block has
   one (engine convention: drop on default/nil). `:error` is included
   only when the block errored. `:channel` is included only when the
   form actually called one or more extension tools. `:duration-ms` is
   derived from the loop's block envelope so persisted TUI replays keep
   the same per-form footer timing as live progress bubbles.

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
  ([block position cursor] (block->envelope block position cursor nil))
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
         channel (seq (:channel block))
         parsed-form (parse-form src)
         duration-ms
           (when-let [envelope (:envelope block)]
             (when (and (nat-int? (:started-at-ms envelope)) (nat-int? (:finished-at-ms envelope)))
               (max 0 (- (long (:finished-at-ms envelope)) (long (:started-at-ms envelope))))))]
     (cond-> {:scope scope, :tag (classify-form-tag src head-tag-resolver), :src src}
       (some? parsed-form) (assoc :form parsed-form)
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
   (`v/patch`, `git/commit!`, any symbol with inline `:tag` on its
   `vis/symbol` entry) classify correctly without the engine
   hard-coding their symbol set."
  ([blocks cursor] (blocks->forms blocks cursor nil))
  ([blocks {:keys [turn iter]} head-tag-resolver]
   (vec (map-indexed (fn [idx block]
                       (block->envelope block (inc idx) {:turn turn, :iter iter} head-tag-resolver))
                     (or blocks [])))))
