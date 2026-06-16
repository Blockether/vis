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
     :update-plan! :plan-step! :task-set! :fact-set!
   Fact relations (depends_on + contradicts) are DECLARATIVE FIELDS on
   `:fact-set!` — no standalone *-depends! / *-contradicts! mutators.

   Hard rejects (engine writes nothing, warnings carry the reason):
     - malformed scope string anywhere
     - depends-on cycle introduced by task-set! / fact-set!
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
(defn- model-error
  "Collapse a host failure envelope to what the MODEL can act on — ONE
   message, its type/reason, one actionable hint. The raw envelope
   repeats the same message up to four times (top level, `:data :error`,
   `:data :tool-result :error`, the channel slice) and drags the failing
   extension's identity (license, author, description, source paths,
   sha256) along — pure prompt-token waste: one blocked shell_run
   pinned ~700 tokens of duplicates into every subsequent prompt."
  [error]
  (if-not (map? error)
    error
    (let [data    (if (map? (:data error)) (:data error) {})
          inner   (if (map? (:error data)) (:error data) {})
          pick3   (fn [k] (or (k inner) (k data) (k error)))
          message (or (:message error) (:message inner))
          etype   (or (:type inner) (:type data) (:type error))
          reason  (pick3 :reason)
          ;; ONE hint survives: `:loop-hint` is the model-facing
          ;; recovery advice (the error normalizer lifts it into the
          ;; trailer); `:hint` is the human/channel field. Extensions
          ;; legitimately set both — the prompt needs at most one.
          hint    (or (pick3 :loop-hint) (pick3 :hint))
          ;; validation-tool forensics the lift contract declares
          ;; model-actionable (`:failures`, `:checks`, `:mode`)
          failures (pick3 :failures)
          checks   (pick3 :checks)
          mode     (pick3 :mode)]
      (cond-> {}
        message (assoc :message message)
        etype   (assoc :type etype)
        reason  (assoc :reason reason)
        ;; the hint earns its tokens only when the message doesn't
        ;; already carry it verbatim
        (and hint (not (str/includes? (str message) (str hint))))
        (assoc :hint hint)
        (seq failures) (assoc :failures failures)
        (seq checks)   (assoc :checks checks)
        mode           (assoc :mode mode)))))

(defn- model-tool-result
  "Strip host bookkeeping from a tool-result envelope riding as a form
   result: `:metadata` carries extension identity + source forensics
   the model can do nothing with; `:success?` is derivable from
   `:error` (nil = success — same rule the renderer applies to the
   form level); `:symbol`/`:tag` duplicate the form envelope; a nested
   `:error` gets the same collapse as the form error."
  [result]
  (if-not (map? result)
    result
    (cond-> (dissoc result :success? :symbol :tag)
      (map? (:metadata result))
      (as-> r (let [m (not-empty (dissoc (:metadata r) :extension :source :tool))]
                (if m (assoc r :metadata m) (dissoc r :metadata))))

      (map? (:error result))
      (update :error model-error))))

(defn model-form-envelope
  "Project one executed-form envelope onto the MODEL contract the
   trailer stores: `:scope :src :result :error` (+ engine forensic
   fields), WITHOUT the channel sink slice, the host failure/metadata
   chains, or `:tag`. The mutation/observation tag stays load-bearing
   on LIVE op envelopes (the done()-as-proposal gate reads it there)
   and on the persisted rows — but NOTHING reads it from the trailer
   (the observation-prune that once did was removed), and the model
   can see what a form does from `:src`. The full envelopes stay on
   the progress chunks and the persisted `session_turn_iteration.forms`
   rows — channels and `recall`'s DB window keep total fidelity; the
   trailer is what rides every prompt.

   Empty payloads are DROPPED, not rendered: `\"result\": None` /
   `[]` / `{}` and empty `:error` maps say nothing the form's absence
   of an error/result doesn't already say — the `:src` stays, the dead
   field goes."
  [r]
  (let [empty-payload? (fn [v] (or (nil? v) (and (coll? v) (empty? v))))
        r* (cond-> (dissoc r :channel :tag)
             (:error r)            (update :error model-error)
             (contains? r :result) (update :result model-tool-result))]
    (cond-> r*
      (and (contains? r* :result) (empty-payload? (:result r*)))
      (dissoc :result)

      (and (contains? r* :error) (empty-payload? (:error r*)))
      (dissoc :error))))

(defn advance-iter
  "Advance the cursor so the next iter starts at :iter (current+1),
   :next-form 1. `form-results-vec` is ignored — there is no trailer to pin
   into (output is never folded/compacted; the loop's message history carries
   prior results verbatim)."
  [ctx _form-results-vec]
  (let [cursor (:session/scope ctx)]
    (assoc ctx
      :session/scope (-> cursor
                       (update :iter inc)
                       (assoc :next-form 1)))))
;; --- GC TTL constants ----------------------------------------------------
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
  ;; Tasks/facts/archive are gone — nothing to GC. Passthrough kept so the
  ;; turn-lifecycle chokepoint (enter-turn) and external callers stay valid.
  ctx)
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
(defn compact-src
  "One-line, length-capped form source — the auto-summary listing AND
   the renderer's src line on pre-turn `<results>` pins share it."
  [src]
  (let [s (-> (or src "") str str/trim (str/replace #"\s+" " "))]
    (if (> (count s) 90) (str (subs s 0 90) "…") s)))
(defn apply-done
  "Process a `(done {…})` form. With tasks/facts/trailer/archive removed there
   is no auto-fact, no trailer sort, and no compaction at close — the loop
   ships `:answer` to the channel and the engine just returns the ctx
   unchanged so the turn can settle."
  [ctx _form-scope _args]
  {:ctx ctx, :warnings []})
;; =============================================================================
;; recall — the single recovery verb (replaces the introspect-* sprawl)
;;
;;   recall by ADDRESS  → char-window a stored value (clipped MIDDLE reachable)
;;   recall by CONTENT  → search the live (non-summarized) trace for a scope
;; Pure halves live here; the effectful DB/ctx-atom halves live in
;; `ctx-loop/build-introspect-bindings`.
;; =============================================================================
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
   :session/env :session/routing :session/resources :session/symbols])

(defn session-view
  "THE single projection from engine-internal ctx to the model-facing
   `:session/*` view — the ONE way the ctx is shaped for the model.

   Both consumers derive from this, so the EDN the model reads and the
   value bound to the bare `ctx` symbol are the same map by construction:
     - `ctx-renderer/render-ctx`  serializes this view to the `;; ctx` text
     - `ctx-loop/session-snapshot` binds this view as read-only `ctx`

   Keeps ONLY `model-facing-keys` (so engine bookkeeping never leaks) and
   projects `:engine/utilization` → `:session/utilization`. The second arity
   takes legacy `warnings` for call-site compatibility but ignores them —
   there is no task/fact/hint surface anymore. Pure."
  ([ctx] (session-view ctx nil))
  ([ctx _warnings]
   (cond-> (select-keys ctx model-facing-keys)
     (:engine/utilization ctx) (assoc :session/utilization (:engine/utilization ctx)))))
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
  #{"update_plan" "plan_step" "fact_set" "done" "set_session_title"})
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
  #{"set_session_title" "done" "update_plan" "plan_step" "fact_set"})
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
(defn- realize-value
  "Force-realize lazy seqs / nested IDeref refs in `v` so the envelope carries
   DATA, not computations: a `(def …)` result is a Var, a tool may return an
   atom or a LazySeq. Without this the freeze-safe persist path stores
   `{:vis/ref :expr}` and the next iteration sees a placeholder, not the
   collection. Walks maps/vectors/sets/sequentials; scalars pass through;
   depth-bounded so a self-referential structure cannot loop."
  ([v] (realize-value v 8))
  ([v depth]
   (cond (or (nil? v) (zero? depth)) v
     (instance? clojure.lang.IDeref v) (try (realize-value (deref v) (dec depth))
                                         (catch Throwable _ v))
     (map? v) (into {} (map (fn [[k val]] [k (realize-value val (dec depth))])) v)
     (vector? v) (mapv #(realize-value % (dec depth)) v)
     (set? v) (into #{} (map #(realize-value % (dec depth))) v)
     (sequential? v) (doall (map #(realize-value % (dec depth)) v))
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
         result (realize-value raw-result)
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
