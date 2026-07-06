(ns com.blockether.vis.internal.ctx-engine
  "Pure helpers for the model-facing context snapshot.

   The mutable session state lives outside this namespace; these functions only
   advance the turn/iteration cursor, project form envelopes for persisted
   result messages, and compute utilization metadata."
  (:require [clojure.string :as str]))

(def DEFAULT_PROMPT_BUDGET_TOKENS
  "Soft per-call request-size guardrail surfaced to the model as
   `session_utilization.auto_compress_above`. ~72% of a typical 200k window,
   leaving headroom for the model's own output. Compared against PROVIDER-reported
   input tokens (no local tokenizer) — see `utilization`."
  144000)
;; =============================================================================
;; Scope parsing — deterministic, regex-driven
;; =============================================================================
;; Iteration scope is `tN/iM`. The legacy per-form `/fK` tail is OPTIONAL (and no
;; longer emitted — one record = one tool call, keyed by `:svar/tool-call-id`).
(def ^:private scope-form-re #"^t([1-9][0-9]*)/i([1-9][0-9]*)(?:/f([1-9][0-9]*))?$")
(defn parse-scope-form
  "Parse a `tN/iM` (or legacy `tN/iM/fK`) scope into `{:turn :iter :form}` or nil
   if malformed. `:form` is nil for an iteration-level scope. Pure value-or-nil."
  [s]
  (when (string? s)
    (when-let [[_ t i f] (re-matches scope-form-re s)]
      {:turn (parse-long t) :iter (parse-long i) :form (when f (parse-long f))})))
(defn malformed-scope?
  "True if `s` is a string but does not parse as `::cs/scope-form`."
  [s]
  (and (string? s) (nil? (parse-scope-form s))))
(defn scope-compare
  "Total order on form-scope strings by (turn, iter, form). Returns int.
   Compares parsed segments; malformed scopes sort before all valid ones to
   make their presence obvious in render."
  [a b]
  (let [pa
        (parse-scope-form a)

        pb
        (parse-scope-form b)]

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
    (let [data
          (if (map? (:data error)) (:data error) {})

          inner
          (if (map? (:error data)) (:error data) {})

          pick3
          (fn [k]
            (or (k inner) (k data) (k error)))

          message
          (or (:message error) (:message inner))

          etype
          (or (:type inner) (:type data) (:type error))

          reason
          (pick3 :reason)

          ;; ONE hint survives: `:loop-hint` is the model-facing
          ;; recovery advice (the error normalizer lifts it into the
          ;; trailer); `:hint` is the human/channel field. Extensions
          ;; legitimately set both — the prompt needs at most one.
          hint
          (or (pick3 :loop-hint) (pick3 :hint))

          ;; validation-tool forensics the lift contract declares
          ;; model-actionable (`:failures`, `:checks`, `:mode`)
          failures
          (pick3 :failures)

          checks
          (pick3 :checks)

          mode
          (pick3 :mode)]

      (cond-> {}
        message
        (assoc :message message)

        etype
        (assoc :type etype)

        reason
        (assoc :reason reason)

        ;; the hint earns its tokens only when the message doesn't
        ;; already carry it verbatim
        (and hint (not (str/includes? (str message) (str hint))))
        (assoc :hint hint)

        (seq failures)
        (assoc :failures failures)

        (seq checks)
        (assoc :checks checks)

        mode
        (assoc :mode mode)))))

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
   on LIVE op envelopes and on the persisted rows — but NOTHING reads
   it from the trailer (the observation-prune that once did was
   removed), and the model can see what a form does from `:src`. The
   full envelopes stay on the progress chunks and the persisted
   `session_turn_iteration.forms` rows — channels and the persisted
   forms rows keep total fidelity; the trailer is what rides every
   prompt.

   Empty payloads are DROPPED, not rendered: `\"result\": None` /
   `[]` / `{}` and empty `:error` maps say nothing the form's absence
   of an error/result doesn't already say — the `:src` stays, the dead
   field goes."
  [r]
  (let [empty-payload?
        (fn [v]
          (or (nil? v) (and (coll? v) (empty? v))))

        r*
        (cond-> (dissoc r :channel :tag)
          (:error r)
          (update :error model-error)

          (contains? r :result)
          (update :result model-tool-result))]

    (cond-> r*
      (and (contains? r* :result) (empty-payload? (:result r*)))
      (dissoc :result)

      (and (contains? r* :error) (empty-payload? (:error r*)))
      (dissoc :error))))

(defn advance-iter
  "Advance the cursor so the next iter starts at \"iter\" (current+1),
   \"next_form\" 1. Prior tool outputs are carried by append-only message
   history, not by mutable context state. CTX is STRING-KEYED end to end —
   it is bound into Python as the `context` dict and nippy-persisted, and
   both surfaces are strings-only."
  [ctx _form-results-vec]
  (let [cursor (get ctx "session_scope")]
    (assoc ctx
      "session_scope" (-> cursor
                          (update "iter" inc)
                          (assoc "next_form" 1)))))
;; --- GC TTL constants ----------------------------------------------------
(defn gc-pass
  "Passthrough. Tasks/facts/archive are gone — there is nothing to GC.
   Kept so the turn-lifecycle chokepoint (`enter-turn`) and any external
   callers stay valid."
  [ctx]
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
        (assoc "session_turn" next-turn)
        (assoc "session_scope" {"turn" next-turn "iter" 1 "next_form" 1})
        (assoc "engine_blockers" [])
        gc-pass)))
;; =============================================================================
;; Empty-ctx constructor — used by tests + scenario replayer
;; =============================================================================
(defn empty-ctx
  "A minimal CTX scaffold with all model-facing keys filled by empty /
   default values. Useful as the starting point for scenario replays.

   Includes the engine-ephemeral key `\"engine_warnings\"` so the rest of
   the system can swap! it without nil-puncturing. Stripped at
   persistence boundaries via `strip-ephemeral`.

   CTX is STRING-KEYED end to end: it is bound into Python as the `context`
   dict (strings-only boundary) and nippy-persisted (strings-only DB), so
   there is no keyword->string projection anywhere between."
  ([] (empty-ctx "test-session"))
  ([session-id]
   {"session_id" session-id
    "session_turn" 1
    "session_scope" {"turn" 1 "iter" 1 "next_form" 1}
    ;; Empty scaffold only. Prompt render replaces this through
    ;; foundation-core with a real workspace identity:
    ;;   {"root" ... "sandbox" ... "vcs_kind" ...}
    ;; `"vcs_kind" "none"` is reserved for an actual root with no supported VCS.
    "session_workspace" {}
    "session_symbols" {}
    "engine_warnings" []}))
(defn strip-ephemeral
  "Remove every `\"engine_*\"` key from a ctx. Call before Nippy-snapshotting
   to persistence so transient mutator state (warnings, pending satisfy
   requests) does not leak into the durable record."
  [ctx]
  (when ctx
    (into {}
          (remove (fn [[k _]]
                    (and (string? k) (str/starts-with? k "engine_")))
            ctx))))
;; =============================================================================
;; Iter-scope parsing + comparator
;; =============================================================================
(defn compact-src
  "One-line, length-capped form source — the auto-summary listing AND
   the renderer's src line on pre-turn `<results>` pins share it."
  [src]
  (let [s (-> (or src "")
              str
              str/trim
              (str/replace #"\s+" " "))]
    (if (> (count s) 90) (str (subs s 0 90) "…") s)))
(defn finalize-turn
  "Finalize a turn: the loop ships `:answer` to the channel and the engine
   returns ctx unchanged so the turn can settle."
  [ctx _form-scope _args]
  {:ctx ctx :warnings []})
(defn utilization
  "Pure: the `\"session_utilization\"` map the model reads to see how much
   of the context window the LAST request consumed. Keys are spelled out
   so they can't be misread:
     last_request_tokens  input size of the most recent model call
     model_input_limit    HARD per-call ceiling (provider rejects above)
     saturation           last-request / model-input-limit, as a rounded
                          percentage — how FULL the per-call window is
     headroom_tokens      tokens still free before the ceiling
                          (model_input_limit - last request); the
                          actionable 'can I keep going or must I fold?'
     auto_compress_above  soft guardrail threshold for request size
     turn_total_tokens    cumulative input this turn (billing, NOT a
                          per-call limit — may exceed the limit safely)
   Returns nil until a request has actually been measured (req <= 0), so
   the first iter of a turn shows nothing rather than a bogus 0%."
  [request-tokens window-tokens turn-tokens fold-cap]
  (let [req
        (long (or request-tokens 0))

        win
        (long (or window-tokens 0))]

    (when (pos? req)
      (cond-> {"last_request_tokens" req
               "turn_total_tokens" (long (or turn-tokens 0))
               "auto_compress_above" (long (or fold-cap 0))}
        (pos? win)
        (assoc "model_input_limit"
          win "saturation"
          (long (Math/round (* 100.0 (/ (double req) (double win))))) "headroom_tokens"
          (max 0 (- win req)))))))

(def model-facing-keys
  "EXACT set of `session_*` keys the model is meant to see. This is the
   SINGLE definition of 'model-facing'; `session-view` selects on it so engine
   bookkeeping cannot leak into the rendered `<context>` or bound `context`
   dict. `\"session_utilization\"` is derived (from `\"engine_utilization\"`),
   so it is folded in by `session-view` rather than listed here."
  ["session_id" "session_turn" "session_scope" "session_workspace" "session_env" "session_routing"
   "session_resources" "session_symbols"])

(defn session-view
  "THE single projection from engine-internal ctx to the model-facing
   `session_*` view.

   Both consumers derive from this, so the rendered `<context>` block and the
   Python `context` dict are the same map by construction:
     - `ctx-renderer/render-ctx` serializes this view
     - `ctx-loop/session-snapshot` binds this view as read-only `context`

   Keeps ONLY `model-facing-keys` (so engine bookkeeping never leaks) and
   projects `\"engine_utilization\"` → `\"session_utilization\"`. The second
   arity takes legacy `warnings` for call-site compatibility but ignores them.
   Pure; STRING keys in and out."
  ([ctx] (session-view ctx nil))
  ([ctx _warnings]
   (cond-> (select-keys ctx model-facing-keys)
     (get ctx "engine_utilization")
     (assoc "session_utilization" (get ctx "engine_utilization")))))
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
   raw source) avoids false positives — a `\"patch(x)\"` inside a string can't
   match. Used by `classify-form-tag`."
  [src]
  (some-> (re-find py-head-name-re (str src))
          second))
(def ^:private core-mutation-heads
  "Engine-owned call NAMES (Python, snake_case) that classify a form as
   `:mutation`. Empty now: the task/fact mutator surface and
   `set_session_title` were removed (titles are host-generated), and
   answers are plain prose (no control-flow verb), so no core head
   remains.

   Extension tools (`patch`, `write`, `git_commit`, anything an extension
   ships) are NOT here. Extensions declare their own observation / mutation
   tag at registration time; the integration layer reaches that tag through
   `extension/op-tag` and passes it to `classify-form-tag` as an optional
   resolver. Keeping the core set pure of tool names stops the engine from
   owning extension policy."
  #{})
;; UI hiding of engine chrome is NOT a head-name concern: a form is
;; silent iff it is structurally code-free OR its RESULT is a
;; `vis_silent` sentinel. Channels read the sentinel rather than a
;; head-name list.
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
         ;; Rebuild into `(empty v)` — the SAME map type — so an ordered map (cat's
         ;; round-tripped `:anchors`, a flatland ordered-map / array-map) keeps its
         ;; insertion order; `(into {} …)` would demote it to a hash-map and scramble
         ;; the file's line order on the wire.
         (map? v) (reduce-kv (fn [m k val]
                               (assoc m k (realize-value val (dec depth))))
                             (empty v)
                             v)
         (vector? v) (mapv #(realize-value % (dec depth)) v)
         (set? v) (into #{} (map #(realize-value % (dec depth))) v)
         (sequential? v) (doall (map #(realize-value % (dec depth)) v))
         :else v)))
(defn block->envelope
  "Project one loop-side block `{:code :result :error :stdout}` plus its
   1-based position and the engine cursor into the form envelope shape
   (one block = one form, 1:1):

     {:scope :tag :src :duration-ms :result :error :stdout}

   `:src` carries the block's source text. `:tag` is derived from the source via
   `classify-form-tag`. `:result` is included only when the block has
   one (engine convention: drop on default/nil). `:error` is included
   only when the block errored. `:stdout` (what the block PRINTED) is the
   single display surface — channels paint it, and the model reads it back.
   `:duration-ms` is derived from the loop's block envelope so persisted TUI
   replays keep the same footer timing as live progress bubbles.

   Every result is walked through `realize-value` so any `IDeref` (Var,
   atom) is derefed and lazy seqs land as data, never as
   `#:vis{:ref :expr}` placeholders left over from persistence flattening
   unrealized seqs."
  ([block position cursor] (block->envelope block position cursor nil))
  ([block _position cursor head-tag-resolver]
   (let [src
         (or (:code block) (:src block) "")

         ;; ITERATION scope `tN/iM`. One record = one tool call, identified by
         ;; `:svar/tool-call-id`; there is no per-form `/fK` index any more.
         scope
         (str "t" (:turn cursor) "/i" (:iter cursor))

         raw-result
         (:result block)

         ;; `realize-value` derefs any `IDeref` it encounters, so every
         ;; block's result — Var, atom, lazy seq, plain data — lands as
         ;; fully realised data in the envelope, ready for prompt
         ;; rendering and introspection.
         result
         (realize-value raw-result)

         duration-ms
         (when-let [envelope (:envelope block)]
           (when (and (nat-int? (:started-at-ms envelope)) (nat-int? (:finished-at-ms envelope)))
             (max 0 (- (long (:finished-at-ms envelope)) (long (:started-at-ms envelope))))))]

     (cond-> {:scope scope :tag (classify-form-tag src head-tag-resolver) :src src}
       (some? duration-ms)
       (assoc :duration-ms duration-ms)

       (contains? block :result)
       (assoc :result result)

       ;; PRINT-ONLY context: the model sees ONLY what it printed. Carry the
       ;; form's captured stdout onto the envelope so iteration-results-message
       ;; can surface it — without this, every print() reads back to the model as
       ;; "(no output)" (it renders :stdout, not :result; bare values aren't echoed).
       (some? (:stdout block))
       (assoc :stdout (:stdout block))

       (some? (:error block))
       (assoc :error (:error block))

       ;; Tool-call identity: which native tool-call (svar tool_use id) this form
       ;; answers, plus its name. `iteration-results-message` groups forms by this
       ;; id so EACH tool_use gets its OWN tool_result carrying its own output —
       ;; the maki model where one of the calls may be `python_execution`.
       (some? (:svar/tool-call-id block))
       (assoc :svar/tool-call-id (:svar/tool-call-id block))

       (some? (:vis/tool-name block))
       (assoc :vis/tool-name (:vis/tool-name block))

       ;; Per-tool BADGE color (read/search/edit/…) so the channel paints the
       ;; native tool's result card in its role color — survives DB round-trip.
       (some? (:tool-color-role block))
       (assoc :tool-color-role (:tool-color-role block))

       ;; The pre-rendered display STRING (native-tool card / pretty result) so a
       ;; DB-restored trace shows the SAME card the live stream did — channels read
       ;; this instead of pr-str'ing the raw `:result` map.
       (some? (:result-render block))
       (assoc :result-render (:result-render block))

       ;; The op-card HEADLINE (tool-authored summary) so a restored trace titles
       ;; the card the same way the live stream did — not a first-line body slice.
       (some? (:result-summary block))
       (assoc :result-summary (:result-summary block))

       ;; MULTI-card: the pre-rendered per-result mini-forms a print-many block shows.
       ;; nippy keeps the nested keywords, so a DB-restored trace paints the SAME
       ;; separate colored cards the live stream did. DISPLAY-ONLY — never read into
       ;; model context (that stays `:stdout`).
       (seq (:cards block))
       (assoc :cards (:cards block))))))
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
                       (block->envelope block (inc idx) {:turn turn :iter iter} head-tag-resolver))
                     (or blocks [])))))
