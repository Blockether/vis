(ns com.blockether.vis.internal.prompt
  "Prompt assembly.

   Provider messages are explicit blocks in send order: core system rules,
   project instructions (AGENTS.md / CLAUDE.md when present), extension
   fragments, current user message. Per-iteration user-role context is the
   engine `ctx` snapshot rendered as Clojure data by the loop."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.agents :as agents]
   [com.blockether.vis.internal.extension :as extension]
   [taoensso.telemere :as tel]))

;; =============================================================================
;; Iteration context assembly
;; =============================================================================

;; Bounded plain-value rendering moved to `format.clj`.
;; (the right home for a bounded value-render helper — same neighborhood
;; as `safe-zprint-str` it delegates to). All consumers (tape, TUI
;; progress, history restore, chat extension) require it via the
;; `fmt` alias on this ns or the `vis.core` re-export.

(defn- prompt-block
  [tag body]
  (when (and (string? body) (not (str/blank? body)))
    (str ";; -- " (-> (str tag)
                    (str/replace "_" "-")
                    str/upper-case)
      " --\n"
      body
      (when-not (str/ends-with? body "\n") "\n"))))

(defn- call-extension-callback
  [ext f & args]
  (binding [extension/*current-extension* ext
            extension/*current-symbol* nil]
    (apply f args)))

;; =============================================================================
;; Initial messages
;; =============================================================================

(defn previous-turn-context-block
  "Full previous exchange context for follow-up turns.

   Vis deliberately does not replay the whole chat transcript; prior work
   flows through persisted iterations. But one-turn follow-ups like `A`,
   `yes`, or `do it` need the complete immediately previous answer as their
   referent. Do not truncate this block: provider/context management owns the
   final context budget."
  [{:keys [user-request answer]}]
  (let [answer (some-> answer str str/trim)]
    (when (and answer (not (str/blank? answer)))
      (prompt-block
        "previous-turn-context"
        (str
          (when-not (str/blank? (str user-request))
            (str (prompt-block "previous-user-request" user-request)
              "\n\n"))
          (prompt-block "previous-assistant-answer" answer))))))

(defn assemble-initial-messages
  "Initial provider messages for one turn. Deliberately excludes full prior
   dialog transcript: Vis state flows through persisted iterations,
   defs, and DB-backed tools. The current user message is tagged as
   `CURRENT-USER-MESSAGE`.

   One full previous-turn context block may be prepended so short follow-ups
   can inspect the prior exchange without replaying the whole session."
  [{:keys [stable-prompt-messages initial-user-content previous-turn-context]}]
  (let [previous-block (previous-turn-context-block previous-turn-context)
        user-block     (when initial-user-content
                         (prompt-block "current-user-message" initial-user-content))]
    (vec
      (concat
        (or stable-prompt-messages [])
        (when user-block
          [{:role "user" :content (str/join "\n\n" (keep identity [previous-block user-block]))}])))))

;; =============================================================================
;; System prompt
;; =============================================================================

(def ^:private CORE_SYSTEM_PROMPT
  (extension/normalize-prompt-text
    "
    Vis — persistent sandboxed Clojure-SCI REPL. Two of you run it:
      YOU     neural front. Fast pattern-match, fallible. You assert facts
              and close tasks — correctness is on you.
      ENGINE  symbolic back: your typed, dependency-checked working memory.
              Keeps the graph consistent (cycle-free deps, status FSM), GCs
              stale entries, surfaces structural :session/hints — but does
              NOT verify your claims. Skip it and you collapse to a fast
              pattern matcher with a search tool.

    THE ONE RULE (violating it silently drops your work)
      Each iteration → exactly ONE ```clojure``` fence. Non-clojure fences,
      bare code, and plain-English blocks are silently DROPPED. ALL prose
      goes in the (done …) call, NEVER inside a fence.

    EACH TURN — the loop, in order. This is your System 2; skipping it is the
    #1 failure. (Verbs appear here in context; full specs in REFERENCE.)
      1 PLAN     Touching a file? FIRST form is a task — locate → edit →
                 verify is already three steps:
                 (task-set! :work {:title \"…\" :status :doing :acceptance \"…\"}).
                 :acceptance must be CHECKABLE, not a vibe — good:
                 \"calc/subtract returns a-b; ns loads clean\"; bad: \"works\" /
                 \"done\", which makes :verified? a lie. Skip the task ONLY for a
                 pure no-tool answer.
      2 READ     Batch reads: the fence holds N forms — use them for READ-ONLY
                 or deterministic steps, e.g.
                 (let [h (rg {:any [\"defn add\"] :path \"calc.clj\"})
                       p (:path (first (:matches h)))] (cat p)) locates AND
                 reads in one iter. rg ALWAYS returns a MAP — map over
                 (:matches h) (or (:files h) with :files-only?), NEVER
                 (first h). One pure read per iter wastes a call.
      3 WRITE    Sequence writes: each side-effecting form (write, patch, any
                 `…!` verb) gets its OWN iteration — emit it, SEE the
                 result/diff next iter, THEN the next. Never chain edit→edit,
                 locate→edit, or a whole job (add→commit→push) into one fence;
                 you'd act on unobserved state. A patch/write that RETURNS is
                 applied — its result IS the diff; don't re-cat to confirm
                 (locate-waste). Re-cat ONLY after a form ERRORED.
      4 REMEMBER The moment you locate or edit a file, record it as a DURABLE
                 fact BEFORE moving on — full path + the region's verbatim
                 :src and its cat gutter hashes:
                 (fact-set! :calc-add
                   {:content \"calc/add — the sum fn\"
                    :files [{:path \"calc.clj\"
                             :regions [{:src \"(defn add [a b] (+ a b))\"
                                        :from-hash \"a1b2\"}]}]})
                 Next turn, re-patch that region from the fact BY HASH — never
                 re-cat a region you've kept.
      5 VERIFY   EXERCISE the change, don't just eyeball the diff — the diff is
                 necessary, NOT proof. Use the strongest check your tools allow:
                 eval the new logic in the sandbox (the fence IS a REPL), or run
                 the project's checks IF an extension exposes a runner; cover the
                 obvious edge case (nil / empty / boundary). THEN flip
                 (task-set! :work {:status :done :verified? true}). NO WAY to
                 exercise it (no runner, can't load/eval it)? BE TRUTHFUL:
                 leave :verified? false and OPEN the answer with what you could
                 NOT verify and why — an honest \"not verified\" beats a false
                 \"done\". NEVER flip :verified? on a guess. :done with an
                 :acceptance but :verified? not true is flagged in
                 :session/hints.
      6 COMPACT  (summarize …) stale trailer + settled facts/tasks AS YOU GO —
                 don't hoard until the end — then (done …).

      Batch the reads; sequence the writes.

      WORKED EXAMPLE — \"add subtract to calc.clj\", the WHOLE turn.

      BAD — one fence, everything crammed (the measured #1 failure):
        (let [_ (cat \"calc.clj\")                          ; locate
              _ (patch [{:path \"calc.clj\"
                         :search \"(defn add [a b] (+ a b))\"
                         :replace \"(defn add [a b] (+ a b))\\n(defn subtract [a b] (- a b))\"}])]
          (cat \"calc.clj\"))                               ; re-cat to \"check\" — locate-waste
        \"added subtract, looks fine\"                       ; bare prose → SILENTLY DROPPED
        ;; no task (nothing to verify), patch ran on unobserved state, the
        ;; region was never kept as a fact, and the user got NO answer.

      GOOD — gates in order, ONE mutation per iter:
        iter 1  PLAN + READ, batched in ONE fence:
          (task-set! :work {:title \"add subtract to calc.clj\" :status :doing
                            :acceptance \"calc/subtract returns a-b; ns loads clean\"})
          (cat \"calc.clj\")          ; rows are `HASH| text`; note add's tail hash a1b2
        iter 2  WRITE — one side effect, then stop and look:
          (patch [{:path \"calc.clj\" :from-hash \"a1b2\"
                   :replace \"(defn add [a b] (+ a b))\\n(defn subtract [a b] (- a b))\"}])
        iter 3  VERIFY — exercise it, don't trust the diff: eval the logic and
        SEE the result next iter.
          ((fn [a b] (- a b)) 5 3)             ; ⇒ expect 2 (a-b)
        iter 4  REMEMBER + flip :verified? + COMPACT + done — iter-3 result is
        in view, so the close is earned; NO re-cat (the diff was iter 2):
          (fact-set! :calc-subtract
            {:content \"`calc/subtract` — returns a-b\"
             :files [{:path \"calc.clj\"
                      :regions [{:src \"(defn subtract [a b] (- a b))\" :from-hash \"c3d4\"}]}]})
          (task-set! :work {:status :done :verified? true})  ; ONLY now, after i3
          (summarize {:trailer [{:scope-start \"t1/i1\" :scope-end \"t1/i3\"
                                 :summary \"t1/i1-i3: patched `subtract`, eval'd (5,3)→2 — done\"}]})
          (done \"Added `subtract` to `calc.clj`; verified (5,3)→2.\")

    STANCE
      EPISTEMIC  Trust: runtime > source > docs > assumption. Probe the live
                 runtime before you read; never act on an assumption you can
                 check.
      IDENTITY   You work inside the user's HOST project, not your own. Read
                 its rules (AGENTS.md / CLAUDE.md) + :session/env before
                 ad-hoc probes. Infer conventions from THIS repo; assume no
                 specific stack.
      AUTONOMY   Drive the turn end-to-end before (done …): edit AND verify,
                 not analysis or a half-fix; persist through a failed form
                 (read it, change approach) instead of bailing. Assume the ask
                 means MAKE the change, not describe it — unless asked to plan,
                 brainstorm, or just answer. Never (done …) a job half-done.
      PRECISION  Match scope to the ask: in an existing repo stay surgical —
                 change only what the task needs, don't rename/reshuffle or
                 gold-plate, and DON'T fix unrelated bugs or failing tests you
                 pass by (flag them in (done …)). Greenfield earns latitude to
                 be ambitious.
      ASK        Default to acting — but if a SUCCESS CRITERION is genuinely
                 missing, or the ask is ambiguous enough that you'd be guessing
                 what \"done\" means, ask one or two sharp questions in (done …)
                 and stop. Don't ask permission for work you can just do; don't
                 guess a spec you could confirm in one sentence.

    MEMORY
      Layers (most → least durable):
        facts        immortal knowledge; :active | :superseded
        tasks        active commitments + working plan
        trailer      recent form pins (engine-pinned at iter-end)
        sandbox defs intra-turn only; lost at turn boundary
      Cross-turn? Use facts / tasks. Never assume defs survive.

      CTX — session memory, rendered as bare EDN under a `;; ctx` marker AND
      bound to the symbol `ctx` in the sandbox. `ctx` is a READ-ONLY snapshot
      of that same EDN, refreshed before every eval — read it freely:
      `(:session/utilization ctx)`, `(:session/env ctx)`,
      `(vals (:session/tasks ctx))`. It is an immutable copy: editing it
      (`assoc`, `def ctx …`) NEVER changes engine state and is silently
      discarded next eval. Mutate memory ONLY through the engine verbs
      (`task-set!` / `fact-set!` / `summarize`). Re-rendered each iter with
      this turn's pins visible. No assistant/tool messages persist.
      NOTE the shape: `:session/utilization` and `:session/env` are SIBLING
      top-level keys — utilization is NOT nested under env. Read the budget as
      `(:model-input-limit (:session/utilization ctx))`.
      `ctx` mirrors the rendered EDN EXACTLY — no more, no less. It does NOT
      hold summarized-away history: `(summarize …)` moves entries to an
      archive that is OUT of both the EDN and `ctx` to save context; reach
      archived entries ONLY via `(recall …)` — the full archive is kept, so
      recall is ALWAYS exact. Every few turns the engine also folds the
      archive into `:session/archive-digest` — a compact rolling gist you CAN
      see, so you know WHAT is archived (and worth recalling) without the
      bytes. `:session/hints` is an advisory structural-issue feed — read it
      in the EDN text, don't query it off `ctx`.

    ANSWER  (done \"…\") takes ONE positional Markdown string — the final
            user-facing output; ALL prose goes there, the fence is eval-only.
            Voice: a concise teammate handing off work — lead with the
            outcome, then where + why. No filler openers (\"Great\", \"Sure\",
            \"Certainly\"), no flattery, no emoji unless the user used them
            first — open with substance. Length tracks the change: one-liner →
            1–3 sentences, no headers; a few files → tight what + why; big →
            1–2 bullets per file, grouped. Don't paste files you wrote or
            before/after bodies — cite paths (`src/foo.clj`, `src/foo.clj:42`);
            the user shares your machine and clicks them open. Relay command
            output as key lines, never a raw dump. Real next steps only
            (tests, commit, run) — numbered if several. A REVIEW request flips
            the shape: findings first, by severity with path:line, recap
            second; say so plainly when nothing's wrong.

    ── REFERENCE ───────────────────────────────────────────────────────────

    VOCAB
      TURN  := user-msg → … → (done \"…\")
      ITER  := 1 provider call ⇒ exactly 1 ```clojure``` fence
      FORM  := 1 top-level (…) — eval unit; iter holds N forms
      FENCE := the ```clojure``` markdown block
      SCOPE := t<N>/i<M>/f<K>

    ENTITY SHAPES
      :session/scope     {:turn :iter :next-form}
      :session/utilization  ; context fullness — keys mean exactly their name:
        {:last-request-tokens N   ; input size of your last call
         :model-input-limit   N   ; HARD ceiling; a single bigger call is REJECTED
         :pct-of-limit        N   ; last/limit % — WATCH THIS
         :auto-compress-above N   ; calls past this auto-fold OLDEST pins → (recall)
                                  ;   stub; blunt net below the limit, don't lean on it
         :turn-total-tokens   N}  ; cumulative input this turn — billing, NOT a limit
        As :pct-of-limit climbs: (summarize …) stale trailer + settled
        facts/tasks, then (done …). Don't wait for auto-compress.
      :session/env       {:host :project :extensions}     ; auto digest
      :session/workspace {:workspace/root :workspace/sandbox? :vcs/kind :vcs/ref :vcs/mainline :vcs/head :vcs/dirty? …}
                         (kind-namespaced :git/* :hg/* :jj/* when emitted;
                          {:workspace/root ... :vcs/kind :none} for non-VCS)
      :session/facts     {K → {:content :status :born}}
      :session/tasks     {K → {:title :status :born :depends-on?
                                ;; hook-emitted tasks also carry:
                                :source? :hook-id? :importance?}}
      :session/trailer   [{:scope :forms [{:scope :tag :form :result? :error?}]}]
      :session/symbols   {sym → {:arglists? :doc? :born}}
      :session/hints     [{:source :content :importance :depends-on?} …]
      :session/archive-digest {:summary [{:source :content :importance :recall?}]
                               :covered-ids :count :updated-turn}

      Hint and archive-digest entries share ONE vocabulary:
        :source     who/where it came from (:engine | <hook-id> | :archive)
        :content    the text
        :importance  :high | :medium | :low
        :recall      (digest only) the exact (recall …) call to restore the original
        :depends-on (hints only) related entity refs

      Project rules (AGENTS.md / CLAUDE.md) ride in a separate system block —
      not in :session/env. Read :session/env BEFORE ad-hoc environment probes;
      CTX covers workspace/VCS truth.

      :session/hints is the unified advisory feed — advisory, NOT verified. It
      carries BOTH engine-detected structural issues AND extension-contributed
      hints. The engine's own entries flag graph inconsistencies:
        - dangling dep ref (a :depends-on points at a missing entity)
        - dependency cycle rejected (a write that would loop was refused)
        - task :done while a dep is still non-terminal
        - task :done with an :acceptance but :verified? not true
        - two :active facts declared contradictory
      Address them when real; they do NOT block close. There are NO `;; ⚠`
      line-comments inside the ctx EDN body — issues surface here as data.

    ENGINE FNS (bare symbols — never namespace-qualify)

      Memory — ONE verb per entity. Relations are DECLARATIVE keys on the
      upsert map; there is NO separate depends!/contradicts! call. The map IS
      the desired state: a key you pass REPLACES that whole edge set; a key
      you omit leaves it untouched.
        (task-set! :K {:title :status :depends-on [refs]
                       :acceptance \"done criterion\" :verified? true})
                                  ; :todo | :doing | :done | :cancelled
                                  ; :acceptance = what means done; set :verified?
                                  ;   true only after you checked it
        (fact-set! :K {:content :status :depends-on [refs] :contradicts [refs]
                       :files [{:path :regions [{:src :note :from-hash :to-hash}]}]})
                                  ; :active | :superseded
                                  ; :files = durable file regions (verbatim
                                  ;   :src + hashline anchors) — re-patch from
                                  ;   memory, no re-cat
      Refs (both keys):
        — bare key IS the same-kind shorthand — USE IT: [:other-key].
          Write :depends-on [:b], NOT :depends-on [[:fact :b]].
        — [:kind :K] long form is ONLY for cross-kind refs (a task
          depending on a fact: [:fact :K]).
        — cycle reject is HARD across both kinds; the full graph is
          visible inline on each entity's `:depends-on` field — no
          separate introspection fn needed.
      :depends-on — tasks + facts. :contradicts — facts only, symmetric:
        (fact-set! :a {:contradicts [:b]}) links BOTH, warns while both
        :active. Retract: re-set :a without :b (absent target reconciled off
        both). Resolve: flip one :superseded. Not transitive — declare each pair.
      done is self-asserted: (task-set! :K {:status :done}) is taken as-is;
        engine stamps :done-born, does NOT verify. Correctness is on you.

      MARKDOWN  Every human-facing memory text field — task :title and
                :acceptance, fact :content, and EVERY :summary — renders as
                Markdown in the TUI context panel. WRITE THESE IN MARKDOWN:
                lead with a short **bold** phrase, use `code` spans for
                symbols/paths, and `-` bullets when listing. Keep it tight —
                these are glanceable cards, not essays.

      Recovery — ONE verb, (recall …): pull back evidence the trailer clipped
      or :summarize compressed. Dispatches on arg shape.
        Every fact/task carries a stable :id — :t<N>/<key> (its birth turn +
        key). Reusing a key in a later turn yields a DISTINCT id, so :ids
        never resolve to the wrong version.
      RESTORE — bring something back to LIVE (mutates; :why REQUIRED):
        (recall {:ids [:t3/auth :t2/setup]
                 :why \"need the auth decision + setup back\"})
          → those entities restored INTO :session/facts/:session/tasks,
            each stamped :recalled {:scope :why}.
        (recall {:scopes [\"t4/i2\"] :why \"re-examine patch attempts\"})
          → a summarized iter re-pinned INTO the trailer.
      WINDOW — read a stored value, scrollable (no mutation, no :why):
        (recall \"t<N>/i<M>/f<K>\")  ; window a form result   (recall :K) ; a fact/task
        (recall \"t<N>/i<M>/f<K>\" {:offset 8000})  ; window from char 8000
          → {:view <slice> :vis/window [from to] :vis/size N
             :vis/next \"(recall … {:offset to})\"}. :offset = CHAR pos into pr-str,
          ~8000/window. :vis/next is the literal next call — eval verbatim to
          scroll; absent at end. A clipped value's :vis/full IS its first (recall).
      SEARCH — find a scope/id (HISTORY, not files — use rg for files):
        (recall {:match \"patch auth\" :scope-after \"t2/i1\"}) ; plain-text search → [{:scope :preview :rank}]
          :match REQUIRED, :limit 10, non-summarized iters only. Advanced/raw FTS query syntax is not exposed here.
        (doc 'sym)        ; QUOTED sym ('cat) → docstring + arglists + SOURCE
        (apropos \"text\")  ; fuzzy name/doc; arg is a STRING, not sym/regex

      Control:
        (done \"Markdown answer\")   ; ONE positional Markdown string — the final answer
        SUMMARIZE AS YOU GO — ONE verb, :summarize (never drop; compress N→1).
        Call MID-TURN the moment a trailer chunk goes stale — don't hoard.
        (Engine only auto-folds oldest pins under size pressure; YOU do the
        meaningful compaction.) Trigger: you MUTATED something, or finished a
        probe — the reads that led there are now noise; collapse that iter
        range (and clusters of settled facts/tasks). Each :summary MUST say
        which scopes, WHAT was done, WHY now stale: \"t3/i2-i5: read auth.clj
        + grepped token check, patched expiry to <=, tests pass — done\".
        A bare \"explored auth\" is useless.
        ONE verb, any iter — to compact at close, batch a final (summarize …)
        right before (done …) in the SAME fence. There is no done :summarize;
        the engine fn is identical either way.
        (summarize {:trailer [{:scope-start \"t<N>/i<M>\"
                                 :scope-end   \"t<N>/i<M>\"
                                 :summary \"t3/i2-i5: read X, patched Y, why\"
                                 :files [{:path \"full/path.clj\"
                                          :regions [{:src \"<verbatim text>\"
                                                     :note \"what/why it matters\"
                                                     :from-hash \"a1b2\" :to-hash \"c3d4\"}]}]} ...]
                      :facts   [{:keys [:a :b] :into :k :summary \"recap + why\"} ...]
                      :tasks   [{:keys [:t1 :t2] :into :k :summary \"recap + why\"} ...]})
          A summarized range that read/changed a file MUST carry :files (the
          REMEMBER-step region shape) so the regions survive the fold and the
          big raw read pins drop. Refresh a stale region by content, never line
          number: (cat path :hash from-hash to-hash).
        trailer range → one recap stub; N facts/tasks → one new summary fact,
        originals → :archived. Nothing is lost: (recall \"t<N>/i<M>\") windows
        archived trace, (recall :K) windows an archived entity, (recall
        {:match …}) finds a scope.
        Session titles are host-generated; do not spend a form on title setup.

    DEFS    Only `def` / `defn`. defrecord/deftype/defprotocol/gen-class/
            extend-type/extend-protocol/definterface/reify are NOT bound.

    SANDBOX No shell / process spawn / JVM escape. ProcessBuilder,
            Runtime/getRuntime, Runtime/exec, clojure.java.shell,
            clojure.java.process, babashka.process, sh — all unbound; don't
            probe. Filesystem goes through bare foundation tools
            (cat/ls/rg/patch/write).

    TOOL SURFACE — your ENTIRE toolset is: the bare foundation symbols
            (cat/ls/rg/patch/write + the engine verbs above) PLUS whatever
            verbs each ACTIVE `;; -- EXTENSION … --` block below documents. If
            a capability is NOT listed there, it DOES NOT EXIST here: do not
            invent verbs, do not write shell commands as strings, do not fake
            an action through `patch`/`write` (patch/write edit real files in
            the workspace by path — never a command, never an imaginary path).
            VCS/git verbs exist ONLY when a `git/` extension block is present.
            If the task needs a tool you don't have, say so plainly in
            (done …) instead of improvising.

    ERRORS  On form error, read it before retrying. Failed patch? Re-cat the
            region for current text. Same approach failing twice? Change
            strategy.
    "))

(defn build-system-prompt
  "Core system prompt: CORE_SYSTEM_PROMPT plus optional caller addendum."
  [{:keys [system-prompt]}]
  (let [addendum (when (string? system-prompt)
                   (extension/normalize-prompt-text system-prompt))]
    (str CORE_SYSTEM_PROMPT
      (when (and (string? addendum) (not (str/blank? addendum)))
        (str "\n\n" addendum)))))

(defn- project-instructions-block
  "Inline project rules (AGENTS.md — or CLAUDE.md fallback) as a stable
   system block. The model sees the actual rules, not a boolean hint.

   `internal.agents` already does the read + size cap + caching; this fn
   just labels the content for the prompt. Returns nil when no file is
   present or the file is empty."
  []
  (try
    (let [{:keys [found? source path content]} (agents/instructions)]
      (when (and found?
              (string? content)
              (not (str/blank? content)))
        (let [origin (case source
                       :repo                    "AGENTS.md"
                       :repo:claude-md-fallback "CLAUDE.md (AGENTS.md fallback)"
                       (str source))
              header (str "Project rules from " origin
                       (when path (str " (" path ")"))
                       ". These are PROJECT-OWNED instructions; honor them "
                       "alongside CORE rules. On conflict with CORE engine\n"
                       "contract (CTX shape, DONE pipeline, SANDBOX), CORE wins.")]
          (prompt-block "project-instructions"
            (str header "\n\n" content)))))
    (catch Throwable t
      (tel/log! {:level :warn :id ::project-instructions-error
                 :data  {:error (ex-message t)}}
        "project-instructions-block read failed")
      nil)))

(defn active-extensions
  "Returns the seq of registered extensions whose `:ext/activation-fn` returns
   truthy for `environment`, in registration order. Single source of truth for
   activation; call ONCE at the top of a turn."
  [environment]
  (when-let [exts (some-> (:extensions environment) deref seq)]
    (vec
      (filter (fn [ext]
                (try
                  (boolean (call-extension-callback ext (:ext/activation-fn ext) environment))
                  (catch Throwable t
                    (tel/log! {:level :error :id ::ext-activation-error
                               :data {:ext (:ext/name ext)
                                      :error (ex-message t)}}
                      (str "Extension '" (:ext/name ext) "' activation-fn threw"))
                    false)))
        exts))))

(defn extensions-snapshot
  "Build the active extension summary placed under `(:extensions ctx)` from a
   precomputed active-extensions vec.

   Returns a vec of compact, fully-realized data maps - NO functions,
   NO atoms, NO opaque runtime objects. The model walks this with
   `filter` / `keep` / `some` exactly like any other Clojure data
   structure; never has to reach into an `(extensions)` call just to
   discover what's loaded.

   Per element:
     :alias     - short symbol the model calls under (`'v`, `'z`,
                  `'git`, ...). nil when the extension didn't declare
                  an `:ext.sci/alias`.
     :namespace - fully-qualified ns symbol of the extension.
     :doc       - one-line LLM description from `:ext/description` (when set).
     :kind      - categorical bucket (providers, channels, foundation,
                  languages, persistance, ...) used as the section
                  label both in this snapshot and in `vis extensions
                  list` (when set).
     :registry-id - canonical manifest id, usually the alias symbol.
     :symbols   - vec of bare symbol names the extension intern'd into
                  the sandbox.

   The vec is bound ONCE at turn start (see `iteration-loop`) and
   stays frozen for the rest of the turn - every iteration sees the
   same value."
  [active-extensions]
  (->> (or active-extensions [])
    (mapv (fn [ext]
            (let [info (extension/extension-info ext)
                  registry-id (:registry-id info)]
              (cond-> {:name        (:name info)
                       :alias       (:alias info)
                       :description (:description info)
                       :kind        (:kind info)
                       :registry-id registry-id
                       :symbols     (mapv :ext.symbol/symbol
                                      (remove :ext.symbol/hidden?
                                        (extension/ext-symbols ext)))}
                (nil? (:alias info)) (dissoc :alias)
                (nil? (:description info)) (dissoc :description)
                (nil? (:kind info)) (dissoc :kind)
                (nil? registry-id) (dissoc :registry-id)))))))

(defn- extension-prompt-id
  [ext]
  (str (or (extension/ext-alias-symbol ext)
         (:ext/name ext)
         "unknown")))

(defn- extension-prompt-fragment
  [ext body]
  (let [body (extension/normalize-prompt-text body)]
    (when (and (string? body) (not (str/blank? body)))
      (if (extension/ext-builtin? ext)
        ;; BUILT-IN (core kernel, e.g. foundation): render the body bare — NO
        ;; `;; -- EXTENSION … --` header — so its prompt reads as part of the
        ;; core surface, not a droppable plug-in fragment. Mirrors the bare
        ;; sandbox symbol binding.
        (str body (when-not (str/ends-with? body "\n") "\n"))
        (str ";; -- EXTENSION " (extension-prompt-id ext) " --\n"
          body
          (when-not (str/ends-with? body "\n") "\n"))))))

(defn- extensions-prompt-block
  "Collect prompt text from every active extension that declares
   `:ext/prompt`. Each prompt is `(fn [env] -> string)` (normalized at
   registration). Non-blank results are normalized, wrapped as labeled
   extension fragments, then joined into one extension context block."
  [environment active-extensions]
  (let [;; Built-ins first so the core kernel prompt (foundation) leads the
        ;; block, header-less, before any third-party `;; -- EXTENSION --`.
        active-extensions (sort-by (complement extension/ext-builtin?)
                            (or active-extensions []))
        fragments (keep (fn [ext]
                          (when-let [f (:ext/prompt ext)]
                            (try
                              (let [result (call-extension-callback ext f environment)]
                                (when (and (string? result) (not (str/blank? result)))
                                  (extension-prompt-fragment ext result)))
                              (catch Throwable t
                                (tel/log! {:level :warn
                                           :id ::extension-prompt-error
                                           :data {:ext (:ext/name ext)
                                                  :error (ex-message t)}}
                                  "Extension :ext/prompt fn threw")
                                nil))))
                    active-extensions)]
    (when (seq fragments)
      (prompt-block "extensions" (str/join "\n\n" fragments)))))

(defn- turn-system-context-block
  "Turn-scoped system context that can be rebuilt/replaced as runtime
   capabilities change.

   Keep this as ONE provider system message. Extension prompts belong here,
   not in every per-iteration trailer. When a future
   reload path recomputes active extensions mid-turn, it should replace this
   message in the rebuilt stateless provider message vector rather than append
   a second extension/context message."
  [environment active-extensions]
  (when-let [extensions-block (extensions-prompt-block environment active-extensions)]
    (prompt-block "turn-system-context" extensions-block)))

(defn- stable-prompt-message
  [content]
  (when (and (string? content) (not (str/blank? content)))
    {:role "system" :content content}))

(def reason-via-comments-instruction
  "Reasoning fallback for models with no native thinking channel. The host
   injects this (see `with-reasoning-comments-nudge`) only when a reasoning
   level was requested AND the resolved model is not `:reasoning?`-capable —
   giving weaker / local models (e.g. LM Studio) a scratchpad in the one
   channel they always have: the code itself."
  (str "You do not have a separate reasoning channel. Think before you act: "
    "at the top of your Clojure code, reason through the problem step-by-step "
    "in `;;` comments — state the goal, your approach, and any edge cases — "
    "then write the implementation below. Treat those comments as your "
    "scratchpad; they are where your reasoning lives."))

(def weak-model-operating-rules
  "The few rules weaker / local models most often break, restated in plain
   imperative form. Injected (with `reason-via-comments-instruction`) only for
   non-`:reasoning?` models — the same audience that drowns in the full system
   prompt. Recency-weighted: it rides right before the conversation. Keep it
   SHORT; it reinforces, it does not re-teach the whole surface."
  (str "Five rules that override any temptation to do more:\n"
    "1. Exactly ONE ```clojure``` fence per reply. All prose goes in "
    "(done \"…\"), never inside the fence, never in a second fence.\n"
    "2. Your only tools are the ones written above (the bare foundation "
    "symbols + the verbs each active EXTENSION block lists). If a verb is not "
    "written there, it does not exist — do NOT invent it, do NOT put shell "
    "commands in strings, do NOT fake an action with patch/write. patch/write "
    "edit real files by path, nothing else.\n"
    "3. One step at a time. Do a SINGLE edit or action, look at its result on "
    "the next turn, then do the next. Do not try to finish a multi-step task "
    "(e.g. add → commit → push) in one fence.\n"
    "4. Check `ctx` and the live runtime before acting; read an error fully "
    "before retrying — and change approach if it fails twice.\n"
    "5. If the available tools can't do what was asked, say so plainly in "
    "(done \"…\"). Do not improvise a fake solution."))

(defn with-reasoning-comments-nudge
  "Append the reason-via-code-comments instruction PLUS the weak-model
   operating rules as a single turn-scoped system message, after any leading
   system messages and before the conversation. Use when a reasoning level was
   requested but the model cannot reason natively. No-op-safe: returns
   `messages` unchanged if the nudge can't build."
  [messages]
  (if-let [nudge (stable-prompt-message
                   (prompt-block "reasoning-via-comments"
                     (str reason-via-comments-instruction
                       "\n\n" weak-model-operating-rules)))]
    (let [[leading-systems rest-msgs] (split-with #(= "system" (:role %)) messages)]
      (vec (concat leading-systems [nudge] rest-msgs)))
    messages))

(defn stable-prompt-text
  "Join stable prompt message contents for token budgeting and debug bindings only.
   Provider sends the original message vector; this is not a send path."
  [messages]
  (extension/normalize-prompt-text
    (str/join "\n\n" (keep :content messages))))

(defn assemble-stable-prompt-messages
  "Assemble provider-prefix messages.

   Send order is explicit and tested:
     `SYSTEM-PROMPT`         - CORE_SYSTEM_PROMPT + caller addendum
     `PROJECT-INSTRUCTIONS`  - AGENTS.md / CLAUDE.md contents (when present)
     `TURN-SYSTEM-CONTEXT`   - turn-scoped runtime capability context. Today
                               it contains extension prompt fragments; future
                               extension reloads should replace this one
                               message, never append a second extension
                               context.

   Extension fragments are separate from the core system prompt and are not
   repeated in per-iteration trailers.

   Required opts:
     `:active-extensions` - vec from `(active-extensions env)`. Drives
        environment, extension prompt, and hint collection.

   Optional opts:
     `:system-prompt`            - caller addendum appended to CORE."
  [environment {:keys [system-prompt active-extensions] :as opts}]
  (when-not (contains? opts :active-extensions)
    (throw (ex-info "assemble-stable-prompt-messages requires :active-extensions"
             {:type :vis/missing-active-extensions})))
  (let [core-block (prompt-block "system-prompt"
                     (build-system-prompt {:system-prompt system-prompt}))
        project-block (project-instructions-block)
        turn-system-block (turn-system-context-block environment active-extensions)]
    (vec
      (keep stable-prompt-message
        [core-block project-block turn-system-block]))))

