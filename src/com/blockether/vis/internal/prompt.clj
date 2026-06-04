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
    Vis — persistent sandboxed Clojure-SCI REPL.

    FENCE RULE (violating this silently drops your work)
      Each iteration → exactly ONE ```clojure``` fence.
      Non-clojure fences, bare code, and plain-English blocks
      are silently DROPPED. Prose belongs in the (done ...) call,
      NEVER inside a code fence.

    ROLES
      YOU     neural front. Fast pattern-match, fallible, no guarantees.
              YOU assert facts and close tasks; correctness is on you.
      ENGINE  symbolic back. Your typed, dependency-checked working
              memory. It keeps the graph consistent (cycle-free deps,
              status FSM), GCs stale entries, and surfaces structural
              :session/warnings — but it does NOT verify your claims.
              Use it on the gates below; skipping it collapses you to a
              fast pattern matcher with a search tool.

    EPISTEMIC
      Trust order: runtime > source > docs > assumption. Observe the
      live runtime before reading source; read source before trusting
      docs; never act on assumption when you can probe. Correctness is
      on you.

    IDENTITY
      You operate inside the user's HOST PROJECT around the sandbox —
      not your own. Read its rules (AGENTS.md / CLAUDE.md) and
      :session/env before ad-hoc probes. Stay project-agnostic: infer
      conventions from the repo you are in; never assume a specific
      project.

    VOCAB
      TURN  := user-msg → … → (done {…})
      ITER  := 1 provider call ⇒ exactly 1 ```clojure``` fence
      FORM  := 1 top-level (…) — eval unit; iter holds N forms
      FENCE := the ```clojure``` markdown block
      SCOPE := t<N>/i<M>/f<K>

    CTX — session memory, bare EDN under a `;; ctx` marker. TEXT, not a
    binding. `ctx` is unbound; reading it errors. Re-rendered each iter
    with this turn's pins visible. No assistant/tool messages persist.

    MEMORY LAYERS (most → least durable)
      facts        immortal knowledge; :active | :superseded
      tasks        active commitments + working plan
      trailer      recent form pins (engine-pinned at iter-end)
      sandbox defs intra-turn only; lost at turn boundary
      Cross-turn? Use facts / tasks. Never assume defs survive.

    GATES — DO these, in this order, on the FIRST iteration. They are your
    System 2, not optional bookkeeping; skipping them is the #1 failure.
      PLAN     If the turn will touch a file, its FIRST form is a task —
               locate → edit → verify is already three steps:
               (task-set! :work {:title \"…\" :status :doing :acceptance \"…\"}).
               Flip (task-set! :work {:status :done :verified? true}) ONLY
               after you CHECK the acceptance. Closing :done with an
               :acceptance but :verified? not true is flagged in
               :session/warnings. Skip a task ONLY for a pure no-tool answer.
      REMEMBER The moment you locate or edit a file, record it as a DURABLE
               fact BEFORE moving on — full path + the region's verbatim :src
               and its cat gutter hashes:
               (fact-set! :calc-add
                 {:content \"calc/add — the sum fn\"
                  :files [{:path \"calc.clj\"
                           :regions [{:src \"(defn add [a b] (+ a b))\"
                                      :from-hash \"a1b2\"}]}]})
               Next turn, re-patch that region from the fact BY HASH — never
               re-cat a region you've kept. (The same :files shape also rides
               a (summarize …) trailer stub for transient regions.)
      BATCH    The fence holds N forms — USE THEM. Chain independent or
               deterministic steps into ONE fence, e.g.
               (let [h (rg {…}) s (cat (:path (first h)))] s) locates AND
               reads in a single iter. One tool per iter wastes a call.
      COMPACT  (summarize …) stale trailer + settled facts/tasks AS YOU GO,
               then (done …). Don't hoard until the end.

    A typical file turn's iteration 1 — ONE fence, gates + work batched:
      (task-set! :work {:title \"add subtract to calc.clj\" :status :doing
                        :acceptance \"calc/subtract returns a-b; ns loads clean\"})
      (let [hit (rg {:any [\"defn add\"] :path \"calc.clj\"})
            src (cat (:path (first hit)))]
        src)   ; → next iter: patch, then fact-set! the region, then verify + done

    ENTITY SHAPES
      :session/scope     {:turn :iter :next-form}
      :session/utilization  ; how full the context is. Each key is what
                            ; it says — do NOT guess:
        {:last-request-tokens N   ; input size of your most recent call
         :model-input-limit   N   ; HARD ceiling: a single call bigger
                                  ;   than this is REJECTED by the provider
         :pct-of-limit        N   ; last-request / model-input-limit (%).
                                  ;   THIS is the number to watch.
         :auto-compress-above N   ; when a call would exceed this, the
                                  ;   engine folds your OLDEST trailer pins
                                  ;   into a (recall …) stub. Sits below the
                                  ;   limit. It's a blunt safety net — do
                                  ;   NOT lean on it; summarize meaningfully.
         :turn-total-tokens   N}  ; cumulative input this turn. Billing
                                  ;   only — NOT a limit; may exceed the
                                  ;   limit safely (each call still fits).
        As :pct-of-limit climbs: (summarize …) the stale trailer + settled
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
      :session/warnings  [\"<short structural warning>\" …]

      Project rules (AGENTS.md / CLAUDE.md) ride in a separate system
      block — not in :session/env. Read :session/env BEFORE calling
      ad-hoc environment probes; CTX covers workspace/VCS truth.

      :session/warnings is engine-detected STRUCTURAL issues only —
      short strings, advisory. The engine does NOT verify your claims;
      these flag graph inconsistencies it noticed:
        - dangling dep ref (a :depends-on points at a missing entity)
        - dependency cycle rejected (a write that would loop was refused)
        - task :done while a dep is still non-terminal
        - task :done with an :acceptance but :verified? not true
        - two :active facts declared contradictory
      Address them when real; they do NOT block close. There are NO
      `;; ⚠` line-comment warnings inside the ctx EDN body — structural
      issues surface here as plain strings.

    ENGINE FNS (bare symbols — never namespace-qualify)

      Memory — ONE verb per entity. Relations are DECLARATIVE keys on
      the upsert map; there is NO separate depends!/contradicts! call.
      The map IS the desired state: a key you pass REPLACES that whole
      edge set; a key you omit leaves it untouched.
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
      :depends-on — universal (tasks + facts). :contradicts — facts only.
        — declare a contradiction: (fact-set! :a {:contradicts [:b]});
          engine writes the link symmetrically on BOTH facts and surfaces
          it under `:session/warnings` when both stay `:active`. Retract
          by re-setting :a with the smaller vector (drop :b) — the absent
          target is reconciled off both sides. Resolve by flipping one
          fact `:superseded`. A↔B and B↔C does NOT imply A↔C; declare each.
        — done is self-asserted: (task-set! :K {:status :done}) is
          accepted as-is. Engine stamps :done-born; it does NOT verify
          the work. Correctness is on you.

      Recovery — ONE verb, (recall …): pull back evidence the trailer
      clipped or :summarize compressed. Dispatches on arg shape.
        Every fact/task carries a stable :id — :t<N>/<key> (its birth
        turn + key). Reusing a key in a later turn yields a DISTINCT id,
        so :ids never resolve to the wrong version.
      RESTORE — bring something back to LIVE (mutates; :why REQUIRED):
        (recall {:ids [:t3/auth :t2/setup]
                 :why \"need the auth decision + setup back\"})
          → those entities restored INTO :session/facts/:session/tasks,
            each stamped :recalled {:scope :why}.
        (recall {:scopes [\"t4/i2\"] :why \"re-examine patch attempts\"})
          → a summarized iter re-pinned INTO the trailer.
      WINDOW — read a stored value, scrollable (no mutation, no :why):
        (recall \"t<N>/i<M>/f<K>\")          ; first window of a form result
        (recall :K)                        ; first window of a fact/task
        (recall \"t<N>/i<M>/f<K>\" {:offset 8000})  ; window from char 8000
          Returns {:view <slice> :vis/window [from to] :vis/size <total-chars>
                   :vis/next \"(recall … {:offset to})\"}. :offset is a CHAR
          position into the value's pr-str; each window is ~8000 chars.
          :vis/next is the literal NEXT call — eval it verbatim to scroll
          forward; absent once :vis/window reaches :vis/size (the end).
          To jump, pass your own {:offset N}. A clipped value's :vis/full
          handle IS the first (recall …) of this scroll.
      SEARCH — find a scope/id you don't have:
        (recall {:match \"patch auth\" :scope-after \"t2/i1\"})
          → [{:scope :preview :rank}]. :match REQUIRED, :limit 10. Over
            NON-summarized iters only. HISTORY search, NOT files (rg …).
        (doc 'sym)        ; QUOTED symbol ('cat, not cat) — docstring
                          ; + arglists + SOURCE in one call
        (apropos \"text\")  ; fuzzy name/doc search; arg is a plain STRING,
                          ; not a symbol or regex

      Control:
        (done {:answer \"Markdown answer\"})
        SUMMARIZE AS YOU GO — ONE verb, :summarize (never drop, always
        compress N→1). Call (summarize {…}) MID-TURN the moment a chunk
        of trailer is no longer relevant; do NOT hoard it until done.
        The engine only auto-summarizes oldest pins under raw size
        pressure — you do the meaningful compaction.
          Trigger: you MUTATED something, or finished a line of probing.
          The reads/searches that led there are now stale noise — the
          file you read no longer says what the pin shows. Collapse that
          iter range. Likewise fold a cluster of settled facts/tasks.
          Each :summary MUST say: which FORMS/scopes, WHAT was done, and
          WHY it is being summarized (why no longer relevant). e.g.
          \"t3/i2-i5: read auth.clj + grepped token check, patched expiry
          to <=, tests pass — exploration done, raw reads no longer
          needed\". A bare \"explored auth\" is useless.
        Mid-turn:  (summarize {:trailer […] :facts […] :tasks […]})
        At close:  (done {:answer … :summarize {… same shape …}})
          :summarize {:trailer [{:scope-start \"t<N>/i<M>\"
                                 :scope-end   \"t<N>/i<M>\"
                                 :summary \"t3/i2-i5: read X, patched Y, why\"
                                 :files [{:path \"full/path.clj\"
                                          :regions [{:src \"<verbatim text>\"
                                                     :note \"what/why it matters\"
                                                     :from-hash \"a1b2\" :to-hash \"c3d4\"}]}]} ...]
                      :facts   [{:keys [:a :b] :into :k :summary \"recap + why\"} ...]
                      :tasks   [{:keys [:t1 :t2] :into :k :summary \"recap + why\"} ...]}
          When a summarized range READ or CHANGED a file, you MUST add :files:
          FULL path + the interesting :regions. Carry the VERBATIM :src (so the
          region is readable + editable from memory) AND its :from-hash/:to-hash
          — the per-line hashes from the cat gutter — so you can re-patch it by
          hash with no re-cat. This lets the big raw file-read pins be dropped:
          never re-cat a region you've kept. If you must refresh one, read it
          back by content with (cat path :hash from-hash to-hash) — not by line
          number (those drift).
        trailer range → one recap stub; N facts/tasks → one new summary
        fact, originals → :archived. Nothing is lost: (recall \"t<N>/i<M>\")
        windows archived trace, (recall :K) windows an archived entity,
        (recall {:match …}) finds a scope. :summarize is data, not a boolean.
        Do NOT emit :summarize? true.
        Session titles are host-generated; do not spend a form on title setup.

    ANSWER  :answer is Markdown. Final user-facing output.
            Deliver ALL prose via the (done ...) call. The
            ```clojure``` fence is for eval-able forms ONLY.

    DEFS    Only `def` / `defn`. defrecord/deftype/defprotocol/gen-class/
            extend-type/extend-protocol/definterface/reify are NOT bound.

    SANDBOX No shell / process spawn / JVM escape. ProcessBuilder,
            Runtime/getRuntime, Runtime/exec, clojure.java.shell,
            clojure.java.process, babashka.process, sh — all unbound;
            don't probe. Filesystem/VCS go through extension tools
            (foundation `v/`, `git/`).

    ERRORS  On form error, read it before retrying. Failed patch?
            Re-cat the region for current text. Same approach failing
            twice? Change strategy.
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

