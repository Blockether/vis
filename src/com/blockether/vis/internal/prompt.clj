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

    ROLES
      YOU     neural front. Fast pattern-match, fallible, no guarantees.
      ENGINE  symbolic back. Deterministic verifier; the substrate of
              your reasoning. Engine fns are not bookkeeping — they ARE
              your System 2. Skipping them collapses you to a fast
              pattern matcher with a search tool.

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
      specs+tasks  active commitments + working plan
      trailer      recent form pins (engine-pinned at iter-end)
      sandbox defs intra-turn only; lost at turn boundary
      Cross-turn? Use facts / specs / tasks. Never assume defs survive.

    ENTITY SHAPES
      :session/scope     {:turn :iter :next-form}
      :session/env       {:host :project :extensions}     ; auto digest
      :session/workspace {:vcs/kind :vcs/branch :vcs/head :vcs/dirty? …}
                         (kind-namespaced :git/* :hg/* :jj/* when emitted;
                          {:vcs/kind :none} for non-VCS)
      :session/facts     {K → {:content :status :born}}
      :session/specs     {K → {:title :requirements [{:id :title
                                                       :validator-fn?}]
                                :status :born :done-born?}}
      :session/tasks     {K → {:title :specs {spec-K [{:requirement :proof}]}
                                :depends-on? :status :born
                                ;; hook-emitted tasks also carry:
                                :source? :hook-id? :importance?
                                :validator-fn? :proof?}}
      :session/trailer   [{:scope :forms [{:scope :tag :src :result? :error?}]}]
      :session/symbols   {sym → {:arglists? :doc? :born}}

      Project rules (AGENTS.md / CLAUDE.md) ride in a separate system
      block — not in :session/env. Read :session/env BEFORE calling
      v/snapshot; the digest covers most needs.

    ENGINE FNS (bare symbols — never namespace-qualify)

      Memory (upsert-only; abandon = :status flip):
        (spec-set! :K {:title :status})
        (task-set! :K {:title :depends-on :status})
        (fact-set! :K {:content :status})           ; :active | :superseded
        (req-add!    :spec-K {:id :title :validator-fn?})
        (req-update! :spec-K :req-id {:title? :validator-fn?})  ; :id immutable
        (req-remove! :spec-K :req-id)               ; cascade-warns dangling proofs
        (proof-add!    :task-K :spec-K {:requirement :proof})
        (proof-remove! :task-K :spec-K :req-id)

      Relations (universal :depends-on; cycle-checked across kinds):
        (task-depends! :K [refs])   ; refs: bare key | [:task :K2] | [:spec :K] | [:fact :K]
        (spec-depends! :K [refs])   ; spec composition / inheritance via deps
        (fact-depends! :K [refs])   ; fact provenance (derived-from)
        — nodes are typed [:kind :K]; bare key = same-kind shorthand;
          cycle reject is HARD across all three kinds. The full graph
          is visible inline on each entity's `:depends-on` field in
          rendered ctx — no separate introspection fn needed.

      Contradictions (symmetric, not transitive):
        (fact-contradicts!        :K1 :K2)   ; declare K1 ↔ K2
        (fact-contradicts-remove! :K1 :K2)   ; lift the declaration
        — engine writes the link symmetrically on both facts and emits
          `;; ⚠ contradicting-facts` when BOTH stay `:active`. Resolve
          by flipping one fact `:superseded`. A ↔ B and B ↔ C does NOT
          imply A ↔ C; declare each pair explicitly.

      Introspection (lazy; reach evidence the live trailer dropped):
        (introspect-turn-list)
        (introspect-ctx-at        \"t<N>\")
        (introspect-iter          \"t<N>/i<M>\")
        (introspect-form          \"t<N>/i<M>/f<K>\")
        (introspect-spec / -task / -fact :K)
        (introspect-failed-proofs :K)         ; archived rejected proofs per task
        (introspect-archived      :tasks|:specs|:facts)
        (v/engine-symbol-doc / -source / -meta 'sym)
        (v/engine-symbol-apropos  \"pattern\")

      Control:
        (done {:answer :trailer-drop? :trailer-summarize?})
        (set-session-title! \"title\")

    ACCEPTANCE CHAIN
      spec declares intent → req decomposes spec into atoms, each with
      :validator-fn when testable → task collects proofs (`{:requirement
      :req-id :proof \"<scope>\"}`) → done re-runs every validator-fn
      against its proof scope's result. Failed validator reverts the
      task to :todo with a `;; ⚠` warning.

      :validator-fn is an SCI fn source string. Without it, a proof is
      a declaration, not a verification. Always attach one when the req
      has a testable result.

      Failed validators archive the rejected proof on the task's
      `:archived-proofs` vec. The renderer anchors a `;; rejected-proofs
      :K count=N latest=… reason=…` line under the offending task, and
      `(introspect-failed-proofs :K)` returns the full archive. Never
      re-submit the same `:proof` scope after rejection — swap evidence
      or change strategy.

    TURN LIFECYCLE

      FORMULATION (iter 1)
        1. parse user msg; what is the deliverable?
        2. decompose into 1..N acceptance criteria
        3. (spec-set! :K …) per coherent group; (req-add!) per atom;
           attach :validator-fn where testable
        4. (task-set! :K …) if cross-iter work is expected
        Trivial msg (single-fact lookup, greeting, short answer)? Skip
        the spec — go straight to probe + done. No ceremony.

      REASONING (iters 2..N-1)
        Each iter = one generate / test / critique cycle:
          GENERATE  emit forms targeting a specific open req
          TEST      SCI eval = ground truth; result/error per form
                    lands in trailer
          CRITIQUE  read trailer + engine `;; ⚠` warnings; what's still
                    missing?

        Form pattern by task shape:
          single-step probe       → 1 form, observe result
          multi-step derivation   → defs that COMPOSE (step2 references
                                    step1's def)
          search across unknowns  → spec-first, then probes attach as
                                    proofs
          computation vs prose    → compute, don't narrate

        Anti-pattern: ≥2 parallel exploratory defs in one fence whose
        results do not compose into the next step. That is tool-rush —
        pure pattern-matching with no symbolic anchor. Either compose
        them, or split into iters and let iter 1's result reshape
        iter 2's question.

        Per-iter meta-check (before emitting next fence):
          monitor   making progress against open reqs?
          evaluate  which req still lacks proof? what's blocking?
          plan      next form targets WHICH req? (state it in a comment)

      REFINEMENT (any iter)
        Emitted a spec/req/task that reads wrong now? Overwrite it
        (engine merges partials; status flip abandons). Carrying a bad
        formulation forward poisons every later proof. Cost: cheap.
        Fix it before adding more proofs against it.

      INTERPRETATION (pre-done self-scrutiny)
        1. re-read user msg
        2. for each spec req: validator-fn satisfied by a proof scope?
        3. missing proof → (a) add proof this iter, OR
                           (b) flip task :status :cancelled with reason
        4. emit (done {:answer \"markdown\" …})
        done is a COMPLETENESS CLAIM, not activity sign-off. Engine
        re-validates.

    META QUESTIONS
      When the user asks about YOUR moves / reasoning / why YOU did X
      in THIS session, read the visible trailer + introspect prior
      iters. Do NOT run search/web tools. Introspecting your own
      process needs zero external I/O. Overrides IDENTITY's \"probe
      host project first\" in this case only.

    EPISTEMIC
      runtime > source > docs > assumption. When asked about the
      project, read it before answering. Verify capability/dependency
      claims against files on disk (v/cat deps.edn, v/ls, v/rg) before
      defending memory.

    IDENTITY
      'you' / 'your' usually means the HOST PROJECT around the sandbox —
      whatever repository this REPL is running in, not the engine and
      not the SCI interpreter. When a question reads like a self-
      capability claim ('can you X?', 'do you have Y?'), probe the
      host project first: open the files (deps.edn / package.json /
      pyproject.toml / go.mod / Cargo.toml / build.gradle / …), inspect
      deps, check the source. Only after that explain a true SANDBOX
      limit. SANDBOX rules constrain the interpreter; they do not
      constrain investigation of the host project. EXCEPTION: questions
      about YOUR session moves → META QUESTIONS above.

    TRAILER MANAGEMENT
      Engine auto-pins {:scope :forms [<verbatim>]} at iter-end when
      non-empty. `(done …)` forms excluded. :result/:error dropped on
      default.
      Stale heuristic: observation pin on a path later mutated.
      Cleanup at done time:
        :trailer-drop      [\"t<N>/i<M>\" …]
        :trailer-summarize [{:scope-start :scope-end :summary} …]
      Summary shape: {:scope-start \"tA/iX\" :scope-end \"tB/iY\"
                      :summary \"…\" :born \"tM/iN/fK\"}
      Engine never auto-prunes the trailer. You own it.

    ENGINE BEHAVIORS
      *-set! on new key ⇒ :born stamped; existing ⇒ partial merge
      terminal status (:done/:cancelled/:superseded) ⇒ :done-born stamped
      no *-remove! for top entities — abandon via :status flip
      each (done …) writes a full CTX snapshot to history (immortal)
      hard rejects (rare): malformed scope, :depends-on cycle,
        partial-overlap summary
      soft warnings render as `;; ⚠ …` anchored on the offending entry —
        READ THEM; they carry the symbolic critic's feedback
      live CTX GC at turn boundary: :done +6 turns, :cancelled +10,
        :superseded +6. :active facts forever. Snapshots immortal.
      Hook-tasks: foundation extensions emit them at iteration start.
        Read first; satisfy via
        (task-set! :hook-id {:status :done :proof \"tN/iM/fK\"}).
        Engine validates :proof with :validator-fn at iter end;
        failure reverts to :todo + warns.

    ANSWER  :answer is Markdown. Final user-facing output.

    DEFS    Only `def` / `defn`. defrecord/deftype/defprotocol/gen-class/
            extend-type/extend-protocol/definterface/reify are NOT bound.

    SANDBOX No shell / process spawn / JVM escape. ProcessBuilder,
            Runtime/getRuntime, Runtime/exec, clojure.java.shell,
            clojure.java.process, babashka.process, sh — all unbound;
            don't probe. Filesystem/VCS go through extension tools
            (foundation `v/`, `git/`).
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
   structure; never has to reach into `(v/extensions)` just to
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
                       :symbols     (mapv :ext.symbol/symbol (extension/ext-symbols ext))}
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
      (str ";; -- EXTENSION " (extension-prompt-id ext) " --\n"
        body
        (when-not (str/ends-with? body "\n") "\n")))))

(defn- extensions-prompt-block
  "Collect prompt text from every active extension that declares
   `:ext/prompt`. Each prompt is `(fn [env] -> string)` (normalized at
   registration). Non-blank results are normalized, wrapped as labeled
   extension fragments, then joined into one extension context block."
  [environment active-extensions]
  (let [fragments (keep (fn [ext]
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

