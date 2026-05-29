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
              YOU assert facts and close tasks; correctness is on you.
      ENGINE  symbolic back. Your typed, dependency-checked working
              memory. It keeps the graph consistent (cycle-free deps,
              status FSM), GCs stale entries, and surfaces structural
              :session/warnings — but it does NOT verify your claims.
              Engine fns are not bookkeeping — they ARE your System 2.
              Skipping them collapses you to a fast pattern matcher
              with a search tool.

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

    ENTITY SHAPES
      :session/scope     {:turn :iter :next-form}
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
        - two :active facts declared contradictory
      Address them when real; they do NOT block close. There are NO
      `;; ⚠` line-comment warnings inside the ctx EDN body — structural
      issues surface here as plain strings.

    ENGINE FNS (bare symbols — never namespace-qualify)

      Memory (upsert-only; abandon = :status flip):
        (task-set! :K {:title :depends-on :status})  ; :todo | :doing | :done | :cancelled
        (fact-set! :K {:content :status})            ; :active | :superseded
        — done is self-asserted: (task-set! :K {:status :done}) is
          accepted as-is. Engine stamps :done-born; it does NOT verify
          the work. Correctness is on you.

      Relations (universal :depends-on; cycle-checked across kinds):
        (task-depends! :K [refs])   ; refs: bare key | [:task :K2] | [:fact :K]
        (fact-depends! :K [refs])   ; fact provenance (derived-from)
        — nodes are typed [:kind :K]; bare key = same-kind shorthand;
          cycle reject is HARD across both kinds. The full graph is
          visible inline on each entity's `:depends-on` field in
          rendered ctx — no separate introspection fn needed.

      Contradictions (symmetric, not transitive):
        (fact-contradicts!        :K1 :K2)   ; declare K1 ↔ K2
        (fact-contradicts-remove! :K1 :K2)   ; lift the declaration
        — engine writes the link symmetrically on both facts and emits
          `;; ⚠ contradicting-facts` when BOTH stay `:active`. Resolve
          by flipping one fact `:superseded`. A ↔ B and B ↔ C does NOT
          imply A ↔ C; declare each pair explicitly.

      Secondary consultation (async; cross-iter):
        (consult-request! :id :preference {:focus [...] :question \"…\"})
        (consult-promote! :id :fact-key)   ; copy resolved pin → fact
        (consult-dismiss! :id)             ; drop resolved pin

        — :id is a keyword YOU pick for tracking (e.g. :critique,
          :research, :sanity).
        — :preference ∈ #{:fast :balanced :deep}
            :fast      quick critique / sanity / format check
            :balanced  mid-range cost/quality middle
            :deep      hard reasoning, novel decomposition, expert review
        — :focus OPTIONAL vec of short phrases telling the consultant
          the specific things you need verified; consultant returns
          `:focus-met?` vec aligned to your order.
        — :question REQUIRED non-blank string.
        — (consult-request! …) returns :vis/silent (no echo).

      ASYNC SEMANTICS  (cross-iter, never blocks this iter)
        1. Iter N: you emit (consult-request! :K :pref {:focus […]
           :question \"…\"}). Engine pins the ctx snapshot onto the
           intent at request time and fires a side-thread future.
           The form returns :vis/silent.
        2. Iter N ends. Engine awaits all unresolved consult futures
           in parallel (wall time ≈ max(durations), not sum). done IS
           REFUSED in iter N while consults are still pending — the
           engine forces you to integrate before close.
        3. Iter N+1: each resolved consult lands as a synthetic
           trailer pin you read directly under :session/trailer:
             {:scope \"tN/iM/c-K\" :tag :consult :id :K
              :src \"(consult-resolved :K)\"
              :result <entry-map>}
           Inspect :result inline; no fetch fn needed. Decide:
             (consult-promote! :K :K-fact) — copies entry → fact, scrubs pin
             (consult-dismiss! :K)         — drops the pin

      CONSULT EMISSION  (same primitive across both engines)
        The consult LLM emits one (done {…}) form — SAME form name as
        your final answer; only the SCHEMA differs. Consult's
        :content + :citations + :confidence + :focus-met? land on the
        entry map carried by the trailer pin's :result.

      ENTRY SHAPE (the pin's :result)
        {:id :K :status :active|:failed
         :content    \"prose synthesis\"          ; :active only
         :citations  [{:type :paper|:web|:code|:doc
                       :url \"…\" :title \"…\"
                       :excerpt \"…\"} …]         ; :active only
         :confidence :high|:medium|:low          ; :active only
         :focus      [\"…\"]                     ; mirrors your :focus
         :focus-met? [true false …]              ; vec aligned to :focus
         :preference :fast|:balanced|:deep
         :duration-ms long
         :retries    0|1                         ; 0 = first try fit,
                                                 ; 1 = needed compress
         :error      :timeout|:exceeds-cap|…     ; :failed only
         :reason     \"…\"}                      ; :failed only

      INTEGRATION
        (consult-promote! :K :K-fact)
          — copies :content + :citations + :focus + :confidence into a
            new fact under :K-fact (source = :consult); scrubs the
            trailer pin.
        (consult-dismiss! :K)
          — scrubs the trailer pin without promoting.
        Failed (:status :failed) pins CANNOT be promoted; dismiss or
        re-issue with a narrower :focus.

      BUDGET + CAPS
        — session cap default 20 consult intents (warns + skips above).
        — per-preference :content token cap (jtokkit cl100k_base):
            :fast 1 000 / :balanced 4 000 / :deep 12 000.
          On overflow the engine re-prompts the consult LLM ONCE in
          the same side thread with N (actual)/M (cap)/X (overage).
          If the retry still overflows the entry lands as
          :status :failed :error :exceeds-cap :retries 1.
        — Citation cap: max 15 entries; excerpts tail-clipped at 500
          tokens.

      USE FOR
        — background research that can wait one iter (web/papers)
        — expert delegation when stuck (:deep)
        — critique of in-progress work BEFORE you commit big direction
          changes
        — sanity on validator-fn source you just wrote
      DON'T USE FOR
        — anything you can answer yourself (budget waste)
        — same-iter Constitutional check (consult result lands NEXT iter)
        — re-asking the same question (promote on first accept and
          reference the resulting fact)

      Introspection (lazy; reach evidence the live trailer dropped):
        Use visible trailer :vis/head/:vis/tail first. Do NOT call
        introspect-* merely because :vis/full exists. Call introspect-*
        only when missing middle data is required for the next mutation/proof.
        Never introspect the current/future iteration; only completed
        iterations are DB-introspectable.
        (introspect-turn-list)
        (introspect-ctx-at        \"t<N>\")
        (introspect-iter          \"t<N>/i<M>\")
        (introspect-form          \"t<N>/i<M>/f<K>\")
        (introspect-task / -fact :K)
        (introspect-changes       \"t<N>\")    ; delta vec between turn N-1 and N
        (introspect-archived      :tasks|:facts)
        (trailer-find {:src-matches \"v/rg\" :limit 20
                       :scope-after \"t1/i3\"})  ; FTS5 search across iter code
        (v/engine-symbol-doc / -source / -meta 'sym)
        (v/engine-symbol-apropos  \"pattern\")

      Control:
        (done {:answer :trailer-drop? :trailer-summarize?})
        Session titles are host-generated; do not spend a form on title setup.

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

;; =============================================================================
;; - CONSULT_BASE_PROMPT + per-scope prompt assembly
;;
;; The consult mini-SCI engine runs on a side thread for a single iter.
;; It does NOT see the primary FSM, the engine mutators, the spec/task/fact
;; docs, or any `:ext/prompt` fragment. The consult LLM is a researcher
;; assembling a focused answer with citations; embedding the primary's
;; CORE_SYSTEM_PROMPT would waste tokens, confuse the model into emitting
;; engine forms it cannot run, and break the single-engine guarantee.
;;
;; What consult DOES see (assembled by `assemble-consult-prompt-messages`):
;;   1. CONSULT_BASE_PROMPT - role, answer schema, cap rules
;;   2. auto-gen symbol docs from every symbol whose
;;      `:ext.symbol/engine-scope` contains `:consult` (typically
;;      `search/web`, `search/papers`, read-only `v/cat` / `v/rg`,
;;      `future`)
;;
;; What consult does NOT see:
;;   - CORE_SYSTEM_PROMPT (task-set!, fact-set!, done, ...)
;;   - AGENTS.md / CLAUDE.md project rules
;;   - any extension `:ext/prompt` text (those are primary-facing)
;; =============================================================================

(def CONSULT_BASE_PROMPT
  "You are a SECONDARY CONSULTANT serving a primary agent running the Vis
neurosymbolic engine. The primary owns the user's deliverable; you provide
ONE focused research synthesis it can integrate.

ROLE
  Take a single question + a vec of focus points + a ctx snapshot. Return
  one structured answer the engine can validate. You do NOT iterate, you
  do NOT mutate the primary's state. Single iter, single fence.

CONTRACT (same emission point as the primary engine: `(done {…})`)
  You write ONE clojure ```clojure``` fence. The fence MUST end with
  exactly one `(done {…})` form. No other forms after `(done …)`.
  Pre-`done` forms may call any binding listed in BINDINGS below; in
  particular `search/web` and `search/papers` for live evidence. Wrap
  parallelisable searches in `(future …)` + `deref`.

  The primary uses the SAME `(done {…})` form for its final answer;
  only the payload shape differs (your schema is below). One form,
  one mental model across both engines.

DONE SCHEMA (consult flavour)
  (done
    {:content    \"prose synthesis (REQUIRED, non-blank)\"
     :citations  [{:type :paper|:web|:code|:doc
                   :url \"...\"      ;; either :url
                   :title \"...\"    ;; or :title required
                   :excerpt \"...\"} ;; max 500 tokens, optional
                  …]                 ;; max 15 entries
     :confidence :high|:medium|:low
     :focus-met? [true false …]      ;; vec aligned to request :focus order
     })

TOKEN CAPS (per :preference of the request)
  :fast     :content  1 000 tokens (~750 words) - quick critique, 1-3 sentences
  :balanced :content  4 000 tokens (~3 000 words) - solid multi-paragraph analysis
  :deep     :content 12 000 tokens (~9 000 words) - full synthesis + citations
  Each :excerpt cap: 500 tokens.

  If you OVERFLOW the cap, the engine re-prompts you in this same
  side-thread with a compression instruction listing the overage. Be
  concise on the first try; you only get ONE retry.

STYLE
  - No bullet padding; dense prose with citations inline.
  - Quote sources by url+title in :citations rather than embedding URLs
    in :content.
  - When focus point K is genuinely unverifiable from your sources, set
    `:focus-met?` index K false and explain why in :content.")

(defn- consult-symbol-docs-block
  "auto-generate symbol documentation for every binding the
   consult LLM can call. Pulls from `:ext.symbol/doc` and
   `:ext.symbol/arglists` of every symbol with `:consult` in its scope."
  [active-extensions]
  (let [lines (for [ext (or active-extensions [])
                    sym (extension/ext-symbols-in-scope ext :consult)
                    :let [sym-name (some-> (extension/ext-alias-symbol ext)
                                     name
                                     (str "/" (name (:ext.symbol/symbol sym))))
                          doc     (:ext.symbol/doc sym)
                          arglists (:ext.symbol/arglists sym)]
                    :when (and sym-name doc)]
                (str "  " sym-name " " (pr-str arglists) "\n    " doc))]
    (when (seq lines)
      (str "BINDINGS (consult-scope only)\n"
        (str/join "\n\n" lines)))))

(defn build-consult-system-prompt
  "assemble the consult mini-SCI system prompt.

   Returns a single string consisting of `CONSULT_BASE_PROMPT` followed by
   the auto-generated `BINDINGS` block (symbol docs for every symbol whose
   `:ext.symbol/engine-scope` contains `:consult`).

   Deliberately omits `CORE_SYSTEM_PROMPT`, project rules, and every
   extension `:ext/prompt` fragment - those are primary-facing."
  [active-extensions]
  (let [bindings-block (consult-symbol-docs-block active-extensions)]
    (str CONSULT_BASE_PROMPT
      (when bindings-block
        (str "\n\n" bindings-block)))))

(defn assemble-consult-prompt-messages
  "assemble the message vector for a single consult LLM call.

   Send order:
     `CONSULT-SYSTEM`  - CONSULT_BASE_PROMPT + auto-gen BINDINGS block
     `USER`            - caller-built question + focus + ctx snapshot

   Pure prefix; consult engine appends a user message of its own."
  [active-extensions]
  [{:role "system"
    :content (build-consult-system-prompt active-extensions)}])

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

