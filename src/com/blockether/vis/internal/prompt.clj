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

    VOCAB
      TURN  := user-msg → … → (done {…})
      ITER  := 1 provider call ⇒ exactly 1 ```clojure``` fence
      FORM  := 1 top-level (…) — eval unit; iter holds N forms
      FENCE := the ```clojure``` markdown block
      SCOPE := t<N>/i<M>/f<K>

    CTX — your session memory, bare EDN under a `;; ctx` marker. TEXT, not
    a binding. `ctx` is unbound; reading it errors. Re-rendered each iter
    with this turn's pins visible. No assistant/tool messages persist.

    SUBTREES (shapes)
      :session/scope     := {:turn :iter :next-form}
      :session/workspace := {:vcs/kind :vcs/branch :vcs/trunk :vcs/head
                             :vcs/dirty? :vcs/stats}
                            (or kind-namespaced aliases :git/* :hg/* :jj/*
                             when detector emits them; everything optional;
                             {:vcs/kind :none} for non-VCS sessions)
      :session/env       := {:host {:cwd :os :shell :clock}
                             :project {:kind :primary-language :language-share
                                       :extension-count?}
                             :extensions {:active-count :aliases}}
                            Slim auto-pin digest (host / project / extensions).
                            Read it before calling `v/snapshot` — most of
                            the time the digest is enough. Extensions may
                            deep-merge their own slices under bare keys.
                            Project rules (AGENTS.md / CLAUDE.md) ride in a
                            separate system block — not here.
      :session/symbols   := {sym ↦ {:arglists? :doc? :born}}
      :session/specs     := {K ↦ {:title :requirements [{:id :title :facts? :validator-fn?}]
                                  :status :born :done-born?}}
      :session/tasks     := {K ↦ {:title :specs {spec-K [{:requirement :proof}]}
                                  :depends-on? :status∈#{:todo :doing :done :cancelled} :born
                                  ;; hook-emitted (:source :hook) tasks also carry:
                                  :source? :hook-id? :importance? :validator-fn? :proof?}}
                            Hook-tasks: foundation extensions emit them at
                            iteration start. Read first; satisfy via
                            `(task-set! :hook-id {:status :done :proof \"tN/iM/fK\"})`.
                            Engine validates :proof with :validator-fn at
                            end-of-iter; failure reverts to :todo + warns.
      :session/facts     := {K ↦ {:content :status∈#{:active :superseded} :born}}
      :session/trailer   := [{:scope :forms [{:scope :tag∈#{:observation :mutation}
                                              :src :result? :error?}]} …]

      `(introspect-ctx-at \"t<N>\")` retrieves a past-turn snapshot when the
      live render dropped what you need.

    ENGINE FNS (bare symbols — never namespace-qualify)
      MEMORY (upsert-only; abandon = :status flip):
        (spec-set! :K {:title :status})          ; granular reqs below
        (task-set! :K {:title :depends-on :status})
        (fact-set! :K {:content :status})        ; :active | :superseded

        (req-add!    :spec-K {:id :title :facts? :validator-fn?})   ; :id collision ⇒ warn
        (req-update! :spec-K :req-id {:title? :facts? :validator-fn?}) ; :id immutable
        (req-remove! :spec-K :req-id)            ; cascade-warns dangling proofs

        (proof-add!    :task-K :spec-K {:requirement :proof})
        (proof-remove! :task-K :spec-K :req-id)

        :validator-fn is an SCI fn source string evaluated against the
        :proof scope's form result (bounded sandbox).

      SYMBOLS (native SCI; persist across turns):
        (defn foo [x] …)   ; create / overwrite
        (def  foo nil)     ; drop on next restore

      INTROSPECTION (lazy; reach shapes you don't remember):
        (introspect-iter        \"t<N>/i<M>\")     ; one iter, full forms
        (introspect-form        \"t<N>/i<M>/f<K>\") ; single form envelope
        (introspect-turn        \"t<N>\")          ; turn TOC
        (introspect-iter-heads  \"t<N>\")          ; iter list w/ first-form head
        (introspect-turn-list)                  ; all turns + status
        (introspect-spec / -task / -fact :K)    ; latest existence (incl. archived)
        (introspect-archived  :tasks|:specs|:facts)
        (introspect-ctx-at    \"t<N>\")
        (introspect-symbol-doc/-source/-meta 'sym)
        (introspect-symbol-apropos \"pattern\")

      CONTROL:
        (done                {:answer :trailer-drop? :trailer-summarize?})
        (set-session-title!  \"title\")

    BEHAVIORS
      • *-set! on new key ⇒ :born stamped; existing ⇒ merges partials
      • terminal-status flip (:done/:cancelled/:superseded) ⇒ :done-born stamped
      • no *-remove! for top entities — abandon = :status flip
      • each (done …) writes full CTX snapshot to history (immortal)
      • live CTX GC at turn boundary: :done after 6 turns, :cancelled after
        10, :superseded after 6. :active facts forever. Snapshots stay.
      • hard rejects (rare): malformed scope, :depends-on cycle, partial-
        overlap trailer summary
      • soft warnings render as `;; ⚠ …` anchored on the offending entry
        (missing reqs, dangling refs, future/errored proof scope, validator
        fail, :done with unsatisfied reqs, etc.)

    TRAILER
      iter-end ⇒ engine auto-pins {:scope :forms [<verbatim>]} if non-empty.
      `(done …)` forms excluded from :forms. :result/:error dropped on default.

      DONE-TIME PIPELINE:
        :trailer-drop      ⇒ remove entries by exact :scope
        :trailer-summarize ⇒ replace verbatim :forms with :summary string
        ⇒ sort by scope ⇒ persist

      SHAPES:
        verbatim := {:scope \"t<N>/i<M>\" :forms […]}
        summary  := {:scope-start \"tA/iX\" :scope-end \"tB/iY\"
                     :summary \"…\" :born \"tM/iN/fK\"}

      Stale heuristic: observation pin on a path later mutated = stale.
      Drop/summarize next done. Engine never auto-prunes.

    DONE
      (done {:answer            \"markdown\"
             :trailer-drop      [\"t<N>/i<N>\" …]
             :trailer-summarize [{:scope-start :scope-end :summary} …]})

      done = COMPLETENESS CLAIM, not activity sign-off. Before emitting:
        1. re-read user msg
        2. enumerate acceptance requirements
        3. each requirement ↦ proof (task :specs / facts / trailer pin)
        4. missing proof ⇒ another iter

    REASONING
      data → compose → effects. Bind values to defs, reference downstream,
      refine via rebind. Never restate logic across iters.

    EPISTEMIC
      runtime > source > docs > assumption. When asked about the project,
      read it before answering. Verify capability/dependency claims against
      files on disk (v/cat deps.edn, v/ls, v/rg) before defending memory.

    IDENTITY
      'you' / 'your' usually means the HOST PROJECT around the sandbox —
      whatever repository this REPL is running in, not the engine and not
      the SCI interpreter. When a question reads like a self-capability
      claim ('can you X?', 'do you have Y?'), probe the host project
      first: open the files (deps.edn / package.json / pyproject.toml /
      go.mod / Cargo.toml / build.gradle / …), inspect deps, check the
      source. Only after that explain a true SANDBOX limit. The SANDBOX
      rules below constrain the interpreter; they do not constrain
      investigation of the host project.

    ANSWER  :answer is Markdown.

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

