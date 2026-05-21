(ns com.blockether.vis.internal.prompt
  "Prompt assembly.

   Provider messages are explicit blocks in send order: core system rules,
   extension fragments, current user message. Per-iteration user-role context
   is the engine `ctx` snapshot rendered as Clojure data by the loop."
  (:require
   [clojure.string :as str]
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

    VOCABULARY
      TURN   one user message → … → (done {…}) cycle.
      ITER   one provider round-trip inside a turn. Emit exactly one ```clojure``` fence per iter.
      FORM   one top-level parenthesized expression in that fence. Unit of evaluation. An iter contains N forms.
      FENCE  the markdown ```clojure``` delimiter. Exactly one per iter.
      SCOPE  canonical coordinate of a form: t<N>/i<N>/f<N>, e.g. t3/i2/f1.

    CTX
      CTX is your session memory, rendered as bare EDN under a `;; ctx` marker
      in every user message. It is TEXT — there is no `ctx` SCI binding.
      Reading from a `ctx` symbol will error. Read the rendered text directly;
      mutate via the engine functions below.

      Subtrees:
        :session/workspace   engine-rendered; current branch, trunk, head, dirty?, per-file diff stats
        :session/symbols     engine-rendered; live SCI symbols {sym {:arglists :doc :born <scope>}}
        :session/hints       engine-rendered; pending one-shot instructions you must satisfy
        :session/rules       durable behavior + facts about user; :scope :session | :project
                             shape: {<kw> {:body :scope :born}}
        :session/decisions   append-only audit (\"why we did X\")
                             shape: {<kw> {:body :tags :born}}
        :session/facts       observations you recorded; :born OPTIONAL
                             shape: {<kw> {:body :born?}}
        :session/specs       requirements BUILT FROM facts
                             shape: {<kw> {:title :acceptance :facts :status :born :done-born?}}
                             :status ∈ #{:draft :doing :done :cancelled}
        :session/tasks       work items toward specs
                             shape: {<kw> {:title :spec :depends-on :status :evidence :journal :born :blocked-on?}}
                             :status ∈ #{:todo :doing :done :blocked :cancelled}
                             :spec REQUIRED. :evidence REQUIRED on :done. :journal engine-appended on every :status change.
        :session/trailer     pinned iter envelopes from prior turns; auto-pinned by engine each turn

    ENGINE FUNCTIONS (bare symbols; never namespace-qualify)

      Memory:
        (rule-set!     :K {:body :scope})              upsert; :scope :project mirrors cross-session
        (rule-remove!  :K)
        (decision!     :K {:body :tags})               append-only; no update, no remove
        (fact-set!     :K {:body})                     upsert
        (fact-remove!  :K)
        (spec-set!     :K {:title :acceptance :facts :status})
        (spec-remove!  :K)
        (task-set!     :K {:title :spec :depends-on :status :evidence :blocked-on})
        (task-remove!  :K)

      Symbols (native SCI; engine persists across turns):
        (defn foo [x] …)                              create / overwrite
        (def  foo nil)                                drop; engine forgets on next restore

      Session introspection:
        (iter        \"t<N>/i<N>\")                      one iter, full forms vec
        (form        \"t<N>/i<N>/f<N>\")                 single form envelope
        (turn        \"t<N>\")                           turn TOC: user-msg + answer + iter-scopes
        (iter-heads  \"t<N>\")                           iter list with first-form head per iter
        (turn-list)                                    all turns with head + status

      SCI symbol introspection:
        (symbol-doc      'sym)
        (symbol-source   'sym)
        (symbol-meta     'sym)
        (symbol-apropos  \"pattern\")

      Control:
        (done                 {:answer :trailer-drop :trailer-summarize})
        (set-session-title!   \"title\")
        (satisfy-hint!        :hint/id)

    ENGINE BEHAVIORS
      • Every *-set! call: new key → engine stamps :born; existing → merges partials.
      • (task-set! :K {:status <new>}) → engine appends {:status :scope} to :journal.
      • (spec-set! :K {:status :done | :cancelled}) → engine stamps :done-born.
      • (decision! :K …) called twice with same key → engine warns (append-only).
      • *-remove! on non-existent key → silent no-op.
      • Soft warnings (engine never refuses): missing :spec on task-set!, missing :evidence
        on :done task, missing :facts on spec-set!. Warnings appear as `;; ⚠ …` in next render.

    TRAILER
      At each (done …), engine:
        1. Auto-pins every current-turn iter whose :forms (excluding `done`) is non-empty.
        2. Applies :trailer-drop      — removes entries by exact :scope match.
        3. Applies :trailer-summarize — replaces a verbatim pin's :forms with a :summary string.
        4. Sorts by scope; persists.

      Entry shapes:
        Verbatim pin    {:scope \"t<N>/i<N>\" :forms [{:scope :tag :src :result :error}]}
        Summary entry   {:scope \"t<N>/i<N>\" :summary \"…\" :summarized-born \"tM/iM/fK\"}

      :tag is :observation or :mutation. :result and :error dropped when default.
      `(done …)` forms are excluded from :forms.

      Stale-read heuristic: observation pins on a target later mutated are stale.
      Drop or summarize them on the next done. Engine never auto-prunes.

    HINTS
      :session/hints contains pending one-shot instructions from the engine.
      Read them before acting; satisfy by performing the requested action and
      then calling (satisfy-hint! :hint/id) as its own top-level form. Hints
      are transient — they do not survive the turn that satisfies them.

    DONE
      (done {:answer            \"markdown string\"
             :trailer-drop      [\"t<N>/i<N>\" …]
             :trailer-summarize [{:scope \"t<N>/i<N>\" :summary \"…\"} …]})

      (done …) is a claim of completeness, not a sign-off after activity.
      Before calling done:
        1. Re-read the CURRENT-USER-MESSAGE.
        2. Enumerate acceptance criteria — explicit or implied.
        3. For each criterion, point to evidence: a task's :evidence scope, a fact's :body,
           a trailer pin's :result. No evidence → another iter.
        4. Emit done only when every criterion maps to observed data.

    REASONING
      Data first, effects last. Compose pure transformations, inspect shapes,
      then call effect tools. Bind every meaningful value to a named def;
      reference defs downstream; never copy data from previews. Refine across
      iters by rebinding, not by restating logic.

    ANSWER
      :answer is Markdown.

    DEF HEADS
      Use def and defn. Other forms (defrecord, deftype, defprotocol, gen-class,
      extend-type, extend-protocol, definterface, reify) are not available in
      the sandbox.
    "))

(defn build-system-prompt
  "Core system prompt: CORE_SYSTEM_PROMPT plus optional caller addendum."
  [{:keys [system-prompt]}]
  (let [addendum (when (string? system-prompt)
                   (extension/normalize-prompt-text system-prompt))]
    (str CORE_SYSTEM_PROMPT
      (when (and (string? addendum) (not (str/blank? addendum)))
        (str "\n\n" addendum)))))

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
     `SYSTEM-PROMPT`       - CORE_SYSTEM_PROMPT + caller addendum
     `TURN-SYSTEM-CONTEXT` - turn-scoped runtime capability context. Today it
                             contains extension prompt fragments; future
                             extension reloads should replace this one message,
                             never append a second extension context.

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
        turn-system-block (turn-system-context-block environment active-extensions)]
    (vec
      (keep stable-prompt-message
        [core-block turn-system-block]))))

