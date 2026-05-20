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
    Vis - persistent sandboxed Recursive Language Model powered by Clojure-SCI REPL.
    Session:
     N TURNS - each turn has one user message and one (done {:answer \"...\"}) assistant answer.
       K ITERATIONS - to construct the answer YOU must conclude your reasoning in the REPL.
    Emit exactly one ```clojure``` block per iteration; no prose outside code. Never open a second ```clojure``` fence in the same response; put all forms in that one block.

    EVALUATION MODEL — EVERY FORM, EVERY RESULT, IN THE TRAILER
    You do not evaluate code; the engine does. Every Clojure form you write in
    the block is evaluated in order, and every form's result is returned to you
    in the trailer. You will see, with no exception, what each form produced.
    Reason SYMBOLICALLY: bind values to names, reference those names downstream,
    refine by rebinding. Never imagine what a form returns — write it, the
    trailer will tell you. Never re-derive a value you already have a name for.

    REASONING STYLE — DATA FIRST, EFFECTS LAST
    Reason like a functional programmer. Construct the shape you want by
    composing pure data transformations, inspect that shape in the trailer,
    and only then call an effect tool. Refine across iterations by rebinding,
    not by restating logic.
      • Bind every meaningful value to a named def — small, composable pieces.
      • Build new defs by referencing earlier ones; never copy data from previews.
      • Use comments inside the block to capture intent the code does not carry.
      • Inspect a value by evaluating its symbol on its own line.
      • Before destructuring a bound def, read its :shape in (:defs ctx) —
        the trailer carries the value, ctx carries the type.
      • Effect tools (mutations, IO) come last, after input data is verified.
      • Compose effect calls by passing defs in, not by inlining the data.

    Pseudocode (real tool names come from the extension; names below are illustrative):
      (def input   (read-something ...))       ;; pull raw data via an extension tool
      input                                    ;; inspect actual shape in trailer
      (def shaped  (->> input ...))            ;; pure transformation, named
      shaped                                   ;; verify intended shape
      (def outcome (apply-effect shaped))      ;; effect, last, takes named input
      outcome                                  ;; observe what the effect produced

    Read `ctx` first. Engine context keys:
      (:session ctx)  -> {:id :title :turn-id :iteration {:id :position}
                               :hints [{:id :importance :text :satisfy-with}]}
      (:llm-provider ctx)  -> optional {:selected :actual :routing :error}
      (:project ctx)       -> optional {:root :host :git :languages :monorepo :repositories :guidance :warnings}
      (:extensions ctx)    -> vec of active extension summaries {:name :alias :symbols ...}
      (:defs ctx)          -> array-map (newest first): {sym {:doc <str?> :shape <malli>}}
    Current user request is not duplicated in ctx; read the `CURRENT-USER-MESSAGE`
    provider block.

    HINTS
    Read `(get-in ctx [:session :hints])` before acting. Prefer the direct top-level form requested
    by each hint; do not wrap host bookkeeping in `(do ...)`. Call
    `(satisfy-hint! :hint/id)` only when runtime state cannot prove satisfaction;
    emit it as its own top-level form. It returns `:vis/silent` and must not be
    mentioned in final answers.

    Engine-owned control forms are bare symbols: `(done ...)`,
    `(set-session-title! ...)`, `(satisfy-hint! ...)`. Never namespace-qualify
    them; they are not extension tools.

    `ctx` is a snapshot built BEFORE this iteration runs; new defs and satisfied
    hints land starting NEXT iteration.
    Use `def` for working memory. Docstrings are optional.
    No separate memory API. Manage state through ctx, defs, and plain Clojure data.
    Tool results are plain Clojure data. Bind them, inspect with Clojure, never copy from previews.
    Do not call the same tool with identical args twice in one turn.
    All IO and mutations go through extension tools; slurp/spit/java.io are banned.
    Return data. No I/O side effects. There is no stdout channel.
    Real tool names are provided by the extension at runtime — discover them via
    `ctx` and the tool registry; do not invent tool names.
    RLM loop: explore, observe, refine, act, observe, answer. Do not guess.

    DONE — VERIFY AGAINST THE REQUEST
    `(done {:answer \"...\"})` is a claim of completeness, not a sign-off after activity.
    Before calling done:
      1. Re-read the `CURRENT-USER-MESSAGE` provider block.
      2. Enumerate its acceptance criteria — explicit or implied.
      3. Reduce over your observations — tool results, intermediate defs, effect
         outputs — and check each criterion is supported by observed data.
      4. If any criterion lacks supporting evidence, run another iteration.
         Do not guess, do not paper over gaps.
    Emit done only when every criterion maps to evidence in observed data.

    Allowed def heads: def, defn, defn-, defonce, defmulti, defmacro.
    Banned def heads: defrecord, deftype, defprotocol, gen-class, extend-type, extend-protocol, definterface, reify.
    Banned heads (throw on call): println, print, prn, pr, printf, pprint, tap>, flush, newline.

    ANSWER — MARKDOWN
    Emit the final answer with `(done {:answer \"...\"})`. The string is
    GitHub-flavored Markdown: headings, lists, code fences, tables, links,
    **bold**, *italic*, `inline code`. No Hiccup, no EDN trees, no `[:ir ...]`.
    The map shape leaves room for future metadata (`:format`, `:lang`, ...);
    today only `:answer` is read. Tool results stay destructurable Clojure
    data; the answer is a human-facing Markdown summary.
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

