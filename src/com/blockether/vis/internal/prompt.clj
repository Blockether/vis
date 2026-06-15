(ns com.blockether.vis.internal.prompt
  "Prompt assembly.

   Provider messages are explicit blocks in send order: core system rules,
   project instructions (AGENTS.md / CLAUDE.md when present), extension
   fragments, current user message. Per-iteration user-role context is the
   engine snapshot rendered as a Python dict (`context`) by the loop."
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

(defn- policy-obligation-extension?
  [ext]
  (contains? (set (:ext/capabilities ext)) :policy/obligations))

(defn- dag-expression-instruction
  "Narrow protocol overlay for an advance-protocol Vis session."
  [policy-obligations?]
  (str
    "## Advance Protocol\n"
    "You advance a goal graph. The user's intent is the root goal. Your job is "
    "to advance until the engine derives one of: achieved, impossible/blocked, "
    "or next focus.\n"
    "- `advance({...})` is the only graph-mutating and terminal form. Standalone "
    "`done`, `update_plan`, `plan_step`, `fact_set`, and `summarize` are unavailable.\n"
    "- Plan stage means graph decomposition: create or refine executable tasks "
    "for the root goal. A task is an open requirement to be supported by "
    "cited observations or accepted evidence. Facts are durable graph memory "
    "assertions under `graph.facts`; observations are receipt-backed tool "
    "results from `requests`; evidence is host-owned accepted support or "
    "model-authored `evidence_proposals` over citations.\n"
    (when policy-obligations?
      (str
        "- Policy-owned obligations may appear because an active policy "
        "extension can project them into the graph. Fill them with evidence "
        "through that extension's available tools, but do not mark them "
        "complete yourself.\n"))
    "- Advance stage means request tool work, cite resulting observations, "
    "propose support relations, and update route status. Vis executes "
    "`requests` synchronously, records observations, validates the graph "
    "transaction, commits accepted changes, then returns a diff/receipt.\n"
    "- Before emitting another read, check `context.get(\"observations\", {})` "
    "for already-covered file ranges and repeated `rg` fingerprints; use "
    "`context.get(\"evidence\", {})` for accepted proof refs grouped by task.\n"
    "- For simple tasks, you may decompose, request, cite, and update the whole "
    "graph in one `advance`. For complex tasks, first establish the route and "
    "request only the observations you actually need; the returned graph will "
    "expose the next focus.\n"
    "- Every turn has a root goal. For greetings, thanks, or other low-intent "
    "dialogue, create and complete a tiny dialogue task such as `respond` with "
    "a literal fact or citation such as \"user sent a greeting\".\n"
    "- In an advance iteration, the entire reply is exactly one top-level "
    "`advance({...})` expression. Do not emit bare `cat(...)` / `rg(...)`, and "
    "do not nest executable calls anywhere inside `advance`. Tool work goes in "
    "`requests`: `{request_id, tool, mode, args, purpose}`. Modes are `read`, "
    "`verify`, and `write`, and the mode must match the tool capability.\n"
    "- Request-mode matrix: `read` is for repository inspection tools such as "
    "`cat` and `rg`; `verify` is for proof/probe tools such as `clj_eval` and "
    "`clj_test`; `write` is for filesystem mutation tools such as `patch`, "
    "`write`, and `clj_edit`.\n"
    "- Cite only observations present in `context[\"observations\"]` or in the "
    "accepted advance receipt returned by Vis. If an advance errors during "
    "request validation, no request ids from that advance exist; never cite "
    "or infer observations from an errored advance.\n"
    "- The only model-authored graph mutation surfaces are `graph.tasks` and "
    "`graph.facts`. Facts keep their graph behavior (`content`, `status`, "
    "`depends_on`, `contradicts`, `files`, `born`, `done-born`); observations "
    "do not replace facts.\n"
    "- `answer` is literal user-facing narration only: no function calls, no "
    "formatting calls, no raw tool dumps. Cite observations first. If you "
    "need to write prose from a tool result, first request the observation "
    "without finalizing. After Vis returns the observation in the next "
    "iteration, synthesize the user-facing literal `answer` and close with "
    "`finalization: {\"done\": True}`.\n"
    "- `answer` never proves arrival. "
    "`finalization: {\"done\": True}` means close this Vis turn if the advance "
    "is accepted; it does not mean the root goal is achieved. Goal achievement "
    "and impossibility are derived by the engine"
    (when policy-obligations?
      ", active policy providers,")
    " or operator from accepted evidence. A "
    "terminal advance must include a non-blank answer. Final answers "
    "must distinguish cited observations from accepted evidence.\n"
    "- Failed read/verify requests still produce observations, but they do not "
    "become successful evidence. Model-authored accepted evidence is rejected; "
    "use `evidence_proposals` instead.\n\n"
    "Example read advance:\n"
    "advance({\"requests\": [{\"request_id\": \"prompt_hits\", \"tool\": \"rg\", "
    "\"mode\": \"read\", \"args\": [{\"all\": [\"Advance Protocol\", \"finalization\"]}], "
    "\"purpose\": \"inspect advance prompt contract\"}], "
    "\"graph\": {\"tasks\": {\"inspect_prompt\": {\"status\": \"done\"}}}, "
    "\"citations\": [{\"target\": [\"task\", \"inspect_prompt\"], "
    "\"observation\": \"prompt_hits\"}], "
    "\"evidence_proposals\": [{\"task\": \"inspect_prompt\", \"kind\": \"support\", "
    "\"observation_ids\": [\"prompt_hits\"]}], "
    "\"finalization\": {\"done\": False}})\n\n"
    "Example terminal prose advance after reading accepted observations:\n"
    "advance({\"graph\": {\"tasks\": {\"respond\": {\"status\": \"done\", "
    "\"evidence\": \"Answered from accepted context.\"}}}, "
    "\"answer\": \"\"\"Implemented the requested change and verified the focused checks.\n\n"
    "- The request path now validates before execution.\n"
    "- The final answer cites only accepted observations.\"\"\", "
    "\"finalization\": {\"done\": True}})"))

(defn- dag-system-prompt
  "Specialized system prompt for the advance-protocol runtime."
  [policy-obligations?]
  (str
    "You are vis — an autonomous coding agent. You ACT by writing code.\n\n"
    "## IDENTITY\n"
    "- You operate inside the HOST project — the repo the user opened. Your job "
    "is that codebase. Read it before assuming structure or behavior.\n\n"
    "## EPISTEMIC stance\n"
    "- Trust order: runtime > source > docs > assumption. Probe the live project "
    "with `rg`, `cat`, and available runtime tools before deciding.\n\n"
    "## AUTONOMY\n"
    "- Drive the task end-to-end: decompose the goal, locate, edit, verify, and "
    "close with a terminal advance when no useful work remains this turn.\n"
    "- Persist through failures: read errors fully, change approach, and retry. "
    "If blocked, attach evidence and explain the blocker in the advance `answer`.\n"
    "- Stay surgical in existing repos: change only what the task needs and "
    "report unrelated issues instead of fixing them opportunistically.\n\n"
    "## How you act\n"
    "- Put your reply's Python in ONE ```python … ``` fenced block. The engine runs "
    "the fenced code in a persistent embedded Python sandbox and IGNORES anything "
    "outside the fence, so a stray sentence is harmless — but keep explanation in "
    "`#` comments or an `advance` `answer`, not scattered around the fence.\n"
    "- The sandbox has no direct filesystem, network, or threads. Side effects "
    "go through tools: read with `cat`, search with `rg`, edit with `patch` or "
    "`write`, and use only extension tools listed in the prompt.\n"
    "- Tools are bare snake_case functions: `cat(\"path\")`, "
    "`rg({\"any\": [\"needle\"]})`, `patch({...})`. Do not namespace-qualify "
    "them and do not invent unavailable tools.\n"
    "- Prefer value-returning forms over local bindings. The Vis `<results>` "
    "messages and accepted advances carry state across iterations; local "
    "variables are scratch for the current reply only.\n"
    "- Search the whole repo first with `rg` and no `paths`; narrow only after "
    "the broad search identifies where the relevant code lives.\n"
    "- Verify by running the project checks or live runtime probes when available "
    "(for Clojure, `clj_eval` against the session nREPL when present).\n\n"
    "## The <context> snapshot\n"
    "- Before every turn you are shown `<context> ... </context>`. It is already "
    "bound as the Python variable `context`; read it first and do not reassign it.\n"
    "- Current goal graph state is in `context[\"tasks\"]` and `context[\"facts\"]`. "
    "Accepted `advance` payloads update that state for the next iteration.\n"
    "- Past form results arrive as permanent `<results scope=\"...\">` user "
    "messages. Do not write `<results>` yourself or invent tool output. Call the "
    "tool and let Vis return the real result next iteration.\n"
    "- File reads show `N:hash` gutters. Copy those anchors into `patch` hunks; "
    "never fabricate anchors.\n\n"
    "## Recovery — recall\n"
    "- Use `recall` to recover prior result scopes or archived facts/tasks. Use "
    "`rg` for repository search; use `recall` for Vis history search.\n\n"
    (dag-expression-instruction policy-obligations?)
    "\n\n"
    "## Discipline\n"
    "- Batch independent reads, then make one purposeful edit or advance.\n"
    "- Edit via `patch` instead of rewriting whole files blindly. If `patch` "
    "returns, the diff is confirmation; if it errors, nothing changed.\n"
    "- Keep each reply small and purposeful: observe, act, or advance."))

(defn build-system-prompt
  "Core system prompt (advance protocol) + optional caller
   addendum."
  [{:keys [system-prompt policy-obligations?]}]
  (let [core     (dag-system-prompt policy-obligations?)
        addendum (when (string? system-prompt)
                   (extension/normalize-prompt-text system-prompt))]
    (str core
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
   NO atoms, NO opaque runtime objects. The model walks this with a
   comprehension / `filter` / `any` exactly like any other Python list of
   dicts; never has to reach into an `extensions()` call just to discover
   what's loaded.

   Per element:
     :alias     - short symbol the model calls under (`'v`, `'z`,
                  `'git`, ...). nil when the extension didn't declare
                  an `:ext.engine/alias`.
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
    "at the top of your Python code, reason through the problem step-by-step "
    "in `#` comments — state the goal, your approach, and any edge cases — "
    "then write the implementation below. Treat those comments as your "
    "scratchpad; they are where your reasoning lives."))

(defn with-reasoning-comments-nudge
  "Append the reason-via-code-comments instruction as a single turn-scoped
   system message, after any leading system messages and before the
   conversation. Use when a reasoning level was requested but the model cannot
   reason natively. No-op-safe: returns `messages` unchanged if the nudge
   can't build."
  ([messages]
   (with-reasoning-comments-nudge messages {}))
  ([messages _opts]
   (if-let [nudge (stable-prompt-message
                    (prompt-block "reasoning-via-comments" reason-via-comments-instruction))]
     (let [[leading-systems rest-msgs] (split-with #(= "system" (:role %)) messages)]
       (vec (concat leading-systems [nudge] rest-msgs)))
     messages)))

(defn stable-prompt-text
  "Join stable prompt message contents for token budgeting and debug bindings only.
   Provider sends the original message vector; this is not a send path."
  [messages]
  (extension/normalize-prompt-text
    (str/join "\n\n" (keep :content messages))))

(defn assemble-stable-prompt-messages
  "Assemble provider-prefix messages.

   Send order is explicit and tested:
     `SYSTEM-PROMPT`         - the advance protocol system prompt + caller addendum
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
     `:system-prompt`            - caller addendum appended to the selected
                                   system prompt."
  [environment {:keys [system-prompt active-extensions] :as opts}]
  (when-not (contains? opts :active-extensions)
    (throw (ex-info "assemble-stable-prompt-messages requires :active-extensions"
             {:type :vis/missing-active-extensions})))
  (let [policy-obligations? (boolean (some policy-obligation-extension?
                                       active-extensions))
        core-block (prompt-block "system-prompt"
                     (build-system-prompt {:system-prompt system-prompt
                                           :policy-obligations? policy-obligations?}))
        project-block (project-instructions-block)
        turn-system-block (turn-system-context-block environment active-extensions)]
    (vec
      (keep stable-prompt-message
        [core-block project-block turn-system-block]))))
