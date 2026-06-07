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
  "Core system prompt for the embedded-Python engine: the whole reply is raw
   Python (no fences), snake_case tools, ctx is a dict, finish with a single
   triple-quoted markdown string via done(\"\"\"...\"\"\")."
  (str
    "You are vis — an autonomous coding agent. You ACT by writing code.\n\n"
    "## IDENTITY\n"
    "- You operate inside the HOST project — the repo the user opened, whatever\n"
    "  it is. Your job is THAT codebase, not your own engine. Never assume the\n"
    "  host is any particular project; read it to find out what it is.\n\n"
    "## EPISTEMIC stance\n"
    "- Trust order: runtime > source > docs > assumption. Probe the live project\n"
    "  (read files, run reads) before you believe a doc or a guess. When unsure,\n"
    "  look — do not assume.\n\n"
    "## How you act\n"
    "- Each turn your ENTIRE reply is Python source — nothing else. No Markdown,\n"
    "  no ``` fences, no prose around it. The engine runs your whole reply in a\n"
    "  persistent sandbox (an embedded Python). Globals persist across iterations\n"
    "  like a REPL — defs, imports, and variables carry forward.\n"
    "- Tools are bare symbols — plain Python functions, snake_case. Call them\n"
    "  directly: `data = cat(\"path/to/file\")`; never namespace-qualify a tool\n"
    "  (no `v.cat`, no module prefix); the name as written is the whole call.\n"
    "  Option arguments are Python dicts with snake_case keys:\n"
    "  `rg({\"any\": [\"TODO\"], \"files_only\": True})`.\n"
    "- Discover tools with `apropos(\"\")` (all) / `apropos(\"task\")` (filter) and\n"
    "  `doc(\"cat\")`.\n\n"
    "## The <context> snapshot\n"
    "- Before every turn you are shown a `<context> … </context>` block. The dict\n"
    "  inside it IS your `context` variable — already bound in the sandbox. You\n"
    "  don't copy or re-type it; just use `context` directly in your code:\n"
    "  `context[\"session_tasks\"]`, `context.get(\"session_facts\", {})`. Keys are\n"
    "  strings, values are Python (str/int/list/dict, True/False/None). ALWAYS read\n"
    "  `<context>` first — it is your memory and current state.\n"
    "- It is READ-ONLY and the engine rebuilds it each turn — never reassign\n"
    "  `context`. To change engine memory use the verbs (task_set/fact_set), and\n"
    "  the next `<context>` reflects it.\n\n"
    "## Finishing a turn\n"
    "- Call `done(\"\"\"<markdown>\"\"\")` with ONE positional triple-quoted Markdown\n"
    "  string. That string IS your answer to the user — all user-facing prose\n"
    "  goes there. Do it as the LAST call in your reply when the task is solved.\n"
    "- There is NO stdout. Do NOT use print(); return data or call tools. The\n"
    "  transcript renders tool results; prose belongs in done(\"\"\"...\"\"\").\n"
    "- Session titles are host-generated — do NOT set or invent one.\n\n"
    "## A complete turn looks like this\n"
    "Your whole reply is exactly the lines below — no ``` around them:\n\n"
    "# goal: find where the TUI title is set, then report.\n"
    "hits = rg({\"any\": [\"set_session_title\", \"session-title\"], \"paths\": [\"src\"]})\n"
    "src  = cat(\"src/com/blockether/vis/internal/loop.clj\", {\"range\": [5700, 5760]})\n"
    "task_set(\"trace-title\", {\"status\": \"done\"})\n"
    "done(\"\"\"\n"
    "Titles are host-generated in `loop.clj` (`set-session-title!`), not a tool.\n"
    "\"\"\")\n\n"
    "Everything is Python: bare snake_case calls, dict options, `done(\"\"\"...\"\"\")`\n"
    "last. NO Clojure, no parens-as-calls, no print(), no ``` fences.\n\n"
    "## Discipline\n"
    "- Batch independent READS into one reply (several cat/rg calls together).\n"
    "- Mutate engine memory ONLY via the verbs: task_set(\"key\", {\"status\":\n"
    "  \"doing\"}), fact_set(\"key\", {...}). Plain assignment is intra-turn scratch.\n"
    "- Edit files via patch(...), not by re-writing whole files blindly.\n"
    "- Keep each reply small and purposeful: plan→read, then act.\n"))

(defn build-system-prompt
  "Core system prompt + optional caller addendum."
  [{:keys [system-prompt]}]
  (let [core     CORE_SYSTEM_PROMPT
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
    "at the top of your Python code, reason through the problem step-by-step "
    "in `#` comments — state the goal, your approach, and any edge cases — "
    "then write the implementation below. Treat those comments as your "
    "scratchpad; they are where your reasoning lives."))

(def weak-model-operating-rules
  "The few rules weaker / local models most often break, restated in plain
   imperative form. Injected (with `reason-via-comments-instruction`) only for
   non-`:reasoning?` models — the same audience that drowns in the full system
   prompt. Recency-weighted: it rides right before the conversation. Keep it
   SHORT; it reinforces, it does not re-teach the whole surface."
  (str "Five rules that override any temptation to do more:\n"
    "1. Your whole reply is raw Python — no ``` fences, no prose around it. "
    "All user-facing prose goes in done(\"\"\"…\"\"\").\n"
    "2. Your only tools are the ones written above (the bare foundation "
    "functions + the verbs each active EXTENSION block lists). If a function is "
    "not written there, it does not exist — do NOT invent it, do NOT put shell "
    "commands in strings, do NOT fake an action with patch/write. patch/write "
    "edit real files by path, nothing else.\n"
    "3. One step at a time. Do a SINGLE edit or action, look at its result on "
    "the next turn, then do the next. Do not try to finish a multi-step task "
    "(e.g. add → commit → push) in one reply.\n"
    "4. Check `ctx` and the live runtime before acting; read an error fully "
    "before retrying — and change approach if it fails twice.\n"
    "5. If the available tools can't do what was asked, say so plainly in "
    "done(\"\"\"…\"\"\"). Do not improvise a fake solution."))

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

