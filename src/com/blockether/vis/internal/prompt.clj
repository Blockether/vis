(ns com.blockether.vis.internal.prompt
  "Prompt assembly.

   Provider messages are explicit blocks in send order: core system rules,
   project instructions (AGENTS.md / CLAUDE.md when present), extension
   fragments, current user message. Per-iteration user-role context is the
   engine snapshot rendered as a Python dict (`session`) by the loop."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.agents :as agents]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.env-python :as env-python]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.workspace :as workspace]
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
    (str ";; -- "
         (-> (str tag)
             (str/replace "_" "-")
             str/upper-case)
         " --\n"
         body
         (when-not (str/ends-with? body "\n") "\n"))))

(defn- call-extension-callback
  [ext f & args]
  (binding
    [extension/*current-extension*
     ext

     extension/*current-symbol*
     nil]

    (apply f args)))

;; =============================================================================
;; Initial messages
;; =============================================================================

(defn previous-turn-context-block
  "Cross-process RESUME context: every prior ANSWERED turn rendered oldest→newest
   as `user asked → what you ran → you answered`, so a fresh process reconstructs
   the conversation. Every prior answer is rendered in FULL — history is
   never truncated. nil when there are no prior turns.

   Takes a VEC of `{:user-request :answer :interrupted? :results}` (results =
   `[{:scope :src}]`). An `:interrupted?` turn was cut off mid-flight (e.g. a
   process restart) with no answer — it's surfaced so a follow-up `continue`
   knows the pending work."
  [turns]
  (when (seq turns)
    (let
      [render-turn
       (fn [i {:keys [user-request answer interrupted? results]}]
         (let
           [req (some-> user-request
                        str
                        str/trim
                        not-empty)
            ans (some-> answer
                        str
                        str/trim
                        not-empty)]

           (when (or req ans (seq results))
             (str
               "# ── turn "
               (inc (long i))
               " ──\n"
               (when req (str "user asked:\n" req "\n"))
               (when (seq results)
                 (str "you ran:\n"
                      (str/join "\n"
                                (map (fn [r]
                                       (str "  "
                                            (cond (:gist r) (str "(folded) " (:gist r))
                                                  (:dropped? r) (str "(dropped)"
                                                                     (when (:note r)
                                                                       (str " " (:note r))))
                                                  :else (:src r))))
                                     results))
                      "\n"))
               (when ans (str "you answered:\n" ans))
               (when (and interrupted? (not ans))
                 "⚠ this turn was INTERRUPTED before it finished — you produced NO answer. The work above is unfinished; continue it.")))))]
      (prompt-block "conversation-so-far" (str/join "\n\n" (keep-indexed render-turn turns))))))

(defn- attached-images-block
  "Manifest for image attachments riding this user message. Lists each
   attached image (path/mime/size, in attachment order) so the model can
   pair the opaque image blocks with the paths the user mentioned, and
   names sniffed-but-skipped images with the WHY (size/count cap) so the
   model doesn't hunt for an attachment that isn't there."
  [attached skipped]
  (when (or (seq attached) (seq skipped))
    (prompt-block "attached-images"
                  (str "You can SEE these images — they ride this message as image blocks. Look at them\n   directly. Do NOT open them with PIL or other imaging libraries to \"read\" their\n   content — that yields only pixel size/mode, never meaning. Reach for PIL ONLY to\n   TRANSFORM an image (resize/crop/convert), never to inspect one you can already see.\n\n"
                       (str/join "\n"
                            (concat (map-indexed (fn [i {:keys [path media-type size-label]}]
                                                   (str "- image "
                                                        (inc (long i))
                                                        ": "
                                                        path
                                                        " ("
                                                        media-type
                                                        ", "
                                                        size-label
                                                        ") — attached to this message"))
                                                 attached)
                                    (map (fn [{:keys [path reason]}]
                                           (str "- " path " — NOT attached: " reason))
                                         skipped)))))))

(defn assemble-initial-messages
  "Initial provider messages for one turn. Deliberately excludes full prior
   dialog transcript: Vis state flows through persisted iterations,
   defs, and DB-backed tools. The current user message is tagged as
   `CURRENT-USER-MESSAGE`.

   One full previous-turn context block may be prepended so short follow-ups
   can inspect the prior exchange without replaying the whole session.

   `:user-images` (from `attachments/collect-user-images`) turns the user
   message multimodal: svar image blocks ride ahead of the text block and
   an `ATTACHED-IMAGES` manifest inside the text names each one.
   `:skipped-images` entries appear in the manifest only."
  [{:keys [stable-prompt-messages initial-user-content previous-turn-context user-images
           skipped-images]}]
  (let
    [previous-block
     (previous-turn-context-block previous-turn-context)

     user-block
     (when initial-user-content (prompt-block "current-user-message" initial-user-content))

     images-block
     (when user-block (attached-images-block user-images skipped-images))

     text
     (str/join "\n\n" (keep identity [previous-block user-block images-block]))]

    (vec (concat
           (or stable-prompt-messages [])
           (when user-block
             [(if (seq user-images)
                (apply svar/user text (map #(svar/image (:base64 %) (:media-type %)) user-images))
                {:role "user" :content text})])))))

;; =============================================================================
;; System prompt
;; =============================================================================

(def ^:private CORE_SYSTEM_PROMPT
  "Cross-tool contract for an autonomous agent. Native descriptions and JSON
   Schemas own tool-specific routing and inputs."
  (str
    "You are vis. Complete the task autonomously.\n\n" "## 1. Identity + Epistemic stance\n"
    "- Work on the host project by default. For vis tasks, inspect `await vis_docs()` and relevant\n"
    "  page; verify against runtime/source.\n"
    "- Route vis issues upstream to `blockether/vis`; open one only when requested.\n"
    "- Trust order: runtime > source > docs > assumption. Inspect; never fabricate tool output.\n"
    "- Native descriptions and JSON Schemas are authoritative. Obey routing,\n"
    "  hard preconditions, and inputs; never guess contracts.\n\n"
    "## 2. Execution surfaces\n"
    "- Prefer `python_execution` for batches/chains: `await gather(...)` independent calls,\n"
    "  then filter/print; use direct native tools for single operations.\n"
    "- `python_execution` persists Python state. Reading `session` is always live; `print`\n"
    "  only displays it—never probe merely to refresh it.\n"
    "- Before `repl_eval` or lifecycle changes, use live REPL state in\n"
    "  `session[\"resources\"][\"repls\"][language][dir]` (`\".\"` is root); follow `repl_start`.\n"
    "- Reuse managed REPLs across turns; after verification, stop only those you\n"
    "  started. External REPLs are user-owned: detach, never kill.\n\n" "## 3. Inspect\n"
    "- Repository tasks: inspect first; answer pure knowledge directly without tools.\n"
    "- Bugs: reproduce before editing if feasible. Prefer an up project `repl_eval` for\n"
    "  isolated behavior; otherwise run the smallest failing test/command. Capture the\n"
    "  failure; rerun the same check after the fix. If impossible, state why.\n"
    "- Use `find_files` for vague paths; `rg` for exact text; `apropos`/`doc` for exact\n"
    "  contracts; `struct_index` for code structure. Read one relevant region; batch\n"
    "  independent reads. Inspect dependencies before adding them.\n"
    "- Performance: inspect config/dependencies; benchmark/profile identical workloads.\n\n"
    "## 4. Edit + verify\n"
    "- Edit supported code via `struct_index`/`struct_patch`; text/unsupported via anchored\n"
    "  `patch`. Batch edits atomically; after writes, refresh anchors via `cat`/`struct_index`.\n"
    "- Make surgical in-scope changes; preserve unrelated work. Create no unrequested\n"
    "  scratch, debug, notes, or report files.\n"
    "- Verify the smallest relevant check and obvious boundaries; read output—a diff is not\n"
    "  proof. If verification is impossible, say why.\n\n" "## 5. Act autonomously\n"
    "- Make non-destructive in-scope changes without asking permission or offering optional\n"
    "  follow-ups. Never expose or log secrets.\n"
    "- Do not commit, push, publish, message people, or mutate external systems unless requested.\n"
    "- Ask one question only if ambiguity changes the result. Read errors; change approach.\n"
    "  Never decide from pending or unseen results.\n\n" "## 6. Manage context\n"
    "- Runtime state is read-only `session`; never use `ctx` or `context`.\n"
    "- New turn: first understand intent; retry any prior blocked fold before new work.\n"
    "- Before every `session_fold`, read `session[\"turn\"]`. Each target `tN` must satisfy\n"
    "  `N < session[\"turn\"]`; never target current/future turns, even after verification.\n"
    "- Fold only completed prior-turn wire steps; preserve a durable gist when useful.\n"
    "  Recover one native result via `ntr[tool_id]`; its breadcrumb lists accessors and\n"
    "  survives restart. Otherwise use `await session_state()` →\n"
    "  `transcript/turns/iterations/blocks` (`code`/`result`).\n"
    "- If the user cites a session UID, resolve it via `await sessions()`.\n"
    "  Broader/newer folds replace covered breadcrumbs.\n\n"
    "## 7. Style and finish\n"
    "- Lead with the answer or next action. Use short plain sentences; default ≤120 words,\n"
    "  ≤3 bullets. No preamble, recap, pleasantries, or tangents.\n"
    "- Multi-step: ≤5 numbered bounded actions. State completed results. Across turns:\n"
    "  `Step N/M complete. Next: ...`; estimate only when useful.\n"
    "- One conclusion: evidence, rationale, consequence. Errors: location → cause → fix.\n"
    "  Reviews: findings first.\n"
    "- Confirm destructive actions. After 3 failed attempts at the same operation, challenge\n"
    "  one assumption with one question. If work remains, end with one action under 2 minutes;\n"
    "  never offer a menu.\n"))

(defn- config-system-prompt
  "Optional `:system-prompt` from Vis config, read from the deep-merged config
   (project `vis.edn` layered over `.vis/config.edn` and the global
   `~/.vis/config.edn`).

   Two shapes are accepted:

   - a **string** — an addendum appended after `CORE_SYSTEM_PROMPT`.
   - a **map** `{:text ... :replace? true}` — when `:replace?` is truthy the
     text fully *replaces* `CORE_SYSTEM_PROMPT` (a full rewrite); otherwise it
     is treated as an addendum, same as the string form.

   Returns `{:text <normalized-non-blank-string> :replace? <bool>}` or nil.
   Tolerant: any read/parse failure yields nil so prompt assembly never breaks
   on a malformed config."
  []
  (try (let
         [raw
          (config/load-config-raw)

          sp
          (when (map? raw) (:system-prompt raw))

          [s replace?]
          (cond (string? sp) [sp false]
                (map? sp) [(:text sp) (boolean (:replace? sp))]
                :else [nil false])]

         (when (string? s)
           (let [t (extension/normalize-prompt-text s)]
             (when-not (str/blank? t) {:text t :replace? replace?}))))
       (catch Throwable _ nil)))

(defn- read-prompt-file
  "Slurp + normalize a markdown prompt file. nil when absent, blank, or
   unreadable — prompt assembly never breaks on a bad file."
  [^java.io.File f]
  (try (when (.isFile f)
         (let [s (extension/normalize-prompt-text (slurp f))]
           (when-not (str/blank? s) s)))
       (catch Throwable t
         (tel/log! {:level :warn
                    :id ::system-prompt-file-read-failed
                    :data {:path (.getAbsolutePath f) :error (ex-message t)}})
         nil)))

(defn- system-prompt-file-overrides
  "pi-style SYSTEM.md / APPEND_SYSTEM.md markdown overrides.

   Replace base (first hit wins): `<workspace>/.vis/SYSTEM.md`, then
   `~/.vis/SYSTEM.md`. Appends (both apply, global first so the project
   file lands nearer the conversation): `~/.vis/APPEND_SYSTEM.md`, then
   `<workspace>/.vis/APPEND_SYSTEM.md`.

   Returns `{:replace <text|nil> :appends [text …]}`."
  []
  (let
    [global-dir
     (io/file (System/getProperty "user.home") ".vis")

     proj-dir
     (try (io/file (workspace/cwd) ".vis") (catch Throwable _ nil))]

    {:replace (or (when proj-dir (read-prompt-file (io/file proj-dir "SYSTEM.md")))
                  (read-prompt-file (io/file global-dir "SYSTEM.md")))
     :appends (vec (keep identity
                         [(read-prompt-file (io/file global-dir "APPEND_SYSTEM.md"))
                          (when proj-dir
                            (read-prompt-file (io/file proj-dir "APPEND_SYSTEM.md")))]))}))

(defn build-system-prompt
  "Core system prompt + optional caller addendum + config prompt +
   SYSTEM.md / APPEND_SYSTEM.md file overrides.

   Assembled in send order (later blocks positionally reinforce earlier):
   base, then the caller's `:system-prompt` addendum, then the
   `:system-prompt` pulled from Vis config (`vis.edn` / `.vis/config.edn` /
   `~/.vis/config.edn`, deep-merged), then `~/.vis/APPEND_SYSTEM.md`, then
   `<workspace>/.vis/APPEND_SYSTEM.md`. The config + file hooks let a project
   append house rules without any caller having to pass them.

   Full rewrite precedence for the base: `<workspace>/.vis/SYSTEM.md` >
   `~/.vis/SYSTEM.md` > config `:system-prompt` map with `:replace? true` >
   `CORE_SYSTEM_PROMPT`. When a file/config replaces the base, addenda and
   append files are still appended after it."
  [{:keys [system-prompt]}]
  (let
    [addendum
     (when (string? system-prompt) (extension/normalize-prompt-text system-prompt))

     cfg
     (config-system-prompt)

     files
     (system-prompt-file-overrides)

     file-replace
     (:replace files)

     cfg-replace?
     (and (nil? file-replace) (boolean (:replace? cfg)))

     cfg-prompt
     (when (and cfg (not (:replace? cfg))) (:text cfg))

     base
     (or file-replace (when cfg-replace? (:text cfg)) CORE_SYSTEM_PROMPT)

     extras
     (into []
           (comp (filter string?) (remove str/blank?))
           (into [addendum cfg-prompt] (:appends files)))]

    (str/join "\n\n" (into [base] extras))))

(defn- project-instructions-block
  "Inline project rules (stacked AGENTS.md / CLAUDE.md context files) as a
   stable system block. The model sees the actual rules, not a boolean hint.

   `internal.agents` already does the reads + stacking + caching; this fn
   just labels the content for the prompt. Files render outermost first
   (user-global → ancestor directories → workspace root) so nearer rules
   positionally override outer ones. Returns nil when no file is present
   or every file is empty."
  [environment]
  (try
    (let
      [{:keys [found? source path content files]}
       (binding [workspace/*filesystem-roots* (workspace/env-filesystem-roots environment)]
         (agents/instructions))

       ;; Back-compat: a single-file legacy shape (no :files) still renders.
       files
       (or (seq files)
           (when (and found? (string? content) (not (str/blank? content)))
             [{:scope :project
               :source (case source
                         :repo
                         :agents-md

                         :repo:claude-md-fallback
                         :claude-md

                         source)
               :path path
               :content content}]))

       files
       (filter (fn [f]
                 (and (string? (:content f)) (not (str/blank? (:content f)))))
               files)]

      (when (and found? (seq files))
        (let
          [multi?
           (> (count files) 1)

           header
           (str "Project rules from "
                (if multi?
                  (str (count files)
                       " stacked guidance files, broadest first; "
                       "later/nearer files override earlier ones.")
                  (str (agents/origin-label (first files)) " (" (:path (first files)) ")."))
                " Honor them with CORE rules; on conflict, CORE wins.")

           body
           (str/join "\n\n"
                     (map (fn [f]
                            (if multi?
                              (str "### " (agents/origin-label f) " — " (:path f) "\n" (:content f))
                              (:content f)))
                          files))]

          (prompt-block "project-instructions" (str header "\n\n" body)))))
    (catch Throwable t
      (tel/log! {:level :warn :id ::project-instructions-error :data {:error (ex-message t)}}
                "project-instructions-block read failed")
      nil)))

(defn active-extensions
  "Returns the seq of registered extensions whose `:ext/activation-fn` returns
   truthy for `environment`, in registration order. Single source of truth for
   activation; call ONCE at the top of a turn."
  [environment]
  (when-let
    [exts (some-> (:extensions environment)
                  deref
                  seq)]
    (vec (filter (fn [ext]
                   (try (boolean (call-extension-callback ext (:ext/activation-fn ext) environment))
                        (catch Throwable t
                          (tel/log! {:level :error
                                     :id ::ext-activation-error
                                     :data {:ext (:ext/name ext) :error (ex-message t)}}
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
       (mapv
         (fn [ext]
           (let
             [info
              (extension/extension-info ext)

              registry-id
              (:registry-id info)]

             (cond->
               {:name (:name info)
                :alias (:alias info)
                :description (:description info)
                :kind (:kind info)
                :registry-id registry-id
                :symbols (mapv :ext.symbol/symbol
                               (remove :ext.symbol/hidden? (extension/ext-symbols ext)))}
               (nil? (:alias info))
               (dissoc :alias)

               (nil? (:description info))
               (dissoc :description)

               (nil? (:kind info))
               (dissoc :kind)

               (nil? registry-id)
               (dissoc :registry-id)))))))

(defn- extension-prompt-id
  [ext]
  (str (or (extension/ext-alias-symbol ext) (:ext/name ext) "unknown")))

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
        (str ";; -- EXTENSION "
             (extension-prompt-id ext)
             " --\n"
             body
             (when-not (str/ends-with? body "\n") "\n"))))))

(defn- extensions-prompt-block
  "Collect prompt text from every active extension that declares
   `:ext/prompt-fn`. Each prompt is `(fn [env] -> string)` (normalized at
   registration). Non-blank results are normalized, wrapped as labeled
   extension fragments, then joined into one extension context block."
  [environment active-extensions]
  (let
    [;; Built-ins first so the core kernel prompt (foundation) leads the
     ;; block, header-less, before any third-party `;; -- EXTENSION --`.
     active-extensions
     (sort-by (complement extension/ext-builtin?) (or active-extensions []))

     fragments
     (keep (fn [ext]
             (when-let [f (:ext/prompt-fn ext)]
               (try (let [result (call-extension-callback ext f environment)]
                      (when (and (string? result) (not (str/blank? result)))
                        (extension-prompt-fragment ext result)))
                    (catch Throwable t
                      (tel/log! {:level :warn
                                 :id ::extension-prompt-error
                                 :data {:ext (:ext/name ext) :error (ex-message t)}}
                                "Extension :ext/prompt-fn fn threw")
                      nil))))
           active-extensions)]

    (when (seq fragments) (prompt-block "extensions" (str/join "\n\n" fragments)))))

(defn- sandbox-shims-prompt-block
  "Advertise Python's execution boundary, auto-imports, and live shim names.
   Full shim contracts stay in `doc(name)` so this provider prompt remains small."
  []
  (let
    [shims
     (try (extension/sandbox-shims) (catch Throwable _ nil))

     shim-names
     (->> shims
          (keep :shim/name)
          distinct
          sort)

     auto-imports
     (str/join "`, `" env-python/AUTO_IMPORTED_PYTHON_NAMES)]

    (prompt-block
      "sandbox-shims"
      (str "Auto-imported by `python_execution` (no `import`): `"
           auto-imports
           "`."
           (when (seq shim-names)
             (str "\nPreinstalled shims (import normally; no pip): `"
                  (str/join "`, `" shim-names)
                  "`."))
           "\nUse `apropos(\"\")` to discover and `doc(name)` for exact support and limits."))))

(defn- turn-system-context-block
  "Turn-scoped system context that can be rebuilt/replaced as runtime
   capabilities change.

   Keep this as ONE provider system message. Extension prompts belong here,
   not in every per-iteration trailer. When a future
   reload path recomputes active extensions mid-turn, it should replace this
   message in the rebuilt stateless provider message vector rather than append
   a second extension/context message."
  [environment active-extensions]
  (let
    [blocks (->> [(extensions-prompt-block environment active-extensions)
                  (sandbox-shims-prompt-block)]
                 (filter #(and (string? %) (not (str/blank? %))))
                 seq)]
    (when blocks (prompt-block "turn-system-context" (str/join "\n\n" blocks)))))

(defn- stable-prompt-message
  [content]
  (when (and (string? content) (not (str/blank? content))) {:role "system" :content content}))

(defn stable-prompt-text
  "Join stable prompt message contents for token budgeting and debug bindings only.
   Provider sends the original message vector; this is not a send path."
  [messages]
  (extension/normalize-prompt-text (str/join "\n\n" (keep :content messages))))

(def cli-autonomous-rules
  "Override injected ONLY for the non-interactive `:cli` channel (headless
   `bin/vis '<task>'` one-shot runs). No human is in the loop, so the model
   must never wait for input — it makes reasonable assumptions and drives the
   work to a finished prose answer."
  (str "NON-INTERACTIVE ONE-SHOT RUN — no human is watching and nothing can "
       "be approved mid-run.\n"
       "- NEVER stop to wait for approval or input — there is no one to answer.\n"
       "- For ordinary ambiguity, state one reasonable assumption and complete the work.\n"
       "- MUST NOT perform destructive or irreversible work that requires confirmation. "
       "Use a safe reversible path; if none exists, finish with the exact blocked action "
       "and required confirmation.\n"))

(defn assemble-stable-prompt-messages
  "Assemble provider-prefix messages.

   Send order is explicit and tested:
     `SYSTEM-PROMPT`         - CORE_SYSTEM_PROMPT + caller addendum
     `PROJECT-INSTRUCTIONS`  - AGENTS.md / CLAUDE.md contents (when present)
     `TURN-SYSTEM-CONTEXT`   - turn-scoped runtime capability context. Today
                               it contains extension prompt fragments; future
                               message, never append a second extension
                               context.

   Extension fragments are separate from the core system prompt and are not
   repeated in per-iteration trailers.

   Required opts:
     `:active-extensions` - vec from `(active-extensions env)`. Drives
        environment, extension prompt, and hint collection.

   Optional opts:
     `:system-prompt`            - caller addendum appended to CORE.
     `:session-context`          - rendered fenced-Python `session = {…}` block
        (standing session state: workspace / env / routing / tools). Embedded
        ONCE here as a cached system message; the loop re-emits only the
        `session[...] = …` structural delta in the conversation when it changes
        mid-turn."
  [environment {:keys [system-prompt active-extensions session-context] :as opts}]
  (when-not (contains? opts :active-extensions)
    (throw (ex-info "assemble-stable-prompt-messages requires :active-extensions"
                    {:type :vis/missing-active-extensions})))
  (let
    [core-block
     (prompt-block "system-prompt" (build-system-prompt {:system-prompt system-prompt}))

     ;; Non-interactive `:cli` runs drop the candidate approval STOP — no
     ;; human can approve a one-shot run. Stable per session (channel never
     ;; changes), so it doesn't churn the prefix cache.
     cli-block
     (when (= :cli (:channel environment)) (prompt-block "cli-autonomous" cli-autonomous-rules))

     project-block
     (project-instructions-block environment)

     turn-system-block
     (turn-system-context-block environment active-extensions)

     ;; Standing session context (workspace/env/routing/tools), rendered
     ;; into the cached prefix so it isn't re-billed every iteration. The
     ;; fenced `session = {…}` block is self-describing, so it rides as its own
     ;; system message (no `;; -- TAG --` wrapper).
     session-context-block
     (not-empty (some-> session-context
                        str/trim))]

    (vec (keep stable-prompt-message
               [core-block cli-block project-block turn-system-block session-context-block]))))
