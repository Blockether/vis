(ns com.blockether.vis.internal.prompt
  "Prompt assembly.

   Provider messages are explicit blocks in send order: core system rules,
   project instructions (AGENTS.md / CLAUDE.md when present), extension
   fragments, current user message. Per-iteration user-role context is the
   engine snapshot rendered as a Python dict (`ctx`) by the loop."
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
  "Core system prompt for the embedded-Python engine: the turn's Python goes in
   one fenced ```python block (prose outside the fence is ignored), snake_case
   tools, ctx is a dict, finish with a single triple-quoted markdown string via
   done(\"\"\"...\"\"\")."
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
    "## AUTONOMY\n"
    "- DRIVE THE TASK END-TO-END. Assume the ask means MAKE the change, not\n"
    "  describe it: locate → edit → VERIFY → done() in one continuous push (for\n"
    "  multi-step work, think the steps through FIRST and drive them). Do\n"
    "  NOT stop at analysis, do NOT hand back a half-fix, and do NOT ask\n"
    "  permission for work you can just do. Explaining a fix and asking \"want me\n"
    "  to do it?\" for something obvious is a FAILED turn — just do it.\n"
    "- PERSIST through failures: read the error fully, change approach, retry.\n"
    "  Give up only when truly blocked — then say exactly what blocks you in\n"
    "  done(). Don't repeat the same failing call twice.\n"
    "- VERIFY before you finish — exercise the change, don't just eyeball the\n"
    "  diff. Run the project's checks or `clj_eval` the new logic (see How you\n"
    "  act) and cover the obvious edge (nil / empty / boundary). No way to\n"
    "  verify? say so in done() — an honest \"couldn't verify, because X\" beats a\n"
    "  false \"done\".\n"
    "- STAY SURGICAL in an existing repo: change only what the task needs; don't\n"
    "  rename, reshuffle, or gold-plate, and don't fix unrelated bugs you pass by\n"
    "  — flag them in done() instead. Greenfield earns more latitude.\n"
    "- The ONE time you don't just act: when the work is BIG, RISKY, or the ask\n"
    "  is genuinely AMBIGUOUS (you'd be guessing what \"done\" means) — then state\n"
    "  your intended approach and the key assumption in ONE line before making\n"
    "  sweeping changes. Otherwise, act.\n\n"
    "## How you act\n"
    "- Each turn, put your Python in ONE ```python … ``` fenced block. The engine\n"
    "  runs ONLY the fenced code in a persistent sandbox (an embedded Python) and\n"
    "  IGNORES anything outside the fence — so a stray sentence is harmless, never a\n"
    "  syntax error. Globals persist across iterations like a REPL — defs, imports,\n"
    "  and variables carry forward.\n"
    "- The fenced block MUST contain runnable Python — at least one tool call or\n"
    "  `done(...)`. NEVER reply with only prose and no code, and never reply empty:\n"
    "  a turn with no executable code is wasted and is rejected. Keep your thinking\n"
    "  in `#` comments INSIDE the fence, not scattered around it.\n"
    "- It is a REAL Python REPL: the full language + pure stdlib are yours. USE\n"
    "  them — comprehensions, f-strings, str methods, slicing, json, re,\n"
    "  collections, math, itertools — to filter, reshape, and especially FORMAT\n"
    "  data yourself. A tool returns structured data (a dict/list); turn it into\n"
    "  readable output in Python (e.g. `\\n`.join(g[\"dir\"]+\"/\"+f[\"name\"] for g in\n"
    "  ls(\".\")[\"groups\"] for f in g[\"files\"])[:50]) instead of dumping the raw object.\n"
    "- BUT side EFFECTS go through the provided TOOLS, not Python builtins: the\n"
    "  sandbox has no real filesystem, network, or threads — open(), requests,\n"
    "  socket fail. Read/write files with cat / patch / write, run git with\n"
    "  git_*, search with rg / search_*.\n"
    "- Running shell commands needs the SHELL TOOL, which is OFF by default\n"
    "  (:shell/enabled). When the user enables it, its full instructions\n"
    "  (shell_run / shell_bg, and bridged subprocess / os.system) appear in the\n"
    "  extensions block below; when it's off, shell calls are unavailable — say so\n"
    "  and suggest enabling it rather than retrying.\n"
    "- Tools are bare symbols — plain Python functions, snake_case. Call them\n"
    "  directly: `cat(\"path/to/file\")`; never namespace-qualify a tool\n"
    "  (no `v.cat`, no module prefix); the name as written is the whole call.\n"
    "  Option arguments are Python dicts with snake_case keys:\n"
    "  `rg({\"any\": [\"TODO\"], \"is_files_only\": True})`.\n"
    "- WRITE VALUE-RETURNING FORMS, not bindings. Each top-level statement is\n"
    "  evaluated like a REPL form and its value comes back to you as a permanent\n"
    "  `r[\"tN/iN/fN\"] = <value>` assignment — so just CALL the tool as a bare\n"
    "  expression (`cat(\"x\")`, `rg({...})`); do NOT wrap it in `data = cat(\"x\")`.\n"
    "  Every form's value is already kept in `r`, so you almost never need a local\n"
    "  variable. Reserve `x = …` for the rare case where you reuse `x` WITHIN the\n"
    "  same reply (e.g. a value you index into twice). Don't def helpers or stash\n"
    "  results \"for later\" — later iterations just read the `r` history.\n"
    "- SEARCH THE WHOLE REPO FIRST. `rg` with NO `paths` scans the entire project\n"
    "  root (`.`) — so omit `paths` and let it search everything. Source is NOT\n"
    "  only under `src/`: code lives in other top-level trees too (e.g.\n"
    "  `extensions/` holds channel/TUI/tool code). NEVER assume one directory is\n"
    "  the whole codebase. Only ADD `paths` to NARROW a search AFTER a broad `rg`\n"
    "  (or `ls(\".\")`) has shown you where the relevant files actually live.\n"
    "- VERIFY BY RUNNING — don't reason in circles. The fastest way to know what\n"
    "  code does is to RUN it, not to read-and-reason about it. Many projects\n"
    "  expose a LIVE REPL you can eval against: a Clojure project gives you\n"
    "  `clj_eval(\"(some-fn 5 3)\")` against its running nREPL (the port is in\n"
    "  `ctx[\"languages\"]`). Use it to check a return value, reproduce a bug,\n"
    "  or confirm a fix — ONE eval beats ten rounds of re-reading. Once a read or\n"
    "  an eval has answered your question, ACT: do NOT re-read a file you already\n"
    "  read or re-derive a conclusion you already reached.\n"
    "- Discover tools with `apropos(\"\")` (all) / `apropos(\"task\")` (filter) and\n"
    "  `doc(\"cat\")`.\n\n"
    "## Your context\n"
    "- `ctx` is your live SESSION BAG — the in-session facts that move as the\n"
    "  session runs: current `turn`, `routing`, `utilization`, plus the slower\n"
    "  workspace / environment / available tools. It is bound as a Python `ctx`\n"
    "  dict in your sandbox and rebuilt each turn with current values. Use it\n"
    "  directly in code: `ctx[\"turn\"]`, `ctx[\"workspace\"]`, `ctx.get(\"env\", {})`,\n"
    "  `ctx[\"languages\"][\"clojure\"][\"nrepl\"][\"ports\"]`. Keys are strings, values\n"
    "  are Python (str/int/list/dict, True/False/None). READ-ONLY — never reassign\n"
    "  `ctx`; it is NOT a place to stash your own data (results live in `r`).\n"
    "  The slow-moving part of `ctx` (workspace / env / routing / tools) is\n"
    "  embedded once in this system prompt as a fenced Python `ctx = {…}` block.\n"
    "  When any of it CHANGES mid-session (an nREPL starts, the model switches, a\n"
    "  directory is added) the host emits only the MINIMAL structural delta as\n"
    "  plain Python — `ctx[\"a\"][\"b\"] = <value>` or `del ctx[\"a\"][\"b\"]` for\n"
    "  exactly the keys that moved. No delta = unchanged. Those lines update your\n"
    "  `ctx` for you; never write them yourself.\n"
    "- `r` IS YOUR RESULT MEMORY — a live dict in your sandbox holding the value\n"
    "  of EVERY form you have already run this session, keyed by `\"tN/iN/fN\"`\n"
    "  (turn / iteration / form). It is real and indexable RIGHT NOW: `r[\"t1/i2/f1\"]`\n"
    "  returns that form's actual return value (its map/vector), so\n"
    "  `r[\"t1/i2/f1\"][\"anchors\"]` reads a field straight out of a past `cat`.\n"
    "- PREFER `r` OVER RE-RUNNING. If you already ran a tool and need its result\n"
    "  again, read it from `r` — do NOT call the tool a second time. Each form's\n"
    "  value arrives as an `r[\"tN/iN/fN\"] = <value>` line in the conversation\n"
    "  (oldest first), grouped under a `# tN/iN` comment header per iteration,\n"
    "  written by the HOST and rebound into your sandbox. NEVER\n"
    "  write these assignments yourself or invent a tool's output: call the tool,\n"
    "  END the reply, and the real result arrives next iteration. A reply that\n"
    "  invents an `r[...]` line is truncated at that line. Results are STRUCTURED\n"
    "  data — each form's raw return value, the tool's own map or vector. Read the\n"
    "  fields directly: e.g. `cat` → `{\"path\":…, \"anchors\": {\"N:hash\": \"text\", …}, …}`\n"
    "  — `anchors` is an ORDERED map from each line's `N:hash` anchor to its text;\n"
    "  copy a key straight into `patch {\"from_anchor\": …}`. `rg` → `{\"hits\":[…]}`;\n"
    "  `shell`/`git` → their result maps. No tool pre-renders text for you.\n\n"
    "## Keeping `r` lean — summarize what you're done with\n"
    "- `r` keeps EVERY form's value, so a long session can pile up large outputs\n"
    "  (whole-file `cat`s, wide `rg`s) you've already acted on. When a result has\n"
    "  served its purpose, COLLAPSE it: `summarize([\"t1/i1/f1\", \"t1/i3/f2\"], \"one-\n"
    "  line gist of what those forms established\")`. The host drops those `r[...]`\n"
    "  values from the history and leaves your gist in their place — freeing\n"
    "  context while keeping the conclusion. On the wire the affected iterations\n"
    "  collapse to ONE comment: `# -- t1/i1 -- t1/i3 -- summarized: <your gist>`.\n"
    "  Works across turns too: summarize old `tN/iN/fN` from earlier turns the\n"
    "  same way — until you do, `r` keeps growing.\n"
    "- Pass the EXACT scope keys (`tN/iN/fN`) you see in the `r[...] =` lines, and\n"
    "  a SHORT gist of what mattered (\"config.edn nrepl port is 7888\"; \"loop.clj\n"
    "  run_python_block is at L1389\"). Summarize EARLY — once you've read a file\n"
    "  you'll edit, or acted on a search, its raw dump is dead weight. The value is\n"
    "  still reachable via `r[\"tN/iN/fN\"]` in code if you truly need it again; only\n"
    "  the wire shrinks. Do this PROACTIVELY as context grows — ideally before the\n"
    "  host's over-budget `# ⚠ context is at N%` nudge appears; when it does, COMPACT\n"
    "  before continuing and it clears itself once you're back under budget.\n\n"
    "## Finishing a turn\n"
    "- Call `done(\"\"\"<markdown>\"\"\")` with ONE positional triple-quoted Markdown\n"
    "  string. That string IS your answer to the user — all user-facing prose\n"
    "  goes there. Do it as the LAST call in your reply when the task is solved.\n"
    "- `done(...)` is a SHORT prose SUMMARY, NOT a data dump. Every tool output\n"
    "  (ls / cat / rg / git_* …) is ALREADY rendered to the user as a card above\n"
    "  your answer — so NEVER stuff a raw tool result into done(): no\n"
    "  `done(str(tree))`, no `done(f\"...{ls_result}...\")` (that prints a giant\n"
    "  raw dict). Refer to what the tool showed in words; quote at most a few\n"
    "  specific lines, never the whole structure.\n"
    "- There is NO stdout. Do NOT use print(); return data or call tools. The\n"
    "  transcript renders tool results; prose belongs in done(\"\"\"...\"\"\").\n"
    "- Session titles are host-generated — do NOT set or invent one.\n"
    "- VOICE + LENGTH: write like a concise teammate handing off work — lead with\n"
    "  the OUTCOME, then where + why. No filler openers (\"Great\", \"Sure\",\n"
    "  \"Certainly\"), no flattery, no emoji unless the user used them first. Length\n"
    "  TRACKS THE CHANGE: a one-line fix → 1–3 sentences, no headers; a few files\n"
    "  → a tight what + why; a big change → 1–2 bullets per file, grouped. Don't\n"
    "  paste files you wrote or before/after bodies — CITE clickable paths\n"
    "  (`src/foo.clj`, `src/foo.clj:42`). End with REAL next steps only (run,\n"
    "  test, commit), if any.\n"
    "- A REVIEW request flips the shape: findings FIRST, ordered by severity with\n"
    "  `path:line`, then a short recap. If nothing's wrong, say so plainly.\n\n"
    "## A complete turn looks like this\n"
    "Your whole reply is exactly the lines below — code only, nothing around\n"
    "them:\n\n"
    "# goal: find where the TUI title is set, then report.\n"
    "# search the WHOLE repo first — no paths — then narrow to the real hit.\n"
    "# bare value-returning forms — no `x = …`; each result is echoed + trailered.\n"
    "rg({\"any\": [\"set_session_title\", \"session-title\"]})\n"
    "cat(\"src/com/blockether/vis/internal/loop.clj\", {\"range\": [5700, 5760]})\n"
    "done(\"\"\"\n"
    "Titles are host-generated in `loop.clj` (`set-session-title!`), not a tool.\n"
    "\"\"\")\n\n"
    "That block is your ENTIRE reply, and it is ordinary, valid Python —\n"
    "exactly what you'd type in a short script: bare snake_case calls, dict\n"
    "literals for options, one statement per line, and `done(\"\"\"...\"\"\")` last.\n"
    "Each call sits on its own line; the reply is code from the first line to\n"
    "the last, with nothing wrapping it.\n\n"
    "## Discipline\n"
    "- Batch independent READS into one reply (several bare cat/rg forms\n"
    "  together) — each echoes its own value.\n"
    "- Prefer bare value-returning forms over `x = …`. A local binding is\n"
    "  throwaway scratch for the current reply; re-read files or prior `r[...]`\n"
    "  results rather than stashing state to carry across iterations.\n"
    "- Edit files via patch(...), not by re-writing whole files blindly. patch is\n"
    "  ATOMIC and all-or-nothing: every hunk resolves against the file as you last\n"
    "  read it, so a batch of hunks to one file is safe — they don't shift each\n"
    "  other. If patch RETURNS, every hunk applied and the returned diff IS your\n"
    "  confirmation — do NOT re-cat to \"check\". If it ERRORS, NOTHING changed —\n"
    "  the file is exactly as before; fix the one failing hunk and resend.\n"
    "- Anchor a hunk with the EXACT `lineno:hash` key from cat's `lines` map (e.g.\n"
    "  `325:0e3`): the line number LOCATES the line, the hash VERIFIES its content.\n"
    "  COPY the whole anchor cat gave you — never fabricate one or reuse one from an\n"
    "  earlier read. If that content has moved far from the line number, patch\n"
    "  REFUSES (`hash-misplaced`) rather than edit the wrong line — re-cat for\n"
    "  fresh anchors and resend. Duplicate lines just differ by line number;\n"
    "  there is no `#N` ordinal.\n"
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

(def weak-model-operating-rules
  "The few rules weaker / local models most often break, restated in plain
   imperative form. Injected (with `reason-via-comments-instruction`) only for
   non-`:reasoning?` models — the same audience that drowns in the full system
   prompt. Recency-weighted: it rides right before the conversation. Keep it
   SHORT; it reinforces, it does not re-teach the whole surface."
  (str "Nine rules that override any temptation to do more:\n"
    "1. Put your Python in ONE ```python … ``` fence; the engine runs only the "
    "fenced code and IGNORES anything outside it. It MUST contain runnable Python "
    "(at least one tool call or "
    "done(...)); NEVER reply with only reasoning/comments and no code, and never "
    "reply empty — that turn is wasted and rejected. All user-facing prose goes "
    "in done(\"\"\"…\"\"\"). done() is a SHORT summary, not a data dump: tool outputs "
    "already show as cards, so NEVER put a raw tool result in it (no "
    "done(str(x)), no f-string of an ls/cat/rg result).\n"
    "2. Search the WHOLE repo first: call rg with NO `paths` (it scans the whole "
    "project root `.`). Source is not only under `src/` — code lives in other "
    "trees like `extensions/`. Add `paths` only to NARROW after a broad search "
    "shows where the files are. Don't keep re-searching one guessed directory.\n"
    "3. Your only tools are the ones written above (the bare foundation "
    "functions + the verbs each active EXTENSION block lists). If a function is "
    "not written there, it does not exist — do NOT invent it, do NOT put shell "
    "commands in strings, do NOT fake an action with patch/write. patch/write "
    "edit real files by path, nothing else.\n"
    "4. One step at a time. Do a SINGLE edit or action, look at its result on "
    "the next turn, then do the next. Do not try to finish a multi-step task "
    "(e.g. add → commit → push) in one reply.\n"
    "5. Check `ctx` and the live runtime before acting; read an error fully "
    "before retrying — and change approach if it fails twice (don't repeat the "
    "same search/edit that already failed).\n"
    "6. If the available tools can't do what was asked, say so plainly in "
    "done(\"\"\"…\"\"\"). Do not improvise a fake solution.\n"
    "7. Call tools as BARE forms — `cat(\"x\")`, `rg({...})` — not `data = "
    "cat(\"x\")`. Each form's value is echoed back next turn and kept in `r`, so "
    "you do NOT need local variables or defs to carry results forward. Reserve "
    "`x = …` for a value you reuse in the SAME reply.\n"
    "8. Don't overthink. The moment a read (or an eval) answers your question, "
    "ACT — edit, answer, or run the check. Do NOT re-read a file you already "
    "read or re-derive a conclusion you already reached; that is wasted thinking. "
    "If the project has a live REPL (Clojure: `clj_eval(\"(some-fn 5 3)\")` against "
    "the nREPL whose port is in `ctx`), RUN the code to verify reality instead "
    "of reasoning about what it does — one eval beats ten paragraphs of analysis.\n"
    "9. Drive it END-TO-END. The ask means MAKE the change, not describe it: "
    "find it, edit it, verify it, done(). Don't stop after analysis to ask \"want "
    "me to fix it?\" for obvious work — just fix it, then report. Only PROPOSE "
    "first (candidate steps + stop) when the work is big, risky, or you'd be "
    "guessing what done means."))

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

(def cli-autonomous-rules
  "Override injected ONLY for the non-interactive `:cli` channel (headless
   `bin/vis '<task>'` one-shot runs). No human is in the loop, so the model
   must never wait for input — it makes reasonable assumptions and drives the
   work to a real `done(...)`."
  (str "NON-INTERACTIVE ONE-SHOT RUN — no human is watching and nothing can "
    "be approved mid-run.\n"
    "- NEVER stop to wait for approval or input — there is no one to answer.\n"
    "- When the work is big, risky, or the ask is ambiguous, do NOT ask: make "
    "the most reasonable assumption, STATE it in one line, and EXECUTE end-to-"
    "end to a real `done(...)`. Drive the work to completion in this single run."))

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
     `:system-prompt`            - caller addendum appended to CORE.
     `:session-context`          - rendered fenced-Python `ctx = {…}` block
        (standing session state: workspace / env / routing / tools). Embedded
        ONCE here as a cached system message; the loop re-emits only the
        `ctx[...] = …` structural delta in the conversation when it changes
        mid-turn."
  [environment {:keys [system-prompt active-extensions session-context] :as opts}]
  (when-not (contains? opts :active-extensions)
    (throw (ex-info "assemble-stable-prompt-messages requires :active-extensions"
             {:type :vis/missing-active-extensions})))
  (let [core-block (prompt-block "system-prompt"
                     (build-system-prompt {:system-prompt system-prompt}))
        ;; Non-interactive `:cli` runs drop the candidate approval STOP — no
        ;; human can approve a one-shot run. Stable per session (channel never
        ;; changes), so it doesn't churn the prefix cache.
        cli-block (when (= :cli (:channel environment))
                    (prompt-block "cli-autonomous" cli-autonomous-rules))
        project-block (project-instructions-block)
        turn-system-block (turn-system-context-block environment active-extensions)
        ;; Standing session context (workspace/env/routing/tools), rendered
        ;; into the cached prefix so it isn't re-billed every iteration. The
        ;; fenced `ctx = {…}` block is self-describing, so it rides as its own
        ;; system message (no `;; -- TAG --` wrapper).
        session-context-block (not-empty (some-> session-context str/trim))]
    (vec
      (keep stable-prompt-message
        [core-block cli-block project-block turn-system-block session-context-block]))))

