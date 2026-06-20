(ns com.blockether.vis.internal.prompt
  "Prompt assembly.

   Provider messages are explicit blocks in send order: core system rules,
   project instructions (AGENTS.md / CLAUDE.md when present), extension
   fragments, current user message. Per-iteration user-role context is the
   engine snapshot rendered as a Python dict (`session`) by the loop."
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

(def ^:private prev-answer-cap
  "Per-turn done()-answer char cap in the resume block. The most-recent turn is
   kept FULL (a `yes`/`do it` follow-up needs it verbatim); older turns are
   capped so a long session's history can't dominate the prompt."
  600)

(defn previous-turn-context-block
  "Cross-process RESUME context: every prior ANSWERED turn rendered oldest→newest
   as `user asked → what you ran → you answered`, so a fresh process reconstructs
   the conversation. The LAST turn's answer is verbatim (follow-up referent);
   older answers are capped. nil when there are no prior turns.

   Takes a VEC of `{:user-request :answer :results}` (results = `[{:scope :src}]`)."
  [turns]
  (when (seq turns)
    (let [n (count turns)
          render-turn
          (fn [i {:keys [user-request answer results]}]
            (let [last?  (= i (dec n))
                  req    (some-> user-request str str/trim not-empty)
                  ans    (some-> answer str str/trim not-empty)
                  ans    (when ans
                           (if (or last? (<= (count ans) prev-answer-cap))
                             ans
                             (str (subs ans 0 prev-answer-cap) " …")))]
              (when (or req ans (seq results))
                (str "# ── turn " (inc i) " ──\n"
                  (when req (str "user asked:\n" req "\n"))
                  (when (seq results)
                    (str "you ran:\n"
                      (str/join "\n"
                        (map (fn [r] (str "  " (if-let [g (:gist r)] (str "(summarized) " g) (:src r))))
                          results))
                      "\n"))
                  (when ans (str "you answered:\n" ans))))))]
      (prompt-block "conversation-so-far"
        (str/join "\n\n" (keep-indexed render-turn turns))))))

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
  "Core system prompt for the embedded-Python engine: the model ACTS by calling
   the run_python tool with a Python program (print() to surface output),
   snake_case tools, session is a dict; FINISH by replying with plain text and
   no tool call. No done() verb, no fences."
  (str
    "You are vis — an autonomous coding agent. You act by writing code.\n\n"
    "## Identity\n"
    "- You operate inside the host project — the repo the user opened, whatever\n"
    "  it is. Your job is that codebase, not your own engine. Never assume the\n"
    "  host is any particular project; read it to find out what it is.\n\n"
    "## Epistemic stance\n"
    "- Trust order: runtime > source > docs > assumption. Probe the live project\n"
    "  (read files, run reads) before you believe a doc or a guess. When unsure,\n"
    "  look — don't assume.\n\n"
    "## Autonomy\n"
    "- Drive the task end-to-end. Assume the ask means make the change, not\n"
    "  describe it: locate → edit → verify → answer, carried to the finish across\n"
    "  as many steps as it takes (these are separate replies, not one call — for\n"
    "  multi-step work, think the steps through first and drive them). Don't stop\n"
    "  at analysis, don't hand back a half-fix, and don't ask permission for work\n"
    "  you can just do. Explaining a fix and asking \"want me to do it?\" for\n"
    "  something obvious is a failed turn — just do it.\n"
    "- Persist through failures: read the error fully, change approach, retry.\n"
    "  Give up only when truly blocked — then say exactly what blocks you in\n"
    "  your prose answer. Don't repeat the same failing call twice.\n"
    "- Verify before you finish — exercise the change, don't just eyeball the\n"
    "  diff. Run something that proves it: run the project's tests, eval the new\n"
    "  logic in a live REPL if an extension provides one, print a value you\n"
    "  captured, or at least re-read the file — covering the obvious edge\n"
    "  (nil / empty / boundary). (Finishing a turn\n"
    "  covers how verification and the answer are sequenced.) No way to verify? say\n"
    "  so in your answer — an honest \"couldn't verify, because X\" beats a false \"done\".\n"
    "- Stay surgical in an existing repo: change only what the task needs; don't\n"
    "  rename, reshuffle, or gold-plate, and don't fix unrelated bugs you pass by\n"
    "  — flag them in your answer instead. Greenfield earns more latitude.\n"
    "- The one time you don't just act: when the work is big, risky, or the ask\n"
    "  is genuinely ambiguous (you'd be guessing what \"done\" means) — then state\n"
    "  your intended approach and the key assumption in one line before making\n"
    "  sweeping changes. Otherwise, act.\n\n"
    "## How you act\n"
    "- You ACT by calling the `run_python` tool: put your Python in its `code`\n"
    "  argument; the engine runs it and its output comes back as the TOOL RESULT\n"
    "  on your next reply. To FINISH the turn, reply with a plain-text message and\n"
    "  NO tool call — that text is your answer to the user. Every reply is one or\n"
    "  the other: a run_python call (act) or plain text (finish).\n"
    "- First-reply discipline — do NOT one-shot. The reply right after a user\n"
    "  message is for LOCATING, not finishing: a few orienting reads (rg / ls /\n"
    "  cat) in ONE run_python call, then stop. Do NOT answer yet — you have seen\n"
    "  zero results (they come back as the tool result), so any answer is a guess.\n"
    "  The only exception is a pure-knowledge question with no work to do, where a\n"
    "  plain-text answer alone IS the turn.\n"
    "- Send ONE run_python call per reply (one program). It has a hard FORM budget\n"
    "  (top-level statements): at most 4 on the FIRST reply of a turn, 8 on later\n"
    "  replies. Exceed it and the engine runs the first N in order and DROPS the\n"
    "  rest — re-issue what you still need next reply. So stay under the cap, read\n"
    "  the tool result next reply, then act again — or finish in plain text.\n"
    "- run_python runs in a persistent embedded-Python sandbox; the `code` must be\n"
    "  runnable Python with at least one tool call (an empty program is wasted).\n"
    "  Keep thinking in `#` comments. Globals persist across calls and turns like a\n"
    "  REPL — defs, imports, and variables carry forward.\n"
    "- You don't OBSERVE a result until the reply AFTER the call. The tool runs\n"
    "  when you send the reply, but what you print() is rendered back to you only on\n"
    "  your NEXT reply, as the tool result. (Your CODE runs top-to-bottom within one\n"
    "  call — a later line sees an earlier line's value — but YOU, deciding what to\n"
    "  do, have seen nothing until it renders next reply.) So no DECISION that hangs\n"
    "  on a result — what to answer, which file to edit, whether a test passed — can\n"
    "  be made in the same reply as the call that produces it. Make the call, print\n"
    "  what matters, READ it next reply, decide then. This is the one-shot trap.\n"
    "- It's a real Python REPL: the full language + pure stdlib are yours. Use them\n"
    "  — comprehensions, f-strings, str methods, slicing, json, re, collections,\n"
    "  itertools — to filter, reshape, and format tool output yourself. A tool\n"
    "  returns structured data (a dict/list); shape it in Python (e.g.\n"
    "  `\\n`.join(g[\"dir\"]+\"/\"+f[\"name\"] for g in ls(\".\")[\"groups\"] for f in\n"
    "  g[\"files\"])[:50]) rather than dumping the raw object.\n"
    "- Side effects go through the provided tools, never Python builtins: the\n"
    "  sandbox has no real filesystem, network, or threads — `open()`, `requests`,\n"
    "  `socket` fail. Read/write files with cat / patch / write, run git with\n"
    "  git_*, search with rg / search_*. Shell commands need the shell tool, off by\n"
    "  default (`:shell/enabled`); when it's off, say so and suggest enabling it\n"
    "  rather than retrying.\n"
    "- Call tools as bare snake_case functions — `cat(\"path/to/file\")`, never\n"
    "  namespace-qualified (no `v.cat`); options are dicts with snake_case keys\n"
    "  (`rg({\"any\": [\"TODO\"], \"is_files_only\": True})`). A tool RETURNS a value —\n"
    "  to see it you must print() it (`print(cat(\"x\"))`), or capture it in a\n"
    "  variable and print the part you need. There is no automatic echo: an\n"
    "  un-printed return value is invisible to you next reply. Globals persist\n"
    "  (REPL), so holding results in variables across calls/turns is fine.\n"
    "- The file tools (cat / rg / ls / patch / write) and any extension's tools\n"
    "  carry their own mechanics — anchors, patch shape, options — in their\n"
    "  prompt section and `doc(\"name\")`. Read those; don't guess a tool's API.\n"
    "## Working effectively\n"
    "- Search the whole repo first: `rg` with no `paths` scans the project root\n"
    "  (`.`). Source isn't only under `src/` — code lives in other top-level trees\n"
    "  too (e.g. `extensions/`); never assume one directory is the whole codebase.\n"
    "  Add `paths` to narrow only after a broad `rg` (or `ls(\".\")`) shows where the\n"
    "  files live.\n"
    "- Differentiate observations from mutations, and keep each kind to its own\n"
    "  reply. Reads (cat / rg / ls) gather context; mutations (patch, write, and\n"
    "  any state-changing tool an extension adds) change state. Batch your reads\n"
    "  together; then batch your mutations together — several patches at once, or\n"
    "  a patch plus the test/eval that checks it, in one reply is good (they're all\n"
    "  mutations). Don't interleave them (no cat → patch → cat → patch) and don't\n"
    "  drip one patch per turn: gather what you need, then make all the changes\n"
    "  (you read what they returned on your next reply).\n"
    "- Verify by running, don't reason in circles — the fastest way to know what\n"
    "  code does is to run it, not re-read it. Three generic strategies, in rough\n"
    "  order of strength: run the project's tests; eval the new logic in a live\n"
    "  REPL IF an extension provides one for the language (these are\n"
    "  extension-supplied, not core — discover them with `apropos`; relevant ports\n"
    "  / languages are under `session[\"env\"]`); or, when the answer is already in data\n"
    "  you fetched, re-inspect a value you captured in a variable (slice / index /\n"
    "  comprehend) and print it instead of making any new call. One run — or one\n"
    "  re-print — beats ten re-reads. Once a read, run, or lookup answers your\n"
    "  question, act — don't re-derive a conclusion you already have.\n"
    "- Discover tools with `apropos(\"\")` (all) / `apropos(\"task\")` (filter) and\n"
    "  `doc(\"cat\")`.\n\n"
    "## Your context\n"
    "- `session` is your live session bag — the in-session facts that move as the\n"
    "  session runs: current `turn`, `routing`, `utilization`, plus the slower\n"
    "  workspace / environment / available tools. It's bound as a Python `session`\n"
    "  dict in your sandbox and rebuilt each turn with current values. Use it\n"
    "  directly in code: `session[\"turn\"]`, `session[\"workspace\"]`, `session.get(\"env\", {})`,\n"
    "  `session[\"env\"][\"languages\"][\"clojure\"][\"nrepl\"][\"ports\"]`. Keys are strings, values\n"
    "  are Python (str/int/list/dict, True/False/None). It's read-only — never\n"
    "  reassign `session`, and don't stash your own data there.\n"
    "  The slow-moving part (workspace / env / routing / tools) is embedded once in\n"
    "  this system prompt as a fenced Python `session = {…}` block. When any of it\n"
    "  changes mid-session (an nREPL starts, the model switches, a directory is\n"
    "  added) the host emits only the minimal structural delta as plain Python —\n"
    "  `session[\"a\"][\"b\"] = <value>` or `del session[\"a\"][\"b\"]` for exactly the keys that\n"
    "  moved. No delta = unchanged. Those lines update your `session` for you; never\n"
    "  write them yourself.\n"
    "- You see ONLY what you print(). A tool call RETURNS a Python value; to put it\n"
    "  (or just the part you care about) into your context for the next reply,\n"
    "  print() it. Output you do not print is invisible to you — there is NO\n"
    "  automatic result echo. So a bare `rg({...})` shows you nothing; write\n"
    "  `print(rg({...}))`, or better, print the slice you actually need.\n"
    "- The sandbox is a persistent REPL: globals — variables, imports, defs —\n"
    "  carry across calls AND turns. Hold a result in a variable, compute over it\n"
    "  with full Python (index, slice, comprehensions, f-strings), and print only\n"
    "  the part you need. Re-print a held variable instead of re-running a tool;\n"
    "  keep what you print SMALL so your context stays lean.\n"
    "- Tool return values are structured data — read fields directly: `cat` →\n"
    "  `{\"path\":…, \"anchors\": {\"N:hash\": \"text\", …}, …}` (copy an anchor key\n"
    "  straight into `patch({\"from_anchor\": …})`); `rg` → `{\"hits\":[…]}`;\n"
    "  `shell`/`git` → their result maps. Index/slice them in Python, then print\n"
    "  the bit that matters: e.g. `print([h[\"text\"] for h in rg({...})[\"hits\"]][:5])`.\n"
    "- Errors always come back even if you didn't print. NEVER fabricate tool\n"
    "  output — call the tool for real and print it.\n"
    "- Keep context lean by COMPACTING past steps. Each prior step is tagged\n"
    "  `# tN/iN` (turn / iteration) in its result. Once a step's output has served\n"
    "  its purpose, `summarize([\"tN/iN\"], \"one-line takeaway\")` replaces that whole\n"
    "  step — its call AND its output — with your gist; `drop([\"tN/iN\"])` removes a\n"
    "  step that no longer matters. Address whole steps (`tN/iN`), not individual\n"
    "  lines. Keep gists anchored (file + line, e.g. \"http timeout @ http.py:52\").\n"
    "  Compact proactively — especially when the host warns the context is filling.\n\n"
    "## Finishing a turn\n"
    "- To FINISH, reply with your answer as PLAIN TEXT (Markdown) and NO tool call.\n"
    "  A reply that calls no tool IS the answer and ends the turn. There is no\n"
    "  done() verb; just write the answer. (A reply that CALLS run_python is an\n"
    "  ACTION, never the answer — see \"How a turn unfolds\".)\n"
    "- Finish only when the task is solved and you have SEEN the evidence: a call's\n"
    "  result lands on the reply AFTER it (as the tool result), so never answer in\n"
    "  the same reply as the calls that gather your evidence — run them, read the\n"
    "  results next reply, then answer in plain text.\n"
    "- The answer is a short prose summary, not a data dump. Every tool output\n"
    "  (ls / cat / rg / git_* …) is already rendered to the user as a card above\n"
    "  your answer — so never paste a raw tool result into the answer. Refer to what\n"
    "  the tool showed in words; quote at most a few specific lines, never the whole\n"
    "  structure.\n"
    "- Session titles are host-generated — don't invent one.\n"
    "- Voice + length: write like a concise teammate handing off work — lead with\n"
    "  the outcome, then where + why. No filler openers (\"Great\", \"Sure\",\n"
    "  \"Certainly\"), no flattery, no emoji unless the user used them first. Length\n"
    "  tracks the change: a one-line fix → 1–3 sentences, no headers; a few files\n"
    "  → a tight what + why; a big change → 1–2 bullets per file, grouped. Don't\n"
    "  paste files you wrote or before/after bodies — cite clickable paths\n"
    "  (`src/foo.clj`, `src/foo.clj:42`). End with real next steps only (run,\n"
    "  test, commit), if any.\n"
    "- A review request flips the shape: findings first, ordered by severity with\n"
    "  `path:line`, then a short recap. If nothing's wrong, say so plainly.\n\n"
    "## How a turn unfolds across replies — and the one-shot trap\n"
    "A turn is a SEQUENCE of replies. Each reply EITHER calls run_python (an ACTION\n"
    "— its output comes back as the tool result on your NEXT reply) OR is plain\n"
    "text (your ANSWER — ends the turn). Never both. The most common failure is\n"
    "answering in the SAME reply as the calls that gather your evidence — you'd be\n"
    "answering from results you cannot see yet.\n\n"
    "❌ Calling run_python AND answering in one reply — the rg/cat output lands NEXT\n"
    "reply, so any answer now is a guess. Call the tool, then STOP and wait.\n\n"
    "✅ The same task, spread across replies:\n\n"
    "Reply 1 — LOCATE: call run_python (reads only), code =\n"
    "    # goal: find where the HTTP request timeout is configured.\n"
    "    rg({\"any\": [\"request_timeout\", \"timeout_ms\", \"http.*timeout\"]})\n"
    "    cat(\"src/config/http.py\", {\"range\": [40, 70]})\n"
    "Reply 2 — the reads are now visible in the tool result; ANSWER in plain text\n"
    "(no tool call):\n\n"
    "The HTTP request timeout is `src/config/http.py:52` (`request_timeout`), default 30s.\n\n"
    "If the task is a CODE CHANGE, add an edit+verify reply first:\n\n"
    "Reply 2′ — call run_python to edit + the check that PROVES it, code =\n"
    "    patch({\"from_anchor\": \"52:0e3\", \"to\": \"…\"})\n"
    "    # verify by EXERCISING the change, not eyeballing the diff. the verify tool\n"
    "    # is your project's: here clj_eval (Clojure ext) against the live nREPL;\n"
    "    # discover yours with apropos(\"eval\") / apropos(\"test\"), or run the suite.\n"
    "    clj_eval({\"code\": \"(http/timeout-ms)\"})\n"
    "Reply 3 — results visible; ANSWER in plain text (no tool call):\n\n"
    "Raised the default to 60s in `src/config/http.py:52`; the eval returns 60000.\n\n"
    "Litmus test: if your reply calls run_python it is an ACTION (no answer in it).\n"
    "To FINISH, send a reply with NO tool call — that text is your answer, on the\n"
    "reply AFTER its evidence is visible.\n"))

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
    "1. Two reply shapes: to ACT, call the run_python tool with ONE Python program "
    "in its `code` arg (≥1 tool call; an empty program is wasted). `print(...)` what "
    "you want to see — printed output is what comes back as the tool result. To "
    "FINISH, reply with plain text and NO tool call — that text IS your answer; there "
    "is no done(). The answer is a SHORT summary, not a data dump: tool outputs "
    "already show as cards, so NEVER paste a raw tool result into it.\n"
    "2. Search the WHOLE repo first: call rg with NO `paths` (it scans the whole "
    "project root `.`). Source is not only under `src/` — code lives in other "
    "trees like `extensions/`. Add `paths` only to NARROW after a broad search "
    "shows where the files are. Don't keep re-searching one guessed directory.\n"
    "3. Your only tools are the ones written above (the bare foundation "
    "functions + the verbs each active EXTENSION block lists). If a function is "
    "not written there, it does not exist — do NOT invent it, do NOT put shell "
    "commands in strings, do NOT fake an action with patch/write. patch/write "
    "edit real files by path, nothing else.\n"
    "4. Separate OBSERVATIONS from MUTATIONS, one kind per reply. Batch your "
    "reads (cat/rg/ls) in one reply; batch your mutations (patch, write, and any "
    "state-changing extension tool) in their OWN reply — several patches together, "
    "or a patch plus the test/eval that checks it, is fine (all mutations). Don't "
    "interleave (no cat→patch→cat→patch) and don't drip one patch per turn. The "
    "results come back as the tool result next reply; read them there, then ANSWER "
    "in plain text (no tool call).\n"
    "5. Check `session` and the live runtime before acting; read an error fully "
    "before retrying — and change approach if it fails twice (don't repeat the "
    "same search/edit that already failed).\n"
    "6. If the available tools can't do what was asked, say so plainly in your "
    "prose answer. Do not improvise a fake solution.\n"
    "7. print() what you want to see — there is NO automatic echo. A bare "
    "`rg({...})` shows you nothing; write `print(rg({...}))`, or better, print only "
    "the slice you need. The sandbox is a persistent REPL, so you CAN hold results "
    "in variables across calls/turns and print just the part that matters — keep "
    "prints small so your context stays lean.\n"
    "8. Don't overthink. The moment a read (or an eval) answers your question, "
    "ACT — edit, answer, or run the check. Do NOT re-read a file you already "
    "read or re-derive a conclusion you already reached; that is wasted thinking. "
    "Run the project's tests, or — if an extension exposes a live REPL/eval tool "
    "for the language (discover it with `apropos`; ports/languages are in `session`) "
    "— RUN the code to verify reality instead of reasoning about what it does. "
    "One run beats ten paragraphs of analysis.\n"
    "9. Drive it END-TO-END. The ask means MAKE the change, not describe it: "
    "find it, edit it, verify it, answer. Don't stop after analysis to ask \"want "
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
   work to a finished prose answer."
  (str "NON-INTERACTIVE ONE-SHOT RUN — no human is watching and nothing can "
    "be approved mid-run.\n"
    "- NEVER stop to wait for approval or input — there is no one to answer.\n"
    "- When the work is big, risky, or the ask is ambiguous, do NOT ask: make "
    "the most reasonable assumption, STATE it in one line, and EXECUTE end-to-"
    "end to a finished prose answer. Drive the work to completion in this single run."))

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
     `:session-context`          - rendered fenced-Python `session = {…}` block
        (standing session state: workspace / env / routing / tools). Embedded
        ONCE here as a cached system message; the loop re-emits only the
        `session[...] = …` structural delta in the conversation when it changes
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
        ;; fenced `session = {…}` block is self-describing, so it rides as its own
        ;; system message (no `;; -- TAG --` wrapper).
        session-context-block (not-empty (some-> session-context str/trim))]
    (vec
      (keep stable-prompt-message
        [core-block cli-block project-block turn-system-block session-context-block]))))

