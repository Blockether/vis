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
  (binding [extension/*current-extension*
            ext

            extension/*current-symbol*
            nil]

    (apply f args)))

;; =============================================================================
;; Initial messages
;; =============================================================================

(def ^:private prev-answer-cap
  "Per-turn answer char cap in the resume block. The most-recent turn is
   kept FULL (a `yes`/`do it` follow-up needs it verbatim); older turns are
   capped so a long session's history can't dominate the prompt."
  600)

(defn previous-turn-context-block
  "Cross-process RESUME context: every prior ANSWERED turn rendered oldest→newest
   as `user asked → what you ran → you answered`, so a fresh process reconstructs
   the conversation. The LAST turn's answer is verbatim (follow-up referent);
   older answers are capped. nil when there are no prior turns.

   Takes a VEC of `{:user-request :answer :interrupted? :results}` (results =
   `[{:scope :src}]`). An `:interrupted?` turn was cut off mid-flight (e.g. a
   process restart) with no answer — it's surfaced so a follow-up `continue`
   knows the pending work."
  [turns]
  (when (seq turns)
    (let
      [n
       (count turns)

       render-turn
       (fn [i {:keys [user-request answer interrupted? results]}]
         (let [last?
               (= i (dec n))

               req
               (some-> user-request
                       str
                       str/trim
                       not-empty)

               ans
               (some-> answer
                       str
                       str/trim
                       not-empty)

               ans
               (when ans
                 (if (or last? (<= (count ans) prev-answer-cap))
                   ans
                   (str (subs ans 0 prev-answer-cap) " …")))]

           (when (or req ans (seq results))
             (str
               "# ── turn "
               (inc i)
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
                  (str/join "\n"
                            (concat (map-indexed (fn [i {:keys [path media-type size-label]}]
                                                   (str "- image "
                                                        (inc i)
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
                                         skipped))))))

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
  (let [previous-block
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
  "Core system prompt for the hybrid tool surface: the model ACTS by calling the
   DIRECT file tools (cat/rg/find/patch/move/delete/ls) or `python_execution`
   (Python program, print() to surface output) for transforms AND for the
   tree-sitter STRUCTURAL editors (struct_patch/outline/sexpr/occurrences/symbol_rename
   — the preferred way to edit CODE); FINISH by replying with plain
   text and no tool call."
  (str
    "You are vis — an autonomous coding agent. You act by writing code.\n\n"
    "## Identity\n" "- You operate inside the host project — the repo the user opened, whatever\n"
    "  it is. Your job is that codebase, not your own engine. Never assume the\n"
    "  host is any particular project; read it to find out what it is.\n\n"
    "## Epistemic stance\n"
    "- Trust order: runtime > source > docs > assumption. Probe the live project\n"
    "  (read files, run reads) before you believe a doc or a guess. When unsure,\n"
    "  look — don't assume.\n\n"
    "## Autonomy\n" "- Drive the task end-to-end. Assume the ask means make the change, not\n"
    "  describe it: locate → edit → verify → answer, carried to the finish across\n"
    "  as many steps as it takes (these are separate replies, not one call — for\n"
    "  multi-step work, think the steps through first and drive them). Don't stop\n"
    "  at analysis, don't hand back a half-fix, and don't ask permission for work\n"
    "  you can just do. Explaining a fix and asking \"want me to do it?\" for\n"
    "  something obvious is a failed turn — just do it.\n"
    "- Persist through failures: read the error fully, change approach, retry.\n"
    "  Give up only when truly blocked — then say exactly what blocks you in\n"
    "  your prose answer. Don't repeat the same failing call twice.\n"
    "- Verify before you finish — exercise the change (run tests / eval the new\n"
    "  logic / print a captured value / re-read), covering the obvious edge\n"
    "  (nil / empty / boundary). Can't verify? say so honestly — \"couldn't verify,\n"
    "  because X\" beats a false \"done\".\n"
    "- Stay surgical in an existing repo: change only what the task needs; don't\n"
    "  rename, reshuffle, or gold-plate, and don't fix unrelated bugs you pass by\n"
    "  — flag them in your answer instead. Greenfield earns more latitude.\n"
    "- The one time you don't just act: when the work is big, risky, or the ask\n"
    "  is genuinely ambiguous (you'd be guessing what \"done\" means) — then state\n"
    "  your intended approach and the key assumption in one line before making\n"
    "  sweeping changes. Otherwise, act.\n\n"
    "## How you act\n"
    "- You ACT by calling tools. The FILE tools — `cat`, `rg`, `find`, `patch`,\n"
    "  `move`, `delete`, `ls` — are DIRECT calls: pass their arguments and the\n"
    "  result comes back as the TOOL RESULT on your NEXT reply. When you need to\n"
    "  TRANSFORM / FILTER / CHAIN tool output — OR to edit code structurally — call\n"
    "  `python_execution` with a Python program instead (it also hosts the structural\n"
    "  editors `struct_patch` / `outline` / `occurrences` / `symbol_rename`; see below).\n"
    "  To FINISH the turn, reply with plain text and NO tool call —\n"
    "  that text is your answer. Every reply is exactly ONE tool call, or a plain-\n"
    "  text finish.\n"
    "- Prefer a DIRECT file tool for plain actions; reach for `python_execution`\n"
    "  ONLY when you actually compute over results (filter a search, read+scan many\n"
    "  files, feed one tool's output into another). Wrapping a single read in\n"
    "  `python_execution` is wasted — call the tool directly.\n"
    "- First-reply discipline — do NOT one-shot. The reply right after a user\n"
    "  message is for LOCATING, not finishing: a few orienting reads (`find` /\n"
    "  `rg` / `ls` / `cat`), then stop. You have seen zero results (they arrive as\n"
    "  the tool result NEXT reply), so any answer now is a guess. The only exception\n"
    "  is a pure-knowledge question with no work to do.\n"
    "- Read the tool result on your NEXT reply BEFORE deciding anything that hangs\n"
    "  on it — what to answer, which file to edit, whether a test passed. Deciding\n"
    "  in the SAME reply as the call that produces the value is the one-shot trap.\n"
    "- Work in FEW FAT steps, not many thin ones. Read a GENEROUS window the first\n"
    "  time — the whole function/region you'll touch — not tiny slices you then\n"
    "  re-read; never re-read a range you already have.\n"
    "### python_execution — transforms / chaining only\n"
    "- Every tool call returns ONE value — the tool result you read next reply. For\n"
    "  `python_execution` that return IS the text it print()s (a string built from\n"
    "  your print()s) — so print() exactly what you want back; the Python last-\n"
    "  expression value is NOT the return. Keep thinking in `#` comments.\n"
    "- A persistent Python sandbox: globals (defs, imports, variables) carry across\n"
    "  calls and turns like a REPL — so REUSE what you already captured. A result you\n"
    "  bound to a var (`r = shell_run(...)`, `hits = await rg(...)`) is STILL THERE next\n"
    "  call: read another slice of `r`, filter `hits` again, chain it onward. NEVER\n"
    "  re-run the same command (a shell build/test, a search, a read) just to look at\n"
    "  its output again, and don't clobber a var with an identical call — that re-pays\n"
    "  the cost for a value you already hold. Empty output means YOU under-printed, not\n"
    "  that the value is gone; print the part of the existing var you need.\n"
    "- The file tools are ALSO bare snake_case async fns in scope here — call them\n"
    "  directly, NEVER `import` or namespace-qualify (`v.cat`) — and `await` them:\n"
    "  `data = await cat(\"x\")`, `print(await cat(\"x\"))`. A bare call on its own line\n"
    "  auto-runs, but anywhere you USE the value (print, another call, a\n"
    "  comprehension, an f-string) you MUST `await`. Options are dicts with snake_case\n"
    "  keys (`rg(\"TODO\", is_files_only=True)`; rg is smart-case, so `rg(\"key\")` finds\n"
    "  Key/KEY/keymap — you rarely list variants). The\n"
    "  full stdlib is yours (re, json, collections, itertools); SHAPE structured\n"
    "  results in Python and print only the compact slice you need, never a raw dump.\n"
    "- PARALLELIZE reads — this is the DEFAULT, not an optimization. Any time two+ tool\n"
    "  calls DON'T depend on each other, batch them into ONE `await gather(...)`: it runs\n"
    "  them CONCURRENTLY on host virtual threads and returns results IN ORDER, so N reads\n"
    "  cost ONE round-trip instead of N. `a, b, c = await gather(cat(\"http.py\"),\n"
    "  rg(\"request_timeout\"), outline(\"config.py\"))`. Reaching for one file/symbol at a\n"
    "  time — a serial `await` per read, a fresh turn per read — is the #1 way a turn\n"
    "  gets needlessly slow. ONLY chain sequentially when a later call genuinely NEEDS an\n"
    "  earlier result (e.g. `outline` a path you just found). When in doubt, gather.\n"
    "- A tool's result auto-returns to your context next reply — do NOT re-print it,\n"
    "  echo a whole file / diff back, or re-`cat` a file you just edited to \"verify\".\n"
    "  An edit's per-file result (`path` + `changed`) IS the confirmation; an in-place\n"
    "  `format_code({\"path\": f})` returns a lean ack, not the file — don't print it back.\n"
    "  Re-dumping a result you already have doubles context for zero new signal.\n"
    "- Need a PRIOR native tool's result again (this turn or an earlier one) — even after\n"
    "  a restart cleared your Python vars? It's kept: `native_tools_results[tool_id]` in\n"
    "  `python_execution` returns that call's EXACT result dict from the store, no re-run.\n"
    "  Use the tool_use id shown on its tool_result (each result is tagged with its\n"
    "  `native_tools_results[\"…\"]` handle). Prefer this over re-`cat`/re-`rg`-ing to\n"
    "  re-read something you already fetched.\n"
    "## Working effectively\n"
    "- Discover: `find_files(query)` for vague names / concepts / unfamiliar modules (ranked\n"
    "  paths), then `cat` the likely files; scoped `rg` for an exact symbol or string;\n"
    "  `ls` for literal directory contents. For a CODE file, `outline(path)` lists its\n"
    "  defs / classes / methods up front — cheaper than reading the whole file. Once you\n"
    "  know the handful of files/symbols to inspect, read them ALL in one\n"
    "  `await gather(...)`, not one per turn.\n"
    "## Editing code — pick by HOW YOU LOCATE the edit\n"
    "- Every code editor (`struct_patch`, `symbol_rename`, AND `patch`) RE-PARSES its\n"
    "  result and REFUSES a syntax-breaking edit — none can leave a broken file. So the\n"
    "  choice is the LOCATOR, not which is 'safer':\n"
    "  - Know the NAME? → `struct_patch` — one atomic call, no `cat`, never stale:\n"
    "      await struct_patch({\"path\": P, \"op\": \"replace\", \"target\": \"fn_name\", \"code\": \"…\"})\n"
    "    ops: replace | delete | insert_before | insert_after | append | add_doc |\n"
    "    replace_doc | replace_node | rename | move_before | move_after; `kind`\n"
    "    disambiguates same-named defs; `move_*` relocates a def next to `anchor`;\n"
    "    `rename` rewrites the identifier syntax-safely EVERYWHERE; `delete` drops it.\n"
    "  - Have a fresh ANCHOR? → `patch` — a `from_anchor` `lineno:hash` (+ optional\n"
    "    `to_anchor` span) from a fresh `cat`/`outline`; atomic; `\"replace\": \"\"` deletes;\n"
    "    anchors go STALE after any write, so re-grab them.\n"
    "  - A sub-def NODE (an expr inside a fn)? → `sexpr(path)` to get its `at` PATH,\n"
    "    then `struct_patch` that path.\n"
    "- Locate cheaply with `outline(path)` — every def + its `@lineno:hash..lineno:hash`\n"
    "  anchors (feeds BOTH struct_patch-by-name and patch, no re-cat).\n"
    "- `occurrences(name)` traces a symbol project-wide: every use PLUS the definition(s),\n"
    "  marked with kind/visibility/signature/span (real identifier boundaries, not strings/\n"
    "  comments). Filter `is_definition`; run before a rename.\n"
    "- `symbol_rename(old, new)` renames a symbol — or a whole Clojure NAMESPACE (the\n"
    "  `(ns …)` form + `:require` targets + qualified usages) — across the project. For a\n"
    "  non-code file / unsupported language / whole-file rewrite → `write` (no anchors).\n"
    "- Edit EXISTING files; do NOT create new ones unless the task genuinely needs them.\n"
    "  NEVER write scratch / debug / notes / report / *.md files to poke at a problem\n"
    "  (`print()` instead) — they pollute the repo and aren't the task.\n"
    "- Verify by RUNNING, not re-reading — run the project's tests (`run_tests(language)`),\n"
    "  RUN new logic with `repl_eval(language, code)` when a pack lists it (see LANGUAGE\n"
    "  TOOLS / `session[\"language_tools\"]`), or re-inspect a value you already captured.\n"
    "  One run beats ten re-reads; once a read/run answers your question, ACT.\n"
    "- Discover tools with `apropos(\"\")` (all) / `apropos(\"struct\")` (filter) and\n"
    "  `doc(\"struct_patch\")`.\n\n" "## Your context\n"
    "- `session` is your live session bag — a READ-ONLY Python dict, rebuilt each\n"
    "  turn: `turn`, `routing`, `utilization` (`saturation` %, `headroom_tokens`),\n"
    "  plus slower `workspace` / `env` /\n"
    "  available tools. Use it in code: `session[\"workspace\"]`, `session.get(\"env\", {})`,\n"
    "  `session[\"env\"][\"languages\"][\"clojure\"][\"nrepl\"][\"ports\"]`. The slow parts are\n"
    "  embedded once as a fenced `session = {…}` block; when they change mid-session\n"
    "  the host emits minimal `session[\"a\"][\"b\"] = …` / `del …` deltas that update it\n"
    "  for you — never write those, never reassign `session` or stash your own data."
    "- Errors always come back even if you didn't print. NEVER fabricate tool\n"
    "  output — call the tool for real and print it.\n"
    "- Keep context lean by COMPACTING past steps. Each prior step is tagged\n"
    "  `# tN/iN` (turn / iteration) in its result. Once a step's output has served\n"
    "  its purpose, `session_fold([\"tN/iN\"], \"what this step established\")` replaces that\n"
    "  whole step — its call AND its output — with your summary; `session_drop([\"tN/iN\"])` removes\n"
    "  a step that no longer matters. Address whole steps (`tN/iN`), not individual\n"
    "  lines. Keep summaries anchored (file + line, e.g. \"http timeout @ http.py:52\").\n"
    "  Compact proactively — especially when the host warns the context is filling.\n\n"
    "## Finishing a turn\n"
    "- Finish only when the task is solved and you've SEEN the evidence (it lands\n"
    "  the reply AFTER the call) — then reply in plain text, no tool call.\n"
    "- The answer is a short prose summary, NOT a data dump: your print()ed stdout\n"
    "  already shows the user everything, so never paste a raw tool result. Cite\n"
    "  clickable paths (`src/foo.clj:42`), quote at most a few lines. Write like a\n"
    "  concise teammate — outcome first, then where + why; no filler openers, no\n"
    "  emoji unless the user did. Length tracks the change (one-line fix → 1–3\n"
    "  sentences; big change → 1–2 bullets/file). End with real next steps only.\n"
    "- A review request flips the shape: findings first by severity with `path:line`.\n"
    "- Session titles are host-generated — don't invent one.\n"
    "- Example turn — act, READ the result next reply, then act again or ANSWER:\n"
    "    R1 LOCATE: hits, src = await gather(rg(\"request_timeout\"), cat(\"http.py\",{\"range\":[40,70]})); print(hits, src)\n"
    "    R2 reads visible → edit + prove: await struct_patch({\"path\":\"http.py\",\"op\":\"rename\",\"target\":\"reqTimeout\",\"code\":\"request_timeout\"}); print(await repl_eval(\"python\",\"http.request_timeout\"))\n"
    "    R3 eval visible → ANSWER in plain text, no tool call.\n"))

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
  (try (let [raw
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
  (let [global-dir
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
  (let [addendum
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
  []
  (try
    (let [{:keys [found? source path content files]}
          (agents/instructions)

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
        (let [multi?
              (> (count files) 1)

              header
              (str "Project rules from "
                   (if multi?
                     (str (count files)
                          " stacked guidance files — "
                          "user-global first, then each ancestor directory, "
                          "then the workspace root. NEARER (later) files "
                          "override earlier ones on conflict.")
                     (str (agents/origin-label (first files)) " (" (:path (first files)) ")."))
                   " These are PROJECT-OWNED instructions; honor them "
                   "alongside CORE rules. On conflict with CORE engine\n"
                   "contract (CTX shape, DONE pipeline, SANDBOX), CORE wins.")

              body
              (str/join
                "\n\n"
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
  (when-let [exts (some-> (:extensions environment)
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
           (let [info
                 (extension/extension-info ext)

                 registry-id
                 (:registry-id info)]

             (cond-> {:name (:name info)
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
  (let [;; Built-ins first so the core kernel prompt (foundation) leads the
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
  (when (and (string? content) (not (str/blank? content))) {:role "system" :content content}))

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
  (str
    "Nine rules that override any temptation to do more:\n"
    "1. Two reply shapes: to ACT, call a tool — a DIRECT file tool "
    "(cat/rg/find/patch/move/delete/ls), or `python_execution` for transforms "
    "(`print(...)` what you want back). The result comes back next reply. To "
    "FINISH, reply with plain text and NO tool call — that text IS your answer. "
    "The answer is a SHORT summary, not a data dump: tool outputs "
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
    "4. EDITS need FRESH anchors. `patch` uses `lineno:hash` anchors from `cat`, and "
    "the hash goes STALE the moment the file changes — after ANY write/patch your old "
    "anchors are DEAD. So `cat` the exact lines, then `patch` them; cleanest is BOTH "
    "from a FRESH `cat`: take the `lineno:hash` you see and pass it as the edit's "
    "`from_anchor` — `patch([{\"path\": p, \"from_anchor\": \"<lineno:hash>\", \"replace\": R}])` "
    "for one line, or add `\"to_anchor\"` for an inclusive span. "
    "To edit a file you already changed this turn, `cat` it AGAIN first. "
    "`patch` is ATOMIC (one bad anchor rejects the whole batch), so only batch edits "
    "whose anchors all came from the SAME fresh cat. For a whole-file rewrite just "
    "`write` it (no anchors, never stale). Read GENEROUSLY up front so you make all "
    "the edits at once.\n"
    "5. Check `session` and the live runtime before acting; read an error fully "
    "before retrying — and change approach if it fails twice (don't repeat the "
    "same search/edit that already failed).\n"
    "6. If the available tools can't do what was asked, say so plainly in your "
    "prose answer. Do not improvise a fake solution.\n"
    "7. print() what you want to see — there is NO automatic echo. `await rg({...})` "
    "unprinted shows you nothing; write `print(await rg({...}))`, or better, print only "
    "the slice you need. The sandbox is a persistent REPL, so you CAN hold results "
    "in variables across calls/turns and print just the part that matters — keep "
    "prints small so your context stays lean. If you see NO output, YOU forgot to "
    "print() (or printed an empty value) — the sandbox is NOT broken. Add an explicit "
    "print() and continue; NEVER abandon a task claiming the tools or sandbox don't "
    "work, and never answer from guesses when a real call would tell you. Your "
    "stdout goes to MODEL CONTEXT, not the human channel; return/display is rendered as Markdown, so "
    "print well-formed markdown: headings/`-` lists/tables for structure, and fence "
    "code or data dumps in ``` so they stay readable. Thinking and printing in "
    "markdown also keeps your own reasoning legible across turns.\n"
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
  (if-let [nudge (stable-prompt-message (prompt-block "reasoning-via-comments"
                                                      (str reason-via-comments-instruction
                                                           "\n\n"
                                                           weak-model-operating-rules)))]
    (let [[leading-systems rest-msgs] (split-with #(= "system" (:role %)) messages)]
      (vec (concat leading-systems [nudge] rest-msgs)))
    messages))

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
  (let [core-block
        (prompt-block "system-prompt" (build-system-prompt {:system-prompt system-prompt}))

        ;; Non-interactive `:cli` runs drop the candidate approval STOP — no
        ;; human can approve a one-shot run. Stable per session (channel never
        ;; changes), so it doesn't churn the prefix cache.
        cli-block
        (when (= :cli (:channel environment)) (prompt-block "cli-autonomous" cli-autonomous-rules))

        project-block
        (project-instructions-block)

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
