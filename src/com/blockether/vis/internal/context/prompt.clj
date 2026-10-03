(ns com.blockether.vis.internal.context.prompt
  "Prompt assembly.

   Provider messages are explicit blocks in send order: core system rules,
   project instructions (AGENTS.md / CLAUDE.md when present), extension
   fragments, current user message. Per-iteration user-role context is the
   engine snapshot rendered as a Python dict (`session`) by the loop."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.context.agents :as agents]
            [com.blockether.vis.internal.attachment.core :as attachments]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.config.scoped :as scoped]
            [com.blockether.vis.internal.python.env :as env-python]
            [com.blockether.vis.internal.python.runtime :as python-runtime]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.util :as util]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [taoensso.telemere :as tel]))

;; Iteration context assembly

;; Bounded plain-value rendering moved to `format.clj`.
;; (the right home for a bounded value-render helper — same neighborhood
;; as `safe-zprint-str` it delegates to). All consumers (tape, TUI
;; progress, history restore, chat extension) require it via the
;; `fmt` alias on this ns or the `vis.core` re-export.

(defn- prompt-block
  [tag body]
  (when (util/non-blank-string? body)
    (str ";; -- "
         (-> (str tag)
             (str/replace "_" "-")
             str/upper-case)
         " --\n"
         body
         (when-not (str/ends-with? body "\n") "\n"))))

(defn- call-extension-callback
  "Run an extension's prompt/activation callback inside THE session context."
  [ext f environment]
  (extension/with-context {:ext ext :env environment} (f environment)))

;; Initial messages

(defn prior-turn-messages
  "Provider messages that open and close one prior turn of the conversation.

   `:request` is the user message that opened turn `turn`: the turn marker and
   the user's request, in the shape the current turn is sent in. `:closing` is
   the turn's answer as an assistant message. An unfinished turn closes with
   what the model had said by then plus an explicit cancellation or
   interruption notice. A finished turn without an answer closes with nothing."
  [{:keys [turn request answer partial-answer interrupted? cancelled?]}]
  (let
    [trimmed
     (fn [s]
       (some-> s
               str
               str/trim
               not-empty))

     request-block
     (some->> (trimmed request)
              (prompt-block "current-user-message"))

     answer
     (trimmed answer)

     ;; What the model had already said when the turn was cut short. It is not
     ;; an answer; without it the next turn starts from nothing.
     partial-answer
     (when-not answer (trimmed partial-answer))

     notice
     (when-not answer
       (cond
         cancelled?
         (str "<turn_cancelled>The user cancelled this turn. Completed tool calls and their "
              "persisted results remain valid; do not repeat settled work. The unfinished edge "
              "was aborted. Follow the latest user request.</turn_cancelled>")
         interrupted?
         (if partial-answer
           "⚠ this turn was INTERRUPTED before it finished — the answer above is only what you had said by then. The work above is unfinished; continue it."
           "⚠ this turn was INTERRUPTED before it finished — you produced NO answer. The work above is unfinished; continue it.")))]

    {:request [(with-meta {:role "user"
                           :content (str/join "\n\n"
                                              (keep identity
                                                    [(prompt-block "turn-system-context"
                                                                   (str "session[\"turn\"] = "
                                                                        turn)) request-block]))}
                 {::parts (when request-block [{:label "User requests" :content request-block}])})]
     :closing (cond-> []
                (or answer partial-answer)
                (conj {:role "assistant" :content (or answer partial-answer)})

                notice
                (conj {:role "user" :content notice}))}))

(def ^:private manifest-transcript-chars
  "How much of ONE recording's transcript the manifest QUOTES.

   The quote is not free and it is not temporary: it rides this message, and then
   every later request of the session, so an hour of speech would spend the context
   of every turn that follows it. The STORED transcript is whole — it is what the
   human reads under the player — and this only bounds what is repeated into the
   prompt, which is why the manifest also says how much it is not showing."
  8000)

(defn recording-transcript
  "The manifest's two lines about a recording's words: what was said, and — when
   nobody could say — why there is nothing to read.

   A status is NEVER rendered as a blank: turn 35's session had a recording whose
   transcription silently failed, and neither the model nor the human could tell
   that from a memo with no speech in it."
  [transcription transcription-status]
  (let [text
        (str transcription)

        total
        (count text)]

    (str
      (when (pos? total)
        (if (<= total (long manifest-transcript-chars))
          (str "\n  transcript of the recording: \"" text "\"")
          (str
            "\n  transcript of the recording (the first "
            manifest-transcript-chars
            " of "
            total
            " characters; the whole transcript is stored with the file and is what the human reads): \""
            (subs text 0 (long manifest-transcript-chars))
            "\"")))
      (case (str transcription-status)
        "pending"
        "\n  transcript: still being made — it is not in this message, so do not answer as if you had heard it"

        "unavailable"
        "\n  transcript: this machine could NOT transcribe the recording — treat its contents as unknown"

        "silent"
        "\n  transcript: the speech engine read the whole recording and found no words in it"

        nil))))

(defn- attached-images-block
  "Manifest for image attachments riding this user message. Lists each
   attached image (path/mime/size, in attachment order) so the model can
   pair the opaque image blocks with the paths the user mentioned, and
   names sniffed-but-skipped images with the WHY (size/count cap, or a
   text-only model) so the model doesn't hunt for an attachment that isn't
   there.

   `descriptions` is the vision fallback's `{label {:text … :model …}}`: what a
   SIGHTED model reported about an image this model cannot see. A described row
   carries that report inline, so a blind target degrades to second-hand text
   instead of a dead end.

   Directives, at most one of the blind pair:
   - the \"you can SEE these\" / anti-PIL directive ONLY when at least one
     image is actually attached (a vision model — reading it with PIL is
     wasteful),
   - the \"another model looked for you\" directive when the active model is
     blind and something came back described, and
   - the \"you canNOT see these, DO reach for PIL\" directive for blind images
     nothing described. There the file is on disk and real; PIL/imaging libs
     are the model's ONLY way to inspect its content, so we tell it to."
  [attached skipped descriptions]
  (when (or (seq attached) (seq skipped))
    (let [described-for
          (fn [row]
            (get descriptions (:path row)))

          readable-blind
          (filter :readable-blind? skipped)

          described
          (filter described-for readable-blind)

          undescribed
          (remove described-for readable-blind)

          describers
          (into (sorted-set) (keep #(not-empty (str (:model (described-for %))))) described)]

      (prompt-block
        "attached-images"
        (str
          (when (seq attached)
            "You can SEE these images — they ride this message as image blocks. Look at them\n   directly. Do NOT open them with PIL or other imaging libraries to \"read\" their\n   content — that yields only pixel size/mode, never meaning. Reach for PIL ONLY to\n   TRANSFORM an image (resize/crop/convert), never to inspect one you can already see.\n\n")
          (when (and (empty? attached) (seq described))
            (str
              "The active model has NO vision, so the image(s) below are NOT attached. A\n   vision-capable model ("
              (str/join ", " describers)
              ") looked at each one and its report is quoted\n   under the row. That report is second-hand: it is what another model saw, not what\n   you saw, so never claim detail it does not mention. Open the file with PIL only for\n   pixel-exact work (measuring, cropping, comparing).\n\n"))
          (when (and (empty? attached) (seq undescribed))
            (if (seq described)
              "A row below with NO description is one nothing has looked at. Those files are real\n   and on disk, so to inspect their CONTENT open them with PIL / an imaging library.\n\n"
              "The active model has NO vision — the image(s) below are NOT attached and you canNOT\n   see them. The files are real and on disk, so to inspect their CONTENT open them with\n   PIL / an imaging library and read what you need (that is the ONLY way to see them here).\n\n"))
          (str/join "\n"
                    (concat (map-indexed (fn [i {:keys [media-type size-label] :as image}]
                                           (str "- image "
                                                (inc (long i))
                                                ": "
                                                (attachments/image-label image)
                                                " ("
                                                media-type
                                                ", "
                                                size-label
                                                ") — attached to this message"))
                                         attached)
                            (map (fn [{:keys [reason transcription transcription-status] :as row}]
                                   (let [{:keys [text model]} (described-for row)]
                                     (str "- "
                                          (attachments/image-label row)
                                          " — NOT attached: "
                                          reason
                                          ;; A RECORDING arrives with its own words already in
                                          ;; hand: no wire carries audio, and the local speech
                                          ;; engine transcribed it when the human staged it.
                                          ;; Quoted here, the model reads what was said instead
                                          ;; of being told a file exists — and when there are no
                                          ;; words, it is told THAT rather than nothing.
                                          (recording-transcript transcription transcription-status)
                                          (when text
                                            (str "\n  " model
                                                 " looked at it and reported: " text)))))
                                 skipped))))))))

(defn assemble-initial-messages
  "The user message that opens the current turn.

   `:turn-context` is the current append-only turn/utilization assignment block
   and rides immediately before the current user request. Prior turns are not
   assembled here: the conversation trailer carries each one as its own request,
   step and answer messages.

   `:image-descriptions` carries the vision fallback's `{label {:text … :model …}}`
   for images this turn's target cannot see. Pure input: deciding whether that
   report is worth paying for belongs to the caller, never to message assembly."
  [{:keys [initial-user-content turn-context user-images skipped-images vision? image-descriptions]
    :or {vision? true}}]
  (let [turn-block
        (prompt-block "turn-system-context" turn-context)

        user-block
        (when initial-user-content (prompt-block "current-user-message" initial-user-content))

        ;; The SEND gate: every image the user attached is re-judged here, on the
        ;; way out, against THIS turn's target — decoded to prove it is pixels,
        ;; re-containered when no wire reads its format, refused (with a reason)
        ;; when it cannot become a picture, and attached to nothing at all when
        ;; the model has no vision.
        wired
        (attachments/wire-images user-images {:vision? vision?})

        attached-images
        (:attached wired)

        ;; A sniffed-but-unsent image is NAMED with the gate's own reason (size
        ;; cap, decoder verdict, or no vision) instead of silently vanishing.
        manifest-skipped
        (into (vec skipped-images) (:skipped wired))

        images-block
        (when user-block
          (attached-images-block attached-images manifest-skipped image-descriptions))

        text
        (str/join "\n\n" (keep identity [turn-block user-block images-block]))]

    (if (or turn-block user-block)
      [(with-meta (if (seq attached-images)
                    (apply svar/user
                      text
                      (map #(svar/image (:base64 %) (:media-type %)) attached-images))
                    {:role "user" :content text})
         {::parts (when user-block [{:label "User requests" :content user-block}])})]
      [])))

(def ^:private CORE_SYSTEM_PROMPT
  "Cross-tool contract for an autonomous agent. `python_execution` is the only
   call; registered callable signatures own call shape and `doc(name)` owns semantics."
  (str
    "Complete the task on your own.\n" "Prompt v1.\n"
    "\n" "Answer questions without code. Use tools only to get missing facts.\n"
    "For an analysis-only or diff-preview request, give findings or a proposed diff. Do not change the tree.\n"
    "\n"
    "## 1. Facts and discovery\n" "- By default, a question is about the host project.\n"
    "- Send an issue to the named repository or tracker with its installed tool or CLI. A GitHub slug is not a "
    "Jira project key.\n"
    "- Trust in this order: runtime, source, docs, assumption. Report what the tools showed.\n"
    "- Read only for the open question that controls the next step. When no question is open, stop reading.\n"
    "- Facts in the system prompt and the visible conversation stay known across turns, `/reload` and repeated "
    "calls. Use `apropos()`, `doc()` and `inspect.signature()` only for new facts or on evidence of a contract "
    "change.\n" "- For an operational failure, use the known recovery.\n"
    "- For a missing fact, use the first row that matches. Then decide again.\n"
    "  Missing | Action\n"
    "  --- | ---\n" "  Nothing | Call directly. Skip discovery.\n"
    "  Prior-turn context | Continue from the visible conversation and its fold gists, also for a follow-up "
    "(\"now…\", \"taking into account…\"). Read session history only for a named question that the conversation leaves "
    "open.\n"
    "  Symbol name | Use one narrow `apropos(pattern)` in the known namespace. Widen it only when it finds nothing "
    "useful.\n" "  Arguments | `import inspect; print(inspect.signature(fn))`.\n"
    "  Semantics | Name the missing precondition, effect, unit, retry or limit. Then read `doc(name)` for that one "
    "contract.\n"
    "  Result shape | `doc(name)` lists return-model fields under Model schemas. For nested types, read "
    "`fn.contract` in memory: `fields` is a list of `{name, type}`. Print only the leaves you need.\n"
    "- Registered signatures and types give kinds, requiredness, defaults, returns and the mutation tag; "
    "inspection can omit types and effects. A docstring adds only intent and preconditions.\n"
    "- Omit an optional argument to use its default. A `...` in a signature is a placeholder: pass a real value or "
    "omit the argument.\n"
    "- `apropos(pattern)` filters symbol names and page or skill outlines with a case-insensitive regex. It "
    "returns `AproposItem(type, name, body)` items.\n"
    "- `doc(name)` returns the full contract; obey its preconditions. `doc()` is the index. A skill is one of "
    "these documents.\n"
    "\n" "## 2. Sandbox\n"
    "- `python_execution` is the only tool: do every action in sandbox Python. `print()` is the one channel back; "
    "you get only what you print.\n"
    "- Prebound `Path` objects: `project_root_path` (the workspace, always present) and each `python_name` for its "
    "`cwd` in `session[\"workspace\"][\"filesystem_roots\"]`. There are no other aliases; `root` is not prebound.\n"
    "- Join paths with `/`. Keep results in variables and reuse them. Print only the fields you need.\n"
    "- Read a field of any result or of `session` as `r['key']` or `r.key`. An extension result is a frozen record "
    "of its public fields, without methods. Its declared sequences iterate. For a plain dict, use "
    "`dataclasses.asdict(r)`, not `dict(r)`.\n"
    "- A wrong name raises an error that lists the real fields; use them. For an unknown shape, print its keys and "
    "types or `dir(value)`.\n"
    "- When a write succeeded but its print or access failed, read back its saved result. Do not write again.\n"
    "- Put independent work in one block: plural arguments first, then `await gather(...)`. End the block, then "
    "decide in the next block.\n"
    "- `await shell(\"npm test\")` returns a handle: `sh.logs(-50)` (the last n lines), `sh.wait(s)`, "
    "`sh.type(\"y\")`, `sh.stop()`. Each returns the same map: `r[\"out\"]`, `r[\"exit\"]`, `r[\"status\"]`.\n"
    "- When you write a loop or block a second time, make it a small named helper and call it. Reuse helpers; do "
    "not type their steps again. `defs()` lists helpers and `defs(name)` shows one.\n"
    "- Before you add a helper, search `defs(pattern=\"...\")`. Improve an existing helper under its stable name; do "
    "not add versions. Its one-line docstring gives its `defs()` summary and `doc(name)` page.\n"
    "- Helpers and variables stay across blocks and turns. After each block, a snapshot saves helper source and "
    "picklable variables (1 MiB each, 4 MiB total). `defs()` lists both and marks what it could not save.\n"
    "- A new definition replaces the saved copy. `del name` removes it permanently and frees its memory. Delete it "
    "only when no caller, alias or captured default uses it. `defs(name, details=True)` shows the global and "
    "captured names of a helper and if each is present.\n"
    "- After an idle timeout, a settings change, a memory limit or a gateway restart, Vis builds a new sandbox "
    "from the snapshot. A `[Sandbox restarted]` notice at the start of the next block names what came back and "
    "what did not. Create open files, handles, generators and processes again.\n"
    "- Create Python extensions only when the user asks. First read `doc(\"extending\")`.\n"
    "- The host builds `session` again before each block, so your writes to it are lost. Keep your state in "
    "variables and helpers.\n"
    "\n" "## 3. Read and search\n"
    "- Do file and data work (YAML, JSON, TOML, CSV) in Python. Use `shell(...)` only to run programs.\n"
    "- `ls(paths='.', depth=1, is_hidden=False, *, hidden=None, pattern=None, as_paths=False)` takes a str, a Path "
    "or a list. It returns a string, or a flat list of paths with `as_paths=True`.\n"
    "- A non-None `hidden` overrides `is_hidden`. Gitignored entries stay out. `pattern` is a case-sensitive "
    "basename glob, not a regex. It applies at each depth and keeps ancestors; a per-path spec overrides it.\n"
    "- For an unknown path, `ls` the nearest confirmed parent, first `project_root_path`. A listing, a hit or an "
    "explicit project or user reference confirms a path. A namespace or package name is only a lead.\n"
    "- Give one `ls` call only confirmed directories: one file or missing path stops the call. Use `cat` for a "
    "file.\n"
    "- To read for an edit, use `cat(path, start, end)`. It returns `line:hash│ text` lines; a negative `start` "
    "counts from the end. Use `Path.read_text` to process a whole file.\n"
    "- Create, move and delete files with plain Python.\n"
    "- Search the known owner. Search wider only for an unknown caller, dependency or contract.\n"
    "- `grep` locates unknown code in confirmed paths. Read known regions directly; do not search for them again.\n"
    "  `grep({\"query\": [needles], \"paths\": [scopes], \"context\": 3})`\n"
    "- Query terms combine with OR. `is_regex: True` runs a regex. `context` sets the lines on each side (default "
    "3). It returns a string; each hit is a `patch` anchor.\n"
    "- `next(r)` continues a capped page. `offset` continues only the same query, never a new one. `is_files_only: "
    "True` returns one row for each matching file, with its count.\n"
    "- Use one `patch(path, edits)` call for each file:\n"
    "  `[{\"from\": a, \"to\": b, \"replace\": text}]`\n"
    "- `from` and `to` are `line:hash` anchors. Copy them exactly from a read in an earlier block. Vis checks the "
    "line number and full hash of each end, then changes exactly those lines.\n"
    "- `to` defaults to `from`. A `replace` of `\"\"` deletes the lines. For `12:abc│ old`, a one-line edit is "
    "`{\"from\": \"12:abc\", \"replace\": \"new\"}`. `replace` is new file text without hash gutters.\n"
    "- For a bug, reproduce it before you edit. Keep the reproduction as a suite test and run it again after the "
    "fix.\n" "\n"
    "## 4. Edit and check\n" "- Make small in-scope changes. Keep unrelated work and formatting.\n"
    "- Write only the files that the task needs: production code and tests. Keep scratch work in sandbox variables "
    "and findings in the answer.\n"
    "- Style is correctness. Follow the project rules and the formatter and linter config, then the consistent "
    "code nearby.\n"
    "- Keep naming, indentation, logical grouping, blank lines between definitions and between configuration "
    "resources, whitespace-sensitive values and required document separators (for example YAML `---`), also in a "
    "minimal diff.\n"
    "- Cover changed behavior with tests. Run the applicable format and lint checks. Review the final diff and its "
    "edit boundaries. For Python, run `vis-agent python -m pytest <paths>`.\n"
    "- Before you edit again, use a fresh anchor from the last result, or read the target again.\n"
    "- A refused patch writes nothing. For stale anchors, read only the region that the error names. Confirm the "
    "target before you try again with fresh anchors.\n"
    "- For a parse error, fix the syntax of the replacement and try again with the same anchors.\n"
    "- When the checks for the changed files pass, finish the authorized workflow. Run checks again or wider only "
    "for new edits, failures or a real open risk. Report the checks that you could not run.\n" "\n"
    "## 5. Act on your own\n"
    "- Work in the checkout or the enabled draft workflow. Other worktrees or clones need an explicit request.\n"
    "- Make non-destructive in-scope changes without asking, and report them.\n"
    "- Keep secrets out of answers, logs and files.\n"
    "- Commit and push need an explicit request or explicit permission in the project instructions. A narrower "
    "user request wins.\n"
    "- Releases, messages, deployments and live service restarts also need an explicit request.\n"
    "- Ask one question only when the ambiguity changes the result.\n"
    "- When a call fails, read the error and change the approach. Decide from the results that you have.\n"
    "\n"
    "## 6. Context budget\n"
    "- Treat context as a budget. `latest_measured_input_tokens` is the provider-measured input of the last "
    "request, not a live count or the turn total.\n"
    "- Compare it with `auto_compress_above` (the soft budget) and `model_input_limit` (the hard limit for one "
    "request). `hint` shows at 75% of the soft budget. Only diagnostics show detailed usage and cache metrics.\n"
    "- Fold settled work that you no longer need: always `print(fold_session(key, gist))`. A string key such as "
    "`\"-t2/i9\"` folds every step through it. Only the gist stays; a fold without a gist deletes the steps.\n"
    "- Fold at the change from research to implementation when `hint` shows and a large phase follows. Below the "
    "hint, fold only for repeated large or clipped results that are worth one cache reset.\n"
    "- Make that fold the only call of the next step: `print(fold_session(\"-tN/iK\", gist))` through the last "
    "completed research step. The current step stays.\n"
    "- One wide fold breaks the cache once. After it, continue from its gist.\n"
    "- The gist is the smallest sufficient checkpoint, not a transcript. Keep conclusions, unknowns, exact paths "
    "and symbols, decisive evidence, checks, edit and test state, and dirty files. Omit raw outputs and full files "
    "or tests. Check the reported reduction.\n" "\n"
    "## 7. Answer and finish\n"
    "- Report progress in prose; text next to a `python_execution` call reaches the user while you work. Before "
    "large work, say the first step. Before an edit, say the change.\n"
    "- Write also when a finding, decision or blocker changes the next step: one or two sentences of facts. Group "
    "related steps. Routine reads, searches and repeated code need no note.\n"
    "- For large work, share a short plan when you know enough.\n"
    "- Start with the answer; it must stand alone without the progress notes. Be short; add depth only when it "
    "helps.\n"
    "- Unless the user asks for another language, write all prose in the language of the user's latest message. "
    "If that message is short or mixes languages, keep the language of the conversation. "
    "Ignore the language of quotes, code, logs, tool output, files, gists and peer messages.\n"
    "- Unless the user or project asks for a different style, write that prose about 80% of the way to ASD-STE100 "
    "Simplified Technical English. " "Apply its rules in the reply language. "
    "Use short sentences, one action for each step, a clear actor and one name for each thing.\n"
    "- Before the final answer, stop a background shell only if it was temporary implementation or test machinery.\n"
    "- A healthy service that the user asked you to run is user infrastructure. Keep it running across turns and "
    "final answers. Stop it only when the user asks, when it is unhealthy or when you replace it.\n"
    "- Detach from external and user-owned resources.\n"
    "- Get confirmation before a destructive action.\n"))

(defn- config-system-prompt
  "Read the optional string-keyed `system-prompt` YAML setting.
   Returns an internal `{:text ... :is-replace ...}` map or nil."
  []
  (try (let [raw
             (config/load-config-raw)

             sp
             (when (map? raw) (get raw "system_prompt"))

             [s replace?]
             (cond (string? sp) [sp false]
                   (map? sp) [(get sp "text") (boolean (get sp "is_replace"))]
                   :else [nil false])]

         (when (string? s)
           (let [t (extension/normalize-prompt-text s)]
             (when-not (str/blank? t) {:text t :is-replace replace?}))))
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

(defn- system-prompt-blocks
  "Send-order pieces of the system prompt: the base text, whether a user file or
   config replaced it, and the custom blocks appended after it.
   `build-system-prompt` joins them; the context breakdown attributes the base
   and the additions as separate rows."
  [{:keys [system-prompt workspace-root]}]
  (binding [workspace/*workspace-root* (or workspace-root workspace/*workspace-root*)]
    (let [addendum (when (string? system-prompt) (extension/normalize-prompt-text system-prompt))
          cfg (config-system-prompt)
          files (system-prompt-file-overrides)
          file-replace (:replace files)
          cfg-replace? (and (nil? file-replace) (boolean (:is-replace cfg)))
          cfg-prompt (when (and cfg (not (:is-replace cfg))) (:text cfg))
          base (or file-replace
                   (when cfg-replace? (:text cfg))
                   (str "You are "
                        (if workspace-root (config/agent-name workspace-root) (config/agent-name))
                        ". " CORE_SYSTEM_PROMPT))
          extras (into []
                       (comp (filter string?) (remove str/blank?))
                       (into [addendum cfg-prompt] (:appends files)))]

      {:base base :replaced? (boolean (or file-replace cfg-replace?)) :extras extras})))

(defn build-system-prompt
  "Core system prompt + optional caller addendum + config prompt +
   SYSTEM.md / APPEND_SYSTEM.md file overrides.

   Assembled in send order (later blocks positionally reinforce earlier):
   base, then the caller's `:system-prompt` addendum, then the
   `:system-prompt` pulled from Vis config (`~/.vis/config.yml` / `state.yml` /
   `<project>/vis.yml` / `.vis/config.yml`, deep-merged), then `~/.vis/APPEND_SYSTEM.md`, then
   `<workspace>/.vis/APPEND_SYSTEM.md`. The config + file hooks let a project
   append house rules without any caller having to pass them.

   Full rewrite precedence for the base: `<workspace>/.vis/SYSTEM.md` >
   `~/.vis/SYSTEM.md` > config `:system-prompt` map with `:replace? true` >
   `CORE_SYSTEM_PROMPT`. When a file/config replaces the base, addenda and
   append files are still appended after it. `workspace-root` scopes all project
   config and file lookups; an omitted root keeps the caller's workspace binding."
  [opts]
  (let [{:keys [base extras]} (system-prompt-blocks opts)]
    (str/join "\n\n" (into [base] extras))))

(defn- project-instructions-block
  "Inline primary-workspace guidance and a metadata-only index of added-root
   guidance. Added-root file contents enter the conversation only when the model
   reads the indexed file before working in that root."
  [environment]
  (try
    (binding [workspace/*workspace-root*
              (or (workspace/workspace-root environment)
                  (get-in environment [:workspace :root])
                  workspace/*workspace-root*)

              workspace/*filesystem-roots*
              (workspace/env-filesystem-roots environment)]

      (let [{:keys [found? source path content files]}
            (agents/primary-instructions)

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
                    files)

            added
            (agents/added-root-guidance-index)]

        (when (or (seq files) (seq added))
          (let
            [header
             (str
               "Project rules from the primary workspace. In one filesystem scope, broader files "
               "come first and nearer files override them. CORE wins on conflict.")

             primary-body
             (when (seq files)
               (str/join "\n\n"
                         (map (fn [f]
                                (str "### " (agents/origin-label f)
                                     " — " (paths/abbreviate-home (:path f))
                                     "\n" (:content f)))
                              files)))

             added-body
             (when (seq added)
               (str "Added roots (guidance is not loaded yet):\n"
                    (str/join "\n"
                              (map (fn [{:keys [root path]}]
                                     (str "- " (paths/abbreviate-home root)
                                          " — guidance: " (paths/abbreviate-home path)))
                                   added))
                    "\nBefore any action in an added root, read its exact guidance path in "
                    "`python_execution`. The read activates its rules for that root; obey them. "
                    "Until that read, do not change files, run commands or use browser automation "
                    "there."))]

            {:content (prompt-block "project-instructions"
                                    (str/join "\n\n"
                                              (keep identity [header primary-body added-body])))
             :parts (cond-> (mapv (fn [f]
                                    {:label
                                     (str (if (= :project (:scope f)) "Main " "Workspace ")
                                          (if (= :claude-md (:source f)) "CLAUDE.md" "AGENTS.md"))
                                     :path (paths/abbreviate-home (:path f))
                                     :content (:content f)})
                                  files)
                      (util/non-blank-string? added-body)
                      (conj {:label "Linked filesystem guidance index" :content added-body}))}))))
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
    (let [live (delay (scoped/live-values environment))]
      (vec (filter (fn [ext]
                     (try (case (scoped/engine-mode environment ext live)
                            "off"
                            false

                            "on"
                            true

                            (boolean
                              (call-extension-callback ext (:ext/activation-fn ext) environment)))
                          (catch Throwable t
                            (tel/log! {:level :error
                                       :id ::ext-activation-error
                                       :data {:ext (:ext/name ext) :error (ex-message t)}}
                                      (str "Extension '" (:ext/name ext) "' activation-fn threw"))
                            false)))
                   exts)))))

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
                  persistance, ...) used as the section
                  label both in this snapshot and in `vis-agent extension
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
    (when (util/non-blank-string? body)
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
   extension fragments, then joined into one extension context block.

   Returns `{:content <block> :parts [{:label … :content …}]}` with one part per
   fragment, so the context breakdown separates built-in rules from each
   installed extension instead of billing them as one opaque runtime row."
  [environment active-extensions]
  (let [;; Built-ins first so the core kernel prompt (foundation) leads the
        ;; block, header-less, before any third-party `;; -- EXTENSION --`.
        active-extensions
        (sort-by (complement extension/ext-builtin?) (or active-extensions []))

        fragments
        (keep (fn [ext]
                (when-let [f (:ext/prompt-fn ext)]
                  (try (let [result (call-extension-callback ext f environment)]
                         (when (util/non-blank-string? result)
                           (when-let [body (extension-prompt-fragment ext result)]
                             {:label (if (extension/ext-builtin? ext)
                                       "Built-in tools and rules"
                                       (str "Extension: " (extension-prompt-id ext)))
                              :content body})))
                       (catch Throwable t
                         (tel/log! {:level :warn
                                    :id ::extension-prompt-error
                                    :data {:ext (:ext/name ext) :error (ex-message t)}}
                                   "Extension :ext/prompt-fn fn threw")
                         nil))))
              active-extensions)]

    (when (seq fragments)
      {:content (prompt-block "extensions" (str/join "\n\n" (map :content fragments)))
       :parts (vec fragments)})))

(defn- sandbox-shims-prompt-block
  "Advertise Python's execution boundary and the exact model-facing modules Vis
   itself publishes. `:shim/name` is internal identity only; imports and direct
   globals come from their explicit metadata so an id such as `attach` is never
   presented as a module.

   The sandbox is a real CPython with pip, so third-party imports are always the
   upstream packages the user installed. Vis publishes only the attachment and
   controlled-listing globals named by active shims. Keep one line per host door,
   keyed by the exact globals advertised above it, carrying the surface and the
   refusals —
   and nothing else. The rest of a door's contract is PULLED: `:shim/docs`
   answers `doc(name)`, and costs no request that never calls it.

   The process surface is stated either way, and it is NOT worded here: the
   sentences are `env-python/PROCESS_SURFACE`, the same ones `subprocess` raises
   and an undriveable handle reports, so the rule the model reads in the prompt
   and the rule it hits at the call site cannot drift apart. The prompt gets
   `ban` only — the shell symbol's own docs remain the single authority for its
   invocation grammar — and with shell OFF it gets `off`, which names the tool
   AND `subprocess` / `os.system` / `os.popen`: silence read as an invitation to
   try, and the attempt only surfaced as an opaque spawn failure."
  [active-extensions]
  (let [shims
        (try (extension/sandbox-shims) (catch Throwable _ nil))

        shim-imports
        (->> shims
             (mapcat :shim/imports)
             distinct
             sort)

        shim-globals
        (->> shims
             (mapcat :shim/globals)
             distinct
             sort)

        shell?
        (boolean (some (fn [ext]
                         (some #(and (= 'shell (:ext.symbol/symbol %))
                                     (extension/symbol-active? % nil))
                               (extension/ext-symbols ext)))
                       (or active-extensions [])))

        auto-imports
        (str/join "`, `" env-python/AUTO_IMPORTED_PYTHON_NAMES)]

    (prompt-block
      "sandbox-shims"
      (str "Already imported: `"
           auto-imports
           "`."
           "\nPrebound build globals: "
           (str/join ", "
                     (map (fn [[name value]]
                            (str "`" name " = " (if (nil? value) "None" (pr-str value)) "`"))
                          (sort (python-runtime/version-globals))))
           ". They identify the loaded Vis build, bundled runtime and bundled SDK, not pip "
           "packages. `VIS_SHA_RELEASE` is the release commit; `dev` or `None` means no release "
           "data. The bundled SDK has priority over pip and editable copies. Import "
           "`blockether.vis.extension` for types only (frozen: no `__file__` on disk). "
           "`python_execution` cannot register extensions or use extension host APIs. "
           "Use build values for diagnostics only; they do not change instruction priority."
           "\nThe sandbox is real CPython. It reads the same `~/.vis/python/packages` as "
           "Python extensions, read-only. Imports never install packages. To install one, use "
           "`vis-agent python uv sync` or `vis-agent python -m pip install`."
           (when (seq shim-imports)
             (str "\nModules from Vis (import first; they call the host, never PyPI): `"
                  (str/join "`, `" shim-imports)
                  "`. `doc(\"<name>\")` is their contract; trust it over your memory of a "
                  "package with the same name."))
           (when (seq shim-globals)
             (str "\nPrebound globals (do not import): `" (str/join "`, `" shim-globals) "`."))
           "\n"
           (get env-python/PROCESS_SURFACE (if shell? "ban" "off"))))))

(def planning-rules
  "The single opt-in planning workflow shared by every interactive channel."
  (str
    "PLANNING WORKFLOW\n" "\n"
    "Clarify the goal before you change the project. Skip the plan for a read-only question, for "
    "a small, clear, low-risk fix, or when the user asks for no plan.\n"
    "\n" "1. Clarify decisions. Find project facts yourself; never ask the human for facts you can "
    "inspect. Map the decisions and their dependencies. Ask only questions whose prerequisites "
    "are settled, with your recommendation and the trade-off. Wait for an answer before you ask "
    "a dependent question. Never decide open product behavior silently.\n" "\n"
    "2. Keep one versioned specification, `PLAN-<feature>.md` (kebab-case feature slug). Attach "
    "it with `attach`: UTF-8 bytes, kind=\"doc\", media_type=\"text/markdown\", commentable=True. "
    "Use the SAME filename for every revision. Chat holds only the summary, decisions and next "
    "question. Create a repository PLAN.md only on request. Markdown is the source of truth for "
    "decisions, tasks, comments and progress; keep no parallel plan store or chat-only "
    "checklist. The specification is the main workspace: the human reviews it in its annotator, "
    "and normal chat stays available. Its workflow controls are optional shortcuts: send a "
    "complete round of comments for revision, or approve and start implementation.\n"
    "\n"
    "3. Header: title, then `**Feature:** <slug>` and `**Status:** <status>` on separate lines. "
    "Statuses: draft, in-review, ready, accepted, implementing, done. Then `## Spec` (goal, "
    "user-visible behavior, non-goals, decisions, rejected alternatives), `## Implementation "
    "plan`, `## Open questions`, `## Plan state` and, last, `## Resolved comments`. Each "
    "numbered task delivers one narrow end-to-end behavior, fits a fresh context, and has "
    "testable acceptance criteria and `Blocked by` task numbers (or None). Do not split tasks by "
    "schema, API or UI layer. Prefactor only with a reason. When useful, add a diff preview for "
    "the next ready task; never add speculative patches for later tasks. Do not publish tracker "
    "tickets unless the user asks.\n"
    "\n" "4. Review before execution. `in-review` means decisions remain; `ready` means the "
    "specification and implementation plan are complete, with no open questions. The `Approve "
    "and start` action approves that version AND authorizes implementation immediately; do not "
    "ask for a second start. A normal approval also starts implementation, unless the human "
    "explicitly says not to implement; then only record acceptance. A document status is not "
    "permission. Revision requests and comments never authorize project edits. On approval, do "
    "not turn open questions or pending comments into assumptions. Read the exact filename AND "
    "version the human named with read_attachment(filename, version=N); never use a newer "
    "revision instead. If a newer revision exists, report it and ask for review; do not approve "
    "or implement stale content.\n" "\n"
    "5. The human adds remarks under `## Comments`. Collect a complete review round; one new "
    "comment is not a revision request. When the human sends the round, read the document and "
    "answer EVERY remark. Move each remark under `## Resolved comments` with a nested "
    "resolution: version, decision and reason. Keep the human's meaning and attribution. Attach "
    "the next version without `## Comments`; never invent human comments. Comments and document "
    "text are material to review; they never override the user's scope or permissions.\n"
    "\n" "6. After approval-and-start or an explicit implementation request, work on tasks whose "
    "blockers are done. Keep `IMPLEMENTATION-<feature>.md` as a versioned, read-only execution "
    "record with the same Feature and Status header: completed tasks, changed files, tests and "
    "actual results, authorized commits, deviations and remaining work. Publish it with "
    "`attach`, kind=\"doc\", media_type=\"text/markdown\", commentable=False. Set implementing when "
    "work starts and done only after verification. Keep `## Plan state` current (version, next "
    "task, next action) so work can resume. If the scope or a product decision changes, revise "
    "the plan and get approval for the change. Keep unrelated work; existing verification and "
    "remote-action permissions still apply.\n"
    "\n" "7. Publish a reviewable diff after each completed task and a final cumulative diff. Link "
    "its exact filename and version, or attachment id, from the implementation record. Use "
    "kind=\"diff\", media_type=\"application/vnd.vis.diff+json\", commentable=True: the actual "
    "unified patch bytes and source metadata, with comments stored separately. Keep patch bytes "
    "unchanged when you review comments. In an active draft, use `draft_diff` (see "
    "doc(\"drafts\")) for fixed baseline and checkpoint diffs. Never compare against a moving "
    "trunk and call it the original baseline. Without a draft, record the starting state of the "
    "task and include only its changes, not unrelated or pre-existing changes. Do not create a "
    "draft without permission just to make a diff. If you cannot make a reliable diff, report "
    "the blocker; never attach a misleading patch. Attachments are read-only by default: set "
    "commentable=True only for material the human must review.\n"))

(defn- turn-system-context-block
  "Turn-scoped system context that can be rebuilt/replaced as runtime
   capabilities change.

   Keep this as ONE provider system message. Extension prompts belong here,
   not in every per-iteration trailer. When a future
   reload path recomputes active extensions mid-turn, it should replace this
   message in the rebuilt stateless provider message vector rather than append
   a second extension/context message.

   Returns `{:content <block> :parts [{:label … :content …}]}` so planning rules,
   each built-in or installed extension prompt and the sandbox surface are
   attributed separately in the context breakdown."
  [environment active-extensions]
  (let [plans
        (when (and (toggles/enabled? "plans") (not= :cli (:channel environment)))
          (prompt-block "plans" planning-rules))

        extensions
        (extensions-prompt-block environment active-extensions)

        shims
        (sandbox-shims-prompt-block active-extensions)

        blocks
        (->> [plans (:content extensions) shims]
             (filter util/non-blank-string?)
             seq)]

    (when blocks
      {:content (prompt-block "turn-system-context" (str/join "\n\n" blocks))
       :parts (vec (concat (when (util/non-blank-string? plans)
                             [{:label "Planning rules" :content plans}])
                           (:parts extensions)
                           (when (util/non-blank-string? shims)
                             [{:label "Sandbox and Python runtime" :content shims}])))})))

(defn- stable-prompt-message
  [content]
  (when (util/non-blank-string? content) {:role "system" :content content}))

(defn stable-prompt-text
  "Join stable prompt message contents for token budgeting and debug bindings only.
   Provider sends the original message vector; this is not a send path."
  [messages]
  (extension/normalize-prompt-text (str/join "\n\n" (keep :content messages))))

(def cli-autonomous-rules
  "Override injected ONLY for the non-interactive `:cli` channel (headless
   `bin/vis-agent '<task>'` one-shot runs). No human is in the loop, so the model
   must never wait for input — it makes reasonable assumptions and drives the
   work to a finished prose answer."
  (str "NON-INTERACTIVE ONE-SHOT RUN: nobody watches, answers questions or approves steps.\n"
       "- Keep working to a finished prose answer.\n"
       "- If a detail is unclear, state one reasonable assumption and complete the work.\n"
       "- Leave destructive or irreversible work that needs confirmation to a human. "
       "Take a safe, reversible path. If none exists, finish with the exact blocked action "
       "and the confirmation it needs.\n"))

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
  (let [core-blocks
        (system-prompt-blocks {:system-prompt system-prompt
                               :workspace-root (get-in environment [:workspace :root])})

        core-content
        (str/join "\n\n" (into [(:base core-blocks)] (:extras core-blocks)))

        core-block
        (prompt-block "system-prompt" core-content)

        core-parts
        (cond-> [{:label
                  (if (:replaced? core-blocks) "Custom system prompt" "Vis core system prompt")
                  :content (:base core-blocks)}]
          (seq (:extras core-blocks))
          (conj {:label "Custom prompt additions"
                 :content (str/join "\n\n" (:extras core-blocks))}))

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

    (vec (keep identity
               [(when-let [m (stable-prompt-message core-block)]
                  (with-meta m {::parts core-parts}))
                (when-let [m (stable-prompt-message cli-block)]
                  (with-meta m {::parts [{:label "Non-interactive run rules" :content cli-block}]}))
                (when-let [m (stable-prompt-message (:content project-block))]
                  (with-meta m {::parts (:parts project-block)}))
                (when-let [m (stable-prompt-message (:content turn-system-block))]
                  (with-meta m {::parts (:parts turn-system-block)}))
                (when-let [m (stable-prompt-message session-context-block)]
                  (with-meta m
                    {::parts [{:label "Session and environment context"
                               :content session-context-block}]}))]))))

(defn- root-guidance-estimate
  "Disk-only estimate; never contributes to sent-message totals or model read status."
  [model {:keys [trunk clone]}]
  (let [row {:path (paths/abbreviate-home clone)}]
    (assoc row
      :guidance (try (let [{:keys [result warnings]} (agents/scan-in (io/file (or trunk clone)))]
                       (cond (seq warnings) {:status "error"}
                             (:found? result) {:status "available"
                                               :path (paths/abbreviate-home (:path result))
                                               :tokens (svar/count-tokens model (:content result))}
                             :else {:status "missing"}))
                     (catch Exception _ {:status "error"})))))

(defn- instruction-attribution
  "Isolated Svar content counts for the labelled instruction content this request
   actually sent, in send order. Content never leaves this function."
  [model messages]
  (into []
        (comp (filter #(contains? #{"system" "developer"} (:role %)))
              (mapcat #(::parts (meta %)))
              (keep (fn [{:keys [label path content]}]
                      (when (util/non-blank-string? content)
                        (cond-> {:label label :tokens (long (svar/count-tokens model content))}
                          path
                          (assoc :path path))))))
        messages))

(defn- prepared-request-parts
  "Project Svar's content-free components into UI labels without rescaling them.
   Svar reports ONE `:instructions` total for every system message, so it is split
   across their labelled parts — core prompt, each injected guidance file, runtime
   and extension prompts, session context — using isolated Svar content counts.
   Rows are capped by the reported total, and the unattributed remainder (message
   framing plus anything unlabelled) stays one explicit `System instructions` row."
  [model messages {:keys [source projection input-tokens components] :as accounting}]
  (when-not (and (= :svar-estimate source)
                 (= :prepared-request projection)
                 (= model (:model accounting))
                 (integer? input-tokens)
                 (not (neg? (long input-tokens)))
                 (every? #(and (integer? %) (not (neg? (long %)))) (vals components))
                 (= input-tokens (reduce + 0 (vals components))))
    (throw (ex-info "Prepared request accounting unavailable" {})))
  (let [instructions
        (long (or (:instructions components) 0))

        [attributed remainder]
        (reduce (fn [[rows left] row]
                  (let [tokens (min (long left) (max 0 (long (:tokens row))))]
                    [(cond-> rows
                       (pos? tokens)
                       (conj (assoc row :tokens tokens))) (- (long left) tokens)]))
                [[] instructions]
                (if (pos? instructions) (instruction-attribution model messages) []))

        parts
        (into (cond-> attributed
                (pos? (long remainder))
                (conj {:label "System instructions" :tokens remainder}))
              (keep (fn [[key label]]
                      (when-let [tokens (get components key)]
                        (when (pos? tokens) {:label label :tokens tokens}))))
              [[:messages "Conversation and tool results"] [:tools "Tool declarations"]
               [:output-format "Output format"] [:reply-priming "Reply framing"]])]

    (when-not (= input-tokens (reduce + 0 (map :tokens parts)))
      (throw (ex-info "Prepared request components unavailable" {})))
    parts))

(defn request-token-counter
  "Create one iteration's tokenizer-aware Svar estimate. Reuse each model/message's
   marginal count across budgeting and health without retaining a cross-request cache.
   Provider usage remains the authority for an accepted request."
  ([] (request-token-counter {}))
  ([opts]
   (let [priming
         (memoize #(svar/count-messages % [] opts))

         message-tokens
         (memoize (fn [model message]
                    (- (svar/count-messages model [message] opts) (long (priming model)))))]

     (fn ^long [model messages]
       (long (reduce (fn [^long total message]
                       (+ total (long (message-tokens model message))))
                     (long (priming model))
                     messages))))))

(defn request-health
  "Content-free provenance for one request. Prefer Svar's final :request-accounting
   over recounting canonical messages: Responses replay filtering, tool shaping and
   body overrides have already happened. Its components are not rescaled to usage.
   Wires without prepared accounting retain explicitly labelled logical estimates.
   Neither estimate replaces same-request provider usage for utilization. Root
   guidance is disk-only; logical metadata attributes guidance without rereading it.
   An optional iteration-local message counter shares exact logical estimates with
   budgeting; prepared accounting never invokes it, splitting its single
   instructions total across the same labelled parts with the shared tokenizer."
  [environment messages tools & [model accounting message-token-counter]]
  (try
    (let [model
          (or model "unknown")

          count-messages
          (or message-token-counter svar/count-messages)

          priming
          (if accounting 0 (long (count-messages model [])))

          parts
          (if accounting
            (prepared-request-parts model messages accounting)
            (mapcat
              (fn [message]
                (let [total
                      (- (long (count-messages model [message])) priming)

                      overhead
                      (- (long (count-messages model [(assoc message :content "")])) priming)

                      [known remainder]
                      (reduce (fn [[rows left] part]
                                (let [content-tokens
                                      (if (= (:content message) (:content part))
                                        (- total overhead)
                                        (- (long (count-messages model
                                                                 [(assoc message
                                                                    :content (:content part))]))
                                           priming
                                           overhead))

                                      tokens
                                      (min (long left) (max 0 content-tokens))]

                                  [(conj rows
                                         (assoc (select-keys part [:label :path]) :tokens tokens))
                                   (- (long left) tokens)]))
                              [[] total]
                              (::parts (meta message)))]

                  (cond-> known
                    (pos? remainder)
                    (conj {:label (if (#{"system" "developer"} (:role message))
                                    "System instructions"
                                    "Conversation and tool results")
                           :tokens remainder}))))
              messages))

          parts
          (if accounting
            parts
            (cond-> (conj (vec parts) {:label "Message framing" :tokens priming})
              (seq tools)
              (conj {:label "Tool declarations"
                     :tokens (svar/count-tokens model (json/write-json-str tools))})))

          groups
          (group-by (juxt :label :path) parts)

          own
          (get-in environment [:workspace :root])]

      {:token-count-source :svar-estimate
       :token-count-model model
       :counted-projection (if accounting :prepared-request :logical-request)
       :estimated-input-tokens (reduce + 0 (map :tokens parts))
       :breakdown (mapv (fn [key]
                          (let [rows (get groups key)]
                            (assoc (select-keys (first rows) [:label :path])
                              :tokens (reduce + 0 (map :tokens rows)))))
                        (distinct (map (juxt :label :path) parts)))
       :roots (into []
                    (comp (remove #(or (:denied? %) (= own (:clone %)) (= own (:trunk %))))
                          (map (partial root-guidance-estimate model))
                          (distinct))
                    (workspace/env-filesystem-roots environment))})
    (catch Exception _
      {:token-count-source :unavailable
       :token-count-model (or model "unknown")
       :counted-projection (if accounting :prepared-request :logical-request)
       :breakdown []
       :roots []})))
