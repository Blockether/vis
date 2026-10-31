(ns com.blockether.vis.internal.context.prompt
  "Prompt assembly.

   Provider messages are explicit blocks in send order: core system rules,
   project instructions (AGENTS.md / CLAUDE.md when present), extension
   fragments, user request. Per-iteration user-role context is the
   engine snapshot rendered as a Python dict (`session`) by the loop."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.context.agents :as agents]
            [com.blockether.vis.internal.attachment.core :as attachments]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.config.scoped :as scoped]
            [com.blockether.vis.internal.python.env :as env-python]
            [com.blockether.vis.internal.python.runtime :as python-runtime]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.perf :as perf]
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

(defn opens-turn?
  "True when MESSAGE is the user message that opens turn TURN.
   Its text starts with the turn boundary; images come before that text."
  [message turn]
  (let [content
        (:content message)

        text
        (if (string? content)
          content
          (some #(when (= "text" (:type %)) (:text %)) (when (sequential? content) content)))]

    (and (= "user" (:role message))
         (string? text)
         (str/starts-with? text
                           (prompt-block "turn-system-context"
                                         (str "session[\"turn\"] = " turn))))))

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
   what the model had said by then plus an explicit fork, cancellation or
   interruption notice. A finished turn without an answer closes with nothing."
  [{:keys [turn request answer partial-answer interrupted? cancelled? forked?]}]
  (let
    [trimmed
     (fn [s]
       (some-> s
               str
               str/trim
               not-empty))

     request-block
     (some->> (trimmed request)
              (prompt-block "user-request"))

     answer
     (trimmed answer)

     ;; What the model had already said when the turn was cut short. It is not
     ;; an answer; without it the next turn starts from nothing.
     partial-answer
     (when-not answer (trimmed partial-answer))

     notice
     (when-not answer
       (cond
         forked? (str
                   "<turn_forked>This session was forked while this turn still ran in the source "
                   "session. Completed tool calls and their persisted results remain valid; do not "
                   "repeat settled work. The unfinished edge did not run here. Follow the latest "
                   "user request.</turn_forked>")
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
        (when initial-user-content (prompt-block "user-request" initial-user-content))

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

(def ^:private PROGRESS_NOTE_RULES
  "The progress-note rule. A one-shot `:cli` run has no reader for that prose, so its
   prompt leaves this rule out."
  (str
    "- Next to each `python_execution` call, write one sentence for the user: the next step, the change or a "
    "finding that changes the plan.\n"))

(def ^:private CORE_SYSTEM_PROMPT
  "Cross-tool contract for an autonomous agent. `python_execution` is the only
   call; registered callable signatures own call shape and `doc(name)` owns semantics.
   It is a state machine: each state names its rules and its exit."
  (str
    "Work in states. In each state, obey its rules, then take its exit. The Always rules apply in every state.\n"
    "\n" "## States\n"
    "- ANSWER: For a question, an analysis or a diff preview, change no files. Use tools only to get missing "
    "facts. Exit: FINISH.\n"
    "- ORIENT: Read the files that the task names. For an unknown path, `ls` the nearest known parent. Read only "
    "what decides the next step. Exit: REPRODUCE for a reported bug, else EDIT.\n"
    "- REPRODUCE: Before you edit, make each bug fail for the reason that you expect. Find every root cause, not "
    "only this case. Exit: EDIT.\n"
    "- EDIT: Make the smallest change that fixes the cause, in the style of the nearby code. A guard that hides "
    "a failure is not a fix. Get the data shape right first. Give shared state one writer. A repeated or "
    "interrupted operation must end in the same state. Add tests only where the project has tests. Keep probes "
    "in sandbox variables and temporary files in `$TMPDIR`. After a refused patch, read the named lines again "
    "and retry. Exit: CHECK.\n"
    "- CHECK: Test the change as its user calls it, also with empty, repeated and boundary input. Remove a test "
    "that passes when the code returns nothing. Measure a stated target on the exact case in a new process; a "
    "miss is a failure. Run the affected tests, format and lint, and read the diff. Report unrelated failures; "
    "do not fix them. Exit: EDIT on a failure, else FINISH.\n"
    "- FINISH: Give the answer first; it must make sense alone. Name the checks that you could not run. Stop "
    "only the background shells that you started for tests.\n" "\n"
    "## Always\n"
    "- Trust in this order: runtime, source, docs, assumption. Facts in the system prompt and the "
    "conversation, with its fold gists, stay known.\n"
    "- For a call, `print(inspect.signature(fn))`. Find a name with `apropos(pattern)`, a regex. `doc(name)` "
    "returns the full contract; `doc()` is the index.\n"
    "- Prebound `Path` objects: `project_root_path` and each `python_name` in "
    "`session[\"workspace\"][\"filesystem_roots\"]`. Join paths with `/`.\n"
    "- Keep results in variables and print only the fields you need. A wrong field name raises an error that "
    "lists the real fields.\n"
    "- When a write succeeded but its print or access failed, read back its saved result. Do not write again.\n"
    "- Put independent calls in one block with `await gather(...)`. Make a repeated block a named helper with "
    "a one-line docstring; `defs()` lists them.\n"
    "- Helpers and variables stay across blocks and turns. The host builds `session` again before each block.\n"
    "- Create, move and delete files with plain Python. Keep secrets out of answers, logs and files.\n"
    "- If `session[\"agent\"][\"interactive\"]` is False, do not ask: state one assumption and finish. Skip "
    "and name a destructive step.\n"
    "- When a call fails, read the error and change the approach. Decide from the results that you have.\n"
    PROGRESS_NOTE_RULES
    "\n" "## Calls\n"
    "- `ls(paths=\".\", depth=1, *, pattern=None)` lists a directory tree as text. Example: "
    "`print(ls(project_root_path / \"src\", depth=2))`.\n"
    "- `cat(path, start=None, end=None)` returns a `path @version` line, then `N│ text` rows. The one space "
    "after `│` is not part of the line. Example: `print(cat(path, 40, 80))`; `cat(path, -20)` shows the last "
    "20 lines.\n"
    "- `grep({\"query\": [needles], \"paths\": [scopes], \"context\": 3})` returns `N│ text` hit rows under "
    "`path @version` headers. Terms combine with OR. `\"is_regex\": True` runs a regex, and `\"include\": "
    "[\"*.py\"]` limits the files. Example: `print(grep({\"query\": [\"def load\"], \"paths\": [\"src\"]}))`. "
    "Read known regions directly.\n"
    "- `patch(path, edits, version=None)` writes all edits of one file at once, or nothing. Use one call for "
    "each file.\n"
    "  - Text edit: `{\"old\": exact_text, \"new\": text}`. `old` must match once; add `\"line\": n` or "
    "`\"all\": True` for more matches. Example: `patch(path, [{\"old\": \"    x = 1\", \"new\": \"    x = 2\"}])`.\n"
    "  - Line edit, for long spans: `{\"from\": a, \"to\": b, \"new\": text}` with `version`, the `@` value "
    "of the read. Example: `patch(path, [{\"from\": 12, \"to\": 14, \"new\": \"\"}], \"1a2b3c4d\")` "
    "deletes lines 12 to 14.\n"
    "\n" "## Context\n"
    "- When `hint` shows and a large phase follows, make `print(fold_session(\"-tN/iK\", gist))` the only call "
    "of a step. The gist keeps conclusions, exact paths, evidence, test state and dirty files.\n"))

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

(defn- core-system-prompt
  "The core rules. A non-interactive run gets them without the progress-note rules."
  [interactive?]
  (if interactive? CORE_SYSTEM_PROMPT (str/replace CORE_SYSTEM_PROMPT PROGRESS_NOTE_RULES "")))

(defn- system-prompt-blocks
  "Send-order pieces of the system prompt: the base text, whether a user file or
   config replaced it, and the custom blocks appended after it.
   `build-system-prompt` joins them; the context breakdown attributes the base
   and the additions as separate rows. `:interactive? false` removes the
   progress-note rules from the core text."
  [{:keys [system-prompt workspace-root interactive?] :or {interactive? true}}]
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
                        ". " (core-system-prompt interactive?)))
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
   reads the indexed file before working in that root. Nested projects that supply
   active extensions get the same index (issue 399)."
  [environment active-extensions]
  (try
    (binding [workspace/*workspace-root*
              (or (workspace/workspace-root environment)
                  (get-in environment [:workspace :root])
                  workspace/*workspace-root*)

              workspace/*config-sources*
              (if (contains? environment :config/sources)
                (:config/sources environment)
                workspace/*config-sources*)

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
            (agents/added-root-guidance-index)

            nested
            (agents/extension-project-guidance-index (keep :ext/project-path active-extensions))]

        (when (or (seq files) (seq added) (seq nested))
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
                    "there."))

             nested-body
             (when (seq nested)
               (str
                 "Extension projects (guidance is not loaded yet):\n"
                 (str/join "\n"
                           (map (fn [{:keys [root path]}]
                                  (str "- " (paths/abbreviate-home root)
                                       " — guidance: " (paths/abbreviate-home path)))
                                nested))
                 "\nThese projects supply extensions of this session. Before you use their tools "
                 "or act in one of these projects, read its exact guidance path in "
                 "`python_execution`. The read activates its rules for that project; obey them."))]

            {:content (prompt-block "project-instructions"
                                    (str/join "\n\n"
                                              (keep identity
                                                    [header primary-body added-body nested-body])))
             :parts (cond-> (mapv (fn [f]
                                    {:label
                                     (str (if (= :project (:scope f)) "Main " "Workspace ")
                                          (if (= :claude-md (:source f)) "CLAUDE.md" "AGENTS.md"))
                                     :path (paths/abbreviate-home (:path f))
                                     :content (:content f)})
                                  files)
                      (util/non-blank-string? added-body)
                      (conj {:label "Linked filesystem guidance index" :content added-body})

                      (util/non-blank-string? nested-body)
                      (conj {:label "Extension project guidance index" :content nested-body}))}))))
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
                              (perf/timed (keyword "activation" (str (:ext/name ext)))
                                          (call-extension-callback ext
                                                                   (:ext/activation-fn ext)
                                                                   environment))))
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
   and the rule it hits at the call site cannot drift apart. With shell ON the
   prompt gets `ban` plus a short handle summary, so the shell rules cost nothing
   when the tool is off; with shell OFF it gets `off`, which names the tool
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
      (str
        "Already imported: `"
        auto-imports
        "`."
        "\nBuild globals, for diagnostics only: "
        (str/join ", "
                  (map (fn [[name value]]
                         (str "`" name " = " (if (nil? value) "None" (pr-str value)) "`"))
                       (sort (python-runtime/version-globals))))
        ". `dev` or `None` means no release data. `blockether.vis.extension` is the bundled, "
        "frozen SDK (no `__file__`; it wins over pip copies): import it for types only. "
        "`python_execution` cannot register extensions or use extension host APIs."
        "\nThe sandbox is real CPython: the Python environment of Vis, not the interpreter of the project. "
        "It reads the same `~/.vis/python/packages` as extensions, read-only. Imports never install "
        "packages; use `vis-agent python uv sync` or `vis-agent python -m pip install`."
        (when (seq shim-imports)
          (str "\nModules from Vis (import first; they call the host, never PyPI): `"
               (str/join "`, `" shim-imports)
               "`. `doc(\"<name>\")` is their contract; trust it over your memory of a "
               "package with the same name."))
        (when (seq shim-globals)
          (str "\nPrebound globals (do not import): `" (str/join "`, `" shim-globals) "`."))
        "\n"
        (get env-python/PROCESS_SURFACE (if shell? "ban" "off"))
        (when shell?
          (str
            "\nUse `shell` only to run programs, such as the project interpreter for its packages. "
            "Do file and data work in Python. `sh = await shell(cmd)` returns a handle with `logs(-n)` "
            "(last n lines), `wait(s)`, `type(text)` and `stop()`; each returns `out`, `exit` and "
            "`status`. Run to the end in one block: `r = (await shell(cmd)).wait(300)`."))))))

(defn- turn-system-context-block
  "Turn-scoped system context that can be rebuilt/replaced as runtime
   capabilities change.

   Keep this as ONE provider system message. Extension prompts belong here,
   not in every per-iteration trailer. When a future
   reload path recomputes active extensions mid-turn, it should replace this
   message in the rebuilt stateless provider message vector rather than append
   a second extension/context message.

   Returns `{:content <block> :parts [{:label … :content …}]}` so each built-in or
   installed extension prompt and the sandbox surface are attributed separately
   in the context breakdown."
  [environment active-extensions]
  (let [extensions
        (extensions-prompt-block environment active-extensions)

        shims
        (sandbox-shims-prompt-block active-extensions)

        blocks
        (->> [(:content extensions) shims]
             (filter util/non-blank-string?)
             seq)]

    (when blocks
      {:content (prompt-block "turn-system-context" (str/join "\n\n" blocks))
       :parts (vec (concat (:parts extensions)
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
                               :workspace-root (get-in environment [:workspace :root])
                               ;; A one-shot `:cli` run is not interactive (see `session.agents`).
                               :interactive? (not= :cli (:channel environment))})

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

        project-block
        (project-instructions-block environment active-extensions)

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

(defn- capped-rows
  "Cap labelled rows in order by `budget`; return the kept rows and the unused budget."
  [budget rows]
  (reduce (fn [[kept left] row]
            (let [tokens (min (long left) (max 0 (long (:tokens row))))]
              [(cond-> kept
                 (pos? tokens)
                 (conj (assoc row :tokens tokens))) (- (long left) tokens)]))
          [[] (long budget)]
          rows))

(defn- prepared-request-parts
  "Project Svar's content-free components into UI labels without rescaling them.
   Svar sends the first system message as Responses `instructions` and each later one
   as an input message. Both totals are split across their labelled parts — core prompt,
   each injected guidance file, runtime and extension prompts, session context — using
   isolated Svar content counts. Rows are capped by the reported totals. The unattributed
   remainders stay explicit `System instructions` and conversation rows."
  [model messages {:keys [source projection input-tokens components] :as accounting}]
  (when-not (and (= :svar-estimate source)
                 (= :prepared-request projection)
                 (= model (:model accounting))
                 (integer? input-tokens)
                 (not (neg? (long input-tokens)))
                 (every? #(and (integer? %) (not (neg? (long %)))) (vals components))
                 (= input-tokens (reduce + 0 (vals components))))
    (throw (ex-info "Prepared request accounting unavailable" {})))
  (let [[first-system later]
        (let [i (first (keep-indexed #(when (= "system" (:role %2)) %1) messages))]
          (if i
            [[(nth messages i)] (into (subvec (vec messages) 0 i) (subvec (vec messages) (inc i)))]
            [[] messages]))

        [instruction-rows instruction-rest]
        (capped-rows (or (:instructions components) 0) (instruction-attribution model first-system))

        [input-rows input-rest]
        (capped-rows (or (:messages components) 0) (instruction-attribution model later))

        parts
        (-> instruction-rows
            (cond->
              (pos? (long instruction-rest))
              (conj {:label "System instructions" :tokens instruction-rest}))
            (into input-rows)
            (cond->
              (pos? (long input-rest))
              (conj {:label "Conversation and tool results" :tokens input-rest}))
            (into (keep (fn [[key label]]
                          (when-let [tokens (get components key)]
                            (when (pos? tokens) {:label label :tokens tokens}))))
                  [[:tools "Tool declarations"] [:output-format "Output format"]
                   [:reply-priming "Reply framing"]]))]

    (when-not (= input-tokens (reduce + 0 (map :tokens parts)))
      (throw (ex-info "Prepared request components unavailable" {})))
    parts))

(defonce ^:private token-encoding-warmup
  (delay (doto (Thread. ^Runnable
                        (fn []
                          (let [start (System/nanoTime)]
                            (try (svar/count-tokens "unknown" "warm" {:tokenizer "o200k_base"})
                                 (tel/log! {:level :debug
                                            :id ::token-encoding-warm
                                            :data {:ms (/ (double (- (System/nanoTime) start))
                                                          1000000.0)}})
                                 (catch Throwable _ nil))))
                        "vis-token-encoding-warmup")
           (.setDaemon true)
           (.start))))

(defn warm-token-encoding!
  "Load the default tokenizer ranks on a daemon thread, once per process.
   The first token count of a turn otherwise loads them, about 160 ms in the native image."
  []
  @token-encoding-warmup
  nil)

(def ^:private cached-message-tokens
  (util/bounded-memoize (fn [model message opts]
                          (svar/count-messages model [message] opts))
                        512
                        (* 4 1024 1024)
                        (fn [args _]
                          (+ 1024 (* 4 (count (pr-str args)))))))

(defn request-token-counter
  "Count messages with bounded reuse by model, tokenizer options and complete content.
   Retain at most 512 messages within a 4 MiB weight limit across requests.
   Provider usage remains the authority for an accepted request."
  ([] (request-token-counter {}))
  ([opts]
   (let [priming (memoize #(svar/count-messages % [] opts))]
     (fn ^long [model messages]
       (let [baseline (long (priming model))]
         (long (reduce (fn [^long total message]
                         (+ total (- (long (cached-message-tokens model message opts)) baseline)))
                       baseline
                       messages)))))))

(defn request-health
  "Content-free provenance for one request. Prefer Svar's final :request-accounting
   over recounting canonical messages: Responses replay filtering, tool shaping and
   body overrides have already happened. Its components are not rescaled to usage.
   Wires without prepared accounting retain explicitly labelled logical estimates.
   Neither estimate replaces same-request provider usage for utilization. Root
   guidance is disk-only; logical metadata attributes guidance without rereading it.
   An optional iteration-local message counter shares exact logical estimates with
   budgeting; prepared accounting never invokes it, splitting its instructions and
   input totals across the same labelled parts with the shared tokenizer."
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
