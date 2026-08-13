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
            [com.blockether.vis.internal.attachments :as attachments]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.env-python :as env-python]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.paths :as paths]
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
  "Run an extension's prompt/activation callback inside THE session context."
  [ext f environment]
  (extension/with-context {:ext ext :env environment} (f environment)))

;; =============================================================================
;; Initial messages
;; =============================================================================

(defn previous-turn-context-block
  "Render prior-turn RESUME entries. Normal entries retain their stable turn
   number and Q/A/result index. Cancelled turns retain settled work plus an
   explicit model-visible cancellation boundary. A `:checkpoint?` entry is the
   sole materialized replacement for all complete turns covered by one broader
   fold."
  [turns]
  (when (seq turns)
    (let
      [render-turn
       (fn
         [i
          {:keys [turn user-request answer interrupted? cancelled? results checkpoint? turns gist]}]
         (if checkpoint?
           (str "# ⋯ folded turn" (when (< 1 (count turns)) "s")
                " " (str/join ", " turns)
                "\n" gist)
           (let
             [req (some-> user-request
                          str
                          str/trim
                          not-empty)
              ans (some-> answer
                          str
                          str/trim
                          not-empty)
              turn-no (or turn (inc (long i)))]

             (when (or req ans (seq results))
               (str
                 "# ── turn "
                 turn-no
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
                 (cond
                   (and cancelled? (not ans))
                   (str
                     "<turn_cancelled>The user cancelled this turn. Completed tool calls and their "
                     "persisted results remain valid; do not repeat settled work. The unfinished edge "
                     "was aborted. Follow the latest user request.</turn_cancelled>")
                   (and interrupted? (not ans))
                   "⚠ this turn was INTERRUPTED before it finished — you produced NO answer. The work above is unfinished; continue it."))))))]
      (prompt-block "conversation-so-far" (str/join "\n\n" (keep-indexed render-turn turns))))))

(defn- attached-images-block
  "Manifest for image attachments riding this user message. Lists each
   attached image (path/mime/size, in attachment order) so the model can
   pair the opaque image blocks with the paths the user mentioned, and
   names sniffed-but-skipped images with the WHY (size/count cap, or a
   text-only model) so the model doesn't hunt for an attachment that isn't
   there.

   Two mutually exclusive directives ride at most one per manifest:
   - the \"you can SEE these\" / anti-PIL directive ONLY when at least one
     image is actually attached (a vision model — reading it with PIL is
     wasteful), and
   - the \"you canNOT see these, DO reach for PIL\" directive when an image
     was demoted purely because the active model has no vision. There the
     file is on disk and real; PIL/imaging libs are the model's ONLY way to
     inspect its content, so we tell it to."
  [attached skipped]
  (when (or (seq attached) (seq skipped))
    (let [readable-blind (filter :readable-blind? skipped)]
      (prompt-block
        "attached-images"
        (str
          (when (seq attached)
            "You can SEE these images — they ride this message as image blocks. Look at them\n   directly. Do NOT open them with PIL or other imaging libraries to \"read\" their\n   content — that yields only pixel size/mode, never meaning. Reach for PIL ONLY to\n   TRANSFORM an image (resize/crop/convert), never to inspect one you can already see.\n\n")
          (when (and (empty? attached) (seq readable-blind))
            "The active model has NO vision — the image(s) below are NOT attached and you canNOT\n   see them. The files are real and on disk, so to inspect their CONTENT open them with\n   PIL / an imaging library and read what you need (that is the ONLY way to see them here).\n\n")
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
                                 skipped))))))))

(defn assemble-initial-messages
  "Initial provider messages for one turn.

   Prior RESUME entries are emitted as one stable user message per turn (or
   materialized fold checkpoint), so adding a turn appends a message instead of
   rewriting one monolithic conversation recap. `:turn-context` is the current
   append-only turn/utilization assignment block and rides immediately before
   the current user request."
  [{:keys [stable-prompt-messages initial-user-content previous-turn-context turn-context
           user-images skipped-images vision?]
    :or {vision? true}}]
  (let
    [prior-messages
     (into []
           (keep (fn [entry]
                   (when-let [block (previous-turn-context-block [entry])]
                     {:role "user" :content block})))
           previous-turn-context)

     turn-block
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
     (when user-block (attached-images-block attached-images manifest-skipped))

     text
     (str/join "\n\n" (keep identity [turn-block user-block images-block]))]

    (vec (concat (or stable-prompt-messages [])
                 prior-messages
                 (when (or turn-block user-block)
                   [(if (seq attached-images)
                      (apply svar/user
                        text
                        (map #(svar/image (:base64 %) (:media-type %)) attached-images))
                      {:role "user" :content text})])))))

;; =============================================================================
;; System prompt
;; =============================================================================

(def ^:private CORE_SYSTEM_PROMPT
  "Cross-tool contract for an autonomous agent. `python_execution` is the only
   call; every other capability is a Python name whose `doc(name)` text owns its
   own inputs and preconditions."
  (str
    "You are vis. Complete the task autonomously.\n\n" "## 1. Identity + Epistemic stance\n"
    "- Host project default. Code: `grep(...)` FIRST, scoped to real paths.\n"
    "- Trust order: runtime > source > docs > assumption; report what the tools showed.\n"
    "- `apropos(text)` full-text searches every function, skill and Vis doc page; `doc(name)` returns\n"
    "  one whole document and is the authoritative contract — obey its stated preconditions; bare\n"
    "  `doc()` is the curated index. A skill is one of those documents: `doc(name)` is all of it.\n\n"
    "## 2. Execution surfaces\n"
    "- ONE call exists: `python_execution` — every action (search, read, edit, test, shell, browse) is\n"
    "  Python in that sandbox, so there is no tool to choose. Data work (YAML/JSON/TOML/CSV) is Python,\n"
    "  not shell.\n"
    "- Batch independent work in ONE block: plural arguments first, `await gather(...)` for\n"
    "  independent calls.\n"
    "- No shell TOOL: `await shell(\"npm test\")` answers a HANDLE — `sh.logs(-50)` (last n LINES)/`sh.wait(s)`/`sh.type(\"y\")`/`sh.stop()`, each carrying status.\n"
    "- Define once and reuse a small higher-order helper (functions that accept or return\n"
    "  callables): NEVER paste a near-identical loop or block twice; on the second occurrence factor it out and call it.\n"
    "- A result is an ordinary Python value: keep it in a variable and print only what the answer needs —\n"
    "  an unprinted value costs no context, and one you did not print is gone when the block ends.\n"
    "  Inspect shape before indexing; after an error inspect keys/types, then adapt.\n"
    "- State persists between blocks; `session` is a live read-only map, inspect it directly.\n"
    "  Reuse a live REPL; nothing lists one for you — `repl` `status` does.\n\n" "## 3. Inspect\n"
    "- **Filesystem work is Python**: `grep(...)` searches, `shell(...)` runs programs, `ls(dir)` maps an\n"
    "  unknown tree FIRST (`depth` descends) so no path is guessed. READING a region you will EDIT is\n"
    "  `cat(path, start, end)` — its output IS the address (`line:hash│ text`); a negative\n"
    "  `start` counts from the end. `Path.read_text` consumes; creating/moving/deleting is plain Python.\n"
    "- After the initial search make at most two targeted discovery rounds before reproducing a\n"
    "  bug or writing the smallest test; exceed only if it fails or a named unresolved decision blocks the edit.\n"
    "- Code: `grep` locates unknown code — ONE `{query, paths}` map ORs every needle, every scope,\n"
    "  and it answers anchored TEXT, never a map: a hit IS a `patch` argument. `struct_index` every known file;\n"
    "  read bodies in ONE call with `struct_nodes` (`line`→`source`+`at`). Edit by NAME with\n"
    "  `struct_patch`, by ADDRESS with `patch(path, anchor, new)` for one line or\n"
    "  `patch(path, from_anchor, to_anchor, new)` for a span (`new=\"\"` deletes) — never\n"
    "  restate the text you replace: quote the anchor, not the file.\n"
    "- Bugs: reproduce before editing: tests-only work starts with `run_tests`; interactive work uses `repl_eval`. Keep reproduction as a suite test and rerun after the fix.\n"
    "- A fix or feature is unverified until a test covers it; Python: `run_tests({\"language\": \"python\"})`; CLI: `vis-agent python -m pytest <paths>`.\n"
    "- BATCH inside one block: `grep` needles, `struct_index` `paths`, `struct_patch` `edits`,\n"
    "  several `patch` calls bottom-up (highest line first) — one call, never one\n"
    "  per file. Each round must identify, reproduce, edit, verify, or conclude; no repeated search/read without a\n"
    "  named unresolved decision.\n\n"
    "## 4. Edit + verify\n"
    "- Surgical in-scope changes; preserve unrelated work. Write only files the task asked for —\n"
    "  production code and tests, not scratch, debug, notes, or report files.\n"
    "- Re-read a file you already wrote before editing it again; `patch` answers with the\n"
    "  RE-ANCHORED window and a stale anchor is refused WITH the fresh one, so neither costs a\n"
    "  re-read. When a locator no longer resolves, re-read that one target and retry.\n\n"
    "## 5. Act autonomously\n"
    "- Make non-destructive in-scope changes on your own and report what you did.\n"
    "- Keep secrets out of answers, logs, and files.\n"
    "- Commit, push, publish, message people, or mutate external systems only when asked.\n"
    "- Ask one question only if ambiguity changes the result. Read errors; change approach;\n"
    "  decide from results you already have.\n\n"
    "## 6. Manage context\n"
    "- Treat context as a budget: in `session[\"utilization\"]` pressure is `last_request_tokens`\n"
    "  against `auto_compress_above`; `saturation`/`headroom_tokens` price the far larger hard per-call\n"
    "  limit and read calm while that budget empties, and `hint` only arms at 75% of it.\n"
    "- Fold obsolete settled work: `fold_session(target, gist)` — one broad `through`/range fold; omitting\n"
    "  the gist discards outright. Folding changes rendering, not storage:\n"
    "  a folded step is NOT re-readable, so the gist is what survives.\n"
    "- When edit-ready and headroom permits, edit before folding. Before unavoidable folds, checkpoint\n"
    "  paths/symbols, hypothesis, edit/test, and dirty files. Keep decisions, verification and\n"
    "  exact paths; confirm reduction.\n"
    "- Fold only settled steps through the last completed scope.\n\n" "## 7. Style and finish\n"
    "- Lead with the answer. Be terse; depth only when earned.\n"
    "- Finish clean: stop managed REPLs you started. Stop a background shell before final answer only\n"
    "  when it was temporary implementation or test machinery.\n"
    "- A healthy service the user asked you to run is persistent user infrastructure: leave it running\n"
    "  across turns and final answers unless asked to stop, unhealthy, or being replaced. External/user-owned resources: detach.\n"
    "- Confirm destructive actions.\n"))

(defn- config-system-prompt
  "Read the optional string-keyed `system-prompt` YAML setting.
   Returns an internal `{:text ... :is-replace ...}` map or nil."
  []
  (try (let
         [raw
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
   `:system-prompt` pulled from Vis config (`~/.vis/config.yml` / `state.yml` /
   `<project>/vis.yml` / `.vis/config.yml`, deep-merged), then `~/.vis/APPEND_SYSTEM.md`, then
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
     (and (nil? file-replace) (boolean (:is-replace cfg)))

     cfg-prompt
     (when (and cfg (not (:is-replace cfg))) (:text cfg))

     base
     (or file-replace (when cfg-replace? (:text cfg)) CORE_SYSTEM_PROMPT)

     extras
     (into []
           (comp (filter string?) (remove str/blank?))
           (into [addendum cfg-prompt] (:appends files)))]

    (str/join "\n\n" (into [base] extras))))

(defn- project-instructions-block
  "Inline primary-workspace guidance and a metadata-only index of added-root
   guidance. Added-root file contents enter the conversation only when the model
   reads the indexed file before working in that root."
  [environment]
  (try
    (binding [workspace/*filesystem-roots* (workspace/env-filesystem-roots environment)]
      (let
        [{:keys [found? source path content files]} (agents/primary-instructions)
         files (or (seq files)
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
         files (filter (fn [f]
                         (and (string? (:content f)) (not (str/blank? (:content f)))))
                       files)
         added (agents/added-root-guidance-index)]

        (when (or (seq files) (seq added))
          (let
            [header
             (str
               "Project rules from the primary workspace guidance chain. "
               "Within one filesystem scope, broader files appear first and nearer files override them. "
               "CORE wins on conflict.")
             primary-body (when (seq files)
                            (str/join "\n\n"
                                      (map (fn [f]
                                             (str "### " (agents/origin-label f)
                                                  " — " (paths/abbreviate-home (:path f))
                                                  "\n" (:content f)))
                                           files)))
             added-body
             (when (seq added)
               (str
                 "Added roots (guidance is not loaded yet):\n"
                 (str/join "\n"
                           (map (fn [{:keys [root path]}]
                                  (str "- " (paths/abbreviate-home root)
                                       " — guidance: " (paths/abbreviate-home path)))
                                added))
                 "\nBefore any action in an added root, read its exact guidance path in `python_execution`; then obey it for that root. The read result activates those rules in the conversation. Never mutate, run commands, or use browser automation there before that read."))]

            (prompt-block "project-instructions"
                          (str/join "\n\n" (keep identity [header primary-body added-body])))))))
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
  "Advertise Python's execution boundary and exact model-facing shim capabilities.
   `:shim/name` is internal identity only; imports and direct globals come from
   their explicit metadata so an id such as `attach` is never presented as a module.

   NAMES alone are a trap: every shim is a REIMPLEMENTATION, so a model that only
   reads `numpy` writes against the real numpy and hits `NotImplementedError` at
   runtime. So one line per shim, keyed by the very names advertised above it,
   carrying the surface and the refusals — and nothing else. The rest of a shim's
   contract is PULLED: `:shim/docs` answers `doc(name)`, costs no request that
   never calls that shim, and is where a query language or a fixture list belongs.

   The process surface is stated either way, and it is NOT worded here: the
   sentences are `env-python/PROCESS_SURFACE`, the same ones `subprocess` raises
   and an undriveable handle reports, so the rule the model reads in the prompt
   and the rule it hits at the call site cannot drift apart. The prompt gets
   `ban` only — the shell symbol's own docs remain the single authority for its
   invocation grammar — and with shell OFF it gets `off`, which names the tool
   AND `subprocess` / `os.system` / `os.popen`: silence read as an invitation to
   try, and the attempt only surfaced as an opaque spawn failure."
  [active-extensions]
  (let
    [shims
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

     shim-capabilities
     (->> shims
          (keep (fn [shim]
                  (let
                    [names
                     (seq (or (seq (:shim/imports shim)) (:shim/globals shim)))

                     description
                     (some-> (:shim/description shim)
                             str/trim
                             not-empty)]

                    (when (and names description)
                      [(first names)
                       (str "- " (str/join ", " (map #(str "`" % "`") names)) ": " description)]))))
          (sort-by first)
          (mapv second))

     shell?
     (boolean (some #(= "foundation-shell" (:ext/name %)) (or active-extensions [])))

     auto-imports
     (str/join "`, `" env-python/AUTO_IMPORTED_PYTHON_NAMES)]

    (prompt-block
      "sandbox-shims"
      (str
        "Auto-imported by `python_execution` (no `import`): `"
        auto-imports
        "`."
        (when (seq shim-imports)
          (str "\nPreinstalled shim modules (no pip; import before use and alias in "
               "the same block, e.g. `import numpy as np`; `np`/`pd` are never "
               "auto-created): `"
               (str/join "`, `" shim-imports)
               "`."))
        (when (seq shim-globals)
          (str "\nPrebound shim globals (use directly; never import them): `"
               (str/join "`, `" shim-globals)
               "`."))
        (when (seq shim-capabilities)
          (str
            "\nEach is a Vis REIMPLEMENTATION, not the upstream package: the line is its surface "
            "and its refusals, so trust it over your memory of the library and never reach "
            "for an API it does not claim.\n" (str/join "\n" shim-capabilities)))
        "\n"
        (get env-python/PROCESS_SURFACE (if shell? "ban" "off"))))))

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
                  (sandbox-shims-prompt-block active-extensions)]
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
   `bin/vis-agent '<task>'` one-shot runs). No human is in the loop, so the model
   must never wait for input — it makes reasonable assumptions and drives the
   work to a finished prose answer."
  (str "NON-INTERACTIVE ONE-SHOT RUN — no human is watching and nothing can "
       "be approved mid-run.\n"
       "- Keep working to a finished prose answer; there is no one to answer a question mid-run.\n"
       "- For ordinary ambiguity, state one reasonable assumption and complete the work.\n"
       "- Leave destructive or irreversible work that requires confirmation to a human. "
       "Take a safe reversible path; when none exists, finish with the exact blocked action "
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
