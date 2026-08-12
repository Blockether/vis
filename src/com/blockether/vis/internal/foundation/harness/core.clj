(ns com.blockether.vis.internal.foundation.harness.core
  "`harness` compatibility layer — a BUILT-IN foundation module (ships in the
   main jar, always present, gated by toggles) that exposes the AGENTS and
   SKILLS vis' own project
   dir and other AI coding HARNESSES (Claude Code, pi, opencode, the agents
   standard, …) leave on disk to the vis model. The sibling of the shell
   layer's POSIX compat. Vis reads its OWN project-local skills from
   `.vis/skills` (highest precedence).

   - SKILLS are DOCUMENTS, never a verb: the prompt lists every skill
     `name — description` (cheap — always present) and the WHOLE `SKILL.md` is
     one document in the `doc`/`apropos` corpus, so `apropos(text)` finds it and
     `doc(name)` prints it whole. Reading a skill has no session effect: there
     is nothing to activate, nothing to re-read and no activation receipt.

   - AGENTS are the layer's one bare verb (bound like cat/rg via
     `:ext.engine/builtin? true`) and an ALIAS to `sub_loop`:
     `agent(name, prompt)` runs the named agent as a CHILD loop whose system
     prompt IS that agent's markdown body, on its declared model.

   Skills/agents/commands have no user toggle; the layer is always active."
  ;; `agent` is the bare model-facing verb; deliberately shadow clojure.core/agent
  ;; (unused here) so loading this ns is warning-free.
  (:refer-clojure :exclude [agent])
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.prompt-templates :as prompt-templates]
            [com.blockether.vis.internal.workspace :as workspace]
            [com.blockether.vis.internal.foundation.harness.discovery :as d]))

;; =============================================================================
;; Small utilities
;; =============================================================================

(defn- clip
  [s ^long n]
  (let [s (str s)]
    (if (> (count s) n) (str (subs s 0 (max 0 (dec n))) "…") s)))

;; =============================================================================
;; Skill ownership — the nested project a SKILL.md was written for
;; =============================================================================

(defn- canonical-path
  [p]
  (when-not (str/blank? (str p))
    (try (.getCanonicalPath (java.io.File. (str p))) (catch Throwable _ (str p)))))

(defn- session-root
  "Canonical root of the workspace this turn runs in."
  []
  (try (canonical-path (.getPath (workspace/cwd))) (catch Throwable _ nil)))

(defn- owner-root
  "The project that OWNS a skill, when that project is NOT the session's own
   root. A repository-root session discovers the skills of every nested project
   (`apps/vis-companion/.agents/skills/impeccable`), and such a SKILL.md was
   written for ITS tree — so every surface that hands the skill over says whose
   it is. Same-root skills carry no owner and read exactly as before."
  [s]
  (let [owner (canonical-path (:project-root s))]
    (when (and owner (not= owner (session-root))) owner)))

(defn- owner-note
  "The one sentence an owned skill is announced with."
  [owner]
  (when owner
    (str "This skill belongs to the project at "
         owner
         " — work under that directory: its paths, guidance and tooling are relative to it.")))

(defn- owner-label
  "Session-relative path of a skill's owning project (`apps/vis-companion`) for
   the cheap prompt listing, or nil when the skill is the session's own."
  [s]
  (when-let [owner (owner-root s)]
    (let [root (session-root)]
      (if (and root (str/starts-with? owner (str root "/")))
        (subs owner (inc (count root)))
        owner))))

(defn- skill-payload
  "The skill as data for a `/<name>` slash expansion: its body, the directory it
   lives in and its bundled resource paths."
  [s]
  (let [owner (owner-root s)]
    (cond->
      {"name" (:name s)
       "description" (:description s)
       "body" (:body s)
       "cwd" (:dir s)
       "resources" (mapv #(str (:dir s) "/" %) (:resources s))}
      (:project-root s)
      (assoc "project_root" (:project-root s))

      owner
      (assoc "note" (owner-note owner)))))

;; =============================================================================
;; /<name> — user-invokable skill templates (pi-style)
;; =============================================================================

(defn- skill-template-text
  "Expanded user-message text for a `/<name> [task]` invocation:
   the full SKILL.md plus the optional task. A skill owned by a nested project
   ALSO says so — the turn is re-rooted there (`prompt-templates/expand`
   carries `:project-root`), and the message must not leave that silent."
  [_env s args]
  (let
    [r
     ;; A slash invocation is an explicit USER-authored injection: the whole
     ;; SKILL.md lands in the current user message, literally.
     (skill-payload s)

     note
     (owner-note (owner-root s))

     task
     (when-not (str/blank? (str args)) (str "\n\nTask: " args))]

    (str "Use the skill \""
         (:name s)
         "\" for this task — its full SKILL.md follows. Follow these instructions.\n\n"
         (when note (str note "\n\n"))
         (get r "body")
         (when (seq (get r "resources"))
           (str "\n\nBundled resources (read them with the file tools as needed):\n"
                (str/join "\n" (map #(str "- " %) (get r "resources")))))
         task)))

(defn- skill-template-entries
  "Every discovered skill as one dynamic prompt template named `<name>`. A
   repository-local skill carries its owning project root so invocation can run
   relative to that project even when selected from a repository-root session."
  []
  (mapv (fn [s]
          (cond->
            {:name (:name s)
             :description (str "Load skill "
                               (:name s)
                               (when-let [d (not-empty (str (:description s)))]
                                 (str " — " (clip d 140))))
             :expand-fn (fn [env args]
                          (skill-template-text env s args))}
            (:project-root s)
            (assoc :project-root (:project-root s))))
        (d/skills)))

(prompt-templates/register-provider! ::skills skill-template-entries)

(defn- command-template-entries
  "Every discovered cross-harness COMMAND as a `/<name>` prompt template, so the
   user can type `/<name> [args]` in any channel. The body is expanded like a
   file template (`$ARGUMENTS`-substituted)."
  []
  (mapv (fn [c]
          {:name (:name c)
           :description (str "Command "
                             (:name c)
                             (when-let [d (not-empty (str (:description c)))]
                               (str " — " (clip d 140))))
           :body (:body c)})
        (d/commands)))

(prompt-templates/register-provider! ::commands command-template-entries)

;; `/reload` refresh: force a full rescan of the harness agent/skill source
;; dirs (the marker cache already catches file edits; the hook also covers
;; sources a stat can miss and gives the user an explicit big hammer).
(extension/register-reload-hook! ::discovery d/reload!)

;; =============================================================================
;; agent(name, prompt) — dispatch a sub-AGENT as a sub_loop CHILD
;; =============================================================================

(defn- agent-result
  "Run the named agent as a sub_loop child: its markdown body becomes the
   child's system prompt, its frontmatter model the routing preference (ALWAYS
   a vector — `router-for-model` falls back on an unknown name). `prompt` is the
   task. Unknown name → an error dict carrying the available names."
  [env nm prompt]
  (if-let [a (d/agent-by-name nm)]
    (let
      [res (lp/sub-loop! env
                         {:prompt (str prompt)
                          :subctx {:focus (:name a)}
                          :models (when (:model a) [(:model a)])
                          :system-prompt (:body a)})
       ;; sub_loop derives status from the focus TASK; an agent dispatch seeds
       ;; none, so a completed child turn carries no status string. Read it
       ;; from the turn OUTCOME instead: errored → failed, otherwise the turn
       ;; ran to completion → done.
       status (or (not-empty (str (:status res))) (if (:error res) "failed" "done"))]

      ;; Model-facing result crosses the strings-only boundary — build it with
      ;; string keys straight from the (internal, keyword-keyed) sub_loop result.
      (cond->
        {"agent" (:name a)
         "task_id" (:task_id res)
         "status" status
         "answer" (:answer res)
         "changed_files" (vec (:changed_files res))}
        (:error res)
        (assoc "error" (:error res))))
    {"error" (str "No agent named " (pr-str (str nm)) ".") "available" (mapv :name (d/agents))}))

(def ^{:doc (str "await agent(name, prompt)\n"
                 "Run a named HARNESS AGENTS sub-agent in an isolated child loop; "
                 "edits merge back. Returns {\"agent\", \"task_id\", \"status\", "
                 "\"answer\", \"changed_files\"}. Unknown name: {\"error\", "
                 "\"available\": [names]}. EXPENSIVE full LLM turn; delegable tasks only.")
       :arglists '([name prompt])}
     agent
  (fn agent-impl [env nm prompt]
    (extension/success {:result (agent-result env nm prompt)})))

(def agent-symbol
  ;; bound Python verb, gated by :active-fn (sync removes it when agents are off)
  ;; and handed `env` via :inject-env? — one gating mechanism, no before-fn.
  (vis/symbol #'agent
              {:symbol 'agent
               :active-fn (fn [_env]
                            true)
               :inject-env? true
               :tag :mutation}))

;; =============================================================================
;; Prompt fragment — the CHEAP progressive listings (name — description)
;; =============================================================================

(defn- skills-prompt
  [_env]
  (let [ss (d/skills)]
    (when (seq ss)
      (str/join
        "\n"
        (cons
          (str
            "Harness SKILLS available — `doc(\"name\")` prints one whole SKILL.md, `apropos(text)`"
            " searches them all; reading one has no session effect"
            " (a `[project]` tag names the nested project that OWNS a skill — work under that"
            " directory when you use it):")
          (for [s ss]
            (str "  "
                 (:name s)
                 (when-let [o (owner-label s)]
                   (str " [" o "]"))
                 " — "
                 (clip (:description s) 180))))))))

(defn- agents-prompt
  [_env]
  (let [as (d/agents)]
    (when (seq as)
      (str/join
        "\n"
        (cons
          "Harness AGENTS available — call agent(\"name\", \"task prompt\") to delegate to a child loop (EXPENSIVE):"
          (for [a as]
            (str "  " (:name a) " — " (clip (:description a) 180))))))))

(defn- harness-prompt
  "Combined always-on harness surface. Empty discovery sections are omitted."
  [env]
  (let [parts (remove str/blank? [(skills-prompt env) (agents-prompt env)])]
    (if (seq parts) (str/join "\n\n" parts) "")))

;; =============================================================================
;; Extension
;; =============================================================================

(def vis-extension
  (vis/extension
    {:ext/name "foundation-harness"
     :ext/description
     "Discovers on-disk Claude Code/opencode skills and agents: every SKILL.md is a `doc`/`apropos` document; `agent(name,prompt)` dispatches a `sub_loop` child. Always available."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     ;; Always active — the agent verb and the skill listing are unconditionally
     ;; available (their user toggles were removed).
     :ext/activation-fn (fn [_env]
                          true)
     ;; builtin? → the symbol binds BARE (agent, not harness_agent).
     :ext/engine {:ext.engine/builtin? true :ext.engine/symbols [agent-symbol]}
     :ext/prompt-fn harness-prompt}))

(vis/register-extension! vis-extension)
