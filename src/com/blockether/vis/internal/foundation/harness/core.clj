(ns com.blockether.vis.internal.foundation.harness.core
  "`harness` compatibility layer — a BUILT-IN foundation module (ships in the
   main jar, always present, gated by toggles) that exposes the AGENTS and
   SKILLS vis' own project
   dir and other AI coding HARNESSES (Claude Code, pi, opencode, the agents
   standard, …) leave on disk to the vis model. The sibling of the shell
   layer's POSIX compat. Vis reads its OWN project-local skills from
   `.vis/skills` (highest precedence).

   Two bare verbs (bound like cat/rg via `:ext.engine/builtin? true`):

   - SKILLS are PROGRESSIVE: the prompt lists every skill `name — description`
     (cheap — always present), and
     `skill(name)` loads the FULL `SKILL.md` + its bundled resource PATHS on
     demand so the model spends tokens only on the one it uses.

   - AGENTS are an ALIAS to `sub_loop`: `agent(name, prompt)` runs the named
     agent as a CHILD loop whose system prompt IS that agent's markdown body,
     on its declared model.

   Both bare verbs are unconditionally available — skills/agents/commands have
   no user toggle; the layer is always active."
  ;; `agent` is the bare model-facing verb; deliberately shadow clojure.core/agent
  ;; (unused here) so loading this ns is warning-free.
  (:refer-clojure :exclude [agent])
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.ctx-loop :as ctx-loop]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.prompt-templates :as prompt-templates]
            [com.blockether.vis.internal.foundation.harness.discovery :as d]))

;; =============================================================================
;; Small utilities
;; =============================================================================

(defn- clip
  [s ^long n]
  (let [s (str s)]
    (if (> (count s) n) (str (subs s 0 (max 0 (dec n))) "…") s)))

;; =============================================================================
;; skill(name) — activate one SKILL.md on demand, once per live provider tape
;; =============================================================================

(defn- skill-payload
  [s]
  {"name" (:name s)
   "description" (:description s)
   "body" (:body s)
   "dir" (:dir s)
   "resources" (mapv #(str (:dir s) "/" %) (:resources s))})

(defn- current-iter-scope
  [env]
  (some-> (ctx-loop/synthesize-scope env)
          (str/split #"/f")
          first))

(defn- same-activation?
  [activation digest]
  (and (= digest (get activation "digest"))
       (not (str/blank? (str (get activation "scope"))))))

(defn- skill-result
  "Return a skill body only when it is not already present on the live provider
   tape (or its content digest changed). `stamp-iter-universe!` derives
   `engine_live_skill_activations` from the post-fold wire before every request;
   `session_active_skills` is only the durable pointer to the exact activation
   scope. A same-iteration second call is handled by that pointer before the
   first result has entered the trailer."
  [env nm]
  (if-let [s (d/skill-by-name nm)]
    (let
      [skill-name
       (:name s)

       digest
       (extension/sha256-hex (:body s))

       scope
       (current-iter-scope env)

       ctx
       (some-> (:ctx-atom env)
               deref)

       live
       (get-in ctx ["engine_live_skill_activations" skill-name])

       pending
       (get-in ctx ["session_active_skills" skill-name])

       already-live?
       (or (same-activation? live digest)
           (and (same-activation? pending digest)
                (= scope (get pending "scope"))))]

      (if already-live?
        {"name" skill-name
         "status" "already-active"
         "scope" (or (get live "scope") (get pending "scope"))
         "note" "Instructions already remain on the live provider tape; body not repeated."}
        (do
          (when-let [ca (:ctx-atom env)]
            (swap! ca assoc-in
              ["session_active_skills" skill-name]
              {"name" skill-name "digest" digest "scope" scope}))
          (skill-payload s))))
    {"error" (str "No skill named " (pr-str (str nm)) ".") "available" (mapv :name (d/skills))}))

(defn skill-tool
  "Load a harness SKILL on demand — its full SKILL.md. Names are in the HARNESS
   SKILLS block. Activation is idempotent while the exact body remains on the
   live provider tape; repeated calls return a compact receipt. A changed or
   absent body is activated once."
  [env input]
  (skill-result env (get input "name")))

(defn- render-skill
  [r]
  (cond (get r "error") {:summary (str "skill not found — " (get r "error"))}
        (= "already-active" (get r "status")) {:summary (str "`" (get r "name") "` already active")}
        :else {:summary (str "loaded skill `" (get r "name") "`")
               :body (when-let [b (not-empty (str (get r "body")))]
                       (str "```\n" b "\n```"))}))

(def skill-symbol
  ;; STRONG flat native-tool form: everything on the SYMBOL. `:native-tool? true`
  ;; (the source of "is a native tool"), `:engine-bound? false` (NOT a Python verb).
  ;; Compact semantics live in :description; exact inputs live in :schema.
  (vis/symbol
    #'skill-tool
    {:symbol 'skill
     :engine-bound? false
     :active-fn (fn [_env]
                  true)
     :tag :observation
     :native-tool? true
     :name "skill"
     :description
     "Activate one advertised harness skill on demand. Returns the full SKILL.md once while it remains on the live provider tape; repeated calls return a compact already-active receipt. If the body changed or is no longer live, it is returned once again."
     :schema {:type "object"
              :properties {"name" {:type "string"
                                   :description "Skill name from the HARNESS SKILLS list."}}
              :required ["name"]
              :additionalProperties false}
     :handler skill-tool
     :render render-skill
     :color-role :tool-color/meta}))

;; =============================================================================
;; /<name> — user-invokable skill templates (pi-style)
;; =============================================================================

(defn- skill-template-text
  "Expanded user-message text for a `/<name> [task]` invocation:
   the full SKILL.md plus the optional task."
  [_env s args]
  (let
    [r
     ;; A slash invocation is an explicit USER-authored injection, not a model
     ;; tool activation. Keep it literal and do not mutate the tool activation
     ;; index; its body lives in the current user message.
     (skill-payload s)

     task
     (when-not (str/blank? (str args)) (str "\n\nTask: " args))]

    (str "Use the skill \""
         (:name s)
         "\" for this task — its full SKILL.md follows. Follow these instructions.\n\n"
         (get r "body")
         (when (seq (get r "resources"))
           (str "\n\nBundled resources (read them with the file tools as needed):\n"
                (str/join "\n" (map #(str "- " %) (get r "resources")))))
         task)))

(defn- skill-template-entries
  "Every discovered skill as a dynamic prompt template named `<name>`,
   so the user can type `/<name> [task]` in any channel."
  []
  (mapv (fn [s]
          {:name (:name s)
           :description (str "Load skill "
                             (:name s)
                             (when-let [d (not-empty (str (:description s)))]
                               (str " — " (clip d 140))))
           :expand-fn (fn [env args]
                        (skill-template-text env s args))})
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
                 "Delegate `prompt` to a harness sub-agent as a child loop (names "
                 "listed in the HARNESS AGENTS prompt block). Child runs isolated; "
                 "edits merge back.\n"
                 "Returns {\"agent\", \"task_id\", \"status\", \"answer\", "
                 "\"changed_files\"}. Unknown name → {\"error\", \"available\": "
                 "[names]}.\n" "EXPENSIVE (a full child LLM turn) — delegable sub-tasks only.")
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
          "Harness SKILLS available — call skill(\"name\") to activate the FULL instructions on demand. Activation is idempotent while that exact body remains in the live context; a repeat returns only a compact receipt:"
          (for [s ss]
            (str "  " (:name s) " — " (clip (:description s) 180))))))))

(defn- agents-prompt
  [_env]
  (let [as (d/agents)]
    (when (seq as)
      (str/join
        "\n"
        (cons
          "Harness AGENTS available — call agent(\"name\", \"task prompt\") to delegate to a child loop (the agent's body is its system prompt; EXPENSIVE):"
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
     "Harness compatibility layer: discover + use the skills and agents other AI coding harnesses (Claude Code, opencode, …) define on disk. skill(name) loads a full SKILL.md on demand; agent(name, prompt) dispatches a sub-agent as a sub_loop child. Always available."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     ;; Always active — the skill/agent verbs are unconditionally available
     ;; (their user toggles were removed).
     :ext/activation-fn (fn [_env]
                          true)
     ;; builtin? → symbols bind BARE (skill / agent, not harness_skill).
     :ext/engine {:ext.engine/builtin? true :ext.engine/symbols [skill-symbol agent-symbol]}
     :ext/prompt-fn harness-prompt}))

(vis/register-extension! vis-extension)
