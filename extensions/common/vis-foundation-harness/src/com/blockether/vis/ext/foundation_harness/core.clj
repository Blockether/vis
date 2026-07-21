(ns com.blockether.vis.ext.foundation-harness.core
  "`harness` compatibility extension — a DROPPABLE classpath plug-in (drop the
   jar, drop the feature) that exposes the AGENTS and SKILLS vis' own project
   dir and other AI coding HARNESSES (Claude Code, pi, opencode, the agents
   standard, …) leave on disk to the vis model. The sibling of the shell
   layer's POSIX compat. Vis reads its OWN project-local skills from
   `.vis/skills` (highest precedence).

   Two bare verbs (bound like cat/rg via `:ext.engine/builtin? true`):

   - SKILLS are PROGRESSIVE: the prompt lists every skill `name — description`
     (cheap — always present while `:vis/harness-skills` is ON), and
     `skill(name)` loads the FULL `SKILL.md` + its bundled resource PATHS on
     demand so the model spends tokens only on the one it uses.

   - AGENTS are an ALIAS to `sub_loop`: `agent(name, prompt)` runs the named
     agent as a CHILD loop whose system prompt IS that agent's markdown body,
     on its declared model. Gated by `:vis/harness-agents`.

   This layer OWNS its toggles — registered here, not in core — so dropping the
   jar drops the toggles. `:owner :vis :group :tools` parks them in Settings →
   Feature Toggles beside the shell toggle. Each verb gates on its OWN toggle
   (the extension activates if EITHER is on), so they switch independently."
  ;; `agent` is the bare model-facing verb; deliberately shadow clojure.core/agent
  ;; (unused here) so loading this ns is warning-free.
  (:refer-clojure :exclude [agent])
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.prompt-templates :as prompt-templates]
            [com.blockether.vis.internal.toggles :as toggles]
            [com.blockether.vis.ext.foundation-harness.discovery :as d]))

;; =============================================================================
;; Toggles — OWNED by this layer (registered on load), not by core. :owner :vis
;; :group :tools → Settings → General → Feature Toggles, beside the shell toggle.
;; =============================================================================

(toggles/register-toggle! {:id :vis/harness-skills
                           :label "Harness skills (compatibility layer)"
                           :description
                           (str "When ON the agent can discover and load SKILLS from vis'"
                                " own project-local .vis/skills (highest precedence) and from"
                                " other AI coding harnesses (Claude Code, pi, opencode, the"
                                " agents standard) — e.g. .claude/skills, ~/.claude/skills,"
                                " plugin caches, ~/.pi/agent/skills, ~/.agents/skills. The"
                                " prompt lists each skill's name + description cheaply;"
                                " skill(name) loads the full SKILL.md + its bundled resource"
                                " paths on demand. ON by default.")
                           :default true
                           :owner :vis
                           :group :tools
                           :persist? true})

(toggles/register-toggle! {:id :vis/harness-agents
                           :label "Harness agents (compatibility layer)"
                           :description
                           (str "When ON the agent can dispatch sub-AGENTS defined by other"
                                " AI coding harnesses (Claude Code, opencode, …): agent(name,"
                                " prompt) runs the named agent as a CHILD loop (a sub_loop)"
                                " whose system prompt is that agent's markdown body, on its"
                                " declared model. The prompt lists each agent's name +"
                                " description. ON by default.")
                           :default true
                           :owner :vis
                           :group :tools
                           :persist? true})

;; =============================================================================
;; Small utilities
;; =============================================================================

(defn- clip
  [s ^long n]
  (let [s (str s)]
    (if (> (count s) n) (str (subs s 0 (max 0 (dec n))) "…") s)))

;; =============================================================================
;; skill(name) — load one SKILL.md on demand (PROGRESSIVE)
;; =============================================================================

(defn- loaded-skill-names
  "The skill names already loaded into THIS session's context. Read off the
   durable `\"session_loaded_skills\"` set on the string-keyed ctx, which rides
   the Nippy session snapshot (like `\"session_summaries\"`), so it persists in
   the DB and survives resume. No transient atom to keep."
  [env]
  (or (some-> env
              :ctx-atom
              deref
              (get "session_loaded_skills")
              set)
      #{}))

(defn- mark-skill-loaded!
  "Record `nm` as loaded on THIS session's string-keyed ctx so a later
   `skill(name)` won't re-inject the body. Persists with the next ctx snapshot."
  [env nm]
  (when-let [ca (:ctx-atom env)]
    (swap! ca update "session_loaded_skills" (fnil conj #{}) nm)))

(defn- skill-result
  [env nm]
  (if-let [s (d/skill-by-name nm)]
    (if (contains? (loaded-skill-names env) (:name s))
      ;; PROGRESSIVE + once-only: the SKILL.md body is already above in this
      ;; session's context — ack WITHOUT re-injecting it (a re-call is cheap).
      {"name" (:name s)
       "status" "already-loaded"
       "note" (str "Already loaded earlier this session — its SKILL.md is above "
                   "in your context; follow it. Not re-injected.")}
      (do (mark-skill-loaded! env (:name s))
          {"name" (:name s)
           "description" (:description s)
           "body" (:body s)
           "dir" (:dir s)
           "resources" (mapv #(str (:dir s) "/" %) (:resources s))}))
    {"error" (str "No skill named " (pr-str (str nm)) ".") "available" (mapv :name (d/skills))}))

(defn skill-tool
  "Load a harness SKILL on demand — its full SKILL.md. Names are in the HARNESS
   SKILLS block (✓ = already loaded). Loaded ONCE per session; a second call acks
   already-loaded without resending the body."
  [env input]
  (skill-result env (get input "name")))

(defn- render-skill
  [r]
  (cond (get r "error") {:summary (str "skill not found — " (get r "error"))}
        (= "already-loaded" (get r "status")) {:summary (str "`" (get r "name") "` already loaded")}
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
                  (toggles/enabled? :vis/harness-skills))
     :tag :observation
     :native-tool? true
     :name "skill"
     :description
     "Load one advertised harness skill on demand. Loading is session-idempotent, so repeated calls acknowledge without resending the instructions."
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
   the full SKILL.md (once — an already-loaded skill gets a pointer
   instead of a re-injection, same as skill()) plus the optional task."
  [env s args]
  (let
    [r
     (skill-result env (:name s))

     task
     (when-not (str/blank? (str args)) (str "\n\nTask: " args))]

    (if (= "already-loaded" (get r "status"))
      (str "Use the skill \""
           (:name s)
           "\" — it was already loaded earlier "
           "this session; its SKILL.md is above in your context. Follow it."
           task)
      (str "Use the skill \""
           (:name s)
           "\" for this task — its full SKILL.md "
           "follows. Follow these instructions.\n\n"
           (get r "body")
           (when (seq (get r "resources"))
             (str "\n\nBundled resources (read them with the file tools as needed):\n"
                  (str/join "\n" (map #(str "- " %) (get r "resources")))))
           task))))

(defn- skill-template-entries
  "Every discovered skill as a dynamic prompt template named `<name>`,
   so the user can type `/<name> [task]` in any
   channel. Gated by the same toggle as the skill() verb."
  []
  (when (toggles/enabled? :vis/harness-skills)
    (mapv (fn [s]
            {:name (:name s)
             :description (str "Load skill "
                               (:name s)
                               (when-let [d (not-empty (str (:description s)))]
                                 (str " — " (clip d 140))))
             :expand-fn (fn [env args]
                          (skill-template-text env s args))})
          (d/skills))))

(prompt-templates/register-provider! ::skills skill-template-entries)

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
                            (toggles/enabled? :vis/harness-agents))
               :inject-env? true
               :tag :mutation}))

;; =============================================================================
;; Prompt fragment — the CHEAP progressive listings (name — description)
;; =============================================================================

(defn- skills-prompt
  [env]
  (when (toggles/enabled? :vis/harness-skills)
    (let
      [ss
       (d/skills)

       loaded
       (loaded-skill-names env)]

      (when (seq ss)
        (str/join
          "\n"
          (cons
            "Harness SKILLS available — call skill(\"name\") to load the FULL instructions on demand (✓ = already loaded this session, its body is in your context):"
            (for [s ss]
              (str "  "
                   (if (contains? loaded (:name s)) "✓ " "")
                   (:name s)
                   " — "
                   (clip (:description s) 180)))))))))

(defn- agents-prompt
  [_env]
  (when (toggles/enabled? :vis/harness-agents)
    (let [as (d/agents)]
      (when (seq as)
        (str/join
          "\n"
          (cons
            "Harness AGENTS available — call agent(\"name\", \"task prompt\") to delegate to a child loop (the agent's body is its system prompt; EXPENSIVE):"
            (for [a as]
              (str "  " (:name a) " — " (clip (:description a) 180)))))))))

(defn- harness-prompt
  "Combined harness surface — each section present only while its toggle is ON
   (a blank string is filtered from the extensions prompt block, so a disabled
   half costs zero prompt tokens)."
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
     "Harness compatibility layer: discover + use the skills and agents other AI coding harnesses (Claude Code, opencode, …) define on disk. skill(name) loads a full SKILL.md on demand; agent(name, prompt) dispatches a sub-agent as a sub_loop child. Toggles :vis/harness-skills / :vis/harness-agents, both ON by default."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     ;; Active when EITHER half is on; each verb's before-fn gates its OWN
     ;; toggle, so they switch independently. Both off → the extension is
     ;; inactive and `sync-active-extension-symbols!` removes both bare verbs.
     :ext/activation-fn (fn [_env]
                          (or (toggles/enabled? :vis/harness-skills)
                              (toggles/enabled? :vis/harness-agents)))
     ;; builtin? → symbols bind BARE (skill / agent, not harness_skill).
     :ext/engine {:ext.engine/builtin? true :ext.engine/symbols [skill-symbol agent-symbol]}
     :ext/prompt-fn harness-prompt}))

(vis/register-extension! vis-extension)
