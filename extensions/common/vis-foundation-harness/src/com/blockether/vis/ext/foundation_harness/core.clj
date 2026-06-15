(ns com.blockether.vis.ext.foundation-harness.core
  "`harness` compatibility extension — a DROPPABLE classpath plug-in (drop the
   jar, drop the feature) that exposes the AGENTS and SKILLS other AI coding
   HARNESSES (Claude Code, opencode, …) leave on disk to the vis model. The
   sibling of the shell layer's POSIX compat.

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
  (:require
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.loop :as lp]
   [com.blockether.vis.internal.toggles :as toggles]
   [com.blockether.vis.ext.foundation-harness.discovery :as d]))

;; =============================================================================
;; Toggles — OWNED by this layer (registered on load), not by core. :owner :vis
;; :group :tools → Settings → General → Feature Toggles, beside the shell toggle.
;; =============================================================================

(toggles/register-toggle!
  {:id :vis/harness-skills :label "Harness skills (compatibility layer)"
   :description (str "When ON the agent can discover and load SKILLS defined by"
                  " other AI coding harnesses (Claude Code, opencode, …) from"
                  " .claude/skills, ~/.claude/skills, and plugin caches. The"
                  " prompt lists each skill's name + description cheaply;"
                  " skill(name) loads the full SKILL.md + its bundled resource"
                  " paths on demand. ON by default.")
   :default true :owner :vis :group :tools :persist? true})

(toggles/register-toggle!
  {:id :vis/harness-agents :label "Harness agents (compatibility layer)"
   :description (str "When ON the agent can dispatch sub-AGENTS defined by other"
                  " AI coding harnesses (Claude Code, opencode, …): agent(name,"
                  " prompt) runs the named agent as a CHILD loop (a sub_loop)"
                  " whose system prompt is that agent's markdown body, on its"
                  " declared model. The prompt lists each agent's name +"
                  " description. ON by default.")
   :default true :owner :vis :group :tools :persist? true})

;; =============================================================================
;; IR helpers + small utilities
;; =============================================================================

(def ^:private ir-root extension/ir-root)
(def ^:private ir-p extension/ir-p)
(def ^:private ir-strong extension/ir-strong)
(def ^:private ir-code extension/ir-code)
(def ^:private ir-code-block extension/ir-code-block)

(defn- clip [s n]
  (let [s (str s)] (if (> (count s) n) (str (subs s 0 (max 0 (dec n))) "…") s)))

(defn- gate-before-fn
  "Per-verb toggle gate. When `toggle` is ON the call proceeds (with `env`
   injected as the hidden first arg when `inject-env?`); when OFF it
   short-circuits into a refusal envelope the loop surfaces as a clean tool
   error. Both verbs share ONE extension, so this — not the extension-level
   activation-fn — is what gives them INDEPENDENT switches."
  [toggle inject-env? label]
  (fn [env f args]
    (if (toggles/enabled? toggle)
      {:env env :fn f :args (if inject-env? (into [env] args) (vec args))}
      (let [t (System/currentTimeMillis)]
        {:result (extension/failure
                   {:result   nil
                    :metadata {:started-at-ms t :finished-at-ms t :duration-ms 0}
                    :error    {:message (str label " is disabled — turn on the "
                                          toggle " toggle in Settings → Feature Toggles.")
                               :type    ::disabled}})}))))

;; =============================================================================
;; skill(name) — load one SKILL.md on demand (PROGRESSIVE)
;; =============================================================================

(defn- skill-result
  [nm]
  (if-let [s (d/skill-by-name nm)]
    {:name        (:name s)
     :description (:description s)
     :body        (:body s)
     :dir         (:dir s)
     :resources   (mapv #(str (:dir s) "/" %) (:resources s))}
    {:error     (str "No skill named " (pr-str (str nm)) ".")
     :available (mapv :name (d/skills))}))

(def ^{:doc (str "Load a SKILL defined by another AI coding harness (Claude Code, "
              "opencode, …) by name. The available skills are listed as "
              "`name — description` in the HARNESS SKILLS prompt block; this "
              "loads the FULL instructions on demand. Returns a dict: "
              "{\"name\", \"description\", \"body\": the complete SKILL.md "
              "markdown — FOLLOW it, \"resources\": [absolute paths of bundled "
              "files; cat() the ones you need], \"dir\"}. Unknown name → "
              "{\"error\", \"available\": [names]}.")
       :arglists '([name])}
  skill
  (fn skill-impl [nm]
    (extension/success {:result (skill-result nm)})))

(defn- render-skill
  [r]
  (if (:error r)
    {:summary {:left (ir-strong "SKILL") :right (ir-code "not found")}
     :display (ir-root
                (ir-p (ir-strong "SKILL") "  " (ir-code "not found"))
                (ir-p (str (:error r)))
                (when (seq (:available r))
                  (ir-p (ir-strong "available: ") (str/join ", " (:available r)))))}
    {:summary {:left (ir-strong "SKILL")
               :right (ir-code (str (:name r)
                                 (when (seq (:resources r))
                                   (str " · " (count (:resources r)) " resources"))))}
     :display (ir-root
                (ir-p (ir-strong (str "SKILL " (:name r))))
                (when (seq (:description r)) (ir-p (str (:description r))))
                (ir-code-block "markdown" (str (:body r)))
                (when (seq (:resources r))
                  (ir-p (ir-strong "resources: ") (str/join "  ·  " (:resources r)))))}))

(defn- model-render-skill
  [r]
  (if (:error r)
    (str (:error r) " Available: " (str/join ", " (:available r)))
    (str "# SKILL: " (:name r)
      (when (seq (:description r)) (str "\n" (:description r)))
      "\n\n" (:body r)
      (when (seq (:resources r))
        (str "\n\nBundled resources (cat to read):\n"
          (str/join "\n" (map #(str "- " %) (:resources r))))))))

(def skill-symbol
  (vis/symbol #'skill
    {:symbol          'skill
     :before-fn       (gate-before-fn :vis/harness-skills false "skill")
     :tag             :observation
     :render-fn       render-skill
     :model-render-fn model-render-skill}))

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
    (let [res (lp/sub-loop! env
                {:prompt        (str prompt)
                 :subctx        {:focus (:name a)}
                 :models        (when (:model a) [(:model a)])
                 :system-prompt (:body a)})
          ;; sub_loop derives status from the focus TASK; an agent dispatch seeds
          ;; none, so a completed child turn carries no status string. Read it
          ;; from the turn OUTCOME instead: errored → failed, otherwise the turn
          ;; ran to completion → done.
          status (or (not-empty (str (:status res)))
                   (if (:error res) "failed" "done"))]
      (assoc res :status status :agent (:name a)))
    {:error     (str "No agent named " (pr-str (str nm)) ".")
     :available (mapv :name (d/agents))}))

(def ^{:doc (str "Dispatch a sub-AGENT defined by another AI coding harness "
              "(Claude Code, opencode, …): agent(name, prompt) runs the named "
              "agent as a CHILD loop (a sub_loop) whose system prompt IS that "
              "agent's markdown body, on its declared model, with `prompt` as "
              "the task. Available agents are listed `name — description` in "
              "the HARNESS AGENTS prompt block. The child runs in an isolated "
              "workspace and its edits merge back. Returns {\"agent\", "
              "\"task_id\", \"status\", \"evidence\", \"answer\", "
              "\"changed_files\", \"facts\"}. Unknown name → {\"error\", "
              "\"available\": [names]}. EXPENSIVE — a full child LLM turn; use "
              "it for genuinely delegable sub-tasks, not trivial reads.")
       :arglists '([name prompt])}
  agent
  (fn agent-impl [env nm prompt]
    (extension/success {:result (agent-result env nm prompt)})))

(defn- render-agent
  [r]
  (if (:error r)
    {:summary {:left (ir-strong "AGENT") :right (ir-code "not found")}
     :display (ir-root
                (ir-p (ir-strong "AGENT") "  " (ir-code "not found"))
                (ir-p (str (:error r)))
                (when (seq (:available r))
                  (ir-p (ir-strong "available: ") (str/join ", " (:available r)))))}
    {:summary {:left (ir-strong "AGENT")
               :right (ir-code (str (:agent r) " → " (:status r)))}
     :display (ir-root
                (ir-p (ir-strong (str "AGENT " (:agent r))) "  "
                  (ir-code (str "status " (:status r))))
                (when (seq (:changed_files r))
                  (ir-p (ir-strong "changed: ") (str/join ", " (:changed_files r))))
                (when (seq (str (:answer r))) (ir-code-block "markdown" (str (:answer r)))))}))

(defn- model-render-agent
  [r]
  (if (:error r)
    (str (:error r) " Available: " (str/join ", " (:available r)))
    (str "AGENT " (:agent r) " → status " (:status r)
      (when (seq (:changed_files r))
        (str "\nchanged_files: " (str/join ", " (:changed_files r))))
      (when (seq (str (:evidence r))) (str "\nevidence: " (:evidence r)))
      (when (seq (str (:answer r))) (str "\n\n" (:answer r))))))

(def agent-symbol
  (vis/symbol #'agent
    {:symbol          'agent
     :before-fn       (gate-before-fn :vis/harness-agents true "agent")
     :tag             :mutation
     :render-fn       render-agent
     :model-render-fn model-render-agent}))

;; =============================================================================
;; Prompt fragment — the CHEAP progressive listings (name — description)
;; =============================================================================

(defn- skills-prompt
  [_env]
  (when (toggles/enabled? :vis/harness-skills)
    (let [ss (d/skills)]
      (when (seq ss)
        (str/join "\n"
          (cons "Harness SKILLS available — call skill(\"name\") to load the FULL instructions on demand:"
            (for [s ss] (str "  " (:name s) " — " (clip (:description s) 180)))))))))

(defn- agents-prompt
  [_env]
  (when (toggles/enabled? :vis/harness-agents)
    (let [as (d/agents)]
      (when (seq as)
        (str/join "\n"
          (cons "Harness AGENTS available — call agent(\"name\", \"task prompt\") to delegate to a child loop (the agent's body is its system prompt; EXPENSIVE):"
            (for [a as] (str "  " (:name a) " — " (clip (:description a) 180)))))))))

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
    {:ext/name        "foundation-harness"
     :ext/description "Harness compatibility layer: discover + use the skills and agents other AI coding harnesses (Claude Code, opencode, …) define on disk. skill(name) loads a full SKILL.md on demand; agent(name, prompt) dispatches a sub-agent as a sub_loop child. Toggles :vis/harness-skills / :vis/harness-agents, both ON by default."
     :ext/version     "0.1.0"
     :ext/author      "Blockether"
     :ext/owner       "vis"
     :ext/license     "Apache-2.0"
     :ext/kind        "foundation"
     ;; Active when EITHER half is on; each verb's before-fn gates its OWN
     ;; toggle, so they switch independently. Both off → the extension is
     ;; inactive and `sync-active-extension-symbols!` removes both bare verbs.
     :ext/activation-fn (fn [_env] (or (toggles/enabled? :vis/harness-skills)
                                     (toggles/enabled? :vis/harness-agents)))
     ;; builtin? → symbols bind BARE (skill / agent, not harness_skill).
     :ext/engine      {:ext.engine/builtin? true
                       :ext.engine/symbols  [skill-symbol agent-symbol]}
     :ext/prompt      harness-prompt}))

(vis/register-extension! vis-extension)
