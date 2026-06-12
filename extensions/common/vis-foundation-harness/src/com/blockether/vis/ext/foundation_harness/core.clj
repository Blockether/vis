(ns com.blockether.vis.ext.foundation-harness.core
  "`harness` compatibility extension — a DROPPABLE classpath plug-in (drop the
   jar, drop the feature) that exposes the AGENTS and SKILLS other AI coding
   HARNESSES (Claude Code, opencode, …) leave on disk to the vis model. The
   sibling of the shell layer's POSIX compat.

   Skills are PROGRESSIVE: the prompt fragment lists every skill as
   `name — description` (cheap — always present while the toggle is ON), and
   `skill(name)` loads the FULL `SKILL.md` + its bundled resource PATHS on
   demand so the model spends tokens only on the one it actually uses.

   The verbs bind BARE (`skill(...)`, not `harness_skill(...)`) via
   `:ext.engine/builtin? true`, alongside the engine verbs. This layer OWNS
   its toggles — they are registered here, not in core — so dropping the jar
   drops the toggles too. (Agents land in the next slice.)"
  (:require
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.toggles :as toggles]
   [com.blockether.vis.ext.foundation-harness.discovery :as d]))

;; =============================================================================
;; Toggles — OWNED by this layer (registered on load), not by core
;; =============================================================================

;; Registered HERE (in the layer), not in internal/toggles.clj — drop the jar,
;; drop the toggle. `:owner :vis :group :tools` places it in Settings' General →
;; Feature Toggles section right beside the shell compatibility toggle (the
;; same home every compat-layer switch gets), not the per-extension settings tab.
(toggles/register-toggle!
  {:id :vis/harness-skills :label "Harness skills (compatibility layer)"
   :description (str "When ON the agent can discover and load SKILLS defined by"
                  " other AI coding harnesses (Claude Code, opencode, …) from"
                  " .claude/skills, ~/.claude/skills, and plugin caches. The"
                  " prompt lists each skill's name + description cheaply;"
                  " skill(name) loads the full SKILL.md + its bundled resource"
                  " paths on demand. ON by default.")
   :default true :owner :vis :group :tools :persist? true})

;; =============================================================================
;; IR helpers (re-exported for terseness)
;; =============================================================================

(def ^:private ir-root extension/ir-root)
(def ^:private ir-p extension/ir-p)
(def ^:private ir-strong extension/ir-strong)
(def ^:private ir-code extension/ir-code)
(def ^:private ir-code-block extension/ir-code-block)

(defn- clip [s n]
  (let [s (str s)] (if (> (count s) n) (str (subs s 0 (max 0 (dec n))) "…") s)))

;; =============================================================================
;; skill(name) — load one SKILL.md on demand
;; =============================================================================

(defn- skill-result
  "Resolve a skill by name to the model-facing dict, or a not-found dict
   carrying the available names so the model can recover."
  [nm]
  (if-let [s (d/skill-by-name nm)]
    {:name        (:name s)
     :description (:description s)
     :body        (:body s)
     :dir         (:dir s)
     ;; absolute paths — the model reads them with the existing cat/read tools
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
  "Channel `{:summary :display}` for a skill load."
  [r]
  (if (:error r)
    {:summary {:left (ir-strong "SKILL") :right (ir-code "not found")}
     :display (ir-root
                (ir-p (ir-strong "SKILL") "  " (ir-code "not found"))
                (ir-p (str (:error r)))
                (when (seq (:available r))
                  (ir-p (ir-strong "available: ") (str/join ", " (:available r)))))}
    {:summary {:left (ir-strong "SKILL") :right (ir-code (:name r))}
     :display (ir-root
                (ir-p (ir-strong (str "SKILL " (:name r))))
                (when (seq (:description r)) (ir-p (str (:description r))))
                (ir-code-block "markdown" (str (:body r)))
                (when (seq (:resources r))
                  (ir-p (ir-strong "resources: ") (str/join "  ·  " (:resources r)))))}))

(defn- model-render-skill
  "Compressed model-facing string: the FULL skill body + resource paths — the
   whole point of `skill(...)` is to put the instructions in front of the model."
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
    {:symbol         'skill
     :tag            :observation
     :render-fn      render-skill
     :model-render-fn model-render-skill}))

;; =============================================================================
;; Prompt fragment — the CHEAP progressive listing (name — description)
;; =============================================================================

(defn- skills-prompt
  "Harness SKILLS surface — only while the toggle is ON (a blank string is
   filtered out of the extensions prompt block, so a disabled layer costs zero
   prompt tokens). Lists name + one-line description; the full body loads via
   skill(name)."
  [_env]
  (if (toggles/enabled? :vis/harness-skills)
    (let [ss (d/skills)]
      (if (empty? ss)
        ""
        (str/join "\n"
          (concat
            ["Harness SKILLS available — call skill(\"name\") to load the FULL instructions on demand:"]
            (for [s ss] (str "  " (:name s) " — " (clip (:description s) 180)))))))
    ""))

;; =============================================================================
;; Extension
;; =============================================================================

(def vis-extension
  (vis/extension
    {:ext/name        "foundation-harness"
     :ext/description "Harness compatibility layer: discover + use the skills (and agents) other AI coding harnesses (Claude Code, opencode, …) define on disk. skill(name) loads a full SKILL.md on demand. Toggle :vis/harness-skills, ON by default."
     :ext/version     "0.1.0"
     :ext/author      "Blockether"
     :ext/owner       "vis"
     :ext/license     "Apache-2.0"
     :ext/kind        "foundation"
     ;; The toggle IS the activation gate: OFF → the extension is inactive, so
     ;; `sync-active-extension-symbols!` REMOVES `skill` from the sandbox
     ;; globals (apropos won't list it, calling raises NameError) and the prompt
     ;; fragment is gone — zero footprint when disabled.
     :ext/activation-fn (fn [_env] (toggles/enabled? :vis/harness-skills))
     ;; builtin? → symbols bind BARE (skill, not harness_skill), like the
     ;; engine verbs cat/rg/ls.
     :ext/engine      {:ext.engine/builtin? true
                       :ext.engine/symbols  [skill-symbol]}
     :ext/prompt      skills-prompt}))

(vis/register-extension! vis-extension)
