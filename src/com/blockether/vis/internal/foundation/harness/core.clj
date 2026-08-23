(ns com.blockether.vis.internal.foundation.harness.core
  "`harness` compatibility layer — a BUILT-IN foundation module (ships in the
   main jar, always present, gated by toggles) that exposes the SKILLS vis'
   own project dir and other AI coding HARNESSES (Claude Code, pi, opencode, the agents
   standard, …) leave on disk to the vis model. The sibling of the shell
   layer's POSIX compat. Vis reads its OWN project-local skills from
   `.vis/skills` (highest precedence).

   - SKILLS are DOCUMENTS, never a verb: the prompt lists every skill
     `name — description` (cheap — always present) and the WHOLE `SKILL.md` is
     one document in the `doc`/`apropos` corpus, so `apropos(text)` finds it and
     `doc(name)` prints it whole. Reading a skill has no session effect: there
     is nothing to activate, nothing to re-read and no activation receipt.

   - The USER's `/skill:<name>` slash is that same document with a POINTER, never a
     copy: it expands to one sentence naming the skill (plus the owning project
     and any bundled resource paths, which the body does not carry) and leaves
     fetching it to the model, which is the only party that knows whether the
     text is still in front of it. No injected body means nothing to remember
     between two `/skill:<name>`s: every skill surface is stateless.

   Skills and commands have no user toggle; the layer is always active."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.prompt-templates :as prompt-templates]
            [com.blockether.vis.internal.workspace :as workspace]
            [com.blockether.vis.internal.foundation.harness.discovery :as d]))

;; Small utilities

(defn- clip
  [s ^long n]
  (let [s (str s)]
    (if (> (count s) n) (str (subs s 0 (max 0 (dec n))) "…") s)))

;; Skill ownership — the nested project a SKILL.md was written for

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
   (`apps/<app>/.agents/skills/<name>`), and such a SKILL.md was
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

;; /skill:<name> — user-invokable skill templates

(defn- skill-template-text
  "Expanded user-message text for a `/skill:<name> [task]` invocation: the SENTENCE
   that names the skill, plus the optional task. Nothing else — no copy of the
   `SKILL.md`, no ledger, no receipt.

   The slash says WHICH skill governs the task; whether the instructions have
   to be fetched is the model's own call, because only the model knows whether
   that text is still in front of it. `doc(<name>)` prints it whole, every
   time, with no session effect — so a second `/skill:<name>` is the same sentence as
   the first and needs nothing remembered between them.

   A skill owned by a NESTED project also says whose it is: the turn is
   re-rooted there (`prompt-templates/expand` carries `:project-root`) and the
   message must not leave that silent. Bundled resource paths are named for the
   same reason — the directory a skill's files live in is not derivable from
   the body `doc` prints."
  [s args]
  (let [note
        (owner-note (owner-root s))

        resources
        (seq (mapv #(str (:dir s) "/" %) (:resources s)))]

    (str "Use the skill \""
         (:name s)
         "\" for this task: read it with doc(\""
         (:name s)
         "\") unless its SKILL.md is already in this conversation, then follow it as written."
         (when note (str "\n\n" note))
         (when resources
           (str "\n\nBundled resources (read them with the file tools as needed):\n"
                (str/join "\n" (map #(str "- " %) resources))))
         (when-not (str/blank? (str args)) (str "\n\nTask: " args)))))

(defn- skill-template-entries
  "Every discovered skill as one dynamic prompt template named `skill:<name>`. A
   repository-local skill carries its owning project root so invocation can run
   relative to that project even when selected from a repository-root session."
  []
  (mapv (fn [s]
          (cond-> {:name (str "skill:" (:name s))
                   :description (str "Load skill "
                                     (:name s)
                                     (when-let [d (not-empty (str (:description s)))]
                                       (str " — " (clip d 140))))
                   :expand-fn (fn [_env args]
                                (skill-template-text s args))}
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

;; `/reload` refresh: force a full rescan of the harness skill source
;; dirs (the marker cache already catches file edits; the hook also covers
;; sources a stat can miss and gives the user an explicit big hammer).
(extension/register-reload-hook! ::discovery d/reload!)

;; Prompt fragment — the CHEAP progressive listings (name — description)

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

(defn- harness-prompt
  "The always-on harness surface: the skill listing, or nothing to say."
  [env]
  (or (skills-prompt env) ""))

(def vis-extension
  (vis/extension
    {:ext/name "foundation-harness"
     :ext/description
     "Discovers on-disk Claude Code/opencode SKILLS: every SKILL.md is a `doc`/`apropos` document. Always available."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     ;; Always active — the skill listing is unconditionally available (its user
     ;; toggle was removed).
     :ext/activation-fn (fn [_env]
                          true)
     :ext/prompt-fn harness-prompt}))

(vis/register-extension! vis-extension)
