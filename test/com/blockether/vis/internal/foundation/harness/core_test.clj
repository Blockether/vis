(ns com.blockether.vis.internal.foundation.harness.core-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.foundation.harness.core :as core]
            [com.blockether.vis.internal.foundation.harness.discovery :as d]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.workspace :as workspace]
            [lazytest.core :refer [defdescribe it expect]]))

(def ^:private skill-template-text @#'core/skill-template-text)

(def ^:private skill-template-entries @#'core/skill-template-entries)

(defdescribe
  skill-template-text-test
  (it "slash skill expansion names the skill, how to read it and its resource paths"
      (let [s
            {:name "demo" :description "d" :body "BODY" :dir "/x" :resources ["ref.md"]}

            text
            (skill-template-text s "do x")]

        (expect (str/includes? text "Use the skill \"demo\""))
        (expect (str/includes? text "doc(\"demo\")"))
        (expect (str/includes? text "- /x/ref.md"))
        (expect (str/includes? text "Task: do x"))))
  (it "names skill slash templates under the skill: namespace"
      (with-redefs [d/skills (constantly [{:name "demo" :description "Demo skill"}])]
        (let [entry (first (skill-template-entries))]
          (expect (= "skill:demo" (:name entry)))
          (expect (str/includes? (:description entry) "demo")))))
  (it "carries a nested skill's owning project into its prompt template"
      (with-redefs [d/skills (constantly
                               [{:name "demo" :description "d" :project-root "/repo/apps/demo"}])]
        (expect (= "/repo/apps/demo" (:project-root (first (skill-template-entries)))))))
  ;; Regression: `/skill:impeccable` typed from the repository root expanded to
  ;; apps/vis-companion's SKILL.md with nothing in the message saying the skill
  ;; belonged to that app, so its relative instructions were followed at the root.
  (it "states the owning project it re-roots the turn on"
      (let [s {:name "demo"
               :description "d"
               :body "BODY"
               :dir "/repo/apps/demo/.agents/skills/demo"
               :project-root "/repo/apps/demo"
               :resources []}]
        (expect (str/includes? (skill-template-text s "") "/repo/apps/demo"))))
  (it "a skill owned by the session's own root carries no owner sentence"
      (let [s {:name "demo"
               :description "d"
               :body "BODY"
               :dir ".vis/skills/demo"
               :project-root (.getCanonicalPath (workspace/cwd))
               :resources []}]
        (expect (not (str/includes? (skill-template-text s "") "belongs to the project at")))))
  (it "the cheap prompt listing tags a nested skill with its project"
      (with-redefs [d/skills (constantly [{:name "demo"
                                           :description "Demo skill"
                                           :project-root (str (.getCanonicalPath (workspace/cwd))
                                                              "/apps/demo")}])]
        (expect (str/includes? ((deref #'core/skills-prompt) {}) "demo [apps/demo]")))))

;; Regression: `/skill:<name>` pasted the whole SKILL.md into the user message and
;; kept a per-session ledger of what it had pasted, so a second `/skill:<name>` told
;; the model the body was "already in this conversation" — a claim nothing
;; could keep once that text folded away.
(defdescribe
  slash-skill-statelessness-test
  (it "expands to a POINTER, never to the body"
      (let [s {:name "demo" :description "d" :body "BODY" :dir "/x" :resources []}]
        (expect (not (str/includes? (skill-template-text s "do x") "BODY")))))
  (it "every invocation of the same skill expands identically"
      (let [s {:name "demo" :description "d" :body "BODY" :dir "/x" :resources ["ref.md"]}]
        (expect (= (skill-template-text s "do x") (skill-template-text s "do x")))
        (expect (not (str/includes? (skill-template-text s "do y") "injected at turn")))))
  (it "records nothing on the session context"
      (with-redefs [d/skills (constantly [{:name "demo" :description "d" :body "BODY" :dir "/x"}])]
        (let [ca (atom {"session_turn" 3})
              expand (:expand-fn (first (skill-template-entries)))]

          (expand {:ctx-atom ca} "do x")
          (expand {:ctx-atom ca} "do y")
          (expect (= {"session_turn" 3} @ca))))))

(defdescribe skills-prompt-test
             (it "skills-prompt is a string listing skills when any exist"
                 (with-redefs [d/skills (constantly [{:name "demo" :description "Demo skill"}])]
                   (expect (string? ((deref #'core/skills-prompt) {})))))
             ;; A skill is a DOCUMENT, not a verb: the cheap listing may only point at
             ;; the retrieval verbs, never at an activation that no longer exists.
             (it "the listing points at doc/apropos and never at an activation verb"
                 (with-redefs [d/skills (constantly [{:name "demo" :description "Demo skill"}])]
                   (let [p ((deref #'core/skills-prompt) {})]
                     (expect (str/includes? p "doc(\"name\")"))
                     (expect (str/includes? p "apropos(text)"))
                     (expect (not (str/includes? p "skill(")))
                     (expect (not (str/includes? p "activates")))))))

(defdescribe verb-shape-test
             ;; There is NO skill verb: a skill is retrieved with `doc(name)` and
             ;; searched with `apropos(text)`, so nothing binds the name `skill` and
             ;; nothing activates.
             (it "no skill verb is bound anywhere"
                 (expect (nil? (first (filter #(= 'skill (:ext.symbol/symbol %))
                                              (mapcat extension/ext-symbols
                                                      [core/vis-extension])))))
                 (expect (not (contains? (extension/builtin-sandbox-bindings (fn []
                                                                               nil))
                                         'skill)))))

(defdescribe extension-shape-test
             (it "binds NO verb — skills are documents, reached with doc/apropos only"
                 (let [e core/vis-extension]
                   (expect (= "foundation-harness" (:ext/name e)))
                   ;; `vis/extension` normalizes :ext/engine to an empty map even
                   ;; when the key is absent, so assert on the SYMBOLS, not the key.
                   (expect (empty? (extension/ext-symbols e)))
                   (expect (empty? (get-in e [:ext/engine :ext.engine/symbols])))
                   (expect (fn? (:ext/prompt-fn e)))
                   ;; always active now — no toggle gate
                   (expect ((:ext/activation-fn e) {})))))
