(ns com.blockether.vis.internal.foundation.harness.core-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.foundation.harness.core :as core]
            [com.blockether.vis.internal.foundation.harness.discovery :as d]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.workspace.core :as workspace]
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

        (expect (str/includes? text "doc(\"demo\")"))
        (expect (str/includes? text "- /x/ref.md"))))
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
(defdescribe slash-skill-statelessness-test
             (it "expands to a POINTER, never to the body"
                 (let [s {:name "demo" :description "d" :body "BODY" :dir "/x" :resources []}]
                   (expect (not (str/includes? (skill-template-text s "do x") "BODY")))))
             (it "records nothing on the session context"
                 (with-redefs [d/skills (constantly
                                          [{:name "demo" :description "d" :body "BODY" :dir "/x"}])]
                   (let [ca (atom {"session_turn" 3})
                         expand (:expand-fn (first (skill-template-entries)))]

                     (expand {:ctx-atom ca} "do x")
                     (expand {:ctx-atom ca} "do y")
                     (expect (= {"session_turn" 3} @ca))))))

(defdescribe verb-shape-test
             ;; There is NO skill verb: a skill is retrieved with `doc(name)` and
             ;; filtered by name with `apropos(pattern)`, so nothing binds the name
             ;; `skill` and nothing activates.
             (it "no skill verb is bound anywhere"
                 (expect (nil? (first (filter #(= 'skill (:ext.symbol/symbol %))
                                              (mapcat extension/ext-symbols
                                                      [core/vis-extension])))))
                 (expect (not (contains? (extension/builtin-sandbox-bindings (fn []
                                                                               nil))
                                         'skill)))))
