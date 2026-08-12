(ns com.blockether.vis.internal.foundation.harness.core-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.foundation.harness.core :as core]
            [com.blockether.vis.internal.foundation.harness.discovery :as d]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.workspace :as workspace]
            [lazytest.core :refer [defdescribe it expect]]))

(def ^:private skill-template-text @#'core/skill-template-text)

(def ^:private skill-template-entries @#'core/skill-template-entries)

(defdescribe
  skill-template-text-test
  (it "slash skill expansion injects the body and bundled resource paths"
      (let [s {:name "demo" :description "d" :body "BODY" :dir "/x" :resources ["ref.md"]}]
        (with-redefs
          [d/skill-by-name (fn [_]
                             s)]
          (let [text (skill-template-text {} s "do x")]
            (expect (str/includes? text "BODY"))
            (expect (str/includes? text "- /x/ref.md"))
            (expect (str/includes? text "Task: do x"))))))
  (it "carries a nested skill's owning project into its prompt template"
      (with-redefs
        [d/skills (constantly [{:name "demo" :description "d" :project-root "/repo/apps/demo"}])]
        (expect (= "/repo/apps/demo" (:project-root (first (skill-template-entries)))))))
  ;; Regression: `/impeccable` typed from the repository root expanded to
  ;; apps/vis-companion's SKILL.md with nothing in the message saying the skill
  ;; belonged to that app, so its relative instructions were followed at the root.
  (it "states the owning project before the body it expands"
      (let
        [s
         {:name "demo"
          :description "d"
          :body "BODY"
          :dir "/repo/apps/demo/.agents/skills/demo"
          :project-root "/repo/apps/demo"
          :resources []}

         text
         (skill-template-text {} s "")]

        (expect (str/includes? text "/repo/apps/demo"))
        (expect (< (.indexOf ^String text "/repo/apps/demo") (.indexOf ^String text "BODY"))))))

(defdescribe skill-ownership-test
             ;; Regression: a repository-root session expanding a nested project's
             ;; skill (apps/vis-companion's `impeccable`) received a payload of
             ;; {name description body cwd resources} only — no key and no sentence said
             ;; the skill belonged to that app, so the model kept working at the root.
             (it "the slash payload names the owning project and where to work"
                 (let
                   [payload ((deref #'core/skill-payload)
                              {:name "demo"
                               :description "d"
                               :body "BODY"
                               :dir "/repo/apps/demo/.agents/skills/demo"
                               :project-root "/repo/apps/demo"
                               :resources []})]
                   (expect (= "/repo/apps/demo" (get payload "project_root")))
                   (expect (str/includes? (str (get payload "note")) "/repo/apps/demo"))))
             (it "a skill owned by the session's own root carries no owner note"
                 (let
                   [payload ((deref #'core/skill-payload)
                              {:name "demo"
                               :description "d"
                               :body "BODY"
                               :dir ".vis/skills/demo"
                               :project-root (.getCanonicalPath (workspace/cwd))
                               :resources []})]
                   (expect (nil? (get payload "note")))))
             (it "the cheap prompt listing tags a nested skill with its project"
                 (with-redefs
                   [d/skills (constantly [{:name "demo"
                                           :description "Demo skill"
                                           :project-root (str (.getCanonicalPath (workspace/cwd))
                                                              "/apps/demo")}])]
                   (expect (str/includes? ((deref #'core/skills-prompt) {}) "demo [apps/demo]")))))

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

;; ── agents surface (slice 3) ──────────────────────────────────────────────

(defdescribe
  agent-verb-test
  (it "an unknown agent returns a success envelope with \"error\" + \"available\" (no sub_loop run)"
      (let
        [env
         (core/agent {} "definitely-not-a-real-agent-zzz" "do x")

         r
         (:result env)]

        (expect (extension/envelope-success? env))
        (expect (string? (get r "error")))
        (expect (vector? (get r "available")))))
  (it "dispatch threads the agent's BODY as the child system prompt and its MODEL as a vector"
      (let [captured (atom nil)]
        (with-redefs
          [d/agent-by-name (fn [nm]
                             (when (= "code-reviewer" nm)
                               {:name "code-reviewer"
                                :description "Review code"
                                :model "review-model"
                                :body "review system prompt"}))
           lp/sub-loop! (fn [_env opts]
                          (reset! captured opts)
                          {:task_id (get-in opts [:subctx :focus])
                           :status "done"
                           :answer "child done"
                           :changed_files ["f.txt"]
                           :facts {}
                           :evidence "ok"})]

          (let [r (:result (core/agent {} "code-reviewer" "review this"))]
            (expect (= "code-reviewer" (get r "agent")))
            (expect (= "done" (get r "status")))
            (expect (= ["f.txt"] (get r "changed_files")))
            ;; the child got the agent's markdown body as :system-prompt
            (expect (seq (str (:system-prompt @captured))))
            ;; model preference is ALWAYS A VECTOR
            (expect (vector? (:models @captured)))
            (expect (= "review this" (:prompt @captured)))
            (expect (= "code-reviewer" (get-in @captured [:subctx :focus])))))))
  (it "a completed child with no status string reports done; an errored one failed"
      (with-redefs
        [d/agent-by-name
         (fn [nm]
           (when (= "code-reviewer" nm) {:name "code-reviewer" :body "review system prompt"}))

         lp/sub-loop!
         (fn [_ _]
           {:status "" :answer "OK" :changed_files []})]

        (expect (= "done" (get (:result (core/agent {} "code-reviewer" "x")) "status"))))
      (with-redefs
        [d/agent-by-name
         (fn [nm]
           (when (= "code-reviewer" nm) {:name "code-reviewer" :body "review system prompt"}))

         lp/sub-loop!
         (fn [_ _]
           {:status "" :error "boom"})]

        (expect (= "failed" (get (:result (core/agent {} "code-reviewer" "x")) "status"))))
      (with-redefs
        [d/agent-by-name
         (fn [nm]
           (when (= "code-reviewer" nm) {:name "code-reviewer" :body "review system prompt"}))

         lp/sub-loop!
         (fn [_ _]
           {:status "rejected"})]

        ;; an explicit child status is preserved, never overwritten
        (expect (= "rejected" (get (:result (core/agent {} "code-reviewer" "x")) "status"))))))

(defdescribe verb-shape-test
             (it "agent verb is unconditionally active and declares :inject-env? with no before-fn"
                 (expect (= true (:ext.symbol/inject-env? core/agent-symbol)))
                 (expect (nil? (:ext.symbol/before-fn core/agent-symbol)))
                 (expect (extension/symbol-active? core/agent-symbol nil)))
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
             (it "binds the one bare verb (agent), builtin? no alias, prompt fn, always active"
                 (let
                   [e
                    core/vis-extension

                    syms
                    (get-in e [:ext/engine :ext.engine/symbols])]

                   (expect (= "foundation-harness" (:ext/name e)))
                   (expect (true? (get-in e [:ext/engine :ext.engine/builtin?])))
                   (expect (nil? (get-in e [:ext/engine :ext.engine/alias])))
                   (expect (= 1 (count syms)))
                   (expect (fn? (:ext/prompt-fn e)))
                   ;; always active now — no toggle gate
                   (expect ((:ext/activation-fn e) {})))))
