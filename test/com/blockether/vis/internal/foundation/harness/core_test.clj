(ns com.blockether.vis.internal.foundation.harness.core-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.foundation.harness.core :as core]
            [com.blockether.vis.internal.foundation.harness.discovery :as d]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.workspace :as workspace]
            [lazytest.core :refer [defdescribe it expect]]))

(def ^:private skill-result @#'core/skill-result)

(def ^:private skill-template-text @#'core/skill-template-text)

(def ^:private skill-template-entries @#'core/skill-template-entries)

(defn- skill-env
  ([ctx] (skill-env ctx 1 1))
  ([ctx turn iter]
   {:ctx-atom (atom ctx)
    :turn-state-atom (atom {:turn-position turn :iteration iter :form-idx 0})}))

(defdescribe
  skill-result-test
  (it "an unknown name returns {\"error\" \"available\"}"
      (let [r (skill-result {} "definitely-not-a-real-skill-zzz")]
        (expect (string? (get r "error")))
        (expect (vector? (get r "available")))))
  (it "returns the full body once, then a compact receipt in the same live iteration"
      (with-redefs
        [d/skill-by-name (fn [_]
                           {:name "demo" :description "d" :body "BODY" :dir "/x" :resources []})]
        (let
          [env (skill-env {})
           r1 (skill-result env "demo")
           r2 (skill-result env "demo")]

          (expect (= "BODY" (get r1 "body")))
          (expect (= "already-active" (get r2 "status")))
          (expect (not (contains? r2 "body")))
          (expect (= "t1/i1" (get r2 "scope"))))))
  (it "dedupes only from the post-fold live-wire index, not a stale durable pointer"
      (with-redefs
        [d/skill-by-name (fn [_]
                           {:name "demo" :description "d" :body "BODY" :dir "/x" :resources []})]
        (let
          [digest (extension/sha256-hex "BODY")
           stale {"session_active_skills" {"demo" {"name" "demo" "digest" digest "scope" "t1/i1"}}}
           env (skill-env stale 2 1)
           r (skill-result env "demo")]

          ;; The old DB pointer is not on this turn's wire. Rehydrate
          ;; exactly once and move the durable pointer to this scope.
          (expect (= "BODY" (get r "body")))
          (expect (= "t2/i1"
                     (get-in @(get env :ctx-atom) ["session_active_skills" "demo" "scope"]))))))
  (it "a matching post-fold live activation returns a receipt; a changed digest reactivates"
      (let
        [body
         (atom "BODY")

         digest
         (extension/sha256-hex "BODY")

         env
         (skill-env
           {"engine_live_skill_activations" {"demo" {"name" "demo" "digest" digest "scope" "t1/i1"}}
            "session_active_skills" {"demo" {"name" "demo" "digest" digest "scope" "t1/i1"}}}
           1
           2)]

        (with-redefs
          [d/skill-by-name (fn [_]
                             {:name "demo" :description "d" :body @body :dir "/x" :resources []})]
          (expect (= "already-active" (get (skill-result env "demo") "status")))
          (reset! body "BODY v2")
          (expect (= "BODY v2" (get (skill-result env "demo") "body")))
          (expect (= (extension/sha256-hex "BODY v2")
                     (get-in @(get env :ctx-atom) ["session_active_skills" "demo" "digest"])))))))

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
             ;; Regression: a repository-root session that activated a nested project's
             ;; skill (apps/vis-companion's `impeccable`) received a payload of
             ;; {name description body cwd resources} only — no key and no sentence said
             ;; the skill belonged to that app, so the model kept working at the root.
             (it "the activation payload names the owning project and where to work"
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

(defdescribe
  skill-activation-is-a-session-effect-test
  ;; `skill` is not a lookup: activation MARKS the skill on the session, resolves its
  ;; owning project root and bundled resources, and survives the iteration. Reading a
  ;; skill is `doc(name)`; working under it is `skill(name)`.
  (let [exts [core/vis-extension]]
    (it "is a bare Python verb in the sandbox, handed the live env"
        (let
          [entry (first (filter #(= 'skill (:ext.symbol/symbol %))
                                (mapcat extension/ext-symbols exts)))]
          ;; Engine-bound (the default): a python_execution block can activate a
          ;; skill inside a `gather(...)` batch.
          (expect (nil? (:ext.symbol/engine-bound? entry)))
          (expect (true? (:ext.symbol/inject-env? entry)))
          (expect (contains? (extension/builtin-sandbox-bindings (fn []
                                                                   nil))
                             'skill))))
    (it "activates once and answers a compact receipt on repeat"
        (with-redefs
          [d/skill-by-name (fn [_]
                             {:name "demo" :body "B" :description "d" :dir "/x" :resources []})]
          (let
            [env (skill-env {})
             r1 (core/skill env {"name" "demo"})
             r2 (core/skill env {"name" "demo"})]

            (expect (= "B" (get (:result r1) "body")))
            (expect (= "already-active" (get (:result r2) "status")))
            (expect (not (contains? (:result r2) "body"))))))
    (it "takes kwargs or a bare name"
        ;; `skill(name="x")` folds its kwargs into ONE trailing dict at the GraalPy
        ;; boundary; `skill("x")` stays positional. Both bind identically, and the
        ;; bound verb returns the canonical envelope the invoker asserts on.
        (with-redefs
          [d/skill-by-name (fn [_]
                             {:name "demo" :body "B" :description "d" :dir "/x" :resources []})]
          (let
            [kw (core/skill (skill-env {}) {"name" "demo"})
             pos (core/skill (skill-env {}) "demo")]

            (expect (extension/envelope-success? kw))
            (expect (= "B" (get (:result kw) "body")))
            (expect (= "B" (get (:result pos) "body"))))))))

(defdescribe skills-prompt-test
             (it "skills-prompt is a string listing skills when any exist"
                 (with-redefs [d/skills (constantly [{:name "demo" :description "Demo skill"}])]
                   (expect (string? ((deref #'core/skills-prompt) {}))))))

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
             (let
               [skill-entry (first (filter #(= 'skill (:ext.symbol/symbol %))
                                           (mapcat extension/ext-symbols [core/vis-extension])))]
               (it "skill verb is unconditionally active (no toggle gate)"
                   (expect (extension/symbol-active? skill-entry nil)))
               (it
                 "agent verb is unconditionally active and declares :inject-env? with no before-fn"
                 (expect (= true (:ext.symbol/inject-env? core/agent-symbol)))
                 (expect (nil? (:ext.symbol/before-fn core/agent-symbol)))
                 (expect (extension/symbol-active? core/agent-symbol nil)))))

(defdescribe extension-shape-test
             (it
               "binds BOTH bare verbs (skill + agent), builtin? no alias, prompt fn, always active"
               (let
                 [e
                  core/vis-extension

                  syms
                  (get-in e [:ext/engine :ext.engine/symbols])]

                 (expect (= "foundation-harness" (:ext/name e)))
                 (expect (true? (get-in e [:ext/engine :ext.engine/builtin?])))
                 (expect (nil? (get-in e [:ext/engine :ext.engine/alias])))
                 (expect (= 2 (count syms)))
                 (expect (fn? (:ext/prompt-fn e)))
                 ;; always active now — no toggle gate
                 (expect ((:ext/activation-fn e) {})))))
