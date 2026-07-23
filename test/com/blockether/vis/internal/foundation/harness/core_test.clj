(ns com.blockether.vis.internal.foundation.harness.core-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.foundation.harness.core :as core]
            [com.blockether.vis.internal.foundation.harness.discovery :as d]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.loop :as lp]
            [lazytest.core :refer [defdescribe it expect]]))

(def ^:private skill-result @#'core/skill-result)

(def ^:private skill-template-text @#'core/skill-template-text)

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

(defdescribe skill-template-text-test
             (it "slash skill expansion injects the body and bundled resource paths"
                 (let
                   [s {:name "demo" :description "d" :body "BODY" :dir "/x" :resources ["ref.md"]}]
                   (with-redefs
                     [d/skill-by-name (fn [_]
                                        s)]
                     (let [text (skill-template-text {} s "do x")]
                       (expect (str/includes? text "BODY"))
                       (expect (str/includes? text "- /x/ref.md"))
                       (expect (str/includes? text "Task: do x")))))))

(defdescribe
  skill-native-tool-test
  (let [exts [core/vis-extension]]
    (it "skill is a native tool (schema + handler + render), NOT bound into the env"
        (expect (some #(= "skill" (:name %)) (extension/native-tool-schemas exts)))
        (expect (fn? (get (extension/native-tool-handlers exts) "skill")))
        (let
          [entry (first (filter #(= 'skill (:ext.symbol/symbol %))
                                (mapcat extension/ext-symbols exts)))]
          (expect (= false (:ext.symbol/engine-bound? entry)))
          (expect (not (extension/symbol-bound? entry)))
          ;; STRONG flat form: schema/render/colour all on the SYMBOL, no :native-tool map
          (expect (true? (:ext.symbol/native-tool? entry)))
          (expect (map? (:ext.symbol/schema entry)))
          (expect (nil? (:ext.symbol/native-tool entry)))
          (expect (fn? (:ext.symbol/render entry)))
          (expect (= :tool-color/meta (:ext.symbol/color-role entry)))))
    (it "renderer/colour resolve; native description stays compact and schema owns inputs"
        (expect (fn? (get (extension/native-tool-renderers exts) "skill")))
        (expect (= :tool-color/meta (get (extension/native-tool-color-roles exts) "skill")))
        (let [schema (first (filter #(= "skill" (:name %)) (extension/native-tool-schemas exts)))]
          (expect (str/includes? (:description schema) "already-active receipt"))
          (expect (not (str/includes? (:description schema) "SKILLS block")))
          (expect (false? (get-in schema [:schema :additionalProperties])))))
    (it "the handler activates once and returns a compact receipt on repeat"
        (with-redefs
          [d/skill-by-name (fn [_]
                             {:name "demo" :body "B" :description "d" :dir "/x" :resources []})]
          (let
            [h (get (extension/native-tool-handlers exts) "skill")
             env (skill-env {})
             r1 (h env {"name" "demo"})
             r2 (h env {"name" "demo"})]

            (expect (= "B" (get r1 "body")))
            (expect (= "already-active" (get r2 "status")))
            (expect (not (contains? r2 "body"))))))
    (it "skill is unconditionally advertised + dispatchable (no toggle gate)"
        (expect (some #(= "skill" (:name %)) (extension/native-tool-schemas exts nil)))
        (expect (contains? (extension/native-tool-handlers exts nil) "skill")))))

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
