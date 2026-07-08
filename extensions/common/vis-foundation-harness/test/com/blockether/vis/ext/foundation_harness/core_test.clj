(ns com.blockether.vis.ext.foundation-harness.core-test
  (:require [com.blockether.vis.ext.foundation-harness.core :as core]
            [com.blockether.vis.ext.foundation-harness.discovery :as d]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.toggles :as toggles]
            [lazytest.core :refer [defdescribe it expect]]))

(def ^:private skill-result @#'core/skill-result)

(defdescribe
  skill-result-test
  (it "an unknown name returns {\"error\" \"available\"}"
      (let [r (skill-result {} "definitely-not-a-real-skill-zzz")]
        (expect (string? (get r "error")))
        (expect (vector? (get r "available")))))
  (it "loads a skill body ONCE per session, then acks already-loaded (tracked on the ctx, no atom)"
      (with-redefs [d/skill-by-name
                    (fn [_]
                      {:name "demo" :description "d" :body "BODY" :dir "/x" :resources []})]
        (let [ca (atom {}) ;; stands in for the DB-persisted session ctx
              env {:ctx-atom ca}
              r1 (skill-result env "demo")
              r2 (skill-result env "demo")]

          (expect (= "BODY" (get r1 "body")))
          (expect (= #{"demo"} (:session/loaded-skills @ca)))
          (expect (= "already-loaded" (get r2 "status")))
          (expect (not (contains? r2 "body")))))))

(defdescribe
  skill-native-tool-test
  (let [exts [core/vis-extension]]
    (it "skill is a native tool (schema + handler + render), NOT bound into the env"
        (expect (some #(= "skill" (:name %)) (extension/native-tool-schemas exts)))
        (expect (fn? (get (extension/native-tool-handlers exts) "skill")))
        (let [entry (first (filter #(= 'skill (:ext.symbol/symbol %))
                                   (mapcat extension/ext-symbols exts)))]
          (expect (= false (:ext.symbol/engine-bound? entry)))
          (expect (not (extension/symbol-bound? entry)))
          ;; STRONG flat form: schema/render/colour all on the SYMBOL, no :native-tool map
          (expect (true? (:ext.symbol/native-tool? entry)))
          (expect (map? (:ext.symbol/schema entry)))
          (expect (nil? (:ext.symbol/native-tool entry)))
          (expect (fn? (:ext.symbol/render entry)))
          (expect (= :tool-color/meta (:ext.symbol/color-role entry)))))
    (it "renderer/colour resolve through the maps; schema description = the docstring (ONE source)"
        (expect (fn? (get (extension/native-tool-renderers exts) "skill")))
        (expect (= :tool-color/meta (get (extension/native-tool-color-roles exts) "skill")))
        (let [schema (first (filter #(= "skill" (:name %)) (extension/native-tool-schemas exts)))]
          (expect (= (:doc (meta #'core/skill-tool)) (:description schema)))))
    (it "the handler runs the load-once logic"
        (with-redefs [d/skill-by-name
                      (fn [_]
                        {:name "demo" :body "B" :description "d" :dir "/x" :resources []})]
          (let [h (get (extension/native-tool-handlers exts) "skill")
                ca (atom {})
                r1 (h {:ctx-atom ca} {"name" "demo"})
                r2 (h {:ctx-atom ca} {"name" "demo"})]

            (expect (= "B" (get r1 "body")))
            (expect (= "already-loaded" (get r2 "status"))))))
    (it ":active-fn gates advertising + dispatch on the skills toggle"
        (with-redefs [toggles/enabled? (fn [id]
                                         (= id :vis/harness-skills))]
          (expect (some #(= "skill" (:name %)) (extension/native-tool-schemas exts nil)))
          (expect (contains? (extension/native-tool-handlers exts nil) "skill")))
        (with-redefs [toggles/enabled? (fn [_]
                                         false)]
          (expect (not (some #(= "skill" (:name %)) (extension/native-tool-schemas exts nil))))
          (expect (not (contains? (extension/native-tool-handlers exts nil) "skill")))))))

(defdescribe
  toggle-ownership-test
  (it
    "the :vis/harness-skills toggle is registered (by loading THIS layer), default ON, General/Tools group"
    (let [spec (first (filter #(= :vis/harness-skills (:id %)) (toggles/registered-toggles)))]
      (expect (some? spec))
      (expect (= true (:default spec)))
      ;; :owner :vis :group :tools → General → Feature Toggles, beside shell-tool
      (expect (= :vis (:owner spec)))
      (expect (= :tools (:group spec)))))
  (it "skills-prompt is a string (lists skills when ON, blank when OFF)"
      (with-redefs [d/skills (constantly [{:name "demo" :description "Demo skill"}])]
        (expect (string? ((deref #'core/skills-prompt) {}))))))

;; ── agents surface (slice 3) ──────────────────────────────────────────────

(defdescribe
  agent-verb-test
  (it "an unknown agent returns a success envelope with \"error\" + \"available\" (no sub_loop run)"
      (let [env
            (core/agent {} "definitely-not-a-real-agent-zzz" "do x")

            r
            (:result env)]

        (expect (extension/envelope-success? env))
        (expect (string? (get r "error")))
        (expect (vector? (get r "available")))))
  (it "dispatch threads the agent's BODY as the child system prompt and its MODEL as a vector"
      (let [captured (atom nil)]
        (with-redefs [d/agent-by-name (fn [nm]
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
      (with-redefs [d/agent-by-name (fn [nm]
                                      (when (= "code-reviewer" nm)
                                        {:name "code-reviewer" :body "review system prompt"}))
                    lp/sub-loop! (fn [_ _]
                                   {:status "" :answer "OK" :changed_files []})]
        (expect (= "done" (get (:result (core/agent {} "code-reviewer" "x")) "status"))))
      (with-redefs [d/agent-by-name (fn [nm]
                                      (when (= "code-reviewer" nm)
                                        {:name "code-reviewer" :body "review system prompt"}))
                    lp/sub-loop! (fn [_ _]
                                   {:status "" :error "boom"})]
        (expect (= "failed" (get (:result (core/agent {} "code-reviewer" "x")) "status"))))
      (with-redefs [d/agent-by-name (fn [nm]
                                      (when (= "code-reviewer" nm)
                                        {:name "code-reviewer" :body "review system prompt"}))
                    lp/sub-loop! (fn [_ _]
                                   {:status "rejected"})]
        ;; an explicit child status is preserved, never overwritten
        (expect (= "rejected" (get (:result (core/agent {} "code-reviewer" "x")) "status"))))))

(defdescribe per-verb-gating-test
             ;; both verbs share one extension; each :active-fn gates its OWN toggle, so a
             ;; disabled verb is inactive while the other stays on. ONE gate, no before-fn.
             (let [skill-entry (first (filter #(= 'skill (:ext.symbol/symbol %))
                                              (mapcat extension/ext-symbols [core/vis-extension])))]
               (it "skill :active-fn gates :vis/harness-skills"
                   (with-redefs [toggles/enabled? (fn [id]
                                                    (= id :vis/harness-skills))]
                     (expect (extension/symbol-active? skill-entry nil)))
                   (with-redefs [toggles/enabled? (fn [_]
                                                    false)]
                     (expect (not (extension/symbol-active? skill-entry nil))))))
             (it "agent :active-fn gates :vis/harness-agents and declares :inject-env?"
                 (expect (= true (:ext.symbol/inject-env? core/agent-symbol)))
                 (expect (nil? (:ext.symbol/before-fn core/agent-symbol)))
                 (with-redefs [toggles/enabled? (fn [id]
                                                  (= id :vis/harness-agents))]
                   (expect (extension/symbol-active? core/agent-symbol nil)))
                 (with-redefs [toggles/enabled? (fn [_]
                                                  false)]
                   (expect (not (extension/symbol-active? core/agent-symbol nil))))))

(defdescribe agents-toggle-test
             (it "the :vis/harness-agents toggle is registered by this layer, default ON"
                 (let [spec (first (filter #(= :vis/harness-agents (:id %))
                                           (toggles/registered-toggles)))]
                   (expect (some? spec))
                   (expect (= true (:default spec)))
                   (expect (= :vis (:owner spec))))))

(defdescribe
  extension-shape-test
  (it
    "binds BOTH bare verbs (skill + agent), builtin? no alias, prompt fn, active if either toggle on"
    (let [e
          core/vis-extension

          syms
          (get-in e [:ext/engine :ext.engine/symbols])]

      (expect (= "foundation-harness" (:ext/name e)))
      (expect (true? (get-in e [:ext/engine :ext.engine/builtin?])))
      (expect (nil? (get-in e [:ext/engine :ext.engine/alias])))
      (expect (= 2 (count syms)))
      (expect (fn? (:ext/prompt-fn e)))
      ;; active while EITHER toggle is on; inactive only when BOTH are off
      (with-redefs [toggles/enabled? (fn [_]
                                       false)]
        (expect (not ((:ext/activation-fn e) {}))))
      (with-redefs [toggles/enabled? (fn [id]
                                       (= id :vis/harness-agents))]
        (expect ((:ext/activation-fn e) {}))))))
