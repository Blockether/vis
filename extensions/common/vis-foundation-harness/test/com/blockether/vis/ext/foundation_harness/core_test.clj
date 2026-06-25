(ns com.blockether.vis.ext.foundation-harness.core-test
  (:require
   [com.blockether.vis.ext.foundation-harness.core :as core]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.loop :as lp]
   [com.blockether.vis.internal.toggles :as toggles]
   [lazytest.core :refer [defdescribe it expect]]))

(defdescribe skill-verb-test
  (it "an unknown name returns a success envelope whose result carries :error + :available"
    (let [env (core/skill "definitely-not-a-real-skill-zzz")
          result (:result env)]
      (expect (extension/envelope-success? env))
      (expect (string? (:error result)))
      (expect (vector? (:available result))))))

(defdescribe toggle-ownership-test
  (it "the :vis/harness-skills toggle is registered (by loading THIS layer), default ON, General/Tools group"
    (let [spec (first (filter #(= :vis/harness-skills (:id %)) (toggles/registered-toggles)))]
      (expect (some? spec))
      (expect (= true (:default spec)))
      ;; :owner :vis :group :tools → General → Feature Toggles, beside shell-tool
      (expect (= :vis (:owner spec)))
      (expect (= :tools (:group spec)))))
  (it "skills-prompt is a string (lists skills when ON, blank when OFF)"
    (expect (string? ((deref #'core/skills-prompt) {})))))

;; ── agents surface (slice 3) ──────────────────────────────────────────────

(defdescribe agent-verb-test
  (it "an unknown agent returns a success envelope with :error + :available (no sub_loop run)"
    (let [env (core/agent {} "definitely-not-a-real-agent-zzz" "do x")
          r (:result env)]
      (expect (extension/envelope-success? env))
      (expect (string? (:error r)))
      (expect (vector? (:available r)))))

  (it "dispatch threads the agent's BODY as the child system prompt and its MODEL as a vector"
    (let [captured (atom nil)]
      (with-redefs [lp/sub-loop! (fn [_env opts]
                                   (reset! captured opts)
                                   {:task_id (get-in opts [:subctx :focus])
                                    :status "done" :answer "child done"
                                    :changed_files ["f.txt"] :facts {} :evidence "ok"})]
        (let [r (:result (core/agent {} "code-reviewer" "review this"))]
          (expect (= "code-reviewer" (:agent r)))
          (expect (= "done" (:status r)))
          (expect (= ["f.txt"] (:changed_files r)))
          ;; the child got the agent's markdown body as :system-prompt
          (expect (seq (str (:system-prompt @captured))))
          ;; model preference is ALWAYS A VECTOR
          (expect (vector? (:models @captured)))
          (expect (= "review this" (:prompt @captured)))
          (expect (= "code-reviewer" (get-in @captured [:subctx :focus])))))))

  (it "a completed child with no status string reports done; an errored one failed"
    (with-redefs [lp/sub-loop! (fn [_ _] {:status "" :answer "OK" :changed_files []})]
      (expect (= "done" (:status (:result (core/agent {} "code-reviewer" "x"))))))
    (with-redefs [lp/sub-loop! (fn [_ _] {:status "" :error "boom"})]
      (expect (= "failed" (:status (:result (core/agent {} "code-reviewer" "x"))))))
    (with-redefs [lp/sub-loop! (fn [_ _] {:status "rejected"})]
      ;; an explicit child status is preserved, never overwritten
      (expect (= "rejected" (:status (:result (core/agent {} "code-reviewer" "x"))))))))

(defdescribe per-verb-gating-test
  ;; both verbs share one extension; each before-fn gates its OWN toggle so a
  ;; disabled verb refuses even while the other is on.
  (it "the skill before-fn refuses when :vis/harness-skills is OFF"
    (let [bf ((deref #'core/gate-before-fn) :vis/harness-skills false "skill")]
      (with-redefs [toggles/enabled? (fn [id] (not= id :vis/harness-skills))]
        (let [out (bf {} identity ["x"])]
          (expect (contains? out :result))
          (expect (extension/envelope-failure? (:result out)))))))
  (it "the agent before-fn passes through (env injected) when :vis/harness-agents is ON"
    (let [bf ((deref #'core/gate-before-fn) :vis/harness-agents true "agent")]
      (with-redefs [toggles/enabled? (fn [_] true)]
        (let [out (bf {:e 1} identity ["nm" "p"])]
          (expect (= [{:e 1} "nm" "p"] (:args out))))))))

(defdescribe agents-toggle-test
  (it "the :vis/harness-agents toggle is registered by this layer, default ON"
    (let [spec (first (filter #(= :vis/harness-agents (:id %)) (toggles/registered-toggles)))]
      (expect (some? spec))
      (expect (= true (:default spec)))
      (expect (= :vis (:owner spec))))))

(defdescribe extension-shape-test
  (it "binds BOTH bare verbs (skill + agent), builtin? no alias, prompt fn, active if either toggle on"
    (let [e core/vis-extension
          syms (get-in e [:ext/engine :ext.engine/symbols])]
      (expect (= "foundation-harness" (:ext/name e)))
      (expect (true? (get-in e [:ext/engine :ext.engine/builtin?])))
      (expect (nil? (get-in e [:ext/engine :ext.engine/alias])))
      (expect (= 2 (count syms)))
      (expect (fn? (:ext/prompt-fn e)))
      ;; active while EITHER toggle is on; inactive only when BOTH are off
      (with-redefs [toggles/enabled? (fn [_] false)]
        (expect (not ((:ext/activation-fn e) {}))))
      (with-redefs [toggles/enabled? (fn [id] (= id :vis/harness-agents))]
        (expect ((:ext/activation-fn e) {}))))))
