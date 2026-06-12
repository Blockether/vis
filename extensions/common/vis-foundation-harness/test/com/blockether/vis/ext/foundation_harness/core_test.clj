(ns com.blockether.vis.ext.foundation-harness.core-test
  (:require
   [clojure.string :as str]
   [com.blockether.vis.ext.foundation-harness.core :as core]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.toggles :as toggles]
   [lazytest.core :refer [defdescribe it expect]]))

(def ^:private ok-skill
  {:name "setup-pre-commit" :description "Set up Husky hooks."
   :body "# Setup\nRun husky init." :dir "/skills/setup-pre-commit"
   :resources ["/skills/setup-pre-commit/scripts/init.sh"]})

(def ^:private bad-skill
  {:error "No skill named \"nope\"." :available ["a" "b"]})

(defdescribe model-render-skill-test
  (it "puts the FULL body + resource paths in front of the model"
    (let [s ((deref #'core/model-render-skill) ok-skill)]
      (expect (str/includes? s "# SKILL: setup-pre-commit"))
      (expect (str/includes? s "Run husky init."))
      (expect (str/includes? s "/skills/setup-pre-commit/scripts/init.sh"))))
  (it "an unknown skill renders the error + available names, never throws"
    (let [s ((deref #'core/model-render-skill) bad-skill)]
      (expect (str/includes? s "No skill named"))
      (expect (str/includes? s "a, b")))))

(defdescribe render-skill-test
  (it "returns the {:summary :display} channel contract for a hit"
    (let [r ((deref #'core/render-skill) ok-skill)]
      (expect (contains? r :summary))
      (expect (contains? r :display))))
  (it "renders a not-found card without throwing"
    (let [r ((deref #'core/render-skill) bad-skill)]
      (expect (contains? r :display)))))

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

(defdescribe extension-shape-test
  (it "binds the bare skill verb (builtin?, no alias) and advertises a prompt"
    (let [e core/vis-extension]
      (expect (= "foundation-harness" (:ext/name e)))
      (expect (true? (get-in e [:ext/engine :ext.engine/builtin?])))
      (expect (nil? (get-in e [:ext/engine :ext.engine/alias])))
      (expect (fn? (:ext/prompt e))))))
