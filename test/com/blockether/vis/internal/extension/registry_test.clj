(ns com.blockether.vis.internal.extension.registry-test
  "Provider descriptors are checked before they enter the registry, including the
   routing policy a provider owns."
  (:require [com.blockether.vis.internal.extension.registry :as registry]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [com.blockether.vis.test-provider-policies :as policies]
            [lazytest.core :refer [defdescribe expect it throws?]]))

(defn- descriptor
  [policy]
  {:provider/id :policy-test :provider/label "Policy Test" :provider/policy policy})

(defdescribe
  provider-policy-validation-test
  (it "accepts every policy the first-party providers declare"
      (doseq [[id policy] policies/first-party]
        (expect (registry/provider? (descriptor policy)) (str id))))
  (it "accepts a descriptor without a policy, and an empty one"
      (expect (registry/provider? {:provider/id :policy-test :provider/label "Policy Test"}))
      (expect (registry/provider? (descriptor {}))))
  (it "rejects a malformed policy before registration"
      (doseq [policy [[:preset-rank 1] {:preset-rank "1"} {:title-rank 1.5} {:initiator-header ""}
                      {:adaptive-reasoning-models "(unclosed"}
                      {:refusal-fallback {:models "(?i)claude" :fallbacks []}}
                      {:refusal-fallback {:models "(?i)claude" :fallbacks '("claude-opus-4-8")}}
                      {:fast-mode {:turn-feature ""}} {:fast-mode "fast"}]]
        (expect (not (registry/provider? (descriptor policy))) (pr-str policy))
        (expect (throws? clojure.lang.ExceptionInfo #(registry/provider (descriptor policy)))))))

;; Issue #410: a machine request binds no project. Machine sign-in then answered
;; unknown-provider for a provider that a project extension registers.
(defdescribe
  project-provider-machine-lookup-test
  (it "finds a provider that only one project registers"
      (let [root
            "/tmp/vis-410-only"

            id
            :issue-410-only]

        (registry/set-project-providers! root [{:provider/id id :provider/label "Only"}] #{})
        (try (expect (= "Only" (:provider/label (registry/provider-by-id id))))
             (expect (= #{(workspace/normalize-root root)} (registry/provider-project-roots id)))
             (binding [workspace/*workspace-root* "/tmp/vis-410-other"]
               (expect (nil? (registry/provider-by-id id))))
             (finally (registry/set-project-providers! root [] #{})))))
  (it "never chooses between two projects that register the same ID"
      (let [roots
            ["/tmp/vis-410-a" "/tmp/vis-410-b"]

            id
            :issue-410-shared]

        (doseq [root roots]
          (registry/set-project-providers! root [{:provider/id id :provider/label root}] #{}))
        (try (expect (nil? (registry/provider-by-id id)))
             (expect (= 2 (count (registry/provider-project-roots id))))
             (binding [workspace/*workspace-root* (first roots)]
               (expect (= (first roots) (:provider/label (registry/provider-by-id id)))))
             (finally (doseq [root roots]
                        (registry/set-project-providers! root [] #{})))))))
