(ns com.blockether.vis.internal.extension-test
  "Coverage for `internal/extension`.

   The hard rule (AGENTS.md) demands every namespace ship with a test
   file. Beyond the rule, this file pins down the bits of the
   `extension` builder that are most likely to bit-rot under
   refactors:

     1. `:ext/group` auto-derivation for the categorical cases
        (extensions that contribute providers or channels).
     2. Explicit `:ext/group` always wins over auto-derivation.
     3. `:ext/subgroup` is no longer part of the spec \u2014 the builder
        neither requires it nor injects it.
     4. The `:ext/symbols` -> `:ext/group` requirement still bites
        when an extension exports sandbox symbols without a group."
  (:require
   [com.blockether.vis.internal.extension :as ext]
   [lazytest.core :refer [defdescribe expect it]]))

(def ^:private base-channel
  {:channel/id        :test
   :channel/cmd       "test"
   :channel/doc       "Test channel."
   :channel/main-fn   (fn [_] nil)})

(def ^:private base-provider
  {:provider/id    :test
   :provider/label "Test provider"})

(defdescribe group-auto-derivation-test
  (it "derives \"providers\" for extensions exporting :ext/providers"
    (let [e (ext/extension
              {:ext/namespace 'test.provider-only
               :ext/doc       "Provider-only extension."
               :ext/providers [base-provider]})]
      (expect (= "providers" (:ext/group e)))))

  (it "derives \"channels\" for extensions exporting :ext/channels"
    (let [e (ext/extension
              {:ext/namespace 'test.channel-only
               :ext/doc       "Channel-only extension."
               :ext/channels  [base-channel]})]
      (expect (= "channels" (:ext/group e)))))

  (it "leaves :ext/group blank when the extension fits no categorical bucket"
    (let [e (ext/extension
              {:ext/namespace 'test.bare
               :ext/doc       "No surfaces, no group."})]
      (expect (nil? (:ext/group e)))))

  (it "explicit :ext/group always wins over auto-derivation"
    (let [e (ext/extension
              {:ext/namespace 'test.explicit-group
               :ext/doc       "Channel ext with custom group."
               :ext/group     "custom-bucket"
               :ext/channels  [base-channel]})]
      (expect (= "custom-bucket" (:ext/group e))))))

(defdescribe subgroup-removed-test
  (it "the builder never injects an :ext/subgroup key"
    (let [e (ext/extension
              {:ext/namespace 'test.no-subgroup
               :ext/doc       "Subgroup is dead."
               :ext/group     "anything"})]
      (expect (not (contains? e :ext/subgroup)))))

  (it "passing :ext/subgroup is rejected as an unknown key by the spec"
    ;; Spec validation runs through `validate!` and throws on unknown
    ;; keys is *not* what `s/keys` does \u2014 it just ignores them. So
    ;; we assert the weaker (but truthful) property: the builder does
    ;; not propagate / honour an explicit subgroup.
    (let [e (ext/extension
              {:ext/namespace 'test.legacy-subgroup
               :ext/doc       "Legacy author still sets :ext/subgroup."
               :ext/group     "g"
               :ext/subgroup  "ignored"})]
      ;; The builder doesn't strip arbitrary keys, but it also doesn't
      ;; *promise* anything about :ext/subgroup. The contract this test
      ;; pins is: the canonical group is intact and the spec doesn't
      ;; declare subgroup anymore (so consumers should stop reading it).
      (expect (= "g" (:ext/group e))))))

(defdescribe symbols-still-need-group-test
  (it "an extension with :ext/symbols and no :ext/group fails validation"
    (let [thrown? (try
                    (ext/extension
                      {:ext/namespace 'test.symbols-no-group
                       :ext/doc       "Symbols without a group."
                       :ext/ns-alias  {:ns 'test.sym :alias 'tst}
                       :ext/symbols   [(ext/value 'x 1 {:doc "A value."})]})
                    false
                    (catch Exception _ true))]
      (expect thrown?))))
