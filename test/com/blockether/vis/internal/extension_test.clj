(ns com.blockether.vis.internal.extension-test
  "Coverage for `internal/extension`.

   The hard rule (AGENTS.md) demands every namespace ship with a test
   file. Beyond the rule, this file pins down the bits of the
   `extension` builder that are most likely to bit-rot under
   refactors:

     1. `:ext/kind` auto-derivation for the categorical cases
        (extensions that contribute providers, channels, or
        persistence backends).
     2. Explicit `:ext/kind` always wins over auto-derivation.
     3. `:ext/subgroup` is no longer part of the spec — the builder
        neither requires it nor injects it.
     4. The `:ext/symbols` -> `:ext/kind` requirement still bites
        when an extension exports sandbox symbols without a kind.
     5. `:ext/owner` round-trips and coexists with `:ext/author`."
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

(defdescribe kind-auto-derivation-test
  (it "derives \"providers\" for extensions exporting :ext/providers"
    (let [e (ext/extension
              {:ext/namespace 'test.provider-only
               :ext/doc       "Provider-only extension."
               :ext/providers [base-provider]})]
      (expect (= "providers" (:ext/kind e)))))

  (it "derives \"channels\" for extensions exporting :ext/channels"
    (let [e (ext/extension
              {:ext/namespace 'test.channel-only
               :ext/doc       "Channel-only extension."
               :ext/channels  [base-channel]})]
      (expect (= "channels" (:ext/kind e)))))

  (it "derives \"persistance\" for extensions exporting :ext/persistance"
    (let [e (ext/extension
              {:ext/namespace 'test.persistance-only
               :ext/doc       "Persistence-only extension."
               :ext/persistance [{:persistance/id :test
                                  :persistance/ns 'test.backend.core}]})]
      (expect (= "persistance" (:ext/kind e)))))

  (it "leaves :ext/kind blank when the extension fits no categorical bucket"
    (let [e (ext/extension
              {:ext/namespace 'test.bare
               :ext/doc       "No surfaces, no kind."})]
      (expect (nil? (:ext/kind e)))))

  (it "explicit :ext/kind always wins over auto-derivation"
    (let [e (ext/extension
              {:ext/namespace 'test.explicit-kind
               :ext/doc       "Channel ext with custom kind."
               :ext/kind      "custom-bucket"
               :ext/channels  [base-channel]})]
      (expect (= "custom-bucket" (:ext/kind e))))))

(defdescribe owner-field-test
  (it "accepts :ext/owner as a non-blank string and round-trips it"
    (let [e (ext/extension
              {:ext/namespace 'test.owned
               :ext/doc       "An owned extension."
               :ext/owner     "vis"
               :ext/channels  [base-channel]})]
      (expect (= "vis" (:ext/owner e)))))

  (it "is independent of :ext/author — both can coexist with different values"
    (let [e (ext/extension
              {:ext/namespace 'test.coexist
               :ext/doc       "Author and owner are distinct."
               :ext/author    "Blockether"
               :ext/owner     "vis"
               :ext/channels  [base-channel]})]
      (expect (= "Blockether" (:ext/author e)))
      (expect (= "vis" (:ext/owner e)))))

  (it "is optional — omitting it does not break validation"
    (let [e (ext/extension
              {:ext/namespace 'test.no-owner
               :ext/doc       "No owner declared."
               :ext/channels  [base-channel]})]
      (expect (not (contains? e :ext/owner))))))

(defdescribe subgroup-removed-test
  (it "the builder never injects an :ext/subgroup key"
    (let [e (ext/extension
              {:ext/namespace 'test.no-subgroup
               :ext/doc       "Subgroup is dead."
               :ext/kind      "anything"})]
      (expect (not (contains? e :ext/subgroup))))))

(defdescribe symbols-still-need-kind-test
  (it "an extension with :ext/symbols and no :ext/kind fails validation"
    (let [thrown? (try
                    (ext/extension
                      {:ext/namespace 'test.symbols-no-kind
                       :ext/doc       "Symbols without a kind."
                       :ext/ns-alias  {:ns 'test.sym :alias 'tst}
                       :ext/symbols   [(ext/value 'x 1 {:doc "A value."})]})
                    false
                    (catch Exception _ true))]
      (expect thrown?))))
