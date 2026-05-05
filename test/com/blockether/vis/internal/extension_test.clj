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
     5. `:ext/owner` round-trips and coexists with `:ext/author`.
     6. Symbol-level parse rescue is part of the validated builder surface.
     7. Extension provenance includes cached source markers."
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

(def ^:private provider-with-hooks
  (assoc base-provider
    :provider/limits-fn (fn [] {:provider-id :test})
    :provider/on-selected-fn (fn [_ctx] nil)
    :provider/prompt-fn (fn [_ctx] "provider prompt")))

(defdescribe symbol-parse-rescue-test
  (it "symbol carries optional :on-parse-error-fn"
    (let [hook (fn [_] "repaired")
          s    (ext/symbol 'cat (fn [& _] nil)
                 {:doc "Read a file."
                  :arglists '([path])
                  :on-parse-error-fn hook})]
      (expect (= hook (:ext.symbol/on-parse-error-fn s))))))

(defdescribe invoke-symbol-wrapper-log-level-test
  (it "logs normal invoke lifecycle at debug level"
    (let [levels (atom [])
          sym-entry (ext/symbol 'ping (fn [] :pong)
                      {:doc "Ping." :arglists '([])})
          extension (ext/extension {:ext/namespace 'test.invoke-log
                                    :ext/doc "Invoke log fixture."
                                    :ext/kind "fixture"
                                    :ext/ns-alias {:ns 'test.invoke-log :alias 'til}
                                    :ext/symbols [sym-entry]})]
      (with-redefs-fn {#'ext/log-hook! (fn [level & _] (swap! levels conj level))}
        (fn []
          (expect (= :pong (#'ext/invoke-symbol-wrapper extension sym-entry [] {})))
          (expect (= [:debug :debug :debug] @levels)))))))

(defdescribe kind-auto-derivation-test
  (it "derives \"providers\" for extensions exporting :ext/providers"
    (let [e (ext/extension
              {:ext/namespace 'test.provider-only
               :ext/doc       "Provider-only extension."
               :ext/providers [base-provider]})]
      (expect (= "providers" (:ext/kind e)))))

  (it "accepts provider entries carrying optional provider hooks"
    (let [e (ext/extension
              {:ext/namespace 'test.provider-with-hooks
               :ext/doc       "Provider extension with runtime hooks."
               :ext/providers [provider-with-hooks]})]
      (expect (ifn? (get-in e [:ext/providers 0 :provider/limits-fn])))
      (expect (ifn? (get-in e [:ext/providers 0 :provider/on-selected-fn])))
      (expect (ifn? (get-in e [:ext/providers 0 :provider/prompt-fn])))))

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
      (expect (= "custom-bucket" (:ext/kind e)))))

  (it "accepts extension-declared environment variables"
    (let [e (ext/extension
              {:ext/namespace 'test.env
               :ext/doc       "Extension with config-backed env declarations."
               :ext/env       [{:name "TEST_API_KEY"
                                :label "Test API key"
                                :description "Secret test token."
                                :secret? true}]})]
      (expect (= [{:name "TEST_API_KEY"
                   :label "Test API key"
                   :description "Secret test token."
                   :secret? true}]
                (:ext/env e))))))

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

(defdescribe extension-provenance-test
  (it "resolves source markers from the extension namespace when available"
    (let [prov (ext/extension-provenance
                 (ext/extension {:ext/namespace 'com.blockether.vis.core
                                 :ext/doc       "vis core"}))]
      (expect (= 'com.blockether.vis.core (:namespace prov)))
      (expect (vector? (:source-paths prov)))
      (expect (or (= -1 (:source-mtime-max prov))
                (pos? (:source-mtime-max prov))))
      (expect (or (nil? (:source-hash-sha256 prov))
                (= 64 (count (:source-hash-sha256 prov)))))))

  (it "keeps declared authoring metadata"
    (let [prov (ext/extension-provenance
                 (ext/extension {:ext/namespace 'test.provenance
                                 :ext/doc       "Fixture"
                                 :ext/kind      "fixture"
                                 :ext/version   "1.2.3"
                                 :ext/author    "Acme"
                                 :ext/owner     "Suite"
                                 :ext/license   "Apache-2.0"}))]
      (expect (= "1.2.3" (:version prov)))
      (expect (= "Acme" (:author prov)))
      (expect (= "Suite" (:owner prov)))
      (expect (= "Apache-2.0" (:license prov)))
      (expect (= [] (:source-paths prov)))
      (expect (= -1 (:source-mtime-max prov)))
      (expect (nil? (:source-hash-sha256 prov))))))

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
