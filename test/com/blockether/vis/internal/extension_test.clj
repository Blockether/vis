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
     3. `:ext/subgroup` is no longer part of the spec - the builder
        neither requires it nor injects it.
     4. The `:ext/symbols` -> `:ext/kind` requirement still bites
        when an extension exports sandbox symbols without a kind.
     5. `:ext/owner` round-trips and coexists with `:ext/author`.
     6. Symbol-level parse rescue is part of the validated builder surface.
     7. Extension info includes cached source markers."
  (:require
   [com.blockether.vis.internal.extension :as ext]
   [com.blockether.vis.internal.registry :as registry]
   [com.blockether.vis.internal.workspace-context :as workspace-context]
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

(defdescribe wrap-extension-workspace-test
  (it "binds env workspace root around sandbox symbol calls"
    (let [sym-entry (ext/symbol 'root (fn [] workspace-context/*workspace-root*)
                      {:doc "Return bound workspace root."
                       :arglists '([])

                       :render-fn (fn [_] "")})
          extension (ext/extension {:ext/namespace 'test.workspace-root
                                    :ext/doc "Workspace root fixture."
                                    :ext/kind "fixture"
                                    :ext/ns-alias {:ns 'test.workspace-root :alias 't}
                                    :ext/symbols [sym-entry]})
          wrapped   (ext/wrap-extension extension {:workspace/root "."})]
      (expect (= (workspace-context/workspace-root ".")
                ((get wrapped 'root)))))))

(defdescribe source-rewrite-test
  (it "symbol carries optional :source-rewrite-fn"
    (let [hook (fn [_] "repaired")
          s    (ext/symbol 'p (fn [& _] nil)
                 {:doc "Paragraph."
                  :arglists '([& parts])
                  :source-rewrite-fn hook})]
      (expect (= hook (:ext.symbol/source-rewrite-fn s)))))

  (it "prefers symbol-level parsed-source rewrite hooks"
    (let [hook (fn [{:keys [code sym]}] (str code "\n:" sym))
          sym-entry (ext/symbol 'p (fn [& _] nil)
                      {:doc "Paragraph."
                       :arglists '([& parts])
                       :source-rewrite-fn hook})
          extension (ext/extension {:ext/namespace 'test.symbol-source-rewrite
                                    :ext/doc "Symbol source rewrite fixture."
                                    :ext/kind "fixture"
                                    :ext/ns-alias {:ns 'test.symbol-source-rewrite :alias 't}
                                    :ext/symbols [sym-entry]
                                    :ext/source-rewrite-fn (fn [_] ":extension")})]
      (expect (= "(t/p hi)\n:p"
                (ext/try-rewrite-source [extension] "(t/p hi)" {})))))

  (it "walks extension-level parsed-source rewrite hooks"
    (let [hook (fn [{:keys [code]}] (str code "\n:rewritten"))
          extension (ext/extension {:ext/namespace 'test.source-rewrite
                                    :ext/doc "Source rewrite fixture."
                                    :ext/kind "fixture"
                                    :ext/source-rewrite-fn hook})]
      (expect (= "(+ 1 2)\n:rewritten"
                (ext/try-rewrite-source [extension] "(+ 1 2)" {})))))

  (it "skips unchanged or non-string source rewrite results"
    (let [same-ext (ext/extension {:ext/namespace 'test.source-same
                                   :ext/doc "Source same fixture."
                                   :ext/kind "fixture"
                                   :ext/source-rewrite-fn (fn [{:keys [code]}] code)})
          nil-ext  (ext/extension {:ext/namespace 'test.source-nil
                                   :ext/doc "Source nil fixture."
                                   :ext/kind "fixture"
                                   :ext/source-rewrite-fn (fn [_] nil)})]
      (expect (nil? (ext/try-rewrite-source [same-ext nil-ext] "(+ 1 2)" {}))))))

(defdescribe tool-result-render-fallback-test
  (it "renders historical tool results through their presentation kind when the symbol is gone"
    (let [renderer (fn [{:keys [value]}] (str "exit=" (:exit value)))
          extension (ext/extension {:ext/namespace 'test.rendering-kind-fallback
                                    :ext/doc "Rendering kind fallback fixture."
                                    :ext/kind "fixture"
                                    :ext/rendering-kinds {:diagnostic renderer}})
          old-result {:success? true
                      :result {:exit 0}
                      :info {:op :v/bash
                             :extension {:namespace 'test.rendering-kind-fallback}
                             :tool {:sym 'bash}}
                      :presentation {:kind :diagnostic}}]
      (with-redefs [ext/registered-extensions (fn [] [extension])]
        (expect (= "exit=0" (ext/render-tool-result :tui old-result {})))))))

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
          (expect (= [:debug :debug :debug] @levels))))))

  (it "binds current extension context while invoking symbols"
    (let [seen (atom nil)
          sym-entry (ext/symbol 'whoami
                      (fn []
                        (reset! seen (ext/current-extension-id))
                        :done)
                      {:doc "Record current extension." :arglists '([])})
          extension (ext/extension {:ext/namespace 'test.current-extension
                                    :ext/doc "Current extension fixture."
                                    :ext/kind "fixture"
                                    :ext/ns-alias {:ns 'test.current-extension :alias 'tce}
                                    :ext/symbols [sym-entry]})]
      (expect (= :done (#'ext/invoke-symbol-wrapper extension sym-entry [] {})))
      (expect (= "test.current-extension" @seen))))

  (it "records tool-start before the symbol function returns"
    (let [started   (promise)
          can-return (promise)
          sym-entry (ext/symbol 'slow-tool
                      (fn []
                        (expect (realized? started))
                        (deliver can-return true)
                        :done)
                      {:doc "Slow tool." :arglists '([])})
          extension (ext/extension {:ext/namespace 'test.tool-start
                                    :ext/doc "Tool start fixture."
                                    :ext/kind "fixture"
                                    :ext/ns-alias {:ns 'test.tool-start :alias 'tts}
                                    :ext/symbols [sym-entry]})]
      (binding [ext/*tool-event-sink* #(deliver started %)]
        (expect (= :done (#'ext/invoke-symbol-wrapper extension sym-entry [] {}))))
      (expect (= true (deref can-return 1000 false)))
      (expect (= :tool-start (:phase @started)))
      (expect (= :tts/slow-tool (:op @started)))
      (expect (= :running (:status @started))))))

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

  (it "dispatches every provider in :ext/providers, not just the first one"
    (let [calls (atom [])
          e (ext/extension
              {:ext/namespace 'test.multi-provider
               :ext/doc       "Multi-provider extension fixture."
               :ext/providers [{:provider/id :multi-a :provider/label "Multi A"}
                               {:provider/id :multi-b :provider/label "Multi B"}]})]
      (with-redefs [registry/register-provider! #(swap! calls conj (:provider/id %))]
        (ext/register-extension! e)
        (expect (= [:multi-a :multi-b] @calls)))))

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
                (:ext/env e)))))

  (it "accepts dedicated environment-info prompt contributors"
    (let [f (fn [_env] "extra environment facts")
          e (ext/extension
              {:ext/namespace 'test.environment-info
               :ext/doc       "Extension with environment-info contribution."
               :ext/environment-info-fn f})]
      (expect (identical? f (:ext/environment-info-fn e))))))

(defdescribe system-nudge-result-spec-test
  (it "spec-checks extension nudge return values"
    (expect (ext/system-nudge-result? nil))
    (expect (ext/system-nudge-result? "plain nudge"))
    (expect (ext/system-nudge-result? {:importance :high :text "important"}))
    (expect (not (ext/system-nudge-result? {:importance :critical :message "stop"})))
    (expect (not (ext/system-nudge-result? {:importance :critical :body "stop"})))
    (expect (not (ext/system-nudge-result? "")))
    (expect (not (ext/system-nudge-result? {:importance :urgent :text "bad level"})))
    (expect (not (ext/system-nudge-result? {:importance :high :text ""})))
    (expect (not (ext/system-nudge-result? {:importance :high :text "ok" :extra true})))))

(defdescribe owner-field-test
  (it "accepts :ext/owner as a non-blank string and round-trips it"
    (let [e (ext/extension
              {:ext/namespace 'test.owned
               :ext/doc       "An owned extension."
               :ext/owner     "vis"
               :ext/channels  [base-channel]})]
      (expect (= "vis" (:ext/owner e)))))

  (it "is independent of :ext/author - both can coexist with different values"
    (let [e (ext/extension
              {:ext/namespace 'test.coexist
               :ext/doc       "Author and owner are distinct."
               :ext/author    "Blockether"
               :ext/owner     "vis"
               :ext/channels  [base-channel]})]
      (expect (= "Blockether" (:ext/author e)))
      (expect (= "vis" (:ext/owner e)))))

  (it "is optional - omitting it does not break validation"
    (let [e (ext/extension
              {:ext/namespace 'test.no-owner
               :ext/doc       "No owner declared."
               :ext/channels  [base-channel]})]
      (expect (not (contains? e :ext/owner))))))

(defdescribe fenced-renderer-test
  (it "accepts fenced renderers and dispatches by normalized language"
    (let [renderer (fn [{:keys [lang source]}]
                     {:lines [(str lang ":" source)]})
          e (ext/extension
              {:ext/namespace 'test.fenced-renderer
               :ext/doc "Fenced renderer fixture."
               :ext/fenced-renderers [{:renderer/id :test/fence
                                       :renderer/langs #{"demo"}
                                       :renderer/render-fn renderer}]})]
      (try
        (ext/register-extension! e)
        (expect (= {:renderer/id :test/fence
                    :lines ["demo:payload"]}
                  (ext/render-fenced-block {:surface :test
                                            :lang " Demo "
                                            :source "payload"})))
        (expect (nil? (ext/render-fenced-block {:surface :test
                                                :lang "other"
                                                :source "payload"})))
        (finally
          (ext/deregister-extension! 'test.fenced-renderer))))))

(defdescribe extension-info-test
  (it "resolves source markers from the extension namespace when available"
    (let [prov (ext/extension-info
                 (ext/extension {:ext/namespace 'com.blockether.vis.core
                                 :ext/doc       "vis core"}))]
      (expect (= 'com.blockether.vis.core (:namespace prov)))
      (expect (vector? (:source-paths prov)))
      (expect (or (= -1 (:source-mtime-max prov))
                (pos? (:source-mtime-max prov))))
      (expect (or (nil? (:source-hash-sha256 prov))
                (= 64 (count (:source-hash-sha256 prov)))))))

  (it "keeps declared authoring metadata"
    (let [prov (ext/extension-info
                 (ext/extension {:ext/namespace 'test.info
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
