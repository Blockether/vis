(ns com.blockether.vis.internal.extension-test
  (:require
   [clojure.string]
   [com.blockether.vis.internal.extension :as extension]
   [lazytest.core :refer [defdescribe expect it throws?]]))

(defn raw-add
  "Raw helper used by symbol builder tests."
  [a b]
  (+ a b))

(defn env-echo-tool
  "Observed helper whose before-fn injects env into call args."
  [_env x]
  (extension/success {:op :demo/env-echo :result x}))

(defn ir-render-tool
  "Observed helper whose channel renderer returns IR."
  [x]
  (extension/success {:op :demo/ir-render :result x}))

(doseq [op [:demo/env-echo :demo/ir-render]]
  (extension/register-op! op {:tag :op.tag/observation}))

(defdescribe symbol-builder-test
  (it "builds raw callable symbols without renderers"
    (let [entry (extension/symbol #'raw-add {:raw? true})]
      (expect (= 'raw-add (:ext.symbol/symbol entry)))
      (expect (identical? raw-add (:ext.symbol/fn entry)))
      (expect (true? (:ext.symbol/raw? entry)))
      (expect (= "Raw helper used by symbol builder tests." (:ext.symbol/doc entry)))
      (expect (= '([a b]) (:ext.symbol/arglists entry)))
      (expect (string? (:ext.symbol/source entry)))
      (expect (clojure.string/includes? (:ext.symbol/source entry) "(defn raw-add"))
      (expect (nil? (:ext.symbol/journal-render-fn entry)))
      (expect (nil? (:ext.symbol/channel-render-fn entry)))))

  (it "raw symbols can use fallback metadata for third-party vars"
    (let [entry (extension/symbol #'raw-add {:symbol 'plus2
                                             :raw? true
                                             :doc "plus helper"
                                             :arglists '([x y])})]
      (expect (= 'plus2 (:ext.symbol/symbol entry)))
      (expect (= "plus helper" (:ext.symbol/doc entry)))
      (expect (= '([x y]) (:ext.symbol/arglists entry)))
      (expect (true? (:ext.symbol/raw? entry))))))

(defdescribe extension-kind-test
  (it "buckets channel-hook-only extensions with channels"
    (let [ext (extension/extension
                {:ext/namespace 'demo.channel-hook
                 :ext/doc       "Demo channel hook."
                 :ext/channel-hooks [{:channel-id :tui
                                      :hook-id    :demo/status}]})]
      (expect (= "channels" (:ext/kind ext)))
      (expect (= [] (:ext/channels ext)))))

  (it "preserves explicit kind for channel-hook extensions"
    (let [ext (extension/extension
                {:ext/namespace 'demo.channel-hook
                 :ext/doc       "Demo channel hook."
                 :ext/kind      "conversation-state"
                 :ext/channel-hooks [{:channel-id :tui
                                      :hook-id    :demo/status}]})]
      (expect (= "conversation-state" (:ext/kind ext))))))

(defdescribe provider-extension-test
  (it "rejects removed provider-specific prompt slots"
    (let [thrown (try
                   (extension/extension
                     {:ext/namespace 'demo.provider
                      :ext/doc       "Demo provider."
                      :ext/providers [{:provider/id :demo
                                       :provider/label "Demo"
                                       :provider/prompt-fn (constantly "nope")}]})
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
      (expect (some? thrown))
      (expect (= :extension/invalid-spec (:type (ex-data thrown)))))))

(defdescribe invoke-symbol-wrapper-test
  (it "renders sink forms from user args, not before-fn injected env"
    ;; Per the fail-closed op-tag contract, every observed extension tool
    ;; MUST register its canonical op-keyword (alias + symbol). The wrapper's
    ;; `ensure-tool-result-op` calls `op-tag` which throws on unregistered ops.
    (extension/register-op! :d/env-echo {:tag :op.tag/observation})
    (let [entry (extension/symbol #'env-echo-tool
                  {:symbol 'env-echo
                   :before-fn (fn [env f args]
                                {:env env :fn f :args (into [env] args)})
                   :journal-render-fn pr-str
                   :channel-render-fn pr-str})
          ext   {:ext/namespace 'demo.ext
                 :ext/alias {:alias 'd}}
          env   {:large "host env must not be printed in sink form"}
          journal (atom [])
          channel (atom [])]
      (binding [extension/*journal-render-sink* journal
                extension/*channel-render-sink* channel
                extension/*sink-position* (atom -1)]
        (expect (= :payload (extension/invoke-symbol-wrapper ext entry [:payload] env))))
      (expect (= "(d/env-echo :payload)" (:form (first @journal))))
      (expect (= "(d/env-echo :payload)" (:form (first @channel))))
      (expect (not (clojure.string/includes? (:form (first @journal)) "host env")))))

  (it "allows channel renderers to record IR"
    (extension/register-op! :d/ir-render {:tag :op.tag/observation})
    (let [entry (extension/symbol #'ir-render-tool
                  {:symbol 'ir-render
                   :journal-render-fn pr-str
                   :channel-render-fn (fn [result]
                                        [:ir [:p "rendered " [:c (str result)]]])})
          ext   {:ext/namespace 'demo.ext
                 :ext/alias {:alias 'd}}
          channel (atom [])]
      (binding [extension/*channel-render-sink* channel
                extension/*sink-position* (atom -1)]
        (expect (= :payload (extension/invoke-symbol-wrapper ext entry [:payload] nil))))
      (expect (= [:ir {} [:p {} [:span {} "rendered "] [:c {} ":payload"]]]
                (:result (first @channel)))))))

(defdescribe extension-manifest-registry-test
  (it "tracks manifest ids and namespaces without docs"
    (let [registry-var (resolve 'com.blockether.vis.internal.extension/extension-manifest-registry)
          registry     @registry-var
          before       @registry]
      (try
        (reset! registry {'demo {:nses ['demo.core 'demo.extra]}})
        (expect (= ['demo.core 'demo.extra] (extension/extension-namespaces 'demo)))
        (expect (= 'demo (extension/extension-id-of-ns 'demo.extra)))
        (expect (= {'demo {:nses ['demo.core 'demo.extra]}}
                  (extension/registered-extensions-summary)))
        (finally
          (reset! registry before))))))

;; ============================================================================
;; Op-tag registry — mandatory tag, fail-closed on unknown ops.
;;
;; The op-tag/mutation gate downstream (final-answer-errors) relies on the
;; deterministic registry, not on a default. If an unregistered op silently
;; classified as `:op.tag/observation`, an extension that forgot to register
;; would bypass the same-iteration-answer gate. Hence: fail closed.
;; ============================================================================

(defn- with-op-registry-snapshot
  "Snapshot/restore @#'op-keyword->meta so each test runs against a known
   state without leaking registrations into other tests in the suite."
  [f]
  (let [registry @#'extension/op-keyword->meta
        before   @registry]
    (try (f) (finally (reset! registry before)))))

(defdescribe op-tag-registry-test
  (it "register-op! requires a :tag from the closed set"
    (with-op-registry-snapshot
      (fn []
        ;; Missing :tag entirely.
        (expect (throws? Throwable #(extension/register-op! :demo/no-tag {})))
        ;; Unknown :tag value.
        (expect (throws? Throwable
                  #(extension/register-op! :demo/bad-tag {:tag :op.tag/nonsense})))
        ;; Valid observation registers cleanly.
        (extension/register-op! :demo/cat {:tag :op.tag/observation})
        (expect (= :op.tag/observation (extension/op-tag :demo/cat)))
        ;; Valid mutation registers cleanly.
        (extension/register-op! :demo/patch {:tag :op.tag/mutation})
        (expect (= :op.tag/mutation (extension/op-tag :demo/patch))))))

  (it "op-tag fails closed on unregistered ops — no :observation fallback"
    (with-op-registry-snapshot
      (fn []
        ;; The closed-set / fail-closed contract: unknown ops MUST throw, not
        ;; silently classify as observation. This is the foundation for the
        ;; downstream any-extension-call-blocks-answer gate — if an extension
        ;; forgot to register its op, the gate must not let it sneak through.
        (expect (throws? Throwable #(extension/op-tag :demo/never-registered))))))

  (it "registered-op? distinguishes registered from unregistered ops"
    (with-op-registry-snapshot
      (fn []
        (extension/register-op! :demo/probe {:tag :op.tag/observation})
        (expect (true? (extension/registered-op? :demo/probe)))
        (expect (false? (extension/registered-op? :demo/ghost))))))

  (it "op-presentation also fails closed on unregistered ops"
    (with-op-registry-snapshot
      (fn []
        (expect (throws? Throwable #(extension/op-presentation :demo/ghost)))
        (extension/register-op! :demo/probe {:tag :op.tag/observation
                                             :self-describing? true})
        (expect (= {:tag :op.tag/observation :self-describing? true}
                  (extension/op-presentation :demo/probe))))))

  (it "extension/validate! requires every observed symbol to have an op tag"
    (with-op-registry-snapshot
      (fn []
        (let [entry    (extension/symbol #'ir-render-tool
                         {:symbol 'untagged
                          :journal-render-fn pr-str
                          :channel-render-fn (constantly [:ir {}])})
              ext-spec {:ext/namespace 'demo.missing-tag
                        :ext/doc       "Missing mandatory op tag."
                        :ext/kind      "tools"
                        :ext/alias     {:ns 'demo.missing :alias 'm}
                        :ext/symbols   [entry]}]
          ;; Without registration, validate! must fail closed.
          (expect (throws? Throwable #(extension/validate! ext-spec)))
          ;; Register the canonical op-keyword — alias 'm' + symbol
          ;; 'untagged → :m/untagged.
          (extension/register-op! :m/untagged {:tag :op.tag/observation})
          ;; Now validate! accepts it; the ext map round-trips its namespace.
          (expect (= 'demo.missing-tag
                    (:ext/namespace (extension/validate! ext-spec)))))))))
