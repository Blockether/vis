(ns com.blockether.vis.internal.extension-test
  (:require
   [clojure.string]
   [com.blockether.vis.internal.extension :as extension]
   [lazytest.core :refer [defdescribe expect it]]))

(defn raw-add
  "Raw helper used by symbol builder tests."
  [a b]
  (+ a b))

(defn env-echo-tool
  "Observed helper whose before-fn injects env into call args."
  [_env x]
  (extension/success {:op :demo/env-echo :result x}))

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

(defdescribe invoke-symbol-wrapper-test
  (it "renders sink forms from user args, not before-fn injected env"
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
      (expect (not (clojure.string/includes? (:form (first @journal)) "host env"))))))

(defdescribe extension-docs-test
  (it "returns authored doc links without computed backlinks"
    (let [registry-var (resolve 'com.blockether.vis.internal.extension/extension-docs-registry)
          registry     @registry-var
          before       @registry
          backlink-key (keyword (str "ref" "links"))]
      (try
        (reset! registry
          {'demo {:nses ['demo.core]
                  :docs {"README.md" {:description "demo"
                                      :content "body"
                                      :links [{:to-doc "OTHER.md"}]}}}})
        (let [doc     (extension/extension-doc 'demo "README.md")
              summary (extension/extension-doc-summary 'demo "README.md")]
          (expect (= [{:to-doc "OTHER.md"}] (:links doc)))
          (expect (not (contains? doc backlink-key)))
          (expect (not (contains? summary :content)))
          (expect (not (contains? summary backlink-key))))
        (finally
          (reset! registry before))))))
