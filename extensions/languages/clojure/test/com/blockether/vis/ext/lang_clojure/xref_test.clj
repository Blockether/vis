(ns com.blockether.vis.ext.lang-clojure.xref-test
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]
   [com.blockether.vis.ext.lang-clojure.xref :as xref]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- temp-project []
  (let [root (fs/file "target/lang-clojure-xref-test")]
    (fs/delete-tree root)
    (fs/create-dirs (fs/file root "src" "demo"))
    (spit (fs/file root "src" "demo" "core.clj")
      (str "(ns demo.core)\n"
        "(defprotocol P (p [this]))\n"
        "(defn bar [x] (inc x))\n"
        "(defn foo [x] (bar x))\n"
        "(defrecord R [] P (p [_] (foo 1)))\n"
        "(defn lonely [] :unused)\n"))
    {:root root
     :src  (str (fs/file root "src"))
     :file (str (fs/file root "src" "demo" "core.clj"))}))

(defn- tool-fn [symbol-entry]
  (:ext.symbol/fn symbol-entry))

(defdescribe clj-xref-tools-test
  (it "analyzes, queries call graph facts, and bridges xref rows to zipper locators"
    (let [{:keys [src file]} (temp-project)
          analyze!          (tool-fn xref/xref-analyze-symbol)
          who-calls         (tool-fn xref/who-calls-symbol)
          calls-who         (tool-fn xref/calls-who-symbol)
          who-implements    (tool-fn xref/who-implements-symbol)
          ns-vars           (tool-fn xref/ns-vars-symbol)
          unused-vars       (tool-fn xref/unused-vars-symbol)
          call-graph        (tool-fn xref/call-graph-symbol)
          definition        (tool-fn xref/definition-symbol)
          call-sites        (tool-fn xref/call-sites-symbol)
          context-for       (tool-fn xref/context-for-symbol)
          locator-for-ref   (tool-fn xref/locator-for-ref-symbol)
          locators-for-sym  (tool-fn xref/locators-for-symbol-symbol)
          analysis          (analyze! {:paths [src]})]
      (expect (true? (:success? analysis)))
      (expect (= [src] (get-in analysis [:result :paths])))
      (expect (<= 5 (get-in analysis [:result :vars])))
      (expect (<= 3 (get-in analysis [:result :refs])))

      (let [callers (:result (who-calls 'demo.core/bar))]
        (expect (= ['demo.core/foo] (mapv :from callers)))
        (expect (= [:call] (mapv :kind callers)))
        (expect (= [file] (mapv :path callers)))
        (let [loc (:result (locator-for-ref (first callers)))]
          (expect (= file (:path loc)))
          (expect (= 'bar (:value loc)))
          (expect (= "bar" (:source loc)))
          (expect (= [[4 16] [4 19]] (:span loc)))))

      (expect (= ['demo.core/bar] (mapv :to (:result (calls-who 'demo.core/foo)))))
      (let [def-result (:result (definition 'demo.core/foo))]
        (expect (= 'demo.core/foo (get-in def-result [:definition :name])))
        (expect (= 'foo (get-in def-result [:locator :value]))))
      (let [sites (:result (call-sites 'demo.core/bar))]
        (expect (= ['demo.core/foo] (mapv :from sites)))
        (expect (= ['bar] (mapv #(get-in % [:locator :value]) sites))))
      (expect (= ['demo.core/R] (mapv :from (:result (who-implements 'demo.core/P)))))
      (expect (some #(= 'demo.core/foo (:name %)) (:result (ns-vars 'demo.core))))
      (expect (some #(= 'demo.core/lonely (:name %)) (:result (unused-vars))))
      (expect (some #(= {:from 'demo.core/foo :to 'demo.core/bar} %)
                (:result (call-graph 'demo.core/foo {:depth 2}))))

      (let [ctx (:result (context-for 'demo.core/foo))]
        (expect (= 'demo.core/foo (:symbol ctx)))
        (expect (= 'demo.core/foo (get-in ctx [:definition :name])))
        (expect (contains? (set (:files ctx)) file))
        (expect (some #(= 'demo.core/bar (:to %)) (:callees ctx)))
        (expect (some #(= 'demo.core/<top-level> (:from %)) (:callers ctx))))

      (let [locators (:result (locators-for-sym 'demo.core/bar))]
        (expect (some #(= 'bar (:value %)) locators))
        (expect (every? #(contains? % :span) locators))
        (expect (every? #(str/includes? (:path %) "target/lang-clojure-xref-test") locators))))))
