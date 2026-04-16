(ns com.blockether.vis.rlm.corpus.markdown-test
  "Unit tests for the RLM gap-bundle surface: markdown parser,
   :extraction-strategy enum validation, dynamic *eval-timeout-ms* binding,
   and new SCI relationship query tool bindings."
  (:require
   [lazytest.core :refer [defdescribe describe expect it throws?]]
   [sci.core :as sci]
   [com.blockether.svar.internal.llm :as llm]
   [com.blockether.vis.rlm :as sut]
   [com.blockether.vis.rlm.corpus.pageindex.markdown :as md]
   [com.blockether.vis.rlm.corpus.pageindex.vision :as vision]
   [com.blockether.vis.rlm.persistence.schema :as schema]))

(defdescribe markdown-parser-test
  (describe "markdown->pages"
    (it "returns empty vector on empty string"
      (expect (= [] (md/markdown->pages ""))))

    (it "single h1 yields one page"
      (let [pages (md/markdown->pages "# Intro\n\nHello world.")]
        (expect (= 1 (count pages)))
        (expect (= 0 (:index (first pages))))
        (expect (pos? (count (:nodes (first pages)))))))

    (it "multiple top-level headings yield multiple pages"
      (let [pages (md/markdown->pages "# One\n\nA.\n\n# Two\n\nB.\n\n# Three\n\nC.")]
        (expect (= 3 (count pages)))
        (expect (= [0 1 2] (mapv :index pages)))))

    (it "nested headings produce nested nodes under a single page"
      (let [pages (md/markdown->pages "# Top\n\n## Sub-a\n\nAlpha.\n\n## Sub-b\n\nBeta.")]
        (expect (= 1 (count pages)))
        (let [nodes (:nodes (first pages))
              heading-texts (->> nodes
                              (filter #(= :heading (:type %)))
                              (map :content))]
          (expect (some #(= "Top" %) heading-texts))
          (expect (some #(= "Sub-a" %) heading-texts))
          (expect (some #(= "Sub-b" %) heading-texts)))))

    (it "code blocks are skipped when detecting headings"
      (let [md-content "# Real\n\n```\n# not a heading\n## also not\n```\n\nBody.\n\n# Second"
            pages (md/markdown->pages md-content)]
        (expect (= 2 (count pages)))))

    (it "paragraph content preserved as paragraph nodes"
      (let [pages (md/markdown->pages "# H\n\nFirst line.\nSecond line.")
            nodes (:nodes (first pages))
            paragraphs (filter #(= :paragraph (:type %)) nodes)]
        (expect (pos? (count paragraphs)))))))

(defdescribe extraction-strategy-enum-test
  (describe ":extraction-strategy validation"
    (it "throws on unknown strategy keyword"
      (expect (throws? clojure.lang.ExceptionInfo
                #(vision/extract-text-from-pdf "dummy.pdf"
                   {:extraction-strategy :invalid}))))

    (it "throws on string instead of keyword"
      (expect (throws? clojure.lang.ExceptionInfo
                #(vision/extract-text-from-pdf "dummy.pdf"
                   {:extraction-strategy "vision"}))))

    (it "throws on nil strategy (bypassing :or default by explicit nil)"
      (expect (throws? clojure.lang.ExceptionInfo
                #(vision/extract-text-from-pdf "dummy.pdf"
                   {:extraction-strategy nil}))))))

(defdescribe eval-timeout-dynamic-binding-test
  (describe "*eval-timeout-ms* dynamic var"
    (it "defaults to DEFAULT_EVAL_TIMEOUT_MS"
      (expect (= schema/DEFAULT_EVAL_TIMEOUT_MS schema/*eval-timeout-ms*)))

    (it "is rebindable"
      (binding [schema/*eval-timeout-ms* 1234]
        (expect (= 1234 schema/*eval-timeout-ms*))))

    (it "default equals 120000"
      (expect (= 120000 schema/DEFAULT_EVAL_TIMEOUT_MS)))

    (it "MIN and MAX bounds are defined"
      (expect (= 1000 schema/MIN_EVAL_TIMEOUT_MS))
      (expect (= (* 30 60 1000) schema/MAX_EVAL_TIMEOUT_MS)))))

(defdescribe eval-timeout-clamping-test
  (describe "query-env! :eval-timeout-ms clamping"
    (let [router (llm/make-router [{:id :test :api-key "test" :base-url "http://localhost"
                                    :models [{:name "stub" :input-cost 0 :output-cost 0}]}])]

      (it "throws on non-integer :eval-timeout-ms (string)"
        (let [env (sut/create-env router {:db :temp})]
          (try
            (expect (throws? clojure.lang.ExceptionInfo
                      #(sut/query-env! env [(llm/user "x")] {:eval-timeout-ms "120000"})))
            (finally (sut/dispose-env! env)))))

      (it "throws on non-integer :eval-timeout-ms (keyword)"
        (let [env (sut/create-env router {:db :temp})]
          (try
            (expect (throws? clojure.lang.ExceptionInfo
                      #(sut/query-env! env [(llm/user "x")] {:eval-timeout-ms :fast})))
            (finally (sut/dispose-env! env)))))

      (it "clamp-eval-timeout-ms floors to MIN"
        (expect (= schema/MIN_EVAL_TIMEOUT_MS (schema/clamp-eval-timeout-ms 10)))
        (expect (= schema/MIN_EVAL_TIMEOUT_MS (schema/clamp-eval-timeout-ms 0)))
        (expect (= schema/MIN_EVAL_TIMEOUT_MS (schema/clamp-eval-timeout-ms -5000))))

      (it "clamp-eval-timeout-ms ceilings to MAX"
        (expect (= schema/MAX_EVAL_TIMEOUT_MS (schema/clamp-eval-timeout-ms Long/MAX_VALUE)))
        (expect (= schema/MAX_EVAL_TIMEOUT_MS (schema/clamp-eval-timeout-ms (* 60 60 1000)))))

      (it "clamp-eval-timeout-ms passes values in range unchanged"
        (expect (= 5000 (schema/clamp-eval-timeout-ms 5000)))
        (expect (= schema/DEFAULT_EVAL_TIMEOUT_MS
                  (schema/clamp-eval-timeout-ms schema/DEFAULT_EVAL_TIMEOUT_MS))))

      (it "clamp-eval-timeout-ms coerces Integer to Long"
        (expect (= 5000 (schema/clamp-eval-timeout-ms (int 5000))))))))

(defdescribe sci-relationship-bindings-test
  (describe "W3 SCI bindings"
    (it "sci sandbox resolves search-documents, get-entity, list-relationships"
      (let [router (llm/make-router [{:id :test :api-key "test" :base-url "http://localhost"
                                      :models [{:name "stub" :input-cost 0 :output-cost 0}]}])
            env (sut/create-env router {:db :temp})]
        (try
          (let [sci-ctx (:sci-ctx env)
                sandbox-ns (:sandbox-ns env)
                resolved? (fn [sym]
                            (some? (:val (sci/eval-string+ sci-ctx
                                           (str "(resolve '" sym ")")
                                           {:ns sandbox-ns}))))]
            (expect (resolved? 'search-documents))
            (expect (resolved? 'get-entity))
            (expect (resolved? 'list-relationships))
            (expect (resolved? 'find-related)))
          (finally (sut/dispose-env! env)))))))
