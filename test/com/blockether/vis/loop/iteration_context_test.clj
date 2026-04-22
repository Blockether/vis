(ns com.blockether.vis.loop.iteration-context-test
  "Integration-style coverage for `build-iteration-context`.

   This is the single place that turns per-iteration state into LLM-
   facing context: `[iter N/M]` header, `<prior_thinking>`, `<journal>`,
   `<var_index>` + SYSTEM_NUDGEs. Each block has unit coverage elsewhere;
   these tests pin the ASSEMBLY — that nothing is dropped, nothing is
   duplicated, and order is stable. A bug in the composer silently
   degrades every iteration, so a thin end-to-end is worth the weight."
  (:require
    [clojure.string :as str]
    [lazytest.core :refer [defdescribe describe expect it]]
    [com.blockether.vis.loop.core :as sut]
    [com.blockether.vis.loop.runtime.conversation.environment.core :as rlm-tools]
    [com.blockether.svar.internal.llm :as llm]))

(defn- make-test-env []
  (let [router (llm/make-router
                 [{:id :test :api-key "test" :base-url "http://localhost"
                   :models [{:name "gpt-4o"}]}])]
    (#'sut/create-rlm-env (atom 0) router {:db :temp})))

(defdescribe build-iteration-context-shape-test
  (describe "build-iteration-context (assembly)"
    (it "emits the `[iter N/M]` header first — LLM always knows its budget position"
      (let [env (make-test-env)]
        (try
          (let [out (sut/build-iteration-context env
                      {:iteration 2 :current-max-iterations 10})]
            (expect (string? out))
            ;; Header is 0-based N bumped to 1-based: iter 2 of 10 → `[iter 3/10]`
            (expect (str/starts-with? out "[iter 3/10]"))
            ;; No other blocks on a bare state — header stands alone.
            (expect (not (str/includes? out "<prior_thinking>")))
            (expect (not (str/includes? out "<var_index>")))
            (expect (not (str/includes? out "<journal>"))))
          (finally (#'sut/dispose-rlm-env! env)))))

    (it "composes header + <prior_thinking> + <journal> + <var_index> in that order"
      (let [env (make-test-env)]
        (try
          (rlm-tools/bind-and-bump! env 'sheet-src "file-content")
          (let [out (sut/build-iteration-context env
                      {:iteration 1
                       :current-max-iterations 10
                       :prior-thinking "[iter 0] read sheet.clj"
                       :prev-executions [{:id 0 :code "(def sheet-src \"file-content\")"
                                          :result "file-content"
                                          :stdout "" :stderr "" :error nil
                                          :execution-time-ms 5}]
                       :prev-iteration 0})]
            (expect (string? out))
            ;; Header first.
            (expect (str/starts-with? out "[iter 2/10]"))
            ;; All three content blocks appear.
            (expect (str/includes? out "<prior_thinking>"))
            (expect (str/includes? out "[iter 0] read sheet.clj"))
            (expect (str/includes? out "<var_index>"))
            (expect (str/includes? out "sheet-src"))
            ;; Journal render varies by format-execution-results; just
            ;; verify the code/result snippet is in the string somewhere.
            (expect (str/includes? out "file-content"))
            ;; Order: iter-header BEFORE prior_thinking BEFORE var_index.
            (let [idx-header (.indexOf out "[iter 2/10]")
                  idx-prior  (.indexOf out "<prior_thinking>")
                  idx-index  (.indexOf out "<var_index>")]
              (expect (< idx-header idx-prior idx-index))))
          (finally (#'sut/dispose-rlm-env! env)))))

    (it "fires the budget + overflow nudges together when both trigger"
      ;; Seed 31 user vars to trigger var-index-overflow, pick iteration
      ;; close to max to also trigger budget-warning.
      (let [env (make-test-env)]
        (try
          (dotimes [i 31]
            (rlm-tools/bind-and-bump! env (symbol (str "probe-" i)) i))
          (let [out (sut/build-iteration-context env
                      {:iteration 9 :current-max-iterations 10})]
            (expect (str/includes? out "LAST ITERATION"))
            (expect (str/includes? out "SYSTEM_NUDGE"))
            (expect (str/includes? out "31 user vars")))
          (finally (#'sut/dispose-rlm-env! env)))))

    (it "returns nil for a completely empty state (no iter, no content)"
      (let [env (make-test-env)]
        (try
          (expect (nil? (sut/build-iteration-context env {})))
          (finally (#'sut/dispose-rlm-env! env)))))))
