(ns com.blockether.vis.internal.sinks-test
  "Reproduces the multi-render expectation across every nesting shape:
   bare top-level, inside `(do ...)`, inside `(let ...)`, do-in-let,
   let-in-do, deeply nested, and failure mid-block. Asserts the
   per-top-level-form sink contract: `(count :journal) == (count :channel)
   == number of tool-symbol calls`, positions are monotonic 0..N-1 within
   one form, `:form` strings round-trip, and entries conform to
   `::sink-entry`.

   Tests use `invoke-symbol-wrapper` directly + sink dynamic vars for the
   contract probes, plus one end-to-end smoke test driving SCI eval to
   verify the wiring through `run-sci-code`."
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [com.blockether.vis.internal.extension :as ext]
   [lazytest.core :refer [defdescribe expect it]]))

;; -----------------------------------------------------------------------------
;; Mock fixture: three symbols (cat, rg, fail) under alias mock/
;; -----------------------------------------------------------------------------

(defn- mock-cat-tool [path]
  ;; PLAN §2.1 envelope shape: {:result :op :metadata}.
  (ext/success {:result {:path path :body (str "BODY-" path)} :op :mock/cat}))

(defn- mock-rg-tool [needle]
  (ext/success {:result {:hits [{:line 1 :text needle}]} :op :mock/rg}))

(defn- mock-fail-tool [_]
  (throw (java.io.FileNotFoundException. "missing.txt")))

;; Renderer fns are simple deterministic stubs so tests can probe the
;; rendered text directly. Real symbols use the foundation/markdown layer;
;; these mocks isolate the contract under test.

(def ^:private cat-symbol
  (ext/symbol 'cat mock-cat-tool
    {:doc "Mock cat" :arglists '([p])
     :result-spec :op/envelope
     :journal-render-fn (fn [r] (str "JOURNAL cat " (:path r)))
     :channel-render-fn (fn [r] (str "CHANNEL cat " (:path r)))}))

(def ^:private rg-symbol
  (ext/symbol 'rg mock-rg-tool
    {:doc "Mock rg" :arglists '([n])
     :result-spec :op/envelope
     :journal-render-fn (fn [r] (str "JOURNAL rg " (-> r :hits first :text)))
     :channel-render-fn (fn [r] (str "CHANNEL rg " (-> r :hits first :text)))}))

;; No `:on-error-fn` - the throw escapes the wrapper. The wrapper's
;; catch block writes a failure sink entry from the exception BEFORE
;; re-throwing, so consumers see the failed call even when the form
;; bubbles the exception.
(def ^:private fail-symbol
  (ext/symbol 'fail mock-fail-tool
    {:doc "Mock fail" :arglists '([x])
     :journal-render-fn (fn [_] "never called on failure path")
     :channel-render-fn (fn [_] "never called on failure path")}))

(def ^:private mock-extension
  (ext/extension {:ext/namespace 'com.blockether.vis.test.mock
                  :ext/doc       "Mock fixture for sink contract tests"
                  :ext/kind      "fixture"
                  :ext/ns-alias  {:ns 'vis.test.mock :alias 'mock}
                  :ext/symbols   [cat-symbol rg-symbol fail-symbol]}))

;; -----------------------------------------------------------------------------
;; Helper: bind sinks, run a sequence of wrapper calls, return state.
;; -----------------------------------------------------------------------------

(defn- with-sinks
  "Bind fresh sink atoms + position counter, invoke `body-fn`, return
   {:journal :channel} as deref'd vecs. SUCCESS-path direct wrapper
   probes only. Failure paths go through the SCI integration tests
   below where the throw is captured naturally by `run-sci-code`."
  [body-fn]
  (let [j (atom [])
        c (atom [])
        p (atom -1)]
    (binding [ext/*journal-render-sink* j
              ext/*channel-render-sink* c
              ext/*sink-position*       p]
      (body-fn))
    {:journal @j :channel @c}))

(defn- invoke!
  [sym-entry args]
  (#'ext/invoke-symbol-wrapper mock-extension sym-entry args {}))

;; -----------------------------------------------------------------------------
;; Contract probes via direct wrapper invocation
;; -----------------------------------------------------------------------------

(defdescribe sink-entry-spec-test
  ;; PLAN §2.1 + §2.6: error is the structured `:op/error` map
  ;; (`{:message :trace? :hint? :block?}`). Sink-entry's `:error`
  ;; field is now typed as `:op/error` (was the dead `::error-map`).
  (it "::sink-entry valid for both success and failure shapes"
    (expect (s/valid? ::ext/sink-entry
              {:position 0 :form "(mock/cat \"a\")"
               :success? true :result "JOURNAL cat a" :error nil}))
    (expect (s/valid? ::ext/sink-entry
              {:position 7 :form "(mock/fail :x)"
               :success? false :result nil
               :error {:message "missing.txt"}})))

  (it "::sink-entry rejects mixed shapes"
    ;; success with non-nil error
    (expect (not (s/valid? ::ext/sink-entry
                   {:position 0 :form "(x)"
                    :success? true :result "ok"
                    :error {:message "y"}})))
    ;; success with nil result
    (expect (not (s/valid? ::ext/sink-entry
                   {:position 0 :form "(x)"
                    :success? true :result nil :error nil})))
    ;; failure with non-nil result
    (expect (not (s/valid? ::ext/sink-entry
                   {:position 0 :form "(x)"
                    :success? false :result "should-be-nil"
                    :error {:message "y"}})))
    ;; failure with nil error
    (expect (not (s/valid? ::ext/sink-entry
                   {:position 0 :form "(x)"
                    :success? false :result nil :error nil})))
    ;; negative position
    (expect (not (s/valid? ::ext/sink-entry
                   {:position -1 :form "(x)"
                    :success? true :result "ok" :error nil})))
    ;; blank form
    (expect (not (s/valid? ::ext/sink-entry
                   {:position 0 :form ""
                    :success? true :result "ok" :error nil})))))

(defdescribe sink-success-write-test
  (it "writes one entry per success call to BOTH journal and channel sinks"
    (let [{:keys [journal channel]}
          (with-sinks #(do (invoke! cat-symbol ["src/a.clj"])
                         (invoke! rg-symbol ["needle"])))]
      (expect (= 2 (count journal)))
      (expect (= 2 (count channel)))
      (expect (s/valid? :ext.sink/journal journal))
      (expect (s/valid? :ext.sink/channel channel))
      (let [[j0 j1] journal]
        (expect (= 0 (:position j0)))
        (expect (= 1 (:position j1)))
        (expect (= "(mock/cat \"src/a.clj\")" (:form j0)))
        (expect (= "(mock/rg \"needle\")" (:form j1)))
        (expect (true? (:success? j0)))
        (expect (= "JOURNAL cat src/a.clj" (:result j0)))
        (expect (= "JOURNAL rg needle" (:result j1)))
        (expect (nil? (:error j0))))
      (let [[c0 c1] channel]
        (expect (= "CHANNEL cat src/a.clj" (:result c0)))
        (expect (= "CHANNEL rg needle" (:result c1))))))

  (it "shares the position counter across journal and channel"
    (let [{:keys [journal channel]}
          (with-sinks #(do (invoke! cat-symbol ["a"])
                         (invoke! rg-symbol ["x"])
                         (invoke! cat-symbol ["b"])))]
      (expect (= [0 1 2] (mapv :position journal)))
      (expect (= [0 1 2] (mapv :position channel)))
      (expect (= (mapv :form journal) (mapv :form channel))))))

;; Failure-path coverage lives in `sci-failure-mid-block-test` below. The
;; wrapper writes a failure sink entry from the exception BEFORE re-throwing,
;; so SCI's existing catch boundary captures the throw, sinks reflect the
;; failed call, and the form's `:error` slot is set. No direct-wrapper
;; failure probes - they would need `try/catch` ceremony only because
;; they bypass the SCI catch boundary that wraps real eval.

(defdescribe sink-no-binding-noop-test
  (it "wrapper succeeds without sinks bound; returns the :op/envelope"
    (let [r (invoke! cat-symbol ["a"])]
      ;; PLAN §2.1: envelope reads use the `op/*` prefix.
      (expect (true? (:op/success? r)))
      (expect (= "a" (-> r :op/result :path)))
      (expect (nil? ext/*journal-render-sink*))
      (expect (nil? ext/*channel-render-sink*)))))

;; -----------------------------------------------------------------------------
;; End-to-end smoke through SCI eval. Verifies that `run-sci-code` binds
;; sinks correctly and attaches them to the form's result map regardless of
;; nesting (do, let, deeply nested, multiple top-level forms).
;; -----------------------------------------------------------------------------

(defn- with-mock-env
  "Register the mock extension globally, build a fresh SCI ctx with the
   mock alias bound, run `body-fn` with `{:sci-ctx :sandbox-ns}` map, and
   deregister the mock on exit."
  [body-fn]
  (let [_ (ext/register-extension! mock-extension)]
    (try
      (let [env-fn (requiring-resolve 'com.blockether.vis.internal.env/create-sci-context)
            install-fn (requiring-resolve 'com.blockether.vis.internal.loop/install-extension!)
            sync-fn (requiring-resolve 'com.blockether.vis.internal.loop/sync-active-extension-symbols!)
            {:keys [sci-ctx sandbox-ns initial-ns-keys]} (env-fn nil)
            env {:sci-ctx        sci-ctx
                 :sandbox-ns     sandbox-ns
                 :initial-ns-keys initial-ns-keys
                 :extensions     (atom [])}]
        (install-fn env mock-extension)
        (sync-fn env [mock-extension])
        (body-fn env))
      (finally
        (ext/deregister-extension! 'com.blockether.vis.test.mock)))))

(defn- run-form
  "Drive `run-sci-code` on `code` and return its block result map."
  [env code]
  (let [run-fn (requiring-resolve 'com.blockether.vis.internal.loop/run-sci-code)]
    (run-fn (:sci-ctx env) code
      :sandbox-ns (:sandbox-ns env)
      :env env)))

(defdescribe sci-bare-top-level-test
  (it "three top-level forms, each with its own per-form sink (one entry each, position 0)"
    (with-mock-env
      (fn [env]
        (let [r1 (run-form env "(mock/cat \"a\")")
              r2 (run-form env "(mock/rg \"x\")")]
          (expect (= 1 (count (:journal r1))))
          (expect (= 0 (-> r1 :journal first :position)))
          (expect (= "(mock/cat \"a\")" (-> r1 :journal first :form)))
          (expect (= 1 (count (:journal r2))))
          (expect (= 0 (-> r2 :journal first :position))))))))

(defdescribe sci-nesting-test
  (it "(do ...) — two tool calls inside a single top-level do, two entries"
    (with-mock-env
      (fn [env]
        (let [r (run-form env "(do (mock/cat \"a\") (mock/rg \"x\") :ok)")]
          (expect (= :ok (:result r)))
          (expect (= 2 (count (:journal r))))
          (expect (= [0 1] (mapv :position (:journal r))))
          (expect (= ["(mock/cat \"a\")" "(mock/rg \"x\")"]
                    (mapv :form (:journal r))))))))

  (it "(let [...]) — two tool calls in let bindings, two entries"
    (with-mock-env
      (fn [env]
        (let [r (run-form env "(let [a (mock/cat \"a\") b (mock/rg \"x\")] :ok)")]
          (expect (= :ok (:result r)))
          (expect (= 2 (count (:journal r))))
          (expect (= [0 1] (mapv :position (:journal r))))))))

  (it "do-in-let — (do ...) wrapped in a let binding"
    (with-mock-env
      (fn [env]
        (let [r (run-form env "(let [r (do (mock/cat \"a\") (mock/rg \"x\"))] r)")]
          (expect (map? (:result r)))
          (expect (= 2 (count (:journal r))))
          (expect (= [0 1] (mapv :position (:journal r))))))))

  (it "let-in-do — (let ...) wrapped in a do"
    (with-mock-env
      (fn [env]
        (let [r (run-form env "(do (let [a (mock/cat \"a\")] a) (mock/rg \"x\") :ok)")]
          (expect (= :ok (:result r)))
          (expect (= 2 (count (:journal r))))
          (expect (= [0 1] (mapv :position (:journal r))))))))

  (it "deeply nested — (do (do (do (cat \"a\") (cat \"b\")) (rg \"x\")))"
    (with-mock-env
      (fn [env]
        (let [r (run-form env
                  "(do (do (do (mock/cat \"a\") (mock/cat \"b\")) (mock/rg \"x\")))")]
          (expect (= 3 (count (:journal r))))
          (expect (= [0 1 2] (mapv :position (:journal r))))
          (expect (= ["(mock/cat \"a\")" "(mock/cat \"b\")" "(mock/rg \"x\")"]
                    (mapv :form (:journal r))))))))

  (it "journal and channel always have the same length and matching positions"
    (with-mock-env
      (fn [env]
        (doseq [code ["(mock/cat \"a\")"
                      "(do (mock/cat \"a\") (mock/rg \"x\"))"
                      "(let [a (mock/cat \"a\")] (mock/rg \"x\"))"
                      "(do (do (mock/cat \"a\") (mock/cat \"b\"))) "]]
          (let [r (run-form env code)]
            (expect (= (count (:journal r)) (count (:channel r))))
            (expect (= (mapv :position (:journal r))
                      (mapv :position (:channel r))))))))))

(defdescribe sci-failure-mid-block-test
  (it "throw mid-(do ...) — entries for cat (success) + fail (failure); rg never runs"
    (with-mock-env
      (fn [env]
        (let [r (run-form env "(do (mock/cat \"a\") (mock/fail :x) (mock/rg \"y\"))")]
          ;; cat succeeded; fail recovered into a failure tool-result;
          ;; the do form's value is the failure envelope. rg never ran.
          (expect (= 2 (count (:journal r))))
          (expect (= [0 1] (mapv :position (:journal r))))
          (let [[e0 e1] (:journal r)]
            (expect (true? (:success? e0)))
            (expect (= "(mock/cat \"a\")" (:form e0)))
            (expect (false? (:success? e1)))
            (expect (= "(mock/fail :x)" (:form e1)))
            ;; PLAN §2.1 + §2.7: structured :op/error has :message and a
            ;; preformatted :trace string whose first line carries the
            ;; underlying exception class name (babashka style).
            (expect (str/includes? (or (-> e1 :error :trace) "")
                      "java.io.FileNotFoundException"))))))))
