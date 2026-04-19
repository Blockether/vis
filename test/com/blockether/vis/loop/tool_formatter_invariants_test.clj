(ns com.blockether.vis.loop.tool-formatter-invariants-test
  "Invariant tests for tool :format-result formatters + the metadata pipeline
   that carries formatted strings from the tool wrapper through to the LLM
   serializer, println override, and var index.

   The formatter contract (see com.blockether.vis.loop.tool/default-format-result):
     1. Arity: strictly 1-arg, returns a string.
     2. Purity: repeated calls on the same value produce the same string.
     3. Dynamic-scope isolation: the formatter MUST NOT read external context.
        Running it under bindings that null *out*/*err* and on a fresh thread
        must produce the same string as an unbound call. Formatters run
        OUTSIDE SCI (as plain Clojure fns invoked by the tool wrapper), so
        only Clojure's *out*/*err* matter here — SCI's sci/out/sci/err are
        irrelevant to formatter isolation.

   Use `check-formatter-invariants!` in per-tool tests:

     (check-formatter-invariants! (:format-result read/tool-def)
                                  \"1: foo\\n[lines 1-1 of 1]\")

   Tool authors bring their own sample values. Samples are NOT declared on
   tool-defs — keep tool-defs lean; test data lives next to the assertions."
  (:require
    [lazytest.core :refer [defdescribe describe expect it]]
    [sci.core :as sci]
    [com.blockether.vis.loop.tool :as tool]
    [com.blockether.vis.loop.runtime.core :as runtime]))

(defn check-formatter-invariants!
  "Assert the three formatter invariants for `fmt` on `value`.

   Throws an ex-info on the first violation so it surfaces loudly in per-tool
   tests without any framework coupling. Returns the baseline formatted string
   on success."
  [fmt value]
  (let [baseline (fmt value)]
    ;; (1) arity + type
    (when-not (string? baseline)
      (throw (ex-info "formatter must return a string"
               {:fmt fmt :value value :returned-type (some-> baseline class .getName)})))
    ;; (2) purity — deterministic on repeated call in the SAME thread
    (let [second-call (fmt value)]
      (when-not (= baseline second-call)
        (throw (ex-info "formatter is non-deterministic (differs across calls)"
                 {:fmt fmt :value value :first baseline :second second-call}))))
    ;; (3) dynamic-scope isolation — run in a fresh thread with every known
    ;; stdout/stderr handle nulled out. If the formatter reads any of these
    ;; it will deviate from baseline.
    (let [isolated @(future
                      (binding [*out* (java.io.StringWriter.)
                                *err* (java.io.StringWriter.)]
                        (fmt value)))]
      (when-not (= baseline isolated)
        (throw (ex-info "formatter leaked dynamic-scope state (*out*/*err* on a fresh thread)"
                 {:fmt fmt :value value :baseline baseline :isolated isolated}))))
    baseline))

(defdescribe default-formatter-invariants
  (describe "default-format-result satisfies the formatter contract"
    (it "handles nil"
      (expect (string? (check-formatter-invariants! tool/default-format-result nil))))
    (it "handles primitives"
      (expect (string? (check-formatter-invariants! tool/default-format-result 42)))
      (expect (string? (check-formatter-invariants! tool/default-format-result "hello")))
      (expect (string? (check-formatter-invariants! tool/default-format-result :kw))))
    (it "handles maps"
      (expect (string? (check-formatter-invariants! tool/default-format-result
                         {:path "/a.txt" :lines 10}))))
    (it "handles vectors"
      (expect (string? (check-formatter-invariants! tool/default-format-result [1 2 3]))))
    (it "handles lazy seqs (realize-value bounds them)"
      (expect (string? (check-formatter-invariants! tool/default-format-result
                         (map inc (range 500))))))
    (it "caps pathological strings"
      (let [big (apply str (repeat 300000 \x))
            out (check-formatter-invariants! tool/default-format-result big)]
        (expect (<= (count out) 200002))))))

(defdescribe invariant-helper-sanity
  (describe "check-formatter-invariants! rejects violating formatters"
    (it "rejects non-string returns"
      (expect
        (try (check-formatter-invariants! (fn [_] 42) "v") false
          (catch clojure.lang.ExceptionInfo _ true))))
    (it "rejects non-deterministic formatters"
      (let [counter (atom 0)
            flaky   (fn [_] (str (swap! counter inc)))]
        (expect
          (try (check-formatter-invariants! flaky "v") false
            (catch clojure.lang.ExceptionInfo _ true)))))
    (it "rejects formatters that read *out*"
      (let [leaky (fn [_] (str *out*))]
        (expect
          (try (check-formatter-invariants! leaky "v") false
            (catch clojure.lang.ExceptionInfo _ true)))))))

(defdescribe tool-def-validation
  (describe "make-tool-def accepts a valid :format-result"
    (it "accepts a custom 1-arg formatter"
      (let [td (tool/make-tool-def 'probe
                 (fn probe-impl [x] x)
                 {:doc "probe" :arglists '[[x]]
                  :examples ["(probe 1)"]
                  :format-result (fn [v] (str "probe:" (pr-str v)))})]
        (expect (= "probe:nil" ((:format-result td) nil)))
        (expect (= "probe:42" ((:format-result td) 42))))))

  (describe "make-tool-def populates :format-result by default"
    (it "uses default-format-result when not specified"
      (let [td (tool/make-tool-def 'probe
                 (fn probe-impl [x] x)
                 {:doc "probe" :arglists '[[x]]
                  :examples ["(probe 1)"]})]
        (expect (= tool/default-format-result (:format-result td))))))

  (describe "make-tool-def rejects bad :format-result"
    (it "rejects non-fn formatter"
      (expect
        (try
          (tool/make-tool-def 'probe (fn [x] x)
            {:doc "d" :arglists '[[x]] :examples ["(probe)"]
             :format-result "not-a-fn"})
          false
          (catch clojure.lang.ExceptionInfo e
            (= :format-result (:field (ex-data e)))))))

    (it "rejects variadic-only formatter (no fixed 1-arg signature)"
      (expect
        (try
          (tool/make-tool-def 'probe (fn [x] x)
            {:doc "d" :arglists '[[x]] :examples ["(probe)"]
             :format-result (fn variadic [& args] (pr-str args))})
          false
          (catch clojure.lang.ExceptionInfo e
            (= :format-result (:field (ex-data e)))))))

    (it "rejects formatter that returns non-string"
      (expect
        (try
          (tool/make-tool-def 'probe (fn [x] x)
            {:doc "d" :arglists '[[x]] :examples ["(probe)"]
             :format-result (fn [_] 42)})
          false
          (catch clojure.lang.ExceptionInfo e
            (= :format-result (:field (ex-data e)))))))

    (it "rejects formatter that throws on nil"
      (expect
        (try
          (tool/make-tool-def 'probe (fn [x] x)
            {:doc "d" :arglists '[[x]] :examples ["(probe)"]
             :format-result (fn [v] (.toUpperCase ^String v))})
          false
          (catch clojure.lang.ExceptionInfo e
            (= :format-result (:field (ex-data e)))))))))

(defdescribe wrapper-metadata-integration
  (describe "tool wrapper attaches :rlm/format + :rlm/formatted on IObj results"
    (it "meta is attached to map returns"
      (let [td (tool/make-tool-def 'probe
                 (fn [_x] {:kind :ok})
                 {:doc "d" :arglists '[[x]] :examples ["(probe 1)"]
                  :format-result (fn [v] (str "OK:" (:kind v)))})
            result (#'runtime/apply-tool-formatter td {:kind :ok})]
        (expect (= "OK::ok" (:formatted result)))
        (expect (= "OK::ok" (:rlm/formatted (meta (:result result)))))
        (expect (fn? (:rlm/format (meta (:result result)))))))

    (it "primitive returns get :formatted but no meta (cannot attach)"
      (let [td (tool/make-tool-def 'probe
                 (fn [_x] "plain-string")
                 {:doc "d" :arglists '[[x]] :examples ["(probe 1)"]
                  :format-result (fn [v] (str "wrapped:" v))})
            result (#'runtime/apply-tool-formatter td "plain-string")]
        (expect (= "wrapped:plain-string" (:formatted result)))
        (expect (= "plain-string" (:result result)))
        (expect (nil? (meta (:result result))))))

    (it "formatter exception falls back to pr-str, doesn't throw"
      (let [td (tool/make-tool-def 'probe
                 (fn [_x] {:k 1})
                 {:doc "d" :arglists '[[x]] :examples ["(probe 1)"]
                  :format-result (fn [_] "safe-default")})
            ;; Swap in a broken formatter post-validation to simulate runtime blow-up.
            td' (assoc td :format-result (fn [_] (throw (ex-info "boom" {}))))
            result (#'runtime/apply-tool-formatter td' {:k 1})]
        (expect (string? (:formatted result)))
        (expect (re-find #":k 1" (:formatted result)))))

    (it "upstream :rlm/format meta is not overwritten"
      (let [td (tool/make-tool-def 'probe
                 (fn [_x] (with-meta {:k 1} {:rlm/format (fn [_] "upstream")
                                             :rlm/formatted "upstream"}))
                 {:doc "d" :arglists '[[x]] :examples ["(probe 1)"]
                  :format-result (fn [v] (str "downstream:" (:k v)))})
            raw {:k 1}
            raw-with-upstream-meta (with-meta raw {:rlm/format (fn [_] "upstream")
                                                   :rlm/formatted "upstream"})
            result (#'runtime/apply-tool-formatter td raw-with-upstream-meta)]
        ;; The outcome's :formatted still reflects the CURRENT tool's formatter
        ;; (downstream semantics in the outcome map), but the VALUE's metadata
        ;; is preserved intact so the serializer/println see the upstream string.
        (expect (= "downstream:1" (:formatted result)))
        (expect (= "upstream" (:rlm/formatted (meta (:result result)))))))))

(defn- capture-sandbox-println
  "Mirror production: invoke sandbox-println inside a sci/binding so
   sandbox-println's internal `(binding [*out* @sci/out] ...)` captures
   into a writer. Using with-out-str would only bind *out*, not sci/out,
   and sandbox-println would bypass it."
  [& args]
  (let [writer (java.io.StringWriter.)]
    (sci/binding [sci/out writer]
      (apply #'runtime/sandbox-println args))
    (str writer)))

(defdescribe sandbox-println-respects-formatter-meta
  (describe "sandbox println override substitutes formatted strings"
    (it "meta'd map is rendered via its :rlm/format when println'd"
      (let [m (with-meta {:path "/a.txt" :bytes 5}
                {:rlm/format (fn [v] (str "wrote " (:path v) " (" (:bytes v) "B)"))
                 :rlm/formatted "wrote /a.txt (5B)"})]
        (expect (= "wrote /a.txt (5B)\n" (capture-sandbox-println m)))))

    (it "plain maps fall through to default println"
      (let [m {:no :meta}]
        (expect (= (str (pr-str m) "\n") (capture-sandbox-println m)))))

    (it "primitive args pass through unchanged"
      (expect (= "hello 42 :kw\n" (capture-sandbox-println "hello" 42 :kw))))

    (it "mixed args — only meta'd values substitute"
      (let [m (with-meta {:k 1}
                {:rlm/format (fn [_] "FORMATTED")
                 :rlm/formatted "FORMATTED"})]
        (expect (= "prefix: FORMATTED suffix\n"
                  (capture-sandbox-println "prefix:" m "suffix")))))

    (it "falls back to original value when formatter throws"
      (let [m (with-meta {:k 1}
                {:rlm/format (fn [_] (throw (Exception. "bad")))
                 ;; :rlm/formatted NOT cached — forces the fn path
                 })]
        ;; Falls back to the raw value, which pr-str's as the plain map.
        (expect (= (str (pr-str m) "\n") (capture-sandbox-println m)))))))

(defdescribe sandbox-println-routes-through-sci-out
  (describe "sandbox println writes to sci/out (the SCI-per-iteration writer)"
    (it "output appears in the sci/binding-bound writer"
      (let [m (with-meta {:k :v}
                {:rlm/format (fn [_] "FMT")
                 :rlm/formatted "FMT"})]
        (expect (= "FMT\n" (capture-sandbox-println m)))))))
