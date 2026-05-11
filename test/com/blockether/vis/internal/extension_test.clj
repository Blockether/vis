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
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
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

(defdescribe hook-phase-test
  (it "accepts canonical namespaced hook phases"
    (doseq [phase [:session/start :turn/start :turn.iteration/start :turn.iteration/stop :turn.answer/validate :turn/stop]]
      (expect (ext/hook-phase? phase))))

  (it "specs pre-phase nudge returns and answer-validation rejects"
    (expect (s/valid? ::ext/system-nudge-hit {:hint "Focus now."}))
    (expect (s/valid? ::ext/system-nudge-hit {:hint "Focus now." :importance :high}))
    (expect (not (s/valid? ::ext/system-nudge-hit {:hint ""})))
    (expect (s/valid? ::ext/answer-validation-reject
              {:reject true
               :message "Answer lacks proof."
               :hint "Run verification."}))
    (expect (not (s/valid? ::ext/answer-validation-reject
                   {:reject false :message "ignored"}))))

  (it "validates extension hooks that use namespaced phases"
    (let [hook (fn [_] nil)
          extension (ext/extension {:ext/namespace 'test.namespaced-hook-phase
                                    :ext/doc "Hook phase fixture."
                                    :ext/kind "fixture"
                                    :ext/hooks [{:id :test/hook
                                                 :doc "Fixture hook."
                                                 :phase :turn.iteration/start
                                                 :fn hook}]})]
      (expect (= :turn.iteration/start (-> extension :ext/hooks first :phase))))))

(defdescribe wrap-extension-workspace-test
  (it "binds env workspace root around sandbox symbol calls"
    (let [sym-entry (ext/symbol 'root (fn [] (ext/success {:result workspace-context/*workspace-root* :op :test/root}))
                      {:doc "Return bound workspace root."
                       :arglists '([])

                       :journal-render-fn (fn [_] "")
                       :channel-render-fn (fn [_ _] "")})
          extension (ext/extension {:ext/namespace 'test.workspace-root
                                    :ext/doc "Workspace root fixture."
                                    :ext/kind "fixture"
                                    :ext/ns-alias {:ns 'test.workspace-root :alias 't}
                                    :ext/symbols [sym-entry]})
          wrapped   (ext/wrap-extension extension {:workspace/root "."})]
      (expect (= (workspace-context/workspace-root ".")
                (:op/result ((get wrapped 'root))))))))

(defdescribe source-rewrite-test
  (it "symbol carries optional :source-rewrite-fn"
    (let [hook (fn [_] "repaired")
          s    (ext/symbol 'p (fn [& _] nil)
                 {:doc "Paragraph."
                  :arglists '([& parts])
                  :journal-render-fn (fn [_] "")
                  :channel-render-fn (fn [_ _] "")
                  :source-rewrite-fn hook})]
      (expect (= hook (:ext.symbol/source-rewrite-fn s)))))

  (it "prefers symbol-level parsed-source rewrite hooks"
    (let [hook (fn [{:keys [code sym]}] (str code "\n:" sym))
          sym-entry (ext/symbol 'p (fn [& _] nil)
                      {:doc "Paragraph."
                       :arglists '([& parts])
                       :journal-render-fn (fn [_] "")
                       :channel-render-fn (fn [_ _] "")
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

(defdescribe symbol-parse-rescue-test
  (it "symbol carries optional :on-parse-error-fn"
    (let [hook (fn [_] "repaired")
          s    (ext/symbol 'cat (fn [& _] nil)
                 {:doc "Read a file."
                  :arglists '([path])
                  :journal-render-fn (fn [_] "")
                  :channel-render-fn (fn [_ _] "")
                  :on-parse-error-fn hook})]
      (expect (= hook (:ext.symbol/on-parse-error-fn s))))))

(defdescribe invoke-symbol-wrapper-log-level-test
  (it "logs normal invoke lifecycle at debug level"
    (let [levels (atom [])
          sym-entry (ext/symbol 'ping (fn [] (ext/success {:result :pong :op :test/ping}))
                      {:doc "Ping." :arglists '([])
                       :journal-render-fn (fn [_] "")
                       :channel-render-fn (fn [_ _] "")})
          extension (ext/extension {:ext/namespace 'test.invoke-log
                                    :ext/doc "Invoke log fixture."
                                    :ext/kind "fixture"
                                    :ext/ns-alias {:ns 'test.invoke-log :alias 'til}
                                    :ext/symbols [sym-entry]})]
      (with-redefs-fn {#'ext/log-hook! (fn [level & _] (swap! levels conj level))}
        (fn []
          (expect (= :pong (:op/result (#'ext/invoke-symbol-wrapper extension sym-entry [] {}))))
          (expect (= [:debug :debug :debug] @levels))))))

  (it "binds current extension context while invoking symbols"
    (let [seen (atom nil)
          sym-entry (ext/symbol 'whoami
                      (fn []
                        (reset! seen (ext/current-extension-id))
                        (ext/success {:result :done :op :test/whoami}))
                      {:doc "Record current extension." :arglists '([])
                       :journal-render-fn (fn [_] "")
                       :channel-render-fn (fn [_ _] "")})
          extension (ext/extension {:ext/namespace 'test.current-extension
                                    :ext/doc "Current extension fixture."
                                    :ext/kind "fixture"
                                    :ext/ns-alias {:ns 'test.current-extension :alias 'tce}
                                    :ext/symbols [sym-entry]})]
      (expect (= :done (:op/result (#'ext/invoke-symbol-wrapper extension sym-entry [] {}))))
      (expect (= "test.current-extension" @seen))))

  (it "records tool-start before the symbol function returns"
    (let [started   (promise)
          can-return (promise)
          sym-entry (ext/symbol 'slow-tool
                      (fn []
                        (expect (realized? started))
                        (deliver can-return true)
                        (ext/success {:result :done :op :test/slow-tool}))
                      {:doc "Slow tool." :arglists '([])
                       :journal-render-fn (fn [_] "")
                       :channel-render-fn (fn [_ _] "")})
          extension (ext/extension {:ext/namespace 'test.tool-start
                                    :ext/doc "Tool start fixture."
                                    :ext/kind "fixture"
                                    :ext/ns-alias {:ns 'test.tool-start :alias 'tts}
                                    :ext/symbols [sym-entry]})]
      (binding [ext/*tool-event-sink* #(deliver started %)]
        (expect (= :done (:op/result (#'ext/invoke-symbol-wrapper extension sym-entry [] {})))))
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

;; ============================================================================
;; § 5.2: Block-global coordinate translation in ex->op-error
;;
;; PLAN.md § 2.5 + § 2.6 + § 7.3.6: ex->op-error must produce block-global
;; :row/:col regardless of which engine threw. SCI per-form eval gives
;; FORM-LOCAL line/col; edamame parse-string-all gives BLOCK-GLOBAL row/col
;; already.
;; ============================================================================

(defdescribe sci-error-coords-translated-to-block-global-test
  (it "form-local SCI :line/:column translate to block-global via :form-row + :form-col"
    ;; Synthesize an ex-info matching SCI's :type :sci/error shape so we
    ;; don't need the SCI runtime in the test classpath.
    (let [block-source "(def x 1)\n(def y 2)\n(inc 1 2 3)\n(def z 99)"
          ;; SCI evaluating form 3 (line 3 of block) standalone reports
          ;; FORM-LOCAL :line 1 :column 1 in ex-data (probed live earlier).
          sci-ex (ex-info "Wrong number of args (3) passed to: clojure.core/inc"
                   {:type :sci/error :line 1 :column 1 :phase "analysis"})
          op-error (ext/ex->op-error sci-ex
                     {:block-source block-source :form-row 3 :form-col 1})]
      (expect (= "Wrong number of args (3) passed to: clojure.core/inc"
                (:message op-error)))
      (expect (= :sci/analysis (get-in op-error [:block :phase])))
      (expect (= 3 (get-in op-error [:block :row]))
        "form-local line 1 + form-row 3 -> block-global row 3")
      (expect (= 1 (get-in op-error [:block :col]))
        "form-local col 1 + form-col 1 -> block-global col 1 (line==1 case)")
      (expect (= block-source (get-in op-error [:block :source])))))

  (it "SCI errors on non-first form line keep their column unshifted"
    ;; When the SCI :line > 1, the column is already block-global because
    ;; only the first line of a form is offset by :form-col.
    (let [block-source "line1\n(let [a 1\n      b (inc 1 2 3)])"
          sci-ex (ex-info "Wrong number of args (3) passed to: clojure.core/inc"
                   {:type :sci/error :line 2 :column 9 :phase "analysis"})
          op-error (ext/ex->op-error sci-ex
                     {:block-source block-source :form-row 2 :form-col 1})]
      (expect (= 3 (get-in op-error [:block :row]))
        "form-row 2 + form-local line 2 -> block-global row 3")
      (expect (= 9 (get-in op-error [:block :col]))
        "form-local col 9 stays put when sci-line > 1")))

  (it "phase derives :sci/runtime when ex-data :phase is absent"
    (let [sci-ex (ex-info "boom" {:type :sci/error :line 1 :column 1})
          op-error (ext/ex->op-error sci-ex {:block-source "(boom)"})]
      (expect (= :sci/runtime (get-in op-error [:block :phase]))))))

(defdescribe edamame-error-coords-stay-block-global-test
  (it "edamame :row/:col are kept verbatim (already block-global from parse-string-all)"
    ;; Synthesize an ex-info matching edamame's :type :edamame/error shape.
    ;; Probed live earlier: edamame parse-string-all over a multi-form block
    ;; reports row/col in BLOCK-global coordinates.
    (let [block-source "(def a 1)\n(def b 2)\n(def c 3)\n(def d 4)"
          ;; Made-up parse failure at row 4 col 6.
          edamame-ex (ex-info "Invalid keyword: ::::bad."
                       {:type :edamame/error :row 4 :col 6})
          op-error (ext/ex->op-error edamame-ex {:block-source block-source})]
      (expect (= :edamame/parse (get-in op-error [:block :phase])))
      (expect (= 4 (get-in op-error [:block :row]))
        "edamame :row passed through unchanged (no translation needed)")
      (expect (= 6 (get-in op-error [:block :col])))))

  (it ":opened-loc beats :row/:col on delimiter mismatches (PLAN § 2.5)"
    ;; Probed live: edamame's primary :row/:col on EOF-while-reading errors
    ;; points at WHERE PARSER GAVE UP (often EOF, useless). The actually-
    ;; actionable position is :edamame/opened-delimiter-loc — the unmatched
    ;; opener.
    (let [block-source "(def a 1)\n(def b 2)\n(def c \"oops\n(def d 4)\n(def e 5)"
          edamame-ex (ex-info "EOF while reading, expected \" to match \" at [3,8]"
                       {:type :edamame/error
                        :row 5 :col 10                              ; EOF, useless
                        :edamame/expected-delimiter "\""
                        :edamame/opened-delimiter "\""
                        :edamame/opened-delimiter-loc {:row 3 :col 8}})  ; the actual fix point
          op-error (ext/ex->op-error edamame-ex {:block-source block-source})]
      (expect (= 5 (get-in op-error [:block :row]))
        ":row stays as edamame's EOF marker (callers/renderer prefer :opened-loc)")
      (expect (= {:row 3 :col 8} (get-in op-error [:block :opened-loc]))
        ":opened-loc lifted from :edamame/opened-delimiter-loc"))))

(defdescribe preflight-error-test
  (it "non-engine throwables get :phase :preflight with no row/col"
    (let [op-error (ext/ex->op-error (ex-info "envelope rejected"
                                       {:type :vis/preflight})
                     {:block-source "bad form"})]
      (expect (= "envelope rejected" (:message op-error)))
      (expect (= :preflight (get-in op-error [:block :phase])))
      (expect (nil? (get-in op-error [:block :row])))
      (expect (nil? (get-in op-error [:block :col]))))))

;; ============================================================================
;; § 5.3: render-error-context — whole-block context with failing-form marker
;;
;; PLAN.md § 2.8: babashka-style display. Every line gutter-numbered;
;; failing form's line range gets `>` gutter prefix; arrow `^---` lands
;; at exact column on the line below the failure. No truncation.
;; ============================================================================

(defdescribe render-error-context-test
  (it "renders all forms (no truncation) with > marker on the failing form"
    (let [block-source "(def a 1)\n(def b 2)\n(inc 1 2 3)\n(def d 4)"
          op-error (ext/ex->op-error
                     (ex-info "Wrong number of args" {:type :sci/error :line 3 :column 1})
                     {:block-source block-source})
          rendered (ext/render-error-context (:block op-error)
                     {:form-start-row 3 :form-end-row 3})
          lines    (str/split-lines rendered)]
      ;; All 4 source lines + 1 arrow line = 5 total (no truncation).
      (expect (= 5 (count lines)))
      ;; Failing-form line 3 has `>` marker, sibling forms have space.
      (expect (str/starts-with? (nth lines 0) "  ")
        "form 1 line: space gutter")
      (expect (str/starts-with? (nth lines 1) "  ")
        "form 2 line: space gutter")
      (expect (str/starts-with? (nth lines 2) "> ")
        "form 3 (failing) line: > gutter")
      ;; Line 3 is followed by the arrow line.
      (expect (str/includes? (nth lines 3) "^---"))
      ;; form 4 line: space gutter
      (expect (str/starts-with? (nth lines 4) "  "))))

  (it "multi-line form: > gutter spans the form's full line range"
    ;; Form is lines 1..5; arrow on line 3.
    (let [block-source "(let [a 1\n      b 2\n      c (inc 1 2 3)\n      d 4]\n  (+ a b c d))"
          op-error (ext/ex->op-error
                     (ex-info "Wrong number of args" {:type :sci/error :line 3 :column 9})
                     {:block-source block-source})
          rendered (ext/render-error-context (:block op-error)
                     {:form-start-row 1 :form-end-row 5})
          lines    (str/split-lines rendered)]
      ;; 5 source + 1 arrow = 6 lines.
      (expect (= 6 (count lines)))
      ;; All 5 source lines have > gutter (the whole let-form is the failing form).
      (doseq [src-idx (filter #(not= 3 %) (range 5))]   ; skip arrow at index 3
        (expect (str/starts-with? (nth lines (if (< src-idx 3) src-idx (inc src-idx)))
                  "> ")
          (str "src line " src-idx " should have > marker")))
      ;; Arrow line at index 3 (between line 3 and line 4).
      (expect (str/includes? (nth lines 3) "^---"))))

  (it "edamame :opened-loc beats :row/:col for arrow placement"
    ;; Probed live: arrow lands at :opened-loc {:row 3 :col 8}, NOT at
    ;; :row 5 :col 10 (which is EOF on this fixture).
    (let [block-source "(def a 1)\n(def b 2)\n(def c \"oops\n(def d 4)\n(def e 5)"
          edamame-ex (ex-info "EOF while reading"
                       {:type :edamame/error
                        :row 5 :col 10
                        :edamame/opened-delimiter-loc {:row 3 :col 8}})
          op-error (ext/ex->op-error edamame-ex {:block-source block-source})
          rendered (ext/render-error-context (:block op-error))
          lines    (str/split-lines rendered)]
      ;; 5 source lines + 1 arrow line on line 3.
      (expect (= 6 (count lines)))
      ;; The arrow line should be RIGHT AFTER line 3 (the unmatched-opener
      ;; line), NOT after line 5 (EOF).
      (let [arrow-line-idx (first (keep-indexed
                                    (fn [i s] (when (str/includes? s "^---") i))
                                    lines))]
        (expect (= 3 arrow-line-idx)
          "arrow at line index 3 (right after source line 3, the unmatched opener)"))))

  (it "no truncation: 50-line forms render in full"
    ;; Pathological case from PLAN § 2.8: long forms still render whole,
    ;; because the size IS often the failure mode.
    (let [block-source (str/join "\n"
                         (for [i (range 50)] (str "(line-" i ")")))
          op-error (ext/ex->op-error
                     (ex-info "boom" {:type :sci/error :line 25 :column 1})
                     {:block-source block-source})
          rendered (ext/render-error-context (:block op-error))]
      (expect (= 51 (count (str/split-lines rendered)))
        "50 source lines + 1 arrow line"))))
