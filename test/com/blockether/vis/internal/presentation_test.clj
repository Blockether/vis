(ns com.blockether.vis.internal.presentation-test
  "Regression tests for the shared presentation/render contract.

   Covers builders, renderers, and the `present` dispatcher so every
   Vis surface (TUI, transcript, audit reports) projects through the
   same shape."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.presentation :as ps]
   [lazytest.core :refer [defdescribe describe expect it]]))

;; ---------------------------------------------------------------------------
;; Builders
;; ---------------------------------------------------------------------------

(defdescribe builders-tag-with-kind-test
  (it "every builder produces a presentation map with a recognized kind"
    (let [maps [(ps/markdown "# hi")
                (ps/details "summary" "body")
                (ps/tool-call {:op :v/bash :args {:cmd "ls"} :result "ok"})
                (ps/system-call {:op :vis/system :body "msg"})
                (ps/provider-error {:message "nope"})
                (ps/mermaid {:source "graph TD\nA --> B"})
                (ps/audit-report {:title "Audit" :events [] :intents []})]]
      (expect (every? ps/presentation? maps))
      (expect (= [:vis.presentation/markdown
                  :vis.presentation/details
                  :vis.presentation/tool-call
                  :vis.presentation/system-call
                  :vis.presentation/provider-error
                  :vis.presentation/mermaid
                  :vis.presentation/audit-report]
                (mapv :vis.presentation/kind maps))))))

;; ---------------------------------------------------------------------------
;; Markdown / details
;; ---------------------------------------------------------------------------

(defdescribe markdown-and-details-test
  (it "markdown is identity on the body"
    (expect (= "# hello" (ps/present (ps/markdown "# hello")))))

  (it "details emits <details>/<summary> in collapsed-by-default form"
    (let [out (ps/present (ps/details "Plan" "Body text"))]
      (expect (str/includes? out "<details>"))
      (expect (str/includes? out "<summary>Plan</summary>"))
      (expect (str/includes? out "Body text"))
      (expect (str/includes? out "</details>"))))

  (it "details body can be a presentation map (recurses through present)"
    (let [out (ps/present
                (ps/details "Inner"
                  (ps/markdown "**bold body**")))]
      (expect (str/includes? out "<summary>Inner</summary>"))
      (expect (str/includes? out "**bold body**"))))

  (it "details body can be a sequence of presentation maps"
    (let [out (ps/present
                (ps/details "Bundle"
                  [(ps/markdown "first")
                   (ps/markdown "second")]))]
      (expect (str/includes? out "first"))
      (expect (str/includes? out "second"))))

  (it "details with empty body collapses cleanly"
    (let [out (ps/present (ps/details "Empty" ""))]
      (expect (str/includes? out "<summary>Empty</summary>"))
      (expect (str/includes? out "</details>")))))

;; ---------------------------------------------------------------------------
;; Tool / system / provider-error
;; ---------------------------------------------------------------------------

(defdescribe tool-call-presentation-test
  (it "renders op, status, duration, and result as a coherent block"
    (let [out (ps/tool-call->md
                {:op :v/bash
                 :args {:cmd "ls"}
                 :result "Ran bash — exit `0`."
                 :status :done
                 :duration-ms 12})]
      (expect (str/includes? out "**TOOL**"))
      (expect (str/includes? out "`v/bash`"))
      (expect (str/includes? out "done"))
      (expect (str/includes? out "12ms"))
      (expect (str/includes? out "Ran bash"))
      (expect (str/includes? out "<summary>args</summary>"))
      (expect (str/includes? out "<summary>result</summary>"))))

  (it "stdout/stderr render as their own collapsed details blocks"
    (let [out (ps/tool-call->md
                {:op :v/bash
                 :stdout "line1\nline2"
                 :stderr "warn: foo"})]
      (expect (str/includes? out "<summary>stdout</summary>"))
      (expect (str/includes? out "line1"))
      (expect (str/includes? out "<summary>stderr</summary>"))
      (expect (str/includes? out "warn: foo"))))

  (it "errors are surfaced inline, not silently dropped"
    (let [out (ps/tool-call->md {:op :v/bash :error "boom"})]
      (expect (str/includes? out "Error:"))
      (expect (str/includes? out "boom")))))

(defdescribe system-call-presentation-test
  (it "renders op, body, and rendering-kind"
    (let [out (ps/system-call->md
                {:op :vis/system
                 :body "Need user input."
                 :rendering-kind :vis/system})]
      (expect (str/includes? out "**SYSTEM**"))
      (expect (str/includes? out "`vis/system`"))
      (expect (str/includes? out "Need user input."))
      (expect (str/includes? out "rendering-kind: vis/system")))))

(defdescribe provider-error-presentation-test
  (it "surfaces type, reason, message, raw preview, and advice"
    (let [out (ps/provider-error->md
                {:message "Invalid response"
                 :type :svar.spec/schema-rejected
                 :reason :missing-field
                 :received-type "string"
                 :raw-preview "Sorry, I can't help with that."
                 :advice "Switch model or retry."
                 :iteration 3
                 :classification :provider-schema-rejected
                 :provider :anthropic
                 :model "claude-3"})]
      (expect (str/includes? out "**PROVIDER ERROR**"))
      (expect (str/includes? out "iteration 3"))
      (expect (str/includes? out "type:"))
      (expect (str/includes? out "svar.spec/schema-rejected"))
      (expect (str/includes? out "Invalid response"))
      (expect (str/includes? out "Sorry, I can't help with that."))
      (expect (str/includes? out "**Advice:**") "renders Advice label as bold")
      (expect (str/includes? out "Switch model or retry."))
      (expect (str/includes? out "anthropic"))))

  (it "missing optional fields leave the renderer well-formed"
    (let [out (ps/provider-error->md {:message "boom"})]
      (expect (str/includes? out "**PROVIDER ERROR**"))
      (expect (str/includes? out "boom")))))

;; ---------------------------------------------------------------------------
;; Mermaid fenced
;; ---------------------------------------------------------------------------

(defdescribe mermaid-presentation-test
  (it "always emits the ```mermaid fenced source"
    (let [src "graph TD\nA --> B"
          out (ps/mermaid->md {:source src})]
      (expect (str/includes? out "```mermaid"))
      (expect (str/includes? out "A --> B"))))

  (it "renders rendered ASCII alongside the source when supplied"
    (let [out (ps/mermaid->md
                {:source "flowchart LR\nA --> B"
                 :rendered-lines ["+---+   +---+" "| A | → | B |" "+---+   +---+"]
                 :diagram-type :flowchart})]
      (expect (str/includes? out "```mermaid"))
      (expect (str/includes? out "<summary>Mermaid diagram (flowchart)</summary>"))
      (expect (str/includes? out "| A | → | B |"))))

  (it "markdown-fence? detects mermaid fences"
    (expect (true? (ps/markdown-fence? "```mermaid\ngraph TD\nA --> B\n```" "mermaid")))
    (expect (false? (ps/markdown-fence? "```clojure\n(+ 1 2)\n```" "mermaid")))
    (expect (false? (ps/markdown-fence? "no fence" "mermaid")))))

;; ---------------------------------------------------------------------------
;; Audit / proof report
;; ---------------------------------------------------------------------------

(defn- sample-audit-report
  ([] (sample-audit-report {}))
  ([overrides]
   (merge
     {:title "Audit"
      :events [{:ref "turn/abcd1234/iteration/1/block/1"
                :kind :eval :op :sci/eval :status :done :duration-ms 2}
               {:ref "turn/abcd1234/iteration/1/block/2"
                :kind :tool :op :v/bash :status :done :duration-ms 5
                :parent-ref "turn/abcd1234/iteration/1/block/1"}]
      :bundles [{:id "bundle-1" :kind :proof :subject-ref "gate-1"
                 :attester "agent" :decision :accept :status :accepted}]
      :attestations [{:id "att-1" :kind :gate/proven :subject-id "gate-1"
                      :decision :proven :status :accepted :reason "Verified."}]
      :gates [{:id "gate-1" :proposition "Verification passes." :status :proven}]
      :plans [{:id "plan-1" :summary "Ship it" :status :active}]
      :intents [{:id "intent-1" :title "Ship it" :status :fulfilled
                 :rationale "User asked for ship."}]
      :guards {:success? true :violations []}
      :mermaid "graph TD\nintent_1 --> plan_1 --> gate_1"}
     overrides)))

(defdescribe audit-report-test
  (describe "collapsed (default) form"
    (it "wraps the body in <details> with a one-line summary"
      (let [out (ps/audit-report->md (ps/audit-report (sample-audit-report)))]
        (expect (str/starts-with? out "<details>"))
        (expect (str/includes? out "<summary>"))
        (expect (str/includes? out "Audit"))
        (expect (str/includes? out "events"))
        (expect (str/includes? out "intents"))
        (expect (str/ends-with? out "</details>"))))

    (it "summary line names every present section count"
      (let [report (ps/audit-report (sample-audit-report))
            line   (ps/audit-report->summary report)]
        (expect (str/starts-with? line "Audit"))
        (expect (str/includes? line "2 events"))
        (expect (str/includes? line "1 attestations"))
        (expect (str/includes? line "1 gates"))
        (expect (str/includes? line "1 intents"))
        (expect (str/includes? line "(1 fulfilled)"))
        (expect (str/includes? line "✓ guards"))))

    (it "guard violations push the summary into the failure form"
      (let [line (ps/audit-report->summary
                   (ps/audit-report
                     (sample-audit-report
                       {:guards {:success? false
                                 :violations [{:type :missing-ref :ref "x"}]}})))]
        (expect (str/includes? line "✗ 1 guard violations")))))

  (describe "expanded body"
    (it "renders every section that is present"
      (let [out (ps/audit-report->md
                  (ps/audit-report
                    (assoc (sample-audit-report) :collapsed? false)))]
        (expect (str/includes? out "## Audit"))
        (expect (str/includes? out "### Events"))
        (expect (str/includes? out "### Bundles"))
        (expect (str/includes? out "### Attestations"))
        (expect (str/includes? out "### Gates"))
        (expect (str/includes? out "### Plans"))
        (expect (str/includes? out "### Intents"))
        (expect (str/includes? out "Verification passes."))
        (expect (str/includes? out "Ship it"))
        (expect (str/includes? out "**Guards:**"))
        (expect (str/includes? out "ok"))))

    (it "always emits the Mermaid graph as a ```mermaid fence"
      (let [out (ps/audit-report->md
                  (ps/audit-report
                    (assoc (sample-audit-report) :collapsed? false)))]
        (expect (str/includes? out "```mermaid"))
        (expect (str/includes? out "graph TD"))
        (expect (str/includes? out "intent_1 --> plan_1 --> gate_1"))))

    (it "falls back gracefully when nothing is supplied"
      (let [out (ps/audit-report->md (ps/audit-report {:collapsed? false}))]
        (expect (str/includes? out "## Audit"))))

    (it "guard violations render a failure block with each cause"
      (let [out (ps/audit-report->md
                  (ps/audit-report
                    (assoc (sample-audit-report)
                      :collapsed? false
                      :guards {:success? false
                               :violations [{:type :missing-ref
                                             :ref "turn/zzzz0000/iteration/1/block/1"
                                             :message "ref not in journal"}]})))]
        (expect (str/includes? out "failed 1 check(s)"))
        (expect (str/includes? out "missing-ref"))
        (expect (str/includes? out "ref not in journal"))))))

;; ---------------------------------------------------------------------------
;; Dispatcher / fallthroughs
;; ---------------------------------------------------------------------------

(defdescribe present-dispatch-test
  (it "passes strings through verbatim"
    (expect (= "hello" (ps/present "hello"))))

  (it "renders nil as empty"
    (expect (= "" (ps/present nil))))

  (it "fallthrough on unknown maps uses pr-str"
    (let [out (ps/present {:no-kind :here})]
      (expect (str/includes? out ":no-kind"))))

  (it "every kind round-trips through present without throwing"
    (let [shapes [(ps/markdown "# m")
                  (ps/details "s" "b")
                  (ps/tool-call {:op :v/bash})
                  (ps/system-call {:op :vis/system :body "x"})
                  (ps/provider-error {:message "boom"})
                  (ps/mermaid {:source "graph TD\nA --> B"})
                  (ps/audit-report {:events [] :intents []})]]
      (doseq [s shapes]
        (expect (string? (ps/present s))))))

  (it "presentation? rejects unrecognized maps"
    (expect (false? (ps/presentation? {})))
    (expect (false? (ps/presentation? {:vis.presentation/kind :unknown})))
    (expect (false? (ps/presentation? "string")))
    (expect (true?  (ps/presentation? (ps/markdown "x"))))))
