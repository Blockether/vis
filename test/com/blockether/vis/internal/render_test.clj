(ns com.blockether.vis.internal.render-test
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.render :as render]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe heading-shorthand-test
  (it "normalizes [:h level ...] without rendering the level as text"
    (let [ast (render/->ast [:ir [:h 3 "Regular hook (" [:c ":ext/hooks"] ")"]])]
      (expect (= [:ir {}
                  [:h {:level 3}
                   [:span {} "Regular hook ("]
                   [:c {} ":ext/hooks"]
                   [:span {} ")"]]]
                ast))
      (expect (= "### Regular hook (`:ext/hooks`)"
                (str/trim (render/render ast :markdown {}))))))

  (it "clamps shorthand heading levels to the supported 1-6 range"
    (expect (= [:ir {} [:h {:level 1} [:span {} "Low"]]]
              (render/->ast [:ir [:h 0 "Low"]])))
    (expect (= [:ir {} [:h {:level 6} [:span {} "High"]]]
              (render/->ast [:ir [:h 9 "High"]])))))

(defdescribe host-bookkeeping-render-test
  (it "classifies only top-level structural host forms (def shapes hide because the DEF SINK already surfaces the bound var)"
    (expect (= [{:kind :title :value "Render cleanup"}
                {:kind :code :source "(def x 1)"}
                {:kind :answer-ref}]
              (render/parse-block-display
                "(set-session-title! \"Render cleanup\")\n(def x 1)\n(done [:ir [:p \"ok\"]])"))))

  (it "does not source-prune nested host calls"
    (expect (= [{:kind :code
                 :source "(do (satisfy-hint! :vis.foundation/session-title) (def x 1))"}]
              (render/parse-block-display
                "(do (satisfy-hint! :vis.foundation/session-title) (def x 1))"))))

  (it "does not treat satisfy-hint! source as structurally silent"
    (expect (false? (render/block-structurally-silent?
                      "(satisfy-hint! :vis.foundation/session-title)"))))

  (it "flags (def NAME (qualified/call …)) wrappers as hidden code"
    ;; The model wraps tool calls in `(def …)` for downstream reuse.
    ;; The channel shows the tool result pane anyway, so the def
    ;; source itself is noise. Mark the segment hidden so the renderer
    ;; collapses the code lines but still keeps the form in the
    ;; iteration trace.
    (expect (= [{:kind :code
                 :source "(def render-sb-code (cat \"x.clj\"))"}]
              (render/parse-block-display
                "(def render-sb-code (cat \"x.clj\"))"))))

  (it "hides plain (def x 1) so the DEF SINK is the single source of bound-var truth"
    ;; Pre-fix the renderer kept bare def source visible because the
    ;; comment claimed \"the value is the only artifact\". In practice
    ;; the trailer's `:vars` envelope already surfaces every bound var
    ;; to both model and user, so painting the raw `(def …)` line
    ;; just duplicates that signal and pushes the actual recap /
    ;; result rows off-screen. Hide every def-shaped form; let the
    ;; def sink speak.
    (expect (= [{:kind :code :source "(def x 1)"}]
              (render/parse-block-display "(def x 1)"))))

  (it "hides defn / defn- / defmacro / defmulti / defmethod / defonce uniformly"
    (doseq [src ["(defn foo [] 1)"
                 "(defn- bar [] 2)"
                 "(defmacro baz [] `(println 1))"
                 "(defmulti qux :kind)"
                 "(defmethod qux :a [_] :a)"
                 "(defonce only-once (atom {}))"]]
      (let [out (render/parse-block-display src)]
        (expect (= 1 (count out)))
        (expect (= :code (-> out first :kind))))))

  (it "hides top-level keyword-lookup and accessor calls (e.g. `(:size ir-file)`, `(get-in m [...])`, `(first xs)`)"
    ;; The model often does `(def x (tool/call))` followed by
    ;; `(:size x)` or `(get-in x [:k])` or `(first (:items x))` to
    ;; project a sub-value. The bound value is already on the DEF
    ;; SINK + the channel preview of the underlying tool call; the
    ;; projection row only repeats `<runtime ref>` next to a code
    ;; line nobody reads. Hide them.
    (doseq [src ["(:size ir-file)"
                 "(:k m :default)"
                 "(get m :k)"
                 "(get-in m [:a :b])"
                 "(select-keys r [:a :b])"
                 "(first xs)"
                 "(count xs)"
                 "(name :foo)"
                 "(str x)"]]
      (let [out (render/parse-block-display src)]
        (expect (= 1 (count out)))
        (expect (= :code (-> out first :kind))))))

  (it "hides bare-symbol top-level forms (e.g. `st` following `(def st (git/status))`)"
    ;; The model often writes `(def NAME (tool/call ...)) NAME` so the
    ;; trailer shows the value. The channel preview of the tool call
    ;; already paints the data; the loose symbol row is just chrome
    ;; that pushes recap / answer rows off-screen. Same hide-by-default
    ;; policy as bare tool calls and def-shaped forms.
    (let [out (render/parse-block-display "(def st (git/status))\nst")]
      ;; Both segments hidden → coalesce merges them into one entry.
      (expect (= 1 (count out)))
      (expect (= :code (-> out first :kind)))))

  (it "drops the engine auto-ack `:vis.foundation/session-title` task-set! — paired with the :title recap, no duplicate row"
    ;; Engine inserts this immediately after every successful
    ;; `(set-session-title! …)` to mark the foundation hook done.
    ;; The user already sees the TITLE recap; the auto-ack would
    ;; show as a TASK recap right below it (`Task — ✓
    ;; :vis.foundation/session-title :done`) which is pure noise.
    (expect (= [{:kind :title :value "Greeting"}]
              (render/parse-block-display
                "(set-session-title! \"Greeting\")\n(task-set! :vis.foundation/session-title {:status :done})"))))

  (it "renders the model's own task-set! as visible code (effect shows in the context dialog)"
    ;; A model-emitted task-set! that ISN'T the session-title auto-ack
    ;; renders as the visible CALL — same as summarize and every other
    ;; model form. The effect surfaces in the F2 context dialog.
    (expect (= [{:kind :code :source "(task-set! :user/migration {:status :doing})"}]
              (render/parse-block-display
                "(task-set! :user/migration {:status :doing})"))))

  (it "hides def docstring form when wrapping a tool call"
    (expect (= [{:kind :code
                 :source "(def named \"docstring\" (cat \"x.clj\"))"}]
              (render/parse-block-display
                "(def named \"docstring\" (cat \"x.clj\"))"))))

  (it "hides bare qualified tool calls so the result pane speaks for itself"
    (expect (= [{:kind :code
                 :source "(cat \"src/foo.clj\")"}]
              (render/parse-block-display "(cat \"src/foo.clj\")"))))

  (it "renders ctx mutators (task-set! / fact-set!) as visible code"
    ;; The CALL is shown like summarize and every other model form; the
    ;; effect surfaces in the F2 context dialog, so no recap chip and no
    ;; hidden :vis/silent tag. The foundation session-title auto-ack is
    ;; still dropped (separate test above).
    (expect (= [{:kind :code :source "(task-set! :K {:status :done})"}]
              (render/parse-block-display "(task-set! :K {:status :done})")))
    (expect (= [{:kind :code :source "(task-set! :K {:title \"x\"})"}]
              (render/parse-block-display "(task-set! :K {:title \"x\"})")))
    (expect (= [{:kind :code :source "(fact-set! :K {:content \"f\"})"}]
              (render/parse-block-display "(fact-set! :K {:content \"f\"})"))))

  (it "coalesces neighboring hidden tool calls into one hidden segment"
    (let [out (render/parse-block-display
                "(cat \"a.clj\")\n(cat \"b.clj\")")]
      (expect (= 1 (count out)))
      (expect (= :code (-> out first :kind)))))

  (it "def and tool call both render hidden so the bubble shows only recap / result rows"
    (let [out (render/parse-block-display
                "(def x 1)\n(cat \"a.clj\")")]
      ;; Both forms hidden — same :hidden? flag means the coalesce
      ;; pass merges them into one hidden code segment.
      (expect (= 1 (count out)))
      (expect (= :code (-> out first :kind)))))

  (it "marks `(def r (ls …))` + `(select-keys r […])` BOTH hidden so the channel preview speaks alone"
    ;; Real-world fence the model emits when it wants to bind a tool
    ;; result and then project a sub-value. The def wrapping AND the
    ;; accessor row are both bookkeeping; the value rides on the DEF
    ;; SINK + the channel preview of the underlying tool call. The
    ;; renderer coalesces consecutive hidden code segments into one
    ;; entry, so the result is a single suppressed source row.
    (let [out (render/parse-block-display
                "(def r (ls \".\"))\n(select-keys r [:entry-count :file-count])")]
      (expect (= 1 (count out)))
      (expect (= :code (-> out first :kind))))))

(defdescribe markdown-soft-break-test
  (it "prose default collapses a bare newline to a single space (CommonMark)"
    ;; Model-authored answers / thinking are prose: soft wraps are not
    ;; intentional breaks, so they join with a space.
    (expect (= [:ir {} [:p {} [:span {} "line A"] [:span {} " "] [:span {} "line B"]]]
              (render/markdown->ir "line A\nline B")))
    (expect (= [:ir {} [:p {} [:span {} "line A"] [:span {} " "] [:span {} "line B"]]]
              (render/markdown->ir "line A\nline B" nil))))

  (it "`{:soft-break :hard}` lifts every bare newline to [:br] (line-oriented user/paste input)"
    ;; A pasted code/observation dump must keep its line structure;
    ;; otherwise consecutive lines merge into one wrapped wall of text.
    (expect (= [:ir {} [:p {} [:span {} "line A"] [:br {}] [:span {} "line B"]]]
              (render/markdown->ir "line A\nline B" {:soft-break :hard}))))

  (it "hard-break mode preserves indentation + line breaks of a pasted code dump"
    (let [ir (render/markdown->ir "2493:    ;; foo\n2494:    (when x)" {:soft-break :hard})
          [_ _ p] ir]
      ;; Three inline children: line, [:br], line — no space-join that
      ;; would mash "2493: ;; foo 2494: (when x)" onto one row.
      (expect (= [:br {}] (nth p 3)))
      (expect (str/starts-with? (last (nth p 2)) "2493:"))
      (expect (str/starts-with? (last (nth p 4)) "2494:"))))

  (it "hard-break mode keeps paragraph breaks (blank line) as separate [:p] blocks"
    (let [ir (render/markdown->ir "a\nb\n\nc" {:soft-break :hard})]
      (expect (= 2 (count (filter #(and (vector? %) (= :p (first %))) ir))))))

  (it "both arities are idempotent on canonical IR"
    (let [canon [:ir {} [:p {} [:span {} "x"]]]]
      (expect (identical? canon (render/markdown->ir canon)))
      (expect (identical? canon (render/markdown->ir canon {:soft-break :hard}))))))
