(ns com.blockether.vis.internal.ctx-renderer-test
  "Tests for the ;; ctx text renderer. Asserts shape, ordering, and that
   warnings + progression annotations land inline next to the right entries."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.ctx-engine :as eng]
   [com.blockether.vis.internal.ctx-renderer :as r]
   [lazytest.core :refer [defdescribe describe expect it]]))

(def ^:private base-ctx
  (-> (eng/empty-ctx "test")
    ;; cursor sits past the proof-form so :future-form does not fire in tests
    (assoc :session/scope {:turn 2 :iter 1 :next-form 5})
    (assoc :session/turn 2)
    (assoc-in [:session/facts :f1]
      {:content "auth uses literal compare" :born "t1/i1/f1"})
    (assoc-in [:session/specs :auth]
      {:title "switch to bcrypt"
       :requirements [{:id :r1 :title "check uses bcrypt" :facts [:f1]}
                      {:id :r2 :title "wrong path covered"}]
       :status :doing
       :born "t1/i2/f1"})
    (assoc-in [:session/tasks :add-bcrypt]
      {:title "add bcrypt dep"
       :specs {:auth [{:requirement :r1 :proof "t2/i1/f3"}]}
       :status :done
       :done-born "t2/i1/f4"
       :born "t1/i2/f2"})
    (assoc-in [:session/tasks :replace-check]
      {:title "replace literal compare"
       :specs {:auth []}
       :depends-on [:add-bcrypt]
       :status :doing
       :born "t1/i2/f3"})))

(defn- render [ctx]
  (let [idx (eng/build-indexes ctx)
        prog (eng/derive-progression ctx idx)
        warns (eng/derive-warnings ctx idx)
        acts (eng/derive-next-actions ctx idx prog)]
    (r/render-ctx {:ctx ctx :warnings warns :progression prog :next-actions acts})))

(defdescribe render-ctx-structural-test
  (describe "render-ctx structural output"
    (let [out (render base-ctx)]

      (it "starts with the `;; ctx` marker"
        (expect (str/starts-with? out ";; ctx\n")))

      (it "is a bare EDN map (starts with `{`, ends with `}`)"
        (let [body (str/replace-first out #"^;; ctx\n" "")]
          (expect (str/starts-with? body "{"))
          (expect (str/ends-with? body "}"))))

      (it "contains every required top-level key in order (D12: no :session/hints)"
        (let [idx-of (fn [s] (str/index-of out s))]
          (expect (< (idx-of ":session/id")
                    (idx-of ":session/turn")
                    (idx-of ":session/scope")
                    (idx-of ":session/workspace")
                    (idx-of ":session/symbols")
                    (idx-of ":session/timeline")
                    (idx-of ":session/orphans")
                    (idx-of ":session/trailer")
                    (idx-of ":session/next-actions")))))

      (it "does NOT render a :session/hints section"
        (expect (nil? (str/index-of out ":session/hints"))))

      (it "renders the scope cursor as a sorted bare-EDN map (no commas)"
        (expect (str/includes? out ":turn 2"))
        (expect (str/includes? out ":iter 1"))
        (expect (str/includes? out ":next-form 5"))
        ;; bare-EDN: no commas anywhere in the scope cursor block
        (expect (not (re-find #"\{:iter 1, " out))))

      (it "renders facts with their :content"
        (expect (str/includes? out "auth uses literal compare")))

      (it "renders task :depends-on"
        (expect (str/includes? out ":depends-on")))

      (it "balanced braces"
        (let [opens (count (re-seq #"\{" out))
              closes (count (re-seq #"\}" out))]
          (expect (= opens closes)))))))

(defdescribe render-progression-test
  ;; Phase G': progression is no longer a separate `;; progression :K`
  ;; annotation line. It rides INSIDE the projected spec entry as
  ;; `:progress "N/M (P%) state"` + `:missing [:rid …]`. The model reads
  ;; the same signal but inside the EDN value instead of out-of-band.
  (describe "progression embedded in projected spec"
    (let [out (render base-ctx)]
      (it "includes the projected :progress string for the spec"
        (expect (str/includes? out ":progress"))
        (expect (str/includes? out "1/2")))

      (it "surfaces the partial state in human-readable form"
        (expect (str/includes? out "partial")))

      (it "lists the missing requirement ids"
        (expect (str/includes? out ":missing [:r2]"))))))

(defdescribe render-warnings-test
  (describe "inline warning annotations"
    (let [ctx (-> base-ctx
                ;; introduce a dangling fact ref so a warning is emitted
                (assoc-in [:session/specs :auth :requirements 0 :facts] [:f1 :nope])
                ;; introduce a dangling spec ref on a task
                (assoc-in [:session/tasks :replace-check :specs :ghost-spec] []))
          out (render ctx)]

      (it "emits `;; ⚠` for the dangling fact ref under specs"
        (expect (str/includes? out ";; ⚠")))

      (it "the warning text mentions the dangling fact id"
        (expect (str/includes? out ":nope")))

      (it "still produces a balanced EDN body"
        ;; ;; ctx header + optional top-of-ctx summary banner (one or more
        ;; ;; ⚠ ctx-summary: … lines) precede the bare-EDN body. Strip every
        ;; leading `;; …` comment line, then assert the remainder is the
        ;; bare-EDN map starting with `{` and ending with `}`.
        (let [body (str/replace-first out #"\A(?:;;[^\n]*\n)+" "")]
          (expect (str/starts-with? body "{"))
          (expect (str/ends-with? body "}")))))))

(defdescribe render-next-actions-test
  (describe "next-actions section"
    (let [out (render base-ctx)]
      (it "contains the :session/next-actions key"
        (expect (str/includes? out ":session/next-actions")))

      (it "renders at least one suggested action"
        (expect (re-find #":type :(prove-requirement|work-unblocked-todo|review-spec|review-task)"
                  out))))))

(defdescribe render-blocking-banner-test
  (describe "P1 fix-consistency actions render as `;; ⛔ BLOCKING` banner"
    ;; Force a P1 action: flip an unsatisfied spec to :done so
    ;; derive-next-actions emits a `:review-spec` with `:blocking? true`.
    ;; The renderer must surface a `;; ⛔ BLOCKING action(s)` banner so
    ;; the model cannot scan past the next-actions list and miss it.
    (let [ctx (-> base-ctx
                (assoc-in [:session/specs :auth :status] :done))
          out (render ctx)]

      (it "emits the BLOCKING banner before the next-actions vec"
        (expect (str/includes? out ";; ⛔"))
        (expect (str/includes? out "BLOCKING action(s)")))

      (it "lists each blocking action's hint inline so the fix is one read away"
        (expect (str/includes? out ":auth :done but"))
        (expect (str/includes? out ";;   → ")))

      (it "keeps the :blocking? flag on the rendered action entry"
        (expect (str/includes? out ":blocking? true"))))))

(defdescribe render-ctx-summary-banner-test
  (describe "top-of-ctx summary banner counts warnings and blocking actions"
    ;; The summary banner sits between `;; ctx` and the bare-EDN body.
    ;; It exists so a model scanning the ctx fast knows up-front that the
    ;; symbolic critic has something to say — even if the trailer is long
    ;; and the next-actions section is far down.
    (let [ctx (-> base-ctx
                ;; dangling fact ref ⇒ warning
                (assoc-in [:session/specs :auth :requirements 0 :facts] [:f1 :nope])
                ;; spec :done with unproven req ⇒ blocking P1 action
                (assoc-in [:session/specs :auth :status] :done))
          out (render ctx)]

      (it "emits a `;; ⚠ ctx-summary:` line just after `;; ctx`"
        (expect (re-find #"\A;; ctx\n;; ⚠ ctx-summary:" out)))

      (it "reports both warning and blocking counts"
        (expect (str/includes? out "warning(s)"))
        (expect (str/includes? out "blocking action(s)")))

      (it "points the model at the actions section explicitly"
        (expect (str/includes? out "read :session/next-actions first"))))

    ;; Clean ctx (no warnings, no blocking actions) ⇒ no banner. Otherwise
    ;; every prompt would carry it as cosmetic noise.
    (let [clean-ctx (-> (eng/empty-ctx "clean")
                      (assoc :session/scope {:turn 1 :iter 1 :next-form 1})
                      (assoc :session/turn 1))
          out (render clean-ctx)]
      (it "omits the banner entirely when nothing is wrong"
        (expect (not (str/includes? out "ctx-summary")))
        (expect (not (str/includes? out "⛔")))))))

(defdescribe render-next-actions-overflow-test
  (describe "next-actions overflow surfaces the suppressed count"
    ;; Engine now returns the FULL ranked list (cap removed from
    ;; derive-next-actions). Renderer caps at NEXT_ACTIONS_BUDGET and
    ;; appends a `;; N more action(s) suppressed` hint so the model sees
    ;; the backlog pressure instead of a silently-truncated list.
    (let [;; spawn many unblocked todo tasks so the engine emits > 5 actions
          many-tasks (into {}
                       (for [i (range 10)]
                         [(keyword (str "todo-" i))
                          {:title (str "todo-" i) :status :todo :born "t1/i1/f1"
                           :specs {}}]))
          ctx (-> base-ctx
                (update :session/tasks merge many-tasks))
          out (render ctx)]

      (it "includes the `more action(s) suppressed` overflow hint"
        (expect (str/includes? out "more action(s) suppressed"))))))

(defdescribe render-trailer-src-verbatim-test
  (describe "trailer :src survives without quote-escape corruption"
    ;; Repro: prior to the verbatim-render fix, the trailer entry
    ;;   {:src "(str \"/\" name)"}
    ;; rendered as `:src "(str \\"/\\" name)"`. The model read the visible
    ;; `\"` as backslash-quote and copied that into the next iter's source,
    ;; producing an SCI parse error ("EOF while reading, expected \" to
    ;; match \""). After the fix the source must appear inside a `;; src …`
    ;; comment block with the inner quotes UNESCAPED.
    (let [trailer [{:scope "t18/i9"
                    :forms [{:scope "t18/i9/f1"
                             :tag :observation
                             :result :ok
                             :src "(str \"/\" name)"}]}]
          ctx (assoc base-ctx :session/trailer trailer)
          out (render ctx)]

      (it "contains the verbatim source with bare double-quotes"
        (expect (str/includes? out ";;   (str \"/\" name)")))

      (it "does NOT emit the backslash-escaped form of the source"
        ;; the corruption-by-zprint pattern is exactly two chars: `\"`
        (expect (not (str/includes? out "(str \\\"/\\\" name)"))))

      (it "prefixes the block with `;; src <scope> (<tag>):`"
        (expect (str/includes? out ";; src t18/i9/f1 (observation):")))

      (it "strips :src from the rendered form map (verbatim block carries it)"
        (let [body (subs out (str/index-of out ";; src t18/i9/f1"))]
          (expect (not (str/includes? body ":src \""))))))

    ;; Multi-line source survives as a multi-line comment block. The model
    ;; must see every original line; no `\n` escape sneaks in.
    (let [src "(let [x 1\n      y 2]\n  (str \"/\" x y))"
          trailer [{:scope "t2/i1"
                    :forms [{:scope "t2/i1/f1" :tag :mutation
                             :result :ok :src src}]}]
          ctx (assoc base-ctx :session/trailer trailer)
          out (render ctx)]

      (it "emits each source line as its own `;;   ` comment line"
        (expect (str/includes? out ";;   (let [x 1"))
        (expect (str/includes? out ";;         y 2]"))
        (expect (str/includes? out ";;     (str \"/\" x y))")))

      (it "does not leak a literal `\\n` escape into the rendered text"
        (expect (not (str/includes? out "\\n      y 2")))))))

(defdescribe render-trailer-summary-pin-test
  (describe "summary trailer pins render unchanged"
    ;; Summary pins (from (done {:trailer-summarize …})) have no :src field;
    ;; their `:summary` string is data and stays as a Clojure-escaped string,
    ;; so the model can still parse it as EDN if it wants. We only special-
    ;; case `:forms` pins.
    (let [trailer [{:born "t17/i3/f1"
                    :scope-start "t1/i1"
                    :scope-end "t1/i14"
                    :summary "Worked on Ctrl+B \"/voice\" handler."}]
          ctx (assoc base-ctx :session/trailer trailer)
          out (render ctx)]
      (it "summary pin remains a normal EDN map"
        (expect (str/includes? out ":scope-start \"t1/i1\""))
        (expect (str/includes? out ":scope-end \"t1/i14\""))
        (expect (str/includes? out ":summary"))))))

(defdescribe render-trailer-no-entry-cap-test
  ;; Regression: the trailer used to cap pins at TRAILER_BUDGET (16) and
  ;; append a `;; ⚠ trailer truncated: 16 of N entries…` hint. That cap
  ;; was removed (see ctx_renderer.clj header note + conversation
  ;; ccee2e1f-16ee-4acf-8d93-b4505034c0de). The model needs the full
  ;; trailer to reason about prior iters; controlling trailer size is the
  ;; upstream pin-policy's job, not a silent renderer cap.
  (describe "trailer with many pins is rendered verbatim"
    (let [many-pins (vec (for [i (range 25)]
                           {:scope (str "t1/i" (inc i))
                            :forms [{:scope (str "t1/i" (inc i) "/f1")
                                     :tag :observation :src "(read)"}]}))
          ctx (assoc base-ctx :session/trailer many-pins)
          out (render ctx)]

      (it "does NOT emit the legacy `trailer truncated` hint"
        (expect (not (str/includes? out "trailer truncated"))))

      (it "every pin scope is present in the rendered trailer (no entries dropped)"
        (doseq [i (range 25)]
          (expect (str/includes? out (str ":scope \"t1/i" (inc i) "\""))))))))

(defdescribe render-trailer-result-full-passthrough-test
  ;; Regression: trailer form results used to be capped at
  ;; TRAILER_FORM_RESULT_MAX_CHARS (1200) and replaced with
  ;; {:preview … :truncated? true}. The model then perceived its own
  ;; bound values as missing and reached for `(println x)` (banned),
  ;; producing the I/O-side-effects error. The cap is gone — trailer
  ;; results render verbatim; if a result is too large for the prompt,
  ;; the upstream pin policy must address it at source, not silently
  ;; here.
  (describe "large trailer form results pass through untruncated"
    (let [huge (apply str (repeat 5000 "x"))
          trailer [{:scope "t1/i1"
                    :forms [{:scope "t1/i1/f1"
                             :tag :observation
                             :src "(def huge-result (v/cat \"big.clj\"))"
                             :result {:path "big.clj"
                                      :lines [[1 huge]]}}]}]
          ctx (assoc base-ctx :session/trailer trailer)
          out (render ctx)]

      (it "renders the full result with no truncation marker"
        (expect (not (str/includes? out ":truncated? true")))
        (expect (not (str/includes? out "...<+")))
        (expect (not (str/includes? out ":preview"))))

      (it "renders every byte of the underlying payload"
        (expect (str/includes? out huge))
        (expect (> (count out) 5000)))

      (it "does not drop source provenance"
        (expect (str/includes? out ";; src t1/i1/f1"))
        (expect (str/includes? out "(def huge-result"))))))

(defdescribe render-empty-subtrees-test
  ;; Phase G': raw `:session/specs`, `:session/tasks`, `:session/facts`
  ;; are no longer top-level rendered sections; the renderer projects
  ;; them into `:session/timeline` (task-rooted) + `:session/orphans`.
  ;; On a fresh ctx both should be empty.
  (describe "empty subtree rendering"
    (let [ctx (eng/empty-ctx "fresh")
          out (render ctx)]
      (it "empty projection emits :session/timeline as []"
        (expect (str/includes? out ":session/timeline\n []")))

      (it "empty projection emits :session/orphans as {}"
        (expect (str/includes? out ":session/orphans\n {}")))

      (it "empty trailer renders as []"
        (expect (str/includes? out ":session/trailer\n []")))

      (it "empty next-actions renders as []"
        (expect (str/includes? out ":session/next-actions\n []"))))))

(defdescribe render-cyclic-dep-graph-test
  ;; Regression: switching sessions in the TUI hung because resumed
  ;; ctx snapshots (predating the Phase B write-time cycle check) can
  ;; carry `:depends-on` cycles in the dep-graph. `build-timeline`'s
  ;; BFS re-enqueued nodes regardless of seen-state, so any cycle
  ;; — even a 2-cycle through spec↔fact — spun forever and pegged a
  ;; CPU. Diamond shapes also exploded the queue exponentially.
  ;;
  ;; The renderer must TERMINATE on any dep-graph it is handed, even
  ;; one the live engine would refuse to write today.
  (describe "cyclic dep-graph rendering"
    (it "renders a spec↔fact cycle without hanging"
      (let [ctx (-> (eng/empty-ctx "cyclic")
                  (assoc-in [:session/tasks :T]
                    {:title "t" :status :todo :depends-on [[:spec :A]]
                     :born "t1/i1/f1"})
                  (assoc-in [:session/specs :A]
                    {:title "a" :status :active :depends-on [[:fact :F]]
                     :requirements [] :born "t1/i1/f2"})
                  (assoc-in [:session/facts :F]
                    {:content "f" :status :active :depends-on [[:spec :A]]
                     :born "t1/i1/f3"}))
            ;; Render in a future with a hard deadline; pre-fix this
            ;; loops forever and the future never completes.
            fut (future (render ctx))
            done? (try (deref fut 3000 ::timeout)
                    (finally (future-cancel fut)))]
        (expect (not (= ::timeout done?)))
        (expect (string? done?))
        (expect (str/includes? done? ":session/timeline"))))))
