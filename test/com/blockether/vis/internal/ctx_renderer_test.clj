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
                    (idx-of ":session/specs")
                    (idx-of ":session/tasks")
                    (idx-of ":session/facts")
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
  (describe "progression annotations on specs"
    (let [out (render base-ctx)]
      (it "emits `;; progression :auth N/M :state` line"
        (expect (str/includes? out ";; progression :auth")))

      (it "shows the partial state derived from the test ctx"
        (expect (str/includes? out ":partial")))

      (it "lists the missing requirement ids"
        (expect (str/includes? out "missing [:r2]"))))))

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
        (let [body (str/replace-first out #"^;; ctx\n" "")]
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

(defdescribe render-trailer-truncation-test
  (describe "trailer truncation"
    (let [many-pins (vec (for [i (range 25)]
                           {:scope (str "t1/i" (inc i))
                            :forms [{:scope (str "t1/i" (inc i) "/f1")
                                     :tag :observation :src "(read)"}]}))
          ctx (assoc base-ctx :session/trailer many-pins)
          out (render ctx)]

      (it "emits a truncation hint when trailer exceeds budget"
        (expect (str/includes? out "trailer truncated")))

      (it "the truncation hint references the actual entry count"
        (expect (str/includes? out " of 25 entries"))))))

(defdescribe render-trailer-result-bounds-test
  (describe "large trailer form results"
    (let [huge (apply str (repeat 5000 "x"))
          trailer [{:scope "t1/i1"
                    :forms [{:scope "t1/i1/f1"
                             :tag :observation
                             :src "(def huge-result (v/cat \"big.clj\"))"
                             :result {:path "big.clj"
                                      :lines [[1 huge]]}}]}]
          ctx (assoc base-ctx :session/trailer trailer)
          out (render ctx)]

      (it "keeps result preview bounded"
        (expect (str/includes? out ":truncated? true"))
        (expect (str/includes? out "...<+"))
        (expect (< (count out) 8000)))

      (it "does not drop source provenance"
        (expect (str/includes? out ";; src t1/i1/f1"))
        (expect (str/includes? out "(def huge-result"))))))

(defdescribe render-empty-subtrees-test
  (describe "empty subtree rendering"
    (let [ctx (eng/empty-ctx "fresh")
          out (render ctx)]
      (it "empty maps render as {}"
        (expect (str/includes? out ":session/specs\n {}"))
        (expect (str/includes? out ":session/tasks\n {}"))
        (expect (str/includes? out ":session/facts\n {}")))

      (it "empty trailer renders as []"
        (expect (str/includes? out ":session/trailer\n []")))

      (it "empty next-actions renders as []"
        (expect (str/includes? out ":session/next-actions\n []"))))))
