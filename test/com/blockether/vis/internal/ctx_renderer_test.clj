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
    (assoc :session/scope {:turn 2 :iter 1 :next-form 1})
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

      (it "contains every required top-level key in order"
        (let [idx-of (fn [s] (str/index-of out s))]
          (expect (< (idx-of ":session/id")
                    (idx-of ":session/turn")
                    (idx-of ":session/scope")
                    (idx-of ":session/workspace")
                    (idx-of ":session/symbols")
                    (idx-of ":session/hints")
                    (idx-of ":session/specs")
                    (idx-of ":session/tasks")
                    (idx-of ":session/facts")
                    (idx-of ":session/trailer")
                    (idx-of ":session/next-actions")))))

      (it "renders the scope cursor as a literal map"
        (expect (str/includes? out "{:turn 2, :iter 1, :next-form 1}")))

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
