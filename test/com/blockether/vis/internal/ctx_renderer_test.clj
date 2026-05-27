(ns com.blockether.vis.internal.ctx-renderer-test
  "Phase G renderer tests. Asserts the prompt-side derived view: pure EDN
   body, single `:session/plan` section (replacing the legacy
   :session/timeline + :session/orphans + :session/next-actions triplet),
   NO trailing `;; …` line-comment annotations, trailer pins keep `:form`
   as native Clojure list (no `;; src` verbatim block)."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.ctx-engine :as eng]
   [com.blockether.vis.internal.ctx-renderer :as r]
   [lazytest.core :refer [defdescribe describe expect it]]))

(def ^:private base-ctx
  (-> (eng/empty-ctx "test")
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
  (let [idx  (eng/build-indexes ctx)
        prog (eng/derive-progression ctx idx)
        plan (eng/derive-plan ctx idx prog)]
    (r/render-ctx {:ctx ctx :progression prog :next-actions plan})))

(defdescribe render-ctx-structural-test
  (describe "render-ctx structural output"
    (let [out (render base-ctx)]

      (it "starts with `;; ctx` marker followed directly by `{`"
        (expect (str/starts-with? out ";; ctx\n{")))

      (it "body is pure EDN \u2014 no `;; \u26a0` / `;; \u26d4` / `;; ctx-summary` annotations"
        (let [body (str/replace-first out #"^;; ctx\n" "")]
          (expect (not (str/includes? body ";; \u26a0")))
          (expect (not (str/includes? body ";; \u26d4")))
          (expect (not (str/includes? body "ctx-summary")))))

      (it "top-level keys appear in deterministic order"
        (let [idx-of (fn [s] (str/index-of out s))]
          (expect (< (idx-of ":session/id")
                    (idx-of ":session/turn")
                    (idx-of ":session/scope")
                    (idx-of ":session/workspace")
                    (idx-of ":session/trailer")
                    (idx-of ":session/plan")))))

      (it "DROPS legacy sections that were folded into :session/plan"
        (expect (nil? (str/index-of out ":session/timeline")))
        (expect (nil? (str/index-of out ":session/orphans")))
        (expect (nil? (str/index-of out ":session/next-actions"))))

      (it "renders raw entity subtrees that contain data"
        (expect (str/includes? out ":session/specs"))
        (expect (str/includes? out ":session/tasks"))
        (expect (str/includes? out ":session/facts")))

      (it "renders the scope cursor as sorted bare-EDN map (no commas)"
        (expect (str/includes? out ":turn 2"))
        (expect (str/includes? out ":iter 1"))
        (expect (str/includes? out ":next-form 5"))
        (expect (not (re-find #"\{:iter 1, " out))))

      (it "balanced braces"
        (let [opens (count (re-seq #"\{" out))
              closes (count (re-seq #"\}" out))]
          (expect (= opens closes)))))))

(defdescribe render-plan-test
  (describe ":session/plan flat ordered vec"
    (let [out (render base-ctx)]

      (it "has the :session/plan section"
        (expect (str/includes? out ":session/plan")))

      (it "renders at least one action entry with canonical keys"
        (expect (re-find #":kind :(prove-requirement|work-unblocked-todo|fix-consistency|blocker)" out))
        (expect (str/includes? out ":status :"))
        (expect (str/includes? out ":remedy "))))))

(defdescribe render-plan-with-blocker-test
  (describe "engine blockers appear FIRST in :session/plan"
    (let [ctx-with-blocker (assoc base-ctx :engine/blockers
                             [{:id     :missing-title
                               :reason "Set the title"
                               :remedy '(set-session-title! "X")}])
          out              (render ctx-with-blocker)]

      (it "renders the blocker as a :session/plan entry"
        (expect (str/includes? out ":kind :blocker"))
        (expect (str/includes? out ":id :missing-title"))
        (expect (str/includes? out ":status :blocked"))
        (expect (str/includes? out ":remedy (set-session-title! \"X\")")))

      (it "blocker entry sits at the head of :session/plan"
        (let [plan-start    (str/index-of out ":session/plan")
              blocker-idx   (str/index-of out ":kind :blocker" plan-start)
              non-blocker   (some #(str/index-of out (str ":kind " %) plan-start)
                              [":prove-requirement" ":work-unblocked-todo"])]
          (expect (some? blocker-idx))
          (when non-blocker
            (expect (< blocker-idx non-blocker))))))))

(defdescribe render-plan-overflow-test
  (describe ":session/plan overflow tag stays in-vec (no trailing comment)"
    (let [many-tasks (into {}
                       (for [i (range 10)]
                         [(keyword (str "todo-" i))
                          {:title (str "todo-" i) :status :todo :born "t1/i1/f1"
                           :specs {}}]))
          ctx        (update base-ctx :session/tasks merge many-tasks)
          out        (render ctx)]

      (it "appends overflow as an in-vec map (NOT a trailing `;;` comment)"
        (expect (str/includes? out ":overflow "))
        (expect (str/includes? out ":hint \"raise NEXT_ACTIONS_BUDGET"))
        (expect (not (str/includes? out ";; more action(s)")))))))

(defdescribe render-trailer-form-pin-test
  (describe "trailer form pin is one Clojure map, :form is a native list"
    (let [trailer [{:scope "t2/i1"
                    :forms [{:scope "t2/i1/f1"
                             :tag :observation
                             :src "(v/ls \".\" :depth 1)"
                             :form '(v/ls "." :depth 1)
                             :channel [{:fake :stripped}]
                             :form-idx 0 :position 0 :success? true :symbol 'ls
                             :result {:vis.op :v/ls :path "." :entry-count 3}}]}]
          ctx     (assoc base-ctx :session/trailer trailer)
          out     (render ctx)]

      (it "drops `;; src <scope>:` verbatim comment block"
        (expect (not (str/includes? out ";; src "))))

      (it "keeps `:form (v/ls \".\" :depth 1)` as native list inside the map"
        (expect (str/includes? out ":form (v/ls \".\" :depth 1)")))

      (it "strips all noise keys (:channel :src :form-idx :position :success? :symbol)"
        (let [pin-region (subs out (str/index-of out ":session/trailer"))]
          (expect (not (str/includes? pin-region ":channel")))
          (expect (not (str/includes? pin-region ":form-idx")))
          (expect (not (str/includes? pin-region ":position 0")))
          (expect (not (str/includes? pin-region ":success?")))
          (expect (not (str/includes? pin-region ":symbol ")))
          ;; :src is the STRING copy; the native :form list survives instead
          (expect (not (str/includes? pin-region ":src \"")))))

      (it "keeps :scope :tag :result on the pin"
        (let [pin-region (subs out (str/index-of out ":session/trailer"))]
          (expect (str/includes? pin-region ":scope \"t2/i1/f1\""))
          (expect (str/includes? pin-region ":tag :observation"))
          (expect (str/includes? pin-region ":result")))))))

(defdescribe render-trailer-summary-pin-test
  (describe "summary trailer pins (from done :trailer-summarize) render unchanged"
    (let [trailer [{:scope-start "t3/i1" :scope-end "t3/i5"
                    :summary "explored auth flow"
                    :born "t4/i1/f1"}]
          ctx     (assoc base-ctx :session/trailer trailer)
          out     (render ctx)]

      (it "renders the summary text inline"
        (expect (str/includes? out "explored auth flow")))

      (it "carries the :scope-start / :scope-end keys"
        (expect (str/includes? out ":scope-start \"t3/i1\""))
        (expect (str/includes? out ":scope-end \"t3/i5\""))))))

(defdescribe render-empty-ctx-test
  (describe "empty ctx renders cleanly"
    (let [clean-ctx (-> (eng/empty-ctx "clean")
                      (assoc :session/scope {:turn 1 :iter 1 :next-form 1})
                      (assoc :session/turn 1))
          out       (render clean-ctx)]

      (it "starts with `;; ctx\\n{` (no preamble banner)"
        (expect (str/starts-with? out ";; ctx\n{")))

      (it "shows empty :session/plan as []"
        (expect (str/includes? out ":session/plan\n []")))

      (it "omits empty :session/specs / :tasks / :facts entirely"
        (expect (not (str/includes? out ":session/specs"))))

      (it "carries no comment annotations anywhere"
        (let [body (str/replace-first out #"^;; ctx\n" "")]
          (expect (not (str/includes? body ";;"))))))))
