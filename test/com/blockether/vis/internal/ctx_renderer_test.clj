(ns com.blockether.vis.internal.ctx-renderer-test
  "Phase G renderer tests. Asserts the prompt-side derived view: pure EDN
   body, a single `:session/warnings` section (vec of short strings,
   rendered only when non-empty), NO trailing `;; …` line-comment
   annotations, trailer pins keep `:form` as native Clojure list (no
   `;; src` verbatim block)."
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
    (assoc-in [:session/tasks :add-bcrypt]
      {:title "add bcrypt dep"
       :status :done
       :done-born "t2/i1/f4"
       :born "t1/i2/f2"})
    (assoc-in [:session/tasks :replace-check]
      {:title "replace literal compare"
       :depends-on [:add-bcrypt]
       :status :doing
       :born "t1/i2/f3"})))

(defn- render
  "Render `ctx` with engine-derived warnings, matching the production
   `{:ctx :warnings}` entry into `render-ctx`."
  [ctx]
  (let [idx      (eng/build-indexes ctx)
        warnings (eng/derive-warnings ctx idx)]
    (r/render-ctx {:ctx ctx :warnings warnings})))

(defdescribe render-ctx-structural-test
  (describe "render-ctx structural output"
    (let [out (render base-ctx)]

      (it "starts with `;; ctx` marker followed directly by `{`"
        (expect (str/starts-with? out ";; ctx\n{")))

      (it "body is pure EDN — no `;; ⚠` / `;; ⛔` / `;; ctx-summary` annotations"
        (let [body (str/replace-first out #"^;; ctx\n" "")]
          (expect (not (str/includes? body ";; ⚠")))
          (expect (not (str/includes? body ";; ⛔")))
          (expect (not (str/includes? body "ctx-summary")))))

      (it "top-level keys appear in deterministic order"
        (let [idx-of (fn [s] (str/index-of out s))]
          (expect (< (idx-of ":session/id")
                    (idx-of ":session/turn")
                    (idx-of ":session/scope")
                    (idx-of ":session/workspace")
                    (idx-of ":session/trailer")))))

      (it "DROPS legacy derived sections"
        (expect (nil? (str/index-of out ":session/specs")))
        (expect (nil? (str/index-of out ":session/stages")))
        (expect (nil? (str/index-of out ":session/timeline")))
        (expect (nil? (str/index-of out ":session/orphans")))
        (expect (nil? (str/index-of out ":session/next-actions"))))

      (it "renders raw entity subtrees that contain data"
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

(defdescribe render-warnings-test
  (describe ":session/warnings vec-of-strings section"

    (it "renders a :session/warnings section when warnings are present"
      (let [out (r/render-ctx {:ctx base-ctx
                               :warnings ["task :t1 :done but dep :t2 is :doing"
                                          "fact :a contradicts :b"]})]
        (expect (str/includes? out ":session/warnings"))
        (expect (str/includes? out "\"task :t1 :done but dep :t2 is :doing\""))
        (expect (str/includes? out "\"fact :a contradicts :b\""))))

    (it "omits the :session/warnings section entirely when empty"
      (let [out (r/render-ctx {:ctx base-ctx :warnings []})]
        (expect (not (str/includes? out ":session/warnings")))))

    (it "omits the :session/warnings section when warnings key absent"
      (let [out (r/render-ctx {:ctx base-ctx})]
        (expect (not (str/includes? out ":session/warnings")))))

    (it "warnings render as pure EDN strings (no `;;` annotations)"
      (let [out  (r/render-ctx {:ctx base-ctx :warnings ["a missing dep"]})
            body (str/replace-first out #"^;; ctx\n" "")]
        (expect (not (str/includes? body ";; ⚠")))))))

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

      (it "keeps short v/cat :range forms on one line"
        (let [ctx (assoc base-ctx
                    :session/trailer
                    [{:scope "t2/i1"
                      :forms [{:scope "t2/i1/f1"
                               :tag :observation
                               :form '(v/cat "src/com/blockether/vis/internal/loop.clj" :range 4380 4525)
                               :result {:ok true}}]}])
              out (render ctx)]
          (expect (str/includes?
                    out
                    ":form (v/cat \"src/com/blockether/vis/internal/loop.clj\" :range 4380 4525)"))))

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

      (it "omits empty :session/tasks / :facts entirely"
        (expect (not (str/includes? out ":session/tasks")))
        (expect (not (str/includes? out ":session/facts"))))

      (it "omits :session/warnings when there are none"
        (expect (not (str/includes? out ":session/warnings"))))

      (it "carries no comment annotations anywhere"
        (let [body (str/replace-first out #"^;; ctx\n" "")]
          (expect (not (str/includes? body ";;"))))))))
