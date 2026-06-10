(ns com.blockether.vis.internal.ctx-renderer-test
  "Renderer tests. The CTX block is a real PYTHON DICT wrapped in a `<context>` tag —
   string keys, snake_case keyword values, Python literals (True/False/None) —
   so it reads exactly like the `context` dict the agent holds in the sandbox. A
   single `session_hints` list (rendered only when non-empty); trailer pins keep
   `form` and drop channel/engine noise keys."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.ctx-engine :as eng]
   [com.blockether.vis.internal.ctx-renderer :as r]
   [lazytest.core :refer [defdescribe describe expect it]]))

(def ^:private base-ctx
  (-> (eng/empty-ctx "test")
    (assoc :session/scope {:turn 2 :iter 1 :next-form 5})
    (assoc :session/turn 2)
    (assoc-in [:session/facts "f1"]
      {:content "auth uses literal compare" :born "t1/i1/f1"})
    (assoc-in [:session/tasks "add-bcrypt"]
      {:title "add bcrypt dep"
       :status :done
       :done-born "t2/i1/f4"
       :born "t1/i2/f2"})
    (assoc-in [:session/tasks "replace-check"]
      {:title "replace literal compare"
       :depends_on ["add-bcrypt"]
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
  (describe "render-ctx Python-dict output"
    (let [out (render base-ctx)]

      (it "is wrapped in a `<context>` … `</context>` tag with a lead-in"
        (expect (str/starts-with? out "<context>\n"))
        (expect (str/ends-with? out "</context>"))
        (expect (str/includes? out "your `context` dict")))

      (it "is a Python dict — string keys, no `:keyword` keys anywhere"
        ;; no namespaced or bare EDN keyword keys: keys are quoted strings
        (expect (nil? (re-find #":session/" out)))
        (expect (str/includes? out "\"session_id\":"))
        (expect (str/includes? out "\"session_turn\":"))
        (expect (nil? (re-find #"\{:" out))))

      (it "renders keyword values as snake_case Python strings"
        ;; status :done -> "done"; :doing -> "doing"
        (expect (str/includes? out "\"status\": \"done\""))
        (expect (str/includes? out "\"status\": \"doing\""))
        ;; a string entity id used as a value renders verbatim (no snaking)
        (expect (str/includes? out "\"depends_on\": [\"add-bcrypt\"]")))

      (it "body carries no Clojure `;;` line-comments"
        (expect (not (str/includes? out ";;"))))

      (it "top-level keys appear in deterministic order"
        (let [idx-of (fn [s] (str/index-of out s))]
          (expect (< (idx-of "\"session_id\"")
                    (idx-of "\"session_turn\"")
                    (idx-of "\"session_scope\"")
                    (idx-of "\"session_workspace\"")
                    (idx-of "\"session_trailer\"")))))

      (it "DROPS legacy derived sections"
        (expect (nil? (str/index-of out "\"session_specs\"")))
        (expect (nil? (str/index-of out "\"session_stages\"")))
        (expect (nil? (str/index-of out "\"session_timeline\"")))
        (expect (nil? (str/index-of out "\"session_next_actions\""))))

      (it "renders raw entity subtrees that contain data"
        (expect (str/includes? out "\"session_tasks\""))
        (expect (str/includes? out "\"session_facts\"")))

      (it "renders the scope cursor as a Python dict with snake_case keys"
        (expect (str/includes? out "\"turn\": 2"))
        (expect (str/includes? out "\"iter\": 1"))
        (expect (str/includes? out "\"next_form\": 5")))

      (it "balanced braces and brackets"
        (let [opens  (count (re-seq #"\{" out))
              closes (count (re-seq #"\}" out))
              bopen  (count (re-seq #"\[" out))
              bclose (count (re-seq #"\]" out))]
          (expect (= opens closes))
          (expect (= bopen bclose)))))))

(defdescribe render-warnings-test
  (describe "session_hints list-of-dicts section"

    (it "renders a session_hints list (as {source, content, importance} dicts) when advisories are present"
      (let [out (r/render-ctx {:ctx base-ctx
                               :warnings ["task t1 done but dep t2 is doing"
                                          "fact a contradicts b"]})]
        (expect (str/includes? out "\"session_hints\""))
        ;; engine advisories are wrapped into hint dicts; their content text survives verbatim
        (expect (str/includes? out "\"task t1 done but dep t2 is doing\""))
        (expect (str/includes? out "\"fact a contradicts b\""))
        (expect (str/includes? out "\"source\":"))
        (expect (str/includes? out "\"importance\":"))))

    (it "omits the session_hints section entirely when empty"
      (let [out (r/render-ctx {:ctx base-ctx :warnings []})]
        (expect (not (str/includes? out "\"session_hints\"")))))

    (it "omits the session_hints section when warnings key absent"
      (let [out (r/render-ctx {:ctx base-ctx})]
        (expect (not (str/includes? out "\"session_hints\"")))))))

(defdescribe render-trailer-form-pin-test
  (describe "trailer form pin is a Python dict; form is a list"
    (let [trailer [{:scope "t2/i1"
                    :forms [{:scope "t2/i1/f1"
                             :tag :observation
                             :src "(ls \".\" :depth 1)"
                             :form '(ls "." :depth 1)
                             :channel [{:fake :stripped}]
                             :form-idx 0 :position 0 :success? true :symbol 'ls
                             :result {:op :ls :path "." :entry-count 3}}]}]
          ctx     (assoc base-ctx :session/trailer trailer)
          out     (render ctx)]

      (it "drops the `;; src` verbatim comment block"
        (expect (not (str/includes? out ";; src "))))

      (it "renders `form` as a Python list of tokens"
        (expect (str/includes? out "\"form\": [\"ls\", \".\", \"depth\", 1]")))

      (it "renders tool/trailer forms across arbitrary aliases (head symbols survive)"
        (let [forms   ['(cat "src/loop.clj" :range 4380 4525)
                       '(rg {:any ["title"] :paths ["src"] :limit 20})
                       '(patch [{:path "x" :search "a" :replace "b"}])
                       '(git/status)
                       '(task-set! "ship" {:status :done})]
              heads   ["\"cat\"" "\"rg\"" "\"patch\"" "\"git/status\"" "\"task_set\""]
              ctx     (assoc base-ctx
                        :session/trailer
                        [{:scope "t2/i1"
                          :forms (mapv (fn [idx form]
                                         {:scope (str "t2/i1/f" idx)
                                          :tag :observation
                                          :form form
                                          :result {:ok true}})
                                   (range 1 (inc (count forms)))
                                   forms)}])
              out     (render ctx)]
          (doseq [head heads]
            (expect (str/includes? out head)))))

      (it "strips all noise keys (channel, src, form_idx, position, success, symbol)"
        (let [pin-region (subs out (str/index-of out "\"session_trailer\""))]
          (expect (not (str/includes? pin-region "\"channel\"")))
          (expect (not (str/includes? pin-region "\"form_idx\"")))
          (expect (not (str/includes? pin-region "\"position\":")))
          (expect (not (str/includes? pin-region "\"success\"")))
          (expect (not (str/includes? pin-region "\"symbol\"")))
          (expect (not (str/includes? pin-region "\"src\":")))))

      (it "keeps scope, tag, result on the pin"
        (let [pin-region (subs out (str/index-of out "\"session_trailer\""))]
          (expect (str/includes? pin-region "\"scope\": \"t2/i1/f1\""))
          (expect (str/includes? pin-region "\"tag\": \"observation\""))
          (expect (str/includes? pin-region "\"result\":")))))))

(defdescribe render-trailer-summary-pin-test
  (describe "summary trailer pins (from summarize) render as dicts"
    (let [trailer [{:scope-start "t3/i1" :scope-end "t3/i5"
                    :summary "explored auth flow"
                    :born "t4/i1/f1"}]
          ctx     (assoc base-ctx :session/trailer trailer)
          out     (render ctx)]

      (it "renders the summary text inline"
        (expect (str/includes? out "explored auth flow")))

      (it "carries the snake_case scope_start / scope_end keys"
        (expect (str/includes? out "\"scope_start\": \"t3/i1\""))
        (expect (str/includes? out "\"scope_end\": \"t3/i5\""))))))

(defdescribe render-empty-ctx-test
  (describe "empty ctx renders cleanly"
    (let [clean-ctx (-> (eng/empty-ctx "clean")
                      (assoc :session/scope {:turn 1 :iter 1 :next-form 1})
                      (assoc :session/turn 1))
          out       (render clean-ctx)]

      (it "is wrapped in `<context>` … `</context>` (no preamble banner)"
        (expect (str/starts-with? out "<context>\n"))
        (expect (str/ends-with? out "</context>")))

      (it "omits empty session_tasks / session_facts entirely"
        (expect (not (str/includes? out "\"session_tasks\"")))
        (expect (not (str/includes? out "\"session_facts\""))))

      (it "omits session_hints when there are none"
        (expect (not (str/includes? out "\"session_hints\""))))

      (it "carries no Clojure comment annotations"
        (let [body (str/replace-first out #"^<context>\n" "")]
          (expect (not (str/includes? body ";;"))))))))
