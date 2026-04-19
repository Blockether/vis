(ns com.blockether.vis.loop.system-vars-test
  "Tests for the earmuffed SYSTEM vars (*query*, *reasoning*, *answer*) —
   they must ALWAYS appear in <var_index>, be marked `(SYSTEM …) …`, sort
   first, and NEVER be forgotten (neither auto-forget nor explicit :forget)."
  (:require
    [clojure.string :as str]
    [lazytest.core :refer [defdescribe describe expect it]]
    [sci.core :as sci]
    [com.blockether.vis.loop.core :as rlm-core]
    [com.blockether.vis.loop.runtime.core :as runtime]))

(defn- make-ctx []
  (let [{:keys [sci-ctx sandbox-ns initial-ns-keys]}
        (runtime/create-sci-context (fn [_] {:content "ok"}) nil nil nil)]
    {:sci-ctx sci-ctx :sandbox-ns sandbox-ns :initial-ns-keys initial-ns-keys}))

(defn- eval-in [ctx code]
  (:val (sci/eval-string+ (:sci-ctx ctx) code {:ns (:sandbox-ns ctx)})))

(defdescribe system-vars-in-var-index
  (describe "earmuffed SYSTEM vars appear in <var_index>"
    (it "*query* shows up with (SYSTEM, never forgotten) doc"
      (let [{:keys [sci-ctx initial-ns-keys] :as ctx} (make-ctx)]
        (eval-in ctx "(def *query* \"what is 2+2\")")
        (let [idx (runtime/build-var-index sci-ctx initial-ns-keys)]
          (expect (string? idx))
          (expect (str/includes? idx "*query*"))
          (expect (str/includes? idx "(SYSTEM, never forgotten) current user query")))))

    (it "*reasoning* shows up with (SYSTEM, never forgotten) doc"
      (let [{:keys [sci-ctx initial-ns-keys] :as ctx} (make-ctx)]
        (eval-in ctx "(def *reasoning* \"I will compute it\")")
        (let [idx (runtime/build-var-index sci-ctx initial-ns-keys)]
          (expect (str/includes? idx "*reasoning*"))
          (expect (str/includes? idx "(SYSTEM, never forgotten) YOUR thinking")))))

    (it "*answer* shows up with (SYSTEM, never forgotten) doc"
      (let [{:keys [sci-ctx initial-ns-keys] :as ctx} (make-ctx)]
        (eval-in ctx "(def *answer* \"4\")")
        (let [idx (runtime/build-var-index sci-ctx initial-ns-keys)]
          (expect (str/includes? idx "*answer*"))
          (expect (str/includes? idx "(SYSTEM, never forgotten) final answer from the previous turn")))))

    (it "all three SYSTEM vars sort BEFORE user vars"
      (let [{:keys [sci-ctx initial-ns-keys] :as ctx} (make-ctx)]
        (eval-in ctx "(def user-var \"my data\" [1 2 3])")
        (eval-in ctx "(def *query* \"q\")")
        (eval-in ctx "(def *reasoning* \"r\")")
        (eval-in ctx "(def *answer* \"a\")")
        (let [idx (runtime/build-var-index sci-ctx initial-ns-keys)
              query-pos     (str/index-of idx "*query*")
              reasoning-pos (str/index-of idx "*reasoning*")
              answer-pos    (str/index-of idx "*answer*")
              user-pos      (str/index-of idx "user-var")]
          (expect (every? some? [query-pos reasoning-pos answer-pos user-pos]))
          (expect (< query-pos     user-pos))
          (expect (< reasoning-pos user-pos))
          (expect (< answer-pos    user-pos)))))

    (it "SYSTEM doc overrides any user-attached docstring"
      ;; Even if the LLM (or something else) attaches a docstring to an
      ;; earmuffed var, the SYSTEM doc wins so the label is consistent.
      (let [{:keys [sci-ctx initial-ns-keys] :as ctx} (make-ctx)]
        (eval-in ctx "(def *query* \"user-supplied docstring\" \"q\")")
        (let [idx (runtime/build-var-index sci-ctx initial-ns-keys)]
          (expect (str/includes? idx "(SYSTEM, never forgotten)"))
          (expect (not (str/includes? idx "user-supplied docstring"))))))

    (it "unknown earmuffed var still gets a generic SYSTEM label"
      (let [{:keys [sci-ctx initial-ns-keys] :as ctx} (make-ctx)]
        (eval-in ctx "(def *future-system* \"value\")")
        (let [idx (runtime/build-var-index sci-ctx initial-ns-keys)]
          (expect (str/includes? idx "(SYSTEM, never forgotten) agent-bound var")))))

    (it "single-char * names are NOT treated as earmuffed (only real earmuffs)"
      ;; Guard: the earmuffed? check must require length > 2 so the degenerate
      ;; name `*` doesn't get SYSTEM treatment.
      (let [{:keys [sci-ctx initial-ns-keys] :as ctx} (make-ctx)]
        (eval-in ctx "(def x \"user var\" 1)")
        (let [idx (runtime/build-var-index sci-ctx initial-ns-keys)]
          (expect (not (str/includes? idx "(SYSTEM"))))))))

(defdescribe system-vars-never-forgotten
  (describe "forget-vars! refuses to drop earmuffed SYSTEM vars"
    (it "keeps *query* even when explicitly listed in :forget"
      (let [{:keys [sci-ctx] :as ctx} (make-ctx)]
        (eval-in ctx "(def *query* \"my query\")")
        (eval-in ctx "(def *answer* \"42\")")
        (eval-in ctx "(def user-var \"user data\")")
        ;; Mix SYSTEM + user vars in a :forget request
        (#'rlm-core/forget-vars! sci-ctx ['*query* '*answer* 'user-var])
        ;; SYSTEM vars survive
        (expect (= "my query" (eval-in ctx "*query*")))
        (expect (= "42"       (eval-in ctx "*answer*")))
        ;; User var is actually dropped
        (let [user-after (try (eval-in ctx "user-var")
                           (catch clojure.lang.ExceptionInfo _ ::unbound))]
          (expect (= ::unbound user-after)))))

    (it "string-named SYSTEM vars in :forget also refused"
      (let [{:keys [sci-ctx] :as ctx} (make-ctx)]
        (eval-in ctx "(def *reasoning* \"thought\")")
        (#'rlm-core/forget-vars! sci-ctx ["*reasoning*"])
        (expect (= "thought" (eval-in ctx "*reasoning*")))))

    (it "unknown earmuffed vars (*foo*) are also never forgotten — policy applies by shape, not by name"
      (let [{:keys [sci-ctx] :as ctx} (make-ctx)]
        (eval-in ctx "(def *custom* \"v\")")
        (#'rlm-core/forget-vars! sci-ctx ['*custom*])
        (expect (= "v" (eval-in ctx "*custom*")))))

    (it "non-earmuffed vars are still forgettable"
      (let [{:keys [sci-ctx] :as ctx} (make-ctx)]
        (eval-in ctx "(def regular 123)")
        (#'rlm-core/forget-vars! sci-ctx ['regular])
        (let [after (try (eval-in ctx "regular")
                      (catch clojure.lang.ExceptionInfo _ ::unbound))]
          (expect (= ::unbound after)))))))
