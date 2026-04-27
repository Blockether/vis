(ns com.blockether.vis.loop.sci-expression-answer-test
  "When the model emits `:answer + :answer-type \"sci-expression\"`,
   the iteration handler evaluates the answer as a Clojure form in
   the SCI sandbox AFTER any :code blocks for this iter have run, and
   uses the printed result as the final answer text.

   The handler logic lives inside a `let` binding in
   `iteration.core/run-iteration`'s FINAL path -- not an exported fn.
   This test exercises the same shape via the same primitives the
   handler uses (`execute-code`, `safe-pr-str`, `vis-error`) so the
   contract is pinned without dragging in the LLM round-trip."
  (:require
   [com.blockether.vis.error :as vis-error]
   [com.blockether.vis.loop.runtime.conversation.environment.core :as env-core]
   [com.blockether.vis.loop.runtime.conversation.environment.query.iteration.core :as iter]
   [com.blockether.vis.persistance.spec :as p-spec]
   [lazytest.core :refer [defdescribe it expect]]
   [sci.core :as sci]))

(def ^:private safe-pr-str-public iter/safe-pr-str)
(def ^:private execute-code-private #'iter/execute-code)

(defn- fresh-environment []
  (env-core/create-sci-context nil))

(defn- eval-in [{:keys [sci-ctx]} source]
  (:val (sci/eval-string+ sci-ctx source
          {:ns (sci/find-ns sci-ctx 'sandbox)})))

(defn- finalize-sci-expression
  "Mirror of the new sci-expression branch in
   `iteration.core/run-iteration`. Evaluates `raw-answer` in the
   sandbox; on success returns {:answer string}; on parse / runtime
   failure returns {:validation-error string}."
  [environment raw-answer]
  (let [exec-result (execute-code-private environment raw-answer
                      :timeout-ms 5000
                      :iteration-id "i0.final")]
    (if-let [err (some-> exec-result :error vis-error/error-message)]
      {:validation-error (vis-error/sci-expression-eval-error-message err)}
      (let [v (:result exec-result)]
        {:answer (cond
                   (string? v) v
                   (nil? v)    "nil"
                   :else       (safe-pr-str-public v))}))))

(defdescribe sci-expression-answer-test

  (it "evaluates a single-form Clojure expression and uses the result"
    (let [environment (fresh-environment)]
      (eval-in environment "(def names [\"alpha\" \"beta\" \"gamma\"])")
      (expect (= {:answer "alpha\nbeta\ngamma"}
                (finalize-sci-expression environment
                  "(clojure.string/join \"\\n\" names)")))))

  (it "stringifies non-string return values via bounded pr-str"
    (let [environment (fresh-environment)]
      (eval-in environment "(def magic 42)")
      (expect (= {:answer "42"}
                (finalize-sci-expression environment "magic")))))

  (it "returns the literal string \"nil\" when the form evaluates to nil"
    (let [environment (fresh-environment)]
      (expect (= {:answer "nil"}
                (finalize-sci-expression environment "nil")))))

  (it "lets the form rely on freshly-defined sandbox vars"
    ;; The model often does:
    ;;   :code   [(def rendered-rows [...]) ...]
    ;;   :answer "(str/join \"\\n\" rendered-rows)"
    ;;   :answer-type "sci-expression"
    ;; The form can read vars defined earlier this iter.
    (let [environment (fresh-environment)]
      (eval-in environment "(def rendered-rows [\"| 1 | a |\" \"| 2 | b |\"])")
      (expect (= {:answer "| 1 | a |\n| 2 | b |"}
                (finalize-sci-expression environment
                  "(clojure.string/join \"\\n\" rendered-rows)")))))

  (it "reports a parse error as a validation error, not a crash"
    (let [environment (fresh-environment)
          {:keys [validation-error]}
          (finalize-sci-expression environment "(this-is-not-balanced ")]
      (expect (string? validation-error))
      (expect (re-find #"sci-expression" validation-error))))

  (it "reports a runtime error (undefined symbol) as a validation error"
    (let [environment (fresh-environment)
          {:keys [validation-error]}
          (finalize-sci-expression environment "(no-such-fn 1 2 3)")]
      (expect (string? validation-error))
      (expect (re-find #"sci-expression" validation-error))
      (expect (re-find #":code first" validation-error))))

  (it "supports multi-form bodies via `do`"
    ;; sci-expression is a single Clojure form; multi-step finalization
    ;; uses `do` (the way :code blocks already do).
    (let [environment (fresh-environment)
          form        "(do (def rows [\"a\" \"b\" \"c\"]) (clojure.string/join \", \" rows))"]
      (expect (= {:answer "a, b, c"}
                (finalize-sci-expression environment form))))))

(defdescribe validate-final-spec-test
  (it "accepts a parseable :sci-expression answer"
    (expect (nil? (p-spec/validate-final
                    {:answer "(clojure.string/join \"\\n\" rendered-rows)"
                     :answer-type :sci-expression}))))

  (it "rejects an unparseable :sci-expression answer"
    (let [error (p-spec/validate-final
                  {:answer "(unbalanced "
                   :answer-type :sci-expression})]
      (expect (string? error)))))
