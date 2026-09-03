(ns com.blockether.vis.contract.surface
  "JSON Schema contract for language-tool results."
  (:require [com.blockether.vis.contract.document :as document]))

(def capability->definition
  "Language-tool capability to its JSON Schema definition."
  {:format-fn "format_result" :lint-fn "lint_result" :test-fn "test_result"})

(defn valid?
  "True when `result` satisfies the capability schema, or no schema is registered."
  [capability result]
  (if-let [definition (get capability->definition capability)]
    (document/valid? "surface" definition result)
    true))

(defn explain
  "A readable JSON Schema explanation for an invalid result, or nil."
  [capability result]
  (when-let [definition (get capability->definition capability)]
    (some-> (document/explain "surface" definition result)
            pr-str)))

(defn check
  "Return a conforming result unchanged and refuse a malformed registered result."
  [capability result]
  (if-let [definition (get capability->definition capability)]
    (if-let [errors (document/explain "surface" definition result)]
      (throw (ex-info
               (str "language-surface contract violation for " capability)
               {:type :surface/contract-violation :capability capability :explain-data errors}))
      result)
    result))

(def test-result-base
  "Defaults shared by every `run_tests` result. Missing counts stay nil;
   collections and flags use neutral values."
  {"mode" nil
   "language" nil
   "framework" nil
   "runner" nil
   "tool" nil
   "command" nil
   "cwd" nil
   "ns" nil
   "target" nil
   "port" nil
   "exit" nil
   "ms" nil
   "is_pass" nil
   "total" nil
   "pass" nil
   "fail" nil
   ;; `errored` is a subset of `fail` and may exist without fault details.
   "errored" nil
   "selected" nil
   "skipped" nil
   "failures" []
   "output" nil
   "note" nil
   "hint" nil
   ;; Runner failures use `error`; test failures use `failures`.
   "error" nil
   "timed_out" false
   "repl_unusable" false
   "repl_wedged" false
   "recovered" false})

(defn- ->count
  "A reported count as a long, or nil when the runner reported nothing."
  [v]
  (when (number? v) (long v)))

(defn- ->faults
  "A fault collection as a vector; anything else (nil included) is no faults."
  [v]
  (if (coll? v) (vec v) []))

(defn complete-test-result
  "Fill derived defaults in a runner result without replacing reported values.
   Non-map results pass through."
  [language result]
  (if-not (map? result)
    result
    (let [pass
          (->count (get result "pass"))

          faults
          (->faults (get result "failures"))

          reported-errored
          (->count (get result "errored"))

          fail
          (->count (get result "fail"))

          ;; Infer the error count only when details cover every failure.
          errored
          (or reported-errored
              (when (and fail (= (count faults) fail))
                (count (filter #(= "error" (get % "type")) faults))))

          skipped
          (->count (get result "skipped"))

          total
          (or (->count (get result "total"))
              (when (and pass fail) (+ (long pass) (long fail) (long (or skipped 0)))))

          exit
          (->count (get result "exit"))

          is-pass
          (cond (some? (get result "is_pass")) (boolean (get result "is_pass"))
                (seq (str (get result "error"))) false
                (some? fail) (zero? (long fail))
                (some? exit) (zero? (long exit))
                :else nil)]

      (-> (merge test-result-base result)
          (assoc "language" (or (get result "language") language)
                 "pass" pass
                 "fail" fail
                 "errored" errored
                 "total" total
                 "skipped" skipped
                 "is_pass" is-pass
                 "failures" faults)))))
