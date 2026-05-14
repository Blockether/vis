#!/usr/bin/env clojure
(require '[charred.api :as json]
         '[clojure.string :as str])

(defn read-json-file [path]
  (json/read-json (slurp path)))

(defn token-hit? [source token]
  (let [quoted (java.util.regex.Pattern/quote token)
        pattern (re-pattern (str "(^|[^A-Za-z0-9_?!*+<>=./-])" quoted "([^A-Za-z0-9_?!*+<>=./-]|$)"))]
    (boolean (re-find pattern source))))

(defn restricted-hits [source restricted]
  (->> restricted
       (map str)
       (filter #(token-hit? source %))
       vec))

(defn require-extra! [requires]
  (doseq [req requires]
    (require (read-string req))))

(defn test-source [test solution-source]
  (str/replace test #"__" solution-source))

(defn run-test [test solution-source]
  (let [source (test-source test solution-source)
        form (read-string source)]
    {:test test
     :pass (boolean (eval form))}))

(defn evaluate [problem solution-source]
  (let [tests (vec (get problem "tests" []))
        restricted (vec (get problem "restricted" []))
        hits (restricted-hits solution-source restricted)]
    (cond
      (str/blank? solution-source)
      {:passed false
       :tests_total (count tests)
       :tests_passed 0
       :error "empty-solution"}

      (seq hits)
      {:passed false
       :tests_total (count tests)
       :tests_passed 0
       :restricted_hits hits
       :error "restricted-symbol"}

      :else
      (try
        (require-extra! (get problem "extra_requires" []))
        (read-string (str "(do " solution-source "\n)"))
        (let [results (mapv #(run-test % solution-source) tests)
              passed-n (count (filter :pass results))]
          {:passed (= passed-n (count tests))
           :tests_total (count tests)
           :tests_passed passed-n
           :failed_tests (->> results (remove :pass) (map :test) vec)})
        (catch Throwable t
          {:passed false
           :tests_total (count tests)
           :tests_passed 0
           :error (str (class t) ": " (.getMessage t))})))))

(let [[problem-path solution-path] *command-line-args*]
  (when-not (and problem-path solution-path)
    (binding [*out* *err*]
      (println "usage: clojure -M dev/benches/4clojure/eval_one.clj problem.json solution.edn"))
    (System/exit 2))
  (let [problem (read-json-file problem-path)
        solution-source (slurp solution-path)]
    (println (json/write-json-str (evaluate problem solution-source)))))
