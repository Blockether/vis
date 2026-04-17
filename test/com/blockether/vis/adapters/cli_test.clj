(ns com.blockether.vis.adapters.cli-test
  (:require
   [lazytest.core :refer [defdescribe describe expect it]]))

(defn- parse-run-args
  [args]
  (require 'com.blockether.vis.adapters.cli)
  ((ns-resolve 'com.blockether.vis.adapters.cli 'parse-run-args) args))

(defn- validate-run-opts!
  [opts]
  (require 'com.blockether.vis.adapters.cli)
  ((ns-resolve 'com.blockether.vis.adapters.cli 'validate-run-opts!) opts))

(defdescribe cli-max-iterations-validation-test
  (describe "CLI --max-iterations validation"
    (it "rejects 0"
      (let [opts (parse-run-args ["--max-iterations" "0" "hello"])
            ex (try
                 (validate-run-opts! opts)
                 nil
                 (catch Exception e e))]
        (expect (some? ex))
        (expect (= :cli/invalid-arg (:type (ex-data ex))))))

    (it "rejects negative values"
      (let [opts (parse-run-args ["--max-iterations" "-3" "hello"])
            ex (try
                 (validate-run-opts! opts)
                 nil
                 (catch Exception e e))]
        (expect (some? ex))
        (expect (= :cli/invalid-arg (:type (ex-data ex))))))

    (it "rejects non-integer values"
      (let [opts (parse-run-args ["--max-iterations" "abc" "hello"])
            ex (try
                 (validate-run-opts! opts)
                 nil
                 (catch Exception e e))]
        (expect (some? ex))
        (expect (= :cli/invalid-arg (:type (ex-data ex))))))

    (it "accepts positive integers"
      (let [opts (parse-run-args ["--max-iterations" "1" "hello"])]
        (expect (true? (validate-run-opts! opts)))))))
