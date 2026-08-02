(ns com.blockether.vis.internal.foundation.serial-batch-test
  "`serial-batch` is the single definition of \"an ordered batch of commands\"
   shared by every batching tool, so the contract under test is the shape of
   that agreement: one wire key, an array (never a set or a bare string), input
   order preserved even when a command blows up, and one card summarising the
   run. Anything that silently reorders or drops a command here corrupts every
   tool built on it."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.foundation.serial-batch :as sb]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe ordered-test
             (it "keeps a vector of commands in input order"
                 (expect (= ["a" "b" "c"] (sb/ordered "shell" ["a" "b" "c"]))))
             (it "accepts a lone command as a one-command batch"
                 (expect (= ["ls"] (sb/ordered "shell" "ls"))))
             (it "refuses an unordered collection"
                 ;; A set would let the runner pick its own order, which is exactly the
                 ;; property every caller of this namespace depends on.
                 (expect (= ::sb/bad-commands
                            (try (sb/ordered "shell" #{"a" "b"})
                                 (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))
             (it "refuses an empty batch"
                 (expect (= ::sb/no-commands
                            (try (sb/ordered "shell" [])
                                 (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))))

(defdescribe run-serial-test
             (it "runs commands strictly in order and returns results in that order"
                 (let
                   [ran
                    (atom [])

                    results
                    (sb/run-serial ["a" "b" "c"]
                                   (fn [c]
                                     (swap! ran conj c)
                                     {"cmd" c}))]

                   (expect (= ["a" "b" "c"] @ran) "no command starts before the previous finished")
                   (expect (= [{"cmd" "a"} {"cmd" "b"} {"cmd" "c"}] results))))
             (it "fills a failed command's slot from on-error and keeps going"
                 ;; Position matters: the caller renders result N next to command N, so a
                 ;; throw must not shorten the vector.
                 (let
                   [results (sb/run-serial
                              ["a" "boom" "c"]
                              (fn [c]
                                (if (= c "boom") (throw (ex-info "nope" {})) {"cmd" c}))
                              (fn [c e]
                                {"cmd" c "error" (ex-message e)}))]
                   (expect (= [{"cmd" "a"} {"cmd" "boom" "error" "nope"} {"cmd" "c"}] results))))
             (it "rethrows without an on-error handler"
                 (expect (= "nope"
                            (try (sb/run-serial ["boom"]
                                                (fn [_]
                                                  (throw (ex-info "nope" {}))))
                                 (catch clojure.lang.ExceptionInfo e (ex-message e)))))))

(defdescribe result-shape-test
             (it "publishes results under the one shared commands key"
                 (expect (= "commands" sb/commands-key))
                 (expect (= {"commands" [{"exit" 0}]} (sb/result [{"exit" 0}]))))
             (it "batch? asks whether commands actually ran, not whether the key exists"
                 (expect (true? (boolean (sb/batch? {"commands" [{"exit" 0}]}))))
                 (expect (false? (boolean (sb/batch? {"commands" []}))))
                 (expect (false? (boolean (sb/batch? {})))))
             (it "failed? covers both a non-zero exit and a timeout"
                 (expect (false? (boolean (sb/failed? {"exit" 0}))))
                 (expect (true? (boolean (sb/failed? {"exit" 2}))))
                 (expect (true? (boolean (sb/failed? {"exit" 0 "timed_out" true})))
                         "a timeout can report exit 0 and still be a failure"))
             (it "tally counts both sides of the run"
                 (expect (= "1 succeeded, 1 failed" (sb/tally [{"exit" 0} {"exit" 1}])))))

(defdescribe card-test
             (it "summarises the batch and numbers each command in the body"
                 (let
                   [{:keys [summary body]}
                    (sb/card {:icon "▶"
                              :noun "shell"
                              :results [{"exit" 0} {"exit" 1}]
                              :render-one (fn [r]
                                            {:summary (str "exit " (get r "exit")) :body "out"})})]
                   (expect (= "▶ 2 shell commands — 1 succeeded, 1 failed" summary))
                   (expect (str/starts-with? body "### 1. exit 0"))
                   (expect (str/includes? body "### 2. exit 1"))
                   (expect (re-find #"\n\n─+\n\n" body) "commands are separated by a rule")))
             (it "lets the caller override the tally"
                 (let
                   [{:keys [summary]} (sb/card {:icon "▶"
                                                :noun "shell"
                                                :results [{"exit" 0}]
                                                :render-one (fn [_]
                                                              {:summary "s" :body "b"})
                                                :tally-fn (constantly "all good")})]
                   (expect (str/ends-with? summary "all good")))))
