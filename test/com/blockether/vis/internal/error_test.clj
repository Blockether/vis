(ns com.blockether.vis.internal.error-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [com.blockether.vis.internal.error :as error]))

(deftest error-message-test
  (testing "Throwable with message"
    (is (= "boom" (error/error-message (Exception. "boom")))))
  (testing "Throwable without message falls back to str"
    (is (string? (error/error-message (Exception.)))))
  (testing "map with :message"
    (is (= "hello" (error/error-message {:message "hello"}))))
  (testing "map with :msg"
    (is (= "world" (error/error-message {:msg "world"}))))
  (testing "map with :message preferred over :msg"
    (is (= "msg" (error/error-message {:message "msg" :msg "other"}))))
  (testing "map with neither key falls back to pr-str"
    (is (string? (error/error-message {:a 1}))))
  (testing "string passthrough"
    (is (= "plain" (error/error-message "plain"))))
  (testing "other types fall back to pr-str"
    (is (= "42" (error/error-message 42)))
    (is (= ":foo" (error/error-message :foo)))
    (is (= "[1 2 3]" (error/error-message [1 2 3])))))

(deftest format-error-test
  (testing "adds ERROR: prefix"
    (is (= "ERROR: boom" (error/format-error (Exception. "boom")))))
  (testing "idempotent — does not double-prefix"
    (is (= "ERROR: boom" (error/format-error "ERROR: boom"))))
  (testing "works with map input"
    (is (= "ERROR: msg" (error/format-error {:message "msg"}))))
  (testing "works with string input"
    (is (= "ERROR: plain" (error/format-error "plain")))))

(deftest final-answer-code-error-message-test
  (testing "includes prefix and exception message"
    (is (= "Final-answer code error: oops"
          (error/final-answer-code-error-message (Exception. "oops")))))
  (testing "works with nil-message exception"
    (let [result (error/final-answer-code-error-message (Exception.))]
      (is (str/starts-with? result "Final-answer code error: ")))))
