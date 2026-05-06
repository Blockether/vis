(ns com.blockether.vis.internal.cancellation-test
  (:require [clojure.test :refer [deftest is testing]]
            [com.blockether.vis.internal.cancellation :as cancel]))

(deftest worker-future-test
  (testing "worker-future returns a cancellable Future and runs the body"
    (let [f (cancel/worker-future "test-worker" (fn [] 42))]
      (is (instance? java.util.concurrent.Future f))
      (is (= 42 (.get ^java.util.concurrent.Future f 5 java.util.concurrent.TimeUnit/SECONDS)))
      (is (= 42 @f))
      (is (realized? f))))
  (testing "worker-future cancellation interrupts blocking work"
    (let [started (promise)
          f       (cancel/worker-future "test-worker-cancel"
                    (fn []
                      (deliver started true)
                      (Thread/sleep 10000)
                      :done))]
      (is (true? (deref started 1000 false)))
      (.cancel ^java.util.concurrent.Future f true)
      (is (.isCancelled ^java.util.concurrent.Future f))))
  (testing "worker runtime reports virtual-thread availability"
    (is (= :vis/worker-future (:worker-helper (cancel/worker-runtime))))
    (is (boolean? (:virtual-threads? (cancel/worker-runtime))))))

(deftest cancellation-token-test
  (testing "token has correct shape with flag and future atoms"
    (let [token (cancel/cancellation-token)]
      (is (map? token))
      (is (some? (cancel/cancellation-atom token)))
      (is (false? @(cancel/cancellation-atom token))))))

(deftest cancellation-atom-test
  (testing "returns the cooperative flag atom"
    (let [token (cancel/cancellation-token)
          a      (cancel/cancellation-atom token)]
      (is (instance? clojure.lang.Atom a))
      (is (false? @a)))))

(deftest cancel-and-cancelled-test
  (testing "cancel! flips the flag; cancelled? reflects it"
    (let [token (cancel/cancellation-token)]
      (is (not (cancel/cancelled? token)))
      (cancel/cancel! token)
      (is (cancel/cancelled? token))))
  (testing "cancel! is idempotent"
    (let [token (cancel/cancellation-token)]
      (cancel/cancel! token)
      (cancel/cancel! token)
      (is (cancel/cancelled? token))))
  (testing "cancel! with nil token does not throw"
    (is (nil? (cancel/cancel! nil))))
  (testing "cancel! cancels a registered future"
    (let [token (cancel/cancellation-token)
          f     (future (Thread/sleep 10000))]
      (cancel/cancellation-set-future! token f)
      (cancel/cancel! token)
      (is (cancel/cancelled? token))
      (is (.isCancelled f))
      (future-cancel f))))

(deftest cancellation-set-future-test
  (testing "registers and returns the future"
    (let [token (cancel/cancellation-token)
          f     (future (Thread/sleep 10000))]
      (is (identical? f (cancel/cancellation-set-future! token f)))
      (future-cancel f))))

(deftest cancellation-predicate-test
  (testing "top-level CancellationException"
    (is (cancel/cancellation? (java.util.concurrent.CancellationException. "test"))))
  (testing "top-level InterruptedException"
    (is (cancel/cancellation? (InterruptedException. "test"))))
  (testing "CompletionException wrapping CancellationException (the bug we fixed)"
    (let [inner (java.util.concurrent.CancellationException. "inner")
          wrapped (java.util.concurrent.CompletionException. inner)]
      (is (cancel/cancellation? wrapped))))
  (testing "RuntimeException wrapping InterruptedException"
    (let [inner (InterruptedException. "inner")
          wrapped (RuntimeException. "wrap" inner)]
      (is (cancel/cancellation? wrapped))))
  (testing "deeply nested CancellationException"
    (let [deep  (java.util.concurrent.CancellationException. "deep")
          mid   (RuntimeException. "mid" deep)
          outer (ex-info "outer" {} mid)]
      (is (cancel/cancellation? outer))))
  (testing "deeply nested InterruptedException"
    (let [deep  (InterruptedException. "deep")
          mid   (RuntimeException. "mid" deep)
          outer (ex-info "outer" {} mid)]
      (is (cancel/cancellation? outer))))
  (testing "returns false for plain exceptions"
    (is (not (cancel/cancellation? (RuntimeException. "plain"))))
    (is (not (cancel/cancellation? (ex-info "info" {}))))
    (is (not (cancel/cancellation? (IllegalStateException. "bad")))))
  (testing "returns false for nil"
    (is (not (cancel/cancellation? nil)))))
