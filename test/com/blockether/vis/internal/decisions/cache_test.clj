(ns com.blockether.vis.internal.decisions.cache-test
  (:require [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]
            [com.blockether.vis.internal.decisions.cache :as cache]))

(def ^:private small-limits
  {:max-models 2 :budget-mb 8 :reserve-mb 4 :max-loads 1 :max-infer 3 :max-waiters 3 :wait-ms 3000})

(deftest loaded-sessions-are-reused-and-idle-lru-closes
  (binding [cache/*limits* small-limits]
    (cache/release-idle!)
    (let [opens (atom [])
          closes (atom [])
          loader (fn [key]
                   (fn []
                     (swap! opens conj key)
                     {:close #(swap! closes conj key)}))]

      (try (testing "a loaded instance is reused instead of reopening the graph"
             (is (= :a (cache/with-resident! :a (loader :a) (constantly :a))))
             (is (= :a (cache/with-resident! :a (loader :a) (constantly :a))))
             (is (= [:a] @opens))
             (is (= :ready (cache/status :a))))
           (cache/with-resident! :b (loader :b) (constantly nil))
           (testing "the oldest idle version is released, not the newest"
             (cache/with-resident! :c (loader :c) (constantly nil))
             (is (= [:a :b :c] @opens))
             (is (= [:a] @closes))
             (is (= :cold (cache/status :a)))
             (is (= :ready (cache/status :b))))
           (finally (cache/release-idle!))))))

(deftest active-lease-is-never-evicted
  (binding [cache/*limits* small-limits]
    (cache/release-idle!)
    (let [entered (promise)
          release (promise)
          closes (atom [])
          loader (fn [key]
                   (fn []
                     {:close #(swap! closes conj key)}))]

      (try (let [active (future (cache/with-resident! :active
                                                      (loader :active)
                                                      (fn [_]
                                                        (deliver entered true)
                                                        @release)))]
             (is (= true (deref entered 3000 nil)))
             (cache/with-resident! :idle (loader :idle) (constantly nil))
             (cache/with-resident! :new (loader :new) (constantly nil))
             (is (= [:idle] @closes))
             (is (= :ready (cache/status :active)))
             (deliver release true)
             (is (= true (deref active 3000 nil))))
           (finally (deliver release true) (cache/release-idle!))))))

(deftest cold-requests-share-one-load-and-failures-can-retry
  (binding [cache/*limits* small-limits]
    (cache/release-idle!)
    (let [entered (promise)
          release (promise)
          loads (atom 0)
          loader (fn []
                   (swap! loads inc)
                   (deliver entered true)
                   @release
                   {:close (fn [])})]

      (try (let [first-request (future (cache/with-resident! :shared loader (constantly :one)))]
             (is (= true (deref entered 3000 nil)))
             (let [second-request (future (cache/with-resident! :shared loader (constantly :two)))]
               (deliver release true)
               (is (= :one (deref first-request 3000 nil)))
               (is (= :two (deref second-request 3000 nil)))
               (is (= 1 @loads))))
           (testing "a failed load does not poison or occupy a cache slot"
             (is (= :broken
                    (:type (ex-data (try (cache/with-resident! :failed
                                                               #(throw (ex-info "broken"
                                                                                {:type :broken}))
                                                               identity)
                                         (catch clojure.lang.ExceptionInfo e e))))))
             (is (= :cold (cache/status :failed)))
             (is (= :ok
                    (cache/with-resident! :failed
                                          (fn []
                                            {:close (fn [])})
                                          (constantly :ok)))))
           (finally (deliver release true) (cache/release-idle!))))))

(deftest shutdown-retires-active-sessions-before-restart
  (binding [cache/*limits* small-limits]
    (cache/enable!)
    (cache/release-idle!)
    (let [closed (atom [])
          entered (promise)
          release (promise)
          loader (fn [key]
                   (fn []
                     {:close #(swap! closed conj key)}))]

      (try (cache/with-resident! :idle (loader :idle) identity)
           (let [active (future (cache/with-resident! :active
                                                      (loader :active)
                                                      (fn [_]
                                                        (deliver entered true)
                                                        @release)))]
             (is (= true (deref entered 3000 nil)))
             (is (= 1 (cache/shutdown!)))
             (is (= [:idle] @closed))
             (is (= :decisions/unavailable
                    (:type (ex-data (try (cache/with-resident! :new (loader :new) identity)
                                         (catch clojure.lang.ExceptionInfo e e))))))
             (cache/enable!)
             (is (= :decisions/unavailable
                    (:type (ex-data (try (cache/with-resident! :active (loader :active) identity)
                                         (catch clojure.lang.ExceptionInfo e e))))))
             (deliver release true)
             (is (= true (deref active 3000 nil)))
             (is (= [:idle :active] @closed))
             (is (= :cold (cache/status :active)))
             (cache/with-resident! :active (loader :active) identity)
             (is (= :ready (cache/status :active))))
           (finally (deliver release true) (cache/enable!) (cache/release-idle!))))))

(deftest shutdown-retires-a-load-that-finishes-late
  (binding [cache/*limits* small-limits]
    (cache/enable!)
    (cache/release-idle!)
    (let [entered (promise)
          release (promise)
          closed (atom 0)
          loading (future (try (cache/with-resident! :loading
                                                     (fn []
                                                       (deliver entered true)
                                                       @release
                                                       {:close #(swap! closed inc)})
                                                     identity)
                               (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))]

      (try (is (= true (deref entered 3000 nil)))
           (is (= 0 (cache/shutdown!)))
           (deliver release true)
           (is (= :decisions/unavailable (deref loading 3000 nil)))
           (is (= 1 @closed))
           (is (= :cold (cache/status :loading)))
           (finally (deliver release true) (cache/enable!) (cache/release-idle!))))))

(deftest training-reservation-excludes-inference-without-retiring-active-leases
  (binding [cache/*limits* small-limits]
    (cache/enable!)
    (cache/release-idle!)
    (let [entered (promise)
          release (promise)
          closed (atom 0)]

      (try (cache/with-resident! :idle
                                 (fn []
                                   {:close #(swap! closed inc)})
                                 identity)
           (let [active (future (cache/with-resident! :active
                                                      (fn []
                                                        {:close #(swap! closed inc)})
                                                      (fn [_]
                                                        (deliver entered true)
                                                        @release)))]
             (is (= true (deref entered 3000 nil)))
             (is (= :decisions/capacity-exceeded
                    (:type (ex-data (try (cache/begin-training!)
                                         (catch clojure.lang.ExceptionInfo e e))))))
             (is (= 0 @closed))
             (deliver release true)
             (is (= true (deref active 3000 nil)))
             (is (= true (cache/begin-training!)))
             (is (= 2 @closed))
             (is (= :decisions/capacity-exceeded
                    (:type (ex-data (try (cache/with-resident! :new
                                                               (fn []
                                                                 {:close (fn [])})
                                                               identity)
                                         (catch clojure.lang.ExceptionInfo e e))))))
             (is (= :decisions/capacity-exceeded
                    (:type (ex-data (try (cache/begin-training!)
                                         (catch clojure.lang.ExceptionInfo e e))))))
             (cache/end-training!)
             (is (= :ok
                    (cache/with-resident! :new
                                          (fn []
                                            {:close (fn [])})
                                          (constantly :ok)))))
           (finally (deliver release true) (cache/end-training!) (cache/release-idle!))))))
