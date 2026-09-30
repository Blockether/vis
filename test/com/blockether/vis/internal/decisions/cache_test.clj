(ns com.blockether.vis.internal.decisions.cache-test
  (:require [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]]
            [com.blockether.vis.internal.decisions.cache :as cache]))

(def ^:private small-limits
  {:max-models 2 :budget-mb 8 :reserve-mb 4 :max-loads 1 :max-infer 3 :max-waiters 3 :wait-ms 3000})

(defdescribe loaded-sessions-are-reused-and-idle-lru-closes
             (it "loaded sessions are reused and idle lru closes"
                 (binding [cache/*limits* small-limits]
                   (cache/release-idle!)
                   (let [opens (atom [])
                         closes (atom [])
                         loader (fn [key]
                                  (fn []
                                    (swap! opens conj key)
                                    {:close #(swap! closes conj key)}))]

                     (try
                       ;; a loaded instance is reused instead of reopening the graph
                       (expect (= :a (cache/with-resident! :a 0 (loader :a) (constantly :a))))
                       (expect (= :a (cache/with-resident! :a 0 (loader :a) (constantly :a))))
                       (expect (= [:a] @opens))
                       (expect (= :ready (cache/status :a)))
                       (cache/with-resident! :b 0 (loader :b) (constantly nil))
                       ;; the oldest idle version is released, not the newest
                       (cache/with-resident! :c 0 (loader :c) (constantly nil))
                       (expect (= [:a :b :c] @opens))
                       (expect (= [:a] @closes))
                       (expect (= :cold (cache/status :a)))
                       (expect (= :ready (cache/status :b)))
                       (finally (cache/release-idle!)))))))

(defdescribe active-lease-is-never-evicted
             (it "active lease is never evicted"
                 (binding [cache/*limits* small-limits]
                   (cache/release-idle!)
                   (let [entered (promise)
                         release (promise)
                         closes (atom [])
                         loader (fn [key]
                                  (fn []
                                    {:close #(swap! closes conj key)}))]

                     (try (let [active (future (cache/with-resident! :active
                                                                     0
                                                                     (loader :active)
                                                                     (fn [_]
                                                                       (deliver entered true)
                                                                       @release)))]
                            (expect (= true (deref entered 3000 nil)))
                            (cache/with-resident! :idle 0 (loader :idle) (constantly nil))
                            (cache/with-resident! :new 0 (loader :new) (constantly nil))
                            (expect (= [:idle] @closes))
                            (expect (= :ready (cache/status :active)))
                            (deliver release true)
                            (expect (= true (deref active 3000 nil))))
                          (finally (deliver release true) (cache/release-idle!)))))))

(defdescribe
  cold-requests-share-one-load-and-failures-can-retry
  (it
    "cold requests share one load and failures can retry"
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

        (try (let [first-request (future (cache/with-resident! :shared 0 loader (constantly :one)))]
               (expect (= true (deref entered 3000 nil)))
               (let [second-request (future
                                      (cache/with-resident! :shared 0 loader (constantly :two)))]
                 (deliver release true)
                 (expect (= :one (deref first-request 3000 nil)))
                 (expect (= :two (deref second-request 3000 nil)))
                 (expect (= 1 @loads))))
             ;; a failed load does not poison or occupy a cache slot
             (expect (= :broken
                        (:type (ex-data (try (cache/with-resident!
                                               :failed
                                               0
                                               #(throw (ex-info "broken" {:type :broken}))
                                               identity)
                                             (catch clojure.lang.ExceptionInfo e e))))))
             (expect (= :cold (cache/status :failed)))
             (expect (= :ok
                        (cache/with-resident! :failed
                                              0
                                              (fn []
                                                {:close (fn [])})
                                              (constantly :ok))))
             (finally (deliver release true) (cache/release-idle!)))))))

(defdescribe
  shutdown-retires-active-sessions-before-restart
  (it
    "shutdown retires active sessions before restart"
    (binding [cache/*limits* small-limits]
      (cache/enable!)
      (cache/release-idle!)
      (let [closed (atom [])
            entered (promise)
            release (promise)
            loader (fn [key]
                     (fn []
                       {:close #(swap! closed conj key)}))]

        (try (cache/with-resident! :idle 0 (loader :idle) identity)
             (let [active (future (cache/with-resident! :active
                                                        0
                                                        (loader :active)
                                                        (fn [_]
                                                          (deliver entered true)
                                                          @release)))]
               (expect (= true (deref entered 3000 nil)))
               (expect (= 1 (cache/shutdown!)))
               (expect (= [:idle] @closed))
               (expect (= :decisions/unavailable
                          (:type (ex-data (try (cache/with-resident! :new 0 (loader :new) identity)
                                               (catch clojure.lang.ExceptionInfo e e))))))
               (cache/enable!)
               (expect (= :decisions/unavailable
                          (:type (ex-data
                                   (try (cache/with-resident! :active 0 (loader :active) identity)
                                        (catch clojure.lang.ExceptionInfo e e))))))
               (deliver release true)
               (expect (= true (deref active 3000 nil)))
               (expect (= [:idle :active] @closed))
               (expect (= :cold (cache/status :active)))
               (cache/with-resident! :active 0 (loader :active) identity)
               (expect (= :ready (cache/status :active))))
             (finally (deliver release true) (cache/enable!) (cache/release-idle!)))))))

(defdescribe
  shutdown-retires-a-load-that-finishes-late
  (it "shutdown retires a load that finishes late"
      (binding [cache/*limits* small-limits]
        (cache/enable!)
        (cache/release-idle!)
        (let [entered (promise)
              release (promise)
              closed (atom 0)
              loading (future (try (cache/with-resident! :loading
                                                         0
                                                         (fn []
                                                           (deliver entered true)
                                                           @release
                                                           {:close #(swap! closed inc)})
                                                         identity)
                                   (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))]

          (try (expect (= true (deref entered 3000 nil)))
               (expect (= 0 (cache/shutdown!)))
               (deliver release true)
               (expect (= :decisions/unavailable (deref loading 3000 nil)))
               (expect (= 1 @closed))
               (expect (= :cold (cache/status :loading)))
               (finally (deliver release true) (cache/enable!) (cache/release-idle!)))))))

(defdescribe
  training-reservation-excludes-inference-without-retiring-active-leases
  (it
    "training reservation excludes inference without retiring active leases"
    (binding [cache/*limits* small-limits]
      (cache/enable!)
      (cache/release-idle!)
      (let [entered (promise)
            release (promise)
            closed (atom 0)]

        (try (cache/with-resident! :idle
                                   0
                                   (fn []
                                     {:close #(swap! closed inc)})
                                   identity)
             (let [active (future (cache/with-resident! :active
                                                        0
                                                        (fn []
                                                          {:close #(swap! closed inc)})
                                                        (fn [_]
                                                          (deliver entered true)
                                                          @release)))]
               (expect (= true (deref entered 3000 nil)))
               (expect (= :decisions/capacity-exceeded
                          (:type (ex-data (try (cache/begin-training!)
                                               (catch clojure.lang.ExceptionInfo e e))))))
               (expect (= 0 @closed))
               (deliver release true)
               (expect (= true (deref active 3000 nil)))
               (expect (= true (cache/begin-training!)))
               (expect (= 2 @closed))
               (expect (= :decisions/capacity-exceeded
                          (:type (ex-data (try (cache/with-resident! :new
                                                                     0
                                                                     (fn []
                                                                       {:close (fn [])})
                                                                     identity)
                                               (catch clojure.lang.ExceptionInfo e e))))))
               (expect (= :decisions/capacity-exceeded
                          (:type (ex-data (try (cache/begin-training!)
                                               (catch clojure.lang.ExceptionInfo e e))))))
               (cache/end-training!)
               (expect (= :ok
                          (cache/with-resident! :new
                                                0
                                                (fn []
                                                  {:close (fn [])})
                                                (constantly :ok)))))
             (finally (deliver release true) (cache/end-training!) (cache/release-idle!)))))))

(defdescribe
  reservations-follow-session-weights
  (it "reserves memory for large weights and refuses a model above the budget"
      (binding [cache/*limits* small-limits]
        (cache/release-idle!)
        (let [closes (atom [])
              loader (fn [key]
                       (fn []
                         {:close #(swap! closes conj key)}))
              mib 1048576]

          (try
            ;; 4 MiB of weights reserves 6 MB, so a 4 MB model evicts it from the 8 MB budget
            (cache/with-resident! :large (* 4 mib) (loader :large) (constantly nil))
            (cache/with-resident! :small 0 (loader :small) (constantly nil))
            (expect (= [:large] @closes))
            (let [error (try (cache/with-resident! :huge (* 6 mib) (loader :huge) (constantly nil))
                             (catch clojure.lang.ExceptionInfo e e))]
              (expect (= :decisions/capacity-exceeded (:type (ex-data error))))
              (expect (= 9 (:required-mb (ex-data error))))
              (expect (str/includes? (ex-message error) "VIS_DECISION_MEMORY_BUDGET_MB")))
            ;; a model that can never fit does not evict a resident one
            (expect (= :ready (cache/status :small)))
            (expect (= [:large] @closes))
            (finally (cache/release-idle!)))))))
