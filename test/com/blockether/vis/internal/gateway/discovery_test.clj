(ns com.blockether.vis.internal.gateway.discovery-test
  "Unit tests for the gateway discovery/registry (build order step 1). Effects
   (registry dir, pid-liveness, spawn) are redirected/injected so nothing touches
   the real `~/.vis` or launches a process."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [com.blockether.vis.internal.gateway.discovery :as disco]))

(def ^:dynamic *tmp* nil)

(defn- with-tmp-registry
  [f]
  (let [dir (io/file (System/getProperty "java.io.tmpdir") (str "vis-disco-" (System/nanoTime)))]
    (.mkdirs dir)
    (try (with-redefs [disco/registry-dir (fn []
                                            dir)]
           (binding [*tmp* dir]
             (f)))
         (finally (run! #(.delete ^java.io.File %) (reverse (file-seq dir)))))))

(use-fixtures :each with-tmp-registry)

(def dead-pid 2147483646)                ; never a live process

(deftest memory-db?-covers-every-ephemeral-spelling
  (is (disco/memory-db? nil))
  (is (disco/memory-db? :memory))
  (is (disco/memory-db? ":memory"))
  (is (disco/memory-db? "memory"))
  (is (not (disco/memory-db? "/home/x/.vis/vis.db")))
  (is (not (disco/memory-db? "vis.db"))))

(deftest registry-key-is-stable-and-path-scoped
  (is (= (disco/registry-key "/a/b/c.db") (disco/registry-key "/a/b/c.db")))
  (is (not= (disco/registry-key "/a/b/c.db") (disco/registry-key "/a/b/d.db")))
  (testing "canonicalization collapses ./x to the absolute path"
    (is (= (disco/registry-key "vis.db")
           (disco/registry-key (.getCanonicalPath (io/file "vis.db")))))))

(deftest registry-roundtrip
  (let [db "/tmp/some/vis.db"]
    (is (nil? (disco/read-registry db)))
    (let [written (disco/write-registry! db {:pid 42 :port 7890 :host "127.0.0.1" :secret "tok"})]
      (is (= 7890 (:port written)))
      (is (contains? written :created-at))
      (is (= (.getCanonicalPath (io/file db)) (:db written))))
    (let [back (disco/read-registry db)]
      (is (= 42 (:pid back)))
      (is (= "tok" (:secret back))))
    (is (true? (disco/delete-registry! db)))
    (is (nil? (disco/read-registry db)))
    (is (false? (disco/delete-registry! db)))))

(deftest registry-fresh?-needs-live-pid-and-probe
  (testing "alive pid + probe true"
    (with-redefs [disco/pid-alive? (fn [_]
                                     true)]
      (is (disco/registry-fresh? {:pid 1} (constantly true)))))
  (testing "dead pid fails regardless of probe"
    (with-redefs [disco/pid-alive? (fn [_]
                                     false)]
      (is (not (disco/registry-fresh? {:pid 1} (constantly true))))))
  (testing "live pid but probe false (pid reuse guard)"
    (with-redefs [disco/pid-alive? (fn [_]
                                     true)]
      (is (not (disco/registry-fresh? {:pid 1} (constantly false))))))
  (testing "no pid / nil / non-map"
    (is (not (disco/registry-fresh? {} (constantly true))))
    (is (not (disco/registry-fresh? nil (constantly true))))
    (is (not (disco/registry-fresh? "nope" (constantly true)))))
  (testing "a real dead pid is not alive"
    (is (not (disco/pid-alive? dead-pid)))
    (is (disco/pid-alive? (disco/current-pid)))))

(deftest spawn-argv-shape
  (let [base
        ["/opt/vis"]

        argv
        (disco/spawn-argv {:db "/x/vis.db" :port 7890 :host "127.0.0.1" :base base})]

    (testing "--db is a start flag"
      (is (< (.indexOf argv "start") (.indexOf argv "--db")))
      (is (= "/x/vis.db" (nth argv (inc (.indexOf argv "--db"))))))
    (testing "start flags follow the `gateway start` subcommand"
      (is (< (.indexOf argv "gateway") (.indexOf argv "start")))
      (is (< (.indexOf argv "start") (.indexOf argv "--port")))
      (is (= "7890" (nth argv (inc (.indexOf argv "--port"))))))
    (is (= "/opt/vis" (first argv))))
  (testing "memory db omits --db"
    (let [argv (disco/spawn-argv {:db :memory :port 1 :base ["v"]})]
      (is (= -1 (.indexOf argv "--db")))
      (is (some #{"gateway"} argv))
      (is (some #{"start"} argv))))
  (testing "require-token? adds the boolean flag"
    (is (some #{"--require-token"}
              (disco/spawn-argv {:db "/x.db" :require-token? true :base ["v"]})))))

(deftest discover-or-start!-memory-is-a-noop
  (is (= {:mode :none} (disco/discover-or-start! {:db :memory}))))

(deftest discover-or-start!-attaches-to-a-fresh-daemon
  (let [db "/tmp/attach/vis.db"]
    (with-redefs [disco/pid-alive? (fn [_]
                                     true)]
      (disco/write-registry! db {:pid 999 :port 7890 :host "127.0.0.1" :secret "s"})
      (let [res (disco/discover-or-start! {:db db}
                                          :probe (constantly true)
                                          :spawn (fn [_]
                                                   (throw (ex-info "should not spawn" {}))))]
        (is (= :attach (:mode res)))
        (is (= 7890 (get-in res [:entry :port])))))))

(deftest discover-or-start!-spawns-when-missing
  (let [db
        "/tmp/spawn/vis.db"

        spawned
        (atom 0)

        spawn
        (fn [_]
          (swap! spawned inc)
          (disco/write-registry! db {:pid 123 :port 8000 :host "127.0.0.1" :secret "s"}))]

    (with-redefs [disco/pid-alive? (fn [_]
                                     true)]
      (let [res (disco/discover-or-start! {:db db}
                                          :probe (constantly true)
                                          :spawn spawn
                                          :timeout-ms 2000
                                          :poll-ms 10)]
        (is (= 1 @spawned))
        (is (= :spawned (:mode res)))
        (is (= 8000 (get-in res [:entry :port])))))))

(deftest discover-or-start!-deletes-stale-then-times-out
  (let [db "/tmp/stale/vis.db"]
    ;; stale entry: dead pid, so not fresh; spawn is a no-op so nothing comes up
    (disco/write-registry! db {:pid dead-pid :port 1 :host "127.0.0.1" :secret "s"})
    (let [res (disco/discover-or-start! {:db db}
                                        :probe (constantly true)
                                        :spawn (fn [_]
                                                 nil)
                                        :timeout-ms 120
                                        :poll-ms 20)]
      (is (= :timeout (:mode res)))
      (is (nil? (disco/read-registry db))))))

(deftest acquire-spawn-lock!-is-exclusive-across-holders
  (let [db
        "/tmp/lock/vis.db"

        h1
        (disco/acquire-spawn-lock! db)]

    (is (some? h1) "first acquirer wins the lock")
    (is (nil? (disco/acquire-spawn-lock! db)) "a second acquirer sees the lock held and backs off")
    (disco/release-spawn-lock! h1)
    (let [h2 (disco/acquire-spawn-lock! db)]
      (is (some? h2) "lock is re-acquirable once released")
      (disco/release-spawn-lock! h2))))

(deftest discover-or-start!-awaits-instead-of-piling-on-when-lock-held
  ;; Simulate a CONCURRENT starter that already holds the spawn lock and is
  ;; bringing a daemon up: we hold the lock here, and a background thread writes
  ;; the fresh registry a beat later (its daemon self-registering). The call
  ;; under test must NOT spawn a competing daemon — it awaits and attaches.
  (let [db
        "/tmp/herd/vis.db"

        spawned
        (atom 0)

        holder
        (disco/acquire-spawn-lock! db)]

    (is (some? holder))
    (try (with-redefs [disco/pid-alive? (fn [_]
                                          true)]
           (future (Thread/sleep 60)
                   (disco/write-registry! db {:pid 777 :port 9100 :host "127.0.0.1" :secret "s"}))
           (let [res (disco/discover-or-start! {:db db}
                                               :probe (constantly true)
                                               :spawn (fn [_]
                                                        (swap! spawned inc))
                                               :timeout-ms 3000
                                               :poll-ms 10)]
             (is (= :awaited (:mode res)))
             (is (= 9100 (get-in res [:entry :port])))
             (is (zero? @spawned) "no competing daemon is spawned while another holds the lock")))
         (finally (disco/release-spawn-lock! holder)))))

(deftest discover-or-start!-emits-nothing-on-the-fast-attach-path
  (let [db "/tmp/ev-attach/vis.db"
        events (atom [])]
    (with-redefs [disco/pid-alive? (fn [_]
                                     true)]
      (disco/write-registry! db {:pid 999 :port 7890 :host "127.0.0.1" :secret "s"})
      (let [res (disco/discover-or-start! {:db db}
                                          :probe (constantly true)
                                          :on-event (fn [ev]
                                                      (swap! events conj ev)))]
        (is (= :attach (:mode res)))
        (is (empty? @events) "an instant attach must stay silent")))))

(deftest discover-or-start!-emits-spawning-tick-and-ready-when-it-spawns
  (let [db "/tmp/ev-spawn/vis.db"
        events (atom [])
        spawn (fn [_]
                (disco/write-registry! db {:pid 123 :port 8000 :host "127.0.0.1" :secret "s"}))]
    ;; pid-alive? is false for the first read (nothing registered yet) so we take
    ;; the spawn path; the spawn writes a live entry that await picks up.
    (with-redefs [disco/pid-alive? (fn [_]
                                     true)]
      (let [res (disco/discover-or-start! {:db db}
                                          :probe (constantly true)
                                          :spawn spawn
                                          :on-event (fn [ev]
                                                      (swap! events conj ev))
                                          :timeout-ms 2000
                                          :poll-ms 10)
            phases (map :phase @events)]
        (is (= :spawned (:mode res)))
        (is (= :spawning (first phases)) "the spawner announces it is starting the daemon")
        (is (= {:phase :ready :mode :spawned :entry (:entry res)} (last @events))
            "a ready event carries the mode + resolved entry")))))

(deftest discover-or-start!-emits-awaiting-and-ready-when-another-process-spawns
  (let [db "/tmp/ev-await/vis.db"
        events (atom [])
        holder (disco/acquire-spawn-lock! db)]
    (is (some? holder))
    (try (with-redefs [disco/pid-alive? (fn [_]
                                          true)]
           (future (Thread/sleep 60)
                   (disco/write-registry! db {:pid 777 :port 9100 :host "127.0.0.1" :secret "s"}))
           (let [res (disco/discover-or-start! {:db db}
                                               :probe (constantly true)
                                               :spawn (fn [_]
                                                        nil)
                                               :on-event (fn [ev]
                                                           (swap! events conj ev))
                                               :timeout-ms 3000
                                               :poll-ms 10)
                 phases (map :phase @events)]
             (is (= :awaited (:mode res)))
             (is (= :awaiting (first phases))
                 "a waiter announces that ANOTHER vis is starting the gateway")
             (is (some #{:tick} phases) "a heartbeat ticks while awaiting")
             (is (= {:phase :ready :mode :awaited :entry (:entry res)} (last @events)))))
         (finally (disco/release-spawn-lock! holder)))))
