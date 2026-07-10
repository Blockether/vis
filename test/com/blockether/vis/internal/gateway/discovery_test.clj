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
