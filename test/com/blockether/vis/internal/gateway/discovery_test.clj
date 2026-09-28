(ns com.blockether.vis.internal.gateway.discovery-test
  "Unit tests for the gateway discovery/registry (build order step 1). Effects
   (registry dir, pid-liveness, spawn) are redirected/injected so nothing touches
   the real `~/.vis`; only the spawn pid test launches a process, a short `sleep`."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.core :as lt :refer [defdescribe expect it]]
            [com.blockether.vis.internal.gateway.discovery :as disco]
            [com.blockether.vis.internal.paths :as paths]))

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

;; A namespace-level `around-each` context runs every test case against its own
;; temporary registry.
(lt/set-ns-context! [(lt/around-each [f] (with-tmp-registry f))])

(def dead-pid 2147483646)                ; never a live process

(defdescribe memory-db?-covers-every-ephemeral-spelling
             (it "memory db? covers every ephemeral spelling"
                 (expect (disco/memory-db? nil))
                 (expect (disco/memory-db? :memory))
                 (expect (disco/memory-db? ":memory"))
                 (expect (disco/memory-db? "memory"))
                 (expect (not (disco/memory-db? "/home/x/.vis/vis.db")))
                 (expect (not (disco/memory-db? "vis.db")))))

(defdescribe registry-key-is-stable-and-path-scoped
             (it "registry key is stable and path scoped"
                 (expect (= (disco/registry-key "/a/b/c.db") (disco/registry-key "/a/b/c.db")))
                 (expect (not= (disco/registry-key "/a/b/c.db") (disco/registry-key "/a/b/d.db")))
                 ;; canonicalization collapses ./x to the absolute path
                 (expect (= (disco/registry-key "vis.db")
                            (disco/registry-key (.getCanonicalPath (io/file "vis.db")))))))

(defdescribe registry-roundtrip
             (it "registry roundtrip"
                 (let [db "/tmp/some/vis.db"]
                   (expect (nil? (disco/read-registry db)))
                   (let [written (disco/write-registry!
                                   db
                                   {:pid 42 :port 7890 :host "127.0.0.1" :secret "tok"})]
                     (expect (= 7890 (:port written)))
                     (expect (contains? written :created-at))
                     (expect (= (.getCanonicalPath (io/file db)) (:db written))))
                   (let [back (disco/read-registry db)]
                     (expect (= 42 (:pid back)))
                     (expect (= "tok" (:secret back))))
                   (expect (true? (disco/delete-registry! db)))
                   (expect (nil? (disco/read-registry db)))
                   (expect (false? (disco/delete-registry! db))))))

(defdescribe registry-conditional-delete-preserves-a-successor
             (it "registry conditional delete preserves a successor"
                 (let [db "/tmp/conditional/vis.db"]
                   (disco/write-registry! db {:pid 11 :port 7890 :host "127.0.0.1" :secret "old"})
                   (expect (false? (disco/delete-registry-if! db #(= 22 (:pid %)))))
                   (expect (= 11 (:pid (disco/read-registry db))))
                   (expect (true? (disco/delete-registry-if! db #(= 11 (:pid %)))))
                   (expect (nil? (disco/read-registry db))))))

(defdescribe deregister-self-never-deletes-another-live-owner
             (it "deregister self never deletes another live owner"
                 (let [db "/tmp/deregister-owner/vis.db"]
                   (disco/write-registry! db {:pid 22 :port 7890 :host "127.0.0.1" :secret "new"})
                   (with-redefs [disco/current-pid (constantly 11)]
                     (disco/deregister-self! db))
                   (expect (= 22 (:pid (disco/read-registry db)))))))

(defdescribe registry-fresh?-needs-live-pid-and-probe
             (it "alive pid + probe true"
                 (with-redefs [disco/pid-alive? (fn [_]
                                                  true)]
                   (expect (disco/registry-fresh? {:pid 1} (constantly true)))))
             (it "dead pid fails regardless of probe"
                 (with-redefs [disco/pid-alive? (fn [_]
                                                  false)]
                   (expect (not (disco/registry-fresh? {:pid 1} (constantly true))))))
             (it "live pid but probe false (pid reuse guard)"
                 (with-redefs [disco/pid-alive? (fn [_]
                                                  true)]
                   (expect (not (disco/registry-fresh? {:pid 1} (constantly false))))))
             (it "no pid / nil / non-map"
                 (expect (not (disco/registry-fresh? {} (constantly true))))
                 (expect (not (disco/registry-fresh? nil (constantly true))))
                 (expect (not (disco/registry-fresh? "nope" (constantly true)))))
             (it "a real dead pid is not alive"
                 (expect (not (disco/pid-alive? dead-pid)))
                 (expect (disco/pid-alive? (disco/current-pid)))))

(defdescribe spawn-argv-shape
             (it "spawn argv shape"
                 (let [base
                       ["/opt/vis"]

                       ^java.util.List argv
                       (disco/spawn-argv {:db "/x/vis.db" :port 7890 :host "127.0.0.1" :base base})]

                   ;; --db is a start flag
                   (expect (< (.indexOf argv "start") (.indexOf argv "--db")))
                   (expect (= "/x/vis.db" (nth argv (inc (.indexOf argv "--db")))))
                   ;; start flags follow the `gateway start` subcommand
                   (expect (< (.indexOf argv "gateway") (.indexOf argv "start")))
                   (expect (< (.indexOf argv "start") (.indexOf argv "--port")))
                   (expect (= "7890" (nth argv (inc (.indexOf argv "--port")))))
                   (expect (= "/opt/vis" (first argv))))
                 ;; memory db omits --db
                 (let [^java.util.List argv (disco/spawn-argv {:db :memory :port 1 :base ["v"]})]
                   (expect (= -1 (.indexOf argv "--db")))
                   (expect (some #{"gateway"} argv))
                   (expect (some #{"start"} argv)))
                 ;; require-token? adds the boolean flag
                 (expect (some #{"--require-token"}
                               (disco/spawn-argv {:db "/x.db" :require-token? true :base ["v"]})))))

;; Regression, Vis session ae259fdd-2712-4591-8f12-e1cdff30b208: a managed
;; gateway inherited terminal stop signals and froze with its WSL TUI parent.
(defdescribe unix-launch-cmd-renames-and-protects-the-daemon
             (it "unix launch cmd renames and protects the daemon"
                 (let [[shell flag script] (disco/unix-launch-cmd ["/opt/vis" "gateway" "start"
                                                                   "--port" "7890"]
                                                                  "/tmp/boot.log"
                                                                  {:bash "/bin/bash" :setsid nil})]
                   (expect (= "/bin/bash" shell))
                   (expect (= "-c" flag))
                   (expect (str/includes? script "'/opt/vis'"))
                   (expect (str/includes? script "'/tmp/boot.log'"))
                   (expect (str/ends-with? script "2>&1 & echo $!")
                           "stays backgrounded and reports the daemon pid")
                   (expect (str/includes? script "trap '' HUP TSTP")
                           "the daemon must ignore terminal hangup and stop signals across exec")
                   (expect (str/includes? script "exec -a 'vis' '/opt/vis' 'gateway' 'start'")))
                 ;; plain sh still ignores job-control signals when neither helper exists
                 (let [[shell _ script] (disco/unix-launch-cmd ["/opt/vis" "gateway" "start"]
                                                               "/tmp/boot.log"
                                                               {:bash nil :setsid nil})]
                   (expect (= "sh" shell))
                   (expect (str/includes? script "trap '' HUP TSTP")))))

;; Regression, Vis session ae259fdd-2712-4591-8f12-e1cdff30b208: Linux/WSL
;; gateways stayed in the TUI's foreground process group instead of a new session.
(defdescribe unix-launch-cmd-uses-setsid-on-linux
             (it "unix launch cmd uses setsid on linux"
                 (let [[shell _ script] (disco/unix-launch-cmd ["/opt/vis" "gateway" "start"]
                                                               "/tmp/boot.log"
                                                               {:bash "/bin/bash"
                                                                :setsid "/usr/bin/setsid"})]
                   (expect (= "/bin/bash" shell))
                   (expect (str/includes? script "'/usr/bin/setsid'"))
                   (expect (str/includes? script "'/bin/bash' -c"))
                   (expect (< (.indexOf script "'/usr/bin/setsid'") (.indexOf script "exec -a"))))))

(defdescribe discover-or-start!-memory-is-a-noop
             (it "discover or start! memory is a noop"
                 (expect (= {:mode :none} (disco/discover-or-start! {:db :memory})))))

(defdescribe discover-or-start!-attaches-to-a-fresh-daemon
             (it "discover or start! attaches to a fresh daemon"
                 (let [db "/tmp/attach/vis.db"]
                   (with-redefs [disco/pid-alive? (fn [_]
                                                    true)]
                     (disco/write-registry! db {:pid 999 :port 7890 :host "127.0.0.1" :secret "s"})
                     (let [res (disco/discover-or-start! {:db db}
                                                         :probe (constantly true)
                                                         :spawn (fn [_]
                                                                  (throw (ex-info "should not spawn"
                                                                                  {}))))]
                       (expect (= :attach (:mode res)))
                       (expect (= 7890 (get-in res [:entry :port]))))))))

(defdescribe
  discover-or-start!-spawns-when-missing
  (it "discover or start! spawns when missing"
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
            (expect (= 1 @spawned))
            (expect (= :spawned (:mode res)))
            (expect (= 8000 (get-in res [:entry :port]))))))))

(defdescribe discover-or-start!-deletes-stale-then-times-out
             (it "discover or start! deletes stale then times out"
                 (let [db "/tmp/stale/vis.db"]
                   ;; stale entry: dead pid, so not fresh; spawn is a no-op so nothing comes up
                   (disco/write-registry! db {:pid dead-pid :port 1 :host "127.0.0.1" :secret "s"})
                   (let [res (disco/discover-or-start! {:db db}
                                                       :probe (constantly true)
                                                       :spawn (fn [_]
                                                                nil)
                                                       :timeout-ms 120
                                                       :poll-ms 20)]
                     (expect (= :timeout (:mode res)))
                     (expect (nil? (disco/read-registry db)))))))

(defdescribe discover-or-start!-preserves-a-live-owner-that-misses-health
             (it "discover or start! preserves a live owner that misses health"
                 (let [db
                       "/tmp/live-but-paused/vis.db"

                       spawned
                       (atom 0)]

                   (disco/write-registry! db {:pid 999 :port 7890 :host "127.0.0.1" :secret "s"})
                   (with-redefs [disco/pid-alive? (constantly true)]
                     (let [res (disco/discover-or-start! {:db db}
                                                         :probe (constantly false)
                                                         :spawn (fn [_]
                                                                  (swap! spawned inc))
                                                         :timeout-ms 80
                                                         :poll-ms 10)]
                       (expect (= :timeout (:mode res)))
                       (expect (zero? @spawned)
                               "a transiently unresponsive live owner must not spawn a bind loser")
                       (expect (= 999 (:pid (disco/read-registry db)))
                               "the live owner's only discovery record must survive"))))))

(defdescribe discover-or-start!-reattaches-when-a-live-owner-recovers
             (it "discover or start! reattaches when a live owner recovers"
                 (let [db
                       "/tmp/live-recovers/vis.db"

                       probes
                       (atom 0)]

                   (disco/write-registry! db {:pid 999 :port 7890 :host "127.0.0.1" :secret "s"})
                   (with-redefs [disco/pid-alive? (constantly true)]
                     (let [res (disco/discover-or-start! {:db db}
                                                         :probe (fn [_]
                                                                  (>= (swap! probes inc) 3))
                                                         :spawn (fn [_]
                                                                  (throw (ex-info "must not spawn"
                                                                                  {})))
                                                         :timeout-ms 500
                                                         :poll-ms 10)]
                       (expect (= :recovered (:mode res)))
                       (expect (= 999 (get-in res [:entry :pid]))))))))

(defdescribe acquire-spawn-lock!-is-exclusive-across-holders
             (it "acquire spawn lock! is exclusive across holders"
                 (let [db
                       "/tmp/lock/vis.db"

                       h1
                       (disco/acquire-spawn-lock! db)]

                   (expect (some? h1) "first acquirer wins the lock")
                   (expect (nil? (disco/acquire-spawn-lock! db))
                           "a second acquirer sees the lock held and backs off")
                   (disco/release-spawn-lock! h1)
                   (let [h2 (disco/acquire-spawn-lock! db)]
                     (expect (some? h2) "lock is re-acquirable once released")
                     (disco/release-spawn-lock! h2)))))

(defdescribe
  discover-or-start!-awaits-instead-of-piling-on-when-lock-held
  (it "discover or start! awaits instead of piling on when lock held"
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

        (expect (some? holder))
        (try (with-redefs [disco/pid-alive? (fn [_]
                                              true)]
               (future (Thread/sleep 60)
                       (disco/write-registry! db
                                              {:pid 777 :port 9100 :host "127.0.0.1" :secret "s"}))
               (let [res (disco/discover-or-start! {:db db}
                                                   :probe (constantly true)
                                                   :spawn (fn [_]
                                                            (swap! spawned inc))
                                                   :timeout-ms 3000
                                                   :poll-ms 10)]
                 (expect (= :awaited (:mode res)))
                 (expect (= 9100 (get-in res [:entry :port])))
                 (expect (zero? @spawned)
                         "no competing daemon is spawned while another holds the lock")))
             (finally (disco/release-spawn-lock! holder))))))

;; Regression #290: a cold JVM daemon still booting when a fixed timeout ran out
;; was abandoned and then exited unused, so `vis-agent tui --jvm` found no gateway.
(defdescribe discover-or-start!-keeps-waiting-while-the-spawned-daemon-boots
             (it "discover or start! keeps waiting while the spawned daemon boots"
                 (let [db
                       "/tmp/slow-boot/vis.db"

                       events
                       (atom [])

                       spawn
                       (fn [_]
                         (future (Thread/sleep 300)
                                 (disco/write-registry!
                                   db
                                   {:pid 4242 :port 8100 :host "127.0.0.1" :secret "s"}))
                         {:pid 4242 :boot-log "/tmp/slow-boot.log"})]

                   (with-redefs [disco/pid-alive? (fn [_]
                                                    true)]
                     (let [res (disco/discover-or-start! {:db db}
                                                         :probe (constantly true)
                                                         :spawn spawn
                                                         :on-event #(swap! events conj %)
                                                         :timeout-ms 50
                                                         :max-wait-ms 10000
                                                         :poll-ms 10)]
                       (expect (= :spawned (:mode res)) "a live daemon is awaited past timeout-ms")
                       (expect (= 8100 (get-in res [:entry :port])))
                       (expect (= {:phase :spawning :pid 4242 :boot-log "/tmp/slow-boot.log"}
                                  (first @events))))))))

;; Regression #290: a daemon that dies while booting fails the start at once and
;; names its boot log instead of holding the caller for the whole wait.
(defdescribe
  discover-or-start!-reports-a-spawned-daemon-that-exits
  (it "discover or start! reports a spawned daemon that exits"
      (let [events
            (atom [])

            started
            (System/nanoTime)

            res
            (disco/discover-or-start! {:db "/tmp/boot-crash/vis.db"}
                                      :probe (constantly true)
                                      :spawn (fn [_]
                                               {:pid dead-pid :boot-log "/tmp/boot-crash.log"})
                                      :on-event #(swap! events conj %)
                                      :timeout-ms 10000
                                      :max-wait-ms 10000
                                      :poll-ms 10)]

        (expect (= {:mode :exited :pid dead-pid :boot-log "/tmp/boot-crash.log"} res))
        (expect (= {:phase :exited :pid dead-pid :boot-log "/tmp/boot-crash.log"} (last @events)))
        (expect (< (/ (- (System/nanoTime) started) 1e6) 5000)
                "the wait ends when the daemon exits"))))

;; Regression #290: a concurrent starter waits as long as the spawner holds the
;; lock, and stops as soon as the spawner lets go without a daemon.
(defdescribe discover-or-start!-awaits-while-another-spawner-holds-the-lock
             (it "a slow daemon started by the lock holder is awaited past timeout-ms"
                 (let [db
                       "/tmp/herd-slow/vis.db"

                       holder
                       (disco/acquire-spawn-lock! db)]

                   (try (with-redefs [disco/pid-alive? (fn [_]
                                                         true)]
                          (future (Thread/sleep 300)
                                  (disco/write-registry!
                                    db
                                    {:pid 778 :port 9200 :host "127.0.0.1" :secret "s"}))
                          (let [res (disco/discover-or-start! {:db db}
                                                              :probe (constantly true)
                                                              :timeout-ms 50
                                                              :max-wait-ms 10000
                                                              :poll-ms 10)]
                            (expect (= :awaited (:mode res)))
                            (expect (= 9200 (get-in res [:entry :port])))))
                        (finally (disco/release-spawn-lock! holder)))))
             (it "a holder that lets go without a daemon ends the wait"
                 (let [db
                       "/tmp/herd-gone/vis.db"

                       holder
                       (disco/acquire-spawn-lock! db)

                       started
                       (System/nanoTime)]

                   (future (Thread/sleep 100) (disco/release-spawn-lock! holder))
                   (let [res (disco/discover-or-start! {:db db}
                                                       :probe (constantly true)
                                                       :timeout-ms 50
                                                       :max-wait-ms 10000
                                                       :poll-ms 10)]
                     (expect (= :timeout (:mode res)))
                     (expect (< (/ (- (System/nanoTime) started) 1e6) 5000))))))

;; Regression #290: the pid the spawner reports is the daemon's own, so a waiter
;; can tell a slow start from an exited one. Launches a short real `sleep`.
(defdescribe spawn-detached!-reports-the-daemon-pid
             (it "spawn detached! reports the daemon pid"
                 (with-redefs [paths/logs-dir
                               #(.getPath (io/file *tmp* "logs"))

                               disco/spawn-argv
                               (constantly ["/bin/sleep" "30"])]

                   (let [{:keys [pid boot-log]}
                         (disco/spawn-detached! {:db "/tmp/pid-report/vis.db"})

                         _
                         (Thread/sleep 300)

                         ^java.lang.ProcessHandle handle
                         (when pid (.orElse (java.lang.ProcessHandle/of (long pid)) nil))]

                     (try (expect (pos-int? pid))
                          (expect (and handle (.isAlive handle))
                                  "the wrapper shell has exited; the daemon runs on")
                          (expect (.isFile (io/file boot-log)))
                          (finally (when handle (.destroy handle))))))))

(defdescribe discover-or-start!-emits-nothing-on-the-fast-attach-path
             (it "discover or start! emits nothing on the fast attach path"
                 (let [db
                       "/tmp/ev-attach/vis.db"

                       events
                       (atom [])]

                   (with-redefs [disco/pid-alive? (fn [_]
                                                    true)]
                     (disco/write-registry! db {:pid 999 :port 7890 :host "127.0.0.1" :secret "s"})
                     (let [res (disco/discover-or-start! {:db db}
                                                         :probe (constantly true)
                                                         :on-event (fn [ev]
                                                                     (swap! events conj ev)))]
                       (expect (= :attach (:mode res)))
                       (expect (empty? @events) "an instant attach must stay silent"))))))

(defdescribe
  discover-or-start!-emits-spawning-tick-and-ready-when-it-spawns
  (it "discover or start! emits spawning tick and ready when it spawns"
      (let [db
            "/tmp/ev-spawn/vis.db"

            events
            (atom [])

            spawn
            (fn [_]
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

            (expect (= :spawned (:mode res)))
            (expect (= :spawning (first phases)) "the spawner announces it is starting the daemon")
            (expect (= {:phase :ready :mode :spawned :entry (:entry res)} (last @events))
                    "a ready event carries the mode + resolved entry"))))))

(defdescribe
  discover-or-start!-emits-awaiting-and-ready-when-another-process-spawns
  (it "discover or start! emits awaiting and ready when another process spawns"
      (let [db
            "/tmp/ev-await/vis.db"

            events
            (atom [])

            holder
            (disco/acquire-spawn-lock! db)]

        (expect (some? holder))
        (try (with-redefs [disco/pid-alive? (fn [_]
                                              true)]
               (future (Thread/sleep 60)
                       (disco/write-registry! db
                                              {:pid 777 :port 9100 :host "127.0.0.1" :secret "s"}))
               (let [res (disco/discover-or-start! {:db db}
                                                   :probe (constantly true)
                                                   :spawn (fn [_]
                                                            nil)
                                                   :on-event (fn [ev]
                                                               (swap! events conj ev))
                                                   :timeout-ms 3000
                                                   :poll-ms 10)
                     phases (map :phase @events)]

                 (expect (= :awaited (:mode res)))
                 (expect (= :awaiting (first phases))
                         "a waiter announces that ANOTHER vis is starting the gateway")
                 (expect (some #{:tick} phases) "a heartbeat ticks while awaiting")
                 (expect (= {:phase :ready :mode :awaited :entry (:entry res)} (last @events)))))
             (finally (disco/release-spawn-lock! holder))))))

;; Regression: the companion showed "Cancelling..." forever and the live turn
;; then vanished from the transcript. A second `vis-agent gateway start` for the
;; same DB (`--pair --host 0.0.0.0` beside a running `127.0.0.1` daemon) did not
;; fail to bind and simply overwrote the registry, so two daemons appended to one
;; session journal and the stop reached the half that did not hold the turn.
(defdescribe a-live-daemon-for-the-db-forbids-a-second-one
             (it "a live daemon for the db forbids a second one"
                 (let [db
                       "/tmp/split-brain/vis.db"

                       owner
                       (fn [opts]
                         (disco/foreign-owner
                           db
                           (merge {:alive? (constantly true) :listening? (constantly true)} opts)))]

                   ;; no registry at all: this process is free to start
                   (expect (nil? (owner {})))
                   (disco/write-registry!
                     db
                     {:pid (disco/current-pid) :port 7890 :host "127.0.0.1" :secret "t"})
                   ;; our own entry is not foreign - an in-process restart is no split brain
                   (expect (nil? (owner {})))
                   (disco/write-registry! db {:pid 4242 :port 7890 :host "0.0.0.0" :secret "t"})
                   ;; another live process owns the DB, whatever host it bound
                   (expect (= 4242 (:pid (owner {}))))
                   ;; a dead daemon frees the DB
                   (expect (nil? (owner {:alive? (constantly false)})))
                   ;; a recycled pid whose endpoint no longer listens frees the DB
                   (expect (nil? (owner {:listening? (constantly false)})))
                   ;; :memory never registers, so it never has an owner
                   (expect (nil? (disco/foreign-owner :memory
                                                      {:alive? (constantly true)
                                                       :listening? (constantly true)}))))))

(defdescribe endpoint-listening?-reads-a-real-socket
             (it "endpoint listening? reads a real socket"
                 (let [server
                       (java.net.ServerSocket. 0)

                       port
                       (.getLocalPort server)]

                   (try (expect (true? (disco/endpoint-listening? "127.0.0.1" port)))
                        ;; a wildcard bind is probed on loopback, which it also answers on
                        (expect (true? (disco/endpoint-listening? "0.0.0.0" port)))
                        (finally (.close server)))
                   (expect (false? (disco/endpoint-listening? "127.0.0.1" port))))
                 ;; an unusable endpoint is never mistaken for a live daemon
                 (expect (false? (disco/endpoint-listening? "127.0.0.1" nil)))
                 (expect (false? (disco/endpoint-listening? "" 0)))))

(defdescribe
  boot-logs-are-dated-and-unique
  (it "boot logs are dated and unique"
      (let [logs
            (io/file *tmp* "logs")

            seen
            (atom [])

            db
            "/tmp/boot-log-test.db"]

        (with-redefs [paths/logs-dir
                      #(.getPath logs)

                      disco/spawn-argv
                      (constantly ["vis-agent" "gateway" "start"])

                      disco/unix-launch-cmd
                      (fn [_ path]
                        (swap! seen conj (io/file path))
                        (throw (ex-info "Intercepted launch" {:intercepted true})))]

          (dotimes [_ 2]
            (try (disco/spawn-detached! {:db db})
                 (catch clojure.lang.ExceptionInfo error (expect (:intercepted (ex-data error)))))))
        (let [[a b] @seen]
          (expect (= 2 (count @seen)))
          (expect (not= a b))
          (doseq [^java.io.File file [a b]]
            (expect (= logs (.getParentFile (.getParentFile file))))
            (expect (some? (re-matches #"\d{4}-\d{2}-\d{2}" (.getName (.getParentFile file)))))
            (expect (str/starts-with? (.getName file)
                                      (str "gateway-boot-" (disco/registry-key db) "-")))
            (expect (.isFile file)))))))
