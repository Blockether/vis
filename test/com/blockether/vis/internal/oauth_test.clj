(ns com.blockether.vis.internal.oauth-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.oauth :as oauth]
            [lazytest.core :refer [defdescribe expect it throws?]]))

(defdescribe
  oauth-facade-test
  (it "fresh-within? is true only for a recent numeric stamp"
      (expect (oauth/fresh-within? (System/currentTimeMillis)))
      (expect (not (oauth/fresh-within? (- (System/currentTimeMillis) 999999))))
      (expect (not (oauth/fresh-within? nil)))
      (expect (not (oauth/fresh-within? "nope"))))
  (it "single-flight! reuses when reuse returns non-nil, else refreshes"
      (let [ran
            (atom 0)

            refresh!
            (fn []
              (swap! ran inc)
              {:token "fresh"})]

        (expect (= {:token "cached"}
                   (oauth/single-flight! (oauth/new-lock)
                                         (fn []
                                           {:token "cached"})
                                         refresh!)))
        (expect (zero? @ran))
        (expect (= {:token "fresh"}
                   (oauth/single-flight! (oauth/new-lock)
                                         (fn []
                                           nil)
                                         refresh!)))
        (expect (= 1 @ran))))
  (it "make-file-refresher collapses a concurrent burst into ONE exchange"
      (let [exchanges
            (atom 0)

            saved
            (atom nil)

            r
            (oauth/make-file-refresher
              {:load (fn []
                       (or @saved {:refresh-token "R"}))
               :saved-at :saved-at-ms
               :refresh-token :refresh-token
               :exchange! (fn [_]
                            (swap! exchanges inc)
                            (Thread/sleep 40)
                            {:access-token "T"})
               :persist! (fn [c]
                           (reset! saved (assoc c :saved-at-ms (System/currentTimeMillis))))
               :->token (fn [c]
                          {:token (:access-token c)})
               :no-token! #(throw (ex-info "no token" {}))})

            results
            (mapv deref
                  (mapv (fn [_]
                          (future (r)))
                        (range 40)))]

        (expect (= 1 @exchanges))
        (expect (every? #(= {:token "T"} %) results))))
  (it
    "make-file-refresher never REUSES the just-rejected token, but still reuses a different one"
    (let [exchanges
          (atom 0)

          ;; on-file token is locally FRESH (saved just now) but DEAD
          ;; server-side — exactly the timing bug: plain freshness reuse
          ;; would hand it straight back.
          saved
          (atom {:access-token "DEAD" :refresh-token "R" :saved-at-ms (System/currentTimeMillis)})

          r
          (oauth/make-file-refresher
            {:load (fn []
                     @saved)
             :saved-at :saved-at-ms
             :refresh-token :refresh-token
             :exchange! (fn [_]
                          (swap! exchanges inc)
                          {:access-token "FRESH" :refresh-token "R2"})
             :persist! (fn [c]
                         (reset! saved (assoc c :saved-at-ms (System/currentTimeMillis))))
             :->token (fn [c]
                        {:token (:access-token c)})
             :no-token! #(throw (ex-info "no token" {}))})]

      ;; 0-arity: no rejected token -> plain freshness reuse hands back DEAD.
      (expect (= {:token "DEAD"} (r)))
      (expect (zero? @exchanges))
      ;; rejecting DEAD forces a real exchange instead of reusing it.
      (expect (= {:token "FRESH"} (r "DEAD")))
      (expect (= 1 @exchanges))
      ;; the file now holds FRESH (differs from the rejected DEAD), so a
      ;; concurrent 401 on DEAD reuses FRESH — storm still collapses, no
      ;; second exchange.
      (expect (= {:token "FRESH"} (r "DEAD")))
      (expect (= 1 @exchanges))))
  (it
    "make-file-refresher PROPAGATES an exchange failure on 401 recovery — never swallows it nor hands back the dead token"
    (let [saved
          (atom {:access-token "DEAD" :refresh-token "R" :saved-at-ms (System/currentTimeMillis)})

          exchanges
          (atom 0)

          r
          (oauth/make-file-refresher
            {:load (fn []
                     @saved)
             :saved-at :saved-at-ms
             :refresh-token :refresh-token
             ;; dead/rotated refresh_token -> the server rejects the exchange
             ;; (HTTP 400 invalid_grant). The refresh genuinely cannot recover.
             :exchange! (fn [_]
                          (swap! exchanges inc)
                          (throw (ex-info "invalid_grant" {:status 400})))
             :persist! (fn [c]
                         (reset! saved (assoc c :saved-at-ms (System/currentTimeMillis))))
             :->token (fn [c]
                        {:token (:access-token c)})
             :no-token! #(throw (ex-info "no token" {}))})]

      ;; On 401 recovery we reject DEAD, so reuse can't short-circuit: the real
      ;; exchange runs, fails, and the failure MUST surface (so the turn loop's
      ;; try-refresh returns false and the original 401 is thrown) — it must not
      ;; be swallowed into a fake success handing DEAD straight back.
      (expect (throws? clojure.lang.ExceptionInfo #(r "DEAD")))
      (expect (= 1 @exchanges))))
  (it "two refreshers (different providers) refresh IN PARALLEL — no shared lock"
      ;; Proven by OVERLAP, not by wall-clock: a loaded CI box makes any "finished
      ;; under N ms" threshold flaky. Both exchanges must be INSIDE the critical
      ;; section at the same instant — under a shared lock the second thread could
      ;; not enter until the first returned, so its await would time out.
      (let [entered
            (java.util.concurrent.CountDownLatch. 2)

            overlapped
            (atom [])

            mk
            (fn []
              (oauth/make-file-refresher
                {:load (fn []
                         {:refresh-token "R" :saved-at-ms 0})
                 :saved-at :saved-at-ms
                 :refresh-token :refresh-token
                 :exchange! (fn [_]
                              (.countDown entered)
                              (swap! overlapped conj
                                (.await entered 5 java.util.concurrent.TimeUnit/SECONDS))
                              {:access-token "T" :saved-at-ms 0})
                 :persist! identity
                 :->token (fn [a]
                            {:token (:access-token a)})
                 :no-token! #(throw (ex-info "no" {}))}))

            fa
            (future ((mk)))

            fb
            (future ((mk)))]

        @fa
        @fb
        (expect (= [true true] @overlapped))))
  (it "make-file-refresher throws via :no-token! when no refresh token"
      (let [r (oauth/make-file-refresher {:load (fn []
                                                  {})
                                          :saved-at :saved-at-ms
                                          :refresh-token :refresh-token
                                          :exchange! (fn [_]
                                                       {:access-token "T"})
                                          :persist! identity
                                          :->token (fn [c]
                                                     {:token (:access-token c)})
                                          :no-token! #(throw (ex-info "no refresh token"
                                                                      {:type :test/no-token}))})]
        (expect (throws? clojure.lang.ExceptionInfo r))))
  (it
    "make-file-refresher with :lock-path collapses a burst into ONE exchange and creates the lock file"
    (let [lock-path
          (str (System/getProperty "java.io.tmpdir")
               "/vis-oauth-lock-test-"
               (System/nanoTime)
               ".lock")

          exchanges
          (atom 0)

          saved
          (atom nil)

          r
          (oauth/make-file-refresher
            {:load (fn []
                     (or @saved {:refresh-token "R"}))
             :lock-path lock-path
             :saved-at :saved-at-ms
             :refresh-token :refresh-token
             :exchange! (fn [_]
                          (swap! exchanges inc)
                          (Thread/sleep 40)
                          {:access-token "T"})
             :persist! (fn [c]
                         (reset! saved (assoc c :saved-at-ms (System/currentTimeMillis))))
             :->token (fn [c]
                        {:token (:access-token c)})
             :no-token! #(throw (ex-info "no token" {}))})

          results
          (mapv deref
                (mapv (fn [_]
                        (future (r)))
                      (range 20)))]

      (expect (= 1 @exchanges))
      (expect (every? #(= {:token "T"} %) results))
      (expect (.exists (io/file lock-path)))
      (.delete (clojure.java.io/file lock-path)))))

(defn- temp-lock-path
  "A throwaway advisory-lock path in a fresh temp dir."
  []
  (str (java.nio.file.Files/createTempDirectory "oauth-lock"
                                                (make-array java.nio.file.attribute.FileAttribute
                                                            0))
       "/lock"))

(defdescribe
  oauth-file-lock-test
  "`call-with-file-lock` guards the ROTATING token exchange. Two failure modes make
   it worse than no lock at all: running the exchange TWICE (the second reuses an
   already-rotated refresh token → HTTP 400 `invalid_grant`, the exact race this
   namespace exists to prevent), and blocking FOREVER on a peer process that
   stalled while holding the OS lock — which, since the caller also holds the
   in-process monitor, freezes every provider refresh in this JVM."
  (it "runs f exactly once when f throws"
      (let [runs (atom 0)]
        (expect (throws? clojure.lang.ExceptionInfo
                         #(oauth/call-with-file-lock (temp-lock-path)
                                                     (fn []
                                                       (swap! runs inc)
                                                       (throw (ex-info "exchange failed" {}))))))
        (expect (= 1 @runs))))
  (it "gives up on a peer that holds the lock and runs f unlocked"
      ;; `try-lock!` returning nil is exactly what a FOREIGN process holding the
      ;; POSIX lock looks like; the real `.lock` parked here forever.
      (with-redefs [oauth/try-lock! (fn [_ch]
                                      nil)]
        (let [t0 (System/currentTimeMillis)
              res (oauth/call-with-file-lock (temp-lock-path)
                                             300
                                             (fn []
                                               :ran))
              ms (- (System/currentTimeMillis) t0)]

          (expect (= :ran res))
          (expect (>= ms 300))
          (expect (< ms 5000)))))
  (it "still runs f when the lock path cannot be opened"
      (expect (= :ran
                 (oauth/call-with-file-lock "/dev/null/nope/lock"
                                            (fn []
                                              :ran))))
      (expect (= :ran
                 (oauth/call-with-file-lock nil
                                            (fn []
                                              :ran)))))
  (it
    "an interrupt while waiting aborts without running f and leaks no handle"
    ;; Cancellation is the ONE input that must not degrade to "run unlocked": a
    ;; rotating exchange fired on a thread that is being torn down spends the
    ;; refresh token for nobody. It must also not leak the lock-file handle —
    ;; an fd left to the GC keeps the OS advisory lock alive for an unbounded
    ;; time, stalling the peer process this lock exists to coordinate with.
    (with-redefs [oauth/try-lock! (fn [_ch]
                                    nil)]
      (let [path (temp-lock-path)
            os (java.lang.management.ManagementFactory/getOperatingSystemMXBean)
            fds (fn []
                  (when (instance? com.sun.management.UnixOperatingSystemMXBean os)
                    (.getOpenFileDescriptorCount ^com.sun.management.UnixOperatingSystemMXBean os)))
            before (fds)
            ran (atom 0)
            seen (atom [])
            threads (doall (for [_ (range 25)]
                             (doto (Thread. #(try (oauth/call-with-file-lock path
                                                                             60000
                                                                             (fn []
                                                                               (swap! ran inc)))
                                                  (catch Throwable t
                                                    (swap! seen conj
                                                      [(class t)
                                                       (.isInterrupted (Thread/currentThread))]))))
                               (.start))))]

        (Thread/sleep 200)
        (doseq [^Thread t threads]
          (.interrupt t))
        (doseq [^Thread t threads]
          (.join t 5000))
        (expect (= 0 @ran))
        (expect (= {[InterruptedException true] 25} (frequencies @seen)))
        (when before
          ;; Pre-fix this grew by one fd per interrupted acquisition.
          (expect (< (- (fds) before) 10)))))))
