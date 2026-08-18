(ns com.blockether.vis.internal.env-python-engine-test
  "What a session's GraalPy Engine costs and when it gives the memory back.

   A GraalVM `Engine` RETAINS every `Context` ever built on it. Closing the
   Context frees nothing while the Engine lives; only closing the Engine returns
   the memory, and then it returns ALL of it at once. Vis used to run one
   process-wide `shared-engine`, so a gateway leaked a whole session's Python
   heap on every session it ever served — measured at ~31 MB per closed context,
   and a 17h gateway sat on 3 GB of live GraalPy structures that its own idle-env
   reaper was structurally unable to shed (it evicts Contexts, and evicting a
   Context freed nothing).

   `env-python/new-engine!` gives every session its own Engine so the leak is
   impossible by construction. These tests pin BOTH halves of that trade:
   the memory behaviour, and the Truffle deadlock the shared engine was
   originally introduced to dodge."
  (:require [com.blockether.vis.internal.env-python :as ep]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.util.concurrent CountDownLatch TimeUnit)
           (org.graalvm.polyglot Context Engine)))

(def ^:private heavy-python
  "Enough of the stdlib that a retained context is measurable rather than noise.

   The size matters: retention scales with how much Python a context loaded, and
   a thin import list produces a signal small enough for GC jitter to hide."
  "import os, sys, json, re, collections, dataclasses, typing, itertools
import functools, datetime, textwrap, pathlib, base64, hashlib, subprocess
import urllib.parse, argparse, logging, csv, unittest, traceback, inspect, ast
")

(defn- heap-used-mb
  "Live heap after a settle. Three passes because one `System/gc` is a request,
   not a collection, and a single pass leaves enough float to swamp the signal
   this test measures."
  ^long []
  (dotimes [_ 3]
    (System/gc)
    (Thread/sleep 300))
  (let [rt (Runtime/getRuntime)] (quot (- (.totalMemory rt) (.freeMemory rt)) 1048576)))

(defn- run-and-close!
  "One session's worth of work: build a context on `engine`, load real Python
   into it, close the context. Deliberately does NOT close the engine — whether
   the memory comes back is the whole question."
  [^Engine engine]
  (let
    [ctx
     (-> (Context/newBuilder (into-array String ["python"]))
         (.engine engine)
         (.allowAllAccess true)
         (.build))]

    (.eval ctx "python" heavy-python)
    (.close ctx true)))

(defdescribe engine-retains-closed-contexts-test
  ;; The measurement the fix rests on, run as an A/B in one JVM so both arms see
  ;; the same GC.
  ;;
  ;; Only arm B is asserted hard. Arm A — how much a SHARED engine retains — is
  ;; reported, not gated: it measured 151 MB over five contexts on one run and
  ;; 46 MB on the next, which is plenty to diagnose but too noisy to fail a build
  ;; on. It is also the number that would legitimately go to zero if GraalVM ever
  ;; fixed the retention upstream, and a test that breaks on an upstream FIX is a
  ;; test nobody thanks you for. Arm B is the contract we actually ship.
  (it "an engine per context does not accumulate, unlike a shared one"
    (let
      [;; ARM A — one engine, five contexts built and CLOSED on it.
       shared
       (ep/new-engine!)

       _
       (run-and-close! shared) ;; warm: first context pays one-off init

       a-before
       (heap-used-mb)

       _
       (dotimes [_ 5] (run-and-close! shared))

       a-growth
       (- (heap-used-mb) a-before)

       _
       (.close shared true)

       ;; ARM B — the shipped shape: an engine per context, closed together.
       b-before
       (heap-used-mb)

       _
       (dotimes [_ 5]
         (let [e (ep/new-engine!)]
           (run-and-close! e)
           (.close e true)))

       b-growth
       (- (heap-used-mb) b-before)]

      ;; THE contract: closing a context AND its own engine gives the memory
      ;; back, so five sessions in a row cost nothing lasting. The bound is loose
      ;; on purpose — this must fail on a LEAK, never on GC jitter.
      (expect (< b-growth 40)
              (str "engine-per-context grew " b-growth " MB over 5 closed contexts"
                   " — something is sharing an engine again (shared arm grew " a-growth " MB)"))
      ;; Directional, and safe if upstream ever fixes retention (both go to ~0).
      (expect (<= b-growth a-growth)
              (str "engine-per-context (" b-growth " MB) retained MORE than a shared engine ("
                   a-growth " MB), which inverts the whole reason for new-engine!")))))

(defdescribe session-context-disposal-test
  ;; End to end, through the real `create-python-context` the loop calls — the
  ;; shape `dispose-environment!` relies on. A regression here means live
  ;; gateways grow again no matter what the synthetic test above says.
  (it "create-python-context hands back an engine, and closing both is flat"
    (let
      [{:keys [python-context python-engine]}
       (ep/create-python-context {})]

      (expect (instance? Engine python-engine)
              "create-python-context must return :python-engine for dispose-environment! to close")
      (expect (identical? python-engine (.getEngine ^Context python-context))
              "the returned engine must be the one the context actually rides")
      (.close ^Context python-context true)
      (.close ^Engine python-engine true))

    (let
      [before
       (heap-used-mb)

       _
       (dotimes [_ 4]
         (let [{:keys [python-context python-engine]} (ep/create-python-context {})]
           (.eval ^Context python-context "python" heavy-python)
           ;; exactly what dispose-environment! does, in that order
           (.close ^Context python-context true)
           (.close ^Engine python-engine true)))

       growth
       (- (heap-used-mb) before)]

      (expect (< growth 60)
              (str "4 disposed sessions grew the heap " growth " MB — the session engine is leaking")))))

(defdescribe engine-build-during-live-eval-test
  ;; The hazard that PUT the shared engine there (GraalVM 25.0.1: a standalone
  ;; `Context.build()` during a live eval froze the whole JVM at a Truffle
  ;; safepoint). Dropping the shared engine is only safe while this passes, so
  ;; it is pinned rather than trusted: a regression here is a hung test, which
  ;; is exactly the failure it guards against.
  (it "builds engines and contexts while another context is mid-eval, on virtual threads"
    (let
      [busy
       (ep/new-engine!)

       ctx
       (-> (Context/newBuilder (into-array String ["python"]))
           (.engine busy)
           (.allowAllAccess true)
           (.build))

       started
       (CountDownLatch. 1)

       eval-done
       (CountDownLatch. 1)

       _evaluator
       (Thread/startVirtualThread
         (fn []
           (.countDown started)
           (try (.eval ctx "python" "
import time
t0 = time.time()
n = 0
while time.time() - t0 < 6:
    n += 1
")
                (catch Throwable _ nil))
           (.countDown eval-done)))

       _
       (expect (.await started 10 TimeUnit/SECONDS) "the busy eval never started")

       ;; Several sessions starting AT ONCE against a busy interpreter — the
       ;; concurrent shape a gateway actually sees, not a single probe.
       outcomes
       (mapv deref
             (mapv (fn [_]
                     (future (try (let [e (ep/new-engine!)
                                        c (-> (Context/newBuilder (into-array String ["python"]))
                                              (.engine e)
                                              (.allowAllAccess true)
                                              (.build))]
                                    (.eval c "python" "y = 2 + 2")
                                    (.close c true)
                                    (.close e true)
                                    :ok)
                                  (catch Throwable t (.getMessage t)))))
                   (range 4)))]

      (expect (= [:ok :ok :ok :ok] outcomes)
              (str "building an engine during a live eval failed: " (pr-str outcomes)))
      (expect (.await eval-done 30 TimeUnit/SECONDS) "the busy eval never finished")
      (.close ctx true)
      (.close busy true))))
