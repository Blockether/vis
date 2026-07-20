(ns com.blockether.vis.internal.providers-test
  "Fleet snapshot cache behavior for `configured-providers-cached` (issue #29):
   the footer-frequency read must never re-run the full config enumeration on
   a warm caller, a stale snapshot must refresh OFF the calling thread, and
   every same-process fleet mutation must invalidate the snapshot."
  (:require [clojure.test :refer [deftest is]]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.providers :as providers]))

(defn- rv
  "Resolve a (possibly private) var in the providers namespace."
  [sym]
  (ns-resolve 'com.blockether.vis.internal.providers sym))

(deftest configured-providers-cached-warm-reads-never-re-enumerate
  (let
    [calls
     (atom 0)

     fleet
     [{:id :fake :models [{:name "m1"}]}]]

    (with-redefs
      [config/load-config (fn []
                            (swap! calls inc)
                            {:providers fleet})]
      (providers/invalidate-configured-providers!)
      (is (= fleet (providers/configured-providers-cached))
          "cold read enumerates synchronously ONCE and returns the real fleet")
      (is (= 1 @calls))
      (dotimes [_ 10]
        (providers/configured-providers-cached))
      (is (= 1 @calls) "warm reads are pure cache hits — no re-enumeration"))
    (providers/invalidate-configured-providers!)))

(deftest configured-providers-cached-stale-serves-old-and-refreshes-in-background
  ;; REGRESSION: the TUI footer calls this on the render thread every ~80ms
  ;; frame. The enumeration (~200ms on machines with slow file IO) must NEVER
  ;; run synchronously on a warm caller — a stale snapshot is served as-is
  ;; while ONE background refresh replaces it.
  (let
    [slow-ms
     200

     calls
     (atom 0)

     fleet
     [{:id :fake :models [{:name "m1"}]}]

     cache
     (rv 'fleet-cache)]

    (with-redefs
      [config/load-config (fn []
                            (swap! calls inc)
                            (Thread/sleep slow-ms)
                            {:providers fleet})]
      ;; plant a STALE snapshot
      (reset! @cache {:at 0 :val [{:id :old}]})
      (let
        [t0 (System/nanoTime)
         stale (providers/configured-providers-cached)
         stale-ms (/ (- (System/nanoTime) t0) 1e6)]

        (is (= [{:id :old}] stale) "stale read serves the last-known snapshot immediately")
        (is (< stale-ms 50.0) "stale read must NOT block on the enumeration")
        ;; several stale reads while the refresh is in flight stay single-flight
        (dotimes [_ 5]
          (providers/configured-providers-cached))
        (Thread/sleep (+ slow-ms 250))
        (is (= 1 @calls) "only ONE background refresh runs (single-flight)")
        (is (= fleet (providers/configured-providers-cached)) "the refreshed snapshot lands")))
    (providers/invalidate-configured-providers!)))

(deftest fleet-mutations-invalidate-the-snapshot
  ;; The issue #29 follow-up: invalidate on change (long TTL stays safe), so a
  ;; provider add/remove/reorder shows in the footer cycle count immediately.
  (let [cache (rv 'fleet-cache)]
    (with-redefs
      [config/load-global-config-raw (constantly {:providers []})
       config/save-config! (fn [& _]
                             nil)
       config/reload-config! (constantly nil)]

      (reset! @cache {:at (System/currentTimeMillis) :val [{:id :warm}]})
      (providers/save-providers! [] nil)
      (is (nil? @@cache) "save-providers! drops the snapshot"))
    (with-redefs
      [config/remove-config-provider! (fn [& _]
                                        true)
       config/reload-config! (constantly nil)]

      (reset! @cache {:at (System/currentTimeMillis) :val [{:id :warm}]})
      (providers/remove-provider! :warm nil)
      (is (nil? @@cache) "remove-provider! drops the snapshot"))))
