(ns com.blockether.vis.internal.notifications-test
  "Coverage for `internal/notifications`.

   The hard rule (AGENTS.md) demands every namespace ship with a
   test file. This one pins down the behaviour every channel
   relying on cross-channel notifications depends on:

     - `notify!` returns a stable id, appends the entry, fires every
       registered watcher with the new full vec.
     - `notifications` returns the active vec and prunes expired
       entries on read.
     - `dismiss!` removes by id and fires watchers.
     - `watch!` / `unwatch!` register and tear down cleanly.
     - `clear-expired!` removes only past-deadline entries.
     - Bad inputs (non-string text, unknown :level) throw with a
       structured `:type` instead of silently degrading."
  (:require
   [com.blockether.vis.internal.notifications :as notif]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- with-clean-state
  "Fixture-ish helper. Drop every notification + watcher before the
   body so concurrent tests don't bleed state across `defdescribe`s."
  [body-fn]
  (notif/dismiss-all!)
  (notif/unwatch! :test)
  (notif/unwatch! :test-2)
  (body-fn))

(defdescribe notify-test
  (it "appends a new entry and returns its id"
    (with-clean-state
      (fn []
        (let [id (notif/notify! "hello" :level :info)]
          (expect (string? id))
          (let [active (notif/notifications)]
            (expect (= 1 (count active)))
            (expect (= id (:id (first active))))
            (expect (= "hello" (:text (first active))))
            (expect (= :info  (:level (first active)))))))))

  (it "fires every registered watcher with the new vec"
    (with-clean-state
      (fn []
        (let [received (atom [])]
          (notif/watch! :test (fn [snapshot] (swap! received conj snapshot)))
          (notif/notify! "first")
          (notif/notify! "second")
          (expect (= 2 (count @received)))
          (expect (= 1 (count (first @received))))
          (expect (= 2 (count (second @received))))
          (expect (= "second" (:text (last (last @received)))))))))

  (it "default level is :info, default ttl-ms is 3000"
    (with-clean-state
      (fn []
        (notif/notify! "x")
        (let [entry (first (notif/notifications))]
          (expect (= :info (:level entry)))
          (expect (some? (:until entry)))
          (expect (<= (- (long (:until entry)) (System/currentTimeMillis)) 3000))))))

  (it "ttl-ms nil produces a sticky entry (no auto-expiry)"
    (with-clean-state
      (fn []
        (notif/notify! "sticky" :ttl-ms nil)
        (expect (nil? (:until (first (notif/notifications))))))))

  (it "rejects non-string text"
    (with-clean-state
      (fn []
        (let [thrown? (try (notif/notify! 42) false (catch Exception _ true))]
          (expect thrown?)))))

  (it "rejects an unknown :level"
    (with-clean-state
      (fn []
        (let [thrown? (try (notif/notify! "x" :level :rainbow) false (catch Exception _ true))]
          (expect thrown?))))))

(defdescribe expiry-test
  (it "notifications-read prunes entries whose :until has passed"
    (with-clean-state
      (fn []
        ;; ttl 1ms -> already expired by the time the next read runs.
        (notif/notify! "ephemeral" :ttl-ms 1)
        (Thread/sleep 5)
        (expect (empty? (notif/notifications))))))

  (it "clear-expired! returns the post-prune vec"
    (with-clean-state
      (fn []
        (notif/notify! "stays"   :ttl-ms 60000)
        (notif/notify! "expires" :ttl-ms 1)
        (Thread/sleep 5)
        (let [pruned (notif/clear-expired!)]
          (expect (= 1 (count pruned)))
          (expect (= "stays" (:text (first pruned)))))))))

(defdescribe dismiss-test
  (it "dismiss! removes the entry by id and fires watchers"
    (with-clean-state
      (fn []
        (let [received (atom [])
              _        (notif/watch! :test (fn [snap] (swap! received conj snap)))
              id-1     (notif/notify! "a")
              _        (notif/notify! "b")
              removed? (notif/dismiss! id-1)]
          (expect (true? removed?))
          (expect (= 1 (count (notif/notifications))))
          (expect (= "b" (:text (first (notif/notifications)))))
          ;; 2 push events + 1 dismiss event = 3 watcher fires.
          (expect (= 3 (count @received)))))))

  (it "dismiss! returns false when no entry has the given id"
    (with-clean-state
      (fn []
        (notif/notify! "x")
        (expect (false? (notif/dismiss! "no-such-id"))))))

  (it "dismiss-all! clears everything"
    (with-clean-state
      (fn []
        (notif/notify! "a")
        (notif/notify! "b")
        (notif/dismiss-all!)
        (expect (empty? (notif/notifications)))))))

(defdescribe watcher-test
  (it "watch! replaces an existing watcher under the same key"
    (with-clean-state
      (fn []
        (let [a (atom 0) b (atom 0)]
          (notif/watch! :test (fn [_] (swap! a inc)))
          (notif/watch! :test (fn [_] (swap! b inc)))
          (notif/notify! "x")
          (expect (= 0 @a))
          (expect (= 1 @b))))))

  (it "unwatch! returns true on a known key, false otherwise"
    (with-clean-state
      (fn []
        (notif/watch! :test (fn [_]))
        (expect (true?  (notif/unwatch! :test)))
        (expect (false? (notif/unwatch! :test))))))

  (it "a watcher that throws is swallowed; other watchers still fire"
    (with-clean-state
      (fn []
        (let [survivor-fired? (atom false)]
          (notif/watch! :test   (fn [_] (throw (RuntimeException. "boom"))))
          (notif/watch! :test-2 (fn [_] (reset! survivor-fired? true)))
          (notif/notify! "x")
          (expect (true? @survivor-fired?)))))))
