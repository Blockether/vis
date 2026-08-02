(ns com.blockether.vis.internal.notifications-test
  "Notifications are process-global mutable state shared by every channel, so
   each test here starts and ends by clearing the store rather than relying on
   ordering. What matters to callers: the id you get back is the id you dismiss
   with, an entry expires on its own deadline, `:ttl-ms nil` means sticky, and
   bad input is refused loudly instead of pushing a broken entry to every UI."
  (:require [com.blockether.vis.internal.notifications :as notif]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- with-clean-store
  "Run `f` against an empty store and leave the store empty afterwards."
  [f]
  (notif/dismiss-all!)
  (try (f) (finally (notif/dismiss-all!))))

(defdescribe
  notify-test
  (it "returns a fresh uuid string per push and keeps insertion order"
      (with-clean-store
        (fn []
          (let
            [id1
             (notif/notify! "first")

             id2
             (notif/notify! "second")]

            (expect (parse-uuid id1) "the id must be a uuid string callers can round-trip")
            (expect (not= id1 id2))
            (expect (= ["first" "second"] (mapv :text (notif/notifications))) "oldest first")))))
  (it "gives every entry the same shape"
      (with-clean-store (fn []
                          (notif/notify! "hello" :level :warn)
                          (let [entry (first (notif/notifications))]
                            (expect (= [:created-at :id :level :text :until] (sort (keys entry))))
                            (expect (= :warn (:level entry)))))))
  (it "treats :ttl-ms nil as sticky"
      (with-clean-store (fn []
                          (notif/notify! "sticky" :ttl-ms nil)
                          ;; No `:until` means `clear-expired!` can never drop it — that is the
                          ;; only way to publish a notice that outlives the default 3s window.
                          (expect (nil? (:until (first (notif/notifications)))))
                          (expect (= ["sticky"] (mapv :text (notif/clear-expired!)))))))
  (it "drops an entry once its own deadline passes"
      (with-clean-store (fn []
                          (notif/notify! "sticky" :ttl-ms nil)
                          (notif/notify! "blink" :ttl-ms 1)
                          (Thread/sleep 20)
                          ;; Reading prunes implicitly, so a stale entry can never be rendered.
                          (expect (= ["sticky"] (mapv :text (notif/notifications)))))))
  (it "refuses bad input instead of publishing it"
      (with-clean-store
        (fn []
          (expect (= :vis/notify-bad-level
                     (try (notif/notify! "x" :level :nope)
                          (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
          (expect (= :vis/notify-bad-text
                     (try (notif/notify! 42)
                          (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
          (expect (= [] (notif/notifications)) "a refused push leaves no trace")))))

(defdescribe dismiss-test
             (it "dismisses by id once, and says so the second time"
                 (with-clean-store (fn []
                                     (let [id (notif/notify! "bye" :ttl-ms nil)]
                                       (expect (true? (notif/dismiss! id)))
                                       (expect (false? (notif/dismiss! id))
                                               "a repeated dismiss is a no-op, not an error")
                                       (expect (= [] (notif/notifications)))))))
             (it "dismiss-all! empties the store and returns the empty vec"
                 (with-clean-store (fn []
                                     (notif/notify! "a" :ttl-ms nil)
                                     (notif/notify! "b" :ttl-ms nil)
                                     (expect (= [] (notif/dismiss-all!)))
                                     (expect (= [] (notif/notifications)))))))

(defdescribe watchers-test
             (it "hands every watcher the full snapshot after each change"
                 (with-clean-store (fn []
                                     (let [seen (atom [])]
                                       (try (notif/watch! ::test #(swap! seen conj (mapv :text %)))
                                            (notif/notify! "a" :ttl-ms nil)
                                            (notif/notify! "b" :ttl-ms nil)
                                            ;; A watcher gets the whole vec, not a delta — a channel can repaint
                                            ;; from the argument without reading the store back.
                                            (expect (= [["a"] ["a" "b"]] @seen))
                                            (finally (notif/unwatch! ::test)))))))
             (it "unwatch! reports whether a watcher was actually registered"
                 (with-clean-store (fn []
                                     (notif/watch! ::test
                                                   (fn [_]))
                                     (expect (true? (notif/unwatch! ::test)))
                                     (expect (false? (notif/unwatch! ::test)))
                                     (expect (false? (notif/unwatch! ::never-registered))))))
             (it "refuses a non-callable watcher"
                 (with-clean-store (fn []
                                     (expect (= :vis/notify-bad-watcher
                                                (try (notif/watch! ::bad 5)
                                                     (catch clojure.lang.ExceptionInfo e
                                                       (:type (ex-data e))))))))))
