(ns com.blockether.vis.tui.notifications-test
  (:require [com.blockether.vis.tui.notifications :as notifications]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe notification-store-test
             ;; A notice without a session reaches every open session with one notice id. Each
             ;; tab stream gives it to this store, which must show one toast, not one per tab.
             (it "replaces the active entry that has the same id"
                 (notifications/dismiss-all!)
                 (notifications/notify! "Deploying" :id "deploy")
                 (notifications/notify! "Deployed" :id "deploy" :level :success)
                 (expect (= [["deploy" "Deployed" :success]]
                            (mapv (juxt :id :text :level) (notifications/notifications))))
                 (notifications/dismiss-all!))
             ;; The screen repaints only when something changes. Without a wake at expiry, a toast
             ;; stayed on an idle screen until the next key press.
             (it "wakes the watchers when an entry expires"
                 (notifications/dismiss-all!)
                 (let [woken (promise)]
                   (notifications/watch! ::expiry #(when (empty? %) (deliver woken true)))
                   (try (notifications/notify! "Copied" :ttl-ms 50)
                        (expect (true? (deref woken 2000 false)))
                        (finally (notifications/unwatch! ::expiry))))))
