(ns com.blockether.vis.tui.notifications-test
  (:require [com.blockether.vis.tui.notifications :as notifications]
            [com.blockether.vis.tui.util :as util]
            [lazytest.core :refer [defdescribe describe expect it]]))

(defn- with-clock
  "Run `(f clock)` with `util/now-ms` reading the `clock` atom, on an empty store."
  [f]
  (let [clock (atom 0)]
    (notifications/dismiss-all!)
    (try (with-redefs [util/now-ms (fn ^long []
                                     (long @clock))]
           (f clock))
         (finally (notifications/dismiss-all!)))))

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
             (it "wakes the watchers when a shown entry expires"
                 (notifications/dismiss-all!)
                 (let [woken (promise)]
                   (notifications/watch! ::expiry #(when (empty? %) (deliver woken true)))
                   (try (notifications/notify! "Copied" :ttl-ms 50)
                        (notifications/show! nil)
                        (expect (true? (deref woken 2000 false)))
                        (finally (notifications/unwatch! ::expiry))))))

(defdescribe
  notification-pager-test
  (describe "clock"
            ;; User request: notices show one at a time as pages. A page that waits must not
            ;; lose its time, or a burst of notices expires before the user reads it.
            (it "counts down only the shown page"
                (with-clock
                  (fn [clock]
                    (notifications/notify! "a" :id "a" :ttl-ms 1000)
                    (notifications/notify! "b" :id "b" :ttl-ms 1000)
                    (expect (= ["b" 0 2]
                               ((juxt (comp :id :entry) :index :total) (notifications/show! nil))))
                    (reset! clock 600)
                    (notifications/page! 1)
                    (expect (= ["a" 1000]
                               ((juxt (comp :id :entry) :remaining-ms) (notifications/show! nil))))
                    (reset! clock 1500)
                    (expect (= {"a" 100 "b" 400}
                               (into {}
                                     (map (juxt :id #(notifications/remaining-ms % 1500)))
                                     (notifications/notifications))))))))
  (describe "session"
            ;; User request: a session notice stays until someone opens its session.
            (it "keeps a session notice, with all its time, until its session is in front"
                (with-clock
                  (fn [clock]
                    (notifications/notify! "Indexed" :id "x" :session-id "s2" :ttl-ms 4000)
                    (expect (nil? (notifications/show! "s1")))
                    (reset! clock 60000)
                    (expect (nil? (notifications/show! "s1")))
                    (expect (= ["x" 4000]
                               ((juxt (comp :id :entry) :remaining-ms)
                                 (notifications/show! "s2")))))))
            (it "shows a global notice in every session"
                (with-clock (fn [_]
                              (notifications/notify! "Saved" :id "g")
                              (expect (= "g"
                                         (-> (notifications/show! "s1")
                                             :entry
                                             :id)))
                              (expect (= "g"
                                         (-> (notifications/show! "s2")
                                             :entry
                                             :id)))))))
  (describe "paging"
            (it "wraps around, and a dismissed page gives its place to the next notice"
                (with-clock (fn [_]
                              (doseq [id ["a" "b" "c"]]
                                (notifications/notify! id :id id))
                              (notifications/show! nil)
                              (expect (= 2 (notifications/page! -1)))
                              (expect (= "a"
                                         (-> (notifications/show! nil)
                                             :entry
                                             :id)))
                              (notifications/page! -1)
                              (notifications/dismiss! "b")
                              (expect (= ["a" 1 2]
                                         ((juxt (comp :id :entry) :index :total)
                                           (notifications/show! nil)))))))))
