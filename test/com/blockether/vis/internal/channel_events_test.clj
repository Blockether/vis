(ns com.blockether.vis.internal.channel-events-test
  (:require [com.blockether.vis.internal.channel-events :as ce]
            [lazytest.core :refer [defdescribe expect it throws?]]))

(defn- fresh-channel
  "A channel id no other test (or a mounted channel) can collide with — the bus
   is a process-local singleton."
  []
  (keyword "vis-test" (str "chan-" (random-uuid))))

(defdescribe subscription-test
             (it "registers, lists and removes a listener"
                 (let [chan (fresh-channel)]
                   (expect (empty? (ce/channel-event-listeners chan)))
                   (expect (= ::a
                              (ce/add-channel-event-listener! chan
                                                              ::a
                                                              (fn [_]))))
                   (ce/add-channel-event-listener! chan
                                                   ::b
                                                   (fn [_]))
                   (expect (= #{::a ::b} (set (ce/channel-event-listeners chan))))
                   (expect (nil? (ce/remove-channel-event-listener! chan ::a)))
                   (expect (= [::b] (ce/channel-event-listeners chan)))
                   (ce/remove-channel-event-listener! chan ::b)
                   (expect (empty? (ce/channel-event-listeners chan)))))
             (it "replaces a listener re-registered under the same id"
                 (let
                   [chan
                    (fresh-channel)

                    seen
                    (atom [])]

                   (ce/add-channel-event-listener! chan
                                                   ::only
                                                   (fn [_]
                                                     (swap! seen conj :first)))
                   (ce/add-channel-event-listener! chan
                                                   ::only
                                                   (fn [_]
                                                     (swap! seen conj :second)))
                   (expect (= 1 (count (ce/channel-event-listeners chan))))
                   (expect (= 1 (ce/publish-channel-event! chan {:op :ping})))
                   (expect (= [:second] @seen))
                   (ce/remove-channel-event-listener! chan ::only)))
             (it "removing an unknown listener or channel is a no-op"
                 (let [chan (fresh-channel)]
                   (expect (nil? (ce/remove-channel-event-listener! chan ::never-registered)))
                   (expect (empty? (ce/channel-event-listeners chan)))))
             (it "refuses a non-keyword channel id and a non-invokable listener"
                 (expect (throws? clojure.lang.ExceptionInfo
                                  #(ce/add-channel-event-listener! "tui"
                                                                   ::a
                                                                   (fn [_]))))
                 (expect (throws? clojure.lang.ExceptionInfo
                                  #(ce/add-channel-event-listener! (fresh-channel) ::a 42)))))

(defdescribe
  publish-channel-event!-test
  (it "delivers the event to every subscriber and counts them"
      (let
        [chan
         (fresh-channel)

         seen
         (atom [])]

        (ce/add-channel-event-listener! chan
                                        ::a
                                        (fn [e]
                                          (swap! seen conj [::a (:op e)])))
        (ce/add-channel-event-listener! chan
                                        ::b
                                        (fn [e]
                                          (swap! seen conj [::b (:op e)])))
        (expect (= 2 (ce/publish-channel-event! chan {:op :refresh})))
        (expect (= #{[::a :refresh] [::b :refresh]} (set @seen)))
        (ce/remove-channel-event-listener! chan ::a)
        (ce/remove-channel-event-listener! chan ::b)))
  (it "stamps :channel/id when absent and never overwrites an explicit one"
      (let
        [chan
         (fresh-channel)

         seen
         (atom nil)]

        (ce/add-channel-event-listener! chan
                                        ::a
                                        (fn [e]
                                          (reset! seen e)))
        (ce/publish-channel-event! chan {:op :ping})
        (expect (= chan (:channel/id @seen)))
        (ce/publish-channel-event! chan {:op :ping :channel/id :explicit})
        (expect (= :explicit (:channel/id @seen)))
        (ce/remove-channel-event-listener! chan ::a)))
  (it "delivers nothing, and throws nothing, when no one is subscribed"
      (expect (zero? (ce/publish-channel-event! (fresh-channel) {:op :ping}))))
  (it "does not deliver across channels"
      (let
        [a
         (fresh-channel)

         b
         (fresh-channel)

         seen
         (atom 0)]

        (ce/add-channel-event-listener! a
                                        ::a
                                        (fn [_]
                                          (swap! seen inc)))
        (ce/publish-channel-event! b {:op :ping})
        (expect (zero? @seen))
        (ce/remove-channel-event-listener! a ::a)))
  (it "a throwing listener takes down neither the publisher nor its siblings"
      ;; The whole point of the bus: an extension cannot crash a mounted channel.
      (let
        [chan
         (fresh-channel)

         seen
         (atom [])]

        (ce/add-channel-event-listener! chan
                                        ::boom
                                        (fn [_]
                                          (throw (ex-info "boom" {}))))
        (ce/add-channel-event-listener! chan
                                        ::ok
                                        (fn [_]
                                          (swap! seen conj :ok)))
        (expect (= 2 (ce/publish-channel-event! chan {:op :ping})))
        (expect (= [:ok] @seen))
        (ce/remove-channel-event-listener! chan ::boom)
        (ce/remove-channel-event-listener! chan ::ok)))
  (it "publishes against a SNAPSHOT, so a listener may unsubscribe mid-dispatch"
      (let
        [chan
         (fresh-channel)

         seen
         (atom [])]

        (ce/add-channel-event-listener! chan
                                        ::self
                                        (fn [_]
                                          (swap! seen conj :self)
                                          (ce/remove-channel-event-listener! chan ::self)))
        (ce/add-channel-event-listener! chan
                                        ::other
                                        (fn [_]
                                          (swap! seen conj :other)))
        (expect (= 2 (ce/publish-channel-event! chan {:op :ping})))
        (expect (= #{:self :other} (set @seen)))
        (expect (= [::other] (ce/channel-event-listeners chan)))
        (ce/remove-channel-event-listener! chan ::other)))
  (it "refuses a non-keyword channel id and a non-map event"
      (expect (throws? clojure.lang.ExceptionInfo #(ce/publish-channel-event! "tui" {:op :ping})))
      (expect (throws? clojure.lang.ExceptionInfo
                       #(ce/publish-channel-event! (fresh-channel) [:op :ping])))
      (expect (throws? clojure.lang.ExceptionInfo
                       #(ce/publish-channel-event! (fresh-channel) nil)))))
