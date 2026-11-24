(ns com.blockether.vis.internal.extension.events-test
  (:require [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.internal.extension.events :as events]
            [lazytest.core :refer [defdescribe expect it throws?]])
  (:import (java.util.concurrent CountDownLatch TimeUnit)))

(defn- collector
  "An atom of the events that a subscriber got, a latch that opens after `n` of
   them, and the subscriber."
  [n]
  (let [got (atom [])
        latch (CountDownLatch. (long n))]
    [got latch (fn [event] (swap! got conj event) (.countDown latch))]))

(defn- waited? [^CountDownLatch latch] (.await latch 5 TimeUnit/SECONDS))

(defn- fresh [] (str "ext-" (random-uuid)))

(defdescribe
  topic-pattern-test
  (it "matches '*' to one word and a last '#' to zero or more words"
      (expect (events/matches? "board.changed" "board.changed"))
      (expect (events/matches? "board.*" "board.changed"))
      (expect (not (events/matches? "board.*" "board")))
      (expect (not (events/matches? "board.*" "board.issue.moved")))
      (expect (events/matches? "board.#" "board"))
      (expect (events/matches? "board.#" "board.issue.moved"))
      (expect (events/matches? "*.issue.#" "board.issue.moved.done"))
      (expect (events/matches? "#" "anything.at.all"))
      (expect (not (events/matches? "board" "board.changed")))
      (expect (not (events/matches? "board.changed" "board.change")))))

(defdescribe
  publish-subscribe-test
  (it "gives each subscriber the events of its own extension in publish order"
      (let [ext (fresh)
            other (fresh)
            [got latch deliver] (collector 3)
            [foreign _ foreign-deliver] (collector 1)]
        (try (events/subscribe! ext :ctx "board.#" deliver)
             (events/subscribe! other :other-ctx "board.#" foreign-deliver)
             (expect (= 1 (events/publish! ext "board.changed" "{\"n\":1}")))
             (expect (= 0 (events/publish! ext "deploy.done" "null")))
             (events/publish! ext "board.issue.moved" "{\"n\":2}")
             (events/publish! ext "board" "[3]")
             (expect (waited? latch))
             (expect (= [["board.changed" {"n" 1}] ["board.issue.moved" {"n" 2}] ["board" [3]]]
                        (mapv (juxt #(get % "topic") #(get % "data")) @got)))
             (expect (every? #(document/valid? "gateway" "extension_event" %) @got))
             (expect (empty? @foreign))
             (finally (events/forget-context! :ctx) (events/forget-context! :other-ctx)))))
  (it "keeps a subscriber after its handler fails"
      (let [ext (fresh)
            [got latch deliver] (collector 1)]
        (try (events/subscribe! ext
                                :ctx
                                "job.*"
                                (fn [event]
                                  (if (= "job.fail" (get event "topic"))
                                    (throw (ex-info "handler failed" {}))
                                    (deliver event))))
             (events/publish! ext "job.fail" "{}")
             (events/publish! ext "job.done" "{}")
             (expect (waited? latch))
             (expect (= ["job.done"] (mapv #(get % "topic") @got)))
             (finally (events/forget-context! :ctx)))))
  (it "stops a subscription at unsubscribe, only for its own extension, and with its context"
      (let [ext (fresh)
            first-id (events/subscribe! ext :ctx-a "a.#" identity)
            second-id (events/subscribe! ext :ctx-b "a.#" identity)]
        (expect (= 2 (events/publish! ext "a.b" "1")))
        (expect (false? (events/unsubscribe! (fresh) first-id)))
        (expect (true? (events/unsubscribe! ext first-id)))
        (expect (false? (events/unsubscribe! ext first-id)))
        (expect (= 1 (events/publish! ext "a.b" "1")))
        (events/forget-context! :ctx-b)
        (expect (= 0 (events/publish! ext "a.b" "1")))
        (expect (false? (events/unsubscribe! ext second-id)))))
  (it "refuses a bad topic, a bad pattern and data over the contract bound"
      (let [ext (fresh)]
        (doseq [topic ["" "Board.changed" "board..changed" "board.*" "board.#" ".board"]]
          (expect (throws? clojure.lang.ExceptionInfo #(events/publish! ext topic "1")) topic))
        (doseq [pattern ["" "board.#.x" "board.**" "Board"]]
          (expect (throws? clojure.lang.ExceptionInfo #(events/subscribe! ext :ctx pattern identity))
                  pattern))
        (expect (throws? clojure.lang.ExceptionInfo
                         #(events/publish! ext "big" (str "\"" (apply str (repeat 70000 "x")) "\"")))))))
