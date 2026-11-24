(ns com.blockether.vis.internal.bus-test
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.internal.bus :as bus]
            [lazytest.core :refer [defdescribe expect it throws?]])
  (:import (java.util.concurrent CountDownLatch TimeUnit)))

(defn- collector
  "An atom of the signals that a subscriber got, a latch that opens after `n` of
   them, and the subscriber."
  [n]
  (let [got (atom [])
        latch (CountDownLatch. (long n))]
    [got latch (fn [signal] (swap! got conj signal) (.countDown latch))]))

(defn- waited? [^CountDownLatch latch] (.await latch 5 TimeUnit/SECONDS))

(defn- fresh [] (str "ext-" (random-uuid)))

(defn- publish
  "Publish JSON text `data-json` under `topic` as extension `ext`, from session `sid`."
  ([ext topic data-json] (publish ext nil topic data-json "global"))
  ([ext sid topic data-json scope]
   (bus/publish-json! {:owner ext :session sid} topic data-json scope)))

(defn- subscribe
  "Subscribe as extension `ext` in namespace `ctx`, from session `sid`, with `scope`."
  ([ext ctx pattern deliver] (subscribe ext ctx nil nil pattern deliver))
  ([ext ctx sid scope pattern deliver]
   (bus/subscribe! {:owner ext :context ctx :session sid :scope scope} pattern deliver)))

;; The SDK outside Vis runs the same cases in `test_outside.py`.
(def ^:private pattern-cases
  (delay (get (json/read-json (slurp (io/resource "vis-contract/fixtures/bus-patterns.json")))
              "cases")))

(defdescribe
  topic-pattern-test
  (it "gives each case of the shared fixture its answer"
      (expect (seq @pattern-cases))
      (doseq [{:strs [pattern topic matches]} @pattern-cases]
        (expect (= matches (bus/matches? pattern topic)) (str pattern " -> " topic)))))

(defdescribe
  bus-test
  (it "delivers in publish order, only to the matching subscriptions of the same extension"
      (let [ext (fresh)
            other (fresh)
            [got latch deliver] (collector 3)
            [foreign _ foreign-deliver] (collector 1)]
        (try (subscribe ext :ctx "board.#" deliver)
             (subscribe other :other-ctx "board.#" foreign-deliver)
             (expect (= 1 (publish ext "board.changed" "{\"n\":1}")))
             (expect (= 0 (publish ext "deploy.done" "null")))
             (publish ext "board.issue.moved" "{\"n\":2}")
             (publish ext "board" "[3]")
             (expect (waited? latch))
             (expect (= [["board.changed" {"n" 1}] ["board.issue.moved" {"n" 2}] ["board" [3]]]
                        (mapv (juxt #(get % "topic") #(get % "data")) @got)))
             (expect (every? #(document/valid? "gateway" "bus_signal" %) @got))
             (expect (empty? @foreign))
             (finally (bus/forget-context! :ctx) (bus/forget-context! :other-ctx)))))
  (it "keeps a subscriber after its handler fails"
      (let [ext (fresh)
            [got latch deliver] (collector 1)]
        (try (subscribe ext
                        :ctx
                        "job.*"
                        (fn [signal]
                          (if (= "job.fail" (get signal "topic"))
                            (throw (RuntimeException. "handler failed"))
                            (deliver signal))))
             (publish ext "job.fail" "{}")
             (publish ext "job.done" "{}")
             (expect (waited? latch))
             (expect (= ["job.done"] (mapv #(get % "topic") @got)))
             (finally (bus/forget-context! :ctx)))))
  (it "stops a subscription at unsubscribe, only for its own extension, and with its context"
      (let [ext (fresh)
            first-id (subscribe ext :ctx-a "a.#" identity)
            second-id (subscribe ext :ctx-b "a.#" identity)]
        (expect (= 2 (publish ext "a.b" "1")))
        (expect (false? (bus/unsubscribe! (fresh) first-id)))
        (expect (true? (bus/unsubscribe! ext first-id)))
        (expect (false? (bus/unsubscribe! ext first-id)))
        (expect (= 1 (publish ext "a.b" "1")))
        (bus/forget-context! :ctx-b)
        (expect (= 0 (publish ext "a.b" "1")))
        (expect (false? (bus/unsubscribe! ext second-id)))))
  (it "refuses a bad topic, a bad pattern, a bad scope and data over the contract bound"
      (let [ext (fresh)]
        (doseq [topic ["" "Board.changed" "board..changed" "board.*" "board.#" ".board"]]
          (expect (throws? clojure.lang.ExceptionInfo #(publish ext topic "1")) topic))
        (doseq [pattern ["" "board.#.x" "board.**" "Board"]]
          (expect (throws? clojure.lang.ExceptionInfo #(subscribe ext :ctx pattern identity))
                  pattern))
        (expect (throws? clojure.lang.ExceptionInfo #(publish ext "s1" "board" "1" "all")))
        (expect (throws? clojure.lang.ExceptionInfo
                         #(subscribe ext :ctx "s1" "all" "board" identity)))
        (expect (throws? clojure.lang.ExceptionInfo
                         #(publish ext "big" (str "\"" (apply str (repeat 70000 "x")) "\"")))))))

(defdescribe
  scope-test
  ;; User request 2026-11-25: a session signal reaches only the subscribers of its
  ;; own session. A global signal reaches each subscriber of the extension.
  (it "gives a session signal only to its session, and a global signal to each subscriber"
      (let [ext (fresh)
            [a-both a-both-latch a-both-deliver] (collector 2)
            [b-both b-both-latch b-both-deliver] (collector 1)
            [app app-latch app-deliver] (collector 1)
            [a-global a-global-latch a-global-deliver] (collector 1)
            [a-session a-session-latch a-session-deliver] (collector 1)]
        (try (subscribe ext :ctx "a" nil "s.#" a-both-deliver)
             (subscribe ext :ctx "b" nil "s.#" b-both-deliver)
             (subscribe ext :ctx nil nil "s.#" app-deliver)
             (subscribe ext :ctx "a" "global" "s.#" a-global-deliver)
             (subscribe ext :ctx "a" "session" "s.#" a-session-deliver)
             (expect (= 2 (publish ext "a" "s.local" "1" "session")))
             (expect (= 4 (publish ext "a" "s.all" "2" "global")))
             (expect (every? waited? [a-both-latch b-both-latch app-latch a-global-latch a-session-latch]))
             (expect (= ["s.local" "s.all"] (mapv #(get % "topic") @a-both)))
             (expect (= ["s.all"] (mapv #(get % "topic") @b-both)))
             (expect (= ["s.all"] (mapv #(get % "topic") @app)))
             (expect (= ["s.all"] (mapv #(get % "topic") @a-global)))
             (expect (= [["s.local" "session"]] (mapv (juxt #(get % "topic") #(get % "scope")) @a-session)))
             (finally (bus/forget-context! :ctx)))))
  (it "refuses scope 'session' to a caller without a session"
      (let [ext (fresh)]
        (expect (throws? clojure.lang.ExceptionInfo #(publish ext nil "s.x" "1" "session")))
        (expect (throws? clojure.lang.ExceptionInfo #(subscribe ext :ctx nil "session" "s.#" identity))))))

(defdescribe
  vis-signal-test
  (it "lets only Vis publish a 'vis.' topic, and gives it only to the 'vis' patterns of each owner"
      (let [ext (fresh)
            other (fresh)
            [all _ all-deliver] (collector 1)
            [mine mine-latch mine-deliver] (collector 1)
            [theirs theirs-latch theirs-deliver] (collector 1)
            [own own-latch own-deliver] (collector 1)
            gateway-id (bus/subscribe! "vis.session.deleted" own-deliver)]
        (try (subscribe ext :ctx "#" all-deliver)
             (subscribe ext :ctx "vis.session.*" mine-deliver)
             (subscribe other :other-ctx "s1" nil "vis.#" theirs-deliver)
             (expect (throws? clojure.lang.ExceptionInfo #(publish ext "vis.session.deleted" "{}")))
             (expect (pos? (long (bus/publish! "vis.session.deleted" {"session_id" "s1"}))))
             (expect (every? waited? [mine-latch theirs-latch own-latch]))
             (expect (empty? @all))
             (doseq [signal (concat @mine @theirs @own)]
               (expect (document/valid? "gateway" "bus_signal" signal))
               (expect (document/valid? "gateway" "bus_vis_signal" (select-keys signal ["topic" "data"])))
               (expect (= {"session_id" "s1"} (get signal "data"))))
             (finally (bus/unsubscribe! :vis gateway-id)
                      (bus/forget-context! :ctx)
                      (bus/forget-context! :other-ctx)))))
  (it "refuses a signal of Vis that the contract does not define"
      (expect (throws? clojure.lang.ExceptionInfo #(bus/publish! "vis.nothing" {})))
      (expect (throws? clojure.lang.ExceptionInfo #(bus/publish! "vis.session.deleted" {"id" "s1"})))
      (expect (throws? clojure.lang.ExceptionInfo #(bus/publish! "board.changed" {})))))
