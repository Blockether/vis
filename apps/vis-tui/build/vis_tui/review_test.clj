(ns vis-tui.review-test
  (:require [clojure.string :as str]
            [vis-tui.review :as review]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]])
  (:import [java.io Closeable]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [com.googlecode.lanterna.terminal.html HtmlTerminal]))

(defn- eventually
  [pred]
  (loop [attempts 100]
    (cond (pred) true
          (zero? attempts) false
          :else (do (Thread/sleep 20) (recur (dec attempts))))))

(deftest live-review-exports-themed-bounded-frames-and-closes-test
  (let [path
        (Files/createTempFile "activity-review-" ".html" (make-array FileAttribute 0))

        handle
        (review/start! path 40)

        ^HtmlTerminal terminal
        (:terminal handle)

        ^Thread worker
        (:worker handle)]

    (try (let [html (slurp (str path))]
           (is (str/includes? html "data-cols=\"40\""))
           (is (not (str/includes? html "data-rows=\"80\"")))
           (is (str/includes? html (str "--paper:" (.defaultBackground (.snapshot terminal)))))
           (is (str/includes? html "Test suite failed")))
         ;; Regression: a resize during the render loop's sleep was lost.
         (dotimes [i 4]
           (.resizeFromBrowser terminal (+ 50 i) 70)
           (is (eventually #(str/includes? (slurp (str path)) (str "data-cols=\"" (+ 50 i) "\"")))))
         (finally (.close ^Closeable handle) (Files/deleteIfExists path)))
    (is (.isClosed terminal))
    (is (not (.isAlive worker)))))
