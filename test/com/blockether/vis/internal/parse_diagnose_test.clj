(ns com.blockether.vis.internal.parse-diagnose-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.parse-diagnose :as parse-diagnose]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe turn-answer-string-restitch-test
  (it "repairs bare prose only inside the new turn-answer! host form"
    (let [candidates (parse-diagnose/try-answer-string-restitch
                       "(turn-answer! Hi there)"
                       "Hi")]
      (expect (seq candidates))
      (expect (some #(str/includes? % "(turn-answer! \"Hi there\")") candidates))))

  (it "does not repair retired answer forms"
    (expect (nil? (parse-diagnose/try-answer-string-restitch
                    "(answer Hi there)"
                    "Hi")))))
