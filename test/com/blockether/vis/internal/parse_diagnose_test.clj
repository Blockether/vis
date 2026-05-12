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

(defdescribe turn-answer-escape-rescue-test
  (it "rewrites terminal \\e notation only inside turn-answer strings"
    (let [src   "(turn-answer! [:ir [:p \"paste markers \" [:c \"\\e[200~\"] [:c \"\\e[201~\"]]])"
          fixed (parse-diagnose/try-answer-escape-rescue
                  src
                  "Unsupported escape character: \\e."
                  (constantly true))]
      (expect (some? fixed))
      (expect (str/includes? fixed "[:c \"\\\\e[200~\"]"))
      (expect (str/includes? fixed "[:c \"\\\\e[201~\"]"))))

  (it "does not relax Clojure strings outside turn-answer"
    (expect (nil? (parse-diagnose/try-answer-escape-rescue
                    "(println \"\\e[200~\")"
                    "Unsupported escape character: \\e."
                    (constantly true))))))
