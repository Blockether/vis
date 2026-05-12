(ns com.blockether.vis.internal.loop-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.loop :as loop]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe host-final-surface-test
  (it "recognizes only the new final-answer and title host forms"
    (let [answer-call? #'loop/answer-call-form?
          title-form?  #'loop/answer-compatible-meta-form?]
      (expect (true? (answer-call? '(turn-answer! [:ir [:p "Done"]]))))
      (expect (false? (answer-call? '(answer [:ir [:p "Done"]]))))
      (expect (true? (title-form? "(set-conversation-title \"Prompt cleanup\")")))
      (expect (false? (title-form? "(conversation-title \"Prompt cleanup\")")))))

  (it "preflight messages teach turn-answer! and not the retired answer form"
    (let [msg (loop/answer-position-error-message 0 2)]
      (expect (str/includes? msg "(turn-answer! ...)"))
      (expect (not (str/includes? msg "(answer ...)")))))

  (it "canonicalizes final answer IR and caps lazy children at the persistence boundary"
    (let [answer (loop/append-runtime-appendices
                   nil
                   [:ir [:ul (map (fn [n] [:li (str "item " n)]) (range))]]
                   nil)
          items (->> (nth answer 2) (drop 2) vec)
          rendered (loop/answer-str answer)]
      (expect (= :ir (first answer)))
      (expect (= 101 (count items)))
      (expect (str/includes? rendered "item 99"))
      (expect (not (str/includes? rendered "item 100")))
      (expect (str/includes? rendered "… many more"))
      (expect (not (str/includes? (pr-str answer) "LazySeq"))))))
