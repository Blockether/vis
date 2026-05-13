(ns com.blockether.vis.internal.render-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.prompt]
            [com.blockether.vis.internal.render :as render]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe answer-ir-lazy-seq-test
  (it "realizes lazy Hiccup children before canonicalization and caps infinite seqs"
    (let [ast (render/->ast
                [:ir [:ul (map (fn [n] [:li (str "item " n)]) (range))]])
          items (->> (nth ast 2) (drop 2) vec)
          rendered (render/render ast :plain)
          printed (pr-str ast)]
      (expect (= :ir (first ast)))
      (expect (= 101 (count items)))
      (expect (str/includes? rendered "item 0"))
      (expect (str/includes? rendered "item 99"))
      (expect (not (str/includes? rendered "item 100")))
      (expect (str/includes? rendered "… many more"))
      (expect (not (str/includes? printed "LazySeq"))))))

(defn- tags-in
  [x]
  (let [out (volatile! [])]
    (letfn [(walk [v]
              (when (vector? v)
                (vswap! out conj (first v))
                (doseq [c (drop 2 v)] (walk c))))]
      (walk x)
      @out)))

(defdescribe answer-ir-retired-disclosure-test
  (it "does not canonicalize :details or :summary as answer IR tags"
    (let [ast (render/->ast
                [:ir [:details {:open? false}
                      [:summary "Plan"]
                      [:p "body"]]])]
      (expect (render/canonical? ast))
      (expect (not-any? #{:details :summary} (tags-in ast)))
      (expect (= "Planbody" (render/render ast :plain)))
      (expect (not (str/includes? (render/render ast :markdown) "<details")))
      (expect (not (str/includes? (render/render ast :markdown) "<summary")))))

  ;; Removed: "removes disclosure tags from the system answer grammar".
  ;; The system prompt was slimmed and no longer carries the
  ;; `forbidden := :details | :summary` directive in prose; the
  ;; canonical grammar still excludes :details/:summary, asserted
  ;; structurally above (`canonical?` + `not-any? #{:details :summary}`).
  )
