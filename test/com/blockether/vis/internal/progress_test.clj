(ns com.blockether.vis.internal.progress-test
  (:require [clojure.test :refer [deftest is testing]]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.progress :as progress]))

(deftest make-progress-tracker-test
  (testing "returns map with :on-chunk and :get-timeline fns"
    (let [tracker (progress/make-progress-tracker)]
      (is (fn? (:on-chunk tracker)))
      (is (fn? (:get-timeline tracker)))
      (is (vector? (:get-timeline tracker)))
      (is (empty? (:get-timeline tracker)))))
  (testing "accepts optional on-update callback"
    (let [calls   (atom 0)
          tracker (progress/make-progress-tracker
                    {:on-update (fn [_timeline _chunk] (swap! calls inc))})]
      ((:on-chunk tracker) {:phase :reasoning :iteration 1 :thinking "hello"})
      (is (= 1 @calls)))))

(deftest on-chunk-reasoning-test
  (testing ":reasoning phase sets thinking on the iteration entry"
    (let [tracker (progress/make-progress-tracker)]
      ((:on-chunk tracker) {:phase :reasoning :iteration 1 :thinking "thinking..."})
      (let [tl (:get-timeline tracker)]
        (is (= 1 (count tl)))
        (is (= 1 (:iteration (first tl))))
        (is (= "thinking..." (:thinking (first tl)))))))
  (testing "reasoning event deltas preserve whitespace-only streaming chunks"
    (let [tracker (progress/make-progress-tracker)]
      (doseq [thinking ["The contract APIs failed in iteration"
                        "The contract APIs failed in iteration "
                        "The contract APIs failed in iteration 1 - lines"
                        "The contract APIs failed in iteration 1 - lines "
                        "The contract APIs failed in iteration 1 - lines 100-169"]]
        ((:on-chunk tracker) {:phase :reasoning :iteration 1 :thinking thinking}))
      (let [entry         (first (:get-timeline tracker))
            reconstructed (apply str (map :thinking (filter #(= :thinking (:type %)) (:events entry))))]
        (is (= "The contract APIs failed in iteration 1 - lines 100-169"
              (:thinking entry)))
        (is (= (:thinking entry) reconstructed))))
    (let [tracker (progress/make-progress-tracker)]
      (doseq [thinking [" " " a"]]
        ((:on-chunk tracker) {:phase :reasoning :iteration 1 :thinking thinking}))
      (let [entry         (first (:get-timeline tracker))
            reconstructed (apply str (map :thinking (filter #(= :thinking (:type %)) (:events entry))))]
        (is (= " a" (:thinking entry)))
        (is (= (:thinking entry) reconstructed))))))

(deftest on-chunk-form-result-test
  (testing ":form_result phase records code and result"
    (let [tracker (progress/make-progress-tracker)]
      ((:on-chunk tracker) {:phase :form-result :iteration 1 :form-idx 0
                            :code "(+ 1 2)" :result "3"
                            :stdout "" :stderr "" :execution-time-ms 5})
      (let [entry (first (:get-timeline tracker))]
        (is (= ["(+ 1 2)"] (:code entry)))
        (is (= ["3"] (:results entry)))))))

(deftest on-chunk-tool-result-render-test
  (testing ":form-result renders tool envelopes through extension renderer"
    (let [tracker (progress/make-progress-tracker)
          result  (extension/merge-provenance
                    (extension/success {:result {:lines ["x"]}
                                        :provenance {:op :demo}})
                    {:tool {:sym 'cat
                            :call "v/cat"}
                     :extension {:namespace 'com.acme.ext.fs}
                     :source {:paths ["/tmp/ext.clj"]
                              :mtime-max 1
                              :hash-sha256 nil}})]
      ((:on-chunk tracker) {:phase :form-result :iteration 1 :form-idx 0
                            :code "(demo)" :result result
                            :stdout "" :stderr "" :execution-time-ms 5})
      (let [entry (first (:get-timeline tracker))]
        (is (= [(str "Tool `:demo` ok — result shape {:type :map, :count 1, :keys (:lines), :shape {:lines {:type :vector, :count 1, :items {:type :string, :chars 1, :lines 1}}}}"
                  "; result {:lines [\"x\"]}"
                  "; provenance {:tool {:sym cat, :call \"v/cat\"}, :extension {:namespace com.acme.ext.fs}, :source {:paths [\"/tmp/ext.clj\"], :mtime-max 1, :hash-sha256 nil}}.")]
              (:results entry)))))))

(deftest on-chunk-iteration-final-test
  (testing ":iteration-final marks iteration as done"
    (let [tracker (progress/make-progress-tracker)]
      ((:on-chunk tracker) {:phase :iteration-final :iteration 1
                            :final {:answer "done"} :done? true})
      (let [entry (first (:get-timeline tracker))]
        (is (:done? entry))
        (is (= {:answer "done"} (:final entry))))))
  (testing "duplicate final chunks do not elide shifted form slots twice"
    (let [tracker (progress/make-progress-tracker)
          form-result (fn [idx code]
                        {:phase :form-result :iteration 1 :form-idx idx
                         :code code :result (str idx)
                         :stdout "" :stderr "" :execution-time-ms 1})
          final-chunk {:phase :iteration-final :iteration 1
                       :final {:answer "done"}
                       :answer-form-idx 0
                       :done? true}]
      ((:on-chunk tracker) (form-result 0 "(answer \"done\")"))
      ((:on-chunk tracker) (form-result 1 "(+ 1 2)"))
      ((:on-chunk tracker) final-chunk)
      ((:on-chunk tracker) final-chunk)
      (let [entry (first (:get-timeline tracker))]
        (is (= ["(+ 1 2)"] (:code entry)))
        (is (= ["1"] (:results entry)))))))

(deftest on-chunk-iteration-error-test
  (testing ":iteration-error records the error"
    (let [tracker (progress/make-progress-tracker)]
      ((:on-chunk tracker) {:phase :iteration-error :iteration 1
                            :thinking "bad" :error "something failed"})
      (let [entry (first (:get-timeline tracker))]
        (is (= "something failed" (:error entry)))))))

(deftest multi-iteration-timeline-test
  (testing "multiple iterations accumulate in order"
    (let [tracker (progress/make-progress-tracker)]
      ((:on-chunk tracker) {:phase :reasoning :iteration 1 :thinking "iter1"})
      ((:on-chunk tracker) {:phase :reasoning :iteration 2 :thinking "iter2"})
      (let [tl (:get-timeline tracker)]
        (is (= 2 (count tl)))
        (is (= 1 (:iteration (first tl))))
        (is (= 2 (:iteration (second tl))))))))
