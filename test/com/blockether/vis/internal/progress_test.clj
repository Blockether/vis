(ns com.blockether.vis.internal.progress-test
  (:require [clojure.test :refer [deftest is testing]]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.progress :as progress]))

(deftest make-progress-tracker-test
  (testing "returns map with :on-chunk and :get-timeline fns"
    (let [tracker (progress/make-progress-tracker)]
      (is (fn? (:on-chunk tracker)))
      (is (fn? (:get-timeline tracker)))
      (is (vector? ((:get-timeline tracker))))
      (is (empty? ((:get-timeline tracker))))))
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
      (let [tl ((:get-timeline tracker))]
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
      (let [entry         (first ((:get-timeline tracker)))
            reconstructed (apply str (map :thinking (filter #(= :thinking (:type %)) (:events entry))))]
        (is (= "The contract APIs failed in iteration 1 - lines 100-169"
              (:thinking entry)))
        (is (= (:thinking entry) reconstructed))))
    (let [tracker (progress/make-progress-tracker)]
      (doseq [thinking [" " " a"]]
        ((:on-chunk tracker) {:phase :reasoning :iteration 1 :thinking thinking}))
      (let [entry         (first ((:get-timeline tracker)))
            reconstructed (apply str (map :thinking (filter #(= :thinking (:type %)) (:events entry))))]
        (is (= " a" (:thinking entry)))
        (is (= (:thinking entry) reconstructed))))))

(deftest on-chunk-form-result-test
  (testing ":form_result phase records code and result"
    (let [tracker (progress/make-progress-tracker)]
      ((:on-chunk tracker) {:phase :form-result :iteration 1 :form-idx 0
                            :code "(+ 1 2)" :result "3"
                            :stdout "" :stderr "" :execution-time-ms 5})
      (let [entry (first ((:get-timeline tracker)))]
        (is (= ["(+ 1 2)"] (:code entry)))
        (is (= ["\"3\""] (:results entry))))))
  (testing ":form_result elides forms marked with silent rendering-kind"
    (let [tracker (progress/make-progress-tracker)]
      ((:on-chunk tracker) {:phase :form-result :iteration 1 :form-idx 0
                            :code "(def hits (v/glob \"src\" \"**/*.clj\"))"
                            :result {:success? true :result ["src/a.clj"] :error nil}
                            :rendering-kind :vis/silent
                            :stdout "" :stderr "" :execution-time-ms 5})
      (let [entry (first ((:get-timeline tracker)))]
        (is (= [] (:code entry)))
        (is (= [] (:results entry)))))))

(deftest on-chunk-tool-result-render-test
  (testing ":form-result renders tool envelopes through extension renderer"
    (let [tracker (progress/make-progress-tracker)
          result  (extension/merge-info
                    (extension/success {:result {:lines ["x"]}
                                        :info {:op :demo}})
                    {:tool {:sym 'cat
                            :call "v/cat"}
                     :extension {:namespace 'com.acme.ext.fs}
                     :source {:paths ["/tmp/ext.clj"]
                              :mtime-max 1
                              :hash-sha256 nil}})]
      (with-redefs [extension/render-tool-result
                    (fn [_surface _result & _]
                      (str "Tool `:demo` ok - result shape {:type :map, :count 1, :keys (:lines), :shape {:lines {:type :vector, :count 1, :items {:type :string, :chars 1, :lines 1}}}}"
                        "; result {:lines [\"x\"]}"
                        "; info {:tool {:sym cat, :call \"v/cat\"}, :extension {:namespace com.acme.ext.fs}, :source {:paths [\"/tmp/ext.clj\"], :mtime-max 1, :hash-sha256 nil}}."))]
        ((:on-chunk tracker) {:phase :form-result :iteration 1 :form-idx 0
                              :code "(demo)" :result result
                              :stdout "" :stderr "" :execution-time-ms 5}))
      (let [entry (first ((:get-timeline tracker)))]
        (is (= [(str "Tool `:demo` ok - result shape {:type :map, :count 1, :keys (:lines), :shape {:lines {:type :vector, :count 1, :items {:type :string, :chars 1, :lines 1}}}}"
                  "; result {:lines [\"x\"]}"
                  "; info {:tool {:sym cat, :call \"v/cat\"}, :extension {:namespace com.acme.ext.fs}, :source {:paths [\"/tmp/ext.clj\"], :mtime-max 1, :hash-sha256 nil}}.")]
              (:results entry))))))
  (testing ":form-result marks preview envelopes and carries raw details"
    (let [tracker (progress/make-progress-tracker)
          result  (extension/merge-info
                    (assoc (extension/success {:result {:lines ["x"]}
                                               :info {:op :v/preview}})
                      :preview-eql {:result [[:lines {:from 0 :to 1}]]}
                      :presentation {:kind :source :path "src/demo.clj"})
                    {:tool {:sym 'preview
                            :call "v/preview"}
                     :extension {:namespace 'com.acme.ext.fs}})]
      (with-redefs [extension/render-tool-result (fn [_surface _result & _]
                                                   "1: x")]
        ((:on-chunk tracker) {:phase :form-result :iteration 1 :form-idx 0
                              :code "(v/preview file)" :result result
                              :stdout "" :stderr "" :execution-time-ms 5}))
      (let [entry (first ((:get-timeline tracker)))]
        (is (= [:preview] (:result-kinds entry)))
        (is (= ["1: x"] (:results entry)))
        (is (nil? (:shape (first (:result-details entry)))))
        (is (= "{:lines [\"x\"]}" (:raw (first (:result-details entry))))))))
  (testing ":form-result renders every captured preview from a compound form"
    (let [tracker (progress/make-progress-tracker)
          preview-a (extension/merge-info
                      (extension/success {:result {:hits [{:path "src/a.clj" :line 1 :text "A"}]}
                                          :info {:op :v/preview}})
                      {:tool {:sym 'preview :call "v/preview"}})
          preview-b (extension/merge-info
                      (extension/success {:result {:hits [{:path "src/b.clj" :line 2 :text "B"}]}
                                          :info {:op :v/preview}})
                      {:tool {:sym 'preview :call "v/preview"}})]
      (with-redefs [extension/render-tool-result (fn [_surface result & _]
                                                   (str "preview " (get-in result [:result :hits 0 :path])))]
        ((:on-chunk tracker) {:phase :form-result
                              :iteration 1
                              :form-idx 0
                              :code "(do (v/preview a) (v/preview b))"
                              :result preview-b
                              :previews [preview-a preview-b]
                              :stdout ""
                              :stderr ""
                              :execution-time-ms 5}))
      (let [entry (first ((:get-timeline tracker)))]
        (is (= [:preview] (:result-kinds entry)))
        (is (= ["preview src/a.clj\npreview src/b.clj"] (:results entry)))
        (is (= 2 (count (-> entry :result-details first :previews)))))))
  (testing ":form-result preserves tool info needed by TUI summary labels"
    (let [tracker (progress/make-progress-tracker)
          result  (extension/success
                    {:result {:hits [] :truncated-by :end-of-results}
                     :info {:op :any
                            :op-class :op/search
                            :presentation-kind :tool/search
                            :color-role :tool-color/search
                            :spec {:any ["alpha" "beta"] :paths ["src"]}
                            :paths ["src"]
                            :hit-count 0
                            :truncated-by :end-of-results}})]
      (with-redefs [extension/render-tool-result (fn [_surface _result & _] "Searched `src`.")]
        ((:on-chunk tracker) {:phase :form-result :iteration 1 :form-idx 0
                              :code "(v/rg {:any [\"alpha\" \"beta\"] :paths [\"src\"]})"
                              :result result
                              :stdout "" :stderr "" :execution-time-ms 5}))
      (let [detail (-> ((:get-timeline tracker)) first :result-details first)]
        (is (= :op/search (:op-class detail)))
        (is (= {:any ["alpha" "beta"] :paths ["src"]} (:spec detail)))
        (is (= ["src"] (:paths detail)))
        (is (= 0 (:hit-count detail)))))))

(deftest on-chunk-iteration-final-test
  (testing ":iteration-final marks iteration as done"
    (let [tracker (progress/make-progress-tracker)]
      ((:on-chunk tracker) {:phase :iteration-final :iteration 1
                            :final {:answer "done"} :done? true})
      (let [entry (first ((:get-timeline tracker)))]
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
      (let [entry (first ((:get-timeline tracker)))]
        (is (= ["(+ 1 2)"] (:code entry)))
        (is (= ["\"1\""] (:results entry)))))))

(deftest on-chunk-iteration-error-test
  (testing ":iteration-error records the error"
    (let [tracker (progress/make-progress-tracker)]
      ((:on-chunk tracker) {:phase :iteration-error :iteration 1
                            :thinking "bad" :error "something failed"})
      (let [entry (first ((:get-timeline tracker)))]
        (is (= "something failed" (:error entry)))))))

(deftest multi-iteration-timeline-test
  (testing "multiple iterations accumulate in order"
    (let [tracker (progress/make-progress-tracker)]
      ((:on-chunk tracker) {:phase :reasoning :iteration 1 :thinking "iter1"})
      ((:on-chunk tracker) {:phase :reasoning :iteration 2 :thinking "iter2"})
      (let [tl ((:get-timeline tracker))]
        (is (= 2 (count tl)))
        (is (= 1 (:iteration (first tl))))
        (is (= 2 (:iteration (second tl))))))))
