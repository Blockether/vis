(ns com.blockether.vis.internal.prompt-tape-test
  "Phase 7 prep tests: REPL tape rendering primitives. Covers
   `format-tape-iteration`, `format-system-vars-block`,
   `format-live-vars-block`, plus the small helpers
   `tape-iteration-header`, `tape-result-line`, `tape-side-effect-line`.

   These primitives live alongside the legacy <journal>/<bindings>
   renderers; until Phase 7 wires them into `build-iteration-context`
   they stay dormant. The contract is fully unit-testable here."
  (:require
   [clojure.string :as string]
   [com.blockether.vis.internal.prompt :as prompt]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe tape-iteration-header-test
  (it "renders all coordinates when present"
    (let [s (prompt/tape-iteration-header
              {:iteration-position 5
               :turn-position 1
               :conv-id "ab12"
               :state-id "cd34"
               :status :done})]
      (expect (= ";; --- iteration 5 | turn=1 | conv=ab12 | state=cd34 | status=done ---" s))))
  (it "elides nil coordinates so partial test contexts render cleanly"
    (let [s (prompt/tape-iteration-header
              {:iteration-position 1 :status :current})]
      (expect (= ";; --- iteration 1 | status=current ---" s))))
  (it "supports each :status keyword"
    (doseq [st [:done :error :current]]
      (let [s (prompt/tape-iteration-header
                {:iteration-position 1 :status st})]
        (expect (string/ends-with? s (str "status=" (name st) " ---")))))))

(defdescribe tape-result-line-test
  (it "renders `;; => <pr-str>` for ordinary values"
    (expect (= ";; => 42" (prompt/tape-result-line 42)))
    (expect (= ";; => \"hello\"" (prompt/tape-result-line "hello")))
    (expect (= ";; => [1 2 3]" (prompt/tape-result-line [1 2 3]))))
  (it "returns nil for the :vis/no-result sentinel"
    (expect (nil? (prompt/tape-result-line :vis/no-result))))
  (it "truncates large pr-strs at TAPE_RESULT_MAX_CHARS"
    (let [big (apply str (repeat 5000 "x"))
          s   (prompt/tape-result-line big)]
      (expect (string/starts-with? s ";; => "))
      (expect (<= (count s) (+ 32 prompt/TAPE_RESULT_MAX_CHARS))))))

(defdescribe tape-side-effect-line-test
  (it "renders stdout / stderr / error / timeout with the proper tag"
    (expect (= ";; ! stdout> hi"  (prompt/tape-side-effect-line :stdout "hi")))
    (expect (= ";; ! stderr> oops" (prompt/tape-side-effect-line :stderr "oops")))
    (expect (= ";; ! ERROR boom"   (prompt/tape-side-effect-line :error "boom")))
    (expect (= ";; ! TIMEOUT 120s" (prompt/tape-side-effect-line :timeout "120s"))))
  (it "returns nil for blank / nil text so the renderer can drop it"
    (expect (nil? (prompt/tape-side-effect-line :stdout "")))
    (expect (nil? (prompt/tape-side-effect-line :stdout "   ")))
    (expect (nil? (prompt/tape-side-effect-line :stderr nil)))))

(defdescribe format-tape-iteration-test
  (it "renders code + result on a clean iteration"
    (let [out (prompt/format-tape-iteration
                {:iteration-position 1 :status :done
                 :code "(def x \"the answer\" 42)"
                 :result 42})]
      (expect (string/includes? out "iteration 1"))
      (expect (string/includes? out "status=done"))
      (expect (string/includes? out "(def x \"the answer\" 42)"))
      (expect (string/includes? out ";; => 42"))))
  (it "omits the => line and shows ERROR when :error is present"
    (let [out (prompt/format-tape-iteration
                {:iteration-position 2 :status :error
                 :code "(/ 1 0)"
                 :result :vis/no-result
                 :error {:message "Divide by zero"}})]
      (expect (string/includes? out "(/ 1 0)"))
      (expect (string/includes? out ";; ! ERROR Divide by zero"))
      (expect (not (string/includes? out ";; =>")))))
  (it "shows TIMEOUT when :timeout? is true"
    (let [out (prompt/format-tape-iteration
                {:iteration-position 3 :status :error
                 :code "(slow)"
                 :result nil
                 :error {:message "Timeout (120s)"}
                 :timeout? true})]
      (expect (string/includes? out ";; ! TIMEOUT Timeout (120s)"))
      (expect (not (string/includes? out ";; =>")))))
  (it "renders stdout + stderr inline above the result"
    (let [out (prompt/format-tape-iteration
                {:iteration-position 4 :status :done
                 :code "(do (println \"hi\") 99)"
                 :result 99
                 :stdout "hi\n"
                 :stderr ""})]
      (expect (string/includes? out ";; ! stdout> hi"))
      (expect (string/includes? out ";; => 99"))
      ;; blank stderr drops out
      (expect (not (string/includes? out "stderr>"))))))

(defdescribe format-system-vars-block-test
  (it "renders names + docstrings, one per line"
    (let [out (prompt/format-system-vars-block
                [{:name "USER_REQUEST" :doc "current turn user request"}
                 {:name "TURN_META"    :doc "turn telemetry"}])]
      (expect (string/starts-with? out ";; system-vars:"))
      (expect (string/includes? out ";;   USER_REQUEST  \"current turn user request\""))
      (expect (string/includes? out ";;   TURN_META  \"turn telemetry\""))))
  (it "returns nil for empty vec"
    (expect (nil? (prompt/format-system-vars-block [])))
    (expect (nil? (prompt/format-system-vars-block nil)))))

(defdescribe format-live-vars-block-test
  (it "renders count and entries; default cap is 30"
    (let [out (prompt/format-live-vars-block
                [{:name "big-handle" :doc "README handle"}
                 {:name "hits"       :doc "TODO grep hits"}])]
      (expect (string/starts-with? out ";; live-vars (2/30):"))
      (expect (string/includes? out ";;   big-handle  \"README handle\""))
      (expect (string/includes? out ";;   hits  \"TODO grep hits\""))))
  (it "honors custom cap"
    (let [out (prompt/format-live-vars-block
                [{:name "x" :doc "x doc"}] 10)]
      (expect (string/starts-with? out ";; live-vars (1/10):"))))
  (it "returns nil for empty vec"
    (expect (nil? (prompt/format-live-vars-block [])))
    (expect (nil? (prompt/format-live-vars-block nil)))))

(defdescribe format-tape-test
  (it "renders one iteration as a single block"
    (let [out (prompt/format-tape
                [{:iteration-position 1 :status :done
                  :code "(def x \"the answer\" 42)"
                  :result 42}])]
      (expect (string/includes? out "iteration 1"))
      (expect (string/includes? out ";; => 42"))))
  (it "renders N-1 + N pair separated by a blank line on error recovery"
    (let [out (prompt/format-tape
                [{:iteration-position 4 :status :done
                  :code "(def big \"handle\" 99)"
                  :result 99}
                 {:iteration-position 5 :status :error
                  :code "(/ 1 0)"
                  :result :vis/no-result
                  :error {:message "Divide by zero"}}])]
      (expect (string/includes? out "iteration 4"))
      (expect (string/includes? out "iteration 5"))
      (expect (string/includes? out "ERROR Divide by zero"))
      (expect (string/includes? out "\n\n;;"))))
  (it "returns nil for empty input"
    (expect (nil? (prompt/format-tape [])))
    (expect (nil? (prompt/format-tape nil)))))

(defdescribe format-user-role-tape-message-test
  (it "joins system-vars + live-vars + tape with blank-line separators"
    (let [out (prompt/format-user-role-tape-message
                {:system-vars [{:name "USER_REQUEST" :doc "current request"}]
                 :live-vars   [{:name "big" :doc "README handle"}]
                 :iters       [{:iteration-position 1 :status :current
                                :code "(def big \"README handle\" (v/cat \"README.md\"))"
                                :result :v.cat-handle}]})]
      (expect (string/includes? out ";; system-vars:"))
      (expect (string/includes? out ";; live-vars (1/30):"))
      (expect (string/includes? out "iteration 1"))
      (expect (string/includes? out "(def big"))))
  (it "omits sections that produce nil"
    (let [out (prompt/format-user-role-tape-message
                {:system-vars []
                 :live-vars [{:name "x" :doc ""}]
                 :iters []})]
      (expect (not (string/includes? out "system-vars:")))
      (expect (string/includes? out ";; live-vars (1/30):"))
      (expect (not (string/includes? out "iteration ")))))
  (it "returns nil when every section is empty"
    (expect (nil? (prompt/format-user-role-tape-message
                    {:system-vars [] :live-vars [] :iters []})))))

(defdescribe iteration->tape-iter-test
  (it "single-block clean iteration becomes a clean tape-iter"
    (let [out (prompt/iteration->tape-iter
                {:position 3
                 :blocks [{:code "(def x \"the answer\" 42)"
                           :result 42}]})]
      (expect (= 3 (:iteration-position out)))
      (expect (= :done (:status out)))
      (expect (= "(def x \"the answer\" 42)" (:code out)))
      (expect (= 42 (:result out)))
      (expect (nil? (:error out)))
      (expect (false? (:timeout? out)))))
  (it "multi-block iteration joins code by newline; result is last successful block"
    (let [out (prompt/iteration->tape-iter
                {:position 4
                 :blocks [{:code "(def a \"a\" 1)" :result 1}
                          {:code "(def b \"b\" 2)" :result 2}
                          {:code "(+ a b)" :result 3}]})]
      (expect (string/includes? (:code out) "(def a"))
      (expect (string/includes? (:code out) "(def b"))
      (expect (string/includes? (:code out) "(+ a b)"))
      (expect (= 3 (:result out)))
      (expect (= :done (:status out)))))
  (it "block error → status :error, :result is :vis/no-result, :error is the first error"
    (let [out (prompt/iteration->tape-iter
                {:position 2
                 :blocks [{:code "(def a \"a\" 1)" :result 1}
                          {:code "(/ 1 0)" :error {:message "Divide by zero"}}
                          {:code "(def b \"b\" 2)" :error {:message "skipped"}}]})]
      (expect (= :error (:status out)))
      (expect (= :vis/no-result (:result out)))
      (expect (= "Divide by zero" (-> out :error :message)))))
  (it "stdout / stderr joined with newlines, blanks dropped"
    (let [out (prompt/iteration->tape-iter
                {:position 1
                 :blocks [{:code "(println :hi)" :result nil :stdout "hi\n" :stderr ""}
                          {:code "(println :bye)" :result nil :stdout "bye\n" :stderr ""}
                          {:code ":done" :result :done :stdout ""}]})]
      (expect (string/includes? (:stdout out) "hi"))
      (expect (string/includes? (:stdout out) "bye"))
      (expect (= "" (:stderr out)))))
  (it "timeout? bubbles up if any block timed out"
    (let [out (prompt/iteration->tape-iter
                {:position 5
                 :blocks [{:code "(slow)" :timeout? true :error {:message "Timeout"}}]})]
      (expect (true? (:timeout? out)))
      (expect (= :error (:status out)))))
  (it "coords overrides supply optional telemetry the iteration row doesn't carry"
    (let [out (prompt/iteration->tape-iter
                {:position 1 :blocks [{:code "42" :result 42}]}
                {:turn-position 7 :conv-id "ab12" :state-id "cd34" :status :current})]
      (expect (= 7 (:turn-position out)))
      (expect (= "ab12" (:conv-id out)))
      (expect (= "cd34" (:state-id out)))
      (expect (= :current (:status out)))))
  (it "renders cleanly through format-tape-iteration"
    (let [iter (prompt/iteration->tape-iter
                 {:position 1
                  :blocks [{:code "(def x \"doc\" 42)" :result 42}]}
                 {:turn-position 1 :conv-id "ab12" :status :done})
          out  (prompt/format-tape-iteration iter)]
      (expect (string/includes? out "iteration 1"))
      (expect (string/includes? out "turn=1"))
      (expect (string/includes? out "conv=ab12"))
      (expect (string/includes? out "(def x \"doc\" 42)"))
      (expect (string/includes? out ";; => 42")))))
