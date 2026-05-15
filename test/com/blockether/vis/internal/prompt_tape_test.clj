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
