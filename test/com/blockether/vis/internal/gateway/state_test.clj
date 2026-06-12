(ns com.blockether.vis.internal.gateway.state-test
  "Wire-event projection. Form errors ship LEAN, single-surface:
   `block.output` carries human text (message + line/col + hint), never the
   pr-str'd op-error map (which nests host trace/data chains), and is omitted
   entirely when an errored op in the form's sink slice already renders the
   same failure as an op card — the web thread painted that failure twice."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.gateway.state :as state]
   [lazytest.core :refer [defdescribe expect it]]))

(def ^:private tool-error
  {:message "rg spec has unknown keys: spec."
   :data {:phase :python/host :type :vis/tool-failure :symbol :rg}})

(defdescribe form-result-error-wire-test
  (it "omits the block-level error when an errored op already carries the same failure"
    (let [[type _ payload] (#'state/chunk->event
                             {:phase :form-result :position 0 :code "rg(...)"
                              :error tool-error
                              :channel [{:position 0 :success? false
                                         :error {:message "rg spec has unknown keys: spec."
                                                 :trace "clojure.lang.ExceptionInfo: ..."}}]})]
      (expect (= "block.output" type))
      (expect (nil? (:error payload)))))
  (it "ships a lean text error (message + line/col + hint), never the pr-str'd map"
    (let [[_ _ payload] (#'state/chunk->event
                          {:phase :form-result :position 0 :code "1/0"
                           :error {:message "ZeroDivisionError: division by zero"
                                   :hint "check denominator"
                                   :data {:phase :python/runtime :line 1 :column 1}}})]
      (expect (= "ZeroDivisionError: division by zero (line 1, col 1)\nhint: check denominator"
                (:error payload)))
      (expect (not (str/includes? (:error payload) ":data")))))
  (it "an errored op with a DIFFERENT message does not swallow the block error"
    (let [[_ _ payload] (#'state/chunk->event
                          {:phase :form-result :position 0 :code "x"
                           :error tool-error
                           :channel [{:position 0 :success? false
                                      :error {:message "some other failure"}}]})]
      (expect (= "rg spec has unknown keys: spec." (:error payload))))))
