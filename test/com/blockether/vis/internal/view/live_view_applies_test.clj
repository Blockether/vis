;; The engine and the recorder of the Python SDK applied a patch in two ways, so one
;; patch landed in one host and was refused, or refused with another text, in the
;; other (#453). Both hosts apply every `applies` case of
;; `vis-contract/fixtures/live-view-checks.json` and must give its answer.
(ns com.blockether.vis.internal.view.live-view-applies-test
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.view.core :as core]
            [com.blockether.vis.internal.view.materializer :as live]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private fixture
  (delay (json/read-json (slurp (io/resource "vis-contract/fixtures/live-view-checks.json")))))

(defn- applied
  "The answer of the engine for case `c`: `{\"refusal\" text}` or the wire value at the
   `at` path of the patched view."
  [c]
  (try (let [patched (binding [core/*live-owner* (when-let [owner (get c "owner")]
                                                   {:extension owner})]
                       (let [view (live/materialize (core/normalize-live-view (wire/->engine
                                                                                (get c "view"))))]
                         (wire/->wire (live/apply-patch
                                        view
                                        (core/landed-patch view (wire/->engine (get c "ops")))))))]
         {"at" (get-in c ["expect" "at"])
          "value" (get-in patched
                          (mapv #(if (string? %) (or (parse-long %) %) %)
                                (get-in c ["expect" "at"])))})
       (catch clojure.lang.ExceptionInfo e {"refusal" (ex-message e)})))

(defdescribe live-view-applies-test
             (it "gives the answer of each shared apply case"
                 (expect (< 10 (count (get @fixture "applies"))))
                 (doseq [c (get @fixture "applies")]
                   (expect (= (get c "expect") (applied c)) (get c "name")))))
