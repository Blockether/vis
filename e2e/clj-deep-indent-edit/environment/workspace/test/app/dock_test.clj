(ns app.dock-test
  (:require [app.dock :as dock]
            [lazytest.core :refer [defdescribe describe expect it]]))

(defdescribe
  dock-summary-test
  (describe "the one-line summary"
            (it "counts each status in a fixed order"
                (doseq [[rows want]
                        [[[] ""]
                         [[{"status" "completed"}] "1 DONE"]
                         [[{"status" "running"}
                           {"status" "running"}
                           {"status" "queued"}
                           {"status" "completed"}]
                          "2 RUNNING · 1 QUEUED · 1 DONE"]
                         [[{"status" "running"}
                           {"status" "running"}
                           {"status" "running"}
                           {"status" "completed"}]
                          "3 RUNNING · 1 DONE"]
                         [[{"status" "completed"}
                           {"status" "failed"}
                           {"status" "failed"}
                           {"status" "failed"}
                           {"status" "failed"}]
                          "1 DONE · 4 FAILED"]]]
                  (expect (= want (dock/summary rows)) (pr-str rows))))
            (describe "inside the dock line"
                      (it "follows the title and the elapsed time"
                          (let [line (dock/line "Agents"
                                                64000
                                                [{"status" "running"}
                                                 {"status" "running"}
                                                 {"status" "running"}
                                                 {"status" "queued"}])]
                            (expect (= "Agents   1m 04s   3 RUNNING · 1 QUEUED"
                                       line)))))))
