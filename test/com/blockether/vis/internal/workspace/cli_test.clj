(ns com.blockether.vis.internal.workspace.cli-test
  (:require [com.blockether.vis.internal.workspace.cli :as workspace-cli]
            [lazytest.core :refer [defdescribe expect it]]))

;; `vis-agent sessions delete` has always existed; there was no way at all to
;; remove a PROJECT and the conversations in it, so the blast radius is opt-in
;; (`--with-sessions`) and the default stays the scatter delete the schema has
;; always had.

(def ^:private match-projects @#'workspace-cli/match-projects)

(defdescribe
  project-id-resolution-test
  (let [projects [{:id "9f2c1a44-0000-0000-0000-000000000001" :name "vis"}
                  {:id "9f2c1a44-0000-0000-0000-000000000002" :name "demo"}
                  {:id "b1000000-0000-0000-0000-000000000003" :name "svar"}]]
    (it "resolves a full id, and an unambiguous prefix"
        (expect (= ["svar"] (mapv :name (match-projects projects "b1000000"))))
        (expect (= ["vis"]
                   (mapv :name (match-projects projects "9F2C1A44-0000-0000-0000-000000000001")))))
    (it "reports EVERY candidate for an ambiguous prefix instead of picking one"
        ;; Deleting a project is irreversible: a prefix that fits two projects
        ;; must never silently resolve to the first one.
        (expect (= ["vis" "demo"] (mapv :name (match-projects projects "9f2c")))))))
