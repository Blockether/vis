(ns com.blockether.vis.internal.foundation.environment.render-test
  (:require [clojure.string :as string]
            [com.blockether.vis.internal.foundation.environment.render :as render]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private base-host {:cwd "/tmp/x" :user "alice" :time "2026-05-05T14:30:00-07:00"})

(defdescribe render-test
             (it "builds project ctx from runtime snapshot with no prompt labels"
                 (let [out
                       (render/project-context {:host base-host :git {:root "/tmp/x"}} nil nil)

                       project
                       (:project out)]

                   (expect (= "/tmp/x" (:root project)))
                   (expect (= "/tmp/x" (get-in project [:host :cwd])))
                   (expect (= "/tmp/x" (get-in project [:git :root])))
                   (expect (not (string/includes? (pr-str out) ";; -- RUNTIME --")))
                   (expect (not (string/includes? (pr-str out) "<environment>")))))
             (it "drops empty optional data"
                 (let [out (render/project-context {:host base-host} {:found? false} [])]
                   (expect (not (contains? (:project out) :guidance)))
                   (expect (not (contains? (:project out) :warnings))))))
