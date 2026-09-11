(ns com.blockether.vis.tui.docs-screenshots-test
  "Reproducible documentation captures of production TUI panes, with example data.
   Call write-screenshots! with the documentation assets directory to regenerate."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.tui.capture :as cap]
            [com.blockether.vis.tui.human-input :as hi]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.live-view :as lv]
            [com.blockether.vis.tui.live-view-fixture :as fixture]
            [com.blockether.vis.tui.view-materializer :as live]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.googlecode.lanterna.screen TerminalScreen]))

(defn- running-pane
  []
  (-> (fixture/view
        {:title "CI · run 42" :description "3 jobs"}
        (fixture/status "run" "Watching" {:tone :running})
        (fixture/progress "progress" {:done 1 :total 3})
        (fixture/table "jobs"
                       [(fixture/table-column "job" "Job") (fixture/table-column "state" "Status")]
                       {:rows [(fixture/table-row "1" ["Tests" "in_progress"] {:tone :running})
                               (fixture/table-row "2" ["Lint" "success"] {:tone :ok})
                               (fixture/table-row "3" ["Build" "queued"] {:tone :idle})]})
        {:id "links"
         :type :link
         :links
         [{:id "run" :label "This run" :target "https://github.com/Blockether/vis/actions"}]})
      live/materialize
      (assoc :id "docs-live"
             :seq 0
             :created-at 0)
      lv/opened))

(defn- ask-form
  []
  (-> (hi/init-form {:id "docs-ask"
                     :title "Deploy"
                     :description "Pick a target"
                     :fields [{:id "env"
                               :type :select
                               :label "Target"
                               :is-required true
                               :description "Deployment environment."
                               :options [{:value "staging" :label "staging"}
                                         {:value "prod" :label "prod"}]}
                              {:id "notes" :type :multiline :label "Release notes"}
                              {:id "token" :type :password :label "Deploy token" :is-secret true}]
                     :submit-label "Submit"
                     :cancel-label "Cancel"
                     :is-cancellable true})
      (assoc :values {"env" "staging" "notes" "Update the documentation" "token" "example-only"})))

(defn- capture-pane
  [kind]
  (let [pane
        (running-pane)

        pane
        (if (= kind :live-stop)
          (reduce (fn [p ch]
                    (:pane (lv/typed p {:kind :char :char ch})))
                  (lv/armed pane)
                  "Check the failing job first")
          pane)]

    (cap/capture! {:cols 80
                   :rows 36
                   :paint! (fn [{:keys [screen]}]
                             (.beginFrame interactions/hit-map)
                             (let [g (.newTextGraphics ^TerminalScreen screen)]
                               (if (= kind :ask)
                                 (hi/paint! g 80 36 (ask-form))
                                 (lv/paint! g 80 36 [pane] 1 3 12000))
                               (.commitFrame interactions/hit-map)
                               (.refresh ^TerminalScreen screen)))})))

(defn write-screenshots!
  "Write the three documentation PNGs to an explicitly supplied directory."
  [out-dir]
  (mapv (fn [kind]
          (let [capture (capture-pane kind)]
            (when-let [error (:error capture)]
              (throw error))
            (cap/shot! {:grid (last (:frames capture))
                        :out (str (io/file out-dir (str (name kind) ".png")))
                        :font-size 18})))
        [:ask :live-running :live-stop]))

(defdescribe
  documentation-captures-test
  (it "renders the documented controls and masks the example password"
      (doseq [[kind labels]
              [[:ask ["Deploy" "Target" "staging" "Release notes" "Deploy token" "Submit"]]
               [:live-running
                ["CI · run 42" "3 jobs" "Watching" "Tests" "Lint" "Build" "in_progress" "This run"]]
               [:live-stop
                ["CI · run 42" "3 jobs" "Watching" "Tests" "Lint" "Build" "This run"
                 "Check the failing job first"]]]

              :let [capture
                    (capture-pane kind)

                    text
                    (cap/frame-text (last (:frames capture)))]]

        (expect (nil? (:error capture)))
        (doseq [label labels]
          (expect (str/includes? text label) (str kind ": " label)))
        (expect (not (str/includes? text "example-only"))))))
