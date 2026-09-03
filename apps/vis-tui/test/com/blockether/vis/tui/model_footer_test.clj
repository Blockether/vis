(ns com.blockether.vis.tui.model-footer-test
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.model-footer :as model-footer]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe model-footer-test
             (it "renders the model picker shortcut"
                 (let [segments
                       (model-footer/segments {:session-model-pref {:provider "openai-codex"
                                                                    :model "gpt-5.5"}}
                                              0)

                       label
                       (get-in (first segments) [:ast 2 2 2])]

                   (expect (str/includes? label "(C-x c)")))))
