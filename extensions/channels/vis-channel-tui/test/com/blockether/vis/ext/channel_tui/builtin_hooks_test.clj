(ns com.blockether.vis.ext.channel-tui.builtin-hooks-test
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.builtin-hooks :as builtin-hooks]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe builtin-hooks-test
             (it "exposes footer contributions without registering a standalone extension"
                 (expect (= [:tui.slot/footer-segment] (keys builtin-hooks/channel-contributions)))
                 (expect (= [:tui.builtin.model/footer]
                            (mapv :id
                                  (:tui.slot/footer-segment builtin-hooks/channel-contributions)))))
             (it "renders C-x c as the footer model-picker shortcut"
                 (let
                   [render-fn
                    (-> builtin-hooks/channel-contributions
                        :tui.slot/footer-segment
                        first
                        :fn
                        deref)

                    segments
                    (render-fn {:session-model-pref {:provider "openai-codex" :model "gpt-5.5"}}
                               0)

                    label
                    (get-in (first segments) [:ast 2 2 2])]

                   (expect (str/includes? label "(C-x c)")))))
