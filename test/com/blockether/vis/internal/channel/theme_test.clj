(ns com.blockether.vis.internal.channel.theme-test
  (:require [com.blockether.vis.internal.channel.theme :as theme]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe monochrome-themes-test
             (doseq [id ["paper" "high-contrast-dark"]]
               (it id
                   (let [t (get theme/built-in-themes id)
                         palette (:palette t)]

                     (expect (some? t))
                     (when t
                       (expect (theme/valid-theme? t))
                       (expect (= (set (keys theme/light-palette)) (set (keys palette))))
                       (expect (every? #{[0 0 0] [255 255 255]} (vals palette)))
                       (doseq [[fg bg] [[:text-fg :terminal-bg] [:dialog-fg :dialog-bg]
                                        [:dialog-title-fg :dialog-title-bg]
                                        [:header-active-tab-fg :header-active-tab-bg]
                                        [:button-fg :button-bg] [:answer-fg :answer-bg]
                                        [:code-block-fg :code-block-bg] [:dialog-hint :dialog-bg]]]
                         (expect (not= (get palette fg) (get palette bg))))
                       (let [css (theme/theme->web-css-vars t)]
                         (expect (= (get css "--fg") (get css "--line") (get css "--line2")))
                         (expect (= (get css "--bg") (get css "--hover")))
                         (expect (= (get css "--primary") (get css "--ok-surface")))))))))
