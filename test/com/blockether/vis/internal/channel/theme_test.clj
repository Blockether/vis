(ns com.blockether.vis.internal.channel.theme-test
  (:require [com.blockether.vis.internal.channel.theme :as theme]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]))

(deftest monochrome-themes-test
  (doseq [id ["paper" "high-contrast-dark"]]
    (testing id
      (let [t (get theme/built-in-themes id)
            palette (:palette t)]

        (is (some? t))
        (when t
          (is (theme/valid-theme? t))
          (is (= (set (keys theme/light-palette)) (set (keys palette))))
          (is (every? #{[0 0 0] [255 255 255]} (vals palette)))
          (doseq [[fg bg] [[:text-fg :terminal-bg] [:dialog-fg :dialog-bg]
                           [:dialog-title-fg :dialog-title-bg]
                           [:header-active-tab-fg :header-active-tab-bg] [:button-fg :button-bg]
                           [:answer-fg :answer-bg] [:code-block-fg :code-block-bg]
                           [:dialog-hint :dialog-bg]]]
            (is (not= (get palette fg) (get palette bg))))
          (let [css (theme/theme->web-css-vars t)]
            (is (= (get css "--fg") (get css "--line") (get css "--line2")))
            (is (= (get css "--bg") (get css "--hover")))
            (is (= (get css "--primary") (get css "--ok-surface")))))))))
