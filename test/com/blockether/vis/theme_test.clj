(ns com.blockether.vis.theme-test
  (:require [com.blockether.vis.theme :as theme]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe theme-data-test
  (it "defines a valid channel-neutral default theme"
    (expect (true? (theme/valid-theme? theme/default-theme)))
    (expect (= "vis-light" (:name theme/default-theme)))
    (expect (= [255 255 255] (theme/color :terminal-bg)))
    (expect (= [150 100 0] (theme/color :md-h1-fg))))

  (it "exposes colors, fonts, palette, widths, spacing, and extension settings"
    (expect (contains? theme/default-theme :palette))
    (expect (contains? theme/default-theme :fonts))
    (expect (contains? theme/default-theme :widths))
    (expect (contains? theme/default-theme :spacing))
    (expect (= "0px" (get-in theme/default-theme [:settings "PADDING"])))
    (expect (= 1 (get-in theme/default-theme [:spacing :pad-x]))))

  (it "accepts extension :ext/theme maps with string theme names and string setting keys"
    (expect (true? (theme/extension-theme-map?
                     {"THEME_NAME" {"PADDING" "0px"
                                    "FONT_FAMILY" "monospace"}}))))

  (it "merges extension-declared theme ids into available themes"
    (expect (= ["THEME_NAME" "vis-light"]
              (theme/available-theme-ids
                [{:ext/theme {"THEME_NAME" {"PADDING" "0px"}}}])))))
