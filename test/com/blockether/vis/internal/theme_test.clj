(ns com.blockether.vis.internal.theme-test
  (:require [com.blockether.vis.internal.theme :as theme]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe theme-data-test
  (it "defines valid channel-neutral light and dark themes"
    (expect (true? (theme/valid-theme? theme/vis-light)))
    (expect (true? (theme/valid-theme? theme/vis-dark)))
    (expect (= "vis-light" (:name theme/default-theme)))
    (expect (= [255 255 255] (theme/color theme/vis-light :terminal-bg)))
    (expect (= [12 14 18] (theme/color theme/vis-dark :terminal-bg)))
    (expect (= [150 100 0] (theme/color :md-h1-fg))))

  (it "exposes colors, fonts, palette, widths, spacing, and extension settings"
    (expect (contains? theme/default-theme :palette))
    (expect (contains? theme/default-theme :fonts))
    (expect (contains? theme/default-theme :widths))
    (expect (contains? theme/default-theme :spacing))
    (expect (= "0px" (get-in theme/default-theme [:settings "PADDING"])))
    (expect (= 1 (get-in theme/default-theme [:spacing :pad-x])))
    (expect (= 24 (get-in theme/default-theme [:widths :dialog-min-height])))
    (expect (= [255 255 252] (get-in theme/default-theme [:palette :input-field-bg]))))

  (it "keeps the process theme registry in an atom"
    (expect (instance? clojure.lang.IDeref theme/themes))
    (expect (= ["vis-dark" "vis-light"] (theme/available-theme-ids))))

  (it "accepts extension :ext/theme maps with string theme names and string setting keys"
    (expect (true? (theme/extension-theme-map?
                     {"THEME_NAME" {"PADDING" "0px"
                                    "FONT_FAMILY" "monospace"}}))))

  (it "registers extension-declared theme ids into the atom-backed registry"
    (try
      (theme/register-themes! {"THEME_NAME" {"PADDING" "0px"}})
      (expect (= ["THEME_NAME" "vis-dark" "vis-light"]
                (theme/available-theme-ids)))
      (expect (= "0px" (get-in (theme/theme "THEME_NAME") [:settings "PADDING"])))
      (finally
        (theme/reset-themes!)))))
