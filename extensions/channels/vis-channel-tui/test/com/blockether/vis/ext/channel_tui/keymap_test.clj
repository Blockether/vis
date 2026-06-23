(ns com.blockether.vis.ext.channel-tui.keymap-test
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.keymap :as keymap]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe chord-label-test
  (it "single letters upper-case; named keys pass through"
    (with-redefs [keymap/mac? (delay false)]
      (expect (= "Alt+H" (keymap/chord \h)))
      (expect (= "Alt+G" (keymap/chord \g)))
      (expect (= "Alt+Enter" (keymap/chord "Enter")))))
  (it "macOS renders the Option glyph instead of Alt"
    (with-redefs [keymap/mac? (delay true)]
      (expect (= "⌥H" (keymap/chord \h)))
      (expect (= "⌥Enter" (keymap/chord "Enter")))
      (expect (= "⌥" (keymap/alt-prefix)))))
  (it "non-macOS uses the Alt+ prefix"
    (with-redefs [keymap/mac? (delay false)]
      (expect (= "Alt+" (keymap/alt-prefix))))))

(defdescribe dispatch-table-test
  (it "action-for resolves the migrated F-key actions, case-insensitively"
    (expect (= :toggle-help    (keymap/action-for \h)))
    (expect (= :toggle-help    (keymap/action-for \H)))
    (expect (= :search-open    (keymap/action-for \g)))
    (expect (= :open-resources (keymap/action-for \j)))
    (expect (= :show-palette   (keymap/action-for \x)))
    (expect (= :show-sessions  (keymap/action-for \s))))
  (it "action-for returns nil for unbound chars (so dispatch falls through)"
    (expect (nil? (keymap/action-for \z)))
    (expect (nil? (keymap/action-for nil))))
  (it "label-for round-trips an action to its platform chord"
    (with-redefs [keymap/mac? (delay true)]
      (expect (= "⌥J" (keymap/label-for :open-resources)))
      (expect (= "⌥H" (keymap/label-for :toggle-help))))
    (with-redefs [keymap/mac? (delay false)]
      (expect (= "Alt+G" (keymap/label-for :search-open))))
    (expect (nil? (keymap/label-for :no-such-action))))
  (it "bindings have unique chord letters (no shadowing)"
    (let [keys (map :key keymap/bindings)]
      (expect (= (count keys) (count (distinct keys)))))))
