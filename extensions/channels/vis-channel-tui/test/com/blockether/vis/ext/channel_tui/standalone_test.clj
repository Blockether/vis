(ns com.blockether.vis.ext.channel-tui.standalone-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.ext.channel-tui.standalone :as standalone]
            [lazytest.core :refer [defdescribe it expect]])
  (:import [com.googlecode.lanterna.terminal.swing SwingTerminalFontConfiguration]))

(def ^:private terminal-font-configuration
  (deref #'standalone/terminal-font-configuration))

(defdescribe standalone-options-test
  (it "keeps the standalone backend conditional and pure before Swing startup"
    (expect (= {:title       "Vis"
                :columns     120
                :rows         36
                :font-size    16
                :font-bundle  :mono-nf
                :pixel-width  1200
                :pixel-height 800
                :maximized    false}
              (standalone/standalone-options {}))))

  (it "accepts Cascadia bundle, maximized, and initial grid overrides"
    (expect (= {:title       "Vis"
                :columns     140
                :rows         42
                :font-size    18
                :font-bundle  :code
                :pixel-width  1600
                :pixel-height 900
                :maximized    true}
              (standalone/standalone-options
                {:columns     140
                 :rows        42
                 :font-size    18
                 :font-bundle  :code
                 :pixel-width  1600
                 :pixel-height 900
                 :maximized    true
                 :font-family "Ignored non-Cascadia system font"
                 :font-path   "/tmp/ignored-custom.ttf"}))))

  (it "builds the Swing font configuration type required by SwingTerminalFrame"
    (expect (instance? SwingTerminalFontConfiguration
              (terminal-font-configuration (standalone/standalone-options {})))))

  (it "exposes the bundled Cascadia Mono NF font resource"
    (expect (= "fonts/cascadia/CascadiaMonoNF.ttf"
              standalone/bundled-cascadia-resource)))

  (it "bundles all Cascadia code and mono variants with italics"
    (expect (= #{:code :code-pl :code-nf :mono :mono-pl :mono-nf}
              (set (keys standalone/bundled-cascadia-fonts))))
    (expect (every? #(= 2 (count %))
              (vals standalone/bundled-cascadia-fonts)))
    (expect (every? some?
              (for [entry (mapcat identity (vals standalone/bundled-cascadia-fonts))]
                (io/resource (:resource entry)))))))
