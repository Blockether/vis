(ns com.blockether.vis.ext.channel-tui.standalone-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.ext.channel-tui.standalone :as standalone]
            [lazytest.core :refer [defdescribe it expect]]))

(defdescribe standalone-options-test
  (it "keeps the standalone backend conditional and pure before Swing startup"
    (expect (= {:title       "Vis"
                :columns     120
                :rows        36
                :font-size   16
                :font-bundle :mono-pl
                :font-family nil
                :font-path   nil}
              (standalone/standalone-options {}))))

  (it "accepts CLI-selected font and initial grid overrides"
    (expect (= {:title       "Vis"
                :columns     140
                :rows        42
                :font-size   18
                :font-bundle :code
                :font-family "Cascadia Mono PL, Menlo, Monospaced"
                :font-path   "/tmp/custom.ttf"}
              (standalone/standalone-options
                {:columns     140
                 :rows        42
                 :font-size   18
                 :font-bundle :code
                 :font-family "Cascadia Mono PL, Menlo, Monospaced"
                 :font-path   "/tmp/custom.ttf"}))))

  (it "exposes the bundled Cascadia Mono PL font resource"
    (expect (= "fonts/cascadia/CascadiaMonoPL.ttf"
              standalone/bundled-cascadia-resource)))

  (it "bundles all Cascadia code and mono variants with italics"
    (expect (= #{:code :code-pl :code-nf :mono :mono-pl :mono-nf}
              (set (keys standalone/bundled-cascadia-fonts))))
    (expect (every? #(= 2 (count %))
              (vals standalone/bundled-cascadia-fonts)))
    (expect (every? some?
              (for [entry (mapcat identity (vals standalone/bundled-cascadia-fonts))]
                (io/resource (:resource entry)))))))
