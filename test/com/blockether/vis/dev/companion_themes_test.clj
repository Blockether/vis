(ns com.blockether.vis.dev.companion-themes-test
  "The companion's shipped theme assets are GENERATED, so they are only ever as
   true as their last generation. These tests are the drift gate: change a
   palette in `theme.clj` without rerunning `clojure -X:companion-themes` and
   the suite says so, in the file that is now the only source of a phone's
   colours."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.contract.gateway :as gateway-contract]
            [com.blockether.vis.dev.companion-themes :as companion-themes]
            [com.blockether.vis.internal.channel.theme :as theme]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- generated [file-name] (slurp (io/file companion-themes/default-dir file-name)))

(defdescribe
  companion-theme-assets-are-in-sync-with-the-clojure-themes
  (it "the shipped stylesheet is exactly what `theme.clj` renders today"
      (expect (= (companion-themes/stylesheet) (generated companion-themes/stylesheet-file-name))
              "run `clojure -X:companion-themes`"))
  (it "so is the catalog module"
      (expect (= (companion-themes/catalog-module) (generated companion-themes/catalog-file-name))
              "run `clojure -X:companion-themes`"))
  (it "and the session-group palette module"
      (expect (= (companion-themes/group-colors-module)
                 (generated companion-themes/group-colors-file-name))
              "run `clojure -X:companion-themes`")))

(defdescribe session-group-colours-ship-from-the-backend
             (it "session group colours ship from the backend"
                 ;; The companion used to keep its own copy of these hues beside the gateway's tokens.
                 ;; Both halves now come from the backend, so a token cannot ship without a colour.
                 ;; every token the gateway accepts has exactly one hue, and no hue is orphaned
                 (expect (= (set gateway-contract/session-group-colors)
                            (set (keys gateway-contract/session-group-swatches))))
                 (expect (every? theme/rgb? (vals gateway-contract/session-group-swatches)))
                 (expect (apply distinct? (vals gateway-contract/session-group-swatches)))))

(defn- contrast-ratio
  [foreground background]
  (let [linear
        (fn [component]
          (let [c (/ (double component) 255.0)]
            (if (<= c 0.04045) (/ c 12.92) (Math/pow (/ (+ c 0.055) 1.055) 2.4))))

        luminance
        (fn [rgb]
          (reduce + (map * [0.2126 0.7152 0.0722] (map linear rgb))))

        a
        (double (luminance foreground))

        b
        (double (luminance background))]

    (/ (+ (max a b) 0.05) (+ (min a b) 0.05))))

(defdescribe code-text-clears-aa-on-its-rendered-background
             (it "code text clears aa on its rendered background"
                 ;; CodeCopy exposed these small-text pairs on Tokyo Day's darker code surfaces.
                 (doseq [[id {:keys [palette]}]
                         theme/built-in-themes

                         [foreground background]
                         [[:code-syntax-special-fg :code-block-bg] [:code-success-fg :code-ok-bg]]]

                   (expect (<= 4.5
                               (contrast-ratio (get palette foreground) (get palette background)))
                           (str id " " foreground " on " background)))))
