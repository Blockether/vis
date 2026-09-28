(ns com.blockether.vis.dev.companion-themes-test
  "The companion's shipped theme assets are GENERATED, so they are only ever as
   true as their last generation. These tests are the drift gate: change a
   palette in `theme.clj` without rerunning `clojure -X:companion-themes` and
   the suite says so, in the file that is now the only source of a phone's
   colours."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
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

(defdescribe
  session-group-colours-ship-from-the-backend
  (it "session group colours ship from the backend"
      ;; The companion used to keep its own copy of these hues beside the gateway's tokens.
      ;; Both halves now come from the backend, so a token cannot ship without a colour.
      ;; every token the gateway accepts has exactly one hue, and no hue is orphaned
      (expect (= (set gateway-contract/session-group-colors)
                 (set (keys theme/session-group-swatches))))
      (expect (every? theme/rgb? (vals theme/session-group-swatches)))
      (expect (apply distinct? (vals theme/session-group-swatches)))
      (let [css
            (companion-themes/stylesheet)

            module
            (companion-themes/group-colors-module)]

        ;; the stylesheet defines each hue once, shared by every theme
        (doseq [token gateway-contract/session-group-colors]
          (expect (= [(str "  --color-group-"
                           token
                           ": "
                           (theme/rgb->css (get theme/session-group-swatches token))
                           ";")]
                     (re-seq (re-pattern (str "(?m)^  --color-group-" token ": .*$")) css))
                  token))
        ;; the module offers the contract's tokens in its order, with its default
        (expect (str/includes? module
                               (str "export const GROUP_COLORS = [\n"
                                    (str/join (map (fn [token]
                                                     (str "  '" token "',\n"))
                                                   gateway-contract/session-group-colors))
                                    "] as const;")))
        (expect (str/includes? module
                               (str "export const DEFAULT_GROUP_COLOR: SessionGroupColor = '"
                                    gateway-contract/default-session-group-color
                                    "';")))
        (doseq [token gateway-contract/session-group-colors]
          (expect (str/includes? module (str "  " token ": 'bg-group-" token "',\n")) token)))))

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

(defdescribe
  all-tokyonight-styles-ship-in-the-application-catalog
  (it "all tokyonight styles ship in the application catalog"
      ;; Regression, user report: the first adapter collapsed TokyoNight's application planes
      ;; onto nearly identical paint and left Tokyo Day's body copy needlessly faint.
      (let [expected
            {"tokyonight-day" {:mode :light :bg "#e1e2e7" :surface "#d0d5e3" :fg "#243b73"}
             "tokyonight-moon" {:mode :dark :bg "#222436" :surface "#191b29" :fg "#c8d3f5"}
             "tokyonight-night" {:mode :dark :bg "#1a1b26" :surface "#0c0e14" :fg "#c0caf5"}
             "tokyonight-storm" {:mode :dark :bg "#24283b" :surface "#1b1e2d" :fg "#c0caf5"}}]
        (doseq [[id {:keys [mode bg surface fg]}] expected]
          (let [theme-map (get theme/built-in-themes id)
                css-vars (theme/theme->web-css-vars theme-map)]

            (expect (some? theme-map) id)
            (expect (= mode (:mode theme-map)) id)
            (expect (= bg (get css-vars "--bg")) id)
            (expect (= surface (get css-vars "--surface")) id)
            (expect (= fg (get css-vars "--fg")) id))))))

(defdescribe
  every-built-in-theme-is-paintable-without-a-gateway
  (it "every built in theme is paintable without a gateway"
      (let [css
            (companion-themes/stylesheet)

            catalog
            (companion-themes/catalog-module)]

        ;; each built-in palette has its own `data-theme` block and catalog row
        (doseq [id (keys theme/built-in-themes)]
          (expect (str/includes? css (str "[data-theme='" id "'] {")) id)
          (expect (str/includes? catalog (str "id: '" id "'")) id))
        ;; the default theme also paints `:root`, so the first frame needs no preference
        (expect (str/includes? css (str ":root,\n[data-theme='" theme/default-theme-id "'] {")))
        ;; every block carries the palette's colours and its own colour scheme
        (doseq [[id theme-map] theme/built-in-themes]
          (expect (str/includes?
                    css
                    (str "  --bg: " (get (theme/theme->web-css-vars theme-map) "--bg") ";"))
                  id)
          (expect (str/includes? css (str "  color-scheme: " (name (:mode theme-map)) ";")) id)))))
