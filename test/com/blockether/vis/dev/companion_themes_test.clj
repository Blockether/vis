(ns com.blockether.vis.dev.companion-themes-test
  "The companion's shipped theme assets are GENERATED, so they are only ever as
   true as their last generation. These tests are the drift gate: change a
   palette in `theme.clj` without rerunning `clojure -X:companion-themes` and
   the suite says so, in the file that is now the only source of a phone's
   colours."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.dev.companion-themes :as companion-themes]
            [com.blockether.vis.internal.channel.theme :as theme]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]))

(defn- generated [file-name] (slurp (io/file companion-themes/default-dir file-name)))

(deftest companion-theme-assets-are-in-sync-with-the-clojure-themes
  (testing "the shipped stylesheet is exactly what `theme.clj` renders today"
    (is (= (companion-themes/stylesheet) (generated companion-themes/stylesheet-file-name))
        "run `clojure -X:companion-themes`"))
  (testing "so is the catalog module"
    (is (= (companion-themes/catalog-module) (generated companion-themes/catalog-file-name))
        "run `clojure -X:companion-themes`")))

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

(deftest code-text-clears-aa-on-its-rendered-background
  ;; CodeCopy exposed these small-text pairs on Tokyo Day's darker code surfaces.
  (doseq [[id {:keys [palette]}]
          theme/built-in-themes

          [foreground background]
          [[:code-syntax-special-fg :code-block-bg] [:code-success-fg :code-ok-bg]]]

    (is (<= 4.5 (contrast-ratio (get palette foreground) (get palette background)))
        (str id " " foreground " on " background))))

(deftest all-tokyonight-styles-ship-in-the-application-catalog
  ;; Regression, user report: the first adapter collapsed TokyoNight's application planes
  ;; onto nearly identical paint and left Tokyo Day's body copy needlessly faint.
  (let [expected {"tokyonight-day" {:mode :light :bg "#e1e2e7" :surface "#d0d5e3" :fg "#243b73"}
                  "tokyonight-moon" {:mode :dark :bg "#222436" :surface "#191b29" :fg "#c8d3f5"}
                  "tokyonight-night" {:mode :dark :bg "#1a1b26" :surface "#0c0e14" :fg "#c0caf5"}
                  "tokyonight-storm" {:mode :dark :bg "#24283b" :surface "#1b1e2d" :fg "#c0caf5"}}]
    (doseq [[id {:keys [mode bg surface fg]}] expected]
      (let [theme-map (get theme/built-in-themes id)
            css-vars (theme/theme->web-css-vars theme-map)]

        (is (some? theme-map) id)
        (is (= mode (:mode theme-map)) id)
        (is (= bg (get css-vars "--bg")) id)
        (is (= surface (get css-vars "--surface")) id)
        (is (= fg (get css-vars "--fg")) id)))))

(deftest every-built-in-theme-is-paintable-without-a-gateway
  (let [css
        (companion-themes/stylesheet)

        catalog
        (companion-themes/catalog-module)]

    (testing "each built-in palette has its own `data-theme` block and catalog row"
      (doseq [id (keys theme/built-in-themes)]
        (is (str/includes? css (str "[data-theme='" id "'] {")) id)
        (is (str/includes? catalog (str "id: '" id "'")) id)))
    (testing "the default theme also paints `:root`, so the first frame needs no preference"
      (is (str/includes? css (str ":root,\n[data-theme='" theme/default-theme-id "'] {"))))
    (testing "every block carries the palette's colours and its own colour scheme"
      (doseq [[id theme-map] theme/built-in-themes]
        (is (str/includes? css
                           (str "  --bg: " (get (theme/theme->web-css-vars theme-map) "--bg") ";"))
            id)
        (is (str/includes? css (str "  color-scheme: " (name (:mode theme-map)) ";")) id)))))
