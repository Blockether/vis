(ns com.blockether.vis.dev.companion-themes-test
  "The companion's shipped theme assets are GENERATED, so they are only ever as
   true as their last generation. These tests are the drift gate: change a
   palette in `theme.clj` without rerunning `clojure -X:companion-themes` and
   the suite says so, in the file that is now the only source of a phone's
   colours."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.dev.companion-themes :as companion-themes]
            [com.blockether.vis.internal.theme :as theme]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]))

(defn- generated [file-name] (slurp (io/file companion-themes/default-dir file-name)))

(deftest companion-theme-assets-are-in-sync-with-the-clojure-themes
  (testing "the shipped stylesheet is exactly what `theme.clj` renders today"
    (is (= (companion-themes/stylesheet) (generated companion-themes/stylesheet-file-name))
        "run `clojure -X:companion-themes`"))
  (testing "so is the catalog module"
    (is (= (companion-themes/catalog-module) (generated companion-themes/catalog-file-name))
        "run `clojure -X:companion-themes`")))

(deftest every-built-in-theme-is-paintable-without-a-gateway
  (let
    [css
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
