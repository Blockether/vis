(ns com.blockether.vis.tui.config-test
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [com.blockether.vis.tui.config :as config]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(deftest runtime-home-preferences-test
  ;; Native-image initializes this namespace in the builder. The preferences
  ;; path must be resolved when read or written, not when the namespace loads.
  (let [home
        (.toFile (Files/createTempDirectory "vis-tui-theme-" (make-array FileAttribute 0)))

        original-home
        (System/getProperty "user.home")

        config-file
        (io/file home ".vis" "tui" "config.json")

        marker
        (str (random-uuid))]

    (try (io/make-parents config-file)
         (spit config-file
               (json/write-json-str {"theme_name" "tokyonight-night" "test_marker" marker}))
         (System/setProperty "user.home" (.getAbsolutePath home))
         (let [loaded (config/load-raw)]
           (is (= "tokyonight-night" (get loaded "theme_name")))
           (is (= marker (get loaded "test_marker")))
           ;; Never let a broken path resolver write to the actual user's store.
           (when (= marker (get loaded "test_marker"))
             (config/update! #(assoc %
                                "theme_name" "vis-dark"
                                "show_python_code" false))
             (config/save-toggles! {"reasoning_level" "deep"})
             (let [saved (json/read-json (slurp config-file))]
               (is (= "vis-dark" (get saved "theme_name")))
               (is (false? (get saved "show_python_code")))
               (is (= {"reasoning_level" "deep"} (get saved "toggles")))
               (is (= saved (config/load-raw))))))
         (finally (System/setProperty "user.home" original-home)
                  (doseq [file (reverse (file-seq home))]
                    (io/delete-file file true))))))
