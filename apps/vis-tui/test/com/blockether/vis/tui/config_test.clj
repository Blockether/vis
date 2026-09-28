(ns com.blockether.vis.tui.config-test
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [com.blockether.vis.tui.config :as config]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defdescribe
  runtime-home-preferences-test
  (it "runtime home preferences"
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
               (expect (= "tokyonight-night" (get loaded "theme_name")))
               (expect (= marker (get loaded "test_marker")))
               ;; Never let a broken path resolver write to the actual user's store.
               (when (= marker (get loaded "test_marker"))
                 (config/update! #(assoc %
                                    "theme_name" "vis-dark"
                                    "show_python_code" false))
                 (config/save-toggles! {"reasoning_level" "deep"})
                 (let [saved (json/read-json (slurp config-file))]
                   (expect (= "vis-dark" (get saved "theme_name")))
                   (expect (false? (get saved "show_python_code")))
                   (expect (= {"reasoning_level" "deep"} (get saved "toggles")))
                   (expect (= saved (config/load-raw))))))
             (finally (System/setProperty "user.home" original-home)
                      (doseq [file (reverse (file-seq home))]
                        (io/delete-file file true)))))))

(def ^:private no-provider-body
  ;; Exactly what the gateway puts on the wire for a 503: a hyphenated type
  ;; nested under "error" (packages/vis-contract .../contract/gateway.clj).
  {"error" {"type" "no-provider" "message" "make-router requires at least one provider"}})

(defn- client-ex
  "The exception the TUI gateway client throws for an HTTP error response."
  [status body]
  (ex-info (get-in body ["error" "message"] "request failed")
           (assoc body
             :http-status status
             :vis/user-error true)))

(defdescribe
  no-provider-ex-test
  (it "no provider ex"
      ;; Regression: the startup 503 must be recognised here, or the TUI reports
      ;; "make-router requires at least one provider" and exits instead of opening
      ;; the Providers dialog.
      (expect (true? (config/no-provider-ex (client-ex 503 no-provider-body))))
      (expect (true? (config/no-provider-ex
                       (ex-info "Could not start session" {} (client-ex 503 no-provider-body)))))
      (expect (false? (config/no-provider-ex
                        (client-ex 500 {"error" {"type" "engine-error" "message" "boom"}}))))
      (expect (false? (config/no-provider-ex (ex-info "no data" {}))))
      (expect (false? (config/no-provider-ex nil)))))
