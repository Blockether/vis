(ns com.blockether.vis.tui.config
  "Process-local configuration for the standalone terminal application.

   Gateway-owned settings and provider credentials never enter this file. It stores
   only client preferences, as JSON, under ~/.vis/tui/config.json."
  (:require [charred.api :as json]
            [clojure.java.io :as io]))

(defn- config-file
  "Resolve the runtime user's preferences path, never the native-image builder's."
  ^java.io.File []
  (io/file (System/getProperty "user.home") ".vis" "tui" "config.json"))

(def ^:private config-lock (Object.))

(defn- read-file
  [^java.io.File file]
  (if (.isFile file)
    (try (let [value (json/read-json (slurp file))]
           (if (map? value) value {}))
         (catch Throwable _ {}))
    {}))

(defn load-raw
  "Read the app-local, string-keyed JSON configuration."
  []
  (locking config-lock (read-file (config-file))))

(defn update!
  "Atomically replace app-local configuration with `(f current)`."
  [f]
  (locking config-lock
    (let [^java.io.File file
          (config-file)

          next-value
          (or (f (read-file file)) {})

          ^java.io.File parent
          (.getParentFile file)

          ^java.io.File temp
          (io/file parent (str ".config-" (random-uuid) ".json"))]

      (when-not (map? next-value)
        (throw (ex-info "TUI configuration update must return a map" {:type :tui/invalid-config})))
      (.mkdirs parent)
      (spit temp (str (json/write-json-str next-value :indent-str "  ") "
"))
      (java.nio.file.Files/move (.toPath temp)
                                (.toPath file)
                                (into-array java.nio.file.CopyOption
                                            [java.nio.file.StandardCopyOption/REPLACE_EXISTING
                                             java.nio.file.StandardCopyOption/ATOMIC_MOVE]))
      next-value)))

(defn save-toggles! [snapshot] (update! #(assoc % "toggles" snapshot)))

(defn no-provider-ex
  [error]
  (let [data (ex-data error)]
    (or (= :provider/no-provider (:type data))
        (= :vis/no-provider (:type data))
        (= "no_provider" (get data "type")))))
