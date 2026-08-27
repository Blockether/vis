(ns com.blockether.vis.retired-feature-hygiene-test
  "Retired features stay absent from every tracked path and text file."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private retired-browser-bridge
  (re-pattern (str "(?i)(^|[^a-z0-9])" "sp" "el" "([^a-z0-9]|$)")))

(defn- tracked-paths
  []
  (let [{:keys [exit out err]} (shell/sh "git" "ls-files" "-z")]
    (when-not (zero? (int exit)) (throw (ex-info (str "git ls-files failed: " err) {:exit exit})))
    (->> (str/split out #"\u0000")
         (remove str/blank?)
         (filter #(.isFile (io/file %))))))

(defn- retired-references
  []
  (vec (for [path
             (tracked-paths)

             :let [file
                   (io/file path)

                   text
                   (when (< (.length file) (* 2 1024 1024))
                     (try (slurp file) (catch Exception _ "")))]
             :when (or (re-find retired-browser-bridge path)
                       (and text (re-find retired-browser-bridge text)))]

         path)))

(defdescribe retired-feature-hygiene-test
             (it "keeps the retired browser bridge out of tracked paths and content"
                 (when (.exists (io/file "deps.edn"))
                   (let [found (retired-references)]
                     (expect (empty? found)
                             (str "retired browser bridge references remain:\n"
                                  (str/join "\n" found)))))))
