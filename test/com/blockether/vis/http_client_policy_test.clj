(ns com.blockether.vis.http-client-policy-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]))

(defn- source-roots
  []
  (cons (io/file "src")
        (->> (file-seq (io/file "extensions"))
             (filter #(and (.isDirectory ^java.io.File %) (= "src" (.getName ^java.io.File %))))
             sort)))

(defn- clojure-sources
  []
  (->> (mapcat file-seq (source-roots))
       (filter #(and (.isFile ^java.io.File %) (str/ends-with? (.getName ^java.io.File %) ".clj")))
       sort))

(deftest outbound-http-uses-babashka-http-client-test
  (testing "production Clojure never bypasses babashka.http-client with a direct JDK HTTP client"
    (let
      [offenders (->> (clojure-sources)
                      (keep (fn [file]
                              (let [source (slurp file)]
                                (when (or (str/includes? source "java.net.http")
                                          (str/includes? source "java.net.URLConnection")
                                          (str/includes? source "java.net.HttpURLConnection"))
                                  (.getPath ^java.io.File file)))))
                      vec)]
      (is (= [] offenders) (str "Direct JDK HTTP clients found in: " (str/join ", " offenders))))))
