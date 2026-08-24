(ns com.blockether.vis.internal.manifest
  "The one closed distribution manifest.

   `META-INF/vis/manifest.edn` has exactly three keys:

     {:version 1
      :initialization [qualified.ns/register! ...]
      :apropos [META-INF/vis/apropos/docs.edn ...]}

   Initialization order is dependency order. Each listed function is resolved and
   invoked exactly once by `initialize!`. `:apropos` is the complete ordered list
   of static document resources; nothing scans the classpath and there is no
   alternate manifest format."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(def manifest-resource "META-INF/vis/manifest.edn")

(defn- qualified-var-symbol?
  [x]
  (and (symbol? x) (not (str/blank? (namespace x))) (not (str/blank? (name x)))))

(defn- resource-path?
  [x]
  (and (string? x)
       (not (str/blank? x))
       (not (str/starts-with? x "/"))
       (not (str/ends-with? x "/"))
       (not (str/includes? x "\\"))
       (not-any? #{"" "." ".."} (str/split x #"/"))))

(s/def ::version #{1})
(s/def ::initialization (s/and (s/coll-of qualified-var-symbol? :kind vector? :distinct true) seq))
(s/def ::apropos (s/and (s/coll-of resource-path? :kind vector? :distinct true) seq))
(s/def ::manifest
  (s/and (s/keys :req-un [::version ::initialization ::apropos])
         #(= #{:version :initialization :apropos} (set (keys %)))))

(defn- read-resource
  [path]
  (let [url (or (io/resource path)
                (throw (ex-info (str "Missing distribution resource " (pr-str path))
                                {:type :manifest/missing-resource :resource path})))]
    (try (edn/read-string {:readers {}
                           :default (fn [tag value]
                                      (throw (ex-info "Tagged literal is not allowed"
                                                      {:tag tag :value value})))}
                          (slurp url))
         (catch Throwable t
           (throw (ex-info (str "Invalid EDN in distribution resource " (pr-str path))
                           {:type :manifest/invalid-edn :resource path}
                           t))))))

(defonce ^:private parsed-manifest
  (delay (let [m (read-resource manifest-resource)]
           (when-not (s/valid? ::manifest m)
             (throw (ex-info "Invalid distribution manifest"
                             {:type :manifest/invalid
                              :resource manifest-resource
                              :explain (s/explain-data ::manifest m)})))
           m)))

(defn read-manifest "Read and validate the single distribution manifest." [] @parsed-manifest)

(defn initialize-manifest!
  "Resolve and invoke every initializer in manifest order. Returns their count."
  [m]
  (when-not (s/valid? ::manifest m)
    (throw (ex-info "Invalid distribution manifest"
                    {:type :manifest/invalid :explain (s/explain-data ::manifest m)})))
  (doseq [initializer (:initialization m)]
    (let [f (or (requiring-resolve initializer)
                (throw (ex-info (str "Initializer does not exist: " initializer)
                                {:type :manifest/missing-initializer :initializer initializer})))]
      (when-not (ifn? f)
        (throw (ex-info (str "Initializer is not callable: " initializer)
                        {:type :manifest/non-callable-initializer :initializer initializer})))
      (f)))
  (count (:initialization m)))

(defonce ^:private initialized (delay (initialize-manifest! (read-manifest))))

(defn initialize! "Initialize the closed distribution once, in manifest order." [] @initialized)

(defn apropos-resource-paths
  "The ordered static document resources declared by the manifest."
  []
  (:apropos (read-manifest)))

(defn read-apropos-resources
  "Read every declared apropos EDN value in manifest order."
  []
  (mapv read-resource (apropos-resource-paths)))
