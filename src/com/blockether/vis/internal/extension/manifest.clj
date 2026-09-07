(ns com.blockether.vis.internal.extension.manifest
  "The ordered registration list for Vis' built-in modules, not a user extension loader.

   `META-INF/vis/manifest.edn` has one key, `:initialization`. Each entry is a
   qualified registration symbol, or `{:register qualified.ns/register!
   :apropos \"META-INF/vis/apropos/docs.edn\"}` when the module owns static docs.
   Keeping documents beside their owner preserves discovery order.

   Every built-in is required. Registration runs once in dependency order; a failure
   stops initialization rather than leaving a partial engine. Successful entries are
   remembered so a retry does not register them again. User extensions are Python
   files, loaded separately by `internal.python.extensions`.

   Nothing scans the classpath and there is no alternate manifest format."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.util :as util]))

(def manifest-resource "META-INF/vis/manifest.edn")

(defn- qualified-var-symbol?
  [x]
  (and (symbol? x) (not (str/blank? (namespace x))) (not (str/blank? (name x)))))

(defn- resource-path?
  [x]
  (and (util/non-blank-string? x)
       (not (str/starts-with? x "/"))
       (not (str/ends-with? x "/"))
       (not (str/includes? x "\\"))
       (not-any? #{"" "." ".."} (str/split x #"/"))))

(defn- entry?
  [x]
  (and (map? x)
       (every? #{:register :apropos} (keys x))
       (qualified-var-symbol? (:register x))
       (or (not (contains? x :apropos)) (resource-path? (:apropos x)))))

(defn manifest?
  [x]
  (and (map? x)
       (= #{:initialization} (set (keys x)))
       (vector? (:initialization x))
       (seq (:initialization x))
       (= (count (:initialization x)) (count (set (:initialization x))))
       (every? #(or (qualified-var-symbol? %) (entry? %)) (:initialization x))))

(defn- read-edn
  "Read manifest EDN `text`, refusing tagged literals - the one reader."
  [source text]
  (try (edn/read-string {:readers {}
                         :default (fn [tag value]
                                    (throw (ex-info "Tagged literal is not allowed"
                                                    {:tag tag :value value})))}
                        text)
       (catch Throwable t
         (throw (ex-info (str "Invalid EDN in distribution resource " (pr-str source))
                         {:type :manifest/invalid-edn :resource source}
                         t)))))

(defn- read-resource
  [path]
  (let [url (or (io/resource path)
                (throw (ex-info (str "Missing distribution resource " (pr-str path))
                                {:type :manifest/missing-resource :resource path})))]
    (read-edn path (slurp url))))

(defn- validated
  [source m]
  (if (manifest? m)
    m
    (throw (ex-info "Invalid distribution manifest"
                    {:type :manifest/invalid :resource source :explain {:valid false :value m}}))))

(defn- normalized-entries [m] (mapv #(if (map? %) % {:register %}) (:initialization m)))

(defonce ^:private parsed-manifest
  (delay (validated manifest-resource (read-resource manifest-resource))))

(defn read-manifest "Read and validate the single distribution manifest." [] @parsed-manifest)

(defn entries
  "Every initialization entry as a map, in manifest order: `:register` always, plus
   `:apropos` when the entry declared its static documents."
  []
  (normalized-entries (read-manifest)))

(defn parse
  "Validate manifest EDN `text`, named by `source`, and answer its normalized entries."
  [source text]
  (normalized-entries (validated source (read-edn source text))))

(defn initializers
  "The qualified symbol of every initializer, in manifest order."
  []
  (mapv :register (entries)))

(defn apropos-resource-paths
  "Every static document resource the manifest names, in manifest order. The order
   is load-bearing: it is the order `apropos` answers in, so the first record to
   claim a name is the one that keeps it."
  []
  (into [] (keep :apropos) (entries)))

(defn read-apropos-resources
  "Read every declared apropos EDN value in manifest order."
  []
  (mapv read-resource (apropos-resource-paths)))

(defonce ^:private state
  ;; Remember only success, not a cached delay exception or unavailable modules.
  (atom #{}))

(defn- run-initializer!
  "Load the namespace, resolve the Var, call it. nil on success, else the failure:
   `:load` (the namespace), `:resolve` (the Var) or `:invoke` (the call itself)."
  [sym]
  (let [f (try (requiring-resolve sym)
               (catch Throwable t {:phase :load :error (or (ex-message t) (str t))}))]
    (cond (map? f) f
          (nil? f) {:phase :resolve :error "initializer does not exist"}
          (not (ifn? f)) {:phase :resolve :error "initializer is not callable"}
          :else
          (try (f) nil (catch Throwable t {:phase :invoke :error (or (ex-message t) (str t))})))))

(defn initialize-entries!
  "Initialize `entries` into the set in `state-atom`, once each and in order.
   A failure stops the walk; a retry resumes after the successful entries."
  [state-atom entries]
  (locking state-atom
    (doseq [{:keys [register]}
            entries

            :when (not (contains? @state-atom register))]

      (if-let [failure (run-initializer! register)]
        (throw (ex-info (str "Required initializer failed: " register)
                        (assoc failure
                          :type :manifest/initializer-failed
                          :initializer register)))
        (swap! state-atom conj register)))
    {:initialized (count @state-atom)}))

(defn initialize!
  "Register every built-in in manifest order; return `{:initialized n}`.
   Repeated or concurrent calls never repeat a successful registration."
  []
  (initialize-entries! state (entries)))
