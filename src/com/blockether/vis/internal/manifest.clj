(ns com.blockether.vis.internal.manifest
  "The one closed distribution manifest.

   `META-INF/vis/manifest.edn` has exactly two keys:

     {:version 1
      :initialization [qualified.ns/register!
                       {:register    qualified.ns/register!
                        :apropos     \"META-INF/vis/apropos/shim-pandas.edn\"
                        :is-optional true
                        :because     \"sherpa-onnx may be absent from this build\"}
                       ...]}

   An entry is a BARE SYMBOL when it has nothing else to say. A map adds what that
   pack owns: `:apropos` names the one static EDN resource carrying its documents,
   so a resource is never declared far from the code that registers it and deleting
   a pack takes its documents with it.

   Initialization order is dependency order and every initializer runs at most once.
   A REQUIRED initializer that fails THROWS: a distribution that cannot build itself
   is a build defect, not a fact about this machine, and a half-registered engine
   that looks alive is worse than a loud death. `:is-optional true` says the
   opposite - that pack may be missing from THIS machine - so its failure is logged,
   recorded in `:failed` and stepped over; it must carry `:because`, because a
   weakness nobody explained is indistinguishable from a forgotten line.

   Nothing scans the classpath and there is no alternate manifest format."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [taoensso.telemere :as tel]))

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
(s/def ::register qualified-var-symbol?)
(s/def ::apropos resource-path?)
(s/def ::is-optional true?)
(s/def ::because (s/and string? (complement str/blank?)))
(s/def ::entry
  (s/and (s/keys :req-un [::register] :opt-un [::apropos ::is-optional ::because])
         #(every? #{:register :apropos :is-optional :because} (keys %))
         ;; An optional pack states WHY it may be absent, and nothing else may.
         #(= (contains? % :is-optional) (contains? % :because))))
(s/def ::initialization
  (s/and (s/coll-of (s/or :required qualified-var-symbol?
                          :declared ::entry)
                    :kind vector?
                    :distinct true)
         seq))
(s/def ::manifest
  (s/and (s/keys :req-un [::version ::initialization])
         #(= #{:version :initialization} (set (keys %)))))

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
  (if (s/valid? ::manifest m)
    m
    (throw (ex-info
             "Invalid distribution manifest"
             {:type :manifest/invalid :resource source :explain (s/explain-data ::manifest m)}))))

(defn- normalized-entries [m] (mapv #(if (map? %) % {:register %}) (:initialization m)))

(defonce ^:private parsed-manifest
  (delay (validated manifest-resource (read-resource manifest-resource))))

(defn read-manifest "Read and validate the single distribution manifest." [] @parsed-manifest)

(defn entries
  "Every initialization entry as a map, in manifest order: `:register` always, plus
   `:apropos`, `:is-optional` and `:because` when the entry declared them."
  []
  (normalized-entries (read-manifest)))

(defn parse
  "Validate manifest EDN `text`, named by `source`, and answer its entries. THE
   parser: `build.clj` derives native reachability from the manifest it just copied
   into the class directory by calling this, so a build and a runtime cannot disagree
   about the shape, the refusal or the error."
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
  ;; `{:initialized #{sym} :failed {sym {:phase :error :because}}}`, an ATOM and
  ;; never `(defonce _ (delay ...))`: a delay that threw caches the THROW and answers
  ;; it for the life of the JVM, so one transient failure would be permanent and no
  ;; reload could undo it. Only SUCCESS is remembered here.
  (atom {:initialized #{} :failed {}}))

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
  "Initialize `entries` into `state-atom` once each - the seam `initialize!` and its
   test share, and the only place that decides what a failure means."
  [state-atom entries]
  (doseq [{:keys [register is-optional because]}
          entries

          :let [{:keys [initialized failed]}
                @state-atom]
          :when (not (or (contains? initialized register) (contains? failed register)))]

    (if-let [failure (run-initializer! register)]
      (if is-optional
        (do (tel/log! {:level :warn
                       :id ::pack-unavailable
                       :msg (str "Pack unavailable: " register)
                       :data (assoc failure
                               :initializer register
                               :because because)})
            (swap! state-atom assoc-in [:failed register] (assoc failure :because because)))
        (throw (ex-info (str "Required initializer failed: " register)
                        (assoc failure
                          :type :manifest/initializer-failed
                          :initializer register))))
      (swap! state-atom update :initialized conj register)))
  (let [{:keys [initialized failed]} @state-atom]
    {:initialized (count initialized)
     :failed (mapv (fn [[sym failure]]
                     (assoc failure :initializer sym))
                   failed)}))

(defn initialize!
  "Initialize the closed distribution, in manifest order, and answer what stands:

     {:initialized 42
      :failed [{:initializer com.../register! :phase :load :error \"...\" :because \"...\"}]}

   Idempotent and NON-RETRYING. Nine call sites reach this function, so retrying a
   namespace that cannot load would pay its full load every time; an optional pack
   that failed stays failed for the life of the process, and that is the honest
   answer anyway - the code in a native image cannot change under a running process."
  []
  (initialize-entries! state (entries)))

(defn failures
  "What is NOT part of this process: one map per optional pack that could not be
   initialized, carrying `:phase`, `:error` and the `:because` it declared."
  []
  (mapv (fn [[sym failure]]
          (assoc failure :initializer sym))
        (:failed @state)))
