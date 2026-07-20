(ns com.blockether.vis.internal.manifest
  "Classpath auto-discovery of vis extension manifests.

   ONE resource per jar: `META-INF/vis-extension/vis.edn`. Each file
   is an EDN map keyed by extension id symbol:

     {git
      {:nses [com.acme.ext.git.core
              com.acme.channel.web.bot
              ...]}}

   `scan-extensions!` is the primitive: it walks every URL, parses
   each map, `require`s every namespace listed under `:nses` exactly
   once across all URLs (so registrar side effects in those namespaces
   fire), and returns the merged manifest map `{<id> {:nses [...]}}`.
   Idempotent and memoized.

   `rediscover!` is a test/REPL helper that drops the cache and
   re-scans."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [taoensso.telemere :as tel]))

(def EXTENSIONS_RESOURCE
  "Classpath path of the per-jar discovery manifest. Resource name
   is preserved across the SDK rename for on-disk back-compat with
   already-shipped extension jars."
  "META-INF/vis-extension/vis.edn")

(defn- normalize-vis-edn
  "Coerce a parsed `vis.edn` payload into the canonical map shape
   `{<id-sym> {:nses [<ns-sym> ...]}}`. Drops malformed entries
   silently; returns `{}` for unrecognized shapes."
  [parsed]
  (when (map? parsed)
    (into {}
          (keep (fn [[id entry]]
                  (when (and (symbol? id) (map? entry))
                    (let [nses (vec (filter symbol? (:nses entry)))]
                      (when (seq nses) [id {:nses nses}])))))
          parsed)))

(defn- merge-manifest-entry
  "Merge two parsed manifest entries for the same id. `:nses` are
   deduped with existing order preserved."
  [existing entry]
  {:nses (vec (distinct (concat (:nses existing) (:nses entry))))})

(defonce ^:private cached-manifests (atom nil))
(defonce ^:private discovered? (atom false))

(defn- truthy-value? [v] (contains? #{"1" "true" "yes" "on"} (str/lower-case (str v))))

(defn- measure-enabled?
  []
  (or (truthy-value? (System/getenv "VIS_MEASURE"))
      (truthy-value? (System/getProperty "vis.measure"))))

(defn- elapsed-ms [^long started-ns] (/ (double (- (System/nanoTime) started-ns)) 1000000.0))

(defn- format-ms [ms] (String/format java.util.Locale/ROOT "%.1f ms" (object-array [(double ms)])))

(defn- measure-line!
  [label & kvs]
  (when (measure-enabled?)
    (binding [*out* *err*]
      (println (str "[vis measure] jvm:manifest "
                    label
                    (when (seq kvs) (str " " (str/join " " (map str kvs)))))))))

(defonce ^:private load-failures-atom
  ;; ::ext-load-failure entries collected during the most recent
  ;; classpath scan. Each entry: {:source :reason :path :extension-id
  ;; :extension-ns :class}. `:source` is the literal keyword
  ;; `:extension-load` so environment warning consumers can
  ;; tag every line with its origin and the user reads "the
  ;; extension-load-failure warnings come from manifest discovery,
  ;; not from runtime features". Public via `load-failures` (read-only); cleared
  ;; on every `scan!`. The atom is the single point that lets us
  ;; surface the failure to TWO consumers - the per-turn
  ;; `(:project ctx) :warnings` slice (so the LLM sees "foundation extension
  ;; failed; cat will be unbound") and the launcher's stderr banner
  ;; (so the user running `bin/vis` notices before they spend an
  ;; iteration loop on phantom errors).
  (atom []))

(defn- scan!
  "One pass: read every vis.edn URL, merge per id, require every
   declared namespace exactly once across all URLs. Returns the
   merged manifest map.

   Side effect: appends every load-failure entry to
   `load-failures-atom` so consumers (`(:project ctx) :warnings`,
   startup launcher banner) can surface them. The atom is
   reset to `[]` on entry so consecutive scans don't compound stale
   warnings."
  []
  (let
    [scan-started
     (System/nanoTime)

     urls
     (try (enumeration-seq (.getResources (.getContextClassLoader (Thread/currentThread))
                                          EXTENSIONS_RESOURCE))
          (catch Exception _ nil))

     merged
     (atom {})

     seen
     (atom #{})]

    (measure-line! "resources" (str "count=" (count urls)))
    (reset! load-failures-atom [])
    (doseq [^java.net.URL url urls]
      (let [url-started (System/nanoTime)]
        (try
          (let
            [content (slurp url)
             parsed (edn/read-string {:readers {}
                                      :default (fn [_ form]
                                                 form)}
                                     content)
             normalized (normalize-vis-edn parsed)]

            (measure-line! "resource parsed"
                           (format-ms (elapsed-ms url-started))
                           (str "url=" url)
                           (str "extensions=" (count normalized)))
            (doseq [[id entry] normalized]
              (swap! merged update id merge-manifest-entry entry)
              (doseq [ns-sym (:nses entry)]
                (when (not (@seen ns-sym))
                  (swap! seen conj ns-sym)
                  (let [require-started (System/nanoTime)]
                    (try (require ns-sym)
                         (tel/log! {:level :info
                                    :id ::discover-extension
                                    :data {:extension-id id :extension-ns ns-sym :source (str url)}
                                    :msg (str "Auto-discovered extension ns '" ns-sym
                                              "' (id " id
                                              ") from " url)})
                         (catch Throwable t
                           (let [msg (or (ex-message t) (str t))]
                             (swap! load-failures-atom conj
                               {:source :extension-load
                                :extension-id id
                                :extension-ns ns-sym
                                :path (str url)
                                :class (.getName (class t))
                                :reason (str "require '" ns-sym
                                             "' threw " (.getSimpleName (class t))
                                             ": " msg)})
                             (tel/log! {:level :error
                                        :id ::discover-extension-failed
                                        :data {:extension-id id
                                               :extension-ns ns-sym
                                               :source (str url)
                                               :class (.getName (class t))
                                               :message msg}
                                        :msg (str "Failed to load extension ns '" ns-sym
                                                  "': " msg)})))
                         (finally (measure-line! "require"
                                                 (format-ms (elapsed-ms require-started))
                                                 (str "id=" id)
                                                 (str "ns=" ns-sym))))))))
            (when (empty? normalized)
              (tel/log! {:level :warn
                         :id ::discover-extension-empty
                         :data {:source (str url)}
                         :msg (str url " parsed but declared no extensions")})))
          (catch Throwable t
            (let [msg (or (ex-message t) (str t))]
              (swap! load-failures-atom conj
                {:source :extension-load
                 :path (str url)
                 :class (.getName (class t))
                 :reason (str "vis.edn parse failed: " msg)})
              (tel/log! {:level :error
                         :id ::discover-extension-parse-failed
                         :data {:source (str url) :message msg}
                         :msg (str "Failed to parse " url ": " msg)}))))))
    (measure-line! "scan total"
                   (format-ms (elapsed-ms scan-started))
                   (str "extensions=" (count @merged))
                   (str "namespaces=" (count @seen)))
    @merged))

(defn scan-extensions!
  "Idempotent classpath scan + namespace requires. Returns the merged
   parsed manifests as `{<id-sym> {:nses [...]}}`. Memoized on first
   success; subsequent calls return the cache.

   Callers that just need the require side effect to drive their own
   registrar (e.g. the persistence facade lazy-discovering backends
   on first connect) can call this directly."
  []
  (if @discovered?
    (do (measure-line! "scan cached" (str "extensions=" (count @cached-manifests)))
        @cached-manifests)
    (let [manifests (scan!)]
      (reset! cached-manifests manifests)
      (reset! discovered? true)
      manifests)))

(defn rediscover!
  "Force a fresh classpath scan, discarding the cached manifests.
   Test/REPL utility - production code should use the idempotent
   `scan-extensions!` instead."
  []
  (reset! discovered? false)
  (reset! cached-manifests nil)
  (scan-extensions!))

(defn load-failures
  "Vec of `{:source :extension-load :extension-id :extension-ns :path
   :class :reason}` maps for every extension namespace whose
   `(require ns)` threw during the most recent classpath scan, plus
   any vis.edn that failed to parse. Empty vec when every namespace
   loaded cleanly.

   The shape of each entry matches `(:project ctx) :warnings` so
   callers can splice it straight into environment context."
  []
  @load-failures-atom)
