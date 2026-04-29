(ns com.blockether.vis.internal.manifest
  "Classpath auto-discovery of vis extension manifests.

   ONE resource per jar: `META-INF/vis-extension/vis.edn`. Each file
   is an EDN map keyed by extension id symbol:

     {git
      {:nses [com.acme.ext.git.core
              com.acme.channel.web.bot
              ...]
       :docs {\"README.md\" {:created-at #inst \"...\"
                              :description   \"...\"
                              :content \"...\"
                              :links       [...]}}}}

   `scan-extensions!` is the primitive: it walks every URL, parses
   each map, normalizes the doc descriptors (rejecting entries that
   miss `:description` or `:content`), `require`s every namespace listed
   under `:nses` exactly once across all URLs (so registrar side
   effects in those namespaces fire), and returns the merged manifest
   map `{<id> {:nses [...] :docs {...}}}`. Idempotent and memoized.

   `rediscover!` is a test/REPL helper that drops the cache and
   re-scans.

   The docs-registry merge (which inverts authored `:links` into
   `:reflinks` on target descriptors and exposes
   `(meta/extension-doc id name)` etc.) lives one layer up in
   `com.blockether.vis.internal.extension`. This namespace knows
   nothing about reflinks; it just produces parsed-and-normalized
   manifests."
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [taoensso.telemere :as tel]))

(def EXTENSIONS_RESOURCE
  "Classpath path of the per-jar discovery manifest. Resource name
   is preserved across the SDK rename for on-disk back-compat with
   already-shipped extension jars."
  "META-INF/vis-extension/vis.edn")

(defn- non-blank-string? [x]
  (and (string? x) (not (str/blank? x))))

(defn- valid-link? [link]
  (and (map? link)
    (or (and (symbol? (:to-id link)) (string? (:to-doc link)))
      (string? (:to-doc link))
      (string? (:url link))
      (string? (:file link)))))

(defn- normalize-doc-descriptor
  "Validate one `[doc-name descriptor]` pair from a vis.edn `:docs`
   map. Returns the descriptor with empty defaults filled in, or
   `nil` when the entry is malformed (missing :description, missing
   :content, etc.). Logs the rejection reason at `:warn`."
  [doc-name descriptor]
  (cond
    (not (string? doc-name))
    (do (tel/log! {:level :warn :id ::doc-bad-name
                   :data {:doc-name doc-name}
                   :msg  (str "Doc name must be a string, got " (pr-str doc-name))})
      nil)

    (not (map? descriptor))
    (do (tel/log! {:level :warn :id ::doc-bad-shape
                   :data {:doc-name doc-name :type (some-> descriptor class .getName)}
                   :msg  (str "Doc descriptor must be a map: " doc-name)})
      nil)

    (not (non-blank-string? (:description descriptor)))
    (do (tel/log! {:level :warn :id ::doc-missing-description
                   :data {:doc-name doc-name}
                   :msg  (str "Doc " doc-name " missing required :description string")})
      nil)

    (not (non-blank-string? (:content descriptor)))
    (do (tel/log! {:level :warn :id ::doc-missing-content
                   :data {:doc-name doc-name}
                   :msg  (str "Doc " doc-name " missing required :content string")})
      nil)

    :else
    {:created-at  (:created-at descriptor)
     :description    (:description descriptor)
     :content (:content descriptor)
     :links       (vec (filter valid-link? (:links descriptor)))
     :reflinks    []}))

(defn- normalize-vis-edn
  "Coerce a parsed `vis.edn` payload into the canonical map shape
   `{<id-sym> {:nses [<ns-sym> ...] :docs {<doc-name> <descriptor>}}}`.
   Drops malformed entries silently; returns `{}` for unrecognized
   shapes."
  [parsed]
  (when (map? parsed)
    (into {}
      (keep (fn [[id entry]]
              (when (and (symbol? id) (map? entry))
                (let [nses (vec (filter symbol? (:nses entry)))
                      docs (when (map? (:docs entry))
                             (into {}
                               (keep (fn [[doc-name descriptor]]
                                       (when-let [norm (normalize-doc-descriptor doc-name descriptor)]
                                         [doc-name norm])))
                               (:docs entry)))]
                  (when (seq nses)
                    [id {:nses nses :docs (or docs {})}])))))
      parsed)))

(defn- merge-manifest-entry
  "Merge two parsed manifest entries for the same id. `:nses` are
   deduped (existing order preserved); `:docs` is a map merge with
   later entries winning per name."
  [existing entry]
  {:nses (vec (distinct (concat (:nses existing) (:nses entry))))
   :docs (merge (or (:docs existing) {}) (or (:docs entry) {}))})

(defonce ^:private cached-manifests (atom nil))
(defonce ^:private discovered? (atom false))

(defn- scan!
  "One pass: read every vis.edn URL, merge per id, require every
   declared namespace exactly once across all URLs. Returns the
   merged manifest map."
  []
  (let [urls   (try
                 (enumeration-seq
                   (.getResources
                     (.getContextClassLoader (Thread/currentThread))
                     EXTENSIONS_RESOURCE))
                 (catch Exception _ nil))
        merged (atom {})
        seen   (atom #{})]
    (doseq [^java.net.URL url urls]
      (try
        (let [content    (slurp url)
              parsed     (edn/read-string {:readers {} :default (fn [_ form] form)} content)
              normalized (normalize-vis-edn parsed)]
          (doseq [[id entry] normalized]
            (swap! merged update id merge-manifest-entry entry)
            (doseq [ns-sym (:nses entry)]
              (when (not (@seen ns-sym))
                (swap! seen conj ns-sym)
                (try
                  (require ns-sym)
                  (tel/log! {:level :info :id ::discover-extension
                             :data  {:extension-id id
                                     :extension-ns ns-sym
                                     :source (str url)}
                             :msg   (str "Auto-discovered extension ns '"
                                      ns-sym "' (id " id ") from " url)})
                  (catch Throwable t
                    (tel/log! {:level :error :id ::discover-extension-failed
                               :data  {:extension-id id
                                       :extension-ns ns-sym
                                       :source (str url)
                                       :class (.getName (class t))
                                       :message (ex-message t)}
                               :msg   (str "Failed to load extension ns '"
                                        ns-sym "': " (ex-message t))}))))))
          (when (empty? normalized)
            (tel/log! {:level :warn :id ::discover-extension-empty
                       :data {:source (str url)}
                       :msg  (str url " parsed but declared no extensions")})))
        (catch Throwable t
          (tel/log! {:level :error :id ::discover-extension-parse-failed
                     :data  {:source (str url) :message (ex-message t)}
                     :msg   (str "Failed to parse " url ": " (ex-message t))}))))
    @merged))

(defn scan-extensions!
  "Idempotent classpath scan + namespace requires. Returns the merged
   parsed manifests as `{<id-sym> {:nses [...] :docs {...}}}`. Memoized
   on first success; subsequent calls return the cache.

   Callers that want the docs-registry side effect (reflinks
   computation + the `(meta/extension-doc ...)` index) should call
   `com.blockether.vis.internal.extension/discover-extensions!`
   instead — which wraps this primitive with the docs merge.

   Callers that just need the require side effect to drive their own
   registrar (e.g. the persistence facade lazy-discovering backends
   on first connect) can call this directly and skip the docs work."
  []
  (if @discovered?
    @cached-manifests
    (let [manifests (scan!)]
      (reset! cached-manifests manifests)
      (reset! discovered? true)
      manifests)))

(defn rediscover!
  "Force a fresh classpath scan, discarding the cached manifests.
   Test/REPL utility — production code should use the idempotent
   `scan-extensions!` instead."
  []
  (reset! discovered? false)
  (reset! cached-manifests nil)
  (scan-extensions!))
