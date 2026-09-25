(ns com.blockether.vis.namespace-layering-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.extension :as ext]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]])
  (:import [java.io File PushbackReader]))

(def ^:private facade 'com.blockether.vis.core)

(def ^:private runtime-facade-lookup
  "A quoted facade reference handed to a runtime loader, e.g.
   `(requiring-resolve 'com.blockether.vis.core/current-config)`."
  #"\((?:requiring-resolve|resolve|ns-resolve|find-ns|the-ns|require)\s+'\[?com\.blockether\.vis\.core[\s/\]\)]")

(defn- internal-sources
  []
  (->> (file-seq (io/file "src/com/blockether/vis/internal"))
       (filter #(and (.isFile ^File %) (str/ends-with? (.getName ^File %) ".clj")))
       sort))

(defn- ns-form
  [^File file]
  (with-open [reader (PushbackReader. (io/reader file))]
    (binding [*read-eval* false]
      (read reader))))

(defn- libspec-names
  "The namespaces one `:require` argument names, prefix lists expanded."
  [spec]
  (cond (symbol? spec) [spec]
        (and (sequential? spec) (symbol? (first spec)))
        (let [[head & more] spec]
          (if (or (empty? more) (keyword? (first more)))
            [head]
            (for [child more
                  :let [child-name (if (sequential? child) (first child) child)]]

              (symbol (str head "." child-name)))))
        :else []))

(defn- required-namespaces
  [form]
  (for [clause
        (rest form)

        :when (and (seq? clause) (= :require (first clause)))
        spec
        (rest clause)

        ns-sym
        (libspec-names spec)]

    ns-sym))

(deftest internal-namespaces-never-load-the-facade-test
  (testing "engine namespaces call owners or the authoring API, never the public facade"
    (let [offenders (->> (internal-sources)
                         (keep (fn [^File file]
                                 (when (or (some #{facade} (required-namespaces (ns-form file)))
                                           (re-find runtime-facade-lookup (slurp file)))
                                   (.getPath file))))
                         vec)]
      (is (= [] offenders)
          (str "Internal namespaces loading " facade ": " (str/join ", " offenders))))))

(def ^:private internal-prefix "com.blockether.vis.internal.")

(def ^:private layers
  "Engine layers, lowest first. A namespace belongs to the layer of its longest
   matching prefix under `com.blockether.vis.internal`; `*.cli` namespaces are
   command-line entrypoints. A namespace requires its own layer or lower ones only:
   a lower layer that must call a higher one exposes a slot the wiring fills."
  '[;; Leaf primitives any layer may use.
    [base
     [util error import paths content format parse-diagnose system-trust jfr activity
      config.toggles]]
    ;; The session store and the attachment blobs it persists.
    [persistance [persistance attachment.core attachment.storage attachment.image-convert]]
    ;; Merged configuration and the catalogs it reads.
    [config
     [config extension.registry extension.manifest provider.catalog provider.credential-command
      provider.error session.cancellation workspace.core]]
    ;; Domain services.
    [services
     [extension provider workspace session channel sandbox attachment external-opener context python
      speech decisions docs council view gateway.client gateway.resources gateway.runtime
      gateway.discovery gateway.diagnostics foundation.harness.discovery foundation.mpl-capture
      foundation.shell-log]]
    ;; Built-in extensions over the services.
    [extensions [foundation python.extensions python.test-runner]]
    ;; The agent loop and its improvement workflow.
    [loop [loop improve]]
    ;; The gateway daemon, including the wiring that fills upward slots.
    [gateway [gateway]]
    ;; Process entrypoints.
    [entrypoints [main doctor commandline]]])

(def ^:private prefix-layers
  (sort-by (comp - count first)
           (for [[layer [_ prefixes]]
                 (map-indexed vector layers)

                 prefix
                 prefixes]

             [(str prefix) layer])))

(defn- layer-of
  [ns-sym]
  (let [relative (subs (str ns-sym) (count internal-prefix))]
    (if (= "cli" (peek (str/split relative #"\.")))
      (dec (count layers))
      (some (fn [[prefix layer]]
              (when (or (= relative prefix) (str/starts-with? relative (str prefix "."))) layer))
            prefix-layers))))

(defn- layer-name [layer] (first (nth layers layer)))

(deftest every-engine-namespace-has-a-layer-test
  (let [unplaced (->> (internal-sources)
                      (map (comp second ns-form))
                      (remove layer-of)
                      vec)]
    (is (= [] unplaced) (str "Place these namespaces in `layers`: " (str/join ", " unplaced)))))

(deftest engine-namespaces-never-require-a-higher-layer-test
  (testing "a static require points to the requiring namespace's layer or a lower one"
    (let [upward
          (vec
            (for [file (internal-sources)
                  :let [form (ns-form file)
                        own (second form)
                        own-layer (layer-of own)]
                  dep (required-namespaces form)
                  :let [dep-layer (when (str/starts-with? (str dep) internal-prefix)
                                    (layer-of dep))]
                  :when (and own-layer dep-layer (> dep-layer own-layer))]

              (str own " (" (layer-name own-layer) ") -> " dep " (" (layer-name dep-layer) ")")))]
      (is (= [] upward) (str "Upward requires:\n" (str/join "\n" upward))))))

(def ^:private runtime-lookup
  "A quoted engine namespace handed to a runtime loader."
  #"\((?:requiring-resolve|resolve|ns-resolve|find-ns|the-ns|require)\s+'\[?(com\.blockether\.vis\.internal\.[\w.\-]+?)[\s/\]\)]")

(def ^:private lazily-loaded
  "Engine namespaces loaded on first use: optional JFR and speech machinery."
  '#{com.blockether.vis.internal.jfr com.blockether.vis.internal.speech.engine
     com.blockether.vis.internal.speech.synthesis})

(deftest engine-namespaces-require-instead-of-resolving-at-runtime-test
  (testing "a runtime lookup hides a dependency from the require graph, so only lazy loads use one"
    (let [lookups (vec (for [^File file (internal-sources)
                             [_ target] (re-seq runtime-lookup (slurp file))
                             :when (not (contains? lazily-loaded (symbol target)))]

                         (str (.getPath file) " -> " target)))]
      (is (= [] lookups) (str "Runtime lookups:\n" (str/join "\n" lookups))))))

(def ^:private wiring 'com.blockether.vis.internal.gateway.wiring)

(def ^:private slots
  "Lower-layer slots that hold a higher layer's function."
  '#{com.blockether.vis.internal.council.core/install-runtime!
     com.blockether.vis.internal.council.core/install-waker!
     com.blockether.vis.internal.gateway.bus/set-deliver-fn!
     com.blockether.vis.internal.gateway.bus/set-relevant-sid-fn!
     com.blockether.vis.internal.gateway.bus/set-relevant-sids-fn!
     com.blockether.vis.internal.provider.service/set-router-rebuild-hook!
     com.blockether.vis.internal.python.env/install-extension-hooks!
     com.blockether.vis.internal.python.extensions/install-test-slash!
     com.blockether.vis.internal.session.agents/install-runtime!
     com.blockether.vis.internal.view.core/set-late-artifact-filer!})

(defn- aliases
  [form]
  (into {}
        (for [clause
              (rest form)

              :when (and (seq? clause) (= :require (first clause)))
              spec
              (rest clause)

              :when (and (vector? spec) (symbol? (first spec)) (keyword? (second spec)))
              :let [{:keys [as as-alias]}
                    (apply hash-map (rest spec))]
              :when (or as as-alias)]

          [(or as as-alias) (first spec)])))

(deftest only-the-wiring-fills-slots-test
  (testing "slots are filled once at startup by the wiring, never at namespace load"
    (let [fills (vec (for [^File file (internal-sources)
                           :let [form (ns-form file)
                                 own (second form)
                                 alias->ns (aliases form)]
                           :when (not= wiring own)
                           [_ qualifier fn-name] (re-seq #"\(([A-Za-z][\w.\-]*)/([^\s()\[\]{}]+)"
                                                         (slurp file))
                           :let [target (symbol (str (get alias->ns (symbol qualifier) qualifier))
                                                fn-name)]
                           :when (contains? slots target)]

                       (str own " -> " target)))]
      (is (= [] fills) (str "Slot fills outside " wiring ":\n" (str/join "\n" fills))))))

(def ^:private authored
  (ext/extension {:ext/name "test.layering-authoring"
                  :ext/description "Authoring API fixture."
                  :ext/prompt-fn "Probe."}))

(def ^:private facade-authored
  (vis/extension
    {:ext/name "test.layering-facade" :ext/description "Facade fixture." :ext/prompt-fn "Probe."}))

(deftest facade-re-exports-the-authoring-api-test
  (testing "every authoring function is the same value through either namespace"
    (doseq [sym '[symbol value render-prompt register-extension! register-toggle!]]
      (is (identical? @(ns-resolve 'com.blockether.vis.extension sym) @(ns-resolve facade sym))
          (str sym))))
  (testing "both `extension` macros stamp the namespace that declared the extension"
    (is (= '[com.blockether.vis.namespace-layering-test] (:ext/source-nses authored)))
    (is (= '[com.blockether.vis.namespace-layering-test] (:ext/source-nses facade-authored)))))
