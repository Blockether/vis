(ns com.blockether.vis.ext.bridge.core
  "Bridge extension entry point."
  (:require
   [clojure.java.io :as io]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.bridge.doctor :as doctor]
   [com.blockether.vis.ext.bridge.languages.clojure :as clj]
   [com.blockether.vis.ext.bridge.languages.registry :as languages]
   [com.blockether.vis.ext.bridge.languages.schema :as schema]
   [com.blockether.vis.internal.workspace :as workspace])
  (:import
   (java.security MessageDigest)))

(defn workspace-file
  "Resolve `path` against Vis' active workspace root, not JVM user.dir.
   Extension invocation binds workspace/*workspace-root* from
   Foundation/workspace env; outside a workspace this falls back to process cwd."
  [path]
  (let [file (io/file (str path))]
    (if (.isAbsolute file)
      file
      (io/file (workspace/cwd) (str path)))))

(defn- slurp-path [path]
  (slurp (workspace-file path)))

(defn content-sha256
  "Return SHA-256 hex digest for text content. Used by Bridge index rows for
   incremental backfill/change detection."
  [content]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                 (.getBytes (str content) "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and 0xff %)) digest))))

(defn clojure-lsp-status
  "Return external clojure-lsp availability used by Bridge's Clojure extractor."
  ([] (clojure-lsp-status nil))
  ([opts]
   (clj/executable-status opts)))

(defn extract-file
  "Extract Bridge facts for one source/document file. Dispatches by path unless
   `:language` is supplied. Does not write storage."
  ([path] (extract-file path nil))
  ([path opts]
   (let [opts (or opts {})
         content (or (:content opts) (slurp-path path))
         result (languages/extract-file (str path) content opts)]
     (assoc-in result [:stats :hash-sha256] (content-sha256 content)))))

(defn extract
  "Generic Bridge extraction entry point.

   Shapes:
   - (bridge/extract {:path 'README.md'}) one-file extraction.
   - (bridge/extract {:paths ['README.md' 'docs/foo.md']}) many-file extraction.
   - (bridge/extract {:language 'clojure' :project-root '.'}) project extraction.
   - (bridge/extract) defaults to Clojure project extraction for v1.

   Extraction returns normalized facts only: `:nodes`, `:edges`,
   `:diagnostics`, `:stats`. Use `bridge/fill!` or
   `bridge/extract-and-fill!` to persist."
  ([] (extract nil))
  ([opts]
   (let [opts (or opts {})]
     (cond
       (:path opts)
       (extract-file (:path opts) opts)

       (seq (:paths opts))
       (let [results (mapv #(extract-file % (dissoc opts :paths)) (:paths opts))]
         (schema/extract-result
           {:nodes (vec (mapcat :nodes results))
            :edges (vec (mapcat :edges results))
            :diagnostics (vec (mapcat :diagnostics results))
            :stats {:language (some-> (:language opts) name)
                    :paths (vec (map str (:paths opts)))
                    :path-hashes (into {}
                                   (map (fn [result]
                                          [(get-in result [:stats :path])
                                           (get-in result [:stats :hash-sha256])])
                                     results))
                    :result-count (count results)
                    :node-count (reduce + (map #(count (:nodes %)) results))
                    :edge-count (reduce + (map #(count (:edges %)) results))}}))

       :else
       (languages/extract-project opts)))))

(defn extract-markdown
  "Extract Bridge Markdown facts from `path` using CommonMark. Kept as a
   convenience wrapper over generic `bridge/extract`."
  [path]
  (extract {:path path :language "markdown"}))

(defn extract-clojure
  "Extract Bridge Clojure facts for a project. Kept as a convenience wrapper
   over generic `bridge/extract`."
  ([] (extract-clojure nil))
  ([opts]
   (extract (assoc (or opts {}) :language "clojure"))))

(defn edge-key
  "Stable aggregate key for a Bridge edge. One row per source/kind/target."
  [{:keys [source edge-kind target]}]
  (str "edge:" source "::" (name edge-kind) "::" target))

(defn node-key [{:keys [qualified-name]}]
  (str "node:" qualified-name))

(defn index-key [path]
  (str "idx:" path))

(defn result-paths
  "All source paths represented by an extraction result."
  [result]
  (->> (concat (map :path (:nodes result))
         (map :path (:edges result))
         [(get-in result [:stats :path])])
    (remove nil?)
    (remove #(= "<clojure-lsp-dep-graph>" %))
    distinct
    vec))

(defn result-language [result]
  (or (get-in result [:stats :language])
    (some :language (:nodes result))
    (some :language (:edges result))))

(defn- node-metadata [{node-kind :kind node-name :name
                       :keys [path language visibility metadata]}]
  (cond-> {:path path
           :language language
           :kind (name node-kind)
           :name node-name}
    visibility (assoc :visibility (name visibility))
    (map? metadata) (merge (select-keys metadata [:symbol-kind :doc-kind :relevance :layer]))))

(defn- edge-metadata [{:keys [path language edge-kind source target resolved? metadata]}]
  (cond-> {:path path
           :edge-kind (name edge-kind)
           :source source
           :target target}
    language (assoc :language language)
    (some? resolved?) (assoc :resolved? resolved?)
    (map? metadata) (merge (select-keys metadata [:syntax :backend :relevance]))))

(defn node-row
  "Map one normalized Bridge node to an extension aggregate row."
  [node]
  {:key (node-key node)
   :kind :bridge/node
   :scope :global
   :metadata (node-metadata node)
   :content node})

(defn edge-row
  "Map one normalized Bridge edge to an extension aggregate row."
  [edge]
  {:key (edge-key edge)
   :kind :bridge/edge
   :scope :global
   :metadata (edge-metadata edge)
   :content edge})

(defn index-row
  "Build the per-path index aggregate row for an extraction result."
  [path result]
  {:key (index-key path)
   :kind :bridge/index
   :scope :global
   :metadata (cond-> {:path path
                      :language (result-language result)}
               (or (get-in result [:stats :path-hashes path])
                 (get-in result [:stats :hash-sha256]))
               (assoc :hash-sha256 (or (get-in result [:stats :path-hashes path])
                                     (get-in result [:stats :hash-sha256]))))
   :content (cond-> {:path path
                     :language (result-language result)
                     :node-count (count (:nodes result))
                     :edge-count (count (:edges result))
                     :stats (:stats result)}
              (or (get-in result [:stats :path-hashes path])
                (get-in result [:stats :hash-sha256]))
              (assoc :hash-sha256 (or (get-in result [:stats :path-hashes path])
                                    (get-in result [:stats :hash-sha256]))))})

(defn aggregate-rows
  "Convert a normalized Bridge extraction result to extension aggregate rows.
   Pure. Does not write storage. Use this to inspect the fill mapping before
   calling `bridge/fill!`."
  ([result] (aggregate-rows result nil))
  ([result {:keys [path paths index?] :or {index? true}}]
   (schema/assert-extract-result! result)
   (let [index-paths (cond
                       path [path]
                       (seq paths) paths
                       (= false index?) []
                       :else (result-paths result))
         rows (vec (concat
                     (map node-row (:nodes result))
                     (map edge-row (:edges result))
                     (map #(index-row % result) index-paths)))]
     (schema/assert-aggregate-rows! rows))))

(defn delete-path!
  "Delete all Bridge aggregate rows for one source path."
  [env path]
  (reduce +
    (map (fn [kind]
           (vis/extension-delete-aggregate! env {:kind kind :metadata {:path path}}))
      [:bridge/node :bridge/edge :bridge/index :bridge/summary])))

(defn delete-language!
  "Delete Bridge rows for one language."
  [env language]
  (reduce +
    (map (fn [kind]
           (vis/extension-delete-aggregate! env {:kind kind :metadata {:language language}}))
      [:bridge/node :bridge/edge :bridge/index :bridge/summary])))

(defn clear!
  "Delete all Bridge aggregate rows for this extension."
  [env]
  (reduce +
    (map (fn [kind] (vis/extension-delete-aggregate! env {:kind kind}))
      [:bridge/node :bridge/edge :bridge/index :bridge/summary])))

(defn- apply-replace!
  [env result {:keys [replace path paths language]}]
  (case replace
    nil 0
    false 0
    :none 0
    :path (reduce + (map #(delete-path! env %) (or (seq paths) (seq (cond-> (result-paths result) path (conj path))) [])))
    :language (if-let [lang (or language (result-language result))]
                (delete-language! env lang)
                (throw (ex-info "Bridge fill :replace :language requires a language"
                         {:type :bridge.fill/missing-language})))
    :all (clear! env)
    (throw (ex-info "Unknown Bridge fill replace mode"
             {:type :bridge.fill/unknown-replace-mode
              :replace replace}))))

(defn- fill-with-env!
  [env result opts]
  (let [opts (or opts {})
        rows (aggregate-rows result opts)
        deleted (apply-replace! env result opts)]
    (doseq [row rows]
      (vis/extension-aggregate-put! env row))
    {:rows (count rows)
     :deleted deleted
     :nodes (count (:nodes result))
     :edges (count (:edges result))
     :indexes (count (filter #(= :bridge/index (:kind %)) rows))}))

(defn indexed-path-state
  "Return current hash/index state for `path` using Bridge index rows.
   Requires extension context because it reads via `vis/extension-aggregate-get`."
  [env path]
  (let [path (str path)
        content (slurp-path path)
        current-hash (content-sha256 content)
        row (vis/extension-aggregate-get env {:kind :bridge/index :metadata {:path path}})
        indexed-hash (or (get-in row [:content :hash-sha256])
                       (get-in row [:metadata :hash-sha256]))]
    {:path path
     :hash-sha256 current-hash
     :indexed-hash indexed-hash
     :indexed? (some? row)
     :changed? (not= current-hash indexed-hash)
     :reason (cond
               (nil? row) :new
               (nil? indexed-hash) :missing-index-hash
               (not= current-hash indexed-hash) :changed
               :else :unchanged)}))

(defn stale-paths
  "Return path-state rows for paths whose current SHA differs from Bridge's
   index aggregate. This is the pure backfill decision seam around extension-aggregate-get."
  [env paths]
  (vec (filter :changed? (map #(indexed-path-state env %) paths))))

(defn- backfill-with-env!
  [env opts]
  (let [opts (or opts {})
        paths (vec (map str (or (:paths opts) (when-let [path (:path opts)] [path]))))]
    (when-not (seq paths)
      (throw (ex-info "Bridge backfill requires :path or :paths"
               {:type :bridge.backfill/missing-paths})))
    (let [stale (stale-paths env paths)
          results (mapv (fn [{:keys [path]}]
                          (let [result (extract-file path opts)
                                fill-stats (fill-with-env! env result
                                             (assoc opts :replace :path :path path))]
                            {:path path
                             :extract (:stats result)
                             :fill fill-stats}))
                    stale)]
      {:requested (count paths)
       :stale (count stale)
       :skipped (- (count paths) (count stale))
       :paths (mapv :path stale)
       :results results})))

(defn- default-replace-mode
  [result opts]
  (or (:replace opts)
    (cond
      (:path opts) :path
      (seq (:paths opts)) :path
      (get-in result [:stats :path]) :path
      (get-in result [:stats :project-root]) :language
      (result-language result) :language
      :else false)))

(defn- fill-result*
  [env result & [opts]]
  (let [opts (or opts {})]
    (fill-with-env! env result
      (assoc opts :replace (default-replace-mode result opts)))))

(defn- extract-and-fill*
  [env & [opts]]
  (let [opts (or opts {})
        result (extract opts)
        fill-stats (fill-with-env! env result
                     (assoc opts :replace (default-replace-mode result opts)))]
    {:extract (:stats result)
     :fill fill-stats}))

(defn- inject-env-before-fn
  [env f args]
  {:env env
   :fn f
   :args (vec (cons env args))})

(defn- render-summary [result]
  (let [{:keys [stats]} result]
    (str "Bridge extraction: "
      (or (:language stats) "unknown")
      ", nodes=" (count (:nodes result))
      ", edges=" (count (:edges result))
      (when-let [path (:path stats)] (str ", path=" path))
      (when-let [backend (:backend stats)] (str ", backend=" backend)))))

(defn- render-channel [result]
  (str (render-summary result)
    "\n\n```clojure\n"
    (pr-str (update result :nodes #(take 20 %)))
    "\n```"))

(defn- render-rows-summary [rows]
  (str "Bridge aggregate rows: " (count rows)
    ", kinds=" (pr-str (frequencies (map :kind rows)))))

(defn- render-fill-summary [stats]
  (str "Bridge fill: rows=" (:rows stats)
    ", deleted=" (:deleted stats)
    ", nodes=" (:nodes stats)
    ", edges=" (:edges stats)
    ", indexes=" (:indexes stats)))

(defn- render-extract-and-fill-summary [stats]
  (str "Bridge extract+fill: extract=" (pr-str (:extract stats))
    ", fill=" (pr-str (:fill stats))))

(defn- render-backfill-summary [stats]
  (str "Bridge backfill: requested=" (:requested stats)
    ", stale=" (:stale stats)
    ", skipped=" (:skipped stats)))

(def bridge-symbols
  [(vis/symbol #'extract
     {:journal-render-fn render-summary
      :channel-render-fn render-channel})
   (vis/symbol #'extract-file
     {:journal-render-fn render-summary
      :channel-render-fn render-channel})
   (vis/symbol #'extract-markdown
     {:journal-render-fn render-summary
      :channel-render-fn render-channel})
   (vis/symbol #'clojure-lsp-status
     {:journal-render-fn #(str "clojure-lsp available=" (:available? %)
                            (when-let [path (:path %)] (str ", path=" path)))
      :channel-render-fn #(str "```clojure\n" (pr-str %) "\n```")})
   (vis/symbol #'extract-clojure
     {:journal-render-fn render-summary
      :channel-render-fn render-channel})
   (vis/symbol #'aggregate-rows
     {:journal-render-fn render-rows-summary
      :channel-render-fn #(str "```clojure\n" (pr-str %) "\n```")})
   (vis/symbol 'fill! fill-result*
     {:doc "Persist a normalized Bridge extraction result through extension aggregates. Defaults to replacing path rows for file results and language rows for project results."
      :arglists '([result] [result opts])
      :before-fn inject-env-before-fn
      :journal-render-fn render-fill-summary
      :channel-render-fn #(str "```clojure\n" (pr-str %) "\n```")})
   (vis/symbol 'extract-and-fill! extract-and-fill*
     {:doc "Extract Bridge facts and persist them. `(bridge/extract-and-fill! {:path ...})` reindexes one file; `(bridge/extract-and-fill! {:language \"clojure\"})` refreshes that language."
      :arglists '([] [opts])
      :before-fn inject-env-before-fn
      :journal-render-fn render-extract-and-fill-summary
      :channel-render-fn #(str "```clojure\n" (pr-str %) "\n```")})
   (vis/symbol 'backfill! backfill-with-env!
     {:doc "Incrementally reindex only changed files from `:path` or `:paths`. Uses SHA-256 hashes stored in Bridge index rows."
      :arglists '([opts])
      :before-fn inject-env-before-fn
      :journal-render-fn render-backfill-summary
      :channel-render-fn #(str "```clojure\n" (pr-str %) "\n```")})])

(defn- prompt [_env]
  (str (vis/render-prompt
         {:ext/doc "Bridge codebase graph tools"
          :ext/alias {:alias 'bridge}
          :ext/symbols bridge-symbols})
    "\nBridge v1 contract: language extractors emit stable normalized facts; bridge/fill! maps them to aggregate rows. Use bridge/extract for facts, bridge/aggregate-rows for preview, bridge/fill! to persist, bridge/extract-and-fill! for forced reindexing, or bridge/backfill! to reindex only changed paths."))

(def vis-extension
  (vis/extension
    {:ext/namespace 'com.blockether.vis.ext.bridge.core
     :ext/doc "Bridge codebase knowledge graph extension: CommonMark Markdown extraction and Clojure semantic extraction via external clojure-lsp."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "code-intelligence"
     :ext/alias {:ns 'vis.ext.bridge :alias 'bridge}
     :ext/symbols bridge-symbols
     :ext/prompt prompt
     :ext/doctor-check-fn doctor/check-fn}))

(vis/register-extension! vis-extension)
