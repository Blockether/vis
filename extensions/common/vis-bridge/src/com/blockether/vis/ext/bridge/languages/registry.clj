(ns com.blockether.vis.ext.bridge.languages.registry
  "Language extractor registry for Bridge.

   New languages plug in here by exposing the same small contract:
   `:language`, `:supports-path?`, optional `:extract-file`, optional
   `:extract-project`. Extractors return normalized Bridge facts only."
  (:require
   [com.blockether.vis.ext.bridge.languages.clojure :as clj]
   [com.blockether.vis.ext.bridge.languages.markdown :as markdown]))

(def extractors
  [{:language "markdown"
    :supports-path? markdown/supports-path?
    :extract-file markdown/extract-file}
   {:language "clojure"
    :supports-path? clj/supports-path?
    :extract-file clj/extract-file
    :extract-project clj/extract-project
    :status clj/executable-status}])

(defn extractor-for-language
  "Return extractor map for language string/keyword/symbol, or nil."
  [language]
  (let [language-name (some-> language name)]
    (first (filter #(= language-name (:language %)) extractors))))

(defn extractor-for-path
  "Return first extractor supporting `path`, or nil."
  [path]
  (first (filter #((:supports-path? %) path) extractors)))

(defn supported-languages []
  (mapv :language extractors))

(defn extract-file
  "Dispatch one-file extraction by path or explicit `:language` option."
  ([path content] (extract-file path content nil))
  ([path content opts]
   (let [extractor (or (some-> (:language opts) extractor-for-language)
                     (extractor-for-path path))]
     (when-not extractor
       (throw (ex-info "No Bridge extractor supports path"
                {:type :bridge.languages/no-extractor
                 :path path
                 :supported-languages (supported-languages)})))
     (when-not (:extract-file extractor)
       (throw (ex-info "Bridge extractor does not support single-file extraction"
                {:type :bridge.languages/file-extraction-unsupported
                 :path path
                 :language (:language extractor)})))
     ((:extract-file extractor) path content opts))))

(defn extract-project
  "Dispatch project extraction by explicit `:language`. Defaults to Clojure for
   v1 because it is the only project-wide extractor."
  ([] (extract-project nil))
  ([opts]
   (let [opts (or opts {})
         language (or (:language opts) "clojure")
         extractor (extractor-for-language language)]
     (when-not extractor
       (throw (ex-info "No Bridge project extractor for language"
                {:type :bridge.languages/no-project-extractor
                 :language language
                 :supported-languages (supported-languages)})))
     (when-not (:extract-project extractor)
       (throw (ex-info "Bridge extractor does not support project extraction"
                {:type :bridge.languages/project-extraction-unsupported
                 :language (:language extractor)})))
     ((:extract-project extractor) opts))))
