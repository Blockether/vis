(ns com.blockether.vis.ext.bridge.languages.schema
  "Normalized Bridge extraction facts.

   Extractors return this language-neutral IR instead of raw ASTs. Storage and
   query layers can then treat Clojure, Markdown, and future tree-sitter-backed
   languages uniformly."
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str]))

(def graph-kinds
  ;; Keep labels stable and sparse. Language/doc-specific detail belongs in
  ;; node metadata (`:symbol-kind`, `:doc-kind`, `:route?`, etc.), not in a
  ;; growing enum that every backend must agree on.
  #{:project :folder :file :module :symbol :doc-section :external})

(def edge-kinds
  ;; Same rule for relationships: few graph verbs, details in metadata.
  #{:contains :imports :calls :uses :mentions :links-to :documents :inherits
    :implements :tests :configures})

(defn non-blank-string? [x]
  (and (string? x) (not (str/blank? x))))

(s/def ::kind graph-kinds)
(s/def ::edge-kind edge-kinds)
(s/def ::language non-blank-string?)
(s/def ::name non-blank-string?)
(s/def ::qualified-name non-blank-string?)
(s/def ::path non-blank-string?)
(s/def ::line-start pos-int?)
(s/def ::line-end pos-int?)
(s/def ::column-start pos-int?)
(s/def ::column-end pos-int?)
(s/def ::visibility #{:public :private :unknown})
(s/def ::metadata map?)

(s/def ::node
  (s/keys :req-un [::kind ::language ::name ::qualified-name ::path]
    :opt-un [::line-start ::line-end ::column-start ::column-end
             ::visibility ::metadata]))

(s/def ::source non-blank-string?)
(s/def ::target non-blank-string?)
(s/def ::resolved? boolean?)
(s/def ::confidence number?)
(s/def ::line pos-int?)
(s/def ::column pos-int?)

(s/def ::edge
  (s/keys :req-un [::edge-kind ::source ::target ::path]
    :opt-un [::language ::line ::column ::resolved? ::confidence ::metadata]))

(s/def ::severity #{:info :warn :error})
(s/def ::message non-blank-string?)
(s/def ::diagnostic
  (s/keys :req-un [::severity ::message]
    :opt-un [::path ::line ::column ::metadata]))

(s/def ::nodes (s/coll-of ::node :kind vector?))
(s/def ::edges (s/coll-of ::edge :kind vector?))
(s/def ::diagnostics (s/coll-of ::diagnostic :kind vector?))
(s/def ::stats map?)

(s/def ::extract-result
  (s/keys :req-un [::nodes ::edges ::diagnostics ::stats]))

;; Extension aggregate row schema. This lives beside the extraction schema so
;; the extractor→fill contract is one stable, inspectable surface.
(def aggregate-kinds #{:bridge/node :bridge/edge :bridge/index :bridge/summary})

(defn aggregate-row? [row]
  (and (map? row)
    (non-blank-string? (:key row))
    (contains? aggregate-kinds (:kind row))
    (= :global (:scope row))
    (map? (:metadata row))
    (map? (:content row))))

(s/def ::aggregate-row aggregate-row?)
(s/def ::aggregate-rows (s/coll-of ::aggregate-row :kind vector?))

(defn valid-extract-result? [x]
  (s/valid? ::extract-result x))

(defn explain-extract-result [x]
  (s/explain-data ::extract-result x))

(defn assert-extract-result!
  "Return `x` when it satisfies Bridge's extractor result schema; throw with
   explain data otherwise."
  [x]
  (when-not (valid-extract-result? x)
    (throw (ex-info "Invalid Bridge extract result"
             {:type :bridge.schema/invalid-extract-result
              :explain (explain-extract-result x)})))
  x)

(defn node
  "Construct a normalized Bridge node."
  [{:keys [kind language name qualified-name path] :as m}]
  (assert-extract-result!
    {:nodes [(merge {:kind kind
                     :language language
                     :name name
                     :qualified-name qualified-name
                     :path path
                     :visibility :unknown
                     :metadata {}}
               (dissoc m :kind :language :name :qualified-name :path))]
     :edges []
     :diagnostics []
     :stats {}})
  (merge {:kind kind
          :language language
          :name name
          :qualified-name qualified-name
          :path path
          :visibility :unknown
          :metadata {}}
    (dissoc m :kind :language :name :qualified-name :path)))

(defn edge
  "Construct a normalized Bridge edge."
  [{:keys [edge-kind source target path] :as m}]
  (merge {:edge-kind edge-kind
          :source source
          :target target
          :path path
          :resolved? false
          :confidence 1.0
          :metadata {}}
    (dissoc m :edge-kind :source :target :path)))

(defn valid-aggregate-rows? [x]
  (s/valid? ::aggregate-rows x))

(defn explain-aggregate-rows [x]
  (s/explain-data ::aggregate-rows x))

(defn assert-aggregate-rows!
  "Return `rows` when they satisfy Bridge's aggregate row schema; throw with
   explain data otherwise."
  [rows]
  (when-not (valid-aggregate-rows? rows)
    (throw (ex-info "Invalid Bridge aggregate rows"
             {:type :bridge.schema/invalid-aggregate-rows
              :explain (explain-aggregate-rows rows)})))
  rows)

(defn extract-result
  "Construct and validate a normalized extractor result."
  [{:keys [nodes edges diagnostics stats]}]
  (assert-extract-result!
    {:nodes (vec nodes)
     :edges (vec edges)
     :diagnostics (vec diagnostics)
     :stats (or stats {})}))
