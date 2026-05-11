(ns com.blockether.vis.ext.bridge.languages.clojure-basic
  "Small Clojure fallback extractor using edamame.

   This is deliberately syntax-only. Prefer `extract-clojure-lsp` whenever the
   external clojure-lsp CLI is available."
  (:require
   [com.blockether.vis.ext.bridge.schema :as schema]
   [edamame.core :as edamame]))

(def ^:private parse-opts
  {:row-key :row
   :col-key :col
   :end-row-key :end-row
   :end-col-key :end-col
   :all true
   :auto-resolve #{:current :keyword}
   :read-cond :allow})

(defn- form-meta [form]
  (meta form))

(defn- list-head [form]
  (when (seq? form)
    (first form)))

(defn- ns-form? [form]
  (= 'ns (list-head form)))

(defn- def-form? [form]
  (contains? '#{def defn defn- defmacro defprotocol defrecord deftype} (list-head form)))

(defn- parse-forms [content]
  (edamame/parse-string-all (or content "") parse-opts))

(defn- source-ns-name [forms]
  (or (some (fn [form]
              (when (ns-form? form)
                (second form)))
        forms)
    'user))

(defn- def-kind [head]
  (case head
    defn :function
    defn- :function
    defmacro :macro
    defprotocol :protocol
    defrecord :record
    deftype :type
    :var))

(defn- qname [ns-sym name-sym]
  (str ns-sym "/" name-sym))

(defn- namespace-node [path ns-sym form]
  (let [m (form-meta form)]
    (schema/node
      {:kind :namespace
       :language "clojure"
       :name (str ns-sym)
       :qualified-name (str ns-sym)
       :path path
       :line-start (or (:row m) 1)
       :line-end (or (:end-row m) (:row m) 1)})))

(defn- def-node [path ns-sym form]
  (let [head (list-head form)
        nm (second form)
        m (form-meta form)]
    (schema/node
      {:kind (def-kind head)
       :language "clojure"
       :name (str nm)
       :qualified-name (qname ns-sym nm)
       :path path
       :line-start (or (:row m) 1)
       :line-end (or (:end-row m) (:row m) 1)
       :visibility (if (= head 'defn-) :private :public)
       :metadata {:backend :edamame/basic
                  :defined-by head}})))

(defn- require-targets [ns-form]
  (letfn [(target [x]
            (cond
              (symbol? x) x
              (and (vector? x) (symbol? (first x))) (first x)
              :else nil))]
    (->> ns-form
      (filter seq?)
      (filter #(= :require (first %)))
      (mapcat rest)
      (keep target))))

(defn- require-edges [path ns-sym forms]
  (vec
    (mapcat (fn [form]
              (when (ns-form? form)
                (let [m (form-meta form)]
                  (map (fn [target]
                         (schema/edge
                           {:edge-kind :requires
                            :source (str ns-sym)
                            :target (str target)
                            :path path
                            :language "clojure"
                            :line (or (:row m) 1)
                            :resolved? true
                            :metadata {:backend :edamame/basic}}))
                    (require-targets form)))))
      forms)))

(defn extract-file
  "Extract basic syntax facts from one Clojure source file."
  [path content]
  (let [path (str path)
        forms (parse-forms content)
        ns-sym (source-ns-name forms)
        ns-form (some #(when (ns-form? %) %) forms)
        nodes (vec (concat
                     [(namespace-node path ns-sym ns-form)]
                     (map #(def-node path ns-sym %) (filter def-form? forms))))
        edges (require-edges path ns-sym forms)]
    (schema/extract-result
      {:nodes nodes
       :edges edges
       :diagnostics []
       :stats {:language "clojure"
               :path path
               :backend :edamame/basic
               :node-count (count nodes)
               :edge-count (count edges)}})))
